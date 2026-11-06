namespace OpticalConstructor.Storage

open System
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open Json.Schema
open Giraffe.ViewEngine
open Softellect.Sys.Primitives
open Berreman.MathNetNumericsMath
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalConstructor.Domain.Project
open OpticalConstructor.Domain.DispersionModels
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.Export
open OpticalConstructor.Storage.ProjectJson

/// Shareable material-library file format (§I.8 / R-4). Import accepts the canonical
/// library JSON `exportMaterials` writes AND refractiveindex.info-style external CSV
/// (delegated to `MaterialImport`/FSharp.Data — no hand-rolled CSV reader). Export
/// writes the canonical JSON, validated against the §A.7 `materialEntry` `$def`
/// (the slice-003 schema, reused), so a library file is itself schema-checked. The
/// dispersion-model→tensor MAPPING is owned by Part D §D.9 and is NOT redefined here;
/// this module owns only the file read and the `MaterialEntry list` boundary.
module MaterialLibrary =

    /// Metadata-only materialEntry DTO: category display names resolve through the caller's categories
    /// on import and export (spec 0035 step 001; spec 0050 S1.7).
    [<Softellect.Sys.ElevationBoundary(nameof Softellect.Sys.StorageBoundary, "the library file's materialEntry shape: System.Text.Json binds these fields by name from the file, and dtoToEntry elevates each one at the read")>]
    type private MaterialEntryDto =
        {
            id : string
            name : string
            category : string
            description : string option
        }

    /// The physics of an entry rebuilt from a metadata-only library JSON: vacuum (n = 1 + 0i) as
    /// constant term data, editable like any constant material (spec 0050 step 11).
    let vacuumPlaceholderComplexity : MaterialComplexity =
        {
            eps = EpsWithoutDispValue (IsotropicAbsorbing (ComplexRefractionIndex (System.Numerics.Complex.create 1.0 0.0)))
            magnetic = None
            active = None
        }

    /// Standalone material-entry structure from the project schema, with internal references preserved.
    /// Category membership is checked against the caller's list at conversion (spec 0050 S1.7).
    /// Loaded once without a `$id` to avoid colliding with the project schema's registration (§I.2).
    let private materialEntrySchema : Lazy<Result<JsonSchema, StorageError>> =
        lazy
            let schemaPath =
                Path.Combine(AppContext.BaseDirectory, "schema", "optical-constructor-project.schema.json")
            if not (File.Exists schemaPath) then Error SchemaMissingErr
            else
                try
                    let text = File.ReadAllText schemaPath
                    let document = JsonNode.Parse text
                    let defs = document.["$defs"]
                    // The project enum predates caller-owned categories; keep every structural constraint
                    // while resolving category membership through the supplied list (spec 0050 S1.7).
                    defs.["materialEntry"].["properties"].["category"].AsObject().Remove("enum") |> ignore
                    let defsText = defs.ToJsonString()
                    let wrapper =
                        $"""{{"$schema":"https://json-schema.org/draft/2020-12/schema","$ref":"#/$defs/materialEntry","$defs":%s{defsText}}}"""
                    Ok (JsonSchema.FromText wrapper)
                with e -> Error (FileIoExn e)

    /// Validate material-entry structure through the reused `$def` (§I.8 / AC-I10; spec 0050 S1.7).
    /// The import/export boundary resolves category membership through the caller's list.
    let validateEntryElement (element : JsonElement) : Result<unit, StorageError> =
        match materialEntrySchema.Value with
        | Error e -> Error e
        | Ok schema ->
            let options = EvaluationOptions(OutputFormat = OutputFormat.List)
            let results = schema.Evaluate(element, options)
            if results.IsValid then Ok ()
            else Error (SchemaValidationErr [ "document violated the materialEntry $def" ])

    /// Parse a persisted DTO back to a domain entry. The persisted `id` is the `MaterialId`'s Guid
    /// STRING form (spec 0033 step 002); a non-Guid id is a typed error — no legacy string-id path,
    /// no fallback, no migration.
    let private dtoToEntry (categories : MaterialCategory list) (dto : MaterialEntryDto) : Result<MaterialEntry, StorageError> =
        // Invalid GUIDs and names absent from the caller's categories remain typed errors
        // (spec 0033 step 002 / 0035 step 001; spec 0050 S1.7).
        let categoryName = CategoryName dto.category
        match MaterialId.tryCreate dto.id, tryFindCategoryByName categories categoryName with
        | Some id, Some category ->
            Ok
                {
                    id = id
                    name = EntryName dto.name
                    category = category.id
                    description = dto.description |> Option.map EntryDescription
                    complexity = vacuumPlaceholderComplexity
                }
        | None, _ -> Error (MalformedJsonErr $"materialEntry id '%s{dto.id}' is not a Guid")
        | _, None -> Error (MalformedJsonErr $"materialEntry category '%s{dto.category}' is not a known category")

    type FileName with

        /// Import the material library in this file (§I.8 / AC-I10). A document that begins with `[`/`{`
        /// is the canonical library JSON `exportMaterials` wrote (deserialized through the
        /// shared `CanonicalJson.options` — no second JSON stack); anything else is treated
        /// as refractiveindex.info-style tabulated CSV and parsed by `MaterialImport`
        /// (FSharp.Data). Returns `MaterialEntry list`; file/parse failures are values,
        /// never thrown.
        member this.importMaterials (categories : MaterialCategory list) : Result<MaterialEntry list, StorageError> =
            let read =
                try Ok(File.ReadAllText this.value)
                with e -> Error(FileIoExn e)
            match read with
            | Error e -> Error e
            | Ok text ->
                let trimmed = text.TrimStart()
                if trimmed.StartsWith "[" || trimmed.StartsWith "{" then
                    try
                        let dtos = JsonSerializer.Deserialize<MaterialEntryDto list>(text, CanonicalJson.options)
                        let rec toEntries (pending : MaterialEntryDto list) (acc : MaterialEntry list) : Result<MaterialEntry list, StorageError> =
                            match pending with
                            | [] -> Ok(List.rev acc)
                            | dto :: rest ->
                                match dtoToEntry categories dto with
                                | Ok entry -> toEntries rest (entry :: acc)
                                | Error e -> Error e
                        toEntries dtos []
                    with e -> Error(MalformedJsonErr e.Message)
                else
                    match (MaterialImport.NkCsvText text).importMaterial with
                    | Ok entry -> Ok [ entry ]
                    | Error e -> Error(MalformedJsonErr(string e))

        /// Export schema-checked material metadata using the caller's category names (§I.8 / AC-I10;
        /// spec 0050 S1.7). Missing category ids or invalid structure return SchemaValidationErr
        /// before writing; absent descriptions are omitted and IO failures remain typed errors.
        member this.exportMaterials (categories : MaterialCategory list) (entries : MaterialEntry list) : Result<unit, StorageError> =
            try
                let rec toValidatedNodes
                    (pending : MaterialEntry list)
                    (acc : System.Collections.Generic.Dictionary<string, obj> list)
                    : Result<System.Collections.Generic.Dictionary<string, obj> list, StorageError> =
                    match pending with
                    | [] -> Ok (List.rev acc)
                    | entry :: rest ->
                        // Only names in the caller's list can round-trip through import (spec 0035 step 001; spec 0050 S1.7).
                        match tryFindCategory categories entry.category with
                        | None ->
                            Error (SchemaValidationErr [ $"materialEntry category '{entry.category.value}' is absent from the supplied categories" ])
                        | Some category ->
                            let dict = System.Collections.Generic.Dictionary<string, obj>()
                            dict.["id"] <- box (string entry.id.value)
                            dict.["name"] <- box entry.name.value
                            dict.["category"] <- box category.name.value
                            match entry.description with
                            | Some description -> dict.["description"] <- box description.value
                            | None -> ()
                            let json = JsonSerializer.Serialize(dict, CanonicalJson.options)
                            use doc = JsonDocument.Parse json
                            match validateEntryElement doc.RootElement with
                            | Ok () -> toValidatedNodes rest (dict :: acc)
                            | Error e -> Error e
                match toValidatedNodes entries [] with
                | Error e -> Error e
                | Ok nodes ->
                    let json = JsonSerializer.Serialize(nodes, CanonicalJson.options)
                    File.WriteAllText(this.value, json, System.Text.UTF8Encoding false)
                    Ok()
            with e -> Error(FileIoExn e)

/// Append-only design history + structural diff (§I.8 / R-4). `appendRevision`
/// persists each revision as a numbered canonical-JSON snapshot under a `history/`
/// folder beside the `.ocproj` (JSON, not `.binz` — revisions stay hand/LLM-readable,
/// binding rule 4). `diffRevisions` is a pure F# comparison of two already-loaded
/// project values into a `ProjectDiff`; it NEVER shells out to git or any external
/// VCS. Full branching/merge version control is out of scope — the committed scope is
/// append-only snapshots plus structural diff (010 §7 "design history & diff").
module DesignHistory =

    /// Whether a diffed item was added, removed, or changed between two revisions.
    type ChangeKind =
        | Added
        | Removed
        | Changed

    /// A diffed item's stable identity: its positional index path in the project aggregate
    /// (`system[0].layer[1]`).
    type DiffIdentity =
        | DiffIdentity of string

        member this.value = let (DiffIdentity i) = this in i

    /// One structural difference, keyed by a stable identity.
    type DiffItem =
        {
            identity : DiffIdentity
            change : ChangeKind
        }

    /// The structural diff of two project revisions (§I.8): added/removed/changed
    /// systems, layers, materials, and sources by stable identity. Net-new record.
    type ProjectDiff =
        {
            systems : DiffItem list
            layers : DiffItem list
            materials : DiffItem list
            sources : DiffItem list
        }

    /// The revision sub-folder beside the `.ocproj` (§I.8).
    [<Literal>]
    let historyFolder = "history"

    /// Persist `project` as the next numbered canonical-JSON snapshot under
    /// `<projectFolder>/history/` (§I.8 / AC-I11), reusing `serializeProject`. The
    /// number is the count of existing `rev-*.json` snapshots plus one, zero-padded.
    let appendRevision (projectFolder : FolderName) (project : OpticalConstructorProject) : Result<unit, StorageError> =
        match project.serializeProject with
        | Error e -> Error e
        | Ok json ->
            try
                let dir = projectFolder.combine (FolderName historyFolder)
                Directory.CreateDirectory dir.value |> ignore
                let next = (Directory.GetFiles(dir.value, "rev-*.json") |> Array.length) + 1
                let file = (FileName $"rev-%04d{next}.json").combine dir
                File.WriteAllText(file.value, json.value, System.Text.UTF8Encoding false)
                Ok()
            with e -> Error(FileIoExn e)

    let private layerDiff (oldSystems : Berreman.Media.OpticalSystem list) (newSystems : Berreman.Media.OpticalSystem list) : DiffItem list =
        let o = List.toArray oldSystems
        let n = List.toArray newSystems
        [ for i in 0 .. (max o.Length n.Length) - 1 do
            let oldFilms = if i < o.Length then List.toArray o.[i].films else [||]
            let newFilms = if i < n.Length then List.toArray n.[i].films else [||]
            for j in 0 .. (max oldFilms.Length newFilms.Length) - 1 do
                let ident = DiffIdentity $"system[%d{i}].layer[%d{j}]"
                if j >= oldFilms.Length then yield { identity = ident; change = Added }
                elif j >= newFilms.Length then yield { identity = ident; change = Removed }
                elif oldFilms.[j] <> newFilms.[j] then yield { identity = ident; change = Changed } ]

    /// Structural diff of two already-loaded project values (§I.8 / AC-I11). Pure —
    /// no external VCS, no git. Systems and sources diff positionally by stable index
    /// identity; layers diff per system. The project aggregate (slice 003/007) carries
    /// `beamTree`/`systems`/`sources` but no top-level materials list yet, so the
    /// `materials` diff is empty until a later slice adds that field.
    let diffRevisions (oldProject : OpticalConstructorProject) (newProject : OpticalConstructorProject) : ProjectDiff =
        // Positional diff of two value-comparable lists: an item present only in the new
        // list is `Added`, only in the old is `Removed`, present in both but unequal is
        // `Changed`, equal is omitted. Identity is `<label>[<index>]`.
        let diffByIndex (label : string) (oldItems : 'Item list) (newItems : 'Item list) : DiffItem list =
            let o = List.toArray oldItems
            let n = List.toArray newItems
            [ for i in 0 .. (max o.Length n.Length) - 1 do
                let ident = DiffIdentity $"%s{label}[%d{i}]"
                if i >= o.Length then yield { identity = ident; change = Added }
                elif i >= n.Length then yield { identity = ident; change = Removed }
                elif o.[i] <> n.[i] then yield { identity = ident; change = Changed } ]
        {
            systems = diffByIndex "system" oldProject.systems newProject.systems
            layers = layerDiff oldProject.systems newProject.systems
            materials = []
            sources = diffByIndex "source" oldProject.sources newProject.sources
        }

/// Report generator (§I.8 / R-4). `generate` assembles a single HTML document with
/// the engine's own HTML-builder library (Giraffe.ViewEngine — the same one
/// `Charting.createDescription` uses, `Charting.fs:9,13`; NOT a second HTML builder)
/// carrying the stack schematic, the project parameters (§A.7), the supplied chart
/// images (via I.7 `exportChartImage`), and any fit-result summary present on the
/// project. PDF output is produced by printing that same HTML through the Plotly /
/// WebView2 path (Part J §J.10) — the Storage layer hosts no browser, so the PDF
/// branch persists the print-source HTML for the UI host to paginate.
module Report =

    /// The report output format (§I.8). Net-new DU.
    type ReportFormat =
        | PdfReport
        | HtmlReport

    /// Conventional per-user reports folder. `generate` carries no output-path
    /// parameter in its spec signature, so it writes to this fixed location and the
    /// UI host (Part J §J.10) owns final placement / "offer the produced file".
    let private reportsDir () : FolderName =
        Path.Combine(
            Environment.GetFolderPath Environment.SpecialFolder.ApplicationData,
            "Softellect", "Berreman", "OpticalConstructor", "reports")
        |> FolderName

    /// One chart a report embeds: its section heading and the image exported beside the report.
    type private ReportChart =
        {
            heading : ChartTitle
            image : FileName
        }

    let private reportDocument
        (project : OpticalConstructorProject)
        (charts : ReportChart list)
        : XmlNode =

        let systemRows =
            project.systems
            |> List.mapi (fun i s ->
                tr [] [
                    td [] [ str $"system[%d{i}]" ]
                    td [] [ str (match s.description with | Some (Berreman.Media.OpticalSystemDescription d) -> d | None -> "(no description)") ]
                ])

        let chartNodes =
            charts
            |> List.map (fun chart ->
                div [] [ h2 [] [ str chart.heading.value ]; img [ _src chart.image.value ] ])

        let bodyNodes =
            [
                h1 [] [ str "Optical Constructor Report" ]
                h2 [] [ str "Stack schematic" ]
                table [] (
                    (tr [] [ th [] [ str "Element" ]; th [] [ str "Description" ] ]) :: systemRows)
                h2 [] [ str "Project parameters" ]
                p [] [ str $"%d{List.length project.systems} system(s), %d{List.length project.sources} source(s)." ]
            ]
            @ chartNodes

        html [] [
            head [] [ title [] [ str "Optical Constructor Report" ] ]
            body [] bodyNodes
        ]

    /// Produce a single report document (§I.8 / R-4). Each supplied chart is exported
    /// to a sibling PNG through I.7 `exportChartImage` (the chart library's own writer)
    /// and embedded; the HTML is assembled with Giraffe.ViewEngine; `Html` writes that
    /// HTML and `Pdf` persists the same print-source HTML for the UI's WebView2 PDF
    /// path. Returns `Result` on completion for the UI to refresh and offer the file.
    let generate
        (format : ReportFormat)
        (project : OpticalConstructorProject)
        (charts : ChartHandle list)
        : Result<unit, StorageError> =
        try
            let dir = reportsDir ()
            Directory.CreateDirectory dir.value |> ignore
            let images =
                charts
                |> List.mapi (fun i h ->
                    let image = (FileName $"chart-%d{i}.png").combine dir
                    // Best-effort image export; the actual render is the chart
                    // library's own writer (UI/WebView2-hosted) and is referenced by
                    // path in the HTML regardless of headless availability.
                    exportChartImage image PngImage h |> ignore
                    { heading = h.title; image = image })
            let htmlText = RenderView.AsString.htmlDocument (reportDocument project images)
            let ext = match format with | HtmlReport -> "html" | PdfReport -> "pdf"
            let outPath = (FileName $"report.%s{ext}").combine dir
            File.WriteAllText(outPath.value, htmlText, System.Text.UTF8Encoding false)
            Ok()
        with e -> Error(FileIoExn e)
