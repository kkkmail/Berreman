namespace OpticalConstructor.Storage

open System
open System.IO
open System.Text.Json
open Json.Schema
open Giraffe.ViewEngine
open Berreman.MathNetNumericsMath
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalConstructor.Domain.Project
open OpticalConstructor.Domain.DispersionModels
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.Export

/// Shareable material-library file format (§I.8 / R-4). Import accepts the canonical
/// library JSON `exportMaterials` writes AND refractiveindex.info-style external CSV
/// (delegated to `MaterialImport`/FSharp.Data — no hand-rolled CSV reader). Export
/// writes the canonical JSON, validated against the §A.7 `materialEntry` `$def`
/// (the slice-003 schema, reused), so a library file is itself schema-checked. The
/// dispersion-model→tensor MAPPING is owned by Part D §D.9 and is NOT redefined here;
/// this module owns only the file read and the `MaterialEntry list` boundary.
module MaterialLibrary =

    /// Persisted-form DTO mirroring the `materialEntry` `$def`'s display fields. The
    /// engine's `OpticalPropertiesWithDisp` holds closures and cannot serialize, so
    /// the library file carries the schema's display metadata only (the analytic
    /// dispersion model lives behind the §D.9 mapping, out of scope here). The
    /// `category` field is the domain `MaterialCategory` DU itself: the shared
    /// `ProjectJson.options` (`.WithUnionUnwrapFieldlessTags()`) round-trips this
    /// four-case fieldless DU to/from its plain-string form (`"Glass"`/`"Metal"`/…)
    /// — the one JSON stack already owns this mapping, so no hand-maintained
    /// `MaterialCategory`↔string table lives here (§I.5/§I.8 "one stack only").
    type private MaterialEntryDto =
        {
            id : string
            name : string
            category : MaterialCategory
            description : string option
        }

    /// Vacuum (n = 1) optical properties — the documented placeholder for the
    /// `properties` of an entry rebuilt from a metadata-only library JSON. Rebuilding
    /// the full `OpticalPropertiesWithDisp` from a persisted dispersion model is the
    /// §D.9 mapping (Part D); this module owns only the metadata boundary.
    let private vacuumProperties : OpticalPropertiesWithDisp =
        createComplex 1.0 0.0
        |> ComplexRefractionIndex
        |> Eps.fromComplexRefractionIndex
        |> EpsWithoutDisp
        |> isotropicProperties

    /// A standalone JSON-Schema that validates a single `materialEntry` document.
    /// Built from the published project schema (the slice-003 build-copied content
    /// item) by pointing `$ref` at `#/$defs/materialEntry` and embedding the original
    /// `$defs` so every internal `#/$defs/...` reference resolves in the one document.
    /// Carries no `$id`, so loading it does not collide with the project schema's
    /// global registration. Loaded once behind `lazy` (I.2 reuse).
    let private materialEntrySchema : Lazy<Result<JsonSchema, StorageError>> =
        lazy
            let schemaPath =
                Path.Combine(AppContext.BaseDirectory, "schema", "optical-constructor-project.schema.json")
            if not (File.Exists schemaPath) then Error SchemaMissing
            else
                try
                    let text = File.ReadAllText schemaPath
                    use doc = JsonDocument.Parse text
                    let defsText = doc.RootElement.GetProperty("$defs").GetRawText()
                    let wrapper =
                        sprintf
                            """{"$schema":"https://json-schema.org/draft/2020-12/schema","$ref":"#/$defs/materialEntry","$defs":%s}"""
                            defsText
                    Ok (JsonSchema.FromText wrapper)
                with e -> Error (FileIoError e)

    /// Validate a single material-entry JSON element against the `materialEntry`
    /// `$def` (§I.8 / AC-I10), reusing the slice-003 schema document.
    let validateEntryElement (element : JsonElement) : Result<unit, StorageError> =
        match materialEntrySchema.Value with
        | Error e -> Error e
        | Ok schema ->
            let options = EvaluationOptions(OutputFormat = OutputFormat.List)
            let results = schema.Evaluate(element, options)
            if results.IsValid then Ok ()
            else Error (SchemaValidationError [ "document violated the materialEntry $def" ])

    let private dtoToEntry (dto : MaterialEntryDto) : MaterialEntry =
        {
            id = dto.id
            name = dto.name
            category = dto.category
            description = dto.description
            properties = vacuumProperties
        }

    /// Import a material library (§I.8 / AC-I10). A document that begins with `[`/`{`
    /// is the canonical library JSON `exportMaterials` wrote (deserialized through the
    /// shared `ProjectJson.options` — no second JSON stack); anything else is treated
    /// as refractiveindex.info-style tabulated CSV and parsed by `MaterialImport`
    /// (FSharp.Data). Returns `MaterialEntry list`; file/parse failures are values,
    /// never thrown.
    let importMaterials (path : string) : Result<MaterialEntry list, StorageError> =
        let read =
            try Ok(File.ReadAllText path)
            with e -> Error(FileIoError e)
        match read with
        | Error e -> Error e
        | Ok text ->
            let trimmed = text.TrimStart()
            if trimmed.StartsWith "[" || trimmed.StartsWith "{" then
                try
                    let dtos = JsonSerializer.Deserialize<MaterialEntryDto list>(text, ProjectJson.options)
                    Ok(dtos |> List.map dtoToEntry)
                with e -> Error(JsonParseError e.Message)
            else
                match MaterialImport.importCsv text with
                | Ok entry -> Ok [ entry ]
                | Error e -> Error(JsonParseError(string e))

    /// Export a material library to canonical JSON (§I.8 / AC-I10). Each entry is
    /// projected to its `materialEntry` display fields (a `description = None` field
    /// is omitted, never written as `null`, so the `$def`'s string-typed `description`
    /// is satisfied), validated against the `$def`, and the array is written through
    /// the shared `ProjectJson.options` formatting. A validation failure short-circuits
    /// to `SchemaValidationError` so an `Ok` guarantees a schema-checked file.
    let exportMaterials (path : string) (entries : MaterialEntry list) : Result<unit, StorageError> =
        try
            let nodes =
                entries
                |> List.map (fun e ->
                    let dict = System.Collections.Generic.Dictionary<string, obj>()
                    dict.["id"] <- box e.id
                    dict.["name"] <- box e.name
                    // The MaterialCategory DU is serialized by the shared
                    // ProjectJson.options (fieldless-tag unwrap) — its runtime type
                    // drives the converter, so this writes "Glass"/"Metal"/… with no
                    // hand-maintained category→string map (§I.5/§I.8 one-stack rule).
                    dict.["category"] <- box e.category
                    match e.description with
                    | Some d -> dict.["description"] <- box d
                    | None -> ()
                    dict)
            // Validate each entry against the materialEntry $def before writing.
            let rec validateAll (ds : System.Collections.Generic.Dictionary<string, obj> list) : Result<unit, StorageError> =
                match ds with
                | [] -> Ok()
                | d :: rest ->
                    let json = JsonSerializer.Serialize(d, ProjectJson.options)
                    use doc = JsonDocument.Parse json
                    match validateEntryElement doc.RootElement with
                    | Ok() -> validateAll rest
                    | Error e -> Error e
            match validateAll nodes with
            | Error e -> Error e
            | Ok() ->
                let json = JsonSerializer.Serialize(nodes, ProjectJson.options)
                File.WriteAllText(path, json, System.Text.UTF8Encoding false)
                Ok()
        with e -> Error(FileIoError e)

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

    /// One structural difference, keyed by a stable identity (the item's positional
    /// index path in the project aggregate).
    type DiffItem =
        {
            identity : string
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
    let appendRevision (projectFolder : string) (project : OpticalConstructorProject) : Result<unit, StorageError> =
        match ProjectJson.serializeProject project with
        | Error e -> Error e
        | Ok json ->
            try
                let dir = Path.Combine(projectFolder, historyFolder)
                Directory.CreateDirectory dir |> ignore
                let next = (Directory.GetFiles(dir, "rev-*.json") |> Array.length) + 1
                let file = Path.Combine(dir, sprintf "rev-%04d.json" next)
                File.WriteAllText(file, json, System.Text.UTF8Encoding false)
                Ok()
            with e -> Error(FileIoError e)

    /// Positional diff of two value-comparable lists: an item present only in the new
    /// list is `Added`, only in the old is `Removed`, present in both but unequal is
    /// `Changed`, equal is omitted. Identity is `<label>[<index>]`.
    let private diffByIndex (label : string) (oldItems : 'a list) (newItems : 'a list) : DiffItem list =
        let o = List.toArray oldItems
        let n = List.toArray newItems
        [ for i in 0 .. (max o.Length n.Length) - 1 do
            let ident = sprintf "%s[%d]" label i
            if i >= o.Length then yield { identity = ident; change = Added }
            elif i >= n.Length then yield { identity = ident; change = Removed }
            elif o.[i] <> n.[i] then yield { identity = ident; change = Changed } ]

    let private layerDiff (oldSystems : Berreman.Media.OpticalSystem list) (newSystems : Berreman.Media.OpticalSystem list) : DiffItem list =
        let o = List.toArray oldSystems
        let n = List.toArray newSystems
        [ for i in 0 .. (max o.Length n.Length) - 1 do
            let oldFilms = if i < o.Length then List.toArray o.[i].films else [||]
            let newFilms = if i < n.Length then List.toArray n.[i].films else [||]
            for j in 0 .. (max oldFilms.Length newFilms.Length) - 1 do
                let ident = sprintf "system[%d].layer[%d]" i j
                if j >= oldFilms.Length then yield { identity = ident; change = Added }
                elif j >= newFilms.Length then yield { identity = ident; change = Removed }
                elif oldFilms.[j] <> newFilms.[j] then yield { identity = ident; change = Changed } ]

    /// Structural diff of two already-loaded project values (§I.8 / AC-I11). Pure —
    /// no external VCS, no git. Systems and sources diff positionally by stable index
    /// identity; layers diff per system. The project aggregate (slice 003/007) carries
    /// `beamTree`/`systems`/`sources` but no top-level materials list yet, so the
    /// `materials` diff is empty until a later slice adds that field.
    let diffRevisions (oldProject : OpticalConstructorProject) (newProject : OpticalConstructorProject) : ProjectDiff =
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
        | Pdf
        | Html

    /// Conventional per-user reports folder. `generate` carries no output-path
    /// parameter in its spec signature, so it writes to this fixed location and the
    /// UI host (Part J §J.10) owns final placement / "offer the produced file".
    let private reportsDir () =
        Path.Combine(
            Environment.GetFolderPath Environment.SpecialFolder.ApplicationData,
            "Softellect", "Berreman", "OpticalConstructor", "reports")

    let private buildHtml
        (project : OpticalConstructorProject)
        (chartImages : (string * string) list)
        : string =

        let systemRows =
            project.systems
            |> List.mapi (fun i s ->
                tr [] [
                    td [] [ str (sprintf "system[%d]" i) ]
                    td [] [ str (defaultArg s.description "(no description)") ]
                ])

        let chartNodes =
            chartImages
            |> List.map (fun (title, imgPath) ->
                div [] [ h2 [] [ str title ]; img [ _src imgPath ] ])

        let bodyNodes =
            [
                h1 [] [ str "Optical Constructor Report" ]
                h2 [] [ str "Stack schematic" ]
                table [] (
                    (tr [] [ th [] [ str "Element" ]; th [] [ str "Description" ] ]) :: systemRows)
                h2 [] [ str "Project parameters" ]
                p [] [ str (sprintf "%d system(s), %d source(s)." (List.length project.systems) (List.length project.sources)) ]
            ]
            @ chartNodes

        let doc =
            html [] [
                head [] [ title [] [ str "Optical Constructor Report" ] ]
                body [] bodyNodes
            ]
        RenderView.AsString.htmlDocument doc

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
            Directory.CreateDirectory dir |> ignore
            let images =
                charts
                |> List.mapi (fun i h ->
                    let imgPath = Path.Combine(dir, sprintf "chart-%d.png" i)
                    // Best-effort image export; the actual render is the chart
                    // library's own writer (UI/WebView2-hosted) and is referenced by
                    // path in the HTML regardless of headless availability.
                    exportChartImage imgPath Png h |> ignore
                    (h.title, imgPath))
            let htmlText = buildHtml project images
            let ext = match format with | Html -> "html" | Pdf -> "pdf"
            let outPath = Path.Combine(dir, sprintf "report.%s" ext)
            File.WriteAllText(outPath, htmlText, System.Text.UTF8Encoding false)
            Ok()
        with e -> Error(FileIoError e)
