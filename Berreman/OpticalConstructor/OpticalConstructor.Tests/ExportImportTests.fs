namespace OpticalConstructor.Tests

open System
open System.IO
open System.Text.Json
open Berreman.Constants
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.BeamTree
open OpticalConstructor.Domain.Project
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Storage
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.Export
open OpticalConstructor.Storage.MaterialLibrary
open OpticalConstructor.Storage.DesignHistory
open Xunit

/// CSV export shape (§I.7, AC-I8), material import/export over the `materialEntry`
/// `$def` (§I.8, AC-I10), and design-history append + diff (§I.8, AC-I11).
module ExportImportTests =

    let private fixtures = Path.Combine(AppContext.BaseDirectory, "fixtures")

    // --- AC-I8: exportCsv writes header + one comma-separated line per row ---------

    [<Fact>]
    let ``AC-I8 exportCsv writes one header line then one comma-separated line per row`` () =
        // The headers/rows are already-computed sweep tuples (no solver re-invocation).
        let headers = [ "wavelength_nm"; "R"; "T" ]
        let rows : float[][] = [| [| 400.0; 0.10; 0.90 |]; [| 500.0; 0.20; 0.80 |] |]
        let path = Path.Combine(Path.GetTempPath(), $"""oc-csv-%s{(Guid.NewGuid().ToString("N"))}.csv""")
        try
            match Export.exportCsv path (headers, rows) with
            | Error e -> Assert.Fail($"%A{e}")
            | Ok () ->
                let lines = File.ReadAllLines path
                Assert.Equal(3, lines.Length)                  // one header + two rows
                Assert.Equal("wavelength_nm,R,T", lines.[0])
                Assert.Equal(3, lines.[1].Split(',').Length)   // one comma-separated value per column
                Assert.Equal(3, lines.[2].Split(',').Length)
        finally
            if File.Exists path then File.Delete path

    // --- AC-I10: material import (CSV via FSharp.Data) + export validates ----------

    [<Fact>]
    let ``AC-I10 importMaterials reads a refractiveindex.info-style CSV and returns MaterialEntry list`` () =
        let csvPath = Path.Combine(fixtures, "sample-nk.csv")
        match importMaterials csvPath with
        | Ok entries -> Assert.False(List.isEmpty entries, "imported at least one entry")
        | Error e -> Assert.Fail($"%A{e}")

    [<Fact>]
    let ``AC-I10 exportMaterials writes JSON that validates against the materialEntry $def`` () =
        // Built-in entries span categories and include None descriptions (the omit-null path).
        let entries = builtInEntries
        let path = Path.Combine(Path.GetTempPath(), $"""oc-mat-%s{(Guid.NewGuid().ToString("N"))}.json""")
        try
            match exportMaterials path entries with
            | Error e -> Assert.Fail($"%A{e}")
            | Ok () ->
                use doc = JsonDocument.Parse(File.ReadAllText path)
                Assert.Equal(JsonValueKind.Array, doc.RootElement.ValueKind)
                let mutable count = 0
                for el in doc.RootElement.EnumerateArray() do
                    count <- count + 1
                    match validateEntryElement el with
                    | Ok () -> ()
                    | Error e -> Assert.Fail($"entry failed materialEntry validation: %A{e}")
                Assert.Equal(List.length entries, count)
        finally
            if File.Exists path then File.Delete path

    // --- spec 0035 step 001: category survives the persistence seam by CategoryId ----
    // Category became DATA (a seeded catalogue keyed by `CategoryId`); the library file carries the
    // display NAME, so `dtoToEntry` must resolve that name back to the matching id on import and reject
    // an unknown name as a typed error. These two facts pin the genuinely-new `tryFindCategoryByName`
    // seam at the `exportMaterials`/`importMaterials` (Report.fs) boundary — the round-trip and its
    // failure case.

    [<Fact>]
    let ``0035 category round-trips through export then import back to the matching CategoryId`` () =
        // Export the built-ins and re-import so `dtoToEntry` runs `tryFindCategoryByName`: a persisted
        // category NAME must resolve back to the SAME `CategoryId` the entry carried — no silent default,
        // no drift across the id<->name hop. Restrict to entries carrying a description (the pre-existing
        // DTO requires the `description` field present on read — see Gotchas); the described built-ins
        // still span every seeded category (Glass/Metal/Semiconductor/Crystal/Vacuum).
        let entries =
            builtInEntries
            |> List.filter (fun e -> match e.description with | Some _ -> true | None -> false)
        let path = Path.Combine(Path.GetTempPath(), $"""oc-cat-%s{(Guid.NewGuid().ToString("N"))}.json""")
        try
            match exportMaterials path entries with
            | Error e -> Assert.Fail($"export failed: %A{e}")
            | Ok () ->
                match importMaterials path with
                | Error e -> Assert.Fail($"import failed: %A{e}")
                | Ok imported ->
                    Assert.Equal(List.length entries, List.length imported)
                    // Align original and re-imported entries by their (order-stable) MaterialId and
                    // assert the CategoryId is unchanged for every category, including HiddenOnCreate.
                    let byId = entries |> List.map (fun e -> e.id, e.category) |> Map.ofList
                    for e in imported do
                        match Map.tryFind e.id byId with
                        | Some expected -> Assert.Equal(expected, e.category)
                        | None -> Assert.Fail($"re-imported an entry with an unexpected id %A{e.id}")
                    // A crisp explicit anchor: the Glass entry's "Glass" name resolves to CategoryIds.glass,
                    // and the Vacuum entry's name resolves to CategoryIds.vacuum (a HiddenOnCreate category).
                    let glass = imported |> List.find (fun e -> e.id = MaterialIds.glass152)
                    Assert.Equal(CategoryIds.glass, glass.category)
                    let vacuum = imported |> List.find (fun e -> e.id = MaterialIds.vacuum)
                    Assert.Equal(CategoryIds.vacuum, vacuum.category)
        finally
            if File.Exists path then File.Delete path

    [<Fact>]
    let ``0035 importMaterials fails with a typed JsonParseError on an unknown category name`` () =
        // A library JSON whose category name is absent from the seeded catalogue is a typed
        // `JsonParseError` at `dtoToEntry` (`tryFindCategoryByName` -> None), never a throw or a silent
        // default to some fallback category. The id is a valid Guid and the `description` field is present,
        // so deserialization reaches `dtoToEntry` and the CATEGORY branch is the one that fails.
        let path = Path.Combine(Path.GetTempPath(), $"""oc-cat-bad-%s{(Guid.NewGuid().ToString("N"))}.json""")
        let json = $"""[{{"id":"%s{Guid.NewGuid().ToString()}","name":"Test material","category":"NotAKnownCategory","description":"probe"}}]"""
        File.WriteAllText(path, json, System.Text.UTF8Encoding false)
        try
            match importMaterials path with
            | Error (JsonParseError msg) -> Assert.Contains("NotAKnownCategory", msg)
            | Error e -> Assert.Fail($"expected JsonParseError on an unknown category, got %A{e}")
            | Ok imported -> Assert.Fail($"expected a typed error, but import silently produced %d{List.length imported} entr(y/ies)")
        finally
            if File.Exists path then File.Delete path

    // --- AC-I11: appendRevision twice + diffRevisions (no external VCS) -------------

    let private filmThickness : float<meter> = 1.0e-7<meter>

    let private vacuumSystem : OpticalSystem =
        {
            description = Some "test stack"
            upper = OpticalProperties.vacuum
            films = [ { properties = OpticalProperties.vacuum; thickness = Thickness.Thickness filmThickness } ]
            substrate = None
            lower = OpticalProperties.vacuum
        }

    let private light = IncidentLightInfo.create (WaveLength.nm 600.0<nm>)

    let private node element children defaultUnit : BeamNode =
        { element = element; system = vacuumSystem; incident = light; children = children; defaultUnit = defaultUnit }

    let private sampleProject (defaultUnit : UnitOfMeasure) : OpticalConstructorProject =
        { beamTree = { root = node (Sample vacuumSystem) Map.empty defaultUnit }; systems = [ vacuumSystem ]; sources = []; placements = []; table = OpticalConstructor.Domain.Table.defaultTable }

    [<Fact>]
    let ``AC-I11 appendRevision twice yields two numbered snapshots and diffRevisions returns a ProjectDiff`` () =
        let folder = Path.Combine(Path.GetTempPath(), $"""oc-hist-%s{(Guid.NewGuid().ToString("N"))}""")
        Directory.CreateDirectory folder |> ignore
        try
            let p1 = sampleProject Nanometer
            let p2 = sampleProject Micrometer
            match appendRevision folder p1, appendRevision folder p2 with
            | Ok (), Ok () ->
                let snaps = Directory.GetFiles(Path.Combine(folder, "history"), "rev-*.json")
                Assert.Equal(2, snaps.Length)
                // diffRevisions is pure: identical systems/sources -> no diff items.
                let same = diffRevisions p1 p2
                Assert.True(List.isEmpty same.systems)
                Assert.True(List.isEmpty same.sources)
                // An added system surfaces as an Added system diff item by index identity.
                let p3 = { p2 with systems = vacuumSystem :: p2.systems }
                let d = diffRevisions p1 p3
                Assert.Contains(d.systems, fun (it : DiffItem) -> it.change = Added)
            | r1, r2 -> Assert.Fail($"appendRevision failed: %A{r1} / %A{r2}")
        finally
            if Directory.Exists folder then Directory.Delete(folder, true)
