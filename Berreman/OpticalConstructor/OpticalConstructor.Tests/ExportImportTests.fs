namespace OpticalConstructor.Tests

open System
open System.IO
open System.Text.Json
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.ExperimentExport
open Softellect.Sys.Primitives
open OpticalConstructor.Storage
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.Export
open OpticalConstructor.Storage.MaterialLibrary
open Xunit

/// CSV export shape (§I.7, AC-I8), material import/export over the `materialEntry`
/// `$def` (§I.8, AC-I10).
module ExportImportTests =

    let private fixtures = Path.Combine(AppContext.BaseDirectory, "fixtures")

    [<Fact>]
    let ``a material entry outside the materialEntry definition is SchemaValidationErr`` () : unit =
        let materialId = MaterialIds.glass152
        let json = $"""{{"id":"{materialId.value}","name":"Glass","category":12}}"""
        use document = JsonDocument.Parse json
        match validateEntryElement document.RootElement with
        | Error (SchemaValidationErr messages) -> Assert.NotEmpty messages
        | Error error -> Assert.Fail($"expected structural validation error: {error.describe}")
        | Ok () -> Assert.Fail("accepted a material entry with a non-string category")

    // --- AC-I8: exportTableCsv writes header + one comma-separated line per row ----

    [<Fact>]
    let ``AC-I8 exportCsv writes one header line then one comma-separated line per row`` () =
        // The headers/rows are already-computed sweep tuples (no solver re-invocation).
        let table : ExportTable =
            {
                headers = [ ExportText "wavelength_nm"; ExportText "R"; ExportText "T" ]
                rows = [| [| 400.0; 0.10; 0.90 |]; [| 500.0; 0.20; 0.80 |] |]
            }
        let path = FileName (Path.Combine(Path.GetTempPath(), $"""oc-csv-%s{(Guid.NewGuid().ToString("N"))}.csv"""))
        try
            match Export.exportTableCsv path table with
            | Error e -> Assert.Fail($"%A{e}")
            | Ok () ->
                let lines = File.ReadAllLines path.value
                Assert.Equal(3, lines.Length)                  // one header + two rows
                Assert.Equal("wavelength_nm,R,T", lines.[0])
                Assert.Equal(3, lines.[1].Split(',').Length)   // one comma-separated value per column
                Assert.Equal(3, lines.[2].Split(',').Length)
        finally
            if File.Exists path.value then File.Delete path.value

    // --- AC-I10: material import (CSV via FSharp.Data) + export validates ----------

    [<Fact>]
    let ``AC-I10 importMaterials reads a refractiveindex.info-style CSV and returns MaterialEntry list`` () =
        let csvPath = FileName (Path.Combine(fixtures, "sample-nk.csv"))
        match csvPath.importMaterials (BuiltinCatalogue.categories ()) with
        | Ok entries -> Assert.False(List.isEmpty entries, "imported at least one entry")
        | Error e -> Assert.Fail($"%A{e}")

    [<Fact>]
    let ``AC-I10 exportMaterials writes JSON that validates against the materialEntry $def`` () =
        // BuiltinCatalogue.materials spans categories and includes None descriptions (the omit-null path).
        let entries = BuiltinCatalogue.materials ()
        let path = FileName (Path.Combine(Path.GetTempPath(), $"""oc-mat-%s{(Guid.NewGuid().ToString("N"))}.json"""))
        try
            match path.exportMaterials (BuiltinCatalogue.categories ()) entries with
            | Error e -> Assert.Fail($"%A{e}")
            | Ok () ->
                use doc = JsonDocument.Parse(File.ReadAllText path.value)
                Assert.Equal(JsonValueKind.Array, doc.RootElement.ValueKind)
                let mutable count = 0
                for el in doc.RootElement.EnumerateArray() do
                    count <- count + 1
                    match validateEntryElement el with
                    | Ok () -> ()
                    | Error e -> Assert.Fail($"entry failed materialEntry validation: %A{e}")
                Assert.Equal(List.length entries, count)
        finally
            if File.Exists path.value then File.Delete path.value

        let malformedEntries =
            [
                """{"name":"Glass","category":"Glass from the caller"}"""
                $"""{{"id":"{MaterialIds.glass152.value}","category":"Glass from the caller"}}"""
                $"""{{"id":"{MaterialIds.glass152.value}","name":"Glass"}}"""
                """{"id":12,"name":"Glass","category":"Glass from the caller"}"""
                $"""{{"id":"{MaterialIds.glass152.value}","name":12,"category":"Glass from the caller"}}"""
                $"""{{"id":"{MaterialIds.glass152.value}","name":"Glass","category":12}}"""
                $"""{{"id":"{MaterialIds.glass152.value}","name":"Glass","category":"Glass from the caller","description":null}}"""
                $"""{{"id":"{MaterialIds.glass152.value}","name":"Glass","category":"Glass from the caller","dispersion":{{"kind":"invalid","wavelengthUnit":"Nanometer"}}}}"""
            ]
        for json in malformedEntries do
            use doc = JsonDocument.Parse json
            match validateEntryElement doc.RootElement with
            | Error (SchemaValidationErr _) -> ()
            | Error error -> Assert.Fail($"expected structural validation error, got %A{error}")
            | Ok () -> Assert.Fail($"accepted malformed material entry: {json}")

        let glass = BuiltinCatalogue.materials () |> List.find (fun entry -> entry.id = MaterialIds.glass152)
        let path = FileName (Path.Combine(AppContext.BaseDirectory, $"""oc-missing-category-{Guid.NewGuid():N}.json"""))
        let original = "existing destination"
        File.WriteAllText(path.value, original)
        try
            match path.exportMaterials [] [ glass ] with
            | Error (SchemaValidationErr _) -> Assert.Equal(original, File.ReadAllText path.value)
            | Error error -> Assert.Fail($"expected category validation error, got %A{error}")
            | Ok () -> Assert.Fail("export accepted a category missing from the supplied list")
        finally
            if File.Exists path.value then File.Delete path.value

    // --- spec 0035 step 001: category survives the persistence seam by CategoryId ----
    // BuiltinCatalogue.categories is keyed by CategoryId; the library file carries the
    // display NAME, so `dtoToEntry` must resolve that name back to the matching id on import and reject
    // an unknown name as a typed error. These two facts pin the genuinely-new `tryFindCategoryByName`
    // seam at the `exportMaterials`/`importMaterials` (Report.fs) boundary — the round-trip and its
    // failure case.

    [<Fact>]
    let ``0035 category round-trips through export then import back to the matching CategoryId`` () =
        // Export BuiltinCatalogue.materials so `dtoToEntry` runs `tryFindCategoryByName`: a persisted
        // category NAME must resolve back to the SAME `CategoryId` the entry carried — no silent default,
        // no drift across the id<->name hop. Restrict to entries carrying a description (the pre-existing
        // DTO requires the `description` field present on read — see Gotchas); the described built-ins
        // still span BuiltinCatalogue.categories (Glass/Metal/Semiconductor/Crystal/Vacuum).
        let entries =
            BuiltinCatalogue.materials ()
            |> List.filter (fun e -> match e.description with | Some _ -> true | None -> false)
        let path = FileName (Path.Combine(Path.GetTempPath(), $"""oc-cat-%s{(Guid.NewGuid().ToString("N"))}.json"""))
        try
            match path.exportMaterials (BuiltinCatalogue.categories ()) entries with
            | Error e -> Assert.Fail($"export failed: %A{e}")
            | Ok () ->
                match path.importMaterials (BuiltinCatalogue.categories ()) with
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
                    let vacuum = imported |> List.find (fun e -> e.id = MaterialIds.vacuumMaterial)
                    Assert.Equal(CategoryIds.vacuumCategory, vacuum.category)
        finally
            if File.Exists path.value then File.Delete path.value

        let renamed = CategoryName "Glass from the caller"
        let categories =
            BuiltinCatalogue.categories ()
            |> List.map (fun category ->
                if category.id = CategoryIds.glass then
                    {
                        category with
                            name = renamed
                    }
                else category)
        let glass = BuiltinCatalogue.materials () |> List.find (fun entry -> entry.id = MaterialIds.glass152)
        let describedGlass =
            {
                glass with
                    description = Some (EntryDescription "Renamed category round-trip")
            }
        let path = FileName (Path.Combine(AppContext.BaseDirectory, $"""oc-live-category-{Guid.NewGuid():N}.json"""))
        try
            match path.exportMaterials categories [ describedGlass ] with
            | Error error -> Assert.Fail($"export failed: %A{error}")
            | Ok () ->
                use doc = JsonDocument.Parse(File.ReadAllText path.value)
                Assert.Equal(1, doc.RootElement.GetArrayLength())
                let exported = doc.RootElement.[0]
                Assert.Equal(renamed.value, exported.GetProperty("category").GetString())
                match validateEntryElement exported with
                | Error error -> Assert.Fail($"entry failed structural validation: %A{error}")
                | Ok () -> ()
                match path.importMaterials categories with
                | Error error -> Assert.Fail($"import failed: %A{error}")
                | Ok [ imported ] ->
                    Assert.Equal(describedGlass.id, imported.id)
                    Assert.Equal(CategoryIds.glass, imported.category)
                    Assert.Equal(describedGlass.description, imported.description)
                | Ok imported -> Assert.Fail($"expected one entry, got {List.length imported}")
        finally
            if File.Exists path.value then File.Delete path.value

    [<Fact>]
    let ``0035 importMaterials fails with a typed MalformedJsonErr on an unknown category name`` () =
        // A JSON category name absent from BuiltinCatalogue.categories is a typed
        // `MalformedJsonErr` at `dtoToEntry` (`tryFindCategoryByName` -> None), never a throw or a silent
        // default to some fallback category. The id is a valid Guid and the `description` field is present,
        // so deserialization reaches `dtoToEntry` and the CATEGORY branch is the one that fails.
        let path = FileName (Path.Combine(Path.GetTempPath(), $"""oc-cat-bad-%s{(Guid.NewGuid().ToString("N"))}.json"""))
        let json = $"""[{{"id":"%s{Guid.NewGuid().ToString()}","name":"Test material","category":"NotAKnownCategory","description":"probe"}}]"""
        File.WriteAllText(path.value, json, System.Text.UTF8Encoding false)
        try
            match path.importMaterials (BuiltinCatalogue.categories ()) with
            | Error (MalformedJsonErr msg) -> Assert.Contains("NotAKnownCategory", msg)
            | Error e -> Assert.Fail($"expected MalformedJsonErr on an unknown category, got %A{e}")
            | Ok imported -> Assert.Fail($"expected a typed error, but import silently produced %d{List.length imported} entr(y/ies)")
        finally
            if File.Exists path.value then File.Delete path.value

        let path = FileName (Path.Combine(AppContext.BaseDirectory, $"""oc-no-category-fallback-{Guid.NewGuid():N}.json"""))
        let json = $"""[{{"id":"{MaterialIds.glass152.value}","name":"Glass","category":"Glass","description":"probe"}}]"""
        File.WriteAllText(path.value, json)
        try
            match path.importMaterials [] with
            | Error (MalformedJsonErr reason) -> Assert.Contains("Glass", reason)
            | Error error -> Assert.Fail($"expected unknown-category error, got %A{error}")
            | Ok _ -> Assert.Fail("import fell back to a category absent from the supplied list")
        finally
            if File.Exists path.value then File.Delete path.value

    [<Fact>]
    let ``a metadata-only library file re-imports each entry as vacuum data`` () =
        // Spec 0050 step 11: the file carries no physics, so `dtoToEntry` states vacuum as term data
        // (`vacuumPlaceholderComplexity`), and `properties` is derived from it. Filtered as the 0035
        // round-trip filters, because the DTO requires the `description` field on read.
        let entries =
            BuiltinCatalogue.materials ()
            |> List.filter (fun e -> match e.description with | Some _ -> true | None -> false)
        let path = FileName (Path.Combine(AppContext.BaseDirectory, $"""oc-vacuum-%s{(Guid.NewGuid().ToString("N"))}.json"""))
        try
            match path.exportMaterials (BuiltinCatalogue.categories ()) entries with
            | Error e -> Assert.Fail($"export failed: %A{e}")
            | Ok () ->
                match path.importMaterials (BuiltinCatalogue.categories ()) with
                | Error e -> Assert.Fail($"import failed: %A{e}")
                | Ok imported ->
                    Assert.Equal(List.length entries, List.length imported)
                    for e in imported do
                        match e.physics with
                        | ComplexityPhysics complexity -> Assert.Equal(vacuumPlaceholderComplexity, complexity)
        finally
            if File.Exists path.value then File.Delete path.value
