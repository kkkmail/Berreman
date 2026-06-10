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
        let path = Path.Combine(Path.GetTempPath(), sprintf "oc-csv-%s.csv" (Guid.NewGuid().ToString("N")))
        try
            match Export.exportCsv path (headers, rows) with
            | Error e -> Assert.Fail(sprintf "%A" e)
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
        | Error e -> Assert.Fail(sprintf "%A" e)

    [<Fact>]
    let ``AC-I10 exportMaterials writes JSON that validates against the materialEntry $def`` () =
        // Built-in entries span categories and include None descriptions (the omit-null path).
        let entries = builtInEntries
        let path = Path.Combine(Path.GetTempPath(), sprintf "oc-mat-%s.json" (Guid.NewGuid().ToString("N")))
        try
            match exportMaterials path entries with
            | Error e -> Assert.Fail(sprintf "%A" e)
            | Ok () ->
                use doc = JsonDocument.Parse(File.ReadAllText path)
                Assert.Equal(JsonValueKind.Array, doc.RootElement.ValueKind)
                let mutable count = 0
                for el in doc.RootElement.EnumerateArray() do
                    count <- count + 1
                    match validateEntryElement el with
                    | Ok () -> ()
                    | Error e -> Assert.Fail(sprintf "entry failed materialEntry validation: %A" e)
                Assert.Equal(List.length entries, count)
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
        let folder = Path.Combine(Path.GetTempPath(), sprintf "oc-hist-%s" (Guid.NewGuid().ToString("N")))
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
            | r1, r2 -> Assert.Fail(sprintf "appendRevision failed: %A / %A" r1 r2)
        finally
            if Directory.Exists folder then Directory.Delete(folder, true)
