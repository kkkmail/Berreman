namespace OpticalConstructor.Tests

open System
open System.IO
open Berreman.Constants
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.BeamTree
open OpticalConstructor.Domain.Project
open Softellect.Sys.Primitives
open OpticalConstructor.Storage
open OpticalConstructor.Storage.History
open OpticalConstructor.Storage.ProjectFile
open OpticalConstructor.Storage.Autosave
open Xunit

/// Undo/redo (§I.6, AC-I7) and autosave distinctness (§I.5, AC-I6). A destructive
/// model edit followed by `undo` restores the pre-edit project; `redo` restores the
/// edit; `undo` on empty history is a no-op. `writeAutosave` writes a distinct
/// `.ocproj.autosave` and leaves the committed `.ocproj` untouched.
module HistoryTests =

    let private filmThickness : float<meter> = 1.0e-7<meter>

    let private vacuumSystem : OpticalSystem =
        {
            description = Some (OpticalSystemDescription "test stack")
            upper = OpticalProperties.vacuum
            films = [ { properties = OpticalProperties.vacuum; thickness = Thickness.Thickness filmThickness } ]
            substrate = None
            lower = OpticalProperties.vacuum
        }

    let private light = IncidentLightInfo.create (WaveLength.nm 600.0<nm>)

    let private node element children defaultUnit : BeamNode =
        { element = element; system = vacuumSystem; incident = light; children = children; defaultUnit = defaultUnit }

    /// Two projects differing in the root `defaultUnit` — a destructive model edit.
    let private projectWith (defaultUnit : UnitOfMeasure) : OpticalConstructorProject =
        { beamTree = { root = node (SampleElement vacuumSystem) Map.empty defaultUnit }; systems = [ vacuumSystem ]; sources = []; placements = []; table = OpticalConstructor.Domain.Table.defaultTable }

    let private preEdit = projectWith Nanometer
    let private edited = projectWith Micrometer

    [<Fact>]
    let ``AC-I7 undo restores the pre-edit project and redo restores the edit`` () =
        let h1 = EditHistory.ofPresent preEdit |> push edited
        Assert.Equal<OpticalConstructorProject>(edited, h1.present)
        let undone = undo h1
        Assert.Equal<OpticalConstructorProject>(preEdit, undone.present)
        let redone = redo undone
        Assert.Equal<OpticalConstructorProject>(edited, redone.present)

    [<Fact>]
    let ``AC-I7 undo on empty history returns the input unchanged`` () =
        let h = EditHistory.ofPresent preEdit
        let undone = undo h
        Assert.Equal<OpticalConstructorProject>(preEdit, undone.present)
        Assert.True(List.isEmpty undone.past)
        Assert.True(List.isEmpty undone.future)
        // redo on empty future is likewise a no-op.
        let redone = redo h
        Assert.Equal<OpticalConstructorProject>(preEdit, redone.present)

    // --- R-1: RecentFiles.bump MRU invariant (pure, no IO) -------------------------

    [<Fact>]
    let ``R-1 bump dedups case-insensitively, pushes most-recent first, and truncates to maxRecent`` () =
        // Most-recent-first ordering: a freshly bumped path leads the list.
        let afterB = RecentFiles.bump (FileName @"C:\proj\b.ocproj") [ FileName @"C:\proj\a.ocproj" ]
        Assert.Equal<FileName list>([ FileName @"C:\proj\b.ocproj"; FileName @"C:\proj\a.ocproj" ], afterB)

        // Case-insensitive dedup: re-pushing an existing path (differing only in case)
        // removes the old occurrence and re-seats it at the front — no duplicate.
        let reBumped = RecentFiles.bump (FileName @"C:\PROJ\A.OCPROJ") afterB
        Assert.Equal<FileName list>([ FileName @"C:\PROJ\A.OCPROJ"; FileName @"C:\proj\b.ocproj" ], reBumped)

        // Truncation to maxRecent (16): bumping 20 distinct paths keeps only the 16
        // most-recent, with the very last bump at the head.
        let full =
            [ 1 .. 20 ]
            |> List.fold (fun acc i -> RecentFiles.bump (FileName $@"C:\proj\f%d{i}.ocproj") acc) []
        Assert.Equal(RecentFiles.maxRecent, List.length full)
        Assert.Equal(FileName @"C:\proj\f20.ocproj", List.head full)
        Assert.Equal(FileName @"C:\proj\f5.ocproj", List.last full)

    [<Fact>]
    let ``AC-I6 writeAutosave leaves the .ocproj untouched and writes a distinct .autosave`` () =
        let dir = Path.Combine(Path.GetTempPath(), $"""oc-autosave-%s{(Guid.NewGuid().ToString("N"))}""")
        Directory.CreateDirectory dir |> ignore
        try
            let ocproj = FileName (Path.Combine(dir, "sample.ocproj"))
            match ocproj.saveProject preEdit with
            | Error e -> Assert.Fail($"saveProject failed: %A{e}")
            | Ok () ->
                let before = File.ReadAllText ocproj.value
                // Autosave a DIFFERENT (edited) project; the .ocproj must not change.
                match ocproj.writeAutosave edited with
                | Error e -> Assert.Fail($"writeAutosave failed: %A{e}")
                | Ok () ->
                    let autosave = ocproj.autosavePath
                    Assert.True(File.Exists autosave.value, "autosave file exists")
                    Assert.NotEqual<FileName>(ocproj, autosave)
                    Assert.EndsWith(".ocproj.autosave", autosave.value)
                    Assert.Equal(before, File.ReadAllText ocproj.value)
        finally
            if Directory.Exists dir then Directory.Delete(dir, true)
