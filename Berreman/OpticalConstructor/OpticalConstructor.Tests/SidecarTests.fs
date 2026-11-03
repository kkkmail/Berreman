namespace OpticalConstructor.Tests

open System.IO
open Softellect.Sys.Primitives
open OpticalConstructor.Storage
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.Sidecar
open Xunit

/// `.binz` sidecar write/read for derived/bulk artefacts (AC-A6 / AC-I4 / AC-I5).
/// A representative parametric-sweep result table (the kind of derived output the
/// `Variables.calculate` producers yield) round-trips byte-for-value through the
/// zipped-FsPickler `.binz` seam; a missing sidecar surfaces `SidecarMissingErr`
/// rather than throwing. The canonical project never travels this path (binding
/// rule 4 — `writeSidecar` is never called with an `OpticalConstructorProject`).
module SidecarTests =

    /// A stand-in sweep table: (plot-x, scalar-result) pairs, as `calculate`
    /// produces (here a value-comparable shape so round-trip identity is exact).
    let private sweepTable : (float * float)[] =
        [| (0.0, 1.0); (0.25, 0.81); (0.5, 0.64); (0.75, 0.49); (1.0, 0.36) |]

    let private withTempFile (f : string -> unit) =
        let path = Path.Combine(Path.GetTempPath(), $"oc-sidecar-{System.Guid.NewGuid():N}{Sidecar.sidecarExtension.value}")
        try f path
        finally if File.Exists path then File.Delete path

    [<Fact>]
    let ``Sidecar.sidecarExtension is the Softellect .binz extension`` () =
        Assert.Equal(".binz", Sidecar.sidecarExtension.value)

    [<Fact>]
    let ``AC-A6 / AC-I4 a sweep table writes to a .binz file and reads back identical`` () =
        withTempFile (fun path ->
            match (FileName path).writeSidecar sweepTable with
            | Error e -> Assert.Fail($"writeSidecar failed: {e}")
            | Ok () ->
                Assert.True(File.Exists path)
                Assert.Equal(".binz", Path.GetExtension path)
                match (FileName path).readSidecar<(float * float)[]> () with
                | Ok back -> Assert.Equal<(float * float)[]>(sweepTable, back)
                | Error e -> Assert.Fail($"readSidecar failed: {e}"))

    [<Fact>]
    let ``AC-I5 a missing sidecar surfaces SidecarMissingErr rather than throwing`` () =
        let missing = FileName (Path.Combine(Path.GetTempPath(), $"oc-missing-{System.Guid.NewGuid():N}.binz"))
        match missing.readSidecar<(float * float)[]> () with
        | Error (SidecarMissingErr p) -> Assert.Equal(missing, p)
        | other -> Assert.Fail($"expected SidecarMissingErr, got {other}")
