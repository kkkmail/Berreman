namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain.MaterialLibrary   // MaterialCategory / MaterialEntry + standardCategories / builtInEntries
open OpticalConstructor.Domain.Library           // Sample / LibraryEntry + SeedSamples / seedEntries
open OpticalConstructor.Seeding.Seeding           // SeedingProxy / SeedingError / seedAll

/// Spec 0038 step 041 (ADD_CONTRACT STORE_XDUO_0008): the seed-push seam — the DECLARED
/// `[<ReferenceEquality>] SeedingProxy` (`saveCategory` / `saveMaterial` / `saveSample` /
/// `saveLibraryEntry`, each `<X> -> Result<unit, SeedingError>`) and the pure `seedAll` orchestrator.
/// This file supplies the recording STUB and the mock-driven test that proves `seedAll` pushes every
/// EXISTING Domain seed value EXACTLY ONCE, in category → material → sample → entry order, through the
/// record's EXACT signatures. It also pins the reference equality and the `SeedRejected` short-circuit.
/// The real store lands in a later IMPLEMENT_CONTRACT.
module SeedingProxyTests =

    /// Which channel a push arrived on, recorded on a global call-order tape so the test proves the
    /// category → material → sample → entry ordering ACROSS the four channels (not merely within each
    /// list). A simple DU (structural equality) so the whole tape compares in one assertion.
    type private Channel =
        | CategoryChannel
        | MaterialChannel
        | SampleChannel
        | EntryChannel

    /// The recording stub's ledger: the global call-order tape plus one per-channel list of the exact
    /// values received. Values are recorded by reference (the lists hold the pushed instances), so the
    /// test can pin that the SAME frozen seed instances flowed through — not merely value-equal copies
    /// (the engine payloads on `MaterialEntry` / `LibraryEntry` carry no structural equality anyway).
    type private Recorder =
        {
            order : ResizeArray<Channel>
            categories : ResizeArray<MaterialCategory>
            materials : ResizeArray<MaterialEntry>
            samples : ResizeArray<Sample>
            entries : ResizeArray<LibraryEntry>
        }

        static member create () : Recorder =
            {
                order = ResizeArray<Channel>()
                categories = ResizeArray<MaterialCategory>()
                materials = ResizeArray<MaterialEntry>()
                samples = ResizeArray<Sample>()
                entries = ResizeArray<LibraryEntry>()
            }

    /// The recording `SeedingProxy`: every save appends its channel tag + the received value and
    /// returns `Ok ()`. Annotated `: SeedingProxy` so the compiler checks each lambda against the
    /// EXACT declared field signature — the "through the exact signatures" acceptance is enforced at
    /// compile time, not merely asserted.
    let private recordingProxy (r : Recorder) : SeedingProxy =
        {
            saveCategory = fun c -> r.order.Add CategoryChannel; r.categories.Add c; Ok ()
            saveMaterial = fun m -> r.order.Add MaterialChannel; r.materials.Add m; Ok ()
            saveSample = fun s -> r.order.Add SampleChannel; r.samples.Add s; Ok ()
            saveLibraryEntry = fun e -> r.order.Add EntryChannel; r.entries.Add e; Ok ()
        }

    /// Assert a recorded channel received EXACTLY the seed list: same length (nothing pushed twice or
    /// dropped) and the SAME instance at each index (the exact seed value flowed through, in order).
    /// Reference identity via `Object.ReferenceEquals` — the seeds are module-level `let` values
    /// (computed once), so the recorded reference is the seed list's own element.
    let private assertPushedInOrder (name : string) (expected : 'Item list) (actual : ResizeArray<'Item>) : unit =
        Assert.Equal(List.length expected, actual.Count)
        expected
        |> List.iteri (fun i e ->
            Assert.True(
                System.Object.ReferenceEquals(box e, box actual.[i]),
                $"{name}[{i}]: seedAll pushed a different instance than the seed value"))

    // ============================ the seedAll acceptance ============================

    [<Fact>]
    let ``seedAll pushes every seed value exactly once, in category-material-sample-entry order through the exact signatures`` () =
        let r = Recorder.create ()
        // Bind seedAll to an explicitly-typed local: the file fails to compile if seedAll drifts from
        // its declared `SeedingProxy -> Result<unit, SeedingError>` shape.
        let runSeed : SeedingProxy -> Result<unit, SeedingError> = seedAll

        match runSeed (recordingProxy r) with
        | Ok () -> ()
        | Error e -> Assert.Fail($"seedAll over an all-accepting stub should be Ok (), got %A{e}")

        // Each channel received EXACTLY its seed list — same count, same instances, in order.
        assertPushedInOrder "categories" standardCategories r.categories
        assertPushedInOrder "materials" builtInEntries r.materials
        assertPushedInOrder "samples" SeedSamples.all r.samples
        assertPushedInOrder "entries" seedEntries r.entries

        // The global tape proves the cross-channel order: all categories, then all materials, then all
        // samples, then all entries (and, by the counts, no interleaving and no extra push).
        let expectedOrder =
            [
                yield! List.replicate (List.length standardCategories) CategoryChannel
                yield! List.replicate (List.length builtInEntries) MaterialChannel
                yield! List.replicate (List.length SeedSamples.all) SampleChannel
                yield! List.replicate (List.length seedEntries) EntryChannel
            ]
        Assert.Equal<Channel list>(expectedOrder, List.ofSeq r.order)

    [<Fact>]
    let ``seedAll stops at the first rejected seed and returns that SeedRejected`` () =
        // A stub that accepts every category but rejects the FIRST material. seedAll must return the
        // rejection and push NO samples / entries (the material group short-circuits the chain).
        let r = Recorder.create ()
        let proxy : SeedingProxy =
            {
                saveCategory = fun c -> r.categories.Add c; Ok ()
                saveMaterial = fun _ -> Error (SeedRejected "the stub rejects every material")
                saveSample = fun s -> r.samples.Add s; Ok ()
                saveLibraryEntry = fun e -> r.entries.Add e; Ok ()
            }

        match seedAll proxy with
        | Error (SeedRejected reason) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | Ok () -> Assert.Fail("seedAll should surface the material rejection, not Ok ()")

        // Categories all pushed (they precede materials); no sample / entry reached after the reject.
        Assert.Equal(List.length standardCategories, r.categories.Count)
        Assert.Equal(0, r.samples.Count)
        Assert.Equal(0, r.entries.Count)

    [<Fact>]
    let ``a SeedingProxy compares by reference (the [<ReferenceEquality>] contract)`` () =
        // Function-valued fields have no structural equality; the [<ReferenceEquality>] proxy compares
        // by identity so a host holding one stays comparable.
        let make () : SeedingProxy = recordingProxy (Recorder.create ())
        let p = make ()
        let same = p
        Assert.True((p = same))
        Assert.False((p = make ()))
