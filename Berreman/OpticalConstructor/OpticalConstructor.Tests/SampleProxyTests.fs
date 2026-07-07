namespace OpticalConstructor.Tests

open System
open Xunit
open OpticalConstructor.Domain.Library

/// Spec 0033 step 004 (ADD_CONTRACT STORE_XDUO_0002) pinned the mutating samples write-seam:
/// `SampleQuery` (name fragment + `SubstrateKind` facet), the `SampleError` channel
/// (UnknownSampleId / DuplicateSampleId / InvalidSample, each carrying `reason`), and the
/// `SampleProxy` functional-proxy record. Step 005 (IMPLEMENT_CONTRACT) put the REAL stateful
/// in-memory store behind that surface: `SampleProxy.createInMemory` closes over a
/// `ref Map<SampleId, Sample>` seeded from the samples in `seedEntries`, so writes PERSIST
/// inside the closure — every test therefore builds a FRESH proxy (a shared stateful one would
/// be order-dependent; xUnit guarantees no ordering). The round-trip tests use FIXED literal
/// Guids — deterministic across runs, no IO. A stub of the SAME record shape still exercises
/// all six functions through their exact signatures (the step-004 acceptance).
module SampleProxyTests =

    /// A fresh, isolated stateful proxy per test — mutation in one test can never leak into
    /// another.
    let private freshProxy () : SampleProxy = SampleProxy.createInMemory ()

    /// A seeded template sample (fixed id, thick glass plate) the write tests reuse.
    let private plate : Sample = SeedSamples.glassPlate1mm

    /// A fresh sample under a MINTED id (not in the seeded store) over the plate's structure.
    let private minted () : Sample =
        { plate with id = newSampleId (); name = "Test plate (minted)" }

    /// FIXED literal ids for the round-trip tests (spec 0033 step 005): never seeded, never
    /// minted — the round-trips stay deterministic across runs.
    let private fixedFreshId : SampleId = Guid.Parse "d4c1a1f0-5a2e-4d0b-9b3c-7f8e6a5d4c3b" |> SampleId
    let private fixedUnknownId : SampleId = Guid.Parse "0e9d8c7b-6a5f-4e3d-2c1b-0a9f8e7d6c5b" |> SampleId

    /// A fresh sample under the FIXED non-seeded id over the plate's structure.
    let private fixedFresh () : Sample =
        { plate with id = fixedFreshId; name = "Test plate (fixed id)" }

    // ============================ the read surface over the seeded store ============================

    [<Fact>]
    let ``listSamples returns the fixed seeded samples with distinct ids`` () =
        let proxy = freshProxy ()
        match proxy.listSamples () with
        | Ok samples ->
            Assert.NotEmpty samples
            Assert.Equal(List.length SeedSamples.all, List.length samples)
            let ids = samples |> List.map (fun s -> s.id)
            Assert.Equal(List.length ids, ids |> List.distinct |> List.length)
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``searchSamples with the empty query matches everything`` () =
        let proxy = freshProxy ()
        match proxy.searchSamples SampleQuery.empty with
        | Ok samples -> Assert.Equal(List.length SeedSamples.all, List.length samples)
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``searchSamples filters by name fragment case-insensitively`` () =
        let proxy = freshProxy ()
        match proxy.searchSamples { SampleQuery.empty with text = "GLASS" } with
        | Ok samples ->
            Assert.NotEmpty samples
            Assert.All(samples, fun s ->
                Assert.True(s.name.IndexOf("glass", StringComparison.OrdinalIgnoreCase) >= 0, s.name))
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``searchSamples filters by substrate kind`` () =
        let proxy = freshProxy ()
        match proxy.searchSamples { SampleQuery.empty with substrate = Some Plate } with
        | Ok plates ->
            Assert.NotEmpty plates
            Assert.All(plates, fun s -> Assert.Equal(Plate, s.substrate))
            Assert.Equal(
                SeedSamples.all |> List.filter (fun s -> s.substrate = Plate) |> List.length,
                List.length plates)
        | Error err -> Assert.Fail($"%A{err}")
        // No seeded sample is a wedge — the facet is total, the result just empty.
        match proxy.searchSamples { SampleQuery.empty with substrate = Some Wedge } with
        | Ok wedges -> Assert.Empty wedges
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``searchSamples composes the text and substrate facets`` () =
        let proxy = freshProxy ()
        // "langasite" matches only the dispersive langasite-on-silicon seed, a thin film.
        match proxy.searchSamples { text = "langasite"; substrate = Some ThinFilm } with
        | Ok [ only ] -> Assert.Equal<SampleId>(SeedSamples.langasiteSilicon.id, only.id)
        | other -> Assert.Fail($"expected exactly the langasite sample, got %A{other}")

    [<Fact>]
    let ``tryGetSample hits a known id and misses a minted one`` () =
        let proxy = freshProxy ()
        match proxy.tryGetSample SeedSamples.euvMoSi.id with
        | Ok (Some s) -> Assert.Equal<SampleId>(SeedSamples.euvMoSi.id, s.id)
        | other -> Assert.Fail($"expected the EUV Mo/Si sample, got %A{other}")
        match proxy.tryGetSample (newSampleId ()) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None, got %A{other}")

    // ============================ the write surface (persists inside the store) ============================

    [<Fact>]
    let ``addSample accepts a fresh minted sample`` () =
        let proxy = freshProxy ()
        match proxy.addSample (minted ()) with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``addSample rejects an id the library already holds with a diagnostic reason`` () =
        let proxy = freshProxy ()
        match proxy.addSample { minted () with id = plate.id } with
        | Error (DuplicateSampleId reason) -> Assert.Contains(string plate.id.value, reason)
        | other -> Assert.Fail($"expected Error (DuplicateSampleId _), got %A{other}")

    [<Fact>]
    let ``addSample rejects a blank name as InvalidSample`` () =
        let proxy = freshProxy ()
        match proxy.addSample { minted () with name = "   " } with
        | Error (InvalidSample reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidSample _), got %A{other}")

    [<Fact>]
    let ``updateSample accepts a known id and rejects an unknown one`` () =
        let proxy = freshProxy ()
        match proxy.updateSample { plate with description = "updated" } with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        let missing = minted ()
        match proxy.updateSample missing with
        | Error (UnknownSampleId reason) -> Assert.Contains(string missing.id.value, reason)
        | other -> Assert.Fail($"expected Error (UnknownSampleId _), got %A{other}")

    [<Fact>]
    let ``updateSample rejects a blank name as InvalidSample`` () =
        let proxy = freshProxy ()
        match proxy.updateSample { plate with name = "" } with
        | Error (InvalidSample reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidSample _), got %A{other}")

    [<Fact>]
    let ``removeSample accepts a known id and rejects an unknown one`` () =
        let proxy = freshProxy ()
        match proxy.removeSample plate.id with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        let missing = newSampleId ()
        match proxy.removeSample missing with
        | Error (UnknownSampleId reason) -> Assert.Contains(string missing.value, reason)
        | other -> Assert.Fail($"expected Error (UnknownSampleId _), got %A{other}")

    // ==================== the stateful round-trips (spec 0033 step 005, the acceptance) ====================

    [<Fact>]
    let ``addSample then listSamples returns the grown library including the added sample`` () =
        let proxy = freshProxy ()
        match proxy.addSample (fixedFresh ()) with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listSamples () with
        | Ok samples ->
            Assert.Equal(List.length SeedSamples.all + 1, List.length samples)
            Assert.True(samples |> List.exists (fun s -> s.id = fixedFreshId))
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``addSample twice under the same fixed id rejects the second add as a duplicate`` () =
        let proxy = freshProxy ()
        match proxy.addSample (fixedFresh ()) with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        // The duplicate is an id ADDED THIS RUN, not a seed — the store really grew.
        match proxy.addSample { fixedFresh () with name = "Test plate (fixed id, again)" } with
        | Error (DuplicateSampleId reason) -> Assert.Contains(string fixedFreshId.value, reason)
        | other -> Assert.Fail($"expected Error (DuplicateSampleId _), got %A{other}")

    [<Fact>]
    let ``updateSample then tryGetSample returns the updated sample`` () =
        let proxy = freshProxy ()
        let updated = { plate with name = "Glass plate (renamed)"; description = "updated in place" }
        match proxy.updateSample updated with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.tryGetSample plate.id with
        | Ok (Some s) ->
            Assert.Equal("Glass plate (renamed)", s.name)
            Assert.Equal("updated in place", s.description)
        | other -> Assert.Fail($"expected the updated sample, got %A{other}")

    [<Fact>]
    let ``updateSample under a fixed never-added id is rejected as UnknownSampleId`` () =
        let proxy = freshProxy ()
        match proxy.updateSample { plate with id = fixedUnknownId } with
        | Error (UnknownSampleId reason) -> Assert.Contains(string fixedUnknownId.value, reason)
        | other -> Assert.Fail($"expected Error (UnknownSampleId _), got %A{other}")

    [<Fact>]
    let ``removeSample then searchSamples no longer finds the removed sample`` () =
        let proxy = freshProxy ()
        let langasiteQuery = { SampleQuery.empty with text = "langasite" }
        match proxy.searchSamples langasiteQuery with
        | Ok [ hit ] -> Assert.Equal<SampleId>(SeedSamples.langasiteSilicon.id, hit.id)
        | other -> Assert.Fail($"expected exactly the langasite sample before removal, got %A{other}")
        match proxy.removeSample SeedSamples.langasiteSilicon.id with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.searchSamples langasiteQuery with
        | Ok hits -> Assert.Empty hits
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listSamples () with
        | Ok samples -> Assert.Equal(List.length SeedSamples.all - 1, List.length samples)
        | Error err -> Assert.Fail($"%A{err}")
        // The removed id is now unknown — the store really forgot it.
        match proxy.removeSample SeedSamples.langasiteSilicon.id with
        | Error (UnknownSampleId reason) -> Assert.Contains(string SeedSamples.langasiteSilicon.id.value, reason)
        | other -> Assert.Fail($"expected Error (UnknownSampleId _), got %A{other}")

    // ============================ the stub seam (the step-004 acceptance) ============================

    [<Fact>]
    let ``a SampleProxy compares by reference (the Elmish-required equality)`` () =
        let p = SampleProxy.createInMemory ()
        let same = p
        Assert.True((p = same))
        Assert.False((p = SampleProxy.createInMemory ()))

    [<Fact>]
    let ``a STUB SampleProxy over a fixed sample list exercises all six functions through their exact signatures`` () =
        // The stub proves the seam: the SAME record shape, in-test functions over a fixed
        // two-sample list — every SampleError case surfaces through it.
        let kept = plate
        let film = SeedSamples.glassFilm600
        let fixedSamples = [ kept; film ]
        let stub : SampleProxy =
            {
                listSamples = fun () -> Ok fixedSamples
                searchSamples =
                    fun (q : SampleQuery) ->
                        Ok (fixedSamples |> List.filter (fun s -> s.name.IndexOf(q.text, StringComparison.OrdinalIgnoreCase) >= 0))
                tryGetSample =
                    fun (id : SampleId) -> Ok (fixedSamples |> List.tryFind (fun s -> s.id = id))
                addSample =
                    fun (s : Sample) ->
                        if fixedSamples |> List.exists (fun x -> x.id = s.id)
                        then Error (DuplicateSampleId ($"sample id '%s{(string s.id.value)}' is already in the library"))
                        else Ok ()
                updateSample =
                    fun (s : Sample) ->
                        if String.IsNullOrWhiteSpace s.name
                        then Error (InvalidSample "blank sample name")
                        else Ok ()
                removeSample =
                    fun (id : SampleId) ->
                        if fixedSamples |> List.exists (fun x -> x.id = id)
                        then Ok ()
                        else Error (UnknownSampleId ($"unknown sample id '%s{(string id.value)}'"))
            }
        // listSamples : unit -> Result<Sample list, SampleError>
        match stub.listSamples () with
        | Ok l -> Assert.Equal(2, List.length l)
        | Error err -> Assert.Fail($"%A{err}")
        // searchSamples : SampleQuery -> Result<Sample list, SampleError>
        match stub.searchSamples { SampleQuery.empty with text = "film" } with
        | Ok [ hit ] -> Assert.Equal<SampleId>(film.id, hit.id)
        | other -> Assert.Fail($"expected exactly the thin-film sample, got %A{other}")
        // tryGetSample : SampleId -> Result<Sample option, SampleError>
        match stub.tryGetSample kept.id with
        | Ok (Some s) -> Assert.Equal<SampleId>(kept.id, s.id)
        | other -> Assert.Fail($"expected the kept sample, got %A{other}")
        match stub.tryGetSample (newSampleId ()) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None, got %A{other}")
        // addSample : Sample -> Result<unit, SampleError>
        match stub.addSample film with
        | Error (DuplicateSampleId reason) -> Assert.Contains(string film.id.value, reason)
        | other -> Assert.Fail($"expected Error (DuplicateSampleId _), got %A{other}")
        match stub.addSample (minted ()) with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok (), got %A{other}")
        // updateSample : Sample -> Result<unit, SampleError>
        match stub.updateSample { film with name = " " } with
        | Error (InvalidSample _) -> ()
        | other -> Assert.Fail($"expected Error (InvalidSample _), got %A{other}")
        match stub.updateSample film with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok (), got %A{other}")
        // removeSample : SampleId -> Result<unit, SampleError>
        let missing = newSampleId ()
        match stub.removeSample missing with
        | Error (UnknownSampleId reason) -> Assert.Contains(string missing.value, reason)
        | other -> Assert.Fail($"expected Error (UnknownSampleId _), got %A{other}")
        match stub.removeSample kept.id with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok (), got %A{other}")
