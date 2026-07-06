namespace OpticalConstructor.Tests

open System
open Xunit
open OpticalConstructor.Domain.Library

/// Spec 0033 step 004 (ADD_CONTRACT STORE_XDUO_0002) — pins the mutating samples write-seam:
/// `SampleQuery` (name fragment + `SubstrateKind` facet), the `SampleError` channel
/// (UnknownSampleId / DuplicateSampleId / InvalidSample, each carrying `reason`), and the
/// `SampleProxy` functional-proxy record. The proxy is the in-memory mock
/// (`createInMemorySampleProxy`, an inline stub record over the FIXED `SeedSamples.all` list —
/// validates, never persists); a stub of the SAME record shape exercises all six functions
/// through their exact signatures (the acceptance).
module SampleProxyTests =

    let private proxy : SampleProxy = createInMemorySampleProxy ()

    /// A seeded template sample (fixed id, thick glass plate) the write tests reuse.
    let private plate : Sample = SeedSamples.glassPlate1mm

    /// A fresh sample under a MINTED id (not in the fixed list) over the plate's structure.
    let private minted () : Sample =
        { plate with id = newSampleId (); name = "Test plate (minted)" }

    // ============================ the read surface over the fixed list ============================

    [<Fact>]
    let ``listSamples returns the fixed seeded samples with distinct ids`` () =
        match proxy.listSamples () with
        | Ok samples ->
            Assert.NotEmpty samples
            Assert.Equal(List.length SeedSamples.all, List.length samples)
            let ids = samples |> List.map (fun s -> s.id)
            Assert.Equal(List.length ids, ids |> List.distinct |> List.length)
        | Error err -> Assert.Fail(sprintf "%A" err)

    [<Fact>]
    let ``searchSamples with the empty query matches everything`` () =
        match proxy.searchSamples SampleQuery.empty with
        | Ok samples -> Assert.Equal(List.length SeedSamples.all, List.length samples)
        | Error err -> Assert.Fail(sprintf "%A" err)

    [<Fact>]
    let ``searchSamples filters by name fragment case-insensitively`` () =
        match proxy.searchSamples { SampleQuery.empty with text = "GLASS" } with
        | Ok samples ->
            Assert.NotEmpty samples
            Assert.All(samples, fun s ->
                Assert.True(s.name.IndexOf("glass", StringComparison.OrdinalIgnoreCase) >= 0, s.name))
        | Error err -> Assert.Fail(sprintf "%A" err)

    [<Fact>]
    let ``searchSamples filters by substrate kind`` () =
        match proxy.searchSamples { SampleQuery.empty with substrate = Some Plate } with
        | Ok plates ->
            Assert.NotEmpty plates
            Assert.All(plates, fun s -> Assert.Equal(Plate, s.substrate))
            Assert.Equal(
                SeedSamples.all |> List.filter (fun s -> s.substrate = Plate) |> List.length,
                List.length plates)
        | Error err -> Assert.Fail(sprintf "%A" err)
        // No seeded sample is a wedge — the facet is total, the result just empty.
        match proxy.searchSamples { SampleQuery.empty with substrate = Some Wedge } with
        | Ok wedges -> Assert.Empty wedges
        | Error err -> Assert.Fail(sprintf "%A" err)

    [<Fact>]
    let ``searchSamples composes the text and substrate facets`` () =
        // "langasite" matches only the dispersive langasite-on-silicon seed, a thin film.
        match proxy.searchSamples { text = "langasite"; substrate = Some ThinFilm } with
        | Ok [ only ] -> Assert.Equal<SampleId>(SeedSamples.langasiteSilicon.id, only.id)
        | other -> Assert.Fail(sprintf "expected exactly the langasite sample, got %A" other)

    [<Fact>]
    let ``tryGetSample hits a known id and misses a minted one`` () =
        match proxy.tryGetSample SeedSamples.euvMoSi.id with
        | Ok (Some s) -> Assert.Equal<SampleId>(SeedSamples.euvMoSi.id, s.id)
        | other -> Assert.Fail(sprintf "expected the EUV Mo/Si sample, got %A" other)
        match proxy.tryGetSample (newSampleId ()) with
        | Ok None -> ()
        | other -> Assert.Fail(sprintf "expected Ok None, got %A" other)

    // ============================ the write surface (validates, never persists) ============================

    [<Fact>]
    let ``addSample accepts a fresh minted sample`` () =
        match proxy.addSample (minted ()) with
        | Ok () -> ()
        | Error err -> Assert.Fail(sprintf "%A" err)

    [<Fact>]
    let ``addSample rejects an id the library already holds with a diagnostic reason`` () =
        match proxy.addSample { minted () with id = plate.id } with
        | Error (DuplicateSampleId reason) -> Assert.Contains(string plate.id.value, reason)
        | other -> Assert.Fail(sprintf "expected Error (DuplicateSampleId _), got %A" other)

    [<Fact>]
    let ``addSample rejects a blank name as InvalidSample`` () =
        match proxy.addSample { minted () with name = "   " } with
        | Error (InvalidSample reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail(sprintf "expected Error (InvalidSample _), got %A" other)

    [<Fact>]
    let ``updateSample accepts a known id and rejects an unknown one`` () =
        match proxy.updateSample { plate with description = "updated" } with
        | Ok () -> ()
        | Error err -> Assert.Fail(sprintf "%A" err)
        let missing = minted ()
        match proxy.updateSample missing with
        | Error (UnknownSampleId reason) -> Assert.Contains(string missing.id.value, reason)
        | other -> Assert.Fail(sprintf "expected Error (UnknownSampleId _), got %A" other)

    [<Fact>]
    let ``updateSample rejects a blank name as InvalidSample`` () =
        match proxy.updateSample { plate with name = "" } with
        | Error (InvalidSample reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail(sprintf "expected Error (InvalidSample _), got %A" other)

    [<Fact>]
    let ``removeSample accepts a known id and rejects an unknown one`` () =
        match proxy.removeSample plate.id with
        | Ok () -> ()
        | Error err -> Assert.Fail(sprintf "%A" err)
        let missing = newSampleId ()
        match proxy.removeSample missing with
        | Error (UnknownSampleId reason) -> Assert.Contains(string missing.value, reason)
        | other -> Assert.Fail(sprintf "expected Error (UnknownSampleId _), got %A" other)

    // ============================ the stub seam (the acceptance) ============================

    [<Fact>]
    let ``a SampleProxy compares by reference (the Elmish-required equality)`` () =
        let p = createInMemorySampleProxy ()
        let same = p
        Assert.True((p = same))
        Assert.False((p = createInMemorySampleProxy ()))

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
                        then Error (DuplicateSampleId (sprintf "sample id '%s' is already in the library" (string s.id.value)))
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
                        else Error (UnknownSampleId (sprintf "unknown sample id '%s'" (string id.value)))
            }
        // listSamples : unit -> Result<Sample list, SampleError>
        match stub.listSamples () with
        | Ok l -> Assert.Equal(2, List.length l)
        | Error err -> Assert.Fail(sprintf "%A" err)
        // searchSamples : SampleQuery -> Result<Sample list, SampleError>
        match stub.searchSamples { SampleQuery.empty with text = "film" } with
        | Ok [ hit ] -> Assert.Equal<SampleId>(film.id, hit.id)
        | other -> Assert.Fail(sprintf "expected exactly the thin-film sample, got %A" other)
        // tryGetSample : SampleId -> Result<Sample option, SampleError>
        match stub.tryGetSample kept.id with
        | Ok (Some s) -> Assert.Equal<SampleId>(kept.id, s.id)
        | other -> Assert.Fail(sprintf "expected the kept sample, got %A" other)
        match stub.tryGetSample (newSampleId ()) with
        | Ok None -> ()
        | other -> Assert.Fail(sprintf "expected Ok None, got %A" other)
        // addSample : Sample -> Result<unit, SampleError>
        match stub.addSample film with
        | Error (DuplicateSampleId reason) -> Assert.Contains(string film.id.value, reason)
        | other -> Assert.Fail(sprintf "expected Error (DuplicateSampleId _), got %A" other)
        match stub.addSample (minted ()) with
        | Ok () -> ()
        | other -> Assert.Fail(sprintf "expected Ok (), got %A" other)
        // updateSample : Sample -> Result<unit, SampleError>
        match stub.updateSample { film with name = " " } with
        | Error (InvalidSample _) -> ()
        | other -> Assert.Fail(sprintf "expected Error (InvalidSample _), got %A" other)
        match stub.updateSample film with
        | Ok () -> ()
        | other -> Assert.Fail(sprintf "expected Ok (), got %A" other)
        // removeSample : SampleId -> Result<unit, SampleError>
        let missing = newSampleId ()
        match stub.removeSample missing with
        | Error (UnknownSampleId reason) -> Assert.Contains(string missing.value, reason)
        | other -> Assert.Fail(sprintf "expected Error (UnknownSampleId _), got %A" other)
        match stub.removeSample kept.id with
        | Ok () -> ()
        | other -> Assert.Fail(sprintf "expected Ok (), got %A" other)
