namespace OpticalConstructor.Tests

open Xunit
open Berreman.Constants
open Berreman.Fields
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.SampleStore

/// Spec 0046 — the Library's light sources: the seeded lasers and UV / EUV lithography sources, the
/// user-sources store behind the source editor, and the LIVE Library that composes the seeded presets
/// with the samples and user-sources stores so an entry created in the Library window is offered for
/// its kind and resolves once a table element is bound to it.
module SourceLibraryTests =

    let private nm (w : WaveLength) : float = w.value / nmToMeter / 1.0<nm>

    let private userSource (id : string) (name : string) (wavelengthNm : float) : SourcePreset =
        { id = id; name = name; waveLength = WaveLength.nm (wavelengthNm * 1.0<nm>); protection = UserManaged }

    let private sourceIds (entries : LibraryEntry list) : string list =
        entries
        |> List.choose (fun e ->
            match e with
            | SourceItem s -> Some s.id
            | SampleItem _ | DetectorItem _ | PolarizerItem _ -> None)

    // ------------------------------ the seeded sources ------------------------------

    [<Fact>]
    let ``the seeded sources are protected, uniquely identified, and cover the lasers and the lithography lines`` () =
        let all = SeedSources.all
        Assert.Equal(List.length all, all |> List.map (fun s -> s.id) |> List.distinct |> List.length)
        Assert.All(all, fun s -> Assert.Equal<EntryProtection>(ProtectedBuiltIn, s.protection))
        Assert.All(all, fun s -> Assert.True(nm s.waveLength > 0.0, $"%s{s.name} has a positive wavelength"))
        let wavelengthOf (id : string) : float = all |> List.find (fun s -> s.id = id) |> fun s -> nm s.waveLength
        Assert.True(abs (wavelengthOf "src-600" - 600.0) < 1.0e-9, "the reference source stays")
        Assert.True(abs (wavelengthOf "src-hene-633" - 632.8) < 1.0e-9)
        Assert.True(abs (wavelengthOf "src-ndyag-1064" - 1064.0) < 1.0e-9)
        Assert.True(abs (wavelengthOf "src-hg-i-365" - 365.0) < 1.0e-9)
        Assert.True(abs (wavelengthOf "src-krf-248" - 248.4) < 1.0e-9)
        Assert.True(abs (wavelengthOf "src-arf-193" - 193.4) < 1.0e-9)
        Assert.True(abs (wavelengthOf "src-euv-13p5" - 13.5) < 1.0e-9)
        // Every seeded source is a Library entry for the light-source kind.
        match (createInMemory ()).entriesForKind LightSource with
        | Ok entries -> Assert.Equal<string list>(all |> List.map (fun s -> s.id), sourceIds entries)
        | Error e -> Assert.Fail($"%A{e}")

    [<Fact>]
    let ``the seeded tree lists every seeded source under Sources`` () =
        let rec leaves (node : LibraryTreeNode) : string list =
            match node with
            | Leaf (_, id) -> [ id ]
            | Group (_, children) -> children |> List.collect leaves
        let sourcesGroup =
            match (createInMemory ()).libraryTrees () with
            | Ok (tree :: _) ->
                match tree.root with
                | Group (_, groups) -> groups |> List.tryPick (fun g -> match g with Group (TreeLabel "Sources", _) -> Some g | _ -> None)
                | Leaf _ -> None
            | Ok [] | Error _ -> None
        match sourcesGroup with
        | Some group -> Assert.Equal<string list>(SeedSources.all |> List.map (fun s -> s.id), leaves group)
        | None -> Assert.Fail "the seeded tree must carry a Sources group"

    // ------------------------------ the user-sources store ------------------------------

    [<Fact>]
    let ``the user-sources store inserts, replaces by id, and removes`` () =
        let store = SourceProxy.createInMemory ()
        Assert.Equal(Ok [], store.listSources ())
        let euv = userSource "src-user-1" "My EUV line" 13.4
        Assert.Equal(Ok (), store.saveSource euv)
        Assert.Equal(Ok (Some euv), store.tryGetSource "src-user-1")
        let renamed = { euv with name = "EUV 13.4 nm" }
        Assert.Equal(Ok (), store.saveSource renamed)
        Assert.Equal(Ok [ renamed ], store.listSources ())
        Assert.Equal(Ok (), store.removeSource "src-user-1")
        Assert.Equal(Ok None, store.tryGetSource "src-user-1")
        match store.removeSource "src-user-1" with
        | Error (UnknownSourceId _) -> ()
        | other -> Assert.Fail($"expected UnknownSourceId, got %A{other}")

    [<Fact>]
    let ``the user-sources store refuses a blank name, a non-positive wavelength, and a protected source`` () =
        let store = SourceProxy.createInMemory ()
        match store.saveSource (userSource "src-a" "   " 500.0) with
        | Error (InvalidSource _) -> ()
        | other -> Assert.Fail($"blank name: %A{other}")
        match store.saveSource (userSource "src-b" "Zero" 0.0) with
        | Error (InvalidSource _) -> ()
        | other -> Assert.Fail($"zero wavelength: %A{other}")
        match store.saveSource { userSource "src-c" "Protected" 500.0 with protection = ProtectedBuiltIn } with
        | Error (ProtectedSourceRefused _) -> ()
        | other -> Assert.Fail($"protected: %A{other}")
        Assert.Equal(Ok [], store.listSources ())

    // ------------------------------ the live Library ------------------------------

    [<Fact>]
    let ``the live Library offers and resolves a user source after the seeded ones`` () =
        let samples = SampleProxy.createInMemory VersionsInUse.empty
        let sources = SourceProxy.createInMemory ()
        let live = liveLibrary (createInMemory ()) samples sources
        let mine = userSource "src-user-krf" "My KrF line" 248.0
        Assert.Equal(Ok (), sources.saveSource mine)
        match live.entriesForKind LightSource with
        | Ok entries -> Assert.Equal<string list>((SeedSources.all |> List.map (fun s -> s.id)) @ [ mine.id ], sourceIds entries)
        | Error e -> Assert.Fail($"%A{e}")
        Assert.Equal(Ok (Some (SourceItem mine)), live.tryGetEntry mine.id)
        // The seeded presets still resolve; a removed user source no longer does.
        Assert.Equal(Ok (Some (DetectorItem { id = "det-intensity"; name = "Intensity detector"; kind = Intensity; protection = ProtectedBuiltIn })), live.tryGetEntry "det-intensity")
        Assert.Equal(Ok (), sources.removeSource mine.id)
        Assert.Equal(Ok None, live.tryGetEntry mine.id)

    [<Fact>]
    let ``the live Library resolves a sample created in the samples store`` () =
        let samples = SampleProxy.createInMemory VersionsInUse.empty
        let live = liveLibrary (createInMemory ()) samples (SourceProxy.createInMemory ())
        let created = { SeedSamples.glassFilm200 with id = newSampleId (); name = "My film" }
        Assert.Equal(Ok (), samples.saveSample created)
        let entryId = (SampleItem created).entryId
        match live.tryGetEntry entryId with
        | Ok (Some (SampleItem s)) -> Assert.Equal("My film", s.name)
        | other -> Assert.Fail($"the created sample must resolve, got %A{other}")
        match live.entriesForKind Sample with
        | Ok entries -> Assert.Contains(entryId, entries |> List.map (fun e -> e.entryId))
        | Error e -> Assert.Fail($"%A{e}")
        // The read-only seeded Library alone never sees it (the gap the live Library closes).
        Assert.Equal(Ok None, (createInMemory ()).tryGetEntry entryId)
