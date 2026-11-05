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
        { id = ElementValueId id; name = MaterialLibrary.EntryName name; waveLength = WaveLength.nm (wavelengthNm * 1.0<nm>); tuning = FixedLine; protection = UserManaged }

    let private sourceIds (entries : LibraryEntry list) : string list =
        entries
        |> List.choose (fun e ->
            match e with
            | SourceItem s -> Some s.id.value
            | SampleItem _ | DetectorItem _ | PolarizerItem _ -> None)

    // ------------------------------ the seeded sources ------------------------------

    [<Fact>]
    let ``the seeded sources are protected, uniquely identified, and cover the lasers and the lithography lines`` () =
        let all = SeedSources.allSeedSources
        Assert.Equal(List.length all, all |> List.map (fun s -> s.id) |> List.distinct |> List.length)
        Assert.All(all, fun s -> Assert.Equal<EntryProtection>(ProtectedBuiltIn, s.protection))
        Assert.All(all, fun s -> Assert.True(nm s.waveLength > 0.0, $"%s{s.name.value} has a positive wavelength"))
        let wavelengthOf (id : ElementValueId) : float = all |> List.find (fun s -> s.id = id) |> fun s -> nm s.waveLength
        Assert.True(abs (wavelengthOf PresetIds.source600 - 600.0) < 1.0e-9, "the reference source stays")
        Assert.True(abs (wavelengthOf PresetIds.heNe633 - 632.8) < 1.0e-9)
        Assert.True(abs (wavelengthOf PresetIds.ndYag1064 - 1064.0) < 1.0e-9)
        Assert.True(abs (wavelengthOf PresetIds.mercuryI365 - 365.0) < 1.0e-9)
        Assert.True(abs (wavelengthOf PresetIds.krf248 - 248.4) < 1.0e-9)
        Assert.True(abs (wavelengthOf PresetIds.arf193 - 193.4) < 1.0e-9)
        Assert.True(abs (wavelengthOf PresetIds.euv13p5 - 13.5) < 1.0e-9)
        // Every seeded source is a Library entry for the light-source kind.
        match (createInMemory ()).entriesForKind LightSource with
        | Ok entries -> Assert.Equal<string list>(all |> List.map (fun s -> s.id.value), sourceIds entries)
        | Error e -> Assert.Fail($"%A{e}")

    [<Fact>]
    let ``the seeded tree lists every seeded source under Sources`` () =
        let rec leaves (node : LibraryTreeNode) : string list =
            match node with
            | Leaf (_, id) -> [ id.value ]
            | Group (_, children) -> children |> List.collect leaves
        let sourcesGroup =
            match (createInMemory ()).libraryTrees () with
            | Ok (tree :: _) ->
                match tree.root with
                | Group (_, groups) -> groups |> List.tryPick (fun g -> match g with Group (TreeLabel "Sources", _) -> Some g | _ -> None)
                | Leaf _ -> None
            | Ok [] | Error _ -> None
        match sourcesGroup with
        | Some group -> Assert.Equal<string list>(SeedSources.allSeedSources |> List.map (fun s -> s.id.value), leaves group)
        | None -> Assert.Fail "the seeded tree must carry a Sources group"

    // ------------------------------ the user-sources store ------------------------------

    [<Fact>]
    let ``the user-sources store inserts, replaces by id, and removes`` () =
        let store = SourceProxy.createInMemory ()
        Assert.Equal(Ok [], store.listSources ())
        let euv = userSource "src-user-1" "My EUV line" 13.4
        Assert.Equal(Ok (), store.saveSource euv)
        Assert.Equal(Ok (Some euv), store.tryGetSource (ElementValueId "src-user-1"))
        let renamed = { euv with name = MaterialLibrary.EntryName "EUV 13.4 nm" }
        Assert.Equal(Ok (), store.saveSource renamed)
        Assert.Equal(Ok [ renamed ], store.listSources ())
        Assert.Equal(Ok (), store.removeSource (ElementValueId "src-user-1"))
        Assert.Equal(Ok None, store.tryGetSource (ElementValueId "src-user-1"))
        match store.removeSource (ElementValueId "src-user-1") with
        | Error (UnknownSourceIdErr _) -> ()
        | other -> Assert.Fail($"expected UnknownSourceIdErr, got %A{other}")

    [<Fact>]
    let ``the user-sources store refuses a blank name, a non-positive wavelength, and a protected source`` () =
        let store = SourceProxy.createInMemory ()
        match store.saveSource (userSource "src-a" "   " 500.0) with
        | Error (InvalidSourceErr _) -> ()
        | other -> Assert.Fail($"blank name: %A{other}")
        match store.saveSource (userSource "src-b" "Zero" 0.0) with
        | Error (InvalidSourceErr _) -> ()
        | other -> Assert.Fail($"zero wavelength: %A{other}")
        match store.saveSource { userSource "src-c" "Protected" 500.0 with protection = ProtectedBuiltIn } with
        | Error (ProtectedSourceRefusedErr _) -> ()
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
        | Ok entries -> Assert.Equal<string list>((SeedSources.allSeedSources |> List.map (fun s -> s.id.value)) @ [ mine.id.value ], sourceIds entries)
        | Error e -> Assert.Fail($"%A{e}")
        Assert.Equal(Ok (Some (SourceItem mine)), live.tryGetEntry mine.id)
        // The seeded presets still resolve; a removed user source no longer does.
        Assert.Equal(Ok (Some (DetectorItem { id = PresetIds.detectorIntensity; name = MaterialLibrary.EntryName "Intensity detector"; kind = Intensity; protection = ProtectedBuiltIn })), live.tryGetEntry PresetIds.detectorIntensity)
        Assert.Equal(Ok (), sources.removeSource mine.id)
        Assert.Equal(Ok None, live.tryGetEntry mine.id)

    [<Fact>]
    let ``the live Library resolves a sample created in the samples store`` () =
        let samples = SampleProxy.createInMemory VersionsInUse.empty
        let live = liveLibrary (createInMemory ()) samples (SourceProxy.createInMemory ())
        let created = { SeedSamples.glassFilm200 with id = newSampleId (); name = MaterialLibrary.EntryName "My film" }
        Assert.Equal(Ok (), samples.saveSample created)
        let entryId = (SampleItem created).entryId
        match live.tryGetEntry entryId with
        | Ok (Some (SampleItem s)) -> Assert.Equal("My film", s.name.value)
        | other -> Assert.Fail($"the created sample must resolve, got %A{other}")
        match live.entriesForKind Sample with
        | Ok entries -> Assert.Contains(entryId, entries |> List.map (fun e -> e.entryId))
        | Error e -> Assert.Fail($"%A{e}")
        // The read-only seeded Library alone never sees it (the gap the live Library closes).
        Assert.Equal(Ok None, (createInMemory ()).tryGetEntry entryId)
