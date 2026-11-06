namespace OpticalConstructor.Tests

open Xunit
open Berreman.Constants
open Berreman.Fields
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Placement

/// Spec 0046; spec 0050 S1.7 — `BuiltinCatalogue.sources` and `BuiltinCatalogue.library`
/// cover shipped sources and live composition with samples and user sources.
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

    // ------------------------------ BuiltinCatalogue.sources ------------------------------

    [<Fact>]
    let ``the seeded sources are protected, uniquely identified, and cover the lasers and the lithography lines`` () =
        let all = BuiltinCatalogue.sources ()
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
        match (BuiltinCatalogue.library ()).entriesForKind LightSource with
        | Ok entries -> Assert.Equal<string list>(all |> List.map (fun s -> s.id.value), sourceIds entries)
        | Error e -> Assert.Fail($"%A{e}")

    [<Fact>]
    let ``the seeded tree lists every seeded source under Sources`` () =
        let rec leaves (node : LibraryTreeNode) : string list =
            match node with
            | Leaf (_, id) -> [ id.value ]
            | Group (_, children) -> children |> List.collect leaves
        let sourcesGroup =
            match (BuiltinCatalogue.library ()).libraryTrees () with
            | Ok (tree :: _) ->
                match tree.root with
                | Group (_, groups) -> groups |> List.tryPick (fun g -> match g with Group (TreeLabel "Sources", _) -> Some g | _ -> None)
                | Leaf _ -> None
            | Ok [] | Error _ -> None
        match sourcesGroup with
        | Some group ->
            Assert.Equal<string list>(
                BuiltinCatalogue.sources () |> List.map (fun s -> s.id.value) |> List.sort,
                leaves group |> List.sort)
        | None -> Assert.Fail "the seeded tree must carry a Sources group"

    // ------------------------------ GUID identity (spec 0050 §3.3) ------------------------------

    /// `entryId` as a GUID when it is that GUID's canonical string (the lowercase hyphenated form
    /// `SampleId.toElementValueId` writes), else the reason it is not.
    let private canonicalGuidOf (entryId : ElementValueId) : Result<System.Guid, string> =
        match System.Guid.TryParse entryId.value with
        | true, guid when string guid = entryId.value -> Ok guid
        | true, guid -> Error $"'{entryId.value}' parses as a GUID but is not its canonical string '{string guid}'"
        | false, _ -> Error $"'{entryId.value}' is not the string of a GUID"

    [<Fact>]
    let ``every seeded Library entry id is the canonical string of a GUID, and no two are equal`` () =
        let entryIds = ((BuiltinCatalogue.samples () |> List.map SampleItem) @ BuiltinCatalogue.presets ()) |> List.map (fun e -> e.entryId)
        let refusals = entryIds |> List.choose (fun entryId -> match canonicalGuidOf entryId with Ok _ -> None | Error reason -> Some reason)
        let listed = String.concat "; " refusals
        Assert.True(List.isEmpty refusals, $"{List.length refusals} of {List.length entryIds} seeded entry ids are not GUIDs: {listed}")
        let repeated = entryIds |> List.countBy id |> List.filter (fun (_, count) -> count > 1) |> List.map fst
        Assert.Empty(repeated)

    [<Fact>]
    let ``newSourceId mints the canonical string of a fresh GUID`` () =
        let first = newSourceId ()
        let second = newSourceId ()
        match canonicalGuidOf first, canonicalGuidOf second with
        | Ok _, Ok _ -> Assert.NotEqual<ElementValueId>(first, second)
        | Error reason, _ | _, Error reason -> Assert.Fail reason

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
        let samples = BuiltinCatalogue.sampleStore ()
        let sources = SourceProxy.createInMemory ()
        let live = liveLibrary (BuiltinCatalogue.library ()) samples sources
        let mine = userSource "src-user-krf" "My KrF line" 248.0
        Assert.Equal(Ok (), sources.saveSource mine)
        match live.entriesForKind LightSource with
        | Ok entries -> Assert.Equal<string list>((BuiltinCatalogue.sources () |> List.map (fun s -> s.id.value)) @ [ mine.id.value ], sourceIds entries)
        | Error e -> Assert.Fail($"%A{e}")
        Assert.Equal(Ok (Some (SourceItem mine)), live.tryGetEntry mine.id)
        // BuiltinCatalogue.presets still resolve after user sources are removed.
        Assert.Equal(Ok (Some (DetectorItem { id = PresetIds.detectorIntensity; name = MaterialLibrary.EntryName "Intensity detector"; kind = Intensity; protection = ProtectedBuiltIn })), live.tryGetEntry PresetIds.detectorIntensity)
        Assert.Equal(Ok (), sources.removeSource mine.id)
        Assert.Equal(Ok None, live.tryGetEntry mine.id)

    [<Fact>]
    let ``the live Library resolves a sample created in the samples store`` () =
        let samples = BuiltinCatalogue.sampleStore ()
        let live = liveLibrary (BuiltinCatalogue.library ()) samples (SourceProxy.createInMemory ())
        let created =
            {
                BuiltinCatalogue.sample SampleIds.glassFilm200Id with
                    id = newSampleId ()
                    name = MaterialLibrary.EntryName "My film"
            }
        Assert.Equal(Ok (), samples.saveSample created)
        let entryId = (SampleItem created).entryId
        match live.tryGetEntry entryId with
        | Ok (Some (SampleItem s)) -> Assert.Equal("My film", s.name.value)
        | other -> Assert.Fail($"the created sample must resolve, got %A{other}")
        match live.entriesForKind Sample with
        | Ok entries -> Assert.Contains(entryId, entries |> List.map (fun e -> e.entryId))
        | Error e -> Assert.Fail($"%A{e}")
        // BuiltinCatalogue.library alone never sees user-created samples.
        Assert.Equal(Ok None, (BuiltinCatalogue.library ()).tryGetEntry entryId)

    /// Spec 0050 §3.3: every id is now a GUID, so `liveLibrary`'s sample probe parses a preset's id too;
    /// it finds no sample there and falls through to the presets.
    [<Fact>]
    let ``the live Library resolves a seeded preset, a user source and a sample by their GUID ids`` () =
        let samples = BuiltinCatalogue.sampleStore ()
        let sources = SourceProxy.createInMemory ()
        let live = liveLibrary (BuiltinCatalogue.library ()) samples sources
        let mine : SourcePreset = { id = newSourceId (); name = MaterialLibrary.EntryName "My GUID line"; waveLength = WaveLength.nm 532.0<nm>; tuning = FixedLine; protection = UserManaged }
        Assert.Equal(Ok (), sources.saveSource mine)
        let created =
            {
                BuiltinCatalogue.sample SampleIds.glassFilm200Id with
                    id = newSampleId ()
                    name = MaterialLibrary.EntryName "My GUID film"
            }
        Assert.Equal(Ok (), samples.saveSample created)
        match live.tryGetEntry PresetIds.detectorEllipsometer with
        | Ok (Some (DetectorItem d)) -> Assert.Equal<DetectorKind>(Ellipsometer, d.kind)
        | other -> Assert.Fail($"the ellipsometer preset must resolve, got %A{other}")
        Assert.Equal(Ok (Some (SourceItem mine)), live.tryGetEntry mine.id)
        match live.tryGetEntry created.id.toElementValueId with
        | Ok (Some (SampleItem s)) -> Assert.Equal<SampleId>(created.id, s.id)
        | other -> Assert.Fail($"the saved sample must resolve, got %A{other}")
        let nothing = ElementValueId (string (System.Guid.NewGuid ()))
        Assert.Equal(Ok None, live.tryGetEntry nothing)
