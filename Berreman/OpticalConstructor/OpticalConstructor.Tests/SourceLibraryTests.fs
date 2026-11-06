namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Fields
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Storage.LibraryDatabaseStore
open OpticalConstructor.Storage.SourceDatabaseStore

module private SourceLibraryTestData =

    let nm (w : WaveLength) : float = w.value / nmToMeter / 1.0<nm>

    let userSource (id : ElementValueId) (name : string) (wavelengthNm : float) : SourcePreset =
        {
            id = id
            name = MaterialLibrary.EntryName name
            waveLength = WaveLength.nm (wavelengthNm * 1.0<nm>)
            tuning = FixedLine
            protection = UserManaged
        }

    let sourceIds (entries : LibraryEntry list) : string list =
        entries
        |> List.choose (fun e ->
            match e with
            | SourceItem s -> Some s.id.value
            | SampleItem _ | DetectorItem _ | PolarizerItem _ -> None)

    /// `entryId` as a GUID when it is that GUID's canonical string (the lowercase hyphenated form
    /// `SampleId.toElementValueId` writes), else the reason it is not.
    let canonicalGuidOf (entryId : ElementValueId) : Result<System.Guid, string> =
        match System.Guid.TryParse entryId.value with
        | true, guid when string guid = entryId.value -> Ok guid
        | true, guid -> Error $"'{entryId.value}' parses as a GUID but is not its canonical string '{string guid}'"
        | false, _ -> Error $"'{entryId.value}' is not the string of a GUID"

open SourceLibraryTestData

/// User-source stores insert, replace and remove entries, and refuse invalid or protected sources.
// Spec 0050 step 0075: adapters inherit these facts under their concrete xUnit class ids.
[<AbstractClass>]
type SourceProxyContract () =

    /// Returns a fresh store containing no user sources.
    abstract freshSources : unit -> SourceProxy

    [<Fact>]
    member this.``the user-sources store inserts, replaces by id, and removes`` () : unit =
        let store = this.freshSources ()
        Assert.Equal(Ok [], store.listSources ())
        let euv = userSource (newSourceId ()) "My EUV line" 13.4
        Assert.Equal(Ok (), store.saveSource euv)
        Assert.Equal(Ok (Some euv), store.tryGetSource euv.id)
        let renamed =
            {
                euv with name = MaterialLibrary.EntryName "EUV 13.4 nm"
            }
        Assert.Equal(Ok (), store.saveSource renamed)
        Assert.Equal(Ok [ renamed ], store.listSources ())
        Assert.Equal(Ok (), store.removeSource euv.id)
        Assert.Equal(Ok None, store.tryGetSource euv.id)
        match store.removeSource euv.id with
        | Error (UnknownSourceIdErr _) -> ()
        | other -> Assert.Fail($"expected UnknownSourceIdErr, got %A{other}")

    [<Fact>]
    member this.``the user-sources store refuses a blank name, a non-positive wavelength, and a protected source`` () : unit =
        let store = this.freshSources ()
        match store.saveSource (userSource (ElementValueId "src-a") "   " 500.0) with
        | Error (InvalidSourceErr _) -> ()
        | other -> Assert.Fail($"blank name: %A{other}")
        match store.saveSource (userSource (ElementValueId "src-b") "Zero" 0.0) with
        | Error (InvalidSourceErr _) -> ()
        | other -> Assert.Fail($"zero wavelength: %A{other}")
        let protectedSource =
            {
                userSource (ElementValueId "src-c") "Protected" 500.0 with protection = ProtectedBuiltIn
            }
        match store.saveSource protectedSource with
        | Error (ProtectedSourceRefusedErr _) -> ()
        | other -> Assert.Fail($"protected: %A{other}")
        Assert.Equal(Ok [], store.listSources ())

/// Covers shipped sources, GUID identities and live composition with samples and user sources.
// Spec 0046; spec 0050 S1.7 and step 0075.
type SourceLibraryTests () =
    inherit SourceProxyContract ()

    override _.freshSources () : SourceProxy = SourceProxy.createInMemory ()

    [<Fact>]
    member _.``the seeded sources are protected, uniquely identified, and cover the lasers and the lithography lines`` () : unit =
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
    member _.``the seeded tree lists every seeded source under Sources`` () : unit =
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

    [<Fact>]
    member _.``every seeded Library entry id is the canonical string of a GUID, and no two are equal`` () : unit =
        let entryIds = ((BuiltinCatalogue.samples () |> List.map SampleItem) @ BuiltinCatalogue.presets ()) |> List.map (fun e -> e.entryId)
        let refusals = entryIds |> List.choose (fun entryId -> match canonicalGuidOf entryId with Ok _ -> None | Error reason -> Some reason)
        let listed = String.concat "; " refusals
        Assert.True(List.isEmpty refusals, $"{List.length refusals} of {List.length entryIds} seeded entry ids are not GUIDs: {listed}")
        let repeated = entryIds |> List.countBy id |> List.filter (fun (_, count) -> count > 1) |> List.map fst
        Assert.Empty(repeated)

    [<Fact>]
    member _.``newSourceId mints the canonical string of a fresh GUID`` () : unit =
        let first = newSourceId ()
        let second = newSourceId ()
        match canonicalGuidOf first, canonicalGuidOf second with
        | Ok _, Ok _ -> Assert.NotEqual<ElementValueId>(first, second)
        | Error reason, _ | _, Error reason -> Assert.Fail reason

    [<Fact>]
    member _.``the live Library offers and resolves a user source after the seeded ones`` () : unit =
        let samples = BuiltinCatalogue.sampleStore ()
        let sources = SourceProxy.createInMemory ()
        let live = liveLibrary (BuiltinCatalogue.library ()) samples sources
        let mine = userSource (ElementValueId "src-user-krf") "My KrF line" 248.0
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
    member _.``the live Library resolves a sample created in the samples store`` () : unit =
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
    member _.``the live Library resolves a seeded preset, a user source and a sample by their GUID ids`` () : unit =
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

type SourceProxySqliteTests () =
    inherit SourceProxyContract ()

    let databases = ResizeArray<LibraryDatabase>()

    member private _.freshDatabase () : LibraryDatabase =
        let database = BuiltinCatalogue.freshLibraryDatabase ()
        databases.Add database
        database

    override this.freshSources () : SourceProxy =
        SourceProxy.createInDatabase (this.freshDatabase ())

    [<Fact>]
    member this.``a user source is versioned in the database, the proxy answers its latest, and a pinned source is not removable`` () : unit =
        let database = this.freshDatabase ()
        let proxy = SourceProxy.createInDatabase database
        let source = userSource (newSourceId ()) "My versioned source" 532.0
        let entryId = source.id.tryToLibraryEntryId |> Option.defaultWith (fun () -> failwith "Expected a minted GUID")
        let latest () : StoredEntryVersion =
            database.run (fun context -> context.tryLatestStored entryId)
            |> Result.bind id
            |> Result.defaultWith (fun error -> failwith error.describe)
            |> Option.defaultWith (fun () -> failwith "Expected the latest source version")
        let storedVersion (version : VersionNumber) : StoredEntryVersion option =
            database.run (fun context -> context.tryStoredVersion entryId version)
            |> Result.bind id
            |> Result.defaultWith (fun error -> failwith error.describe)
        let pinVersion (version : VersionNumber) : unit =
            database.run (fun context ->
                context.tryStoredVersion entryId version
                |> Result.bind (fun storedOpt ->
                    let stored = storedOpt |> Option.defaultWith (fun () -> failwith "Expected a version to pin")
                    let pinned =
                        {
                            stored with
                                state =
                                    {
                                        stored.state with pin = VersionPinned
                                    }
                        }
                    context.stageVersion pinned
                    |> Result.map (fun () -> context.SaveChanges() |> ignore)))
            |> Result.bind id
            |> Result.defaultWith (fun error -> failwith error.describe)

        Assert.Equal(Ok (), proxy.saveSource source)
        let first = latest ()
        Assert.Equal(VersionNumber.first, first.envelope.content.version)
        Assert.Equal(UserProvenance, first.envelope.content.provenance)
        Assert.Equal(VersionUnpinned, first.state.pin)
        Assert.Equal(ActiveEntry, first.state.lifecycle)
        Assert.Equal(Ok source, first.envelope.content.toSourcePreset)

        pinVersion VersionNumber.first
        let changed =
            {
                source with waveLength = WaveLength.nm 633.0<nm>
            }
        Assert.Equal(Ok (), proxy.saveSource changed)
        let second = latest ()
        Assert.Equal(VersionNumber.first.next, second.envelope.content.version)
        Assert.Equal(UserProvenance, second.envelope.content.provenance)
        Assert.Equal(VersionUnpinned, second.state.pin)
        let frozen = storedVersion VersionNumber.first |> Option.defaultWith (fun () -> failwith "Expected version 1")
        Assert.Equal(VersionPinned, frozen.state.pin)
        Assert.Equal(first.envelope, frozen.envelope)
        Assert.Equal(Ok source, frozen.envelope.content.toSourcePreset)
        Assert.Equal(Ok (Some changed), proxy.tryGetSource source.id)
        Assert.Equal(Ok [ changed ], proxy.listSources ())

        let renamed =
            {
                changed with name = EntryName "Renamed versioned source"
            }
        Assert.Equal(Ok (), proxy.saveSource renamed)
        Assert.Equal(Ok (), proxy.saveSource renamed)
        let edited = latest ()
        Assert.Equal(second.envelope.content.version, edited.envelope.content.version)
        Assert.Equal(Ok renamed, edited.envelope.content.toSourcePreset)
        Assert.Equal<StoredEntryVersion option>(None, storedVersion VersionNumber.first.next.next)
        match proxy.removeSource source.id with
        | Error (SourceVersionPinnedErr reason) ->
            Assert.Contains(renamed.name.value, reason)
            Assert.Contains(source.id.value, reason)
            Assert.Contains("v1", reason)
        | other -> Assert.Fail($"Expected a pinned-source refusal, got %A{other}")
        Assert.Equal(Ok (Some renamed), proxy.tryGetSource source.id)

        pinVersion second.envelope.content.version
        let limits : Berreman.Dispersion.WaveLengthInterval =
            {
                lower = WaveLength.nm 500.0<nm>
                upper = WaveLength.nm 700.0<nm>
            }
        let tunable =
            {
                renamed with
                    tuning = TunableBetween limits
            }
        Assert.Equal(Ok (), proxy.saveSource tunable)
        let third = latest ()
        Assert.Equal(VersionNumber.first.next.next, third.envelope.content.version)
        Assert.Equal(VersionUnpinned, third.state.pin)
        let pinnedSecond = storedVersion second.envelope.content.version |> Option.defaultWith (fun () -> failwith "Expected version 2")
        Assert.Equal(edited.envelope, pinnedSecond.envelope)
        Assert.Equal(Ok (Some tunable), proxy.tryGetSource source.id)
        Assert.Equal(Ok [ tunable ], proxy.listSources ())
        match proxy.removeSource source.id with
        | Error (SourceVersionPinnedErr reason) -> Assert.Contains("v1, v2", reason)
        | other -> Assert.Fail($"Expected both pinned versions in the refusal, got %A{other}")

    [<Fact>]
    member this.``the database user-sources store holds user sources only and refuses an id it cannot hold`` () : unit =
        let database = this.freshDatabase ()
        let proxy = SourceProxy.createInDatabase database
        let source = userSource (newSourceId ()) "User source" 532.0
        let builtIn = BuiltinCatalogue.source PresetIds.source600
        let presetId = builtIn.id.tryToLibraryEntryId |> Option.defaultWith (fun () -> failwith "Expected a preset GUID")
        let presetBefore =
            database.run (fun context -> context.tryLatestStored presetId)
            |> Result.bind id
            |> Result.defaultWith (fun error -> failwith error.describe)
            |> Option.defaultWith (fun () -> failwith "Expected the catalogue source")
        Assert.Equal(VersionNumber.first, presetBefore.envelope.content.version)
        Assert.Equal(Ok builtIn, presetBefore.envelope.content.toSourcePreset)
        Assert.Equal(Ok [], proxy.listSources ())

        let protectedCollision =
            {
                source with id = PresetIds.source600
            }
        match proxy.saveSource protectedCollision with
        | Error (ProtectedSourceRefusedErr reason) -> Assert.Contains(builtIn.name.value, reason)
        | other -> Assert.Fail($"Expected a protected source id refusal, got %A{other}")
        let sampleCollision =
            {
                source with id = SampleIds.multilayerQwId.toElementValueId
            }
        match proxy.saveSource sampleCollision with
        | Error (InvalidSourceErr reason) ->
            Assert.Contains(sampleCollision.id.value, reason)
            Assert.Contains(".sample-json", reason)
        | other -> Assert.Fail($"Expected a cross-kind id refusal, got %A{other}")
        let textId = ElementValueId "not-a-guid"
        let invalidId =
            {
                source with id = textId
            }
        match proxy.saveSource invalidId with
        | Error (InvalidSourceErr reason) -> Assert.Contains(textId.value, reason)
        | other -> Assert.Fail($"Expected a non-GUID id refusal, got %A{other}")
        for absentId in [ PresetIds.source600; sampleCollision.id; textId; newSourceId () ] do
            Assert.Equal(Ok None, proxy.tryGetSource absentId)
            Assert.Equal(Error (SourceError.unknownSource absentId), proxy.removeSource absentId)
        let presetAfter =
            database.run (fun context -> context.tryLatestStored presetId)
            |> Result.bind id
            |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal(Some presetBefore, presetAfter)
        Assert.Equal(Ok [], proxy.listSources ())

        let imported = userSource (newSourceId ()) "A source" 500.0
        let tied = userSource (newSourceId ()) "A source" 600.0
        let kitId = Guid.NewGuid() |> KitId
        let provenance = KitImportProvenance kitId
        let content =
            LibraryEntryContent.ofSourcePreset VersionNumber.first provenance imported
            |> Result.defaultWith (fun error -> failwith error.describe)
        database.run (fun context ->
            context.stageNewEntry content
            |> Result.map (fun () -> context.SaveChanges() |> ignore))
        |> Result.bind id
        |> Result.defaultWith (fun error -> failwith error.describe)
        let inactive =
            database.run (fun context -> context.tryLatestStored content.id)
            |> Result.bind id
            |> Result.defaultWith (fun error -> failwith error.describe)
            |> Option.defaultWith (fun () -> failwith "Expected the imported source")
            |> fun stored ->
                {
                    stored with
                        state =
                            {
                                stored.state with lifecycle = InactiveEntry
                            }
                }
        database.run (fun context ->
            context.stageVersion inactive
            |> Result.map (fun () -> context.SaveChanges() |> ignore))
        |> Result.bind id
        |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal(Ok (), proxy.saveSource source)
        Assert.Equal(Ok (), proxy.saveSource tied)
        let editedImport =
            {
                imported with waveLength = WaveLength.nm 510.0<nm>
            }
        Assert.Equal(Ok (), proxy.saveSource editedImport)
        let heldImport =
            database.run (fun context -> context.tryLatestStored content.id)
            |> Result.bind id
            |> Result.defaultWith (fun error -> failwith error.describe)
            |> Option.defaultWith (fun () -> failwith "Expected the edited imported source")
        Assert.Equal(provenance, heldImport.envelope.content.provenance)
        Assert.Equal(VersionNumber.first, heldImport.envelope.content.version)
        Assert.Equal(InactiveEntry, heldImport.state.lifecycle)
        Assert.Equal(Ok (Some editedImport), proxy.tryGetSource imported.id)
        let expected = (List.sortBy (fun (entry : SourcePreset) -> entry.id) [ editedImport; tied ]) @ [ source ]
        Assert.Equal(Ok expected, proxy.listSources ())

    [<Fact>]
    member this.``a database failure below the user-sources store is SourceStoreUnavailableErr, never a throw`` () : unit =
        let database = this.freshDatabase ()
        let proxy = SourceProxy.createInDatabase database
        (database :> IDisposable).Dispose()

        let unavailable (result : Result<'Outcome, SourceError>) : unit =
            match result with
            | Error (SourceStoreUnavailableErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
            | other -> Assert.Fail($"Expected a typed database failure, got %A{other}")

        let source = userSource (newSourceId ()) "Unavailable source" 532.0
        proxy.listSources () |> unavailable
        proxy.tryGetSource source.id |> unavailable
        proxy.saveSource source |> unavailable
        proxy.removeSource source.id |> unavailable

    interface IDisposable with
        member _.Dispose () : unit =
            for database in databases do
                (database :> IDisposable).Dispose()
