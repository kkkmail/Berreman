namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Media
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Storage.LibraryDatabaseStore
open OpticalConstructor.Storage.MaterialDatabaseStore
open OpticalConstructor.Storage.SampleDatabaseStore

module private SampleProxyTestData =

    let plate : Sample = BuiltinCatalogue.sample SampleIds.glassPlate1mmId

    let otherStructure : Sample = BuiltinCatalogue.sample SampleIds.glassPlate2mmId

    let platePhysicsChanged : Sample =
        { plate with structure = otherStructure.structure }

    let plateRenamed : Sample =
        { plate with name = EntryName "Renamed plate (metadata only)" }

    let v1Id (id : SampleId) : SampleVersionId = { sampleId = id; version = VersionNumber.first }
    let v2Id (id : SampleId) : SampleVersionId = { sampleId = id; version = VersionNumber.first.next }

    let minted () : Sample =
        { plate with id = newSampleId (); name = EntryName "Test plate (minted)" }

    // Spec 0035 step 012: the engine mapping has nothing to expand without films or a substrate.
    let emptyStructure : SampleStructure =
        { films = []; substrate = None; lower = None }

    // Relabel a film to isolate the missing-substrate rule from the structurally-empty rejection
    // (spec 0040 D.1; spec 0050 S1.7).
    let plateMissingSubstrate : Sample =
        {
            BuiltinCatalogue.sample SampleIds.glassFilm600Id with
                substrate = ThickPlate
        }

open SampleProxyTestData

/// Sample stores preserve version history, lifecycle visibility and pinned-version removal guards.
// Spec 0050 step 0073: adapters inherit these facts under their concrete xUnit class ids.
[<AbstractClass>]
type SampleProxyContract () =

    /// Returns the Built-in samples at version 1, active and unpinned, in a fresh store.
    abstract freshProxy : unit -> SampleProxy

    member private this.proxyWithPinned (versions : SampleVersionId list) : SampleProxy =
        let proxy = this.freshProxy ()
        for version in versions do
            Assert.Equal<Result<unit, SampleError>>(Ok (), proxy.pinSampleVersion version)
        proxy

    [<Fact>]
    member this.``listSamples ActiveOnly returns the seeded samples with distinct ids`` () : unit =
        match (this.freshProxy ()).listSamples ActiveOnly with
        | Ok samples ->
            Assert.NotEmpty samples
            Assert.Equal(List.length (BuiltinCatalogue.samples ()), List.length samples)
            let ids = samples |> List.map (fun s -> s.id)
            Assert.Equal(List.length ids, ids |> List.distinct |> List.length)
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    member this.``searchSamples with the empty query matches every seeded sample`` () : unit =
        match (this.freshProxy ()).searchSamples SampleQuery.empty with
        | Ok samples -> Assert.Equal(List.length (BuiltinCatalogue.samples ()), List.length samples)
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    member this.``searchSamples filters by name fragment case-insensitively`` () : unit =
        match (this.freshProxy ()).searchSamples { SampleQuery.empty with text = NameFragment "GLASS" } with
        | Ok samples ->
            Assert.NotEmpty samples
            Assert.All(samples, fun s ->
                Assert.True(s.name.value.IndexOf("glass", StringComparison.OrdinalIgnoreCase) >= 0, s.name.value))
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    member this.``searchSamples filters by substrate kind, composing with the text facet`` () : unit =
        let proxy = this.freshProxy ()
        match proxy.searchSamples { SampleQuery.empty with substrate = Some ThickPlate } with
        | Ok plates ->
            Assert.NotEmpty plates
            Assert.All(plates, fun s -> Assert.Equal(ThickPlate, s.substrate))
            Assert.Equal(
                BuiltinCatalogue.samples () |> List.filter (fun s -> s.substrate = ThickPlate) |> List.length,
                List.length plates)
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.searchSamples { text = NameFragment "langasite"; substrate = Some ThinFilm } with
        | Ok [ only ] -> Assert.Equal<SampleId>(SampleIds.langasiteSiliconId, only.id)
        | other -> Assert.Fail($"expected exactly the langasite sample, got %A{other}")

    [<Fact>]
    member this.``tryGetSample hits a known id and misses a minted one`` () : unit =
        let proxy = this.freshProxy ()
        match proxy.tryGetSample SampleIds.euvMoSiId with
        | Ok (Some s) -> Assert.Equal<SampleId>(SampleIds.euvMoSiId, s.id)
        | other -> Assert.Fail($"expected the EUV Mo/Si sample, got %A{other}")
        match proxy.tryGetSample (newSampleId ()) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None, got %A{other}")

    [<Fact>]
    member this.``resolveVersion hits version 1 of a seed, misses a non-existent version, and misses an unknown id`` () : unit =
        let proxy = this.freshProxy ()
        match proxy.resolveVersion (v1Id SampleIds.glassPlate1mmId) with
        | Ok (Some s) -> Assert.Equal<SampleId>(SampleIds.glassPlate1mmId, s.id)
        | other -> Assert.Fail($"expected version 1 of the glass plate, got %A{other}")
        match proxy.resolveVersion (v2Id SampleIds.glassPlate1mmId) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None for a non-existent version 2, got %A{other}")
        match proxy.resolveVersion (v1Id (newSampleId ())) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None for an unknown id, got %A{other}")

    [<Fact>]
    member this.``saveSample on a new id inserts version 1 active — appears in listing and resolves at v1`` () : unit =
        let proxy = this.freshProxy ()
        let sample = minted ()
        match proxy.saveSample sample with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listSamples ActiveOnly with
        | Ok samples ->
            Assert.Equal(List.length (BuiltinCatalogue.samples ()) + 1, List.length samples)
            Assert.Contains(sample.id, samples |> List.map (fun s -> s.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.resolveVersion (v1Id sample.id) with
        | Ok (Some s) -> Assert.Equal<SampleId>(sample.id, s.id)
        | other -> Assert.Fail($"expected version 1 of the minted sample, got %A{other}")

    [<Fact>]
    member this.``saveSample rejects a blank name as InvalidSample and persists nothing`` () : unit =
        let proxy = this.freshProxy ()
        let blank = { minted () with name = EntryName "   " }
        match proxy.saveSample blank with
        | Error (InvalidSampleErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidSampleErr _), got %A{other}")
        match proxy.tryGetSample blank.id with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None (nothing persisted), got %A{other}")

    [<Fact>]
    member this.``saveSample rejects a structurally-empty sample (no films, no substrate) as InvalidSample even when named`` () : unit =
        let proxy = this.freshProxy ()
        match proxy.saveSample { minted () with structure = emptyStructure } with
        | Error (InvalidSampleErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidSampleErr _), got %A{other}")

    [<Fact>]
    member this.``KeepCurrent — an identical save on a pinned version leaves exactly version 1 unchanged`` () : unit =
        let proxy = this.proxyWithPinned [ v1Id plate.id ]
        match proxy.saveSample plate with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.resolveVersion (v2Id plate.id) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected no version 2 after a no-op save, got %A{other}")
        match proxy.resolveVersion (v1Id plate.id) with
        | Ok (Some s) -> Assert.Equal(plate.structure, s.structure)
        | other -> Assert.Fail($"expected version 1 intact, got %A{other}")

    [<Fact>]
    member this.``MutateInPlace — a metadata-only edit of a PINNED version updates version 1 and does NOT mint`` () : unit =
        let proxy = this.proxyWithPinned [ v1Id plate.id ]
        match proxy.saveSample plateRenamed with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.resolveVersion (v2Id plate.id) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected NO version 2 from a metadata-only edit, got %A{other}")
        match proxy.resolveVersion (v1Id plate.id) with
        | Ok (Some s) -> Assert.Equal(plateRenamed.name, s.name)
        | other -> Assert.Fail($"expected version 1 renamed in place, got %A{other}")

    [<Fact>]
    member this.``MutateInPlace — a structure change of an UNPINNED version updates version 1 in place`` () : unit =
        let proxy = this.freshProxy ()
        match proxy.saveSample platePhysicsChanged with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        Assert.Equal<Result<Sample option, SampleError>>(Ok None, proxy.resolveVersion (v2Id plate.id))
        match proxy.resolveVersion (v1Id plate.id) with
        | Ok (Some s) -> Assert.Equal(otherStructure.structure, s.structure)
        | other -> Assert.Fail($"expected version 1 structure mutated in place, got %A{other}")

    [<Fact>]
    member this.``MintNextVersion — a structure change of a PINNED version freezes version 1 and mints version 2`` () : unit =
        let proxy = this.proxyWithPinned [ v1Id plate.id ]
        match proxy.saveSample platePhysicsChanged with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.resolveVersion (v1Id plate.id) with
        | Ok (Some s) -> Assert.Equal(plate.structure, s.structure)
        | other -> Assert.Fail($"expected version 1 frozen with original structure, got %A{other}")
        match proxy.resolveVersion (v2Id plate.id) with
        | Ok (Some s) -> Assert.Equal(otherStructure.structure, s.structure)
        | other -> Assert.Fail($"expected version 2 with the new structure, got %A{other}")
        match proxy.tryGetSample plate.id with
        | Ok (Some s) -> Assert.Equal(otherStructure.structure, s.structure)
        | other -> Assert.Fail($"expected the latest sample to be version 2, got %A{other}")

    [<Fact>]
    member this.``latest-active default listing shows one entry per sample after a mint, not the frozen history`` () : unit =
        let proxy = this.proxyWithPinned [ v1Id plate.id ]
        match proxy.saveSample platePhysicsChanged with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listSamples ActiveOnly with
        | Ok samples ->
            Assert.Equal(List.length (BuiltinCatalogue.samples ()), List.length samples)
            let plateEntries = samples |> List.filter (fun s -> s.id = plate.id)
            Assert.Equal(1, List.length plateEntries)
            Assert.Equal(otherStructure.structure, plateEntries.Head.structure)
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    member this.``markSampleInactive hides a sample from the offers yet keeps it resolvable, and markSampleActive revives it`` () : unit =
        let proxy = this.freshProxy ()
        match proxy.markSampleInactive plate.id with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listSamples ActiveOnly with
        | Ok samples -> Assert.DoesNotContain(plate.id, samples |> List.map (fun s -> s.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.searchSamples { SampleQuery.empty with text = NameFragment plate.name.value } with
        | Ok samples -> Assert.DoesNotContain(plate.id, samples |> List.map (fun s -> s.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listSamples IncludeInactive with
        | Ok samples -> Assert.Contains(plate.id, samples |> List.map (fun s -> s.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.resolveVersion (v1Id plate.id) with
        | Ok (Some _) -> ()
        | other -> Assert.Fail($"expected the inactive version to resolve, got %A{other}")
        match proxy.markSampleActive plate.id with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listSamples ActiveOnly with
        | Ok samples -> Assert.Contains(plate.id, samples |> List.map (fun s -> s.id))
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    member this.``supersedeSample behaves as inactive — hidden from offers, present under include-inactive, resolvable by version`` () : unit =
        let proxy = this.freshProxy ()
        match proxy.supersedeSample plate.id with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listSamples ActiveOnly with
        | Ok samples -> Assert.DoesNotContain(plate.id, samples |> List.map (fun s -> s.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listSamples IncludeInactive with
        | Ok samples -> Assert.Contains(plate.id, samples |> List.map (fun s -> s.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.resolveVersion (v1Id plate.id) with
        | Ok (Some _) -> ()
        | other -> Assert.Fail($"expected the superseded version to resolve, got %A{other}")

    [<Fact>]
    member this.``the lifecycle verbs reject an unknown id with a diagnostic reason`` () : unit =
        let proxy = this.freshProxy ()
        let missing = (minted ()).id
        for verb in [ proxy.markSampleInactive; proxy.markSampleActive; proxy.supersedeSample ] do
            match verb missing with
            | Error (UnknownSampleIdErr reason) -> Assert.Contains(string missing.value, reason)
            | other -> Assert.Fail($"expected Error (UnknownSampleIdErr _), got %A{other}")

    [<Fact>]
    member this.``removeSample on a sample with a pinned version is blocked with a typed refusal naming the version`` () : unit =
        let proxy = this.proxyWithPinned [ v1Id plate.id ]
        match proxy.removeSample plate.id with
        | Error (SampleVersionPinnedErr reason) ->
            Assert.Contains(string plate.id.value, reason)
            Assert.Contains("v1", reason)
        | other -> Assert.Fail($"expected Error (SampleVersionPinnedErr _), got %A{other}")
        match proxy.tryGetSample plate.id with
        | Ok (Some _) -> ()
        | other -> Assert.Fail($"expected the sample to survive the pinned-version block, got %A{other}")

    [<Fact>]
    member this.``removeSample succeeds when no version is pinned, then the id is unknown`` () : unit =
        let proxy = this.freshProxy ()
        match proxy.removeSample SampleIds.langasiteSiliconId with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.tryGetSample SampleIds.langasiteSiliconId with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None after removal, got %A{other}")
        match proxy.removeSample SampleIds.langasiteSiliconId with
        | Error (UnknownSampleIdErr reason) -> Assert.Contains(string SampleIds.langasiteSiliconId.value, reason)
        | other -> Assert.Fail($"expected Error (UnknownSampleIdErr _), got %A{other}")

    [<Fact>]
    member this.``a SampleProxy compares by reference (the Elmish-required equality)`` () : unit =
        let p = this.freshProxy ()
        let same = p
        Assert.True((p = same))
        Assert.False((p = this.freshProxy ()))

    [<Fact>]
    member this.``pinSampleVersion pins one version, a minted version starts unpinned, and an unknown version is refused`` () : unit =
        let proxy = this.freshProxy ()
        let first = SampleVersionId.firstOf SampleIds.glassPlate1mmId
        let second : SampleVersionId =
            {
                first with version = first.version.next
            }
        let third : SampleVersionId =
            {
                first with version = second.version.next
            }
        let missingId = newSampleId ()
        let missing = SampleVersionId.firstOf missingId

        Assert.Equal<Result<unit, SampleError>>(Ok (), proxy.pinSampleVersion first)
        Assert.Equal<Result<unit, SampleError>>(Ok (), proxy.pinSampleVersion first)
        Assert.Equal<Result<unit, SampleError>>(Ok (), proxy.saveSample plate)
        Assert.Equal<Result<unit, SampleError>>(Ok (), proxy.saveSample plateRenamed)
        for verb in [ proxy.markSampleInactive; proxy.markSampleActive; proxy.supersedeSample ] do
            Assert.Equal<Result<unit, SampleError>>(Ok (), verb plate.id)
            match proxy.removeSample plate.id with
            | Error (SampleVersionPinnedErr reason) -> Assert.Contains("v1", reason)
            | other -> Assert.Fail($"expected the pin to survive metadata and lifecycle edits, got %A{other}")

        Assert.Equal<Result<unit, SampleError>>(Ok (), proxy.saveSample platePhysicsChanged)
        match proxy.resolveVersion second with
        | Ok (Some entry) -> Assert.Equal(platePhysicsChanged.structure, entry.structure)
        | other -> Assert.Fail($"expected a minted version 2, got %A{other}")
        Assert.Equal<Result<unit, SampleError>>(Ok (), proxy.saveSample plate)
        match proxy.resolveVersion second with
        | Ok (Some entry) -> Assert.Equal(plate.structure, entry.structure)
        | other -> Assert.Fail($"expected version 2 to mutate in place, got %A{other}")
        match proxy.resolveVersion third with
        | Ok None -> ()
        | other -> Assert.Fail($"expected no version 3, got %A{other}")
        match proxy.resolveVersion first with
        | Ok (Some entry) -> Assert.Equal(plate.structure, entry.structure)
        | other -> Assert.Fail($"expected version 1 to keep its original physics, got %A{other}")

        for version in [ missing; third ] do
            match proxy.pinSampleVersion version with
            | Error (UnknownSampleIdErr reason) ->
                Assert.Contains(string version.sampleId.value, reason)
                Assert.Contains($"v{version.version.value}", reason)
            | other -> Assert.Fail($"expected an unknown-version refusal, got %A{other}")

        Assert.Equal<Result<unit, SampleError>>(Ok (), proxy.pinSampleVersion second)
        match proxy.removeSample plate.id with
        | Error (SampleVersionPinnedErr reason) ->
            Assert.Contains(plate.name.value, reason)
            Assert.Contains(string plate.id.value, reason)
            Assert.Contains("2 version(s) pinned: v1, v2", reason)
        | other -> Assert.Fail($"expected both pinned versions to block removal, got %A{other}")

type SampleProxyTests () =
    inherit SampleProxyContract ()

    override _.freshProxy () : SampleProxy =
        BuiltinCatalogue.sampleStore ()

    [<Fact>]
    member _.``validateSample rejects a Plate whose structure.substrate is None as InvalidSample`` () : unit =
        match plateMissingSubstrate.validate with
        | Error (InvalidSampleErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidSampleErr _) for a Plate with no substrate, got %A{other}")

    [<Fact>]
    member _.``validateSample accepts a Plate that carries a substrate and a non-blank name`` () : unit =
        match (BuiltinCatalogue.sample SampleIds.glassPlate1mmId).validate with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () for a Plate with a substrate, got %A{other}")

    [<Fact>]
    member _.``validateSample leaves a ThinFilm with no substrate Ok`` () : unit =
        match (BuiltinCatalogue.sample SampleIds.glassFilm600Id).validate with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () for a ThinFilm with no substrate, got %A{other}")

    [<Fact>]
    member _.``defaultSupportedEmission pins a Mirror to EmitReflectedOnly; a ThinFilm and a Plate emit both`` () : unit =
        // Spec 0046 (operator): locked R-only belongs to mirrors alone.
        Assert.Equal<Placement.Emission>(Placement.EmitReflectedOnly, Mirror.defaultSupportedEmission)
        Assert.Equal<Placement.Emission>(Placement.EmitBoth, ThinFilm.defaultSupportedEmission)
        Assert.Equal<Placement.Emission>(Placement.EmitBoth, ThickPlate.defaultSupportedEmission)

    [<Fact>]
    member _.``the seeded EUV multilayer is the one Mirror; seeded thin films and plates support both`` () : unit =
        Assert.Equal(Mirror, (BuiltinCatalogue.sample SampleIds.euvMoSiId).substrate)
        Assert.Equal<Placement.Emission>(Placement.EmitReflectedOnly, (BuiltinCatalogue.sample SampleIds.euvMoSiId).supportedEmission)
        let samples =
            [ SampleIds.glassFilm600Id; SampleIds.glassFilm200Id; SampleIds.multilayerQwId; SampleIds.glassPlate1mmId ]
            |> List.map BuiltinCatalogue.sample
        for s in samples do
            Assert.Equal<Placement.Emission>(Placement.EmitBoth, s.supportedEmission)
        for s in BuiltinCatalogue.samples () do
            match s.supportedEmission with
            | Placement.EmitReflectedOnly -> Assert.Equal(Mirror, s.substrate)
            | Placement.EmitBoth | Placement.EmitTransmittedOnly -> ()

    [<Fact>]
    member _.``a Mirror's supportedEmission is unclearable: every candidate collapses to EmitReflectedOnly`` () : unit =
        for candidate in [ Placement.EmitReflectedOnly; Placement.EmitTransmittedOnly; Placement.EmitBoth ] do
            Assert.Equal<Placement.Emission>(Placement.EmitReflectedOnly, constrainEmission Mirror candidate)
        let mirror = BuiltinCatalogue.sample SampleIds.euvMoSiId
        let clearedR = mirror.supportedEmission |> Placement.Emission.withReflected Placement.RayGroupOff
        let after = mirror |> withSupportedEmission clearedR
        Assert.Equal<Placement.Emission>(Placement.EmitReflectedOnly, after.supportedEmission)
        let film = BuiltinCatalogue.sample SampleIds.glassFilm600Id |> withSupportedEmission Placement.EmitTransmittedOnly
        Assert.Equal<Placement.Emission>(Placement.EmitTransmittedOnly, film.supportedEmission)

    [<Fact>]
    member _.``a Plate is constrainable: clearing R via Emission.withReflected Placement.RayGroupOff leaves T on`` () : unit =
        let plate = BuiltinCatalogue.sample SampleIds.glassPlate1mmId
        Assert.Equal<Placement.Emission>(Placement.EmitBoth, plate.supportedEmission)
        let clearedR = plate.supportedEmission |> Placement.Emission.withReflected Placement.RayGroupOff
        Assert.Equal<Placement.Emission>(Placement.EmitTransmittedOnly, clearedR)
        let tOnly = plate |> withSupportedEmission clearedR
        Assert.Equal<Placement.Emission>(Placement.EmitTransmittedOnly, tOnly.supportedEmission)
        let rOnly = plate |> withSupportedEmission (plate.supportedEmission |> Placement.Emission.withTransmitted Placement.RayGroupOff)
        Assert.Equal<Placement.Emission>(Placement.EmitReflectedOnly, rOnly.supportedEmission)

    [<Fact>]
    member _.``a STUB SampleProxy over the versioned surface exercises every function through its exact signature`` () : unit =
        let kept = plate
        let free = { plate with id = newSampleId (); name = EntryName "Unreferenced test plate" }
        let fixedSamples = [ kept; free ]
        let stub : SampleProxy =
            {
                listSamples = fun _ -> Ok fixedSamples
                searchSamples =
                    fun (q : SampleQuery) ->
                        Ok (fixedSamples |> List.filter (fun s -> s.name.value.IndexOf(q.text.value, StringComparison.OrdinalIgnoreCase) >= 0))
                tryGetSample = fun (id : SampleId) -> Ok (fixedSamples |> List.tryFind (fun s -> s.id = id))
                resolveVersion =
                    fun (svid : SampleVersionId) ->
                        Ok (fixedSamples |> List.tryFind (fun s -> s.id = svid.sampleId))
                saveSample =
                    fun (s : Sample) ->
                        if String.IsNullOrWhiteSpace s.name.value then Error (InvalidSampleErr "blank sample name") else Ok ()
                markSampleInactive = fun (_ : SampleId) -> Ok ()
                markSampleActive = fun (_ : SampleId) -> Ok ()
                supersedeSample = fun (_ : SampleId) -> Ok ()
                pinSampleVersion = fun _ -> Ok ()
                removeSample =
                    fun (id : SampleId) ->
                        if id = kept.id
                        then Error (SampleVersionPinnedErr ($"sample '%s{(string id.value)}' has a version pinned"))
                        else Ok ()
            }
        match stub.listSamples ActiveOnly with
        | Ok l -> Assert.Equal(2, List.length l)
        | Error err -> Assert.Fail($"%A{err}")
        match stub.searchSamples { SampleQuery.empty with text = NameFragment "unreferenced" } with
        | Ok [ hit ] -> Assert.Equal<SampleId>(free.id, hit.id)
        | other -> Assert.Fail($"expected exactly the unreferenced sample, got %A{other}")
        match stub.tryGetSample kept.id with
        | Ok (Some s) -> Assert.Equal<SampleId>(kept.id, s.id)
        | other -> Assert.Fail($"expected the kept sample, got %A{other}")
        match stub.resolveVersion (v1Id free.id) with
        | Ok (Some s) -> Assert.Equal<SampleId>(free.id, s.id)
        | other -> Assert.Fail($"expected the free sample by version, got %A{other}")
        match stub.saveSample { free with name = EntryName " " } with
        | Error (InvalidSampleErr _) -> ()
        | other -> Assert.Fail($"expected Error (InvalidSampleErr _), got %A{other}")
        match stub.saveSample free with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok (), got %A{other}")
        for verb in [ stub.markSampleInactive; stub.markSampleActive; stub.supersedeSample ] do
            match verb free.id with
            | Ok () -> ()
            | other -> Assert.Fail($"expected Ok (), got %A{other}")
        Assert.Equal<Result<unit, SampleError>>(Ok (), stub.pinSampleVersion (v1Id free.id))
        match stub.removeSample kept.id with
        | Error (SampleVersionPinnedErr reason) -> Assert.Contains(string kept.id.value, reason)
        | other -> Assert.Fail($"expected Error (SampleVersionPinnedErr _), got %A{other}")
        match stub.removeSample free.id with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok (), got %A{other}")

    [<Fact>]
    member _.``a sample layer keeps resolving its pinned material version after the material mints a newer one`` () : unit =
        let materials = BuiltinCatalogue.materialStore (fun _ -> [])
        let firstVersion = MaterialVersionId.firstOf MaterialIds.glass152
        Assert.Equal<Result<unit, MaterialError>>(Ok (), materials.pinMaterialVersion firstVersion)
        let glass = BuiltinCatalogue.materials () |> List.find (fun e -> e.id = MaterialIds.glass152)
        let other = BuiltinCatalogue.materials () |> List.find (fun e -> e.id = MaterialIds.glass200)
        let glassPhysicsChanged = { glass with complexity = other.complexity }
        match materials.saveMaterial glassPhysicsChanged with
        | Ok () -> ()
        | Error err -> Assert.Fail($"the mint failed: %A{err}")
        let pinned = MaterialVersionId.firstOf MaterialIds.glass152
        let sample : Sample =
            {
                BuiltinCatalogue.sample SampleIds.glassFilm200Id with
                    id = newSampleId ()
                    structure =
                        {
                            films = [ SingleLayer { materialId = pinned; thickness = Thickness.nm 100.0<nm>; orientation = PrimaryAxes } ]
                            substrate = None
                            lower = None
                        }
            }
        match Propagation.resolveSampleMaterials materials sample with
        | Ok _ -> ()
        | Error err -> Assert.Fail($"the pinned layer failed to resolve after the mint: %A{err}")
        match materials.resolveVersion pinned with
        | Ok (Some e) -> Assert.Equal(glass.complexity, e.complexity)
        | other -> Assert.Fail($"expected the pinned v1 to keep its original physics, got %A{other}")
        match materials.tryGetMaterial MaterialIds.glass152 with
        | Ok (Some e) -> Assert.Equal(other.complexity, e.complexity)
        | other -> Assert.Fail($"expected the latest (v2) to carry the new physics, got %A{other}")

type SampleProxySqliteTests () =
    inherit SampleProxyContract ()

    let databases = ResizeArray<LibraryDatabase>()

    member private _.freshDatabase () : LibraryDatabase =
        let database = BuiltinCatalogue.freshLibraryDatabase ()
        databases.Add database
        database

    override this.freshProxy () : SampleProxy =
        SampleProxy.createInDatabase (this.freshDatabase ())

    [<Fact>]
    member this.``a sample first saved through the database store is a user entry, and an edit keeps the stored provenance and keys the material versions of its own structure`` () : unit =
        let database = this.freshDatabase ()
        let proxy = SampleProxy.createInDatabase database
        let latestOf (sampleId : SampleId) : StoredEntryVersion =
            database.run (fun context -> context.tryLatestStored sampleId.toLibraryEntryId)
            |> Result.bind id
            |> Result.defaultWith (fun error -> failwith error.describe)
            |> Option.defaultWith (fun () -> failwith $"Missing sample {sampleId.value}")

        let storedVersion (version : SampleVersionId) : StoredEntryVersion =
            database.run (fun context -> context.tryStoredVersion version.sampleId.toLibraryEntryId version.version)
            |> Result.bind id
            |> Result.defaultWith (fun error -> failwith error.describe)
            |> Option.defaultWith (fun () -> failwith $"Missing sample {version.sampleId.value} v{version.version.value}")

        let materialVersions (version : SampleVersionId) : Set<MaterialVersionId> =
            database.run (fun context ->
                query
                    {
                        for row in context.SampleMaterials do
                        where (row.SampleId = version.sampleId.value && row.SampleVersion = version.version.value)
                        select row
                    }
                |> Seq.map (fun row ->
                    {
                        materialId = MaterialId row.MaterialId
                        version = VersionNumber row.MaterialVersion
                    })
                |> Set.ofSeq)
            |> Result.defaultWith (fun error -> failwith error.describe)

        let entry = minted ()
        Assert.Equal<Result<unit, SampleError>>(Ok (), proxy.saveSample entry)
        let firstUser = latestOf entry.id
        Assert.Equal(VersionNumber.first, firstUser.envelope.content.version)
        Assert.Equal(UserProvenance, firstUser.envelope.content.provenance)
        Assert.Equal(VersionUnpinned, firstUser.state.pin)
        Assert.Equal(ActiveEntry, firstUser.state.lifecycle)
        Assert.Equal<Result<Sample, LibraryEntryError>>(Ok entry, firstUser.envelope.content.toSample)
        let userVersion = SampleVersionId.firstOf entry.id
        Assert.Equal<Set<MaterialVersionId>>(entry.structure.referencedMaterialVersions, materialVersions userVersion)

        let multilayer = BuiltinCatalogue.sample SampleIds.multilayerQwId
        let renamed =
            {
                multilayer with name = EntryName "Renamed multilayer"
            }
        let builtInProvenance = BuiltInProvenance CatalogueIds.builtInCatalogue
        Assert.Equal<Result<unit, SampleError>>(Ok (), proxy.saveSample renamed)
        let edited = latestOf multilayer.id
        Assert.Equal(VersionNumber.first, edited.envelope.content.version)
        Assert.Equal(renamed.name, edited.envelope.content.name)
        Assert.Equal(builtInProvenance, edited.envelope.content.provenance)

        let first = SampleVersionId.firstOf multilayer.id
        let second : SampleVersionId =
            {
                first with version = first.version.next
            }
        let changed =
            {
                renamed with structure = (BuiltinCatalogue.sample SampleIds.langasiteSiliconId).structure
            }
        Assert.Equal<Result<unit, SampleError>>(Ok (), proxy.pinSampleVersion first)
        Assert.Equal<Result<unit, SampleError>>(Ok (), proxy.saveSample changed)
        let next = latestOf multilayer.id
        Assert.Equal(second.version, next.envelope.content.version)
        Assert.Equal(builtInProvenance, next.envelope.content.provenance)
        Assert.Equal(VersionUnpinned, next.state.pin)
        Assert.Equal(ActiveEntry, next.state.lifecycle)
        Assert.Equal<Result<Sample, LibraryEntryError>>(Ok changed, next.envelope.content.toSample)
        let frozen = storedVersion first
        Assert.Equal(VersionPinned, frozen.state.pin)
        Assert.Equal(builtInProvenance, frozen.envelope.content.provenance)
        Assert.Equal<Result<Sample, LibraryEntryError>>(Ok renamed, frozen.envelope.content.toSample)
        let originalMaterials =
            [ MaterialVersionId.firstOf MaterialIds.glass152; MaterialVersionId.firstOf MaterialIds.vacuumMaterial ]
            |> Set.ofList
        let changedMaterials =
            [ MaterialVersionId.firstOf MaterialIds.langasite; MaterialVersionId.firstOf MaterialIds.silicon ]
            |> Set.ofList
        Assert.Equal<Set<MaterialVersionId>>(originalMaterials, materialVersions first)
        Assert.Equal<Set<MaterialVersionId>>(changedMaterials, materialVersions second)

        let mutatedUser =
            {
                entry with
                    structure = changed.structure
                    substrate = changed.substrate
            }
        Assert.Equal<Result<unit, SampleError>>(Ok (), proxy.saveSample mutatedUser)
        let mutated = latestOf entry.id
        Assert.Equal(VersionNumber.first, mutated.envelope.content.version)
        Assert.Equal(UserProvenance, mutated.envelope.content.provenance)
        Assert.Equal<Set<MaterialVersionId>>(changedMaterials, materialVersions userVersion)
        Assert.Equal<Result<unit, SampleError>>(Ok (), proxy.removeSample entry.id)
        Assert.Empty(materialVersions userVersion)
        Assert.Equal<Result<Sample option, SampleError>>(Ok None, proxy.resolveVersion userVersion)

        let collisionGuid = Guid.NewGuid()
        let materialId = MaterialId collisionGuid
        let sampleId = SampleId collisionGuid
        let material =
            {
                BuiltinCatalogue.material MaterialIds.glass152 with id = materialId
            }
        let collision =
            {
                plate with id = sampleId
            }
        let materials = MaterialProxy.createInDatabase database
        Assert.Equal<Result<unit, MaterialError>>(Ok (), materials.saveMaterial material)
        match proxy.saveSample collision with
        | Error (DuplicateSampleIdErr reason) ->
            Assert.Contains(string sampleId.value, reason)
            Assert.Contains(".material-json", reason)
        | other -> Assert.Fail($"Expected a cross-kind duplicate refusal, got %A{other}")
        let collisionVersion = SampleVersionId.firstOf sampleId
        Assert.Equal<Result<Sample option, SampleError>>(Ok None, proxy.tryGetSample sampleId)
        Assert.Equal<Result<Sample option, SampleError>>(Ok None, proxy.resolveVersion collisionVersion)
        for verb in [ proxy.markSampleInactive; proxy.markSampleActive; proxy.supersedeSample; proxy.removeSample ] do
            match verb sampleId with
            | Error (UnknownSampleIdErr reason) -> Assert.Contains(string sampleId.value, reason)
            | other -> Assert.Fail($"Expected a material id to be no sample, got %A{other}")
        match proxy.pinSampleVersion collisionVersion with
        | Error (UnknownSampleIdErr reason) -> Assert.Contains(string sampleId.value, reason)
        | other -> Assert.Fail($"Expected a material version to be no sample version, got %A{other}")
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some material), materials.tryGetMaterial materialId)

    [<Fact>]
    member this.``a database failure below the samples store is SampleStoreUnavailableErr, never a throw`` () : unit =
        let database = this.freshDatabase ()
        let proxy = SampleProxy.createInDatabase database
        (database :> IDisposable).Dispose()

        let unavailable (result : Result<'Outcome, SampleError>) : unit =
            match result with
            | Error (SampleStoreUnavailableErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
            | other -> Assert.Fail($"Expected a typed database failure, got %A{other}")

        let version = SampleVersionId.firstOf plate.id
        proxy.listSamples ActiveOnly |> unavailable
        proxy.searchSamples SampleQuery.empty |> unavailable
        proxy.tryGetSample plate.id |> unavailable
        proxy.resolveVersion version |> unavailable
        proxy.saveSample plate |> unavailable
        proxy.markSampleInactive plate.id |> unavailable
        proxy.markSampleActive plate.id |> unavailable
        proxy.supersedeSample plate.id |> unavailable
        proxy.pinSampleVersion version |> unavailable
        proxy.removeSample plate.id |> unavailable

    interface IDisposable with
        member _.Dispose () : unit =
            for database in databases do
                (database :> IDisposable).Dispose()
