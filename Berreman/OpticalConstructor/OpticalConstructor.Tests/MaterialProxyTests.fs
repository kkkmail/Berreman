namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Media
open Berreman.MaterialProperties
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialStore
open OpticalConstructor.Domain.SampleStore
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.LibraryDatabaseStore
open OpticalConstructor.Storage.MaterialDatabaseStore

/// One crystal of each record, shared by this project's crystal-parameter material facts (spec 0050 S11.1).
/// They live here because this file compiles first among those facts.
module CrystalMaterialFixtures =

    let uniaxialParameters : UniaxialParameters =
        {
            ordinaryIndex = RefractionIndex 1.544
            extraordinaryIndex = RefractionIndex 1.553
            g11 = RhoValue 3.0e-5
            g33 = RhoValue (-1.0e-5)
        }

    let triclinicParameters : TriclinicParameters =
        {
            index1 = RefractionIndex 1.52
            index2 = RefractionIndex 1.56
            index3 = RefractionIndex 1.61
            g11 = RhoValue 3.0e-5
            g22 = RhoValue 4.0e-5
            g33 = RhoValue 5.0e-5
            g23 = RhoValue 1.0e-5
            g13 = RhoValue 2.0e-5
            g12 = RhoValue 6.0e-6
        }

    /// Fifteen distinct values, so a field read from the wrong slot shows.
    let absorbingTriclinicParameters : AbsorbingTriclinicParameters =
        {
            index1 = RefractionIndex 1.71
            index2 = RefractionIndex 1.74
            index3 = RefractionIndex 1.78
            epsIm11 = EpsValue 2.0e-3
            epsIm22 = EpsValue 3.0e-3
            epsIm33 = EpsValue 4.0e-3
            epsIm23 = EpsValue 5.0e-4
            epsIm13 = EpsValue 6.0e-4
            epsIm12 = EpsValue 7.0e-4
            g11 = RhoValue 3.0e-5
            g22 = RhoValue 4.0e-5
            g33 = RhoValue 5.0e-5
            g23 = RhoValue 1.0e-5
            g13 = RhoValue 2.0e-5
            g12 = RhoValue 6.0e-6
        }

    /// The three crystals in case order: uniaxial, triclinic, absorbing triclinic.
    let crystals : CrystalParameters list =
        [
            UniaxialCrystal uniaxialParameters
            TriclinicCrystal triclinicParameters
            AbsorbingTriclinicCrystal absorbingTriclinicParameters
        ]

open CrystalMaterialFixtures

module private MaterialProxyTestData =

    let glass : MaterialEntry =
        BuiltinCatalogue.material MaterialIds.glass152

    let otherPhysics : MaterialEntry =
        BuiltinCatalogue.material MaterialIds.glass200

    let glassPhysicsChanged : MaterialEntry =
        { glass with physics = otherPhysics.physics }

    let glassRenamed : MaterialEntry =
        { glass with name = EntryName "Renamed glass (metadata only)" }

    let v1Id (id : MaterialId) : MaterialVersionId = { materialId = id; version = VersionNumber.first }
    let v2Id (id : MaterialId) : MaterialVersionId = { materialId = id; version = VersionNumber.first.next }

    let minted () : MaterialEntry =
        { glass with id = newMaterialId (); name = EntryName "Test glass (minted)" }

open MaterialProxyTestData

type ReferencingSamples =
    {
        activeSampleIds : unit -> SampleId list
        removeSample : SampleId -> unit
    }

/// Material stores preserve version history, lifecycle visibility and sample/version removal guards.
[<AbstractClass>]
type MaterialProxyContract () =

    abstract freshProxy : unit -> MaterialProxy
    abstract composedProxies : unit -> ReferencingSamples * MaterialProxy

    member private this.proxyWithPinned (versions : MaterialVersionId list) : MaterialProxy =
        let proxy = this.freshProxy ()
        for version in versions do
            Assert.Equal<Result<unit, MaterialError>>(Ok (), proxy.pinMaterialVersion version)
        proxy

    [<Fact>]
    member this.``listMaterials ActiveOnly returns the seeded built-in entries with distinct ids`` () : unit =
        match (this.freshProxy ()).listMaterials ActiveOnly with
        | Ok entries ->
            Assert.NotEmpty entries
            Assert.Equal(List.length (BuiltinCatalogue.materials ()), List.length entries)
            let ids = entries |> List.map (fun e -> e.id)
            Assert.Equal(List.length ids, ids |> List.distinct |> List.length)
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    member this.``searchMaterials with the empty query matches every seeded entry`` () : unit =
        match (this.freshProxy ()).searchMaterials MaterialQuery.empty with
        | Ok entries -> Assert.Equal(List.length (BuiltinCatalogue.materials ()), List.length entries)
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    member this.``searchMaterials filters by name fragment case-insensitively`` () : unit =
        match (this.freshProxy ()).searchMaterials { MaterialQuery.empty with text = NameFragment "GLASS" } with
        | Ok entries ->
            Assert.NotEmpty entries
            Assert.All(entries, fun e ->
                Assert.True(e.name.value.IndexOf("glass", StringComparison.OrdinalIgnoreCase) >= 0, e.name.value))
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    member this.``searchMaterials dispersion facets partition the library`` () : unit =
        let proxy = this.freshProxy ()
        match proxy.searchMaterials { MaterialQuery.empty with dispersion = OnlyDispersive } with
        | Ok dispersive ->
            let ids = dispersive |> List.map (fun e -> e.id) |> Set.ofList
            Assert.Equal<Set<MaterialId>>(Set.ofList [ MaterialIds.silicon; MaterialIds.langasite ], ids)
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.searchMaterials { MaterialQuery.empty with dispersion = OnlyNonDispersive } with
        | Ok nonDispersive -> Assert.Equal(List.length (BuiltinCatalogue.materials ()) - 2, List.length nonDispersive)
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    member this.``tryGetMaterial hits a known id and misses a minted one`` () : unit =
        let proxy = this.freshProxy ()
        match proxy.tryGetMaterial MaterialIds.silicon with
        | Ok (Some e) -> Assert.Equal<MaterialId>(MaterialIds.silicon, e.id)
        | other -> Assert.Fail($"expected the silicon entry, got %A{other}")
        match proxy.tryGetMaterial (newMaterialId ()) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None, got %A{other}")

    [<Fact>]
    member this.``resolveVersion hits version 1 of a seed, misses a non-existent version, and misses an unknown id`` () : unit =
        let proxy = this.freshProxy ()
        match proxy.resolveVersion (v1Id MaterialIds.glass152) with
        | Ok (Some e) -> Assert.Equal<MaterialId>(MaterialIds.glass152, e.id)
        | other -> Assert.Fail($"expected version 1 of glass152, got %A{other}")
        match proxy.resolveVersion (v2Id MaterialIds.glass152) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None for a non-existent version 2, got %A{other}")
        match proxy.resolveVersion (v1Id (newMaterialId ())) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None for an unknown id, got %A{other}")

    [<Fact>]
    member this.``saveMaterial on a new id inserts version 1 active — appears in listing and resolves at v1`` () : unit =
        let proxy = this.freshProxy ()
        let entry = minted ()
        match proxy.saveMaterial entry with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listMaterials ActiveOnly with
        | Ok entries ->
            Assert.Equal(List.length (BuiltinCatalogue.materials ()) + 1, List.length entries)
            Assert.Contains(entry.id, entries |> List.map (fun e -> e.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.resolveVersion (v1Id entry.id) with
        | Ok (Some e) -> Assert.Equal<MaterialId>(entry.id, e.id)
        | other -> Assert.Fail($"expected version 1 of the minted entry, got %A{other}")

    [<Fact>]
    member this.``saveMaterial rejects a blank name as InvalidMaterial and persists nothing`` () : unit =
        let proxy = this.freshProxy ()
        let blank = { minted () with name = EntryName "   " }
        match proxy.saveMaterial blank with
        | Error (InvalidMaterialErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidMaterialErr _), got %A{other}")
        match proxy.tryGetMaterial blank.id with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None (nothing persisted), got %A{other}")

    [<Fact>]
    member this.``KeepCurrent — an identical save on a pinned version leaves exactly version 1 unchanged`` () : unit =
        let proxy = this.proxyWithPinned [ v1Id MaterialIds.glass152 ]
        match proxy.saveMaterial glass with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.resolveVersion (v2Id MaterialIds.glass152) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected no version 2 after a no-op save, got %A{other}")
        match proxy.resolveVersion (v1Id MaterialIds.glass152) with
        | Ok (Some e) -> Assert.Equal(glass.physics, e.physics)
        | other -> Assert.Fail($"expected version 1 intact, got %A{other}")

    [<Fact>]
    member this.``MutateInPlace — a metadata-only edit of a PINNED version updates version 1 and does NOT mint`` () : unit =
        let proxy = this.proxyWithPinned [ v1Id MaterialIds.glass152 ]
        match proxy.saveMaterial glassRenamed with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.resolveVersion (v2Id MaterialIds.glass152) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected NO version 2 from a metadata-only edit, got %A{other}")
        match proxy.resolveVersion (v1Id MaterialIds.glass152) with
        | Ok (Some e) -> Assert.Equal(glassRenamed.name, e.name)
        | other -> Assert.Fail($"expected version 1 renamed in place, got %A{other}")

    [<Fact>]
    member this.``MutateInPlace — a physics change of an UNPINNED version updates version 1 in place`` () : unit =
        let proxy = this.freshProxy ()
        match proxy.saveMaterial glassPhysicsChanged with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok None, proxy.resolveVersion (v2Id MaterialIds.glass152))
        match proxy.resolveVersion (v1Id MaterialIds.glass152) with
        | Ok (Some e) -> Assert.Equal(otherPhysics.physics, e.physics)
        | other -> Assert.Fail($"expected version 1 physics mutated in place, got %A{other}")

    [<Fact>]
    member this.``MintNextVersion — a physics change of a PINNED version freezes version 1 and mints version 2`` () : unit =
        let proxy = this.proxyWithPinned [ v1Id MaterialIds.glass152 ]
        match proxy.saveMaterial glassPhysicsChanged with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.resolveVersion (v1Id MaterialIds.glass152) with
        | Ok (Some e) -> Assert.Equal(glass.physics, e.physics)
        | other -> Assert.Fail($"expected version 1 frozen with original physics, got %A{other}")
        match proxy.resolveVersion (v2Id MaterialIds.glass152) with
        | Ok (Some e) -> Assert.Equal(otherPhysics.physics, e.physics)
        | other -> Assert.Fail($"expected version 2 with the new physics, got %A{other}")
        match proxy.tryGetMaterial MaterialIds.glass152 with
        | Ok (Some e) -> Assert.Equal(otherPhysics.physics, e.physics)
        | other -> Assert.Fail($"expected the latest entry to be version 2, got %A{other}")

    [<Fact>]
    member this.``latest-active default listing shows one entry per material after a mint, not the frozen history`` () : unit =
        let proxy = this.proxyWithPinned [ v1Id MaterialIds.glass152 ]
        match proxy.saveMaterial glassPhysicsChanged with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listMaterials ActiveOnly with
        | Ok entries ->
            Assert.Equal(List.length (BuiltinCatalogue.materials ()), List.length entries)
            let glassEntries = entries |> List.filter (fun e -> e.id = MaterialIds.glass152)
            Assert.Equal(1, List.length glassEntries)
            Assert.Equal(otherPhysics.physics, glassEntries.Head.physics)
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    member this.``markMaterialInactive hides an entry from the offers yet keeps it resolvable, and markMaterialActive revives it`` () : unit =
        let proxy = this.freshProxy ()
        match proxy.markMaterialInactive MaterialIds.glass152 with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listMaterials ActiveOnly with
        | Ok entries -> Assert.DoesNotContain(MaterialIds.glass152, entries |> List.map (fun e -> e.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.searchMaterials { MaterialQuery.empty with text = NameFragment glass.name.value } with
        | Ok entries -> Assert.DoesNotContain(MaterialIds.glass152, entries |> List.map (fun e -> e.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listMaterials IncludeInactive with
        | Ok entries -> Assert.Contains(MaterialIds.glass152, entries |> List.map (fun e -> e.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.resolveVersion (v1Id MaterialIds.glass152) with
        | Ok (Some _) -> ()
        | other -> Assert.Fail($"expected the inactive version to resolve, got %A{other}")
        match proxy.markMaterialActive MaterialIds.glass152 with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listMaterials ActiveOnly with
        | Ok entries -> Assert.Contains(MaterialIds.glass152, entries |> List.map (fun e -> e.id))
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    member this.``supersedeMaterial behaves as inactive — hidden from offers, present under include-inactive, resolvable by version`` () : unit =
        let proxy = this.freshProxy ()
        match proxy.supersedeMaterial MaterialIds.glass152 with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listMaterials ActiveOnly with
        | Ok entries -> Assert.DoesNotContain(MaterialIds.glass152, entries |> List.map (fun e -> e.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listMaterials IncludeInactive with
        | Ok entries -> Assert.Contains(MaterialIds.glass152, entries |> List.map (fun e -> e.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.resolveVersion (v1Id MaterialIds.glass152) with
        | Ok (Some _) -> ()
        | other -> Assert.Fail($"expected the superseded version to resolve, got %A{other}")

    [<Fact>]
    member this.``the lifecycle verbs reject an unknown id with a diagnostic reason`` () : unit =
        let proxy = this.freshProxy ()
        let missing = (minted ()).id
        for verb in [ proxy.markMaterialInactive; proxy.markMaterialActive; proxy.supersedeMaterial ] do
            match verb missing with
            | Error (UnknownMaterialIdErr reason) -> Assert.Contains(string missing.value, reason)
            | other -> Assert.Fail($"expected Error (UnknownMaterialIdErr _), got %A{other}")

    [<Fact>]
    member this.``removeMaterial on a material referenced by seeded samples is blocked and leaves the store unchanged`` () : unit =
        let samples, proxy = this.composedProxies ()
        let pinned = MaterialVersionId.firstOf MaterialIds.glass152
        Assert.Equal<Result<unit, MaterialError>>(Ok (), proxy.pinMaterialVersion pinned)
        match proxy.removeMaterial MaterialIds.glass152 with
        | Error (MaterialStillReferencedErr reason) ->
            Assert.Contains((BuiltinCatalogue.sample SampleIds.glassPlate1mmId).name.value, reason)
            Assert.Contains((BuiltinCatalogue.sample SampleIds.multilayerQwId).name.value, reason)
        | other -> Assert.Fail($"expected Error (MaterialStillReferencedErr _), got %A{other}")
        match proxy.tryGetMaterial MaterialIds.glass152 with
        | Ok (Some _) -> ()
        | other -> Assert.Fail($"expected the glass entry to survive, got %A{other}")
        let activeSampleIds = samples.activeSampleIds ()
        Assert.Contains(SampleIds.glassPlate1mmId, activeSampleIds)
        Assert.Contains(SampleIds.multilayerQwId, activeSampleIds)

    [<Fact>]
    member this.``removeMaterial on a material with a pinned version is blocked with a typed refusal naming the version`` () : unit =
        let proxy = this.proxyWithPinned [ v1Id MaterialIds.glass200 ]
        match proxy.removeMaterial MaterialIds.glass200 with
        | Error (MaterialVersionPinnedErr reason) ->
            Assert.Contains(string MaterialIds.glass200.value, reason)
            Assert.Contains("v1", reason)
        | other -> Assert.Fail($"expected Error (MaterialVersionPinnedErr _), got %A{other}")
        match proxy.tryGetMaterial MaterialIds.glass200 with
        | Ok (Some _) -> ()
        | other -> Assert.Fail($"expected the entry to survive the pinned-version block, got %A{other}")

    [<Fact>]
    member this.``removeMaterial succeeds when the material is neither sample-referenced nor pinned`` () : unit =
        let _, proxy = this.composedProxies ()
        match proxy.removeMaterial MaterialIds.glass200 with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.tryGetMaterial MaterialIds.glass200 with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None after removal, got %A{other}")
        match proxy.removeMaterial MaterialIds.glass200 with
        | Error (UnknownMaterialIdErr reason) -> Assert.Contains(string MaterialIds.glass200.value, reason)
        | other -> Assert.Fail($"expected Error (UnknownMaterialIdErr _), got %A{other}")

    [<Fact>]
    member this.``the referencing lookup is live — removing the referencing sample unblocks the material`` () : unit =
        let samples, proxy = this.composedProxies ()
        match proxy.removeMaterial MaterialIds.glass175 with
        | Error (MaterialStillReferencedErr reason) -> Assert.Contains((BuiltinCatalogue.sample SampleIds.glassFilm600Id).name.value, reason)
        | other -> Assert.Fail($"expected Error (MaterialStillReferencedErr _), got %A{other}")
        samples.removeSample SampleIds.glassFilm600Id
        match proxy.removeMaterial MaterialIds.glass175 with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () once unreferenced, got %A{other}")

    [<Fact>]
    member this.``a MaterialProxy compares by reference (the Elmish-required equality)`` () : unit =
        let p = this.freshProxy ()
        let same = p
        Assert.True((p = same))
        Assert.False((p = this.freshProxy ()))

    [<Fact>]
    member this.``pinMaterialVersion pins one version, a minted version starts unpinned, and an unknown version is refused`` () : unit =
        let proxy = this.freshProxy ()
        let first = MaterialVersionId.firstOf MaterialIds.glass152
        let second : MaterialVersionId =
            {
                first with version = first.version.next
            }
        let third : MaterialVersionId =
            {
                first with version = second.version.next
            }
        let missingId = newMaterialId ()
        let missing = MaterialVersionId.firstOf missingId

        Assert.Equal<Result<unit, MaterialError>>(Ok (), proxy.pinMaterialVersion first)
        Assert.Equal<Result<unit, MaterialError>>(Ok (), proxy.pinMaterialVersion first)
        Assert.Equal<Result<unit, MaterialError>>(Ok (), proxy.saveMaterial glass)
        Assert.Equal<Result<unit, MaterialError>>(Ok (), proxy.saveMaterial glassRenamed)
        for verb in [ proxy.markMaterialInactive; proxy.markMaterialActive; proxy.supersedeMaterial ] do
            Assert.Equal<Result<unit, MaterialError>>(Ok (), verb glass.id)
            match proxy.removeMaterial glass.id with
            | Error (MaterialVersionPinnedErr reason) -> Assert.Contains("v1", reason)
            | other -> Assert.Fail($"expected the pin to survive metadata and lifecycle edits, got %A{other}")

        Assert.Equal<Result<unit, MaterialError>>(Ok (), proxy.saveMaterial glassPhysicsChanged)
        match proxy.resolveVersion second with
        | Ok (Some entry) -> Assert.Equal(glassPhysicsChanged.physics, entry.physics)
        | other -> Assert.Fail($"expected a minted version 2, got %A{other}")
        Assert.Equal<Result<unit, MaterialError>>(Ok (), proxy.saveMaterial glass)
        match proxy.resolveVersion second with
        | Ok (Some entry) -> Assert.Equal(glass.physics, entry.physics)
        | other -> Assert.Fail($"expected version 2 to mutate in place, got %A{other}")
        match proxy.resolveVersion third with
        | Ok None -> ()
        | other -> Assert.Fail($"expected no version 3, got %A{other}")
        match proxy.resolveVersion first with
        | Ok (Some entry) -> Assert.Equal(glass.physics, entry.physics)
        | other -> Assert.Fail($"expected version 1 to keep its original physics, got %A{other}")

        for version in [ missing; third ] do
            match proxy.pinMaterialVersion version with
            | Error (UnknownMaterialIdErr reason) ->
                Assert.Contains(string version.materialId.value, reason)
                Assert.Contains($"v{version.version.value}", reason)
            | other -> Assert.Fail($"expected an unknown-version refusal, got %A{other}")

        Assert.Equal<Result<unit, MaterialError>>(Ok (), proxy.pinMaterialVersion second)
        match proxy.removeMaterial glass.id with
        | Error (MaterialVersionPinnedErr reason) ->
            Assert.Contains(glass.name.value, reason)
            Assert.Contains(string glass.id.value, reason)
            Assert.Contains("2 version(s) pinned: v1, v2", reason)
        | other -> Assert.Fail($"expected both pinned versions to block removal, got %A{other}")

    [<Fact>]
    member this.``a crystal-parameter material is stored and resolved at its version by its physics`` () : unit =
        // Spec 0050 S11.1: a crystal physics is a material version's physics like any complexity.
        let proxy = this.freshProxy ()
        let crystal = { minted () with physics = CrystalParameterPhysics (AbsorbingTriclinicCrystal absorbingTriclinicParameters) }
        Assert.Equal<Result<unit, MaterialError>>(Ok (), proxy.saveMaterial crystal)
        match proxy.resolveVersion (v1Id crystal.id) with
        | Ok (Some entry) -> Assert.Equal<MaterialEntry>(crystal, entry)
        | other -> Assert.Fail($"expected version 1 of the crystal material, got %A{other}")
        match proxy.listMaterials ActiveOnly with
        | Ok entries -> Assert.Contains(crystal, entries)
        | Error err -> Assert.Fail($"%A{err}")

type MaterialProxyTests () =
    inherit MaterialProxyContract ()

    override _.freshProxy () : MaterialProxy =
        BuiltinCatalogue.materialStore (fun _ -> [])

    override _.composedProxies () : ReferencingSamples * MaterialProxy =
        let samples = BuiltinCatalogue.sampleStore ()
        let referencingSamples : ReferencingSamples =
            {
                activeSampleIds =
                    fun () ->
                        match samples.listSamples ActiveOnly with
                        | Ok entries -> entries |> List.map (fun sample -> sample.id)
                        | Error err -> failwith $"%A{err}"
                removeSample =
                    fun id ->
                        match samples.removeSample id with
                        | Ok () -> ()
                        | Error err -> Assert.Fail($"%A{err}")
            }
        referencingSamples, BuiltinCatalogue.materialStore (samplesReferencing samples)

    [<Fact>]
    member _.``a STUB MaterialProxy over the versioned surface exercises every function through its exact signature`` () : unit =
        let referenced = glass
        let free = { glass with id = newMaterialId (); name = EntryName "Unreferenced test glass" }
        let fixedEntries = [ referenced; free ]
        let stub : MaterialProxy =
            {
                listMaterials = fun _ -> Ok fixedEntries
                searchMaterials =
                    fun (q : MaterialQuery) ->
                        Ok (fixedEntries |> List.filter (fun e -> e.name.value.IndexOf(q.text.value, StringComparison.OrdinalIgnoreCase) >= 0))
                tryGetMaterial = fun (id : MaterialId) -> Ok (fixedEntries |> List.tryFind (fun e -> e.id = id))
                resolveVersion =
                    fun (mvid : MaterialVersionId) ->
                        Ok (fixedEntries |> List.tryFind (fun e -> e.id = mvid.materialId))
                saveMaterial =
                    fun (e : MaterialEntry) ->
                        if String.IsNullOrWhiteSpace e.name.value then Error (InvalidMaterialErr "blank material name") else Ok ()
                markMaterialInactive = fun (_ : MaterialId) -> Ok ()
                markMaterialActive = fun (_ : MaterialId) -> Ok ()
                supersedeMaterial = fun (_ : MaterialId) -> Ok ()
                pinMaterialVersion = fun _ -> Ok ()
                removeMaterial =
                    fun (id : MaterialId) ->
                        if id = referenced.id
                        then Error (MaterialStillReferencedErr ($"material '%s{(string id.value)}' is still referenced by a sample structure"))
                        else Ok ()
            }
        match stub.listMaterials ActiveOnly with
        | Ok l -> Assert.Equal(2, List.length l)
        | Error err -> Assert.Fail($"%A{err}")
        match stub.searchMaterials { MaterialQuery.empty with text = NameFragment "unreferenced" } with
        | Ok [ hit ] -> Assert.Equal<MaterialId>(free.id, hit.id)
        | other -> Assert.Fail($"expected exactly the unreferenced entry, got %A{other}")
        match stub.tryGetMaterial referenced.id with
        | Ok (Some e) -> Assert.Equal<MaterialId>(referenced.id, e.id)
        | other -> Assert.Fail($"expected the referenced entry, got %A{other}")
        match stub.resolveVersion (v1Id free.id) with
        | Ok (Some e) -> Assert.Equal<MaterialId>(free.id, e.id)
        | other -> Assert.Fail($"expected the free entry by version, got %A{other}")
        match stub.saveMaterial { free with name = EntryName " " } with
        | Error (InvalidMaterialErr _) -> ()
        | other -> Assert.Fail($"expected Error (InvalidMaterialErr _), got %A{other}")
        match stub.saveMaterial free with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok (), got %A{other}")
        for verb in [ stub.markMaterialInactive; stub.markMaterialActive; stub.supersedeMaterial ] do
            match verb free.id with
            | Ok () -> ()
            | other -> Assert.Fail($"expected Ok (), got %A{other}")
        Assert.Equal<Result<unit, MaterialError>>(Ok (), stub.pinMaterialVersion (v1Id free.id))
        match stub.removeMaterial referenced.id with
        | Error (MaterialStillReferencedErr reason) -> Assert.Contains(string referenced.id.value, reason)
        | other -> Assert.Fail($"expected Error (MaterialStillReferencedErr _), got %A{other}")
        match stub.removeMaterial free.id with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok (), got %A{other}")

    [<Fact>]
    member _.``referencedMaterialVersions keeps every referenced material version, a repeated cell once`` () : unit =
        Assert.Equal<Set<MaterialVersionId>>(
            Set.ofList
                [ MaterialVersionId.firstOf MaterialIds.glass152
                  MaterialVersionId.firstOf MaterialIds.vacuumMaterial ],
            (BuiltinCatalogue.sample SampleIds.multilayerQwId).structure.referencedMaterialVersions)
        Assert.Equal<Set<MaterialVersionId>>(
            Set.ofList
                [ MaterialVersionId.firstOf MaterialIds.langasite
                  MaterialVersionId.firstOf MaterialIds.silicon ],
            (BuiltinCatalogue.sample SampleIds.langasiteSiliconId).structure.referencedMaterialVersions)

        let glassFirst = MaterialVersionId.firstOf MaterialIds.glass152
        let glassNext : MaterialVersionId =
            {
                materialId = MaterialIds.glass152
                version = VersionNumber.first.next
            }
        let film : SampleLayer =
            {
                materialId = glassNext
                thickness = Thickness.nm 100.0<nm>
                orientation = PrimaryAxes
            }
        let structure : SampleStructure =
            {
                films = [ SingleLayer film ]
                substrate = None
                lower = Some glassFirst
            }
        Assert.Equal<Set<MaterialVersionId>>(
            Set.ofList [ glassFirst; glassNext ],
            structure.referencedMaterialVersions)
        Assert.Equal<Set<MaterialId>>(Set.singleton MaterialIds.glass152, structure.referencedMaterials)

    [<Fact>]
    member _.``referencedMaterials covers films, repeated cells, the substrate plate, and the lower half-space`` () : unit =
        Assert.Equal<Set<MaterialId>>(
            Set.ofList [ MaterialIds.glass152 ],
            (BuiltinCatalogue.sample SampleIds.glassFilm200Id).structure.referencedMaterials)
        Assert.Equal<Set<MaterialId>>(
            Set.ofList [ MaterialIds.euvMolybdenum; MaterialIds.euvSilicon ],
            (BuiltinCatalogue.sample SampleIds.euvMoSiId).structure.referencedMaterials)
        Assert.Equal<Set<MaterialId>>(
            Set.ofList [ MaterialIds.glass152 ],
            (BuiltinCatalogue.sample SampleIds.glassPlate1mmId).structure.referencedMaterials)
        Assert.Equal<Set<MaterialId>>(
            Set.ofList [ MaterialIds.langasite; MaterialIds.silicon ],
            (BuiltinCatalogue.sample SampleIds.langasiteSiliconId).structure.referencedMaterials)

type MaterialProxySqliteTests () =
    inherit MaterialProxyContract ()

    let databases = ResizeArray<LibraryDatabase>()

    member private _.freshDatabase () : LibraryDatabase =
        let database = BuiltinCatalogue.freshLibraryDatabase ()
        databases.Add database
        database

    member private _.referencingSamples (database : LibraryDatabase) : ReferencingSamples =
        {
            activeSampleIds =
                fun () ->
                    database.run (fun context ->
                        context.latestStoredOfKind SampleEntryKind
                        |> Result.bind (fun versions ->
                            versions
                            |> List.filter (fun stored -> stored.state.lifecycle = ActiveEntry)
                            |> List.fold (fun samples stored ->
                                samples
                                |> Result.bind (fun samples ->
                                    stored.envelope.content.toSample
                                    |> Result.mapError (fun error -> LibraryRowInvalidErr error.describe)
                                    |> Result.map (fun sample -> sample.id :: samples))) (Ok [])))
                    |> Result.bind id
                    |> Result.defaultWith (fun error -> failwith error.describe)
            removeSample =
                fun sampleId ->
                    database.run (fun context ->
                        context.stageRemoval sampleId.toLibraryEntryId
                        context.SaveChanges() |> ignore)
                    |> Result.defaultWith (fun error -> failwith error.describe)
        }

    override this.freshProxy () : MaterialProxy =
        let database = this.freshDatabase ()
        // Spec 0050 S2.2: match the contract's reference-free factory; composedProxies retains samples.
        database.run (fun context ->
            context.latestStoredOfKind SampleEntryKind
            |> Result.map (fun samples ->
                for sample in samples do
                    context.stageRemoval sample.envelope.content.id
                context.SaveChanges() |> ignore))
        |> Result.bind id
        |> Result.defaultWith (fun error -> failwith error.describe)
        MaterialProxy.createInDatabase database

    override this.composedProxies () : ReferencingSamples * MaterialProxy =
        let database = this.freshDatabase ()
        this.referencingSamples database, MaterialProxy.createInDatabase database

    [<Fact>]
    member this.``a material first saved through the database store is a user entry, and an edit keeps the stored provenance`` () : unit =
        let database = this.freshDatabase ()
        let proxy = MaterialProxy.createInDatabase database
        let latestOf (materialId : MaterialId) : StoredEntryVersion =
            database.run (fun context -> context.tryLatestStored materialId.toLibraryEntryId)
            |> Result.bind id
            |> Result.defaultWith (fun error -> failwith error.describe)
            |> Option.defaultWith (fun () -> failwith $"Missing material {materialId.value}")

        let entry = minted ()
        Assert.Equal<Result<unit, MaterialError>>(Ok (), proxy.saveMaterial entry)
        let firstUser = latestOf entry.id
        Assert.Equal(VersionNumber.first, firstUser.envelope.content.version)
        Assert.Equal(UserProvenance, firstUser.envelope.content.provenance)
        Assert.Equal(VersionUnpinned, firstUser.state.pin)

        let silicon = BuiltinCatalogue.material MaterialIds.silicon
        let renamed =
            {
                silicon with name = EntryName "Renamed silicon"
            }
        let builtInProvenance = BuiltInProvenance CatalogueIds.builtInCatalogue
        Assert.Equal<Result<unit, MaterialError>>(Ok (), proxy.saveMaterial renamed)
        let edited = latestOf silicon.id
        Assert.Equal(VersionNumber.first, edited.envelope.content.version)
        Assert.Equal(renamed.name, edited.envelope.content.name)
        Assert.Equal(builtInProvenance, edited.envelope.content.provenance)

        let first = MaterialVersionId.firstOf silicon.id
        let changed =
            {
                renamed with physics = (BuiltinCatalogue.material MaterialIds.langasite).physics
            }
        Assert.Equal<Result<unit, MaterialError>>(Ok (), proxy.pinMaterialVersion first)
        Assert.Equal<Result<unit, MaterialError>>(Ok (), proxy.saveMaterial changed)
        let next = latestOf silicon.id
        Assert.Equal(VersionNumber.first.next, next.envelope.content.version)
        Assert.Equal(builtInProvenance, next.envelope.content.provenance)
        Assert.Equal(VersionUnpinned, next.state.pin)
        Assert.Equal(ActiveEntry, next.state.lifecycle)
        Assert.Equal<Result<MaterialEntry, LibraryEntryError>>(Ok changed, next.envelope.content.toMaterialEntry)

    [<Fact>]
    member this.``a database failure below the materials store is MaterialStoreUnavailableErr, never a throw`` () : unit =
        let database = this.freshDatabase ()
        let proxy = MaterialProxy.createInDatabase database
        (database :> IDisposable).Dispose()

        let unavailable (result : Result<'Outcome, MaterialError>) : unit =
            match result with
            | Error (MaterialStoreUnavailableErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
            | other -> Assert.Fail($"Expected a typed database failure, got %A{other}")

        let version = MaterialVersionId.firstOf glass.id
        proxy.listMaterials ActiveOnly |> unavailable
        proxy.searchMaterials MaterialQuery.empty |> unavailable
        proxy.tryGetMaterial glass.id |> unavailable
        proxy.resolveVersion version |> unavailable
        proxy.saveMaterial glass |> unavailable
        proxy.markMaterialInactive glass.id |> unavailable
        proxy.markMaterialActive glass.id |> unavailable
        proxy.supersedeMaterial glass.id |> unavailable
        proxy.pinMaterialVersion version |> unavailable
        proxy.removeMaterial glass.id |> unavailable

    interface IDisposable with
        member _.Dispose () : unit =
            for database in databases do
                (database :> IDisposable).Dispose()
