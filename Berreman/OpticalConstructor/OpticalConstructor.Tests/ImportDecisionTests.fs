namespace OpticalConstructor.Tests

open System
open Softellect.Sys.Primitives
open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Storage.EntryFileJson

/// The pure per-entry import decision table and explicit confirmation rule (spec 0050 S3.1, 001 §B).
module ImportDecisionTests =

    let private builtInOrigin : ImportOrigin = CatalogueImportOrigin (CatalogueIds.builtInCatalogue, CatalogueName "Built-in")
    let private labCatalogueId : CatalogueId = CatalogueId (Guid.Parse "24f7a423-0632-45db-9469-b908eb523a7d")
    let private labOrigin : ImportOrigin = CatalogueImportOrigin (labCatalogueId, CatalogueName "Lab")

    let private envelopeById (id : LibraryEntryId) : LibraryEntryEnvelope =
        (BuiltinCatalogue.catalogueData ()).envelopes |> List.find (fun envelope -> envelope.content.id = id)

    let private sealContent (content : LibraryEntryContent) : LibraryEntryEnvelope =
        content.``sealed`` |> Result.defaultWith (fun error -> failwith error.describe)

    let private heldVersionOf (envelope : LibraryEntryEnvelope) : HeldEntryVersion =
        {
            version = envelope.content.version
            contentHash = envelope.contentHash
        }

    let private heldOf (envelope : LibraryEntryEnvelope) : HeldEntry =
        {
            id = envelope.content.id
            kind = envelope.content.schema.kind
            name = envelope.content.name
            latest = envelope.content.version
            lifecycle = ActiveEntry
            provenance = envelope.content.provenance
            versions = [ heldVersionOf envelope ]
        }

    let private siliconEnvelope () : LibraryEntryEnvelope = envelopeById MaterialIds.silicon.toLibraryEntryId

    [<Fact>]
    let ``a new id is added, or skipped`` () : unit =
        let incoming = siliconEnvelope ()
        let decision = ImportDecision.decide builtInOrigin incoming None []
        let expectedChoice : ImportChoice =
            {
                decision = decision
                action = AddIncomingEntry
            }

        Assert.Equal<ImportOrigin>(builtInOrigin, decision.origin)
        Assert.Equal<ImportStatus>(EntryIdNotHeld incoming, decision.status)
        Assert.Equal<ImportAction>(AddIncomingEntry, decision.defaultAction)
        Assert.Equal<ImportAction list>([ SkipIncomingEntry ], decision.alternatives)
        Assert.Equal<LibraryEntryId>(incoming.content.id, decision.entryId)
        Assert.Equal<LibraryEntryKind>(MaterialEntryKind, decision.kind)
        Assert.Equal<EntryName>(incoming.content.name, decision.rowName)
        Assert.Equal<ImportChoice>(expectedChoice, decision.byDefault)

    [<Fact>]
    let ``the same id with the same hash is reused and offers nothing else`` () : unit =
        let incoming = siliconEnvelope ()
        let nextContent =
            { incoming.content with version = incoming.content.version.next }
        let next = sealContent nextContent
        let held =
            { heldOf next with
                versions = [ heldVersionOf incoming; heldVersionOf next ] }
        let decision = ImportDecision.decide builtInOrigin incoming (Some held) [ held ]
        let expectedChoice : ImportChoice =
            {
                decision = decision
                action = ReuseHeldEntry
            }

        Assert.Equal<ImportStatus>(SameContentHash (incoming, held), decision.status)
        Assert.Equal<ImportAction>(ReuseHeldEntry, decision.defaultAction)
        Assert.Empty(decision.alternatives)
        Assert.Equal<EntryName>(incoming.content.name, decision.rowName)
        Assert.Equal<ImportChoice>(expectedChoice, decision.byDefault)

    [<Fact>]
    let ``a version not held is added, and only a protected built-in from its own catalogue moves the latest`` () : unit =
        let original = siliconEnvelope ()
        let versionTwoContent =
            { original.content with version = original.content.version.next }
        let versionTwo = sealContent versionTwoContent
        let held = heldOf original

        let assertMove (origin : ImportOrigin) (incoming : LibraryEntryEnvelope) (local : HeldEntry) (move : LatestMove) : unit =
            let decision = ImportDecision.decide origin incoming (Some local) [ local ]
            Assert.Equal<ImportStatus>(VersionNotHeld (incoming, local), decision.status)
            Assert.Equal<ImportAction>(AddIncomingVersion move, decision.defaultAction)
            Assert.Equal<ImportAction list>([ SkipIncomingEntry ], decision.alternatives)

        assertMove builtInOrigin versionTwo held MoveLatestVersion
        assertMove labOrigin versionTwo held KeepLatestVersion

        let userContent =
            { original.content with provenance = UserProvenance }
        let userOriginal = sealContent userContent
        let userVersionTwoContent =
            { userContent with version = versionTwo.content.version }
        let userVersionTwo = sealContent userVersionTwoContent
        assertMove builtInOrigin userVersionTwo (heldOf userOriginal) KeepLatestVersion

        let versionThreeContent =
            { original.content with version = versionTwo.content.version.next }
        let versionThree = sealContent versionThreeContent
        let heldWithLater =
            { heldOf versionThree with
                versions = [ heldVersionOf original; heldVersionOf versionThree ] }
        assertMove builtInOrigin versionTwo heldWithLater KeepLatestVersion

    [<Fact>]
    let ``a dropped file keeps the latest and names itself in a collision`` () : unit =
        let original = siliconEnvelope ()
        let held = heldOf original
        let versionTwoContent =
            { original.content with version = original.content.version.next }
        let versionTwo = sealContent versionTwoContent
        let siliconOrigin = DroppedFileOrigin (FileName "Silicon.material-json")
        let versionDecision = ImportDecision.decide siliconOrigin versionTwo (Some held) []

        Assert.Equal<ImportOrigin>(siliconOrigin, versionDecision.origin)
        Assert.Equal<ImportStatus>(VersionNotHeld (versionTwo, held), versionDecision.status)
        Assert.Equal<ImportAction>(AddIncomingVersion KeepLatestVersion, versionDecision.defaultAction)
        Assert.Equal<ImportAction list>([ SkipIncomingEntry ], versionDecision.alternatives)

        let userContent =
            { original.content with
                id = LibraryEntryId (Guid.Parse "a2653a53-fd68-4411-91d0-e4c1f70e5df4")
                provenance = UserProvenance }
        let userMaterial = sealContent userContent
        let labFileOrigin = DroppedFileOrigin (FileName "lab-silicon.material-json")
        let collision = ImportDecision.decide labFileOrigin userMaterial None [ held ]

        Assert.Equal<ImportOrigin>(labFileOrigin, collision.origin)
        Assert.Equal<ImportStatus>(NameCollision (userMaterial, [ held ]), collision.status)
        Assert.Equal<ImportAction>(AddIncomingEntry, collision.defaultAction)
        Assert.Equal<ImportAction list>([ RebindToHeldEntry held.id; SkipIncomingEntry ], collision.alternatives)
        Assert.Equal<EntryName>(EntryName "Silicon — from lab-silicon.material-json", collision.rowName)
        Assert.Equal("Built-in", builtInOrigin.describe)
        Assert.Equal("lab-silicon.material-json", labFileOrigin.describe)

    [<Fact>]
    let ``the same id and version with another hash, or an id held as another kind, is a divergence forked by default`` () : unit =
        let original = siliconEnvelope ()
        let changedContent =
            { original.content with descriptionOpt = Some (EntryDescription "A divergent Silicon description") }
        let changed = sealContent changedContent
        let held = heldOf original
        Assert.NotEqual<ContentHash>(original.contentHash, changed.contentHash)

        let assertDivergence (incoming : LibraryEntryEnvelope) (local : HeldEntry) : unit =
            let decision = ImportDecision.decide builtInOrigin incoming (Some local) [ local ]
            Assert.Equal<ImportStatus>(DivergentContent (incoming, local), decision.status)
            Assert.Equal<ImportAction>(ForkIncomingEntry, decision.defaultAction)
            Assert.Equal<ImportAction list>([ SkipIncomingEntry ], decision.alternatives)
            Assert.Equal<LibraryEntryKind>(incoming.content.schema.kind, decision.kind)

        assertDivergence changed held
        let otherKind =
            { held with kind = SampleEntryKind }
        assertDivergence original otherKind
        let nextContent =
            { original.content with version = original.content.version.next }
        assertDivergence (sealContent nextContent) otherKind

    [<Fact>]
    let ``a new id with a held entry's name collides, side by side by default, with a re-binding to each held entry`` () : unit =
        let original = siliconEnvelope ()
        let incomingContent =
            { original.content with
                id = LibraryEntryId (Guid.Parse "3a8bfe3c-e409-4294-b0f3-9ea912774395")
                provenance = UserProvenance }
        let incoming = sealContent incomingContent
        let secondContent =
            { original.content with
                id = LibraryEntryId (Guid.Parse "4fb8c8d8-6703-40a9-9131-5d1ac2be21b3")
                provenance = UserProvenance }
        let second = sealContent secondContent |> heldOf
        let first = heldOf original
        let sameName = [ second; first ]
        let decision = ImportDecision.decide labOrigin incoming None sameName

        Assert.Equal<ImportStatus>(NameCollision (incoming, sameName), decision.status)
        Assert.Equal<ImportAction>(AddIncomingEntry, decision.defaultAction)
        Assert.Equal<ImportAction list>([ RebindToHeldEntry second.id; RebindToHeldEntry first.id; SkipIncomingEntry ], decision.alternatives)
        Assert.Equal<EntryName>(EntryName "Silicon — from Lab", decision.rowName)
        Assert.Equal<LibraryEntryId>(incoming.content.id, decision.entryId)
        Assert.Equal<LibraryEntryKind>(MaterialEntryKind, decision.kind)
        Assert.Equal<EntryName>(original.content.name, incoming.content.name)
        Assert.Equal<LibraryEntryEnvelope>(incoming, sealContent incomingContent)

    [<Fact>]
    let ``an active entry its catalogue no longer lists is retired`` () : unit =
        let held = siliconEnvelope () |> heldOf
        let decision = ImportDecision.unlisted builtInOrigin held
        let expectedChoice : ImportChoice =
            {
                decision = decision
                action = RetireHeldEntry
            }

        Assert.Equal<ImportOrigin>(builtInOrigin, decision.origin)
        Assert.Equal<ImportStatus>(EntryUnlisted held, decision.status)
        Assert.Equal<ImportAction>(RetireHeldEntry, decision.defaultAction)
        Assert.Empty(decision.alternatives)
        Assert.Equal<LibraryEntryId>(held.id, decision.entryId)
        Assert.Equal<LibraryEntryKind>(held.kind, decision.kind)
        Assert.Equal<EntryName>(held.name, decision.rowName)
        Assert.Equal<ImportChoice>(expectedChoice, decision.byDefault)
        Assert.Equal<EntryLifecycle>(ActiveEntry, held.lifecycle)

    [<Fact>]
    let ``every decision row awaits confirmation`` () : unit =
        let incoming = siliconEnvelope ()
        let held = heldOf incoming
        let nextContent =
            { incoming.content with version = incoming.content.version.next }
        let next = sealContent nextContent
        let changedContent =
            { incoming.content with descriptionOpt = Some (EntryDescription "Changed description") }
        let changed = sealContent changedContent
        let collidingContent =
            { incoming.content with
                id = LibraryEntryId (Guid.Parse "58f40a23-3e42-454d-8313-d7ce6111f8a2")
                provenance = UserProvenance }
        let colliding = sealContent collidingContent
        let decisions =
            [
                ImportDecision.decide builtInOrigin incoming None []
                ImportDecision.decide builtInOrigin incoming (Some held) []
                ImportDecision.decide builtInOrigin next (Some held) []
                ImportDecision.decide builtInOrigin changed (Some held) []
                ImportDecision.decide labOrigin colliding None [ held ]
                ImportDecision.unlisted builtInOrigin held
            ]
        let statuses =
            [
                EntryIdNotHeld incoming
                SameContentHash (incoming, held)
                VersionNotHeld (next, held)
                DivergentContent (changed, held)
                NameCollision (colliding, [ held ])
                EntryUnlisted held
            ]

        Assert.Equal<ImportStatus list>(statuses, decisions |> List.map (fun decision -> decision.status))
        Assert.Equal<ImportDecision list>(decisions, (ImportDecision.split decisions).awaiting)
        Assert.Empty((ImportDecision.split []).awaiting)

    [<Fact>]
    let ``a row naming an entry that awaits a choice waits with it`` () : unit =
        let silicon = siliconEnvelope ()
        let category =
            match silicon.content.categoryOpt with
            | Some id -> envelopeById id.toLibraryEntryId
            | None -> failwith "The shipped Silicon material must name its category."
        let categoryId = CategoryId (Guid.Parse "67ac2de1-291a-47e0-abbe-8ad94df57b40")
        let categoryContent =
            { category.content with id = categoryId.toLibraryEntryId; provenance = UserProvenance }
        let collidingCategory = sealContent categoryContent
        let materialId = MaterialId (Guid.Parse "7499c713-24c0-4bd3-aaec-af71e62cf9bf")
        let materialContent =
            { silicon.content with
                id = materialId.toLibraryEntryId
                categoryOpt = Some categoryId
                provenance = UserProvenance }
        let material = sealContent materialContent
        let sample = envelopeById SampleIds.multilayerQwId.toLibraryEntryId
        let samplePayload =
            match sample.content.payload with
            | SamplePayload payload -> payload
            | _ -> failwith "The shipped multilayer sample must have a sample payload."
        let layer =
            { List.head samplePayload.structure.expandedFilms with
                materialId = MaterialVersionId.firstOf materialId }
        let structure : SampleStructure =
            {
                films = [ SingleLayer layer ]
                substrate = None
                lower = None
            }
        let updatedSamplePayload =
            { samplePayload with structure = structure }
        let sampleContent =
            { sample.content with
                id = LibraryEntryId (Guid.Parse "863c97f8-b4e0-40f7-bcbf-1fb2540da975")
                payload = SamplePayload updatedSamplePayload
                provenance = UserProvenance }
        let incomingSample = sealContent sampleContent
        let source =
            (BuiltinCatalogue.catalogueData ()).envelopes
            |> List.find (fun envelope -> envelope.content.schema.kind = SourceEntryKind)
        let sourceContent =
            { source.content with
                id = LibraryEntryId (Guid.Parse "953c8162-4fe7-42de-b7ae-221a452ec53a")
                provenance = UserProvenance }
        let incomingSource = sealContent sourceContent
        let heldCategory = heldOf category
        let decisions =
            [
                ImportDecision.decide labOrigin collidingCategory None [ heldCategory ]
                ImportDecision.decide labOrigin material None []
                ImportDecision.decide labOrigin incomingSample None []
                ImportDecision.decide labOrigin incomingSource None []
            ]
        let statuses =
            [
                NameCollision (collidingCategory, [ heldCategory ])
                EntryIdNotHeld material
                EntryIdNotHeld incomingSample
                EntryIdNotHeld incomingSource
            ]

        Assert.Equal<Set<LibraryEntryId>>(Set.singleton collidingCategory.content.id, material.content.referencedEntryIds)
        Assert.Equal<Set<LibraryEntryId>>(Set.singleton material.content.id, incomingSample.content.referencedEntryIds)
        Assert.Empty(incomingSource.content.referencedEntryIds)
        Assert.Equal<ImportStatus list>(statuses, decisions |> List.map (fun decision -> decision.status))
        Assert.Equal<ImportDecision list>(decisions, (ImportDecision.split decisions).awaiting)
