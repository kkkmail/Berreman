namespace OpticalConstructor.Tests

open System
open System.Text
open Xunit
open Berreman.Constants
open Berreman.Geometry
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Storage.LibraryDatabaseStore
open OpticalConstructor.Storage.MaterialDatabaseStore
open CrystalMaterialFixtures

/// Spec 0050 S11.1 — the study messages applied to an in-memory kit through `StudyMsg.applyTo`, each answered study
/// recorded through `recordStudy`: the study file they write, the material version OpenMaterial snapshots, and a
/// refusal that records nothing.
module StudyMessageTests =

    /// An in-memory kit opened read-write over one library database, whose material store is the library's.
    type private StudyKitFixture =
        {
            folder : InMemoryKitFolder
            library : KitLibraryContext
            materials : MaterialProxy
        }

    let private expectKitOk (result : Result<'Value, KitError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private expectRecordOk (result : Result<'Value, KitLibraryError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private qzLr : StudyName = StudyName "QZ-LR"
    let private e1 : ExperimentName = ExperimentName "E1"
    let private e2 : ExperimentName = ExperimentName "E2"
    let private e3 : ExperimentName = ExperimentName "E3"
    let private retardance : ParameterName = ParameterName "c2.retardance"

    let private circularPolarizer : ItemModelChoice = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId)

    /// Step 362's CPL1, CPL2 and Phone: two dial-mounted circular polarizers and a fixed intensity detector.
    let private cpl1 : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "36200000-0000-4000-8000-000000000011")
                label = ItemLabel "CPL1"
                model = circularPolarizer
                dial = DialMounted DialPositive
            }
        draft.toItem

    let private cpl2 : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "36200000-0000-4000-8000-000000000012")
                label = ItemLabel "CPL2"
                model = circularPolarizer
                dial = DialMounted DialPositive
            }
        draft.toItem

    let private phone : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "36200000-0000-4000-8000-000000000013")
                label = ItemLabel "Phone"
                model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.intensityDetectorModelId)
                dial = FixedMounting
            }
        draft.toItem

    let private slotAt (id : ElementId) (kind : CatalogueKind) (point : TablePoint) (content : SlotContent) : SetupSlot =
        {
            id = id
            placement = ElementPlacement.create kind point
            zoom = BeamLayout.ElementZoom 5.0
            beam = BeamLayout.OnMainBeam
            content = content
        }

    let private cpl2SlotId : ElementId = ElementId "cpl2"

    /// The source, CPL1 at its dial's zero, CPL2 at 45° and the Phone, in light order.
    let private bench : Setup =
        let slots =
            [
                slotAt (ElementId "src") LightSource RayModel.defaultSourcePoint EmptySlotContent
                slotAt (ElementId "cpl1") CircularPolarizer { x = -0.4<meter>; y = 0.0<meter> } (ItemInSlot (cpl1.id, DialTurn (Angle.degree 0.0<deg>)))
                slotAt cpl2SlotId CircularPolarizer { x = -0.2<meter>; y = 0.0<meter> } (ItemInSlot (cpl2.id, DialTurn (Angle.degree 45.0<deg>)))
                slotAt (ElementId "det") Detector RayModel.defaultDetectorPoint (ItemInSlot (phone.id, PlacementTurn))
            ]

        match Setup.create (SetupName "QZ-LR bench") slots [] with
        | Ok setup -> setup
        | Error error -> failwith $"the bench was refused: {error.describe}"

    /// A Science run over the bench turning CPL2.
    let private benchRun (id : ExperimentId) (name : ExperimentName) : Experiment =
        let axis : ScanAxis =
            {
                elementId = cpl2SlotId
                label = ElementLabel "CPL2"
                variable = VaryR1
                range = VariableRange.forVariable VaryR1
            }

        {
            id = id
            name = name
            setup = bench.reference
            scan = ExperimentScan.oneAxis axis CaptureTransmitted None
            role = ScienceRun
            sessionOpt = None
            notes = ExperimentNotes.none
            dataOpt = None
            corrections = []
        }

    /// The user absorbing crystal material the library saves, at its first version.
    let private crystalVersion : MaterialVersionId =
        MaterialVersionId.firstOf (MaterialId (Guid.Parse "36600000-0000-4000-8000-000000000001"))

    let private crystalEntry : MaterialEntry =
        {
            id = crystalVersion.materialId
            name = EntryName "QZ-LR absorbing crystal"
            category = CategoryIds.crystal
            description = None
            physics = CrystalParameterPhysics (AbsorbingTriclinicCrystal absorbingTriclinicParameters)
        }

    /// The kit over `database`, holding CPL1, CPL2 and the Phone, the bench and E1, E2 and E3 over it, beside a library
    /// that has saved the crystal material. The library's import seam and material store share the database, so the
    /// saved version is one the kit can snapshot (spec 0050 S11.1, as step 359's study recording).
    let private studyKit (database : LibraryDatabase) : StudyKitFixture =
        let folder = InMemoryKitFolder.create ()
        let proxy = KitProxy.createInMemory folder
        let mutable next = ClockTime (DateTime(2026, 11, 13, 12, 0, 0, DateTimeKind.Utc))
        let clock : ClockProvider =
            {
                now = fun () ->
                    let current = next
                    next <- ClockTime (current.value.AddSeconds 1.0)
                    current
            }
        let kitFile = KitFile.create (KitId (Guid.Parse "36600000-0000-4000-8000-000000000101")) (KitName "Study message kit") (KitAuthor "Study message tests") (clock.now ())
        proxy.initialiseKit kitFile |> expectKitOk

        let library : KitLibraryContext =
            {
                kit = KitContext.openKit proxy ReadWriteKitAccess clock |> expectKitOk
                imports = CatalogueImportContext.create (CatalogueImportProxy.createInDatabase database)
            }

        for item in [ cpl1; cpl2; phone ] do
            library.recordItem item |> expectRecordOk

        library.recordSetup bench |> expectRecordOk

        for id, name in [ ExperimentId 1, e1; ExperimentId 2, e2; ExperimentId 3, e3 ] do
            library.recordExperiment (benchRun id name) |> expectRecordOk

        let materials = MaterialProxy.createInDatabase database
        Assert.Equal<Result<unit, MaterialError>>(Ok (), materials.saveMaterial crystalEntry)

        {
            folder = folder
            library = library
            materials = materials
        }

    /// The study messages' context as the kit reads now, resolving a material version through the library's store.
    let private studyContextOf (fixture : StudyKitFixture) : StudyContext =
        let proxy = fixture.library.kit.proxy
        {
            studies = proxy.readStudies () |> expectKitOk
            experiments = proxy.readExperiments () |> expectKitOk
            setups = proxy.readSetups () |> expectKitOk
            items = proxy.readItems () |> expectKitOk
            resolveMaterial =
                fun version ->
                    let named = $"material {version.materialId.value} v{version.version.value}"
                    match fixture.materials.resolveVersion version with
                    | Ok (Some entry) -> Ok entry
                    | Ok None -> Error (MaterialUnavailableErr $"{named} is not in the library")
                    | Error _ -> Error (MaterialUnavailableErr $"{named}: the library's material store refused the read")
        }

    /// The message applied to the kit as it reads now, as the study view sends it: an answered study is recorded.
    let private sent (fixture : StudyKitFixture) (message : StudyMsg) : Result<Study, StudyError> =
        message.applyTo (studyContextOf fixture)
        |> Result.map (fun study ->
            fixture.library.recordStudy study |> expectRecordOk
            study)

    let private recorded (fixture : StudyKitFixture) (message : StudyMsg) : Study =
        match sent fixture message with
        | Ok study -> study
        | Error error -> failwith $"expected a study, got {error.describe}"

    let private storedStudy (fixture : StudyKitFixture) : Study =
        fixture.library.kit.proxy.readStudies () |> expectKitOk |> Assert.Single

    /// Every file of the kit with its bytes: each stored text as the kit reads it, each snapshot as its file text.
    let private kitFileBytes (fixture : StudyKitFixture) : Map<KitRelativePath, FileByteContent> =
        let proxy = fixture.library.kit.proxy
        let texts =
            fixture.folder.paths ()
            |> List.choose (fun path ->
                match proxy.readLabFile path with
                | Ok source -> Some (path, source.content)
                | Error _ -> None)
        let snapshots =
            proxy.listSnapshots ()
            |> expectKitOk
            |> List.map (fun envelope ->
                let text = envelope.fileText |> Result.defaultWith (fun error -> failwith error.describe)
                envelope.snapshotPath, FileByteContent (Encoding.UTF8.GetBytes text.value))
        Map.ofList (texts @ snapshots)

    [<Fact>]
    let ``CreateStudy and AddExperimentsToStudy write the study file over the kit's experiments`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let fixture = studyKit database
        let studyPath = KitRelativePath "QZ-LR.study-json"
        Assert.DoesNotContain(studyPath, fixture.folder.paths ())

        let created = recorded fixture (CreateStudy (qzLr, [ e1 ]))
        Assert.Contains(studyPath, fixture.folder.paths ())
        Assert.Equal(qzLr, created.name)
        Assert.Equal<ExperimentName list>([ e1 ], created.experiments)
        Assert.Equal(created, storedStudy fixture)

        // The second message reads the refreshed list, so it finds the study the first one recorded.
        let added = recorded fixture (AddExperimentsToStudy (qzLr, [ e2; e1 ]))
        let stored = storedStudy fixture
        Assert.Equal<ExperimentName list>([ e1; e2 ], stored.experiments)
        Assert.Equal({ created with experiments = [ e1; e2 ] }, stored)
        Assert.Equal(added, stored)

    /// The material edit uses the same message and recording path as the study view (spec 0050 S11.5).
    [<Fact>]
    let ``SetOpenedMaterialParameter frees an opened component in the stored study, and a refused one records nothing`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let fixture = studyKit database
        recorded fixture (CreateStudy (qzLr, [ e1; e2; e3 ])) |> ignore
        let opened = recorded fixture (OpenMaterial (qzLr, crystalVersion))
        let originalCopy = Assert.Single(opened.materials)
        let n1 : ParameterName = ParameterName "n1"
        let unknown : UnknownParameterData =
            {
                start = ParameterValue 1.70
                bounds =
                    {
                        lower = ParameterValue 1.60
                        upper = ParameterValue 1.80
                    }
                priorOpt = None
            }
        let parameter : Parameter =
            {
                name = n1
                state = UnknownParameter unknown
            }

        let freed = recorded fixture (SetOpenedMaterialParameter (qzLr, crystalVersion, parameter))
        let stored = storedStudy fixture
        let copy = Assert.Single(stored.materials)
        Assert.Equal(parameter, copy.parameters |> List.find (fun held -> held.name = n1))
        Assert.Equal(15, copy.parameters.Length)

        for held in copy.parameters |> List.filter (fun held -> held.name <> n1) do
            match held.state with
            | KnownParameter _ -> ()
            | UnknownParameter _
            | CalibratedParameter _ -> Assert.Fail($"{held.name.value} must remain known")

        let expectedCopy =
            {
                originalCopy with
                    parameters = originalCopy.parameters |> List.map (fun held -> if held.name = n1 then parameter else held)
            }
        let expectedStudy =
            {
                opened with materials = [ expectedCopy ]
            }
        Assert.Equal(expectedCopy, copy)
        Assert.Equal(expectedStudy, stored)
        Assert.Equal(freed, stored)

        let paths = fixture.folder.paths ()
        let bytes = kitFileBytes fixture
        let assertUnchanged () : unit =
            Assert.Equal<KitRelativePath list>(paths, fixture.folder.paths ())
            Assert.Equal<Map<KitRelativePath, FileByteContent>>(bytes, kitFileBytes fixture)
            Assert.Equal(stored, storedStudy fixture)

        let unopenedVersion = MaterialVersionId.firstOf (MaterialId (Guid.Parse "40900000-0000-4000-8000-000000000009"))
        match sent fixture (SetOpenedMaterialParameter (qzLr, unopenedVersion, parameter)) with
        | Error (UnknownStudyMaterialErr reason) -> Assert.Contains($"{unopenedVersion.materialId.value}", reason)
        | Error error -> Assert.Fail($"expected UnknownStudyMaterialErr, got {error.describe}")
        | Ok _ -> Assert.Fail("an unopened material version must be refused")
        assertUnchanged ()

        let reversed : Parameter =
            {
                parameter with
                    state =
                        UnknownParameter
                            {
                                unknown with
                                    bounds =
                                        {
                                            lower = unknown.bounds.upper
                                            upper = unknown.bounds.lower
                                        }
                            }
            }
        match sent fixture (SetOpenedMaterialParameter (qzLr, crystalVersion, reversed)) with
        | Error (InvalidStudyParameterErr reason) -> Assert.Contains(n1.value, reason)
        | Error error -> Assert.Fail($"expected InvalidStudyParameterErr, got {error.describe}")
        | Ok _ -> Assert.Fail("reversed parameter bounds must be refused")
        assertUnchanged ()

        let missingParameter : Parameter =
            {
                parameter with name = ParameterName "n_o"
            }
        match sent fixture (SetOpenedMaterialParameter (qzLr, crystalVersion, missingParameter)) with
        | Error (UnknownStudyParameterErr reason) -> Assert.Contains(missingParameter.name.value, reason)
        | Error error -> Assert.Fail($"expected UnknownStudyParameterErr, got {error.describe}")
        | Ok _ -> Assert.Fail("an absent material component must be refused")
        assertUnchanged ()

        match sent fixture (SetOpenedMaterialParameter (StudyName "LR", crystalVersion, parameter)) with
        | Error (UnknownStudyErr reason) -> Assert.Contains("'LR'", reason)
        | Error error -> Assert.Fail($"expected UnknownStudyErr, got {error.describe}")
        | Ok _ -> Assert.Fail("an absent study must be refused")
        assertUnchanged ()

    [<Fact>]
    let ``OpenMaterial snapshots the material version into the kit and stores its working copy in the study file`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let fixture = studyKit database
        let created = recorded fixture (CreateStudy (qzLr, [ e1; e2; e3 ]))
        let snapshotPath = KitRelativePath $"library/{crystalVersion.materialId.value:D}@v1.material-json"
        Assert.DoesNotContain(snapshotPath, fixture.folder.paths ())

        let opened = recorded fixture (OpenMaterial (qzLr, crystalVersion))
        Assert.Contains(snapshotPath, fixture.folder.paths ())

        let stored = storedStudy fixture
        let copy = Assert.Single(stored.materials)
        Assert.Equal(crystalVersion, copy.version)
        Assert.Equal(crystalEntry.name, copy.name)
        Assert.Equal(15, copy.parameters.Length)

        for parameter in copy.parameters do
            match parameter.state with
            | KnownParameter _ -> ()
            | UnknownParameter _
            | CalibratedParameter _ -> Assert.Fail($"{parameter.name.value} must open as a known parameter")

        Assert.Equal<Result<OpenedMaterial, StudyError>>(OpenedMaterial.openFrom crystalVersion crystalEntry, Ok copy)
        Assert.Equal({ created with materials = [ copy ] }, stored)
        Assert.Equal(opened, stored)

    [<Fact>]
    let ``SetParameterScope and LinkParameters change the stored study`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let fixture = studyKit database
        let created = recorded fixture (CreateStudy (qzLr, [ e1; e2; e3 ]))

        // The bench holds all three items, so every run of the study uses them, in the kit's order.
        Assert.Equal<Item list>([ cpl1; cpl2; phone ], (studyContextOf fixture).itemsUsedBy created)

        let scoped : ScopedParameter =
            {
                parameter = ItemParameterRef (phone.id, ParameterName.gain)
                scope = ExperimentScope
            }

        let link : ParameterLink =
            {
                follower = ItemParameterRef (cpl2.id, retardance)
                leader = ItemParameterRef (cpl1.id, retardance)
            }

        let perExperiment = recorded fixture (SetParameterScope (qzLr, scoped))
        Assert.Equal<ScopedParameter list>([ scoped ], (storedStudy fixture).scopes)

        let linked = recorded fixture (LinkParameters (qzLr, link))
        let stored = storedStudy fixture
        Assert.Equal<ScopedParameter list>([ scoped ], stored.scopes)
        Assert.Equal<ParameterLink list>([ link ], stored.links)
        Assert.Equal({ created with scopes = [ scoped ]; links = [ link ] }, stored)
        Assert.Equal({ perExperiment with links = [ link ] }, linked)
        Assert.Equal(linked, stored)

    [<Fact>]
    let ``a refused study message records nothing`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let fixture = studyKit database
        let created = recorded fixture (CreateStudy (qzLr, [ e1 ]))
        let paths = fixture.folder.paths ()
        let bytes = kitFileBytes fixture

        match sent fixture (AddExperimentsToStudy (StudyName "LR", [ e1 ])) with
        | Error (UnknownStudyErr reason) -> Assert.Contains("'LR'", reason)
        | other -> Assert.Fail($"expected Error (UnknownStudyErr _) naming LR, got %A{other}")

        let lacking = MaterialVersionId.firstOf (MaterialId (Guid.Parse "36600000-0000-4000-8000-000000000009"))

        match sent fixture (OpenMaterial (qzLr, lacking)) with
        | Error (MaterialUnavailableErr reason) -> Assert.Contains($"{lacking.materialId.value}", reason)
        | other -> Assert.Fail($"expected Error (MaterialUnavailableErr _) naming the version, got %A{other}")

        Assert.Equal<KitRelativePath list>(paths, fixture.folder.paths ())
        Assert.Equal<Map<KitRelativePath, FileByteContent>>(bytes, kitFileBytes fixture)
        Assert.Equal(created, storedStudy fixture)
