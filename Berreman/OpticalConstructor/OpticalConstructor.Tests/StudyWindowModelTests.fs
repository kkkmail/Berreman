namespace OpticalConstructor.Tests

open System
open System.Collections.Concurrent
open System.IO
open System.Threading
open Xunit
open Berreman.Constants
open Berreman.Geometry
open Berreman.Solvers
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.FitRuns
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.StudyPublishing
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Storage.MaterialDatabaseStore
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.StudyEngine
open OpticalConstructor.Ui.ConstructorUiIdOfDomain
open OpticalConstructor.Ui.JobRunner
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.StudyFitJob
open OpticalConstructor.Ui.TableAndElementRotationView

/// The window's study commands over real kit files and one shared catalogue store set (spec 0050 S11.5).
module StudyWindowModelTests =

    let private kitValue (result : Result<'Value, KitError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private libraryValue (result : Result<'Value, KitLibraryError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private studyValue (result : Result<'Value, StudyError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private author = KitAuthor "Study window tests"
    let private qzLr = StudyName "QZ-LR"
    let private lr = StudyName "LR"
    let private e1 = ExperimentName "E1"
    let private e2 = ExperimentName "E2"
    let private e3 = ExperimentName "E3"
    let private retardance = ParameterName "c2.retardance"

    let private polarizerUnknown : UnknownParameterData =
        {
            start = ParameterValue 1.5
            bounds =
                {
                    lower = ParameterValue 1.0
                    upper = ParameterValue 2.0
                }
            priorOpt = None
        }

    let private phoneUnknown : UnknownParameterData =
        {
            start = ParameterValue 1000.0
            bounds =
                {
                    lower = ParameterValue 100.0
                    upper = ParameterValue 10000.0
                }
            priorOpt = None
        }

    let private freedItem (draft : ItemDraft) (parameter : ParameterName) (unknown : UnknownParameterData) : Item =
        let state = UnknownParameter unknown
        draft.toItem.withParameterState parameter state
        |> Result.defaultWith (fun error -> failwith error.describe)

    let private cpl1 : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "41000000-0000-4000-8000-000000000011")
                label = ItemLabel "CPL1"
                model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId)
                dial = DialMounted DialPositive
            }
        freedItem draft retardance polarizerUnknown

    let private cpl2 : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "41000000-0000-4000-8000-000000000012")
                label = ItemLabel "CPL2"
                model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId)
                dial = DialMounted DialPositive
            }
        freedItem draft retardance polarizerUnknown

    let private phone : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "41000000-0000-4000-8000-000000000013")
                label = ItemLabel "Phone"
                model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.intensityDetectorModelId)
                dial = FixedMounting
            }
        freedItem draft (ParameterName "gain") phoneUnknown

    let private cpl2SlotId = ElementId "cpl2"

    let private bench : Setup =
        let slot (id : ElementId) (kind : CatalogueKind) (point : TablePoint) (content : SlotContent) : SetupSlot =
            {
                id = id
                placement = ElementPlacement.create kind point
                zoom = BeamLayout.ElementZoom 5.0
                beam = BeamLayout.OnMainBeam
                content = content
            }
        let firstPoint : TablePoint =
            {
                x = -0.4<meter>
                y = 0.0<meter>
            }
        let secondPoint : TablePoint =
            {
                x = -0.2<meter>
                y = 0.0<meter>
            }
        let slots =
            [
                slot (ElementId "src") LightSource RayModel.defaultSourcePoint EmptySlotContent
                slot (ElementId "cpl1") CircularPolarizer firstPoint (ItemInSlot (cpl1.id, DialTurn (Angle.degree 0.0<deg>)))
                slot cpl2SlotId CircularPolarizer secondPoint (ItemInSlot (cpl2.id, DialTurn (Angle.degree 45.0<deg>)))
                slot (ElementId "det") Detector RayModel.defaultDetectorPoint (ItemInSlot (phone.id, PlacementTurn))
            ]
        Setup.create (SetupName "Bench") slots [] |> Result.defaultWith (fun error -> failwith error.describe)

    let private experiment (id : ExperimentId) (name : ExperimentName) (role : ExperimentRole) : Experiment =
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
            role = role
            sessionOpt = None
            notes = ExperimentNotes.none
            dataOpt = None
            corrections = []
        }

    let private experiments : Experiment list =
        [ experiment (ExperimentId 1) e1 ScienceRun; experiment (ExperimentId 2) e2 ScienceRun; experiment (ExperimentId 3) e3 CalibrationRun ]

    let private studyOf (name : StudyName) : Study = Study.create name [ e1; e2 ] [] experiments |> studyValue

    let private qzRun : FitRun = FitRunFileFixtures.fitRunOf qzLr FitRunFileFixtures.firstFitRunStamp

    let private jointRequest : StudyFitRequest =
        {
            study = qzLr
            stage = JointStage
            calibrationUse = CalibrationHeldFixed
        }

    let private recordingFitHost
        (jobs : ResizeArray<IterationWatch -> Result<FitRun, StudyFitError>>)
        (cancel : unit -> unit)
        : StudyFitJobHost =
        {
            startStudyFit = fun job ->
                jobs.Add job
                {
                    cancelStudyFit = cancel
                }
        }

    let private folderBytes (folder : KitFolder) : Map<FileName, FileByteContent> =
        Directory.GetFiles(folder.value.value, "*", SearchOption.AllDirectories)
        |> Array.map (fun path ->
            let name = FileName (Path.GetRelativePath(folder.value.value, path))
            let content = FileByteContent (File.ReadAllBytes path)
            name, content)
        |> Map.ofArray

    let private withWindow
        (access : KitAccess)
        (studies : Study list)
        (runs : FitRun list)
        (run : KitFolder -> TableAndElementRotationModel -> unit)
        : unit =
        let root = FolderName (Path.Combine(AppContext.BaseDirectory, "StudyWindowModelTests", Guid.NewGuid().ToString("N")))
        let folder = KitFolder root
        let stores = BuiltinCatalogue.freshStores ()
        let imports = CatalogueImportContext.create stores.proxy
        try
            let created = openKitAt folder ReadWriteKitAccess ClockProvider.system author imports |> libraryValue
            try
                for item in [ cpl1; cpl2; phone ] do created.library.recordItem item |> libraryValue
                created.library.recordSetup bench |> libraryValue
                for experiment in experiments do created.library.recordExperiment experiment |> libraryValue
                for study in studies do created.library.recordStudy study |> libraryValue
                for fitRun in runs do created.library.kit.proxy.writeFitRun fitRun |> kitValue
            finally created.library.kit.close () |> kitValue

            let opened = openKitAt folder access ClockProvider.system author imports |> libraryValue
            try
                let sources = SourceProxy.createInMemory ()
                let library = liveLibrary stores.libraryEntries stores.samples sources
                let initial = initInverse library (createInMemoryExperiments ()) stores.materials stores.samples stores.categories |> kitValue
                try
                    let model =
                        {
                            initial with
                                sources = sources
                                catalogueImports = imports
                                itemModels = stores.itemModels
                        }
                    run folder (model.loadKit opened)
                finally initial.kit.close () |> kitValue
            finally opened.library.kit.close () |> kitValue
        finally
            if Directory.Exists root.value then Directory.Delete(root.value, true)

    let private calibrationEntry : FitRunEntry =
        {
            parameter = ItemParameterRef (cpl1.id, retardance)
            owner = ItemOwner cpl1.id
            start = polarizerUnknown.start
            fitted = ParameterValue 1.55
        }

    let private calibrationRun : FitRun =
        {
            schemaVersion = FitRun.currentSchemaVersion
            name = qzLr.fitRunNameAt FitRunFileFixtures.firstFitRunStamp
            study = qzLr
            stamp = FitRunFileFixtures.firstFitRunStamp
            mode = StagedFit
            stage = CalibrationStage
            calibrationUse = CalibrationHeldFixed
            engine = NonlinearStudyEngine
            runs = [ e1; e2 ]
            dataRead = []
            parameters =
                [ cpl1; cpl2; phone ]
                |> List.collect (fun item ->
                    item.parameters |> List.map (fun parameter ->
                        {
                            parameter = ItemParameterRef (item.id, parameter.name)
                            state = parameter.state
                        }))
            entries = [ calibrationEntry ]
            members = []
            objectMatrices = []
            results =
                {
                    sigmas =
                        [
                            {
                                entry =
                                    {
                                        parameter = calibrationEntry.parameter
                                        owner = calibrationEntry.owner
                                    }
                                sigmaOpt = Some (ParameterSigma 0.002)
                            }
                        ]
                    problems = []
                    runResiduals = []
                    realizability = []
                    productChecks = []
                    profiles = []
                }
            notes = FitRunNotes.none
        }

    let private linkPolarizers (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let link : ParameterLink =
            {
                follower = ItemParameterRef (cpl2.id, retardance)
                leader = calibrationEntry.parameter
            }
        let linked = model |> updateTableAndElementRotation (StudyCommand (LinkParameters (qzLr, link)))
        Assert.Equal<ControlCaption option>(None, linked.kitStatus)
        linked

    let private assertStatusOnly (caption : ControlCaption) (before : TableAndElementRotationModel) (after : TableAndElementRotationModel) : unit =
        let expected =
            {
                before with kitStatus = Some caption
            }
        Assert.Equal<TableAndElementRotationModel>(expected, after)

    let private withMaterialFit
        (access : KitAccess)
        (run : MaterialVersionId -> FitRun -> KitFolder -> TableAndElementRotationModel -> unit)
        : unit =
        withWindow ReadWriteKitAccess [ studyOf qzLr; studyOf lr ] [ calibrationRun ] (fun folder loaded ->
            use database = BuiltinCatalogue.freshLibraryDatabase ()
            let shared =
                {
                    loaded with
                        materials = MaterialProxy.createInDatabase database
                        catalogueImports = CatalogueImportContext.create (CatalogueImportProxy.createInDatabase database)
                }
                |> linkPolarizers
            let materialId = MaterialId (Guid.Parse "41300000-0000-4000-8000-000000000001")
            let version = MaterialVersionId.firstOf materialId
            let crystal = AbsorbingTriclinicCrystal CrystalMaterialFixtures.absorbingTriclinicParameters
            let entry : MaterialEntry =
                {
                    id = materialId
                    name = EntryName "Window promotion crystal"
                    category = CategoryIds.crystal
                    description = None
                    physics = CrystalParameterPhysics crystal
                }
            Assert.Equal<Result<unit, MaterialError>>(Ok (), shared.materials.saveMaterial entry)
            let opened = shared |> updateTableAndElementRotation (StudyCommand (OpenMaterial (qzLr, version)))
            Assert.Equal<ControlCaption option>(None, opened.kitStatus)
            let study = opened.kitStudies |> List.find (fun held -> held.name = qzLr)
            let n1 = ParameterName "n1"
            let stamp = ClockTime (calibrationRun.stamp.value.AddHours 1.0)
            let fitted : FitRunEntry =
                {
                    parameter = MaterialParameterRef (version, n1)
                    owner = MaterialOwner version
                    start = (crystal.parameterAxes |> List.find (fun axis -> axis.name = n1)).parameterValueOf crystal
                    fitted = ParameterValue 1.7
                }
            let materialRun =
                {
                    calibrationRun with
                        name = qzLr.fitRunNameAt stamp
                        stamp = stamp
                        stage = ObjectStage
                        parameters =
                            study.materials |> List.collect (fun material ->
                                material.parameters |> List.map (fun parameter ->
                                    {
                                        parameter = MaterialParameterRef (material.version, parameter.name)
                                        state = parameter.state
                                    }))
                        entries = [ fitted ]
                        results =
                            {
                                calibrationRun.results with sigmas = []
                            }
                }
            opened.kit.proxy.writeFitRun materialRun |> kitValue
            match access with
            | ReadWriteKitAccess ->
                let refreshed = opened |> updateTableAndElementRotation ReloadKitChanges
                Assert.Equal<FitRun list>([ calibrationRun; materialRun ], refreshed.kitFitRuns)
                run version materialRun folder refreshed
            | ReadOnlyKitAccess ->
                let readOnly = openKitAt folder ReadOnlyKitAccess ClockProvider.system author shared.catalogueImports |> libraryValue
                try run version materialRun folder (opened.loadKit readOnly)
                finally readOnly.library.kit.close () |> kitValue)

    [<Fact>]
    let ``PublishCalibration records each calibrated item's next revision in the kit and tells which`` () : unit =
        withWindow ReadWriteKitAccess [ studyOf qzLr; studyOf lr ] [ calibrationRun ] (fun folder loaded ->
            let linked = loaded |> linkPolarizers |> updateTableAndElementRotation (SelectStudy lr)
            let before = folderBytes folder
            let published = linked |> updateTableAndElementRotation (PublishCalibration calibrationRun.name)
            let calibration : CalibratedParameterData =
                {
                    value = ParameterValue 1.55
                    sigma = ParameterSigma 0.002
                    fitRun = calibrationRun.name
                }
            for original in [ cpl1; cpl2 ] do
                let item = published.kitItems |> List.find (fun held -> held.id = original.id)
                Assert.Equal<ItemRevision>(ItemRevision 2, item.revision)
                Assert.Equal<ParameterState option>(Some (CalibratedParameter calibration), item.tryFindParameter retardance |> Option.map _.state)
                let history : ItemRevisionRecord =
                    {
                        revision = original.revision
                        parameters = original.parameters
                    }
                Assert.Equal<ItemRevisionRecord list>([ history ], item.history)
                let restored =
                    {
                        item with parameters = original.parameters; revision = original.revision; history = original.history
                    }
                Assert.Equal<Item>(original, restored)
                Assert.Equal<Parameter list>(
                    original.parameters |> List.filter (fun parameter -> parameter.name <> retardance),
                    item.parameters |> List.filter (fun parameter -> parameter.name <> retardance))
            Assert.Equal<Item>(phone, published.kitItems |> List.find (fun item -> item.id = phone.id))
            Assert.Equal<Item list>(published.kitItems, published.kit.proxy.readItems () |> kitValue)
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Published the calibration of CPL1, CPL2 from QZ-LR@20261101T120000Z."), published.kitStatus)
            let expected =
                {
                    linked with kitItems = published.kitItems; kitStatus = published.kitStatus
                }
            Assert.Equal<TableAndElementRotationModel>(expected, published)
            let after = folderBytes folder
            for KeyValue(file, bytes) in before do
                if file <> FileName "CPL1.item-json" && file <> FileName "CPL2.item-json" then
                    Assert.Equal<FileByteContent>(bytes, Map.find file after))

    [<Fact>]
    let ``a publish the run refuses, or of a fit run the kit lacks, is told and records nothing`` () : unit =
        withWindow ReadWriteKitAccess [ studyOf qzLr ] [ calibrationRun ] (fun folder loaded ->
            let linked = linkPolarizers loaded
            let beforeFailure = folderBytes folder
            let noSigma =
                {
                    calibrationRun with
                        results =
                            {
                                calibrationRun.results with sigmas = []
                            }
                }
            let undetermined =
                {
                    linked with kitFitRuns = [ noSigma ]
                }
            let refused = undetermined |> updateTableAndElementRotation (PublishCalibration noSigma.name)
            match refused.kitStatus with
            | Some caption ->
                Assert.StartsWith("Nothing was published — the fit leaves a sigma undetermined", caption.value)
                assertStatusOnly caption undetermined refused
            | None -> Assert.Fail "a refused calibration must tell why"
            Assert.Equal<Map<FileName, FileByteContent>>(beforeFailure, folderBytes folder)

            let failure = KitFileRejectedErr "CPL2 write refused by the fixture"
            let attempts = ResizeArray<FileStem>()
            let partial =
                {
                    linked with
                        kit =
                            {
                                linked.kit with
                                    proxy =
                                        {
                                            linked.kit.proxy with
                                                writeKindFile = fun kind stem text ->
                                                    attempts.Add stem
                                                    if kind = ItemKitFile && stem = FileStem "CPL2" then Error failure
                                                    else linked.kit.proxy.writeKindFile kind stem text
                                        }
                            }
                }
                |> updateTableAndElementRotation (PublishCalibration calibrationRun.name)
            Assert.Equal<FileStem list>([ FileStem "CPL1"; FileStem "CPL2" ], List.ofSeq attempts)
            Assert.Equal<ItemRevision>(cpl1.revision.next, (partial.kitItems |> List.find (fun item -> item.id = cpl1.id)).revision)
            Assert.Equal<Item>(cpl2, partial.kitItems |> List.find (fun item -> item.id = cpl2.id))
            Assert.Equal<Item list>(partial.kitItems, linked.kit.proxy.readItems () |> kitValue)
            let error = KitLibraryKitErr failure
            Assert.Equal<ControlCaption option>(Some (ControlCaption $"Not saved to the kit — {error.describe}"), partial.kitStatus)
            Assert.Equal<FileByteContent>(Map.find (FileName "CPL2.item-json") beforeFailure, Map.find (FileName "CPL2.item-json") (folderBytes folder))
            let retryable =
                {
                    partial with kit = linked.kit
                }
            let published = retryable |> updateTableAndElementRotation (PublishCalibration calibrationRun.name)
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Published the calibration of CPL2 from QZ-LR@20261101T120000Z."), published.kitStatus)
            let before = folderBytes folder
            let repeated = published |> updateTableAndElementRotation (PublishCalibration calibrationRun.name)
            match repeated.kitStatus with
            | Some caption ->
                Assert.StartsWith("Nothing was published — the calibration is already published", caption.value)
                assertStatusOnly caption published repeated
            | None -> Assert.Fail "a repeated calibration must tell why it writes nothing"
            let missing = FitRunName "Missing@20261101T120000Z"
            let withoutStudy =
                {
                    published with kitStudies = []
                }
            for name, model in [ missing, published; calibrationRun.name, withoutStudy ] do
                let absent = model |> updateTableAndElementRotation (PublishCalibration name)
                assertStatusOnly (ControlCaption $"The kit holds no fit run {name.value}.") model absent
            Assert.Equal<Item list>(published.kitItems, published.kit.proxy.readItems () |> kitValue)
            Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder))

    [<Fact>]
    let ``PromoteToLibrary of a fitted material mints its next library version and snapshots it into the kit`` () : unit =
        withMaterialFit ReadWriteKitAccess (fun version fit folder loaded ->
            let selected = loaded |> updateTableAndElementRotation (SelectStudy lr)
            let before = folderBytes folder
            let original = selected.materials.resolveVersion version
            let promoted = selected |> updateTableAndElementRotation (PromoteToLibrary (fit.name, MaterialPromotion version))
            let next =
                {
                    version with version = version.version.next
                }
            let caption = ControlCaption $"Promoted to the library as {version.materialId.value} v2."
            assertStatusOnly caption selected promoted
            let envelope =
                promoted.catalogueImports.proxy.tryHeldEnvelope version.materialId.toLibraryEntryId next.version
                |> Result.defaultWith (fun error -> failwith error.describe)
                |> Option.defaultWith (fun () -> failwith "the promoted material envelope is missing")
            Assert.Equal<EntryProvenance>(fit.provenanceIn promoted.kit.kitFile.id, envelope.content.provenance)
            Assert.Contains<LibraryEntryEnvelope>(envelope, promoted.kit.proxy.listSnapshots () |> kitValue)
            let path = KitRelativePath $"library/{version.materialId.value:D}@v2.material-json"
            promoted.kit.proxy.readLabFile path |> kitValue |> ignore
            let entry =
                promoted.materials.resolveVersion next
                |> Result.defaultWith (fun error -> failwith error.caption.value)
                |> Option.defaultWith (fun () -> failwith "the promoted material version is missing")
            match entry.physics with
            | CrystalParameterPhysics crystal ->
                let n1 = ParameterName "n1"
                Assert.Equal<ParameterValue>(ParameterValue 1.7, (crystal.parameterAxes |> List.find (fun axis -> axis.name = n1)).parameterValueOf crystal)
            | _ -> Assert.Fail "the promoted material must keep its crystal physics"
            Assert.Equal<Result<MaterialEntry option, MaterialError>>(original, promoted.materials.resolveVersion version)
            let after = folderBytes folder
            for KeyValue(file, bytes) in before do Assert.Equal<FileByteContent>(bytes, Map.find file after)

            let held = promoted.catalogueImports.proxy.tryHeldEntry version.materialId.toLibraryEntryId
            for result in [ Ok None; Error (MaterialStoreUnavailableErr "unreadable material") ] do
                let unreadable =
                    {
                        promoted with
                            materials =
                                {
                                    promoted.materials with resolveVersion = fun _ -> result
                                }
                    }
                let refused = unreadable |> updateTableAndElementRotation (PromoteToLibrary (fit.name, MaterialPromotion version))
                assertStatusOnly (ControlCaption "The material version could not be read.") unreadable refused
            let unfitted = promoted |> updateTableAndElementRotation (PromoteToLibrary (calibrationRun.name, MaterialPromotion version))
            match unfitted.kitStatus with
            | Some caption ->
                Assert.StartsWith("Not promoted — the fit run fitted none of its values", caption.value)
                assertStatusOnly caption promoted unfitted
            | None -> Assert.Fail "an unfitted promotion must tell why"
            let missing = FitRunName "Missing@20261101T120000Z"
            let withoutStudy =
                {
                    promoted with kitStudies = []
                }
            for name, model in [ missing, promoted; fit.name, withoutStudy ] do
                let refused = model |> updateTableAndElementRotation (PromoteToLibrary (name, MaterialPromotion version))
                assertStatusOnly (ControlCaption $"The kit holds no fit run {name.value}.") model refused
            Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(held, promoted.catalogueImports.proxy.tryHeldEntry version.materialId.toLibraryEntryId)
            Assert.Equal<Map<FileName, FileByteContent>>(after, folderBytes folder))

        withWindow ReadWriteKitAccess [ studyOf qzLr ] [] (fun folder loaded ->
            let modelVersion = BuiltinCatalogue.itemModel ItemModelIds.blackBoxMuellerModelId
            let draft : ItemDraft =
                {
                    id = ItemId (Guid.Parse "41300000-0000-4000-8000-000000000002")
                    label = ItemLabel "QZ"
                    model = LibraryModelChoice modelVersion
                    dial = FixedMounting
                }
            let added = loaded |> updateTableAndElementRotation (AddItem draft)
            Assert.Equal<ControlCaption option>(None, added.kitStatus)
            let item = added.kitItems |> List.find (fun held -> held.id = draft.id)
            let m00 = ParameterName "m00"
            let fitted : FitRunEntry =
                {
                    parameter = ItemParameterRef (item.id, m00)
                    owner = ItemOwner item.id
                    start = ParameterValue 1.0
                    fitted = ParameterValue 0.9
                }
            let fit =
                {
                    calibrationRun with
                        stage = ObjectStage
                        entries = [ fitted ]
                        parameters =
                            item.parameters |> List.map (fun parameter ->
                                {
                                    parameter = ItemParameterRef (item.id, parameter.name)
                                    state = parameter.state
                                })
                        results =
                            {
                                calibrationRun.results with sigmas = []
                            }
                }
            added.kit.proxy.writeFitRun fit |> kitValue
            let refreshed = added |> updateTableAndElementRotation ReloadKitChanges
            let requested = ResizeArray<ItemModelVersionId>()
            let resolving =
                {
                    refreshed with
                        itemModels =
                            {
                                refreshed.itemModels with
                                    resolveItemModelVersion = fun version ->
                                        requested.Add version
                                        refreshed.itemModels.resolveItemModelVersion version
                            }
                }
            let before = folderBytes folder
            let promoted = resolving |> updateTableAndElementRotation (PromoteToLibrary (fit.name, BlackBoxPromotion item.id))
            assertStatusOnly (ControlCaption $"Promoted to the library as {item.id.value} v1.") resolving promoted
            Assert.Equal<ItemModelVersionId list>([ modelVersion.versionId ], List.ofSeq requested)
            let envelope =
                promoted.catalogueImports.proxy.tryHeldEnvelope item.id.toLibraryEntryId VersionNumber.first
                |> Result.defaultWith (fun error -> failwith error.describe)
                |> Option.defaultWith (fun () -> failwith "the promoted black-box envelope is missing")
            Assert.Equal<EntryProvenance>(fit.provenanceIn promoted.kit.kitFile.id, envelope.content.provenance)
            Assert.Contains<LibraryEntryEnvelope>(envelope, promoted.kit.proxy.listSnapshots () |> kitValue)
            let path = KitRelativePath $"library/{item.id.value:D}@v1.polarizer-json"
            promoted.kit.proxy.readLabFile path |> kitValue |> ignore
            let preset = envelope.content.toPolarizerPreset |> Result.defaultWith (fun error -> failwith error.describe)
            match preset.behavior with
            | ConstantMueller [ muellerPart ] ->
                let stored = muellerPart.matrix.element 0 0 |> ParameterValue
                Assert.Equal<ParameterValue>(fitted.fitted, stored)
            | _ -> Assert.Fail "the fitted black box must promote its constant Mueller matrix"
            let after = folderBytes folder
            for KeyValue(file, bytes) in before do Assert.Equal<FileByteContent>(bytes, Map.find file after)

            let missingItem = ItemId (Guid.Parse "41300000-0000-4000-8000-000000000099")
            let absent = promoted |> updateTableAndElementRotation (PromoteToLibrary (fit.name, BlackBoxPromotion missingItem))
            assertStatusOnly (ControlCaption "The item or its model is not in the kit.") promoted absent
            for result in [ Ok None; Error (ItemModelStoreUnavailableErr "unreadable model") ] do
                let unreadable =
                    {
                        promoted with
                            itemModels =
                                {
                                    promoted.itemModels with resolveItemModelVersion = fun _ -> result
                                }
                    }
                let refused = unreadable |> updateTableAndElementRotation (PromoteToLibrary (fit.name, BlackBoxPromotion item.id))
                assertStatusOnly (ControlCaption "The item or its model is not in the kit.") unreadable refused
            let notBlackBox = promoted |> updateTableAndElementRotation (PromoteToLibrary (fit.name, BlackBoxPromotion cpl1.id))
            match notBlackBox.kitStatus with
            | Some caption ->
                Assert.StartsWith("Not promoted — the item is not a black-box Mueller matrix", caption.value)
                assertStatusOnly caption promoted notBlackBox
            | None -> Assert.Fail "a non-black-box promotion must tell why"
            Assert.Equal<Map<FileName, FileByteContent>>(after, folderBytes folder))

    [<Fact>]
    let ``a read-only kit publishes and promotes nothing`` () : unit =
        withMaterialFit ReadOnlyKitAccess (fun version fit folder loaded ->
            Assert.Equal<KitAccess>(ReadOnlyKitAccess, loaded.kit.access)
            let before = folderBytes folder
            let held = loaded.catalogueImports.proxy.tryHeldEntry version.materialId.toLibraryEntryId
            let guarded =
                {
                    loaded with
                        materials =
                            {
                                loaded.materials with resolveVersion = fun _ -> failwith "read-only promotion must not resolve a material"
                            }
                        itemModels =
                            {
                                loaded.itemModels with resolveItemModelVersion = fun _ -> failwith "read-only promotion must not resolve an item model"
                            }
                }
            for message in
                [
                    PublishCalibration calibrationRun.name
                    PromoteToLibrary (fit.name, MaterialPromotion version)
                    PromoteToLibrary (calibrationRun.name, BlackBoxPromotion cpl1.id)
                    PublishCalibration (FitRunName "Absent")
                ] do
                let refused = guarded |> updateTableAndElementRotation message
                assertStatusOnly (ControlCaption "The kit is open read-only: nothing is saved.") guarded refused
                Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder)
                Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(held, loaded.catalogueImports.proxy.tryHeldEntry version.materialId.toLibraryEntryId))

    [<Fact>]
    let ``a loaded kit's studies and fit runs are the window's, and Reload reads a study another writer wrote`` () : unit =
        let qz = studyOf qzLr
        let other = studyOf lr
        withWindow ReadWriteKitAccess [ qz ] [ qzRun ] (fun folder loaded ->
            Assert.Equal<Study list>([ qz ], loaded.kitStudies)
            Assert.Equal<FitRun list>([ qzRun ], loaded.kitFitRuns)
            Assert.Equal<StudyName option>(Some qzLr, loaded.studySurface.studyOpt)
            Assert.Equal<Study option>(Some qz, loaded.selectedStudyOpt)
            let selected = loaded |> updateTableAndElementRotation (SelectStudy qzLr)
            let writer : KitLibraryContext =
                {
                    kit =
                        {
                            loaded.kit with proxy = KitProxy.create folder
                        }
                    imports = loaded.catalogueImports
                }
            writer.recordStudy other |> libraryValue
            let offered = selected |> updateTableAndElementRotation (CheckKitChanges KitCheckOnActivation)
            Assert.Contains(FileName "LR.study-json", offered.kitChanges)
            let reloaded = offered |> updateTableAndElementRotation ReloadKitChanges
            Assert.Equal<Study list>([ other; qz ], reloaded.kitStudies)
            Assert.Equal<FitRun list>([ qzRun ], reloaded.kitFitRuns)
            Assert.Equal<StudyName option>(Some qzLr, reloaded.studySurface.studyOpt)
            Assert.Equal<FitRunName option>(Some qzRun.name, reloaded.studySurface.fitRunOpt)
            Assert.Empty reloaded.kitChanges

            let laterStamp = ClockTime (qzRun.stamp.value.AddHours 1.0)
            let laterRun = FitRunFileFixtures.fitRunOf qzLr laterStamp
            writer.kit.proxy.writeFitRun laterRun |> kitValue
            let runOffer = reloaded |> updateTableAndElementRotation (CheckKitChanges KitCheckOnActivation)
            Assert.Contains(FitRunKitFile.fileNameOf laterRun.name.fileStem, runOffer.kitChanges)
            let refreshed = runOffer |> updateTableAndElementRotation ReloadKitChanges
            Assert.Equal<FitRun list>([ qzRun; laterRun ], refreshed.kitFitRuns)
            Assert.Equal<FitRunName option>(Some qzRun.name, refreshed.studySurface.fitRunOpt)

            for file in [ FileName "Broken.study-json"; FileName "Broken.fitrun-json" ] do
                let path = file.combine folder.value
                File.WriteAllText(path.value, "{")
                let offered = refreshed |> updateTableAndElementRotation (CheckKitChanges KitCheckOnActivation)
                Assert.Contains(file, offered.kitChanges)
                let refused = offered |> updateTableAndElementRotation ReloadKitChanges
                let expectedRefusal =
                    {
                        offered with kitStatus = refused.kitStatus
                    }
                Assert.Equal<TableAndElementRotationModel>(expectedRefusal, refused)
                match refused.kitStatus with
                | Some caption ->
                    Assert.StartsWith("The kit could not be reloaded — ", caption.value)
                    Assert.Contains(file.value, caption.value)
                | None -> Assert.Fail "a failed reload must tell its refusal"
                File.Delete path.value

            writer.kit.proxy.deleteKindFile StudyKitFile qzLr.fileStem |> kitValue
            writer.kit.proxy.deleteKindFile FitRunKitFile qzRun.name.fileStem |> kitValue
            writer.kit.proxy.deleteKindFile FitRunKitFile laterRun.name.fileStem |> kitValue
            let fallback = refreshed |> updateTableAndElementRotation ReloadKitChanges
            Assert.Equal<StudyName option>(Some lr, fallback.studySurface.studyOpt)
            Assert.Equal<FitRunName option>(None, fallback.studySurface.fitRunOpt)
            let withoutRuns = fallback |> updateTableAndElementRotation (SelectStudy lr)
            Assert.Empty withoutRuns.studyFitRuns
            Assert.Equal<FitRunName option>(None, withoutRuns.studySurface.fitRunOpt)
            writer.kit.proxy.deleteKindFile StudyKitFile lr.fileStem |> kitValue
            let empty = fallback |> updateTableAndElementRotation ReloadKitChanges
            Assert.Equal<StudyName option>(None, empty.studySurface.studyOpt)
            Assert.Equal<Study option>(None, empty.selectedStudyOpt)
            Assert.Empty empty.studyFitRuns)

    [<Fact>]
    let ``CreateStudy from the window records the study, selects it and clears the New study form`` () : unit =
        withWindow ReadWriteKitAccess [] [] (fun folder loaded ->
            Assert.Equal<StudySurfaceState>(StudySurfaceState.initial, loaded.studySurface)
            let before = folderBytes folder
            let drafted =
                loaded
                |> updateTableAndElementRotation (SetNewStudyName (ControlCaption "QZ-LR"))
                |> updateTableAndElementRotation (ToggleNewStudyExperiment e1)
                |> updateTableAndElementRotation (ToggleNewStudyExperiment e2)
                |> updateTableAndElementRotation (ToggleNewStudyExperiment e3)
                |> updateTableAndElementRotation (ToggleNewStudyExperiment e3)
            Assert.Equal<ExperimentName list>([ e1; e2 ], drafted.studySurface.newStudyExperiments)
            Assert.Equal<ControlCaption>(ControlCaption "QZ-LR", drafted.studySurface.newStudyName)
            Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder)
            let created = drafted |> updateTableAndElementRotation (StudyCommand (CreateStudy (qzLr, [ e1; e2 ])))
            let expected = studyOf qzLr
            Assert.Equal<Study list>([ expected ], created.kitStudies)
            Assert.Equal<Study list>(created.kitStudies, created.kit.proxy.readStudies () |> kitValue)
            let expectedSurface =
                {
                    StudySurfaceState.initial with studyOpt = Some qzLr
                }
            Assert.Equal<StudySurfaceState>(expectedSurface, created.studySurface)
            Assert.Equal<ControlCaption option>(None, created.kitStatus)

            let scope : ScopedParameter =
                {
                    parameter = ItemParameterRef (phone.id, ParameterName "gain")
                    scope = ExperimentScope
                }
            let link : ParameterLink =
                {
                    follower = ItemParameterRef (cpl2.id, retardance)
                    leader = ItemParameterRef (cpl1.id, retardance)
                }
            let editing = created |> updateTableAndElementRotation (SetNewStudyName (ControlCaption "Keep this draft"))
            let edited =
                editing
                |> updateTableAndElementRotation (StudyCommand (AddExperimentsToStudy (qzLr, [ e3 ])))
                |> updateTableAndElementRotation (StudyCommand (SetParameterScope (qzLr, scope)))
                |> updateTableAndElementRotation (StudyCommand (LinkParameters (qzLr, link)))
            let study = edited.kitStudies |> List.exactlyOne
            Assert.Equal<ExperimentName list>([ e1; e2; e3 ], study.experiments)
            Assert.Equal<ScopedParameter list>([ scope ], study.scopes)
            Assert.Equal<ParameterLink list>([ link ], study.links)
            Assert.Equal<ControlCaption>(editing.studySurface.newStudyName, edited.studySurface.newStudyName)
            Assert.Equal<Study list>(edited.kitStudies, edited.kit.proxy.readStudies () |> kitValue)
            let second = edited |> updateTableAndElementRotation (StudyCommand (CreateStudy (lr, [ e1 ])))
            Assert.Equal<StudyName list>([ lr; qzLr ], second.kitStudies |> List.map _.name)
            Assert.Equal<StudyName option>(Some lr, second.studySurface.studyOpt)
            Assert.Equal<Study list>(second.kitStudies, second.kit.proxy.readStudies () |> kitValue))

    [<Fact>]
    let ``a refused study message is told and records nothing, and a read-only kit takes none`` () : unit =
        withWindow ReadWriteKitAccess [] [] (fun folder loaded ->
            let before = folderBytes folder
            let refused = loaded |> updateTableAndElementRotation (StudyCommand (AddExperimentsToStudy (lr, [ e1 ])))
            let expectedRefusal =
                {
                    loaded with kitStatus = refused.kitStatus
                }
            Assert.Equal<TableAndElementRotationModel>(expectedRefusal, refused)
            match refused.kitStatus with
            | Some caption ->
                Assert.StartsWith("The study was not changed — ", caption.value)
                Assert.Contains("the kit has no such study", caption.value)
            | None -> Assert.Fail "a refused study command must tell its reason"
            Assert.Empty refused.kitStudies
            Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder)

            let context = loaded.studyContext
            Assert.Equal<Study list>(loaded.kitStudies, context.studies)
            Assert.Equal<Experiment list>(loaded.experimentCollection.experiments, context.experiments)
            Assert.Equal<Setup list>(loaded.kitSetups, context.setups)
            Assert.Equal<Item list>(loaded.kitItems, context.items)
            let glassVersion = MaterialVersionId.firstOf MaterialIds.glass152
            Assert.Equal<MaterialEntry>(BuiltinCatalogue.material MaterialIds.glass152, context.resolveMaterial glassVersion |> studyValue)
            let missingId = MaterialId (Guid.Parse "41000000-0000-4000-8000-000000000099")
            let missingVersion = MaterialVersionId.firstOf missingId
            match context.resolveMaterial missingVersion with
            | Error (MaterialUnavailableErr reason) ->
                Assert.Contains(missingId.value.ToString(), reason)
                Assert.Contains($"v{missingVersion.version.value}", reason)
            | Error error -> Assert.Fail error.describe
            | Ok _ -> Assert.Fail "a missing material must be a typed study error"
            let unreadable =
                {
                    loaded with
                        materials =
                            {
                                loaded.materials with resolveVersion = fun _ -> Error (MaterialStoreUnavailableErr "material read refused")
                            }
                }
            Assert.Equal<Result<MaterialEntry, StudyError>>(
                Error (MaterialUnavailableErr "material read refused"),
                unreadable.studyContext.resolveMaterial glassVersion)
            Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder)

            let failure = KitFileRejectedErr "study write refused by the fixture"
            let failing =
                {
                    loaded with
                        kit =
                            {
                                loaded.kit with
                                    proxy =
                                        {
                                            loaded.kit.proxy with writeKindFile = fun _ _ _ -> Error failure
                                        }
                            }
                }
            let notSaved = failing |> updateTableAndElementRotation (StudyCommand (CreateStudy (qzLr, [ e1 ])))
            let expectedNotSaved =
                {
                    failing with kitStatus = notSaved.kitStatus
                }
            Assert.Equal<TableAndElementRotationModel>(expectedNotSaved, notSaved)
            match notSaved.kitStatus with
            | Some caption ->
                let expectedError = KitLibraryKitErr failure
                Assert.Equal($"Not saved to the kit — {expectedError.describe}", caption.value)
            | None -> Assert.Fail "a recording refusal must be told"
            Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder))

        withWindow ReadOnlyKitAccess [] [] (fun folder loaded ->
            let before = folderBytes folder
            let refused = loaded |> updateTableAndElementRotation (StudyCommand (CreateStudy (qzLr, [ e1; e2 ])))
            let expectedRefusal =
                {
                    loaded with kitStatus = refused.kitStatus
                }
            Assert.Equal<TableAndElementRotationModel>(expectedRefusal, refused)
            Assert.Empty refused.kitStudies
            match refused.kitStatus with
            | Some caption -> Assert.Contains("read-only", caption.value)
            | None -> Assert.Fail "the kit guard must refuse a read-only study write"
            Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder))

    [<Fact>]
    let ``the study's parameter editor opens on an item parameter or an opened material component, and a parameter it lacks opens nothing`` () : unit =
        withWindow ReadWriteKitAccess [ studyOf qzLr; studyOf lr ] [] (fun folder loaded ->
            // Saving a material and recording its study snapshot must share a database (spec 0050 S11.5).
            use database = BuiltinCatalogue.freshLibraryDatabase ()
            let selected =
                {
                    loaded with
                        materials = MaterialProxy.createInDatabase database
                        catalogueImports = CatalogueImportContext.create (CatalogueImportProxy.createInDatabase database)
                }
                |> updateTableAndElementRotation (SelectStudy qzLr)
                |> updateTableAndElementRotation (SelectKitItem cpl2.id)
            let draftOf (reference : StudyParameterRef) (model : TableAndElementRotationModel) : ParameterEditDraft =
                let before = folderBytes folder
                let edited = model |> updateTableAndElementRotation (EditStudyParameter reference)
                let expected =
                    {
                        model with parameterEditOpt = edited.parameterEditOpt
                    }
                Assert.Equal<TableAndElementRotationModel>(expected, edited)
                Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder)
                match edited.parameterEditOpt with
                | Some draft ->
                    Assert.Equal<StudyParameterRef>(reference, draft.parameter)
                    draft
                | None -> failwith "the study's held parameter must open its editor"

            let itemReference = ItemParameterRef (cpl1.id, retardance)
            let itemDraft = draftOf itemReference selected
            Assert.Equal<ParameterStateChoice>(UnknownStateChoice, itemDraft.choice)
            Assert.Equal<ControlCaption>(ControlCaption "1.5", itemDraft.valueText)
            Assert.Equal<ControlCaption>(ControlCaption "1", itemDraft.lowerText)
            Assert.Equal<ControlCaption>(ControlCaption "2", itemDraft.upperText)
            Assert.Equal<ControlCaption>(ControlCaption "", itemDraft.priorSigmaText)
            Assert.Equal<Result<ParameterState, ItemError>>(Ok (UnknownParameter polarizerUnknown), itemDraft.tryState)

            let materialId = MaterialId (Guid.Parse "41100000-0000-4000-8000-000000000001")
            let version = MaterialVersionId.firstOf materialId
            let crystal = AbsorbingTriclinicCrystal CrystalMaterialFixtures.absorbingTriclinicParameters
            let entry : MaterialEntry =
                {
                    id = materialId
                    name = EntryName "Study editor absorbing crystal"
                    category = CategoryIds.crystal
                    description = None
                    physics = CrystalParameterPhysics crystal
                }
            Assert.Equal<Result<unit, MaterialError>>(Ok (), selected.materials.saveMaterial entry)
            let opened = selected |> updateTableAndElementRotation (StudyCommand (OpenMaterial (qzLr, version)))
            Assert.Equal<ControlCaption option>(None, opened.kitStatus)
            Assert.Equal<StudyName option>(Some qzLr, opened.studySurface.studyOpt)
            let study = opened.kitStudies |> List.find (fun held -> held.name = qzLr)
            let material = Assert.Single(study.materials)
            Assert.Equal<MaterialVersionId>(version, material.version)
            Assert.Equal<Study list>(opened.kitStudies, opened.kit.proxy.readStudies () |> kitValue)

            let n1 = ParameterName "n1"
            let materialReference = MaterialParameterRef (version, n1)
            let n1Value = (crystal.parameterAxes |> List.find (fun axis -> axis.name = n1)).parameterValueOf crystal
            let materialDraft = draftOf materialReference opened
            Assert.Equal<ParameterStateChoice>(KnownStateChoice, materialDraft.choice)
            let n1Text = ControlCaption (n1Value.value.ToString("R", Globalization.CultureInfo.InvariantCulture))
            Assert.Equal<ControlCaption>(n1Text, materialDraft.valueText)
            Assert.Equal<Result<ParameterState, ItemError>>(Ok (KnownParameter n1Value), materialDraft.tryState)
            for empty in [ materialDraft.lowerText; materialDraft.upperText; materialDraft.priorSigmaText ] do
                Assert.Equal<ControlCaption>(ControlCaption "", empty)

            let readOnly =
                {
                    opened with
                        kit =
                            {
                                opened.kit with
                                    proxy = opened.kit.proxy.readOnlyView
                                    access = ReadOnlyKitAccess
                            }
                }
            Assert.Equal<ParameterEditDraft>(itemDraft, draftOf itemReference readOnly)
            Assert.Equal<ParameterEditDraft>(materialDraft, draftOf materialReference readOnly)

            let missingName = ParameterName "absent"
            let missingItem = ItemId (Guid.Parse "41100000-0000-4000-8000-000000000099")
            let missingMaterial = MaterialId (Guid.Parse "41100000-0000-4000-8000-000000000099")
            let missingVersion = MaterialVersionId.firstOf missingMaterial
            let unopenedVersion =
                {
                    version with version = version.version.next
                }
            let missingReferences =
                [
                    ItemParameterRef (cpl1.id, missingName)
                    ItemParameterRef (missingItem, retardance)
                    MaterialParameterRef (version, missingName)
                    MaterialParameterRef (missingVersion, n1)
                    MaterialParameterRef (unopenedVersion, n1)
                ]
            let beforeMissing = folderBytes folder
            for reference in missingReferences do
                let unchanged = opened |> updateTableAndElementRotation (EditStudyParameter reference)
                Assert.Equal<ParameterEditDraft option>(None, unchanged.parameterEditOpt)
                Assert.Equal<TableAndElementRotationModel>(opened, unchanged)
                let editing = opened |> updateTableAndElementRotation (EditStudyParameter itemReference)
                Assert.Equal<TableAndElementRotationModel>(editing, editing |> updateTableAndElementRotation (EditStudyParameter reference))

            let otherStudy = opened |> updateTableAndElementRotation (SelectStudy lr)
            let noStudy =
                {
                    opened with
                        studySurface =
                            {
                                opened.studySurface with studyOpt = None
                            }
                }
            for model in [ selected; otherStudy; noStudy ] do
                let unchanged = model |> updateTableAndElementRotation (EditStudyParameter materialReference)
                Assert.Equal<ParameterEditDraft option>(None, unchanged.parameterEditOpt)
                Assert.Equal<TableAndElementRotationModel>(model, unchanged)
            Assert.Equal<ParameterEditDraft>(itemDraft, draftOf itemReference noStudy)
            Assert.Equal<Map<FileName, FileByteContent>>(beforeMissing, folderBytes folder))

    [<Fact>]
    let ``RunFit starts one fit through the shell's host, Cancel cancels it, and a read-only kit starts none`` () : unit =
        let inert = StudyFitJobHost.inert.startStudyFit (fun _ -> failwith "the inert host must run no fit")
        inert.cancelStudyFit ()
        let jobs = ResizeArray<IterationWatch -> Result<FitRun, StudyFitError>> ()
        let cancels = ResizeArray<unit> ()
        let host = recordingFitHost jobs cancels.Add
        withWindow ReadWriteKitAccess [ studyOf qzLr ] [] (fun folder loaded ->
            Assert.Equal<StudyFitJobState>(StudyFitJobState.idle, loaded.studyFit)
            let forward = loaded.studyForwardContext
            Assert.Equal<Study list>(loaded.kitStudies, forward.kit.studies)
            Assert.Equal<Experiment list>(loaded.experimentCollection.experiments, forward.kit.experiments)
            Assert.Equal<Setup list>(loaded.kitSetups, forward.kit.setups)
            Assert.Equal<Item list>(loaded.kitItems, forward.kit.items)
            Assert.Same(loaded.library, forward.library)
            Assert.Same(loaded.materials, forward.materials)
            Assert.Same(loaded.experimentData, forward.experimentData)
            Assert.Equal<SolverParameters>(SolverParameters.defaultValue, forward.solverParameters)
            for item in loaded.kitItems do
                match forward.choiceOf item, loaded.tryItemModelChoice item with
                | Some actual, Some expected -> Assert.Equal<ItemModelChoice>(expected, actual)
                | _ -> Assert.Fail "the forward context must resolve the stored item model version"

            let ready =
                {
                    loaded with
                        kitShell =
                            {
                                loaded.kitShell with studyFits = host
                            }
                }
            let before = folderBytes folder
            let running = ready |> updateTableAndElementRotation (StudyFit (RunFit jointRequest))
            Assert.True running.studyFit.job.isRunning
            Assert.Single jobs |> ignore
            Assert.Empty cancels
            let still = running |> updateTableAndElementRotation (StudyFit (RunFit jointRequest))
            Assert.Equal<TableAndElementRotationModel>(running, still)
            Assert.Single jobs |> ignore

            let progress : JobProgress =
                {
                    completed = JobStepCount 3
                    total = None
                    elapsed = JobElapsed (TimeSpan.FromMilliseconds 25.0)
                }
            let progressed = still |> updateTableAndElementRotation (StudyFit (StudyFitProgressed progress))
            Assert.Equal<JobProgress option>(Some progress, progressed.studyFit.job.progress)
            let cancelling = progressed |> updateTableAndElementRotation (StudyFit CancelStudyFit)
            Assert.Single cancels |> ignore
            Assert.True cancelling.studyFit.job.isRunning
            let cancelled = cancelling |> updateTableAndElementRotation (StudyFit (StudyFitEnded (Error (FitCancelledErr "cancelled by the recording host"))))
            Assert.Equal<JobStatus>(Cancelled, cancelled.studyFit.job.status)
            Assert.Equal<StudyFitJobHandle option>(None, cancelled.studyFit.handleOpt)
            Assert.Equal<StudyFitError option>(None, cancelled.studyFit.errorOpt)
            Assert.Empty cancelled.kitFitRuns
            Assert.Equal<FitRunName option>(None, cancelled.studySurface.fitRunOpt)
            Assert.Empty(cancelled.kit.proxy.listKindFiles FitRunKitFile |> kitValue)
            Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder))

        jobs.Clear ()
        cancels.Clear ()
        withWindow ReadOnlyKitAccess [ studyOf qzLr ] [] (fun folder loaded ->
            let ready =
                {
                    loaded with
                        kitShell =
                            {
                                loaded.kitShell with studyFits = host
                            }
                }
            let before = folderBytes folder
            let refused = ready |> updateTableAndElementRotation (StudyFit (RunFit jointRequest))
            let expected =
                {
                    ready with kitStatus = refused.kitStatus
                }
            Assert.Equal<TableAndElementRotationModel>(expected, refused)
            Assert.False refused.studyFit.job.isRunning
            Assert.Empty jobs
            Assert.Empty cancels
            match refused.kitStatus with
            | Some caption -> Assert.Contains("read-only", caption.value)
            | None -> Assert.Fail "the kit guard must refuse a read-only fit start"
            Assert.Empty(refused.kit.proxy.listKindFiles FitRunKitFile |> kitValue)
            Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder))

    [<Fact>]
    let ``a fit's end writes its fit run into the kit and shows it in the history at once`` () : unit =
        let stamp = ClockTime (qzRun.stamp.value.AddHours 1.0)
        let run = FitRunFileFixtures.fitRunOf qzLr stamp
        withWindow ReadWriteKitAccess [ studyOf qzLr ] [ qzRun ] (fun folder loaded ->
            let jobs = ResizeArray<IterationWatch -> Result<FitRun, StudyFitError>> ()
            let host = recordingFitHost jobs ignore
            let ready =
                {
                    loaded with
                        kitShell =
                            {
                                loaded.kitShell with studyFits = host
                            }
                }
            let running = ready |> updateTableAndElementRotation (StudyFit (RunFit jointRequest))
            Assert.True running.studyFit.job.isRunning
            Assert.Single jobs |> ignore
            Assert.Equal<FitRun list>([ qzRun ], running.kit.proxy.readFitRuns qzLr |> kitValue)
            let completed = running |> updateTableAndElementRotation (StudyFit (StudyFitEnded (Ok run)))
            Assert.Equal<JobStatus>(Completed, completed.studyFit.job.status)
            Assert.Equal<FitRunName option>(Some run.name, completed.studyFit.job.committedResults)
            Assert.Equal<StudyFitJobHandle option>(None, completed.studyFit.handleOpt)
            Assert.Equal<StudyFitError option>(None, completed.studyFit.errorOpt)
            Assert.Equal<FitRun list>([ qzRun; run ], completed.kit.proxy.readFitRuns qzLr |> kitValue)
            Assert.Equal<FitRun list>(loaded.kitFitRuns @ [ run ], completed.kitFitRuns)
            Assert.Equal<FitRun list>([ qzRun; run ], completed.studyFitRuns)
            Assert.Equal<FitRunName option>(Some run.name, completed.studySurface.fitRunOpt)

            let before = folderBytes folder
            let duplicate : FitRun =
                {
                    run with
                        entries =
                            run.entries
                            |> List.map (fun entry ->
                                {
                                    entry with fitted = ParameterValue 0.5
                                })
                }
            let restarted = completed |> updateTableAndElementRotation (StudyFit (RunFit jointRequest))
            Assert.True restarted.studyFit.job.isRunning
            Assert.Equal(2, jobs.Count)
            let refused = restarted |> updateTableAndElementRotation (StudyFit (StudyFitEnded (Ok duplicate)))
            match refused.studyFit.errorOpt with
            | Some (FitRunKitErr (FitRunExistsErr reason)) -> Assert.Contains(run.name.fileStem.value, reason)
            | Some error -> Assert.Fail($"expected the duplicate fit-run refusal, got {error.describe}")
            | None -> Assert.Fail "a fit run with an existing name must be refused"
            Assert.Equal<JobStatus>(Idle, refused.studyFit.job.status)
            Assert.Equal<FitRunName option>(Some run.name, refused.studyFit.job.committedResults)
            Assert.Equal<FitRun list>(completed.kitFitRuns, refused.kitFitRuns)
            Assert.Equal<StudySurfaceState>(completed.studySurface, refused.studySurface)
            Assert.Equal<FitRun list>([ qzRun; run ], refused.kit.proxy.readFitRuns qzLr |> kitValue)
            Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder))

    [<Fact>]
    let ``a fit that cannot run tells why and writes nothing`` () : unit =
        withWindow ReadWriteKitAccess [] [] (fun folder loaded ->
            use ended = new ManualResetEventSlim (false)
            let messages = ConcurrentQueue<TableAndElementRotationMsg> ()
            let host =
                StudyFitJobHost.posting
                    (fun action -> action ())
                    (fun progress -> messages.Enqueue (StudyFit (StudyFitProgressed progress)))
                    (fun result ->
                        messages.Enqueue (StudyFit (StudyFitEnded result))
                        ended.Set ())
            let empty = StudyName "Empty"
            let ready =
                {
                    loaded with
                        kitShell =
                            {
                                loaded.kitShell with studyFits = host
                            }
                }
                |> updateTableAndElementRotation (StudyCommand (CreateStudy (empty, [])))
            Assert.Equal<StudyName option>(Some empty, ready.studySurface.studyOpt)
            Assert.Equal<ControlCaption option>(None, ready.kitStatus)
            let before = folderBytes folder
            let request =
                {
                    jointRequest with study = empty
                }
            let running = ready |> updateTableAndElementRotation (StudyFit (RunFit request))
            Assert.True running.studyFit.job.isRunning
            Assert.True(ended.Wait(TimeSpan.FromSeconds 10.0), "the empty study's fit did not end within ten seconds")
            let finished =
                messages.ToArray ()
                |> Array.fold (fun model message -> updateTableAndElementRotation message model) running
            match finished.studyFit.errorOpt with
            | Some (FitStageErr (NoStageRunErr reason)) -> Assert.Contains(empty.value, reason)
            | Some error -> Assert.Fail($"expected the empty stage refusal, got {error.describe}")
            | None -> Assert.Fail "a study with no experiment must report why it cannot fit"
            Assert.False finished.studyFit.job.isRunning
            Assert.Equal<StudyFitJobHandle option>(None, finished.studyFit.handleOpt)
            Assert.Equal<FitRunName option>(None, finished.studyFit.job.committedResults)
            Assert.Empty finished.kitFitRuns
            Assert.Equal<FitRunName option>(None, finished.studySurface.fitRunOpt)
            Assert.Empty(finished.kit.proxy.readFitRuns empty |> kitValue)
            Assert.Empty(finished.kit.proxy.listKindFiles FitRunKitFile |> kitValue)
            Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder))

    [<Fact>]
    let ``selecting a study shows its own fit runs, oldest first`` () : unit =
        let secondStamp = ClockTime (qzRun.stamp.value.AddHours 1.0)
        let lrFirst = FitRunFileFixtures.fitRunOf lr qzRun.stamp
        let lrSecond = FitRunFileFixtures.fitRunOf lr secondStamp
        let qzSecond = FitRunFileFixtures.fitRunOf qzLr secondStamp
        withWindow ReadWriteKitAccess [ studyOf qzLr; studyOf lr ] [ qzSecond; lrSecond; qzRun; lrFirst ] (fun folder loaded ->
            let before = folderBytes folder
            Assert.Equal<FitRun list>([ lrFirst; lrSecond; qzRun; qzSecond ], loaded.kitFitRuns)
            let selected = loaded |> updateTableAndElementRotation (SelectStudy lr)
            Assert.Equal<StudyName option>(Some lr, selected.studySurface.studyOpt)
            Assert.Equal<FitRun list>([ lrFirst; lrSecond ], selected.studyFitRuns)
            Assert.Equal<FitRunName option>(Some lrSecond.name, selected.studySurface.fitRunOpt)
            let other = selected |> updateTableAndElementRotation (SelectStudy qzLr)
            Assert.Equal<FitRun list>([ qzRun; qzSecond ], other.studyFitRuns)
            Assert.Equal<FitRunName option>(Some qzSecond.name, other.studySurface.fitRunOpt)
            let unknown = other |> updateTableAndElementRotation (SelectStudy (StudyName "Absent"))
            Assert.Equal<TableAndElementRotationModel>(other, unknown)
            Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder))
