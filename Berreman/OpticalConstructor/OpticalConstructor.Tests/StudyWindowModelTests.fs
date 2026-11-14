namespace OpticalConstructor.Tests

open System
open System.IO
open Xunit
open Berreman.Constants
open Berreman.Geometry
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.FitRuns
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Ui.KitOpening
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
