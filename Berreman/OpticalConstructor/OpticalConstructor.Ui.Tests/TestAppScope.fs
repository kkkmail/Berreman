namespace OpticalConstructor.Ui.Tests

open System.IO
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui
open Berreman.Constants
open Berreman.Geometry
open Berreman.Media
open Berreman.MaterialProperties
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Propagation
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Tests
open OpticalConstructor.Ui.TableAndElementRotationView

module TestAppScope =

    /// Isolate each owner's locations under the test output (spec 0050 .spec-md §3.9).
    let testLocations (owner : FolderName) : DataLocations =
        let root = FolderName (Path.Combine(System.AppContext.BaseDirectory, "TestAppScope", owner.value))
        let install = FolderName System.AppContext.BaseDirectory
        let backups = FolderName "Backups"
        let preferences = FolderName "Preferences"
        let kits = FolderName "Kits"
        let exports = FolderName "Exports"
        let catalogues = FolderName "Catalogues"
        let sources = FolderName "Sources"

        {
            libraryConnection = ConnectionString "Data Source=:memory:"
            backupsFolder = root.combine backups
            preferencesFolder = root.combine preferences
            kitsFolder = root.combine kits
            exportsFolder = root.combine exports
            cataloguesFolder = root.combine catalogues
            shippedCatalogueFolder = install.combine DataLocations.shippedCatalogueDefault
            materialSourcesFolder = root.combine sources
        }

    /// Isolate writable locations and a file library beneath a fresh owner folder (spec 0050 S2.4).
    let fileLocations (owner : FolderName) : DataLocations =
        let instance = FolderName (System.Guid.NewGuid().ToString("N"))
        let locations = testLocations (owner.combine instance)
        let root = Path.GetDirectoryName locations.backupsFolder.value
        let databaseFile = FileName (Path.Combine(root, "library.db"))

        {
            locations with
                libraryConnection = ConnectionString $"Data Source={databaseFile.value}"
        }

    /// Explicitly confirms every pending catalogue's complete default choice list in populated fixtures (spec 0050 S3.1).
    let settlePendingImports (context : AppContext) : unit =
        for pending in context.pendingImports do
            let choices = pending.decisions |> List.map _.byDefault
            context.catalogueImports.settle pending choices
            |> Result.defaultWith (fun error -> failwith error.describe) |> ignore

    /// Compose a fresh database app scope and explicitly confirm every startup choice (spec 0050 S2.4; S3.1).
    let freshAppContext (owner : FolderName) : AppContext =
        let context =
            AppContext.create (testLocations owner) WorkbenchSettings.defaults
            |> Result.defaultWith (fun error -> failwith error.describe)
        settlePendingImports context
        context

    /// Open against an empty library and a fresh empty shipped-catalogue folder (spec 0050 S12.1; 001 §Q 'Open').
    let emptyLibraryAppContext (owner : FolderName) : AppContext =
        let instance = FolderName (System.Guid.NewGuid().ToString("N"))
        let locations = testLocations owner
        let shipped = locations.cataloguesFolder.combine instance
        Directory.CreateDirectory shipped.value |> ignore
        let emptyLocations =
            {
                locations with shippedCatalogueFolder = shipped
            }
        AppContext.create emptyLocations WorkbenchSettings.defaults
        |> Result.defaultWith (fun error -> failwith error.describe)

    /// Returns an uncreated GUID-named kit folder under the owner's test output (spec 0050 .spec-md §3.10).
    let freshKitFolder (owner : FolderName) : KitFolder =
        let instance = FolderName (System.Guid.NewGuid().ToString("N"))
        let folder = (testLocations owner).kitsFolder.combine instance
        KitFolder folder

    /// Answers the folder picker immediately with the supplied test kit (spec 0050 S4.3).
    let kitPickerOf (folder : KitFolder) : KitFolderPicker =
        {
            pickKitFolder = fun () -> async { return Some folder }
        }

    /// Opens a fresh read-write test kit, failing the calling fact with its error (spec 0050 .spec-md §3.10).
    let openedKitIn (owner : FolderName) (imports : CatalogueImportContext) : OpenedKit =
        let author = KitAuthor "test"
        KitOpening.openKitAt (freshKitFolder owner) ReadWriteKitAccess ClockProvider.system author imports
        |> Result.defaultWith (fun error -> failwith error.describe)

    /// Compose the Inverse window over its opened kit and snapshot-backed app stores (spec 0050 S12.1).
    let inverseModelOver (context : AppContext) (opened : OpenedKit) : TableAndElementRotationModel =
        let stores : KitLibraryStores =
            {
                library = context.library
                materials = context.materials
                samples = context.samples
                categories = context.categories
            }
        let overlaid = stores.overlaidWith opened.overlay
        let initial =
            {
                initInverseWithKit overlaid.library context.experiments overlaid.materials overlaid.samples overlaid.categories opened.library.kit with
                    catalogueImports = context.catalogueImports
                    itemModels = context.itemModels.overlaidWith opened.overlay
                    sources = context.sources
                    libraryBackups = context.libraryBackups
                    experimentExport = context.experimentExport
            }
        initial.loadKit opened

    /// A reopened folder kit holding CPL1, CPL2, Phone and three Science scans in the study QZ-LR (spec 0050 S11.5).
    let studyKitIn (owner : FolderName) : TableAndElementRotationModel =
        let context = freshAppContext owner
        let opened = openedKitIn owner context.catalogueImports
        let polarizerUnknown : UnknownParameterData =
            {
                start = ParameterValue 1.5
                bounds =
                    {
                        lower = ParameterValue 1.0
                        upper = ParameterValue 2.0
                    }
                priorOpt = None
            }
        let detectorUnknown : UnknownParameterData =
            {
                start = ParameterValue 1000.0
                bounds =
                    {
                        lower = ParameterValue 100.0
                        upper = ParameterValue 10000.0
                    }
                priorOpt = None
            }
        let freedItem (draft : ItemDraft) (name : ParameterName) (unknown : UnknownParameterData) : Item =
            let state = UnknownParameter unknown
            draft.toItem.withParameterState name state |> Result.defaultWith (fun error -> failwith error.describe)
        let retardance = ParameterName "c2.retardance"
        let circular = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId)
        let polarizer (id : ItemId) (label : ItemLabel) : Item =
            let draft : ItemDraft =
                {
                    id = id
                    label = label
                    model = circular
                    dial = DialMounted DialPositive
                }
            freedItem draft retardance polarizerUnknown
        let firstId = ItemId (System.Guid.Parse "41400000-0000-4000-8000-000000000011")
        let secondId = ItemId (System.Guid.Parse "41400000-0000-4000-8000-000000000012")
        let firstLabel = ItemLabel "CPL1"
        let secondLabel = ItemLabel "CPL2"
        let first = polarizer firstId firstLabel
        let second = polarizer secondId secondLabel
        let phoneDraft : ItemDraft =
            {
                id = ItemId (System.Guid.Parse "41400000-0000-4000-8000-000000000013")
                label = ItemLabel "Phone"
                model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.intensityDetectorModelId)
                dial = FixedMounting
            }
        let phone = freedItem phoneDraft ParameterName.gain detectorUnknown
        let slot (id : ElementId) (kind : CatalogueKind) (point : TablePoint) (content : SlotContent) : SetupSlot =
            {
                id = id
                placement = ElementPlacement.create kind point
                zoom = BeamLayout.ElementZoom 5.0
                beam = BeamLayout.OnMainBeam
                content = content
            }
        let sourceId = ElementId "src"
        let prepId = ElementId "cpl1"
        let analyzerId = ElementId "cpl2"
        let detectorId = ElementId "det"
        let prepPoint : TablePoint =
            {
                x = -0.4<meter>
                y = 0.0<meter>
            }
        let analyzerPoint : TablePoint =
            {
                x = -0.2<meter>
                y = 0.0<meter>
            }
        let prepContent = ItemInSlot (first.id, DialTurn (Angle.degree 0.0<deg>))
        let analyzerContent = ItemInSlot (second.id, DialTurn (Angle.degree 45.0<deg>))
        let detectorContent = ItemInSlot (phone.id, PlacementTurn)
        let slots =
            [
                slot sourceId LightSource RayModel.defaultSourcePoint EmptySlotContent
                slot prepId CircularPolarizer prepPoint prepContent
                slot analyzerId CircularPolarizer analyzerPoint analyzerContent
                slot detectorId Detector RayModel.defaultDetectorPoint detectorContent
            ]
        let benchName = SetupName "Bench"
        let bench = Setup.create benchName slots [] |> Result.defaultWith (fun error -> failwith error.describe)
        let names = [ ExperimentName "E1"; ExperimentName "E2"; ExperimentName "E3" ]
        try
            for item in [ first; second; phone ] do
                opened.library.recordItem item |> Result.defaultWith (fun error -> failwith error.describe)
            opened.library.recordSetup bench |> Result.defaultWith (fun error -> failwith error.describe)
            for index, name in List.indexed names do
                let axis : ScanAxis =
                    {
                        elementId = analyzerId
                        label = ElementLabel "CPL2"
                        variable = VaryR1
                        range =
                            {
                                min = SweptValue 0.0
                                max = SweptValue 90.0
                                points = CurvePointCount 4
                            }
                    }
                let dataOpt =
                    if index = 1 then None
                    else
                        let file = FileName $"e{index + 1}.csv"
                        let content = FileByteContent (System.Text.Encoding.UTF8.GetBytes "x,y\n0,1\n30,0.75\n60,0.25\n90,0\n")
                        let attached = opened.library.kit.proxy.copyInFile KitDataFolder.root file content |> SceneTestFixture.kitSuccess
                        Some
                            {
                                attachment = attached.attachment
                                profile = ImportProfileName.intensitySeries
                                rows = AllDataRows
                                exclusions = []
                            }
                let experiment : Experiment =
                    {
                        id = ExperimentId (index + 1)
                        name = name
                        setup = bench.reference
                        scan = ExperimentScan.oneAxis axis CaptureTransmitted None
                        role = ScienceRun
                        sessionOpt = None
                        notes = ExperimentNotes.none
                        dataOpt = dataOpt
                        corrections = []
                    }
                opened.library.recordExperiment experiment |> Result.defaultWith (fun error -> failwith error.describe)
        finally opened.library.kit.close () |> SceneTestFixture.kitSuccess
        let author = KitAuthor "test"
        let reopened =
            openKitAt opened.folder ReadWriteKitAccess ClockProvider.system author context.catalogueImports
            |> Result.defaultWith (fun error -> failwith error.describe)
        let studyName = StudyName "QZ-LR"
        let model = inverseModelOver context reopened |> updateTableAndElementRotation (StudyCommand (CreateStudy (studyName, names)))
        match model.kitStatus with
        | None -> model
        | Some error ->
            model.kit.close () |> SceneTestFixture.kitSuccess
            failwith error.value

    /// The UI suite's user absorbing crystal, saved and opened through the selected study's stores (spec 0050 S11.5).
    let internal withStudyMaterial (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let crystal : AbsorbingTriclinicParameters =
            {
                index1 = RefractionIndex 1.6
                index2 = RefractionIndex 1.7
                index3 = RefractionIndex 1.8
                epsIm11 = EpsValue 0.01
                epsIm22 = EpsValue 0.02
                epsIm33 = EpsValue 0.03
                epsIm23 = EpsValue 0.001
                epsIm13 = EpsValue 0.002
                epsIm12 = EpsValue 0.003
                g11 = RhoValue 0.0001
                g22 = RhoValue 0.0002
                g33 = RhoValue 0.0003
                g23 = RhoValue 0.00001
                g13 = RhoValue 0.00002
                g12 = RhoValue 0.00003
            }
        let entry : MaterialEntry =
            {
                id = MaterialId (System.Guid.Parse "41400000-0000-4000-8000-000000000101")
                name = EntryName "Study absorbing crystal"
                category = CategoryIds.crystal
                description = None
                physics = CrystalParameterPhysics (AbsorbingTriclinicCrystal crystal)
            }
        model.materials.saveMaterial entry |> Result.defaultWith (fun error -> failwith error.caption.value)
        let study = model.selectedStudyOpt |> Option.defaultWith (fun () -> failwith "The fixture must have a selected study")
        let version = MaterialVersionId.firstOf entry.id
        let opened = model |> updateTableAndElementRotation (StudyCommand (OpenMaterial (study.name, version)))
        match opened.kitStatus with
        | None -> opened
        | Some error -> failwith error.value

    /// The UI suite's one plan fixture: Laser, QZ-Z of the probe quartz and Polarimeter in a reopened kit over `context`,
    /// the Polarimeter in `det`, and the study QZ freeing the quartz's g11 and g33 within ±50 % (spec 0050 S14.2).
    let planKitOver (context : AppContext) (owner : FolderName) : TableAndElementRotationModel =
        let opened = openedKitIn owner context.catalogueImports
        let quartz : UniaxialParameters =
            {
                ordinaryIndex = RefractionIndex 1.544
                extraordinaryIndex = RefractionIndex 1.553
                g11 = RhoValue 3.0e-5
                g33 = RhoValue (-1.0e-5)
            }
        let entry : MaterialEntry =
            {
                id = MaterialId (System.Guid.Parse "49000000-0000-4000-8000-000000000001")
                name = EntryName "Probe quartz"
                category = CategoryIds.crystal
                description = None
                physics = CrystalParameterPhysics (UniaxialCrystal quartz)
            }
        let version = MaterialVersionId.firstOf entry.id
        let substrate : SampleLayer =
            {
                materialId = version
                thickness = Thickness.mm 0.5<mm>
                orientation = PrimaryAxes
            }
        let plate : Sample =
            {
                id = SampleId (System.Guid.Parse "49000000-0000-4000-8000-000000000011")
                name = EntryName "QZ-Z"
                structure =
                    {
                        films = []
                        substrate = Some substrate
                        lower = None
                    }
                substrate = ThickPlate
                description = EntryDescription "A z-cut plate of the probe quartz"
                supportedEmission = EmitBoth
            }
        let draft (id : string) (label : string) (model : ItemModelChoice) : ItemDraft =
            {
                id = ItemId (System.Guid.Parse id)
                label = ItemLabel label
                model = model
                dial = FixedMounting
            }
        let laser =
            draft "49000000-0000-4000-8000-000000000021" "Laser"
                (SourcePresetChoice (BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId, BuiltinCatalogue.source PresetIds.heNe633))
        let qzZ =
            draft "49000000-0000-4000-8000-000000000022" "QZ-Z"
                (StackSampleChoice (BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId, SampleVersionId.firstOf plate.id, plate))
        let polarimeter =
            draft "49000000-0000-4000-8000-000000000023" "Polarimeter"
                (LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.muellerPolarimeterModelId))
        try
            context.materials.saveMaterial entry |> Result.defaultWith (fun error -> failwith error.caption.value)
            context.samples.saveSample plate |> Result.defaultWith (fun error -> failwith error.caption.value)
            for item in [ laser; qzZ; polarimeter ] do
                opened.library.recordItem item.toItem |> Result.defaultWith (fun error -> failwith error.describe)
        finally opened.library.kit.close () |> SceneTestFixture.kitSuccess
        let reopened =
            openKitAt opened.folder ReadWriteKitAccess ClockProvider.system (KitAuthor "test") context.catalogueImports
            |> Result.defaultWith (fun error -> failwith error.describe)
        let study = StudyName "QZ"
        let checkedAfter (message : TableAndElementRotationMsg) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
            let next = model |> updateTableAndElementRotation message
            match next.kitStatus with
            | None -> next
            | Some error ->
                next.kit.close () |> SceneTestFixture.kitSuccess
                failwith error.value
        let freed (model : TableAndElementRotationModel) (name : ParameterName) : TableAndElementRotationModel =
            let copy =
                model.planMaterials
                |> List.tryFind (fun (material : OpenedMaterial) -> material.version = version)
                |> Option.defaultWith (fun () -> failwith "The study QZ must open the probe quartz")
            match copy.physics.componentValue name with
            | Some (ParameterValue value) ->
                let ends = [ 0.5 * value; 1.5 * value ]
                let parameter : Parameter =
                    {
                        name = name
                        state =
                            UnknownParameter
                                {
                                    start = ParameterValue value
                                    bounds =
                                        {
                                            lower = ParameterValue (List.min ends)
                                            upper = ParameterValue (List.max ends)
                                        }
                                    priorOpt = None
                                }
                    }
                model |> checkedAfter (StudyCommand (SetOpenedMaterialParameter (study, version, parameter)))
            | None ->
                model.kit.close () |> SceneTestFixture.kitSuccess
                failwith $"The probe quartz's copy has no component {name.value}"
        let prepared =
            inverseModelOver context reopened
            |> checkedAfter (AssignSlot (ElementId "det", Some polarimeter.id))
            |> checkedAfter (StudyCommand (CreateStudy (study, [])))
            |> checkedAfter (StudyCommand (OpenMaterial (study, version)))
        [ ParameterName "g11"; ParameterName "g33" ] |> List.fold freed prepared

    /// `planKitOver` over a fresh app context of `owner` (spec 0050 S14.2).
    let planKitIn (owner : FolderName) : TableAndElementRotationModel = planKitOver (freshAppContext owner) owner
