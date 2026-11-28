namespace OpticalConstructor.Ui.Tests

open System
open System.Collections.Concurrent
open System.IO
open System.Threading
open Xunit
open Softellect.Sys
open Softellect.Sys.Primitives
open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open BerremanTestSupport
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.DataSplits
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.FitRuns
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitArchive
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitHistory
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.NamingTemplates
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Propagation
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.SetupGrids
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.StudyForward
open OpticalConstructor.Domain.StudyPublishing
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Optimization
open OpticalConstructor.Optimization.BenchNoise
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.StudyEngine
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Storage.KitArchiveStore
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Storage.LabFileImport
open OpticalConstructor.Ui
open OpticalConstructor.Ui.ImportDialogView
open OpticalConstructor.Ui.KitArchiveJob
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.KitStartView
open OpticalConstructor.Ui.StudyFitJob
open OpticalConstructor.Ui.TableAndElementRotationView

/// AT-1 and AT-2 build the reference kits through the app's messages and prove them against the committed kits
/// (spec 0050 S12.1; 001 §Q).
module KitAcceptanceTests =

    type private AcceptanceItem =
        {
            id : ItemId
            label : ItemLabel
            modelId : ItemModelId
            /// The preset or the sample the model binds, by its library value id (spec 0050 S12.2).
            bindingOpt : ElementValueId option
            dial : ItemDial
        }

    type private BuiltAcceptanceKit =
        {
            folder : KitFolder
            context : OpticalConstructor.Ui.AppContext
            model : TableAndElementRotationModel
            clock : ClockProvider
            /// Moves `clock` to the instant: the build's fits, and AT-2's ensemble after its build (spec 0050 S12.2).
            setTime : ClockTime -> unit
        }

    type private AcceptanceObjects =
        {
            caption : ControlCaption
            first : ItemLabel * Angle
            secondOpt : (ItemLabel * Angle) option
        }

    type private AcceptanceSetup =
        {
            name : SetupName
            prep : ItemLabel
            objects : AcceptanceObjects
            analyzer : ItemLabel
        }

    type private AcceptanceLabGroup =
        {
            key : DataRowKey
            name : ExperimentName
            captures : MuellerRawRow list
        }

    type private AcceptanceLabFile =
        {
            path : KitRelativePath
            groups : AcceptanceLabGroup list
        }

    let private initialTime = ClockTime (DateTime(2026, 5, 31, 12, 0, 0, DateTimeKind.Utc))
    let private calibrationTime = ClockTime (DateTime(2026, 5, 31, 12, 1, 0, DateTimeKind.Utc))
    let private objectTime = ClockTime (DateTime(2026, 5, 31, 12, 2, 0, DateTimeKind.Utc))
    let private studyName = StudyName "QZ-LR"
    let private calibrationRunName = FitRunName "QZ-LR@20260531T120100Z"
    let private objectRunName = FitRunName "QZ-LR@20260531T120200Z"
    let private labTemplate = ControlCaption "{source:LP1=LP,CPL1=CPL}-({obj1}#{phi1}-{obj2}#{phi2})-{analyzer:LP2=LP,CPL2=CPL}-{note}"
    let private sourceSlot = ElementId "src"
    let private firstObjectSlot = ElementId "sample"
    let private detectorSlot = ElementId "det"
    // The prep, second-object and analyzer slots `buildAcceptanceSetups` adds, fixed so builds are byte-equal (`.manual/033`).
    let private addedSlotIds =
        [
            ElementId "7b44e06c-795f-4513-99ac-b60dc124d026"
            ElementId "bf29d8a6-8986-4ab9-898e-d6084d4bc767"
            ElementId "97b4178c-8eee-42f9-8f2d-23499a64f3eb"
        ]
    let private prepRole = PrepSlotRole (SlotPosition 1)
    let private firstObjectRole = ObjectSlotRole (SlotPosition 1)
    let private secondObjectRole = ObjectSlotRole (SlotPosition 2)
    let private analyzerRole = AnalyzerSlotRole (SlotPosition 1)
    let private detectorRole = DetectorSlotRole (SlotPosition 1)
    let private retardanceName = ParameterName "c2.retardance"
    let private fastAxisOffsetName = ParameterName "c2.fastAxisOffset"
    let private lpLpFile = KitRelativePath "data/raw/final/lp_lp.csv"
    let private lpCplFile = KitRelativePath "data/raw/final/lp_cpl.csv"
    let private cplLpFile = KitRelativePath "data/raw/final/cpl_lp.csv"
    let private cplCplDay1File = KitRelativePath "data/raw/final/cpl_cpl_day1_main.csv"
    let private cplCplDay2File = KitRelativePath "data/raw/final/cpl_cpl_day2_corrections.csv"
    let private darknessFile = KitRelativePath "data/raw/final/darkness_checks.csv"
    let private diagnosticFile = KitRelativePath "data/raw/final/bullshit_checks.csv"
    let private finalLabFiles = [ lpLpFile; lpCplFile; cplLpFile; cplCplDay1File; cplCplDay2File; darknessFile; diagnosticFile ]
    let private sessionGainSettings : SessionGainSetting list =
        [
            {
                session = SessionName "LP-LP"
                model = AnchoredSessionGain [ ExperimentName "LP-(AIR#0)-LP-2"; ExperimentName "LP-(AIR#0)-LP-2R" ]
            }
            {
                session = SessionName "LP-CPL"
                model = AnchoredSessionGain [ ExperimentName "LP-(AIR#0)-CPL-2"; ExperimentName "LP-(AIR#0)-CPL-2R" ]
            }
            {
                session = SessionName "CPL-LP before the restart"
                model = AnchoredSessionGain [ ExperimentName "CPL-(AIR#0)-LP-2" ]
            }
            {
                session = SessionName "CPL-LP after the restart"
                model = AnchoredSessionGain [ ExperimentName "CPL-(AIR#0)-LP-2R" ]
            }
            {
                session = SessionName "CPL-CPL day 1"
                model =
                    InterpolatedSessionGain
                        {
                            opening = ExperimentName "CPL-(AIR#0)-CPL"
                            closing = ExperimentName "CPL-(AIR#0)-CPL-BR"
                        }
            }
            {
                session = SessionName "CPL-CPL day 2"
                model = AnchoredSessionGain [ ExperimentName "CPL-(AIR#0)-CPL-2" ]
            }
        ]
    let private calibrationNames : ExperimentName list =
        sessionGainSettings
        |> List.collect (fun setting ->
            match setting.model with
            | AnchoredSessionGain anchors -> anchors
            | InterpolatedSessionGain repeats -> [ repeats.opening; repeats.closing ]
            | FreeSessionGain -> [])
    let private manualSetups : Map<DataRowKey, SetupName> =
        [
            DataRowKey "Darkness", SetupName "CPL-(AIR#0)-CPL"
            DataRowKey "LP-(Darkness)-CPL2", SetupName "LP-(AIR#0)-CPL"
            DataRowKey "LP-(Darkness)-LP-2", SetupName "LP-(AIR#0)-LP"
            DataRowKey "LP-(Darkness)-LP-2R", SetupName "LP-(AIR#0)-LP"
            DataRowKey "CPL-(Darkness)-LP-2", SetupName "CPL-(AIR#0)-LP"
            DataRowKey "CPL-(Darkness)-LP-2R", SetupName "CPL-(AIR#0)-LP"
            DataRowKey "BullshitCheck", SetupName "CPL-(AIR#0)-LP"
            DataRowKey "BullshitCheck-2", SetupName "CPL-(AIR#0)-LP"
        ] |> Map.ofList
    let private correctedSetups : Map<ExperimentName, SetupName> =
        [
            ExperimentName "CPL-(LR#90)-CPL", SetupName "CPL-(LR#-90)-CPL"
            ExperimentName "CPL-(QZ#90-LR#90)-CPL", SetupName "CPL-(QZ#90-LR#-90)-CPL"
        ] |> Map.ofList
    let private lrCorrectionNote =
        ControlCaption "LR was labelled +90 but physically sat at -90 (optics-mueller AGENTS.md, Current Known Caveats)."
    let private frameExclusionReason =
        ControlCaption "Likely contaminated frame; keep visible in plots but exclude from harmonic fits."
    let private analyzerDialRange : VariableRange =
        {
            min = SweptValue 0.0
            max = SweptValue 360.0
            points = CurvePointCount 19
        }
    let private muellerNames : ParameterName list =
        let indices = [ ParameterNameIndex 0; ParameterNameIndex 1; ParameterNameIndex 2; ParameterNameIndex 3 ]
        [ for row in indices do for column in indices do ParameterName.muellerElement row column ]

    let private itemSpecs : AcceptanceItem list =
        [
            {
                id = ItemId (Guid.Parse "d4d8dfc0-3d43-4a24-853c-369479de4f5d")
                label = ItemLabel "Laser"
                modelId = ItemModelIds.sourcePresetItemModelId
                bindingOpt = Some PresetIds.heNe633
                dial = FixedMounting
            }
            {
                id = ItemId (Guid.Parse "375bde88-fe45-4bd7-ad77-f13708076224")
                label = ItemLabel "LP1"
                modelId = ItemModelIds.realLinearPolarizerModelId
                bindingOpt = None
                dial = FixedMounting
            }
            {
                id = ItemId (Guid.Parse "72d0505d-8685-4884-89ad-ea13694c8693")
                label = ItemLabel "LP2"
                modelId = ItemModelIds.realLinearPolarizerModelId
                bindingOpt = None
                dial = DialMounted DialNegative
            }
            {
                id = ItemId (Guid.Parse "0d2c2b6e-c91f-486b-b80b-a1b815c03aac")
                label = ItemLabel "CPL1"
                modelId = ItemModelIds.circularPolarizerModelId
                bindingOpt = None
                dial = FixedMounting
            }
            {
                id = ItemId (Guid.Parse "b9f73589-6cfe-46d2-a5eb-2b11d6d6cd5f")
                label = ItemLabel "CPL2"
                modelId = ItemModelIds.circularPolarizerModelId
                bindingOpt = None
                dial = DialMounted DialPositive
            }
            {
                id = ItemId (Guid.Parse "dc89cf47-6d94-4887-a0fb-49e3a8ca1cb4")
                label = ItemLabel "AIR"
                modelId = ItemModelIds.emptySlotModelId
                bindingOpt = None
                dial = FixedMounting
            }
            {
                id = ItemId (Guid.Parse "436f47ea-a1c4-47ed-8c5c-f09eae8098b1")
                label = ItemLabel "QZ"
                modelId = ItemModelIds.blackBoxMuellerModelId
                bindingOpt = None
                dial = FixedMounting
            }
            {
                id = ItemId (Guid.Parse "69c2edf9-3a4d-4b23-bea2-94ba305ca507")
                label = ItemLabel "LR"
                modelId = ItemModelIds.blackBoxMuellerModelId
                bindingOpt = None
                dial = FixedMounting
            }
            {
                id = ItemId (Guid.Parse "93fae6cc-d838-45b7-b5f0-949f7a759e91")
                label = ItemLabel "Phone"
                modelId = ItemModelIds.intensityDetectorModelId
                bindingOpt = None
                dial = FixedMounting
            }
        ]

    let private absorbingStartTime = ClockTime (DateTime(2026, 6, 1, 12, 0, 0, DateTimeKind.Utc))
    let private absorbingTemplate = ControlCaption "{obj1}"
    // The detector `buildAbsorbingTable` adds on the plate's reflected ray, fixed so builds are byte-equal (`.manual/033`).
    let private reflectedDetectorSlot = ElementId "45dba7c2-156b-4d31-895a-1179ff672cfc"

    /// The one material version every AT-2 plate is made of, holding the `absorbing` ground truth (spec 0050 S12.2; 001 §Q AT-2).
    let private absorbingMaterial : MaterialEntry =
        {
            id = MaterialId (Guid.Parse "81e1d115-5d06-4337-80fd-50765a403de7")
            name = EntryName "Absorbing triclinic crystal"
            category = CategoryIds.crystal
            description = Some (EntryDescription "The fifteen-parameter absorbing, gyrotropic, triclinic crystal of spec 0044 (AbsorbingBiaxialSample.absorbing).")
            physics = CrystalParameterPhysics (CrystalPhysics.AbsorbingTriclinicCrystal AbsorbingBiaxialSample.absorbing)
        }

    /// The material's first version: what the plate is made of and what AT-2's study opens (spec 0050 S12.2).
    let private absorbingMaterialVersion : MaterialVersionId = MaterialVersionId.firstOf absorbingMaterial.id

    /// AT-2's one study, of the five plates' experiments (spec 0050 S12.2; 001 §Q AT-2).
    let private absorbingStudyName = StudyName "AbsorbingTriclinic"

    /// AT-2's one fit: its study's joint fit, calibrations held fixed, run one minute after the kit's start (spec 0050 S12.2; 001 §Q AT-2).
    let private absorbingFitRequest : StudyFitRequest =
        {
            study = absorbingStudyName
            stage = JointStage
            calibrationUse = CalibrationHeldFixed
        }
    let private absorbingFitTime = ClockTime (DateTime(2026, 6, 1, 12, 1, 0, DateTimeKind.Utc))
    let private absorbingFitRunName = FitRunName "AbsorbingTriclinic@20260601T120100Z"

    /// Each seed of `noisySeeds` and the instant its fit runs, the k-th at 12:0k, so seed 1's is `absorbingFitTime` (spec 0050 S12.2).
    let private ensembleFitTimes : (NoiseSeed * ClockTime) list =
        AbsorbingBiaxialSample.noisySeeds
        |> List.mapi (fun index seed -> seed, ClockTime (DateTime(2026, 6, 1, 12, index + 1, 0, DateTimeKind.Utc)))

    /// The 100 µm plate of that material every AT-2 plate item binds; each observation carries its cut (spec 0050 S12.2).
    let private absorbingPlate : Library.Sample =
        let substrate : SampleLayer =
            {
                materialId = absorbingMaterialVersion
                thickness = AbsorbingBiaxialSample.plate
                orientation = PrimaryAxes
            }
        let structure : SampleStructure =
            {
                films = []
                substrate = Some substrate
                lower = None
            }
        {
            id = SampleId (Guid.Parse "af711361-2854-4b61-ade4-bdb47a184d9f")
            name = EntryName "Absorbing triclinic plate 100 um"
            structure = structure
            substrate = ThickPlate
            description = EntryDescription "A 100 um plate of the absorbing triclinic crystal; each observation carries its cut."
            supportedEmission = EmitBoth
        }

    let private absorbingLaser : AcceptanceItem =
        {
            id = ItemId (Guid.Parse "9f304813-02a2-4624-b7e3-eea3e75193e7")
            label = ItemLabel "Laser"
            modelId = ItemModelIds.sourcePresetItemModelId
            bindingOpt = Some PresetIds.heNe633
            dial = FixedMounting
        }

    /// A Mueller polarimeter, its noise axes defaulting to `NoiseParam.decentLab`'s budget (spec 0050 S12.2).
    let private polarimeterItem (id : ItemId) (label : ItemLabel) : AcceptanceItem =
        {
            id = id
            label = label
            modelId = ItemModelIds.muellerPolarimeterModelId
            bindingOpt = None
            dial = FixedMounting
        }

    /// A plate of `absorbingPlate` through the stack-sample model (spec 0050 S12.2).
    let private plateItem (id : ItemId) (label : ItemLabel) : AcceptanceItem =
        {
            id = id
            label = label
            modelId = ItemModelIds.stackSampleItemModelId
            bindingOpt = Some absorbingPlate.id.toElementValueId
            dial = FixedMounting
        }

    let private polarimeterT = polarimeterItem (ItemId (Guid.Parse "f7e47d80-d265-481e-b351-6e6bf48785ee")) (ItemLabel "Polarimeter T")
    let private polarimeterR = polarimeterItem (ItemId (Guid.Parse "938e1eb1-4b3d-43ea-b4a0-12c1dc5c4191")) (ItemLabel "Polarimeter R")
    let private opticAxisCutPlate = plateItem (ItemId (Guid.Parse "f61f5339-1db2-4c6c-a453-a9a9f0c11594")) (ItemLabel "OpticAxisCut")
    let private otherOpticAxisCutPlate = plateItem (ItemId (Guid.Parse "10409181-5b7a-47b6-82df-525608098a65")) (ItemLabel "OtherOpticAxisCut")
    let private zCutPlate = plateItem (ItemId (Guid.Parse "1a62e3fd-a168-4c03-a653-1ed3ae71789b")) (ItemLabel "ZCut")
    let private xCutPlate = plateItem (ItemId (Guid.Parse "d64587f5-a4dd-4d6e-b85d-c6fecdc24f9b")) (ItemLabel "XCut")
    let private yCutPlate = plateItem (ItemId (Guid.Parse "5b34e704-1b59-4aa7-9904-f5ccbbb3ef71")) (ItemLabel "YCut")

    /// AT-2's eight items in the order the build adds them, each taking the next minted id (spec 0050 S12.2).
    let private absorbingItemSpecs : AcceptanceItem list =
        [
            absorbingLaser
            polarimeterT
            polarimeterR
            opticAxisCutPlate
            otherOpticAxisCutPlate
            zCutPlate
            xCutPlate
            yCutPlate
        ]

    /// Each AT-2 plate, in the order its setup and experiment are recorded, and the cut `AbsorbingBiaxialSample` builds
    /// its configurations with (spec 0050 S12.2; 001 §Q AT-2).
    let private absorbingPlateCuts : (AcceptanceItem * SampleCut) list =
        [
            opticAxisCutPlate, AbsorbingBiaxialSample.opticAxisCut
            otherOpticAxisCutPlate, AbsorbingBiaxialSample.otherOpticAxisCut
            zCutPlate, ZCut
            xCutPlate, XCut
            yCutPlate, YCut
        ]

    let private itemIdOf (label : ItemLabel) : ItemId =
        (itemSpecs |> List.find (fun item -> item.label = label)).id

    let private apply (messages : TableAndElementRotationMsg list) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        messages |> List.fold (fun current message -> updateTableAndElementRotation message current) model

    let private unknownBetween (start : ParameterValue) (lower : ParameterValue) (upper : ParameterValue) : ParameterState =
        UnknownParameter
            {
                start = start
                bounds =
                    {
                        lower = lower
                        upper = upper
                    }
                priorOpt = None
            }

    let private angleUnknown (start : Angle) (lower : Angle) (upper : Angle) : ParameterState =
        unknownBetween start.parameterValue lower.parameterValue upper.parameterValue

    let private parameterStateFor (spec : AcceptanceItem) (axis : ItemParameterAxis) : ParameterState =
        match spec.label.value, axis.name with
        | "LP2", name when name = ParameterName.dialZero ->
            angleUnknown (Angle.degree 152.0<deg>) Angle.zero (Angle.degree 360.0<deg>)
        | "CPL2", name when name = ParameterName.dialZero ->
            angleUnknown (Angle.degree 80.0<deg>) Angle.zero (Angle.degree 360.0<deg>)
        | ("CPL1" | "CPL2"), name when name = retardanceName ->
            angleUnknown (Angle.degree 90.0<deg>) Angle.zero (Angle.degree 180.0<deg>)
        | "CPL1", name when name = fastAxisOffsetName ->
            angleUnknown Angle.zero (Angle.degree -45.0<deg>) (Angle.degree 45.0<deg>)
        | ("QZ" | "LR"), name when List.contains name muellerNames ->
            unknownBetween axis.defaultValue (ParameterValue -2.0) (ParameterValue 2.0)
        | _ -> KnownParameter axis.defaultValue

    let private unknownNamesFor (spec : AcceptanceItem) : ParameterName list =
        match spec.label.value with
        | "LP2" -> [ ParameterName.dialZero ]
        | "CPL1" -> [ retardanceName; fastAxisOffsetName ]
        | "CPL2" -> [ retardanceName; ParameterName.dialZero ]
        | "QZ" | "LR" -> muellerNames
        | _ -> []

    let private formOf (model : TableAndElementRotationModel) : AddItemForm =
        model.addItemFormOpt |> Option.defaultWith (fun () -> failwith "Items: expected an open Add item form; actual none")

    let private choiceFor (spec : AcceptanceItem) (offers : ItemModelChoice list) : ItemModelChoice =
        offers
        |> List.tryFind (fun choice ->
            match choice, spec.bindingOpt with
            | SourcePresetChoice (model, source), Some preset -> model.entry.id = spec.modelId && source.id = preset
            | StackSampleChoice (model, _, sample), Some bound -> model.entry.id = spec.modelId && sample.id.toElementValueId = bound
            | PolarizerPresetChoice (model, polarizer), Some preset -> model.entry.id = spec.modelId && polarizer.id = preset
            | LibraryModelChoice model, None -> model.entry.id = spec.modelId
            | _ -> false)
        |> Option.defaultWith (fun () -> failwith $"{spec.label.value}.item-json: expected model {spec.modelId.value}; actual no matching offer")

    /// Add `spec` through the Add item form: open it, choose its offer, label it, send `formEdits`, then Add (spec 0050 S12.1, S12.2).
    let private addItemThroughForm
        (formEdits : TableAndElementRotationMsg list)
        (spec : AcceptanceItem)
        (model : TableAndElementRotationModel)
        : TableAndElementRotationModel * ItemModelChoice =
        let file = ItemKitFile.fileNameOf spec.label.fileStem
        let opened = model |> updateTableAndElementRotation OpenAddItemForm
        let offer = choiceFor spec (formOf opened).offers
        let filled =
            opened |> apply ([ ChooseNewItemModel offer; SetNewItemLabel spec.label.caption ] @ formEdits)
        let draft = (formOf filled).itemDraftOpt |> Option.defaultWith (fun () -> failwith $"{file.value}: expected an Add draft; actual none")
        let added = filled |> updateTableAndElementRotation (AddItem draft)
        TestAppScope.expect file "Add item status" None added.kitRefusalOpt
        added, offer

    let private addAcceptanceItem (model : TableAndElementRotationModel) (spec : AcceptanceItem) : TableAndElementRotationModel =
        let file = ItemKitFile.fileNameOf spec.label.fileStem
        let added, offer = addItemThroughForm [ SetNewItemDial spec.dial ] spec model
        let edits =
            offer.parameterAxes @ spec.dial.parameterAxes
            |> List.choose (fun axis ->
                match parameterStateFor spec axis with
                | UnknownParameter _ as state -> Some (SetParameterState (spec.id, axis.name, state))
                | KnownParameter _ | CalibratedParameter _ -> None)
        let edited = added |> apply edits
        TestAppScope.expect file "parameter edit status" None edited.kitRefusalOpt
        edited

    let private gridOf (model : TableAndElementRotationModel) : SetupGrid =
        model.setupGrid |> Option.defaultWith (fun () -> failwith "*.setup-json: expected an open setup grid; actual none")

    let private slotFor (role : SlotRole) (model : TableAndElementRotationModel) : ElementId =
        (SetupSlot.inLightOrder model.elements |> List.find (fun roled -> roled.role = role)).slot.id

    let private gridItems (slot : ElementId) (labels : ItemLabel list) : TableAndElementRotationMsg list =
        labels |> List.map (fun label -> ToggleSetupGridItem (slot, itemIdOf label))

    let private gridAngles (slot : ElementId) : TableAndElementRotationMsg list =
        [ BarDegrees 0.0; BarDegrees 45.0; BarDegrees 90.0 ] |> List.map (fun angle -> AddSetupGridAngle (slot, angle))

    let private generateGrid (omit : GridCombination -> bool) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        TestAppScope.expect KitFile.fileName "grid draft capture" None model.draftCapture
        let exclusions = (gridOf model).combinations |> List.filter omit |> List.map ToggleSetupGridCombination
        let chosen = model |> apply exclusions
        chosen |> updateTableAndElementRotation (GenerateSetupGrid (gridOf chosen))

    let private correctSetup (original : SetupName) (corrected : SetupName) (role : SlotRole) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let loaded = model |> updateTableAndElementRotation (ActivateSetup original)
        let slot = slotFor role loaded
        let index = loaded.elements |> List.findIndex (fun element -> element.id = slot) |> SceneElementIndex
        // Spec 0050 S12.1: transient focus needs no pointer; RotSetAxis owns the edit (0421-impl-log.md).
        let focused =
            {
                loaded with selection = ElementSelected index
            }
        focused
        |> apply
            [
                RotSetAxis (RotationControls.R1, BarDegrees -90.0)
                SetSetupName corrected.caption
                CreateSetup
            ]

    let private buildAcceptanceSetups (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let table =
            model
            |> apply
                [
                    AddElement LinearPolarizer
                    SlideSelectedTo (BarMeters -0.4)
                    AddElement Sample
                    SlideSelectedTo (BarMeters 0.25)
                    AddElement LinearPolarizer
                    SlideSelectedTo (BarMeters 0.5)
                    AssignSlot (sourceSlot, Some (itemIdOf (ItemLabel "Laser")))
                    AssignSlot (detectorSlot, Some (itemIdOf (ItemLabel "Phone")))
                ]
        let roles = [ SourceSlotRole; prepRole; firstObjectRole; secondObjectRole; analyzerRole; detectorRole ]
        TestAppScope.expect KitFile.fileName "table light order" roles (SetupSlot.inLightOrder table.elements |> List.map _.role)
        let prep = slotFor prepRole table
        let secondObject = slotFor secondObjectRole table
        let analyzer = slotFor analyzerRole table
        let polarizers =
            gridItems prep [ ItemLabel "LP1"; ItemLabel "CPL1" ]
            @ gridItems analyzer [ ItemLabel "LP2"; ItemLabel "CPL2" ]
        let firstGrid =
            table
            |> updateTableAndElementRotation OpenSetupGrid
            |> apply (polarizers @ gridItems firstObjectSlot [ ItemLabel "AIR"; ItemLabel "QZ"; ItemLabel "LR" ] @ gridAngles firstObjectSlot)
        let air = itemIdOf (ItemLabel "AIR")
        let singles =
            firstGrid
            |> generateGrid (fun combination ->
                combination.picks |> List.exists (fun pick ->
                    pick.slot = firstObjectSlot && pick.choice.itemOpt = Some air && pick.choice.r1 <> Angle.zero))
        TestAppScope.expect (FileName "*.setup-json") "first grid setup count" 28 singles.kitSetups.Length
        let secondGrid =
            singles
            |> apply [ CloseSetupGrid; OpenSetupGrid ]
            |> apply (polarizers @ gridItems firstObjectSlot [ ItemLabel "QZ" ] @ gridAngles firstObjectSlot
                       @ gridItems secondObject [ ItemLabel "LR" ] @ gridAngles secondObject)
        let pairs =
            secondGrid
            |> generateGrid (fun combination ->
                let first = combination.picks |> List.find (fun pick -> pick.slot = firstObjectSlot)
                let second = combination.picks |> List.find (fun pick -> pick.slot = secondObject)
                first.choice.r1 <> second.choice.r1)
        TestAppScope.expect (FileName "*.setup-json") "both grids setup count" 40 pairs.kitSetups.Length
        pairs
        |> updateTableAndElementRotation CloseSetupGrid
        |> correctSetup (SetupName "CPL-(LR#90)-CPL") (SetupName "CPL-(LR#-90)-CPL") firstObjectRole
        |> correctSetup (SetupName "CPL-(QZ#90-LR#90)-CPL") (SetupName "CPL-(QZ#90-LR#-90)-CPL") secondObjectRole

    let private labFileAt (folder : KitFolder) (path : KitRelativePath) : FileName =
        FileName (Path.Combine(folder.value.value, path.value))

    let private readAcceptanceLabFiles (folder : KitFolder) : AcceptanceLabFile list =
        finalLabFiles
        |> List.map (fun path ->
            let file = labFileAt folder path
            let content = KitFileText (File.ReadAllText file.value)
            let mapped =
                (BuiltInProfileChoice PhoneRoiCsvProfile).mapText content
                |> Result.defaultWith (fun error -> failwith $"{file.value}: {error.caption.value}")
            let captures =
                match mapped with
                | CapturePoints rows -> rows
                | _ -> failwith $"{file.value}: expected Phone ROI captures"
            let groups : AcceptanceLabGroup list =
                captures
                |> List.groupBy _.experiment
                |> List.map (fun (label, rows) ->
                    let key = label.rowKey
                    let name =
                        key.nameText.toExperimentName
                        |> Result.defaultWith (fun error -> failwith $"{file.value}: {error.describe}")
                    {
                        key = key
                        name = name
                        captures = rows
                    })
            {
                path = path
                groups = groups
            })

    let private restartTimeOf (files : AcceptanceLabFile list) : RowTimestamp =
        let diagnostic = files |> List.find (fun file -> file.path = diagnosticFile)
        let firstCheck = diagnostic.groups |> List.find (fun group -> group.key = DataRowKey "BullshitCheck")
        firstCheck.captures |> List.map (fun row -> RowTimestamp row.capturedAt) |> List.min

    let private roleForGroup (file : AcceptanceLabFile) (group : AcceptanceLabGroup) : ExperimentRole =
        if file.path = darknessFile then DarkRun
        elif file.path = diagnosticFile then DiagnosticRun
        elif List.contains group.name calibrationNames then CalibrationRun
        else ScienceRun

    let private sessionForGroup (restart : RowTimestamp) (file : AcceptanceLabFile) (group : AcceptanceLabGroup) : SessionName option =
        if file.path = lpLpFile then Some (SessionName "LP-LP")
        elif file.path = lpCplFile then Some (SessionName "LP-CPL")
        elif file.path = cplLpFile then
            if group.captures |> List.forall (fun row -> row.capturedAt < restart.value) then
                Some (SessionName "CPL-LP before the restart")
            else Some (SessionName "CPL-LP after the restart")
        elif file.path = cplCplDay1File then Some (SessionName "CPL-CPL day 1")
        elif file.path = cplCplDay2File then Some (SessionName "CPL-CPL day 2")
        else None

    let private excludedNumbersOf (group : AcceptanceLabGroup) : MappedPointNumber list =
        MuellerReference.excludedPoints
        |> List.filter (fun excluded -> excluded.experiment.rowKey = group.key)
        |> List.map (fun excluded ->
            group.captures
            |> List.mapi (fun index row -> MappedPointNumber (index + 1), row)
            |> List.filter (fun (_, row) -> row.captureIndex = excluded.captureIndex && row.description = excluded.description)
            |> List.exactlyOne
            |> fst)

    let private splitOf (file : FileName) (model : TableAndElementRotationModel) : DataSplitState =
        model.dataSplitOpt |> Option.defaultWith (fun () -> failwith $"{file.value}: expected the split preview")

    let private attachAcceptanceLabFile
        (folder : KitFolder)
        (model : TableAndElementRotationModel)
        (lab : AcceptanceLabFile)
        : TableAndElementRotationModel =
        let file = labFileAt folder lab.path
        let split = model |> updateTableAndElementRotation (SplitByColumn (file, ImportProfileName.phoneRoiCsv))
        let preview = splitOf file split
        TestAppScope.expect file "split key column" (SplitKeyColumn "experiment") preview.key
        TestAppScope.expect file "split group order" (lab.groups |> List.map _.key) (preview.split.groups |> List.map _.key)
        let offers =
            preview.split.groups
            |> List.map (fun group ->
                match group.matching with
                | SetupOfferForGroup offer ->
                    TestAppScope.expect file "template offers an existing grid setup" true (List.contains offer.setup model.kitSetups)
                    ToggleSplitOffer group.key
                | GroupWithoutMatch _ ->
                    let setup =
                        manualSetups |> Map.tryFind group.key
                        |> Option.defaultWith (fun () -> failwith $"{file.value}: unexpected unreadable key {group.key.value}")
                    OfferSplitGroupOver (group.key, setup)
                | ExperimentForGroup name -> failwith $"{file.value}: {name.value} was already recorded")
        let offered = split |> apply offers
        let ready = splitOf file offered
        TestAppScope.expect file "unmatched groups after manual offers" [] ready.split.unmatched
        TestAppScope.expect file "every group selected" (lab.groups |> List.map _.key |> List.sort) (ready.creating |> List.sort)
        let attached = offered |> updateTableAndElementRotation MatchByTemplate
        TestAppScope.expect file "split completed" None attached.dataSplitOpt
        let appended = attached.experimentCollection.experiments |> List.skip model.experimentCollection.experiments.Length
        TestAppScope.expect file "created names in file group order" (lab.groups |> List.map _.name) (appended |> List.map _.name)
        TestAppScope.expect file "no setup added by the split" model.kitSetups attached.kitSetups
        attached

    let private buildAcceptanceExperiments (folder : KitFolder) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let activated = model |> updateTableAndElementRotation (ActivateSetup (SetupName "LP-(AIR#0)-LP"))
        let analyzer = slotFor analyzerRole activated
        let analyzerSlot = activated.elements |> List.find (fun slot -> slot.id = analyzer)
        TestAppScope.expect KitFile.fileName "dial reading offered for LP2" true (List.contains VaryDialReading (variablesOfElement activated analyzerSlot))
        let captures = availableCaptures activated analyzer
        TestAppScope.expect KitFile.fileName "Phone capture" [ CaptureTransmitted ] captures
        let captureChoice =
            match List.exactlyOne captures with
            | CaptureTransmitted -> ExperimentControls.CaptureT
            | CaptureReflected -> ExperimentControls.CaptureR
            | CaptureBoth -> ExperimentControls.CaptureBoth
        let drafted =
            activated |> apply [ ExpChooseElement analyzer; ExpChooseVariable ExperimentControls.VaryDialReading; ExpChooseMeasurement captureChoice ]
        TestAppScope.expect KitFile.fileName "raw dial draft" (Some VaryDialReading) drafted.experimentCollection.draft.variable
        TestAppScope.expect KitFile.fileName "raw dial range" analyzerDialRange drafted.experimentCollection.draft.range
        TestAppScope.expect KitFile.fileName "draft capture" (Some CaptureTransmitted) drafted.draftCapture
        let files = readAcceptanceLabFiles folder
        let restart = restartTimeOf files
        let attached = files |> List.fold (attachAcceptanceLabFile folder) drafted
        let edits =
            [
                for file in files do
                    for group in file.groups do
                        let experiment = attached.experimentCollection.experiments |> List.find (fun held -> held.name = group.name)
                        match roleForGroup file group with
                        | ScienceRun -> ()
                        | role -> yield SetExperimentRole (experiment.id, role)
                        match sessionForGroup restart file group with
                        | Some session -> yield SetExperimentSession (experiment.id, session.caption)
                        | None -> ()
                        match correctedSetups |> Map.tryFind group.name with
                        | Some name ->
                            let setup = attached.kitSetups |> List.find (fun held -> held.name = name)
                            yield CorrectSetup (experiment.id, setup.reference, lrCorrectionNote)
                        | None -> ()
                        for number in excludedNumbersOf group do
                            yield ReviewExperimentData experiment.id
                            yield ExcludePoint (experiment.id, number, frameExclusionReason)
            ]
        attached |> apply edits

    /// Runs `request` through the shell's `StudyFitJobHost.posting`, its post running each job at once, then the recorded messages through the update in order; a fit not ended by `deadline` fails the fact naming the request (spec 0050 S12.1, S12.2).
    let private runStudyFitThroughShell
        (model : TableAndElementRotationModel)
        (request : StudyFitRequest)
        (deadline : TimeSpan)
        : TableAndElementRotationModel =
        let file = FileName $"{request.study.fileStem.value}@*.fitrun-json"
        let requested = $"the {request.stage.text} fit of {request.study.value} ({request.calibrationUse.text})"
        use ended = new ManualResetEventSlim (false)
        let messages = ConcurrentQueue<TableAndElementRotationMsg> ()
        let host =
            StudyFitJobHost.posting
                (fun action -> action ())
                (fun progress -> messages.Enqueue (StudyFit (StudyFitProgressed progress)))
                (fun result ->
                    messages.Enqueue (StudyFit (StudyFitEnded result))
                    ended.Set ())
        let ready =
            {
                model with
                    kitShell =
                        {
                            model.kitShell with studyFits = host
                        }
            }
        let running = ready |> updateTableAndElementRotation (StudyFit (RunFit request))
        TestAppScope.expect file $"{requested} started" true running.studyFit.job.isRunning
        if not (ended.Wait deadline) then
            Assert.Fail($"{file.value}: expected {requested} to end within {deadline.TotalSeconds} s; actual no end")
        let recorded = messages.ToArray () |> Array.toList
        let completions = recorded |> List.choose (function StudyFit (StudyFitEnded result) -> Some result | _ -> None)
        TestAppScope.expect file "completion message count" 1 completions.Length
        let finished = running |> apply recorded
        match finished.studyFit.errorOpt with
        | Some error -> Assert.Fail($"{file.value}: expected {requested} to succeed; actual {error.describe}")
        | None -> ()
        TestAppScope.expect file "fit finished" false finished.studyFit.job.isRunning
        TestAppScope.expect file "fit handle released" None finished.studyFit.handleOpt
        match completions with
        | [ Ok run ] ->
            TestAppScope.expect file "committed fit run" (Some run.name) finished.studyFit.job.committedResults
            TestAppScope.expect file "appended fit run names" ((model.kitFitRuns |> List.map _.name) @ [ run.name ]) (finished.kitFitRuns |> List.map _.name)
        | [ Error error ] -> Assert.Fail($"{file.value}: expected {requested} to end with a fit run; actual {error.describe}")
        | _ -> Assert.Fail($"{file.value}: expected one completion of {requested}; actual {completions.Length}")
        {
            finished with kitShell = model.kitShell
        }

    /// AT-1's fit of `stage` through the shell within its sixty seconds, committing `expectedName` (spec 0050 S12.1, S12.2).
    let private runAcceptanceFit
        (stage : FitStage)
        (expectedName : FitRunName)
        (model : TableAndElementRotationModel)
        : TableAndElementRotationModel =
        let file = FitRunKitFile.fileNameOf expectedName.fileStem
        let request : StudyFitRequest =
            {
                study = studyName
                stage = stage
                calibrationUse = CalibrationHeldFixed
            }
        let finished = runStudyFitThroughShell model request (TimeSpan.FromSeconds 60.0)
        TestAppScope.expect file "committed fit run" (Some expectedName) finished.studyFit.job.committedResults
        finished

    let private buildAcceptanceStudy
        (setTime : ClockTime -> unit)
        (model : TableAndElementRotationModel)
        : TableAndElementRotationModel =
        let file = StudyKitFile.fileNameOf studyName.fileStem
        let experiments = model.experimentCollection.experiments |> List.map _.name
        TestAppScope.expect file "study experiment count" 53 experiments.Length
        let configured =
            model
            |> apply
                ([ StudyCommand (CreateStudy (studyName, experiments)); StudyCommand (SetFitMode (studyName, StagedFit)) ]
                 @ (sessionGainSettings |> List.map (fun setting -> StudyCommand (SetSessionGainModel (studyName, setting)))))
        TestAppScope.expect file "study configuration status" None configured.kitRefusalOpt
        setTime calibrationTime
        let calibrated = configured |> runAcceptanceFit CalibrationStage calibrationRunName
        let calibrationFile = FitRunKitFile.fileNameOf calibrationRunName.fileStem
        let calibrationText = calibrated.kit.proxy.readKindFile FitRunKitFile calibrationRunName.fileStem |> TestAppScope.readKitResult calibrationFile
        let published = calibrated |> updateTableAndElementRotation (PublishCalibration calibrationRunName)
        for label in [ ItemLabel "LP2"; ItemLabel "CPL1"; ItemLabel "CPL2" ] do
            let item = published.kitItems |> List.find (fun held -> held.label = label)
            TestAppScope.expect (ItemKitFile.fileNameOf label.fileStem) "published revision" (ItemRevision 2) item.revision
        setTime objectTime
        let reconstructed = published |> runAcceptanceFit ObjectStage objectRunName
        TestAppScope.expect calibrationFile "calibration run unchanged after publication and stage two" calibrationText
            (reconstructed.kit.proxy.readKindFile FitRunKitFile calibrationRunName.fileStem |> TestAppScope.readKitResult calibrationFile)
        reconstructed

    /// Mints `ids` in order and fails on a request past the last; the cell holds the ids not yet minted (spec 0050 S12.1, S12.2).
    let private mintingFrom<'Id> (file : FileName) (what : string) (ids : 'Id list) : (unit -> 'Id) * 'Id list ref =
        let unused = ref ids
        let mint () : 'Id =
            match unused.Value with
            | next :: rest ->
                unused.Value <- rest
                next
            | [] -> failwith $"{file.value}: expected {ids.Length} {what} requests; actual an extra request"
        mint, unused

    /// The folder a run's kit, zip and received copy live in, `KitAcceptanceTests/<run>` under the test output (spec 0050 S12.1, S12.2).
    let private runFolderOf (run : FolderName) : FolderName =
        FolderName (Path.Combine(System.AppContext.BaseDirectory, "KitAcceptanceTests", run.value))

    /// Build through application messages, retaining the setup and experiment checkpoints (spec 0050 S12.1).
    let private buildOpticsMuellerKitWith
        (finish : (ClockTime -> unit) -> KitFolder -> TableAndElementRotationModel -> TableAndElementRotationModel)
        (run : FolderName)
        : BuiltAcceptanceKit =
        let runFolder = runFolderOf run
        let folder = KitFolder (runFolder.combine (FolderName "optics-mueller"))
        if Directory.Exists runFolder.value then failwith $"{runFolder.value}: expected a fresh run folder; actual already exists"
        let mutable openedOpt : OpenedKit option = None
        try
            let sourceData = Path.Combine((TestAppScope.committedFolder OpticsMuellerKit).value.value, "data")
            let targetData = Path.Combine(folder.value.value, "data")
            Directory.CreateDirectory targetData |> ignore
            for directory in Directory.EnumerateDirectories(sourceData, "*", SearchOption.AllDirectories) do
                Directory.CreateDirectory(Path.Combine(targetData, Path.GetRelativePath(sourceData, directory))) |> ignore
            for file in Directory.EnumerateFiles(sourceData, "*", SearchOption.AllDirectories) do
                File.Copy(file, Path.Combine(targetData, Path.GetRelativePath(sourceData, file)))
            let context = TestAppScope.freshAppContext (FolderName "KitAcceptanceTests")
            let clock, setTime = TestAppScope.testClockAt initialTime
            let start, shown = TestAppScope.kitStartOver context clock TestAppScope.kitId TestAppScope.kitAuthor folder
            let started = start |> updateKitStart (CreateKit folder)
            openedOpt <- shown.Value
            TestAppScope.expect KitFile.fileName "CreateKit status" None started.status
            let opened = openedOpt |> Option.defaultWith (fun () -> failwith $"{folder.value.value}: expected CreateKit to open the kit; actual no kit")
            let mintItemId, unusedItemIds = mintingFrom (FileName "*.item-json") "item-id" (itemSpecs |> List.map _.id)
            let mintElementId, unusedSlotIds = mintingFrom (FileName "*.setup-json") "added-slot id" addedSlotIds
            let itemIds : ItemIdProvider =
                {
                    mintItemId = mintItemId
                }
            let elementIds : ElementIdProvider =
                {
                    mintElementId = mintElementId
                }
            let initial =
                {
                    TestAppScope.inverseModelOver context opened with itemIds = itemIds; elementIds = elementIds
                }
                |> apply [ SetNamingTemplateText labTemplate; SaveNamingTemplate ]
            let stocked = itemSpecs |> List.fold addAcceptanceItem initial
            TestAppScope.expect (FileName "*.item-json") "unused item identities" [] unusedItemIds.Value
            let setups = buildAcceptanceSetups stocked
            TestAppScope.expect (FileName "*.setup-json") "unused added-slot identities" [] unusedSlotIds.Value
            TestAppScope.expect (FileName "*.setup-json") "setups including both corrections" 42 setups.kitSetups.Length
            let model = finish setTime folder setups
            {
                folder = folder
                context = context
                model = model
                clock = clock
                setTime = setTime
            }
        with _ ->
            try
                match openedOpt with
                | Some opened -> opened.library.kit.close () |> TestAppScope.readKitResult KitFile.fileName
                | None -> ()
            finally
                if Directory.Exists runFolder.value then Directory.Delete(runFolder.value, true)
            reraise ()

    let private buildOpticsMuellerKit (run : FolderName) : BuiltAcceptanceKit =
        buildOpticsMuellerKitWith (fun setTime folder model -> model |> buildAcceptanceExperiments folder |> buildAcceptanceStudy setTime) run

    let private expectedSetups () : AcceptanceSetup list =
        // Spec 0050 S12.1: the Rotate bay stores -90 degrees in its [0, 360) representation.
        let correctedTurn = Angle.degree 270.0<deg>
        let objects =
            [
                {
                    caption = ControlCaption "AIR#0"
                    first = ItemLabel "AIR", Angle.zero
                    secondOpt = None
                }
                for label in [ ItemLabel "QZ"; ItemLabel "LR" ] do
                    for angle in [ Angle.zero; Angle.degree 45.0<deg>; Angle.degree 90.0<deg> ] do
                        {
                            caption = ControlCaption $"{label.value}#{angle.degrees:g}"
                            first = label, angle
                            secondOpt = None
                        }
                for angle in [ Angle.zero; Angle.degree 45.0<deg>; Angle.degree 90.0<deg> ] do
                    {
                        caption = ControlCaption $"QZ#{angle.degrees:g}-LR#{angle.degrees:g}"
                        first = ItemLabel "QZ", angle
                        secondOpt = Some (ItemLabel "LR", angle)
                    }
            ]
        [
            for prep in [ ItemLabel "LP1"; ItemLabel "CPL1" ] do
                for analyzer in [ ItemLabel "LP2"; ItemLabel "CPL2" ] do
                    for pair in objects do
                        let sourceName = if prep = ItemLabel "LP1" then "LP" else "CPL"
                        let analyzerName = if analyzer = ItemLabel "LP2" then "LP" else "CPL"
                        {
                            name = SetupName $"{sourceName}-({pair.caption.value})-{analyzerName}"
                            prep = prep
                            objects = pair
                            analyzer = analyzer
                        }
            {
                name = SetupName "CPL-(LR#-90)-CPL"
                prep = ItemLabel "CPL1"
                objects =
                    {
                        caption = ControlCaption "LR#-90"
                        first = ItemLabel "LR", correctedTurn
                        secondOpt = None
                    }
                analyzer = ItemLabel "CPL2"
            }
            {
                name = SetupName "CPL-(QZ#90-LR#-90)-CPL"
                prep = ItemLabel "CPL1"
                objects =
                    {
                        caption = ControlCaption "QZ#90-LR#-90"
                        first = ItemLabel "QZ", Angle.degree 90.0<deg>
                        secondOpt = Some (ItemLabel "LR", correctedTurn)
                    }
                analyzer = ItemLabel "CPL2"
            }
        ]

    let private assertStoredItems (built : BuiltAcceptanceKit) (reader : KitProxy) : unit =
        let items = reader.readItems () |> TestAppScope.readKitResult (FileName "*.item-json")
        TestAppScope.expect (FileName "*.item-json") "labels" (itemSpecs |> List.map _.label |> List.sort) (items |> List.map _.label |> List.sort)
        let models = built.context.itemModels.listItemModels () |> Result.defaultWith (fun error -> failwith error.describe)
        for spec in itemSpecs do
            let file = ItemKitFile.fileNameOf spec.label.fileStem
            let item = items |> List.find (fun item -> item.label = spec.label)
            let model = models |> List.find (fun model -> model.entry.id = spec.modelId)
            let expectedModel =
                match spec.bindingOpt with
                | Some preset -> PresetInstance (model.versionId, preset)
                | None -> ItemModelVersionInstance model.versionId
            TestAppScope.expect file "id" spec.id item.id
            TestAppScope.expect file "model and version" expectedModel item.model
            TestAppScope.expect file "dial" spec.dial item.dial
            TestAppScope.expect file "revision" ItemRevision.first item.revision
            TestAppScope.expect file "history" [] item.history
            TestAppScope.expect file "photo" None item.photoOpt
            TestAppScope.expect file "notes" (ItemNotes "") item.notes
            let axes =
                match spec.bindingOpt with
                | Some preset ->
                    match built.context.library.tryGetEntry preset with
                    | Ok (Some (SourceItem source)) -> (SourcePresetChoice (model, source)).parameterAxes
                    | _ -> failwith $"{file.value}: expected source preset {preset.value}; actual unavailable"
                | None -> model.entry.model.parameterAxes
            let expectedParameters : Parameter list =
                axes @ spec.dial.parameterAxes
                |> List.map (fun axis ->
                    {
                        name = axis.name
                        state = parameterStateFor spec axis
                    })
            TestAppScope.expect file "parameters (including starts, bounds, priors and known defaults)" expectedParameters item.parameters
            let unknowns =
                item.parameters
                |> List.choose (fun parameter ->
                    match parameter.state with
                    | UnknownParameter _ -> Some parameter.name
                    | KnownParameter _ | CalibratedParameter _ -> None)
                |> List.sort
            TestAppScope.expect file "unknown parameter names" (unknownNamesFor spec |> List.sort) unknowns
            if spec.label = ItemLabel "Phone" then
                TestAppScope.expect file "gain" (Some (KnownParameter (ParameterValue 1.0))) (item.tryFindParameter ParameterName.gain |> Option.map _.state)
                TestAppScope.expect file "dark offset" (Some (KnownParameter (ParameterValue 0.0))) (item.tryFindParameter ParameterName.darkOffset |> Option.map _.state)

    let private assertStoredSetups (reader : KitProxy) : unit =
        let setups = reader.readSetups () |> TestAppScope.readKitResult (FileName "*.setup-json")
        let expected = expectedSetups ()
        TestAppScope.expect (FileName "*.setup-json") "names" (expected |> List.map _.name |> List.sort) (setups |> List.map _.name |> List.sort)
        for spec in expected do
            let setup = setups |> List.find (fun setup -> setup.name = spec.name)
            let file = SetupKitFile.fileNameOf spec.name.fileStem
            let ordered = setup.lightOrder
            TestAppScope.expect file "revision" SetupRevision.first setup.revision
            TestAppScope.expect file "history" [] setup.history
            TestAppScope.expect file "light order" [ SourceSlotRole; prepRole; firstObjectRole; secondObjectRole; analyzerRole; detectorRole ] (ordered |> List.map _.role)
            let slot (role : SlotRole) : SetupSlot = (ordered |> List.find (fun held -> held.role = role)).slot
            let fixedItem (role : SlotRole) (label : ItemLabel) (angle : Angle) : unit =
                let held = slot role
                TestAppScope.expect file $"{role.text} content" (ItemInSlot (itemIdOf label, PlacementTurn)) held.content
                TestAppScope.expect file $"{role.text} angle" angle held.placement.r1
            fixedItem SourceSlotRole (ItemLabel "Laser") Angle.zero
            fixedItem prepRole spec.prep Angle.zero
            let firstLabel, firstAngle = spec.objects.first
            fixedItem firstObjectRole firstLabel firstAngle
            match spec.objects.secondOpt with
            | Some (label, angle) -> fixedItem secondObjectRole label angle
            | None ->
                TestAppScope.expect file "Obj 2 content" EmptySlotContent (slot secondObjectRole).content
                TestAppScope.expect file "Obj 2 angle" Angle.zero (slot secondObjectRole).placement.r1
            let reading = if spec.analyzer = ItemLabel "LP2" then Angle.degree 152.0<deg> else Angle.degree 80.0<deg>
            TestAppScope.expect file "Analyzer 1 content and raw dial reading" (ItemInSlot (itemIdOf spec.analyzer, DialTurn reading)) (slot analyzerRole).content
            TestAppScope.expect file "Analyzer 1 physical angle" Angle.zero (slot analyzerRole).placement.r1
            fixedItem detectorRole (ItemLabel "Phone") Angle.zero

    let private kitContentFiles (folder : KitFolder) : Set<FileName> =
        TestAppScope.relativeFiles folder.value
        |> List.filter (fun file -> not (file.value.StartsWith($".oc{Path.DirectorySeparatorChar}", StringComparison.Ordinal)))
        |> Set.ofList

    let private assertSameBytes (expected : FileName) (actual : FileName) : unit =
        let expectedBytes = File.ReadAllBytes expected.value
        let actualBytes = File.ReadAllBytes actual.value
        TestAppScope.expect actual $"bytes against {expected.value}" expectedBytes actualBytes

    /// Fails at the first relative path, `.oc/` left out of both, that `other` lacks, adds or holds in other bytes, naming `built`'s folder (spec 0050 S12.2).
    let private assertSameKitFiles (built : KitFolder) (other : KitFolder) : unit =
        let builtFiles = kitContentFiles built
        let otherFiles = kitContentFiles other
        for file in Set.union builtFiles otherFiles do
            let diagnostic = $"Built kit: {built.value.value}\n{file.value}"
            if not (Set.contains file otherFiles) then
                Assert.Fail($"{diagnostic}: missing from {other.value.value} (extra in the built kit).")
            elif not (Set.contains file builtFiles) then
                Assert.Fail($"{diagnostic}: extra in {other.value.value} (missing from the built kit).")
            else
                try assertSameBytes (file.combine other.value) (file.combine built.value)
                with :? Xunit.Sdk.XunitException as error ->
                    Assert.Fail($"{diagnostic}: different bytes.\n{error.Message}")

    /// The `.optics-kit` a build's start screen and template write: created and modified at `instant`, no notes, profile or exclusion (spec 0050 S12.1, S12.2).
    let private assertKitFile
        (reader : KitProxy)
        (kitId : KitId)
        (name : KitName)
        (author : KitAuthor)
        (instant : ClockTime)
        (template : ControlCaption)
        : unit =
        let metadata = reader.readKitFile () |> TestAppScope.readKitResult KitFile.fileName
        TestAppScope.expect KitFile.fileName "schema version" KitFile.currentSchemaVersion metadata.schemaVersion
        TestAppScope.expect KitFile.fileName "id" kitId metadata.id
        TestAppScope.expect KitFile.fileName "name" name metadata.name
        TestAppScope.expect KitFile.fileName "author" author metadata.author
        TestAppScope.expect KitFile.fileName "notes" (KitNotes "") metadata.notes
        TestAppScope.expect KitFile.fileName "created" instant metadata.created
        TestAppScope.expect KitFile.fileName "modified" instant metadata.modified
        TestAppScope.expect KitFile.fileName "naming template" template.value metadata.namingTemplate.value
        TestAppScope.expect KitFile.fileName "import profiles" [] metadata.importProfiles
        TestAppScope.expect KitFile.fileName "send exclusions" [] metadata.sendExclude

    let private assertLibrarySnapshots (built : BuiltAcceptanceKit) (reader : KitProxy) : unit =
        let snapshots = reader.listSnapshots () |> TestAppScope.readKitResult (FileName "library/*")
        let library = built.folder.value.combine (FolderName "library")
        TestAppScope.expect (FileName library.value) "snapshot files" (snapshots |> List.map _.snapshotFileName |> List.sort) (TestAppScope.relativeFiles library)
        let requiredModels = itemSpecs |> List.map _.modelId |> List.distinct |> List.sort
        TestAppScope.expect (FileName "library/*.item-model-json") "instantiated model count" 6 requiredModels.Length
        let storedModels =
            snapshots
            |> List.choose (fun envelope ->
                match envelope.content.toItemModel with
                | Ok entry -> Some entry.id
                | Error _ -> None)
            |> List.filter (fun id -> List.contains id requiredModels)
            |> List.sort
        TestAppScope.expect (FileName "library/*.item-model-json") "instantiated models" requiredModels storedModels
        let builtIn = CatalogueProxy.builtInFolderUnder built.context.locations.shippedCatalogueFolder
        for envelope in snapshots do
            let source = envelope.content.fileName.combine builtIn
            let target = envelope.snapshotFileName.combine library
            TestAppScope.expect target $"built-in entry {source.value} exists" true (File.Exists source.value)
            assertSameBytes source target

    let private assertLabBytes (folder : KitFolder) : unit =
        let original = (TestAppScope.committedFolder OpticsMuellerKit).value.combine (FolderName "data")
        let copied = folder.value.combine (FolderName "data")
        let files = TestAppScope.relativeFiles original
        TestAppScope.expect (FileName copied.value) "lab files" files (TestAppScope.relativeFiles copied)
        for file in files do assertSameBytes (file.combine original) (file.combine copied)

    let private hashOfFile (file : FileName) : ContentHash =
        let bytes = File.ReadAllBytes file.value |> FileByteContent
        ContentHash.ofBytes bytes

    let private labHashes (folder : KitFolder) : Map<FileName, ContentHash> =
        let data = folder.value.combine (FolderName "data")
        TestAppScope.relativeFiles data |> List.map (fun file -> file, hashOfFile (file.combine data)) |> Map.ofList

    let private labelledSetupFor (template : NamingTemplate) (group : AcceptanceLabGroup) : SetupName =
        match manualSetups |> Map.tryFind group.key with
        | Some name -> name
        | None ->
            let reading =
                template.readName group.key.nameText
                |> Result.defaultWith (fun error -> failwith $"{group.key.value}: {error.describe}")
            let withoutNote =
                {
                    reading with noteOpt = None
                }
            let written =
                template.writeName withoutNote
                |> Result.defaultWith (fun error -> failwith $"{group.key.value}: {error.describe}")
            written.toSetupName |> Result.defaultWith (fun error -> failwith $"{group.key.value}: {error.describe}")

    let private assertStoredExperiments (built : BuiltAcceptanceKit) (reader : KitProxy) (files : AcceptanceLabFile list) : unit =
        let experimentsFile = FileName "*.experiment-json"
        let experiments = reader.readExperiments () |> TestAppScope.readKitResult experimentsFile
        let setups = reader.readSetups () |> TestAppScope.readKitResult (FileName "*.setup-json")
        let metadata = reader.readKitFile () |> TestAppScope.readKitResult KitFile.fileName
        let groups = files |> List.collect _.groups
        TestAppScope.expect experimentsFile "lab group count" 53 groups.Length
        TestAppScope.expect experimentsFile "experiment count" 53 experiments.Length
        TestAppScope.expect experimentsFile "experiment order" (groups |> List.map _.name) (experiments |> List.map _.name)
        TestAppScope.expect experimentsFile "ids in file group order" (groups |> List.mapi (fun index _ -> ExperimentId (index + 1))) (experiments |> List.map _.id)
        TestAppScope.expect experimentsFile "stored model" built.model.experimentCollection.experiments experiments
        TestAppScope.expect experimentsFile "Calibration runs" (calibrationNames |> List.sort) (experiments |> List.filter (fun experiment -> experiment.role = CalibrationRun) |> List.map _.name |> List.sort)
        for role, count in [ ScienceRun, 36; CalibrationRun, 9; DarkRun, 6; DiagnosticRun, 2 ] do
            TestAppScope.expect experimentsFile $"{role.text} count" count (experiments |> List.filter (fun experiment -> experiment.role = role) |> List.length)
        TestAppScope.expect experimentsFile "gain session count" 6 (experiments |> List.choose _.sessionOpt |> List.distinct |> List.length)
        TestAppScope.expect experimentsFile "setup corrections" 2 (experiments |> List.sumBy (fun experiment -> experiment.corrections.Length))
        TestAppScope.expect experimentsFile "excluded frames" 1 (experiments |> List.sumBy (fun experiment -> experiment.dataOpt |> Option.map (fun data -> data.exclusions.Length) |> Option.defaultValue 0))
        let restart = restartTimeOf files
        let original = TestAppScope.committedFolder OpticsMuellerKit
        for lab in files do
            let source = labFileAt original lab.path
            let attachment : DataAttachment =
                {
                    path = lab.path
                    contentHash = hashOfFile source
                }
            for group in lab.groups do
                let experiment = experiments |> List.find (fun held -> held.name = group.name)
                let file = ExperimentKitFile.fileNameOf group.name.fileStem
                let labelledName = labelledSetupFor metadata.namingTemplate group
                let labelled = setups |> List.find (fun setup -> setup.name = labelledName)
                let correctedNameOpt = correctedSetups |> Map.tryFind group.name
                let expectedName = correctedNameOpt |> Option.defaultValue labelledName
                let expectedSetup = setups |> List.find (fun setup -> setup.name = expectedName)
                TestAppScope.expect file "setup and revision" expectedSetup.reference experiment.setup
                match correctedNameOpt with
                | Some _ ->
                    let correction = experiment.corrections |> List.exactlyOne
                    TestAppScope.expect file "correction's labelled setup" labelled.reference correction.previous
                    TestAppScope.expect file "correction's actual setup" expectedSetup.reference correction.corrected
                    TestAppScope.expect file "correction audit note" lrCorrectionNote.value correction.note.value
                | None -> TestAppScope.expect file "no correction" [] experiment.corrections
                let analyzer = expectedSetup.lightOrder |> List.find (fun slot -> slot.role = analyzerRole)
                let axis : ScanAxis =
                    {
                        elementId = analyzer.slot.id
                        variable = VaryDialReading
                        range = analyzerDialRange
                    }
                let expectedScan = ExperimentScan.oneAxis axis CaptureTransmitted None
                TestAppScope.expect file "analyzer raw-dial sweep and Phone capture" expectedScan experiment.scan
                let dialReadings = analyzerDialRange.sweptValues |> List.map (fun reading -> Angle.degree (reading.value * 1.0<deg>))
                for row in group.captures do
                    TestAppScope.expect file $"capture {row.captureIndex.value} at {row.description.degrees} degrees is on the dial sweep" true (List.contains row.description dialReadings)
                TestAppScope.expect file "role" (roleForGroup lab group) experiment.role
                TestAppScope.expect file "session" (sessionForGroup restart lab group) experiment.sessionOpt
                TestAppScope.expect file "notes" ExperimentNotes.none experiment.notes
                let data = experiment.dataOpt |> Option.defaultWith (fun () -> failwith $"{file.value}: expected its lab attachment")
                TestAppScope.expect file "attachment path and committed SHA-256" attachment data.attachment
                TestAppScope.expect file "import profile" ImportProfileName.phoneRoiCsv data.profile
                TestAppScope.expect file "keyed selection" (KeyedDataRows group.key) data.rows
                TestAppScope.expect file "excluded point numbers in group order" (excludedNumbersOf group) (data.exclusions |> List.map _.point)
                for exclusion in data.exclusions do TestAppScope.expect file "exclusion reason" frameExclusionReason.value exclusion.reason.value
                let mapped =
                    built.model.experimentData.readMapped data
                    |> Result.defaultWith (fun error -> failwith $"{file.value}: {error.caption.value}")
                TestAppScope.expect file "mapped captures, including excluded frames" (CapturePoints group.captures) mapped
                TestAppScope.expect file "all capture descriptions validate against the scan" (Ok mapped) (mapped.validatedFor Intensity experiment.scan)
                let kept =
                    group.captures
                    |> List.filter (fun row ->
                        MuellerReference.excludedPoints
                        |> List.exists (fun excluded ->
                            excluded.experiment = row.experiment && excluded.captureIndex = row.captureIndex && excluded.description = row.description)
                        |> not)
                TestAppScope.expect file "keptBy removes only the reference's contaminated frame" (CapturePoints kept) (mapped.keptBy data.exclusions)

    let private referenceInstrumentAngles : (ItemLabel * ParameterName * BarDegrees) list =
        [
            ItemLabel "LP2", ParameterName.dialZero, BarDegrees MuellerReference.lpAnalyzerZero.degrees
            ItemLabel "CPL2", ParameterName.dialZero, BarDegrees MuellerReference.cplAnalyzerZero.degrees
            ItemLabel "CPL2", retardanceName, BarDegrees MuellerReference.cplAnalyzerRetardance.degrees
            ItemLabel "CPL1", retardanceName, BarDegrees MuellerReference.cplSourceRetardance.degrees
            ItemLabel "CPL1", fastAxisOffsetName, BarDegrees ((MuellerReference.cplSourceAxisOffset - Angle.degree 45.0<deg>).degrees)
        ]

    [<ElevationBoundary(nameof InteropBoundary, "xUnit's scalar tolerance assertion consumes doubles")>]
    let private expectReferenceValue (quantity : ControlCaption) (expected : float) (actual : float) : unit =
        try Assert.Equal(expected, actual, MuellerReference.dataMatrixTol)
        with :? Xunit.Sdk.XunitException as error ->
            Assert.Fail($"{quantity.value}: expected {expected:G17}; actual {actual:G17}; tolerance {MuellerReference.dataMatrixTol:G17}\n{error.Message}")

    let private assertReferenceInstruments (items : Item list) : unit =
        for label, name, expected in referenceInstrumentAngles do
            let quantity = ControlCaption $"{label.value}.{name.value} (degrees)"
            let parameterOpt =
                items |> List.tryFind (fun item -> item.label = label) |> Option.bind (fun item -> item.tryFindParameter name)
            match parameterOpt |> Option.map _.state with
            | Some (CalibratedParameter calibrated) ->
                expectReferenceValue quantity expected.value calibrated.value.angle.degrees
            | _ -> Assert.Fail($"{quantity.value}: expected a calibrated value of {expected.value:G17}; actual no calibrated parameter")

    let private assertReferenceObjects (run : FitRun) : unit =
        let file = FitRunKitFile.fileNameOf run.name.fileStem
        let qz = itemIdOf (ItemLabel "QZ")
        let lr = itemIdOf (ItemLabel "LR")
        let references : (ItemId list * MuellerMatrix) list =
            [ [ qz ], MuellerReference.qzMatrix; [ lr ], MuellerReference.lrMatrix; [ qz; lr ], MuellerReference.combinedMatrix ]
        TestAppScope.expect file "object matrix groups" (references |> List.map fst |> List.sort) (run.objectMatrices |> List.map _.items |> List.sort)
        for ids, reference in references do
            let labels = ids |> List.map (fun id -> (itemSpecs |> List.find (fun item -> item.id = id)).label.value) |> String.concat " then "
            let fitted = run.objectMatrices |> List.find (fun matrix -> matrix.items = ids)
            TestAppScope.expect file $"{labels} matrix element count" 16 fitted.elements.Length
            let expectedElements = reference.rowMajorValues
            fitted.elements |> List.iteri (fun index actual ->
                let quantity = ControlCaption $"{file.value}: {labels}.{muellerNames.[index].value}"
                expectReferenceValue quantity expectedElements.[index].value actual.value)
        TestAppScope.expect file "product check count" 1 run.results.productChecks.Length
        let check = List.exactlyOne run.results.productChecks
        TestAppScope.expect file "product check objects in light order" [ qz; lr ] check.items
        let ratio = ControlCaption $"{file.value}: QZ then LR product-check RMSE ratio"
        expectReferenceValue ratio MuellerReference.productCheckRmseRatio.value check.rmseRatio.value

    /// The kit window's study of that name; a kit without it fails the fact (spec 0050 S12.1, S12.2).
    let private studyIn (model : TableAndElementRotationModel) (study : StudyName) : Study =
        let file = StudyKitFile.fileNameOf study.fileStem
        model.kitStudies
        |> List.tryFind (fun held -> held.name = study)
        |> Option.defaultWith (fun () -> failwith $"{file.value}: expected the study in the kit")

    /// The study's data as its first fit reads it, `prefitWith` before any fit run then `dataWith` (spec 0050 S12.1, S12.2).
    let private studyDataOf (model : TableAndElementRotationModel) (study : StudyName) : Result<StudyData, StudyForwardError> =
        let held = studyIn model study
        let forward = model.studyForwardContext
        held.prefitWith forward [] |> Result.bind (held.dataWith forward)

    /// The study's residual over that data at its layout's start vector, the one composition AT-1 and AT-2 share (spec 0050 S12.1, S12.2).
    let private studyResidualAtStart (model : TableAndElementRotationModel) (study : StudyName) : Result<ResidualVector, StudyForwardError> =
        let held = studyIn model study
        let forward = model.studyForwardContext
        let layout = held.parameterLayoutWith forward
        studyDataOf model study |> Result.bind (fun data -> held.residualWith forward layout data layout.startVector)

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``the built optics-mueller kit equals the committed kit file by file`` () : unit =
        let run = FolderName (Guid.NewGuid().ToString("N"))
        let built = buildOpticsMuellerKit run
        try assertSameKitFiles built.folder (TestAppScope.committedFolder OpticsMuellerKit)
        finally
            built.model.kit.close () |> TestAppScope.readKitResult KitFile.fileName
        // Spec 0050 S12.1 / step 0428: a failed comparison retains the named kit for regeneration.
        Directory.Delete(Path.GetDirectoryName built.folder.value.value, true)

    /// The study engine's fit over a read-only window: the session's clock, the solver and deviates `StudyFitEnvironment.create` composes, and the window's fit runs (spec 0050 S12.1, S12.2).
    let private readOnlyStudyFit (session : TestAppScope.CommittedKitSession) : StudyFitParam =
        let fit : StudyFitParam =
            {
                forward = session.model.studyForwardContext
                clock = session.clock
                linearSolver = createMathNetSvd ()
                deviates = UniformDeviateProvider.fromSeed (NoiseSeed 1)
                fitRuns = session.model.kitFitRuns
            }
        fit

    /// Closes the session's kit, then checks the committed folder is as recorded, even when the close fails (spec 0050 S12.2).
    let private closeCommittedReadOnly (kit : CommittedKit) (session : TestAppScope.CommittedKitSession) : unit =
        try session.opened.library.kit.close () |> TestAppScope.readKitResult KitFile.fileName
        finally TestAppScope.assertCommittedUnchanged kit session.recorded

    /// Reconstruct from committed snapshots and lab bytes without changing the kit (spec 0050 S12.1; 001 §Q 'Open').
    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``the committed optics-mueller kit opens read-only against an empty library, writes nothing, and its study reproduces the reference reconstruction`` () : unit =
        let session = TestAppScope.openCommittedReadOnly (FolderName "KitAcceptanceTests") OpticsMuellerKit
        let context = session.context
        let opened = session.opened
        let model = session.model
        try
            TestAppScope.expect KitFile.fileName "experiment count" 53 opened.experiments.Length
            TestAppScope.expect KitFile.fileName "setup count" 42 opened.setups.Length
            TestAppScope.expect KitFile.fileName "item count" 9 opened.items.Length
            let studyFile = StudyKitFile.fileNameOf studyName.fileStem
            TestAppScope.expect studyFile "study names" [ studyName ] (opened.studies |> List.map _.name)
            let study = List.exactlyOne opened.studies
            TestAppScope.expect studyFile "all experiments in the study" (opened.experiments |> List.map _.name) study.experiments
            TestAppScope.expect studyFile "staged fit" StagedFit study.settings.mode
            let fitFile = FileName "QZ-LR@*.fitrun-json"
            TestAppScope.expect fitFile "two stored fit runs" [ calibrationRunName; objectRunName ] (opened.fitRuns |> List.map _.name)
            TestAppScope.expect fitFile "stored fit stages" [ CalibrationStage; ObjectStage ] (opened.fitRuns |> List.map _.stage)
            TestAppScope.expect (FileName "*.item-json") "all nine item identities"
                (itemSpecs |> List.map _.id |> List.sort) (model.kitItems |> List.map _.id |> List.sort)
            TestAppScope.assertCommittedUnchanged OpticsMuellerKit session.recorded
            TestAppScope.assertEmptyLibrary context
            for item in model.kitItems do
                let file = ItemKitFile.fileNameOf item.label.fileStem
                let choice =
                    model.tryItemModelChoice item
                    |> Option.defaultWith (fun () -> failwith $"{file.value}: the kit snapshots did not resolve its stored model")
                TestAppScope.expect file "resolved stored model and binding" item.model choice.instance
                let version =
                    match choice with
                    | LibraryModelChoice version
                    | SourcePresetChoice (version, _)
                    | PolarizerPresetChoice (version, _)
                    | StackSampleChoice (version, _, _) -> version
                TestAppScope.expect file "model absent from the database" (Ok None) (context.itemModels.resolveItemModelVersion version.versionId)
                TestAppScope.expect file "model present in the snapshot overlay" (Ok (Some version)) (model.itemModels.resolveItemModelVersion version.versionId)
                match item.model with
                | PresetInstance (_, preset) ->
                    TestAppScope.expect file "bound preset absent from the database" (Ok None) (context.library.tryGetEntry preset)
                | ItemModelVersionInstance _
                | SampleInstance _ -> ()

            let labFiles = readAcceptanceLabFiles opened.folder
            for experiment in model.experimentCollection.experiments do
                let file = ExperimentKitFile.fileNameOf experiment.name.fileStem
                let data =
                    experiment.dataOpt
                    |> Option.defaultWith (fun () -> failwith $"{file.value}: expected its lab attachment")
                let lab = labFiles |> List.find (fun lab -> lab.path = data.attachment.path)
                let group = lab.groups |> List.find (fun group -> group.name = experiment.name)
                TestAppScope.expect file "recorded attachment hash unchanged" (Ok AttachedDataUnchanged)
                    (model.experimentData.attachmentStatus data.attachment)
                TestAppScope.expect file "Phone ROI CSV profile" ImportProfileName.phoneRoiCsv data.profile
                TestAppScope.expect file "attachment's own group" (KeyedDataRows group.key) data.rows
                let mapped =
                    model.experimentData.readMapped data
                    |> Result.defaultWith (fun error -> failwith $"{file.value}: {error.caption.value}")
                TestAppScope.expect file "all mapped group points" (CapturePoints group.captures) mapped
            assertReferenceInstruments model.kitItems

            let fit = readOnlyStudyFit session
            let request : StudyFitRequest =
                {
                    study = studyName
                    stage = ObjectStage
                    calibrationUse = CalibrationHeldFixed
                }
            let run =
                fit.runFit request UnwatchedIterations
                |> Result.defaultWith (fun error -> failwith $"{studyFile.value}: {error.describe}")
            TestAppScope.expect fitFile "refit study" studyName run.study
            TestAppScope.expect fitFile "refit stamp" (TestAppScope.refitTimeOf OpticsMuellerKit) run.stamp
            TestAppScope.expect fitFile "refit stage" ObjectStage run.stage
            TestAppScope.expect fitFile "fixed calibrations" CalibrationHeldFixed run.calibrationUse
            TestAppScope.expect fitFile "linear reconstruction" LinearStudyEngine run.engine
            assertReferenceObjects run
            TestAppScope.assertEmptyLibrary context
            TestAppScope.assertCommittedUnchanged OpticsMuellerKit session.recorded
        finally
            closeCommittedReadOnly OpticsMuellerKit session

    /// `studyResidualAtStart` of `study`, which must answer `Ok` with at least one entry (spec 0050 S12.1, S12.2).
    let private startResidualOf (model : TableAndElementRotationModel) (study : StudyName) : ResidualVector =
        let studyFile = StudyKitFile.fileNameOf study.fileStem
        let residual =
            studyResidualAtStart model study
            |> Result.defaultWith (fun error -> failwith $"{studyFile.value}: expected a start-point residual; actual {error.describe}")
        Assert.NotEmpty residual.value
        residual

    /// Sends the window's kit with `SendKit` to a zip in the run folder, then reopens it with `OpenKitZip` in a fresh `received/<kit>` folder over `context`. It answers that folder and the window over it, which the caller closes (spec 0050 S12.1, S12.2; 001 §H, §Q 'Send').
    let private sendAndReopen
        (context : OpticalConstructor.Ui.AppContext)
        (model : TableAndElementRotationModel)
        (run : FolderName)
        : KitFolder * TableAndElementRotationModel =
        let kitFile = model.kit.kitFile
        let folder =
            model.kitFolderOpt
            |> Option.defaultWith (fun () -> failwith $"{kitFile.name.value}: expected the window's kit folder; actual none")
        let runFolder = runFolderOf run
        Directory.CreateDirectory runFolder.value |> ignore
        let expectedFiles = kitContentFiles folder
        let zip = kitFile.name.zipFileName.combine runFolder
        let archive = KitArchiveProxy.create ()
        let sendMessages = ResizeArray<TableAndElementRotationMsg> ()
        let sending =
            {
                model with
                    kitShell =
                        {
                            model.kitShell with
                                archive = archive
                                archiveJobs = KitArchiveJobHost.immediate (SendKitProgressed >> sendMessages.Add) (SendKitEnded >> sendMessages.Add)
                        }
            }
            |> updateTableAndElementRotation (SendKit zip)
        TestAppScope.expect zip "send started" true sending.kitSend.job.isRunning
        let recordedSend = List.ofSeq sendMessages
        match List.rev recordedSend with
        | SendKitEnded (Ok (KitArchiveFinished files)) :: progresses ->
            TestAppScope.expect zip "sent file count" expectedFiles.Count files.value
            TestAppScope.expect zip "one send progress per file" files.value progresses.Length
            for message in progresses do
                match message with
                | SendKitProgressed _ -> ()
                | _ -> Assert.Fail($"{zip.value}: expected only send progress before the successful end")
        | SendKitEnded (Error error) :: _ -> Assert.Fail($"{zip.value}: SendKit failed: {error.describe}")
        | _ -> Assert.Fail($"{zip.value}: expected SendKitEnded with a successfully finished archive")
        let sent = sending |> apply recordedSend
        TestAppScope.expect zip "send job returned to idle" KitArchiveJobState.idle sent.kitSend
        TestAppScope.expect zip "send completion status"
            (Some (ControlCaption $"The kit was sent: {expectedFiles.Count} file(s) zipped.")) sent.kitStatusCaption
        TestAppScope.expect zip "zip written outside the kit" true (File.Exists zip.value)
        let entries =
            archive.listZipEntries zip
            |> Result.defaultWith (fun error -> failwith $"{zip.value}: {error.describe}")
        for entry in entries do
            match entry.path.pathRole with
            | KitAppCachePath -> Assert.Fail($"{zip.value}: local cache entry {entry.path.value} must not travel")
            | KitContentPath -> ()

        let target = KitFolder (FolderName (Path.Combine(runFolder.value, "received", kitFile.name.fileStem.value)))
        TestAppScope.expect zip "fresh extraction target" false (Directory.Exists target.value.value)
        Directory.CreateDirectory target.value.value |> ignore
        let mutable openedOpt : OpenedKit option = None
        try
            let openMessages = ResizeArray<KitStartMsg> ()
            let startContext : KitStartContext =
                {
                    kitHistory = KitHistoryProxy.createInMemory [ target ]
                    limit = KitHistoryLimit.defaultValue
                    clock = model.kit.clock
                    kitIds =
                        {
                            mintKitId = fun () -> kitFile.id
                        }
                    author = kitFile.author
                    imports = context.catalogueImports
                    picker = TestAppScope.kitPickerOf target
                    kitFolderMemory = KitFolderMemory.fixedAt (FolderName "kits")
                    showKit = fun opened -> openedOpt <- Some opened
                    activateKitWindow = fun _ -> NoKitWindowOpen
                    zipPicker = KitZipPicker.none
                    archive = KitArchiveProxy.create ()
                    archiveJobs = KitArchiveJobHost.immediate (OpenKitZipProgressed >> openMessages.Add) (OpenKitZipEnded >> openMessages.Add)
                }
            let opening : KitZipOpening =
                {
                    zip = zip
                    target = target
                }
            let extracting = initKitStart startContext InverseConstructor |> updateKitStart (OpenKitZip opening)
            TestAppScope.expect zip "extraction started" true extracting.zipJob.job.isRunning
            match openedOpt with
            | None -> ()
            | Some _ -> Assert.Fail($"{zip.value}: the kit must open only after OpenKitZipEnded is handled")
            let recordedOpen = List.ofSeq openMessages
            match List.rev recordedOpen with
            | OpenKitZipEnded (Ok (KitArchiveFinished files)) :: progresses ->
                TestAppScope.expect zip "extracted file count" expectedFiles.Count files.value
                TestAppScope.expect zip "one extraction progress per file" files.value progresses.Length
                for message in progresses do
                    match message with
                    | OpenKitZipProgressed _ -> ()
                    | _ -> Assert.Fail($"{zip.value}: expected only extraction progress before the successful end")
            | OpenKitZipEnded (Error error) :: _ -> Assert.Fail($"{zip.value}: OpenKitZip failed: {error.describe}")
            | _ -> Assert.Fail($"{zip.value}: expected OpenKitZipEnded with a successfully finished extraction")
            let ended = recordedOpen |> List.fold (fun start message -> updateKitStart message start) extracting
            TestAppScope.expect zip "open status" None ended.status
            TestAppScope.expect zip "extraction job returned to idle" KitArchiveJobState.idle ended.zipJob
            TestAppScope.expect zip "completed extraction cleared" None ended.zipOpeningOpt
            TestAppScope.expect zip "no unresolved open offer" NoKitStartOffer ended.offer
            let opened =
                openedOpt
                |> Option.defaultWith (fun () -> failwith $"{target.value.value}: OpenKitZip did not open the extracted kit")
            TestAppScope.expect KitFile.fileName "reopened folder" target opened.folder
            TestAppScope.expect KitFile.fileName "reopened access" ReadWriteKitAccess opened.library.kit.access
            target, TestAppScope.inverseModelOver context opened
        with _ ->
            match openedOpt with
            | Some opened -> opened.library.kit.close () |> TestAppScope.readKitResult KitFile.fileName
            | None -> ()
            reraise ()

    /// Send and reopen the built kit without changing its files or study residual (spec 0050 S12.1; 001 §H, §Q 'Send').
    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``the optics-mueller kit travels through Send kit and Open kit from zip unchanged, its study's residual at the start point included`` () : unit =
        let run = FolderName (Guid.NewGuid().ToString("N"))
        let built = buildOpticsMuellerKit run
        try
            let studyFile = StudyKitFile.fileNameOf studyName.fileStem
            let builtResidual = startResidualOf built.model studyName
            let target, reopened = sendAndReopen built.context built.model run
            try
                assertSameKitFiles built.folder target

                let reopenedResidual = startResidualOf reopened studyName
                TestAppScope.expect studyFile "start-point residual after SendKit and OpenKitZip, element for element"
                    builtResidual.value reopenedResidual.value
            finally
                reopened.kit.close () |> TestAppScope.readKitResult KitFile.fileName
        finally
            try built.model.kit.close () |> TestAppScope.readKitResult KitFile.fileName
            finally Directory.Delete((runFolderOf run).value, true)

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``AT-1 builds the optics-mueller kit's items and setups through the app's messages`` () : unit =
        let run = FolderName (Guid.NewGuid().ToString("N"))
        let built = buildOpticsMuellerKitWith (fun _ _ model -> model) run
        try
            let reader = KitProxy.create built.folder
            assertKitFile reader TestAppScope.kitId (KitName "optics-mueller") TestAppScope.kitAuthor initialTime labTemplate
            TestAppScope.expect KitFile.fileName "clock" initialTime (built.clock.now ())
            TestAppScope.expect (FileName "*.experiment-json") "setups-only build" [] (reader.readExperiments () |> TestAppScope.readKitResult (FileName "*.experiment-json"))
            assertStoredItems built reader
            assertStoredSetups reader
            assertLibrarySnapshots built reader
            assertLabBytes built.folder
        finally
            try built.model.kit.close () |> TestAppScope.readKitResult KitFile.fileName
            finally Directory.Delete(Path.GetDirectoryName built.folder.value.value, true)

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``AT-1 attaches every lab group of the final data files through the Phone ROI CSV profile, with its role, session, correction and exclusion`` () : unit =
        let original = TestAppScope.committedFolder OpticsMuellerKit
        let originalHashes = labHashes original
        let files = readAcceptanceLabFiles original
        let run = FolderName (Guid.NewGuid().ToString("N"))
        let built = buildOpticsMuellerKitWith (fun _ folder model -> buildAcceptanceExperiments folder model) run
        try
            let reader = KitProxy.create built.folder
            assertStoredExperiments built reader files
            assertStoredItems built reader
            assertStoredSetups reader
            assertLibrarySnapshots built reader
            assertLabBytes built.folder
            TestAppScope.expect (FileName "data/*") "built lab file inventory and SHA-256 unchanged" originalHashes (labHashes built.folder)
            TestAppScope.expect (FileName "data/optics-mueller/data/*") "committed lab file inventory and SHA-256 unchanged" originalHashes (labHashes original)
        finally
            try built.model.kit.close () |> TestAppScope.readKitResult KitFile.fileName
            finally Directory.Delete(Path.GetDirectoryName built.folder.value.value, true)

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``AT-1's staged study calibrates the instruments on the AIR runs, publishes them and reconstructs QZ, LR and QZ then LR as the reference does`` () : unit =
        let run = FolderName (Guid.NewGuid().ToString("N"))
        let built = buildOpticsMuellerKit run
        try
            let reader = KitProxy.create built.folder
            let studyFile = StudyKitFile.fileNameOf studyName.fileStem
            let studies = reader.readStudies () |> TestAppScope.readKitResult studyFile
            TestAppScope.expect studyFile "study names" [ studyName ] (studies |> List.map _.name)
            let study = List.exactlyOne studies
            let experiments = reader.readExperiments () |> TestAppScope.readKitResult (FileName "*.experiment-json")
            TestAppScope.expect studyFile "experiment count" 53 study.experiments.Length
            TestAppScope.expect studyFile "experiments in kit order" (experiments |> List.map _.name) study.experiments
            TestAppScope.expect studyFile "fit mode" StagedFit study.settings.mode
            TestAppScope.expect studyFile "six session gain settings" sessionGainSettings study.sessionGains
            let sessions = study.sessionsIn experiments
            let darkRuns = experiments |> List.filter (fun experiment -> experiment.role = DarkRun) |> List.map _.name
            TestAppScope.expect studyFile "shared dark run count" 6 darkRuns.Length
            for setting in sessionGainSettings do
                let session = sessions |> List.find (fun session -> session.name = setting.session)
                TestAppScope.expect studyFile $"{setting.session.value} gain is offered" true (List.contains setting.model (session.gainModelOffers experiments))
                TestAppScope.expect studyFile $"{setting.session.value} has no own dark run" [] (session.experiments |> List.filter (fun name -> List.contains name darkRuns))
                TestAppScope.expect studyFile $"{setting.session.value} uses all dark runs" darkRuns (session.darkRunsIn experiments)

            let fitFile = FileName "QZ-LR@*.fitrun-json"
            let runs = reader.readFitRuns studyName |> TestAppScope.readKitResult fitFile
            TestAppScope.expect fitFile "fit runs in stamp order" [ calibrationRunName; objectRunName ] (runs |> List.map _.name)
            TestAppScope.expect fitFile "fit stamps" [ calibrationTime; objectTime ] (runs |> List.map _.stamp)
            TestAppScope.expect fitFile "fit stages" [ CalibrationStage; ObjectStage ] (runs |> List.map _.stage)
            TestAppScope.expect fitFile "stored fit runs match completed window jobs" built.model.kitFitRuns runs
            for fitted in runs do
                let file = FitRunKitFile.fileNameOf fitted.name.fileStem
                TestAppScope.expect file "study" studyName fitted.study
                TestAppScope.expect file "fit mode" StagedFit fitted.mode
                TestAppScope.expect file "engine" LinearStudyEngine fitted.engine
                TestAppScope.expect file "calibration use" CalibrationHeldFixed fitted.calibrationUse
            let calibration = List.head runs
            let reconstructed = List.last runs
            let expectedEntries : FitEntryRef list =
                referenceInstrumentAngles
                |> List.map (fun (label, name, _) ->
                    let id = itemIdOf label
                    {
                        parameter = ItemParameterRef (id, name)
                        owner = ItemOwner id
                    })
            let actualEntries : FitEntryRef list =
                calibration.entries |> List.map (fun entry ->
                    {
                        parameter = entry.parameter
                        owner = entry.owner
                    })
            TestAppScope.expect fitFile "five instrument unknowns" (expectedEntries |> List.sort) (actualEntries |> List.sort)
            TestAppScope.expect fitFile "five instrument sigma entries" (expectedEntries |> List.sort) (calibration.results.sigmas |> List.map _.entry |> List.sort)
            TestAppScope.expect fitFile "calibration AIR runs in kit order"
                (experiments |> List.filter (fun experiment -> experiment.role = CalibrationRun) |> List.map _.name) calibration.runs
            TestAppScope.expect fitFile "object Science runs in kit order"
                (experiments |> List.filter (fun experiment -> experiment.role = ScienceRun) |> List.map _.name) reconstructed.runs

            let itemsFile = FileName "*.item-json"
            let items = reader.readItems () |> TestAppScope.readKitResult itemsFile
            TestAppScope.expect itemsFile "item identities" (itemSpecs |> List.map _.id |> List.sort) (items |> List.map _.id |> List.sort)
            let calibratedIds = referenceInstrumentAngles |> List.map (fun (label, _, _) -> itemIdOf label) |> List.distinct
            for item in items do
                let file = ItemKitFile.fileNameOf item.label.fileStem
                let expectedRevision = if List.contains item.id calibratedIds then ItemRevision 2 else ItemRevision.first
                TestAppScope.expect file "revision after calibration publication" expectedRevision item.revision
            for label, name, _ in referenceInstrumentAngles do
                let item = items |> List.find (fun item -> item.label = label)
                let file = ItemKitFile.fileNameOf label.fileStem
                let parameter = ItemParameterRef (item.id, name)
                let entry = calibration.entries |> List.find (fun entry -> entry.parameter = parameter && entry.owner = ItemOwner item.id)
                let reference : FitEntryRef =
                    {
                        parameter = entry.parameter
                        owner = entry.owner
                    }
                let sigma =
                    calibration.results.sigmas
                    |> List.find (fun held -> held.entry = reference)
                    |> _.sigmaOpt
                    |> Option.defaultWith (fun () -> failwith $"{file.value}: {name.value} expected a fitted sigma; actual none")
                let expected : CalibratedParameterData =
                    {
                        value = entry.fitted
                        sigma = sigma
                        fitRun = calibrationRunName
                    }
                TestAppScope.expect file $"{name.value} fitted value, sigma and fit-run provenance"
                    (Some (CalibratedParameter expected)) (item.tryFindParameter name |> Option.map _.state)
                let heldByStageTwo = reconstructed.parameters |> List.find (fun held -> held.parameter = parameter)
                TestAppScope.expect file $"{name.value} held by stage two" (CalibratedParameter expected) heldByStageTwo.state

            TestAppScope.expect KitFile.fileName "clock after the object fit" objectTime (built.clock.now ())
            assertReferenceInstruments items
            assertReferenceObjects reconstructed
            assertLabBytes built.folder
        finally
            try built.model.kit.close () |> TestAppScope.readKitResult KitFile.fileName
            finally Directory.Delete(Path.GetDirectoryName built.folder.value.value, true)

    /// The folder beside an AT-2 kit that holds the entry files its build drops (spec 0050 S12.2).
    let private droppedEntriesFolderOf (kit : KitFolder) : FolderName =
        (FolderName (Path.GetDirectoryName kit.value.value)).combine (FolderName "entries")

    /// Seals `content` and writes its text under `folder` as its entry file, UTF-8 without a byte-order mark (spec 0050 S12.2).
    let private writeEntryFile (folder : FolderName) (content : LibraryEntryContent) : FileName =
        let file = content.fileName.combine folder
        let text =
            content.``sealed``
            |> Result.bind (fun (envelope : LibraryEntryEnvelope) -> envelope.fileText)
            |> Result.defaultWith (fun error -> failwith $"{file.value}: expected a sealed entry file; actual {error.describe}")
        File.WriteAllText(file.value, text.value, System.Text.UTF8Encoding(false))
        file

    /// Drops `files` on a Library window and applies the import dialog's rows, each adding its entry, as a user would (spec 0050 S12.2; 001 §H).
    let private dropOnLibraryWindow (context : OpticalConstructor.Ui.AppContext) (files : FileName list) (added : LibraryEntryId list) : unit =
        let drop = FileName (files |> List.map _.value |> String.concat ", ")
        let requests = ResizeArray<ImportDialogRequest> ()
        let windowContext : LibraryWindowView.LibraryWindowContext =
            {
                library = context.library
                samples = context.samples
                materials = context.materials
                categories = context.categories
                libraryBackups = context.libraryBackups
                catalogueImports = context.catalogueImports
                openImportDialog = requests.Add
                treeAutoBuildThreshold = TreeAutoBuildThreshold.defaultValue
                thicknessBucketCap = ThicknessBucketCap.defaultValue
                openSampleEditor = fun _ -> ()
                sources = context.sources
                openSourceEditor = fun _ -> ()
            }
        let dropped =
            LibraryWindowView.initLibraryWindow windowContext
            |> LibraryWindowView.updateLibraryWindow (LibraryWindowView.DropLibraryEntryFiles files)
        TestAppScope.expect drop "drop refusal" None dropped.lastError
        let request =
            match List.ofSeq requests with
            | [ only ] -> only
            | held -> failwith $"{drop.value}: expected one import dialog request; actual {held.Length}"
        let decisions =
            match request with
            | ApplyDroppedEntries decisions -> decisions
            | SettlePendingCatalogue _ | ResolveKitSnapshot _ -> failwith $"{drop.value}: expected the dropped files' rows; actual {request.heading.value}"
        TestAppScope.expect drop "dropped rows" added (decisions |> List.map _.entryId)
        TestAppScope.expect drop "each row's default action" (added |> List.map (fun _ -> CatalogueImport.AddIncomingEntry)) (decisions |> List.map _.defaultAction)
        let applied : ImportDialogApplied option ref = ref None
        let dialog : ImportDialogContext =
            {
                imports = context.catalogueImports
                request = request
                onApplied = fun result -> applied.Value <- Some result
                requestClose = fun () -> ()
            }
        let confirmed = dialog.initImportDialog |> updateImportDialog ApplyImportChoices
        TestAppScope.expect drop "import refusal" None confirmed.lastError
        let result = applied.Value |> Option.defaultWith (fun () -> failwith $"{drop.value}: expected the import dialog to apply; actual nothing applied")
        let completed = dropped |> LibraryWindowView.updateLibraryWindow (LibraryWindowView.LibraryImportApplied result)
        TestAppScope.expect drop "Library window refusal" None completed.lastError
        TestAppScope.expect drop "Library window status" (Some result.summary) completed.importStatus

    /// Fills the table's slots, then adds Polarimeter R's detector on the plate's reflected ray, so the plate is captured in T and R (spec 0050 S12.2).
    let private buildAbsorbingTable (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let assigned (slot : ElementId) (spec : AcceptanceItem) (current : TableAndElementRotationModel) : TableAndElementRotationModel =
            let next = current |> updateTableAndElementRotation (AssignSlot (slot, Some spec.id))
            TestAppScope.expect KitFile.fileName $"{spec.label.value} assigned to {slot.value}" None next.kitRefusalOpt
            next
        let table =
            model
            |> assigned sourceSlot absorbingLaser
            |> assigned firstObjectSlot opticAxisCutPlate
            |> assigned detectorSlot polarimeterT
            |> apply [ AddElement Detector; PlaceOnBeam (reflectedDetectorSlot, BeamLayout.OnReflectedBeamOf firstObjectSlot) ]
            |> assigned reflectedDetectorSlot polarimeterR
        TestAppScope.expect KitFile.fileName "plate captures" [ CaptureTransmitted; CaptureReflected; CaptureBoth ] (availableCaptures table firstObjectSlot)
        table

    /// The draft's edited axis: the plate's `variable` over `low`…`high` in `points`, as the Experiments bay sets one (spec 0050 S12.2).
    let private plateAxis
        (variable : ExperimentControls.VariableChoice)
        (low : ExperimentControls.ExperimentRangeEnd)
        (high : ExperimentControls.ExperimentRangeEnd)
        (points : ExperimentControls.ExperimentPointCount)
        : TableAndElementRotationMsg list =
        [
            ExpChooseElement firstObjectSlot
            ExpChooseVariable variable
            ExpSetRangeMin low
            ExpSetRangeMax high
            ExpSetRangePoints points
        ]

    /// A1's four azimuths of a plate, 0…67.5° in steps of 22.5°, read in T (spec 0050 S12.2; 001 §Q AT-2).
    let private azimuthsInT : TableAndElementRotationMsg list =
        plateAxis
            ExperimentControls.VaryR1
            (ExperimentControls.ExperimentRangeEnd 0.0)
            (ExperimentControls.ExperimentRangeEnd 67.5)
            (ExperimentControls.ExperimentPointCount 4)
        @ [ ExpChooseMeasurement ExperimentControls.CaptureT ]

    /// The incidences `low`…`high` in `points` by the azimuths 0 and 90°, each read in T then R, as A2 and A3 read them (spec 0050 S12.2).
    let private incidencesByAzimuthInTAndR
        (low : ExperimentControls.ExperimentRangeEnd)
        (high : ExperimentControls.ExperimentRangeEnd)
        (points : ExperimentControls.ExperimentPointCount)
        : TableAndElementRotationMsg list =
        plateAxis ExperimentControls.VaryR2 low high points
        @ [ ExpAddScanAxis ]
        @ plateAxis
            ExperimentControls.VaryR1
            (ExperimentControls.ExperimentRangeEnd 0.0)
            (ExperimentControls.ExperimentRangeEnd 90.0)
            (ExperimentControls.ExperimentPointCount 2)
        @ [ ExpChooseMeasurement ExperimentControls.CaptureBoth ]

    /// Over a fresh draft and grid panel, each plate tried in Object 1 and the grid generated: its setups and one experiment of the draft over each (spec 0050 S12.2; 001 §J).
    let private generatePlateGrid
        (draft : TableAndElementRotationMsg list)
        (plates : AcceptanceItem list)
        (model : TableAndElementRotationModel)
        : TableAndElementRotationModel =
        let opened =
            model
            |> apply (ExpNew :: draft)
            |> updateTableAndElementRotation OpenSetupGrid
            |> apply (plates |> List.map (fun plate -> ToggleSetupGridItem (firstObjectSlot, plate.id)))
        let generated = opened |> updateTableAndElementRotation (GenerateSetupGrid (gridOf opened))
        let status = generated.kitStatusCaption |> Option.map _.value |> Option.defaultValue "none"
        let names = plates |> List.map _.label.value
        let heldSetups = opened.kitSetups |> List.map _.name
        let addedSetups = generated.kitSetups |> List.map _.name |> List.filter (fun name -> not (List.contains name heldSetups))
        TestAppScope.expect (FileName "*.setup-json") $"setups the grid added (status: {status})" (List.sort names) (addedSetups |> List.map _.value |> List.sort)
        let appended = generated.experimentCollection.experiments |> List.skip opened.experimentCollection.experiments.Length
        TestAppScope.expect (FileName "*.experiment-json") $"experiments the grid appended (status: {status})" names (appended |> List.map _.name.value)
        generated |> updateTableAndElementRotation CloseSetupGrid

    /// Three grids record the five setups and one experiment per plate, whose scans read the design's 44 configurations (spec 0050 S12.2; 001 §Q AT-2).
    let private buildAbsorbingGrids (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let opticAxisScan =
            azimuthsInT
            @ [ ExpAddScanBlock ]
            @ incidencesByAzimuthInTAndR
                (ExperimentControls.ExperimentRangeEnd 20.0)
                (ExperimentControls.ExperimentRangeEnd 60.0)
                (ExperimentControls.ExperimentPointCount 3)
        let principalCutScan =
            incidencesByAzimuthInTAndR
                (ExperimentControls.ExperimentRangeEnd 0.0)
                (ExperimentControls.ExperimentRangeEnd 45.0)
                (ExperimentControls.ExperimentPointCount 2)
        model
        |> generatePlateGrid opticAxisScan [ opticAxisCutPlate ]
        |> generatePlateGrid azimuthsInT [ otherOpticAxisCutPlate ]
        |> generatePlateGrid principalCutScan [ zCutPlate; xCutPlate; yCutPlate ]

    /// The harness's forward model of the absorbing crystal: it draws the observations and forms the harness residual (spec 0050 S12.2).
    let private absorbingForward : ForwardModelProxy<CrystalPhysics.AbsorbingTriclinicParameters> =
        MuellerInverse.createBerremanForward
            (fun (p : CrystalPhysics.AbsorbingTriclinicParameters) -> p.opticalProperties)
            BiaxialSample.solverParameters

    /// Seed `seed`'s noisy observations of the 44 configurations, drawn as `AbsorbingBiaxialNoisyTests` draws them (spec 0050 S12.2).
    let private noisyObservationsOf (seed : NoiseSeed) : MuellerObservation list =
        noisyObserveAbsoluteWith absorbingForward AbsorbingBiaxialSample.absorbing NoiseParam.decentLab seed AbsorbingBiaxialSample.fullConfigurations
        |> Result.defaultWith (fun error -> failwith $"noise seed {seed.value}: expected its noisy observations; actual %A{error}")

    /// The observations of each plate together, plate by plate in kit order, each plate's in their own order: how the study reads seed 1's file (spec 0050 S12.2).
    let private byPlate (observations : MuellerObservation list) : MuellerObservation list =
        absorbingPlateCuts
        |> List.collect (fun (_, cut) -> observations |> List.filter (fun observation -> observation.configuration.cut = cut))

    /// The label of the plate cut as `cut`, by `absorbingPlateCuts` read from cut to plate (spec 0050 S12.2).
    let private plateLabelOf (cut : SampleCut) : ItemLabel =
        match absorbingPlateCuts |> List.tryFind (fun (_, plateCut) -> plateCut = cut) with
        | Some (plate, _) -> plate.label
        | None -> failwith $"*.item-json: expected a plate of the cut %A{cut}; actual none"

    /// Seed `seed`'s noisy observations in the harness's order, each keyed by its plate's label (spec 0050 S12.2).
    let private plateKeyedObservationsOf (seed : NoiseSeed) : KeyedObservation list =
        noisyObservationsOf seed
        |> List.map (fun observation ->
            let keyed : KeyedObservation =
                {
                    key = DataRowKey (plateLabelOf observation.configuration.cut).value
                    observation = observation
                }
            keyed)

    /// Seed `seed`'s observation file, written by `KeyedObservation.observationFileText` (spec 0050 S12.2).
    let private observationFileOf (seed : NoiseSeed) : KitFileText =
        KeyedObservation.observationFileText (plateKeyedObservationsOf seed)
        |> Result.defaultWith (fun error -> failwith $"noise seed {seed.value}: expected its observation file; actual {error.caption.value}")

    /// Where the build writes seed `seed`'s observation file in the kit (spec 0050 S12.2).
    let private noisyObservationsFile (seed : NoiseSeed) : KitRelativePath =
        KitRelativePath $"data/noisy-observations-seed-{seed.value}.csv"

    /// The seed whose file the build attaches; the other files wait in `data/` for the ensemble (spec 0050 S12.2).
    let private attachedSeed : NoiseSeed = List.head AbsorbingBiaxialSample.noisySeeds

    /// Splits seed `seed`'s file in `data/` by `sample` and attaches it in place, each plate's experiment taking its key's rows (spec 0050 S12.2; 001 §K).
    let private attachSeedObservations (folder : KitFolder) (seed : NoiseSeed) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let file = labFileAt folder (noisyObservationsFile seed)
        let split = model |> updateTableAndElementRotation (SplitByColumn (file, ImportProfileName.muellerObservations))
        let statusOf (current : TableAndElementRotationModel) : string = current.kitStatusCaption |> Option.map _.value |> Option.defaultValue "none"
        let preview = splitOf file split
        let plates = absorbingPlateCuts |> List.map fst
        let experimentForEach : (DataRowKey * ExperimentName option) list =
            plates |> List.map (fun plate -> DataRowKey plate.label.value, Some (ExperimentName plate.label.value))
        let matched =
            preview.split.groups
            |> List.map (fun group ->
                match group.matching with
                | ExperimentForGroup name -> group.key, Some name
                | SetupOfferForGroup _
                | GroupWithoutMatch _ -> group.key, None)
        TestAppScope.expect file "split key column" (SplitKeyColumn "sample") preview.key
        TestAppScope.expect file $"groups in file order, each matched to the experiment its key names (status: {statusOf split})" experimentForEach matched
        let attached = split |> updateTableAndElementRotation MatchByTemplate
        TestAppScope.expect file $"split completed (status: {statusOf attached})" None attached.dataSplitOpt
        TestAppScope.expect file "experiments after the split" (model.experimentCollection.experiments |> List.map _.name) (attached.experimentCollection.experiments |> List.map _.name)
        TestAppScope.expect file "no setup added by the split" model.kitSetups attached.kitSetups
        let profiles = attached.experimentCollection.experiments |> List.map (fun experiment -> experiment.dataOpt |> Option.map _.profile)
        TestAppScope.expect file "every experiment attached through the profile" (plates |> List.map (fun _ -> Some ImportProfileName.muellerObservations)) profiles
        let attachments = attached.experimentCollection.experiments |> List.map (fun experiment -> experiment.dataOpt |> Option.map (fun data -> data.attachment.path, data.rows))
        let keyedRows = plates |> List.map (fun plate -> Some (noisyObservationsFile seed, KeyedDataRows (DataRowKey plate.label.value)))
        TestAppScope.expect file "each experiment holding this file's rows of its key" keyedRows attachments
        attached

    /// Writes each seed's observation file into `data/`, then attaches seed 1's (spec 0050 S12.2; 001 §K).
    let private attachNoisyObservations (folder : KitFolder) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        for seed in AbsorbingBiaxialSample.noisySeeds do
            let written = labFileAt folder (noisyObservationsFile seed)
            Directory.CreateDirectory(Path.GetDirectoryName written.value) |> ignore
            File.WriteAllText(written.value, (observationFileOf seed).value, System.Text.UTF8Encoding(false))
        attachSeedObservations folder attachedSeed model

    /// Each generated axis of `crystal` freed from `perturbedStart` within ± `searchBox.halfWidth` × the harness's scale on the record axis at its position (spec 0050 S12.2).
    let private perturbedStartUnknownsOf (crystal : CrystalPhysics.CrystalParameters) : Parameter list =
        let file = StudyKitFile.fileNameOf absorbingStudyName.fileStem
        let generated = crystal.parameterAxes
        let records = CrystalPhysics.AbsorbingTriclinicParameters.axes
        if generated.Length <> records.Length then
            failwith $"{file.value}: expected {records.Length} generated axes, one per record axis; actual {generated.Length}"
        let start = CrystalPhysics.AbsorbingTriclinicCrystal AbsorbingBiaxialSample.perturbedStart
        let scale = (AbsorbingBiaxialSample.absorbingScalingAround AbsorbingBiaxialSample.perturbedStart).scale
        List.map2
            (fun (a : ParameterAxis<CrystalPhysics.CrystalParameters>) (r : ParameterAxis<CrystalPhysics.AbsorbingTriclinicParameters>) ->
                let s = a.parameterValueOf start
                let h = AbsorbingBiaxialSample.searchBox.halfWidth * r.read scale
                let parameter : Parameter =
                    {
                        name = a.name
                        state = unknownBetween s (ParameterValue (s.value - h)) (ParameterValue (s.value + h))
                    }
                parameter)
            generated
            records

    /// The crystal AT-2's opened copy holds, failing on an opened table (spec 0050 S12.2, S13.2).
    let private openedCrystalOf (material : OpenedMaterial) : CrystalPhysics.CrystalParameters =
        match material.physics with
        | OpenedCrystalPhysics crystal -> crystal
        | OpenedTablePhysics _ ->
            let file = StudyKitFile.fileNameOf absorbingStudyName.fileStem
            failwith $"{file.value}: expected '{material.name.value}' opened as a crystal; actual an opened table"

    /// AT-2's study over the five experiments in kit order, the material opened and its components freed from `perturbedStart`; no scope is set and no fit runs (spec 0050 S12.2; 001 §Q AT-2).
    let private buildAbsorbingStudy (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let file = StudyKitFile.fileNameOf absorbingStudyName.fileStem
        let sent (current : TableAndElementRotationModel) (message : StudyMsg) : TableAndElementRotationModel =
            let next = current |> updateTableAndElementRotation (StudyCommand message)
            TestAppScope.expect file "study command status" None next.kitRefusalOpt
            next
        let experiments = model.experimentCollection.experiments |> List.map _.name
        let opened =
            [ CreateStudy (absorbingStudyName, experiments); OpenMaterial (absorbingStudyName, absorbingMaterialVersion) ]
            |> List.fold sent model
        let copy =
            match (studyIn opened absorbingStudyName).materials with
            | [ material ] -> material
            | materials -> failwith $"{file.value}: expected one opened material; actual {materials.Length}"
        perturbedStartUnknownsOf (openedCrystalOf copy)
        |> List.map (fun parameter -> SetOpenedMaterialParameter (absorbingStudyName, copy.version, parameter))
        |> List.fold sent opened

    /// AT-2's kit, built through the Library drop, the import dialog, the start screen and the kit window's messages alone, ending with its study's fit (spec 0050 S12.2; 001 §Q).
    let private buildAbsorbingKit (run : FolderName) : BuiltAcceptanceKit =
        let runFolder = runFolderOf run
        let folder = KitFolder (runFolder.combine (FolderName "absorbing-biaxial-noisy"))
        let entries = droppedEntriesFolderOf folder
        // A run's name is fixed, so the previous run's folder, left for diagnosis, is removed first (spec 0050 S12.2).
        if Directory.Exists runFolder.value then Directory.Delete(runFolder.value, true)
        Directory.CreateDirectory folder.value.value |> ignore
        Directory.CreateDirectory entries.value |> ignore
        let context = TestAppScope.freshAppContext (FolderName "KitAcceptanceTests")
        let clock, setTime = TestAppScope.testClockAt absorbingStartTime
        let material = LibraryEntryContent.ofMaterialEntry VersionNumber.first UserProvenance absorbingMaterial
        let sample = LibraryEntryContent.ofSample VersionNumber.first UserProvenance absorbingPlate
        dropOnLibraryWindow context [ writeEntryFile entries material; writeEntryFile entries sample ] [ material.id; sample.id ]
        let heldMaterial =
            context.materials.resolveVersion absorbingMaterialVersion
            |> Result.map (Option.map (fun (held : MaterialEntry) -> held.id))
        TestAppScope.expect material.fileName "material held at version 1" (Ok (Some absorbingMaterial.id)) heldMaterial
        let heldSample =
            context.samples.resolveVersion (Library.SampleVersionId.firstOf absorbingPlate.id)
            |> Result.map (Option.map (fun (held : Library.Sample) -> held.id))
        TestAppScope.expect sample.fileName "sample held at version 1" (Ok (Some absorbingPlate.id)) heldSample
        let mutable openedOpt : OpenedKit option = None
        try
            let start, shown = TestAppScope.kitStartOver context clock TestAppScope.absorbingKitId TestAppScope.absorbingKitAuthor folder
            let started = start |> updateKitStart (CreateKit folder)
            openedOpt <- shown.Value
            TestAppScope.expect KitFile.fileName "CreateKit status" None started.status
            let opened = openedOpt |> Option.defaultWith (fun () -> failwith $"{folder.value.value}: expected CreateKit to open the kit; actual no kit")
            let mintItemId, unusedItemIds = mintingFrom (FileName "*.item-json") "item-id" (absorbingItemSpecs |> List.map _.id)
            let mintElementId, unusedSlotIds = mintingFrom (FileName "*.setup-json") "added-slot id" [ reflectedDetectorSlot ]
            let itemIds : ItemIdProvider =
                {
                    mintItemId = mintItemId
                }
            let elementIds : ElementIdProvider =
                {
                    mintElementId = mintElementId
                }
            let initial =
                {
                    TestAppScope.inverseModelOver context opened with itemIds = itemIds; elementIds = elementIds
                }
                |> apply [ SetNamingTemplateText absorbingTemplate; SaveNamingTemplate ]
            let stocked = absorbingItemSpecs |> List.fold (fun model spec -> addItemThroughForm [] spec model |> fst) initial
            TestAppScope.expect (FileName "*.item-json") "unused item identities" [] unusedItemIds.Value
            let table = buildAbsorbingTable stocked
            TestAppScope.expect (FileName "*.setup-json") "unused added-slot identities" [] unusedSlotIds.Value
            let recorded = buildAbsorbingGrids table
            let attached = attachNoisyObservations folder recorded
            let studied = buildAbsorbingStudy attached
            setTime absorbingFitTime
            let fitted = runStudyFitThroughShell studied absorbingFitRequest (TimeSpan.FromSeconds 1800.0)
            TestAppScope.expect (FitRunKitFile.fileNameOf absorbingFitRunName.fileStem) "committed fit run" (Some absorbingFitRunName) fitted.studyFit.job.committedResults
            {
                folder = folder
                context = context
                model = fitted
                clock = clock
                setTime = setTime
            }
        with _ ->
            match openedOpt with
            | Some opened -> opened.library.kit.close () |> TestAppScope.readKitResult KitFile.fileName
            | None -> ()
            reraise ()

    /// Every AT-2 fact but the ensemble reads this build, run once as xUnit runs a class's facts in turn; none writes into it (spec 0050 S12.2).
    let private absorbingBuild : Lazy<BuiltAcceptanceKit> = lazy (buildAbsorbingKit (FolderName "AT-2"))

    /// The offer an AT-2 item instantiates: its shipped model's held version over the dropped plate or its preset (spec 0050 S12.2).
    let private absorbingChoiceOf (context : OpticalConstructor.Ui.AppContext) (spec : AcceptanceItem) : ItemModelChoice =
        let file = ItemKitFile.fileNameOf spec.label.fileStem
        let model =
            context.itemModels.listItemModels ()
            |> Result.defaultWith (fun error -> failwith $"{file.value}: {error.describe}")
            |> List.tryFind (fun held -> held.entry.id = spec.modelId)
            |> Option.defaultWith (fun () -> failwith $"{file.value}: expected the shipped model {spec.modelId.value}; actual none")
        match spec.bindingOpt with
        | None -> LibraryModelChoice model
        | Some bound when bound = absorbingPlate.id.toElementValueId ->
            StackSampleChoice (model, Library.SampleVersionId.firstOf absorbingPlate.id, absorbingPlate)
        | Some bound ->
            match context.library.tryGetEntry bound with
            | Ok (Some (SourceItem source)) -> SourcePresetChoice (model, source)
            | _ -> failwith $"{file.value}: expected the source preset {bound.value}; actual unavailable"

    /// The eight items as added, every parameter known at its default: the decent lab's noise budget, the plate's thickness (spec 0050 S12.2).
    let private assertAbsorbingItems (built : BuiltAcceptanceKit) (reader : KitProxy) : unit =
        let itemsFile = FileName "*.item-json"
        let items = reader.readItems () |> TestAppScope.readKitResult itemsFile
        TestAppScope.expect itemsFile "labels" (absorbingItemSpecs |> List.map _.label |> List.sort) (items |> List.map _.label |> List.sort)
        for spec in absorbingItemSpecs do
            let file = ItemKitFile.fileNameOf spec.label.fileStem
            let item = items |> List.find (fun held -> held.label = spec.label)
            let choice = absorbingChoiceOf built.context spec
            let known : Parameter list =
                choice.parameterAxes @ spec.dial.parameterAxes
                |> List.map (fun axis ->
                    {
                        name = axis.name
                        state = KnownParameter axis.defaultValue
                    })
            let stateOf (name : ParameterName) : ParameterState option = item.tryFindParameter name |> Option.map _.state
            TestAppScope.expect file "id" spec.id item.id
            TestAppScope.expect file "model and version" choice.instance item.model
            TestAppScope.expect file "dial" FixedMounting item.dial
            TestAppScope.expect file "revision" ItemRevision.first item.revision
            TestAppScope.expect file "history" [] item.history
            TestAppScope.expect file "every parameter known at its default" known item.parameters
            if spec.modelId = ItemModelIds.muellerPolarimeterModelId then
                for axis in NoiseParam.decentLab.parameterAxes do
                    TestAppScope.expect file $"{axis.name.value} at the decent lab's budget" (Some (KnownParameter axis.defaultValue)) (stateOf axis.name)
            elif spec.modelId = ItemModelIds.stackSampleItemModelId then
                TestAppScope.expect file "substrate.thickness in metres" (AbsorbingBiaxialSample.plate.parameterValueOpt |> Option.map KnownParameter)
                    (stateOf ParameterName.substrateThickness)

    /// The library entry a preset's value id names (spec 0050 S12.2).
    let private presetEntryIdOf (preset : ElementValueId) : LibraryEntryId =
        preset.tryToLibraryEntryId
        |> Option.defaultWith (fun () -> failwith $"library/: expected a GUID preset id; actual {preset.value}")

    /// `library/` holds the dropped entries as dropped and the shipped closure as shipped, byte for byte (spec 0050 S12.2; 0432-impl-log.md, 0433-impl-log.md).
    let private assertAbsorbingLibrary (built : BuiltAcceptanceKit) (reader : KitProxy) : unit =
        let snapshots = reader.listSnapshots () |> TestAppScope.readKitResult (FileName "library/*")
        let library = built.folder.value.combine (FolderName "library")
        let libraryFile = FileName library.value
        TestAppScope.expect libraryFile "snapshot files" (snapshots |> List.map _.snapshotFileName |> List.sort) (TestAppScope.relativeFiles library)
        let dropped = [ absorbingMaterial.id.toLibraryEntryId; absorbingPlate.id.toLibraryEntryId ]
        // A palette element in a kit window is an empty slot, so no palette preset is snapshotted (spec 0051 `.manual/003` 3.8, D7).
        let shipped =
            [
                presetEntryIdOf PresetIds.heNe633
                ItemModelIds.sourcePresetItemModelId.toLibraryEntryId
                ItemModelIds.muellerPolarimeterModelId.toLibraryEntryId
                ItemModelIds.stackSampleItemModelId.toLibraryEntryId
                CategoryIds.crystal.toLibraryEntryId
            ]
        let expected : LibraryVersionRef list =
            dropped @ shipped
            |> List.map (fun entryId ->
                let reference : LibraryVersionRef =
                    {
                        entryId = entryId
                        version = VersionNumber.first
                    }
                reference)
            |> List.sort
        TestAppScope.expect libraryFile "snapshot entries at version 1" expected (snapshots |> List.map _.libraryVersionRef |> List.sort)
        let builtIn = CatalogueProxy.builtInFolderUnder built.context.locations.shippedCatalogueFolder
        let entries = droppedEntriesFolderOf built.folder
        for envelope in snapshots do
            let origin = if List.contains envelope.content.id dropped then entries else builtIn
            let source = envelope.content.fileName.combine origin
            let target = envelope.snapshotFileName.combine library
            TestAppScope.expect target $"entry file {source.value} exists" true (File.Exists source.value)
            assertSameBytes source target

    /// The table's and every AT-2 setup's slots: the laser, `plate` and both polarimeters, Polarimeter R on the plate's reflected ray (spec 0050 S12.2).
    let private assertAbsorbingSlots (file : FileName) (plate : AcceptanceItem) (slots : SetupSlot list) : unit =
        let slotOf (slot : ElementId) : SetupSlot =
            slots
            |> List.tryFind (fun element -> element.id = slot)
            |> Option.defaultWith (fun () -> failwith $"{file.value}: expected the slot {slot.value}; actual none")
        TestAppScope.expect file "slots" [ sourceSlot; firstObjectSlot; detectorSlot; reflectedDetectorSlot ] (slots |> List.map _.id)
        for slot, spec in [ sourceSlot, absorbingLaser; firstObjectSlot, plate; detectorSlot, polarimeterT; reflectedDetectorSlot, polarimeterR ] do
            TestAppScope.expect file $"{slot.value} content" (ItemInSlot (spec.id, PlacementTurn)) (slotOf slot).content
        TestAppScope.expect file "the added detector's ray" (BeamLayout.OnReflectedBeamOf firstObjectSlot) (slotOf reflectedDetectorSlot).beam

    /// The table holds the laser, the plate and both polarimeters, Polarimeter R on the plate's reflected ray (spec 0050 S12.2).
    let private assertAbsorbingTable (model : TableAndElementRotationModel) : unit =
        assertAbsorbingSlots KitFile.fileName opticAxisCutPlate model.elements
        TestAppScope.expect KitFile.fileName "plate captures" [ CaptureTransmitted; CaptureReflected; CaptureBoth ] (availableCaptures model firstObjectSlot)

    /// The configurations the experiment's scan reads: each point in order on its capture, T then R for both, as `AbsorbingBiaxialSample.configuration` (spec 0050 S12.2).
    let private configurationsOf (setups : Setup list) (experiment : Experiment) : MeasurementConfiguration list =
        let file = ExperimentKitFile.fileNameOf experiment.name.fileStem
        let setup =
            setups
            |> List.tryFind (fun held -> held.name = experiment.setup.name)
            |> Option.defaultWith (fun () -> failwith $"{file.value}: expected the setup {experiment.setup.describe}; actual none")
        let slots = setup.slotsAt experiment.setup.revision |> Result.defaultWith (fun error -> failwith $"{file.value}: {error.describe}")
        let plateSlot = (SetupSlot.inLightOrder slots |> List.find (fun roled -> roled.role = firstObjectRole)).slot
        let cut =
            match plateSlot.content.itemOpt |> Option.bind (fun id -> absorbingPlateCuts |> List.tryFind (fun (plate, _) -> plate.id = id)) with
            | Some (_, plateCut) -> plateCut
            | None -> failwith $"{file.value}: expected a plate in Obj 1 of {experiment.setup.describe}; actual %A{plateSlot.content}"
        let degreesOf (variable : VariableParameter) (point : ScanPoint) : float option =
            point.settings
            |> List.tryFind (fun setting -> setting.elementId = plateSlot.id && setting.variable = variable)
            |> Option.map _.value.value
        experiment.scan.points
        |> List.collect (fun point ->
            // Spec 0050 S12.2: a point that sets no incidence holds the plate at normal incidence.
            let incidence = degreesOf VaryR2 point |> Option.defaultValue 0.0
            let azimuth =
                degreesOf VaryR1 point
                |> Option.defaultWith (fun () -> failwith $"{file.value}: expected every point to set the plate's R1; actual %A{point.settings}")
            let observables =
                match point.capture with
                | CaptureTransmitted -> [ TransmittedMueller ]
                | CaptureReflected -> [ ReflectedMueller ]
                | CaptureBoth -> [ TransmittedMueller; ReflectedMueller ]
            observables |> List.map (AbsorbingBiaxialSample.configuration cut incidence azimuth))

    /// The five setups in their names' ordinal order, each at its first revision over the table's slots, its plate in Object 1 (spec 0050 S12.2; 001 §J).
    let private assertAbsorbingSetups (setups : Setup list) : unit =
        let plates = absorbingPlateCuts |> List.map fst
        let names =
            plates
            |> List.map (fun plate -> SetupName plate.label.value)
            |> List.sortWith (fun first second -> String.CompareOrdinal(first.value, second.value))
        TestAppScope.expect (FileName "*.setup-json") "the five setups in their names' ordinal order" names (setups |> List.map _.name)
        for plate in plates do
            let setup = setups |> List.find (fun held -> held.name.value = plate.label.value)
            let file = SetupKitFile.fileNameOf setup.name.fileStem
            TestAppScope.expect file "revision" SetupRevision.first setup.revision
            TestAppScope.expect file "history" [] setup.history
            assertAbsorbingSlots file plate setup.slots

    /// One experiment per plate in id order, a `ScienceRun` in no session over its setup's first revision; OpticAxisCut's scan has two blocks (spec 0050 S12.2).
    let private assertAbsorbingExperiments (experiments : Experiment list) : unit =
        let experimentsFile = FileName "*.experiment-json"
        let plates = absorbingPlateCuts |> List.map fst
        TestAppScope.expect experimentsFile "the five experiments in id order" (plates |> List.map (fun plate -> ExperimentName plate.label.value)) (experiments |> List.map _.name)
        TestAppScope.expect experimentsFile "ids" (plates |> List.mapi (fun index _ -> ExperimentId (index + 1))) (experiments |> List.map _.id)
        for plate, experiment in List.zip plates experiments do
            let file = ExperimentKitFile.fileNameOf experiment.name.fileStem
            let setup : SetupReference =
                {
                    name = SetupName plate.label.value
                    revision = SetupRevision.first
                }
            TestAppScope.expect file "setup and revision" setup experiment.setup
            TestAppScope.expect file "role" ScienceRun experiment.role
            TestAppScope.expect file "session" None experiment.sessionOpt
            TestAppScope.expect file "scan blocks" (if plate = opticAxisCutPlate then 2 else 1) experiment.scan.blocks.Length

    /// The experiments' configurations in kit order are A1–A3 with each plate's together, 16, 4, 8, 8 and 8 of them (spec 0050 S12.2; 001 §Q AT-2).
    let private assertAbsorbingConfigurations (setups : Setup list) (experiments : Experiment list) : unit =
        let experimentsFile = FileName "*.experiment-json"
        let a1 = AbsorbingBiaxialSample.a1
        let expected = (a1 |> List.take 4) @ AbsorbingBiaxialSample.a2 @ (a1 |> List.skip 4) @ AbsorbingBiaxialSample.a3
        let full = AbsorbingBiaxialSample.fullConfigurations
        let holdsAll (configurations : MeasurementConfiguration list) (others : MeasurementConfiguration list) : bool =
            others |> List.forall (fun configuration -> List.contains configuration configurations)
        TestAppScope.expect experimentsFile "expected configuration count" full.Length expected.Length
        TestAppScope.expect experimentsFile "the expected order holds fullConfigurations, each plate's together" true (holdsAll expected full && holdsAll full expected)
        let read = experiments |> List.map (fun experiment -> experiment.name, configurationsOf setups experiment)
        TestAppScope.expect experimentsFile "configurations per experiment" [ 16; 4; 8; 8; 8 ] (read |> List.map (fun (_, configurations) -> configurations.Length))
        let numbered =
            read |> List.collect (fun (name, configurations) -> configurations |> List.mapi (fun index configuration -> name, index + 1, configuration))
        List.zip expected numbered
        |> List.iteri (fun position (wanted, (name, index, actual)) ->
            if actual <> wanted then
                let file = ExperimentKitFile.fileNameOf name.fileStem
                Assert.Fail($"{file.value}: configuration {index} (position {position + 1} of {expected.Length}): expected %A{wanted}; actual %A{actual}"))

    /// The kit's plate and Laser are the plate and the wavelength `AbsorbingBiaxialSample.configuration` builds with (spec 0050 S12.2).
    let private assertConfigurationInputs (folder : KitFolder) (reader : KitProxy) : unit =
        let snapshots = reader.listSnapshots () |> TestAppScope.readKitResult (FileName "library/*")
        let library = folder.value.combine (FolderName "library")
        let snapshotOf (entryId : LibraryEntryId) : LibraryEntryContent * FileName =
            match snapshots |> List.tryFind (fun envelope -> envelope.content.id = entryId) with
            | Some envelope -> envelope.content, envelope.snapshotFileName.combine library
            | None -> failwith $"{library.value}: expected the snapshot of {entryId.value}; actual none"
        let plateEntry, plateFile = snapshotOf absorbingPlate.id.toLibraryEntryId
        match plateEntry.toSample |> Result.map _.structure.substrate with
        | Ok (Some substrate) -> TestAppScope.expect plateFile "substrate thickness" AbsorbingBiaxialSample.plate substrate.thickness
        | Ok None -> Assert.Fail($"{plateFile.value}: expected a substrate; actual none")
        | Error error -> Assert.Fail($"{plateFile.value}: expected a sample; actual {error.describe}")
        let laserEntry, laserFile = snapshotOf (presetEntryIdOf PresetIds.heNe633)
        match laserEntry.toSourcePreset with
        | Ok source -> TestAppScope.expect laserFile "wavelength" AbsorbingBiaxialSample.waveLength source.waveLength
        | Error error -> Assert.Fail($"{laserFile.value}: expected a source preset; actual {error.describe}")

    /// `actual` is `expected`: its configuration and every `measured.element i j`, exactly (spec 0050 S12.2).
    let private assertSameObservation (file : FileName) (row : ControlCaption) (expected : MuellerObservation) (actual : MuellerObservation) : unit =
        TestAppScope.expect file $"{row.value}: configuration" expected.configuration actual.configuration
        for i in 0 .. 3 do
            for j in 0 .. 3 do
                TestAppScope.expect file $"{row.value}: m{i}{j}" (expected.measured.element i j) (actual.measured.element i j)

    /// `actual` is `expected` in order: every key, every configuration and every `measured.element i j` exactly (spec 0050 S12.2).
    let private assertSameObservations (file : FileName) (expected : KeyedObservation list) (actual : KeyedObservation list) : unit =
        TestAppScope.expect file "observation count" expected.Length actual.Length
        List.zip expected actual
        |> List.iteri (fun index (wanted, read) ->
            let row = ControlCaption $"observation {index + 1}"
            TestAppScope.expect file $"{row.value}: key" wanted.key read.key
            assertSameObservation file row wanted.observation read.observation)

    /// `data/` holds exactly the five seeds' files, each regenerated byte for byte and parsed back to its observations (spec 0050 S12.2).
    let private assertNoisyObservationFiles (folder : KitFolder) : unit =
        let data = folder.value.combine (FolderName "data")
        let seeds = AbsorbingBiaxialSample.noisySeeds
        TestAppScope.expect (FileName data.value) "data files" (seeds |> List.map (fun seed -> (noisyObservationsFile seed).leafName) |> List.sort) (TestAppScope.relativeFiles data)
        for seed in seeds do
            let file = labFileAt folder (noisyObservationsFile seed)
            let regenerated = observationFileOf seed
            let bytes = File.ReadAllBytes file.value
            TestAppScope.expect file "bytes against the regenerated observation file" (System.Text.UTF8Encoding(false).GetBytes regenerated.value) bytes
            match (FileByteContent bytes).kitFileText.parseMuellerObservations with
            | Ok readBack -> assertSameObservations file (plateKeyedObservationsOf seed) readBack
            | Error error -> Assert.Fail($"{file.value}: expected the observation profile to read it; actual {error.caption.value}")

    /// Each experiment holds seed 1's file in place, keyed by its name, and reads to its plate's observations in file order (spec 0050 S12.2).
    let private assertNoisyObservationAttachments (model : TableAndElementRotationModel) (reader : KitProxy) : unit =
        let experimentsFile = FileName "*.experiment-json"
        let experiments = reader.readExperiments () |> TestAppScope.readKitResult experimentsFile
        let seedOne = NoiseSeed 1
        let attachment : DataAttachment =
            {
                path = KitRelativePath "data/noisy-observations-seed-1.csv"
                contentHash = ContentHash.ofBytes (FileByteContent (System.Text.UTF8Encoding(false).GetBytes (observationFileOf seedOne).value))
            }
        let seedOneObservations = plateKeyedObservationsOf seedOne
        TestAppScope.expect experimentsFile "stored model" model.experimentCollection.experiments experiments
        TestAppScope.expect experimentsFile "experiments" (absorbingPlateCuts |> List.map (fun (plate, _) -> ExperimentName plate.label.value)) (experiments |> List.map _.name)
        for experiment in experiments do
            let file = ExperimentKitFile.fileNameOf experiment.name.fileStem
            let key = DataRowKey experiment.name.value
            let expected : MappedAttachment =
                {
                    attachment = attachment
                    profile = ImportProfileName.muellerObservations
                    rows = KeyedDataRows key
                    exclusions = []
                }
            TestAppScope.expect file "data attachment" (Some expected) experiment.dataOpt
            let data = experiment.dataOpt |> Option.defaultWith (fun () -> failwith $"{file.value}: expected its data attachment; actual none")
            match model.experimentData.readMapped data with
            | Ok (ObservationPoints observations) ->
                assertSameObservations file (seedOneObservations |> List.filter (fun keyed -> keyed.key = key)) observations
            | Ok _ -> Assert.Fail($"{file.value}: expected observation points; actual another shape of points")
            | Error error -> Assert.Fail($"{file.value}: expected its seed-1 observations; actual {error.caption.value}")

    /// The field case an axis lives in, by its case name, as the axis assertion compares it (spec 0050 S12.2).
    let private fieldCaseOf<'Parameters> (field : ParameterField<'Parameters>) : ControlCaption =
        match field with
        | RefractionIndexField _ -> ControlCaption "RefractionIndexField"
        | RhoValueField _ -> ControlCaption "RhoValueField"
        | EpsValueField _ -> ControlCaption "EpsValueField"

    /// The opened copy's generated axes are `AbsorbingTriclinicParameters.axes`: names and field cases in order, and reads of `perturbedStart` exactly (spec 0050 S12.2).
    let private assertAbsorbingAxes (file : FileName) (crystal : CrystalPhysics.CrystalParameters) : unit =
        let generated = crystal.parameterAxes
        let records = CrystalPhysics.AbsorbingTriclinicParameters.axes
        TestAppScope.expect file "fifteen record axes" 15 records.Length
        TestAppScope.expect file "generated axis names in the record's order" (records |> List.map _.name) (generated |> List.map _.name)
        TestAppScope.expect file "generated field cases" (records |> List.map (fun r -> fieldCaseOf r.field)) (generated |> List.map (fun a -> fieldCaseOf a.field))
        let start = CrystalPhysics.AbsorbingTriclinicCrystal AbsorbingBiaxialSample.perturbedStart
        List.zip records generated
        |> List.iteri (fun index (r, a) ->
            TestAppScope.expect file $"axis {index + 1} ({r.name.value}): the generated read of perturbedStart"
                (r.parameterValueOf AbsorbingBiaxialSample.perturbedStart) (a.parameterValueOf start))

    /// `AbsorbingTriclinic.study-json` holds the five experiments and the material at version 1, its components freed as set, no scope, link or session; each is per material (spec 0050 S12.2).
    let private assertAbsorbingStudy (model : TableAndElementRotationModel) (reader : KitProxy) : unit =
        let file = StudyKitFile.fileNameOf absorbingStudyName.fileStem
        let studies = reader.readStudies () |> TestAppScope.readKitResult file
        TestAppScope.expect file "study names" [ absorbingStudyName ] (studies |> List.map _.name)
        let study = List.exactlyOne studies
        TestAppScope.expect file "the five experiments in kit order" (absorbingPlateCuts |> List.map (fun (plate, _) -> ExperimentName plate.label.value)) study.experiments
        let material =
            match study.materials with
            | [ only ] -> only
            | held -> failwith $"{file.value}: expected one opened material; actual {held.Length}"
        TestAppScope.expect file "opened version" absorbingMaterialVersion material.version
        TestAppScope.expect file "opened name" absorbingMaterial.name material.name
        TestAppScope.expect file "opened crystal" (OpenedCrystalPhysics (CrystalPhysics.AbsorbingTriclinicCrystal AbsorbingBiaxialSample.absorbing)) material.physics
        let unknowns =
            material.parameters
            |> List.choose (fun parameter ->
                match parameter.state with
                | UnknownParameter _ -> Some parameter.name
                | KnownParameter _ | CalibratedParameter _ -> None)
        TestAppScope.expect file "fifteen unknown components" 15 unknowns.Length
        TestAppScope.expect file "components freed from perturbedStart within the harness's box" (perturbedStartUnknownsOf (openedCrystalOf material)) material.parameters
        TestAppScope.expect file "scopes" [] study.scopes
        TestAppScope.expect file "links" [] study.links
        TestAppScope.expect file "session gains" [] study.sessionGains
        let held = studyIn model absorbingStudyName
        TestAppScope.expect file "the window's study" held study
        for parameter in material.parameters do
            let reference = MaterialParameterRef (material.version, parameter.name)
            TestAppScope.expect file $"{reference.describe} scope" MaterialScope (held.scopeOf reference)
        let shared =
            CrystalPhysics.AbsorbingTriclinicParameters.axes
            |> List.map (fun r -> MaterialParameterRef (material.version, r.name), MaterialOwner material.version)
        let layout = held.parameterLayoutWith model.studyForwardContext
        TestAppScope.expect file "one vector entry per component, shared by the five plates" shared (layout.entries |> List.map (fun entry -> entry.unknown.parameter, entry.owner))
        assertAbsorbingAxes file (openedCrystalOf material)

    /// `studyDataOf`'s runs are the five experiments in study order, holding only observation rows that equal `expected` exactly (spec 0050 S12.2).
    let private assertAbsorbingStudyData (model : TableAndElementRotationModel) (expected : MuellerObservation list) : unit =
        let file = StudyKitFile.fileNameOf absorbingStudyName.fileStem
        let data =
            studyDataOf model absorbingStudyName
            |> Result.defaultWith (fun error -> failwith $"{file.value}: expected the study's data; actual {error.describe}")
        TestAppScope.expect file "fitted runs in study order" (studyIn model absorbingStudyName).experiments (data.runs |> List.map _.experiment.name)
        let observations =
            data.runs
            |> List.collect (fun run ->
                run.rows
                |> List.mapi (fun index row ->
                    match row with
                    | ObservationRow observation -> observation
                    | SettingsRow _ -> failwith $"{file.value}: {run.experiment.name.value} row {index + 1}: expected an observation row; actual a settings row"))
        TestAppScope.expect file "observation count" expected.Length observations.Length
        List.zip expected observations
        |> List.iteri (fun index (wanted, read) -> assertSameObservation file (ControlCaption $"observation {index + 1}") wanted read)

    /// The study's start residual is `throughputResidualFor` of the harness's forward over `observations` at the zero scaled vector, each entry within 1e-12 relative (spec 0050 S12.2).
    let private assertAbsorbingStartResidual (model : TableAndElementRotationModel) (observations : MuellerObservation list) : unit =
        let file = StudyKitFile.fileNameOf absorbingStudyName.fileStem
        let scaling = AbsorbingBiaxialSample.absorbingScalingAround AbsorbingBiaxialSample.perturbedStart
        let origin = ScaledVector (Array.zeroCreate scaling.dimension)
        let harness = (MuellerFitting.throughputResidualFor absorbingForward observations scaling origin).value
        let residual =
            studyResidualAtStart model absorbingStudyName
            |> Result.defaultWith (fun error -> failwith $"{file.value}: expected the residual at perturbedStart; actual {error.describe}")
        TestAppScope.expect file "residual entries, 16 per observation" (16 * observations.Length) residual.value.Length
        TestAppScope.expect file "residual entries" 704 residual.value.Length
        let mismatches =
            Array.zip harness residual.value
            |> Array.indexed
            |> Array.choose (fun (k, (h, a)) ->
                if abs (a - h) <= 1.0e-12 * max 1.0 (abs h) then None
                else Some $"entry {k}: expected {h:R}; actual {a:R}")
        if mismatches.Length > 0 then
            let listed = String.concat "\n" mismatches
            Assert.Fail($"{file.value}: {mismatches.Length} residual entries off the harness's by more than 1e-12 · max(1, |h_k|)\n{listed}")

    /// `run` is AT-2's one fit: the study's joint fit at its stamp over the five plates, nonlinear, one member converged from the entries' starts, and the material's fifteen components in the axes' order from `perturbedStart` (spec 0050 S12.2; 001 §Q AT-2).
    let private assertAbsorbingFitRun (run : FitRun) : unit =
        let file = FitRunKitFile.fileNameOf run.name.fileStem
        TestAppScope.expect file "name" absorbingFitRunName run.name
        TestAppScope.expect file "study" absorbingStudyName run.study
        TestAppScope.expect file "stamp" absorbingFitTime run.stamp
        TestAppScope.expect file "stage" JointStage run.stage
        TestAppScope.expect file "calibration use" CalibrationHeldFixed run.calibrationUse
        TestAppScope.expect file "engine" NonlinearStudyEngine run.engine
        TestAppScope.expect file "fitted runs, the five plates' experiments in kit order" (absorbingPlateCuts |> List.map (fun (plate, _) -> ExperimentName plate.label.value)) run.runs
        let expected : (FitEntryRef * ParameterValue) list =
            CrystalPhysics.AbsorbingTriclinicParameters.axes
            |> List.map (fun r ->
                let entry : FitEntryRef =
                    {
                        parameter = MaterialParameterRef (absorbingMaterialVersion, r.name)
                        owner = MaterialOwner absorbingMaterialVersion
                    }
                entry, r.parameterValueOf AbsorbingBiaxialSample.perturbedStart)
        let actual : (FitEntryRef * ParameterValue) list =
            run.entries
            |> List.map (fun held ->
                let entry : FitEntryRef =
                    {
                        parameter = held.parameter
                        owner = held.owner
                    }
                entry, held.start)
        TestAppScope.expect file "fifteen entries, the material's components in the axes' order, each starting at perturbedStart's value" expected actual
        match run.members with
        | [ only ] ->
            match only.outcome with
            | MemberConverged _ -> TestAppScope.expect file "the member's start, the entries' starts" (run.entries |> List.map _.start) only.start
            | MemberFailed failure -> Assert.Fail($"{file.value}: expected its one member to converge; actual {failure.value}")
        | members -> Assert.Fail($"{file.value}: expected one member, the study's single start; actual {members.Length}")

    /// The absorbing triclinic crystal `run` promotes the study's material to (spec 0050 S12.2; 001 §Q AT-2).
    let private recoveredCrystalOf (study : Study) (run : FitRun) : CrystalPhysics.AbsorbingTriclinicParameters =
        let file = FitRunKitFile.fileNameOf run.name.fileStem
        match study.promotedMaterialWith run absorbingMaterialVersion absorbingMaterial with
        | Ok promoted ->
            match promoted.physics with
            | CrystalParameterPhysics (CrystalPhysics.AbsorbingTriclinicCrystal p) -> p
            | other -> failwith $"{file.value}: expected the promoted material's absorbing triclinic crystal; actual %A{other}"
        | Error error -> failwith $"{file.value}: expected the study to promote its material; actual {error.describe}"

    /// The crystal `run` promotes the study's material to holds every reported quantity within the noisy suite's bands, each by its relative error to `absorbing` (spec 0050 S12.2; 001 §Q AT-2).
    let private assertAbsorbingRecovery (study : Study) (run : FitRun) : unit =
        let file = FitRunKitFile.fileNameOf run.name.fileStem
        let recovered = recoveredCrystalOf study run
        let truth = AbsorbingBiaxialSample.absorbing
        let breaches =
            AbsorbingBiaxialSample.absorbingBandBreaches (fun q -> MuellerFitting.RelativeError.between (q.valueIn recovered) (q.valueIn truth))
        if not breaches.IsEmpty then
            let chiSquared =
                run.members
                |> List.choose (fun held ->
                    match held.outcome with
                    | MemberConverged (_, chiSquaredOpt) -> chiSquaredOpt
                    | MemberFailed _ -> None)
                |> List.map (fun chi -> $"{chi.value:R}")
                |> String.concat ", "
            let listed = breaches |> List.map _.describe |> String.concat "; "
            Assert.Fail($"{file.value}: {breaches.Length} reported quantities out of the noisy suite's bands, chi-squared {chiSquared}: {listed}")

    /// The kit's stored `AbsorbingTriclinic` study as `reader` reads it, the one each fit run's crystal is promoted through (spec 0050 S12.2).
    let private storedAbsorbingStudy (reader : KitProxy) : Study =
        let studyFile = StudyKitFile.fileNameOf absorbingStudyName.fileStem
        reader.readStudies ()
        |> TestAppScope.readKitResult studyFile
        |> List.tryFind (fun held -> held.name = absorbingStudyName)
        |> Option.defaultWith (fun () -> failwith $"{studyFile.value}: expected the stored study; actual none")

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``AT-2 builds the absorbing-biaxial-noisy kit's library entries, items and table through the app's messages`` () : unit =
        let built = absorbingBuild.Force ()
        let reader = KitProxy.create built.folder
        assertKitFile reader TestAppScope.absorbingKitId (KitName "absorbing-biaxial-noisy") TestAppScope.absorbingKitAuthor absorbingStartTime absorbingTemplate
        assertAbsorbingItems built reader
        assertAbsorbingLibrary built reader
        assertAbsorbingTable built.model

    // F# reserves '@' in identifiers (FS1104) and compiles one as `<prefix>@<line>`, so the census id rides on the
    // compiled name (spec 0050 step 0433).
    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    [<CompiledName("AT-2's grid generator records one experiment per plate whose multi-variable scan reproduces a1 @ a2 @ a3 configuration by configuration")>]
    let ``AT-2's grid generator records one experiment per plate whose multi-variable scan reproduces a1, a2 and a3 configuration by configuration`` () : unit =
        let built = absorbingBuild.Force ()
        let reader = KitProxy.create built.folder
        let setups = reader.readSetups () |> TestAppScope.readKitResult (FileName "*.setup-json")
        let experiments = reader.readExperiments () |> TestAppScope.readKitResult (FileName "*.experiment-json")
        assertAbsorbingSetups setups
        assertAbsorbingExperiments experiments
        assertAbsorbingConfigurations setups experiments
        assertConfigurationInputs built.folder reader

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``AT-2 writes each seed's noisy observations through the Mueller-matrix observation profile and attaches seed 1's to its plates`` () : unit =
        let built = absorbingBuild.Force ()
        let reader = KitProxy.create built.folder
        assertNoisyObservationFiles built.folder
        assertNoisyObservationAttachments built.model reader

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``AT-2's study opens the material across the five plates, loads the harness's observations exactly, and its residual at perturbedStart is the harness's`` () : unit =
        let built = absorbingBuild.Force ()
        let reader = KitProxy.create built.folder
        assertAbsorbingStudy built.model reader
        let observations = byPlate (noisyObservationsOf attachedSeed)
        assertAbsorbingStudyData built.model observations
        assertAbsorbingStartResidual built.model observations

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``AT-2's study engine recovers one seed's fifteen constants and derived quantities within the bands AbsorbingBiaxialNoisyTests asserts`` () : unit =
        let built = absorbingBuild.Force ()
        let reader = KitProxy.create built.folder
        let fitFile = FileName $"{absorbingStudyName.fileStem.value}@*.fitrun-json"
        let runs = reader.readFitRuns absorbingStudyName |> TestAppScope.readKitResult fitFile
        let run =
            match runs with
            | [ only ] -> only
            | held -> failwith $"{fitFile.value}: expected exactly one fit run; actual {held.Length}"
        TestAppScope.expect fitFile "the stored fit run, the window's completed job" built.model.kitFitRuns runs
        assertAbsorbingFitRun run
        assertAbsorbingRecovery (storedAbsorbingStudy reader) run

    /// Every kit file, `library/` snapshot and `data/` observation file AT-2 writes is the committed kit's, `.oc/` aside (spec 0050 S12.2; 001 §Q AT-2).
    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``the built absorbing-biaxial-noisy kit equals the committed kit file by file`` () : unit =
        let built = absorbingBuild.Force ()
        match CommittedKit.AbsorbingBiaxialNoisyKit.tryFindFolder with
        | Some committed -> assertSameKitFiles built.folder committed
        | None -> Assert.Fail($"Built kit: {built.folder.value.value}\ndata/absorbing-biaxial-noisy: expected the committed kit; actual folder missing")

    /// Each AT-2 item resolves through the kit's snapshots alone: its shipped model, the laser's preset, and each plate's dropped sample, whose material version resolves from the material's snapshot (spec 0050 S12.2; 001 §Q 'Open').
    let private assertAbsorbingSnapshotResolution (session : TestAppScope.CommittedKitSession) : unit =
        let context = session.context
        let model = session.model
        TestAppScope.expect (FileName "*.item-json") "the eight item identities"
            (absorbingItemSpecs |> List.map _.id |> List.sort) (model.kitItems |> List.map _.id |> List.sort)
        for item in model.kitItems do
            let file = ItemKitFile.fileNameOf item.label.fileStem
            let spec = absorbingItemSpecs |> List.find (fun held -> held.id = item.id)
            let choice =
                model.tryItemModelChoice item
                |> Option.defaultWith (fun () -> failwith $"{file.value}: expected the kit snapshots to resolve its stored model; actual none")
            TestAppScope.expect file "resolved stored model and binding" item.model choice.instance
            let version =
                match choice with
                | LibraryModelChoice version
                | SourcePresetChoice (version, _)
                | PolarizerPresetChoice (version, _)
                | StackSampleChoice (version, _, _) -> version
            TestAppScope.expect file "the shipped model entry" spec.modelId version.entry.id
            TestAppScope.expect file "model absent from the database" (Ok None) (context.itemModels.resolveItemModelVersion version.versionId)
            TestAppScope.expect file "model present in the snapshot overlay" (Ok (Some version)) (model.itemModels.resolveItemModelVersion version.versionId)
            match choice with
            | SourcePresetChoice (_, source) ->
                TestAppScope.expect file "bound preset" spec.bindingOpt (Some source.id)
                TestAppScope.expect file "bound preset absent from the database" (Ok None) (context.library.tryGetEntry source.id)
            | StackSampleChoice (_, sampleVersion, sample) ->
                TestAppScope.expect file "the dropped sample's first version" (Library.SampleVersionId.firstOf absorbingPlate.id) sampleVersion
                TestAppScope.expect file "the dropped sample, as the kit's snapshot holds it" absorbingPlate sample
                TestAppScope.expect file "sample absent from the database" (Ok None) (context.samples.resolveVersion sampleVersion)
                match sample.structure.substrate with
                | Some substrate ->
                    TestAppScope.expect file "the sample's material version" absorbingMaterialVersion substrate.materialId
                    TestAppScope.expect file "material absent from the database" (Ok None) (context.materials.resolveVersion substrate.materialId)
                    TestAppScope.expect file "material resolved from the kit's snapshot" (Ok (Some absorbingMaterial)) (model.materials.resolveVersion substrate.materialId)
                | None -> Assert.Fail($"{file.value}: expected the dropped sample's substrate; actual none")
            | LibraryModelChoice _
            | PolarizerPresetChoice _ -> ()

    /// Reproduce AT-2 from the committed kit's snapshots and observation files without changing the kit (spec 0050 S12.2; 001 §Q 'Open', AT-2).
    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``the committed absorbing-biaxial-noisy kit opens read-only against an empty library, writes nothing, and reproduces AT-2's data, model and fit`` () : unit =
        let session = TestAppScope.openCommittedReadOnly (FolderName "KitAcceptanceTests") AbsorbingBiaxialNoisyKit
        let opened = session.opened
        let model = session.model
        let reader = model.kit.proxy
        try
            assertNoisyObservationFiles opened.folder

            TestAppScope.assertEmptyLibrary session.context
            assertAbsorbingSnapshotResolution session

            assertKitFile reader TestAppScope.absorbingKitId (KitName "absorbing-biaxial-noisy") TestAppScope.absorbingKitAuthor absorbingStartTime absorbingTemplate
            assertAbsorbingSetups opened.setups
            assertAbsorbingExperiments opened.experiments
            assertAbsorbingConfigurations opened.setups opened.experiments
            assertConfigurationInputs opened.folder reader
            assertAbsorbingStudy model reader
            let fitFile = FileName $"{absorbingStudyName.fileStem.value}@*.fitrun-json"
            match opened.fitRuns with
            | [ stored ] -> assertAbsorbingFitRun stored
            | held -> Assert.Fail($"{fitFile.value}: expected the one committed fit run; actual {held.Length}")
            for experiment in model.experimentCollection.experiments do
                let file = ExperimentKitFile.fileNameOf experiment.name.fileStem
                let data =
                    experiment.dataOpt
                    |> Option.defaultWith (fun () -> failwith $"{file.value}: expected its data attachment; actual none")
                TestAppScope.expect file "recorded attachment hash unchanged" (Ok AttachedDataUnchanged) (model.experimentData.attachmentStatus data.attachment)
            assertNoisyObservationAttachments model reader
            TestAppScope.assertCommittedUnchanged AbsorbingBiaxialNoisyKit session.recorded

            let observations = byPlate (noisyObservationsOf attachedSeed)
            assertAbsorbingStudyData model observations
            assertAbsorbingStartResidual model observations

            let run =
                (readOnlyStudyFit session).runFit absorbingFitRequest UnwatchedIterations
                |> Result.defaultWith (fun error -> failwith $"{fitFile.value}: expected the study engine's fit run; actual {error.describe}")
            TestAppScope.expect fitFile "refit study" absorbingStudyName run.study
            TestAppScope.expect fitFile "refit stamp" (TestAppScope.refitTimeOf AbsorbingBiaxialNoisyKit) run.stamp
            TestAppScope.expect fitFile "refit stage" JointStage run.stage
            TestAppScope.expect fitFile "fixed calibrations" CalibrationHeldFixed run.calibrationUse
            TestAppScope.expect fitFile "nonlinear engine" NonlinearStudyEngine run.engine
            TestAppScope.expect fitFile "refit runs, the five plates' experiments in kit order"
                (absorbingPlateCuts |> List.map (fun (plate, _) -> ExperimentName plate.label.value)) run.runs
            assertAbsorbingRecovery (studyIn model absorbingStudyName) run
            TestAppScope.assertEmptyLibrary session.context
            TestAppScope.assertCommittedUnchanged AbsorbingBiaxialNoisyKit session.recorded
        finally
            closeCommittedReadOnly AbsorbingBiaxialNoisyKit session

    /// Send and reopen AT-2's built kit, its five observation files included, without changing its files or its study's start residual (spec 0050 S12.2; 001 §H, §Q 'Send').
    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``the absorbing-biaxial-noisy kit travels through Send kit and Open kit from zip unchanged, its study's residual at the start point included`` () : unit =
        let built = absorbingBuild.Force ()
        let run = FolderName "AT-2-send"
        let runFolder = runFolderOf run
        // A run's name is fixed, so a previous run's folder is removed first (spec 0050 S12.2).
        if Directory.Exists runFolder.value then Directory.Delete(runFolder.value, true)
        try
            let studyFile = StudyKitFile.fileNameOf absorbingStudyName.fileStem
            let builtResidual = startResidualOf built.model absorbingStudyName
            let target, reopened = sendAndReopen built.context built.model run
            try
                assertSameKitFiles built.folder target
                assertNoisyObservationFiles target

                let reopenedResidual = startResidualOf reopened absorbingStudyName
                TestAppScope.expect studyFile "start-point residual after SendKit and OpenKitZip, element for element"
                    builtResidual.value reopenedResidual.value
            finally
                reopened.kit.close () |> TestAppScope.readKitResult KitFile.fileName
        finally
            if Directory.Exists runFolder.value then Directory.Delete(runFolder.value, true)

    /// Seeds 2 to 5 re-attached to the plates and fitted through the shell after seed 1's, on demand only, as xUnit v3 runs an `Explicit` fact (spec 0050 S12.2).
    [<Fact(Explicit = true)>]
    [<Trait("Category", "kit-acceptance")>]
    let ``AT-2's five-seed ensemble through the study engine keeps every reported quantity's scatter within the noisy suite's bands`` () : unit =
        let built = buildAbsorbingKit (FolderName "AT-2-ensemble")
        try
            let refit (model : TableAndElementRotationModel) (seed : NoiseSeed, at : ClockTime) : TableAndElementRotationModel =
                built.setTime at
                let attached = attachSeedObservations built.folder seed model
                assertAbsorbingStudyData attached (byPlate (noisyObservationsOf seed))
                let fitted = runStudyFitThroughShell attached absorbingFitRequest (TimeSpan.FromSeconds 1800.0)
                let name = absorbingStudyName.fitRunNameAt at
                TestAppScope.expect (FitRunKitFile.fileNameOf name.fileStem) "committed fit run" (Some name) fitted.studyFit.job.committedResults
                fitted
            let fitted = ensembleFitTimes |> List.tail |> List.fold refit built.model
            let reader = KitProxy.create built.folder
            let fitFile = FileName $"{absorbingStudyName.fileStem.value}@*.fitrun-json"
            let runs = reader.readFitRuns absorbingStudyName |> TestAppScope.readKitResult fitFile
            TestAppScope.expect fitFile "the five seeds' fit runs in seed order" (ensembleFitTimes |> List.map (fun (_, at) -> absorbingStudyName.fitRunNameAt at)) (runs |> List.map _.name)
            TestAppScope.expect fitFile "the stored fit runs, the window's completed jobs" fitted.kitFitRuns runs
            let study = storedAbsorbingStudy reader
            let crystals = runs |> List.map (recoveredCrystalOf study)
            let table =
                AbsorbingBiaxialSample.absorbingReportedQuantities
                |> List.map (RecoveryEnsembles.statisticsFor AbsorbingBiaxialSample.absorbing crystals)
            let report = table |> List.map _.describe |> String.concat "; "
            let unscattered = table |> List.filter (fun row -> row.scatter.value <= 0.0) |> List.map _.quantity.name
            TestAppScope.expect fitFile $"quantities whose seed-to-seed scatter is zero, the noise never reaching the fit ({report})" [] unscattered
            let scatterOf (quantity : RecoveryEnsembles.RecoveredQuantity<CrystalPhysics.AbsorbingTriclinicParameters>) : MuellerFitting.RelativeError =
                match RecoveryEnsembles.rowFor table quantity.name with
                | Ok row -> row.scatter
                | Error error -> failwith $"{fitFile.value}: expected the ensemble row of {quantity.name.value}; actual %A{error}"
            let breaches = AbsorbingBiaxialSample.absorbingBandBreaches scatterOf
            if not breaches.IsEmpty then
                let listed = breaches |> List.map _.describe |> String.concat "; "
                Assert.Fail($"{fitFile.value}: {breaches.Length} reported quantities scatter out of the noisy suite's bands: {listed} ({report})")
        finally
            built.model.kit.close () |> TestAppScope.readKitResult KitFile.fileName

    // ============ AT-3 and AT-4: the direct reference kits, built through the pages' messages (spec 0051 `.manual/007` 9.1) ============

    /// The source, polarizer and detectors both direct kits hold, and the item labels their tables use.
    let private xenonLabel = ItemLabel "Xenon"
    let private polarizerLabel = ItemLabel "LP"
    let private transmittedDetectorLabel = ItemLabel "Detector T"
    let private reflectedDetectorLabel = ItemLabel "Detector R"

    /// A direct kit's naming template: the sample's label, then the experiment's note (spec 0050 S8.3).
    let private directTemplate = ControlCaption "{obj1}-{note}"

    /// A direct kit's item over `modelId`, bound to `bindingOpt`.
    let private directItem (id : string) (label : ItemLabel) (modelId : ItemModelId) (bindingOpt : ElementValueId option) : AcceptanceItem =
        {
            id = ItemId (Guid.Parse id)
            label = label
            modelId = modelId
            bindingOpt = bindingOpt
            dial = FixedMounting
        }

    /// One direct reference kit: its folder, its start screen's identity and clock, its items, the ids its tables' added
    /// slots take in turn, and how its setups and experiments are recorded.
    type private DirectKitSpec =
        {
            run : FolderName
            kit : CommittedKit
            folderName : FolderName
            kitId : KitId
            author : KitAuthor
            startTime : ClockTime
            items : AcceptanceItem list
            addedSlots : ElementId list
            record : AcceptanceItem list -> TableAndElementRotationModel -> TableAndElementRotationModel
        }

    /// The id of the item labelled `label` among `items`.
    let private directItemIdOf (items : AcceptanceItem list) (label : ItemLabel) : ItemId =
        (items |> List.find (fun item -> item.label = label)).id

    /// A new setup through the Setups page: New setup (the light source and the detector, D14), its name, the polarizer,
    /// the sample and a detector on the sample's reflected ray, each slot given its item by its chip's message, then
    /// Create (spec 0051 `.manual/007` 5.2).
    let private newDirectSetup
        (items : AcceptanceItem list)
        (name : SetupName)
        (sampleLabel : ItemLabel)
        (model : TableAndElementRotationModel)
        : TableAndElementRotationModel =
        let file = SetupKitFile.fileNameOf name.fileStem
        let creating = model |> apply [ ShowFlowStep SetupsFlowStep; BeginPageCreate; SetSetupName name.caption ]
        TestAppScope.expect file "New setup's page" CreatingPageEntry creating.pageMode
        TestAppScope.expect file "New setup's slots" [ LightSource; Detector ] (creating.elements |> List.map (fun slot -> slot.placement.catalogueKind))
        let slotOfKind (kind : CatalogueKind) (current : TableAndElementRotationModel) : ElementId =
            (current.elements |> List.findBack (fun slot -> slot.placement.catalogueKind = kind)).id
        let withPolarizer = creating |> updateTableAndElementRotation (AddElement LinearPolarizer)
        let polarizer = slotOfKind LinearPolarizer withPolarizer
        let withSample = withPolarizer |> updateTableAndElementRotation (AddElement Sample)
        let sample = slotOfKind Sample withSample
        let withReflected = withSample |> updateTableAndElementRotation (AddElement Detector)
        let reflected = slotOfKind Detector withReflected
        let idOf = directItemIdOf items
        let assigned =
            withReflected
            |> apply
                [
                    PlaceOnBeam (reflected, BeamLayout.OnReflectedBeamOf sample)
                    AssignSlot (sourceSlot, Some (idOf xenonLabel))
                    AssignSlot (polarizer, Some (idOf polarizerLabel))
                    AssignSlot (sample, Some (idOf sampleLabel))
                    AssignSlot (detectorSlot, Some (idOf transmittedDetectorLabel))
                    AssignSlot (reflected, Some (idOf reflectedDetectorLabel))
                ]
        TestAppScope.expect file "slot assignments" None assigned.kitRefusalOpt
        TestAppScope.expect file "sample captures" [ CaptureTransmitted; CaptureReflected; CaptureBoth ] (availableCaptures assigned sample)
        let created = assigned |> updateTableAndElementRotation CommitPageDraft
        TestAppScope.expect file "Create status" (Some (ControlCaption $"Created setup '{name.value}'.")) created.kitStatusCaption
        TestAppScope.expect file "page after Create" BrowsingPage created.pageMode
        TestAppScope.expect file "the chosen setup" (Some name) (created.activeSetupOpt |> Option.map (fun setup -> setup.name))
        created

    /// The chosen setup's sample slot.
    let private directSampleSlot (model : TableAndElementRotationModel) : ElementId =
        (model.elements |> List.find (fun slot -> slot.placement.catalogueKind = Sample)).id

    /// A new experiment over the chosen setup through the Experiments page: New experiment, `axis`, capture both and the
    /// note its name ends in, then Create, and OK when another experiment already uses the setup (spec 0051 `.manual/007`
    /// 6.4, 6.5).
    let private newDirectExperiment (note : string) (axis : TableAndElementRotationMsg list) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let file = FileName $"*-{note}.experiment-json"
        let creating =
            model
            |> apply ([ ShowFlowStep ExperimentsFlowStep; BeginPageCreate ] @ axis @ [ ExpChooseMeasurement ExperimentControls.CaptureBoth; SetRecordNote (ControlCaption note) ])
        TestAppScope.expect file "New experiment's page" CreatingPageEntry creating.pageMode
        let asked = creating |> updateTableAndElementRotation CommitPageDraft
        let created =
            match asked.sameSetupWarningOpt with
            | Some _ -> asked |> updateTableAndElementRotation ConfirmSameSetup
            | None -> asked
        TestAppScope.expect file "Create refusal" None created.kitRefusalOpt
        TestAppScope.expect file "page after Create" BrowsingPage created.pageMode
        created

    /// The sample's incidence (R2) over 0–89° in 90 points, the tunable source set to 600 nm (spec 0051 `.manual/007` 9.1,
    /// D10).
    let private incidenceAxisAt600nm (model : TableAndElementRotationModel) : TableAndElementRotationMsg list =
        [
            ExpChooseElement (directSampleSlot model)
            ExpChooseVariable ExperimentControls.VaryR2
            ExpSetRangeMin (ExperimentControls.ExperimentRangeEnd 0.0)
            ExpSetRangeMax (ExperimentControls.ExperimentRangeEnd 89.0)
            ExpSetRangePoints (ExperimentControls.ExperimentPointCount 90)
            ExpSetSourceWaveLength (ExperimentControls.BayNanometers 600.0)
        ]

    /// The source's wavelength over `low`–`high` nm in 1 nm steps (spec 0051 `.manual/007` 9.1, D10).
    let private waveLengthAxis (low : float) (high : float) : TableAndElementRotationMsg list =
        [
            ExpChooseElement sourceSlot
            ExpChooseVariable ExperimentControls.VaryWaveLength
            ExpSetRangeMin (ExperimentControls.ExperimentRangeEnd low)
            ExpSetRangeMax (ExperimentControls.ExperimentRangeEnd high)
            ExpSetRangePoints (ExperimentControls.ExperimentPointCount (int (high - low) + 1))
        ]

    let private multilayerStartTime = ClockTime (DateTime(2026, 6, 2, 12, 0, 0, DateTimeKind.Utc))
    let private langasiteStartTime = ClockTime (DateTime(2026, 6, 3, 12, 0, 0, DateTimeKind.Utc))
    let private stackLabel = ItemLabel "Stack"
    let private filmLabel = ItemLabel "Film"
    let private plateLabel = ItemLabel "Plate"
    let private stackBench = SetupName "Stack bench"
    let private filmBench = SetupName "Film bench"
    let private plateBench = SetupName "Plate bench"

    /// The items both direct kits hold, by their fixed ids.
    let private directCommonItems (xenon : string) (polarizer : string) (transmitted : string) (reflected : string) : AcceptanceItem list =
        [
            directItem xenon xenonLabel ItemModelIds.sourcePresetItemModelId (Some PresetIds.xenonMonochromator)
            directItem polarizer polarizerLabel ItemModelIds.idealPolarizerItemModelId (Some PresetIds.polarizerLp)
            directItem transmitted transmittedDetectorLabel ItemModelIds.intensityDetectorModelId None
            directItem reflected reflectedDetectorLabel ItemModelIds.intensityDetectorModelId None
        ]

    /// AT-3: the 100-pair stack, one setup, R and T against incidence at 600 nm and against wavelength over 500–700 nm.
    let private multilayerSpec : DirectKitSpec =
        {
            run = FolderName "AT-3"
            kit = MultilayerThinFilmKit
            folderName = FolderName "multilayer-thin-film"
            kitId = TestAppScope.multilayerKitId
            author = TestAppScope.multilayerKitAuthor
            startTime = multilayerStartTime
            items =
                directCommonItems "4b8e1a2c-6f3d-4c1e-9a7b-2d5e8f1a3c01" "4b8e1a2c-6f3d-4c1e-9a7b-2d5e8f1a3c02" "4b8e1a2c-6f3d-4c1e-9a7b-2d5e8f1a3c03" "4b8e1a2c-6f3d-4c1e-9a7b-2d5e8f1a3c04"
                @ [ directItem "4b8e1a2c-6f3d-4c1e-9a7b-2d5e8f1a3c05" stackLabel ItemModelIds.stackSampleItemModelId (Some SampleIds.multilayerGlassPairsId.toElementValueId) ]
            addedSlots =
                [
                    ElementId "6c1f3d2a-8e4b-4a7c-b5d9-1e2f3a4b5c01"
                    ElementId "6c1f3d2a-8e4b-4a7c-b5d9-1e2f3a4b5c02"
                    ElementId "6c1f3d2a-8e4b-4a7c-b5d9-1e2f3a4b5c03"
                ]
            record =
                fun items model ->
                    let onBench = model |> newDirectSetup items stackBench stackLabel
                    onBench
                    |> newDirectExperiment "incidence" (incidenceAxisAt600nm onBench)
                    |> newDirectExperiment "wavelength" (waveLengthAxis 500.0 700.0)
        }

    /// AT-4: the langasite film and the langasite plate on silicon, a setup each, R and T against wavelength over
    /// 250–600 nm.
    let private langasiteSpec : DirectKitSpec =
        {
            run = FolderName "AT-4"
            kit = LangasiteOnSiliconKit
            folderName = FolderName "langasite-on-silicon"
            kitId = TestAppScope.langasiteKitId
            author = TestAppScope.langasiteKitAuthor
            startTime = langasiteStartTime
            items =
                directCommonItems "7d2a4e6c-1b3f-4d5a-8c9e-0f1a2b3c4d01" "7d2a4e6c-1b3f-4d5a-8c9e-0f1a2b3c4d02" "7d2a4e6c-1b3f-4d5a-8c9e-0f1a2b3c4d03" "7d2a4e6c-1b3f-4d5a-8c9e-0f1a2b3c4d04"
                @ [
                    directItem "7d2a4e6c-1b3f-4d5a-8c9e-0f1a2b3c4d05" filmLabel ItemModelIds.stackSampleItemModelId (Some SampleIds.langasiteSiliconId.toElementValueId)
                    directItem "7d2a4e6c-1b3f-4d5a-8c9e-0f1a2b3c4d06" plateLabel ItemModelIds.stackSampleItemModelId (Some SampleIds.langasitePlateSiliconId.toElementValueId)
                ]
            addedSlots =
                [
                    ElementId "9e3b5f7d-2c4a-4e6b-9d0f-1a2b3c4d5e01"
                    ElementId "9e3b5f7d-2c4a-4e6b-9d0f-1a2b3c4d5e02"
                    ElementId "9e3b5f7d-2c4a-4e6b-9d0f-1a2b3c4d5e03"
                    ElementId "9e3b5f7d-2c4a-4e6b-9d0f-1a2b3c4d5e04"
                    ElementId "9e3b5f7d-2c4a-4e6b-9d0f-1a2b3c4d5e05"
                    ElementId "9e3b5f7d-2c4a-4e6b-9d0f-1a2b3c4d5e06"
                ]
            record =
                fun items model ->
                    model
                    |> newDirectSetup items filmBench filmLabel
                    |> newDirectExperiment "wavelength" (waveLengthAxis 250.0 600.0)
                    |> newDirectSetup items plateBench plateLabel
                    |> newDirectExperiment "wavelength" (waveLengthAxis 250.0 600.0)
        }

    /// Builds `spec`'s kit through the start screen, the Kit page's template, the add form and the Setups and Experiments
    /// pages; the run folder of a previous build, left for diagnosis, is removed first (spec 0050 S12.2; 0051 `.manual/007`
    /// 9.1).
    let private buildDirectKit (spec : DirectKitSpec) : BuiltAcceptanceKit =
        let runFolder = runFolderOf spec.run
        let folder = KitFolder (runFolder.combine spec.folderName)
        if Directory.Exists runFolder.value then Directory.Delete(runFolder.value, true)
        Directory.CreateDirectory folder.value.value |> ignore
        let context = TestAppScope.freshAppContext (FolderName "KitAcceptanceTests")
        let clock, setTime = TestAppScope.testClockAt spec.startTime
        let mutable openedOpt : OpenedKit option = None
        try
            let start, shown = TestAppScope.kitStartOver context clock spec.kitId spec.author folder
            let started = start |> updateKitStart (CreateKit folder)
            openedOpt <- shown.Value
            TestAppScope.expect KitFile.fileName "CreateKit status" None started.status
            let opened = openedOpt |> Option.defaultWith (fun () -> failwith $"{folder.value.value}: expected CreateKit to open the kit; actual no kit")
            let mintItemId, unusedItemIds = mintingFrom (FileName "*.item-json") "item-id" (spec.items |> List.map _.id)
            let mintElementId, unusedSlotIds = mintingFrom (FileName "*.setup-json") "added-slot id" spec.addedSlots
            let initial =
                {
                    TestAppScope.inverseModelOver context opened with
                        itemIds = { mintItemId = mintItemId }
                        elementIds = { mintElementId = mintElementId }
                }
                |> apply [ ShowFlowStep KitFlowStep; BeginPageEdit; SetNamingTemplateText directTemplate; CommitPageDraft ]
            TestAppScope.expect KitFile.fileName "template status" (Some (ControlCaption "Saved the naming template.")) initial.kitStatusCaption
            let stocked = spec.items |> List.fold (fun model item -> addItemThroughForm [] item model |> fst) initial
            TestAppScope.expect (FileName "*.item-json") "unused item identities" [] unusedItemIds.Value
            let recorded = spec.record spec.items stocked
            TestAppScope.expect (FileName "*.setup-json") "unused added-slot identities" [] unusedSlotIds.Value
            {
                folder = folder
                context = context
                model = recorded
                clock = clock
                setTime = setTime
            }
        with _ ->
            match openedOpt with
            | Some opened -> opened.library.kit.close () |> TestAppScope.readKitResult KitFile.fileName
            | None -> ()
            reraise ()

    let private multilayerBuild : Lazy<BuiltAcceptanceKit> = lazy (buildDirectKit multilayerSpec)
    let private langasiteBuild : Lazy<BuiltAcceptanceKit> = lazy (buildDirectKit langasiteSpec)

    /// What each direct kit's experiment draws and its script computes: the experiment's name, the script's light and
    /// system over the kit's materials, the sweep the script runs, and the script's own system when its materials are
    /// not the catalogue's (spec 0051 `.manual/007` 9.1).
    type private ScriptedExperiment =
        {
            experiment : ExperimentName
            info : Analytics.Variables.FixedInfo
            range : Analytics.Variables.RangedVariable
            scriptOpt : Analytics.Variables.FixedInfo option
        }

    /// The most the script's published gyration law moves LangasiteOnSilicon.fsx's R or T away from the catalogue's
    /// constant ρ over 250–600 nm: measured 3.0e-4 for the film's R and T, under 1e-5 for the plate's.
    [<Literal>]
    let private langasiteGyrationGap = 5.0e-4

    let private scriptedOf (kit : CommittedKit) : ScriptedExperiment list =
        let light = OpticalProperties.Standard.light600nmNormalLPs
        match kit with
        | MultilayerThinFilmKit ->
            let info : Analytics.Variables.FixedInfo = { incidentLightInfo = light; opticalSystem = ScriptCurves.multilayerGlassPairsSystem }
            [
                { experiment = ExperimentName "Stack-incidence"; info = info; range = ScriptCurves.incidence0to89; scriptOpt = None }
                { experiment = ExperimentName "Stack-wavelength"; info = info; range = ScriptCurves.waveLengthRange 200 500.0 700.0; scriptOpt = None }
            ]
        | LangasiteOnSiliconKit ->
            let overCatalogue (script : Analytics.Variables.FixedInfo) : ScriptedExperiment -> ScriptedExperiment =
                let system =
                    script.opticalSystem
                    |> ScriptCurves.overCatalogueMaterials (OpticalConstructor.Tests.BuiltinCatalogue.material MaterialIds.langasite).properties (OpticalConstructor.Tests.BuiltinCatalogue.material MaterialIds.silicon).properties
                fun scripted -> { scripted with info = { script with opticalSystem = system }; scriptOpt = Some script }
            let film = Analytics.StandardSystems.langasiteFilmOnSilicon ScriptCurves.langasiteThickness light
            let plate = Analytics.StandardSystems.langasiteSubstrateOnSilicon ScriptCurves.langasiteThickness light
            [
                { experiment = ExperimentName "Film-wavelength"; info = film; range = ScriptCurves.waveLengthRange 350 250.0 600.0; scriptOpt = None }
                |> overCatalogue film
                { experiment = ExperimentName "Plate-wavelength"; info = plate; range = ScriptCurves.waveLengthRange 350 250.0 600.0; scriptOpt = None }
                |> overCatalogue plate
            ]
        | OpticsMuellerKit
        | AbsorbingBiaxialNoisyKit -> []

    /// Each experiment's chart draws its script's R and T, point by point (spec 0051 `.manual/007` 9.1).
    let private assertScriptCurves (kit : CommittedKit) (model : TableAndElementRotationModel) : unit =
        let scripted = scriptedOf kit
        TestAppScope.expect (FileName "*.experiment-json") "the experiments, by name"
            (scripted |> List.map (fun one -> one.experiment)) (model.experimentCollection.experiments |> List.map (fun experiment -> experiment.name))
        for one in scripted do
            let file = ExperimentKitFile.fileNameOf one.experiment.fileStem
            let experiment = model.experimentCollection.experiments |> List.find (fun held -> held.name = one.experiment)
            let chart =
                match (model.withChartCachedFor experiment).experimentChartCacheOpt with
                | Some cached -> cached.chart
                | None -> failwith $"{file.value}: expected its chart; actual none"
            let seriesNamed (name : string) : (float * float) list =
                match chart.series |> List.tryFind (fun series -> series.name.value = name) with
                | Some series -> series.points |> List.map (fun point -> point.x, point.y)
                | None ->
                    let names = chart.series |> List.map (fun series -> series.name.value) |> String.concat ", "
                    failwith $"{file.value}: expected the series '{name}'; actual {names}"
            ScriptCurves.assertSameCurve $"{file.value}: T" 1.0e-6 (ScriptCurves.scriptData one.info one.range Berreman.FieldFunctions.T) (seriesNamed "Intensity (T)")
            ScriptCurves.assertSameCurve $"{file.value}: R" 1.0e-6 (ScriptCurves.scriptData one.info one.range Berreman.FieldFunctions.R) (seriesNamed "Intensity (R)")
            // The script's own materials draw within the gyration gap of the catalogue's.
            match one.scriptOpt with
            | Some script ->
                for fn, name in [ Berreman.FieldFunctions.T, "T"; Berreman.FieldFunctions.R, "R" ] do
                    let gap = ScriptCurves.largestGap (ScriptCurves.scriptData script one.range fn) (ScriptCurves.scriptData one.info one.range fn)
                    Assert.True(gap <= langasiteGyrationGap, $"{file.value}: the script's {name} is %.3e{gap} from the catalogue's, past %.3e{langasiteGyrationGap}")
            | None -> ()

    /// The kit's files: its items, setups and experiments, and no data, study or fit run (spec 0051 `.manual/007` 9.1).
    let private assertDirectKitFiles (spec : DirectKitSpec) (setups : SetupName list) (folder : KitFolder) : unit =
        let named (pattern : string) : string list =
            Directory.GetFiles(folder.value.value, pattern) |> Array.map Path.GetFileName |> Array.sort |> List.ofArray
        TestAppScope.expect (FileName "*.item-json") "item files"
            (spec.items |> List.map (fun item -> (ItemKitFile.fileNameOf item.label.fileStem).value) |> List.sort) (named "*.item-json")
        TestAppScope.expect (FileName "*.setup-json") "setup files"
            (setups |> List.map (fun name -> (SetupKitFile.fileNameOf name.fileStem).value) |> List.sort) (named "*.setup-json")
        TestAppScope.expect (FileName "*.experiment-json") "experiment files"
            (scriptedOf spec.kit |> List.map (fun one -> (ExperimentKitFile.fileNameOf one.experiment.fileStem).value) |> List.sort) (named "*.experiment-json")
        TestAppScope.expect (FileName "*.study-json") "study files" [] (named "*.study-json")
        TestAppScope.expect (FileName "*.fitrun-json") "fit run files" [] (named "*.fitrun-json")
        let data = Path.Combine(folder.value.value, "data")
        TestAppScope.expect (FileName "data") "data files" [||] (if Directory.Exists data then Directory.GetFiles(data, "*", SearchOption.AllDirectories) else [||])

    /// The committed kit opens read-only against an empty library, every item resolving from its snapshots, draws its
    /// scripts' curves and writes nothing: it is whole on disk, as D7 asks (spec 0051 `.manual/007` 9.1, D7).
    let private assertCommittedDirectKit (spec : DirectKitSpec) (setups : SetupName list) : unit =
        let session = TestAppScope.openCommittedReadOnly (FolderName "KitAcceptanceTests") spec.kit
        try
            TestAppScope.assertEmptyLibrary session.context
            let reader = session.model.kit.proxy
            assertKitFile reader spec.kitId (KitName spec.folderName.value) spec.author spec.startTime directTemplate
            assertDirectKitFiles spec setups session.opened.folder
            for item in session.model.kitItems do
                let file = ItemKitFile.fileNameOf item.label.fileStem
                match session.model.tryItemModelChoice item with
                | Some choice -> TestAppScope.expect file "model resolved from the kit's snapshots" item.model choice.instance
                | None -> Assert.Fail($"{file.value}: expected the kit's snapshots to resolve its model; actual none")
            assertScriptCurves spec.kit session.model
            TestAppScope.assertEmptyLibrary session.context
            TestAppScope.assertCommittedUnchanged spec.kit session.recorded
        finally
            closeCommittedReadOnly spec.kit session

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``AT-3 builds the multilayer-thin-film kit through the pages' messages, and its two experiments draw MultilayerThinFilm_02.fsx's R and T`` () : unit =
        let built = multilayerBuild.Force ()
        assertKitFile built.model.kit.proxy multilayerSpec.kitId (KitName "multilayer-thin-film") multilayerSpec.author multilayerStartTime directTemplate
        assertDirectKitFiles multilayerSpec [ stackBench ] built.folder
        assertScriptCurves MultilayerThinFilmKit built.model

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``the built multilayer-thin-film kit equals the committed kit file by file`` () : unit =
        let built = multilayerBuild.Force ()
        match MultilayerThinFilmKit.tryFindFolder with
        | Some committed -> assertSameKitFiles built.folder committed
        | None -> Assert.Fail($"Built kit: {built.folder.value.value}\ndata/multilayer-thin-film: expected the committed kit; actual folder missing")

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``the committed multilayer-thin-film kit opens read-only against an empty library, writes nothing, and draws its script's curves`` () : unit =
        assertCommittedDirectKit multilayerSpec [ stackBench ]

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``AT-4 builds the langasite-on-silicon kit through the pages' messages, and its film and plate draw LangasiteOnSilicon.fsx's R and T`` () : unit =
        let built = langasiteBuild.Force ()
        assertKitFile built.model.kit.proxy langasiteSpec.kitId (KitName "langasite-on-silicon") langasiteSpec.author langasiteStartTime directTemplate
        assertDirectKitFiles langasiteSpec [ filmBench; plateBench ] built.folder
        assertScriptCurves LangasiteOnSiliconKit built.model

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``the built langasite-on-silicon kit equals the committed kit file by file`` () : unit =
        let built = langasiteBuild.Force ()
        match LangasiteOnSiliconKit.tryFindFolder with
        | Some committed -> assertSameKitFiles built.folder committed
        | None -> Assert.Fail($"Built kit: {built.folder.value.value}\ndata/langasite-on-silicon: expected the committed kit; actual folder missing")

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``the committed langasite-on-silicon kit opens read-only against an empty library, writes nothing, and draws its script's curves`` () : unit =
        assertCommittedDirectKit langasiteSpec [ filmBench; plateBench ]
