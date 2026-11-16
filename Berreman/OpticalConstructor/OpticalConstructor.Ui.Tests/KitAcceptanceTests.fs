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

    let private kitId = KitId (Guid.Parse "e63c9d6e-7961-4d9f-9c8d-e5a151414e0e")
    let private kitAuthor = KitAuthor "optics-mueller lab"
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

    let private absorbingKitId = KitId (Guid.Parse "e15b30f6-b9ff-4a6b-a88d-b5401df2a7ae")
    let private absorbingKitAuthor = KitAuthor "absorbing-biaxial-noisy bench"
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

    let private expect<'Value> (file : FileName) (field : string) (expected : 'Value) (actual : 'Value) : unit =
        try Assert.Equal<'Value>(expected, actual)
        with :? Xunit.Sdk.XunitException as error -> Assert.Fail($"{file.value}: {field}\n{error.Message}")

    let private readKitResult<'Value> (file : FileName) (result : Result<'Value, KitError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith $"{file.value}: expected a successful read/write; actual {error.describe}")

    let private committedFolder () : KitFolder =
        CommittedKit.OpticsMuellerKit.tryFindFolder
        |> Option.defaultWith (fun () -> failwith "data/optics-mueller: expected the committed kit; actual folder missing")

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
        expect file "Add item status" None added.kitStatus
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
        expect file "parameter edit status" None edited.kitStatus
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
        expect KitFile.fileName "grid draft capture" None model.draftCapture
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
        expect KitFile.fileName "table light order" roles (SetupSlot.inLightOrder table.elements |> List.map _.role)
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
        expect (FileName "*.setup-json") "first grid setup count" 28 singles.kitSetups.Length
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
        expect (FileName "*.setup-json") "both grids setup count" 40 pairs.kitSetups.Length
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
        expect file "split key column" (SplitKeyColumn "experiment") preview.key
        expect file "split group order" (lab.groups |> List.map _.key) (preview.split.groups |> List.map _.key)
        let offers =
            preview.split.groups
            |> List.map (fun group ->
                match group.matching with
                | SetupOfferForGroup offer ->
                    expect file "template offers an existing grid setup" true (List.contains offer.setup model.kitSetups)
                    ToggleSplitOffer group.key
                | GroupWithoutMatch _ ->
                    let setup =
                        manualSetups |> Map.tryFind group.key
                        |> Option.defaultWith (fun () -> failwith $"{file.value}: unexpected unreadable key {group.key.value}")
                    OfferSplitGroupOver (group.key, setup)
                | ExperimentForGroup name -> failwith $"{file.value}: {name.value} was already recorded")
        let offered = split |> apply offers
        let ready = splitOf file offered
        expect file "unmatched groups after manual offers" [] ready.split.unmatched
        expect file "every group selected" (lab.groups |> List.map _.key |> List.sort) (ready.creating |> List.sort)
        let attached = offered |> updateTableAndElementRotation MatchByTemplate
        expect file "split completed" None attached.dataSplitOpt
        let appended = attached.experimentCollection.experiments |> List.skip model.experimentCollection.experiments.Length
        expect file "created names in file group order" (lab.groups |> List.map _.name) (appended |> List.map _.name)
        expect file "no setup added by the split" model.kitSetups attached.kitSetups
        attached

    let private buildAcceptanceExperiments (folder : KitFolder) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let activated = model |> updateTableAndElementRotation (ActivateSetup (SetupName "LP-(AIR#0)-LP"))
        let analyzer = slotFor analyzerRole activated
        let analyzerSlot = activated.elements |> List.find (fun slot -> slot.id = analyzer)
        expect KitFile.fileName "dial reading offered for LP2" true (List.contains VaryDialReading (variablesOfElement activated analyzerSlot))
        let captures = availableCaptures activated analyzer
        expect KitFile.fileName "Phone capture" [ CaptureTransmitted ] captures
        let captureChoice =
            match List.exactlyOne captures with
            | CaptureTransmitted -> ExperimentControls.CaptureT
            | CaptureReflected -> ExperimentControls.CaptureR
            | CaptureBoth -> ExperimentControls.CaptureBoth
        let drafted =
            activated |> apply [ ExpChooseElement analyzer; ExpChooseVariable ExperimentControls.VaryDialReading; ExpChooseMeasurement captureChoice ]
        expect KitFile.fileName "raw dial draft" (Some VaryDialReading) drafted.experimentCollection.draft.variable
        expect KitFile.fileName "raw dial range" analyzerDialRange drafted.experimentCollection.draft.range
        expect KitFile.fileName "draft capture" (Some CaptureTransmitted) drafted.draftCapture
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

    let private runAcceptanceFit
        (stage : FitStage)
        (expectedName : FitRunName)
        (model : TableAndElementRotationModel)
        : TableAndElementRotationModel =
        let file = FitRunKitFile.fileNameOf expectedName.fileStem
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
        let request : StudyFitRequest =
            {
                study = studyName
                stage = stage
                calibrationUse = CalibrationHeldFixed
            }
        let running = ready |> updateTableAndElementRotation (StudyFit (RunFit request))
        expect file "fit started" true running.studyFit.job.isRunning
        Assert.True(ended.Wait(TimeSpan.FromSeconds 60.0), $"{file.value}: expected fit completion within sixty seconds; actual no completion")
        let recorded = messages.ToArray () |> Array.toList
        let completions = recorded |> List.choose (function StudyFit (StudyFitEnded result) -> Some result | _ -> None)
        expect file "completion message count" 1 completions.Length
        let finished = running |> apply recorded
        match finished.studyFit.errorOpt with
        | Some error -> Assert.Fail($"{file.value}: expected a successful fit; actual {error.describe}")
        | None -> ()
        expect file "fit finished" false finished.studyFit.job.isRunning
        expect file "fit handle released" None finished.studyFit.handleOpt
        expect file "committed fit run" (Some expectedName) finished.studyFit.job.committedResults
        expect file "appended fit run names" ((model.kitFitRuns |> List.map _.name) @ [ expectedName ]) (finished.kitFitRuns |> List.map _.name)
        {
            finished with kitShell = model.kitShell
        }

    let private buildAcceptanceStudy
        (setTime : ClockTime -> unit)
        (model : TableAndElementRotationModel)
        : TableAndElementRotationModel =
        let file = StudyKitFile.fileNameOf studyName.fileStem
        let experiments = model.experimentCollection.experiments |> List.map _.name
        expect file "study experiment count" 53 experiments.Length
        let configured =
            model
            |> apply
                ([ StudyCommand (CreateStudy (studyName, experiments)); StudyCommand (SetFitMode (studyName, StagedFit)) ]
                 @ (sessionGainSettings |> List.map (fun setting -> StudyCommand (SetSessionGainModel (studyName, setting)))))
        expect file "study configuration status" None configured.kitStatus
        setTime calibrationTime
        let calibrated = configured |> runAcceptanceFit CalibrationStage calibrationRunName
        let calibrationFile = FitRunKitFile.fileNameOf calibrationRunName.fileStem
        let calibrationText = calibrated.kit.proxy.readKindFile FitRunKitFile calibrationRunName.fileStem |> readKitResult calibrationFile
        let published = calibrated |> updateTableAndElementRotation (PublishCalibration calibrationRunName)
        for label in [ ItemLabel "LP2"; ItemLabel "CPL1"; ItemLabel "CPL2" ] do
            let item = published.kitItems |> List.find (fun held -> held.label = label)
            expect (ItemKitFile.fileNameOf label.fileStem) "published revision" (ItemRevision 2) item.revision
        setTime objectTime
        let reconstructed = published |> runAcceptanceFit ObjectStage objectRunName
        expect calibrationFile "calibration run unchanged after publication and stage two" calibrationText
            (reconstructed.kit.proxy.readKindFile FitRunKitFile calibrationRunName.fileStem |> readKitResult calibrationFile)
        reconstructed

    /// A clock reading `instant` and the cell only a build sets to move it (spec 0050 S12.1, S12.2).
    let private testClockAt (instant : ClockTime) : ClockProvider * ClockTime ref =
        let time = ref instant
        let clock : ClockProvider =
            {
                now = fun () -> time.Value
            }
        clock, time

    /// The Inverse start screen over `folder` and the cell its `showKit` keeps the opened kit in (spec 0050 S12.1, S12.2).
    let private kitStartOver
        (context : OpticalConstructor.Ui.AppContext)
        (clock : ClockProvider)
        (kitId : KitId)
        (author : KitAuthor)
        (folder : KitFolder)
        : KitStartModel * OpenedKit option ref =
        let shown : OpenedKit option ref = ref None
        let startContext : KitStartContext =
            {
                kitHistory = KitHistoryProxy.createInMemory [ folder ]
                limit = KitHistoryLimit.defaultValue
                clock = clock
                kitIds =
                    {
                        mintKitId = fun () -> kitId
                    }
                author = author
                imports = context.catalogueImports
                picker = TestAppScope.kitPickerOf folder
                showKit = fun opened -> shown.Value <- Some opened
                activateKitWindow = fun _ -> NoKitWindowOpen
                zipPicker = KitZipPicker.none
                archive = KitArchiveProxy.create ()
                archiveJobs = KitArchiveJobHost.inert
            }
        initKitStart startContext InverseConstructor, shown

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

    /// Build through application messages, retaining the setup and experiment checkpoints (spec 0050 S12.1).
    let private buildOpticsMuellerKitWith
        (finish : (ClockTime -> unit) -> KitFolder -> TableAndElementRotationModel -> TableAndElementRotationModel)
        (run : FolderName)
        : BuiltAcceptanceKit =
        let runFolder = FolderName (Path.Combine(System.AppContext.BaseDirectory, "KitAcceptanceTests", run.value))
        let folder = KitFolder (runFolder.combine (FolderName "optics-mueller"))
        if Directory.Exists runFolder.value then failwith $"{runFolder.value}: expected a fresh run folder; actual already exists"
        let mutable openedOpt : OpenedKit option = None
        try
            let sourceData = Path.Combine((committedFolder ()).value.value, "data")
            let targetData = Path.Combine(folder.value.value, "data")
            Directory.CreateDirectory targetData |> ignore
            for directory in Directory.EnumerateDirectories(sourceData, "*", SearchOption.AllDirectories) do
                Directory.CreateDirectory(Path.Combine(targetData, Path.GetRelativePath(sourceData, directory))) |> ignore
            for file in Directory.EnumerateFiles(sourceData, "*", SearchOption.AllDirectories) do
                File.Copy(file, Path.Combine(targetData, Path.GetRelativePath(sourceData, file)))
            let context = TestAppScope.freshAppContext (FolderName "KitAcceptanceTests")
            let clock, time = testClockAt initialTime
            let start, shown = kitStartOver context clock kitId kitAuthor folder
            let started = start |> updateKitStart (CreateKit folder)
            openedOpt <- shown.Value
            expect KitFile.fileName "CreateKit status" None started.status
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
            expect (FileName "*.item-json") "unused item identities" [] unusedItemIds.Value
            let setups = buildAcceptanceSetups stocked
            expect (FileName "*.setup-json") "unused added-slot identities" [] unusedSlotIds.Value
            expect (FileName "*.setup-json") "setups including both corrections" 42 setups.kitSetups.Length
            let model = finish (fun at -> time.Value <- at) folder setups
            {
                folder = folder
                context = context
                model = model
                clock = clock
            }
        with _ ->
            try
                match openedOpt with
                | Some opened -> opened.library.kit.close () |> readKitResult KitFile.fileName
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
        let items = reader.readItems () |> readKitResult (FileName "*.item-json")
        expect (FileName "*.item-json") "labels" (itemSpecs |> List.map _.label |> List.sort) (items |> List.map _.label |> List.sort)
        let models = built.context.itemModels.listItemModels () |> Result.defaultWith (fun error -> failwith error.describe)
        for spec in itemSpecs do
            let file = ItemKitFile.fileNameOf spec.label.fileStem
            let item = items |> List.find (fun item -> item.label = spec.label)
            let model = models |> List.find (fun model -> model.entry.id = spec.modelId)
            let expectedModel =
                match spec.bindingOpt with
                | Some preset -> PresetInstance (model.versionId, preset)
                | None -> ItemModelVersionInstance model.versionId
            expect file "id" spec.id item.id
            expect file "model and version" expectedModel item.model
            expect file "dial" spec.dial item.dial
            expect file "revision" ItemRevision.first item.revision
            expect file "history" [] item.history
            expect file "photo" None item.photoOpt
            expect file "notes" (ItemNotes "") item.notes
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
            expect file "parameters (including starts, bounds, priors and known defaults)" expectedParameters item.parameters
            let unknowns =
                item.parameters
                |> List.choose (fun parameter ->
                    match parameter.state with
                    | UnknownParameter _ -> Some parameter.name
                    | KnownParameter _ | CalibratedParameter _ -> None)
                |> List.sort
            expect file "unknown parameter names" (unknownNamesFor spec |> List.sort) unknowns
            if spec.label = ItemLabel "Phone" then
                expect file "gain" (Some (KnownParameter (ParameterValue 1.0))) (item.tryFindParameter ParameterName.gain |> Option.map _.state)
                expect file "dark offset" (Some (KnownParameter (ParameterValue 0.0))) (item.tryFindParameter ParameterName.darkOffset |> Option.map _.state)

    let private assertStoredSetups (reader : KitProxy) : unit =
        let setups = reader.readSetups () |> readKitResult (FileName "*.setup-json")
        let expected = expectedSetups ()
        expect (FileName "*.setup-json") "names" (expected |> List.map _.name |> List.sort) (setups |> List.map _.name |> List.sort)
        for spec in expected do
            let setup = setups |> List.find (fun setup -> setup.name = spec.name)
            let file = SetupKitFile.fileNameOf spec.name.fileStem
            let ordered = setup.lightOrder
            expect file "revision" SetupRevision.first setup.revision
            expect file "history" [] setup.history
            expect file "light order" [ SourceSlotRole; prepRole; firstObjectRole; secondObjectRole; analyzerRole; detectorRole ] (ordered |> List.map _.role)
            let slot (role : SlotRole) : SetupSlot = (ordered |> List.find (fun held -> held.role = role)).slot
            let fixedItem (role : SlotRole) (label : ItemLabel) (angle : Angle) : unit =
                let held = slot role
                expect file $"{role.text} content" (ItemInSlot (itemIdOf label, PlacementTurn)) held.content
                expect file $"{role.text} angle" angle held.placement.r1
            fixedItem SourceSlotRole (ItemLabel "Laser") Angle.zero
            fixedItem prepRole spec.prep Angle.zero
            let firstLabel, firstAngle = spec.objects.first
            fixedItem firstObjectRole firstLabel firstAngle
            match spec.objects.secondOpt with
            | Some (label, angle) -> fixedItem secondObjectRole label angle
            | None ->
                expect file "Obj 2 content" EmptySlotContent (slot secondObjectRole).content
                expect file "Obj 2 angle" Angle.zero (slot secondObjectRole).placement.r1
            let reading = if spec.analyzer = ItemLabel "LP2" then Angle.degree 152.0<deg> else Angle.degree 80.0<deg>
            expect file "Analyzer 1 content and raw dial reading" (ItemInSlot (itemIdOf spec.analyzer, DialTurn reading)) (slot analyzerRole).content
            expect file "Analyzer 1 physical angle" Angle.zero (slot analyzerRole).placement.r1
            fixedItem detectorRole (ItemLabel "Phone") Angle.zero

    let private relativeFiles (folder : FolderName) : FileName list =
        Directory.GetFiles(folder.value, "*", SearchOption.AllDirectories)
        |> Array.map (fun file -> FileName (Path.GetRelativePath(folder.value, file)))
        |> Array.sort
        |> List.ofArray

    let private kitContentFiles (folder : KitFolder) : Set<FileName> =
        relativeFiles folder.value
        |> List.filter (fun file -> not (file.value.StartsWith($".oc{Path.DirectorySeparatorChar}", StringComparison.Ordinal)))
        |> Set.ofList

    let private assertSameBytes (expected : FileName) (actual : FileName) : unit =
        let expectedBytes = File.ReadAllBytes expected.value
        let actualBytes = File.ReadAllBytes actual.value
        expect actual $"bytes against {expected.value}" expectedBytes actualBytes

    /// The `.kit-json` a build's start screen and template write: created and modified at `instant`, no notes, profile or exclusion (spec 0050 S12.1, S12.2).
    let private assertKitFile
        (reader : KitProxy)
        (kitId : KitId)
        (name : KitName)
        (author : KitAuthor)
        (instant : ClockTime)
        (template : ControlCaption)
        : unit =
        let metadata = reader.readKitFile () |> readKitResult KitFile.fileName
        expect KitFile.fileName "schema version" KitFile.currentSchemaVersion metadata.schemaVersion
        expect KitFile.fileName "id" kitId metadata.id
        expect KitFile.fileName "name" name metadata.name
        expect KitFile.fileName "author" author metadata.author
        expect KitFile.fileName "notes" (KitNotes "") metadata.notes
        expect KitFile.fileName "created" instant metadata.created
        expect KitFile.fileName "modified" instant metadata.modified
        expect KitFile.fileName "naming template" template.value metadata.namingTemplate.value
        expect KitFile.fileName "import profiles" [] metadata.importProfiles
        expect KitFile.fileName "send exclusions" [] metadata.sendExclude

    let private assertLibrarySnapshots (built : BuiltAcceptanceKit) (reader : KitProxy) : unit =
        let snapshots = reader.listSnapshots () |> readKitResult (FileName "library/*")
        let library = built.folder.value.combine (FolderName "library")
        expect (FileName library.value) "snapshot files" (snapshots |> List.map _.snapshotFileName |> List.sort) (relativeFiles library)
        let requiredModels = itemSpecs |> List.map _.modelId |> List.distinct |> List.sort
        expect (FileName "library/*.item-model-json") "instantiated model count" 6 requiredModels.Length
        let storedModels =
            snapshots
            |> List.choose (fun envelope ->
                match envelope.content.toItemModel with
                | Ok entry -> Some entry.id
                | Error _ -> None)
            |> List.filter (fun id -> List.contains id requiredModels)
            |> List.sort
        expect (FileName "library/*.item-model-json") "instantiated models" requiredModels storedModels
        let builtIn = CatalogueProxy.builtInFolderUnder built.context.locations.shippedCatalogueFolder
        for envelope in snapshots do
            let source = envelope.content.fileName.combine builtIn
            let target = envelope.snapshotFileName.combine library
            expect target $"built-in entry {source.value} exists" true (File.Exists source.value)
            assertSameBytes source target

    let private assertLabBytes (folder : KitFolder) : unit =
        let original = (committedFolder ()).value.combine (FolderName "data")
        let copied = folder.value.combine (FolderName "data")
        let files = relativeFiles original
        expect (FileName copied.value) "lab files" files (relativeFiles copied)
        for file in files do assertSameBytes (file.combine original) (file.combine copied)

    let private hashOfFile (file : FileName) : ContentHash =
        let bytes = File.ReadAllBytes file.value |> FileByteContent
        ContentHash.ofBytes bytes

    let private labHashes (folder : KitFolder) : Map<FileName, ContentHash> =
        let data = folder.value.combine (FolderName "data")
        relativeFiles data |> List.map (fun file -> file, hashOfFile (file.combine data)) |> Map.ofList

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
        let experiments = reader.readExperiments () |> readKitResult experimentsFile
        let setups = reader.readSetups () |> readKitResult (FileName "*.setup-json")
        let metadata = reader.readKitFile () |> readKitResult KitFile.fileName
        let groups = files |> List.collect _.groups
        expect experimentsFile "lab group count" 53 groups.Length
        expect experimentsFile "experiment count" 53 experiments.Length
        expect experimentsFile "experiment order" (groups |> List.map _.name) (experiments |> List.map _.name)
        expect experimentsFile "ids in file group order" (groups |> List.mapi (fun index _ -> ExperimentId (index + 1))) (experiments |> List.map _.id)
        expect experimentsFile "stored model" built.model.experimentCollection.experiments experiments
        expect experimentsFile "Calibration runs" (calibrationNames |> List.sort) (experiments |> List.filter (fun experiment -> experiment.role = CalibrationRun) |> List.map _.name |> List.sort)
        for role, count in [ ScienceRun, 36; CalibrationRun, 9; DarkRun, 6; DiagnosticRun, 2 ] do
            expect experimentsFile $"{role.text} count" count (experiments |> List.filter (fun experiment -> experiment.role = role) |> List.length)
        expect experimentsFile "gain session count" 6 (experiments |> List.choose _.sessionOpt |> List.distinct |> List.length)
        expect experimentsFile "setup corrections" 2 (experiments |> List.sumBy (fun experiment -> experiment.corrections.Length))
        expect experimentsFile "excluded frames" 1 (experiments |> List.sumBy (fun experiment -> experiment.dataOpt |> Option.map (fun data -> data.exclusions.Length) |> Option.defaultValue 0))
        let restart = restartTimeOf files
        let original = committedFolder ()
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
                expect file "setup and revision" expectedSetup.reference experiment.setup
                match correctedNameOpt with
                | Some _ ->
                    let correction = experiment.corrections |> List.exactlyOne
                    expect file "correction's labelled setup" labelled.reference correction.previous
                    expect file "correction's actual setup" expectedSetup.reference correction.corrected
                    expect file "correction audit note" lrCorrectionNote.value correction.note.value
                | None -> expect file "no correction" [] experiment.corrections
                let analyzer = expectedSetup.lightOrder |> List.find (fun slot -> slot.role = analyzerRole)
                let axis : ScanAxis =
                    {
                        elementId = analyzer.slot.id
                        label = built.model.experimentCollection.draft.elementLabel
                        variable = VaryDialReading
                        range = analyzerDialRange
                    }
                let expectedScan = ExperimentScan.oneAxis axis CaptureTransmitted None
                expect file "analyzer raw-dial sweep and Phone capture" expectedScan experiment.scan
                let dialReadings = analyzerDialRange.sweptValues |> List.map (fun reading -> Angle.degree (reading.value * 1.0<deg>))
                for row in group.captures do
                    expect file $"capture {row.captureIndex.value} at {row.description.degrees} degrees is on the dial sweep" true (List.contains row.description dialReadings)
                expect file "role" (roleForGroup lab group) experiment.role
                expect file "session" (sessionForGroup restart lab group) experiment.sessionOpt
                expect file "notes" ExperimentNotes.none experiment.notes
                let data = experiment.dataOpt |> Option.defaultWith (fun () -> failwith $"{file.value}: expected its lab attachment")
                expect file "attachment path and committed SHA-256" attachment data.attachment
                expect file "import profile" ImportProfileName.phoneRoiCsv data.profile
                expect file "keyed selection" (KeyedDataRows group.key) data.rows
                expect file "excluded point numbers in group order" (excludedNumbersOf group) (data.exclusions |> List.map _.point)
                for exclusion in data.exclusions do expect file "exclusion reason" frameExclusionReason.value exclusion.reason.value
                let mapped =
                    built.model.experimentData.readMapped data
                    |> Result.defaultWith (fun error -> failwith $"{file.value}: {error.caption.value}")
                expect file "mapped captures, including excluded frames" (CapturePoints group.captures) mapped
                expect file "all capture descriptions validate against the scan" (Ok mapped) (mapped.validatedFor Intensity experiment.scan)
                let kept =
                    group.captures
                    |> List.filter (fun row ->
                        MuellerReference.excludedPoints
                        |> List.exists (fun excluded ->
                            excluded.experiment = row.experiment && excluded.captureIndex = row.captureIndex && excluded.description = row.description)
                        |> not)
                expect file "keptBy removes only the reference's contaminated frame" (CapturePoints kept) (mapped.keptBy data.exclusions)

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
        expect file "object matrix groups" (references |> List.map fst |> List.sort) (run.objectMatrices |> List.map _.items |> List.sort)
        for ids, reference in references do
            let labels = ids |> List.map (fun id -> (itemSpecs |> List.find (fun item -> item.id = id)).label.value) |> String.concat " then "
            let fitted = run.objectMatrices |> List.find (fun matrix -> matrix.items = ids)
            expect file $"{labels} matrix element count" 16 fitted.elements.Length
            let expectedElements = reference.rowMajorValues
            fitted.elements |> List.iteri (fun index actual ->
                let quantity = ControlCaption $"{file.value}: {labels}.{muellerNames.[index].value}"
                expectReferenceValue quantity expectedElements.[index].value actual.value)
        expect file "product check count" 1 run.results.productChecks.Length
        let check = List.exactlyOne run.results.productChecks
        expect file "product check objects in light order" [ qz; lr ] check.items
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
        try
            let original = committedFolder ()
            let expectedFiles = kitContentFiles original
            let actualFiles = kitContentFiles built.folder
            for file in Set.union expectedFiles actualFiles do
                let diagnostic = $"Built kit: {built.folder.value.value}\n{file.value}"
                if not (Set.contains file expectedFiles) then
                    Assert.Fail($"{diagnostic}: missing from the committed kit (extra in the built kit).")
                elif not (Set.contains file actualFiles) then
                    Assert.Fail($"{diagnostic}: extra in the committed kit (missing from the built kit).")
                else
                    try assertSameBytes (file.combine original.value) (file.combine built.folder.value)
                    with :? Xunit.Sdk.XunitException as error ->
                        Assert.Fail($"{diagnostic}: different bytes.\n{error.Message}")
        finally
            built.model.kit.close () |> readKitResult KitFile.fileName
        // Spec 0050 S12.1 / step 0428: a failed comparison retains the named kit for regeneration.
        Directory.Delete(Path.GetDirectoryName built.folder.value.value, true)

    /// Reconstruct from committed snapshots and lab bytes without changing the kit (spec 0050 S12.1; 001 §Q 'Open').
    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``the committed optics-mueller kit opens read-only against an empty library, writes nothing, and its study reproduces the reference reconstruction`` () : unit =
        let original = committedFolder ()
        let originalFiles = relativeFiles original.value
        let originalBytes : Map<FileName, FileByteContent> =
            originalFiles
            |> List.map (fun file -> file, FileByteContent (File.ReadAllBytes (file.combine original.value).value))
            |> Map.ofList
        let directoriesInCommittedKit () : FolderName list =
            Directory.GetDirectories(original.value.value, "*", SearchOption.AllDirectories)
            |> Array.map (fun directory -> FolderName (Path.GetRelativePath(original.value.value, directory)))
            |> Array.sort
            |> List.ofArray
        let originalDirectories = directoriesInCommittedKit ()
        let assertCommittedKitUnchanged () : unit =
            let folderFile = FileName original.value.value
            expect folderFile "every relative file path unchanged, including .oc" originalFiles (relativeFiles original.value)
            expect folderFile "every directory unchanged, including empty cache folders" originalDirectories (directoriesInCommittedKit ())
            for KeyValue(file, expected) in originalBytes do
                let actual = File.ReadAllBytes (file.combine original.value).value |> FileByteContent
                expect file "committed bytes unchanged by the read-only session" expected.value actual.value
        let mutable openedOpt : OpenedKit option = None
        try
            let context = TestAppScope.emptyLibraryAppContext (FolderName "KitAcceptanceTests")
            let assertEmptyLibrary () : unit =
                expect KitFile.fileName "empty item-model database" (Ok []) (context.itemModels.listItemModels ())
                expect KitFile.fileName "empty live library" (Ok [ LibraryTree.byKind [] ]) (context.library.libraryTrees ())
            assertEmptyLibrary ()
            let refitTime = ClockTime (DateTime(2026, 5, 31, 12, 3, 0, DateTimeKind.Utc))
            let clock : ClockProvider =
                {
                    now = fun () -> refitTime
                }
            let startContext : KitStartContext =
                {
                    kitHistory = KitHistoryProxy.createInMemory [ original ]
                    limit = KitHistoryLimit.defaultValue
                    clock = clock
                    kitIds = KitIdProvider.system
                    author = kitAuthor
                    imports = context.catalogueImports
                    picker = TestAppScope.kitPickerOf original
                    showKit = fun opened -> openedOpt <- Some opened
                    activateKitWindow = fun _ -> NoKitWindowOpen
                    zipPicker = KitZipPicker.none
                    archive = KitArchiveProxy.create ()
                    archiveJobs = KitArchiveJobHost.inert
                }
            let started = initKitStart startContext InverseConstructor |> updateKitStart (OpenKitReadOnly original)
            expect KitFile.fileName "OpenKitReadOnly status" None started.status
            let opened =
                openedOpt
                |> Option.defaultWith (fun () -> failwith $"{original.value.value}: OpenKitReadOnly did not open the committed kit")
            expect KitFile.fileName "opened folder" original opened.folder
            expect KitFile.fileName "read-only access" ReadOnlyKitAccess opened.library.kit.access
            expect KitFile.fileName "experiment count" 53 opened.experiments.Length
            expect KitFile.fileName "setup count" 42 opened.setups.Length
            expect KitFile.fileName "item count" 9 opened.items.Length
            let studyFile = StudyKitFile.fileNameOf studyName.fileStem
            expect studyFile "study names" [ studyName ] (opened.studies |> List.map _.name)
            let study = List.exactlyOne opened.studies
            expect studyFile "all experiments in the study" (opened.experiments |> List.map _.name) study.experiments
            expect studyFile "staged fit" StagedFit study.settings.mode
            let fitFile = FileName "QZ-LR@*.fitrun-json"
            expect fitFile "two stored fit runs" [ calibrationRunName; objectRunName ] (opened.fitRuns |> List.map _.name)
            expect fitFile "stored fit stages" [ CalibrationStage; ObjectStage ] (opened.fitRuns |> List.map _.stage)

            let model = TestAppScope.inverseModelOver context opened
            expect KitFile.fileName "window's read-only access" ReadOnlyKitAccess model.kit.access
            expect KitFile.fileName "window's experiments" opened.experiments model.experimentCollection.experiments
            expect KitFile.fileName "window's setups" opened.setups model.kitSetups
            expect KitFile.fileName "window's items" opened.items model.kitItems
            expect studyFile "window's studies" opened.studies model.kitStudies
            expect fitFile "window's fit runs" opened.fitRuns model.kitFitRuns
            expect (FileName "*.item-json") "all nine item identities"
                (itemSpecs |> List.map _.id |> List.sort) (model.kitItems |> List.map _.id |> List.sort)
            assertCommittedKitUnchanged ()
            assertEmptyLibrary ()
            for item in model.kitItems do
                let file = ItemKitFile.fileNameOf item.label.fileStem
                let choice =
                    model.tryItemModelChoice item
                    |> Option.defaultWith (fun () -> failwith $"{file.value}: the kit snapshots did not resolve its stored model")
                expect file "resolved stored model and binding" item.model choice.instance
                let version =
                    match choice with
                    | LibraryModelChoice version
                    | SourcePresetChoice (version, _)
                    | PolarizerPresetChoice (version, _)
                    | StackSampleChoice (version, _, _) -> version
                expect file "model absent from the database" (Ok None) (context.itemModels.resolveItemModelVersion version.versionId)
                expect file "model present in the snapshot overlay" (Ok (Some version)) (model.itemModels.resolveItemModelVersion version.versionId)
                match item.model with
                | PresetInstance (_, preset) ->
                    expect file "bound preset absent from the database" (Ok None) (context.library.tryGetEntry preset)
                | ItemModelVersionInstance _
                | SampleInstance _ -> ()

            let labFiles = readAcceptanceLabFiles original
            for experiment in model.experimentCollection.experiments do
                let file = ExperimentKitFile.fileNameOf experiment.name.fileStem
                let data =
                    experiment.dataOpt
                    |> Option.defaultWith (fun () -> failwith $"{file.value}: expected its lab attachment")
                let lab = labFiles |> List.find (fun lab -> lab.path = data.attachment.path)
                let group = lab.groups |> List.find (fun group -> group.name = experiment.name)
                expect file "recorded attachment hash unchanged" (Ok AttachedDataUnchanged)
                    (model.experimentData.attachmentStatus data.attachment)
                expect file "Phone ROI CSV profile" ImportProfileName.phoneRoiCsv data.profile
                expect file "attachment's own group" (KeyedDataRows group.key) data.rows
                let mapped =
                    model.experimentData.readMapped data
                    |> Result.defaultWith (fun error -> failwith $"{file.value}: {error.caption.value}")
                expect file "all mapped group points" (CapturePoints group.captures) mapped
            assertReferenceInstruments model.kitItems

            let seed = NoiseSeed 1
            let fit : StudyFitParam =
                {
                    forward = model.studyForwardContext
                    clock = clock
                    linearSolver = createMathNetSvd ()
                    deviates = UniformDeviateProvider.fromSeed seed
                    fitRuns = model.kitFitRuns
                }
            let request : StudyFitRequest =
                {
                    study = studyName
                    stage = ObjectStage
                    calibrationUse = CalibrationHeldFixed
                }
            let run =
                fit.runFit request UnwatchedIterations
                |> Result.defaultWith (fun error -> failwith $"{studyFile.value}: {error.describe}")
            expect fitFile "refit study" studyName run.study
            expect fitFile "refit stamp" refitTime run.stamp
            expect fitFile "refit stage" ObjectStage run.stage
            expect fitFile "fixed calibrations" CalibrationHeldFixed run.calibrationUse
            expect fitFile "linear reconstruction" LinearStudyEngine run.engine
            assertReferenceObjects run
            assertEmptyLibrary ()
            assertCommittedKitUnchanged ()
        finally
            try
                match openedOpt with
                | Some opened -> opened.library.kit.close () |> readKitResult KitFile.fileName
                | None -> ()
            finally
                assertCommittedKitUnchanged ()

    /// Send and reopen the built kit without changing its files or study residual (spec 0050 S12.1; 001 §H, §Q 'Send').
    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``the optics-mueller kit travels through Send kit and Open kit from zip unchanged, its study's residual at the start point included`` () : unit =
        let run = FolderName (Guid.NewGuid().ToString("N"))
        let built = buildOpticsMuellerKit run
        let runFolder = FolderName (Path.GetDirectoryName built.folder.value.value)
        let mutable openedOpt : OpenedKit option = None
        try
            let studyFile = StudyKitFile.fileNameOf studyName.fileStem
            let residualAtStart (model : TableAndElementRotationModel) : ResidualVector =
                studyResidualAtStart model studyName
                |> Result.defaultWith (fun error -> failwith $"{studyFile.value}: expected a start-point residual; actual {error.describe}")

            let builtResidual = residualAtStart built.model
            Assert.NotEmpty builtResidual.value
            let expectedFiles = kitContentFiles built.folder
            let zip = built.model.kit.kitFile.name.zipFileName.combine runFolder
            let archive = KitArchiveProxy.create ()
            let sendMessages = ResizeArray<TableAndElementRotationMsg> ()
            let sending =
                {
                    built.model with
                        kitShell =
                            {
                                built.model.kitShell with
                                    archive = archive
                                    archiveJobs = KitArchiveJobHost.immediate (SendKitProgressed >> sendMessages.Add) (SendKitEnded >> sendMessages.Add)
                            }
                }
                |> updateTableAndElementRotation (SendKit zip)
            expect zip "send started" true sending.kitSend.job.isRunning
            let recordedSend = List.ofSeq sendMessages
            match List.rev recordedSend with
            | SendKitEnded (Ok (KitArchiveFinished files)) :: progresses ->
                expect zip "sent file count" expectedFiles.Count files.value
                expect zip "one send progress per file" files.value progresses.Length
                for message in progresses do
                    match message with
                    | SendKitProgressed _ -> ()
                    | _ -> Assert.Fail($"{zip.value}: expected only send progress before the successful end")
            | SendKitEnded (Error error) :: _ -> Assert.Fail($"{zip.value}: SendKit failed: {error.describe}")
            | _ -> Assert.Fail($"{zip.value}: expected SendKitEnded with a successfully finished archive")
            let sent = sending |> apply recordedSend
            expect zip "send job returned to idle" KitArchiveJobState.idle sent.kitSend
            expect zip "send completion status"
                (Some (ControlCaption $"The kit was sent: {expectedFiles.Count} file(s) zipped.")) sent.kitStatus
            expect zip "zip written outside the kit" true (File.Exists zip.value)
            let entries =
                archive.listZipEntries zip
                |> Result.defaultWith (fun error -> failwith $"{zip.value}: {error.describe}")
            for entry in entries do
                match entry.path.pathRole with
                | KitAppCachePath -> Assert.Fail($"{zip.value}: local cache entry {entry.path.value} must not travel")
                | KitContentPath -> ()

            let target = KitFolder (FolderName (Path.Combine(runFolder.value, "received", "optics-mueller")))
            expect zip "fresh extraction target" false (Directory.Exists target.value.value)
            Directory.CreateDirectory target.value.value |> ignore
            let openMessages = ResizeArray<KitStartMsg> ()
            let startContext : KitStartContext =
                {
                    kitHistory = KitHistoryProxy.createInMemory [ target ]
                    limit = KitHistoryLimit.defaultValue
                    clock = built.clock
                    kitIds =
                        {
                            mintKitId = fun () -> kitId
                        }
                    author = kitAuthor
                    imports = built.context.catalogueImports
                    picker = TestAppScope.kitPickerOf target
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
            expect zip "extraction started" true extracting.zipJob.job.isRunning
            match openedOpt with
            | None -> ()
            | Some _ -> Assert.Fail($"{zip.value}: the kit must open only after OpenKitZipEnded is handled")
            let recordedOpen = List.ofSeq openMessages
            match List.rev recordedOpen with
            | OpenKitZipEnded (Ok (KitArchiveFinished files)) :: progresses ->
                expect zip "extracted file count" expectedFiles.Count files.value
                expect zip "one extraction progress per file" files.value progresses.Length
                for message in progresses do
                    match message with
                    | OpenKitZipProgressed _ -> ()
                    | _ -> Assert.Fail($"{zip.value}: expected only extraction progress before the successful end")
            | OpenKitZipEnded (Error error) :: _ -> Assert.Fail($"{zip.value}: OpenKitZip failed: {error.describe}")
            | _ -> Assert.Fail($"{zip.value}: expected OpenKitZipEnded with a successfully finished extraction")
            let ended = recordedOpen |> List.fold (fun model message -> updateKitStart message model) extracting
            expect zip "open status" None ended.status
            expect zip "extraction job returned to idle" KitArchiveJobState.idle ended.zipJob
            expect zip "completed extraction cleared" None ended.zipOpeningOpt
            expect zip "no unresolved open offer" NoKitStartOffer ended.offer
            let opened =
                openedOpt
                |> Option.defaultWith (fun () -> failwith $"{target.value.value}: OpenKitZip did not open the extracted kit")
            expect KitFile.fileName "reopened folder" target opened.folder
            expect KitFile.fileName "reopened access" ReadWriteKitAccess opened.library.kit.access
            let reopened = TestAppScope.inverseModelOver built.context opened
            expect KitFile.fileName "all relative kit, library and data paths outside .oc"
                expectedFiles (kitContentFiles target)
            for file in expectedFiles do
                assertSameBytes (file.combine built.folder.value) (file.combine target.value)

            let reopenedResidual = residualAtStart reopened
            Assert.NotEmpty reopenedResidual.value
            expect studyFile "start-point residual after SendKit and OpenKitZip, element for element"
                builtResidual.value reopenedResidual.value
        finally
            try
                match openedOpt with
                | Some opened -> opened.library.kit.close () |> readKitResult KitFile.fileName
                | None -> ()
            finally
                try built.model.kit.close () |> readKitResult KitFile.fileName
                finally Directory.Delete(runFolder.value, true)

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``AT-1 builds the optics-mueller kit's items and setups through the app's messages`` () : unit =
        let run = FolderName (Guid.NewGuid().ToString("N"))
        let built = buildOpticsMuellerKitWith (fun _ _ model -> model) run
        try
            let reader = KitProxy.create built.folder
            assertKitFile reader kitId (KitName "optics-mueller") kitAuthor initialTime labTemplate
            expect KitFile.fileName "clock" initialTime (built.clock.now ())
            expect (FileName "*.experiment-json") "setups-only build" [] (reader.readExperiments () |> readKitResult (FileName "*.experiment-json"))
            assertStoredItems built reader
            assertStoredSetups reader
            assertLibrarySnapshots built reader
            assertLabBytes built.folder
        finally
            try built.model.kit.close () |> readKitResult KitFile.fileName
            finally Directory.Delete(Path.GetDirectoryName built.folder.value.value, true)

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``AT-1 attaches every lab group of the final data files through the Phone ROI CSV profile, with its role, session, correction and exclusion`` () : unit =
        let original = committedFolder ()
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
            expect (FileName "data/*") "built lab file inventory and SHA-256 unchanged" originalHashes (labHashes built.folder)
            expect (FileName "data/optics-mueller/data/*") "committed lab file inventory and SHA-256 unchanged" originalHashes (labHashes original)
        finally
            try built.model.kit.close () |> readKitResult KitFile.fileName
            finally Directory.Delete(Path.GetDirectoryName built.folder.value.value, true)

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``AT-1's staged study calibrates the instruments on the AIR runs, publishes them and reconstructs QZ, LR and QZ then LR as the reference does`` () : unit =
        let run = FolderName (Guid.NewGuid().ToString("N"))
        let built = buildOpticsMuellerKit run
        try
            let reader = KitProxy.create built.folder
            let studyFile = StudyKitFile.fileNameOf studyName.fileStem
            let studies = reader.readStudies () |> readKitResult studyFile
            expect studyFile "study names" [ studyName ] (studies |> List.map _.name)
            let study = List.exactlyOne studies
            let experiments = reader.readExperiments () |> readKitResult (FileName "*.experiment-json")
            expect studyFile "experiment count" 53 study.experiments.Length
            expect studyFile "experiments in kit order" (experiments |> List.map _.name) study.experiments
            expect studyFile "fit mode" StagedFit study.settings.mode
            expect studyFile "six session gain settings" sessionGainSettings study.sessionGains
            let sessions = study.sessionsIn experiments
            let darkRuns = experiments |> List.filter (fun experiment -> experiment.role = DarkRun) |> List.map _.name
            expect studyFile "shared dark run count" 6 darkRuns.Length
            for setting in sessionGainSettings do
                let session = sessions |> List.find (fun session -> session.name = setting.session)
                expect studyFile $"{setting.session.value} gain is offered" true (List.contains setting.model (session.gainModelOffers experiments))
                expect studyFile $"{setting.session.value} has no own dark run" [] (session.experiments |> List.filter (fun name -> List.contains name darkRuns))
                expect studyFile $"{setting.session.value} uses all dark runs" darkRuns (session.darkRunsIn experiments)

            let fitFile = FileName "QZ-LR@*.fitrun-json"
            let runs = reader.readFitRuns studyName |> readKitResult fitFile
            expect fitFile "fit runs in stamp order" [ calibrationRunName; objectRunName ] (runs |> List.map _.name)
            expect fitFile "fit stamps" [ calibrationTime; objectTime ] (runs |> List.map _.stamp)
            expect fitFile "fit stages" [ CalibrationStage; ObjectStage ] (runs |> List.map _.stage)
            expect fitFile "stored fit runs match completed window jobs" built.model.kitFitRuns runs
            for fitted in runs do
                let file = FitRunKitFile.fileNameOf fitted.name.fileStem
                expect file "study" studyName fitted.study
                expect file "fit mode" StagedFit fitted.mode
                expect file "engine" LinearStudyEngine fitted.engine
                expect file "calibration use" CalibrationHeldFixed fitted.calibrationUse
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
            expect fitFile "five instrument unknowns" (expectedEntries |> List.sort) (actualEntries |> List.sort)
            expect fitFile "five instrument sigma entries" (expectedEntries |> List.sort) (calibration.results.sigmas |> List.map _.entry |> List.sort)
            expect fitFile "calibration AIR runs in kit order"
                (experiments |> List.filter (fun experiment -> experiment.role = CalibrationRun) |> List.map _.name) calibration.runs
            expect fitFile "object Science runs in kit order"
                (experiments |> List.filter (fun experiment -> experiment.role = ScienceRun) |> List.map _.name) reconstructed.runs

            let itemsFile = FileName "*.item-json"
            let items = reader.readItems () |> readKitResult itemsFile
            expect itemsFile "item identities" (itemSpecs |> List.map _.id |> List.sort) (items |> List.map _.id |> List.sort)
            let calibratedIds = referenceInstrumentAngles |> List.map (fun (label, _, _) -> itemIdOf label) |> List.distinct
            for item in items do
                let file = ItemKitFile.fileNameOf item.label.fileStem
                let expectedRevision = if List.contains item.id calibratedIds then ItemRevision 2 else ItemRevision.first
                expect file "revision after calibration publication" expectedRevision item.revision
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
                expect file $"{name.value} fitted value, sigma and fit-run provenance"
                    (Some (CalibratedParameter expected)) (item.tryFindParameter name |> Option.map _.state)
                let heldByStageTwo = reconstructed.parameters |> List.find (fun held -> held.parameter = parameter)
                expect file $"{name.value} held by stage two" (CalibratedParameter expected) heldByStageTwo.state

            expect KitFile.fileName "clock after the object fit" objectTime (built.clock.now ())
            assertReferenceInstruments items
            assertReferenceObjects reconstructed
            assertLabBytes built.folder
        finally
            try built.model.kit.close () |> readKitResult KitFile.fileName
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
                requestClose = fun () -> ()
            }
        let dropped =
            LibraryWindowView.initLibraryWindow windowContext WindowMode.Browse
            |> LibraryWindowView.updateLibraryWindow (LibraryWindowView.DropLibraryEntryFiles files)
        expect drop "drop refusal" None dropped.lastError
        let request =
            match List.ofSeq requests with
            | [ only ] -> only
            | held -> failwith $"{drop.value}: expected one import dialog request; actual {held.Length}"
        let decisions =
            match request with
            | ApplyDroppedEntries decisions -> decisions
            | SettlePendingCatalogue _ | ResolveKitSnapshot _ -> failwith $"{drop.value}: expected the dropped files' rows; actual {request.heading.value}"
        expect drop "dropped rows" added (decisions |> List.map _.entryId)
        expect drop "each row's default action" (added |> List.map (fun _ -> CatalogueImport.AddIncomingEntry)) (decisions |> List.map _.defaultAction)
        let applied : ImportDialogApplied option ref = ref None
        let dialog : ImportDialogContext =
            {
                imports = context.catalogueImports
                request = request
                onApplied = fun result -> applied.Value <- Some result
                requestClose = fun () -> ()
            }
        let confirmed = dialog.initImportDialog |> updateImportDialog ApplyImportChoices
        expect drop "import refusal" None confirmed.lastError
        let result = applied.Value |> Option.defaultWith (fun () -> failwith $"{drop.value}: expected the import dialog to apply; actual nothing applied")
        let completed = dropped |> LibraryWindowView.updateLibraryWindow (LibraryWindowView.LibraryImportApplied result)
        expect drop "Library window refusal" None completed.lastError
        expect drop "Library window status" (Some result.summary) completed.importStatus

    /// Fills the table's slots, then adds Polarimeter R's detector on the plate's reflected ray, so the plate is captured in T and R (spec 0050 S12.2).
    let private buildAbsorbingTable (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let assigned (slot : ElementId) (spec : AcceptanceItem) (current : TableAndElementRotationModel) : TableAndElementRotationModel =
            let next = current |> updateTableAndElementRotation (AssignSlot (slot, Some spec.id))
            expect KitFile.fileName $"{spec.label.value} assigned to {slot.value}" None next.kitStatus
            next
        let table =
            model
            |> assigned sourceSlot absorbingLaser
            |> assigned firstObjectSlot opticAxisCutPlate
            |> assigned detectorSlot polarimeterT
            |> apply [ AddElement Detector; PlaceOnBeam (reflectedDetectorSlot, BeamLayout.OnReflectedBeamOf firstObjectSlot) ]
            |> assigned reflectedDetectorSlot polarimeterR
        expect KitFile.fileName "plate captures" [ CaptureTransmitted; CaptureReflected; CaptureBoth ] (availableCaptures table firstObjectSlot)
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
        let status = generated.kitStatus |> Option.map _.value |> Option.defaultValue "none"
        let names = plates |> List.map _.label.value
        let heldSetups = opened.kitSetups |> List.map _.name
        let addedSetups = generated.kitSetups |> List.map _.name |> List.filter (fun name -> not (List.contains name heldSetups))
        expect (FileName "*.setup-json") $"setups the grid added (status: {status})" (List.sort names) (addedSetups |> List.map _.value |> List.sort)
        let appended = generated.experimentCollection.experiments |> List.skip opened.experimentCollection.experiments.Length
        expect (FileName "*.experiment-json") $"experiments the grid appended (status: {status})" names (appended |> List.map _.name.value)
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

    /// Writes each seed's observation file into `data/`, then splits seed 1's by `sample` and attaches it in place (spec 0050 S12.2; 001 §K).
    let private attachNoisyObservations (folder : KitFolder) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        for seed in AbsorbingBiaxialSample.noisySeeds do
            let written = labFileAt folder (noisyObservationsFile seed)
            Directory.CreateDirectory(Path.GetDirectoryName written.value) |> ignore
            File.WriteAllText(written.value, (observationFileOf seed).value, System.Text.UTF8Encoding(false))
        let file = labFileAt folder (noisyObservationsFile attachedSeed)
        let split = model |> updateTableAndElementRotation (SplitByColumn (file, ImportProfileName.muellerObservations))
        let statusOf (current : TableAndElementRotationModel) : string = current.kitStatus |> Option.map _.value |> Option.defaultValue "none"
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
        expect file "split key column" (SplitKeyColumn "sample") preview.key
        expect file $"groups in file order, each matched to the experiment its key names (status: {statusOf split})" experimentForEach matched
        let attached = split |> updateTableAndElementRotation MatchByTemplate
        expect file $"split completed (status: {statusOf attached})" None attached.dataSplitOpt
        expect file "experiments after the split" (model.experimentCollection.experiments |> List.map _.name) (attached.experimentCollection.experiments |> List.map _.name)
        expect file "no setup added by the split" model.kitSetups attached.kitSetups
        let profiles = attached.experimentCollection.experiments |> List.map (fun experiment -> experiment.dataOpt |> Option.map _.profile)
        expect file "every experiment attached through the profile" (plates |> List.map (fun _ -> Some ImportProfileName.muellerObservations)) profiles
        attached

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

    /// AT-2's study over the five experiments in kit order, the material opened and its components freed from `perturbedStart`; no scope is set and no fit runs (spec 0050 S12.2; 001 §Q AT-2).
    let private buildAbsorbingStudy (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let file = StudyKitFile.fileNameOf absorbingStudyName.fileStem
        let sent (current : TableAndElementRotationModel) (message : StudyMsg) : TableAndElementRotationModel =
            let next = current |> updateTableAndElementRotation (StudyCommand message)
            expect file "study command status" None next.kitStatus
            next
        let experiments = model.experimentCollection.experiments |> List.map _.name
        let opened =
            [ CreateStudy (absorbingStudyName, experiments); OpenMaterial (absorbingStudyName, absorbingMaterialVersion) ]
            |> List.fold sent model
        let copy =
            match (studyIn opened absorbingStudyName).materials with
            | [ material ] -> material
            | materials -> failwith $"{file.value}: expected one opened material; actual {materials.Length}"
        perturbedStartUnknownsOf copy.crystal
        |> List.map (fun parameter -> SetOpenedMaterialParameter (absorbingStudyName, copy.version, parameter))
        |> List.fold sent opened

    /// AT-2's kit, built through the Library drop, the import dialog, the start screen and the kit window's messages alone (spec 0050 S12.2; 001 §Q).
    let private buildAbsorbingKit (run : FolderName) : BuiltAcceptanceKit =
        let runFolder = FolderName (Path.Combine(System.AppContext.BaseDirectory, "KitAcceptanceTests", run.value))
        let folder = KitFolder (runFolder.combine (FolderName "absorbing-biaxial-noisy"))
        let entries = droppedEntriesFolderOf folder
        // A run's name is fixed, so the previous run's folder, left for diagnosis, is removed first (spec 0050 S12.2).
        if Directory.Exists runFolder.value then Directory.Delete(runFolder.value, true)
        Directory.CreateDirectory folder.value.value |> ignore
        Directory.CreateDirectory entries.value |> ignore
        let context = TestAppScope.freshAppContext (FolderName "KitAcceptanceTests")
        let clock, _ = testClockAt absorbingStartTime
        let material = LibraryEntryContent.ofMaterialEntry VersionNumber.first UserProvenance absorbingMaterial
        let sample = LibraryEntryContent.ofSample VersionNumber.first UserProvenance absorbingPlate
        dropOnLibraryWindow context [ writeEntryFile entries material; writeEntryFile entries sample ] [ material.id; sample.id ]
        let heldMaterial =
            context.materials.resolveVersion absorbingMaterialVersion
            |> Result.map (Option.map (fun (held : MaterialEntry) -> held.id))
        expect material.fileName "material held at version 1" (Ok (Some absorbingMaterial.id)) heldMaterial
        let heldSample =
            context.samples.resolveVersion (Library.SampleVersionId.firstOf absorbingPlate.id)
            |> Result.map (Option.map (fun (held : Library.Sample) -> held.id))
        expect sample.fileName "sample held at version 1" (Ok (Some absorbingPlate.id)) heldSample
        let mutable openedOpt : OpenedKit option = None
        try
            let start, shown = kitStartOver context clock absorbingKitId absorbingKitAuthor folder
            let started = start |> updateKitStart (CreateKit folder)
            openedOpt <- shown.Value
            expect KitFile.fileName "CreateKit status" None started.status
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
            expect (FileName "*.item-json") "unused item identities" [] unusedItemIds.Value
            let table = buildAbsorbingTable stocked
            expect (FileName "*.setup-json") "unused added-slot identities" [] unusedSlotIds.Value
            let recorded = buildAbsorbingGrids table
            let attached = attachNoisyObservations folder recorded
            let studied = buildAbsorbingStudy attached
            {
                folder = folder
                context = context
                model = studied
                clock = clock
            }
        with _ ->
            match openedOpt with
            | Some opened -> opened.library.kit.close () |> readKitResult KitFile.fileName
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
        let items = reader.readItems () |> readKitResult itemsFile
        expect itemsFile "labels" (absorbingItemSpecs |> List.map _.label |> List.sort) (items |> List.map _.label |> List.sort)
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
            expect file "id" spec.id item.id
            expect file "model and version" choice.instance item.model
            expect file "dial" FixedMounting item.dial
            expect file "revision" ItemRevision.first item.revision
            expect file "history" [] item.history
            expect file "every parameter known at its default" known item.parameters
            if spec.modelId = ItemModelIds.muellerPolarimeterModelId then
                for axis in NoiseParam.decentLab.parameterAxes do
                    expect file $"{axis.name.value} at the decent lab's budget" (Some (KnownParameter axis.defaultValue)) (stateOf axis.name)
            elif spec.modelId = ItemModelIds.stackSampleItemModelId then
                expect file "substrate.thickness in metres" (AbsorbingBiaxialSample.plate.parameterValueOpt |> Option.map KnownParameter)
                    (stateOf ParameterName.substrateThickness)

    /// The library entry a preset's value id names (spec 0050 S12.2).
    let private presetEntryIdOf (preset : ElementValueId) : LibraryEntryId =
        preset.tryToLibraryEntryId
        |> Option.defaultWith (fun () -> failwith $"library/: expected a GUID preset id; actual {preset.value}")

    /// `library/` holds the dropped entries as dropped and the shipped closure as shipped, byte for byte (spec 0050 S12.2; 0432-impl-log.md, 0433-impl-log.md).
    let private assertAbsorbingLibrary (built : BuiltAcceptanceKit) (reader : KitProxy) : unit =
        let snapshots = reader.listSnapshots () |> readKitResult (FileName "library/*")
        let library = built.folder.value.combine (FolderName "library")
        let libraryFile = FileName library.value
        expect libraryFile "snapshot files" (snapshots |> List.map _.snapshotFileName |> List.sort) (relativeFiles library)
        let dropped = [ absorbingMaterial.id.toLibraryEntryId; absorbingPlate.id.toLibraryEntryId ]
        let shipped =
            [
                presetEntryIdOf PresetIds.heNe633
                // The reflected detector keeps `AddElement Detector`'s seed, which its setups snapshot (0433-impl-log.md).
                presetEntryIdOf PresetIds.detectorIntensity
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
        expect libraryFile "snapshot entries at version 1" expected (snapshots |> List.map _.libraryVersionRef |> List.sort)
        let builtIn = CatalogueProxy.builtInFolderUnder built.context.locations.shippedCatalogueFolder
        let entries = droppedEntriesFolderOf built.folder
        for envelope in snapshots do
            let origin = if List.contains envelope.content.id dropped then entries else builtIn
            let source = envelope.content.fileName.combine origin
            let target = envelope.snapshotFileName.combine library
            expect target $"entry file {source.value} exists" true (File.Exists source.value)
            assertSameBytes source target

    /// The table's and every AT-2 setup's slots: the laser, `plate` and both polarimeters, Polarimeter R on the plate's reflected ray (spec 0050 S12.2).
    let private assertAbsorbingSlots (file : FileName) (plate : AcceptanceItem) (slots : SetupSlot list) : unit =
        let slotOf (slot : ElementId) : SetupSlot =
            slots
            |> List.tryFind (fun element -> element.id = slot)
            |> Option.defaultWith (fun () -> failwith $"{file.value}: expected the slot {slot.value}; actual none")
        expect file "slots" [ sourceSlot; firstObjectSlot; detectorSlot; reflectedDetectorSlot ] (slots |> List.map _.id)
        for slot, spec in [ sourceSlot, absorbingLaser; firstObjectSlot, plate; detectorSlot, polarimeterT; reflectedDetectorSlot, polarimeterR ] do
            expect file $"{slot.value} content" (ItemInSlot (spec.id, PlacementTurn)) (slotOf slot).content
        expect file "the added detector's ray" (BeamLayout.OnReflectedBeamOf firstObjectSlot) (slotOf reflectedDetectorSlot).beam

    /// The table holds the laser, the plate and both polarimeters, Polarimeter R on the plate's reflected ray (spec 0050 S12.2).
    let private assertAbsorbingTable (model : TableAndElementRotationModel) : unit =
        assertAbsorbingSlots KitFile.fileName opticAxisCutPlate model.elements
        expect KitFile.fileName "plate captures" [ CaptureTransmitted; CaptureReflected; CaptureBoth ] (availableCaptures model firstObjectSlot)

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
        expect (FileName "*.setup-json") "the five setups in their names' ordinal order" names (setups |> List.map _.name)
        for plate in plates do
            let setup = setups |> List.find (fun held -> held.name.value = plate.label.value)
            let file = SetupKitFile.fileNameOf setup.name.fileStem
            expect file "revision" SetupRevision.first setup.revision
            expect file "history" [] setup.history
            assertAbsorbingSlots file plate setup.slots

    /// One experiment per plate in id order, a `ScienceRun` in no session over its setup's first revision; OpticAxisCut's scan has two blocks (spec 0050 S12.2).
    let private assertAbsorbingExperiments (experiments : Experiment list) : unit =
        let experimentsFile = FileName "*.experiment-json"
        let plates = absorbingPlateCuts |> List.map fst
        expect experimentsFile "the five experiments in id order" (plates |> List.map (fun plate -> ExperimentName plate.label.value)) (experiments |> List.map _.name)
        expect experimentsFile "ids" (plates |> List.mapi (fun index _ -> ExperimentId (index + 1))) (experiments |> List.map _.id)
        for plate, experiment in List.zip plates experiments do
            let file = ExperimentKitFile.fileNameOf experiment.name.fileStem
            let setup : SetupReference =
                {
                    name = SetupName plate.label.value
                    revision = SetupRevision.first
                }
            expect file "setup and revision" setup experiment.setup
            expect file "role" ScienceRun experiment.role
            expect file "session" None experiment.sessionOpt
            expect file "scan blocks" (if plate = opticAxisCutPlate then 2 else 1) experiment.scan.blocks.Length

    /// The experiments' configurations in kit order are A1–A3 with each plate's together, 16, 4, 8, 8 and 8 of them (spec 0050 S12.2; 001 §Q AT-2).
    let private assertAbsorbingConfigurations (setups : Setup list) (experiments : Experiment list) : unit =
        let experimentsFile = FileName "*.experiment-json"
        let a1 = AbsorbingBiaxialSample.a1
        let expected = (a1 |> List.take 4) @ AbsorbingBiaxialSample.a2 @ (a1 |> List.skip 4) @ AbsorbingBiaxialSample.a3
        let full = AbsorbingBiaxialSample.fullConfigurations
        let holdsAll (configurations : MeasurementConfiguration list) (others : MeasurementConfiguration list) : bool =
            others |> List.forall (fun configuration -> List.contains configuration configurations)
        expect experimentsFile "expected configuration count" full.Length expected.Length
        expect experimentsFile "the expected order holds fullConfigurations, each plate's together" true (holdsAll expected full && holdsAll full expected)
        let read = experiments |> List.map (fun experiment -> experiment.name, configurationsOf setups experiment)
        expect experimentsFile "configurations per experiment" [ 16; 4; 8; 8; 8 ] (read |> List.map (fun (_, configurations) -> configurations.Length))
        let numbered =
            read |> List.collect (fun (name, configurations) -> configurations |> List.mapi (fun index configuration -> name, index + 1, configuration))
        List.zip expected numbered
        |> List.iteri (fun position (wanted, (name, index, actual)) ->
            if actual <> wanted then
                let file = ExperimentKitFile.fileNameOf name.fileStem
                Assert.Fail($"{file.value}: configuration {index} (position {position + 1} of {expected.Length}): expected %A{wanted}; actual %A{actual}"))

    /// The kit's plate and Laser are the plate and the wavelength `AbsorbingBiaxialSample.configuration` builds with (spec 0050 S12.2).
    let private assertConfigurationInputs (folder : KitFolder) (reader : KitProxy) : unit =
        let snapshots = reader.listSnapshots () |> readKitResult (FileName "library/*")
        let library = folder.value.combine (FolderName "library")
        let snapshotOf (entryId : LibraryEntryId) : LibraryEntryContent * FileName =
            match snapshots |> List.tryFind (fun envelope -> envelope.content.id = entryId) with
            | Some envelope -> envelope.content, envelope.snapshotFileName.combine library
            | None -> failwith $"{library.value}: expected the snapshot of {entryId.value}; actual none"
        let plateEntry, plateFile = snapshotOf absorbingPlate.id.toLibraryEntryId
        match plateEntry.toSample |> Result.map _.structure.substrate with
        | Ok (Some substrate) -> expect plateFile "substrate thickness" AbsorbingBiaxialSample.plate substrate.thickness
        | Ok None -> Assert.Fail($"{plateFile.value}: expected a substrate; actual none")
        | Error error -> Assert.Fail($"{plateFile.value}: expected a sample; actual {error.describe}")
        let laserEntry, laserFile = snapshotOf (presetEntryIdOf PresetIds.heNe633)
        match laserEntry.toSourcePreset with
        | Ok source -> expect laserFile "wavelength" AbsorbingBiaxialSample.waveLength source.waveLength
        | Error error -> Assert.Fail($"{laserFile.value}: expected a source preset; actual {error.describe}")

    /// `actual` is `expected`: its configuration and every `measured.element i j`, exactly (spec 0050 S12.2).
    let private assertSameObservation (file : FileName) (row : ControlCaption) (expected : MuellerObservation) (actual : MuellerObservation) : unit =
        expect file $"{row.value}: configuration" expected.configuration actual.configuration
        for i in 0 .. 3 do
            for j in 0 .. 3 do
                expect file $"{row.value}: m{i}{j}" (expected.measured.element i j) (actual.measured.element i j)

    /// `actual` is `expected` in order: every key, every configuration and every `measured.element i j` exactly (spec 0050 S12.2).
    let private assertSameObservations (file : FileName) (expected : KeyedObservation list) (actual : KeyedObservation list) : unit =
        expect file "observation count" expected.Length actual.Length
        List.zip expected actual
        |> List.iteri (fun index (wanted, read) ->
            let row = ControlCaption $"observation {index + 1}"
            expect file $"{row.value}: key" wanted.key read.key
            assertSameObservation file row wanted.observation read.observation)

    /// `data/` holds exactly the five seeds' files, each regenerated byte for byte and parsed back to its observations (spec 0050 S12.2).
    let private assertNoisyObservationFiles (folder : KitFolder) : unit =
        let data = folder.value.combine (FolderName "data")
        let seeds = AbsorbingBiaxialSample.noisySeeds
        expect (FileName data.value) "data files" (seeds |> List.map (fun seed -> (noisyObservationsFile seed).leafName) |> List.sort) (relativeFiles data)
        for seed in seeds do
            let file = labFileAt folder (noisyObservationsFile seed)
            let regenerated = observationFileOf seed
            let bytes = File.ReadAllBytes file.value
            expect file "bytes against the regenerated observation file" (System.Text.UTF8Encoding(false).GetBytes regenerated.value) bytes
            match (FileByteContent bytes).kitFileText.parseMuellerObservations with
            | Ok readBack -> assertSameObservations file (plateKeyedObservationsOf seed) readBack
            | Error error -> Assert.Fail($"{file.value}: expected the observation profile to read it; actual {error.caption.value}")

    /// Each experiment holds seed 1's file in place, keyed by its name, and reads to its plate's observations in file order (spec 0050 S12.2).
    let private assertNoisyObservationAttachments (built : BuiltAcceptanceKit) (reader : KitProxy) : unit =
        let experimentsFile = FileName "*.experiment-json"
        let experiments = reader.readExperiments () |> readKitResult experimentsFile
        let seedOne = NoiseSeed 1
        let attachment : DataAttachment =
            {
                path = KitRelativePath "data/noisy-observations-seed-1.csv"
                contentHash = ContentHash.ofBytes (FileByteContent (System.Text.UTF8Encoding(false).GetBytes (observationFileOf seedOne).value))
            }
        let seedOneObservations = plateKeyedObservationsOf seedOne
        expect experimentsFile "stored model" built.model.experimentCollection.experiments experiments
        expect experimentsFile "experiments" (absorbingPlateCuts |> List.map (fun (plate, _) -> ExperimentName plate.label.value)) (experiments |> List.map _.name)
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
            expect file "data attachment" (Some expected) experiment.dataOpt
            let data = experiment.dataOpt |> Option.defaultWith (fun () -> failwith $"{file.value}: expected its data attachment; actual none")
            match built.model.experimentData.readMapped data with
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
        expect file "fifteen record axes" 15 records.Length
        expect file "generated axis names in the record's order" (records |> List.map _.name) (generated |> List.map _.name)
        expect file "generated field cases" (records |> List.map (fun r -> fieldCaseOf r.field)) (generated |> List.map (fun a -> fieldCaseOf a.field))
        let start = CrystalPhysics.AbsorbingTriclinicCrystal AbsorbingBiaxialSample.perturbedStart
        List.zip records generated
        |> List.iteri (fun index (r, a) ->
            expect file $"axis {index + 1} ({r.name.value}): the generated read of perturbedStart"
                (r.parameterValueOf AbsorbingBiaxialSample.perturbedStart) (a.parameterValueOf start))

    /// `AbsorbingTriclinic.study-json` holds the five experiments and the material at version 1, its components freed as set, no scope, link or session; each is per material (spec 0050 S12.2).
    let private assertAbsorbingStudy (built : BuiltAcceptanceKit) (reader : KitProxy) : unit =
        let file = StudyKitFile.fileNameOf absorbingStudyName.fileStem
        let studies = reader.readStudies () |> readKitResult file
        expect file "study names" [ absorbingStudyName ] (studies |> List.map _.name)
        let study = List.exactlyOne studies
        expect file "the five experiments in kit order" (absorbingPlateCuts |> List.map (fun (plate, _) -> ExperimentName plate.label.value)) study.experiments
        let material =
            match study.materials with
            | [ only ] -> only
            | held -> failwith $"{file.value}: expected one opened material; actual {held.Length}"
        expect file "opened version" absorbingMaterialVersion material.version
        expect file "opened name" absorbingMaterial.name material.name
        expect file "opened crystal" (CrystalPhysics.AbsorbingTriclinicCrystal AbsorbingBiaxialSample.absorbing) material.crystal
        let unknowns =
            material.parameters
            |> List.choose (fun parameter ->
                match parameter.state with
                | UnknownParameter _ -> Some parameter.name
                | KnownParameter _ | CalibratedParameter _ -> None)
        expect file "fifteen unknown components" 15 unknowns.Length
        expect file "components freed from perturbedStart within the harness's box" (perturbedStartUnknownsOf material.crystal) material.parameters
        expect file "scopes" [] study.scopes
        expect file "links" [] study.links
        expect file "session gains" [] study.sessionGains
        let held = studyIn built.model absorbingStudyName
        expect file "the window's study" held study
        for parameter in material.parameters do
            let reference = MaterialParameterRef (material.version, parameter.name)
            expect file $"{reference.describe} scope" MaterialScope (held.scopeOf reference)
        let shared =
            CrystalPhysics.AbsorbingTriclinicParameters.axes
            |> List.map (fun r -> MaterialParameterRef (material.version, r.name), MaterialOwner material.version)
        let layout = held.parameterLayoutWith built.model.studyForwardContext
        expect file "one vector entry per component, shared by the five plates" shared (layout.entries |> List.map (fun entry -> entry.unknown.parameter, entry.owner))
        assertAbsorbingAxes file material.crystal

    /// `studyDataOf`'s runs are the five experiments in study order, holding only observation rows that equal `expected` exactly (spec 0050 S12.2).
    let private assertAbsorbingStudyData (model : TableAndElementRotationModel) (expected : MuellerObservation list) : unit =
        let file = StudyKitFile.fileNameOf absorbingStudyName.fileStem
        let data =
            studyDataOf model absorbingStudyName
            |> Result.defaultWith (fun error -> failwith $"{file.value}: expected the study's data; actual {error.describe}")
        expect file "fitted runs in study order" (studyIn model absorbingStudyName).experiments (data.runs |> List.map _.experiment.name)
        let observations =
            data.runs
            |> List.collect (fun run ->
                run.rows
                |> List.mapi (fun index row ->
                    match row with
                    | ObservationRow observation -> observation
                    | SettingsRow _ -> failwith $"{file.value}: {run.experiment.name.value} row {index + 1}: expected an observation row; actual a settings row"))
        expect file "observation count" expected.Length observations.Length
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
        expect file "residual entries, 16 per observation" (16 * observations.Length) residual.value.Length
        expect file "residual entries" 704 residual.value.Length
        let mismatches =
            Array.zip harness residual.value
            |> Array.indexed
            |> Array.choose (fun (k, (h, a)) ->
                if abs (a - h) <= 1.0e-12 * max 1.0 (abs h) then None
                else Some $"entry {k}: expected {h:R}; actual {a:R}")
        if mismatches.Length > 0 then
            let listed = String.concat "\n" mismatches
            Assert.Fail($"{file.value}: {mismatches.Length} residual entries off the harness's by more than 1e-12 · max(1, |h_k|)\n{listed}")

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``AT-2 builds the absorbing-biaxial-noisy kit's library entries, items and table through the app's messages`` () : unit =
        let built = absorbingBuild.Force ()
        let reader = KitProxy.create built.folder
        assertKitFile reader absorbingKitId (KitName "absorbing-biaxial-noisy") absorbingKitAuthor absorbingStartTime absorbingTemplate
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
        let setups = reader.readSetups () |> readKitResult (FileName "*.setup-json")
        let experiments = reader.readExperiments () |> readKitResult (FileName "*.experiment-json")
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
        assertNoisyObservationAttachments built reader

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``AT-2's study opens the material across the five plates, loads the harness's observations exactly, and its residual at perturbedStart is the harness's`` () : unit =
        let built = absorbingBuild.Force ()
        let reader = KitProxy.create built.folder
        assertAbsorbingStudy built reader
        let observations = byPlate (noisyObservationsOf attachedSeed)
        assertAbsorbingStudyData built.model observations
        assertAbsorbingStartResidual built.model observations
