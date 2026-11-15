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
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.NamingTemplates
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Propagation
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.SetupGrids
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Optimization.BenchNoise
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.StudyEngine
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.KitArchiveStore
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Storage.LabFileImport
open OpticalConstructor.Ui
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
            presetOpt : ElementValueId option
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
                presetOpt = Some PresetIds.heNe633
                dial = FixedMounting
            }
            {
                id = ItemId (Guid.Parse "375bde88-fe45-4bd7-ad77-f13708076224")
                label = ItemLabel "LP1"
                modelId = ItemModelIds.realLinearPolarizerModelId
                presetOpt = None
                dial = FixedMounting
            }
            {
                id = ItemId (Guid.Parse "72d0505d-8685-4884-89ad-ea13694c8693")
                label = ItemLabel "LP2"
                modelId = ItemModelIds.realLinearPolarizerModelId
                presetOpt = None
                dial = DialMounted DialNegative
            }
            {
                id = ItemId (Guid.Parse "0d2c2b6e-c91f-486b-b80b-a1b815c03aac")
                label = ItemLabel "CPL1"
                modelId = ItemModelIds.circularPolarizerModelId
                presetOpt = None
                dial = FixedMounting
            }
            {
                id = ItemId (Guid.Parse "b9f73589-6cfe-46d2-a5eb-2b11d6d6cd5f")
                label = ItemLabel "CPL2"
                modelId = ItemModelIds.circularPolarizerModelId
                presetOpt = None
                dial = DialMounted DialPositive
            }
            {
                id = ItemId (Guid.Parse "dc89cf47-6d94-4887-a0fb-49e3a8ca1cb4")
                label = ItemLabel "AIR"
                modelId = ItemModelIds.emptySlotModelId
                presetOpt = None
                dial = FixedMounting
            }
            {
                id = ItemId (Guid.Parse "436f47ea-a1c4-47ed-8c5c-f09eae8098b1")
                label = ItemLabel "QZ"
                modelId = ItemModelIds.blackBoxMuellerModelId
                presetOpt = None
                dial = FixedMounting
            }
            {
                id = ItemId (Guid.Parse "69c2edf9-3a4d-4b23-bea2-94ba305ca507")
                label = ItemLabel "LR"
                modelId = ItemModelIds.blackBoxMuellerModelId
                presetOpt = None
                dial = FixedMounting
            }
            {
                id = ItemId (Guid.Parse "93fae6cc-d838-45b7-b5f0-949f7a759e91")
                label = ItemLabel "Phone"
                modelId = ItemModelIds.intensityDetectorModelId
                presetOpt = None
                dial = FixedMounting
            }
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
            match choice, spec.presetOpt with
            | SourcePresetChoice (model, source), Some preset -> model.entry.id = spec.modelId && source.id = preset
            | LibraryModelChoice model, None -> model.entry.id = spec.modelId
            | _ -> false)
        |> Option.defaultWith (fun () -> failwith $"{spec.label.value}.item-json: expected model {spec.modelId.value}; actual no matching offer")

    let private addAcceptanceItem (model : TableAndElementRotationModel) (spec : AcceptanceItem) : TableAndElementRotationModel =
        let file = ItemKitFile.fileNameOf spec.label.fileStem
        let opened = model |> updateTableAndElementRotation OpenAddItemForm
        let offer = choiceFor spec (formOf opened).offers
        let filled =
            opened |> apply [ ChooseNewItemModel offer; SetNewItemLabel spec.label.caption; SetNewItemDial spec.dial ]
        let draft = (formOf filled).itemDraftOpt |> Option.defaultWith (fun () -> failwith $"{file.value}: expected an Add draft; actual none")
        let added = filled |> updateTableAndElementRotation (AddItem draft)
        expect file "Add item status" None added.kitStatus
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
            let mutable now = initialTime
            let clock : ClockProvider =
                {
                    now = fun () -> now
                }
            let startContext : KitStartContext =
                {
                    kitHistory = KitHistoryProxy.createInMemory [ folder ]
                    limit = KitHistoryLimit.defaultValue
                    clock = clock
                    kitIds =
                        {
                            mintKitId = fun () -> kitId
                        }
                    author = kitAuthor
                    imports = context.catalogueImports
                    picker = TestAppScope.kitPickerOf folder
                    showKit = fun opened -> openedOpt <- Some opened
                    activateKitWindow = fun _ -> NoKitWindowOpen
                    zipPicker = KitZipPicker.none
                    archive = KitArchiveProxy.create ()
                    archiveJobs = KitArchiveJobHost.inert
                }
            let started = initKitStart startContext InverseConstructor |> updateKitStart (CreateKit folder)
            expect KitFile.fileName "CreateKit status" None started.status
            let opened = openedOpt |> Option.defaultWith (fun () -> failwith $"{folder.value.value}: expected CreateKit to open the kit; actual no kit")
            let mutable remaining = itemSpecs |> List.map _.id
            let itemIds : ItemIdProvider =
                {
                    mintItemId = fun () ->
                        match remaining with
                        | next :: rest ->
                            remaining <- rest
                            next
                        | [] -> failwith "*.item-json: expected nine item-id requests; actual an extra request"
                }
            let mutable remainingSlots = addedSlotIds
            let elementIds : ElementIdProvider =
                {
                    mintElementId = fun () ->
                        match remainingSlots with
                        | next :: rest ->
                            remainingSlots <- rest
                            next
                        | [] -> failwith "*.setup-json: expected three added-slot id requests; actual an extra request"
                }
            let initial =
                {
                    TestAppScope.inverseModelOver context opened with itemIds = itemIds; elementIds = elementIds
                }
                |> apply [ SetNamingTemplateText labTemplate; SaveNamingTemplate ]
            let stocked = itemSpecs |> List.fold addAcceptanceItem initial
            expect (FileName "*.item-json") "unused item identities" [] remaining
            let setups = buildAcceptanceSetups stocked
            expect (FileName "*.setup-json") "unused added-slot identities" [] remainingSlots
            expect (FileName "*.setup-json") "setups including both corrections" 42 setups.kitSetups.Length
            let model = finish (fun at -> now <- at) folder setups
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
                match spec.presetOpt with
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
                match spec.presetOpt with
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

    let private assertSameBytes (expected : FileName) (actual : FileName) : unit =
        let expectedBytes = File.ReadAllBytes expected.value
        let actualBytes = File.ReadAllBytes actual.value
        expect actual $"bytes against {expected.value}" expectedBytes actualBytes

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

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``the built optics-mueller kit equals the committed kit file by file`` () : unit =
        let run = FolderName (Guid.NewGuid().ToString("N"))
        let built = buildOpticsMuellerKit run
        try
            let original = committedFolder ()
            let filesOf (folder : KitFolder) : Set<FileName> =
                relativeFiles folder.value
                |> List.filter (fun file -> not (file.value.StartsWith($".oc{Path.DirectorySeparatorChar}", StringComparison.Ordinal)))
                |> Set.ofList
            let expectedFiles = filesOf original
            let actualFiles = filesOf built.folder
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

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``AT-1 builds the optics-mueller kit's items and setups through the app's messages`` () : unit =
        let run = FolderName (Guid.NewGuid().ToString("N"))
        let built = buildOpticsMuellerKitWith (fun _ _ model -> model) run
        try
            let reader = KitProxy.create built.folder
            let metadata = reader.readKitFile () |> readKitResult KitFile.fileName
            expect KitFile.fileName "schema version" KitFile.currentSchemaVersion metadata.schemaVersion
            expect KitFile.fileName "id" kitId metadata.id
            expect KitFile.fileName "name" (KitName "optics-mueller") metadata.name
            expect KitFile.fileName "author" kitAuthor metadata.author
            expect KitFile.fileName "notes" (KitNotes "") metadata.notes
            expect KitFile.fileName "created" initialTime metadata.created
            expect KitFile.fileName "modified" initialTime metadata.modified
            expect KitFile.fileName "clock" initialTime (built.clock.now ())
            expect KitFile.fileName "naming template" labTemplate.value metadata.namingTemplate.value
            expect KitFile.fileName "import profiles" [] metadata.importProfiles
            expect KitFile.fileName "send exclusions" [] metadata.sendExclude
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
