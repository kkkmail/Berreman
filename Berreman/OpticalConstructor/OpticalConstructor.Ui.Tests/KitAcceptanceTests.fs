namespace OpticalConstructor.Ui.Tests

open System
open System.IO
open Xunit
open Softellect.Sys.Primitives
open Berreman.Constants
open Berreman.Geometry
open BerremanTestSupport
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.DataSplits
open OpticalConstructor.Domain.Experiments
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
open OpticalConstructor.Domain.WorkbenchSettings
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
    let private labTemplate = ControlCaption "{source:LP1=LP,CPL1=CPL}-({obj1}#{phi1}-{obj2}#{phi2})-{analyzer:LP2=LP,CPL2=CPL}-{note}"
    let private sourceSlot = ElementId "src"
    let private firstObjectSlot = ElementId "sample"
    let private detectorSlot = ElementId "det"
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
    let private calibrationNames : ExperimentName list =
        [
            ExperimentName "LP-(AIR#0)-LP-2"
            ExperimentName "LP-(AIR#0)-LP-2R"
            ExperimentName "LP-(AIR#0)-CPL-2"
            ExperimentName "LP-(AIR#0)-CPL-2R"
            ExperimentName "CPL-(AIR#0)-LP-2"
            ExperimentName "CPL-(AIR#0)-LP-2R"
            ExperimentName "CPL-(AIR#0)-CPL"
            ExperimentName "CPL-(AIR#0)-CPL-BR"
            ExperimentName "CPL-(AIR#0)-CPL-2"
        ]
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

    /// Build through application messages, keeping the setup-only checkpoint available to AT-1 (spec 0050 S12.1).
    let private buildOpticsMuellerKitWith
        (finish : KitFolder -> TableAndElementRotationModel -> TableAndElementRotationModel)
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
            let initial =
                {
                    TestAppScope.inverseModelOver context opened with itemIds = itemIds
                }
                |> apply [ SetNamingTemplateText labTemplate; SaveNamingTemplate ]
            let stocked = itemSpecs |> List.fold addAcceptanceItem initial
            expect (FileName "*.item-json") "unused item identities" [] remaining
            let setups = buildAcceptanceSetups stocked
            expect (FileName "*.setup-json") "setups including both corrections" 42 setups.kitSetups.Length
            let model = finish folder setups
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
        buildOpticsMuellerKitWith buildAcceptanceExperiments run

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

    [<Fact>]
    [<Trait("Category", "kit-acceptance")>]
    let ``AT-1 builds the optics-mueller kit's items and setups through the app's messages`` () : unit =
        let run = FolderName (Guid.NewGuid().ToString("N"))
        let built = buildOpticsMuellerKitWith (fun _ model -> model) run
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
        let built = buildOpticsMuellerKit run
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
