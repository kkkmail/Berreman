namespace OpticalConstructor.Domain

open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open Berreman.Solvers
open Analytics.AnalysisFunctions
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.ExperimentRun
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.BeamLayout
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.Propagation
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.ExperimentData
open OpticalConstructor.Domain.ItemEvaluation

/// The forward model of an experiment and the study residual (spec 0050 S11.2).
module StudyForward =

    /// Why an experiment's forward model or the study residual refused (spec 0050 S11.2).
    type StudyForwardError =
        /// A mapped point the experiment's scan cannot place (spec 0050 S11.2).
        | UnpairedPointsErr of reason : string
        /// A parameter vector that does not hold one value per entry of the study's layout (spec 0050 S11.2).
        | VectorLengthErr of reason : string
        /// The setup revision the experiment references does not resolve (spec 0050 S11.2).
        | ForwardSetupErr of error : SetupError
        /// A slot's item does not resolve, or its evaluation refused (spec 0050 S11.2).
        | ForwardSlotErr of error : SlotRunError
        /// No detector slot of the setup holds a detector item to read the light (spec 0050 S11.2).
        | StudyDetectorErr of reason : string
        /// A Mueller observation whose setup holds no one object item made of one opened material (spec 0050 S11.2).
        | ObservationPlateErr of reason : string
        /// A measured row the experiment's detector does not record, such as a Mueller observation under an intensity
        /// detector (spec 0050 S11.2).
        | ReadingMismatchErr of reason : string
        /// A Mueller row whose residual cannot be formed, such as a matrix without positive throughput (spec 0050 S11.2).
        | RowResidualErr of error : InversionError

        member this.describe : string =
            match this with
            | UnpairedPointsErr reason -> $"the data cannot be placed on the scan: {reason}"
            | VectorLengthErr reason -> $"the parameter vector does not fit the study: {reason}"
            | ForwardSetupErr error -> $"the experiment's setup does not resolve: {error.describe}"
            | ForwardSlotErr error -> $"a slot cannot be evaluated: {error.describe}"
            | StudyDetectorErr reason -> $"the setup has no detector item: {reason}"
            | ObservationPlateErr reason -> $"the observation has no single plate of one opened crystal: {reason}"
            | ReadingMismatchErr reason -> $"the reading is not what the detector records: {reason}"
            | RowResidualErr error ->
                let cause =
                    match error with
                    | NonPositiveIntensityErr m00 -> $"m₀₀ is {m00}, not a positive finite throughput"
                    | DegenerateRotationErr reason -> reason
                    | MismatchedObservationsErr counts ->
                        $"{counts.measuredCount.value} measured and {counts.modelCount.value} model observation(s) do not pair"

                $"the residual cannot be formed: {cause}"

    /// What a detector records, predicted or measured: an intensity detector's signal, an ellipsometer's Ψ and Δ, or a
    /// polarimeter's absolute Mueller matrix, its m₀₀ the throughput as S9.2's readings hold it (spec 0050 S11.2).
    type RecordedReading =
        | RecordedSignal of MeasuredIntensity
        | RecordedPsiDelta of PsiDelta
        | RecordedMueller of MuellerMatrix

    /// One measured point with what fixes its prediction: the scan settings it was taken at, at its own wavelength when it
    /// carries one, or an observation carrying its own configuration (spec 0050 S11.2).
    type MeasuredRow =
        | SettingsRow of settings : ScanSetting list * waveLengthOpt : WaveLength option * measured : RecordedReading
        | ObservationRow of observation : MuellerObservation

    type MeasuredAbscissa with

        /// The abscissa as a scan value: an X,Y file writes it in the varied quantity's display unit (spec 0050 S9.2, S11.2).
        member this.toSweptValue : SweptValue = SweptValue this.value

    type MappedPoints with

        /// The points in order, each placed on `scan.quantities` by the rules `validatedFor` checks; a point the scan cannot
        /// place answers `UnpairedPointsErr` naming its number and the counts (spec 0050 S11.2; 001 §K).
        member this.measuredRowsFor (scan : ExperimentScan) : Result<MeasuredRow list, StudyForwardError> =
            let quantities = scan.quantities

            let settingOf (quantity : ScanQuantity) (value : SweptValue) : ScanSetting =
                {
                    elementId = quantity.elementId
                    variable = quantity.variable
                    value = value
                }

            // An X,Y point or a capture row holds one scan value; a dark line varies nothing (spec 0050 S9.1, S9.2).
            let oneValueSettings (number : MappedPointNumber) (value : SweptValue) : Result<ScanSetting list, StudyForwardError> =
                match quantities with
                | [] -> Ok []
                | [ quantity ] -> Ok [ settingOf quantity value ]
                | _ -> Error (UnpairedPointsErr $"point {number.value} holds one scan value, and the scan varies {quantities.Length}")

            let rows : Result<MeasuredRow, StudyForwardError> list =
                match this with
                | SeriesPoints (IntensityData series) ->
                    series.points
                    |> List.mapi (fun index point ->
                        oneValueSettings (MappedPointNumber (index + 1)) point.x.toSweptValue
                        |> Result.map (fun settings -> SettingsRow (settings, None, RecordedSignal point.intensity)))
                | SeriesPoints (EllipsometricData series) ->
                    let incidenceOpt = quantities |> List.tryFind (fun quantity -> quantity.variable = VaryR2)

                    series.points
                    |> List.map (fun point ->
                        let settings : ScanSetting list =
                            match incidenceOpt, point.aoiOpt with
                            | Some quantity, Some aoi -> [ settingOf quantity (SweptValue aoi.degrees) ]
                            | Some _, None
                            | None, _ -> []

                        let psiDelta : PsiDelta = { psi = point.psi; delta = point.delta }
                        Ok (SettingsRow (settings, Some point.waveLength, RecordedPsiDelta psiDelta)))
                | CapturePoints captures ->
                    captures
                    |> List.mapi (fun index capture ->
                        oneValueSettings (MappedPointNumber (index + 1)) (SweptValue capture.description.degrees)
                        |> Result.map (fun settings -> SettingsRow (settings, None, RecordedSignal capture.avgTotal)))
                | ObservationPoints observations -> observations |> List.map (fun keyed -> Ok (ObservationRow keyed.observation))
                | ColumnPoints mapped ->
                    mapped
                    |> List.mapi (fun index row ->
                        if row.scan.Length = quantities.Length then
                            // The i-th scan cell is the i-th quantity's; a σ is not read, a study weighs runs (001 §L).
                            let settings = List.map2 (fun (cell : ScanCell) quantity -> settingOf quantity cell.displayValue) row.scan quantities

                            let measured =
                                match row.reading with
                                | SignalReading (signal, _) -> RecordedSignal signal
                                | MuellerReading matrix -> RecordedMueller matrix

                            Ok (SettingsRow (settings, None, measured))
                        else
                            Error (UnpairedPointsErr $"point {index + 1} holds {row.scan.Length} scan value(s), and the scan varies {quantities.Length}"))

            ItemEvaluation.sequenced rows

    /// What a study's forward model reads: the kit as the study messages see it, the library, material and data stores,
    /// an item's model as the window's stores resolve it, and the solver parameters a cut plate is solved with, passed
    /// rather than defaulted as `createBerremanForward` asks (spec 0050 S11.2).
    [<ReferenceEquality>]
    type StudyForwardContext =
        {
            kit : StudyContext
            library : Library.LibraryProxy
            materials : MaterialProxy
            experimentData : ExperimentDataProxy
            choiceOf : Item -> ItemModelChoice option
            solverParameters : SolverParameters
        }

    /// What a study reads from a run: residual rows, its session's dark level, or nothing. Derived from the run's role,
    /// never stored, so it carries no tag (spec 0050 S11.2; 001 §L).
    type StudyRunUse =
        | ResidualRunUse
        | DarkLevelRunUse
        | UnreadRunUse

    type ExperimentRole with

        /// A Science, Calibration or Drift-check run gives residual rows, a Dark run its session's dark level; a
        /// Diagnostic, Ignored or Planned run is not read (spec 0050 S11.2; 001 §L).
        member this.studyUse : StudyRunUse =
            match this with
            | ScienceRun
            | CalibrationRun
            | DriftCheckRun -> ResidualRunUse
            | DarkRun -> DarkLevelRunUse
            | DiagnosticRun
            | IgnoredRun
            | PlannedRun -> UnreadRunUse

    type Study with

        /// The study's enabled experiments whose role gives residual rows, as `experiments` holds them, in study order
        /// (spec 0050 S11.2).
        member this.fittedRunsIn (experiments : Experiment list) : Experiment list =
            this.enabledExperiments
            |> List.choose (fun run -> experiments |> List.tryFind (fun experiment -> experiment.name = run))
            |> List.filter (fun experiment -> experiment.role.studyUse = ResidualRunUse)

    /// One value of a study's parameter vector: the unknown and the owner holding that value (spec 0050 S11.2).
    type StudyVectorEntry =
        {
            unknown : StudyUnknown
            owner : UnknownOwner
        }

    /// A place in a study's parameter vector, counted from 0; elevated because ELEV010 refuses an `int` beside
    /// `ParameterValue` (spec 0050 S11.2).
    type StudyVectorIndex =
        | StudyVectorIndex of int

        member this.value : int = let (StudyVectorIndex index) = this in index

    /// The values a fit searches, one per entry of the study's layout, in its order (spec 0050 S11.2).
    [<Softellect.Sys.NumericCarrier("the study's parameter vector: built whole for every residual evaluation and handed whole to the fit")>]
    type StudyParameterVector =
        | StudyParameterVector of float[]

        member this.value : float[] = let (StudyParameterVector values) = this in values

        /// The vector of these values, in order (spec 0050 S11.2).
        static member ofParameterValues (values : ParameterValue list) : StudyParameterVector =
            values |> List.map (fun value -> value.value) |> Array.ofList |> StudyParameterVector

        /// The value at that place; the caller has checked the vector holds one value per entry (spec 0050 S11.2).
        member this.valueAt (index : StudyVectorIndex) : ParameterValue = ParameterValue this.value.[index.value]

    /// How a study's parameter vector is laid out: one entry per owner of each unknown, in the unknowns' order, and the
    /// sessions a session owner names (spec 0050 S11.2; 001 §L, §N).
    type StudyParameterLayout =
        {
            entries : StudyVectorEntry list
            sessions : StudySession list
        }

        /// The vector a fit starts from: each entry's unknown's start, in order (spec 0050 S11.2).
        member this.startVector : StudyParameterVector =
            this.entries |> List.map (fun entry -> entry.unknown.unknown.start) |> StudyParameterVector.ofParameterValues

    type Study with

        /// The layout of the vector a fit of the study searches: for each of its unknowns over the items its experiments
        /// use, one entry per owner, the users being the fitted runs whose slots hold the unknown's item, or an item whose
        /// sample is made of the unknown's opened material; no bound, scaling or option (spec 0050 S11.2, S11.3).
        member this.parameterLayoutWith (context : StudyForwardContext) : StudyParameterLayout =
            let kit = context.kit
            let sessions = this.sessionsIn kit.experiments
            let fitted = this.fittedRunsIn kit.experiments

            // A run whose setup does not resolve holds nothing (spec 0050 S8.2).
            let heldIds (run : Experiment) : ItemId list =
                match run.slotsIn kit.setups with
                | Ok slots -> slots |> List.choose (fun slot -> slot.content.itemOpt)
                | Error _ -> []

            let madeOf (version : MaterialVersionId) (item : Item) : bool =
                match context.choiceOf item with
                | Some (StackSampleChoice (_, _, sample)) -> sample.openedMaterialsIn this |> List.exists (fun material -> material.version = version)
                | Some (SourcePresetChoice _)
                | Some (PolarizerPresetChoice _)
                | Some (LibraryModelChoice _)
                | None -> false

            let usedBy (run : Experiment) (parameter : StudyParameterRef) : bool =
                let held = heldIds run

                match parameter with
                | ItemParameterRef (item, _) -> List.contains item held
                | MaterialParameterRef (version, _) ->
                    kit.items |> List.exists (fun item -> List.contains item.id held && madeOf version item)

            let entries : StudyVectorEntry list =
                this.unknownsWith (kit.itemsUsedBy this)
                |> List.collect (fun unknown ->
                    let users = fitted |> List.filter (fun run -> usedBy run unknown.parameter) |> List.map (fun run -> run.name)
                    unknown.owners sessions users |> List.map (fun owner -> ({ unknown = unknown; owner = owner } : StudyVectorEntry)))

            { entries = entries; sessions = sessions }

    /// The numbers one experiment is evaluated with at one parameter vector: each item's values and each opened
    /// material's crystal, which every sample made of it shares (spec 0050 S11.2; 001 §I, §L).
    type ExperimentAssignment =
        {
            itemValues : Map<ItemId, ParameterValues>
            crystals : Map<MaterialVersionId, CrystalParameters>
        }

        /// The item's assigned values, or its own for an item the assignment lacks (spec 0050 S11.2).
        member this.valuesOf (item : Item) : ParameterValues =
            match this.itemValues |> Map.tryFind item.id with
            | Some values -> values
            | None -> ParameterValues.ofParameters item.parameters

        /// The opened material's assigned crystal, or its working copy's current one (spec 0050 S11.2).
        member this.crystalOf (material : OpenedMaterial) : CrystalParameters =
            match this.crystals |> Map.tryFind material.version with
            | Some crystal -> crystal
            | None -> material.currentCrystal

        /// `materials` with an assigned version resolving to the store's entry over the assigned crystal, so a stack sample
        /// made of an opened material is solved with the study's working copy; all else passes through (spec 0050 S11.2).
        member this.materialProxyOver (materials : MaterialProxy) : MaterialProxy =
            let crystals = this.crystals

            {
                materials with
                    resolveVersion =
                        fun version ->
                            match crystals |> Map.tryFind version with
                            | Some crystal ->
                                materials.resolveVersion version
                                |> Result.map (Option.map (fun (entry : MaterialEntry) -> { entry with physics = CrystalParameterPhysics crystal }))
                            | None -> materials.resolveVersion version
            }

    type StudyParameterLayout with

        /// The experiment's assignment at `vector`: each of `items` and each opened material takes the value of every entry
        /// whose owner holds the experiment, else its own, then each link's follower its leader's value as assigned; refused
        /// when the vector does not hold one value per entry (spec 0050 S11.2; 001 §L).
        member this.assignmentFor
            (study : Study)
            (items : Item list)
            (experiment : ExperimentName)
            (vector : StudyParameterVector)
            : Result<ExperimentAssignment, StudyForwardError> =
            let length = vector.value.Length
            let count = this.entries.Length

            if length <> count then
                Error (VectorLengthErr $"it holds {length} value(s), and the layout has {count} entry(ies)")
            else
                let holds (owner : UnknownOwner) : bool =
                    match owner with
                    | ItemOwner _
                    | MaterialOwner _ -> true
                    | SessionOwner session ->
                        this.sessions |> List.exists (fun block -> block.name = session && List.contains experiment block.experiments)
                    | ExperimentOwner run -> run = experiment

                let held : (StudyParameterRef * ParameterValue) list =
                    this.entries
                    |> List.mapi (fun k entry -> entry, vector.valueAt (StudyVectorIndex k))
                    |> List.filter (fun (entry, _) -> holds entry.owner)
                    |> List.map (fun (entry, value) -> entry.unknown.parameter, value)

                let writtenComponent (name : ParameterName) (value : ParameterValue) (crystal : CrystalParameters) : CrystalParameters =
                    match crystal.parameterAxes |> List.tryFind (fun axis -> axis.name = name) with
                    | Some axis -> axis.writeValue value crystal
                    | None -> crystal

                let itemValuesOf (item : Item) : ParameterValues =
                    held
                    |> List.fold (fun (values : ParameterValues) (parameter, value) ->
                        match parameter with
                        | ItemParameterRef (id, name) when id = item.id -> values.withValue name value
                        | ItemParameterRef _
                        | MaterialParameterRef _ -> values) (ParameterValues.ofParameters item.parameters)

                let crystalOf (material : OpenedMaterial) : CrystalParameters =
                    held
                    |> List.fold (fun (crystal : CrystalParameters) (parameter, value) ->
                        match parameter with
                        | MaterialParameterRef (version, name) when version = material.version -> writtenComponent name value crystal
                        | MaterialParameterRef _
                        | ItemParameterRef _ -> crystal) material.currentCrystal

                let assigned : ExperimentAssignment =
                    {
                        itemValues = items |> List.map (fun item -> item.id, itemValuesOf item) |> Map.ofList
                        crystals = study.materials |> List.map (fun material -> material.version, crystalOf material) |> Map.ofList
                    }

                // A leader never follows (`withLink` refuses a chain), so each follower reads its leader once; a link whose
                // leader or follower the assignment does not hold changes nothing (spec 0050 S11.1, S11.2).
                let leaderValueOpt (assignment : ExperimentAssignment) (leader : StudyParameterRef) : ParameterValue option =
                    match leader with
                    | ItemParameterRef (item, name) ->
                        assignment.itemValues |> Map.tryFind item |> Option.bind (fun values -> values.valueOf name |> Result.toOption)
                    | MaterialParameterRef (version, name) ->
                        assignment.crystals
                        |> Map.tryFind version
                        |> Option.bind (fun crystal ->
                            crystal.parameterAxes |> List.tryFind (fun axis -> axis.name = name) |> Option.map (fun axis -> axis.parameterValueOf crystal))

                let followed (assignment : ExperimentAssignment) (link : ParameterLink) : ExperimentAssignment =
                    match leaderValueOpt assignment link.leader, link.follower with
                    | Some value, ItemParameterRef (item, name) ->
                        match assignment.itemValues |> Map.tryFind item with
                        | Some values -> { assignment with itemValues = assignment.itemValues |> Map.add item (values.withValue name value) }
                        | None -> assignment
                    | Some value, MaterialParameterRef (version, name) ->
                        match assignment.crystals |> Map.tryFind version with
                        | Some crystal -> { assignment with crystals = assignment.crystals |> Map.add version (writtenComponent name value crystal) }
                        | None -> assignment
                    | None, _ -> assignment

                Ok (study.links |> List.fold followed assigned)

    type ParameterValue with

        /// The number as a dark level in the detector's own units, a `ReadingParameter`'s (spec 0050 S11.2; step 207).
        member this.darkMean : MuellerReconstruction.DarkMean =
            let (ParameterValue level) = this
            MuellerReconstruction.DarkMean level

    /// How the detector item reads the light reaching it: an intensity detector S₀ through its gain and dark level, a
    /// polarimeter the path's Mueller matrix, an ellipsometer Ψ and Δ; no noise is simulated (spec 0050 S11.2).
    type DetectorReadout =
        | IntensityDetectorReadout of gain : ParameterValue * dark : MuellerReconstruction.DarkMean
        | PolarimeterDetectorReadout
        | EllipsometerDetectorReadout

    /// One run element on the detector's path, bound over the assignment: its drawn slot, the branch the light leaves it
    /// by, its drawn incidence and its item's dial and dial zero (spec 0050 S11.2).
    type BoundPathStep =
        {
            slot : SetupSlot
            element : BoundOpticalElement
            branch : Propagation.Branch
            drawnIncidence : IncidenceAngle
            dial : ItemDial
            dialZero : Angle
        }

    /// An experiment bound at one parameter vector: its source's emission and operating wavelength, the run elements on
    /// its detector's path in light order, the detector item and its readout, and the crystal and solver parameters its
    /// Mueller observations are predicted with (spec 0050 S11.2).
    type BoundExperiment =
        {
            experiment : Experiment
            emission : SourceEmission
            operatingWaveLength : WaveLength
            path : BoundPathStep list
            detectorItem : ItemId
            readout : DetectorReadout
            observedCrystalOpt : CrystalParameters option
            solverParameters : SolverParameters
        }

    type Experiment with

        /// The setup revision drawn, its slots run over `assignment`, read by the first detector item in light order (dark
        /// level `darkOpt` else its own), observing the assigned crystal of a lone object item made of one material `study`
        /// opened; an unresolved setup or slot item, or no detector item, is refused (spec 0050 S11.2; 001 §K, §N).
        member this.boundWith
            (context : StudyForwardContext)
            (study : Study)
            (assignment : ExperimentAssignment)
            (darkOpt : MuellerReconstruction.DarkMean option)
            : Result<BoundExperiment, StudyForwardError> =
            let items = context.kit.items

            let param : SlotRunParam =
                {
                    library = context.library
                    materials = assignment.materialProxyOver context.materials
                    items = items
                    choiceOf = context.choiceOf
                    valuesOf = assignment.valuesOf
                }

            let intensityReadoutOf (detector : Item) : Result<DetectorReadout, StudyForwardError> =
                let values = param.valuesOf detector

                let dark : Result<MuellerReconstruction.DarkMean, ItemEvaluationError> =
                    match darkOpt with
                    | Some level -> Ok level
                    | None -> values.valueOf ParameterName.darkOffset |> Result.map (fun offset -> offset.darkMean)

                values.valueOf ParameterName.gain
                |> Result.bind (fun gain -> dark |> Result.map (fun level -> IntensityDetectorReadout (gain, level)))
                |> Result.mapError (SlotEvaluationErr >> ForwardSlotErr)

            // A detector slot reads only through an item of a detector form (spec 0050 S8.2, S11.2; step 208).
            let detectorOf (roled : RoledSlot) : (SetupSlot * Item * Result<DetectorReadout, StudyForwardError>) option =
                let heldOpt : (Item * ItemModelChoice) option =
                    match roled.role, roled.slot.content.itemOpt with
                    | DetectorSlotRole _, Some itemId ->
                        items
                        |> List.tryFind (fun item -> item.id = itemId)
                        |> Option.bind (fun item -> context.choiceOf item |> Option.map (fun choice -> item, choice))
                    | DetectorSlotRole _, None
                    | (SourceSlotRole | PrepSlotRole _ | ObjectSlotRole _ | AnalyzerSlotRole _), _ -> None

                match heldOpt with
                | Some (item, LibraryModelChoice model) ->
                    match model.entry.model with
                    | IntensityDetectorModel _ -> Some (roled.slot, item, intensityReadoutOf item)
                    | MuellerPolarimeterModel _ -> Some (roled.slot, item, Ok PolarimeterDetectorReadout)
                    | EllipsometerModel _ -> Some (roled.slot, item, Ok EllipsometerDetectorReadout)
                    | SourcePresetItemModel
                    | IdealPolarizerItemModel
                    | StackSampleItemModel
                    | OpticalElementModel _
                    | EmptySlotModel -> None
                | Some (_, (SourcePresetChoice _ | PolarizerPresetChoice _ | StackSampleChoice _))
                | None -> None

            // A fixed item, or a slot holding none, turns on no dial (spec 0050 S8.2, S11.2).
            let dialOf (slot : SetupSlot) : Result<ItemDial * Angle, StudyForwardError> =
                match slot.content.itemOpt with
                | None -> Ok (FixedMounting, Angle.zero)
                | Some itemId ->
                    param.resolveSlotItem itemId
                    |> Result.mapError (SlotItemErr >> ForwardSlotErr)
                    |> Result.bind (fun (item, _) ->
                        match item.dial with
                        | FixedMounting -> Ok (FixedMounting, Angle.zero)
                        | DialMounted _ ->
                            (param.valuesOf item).valueOf ParameterName.dialZero
                            |> Result.map (fun zero -> item.dial, zero.angle)
                            |> Result.mapError (SlotEvaluationErr >> ForwardSlotErr))

            // An observation is a plate of one crystal: the lone object item's stack sample, made of one opened material,
            // at its assigned crystal (spec 0050 S11.2; 001 §K).
            let observedCrystalIn (slots : SetupSlot list) : CrystalParameters option =
                let objectItems : ItemId list =
                    SetupSlot.inLightOrder slots
                    |> List.choose (fun roled ->
                        match roled.role with
                        | ObjectSlotRole _ -> roled.slot.content.itemOpt
                        | SourceSlotRole
                        | PrepSlotRole _
                        | AnalyzerSlotRole _
                        | DetectorSlotRole _ -> None)

                match objectItems with
                | [ itemId ] ->
                    match items |> List.tryFind (fun item -> item.id = itemId) |> Option.bind context.choiceOf with
                    | Some (StackSampleChoice (_, _, sample)) ->
                        match sample.openedMaterialsIn study with
                        | [ material ] -> Some (assignment.crystalOf material)
                        | _ -> None
                    | Some (SourcePresetChoice _ | PolarizerPresetChoice _ | LibraryModelChoice _)
                    | None -> None
                | _ -> None

            this.slotsIn context.kit.setups
            |> Result.mapError ForwardSetupErr
            |> Result.bind (fun recorded ->
                let slots = recorded |> List.map (fun slot -> slot.drawnWith items)
                let layout = slots |> List.map (fun slot -> slot.layoutElement) |> BeamLayout.layout

                let placedOf (id : Library.ElementId) : (SetupSlot * BeamLayout.PlacedElement) option =
                    slots
                    |> List.tryFind (fun slot -> slot.id = id)
                    |> Option.bind (fun slot ->
                        layout.elements |> List.tryFind (fun placed -> placed.id = id) |> Option.map (fun placed -> slot, placed))

                // A source, detector, mirror or lens, or an unbound slot, is no run element (spec 0050 S11.2; step 368).
                let boundStepOf (step : BeamLayout.PathStep) : Result<BoundPathStep option, StudyForwardError> =
                    match placedOf step.element with
                    | None -> Ok None
                    | Some (slot, placed) ->
                        slot.runElementWith param
                        |> Result.mapError ForwardSlotErr
                        |> Result.bind (fun elementOpt ->
                            match elementOpt with
                            | None -> Ok None
                            | Some element ->
                                dialOf slot
                                |> Result.map (fun (dial, dialZero) ->
                                    let bound : BoundPathStep =
                                        {
                                            slot = slot
                                            element = element
                                            branch = step.branch.propagationBranch
                                            drawnIncidence = placed.incidenceOn slot.placement
                                            dial = dial
                                            dialZero = dialZero
                                        }

                                    Some bound))

                SetupSlot.runEmissionOf slots param
                |> Result.mapError ForwardSlotErr
                |> Result.bind (fun emission ->
                    match SetupSlot.inLightOrder slots |> List.tryPick detectorOf with
                    | None -> Error (StudyDetectorErr $"{this.setup.describe}, which {this.name.value} references")
                    | Some (detector, detectorItem, readoutResult) ->
                        let path : BeamLayout.PathStep list =
                            match placedOf detector.id with
                            | Some (_, placed) -> placed.path
                            | None -> []

                        readoutResult
                        |> Result.bind (fun readout ->
                            path
                            |> List.map boundStepOf
                            |> ItemEvaluation.sequenced
                            |> Result.map (fun steps ->
                                let source = SetupSlot.boundSourceIn slots context.library

                                let bound : BoundExperiment =
                                    {
                                        experiment = this
                                        emission = emission
                                        operatingWaveLength = Library.SourcePreset.operatingWaveLengthOf source this.scan.sourceWaveLengthOpt
                                        path = List.choose id steps
                                        detectorItem = detectorItem.id
                                        readout = readout
                                        observedCrystalOpt = observedCrystalIn slots
                                        solverParameters = context.solverParameters
                                    }

                                bound))))

    type BoundExperiment with

        /// The detector's predicted reading at the point's settings, at `waveLengthOpt` else the point's wavelength else
        /// the operating one: S₀ · gain + dark, Ψ and Δ, or the path's Mueller matrix (spec 0050 S11.2; 001 §N).
        member this.readingAt (settings : ScanSetting list) (waveLengthOpt : WaveLength option) : Result<RecordedReading, StudyForwardError> =
            let degreesOf (value : SweptValue) : Angle = Angle.degree (value.value * 1.0<deg>)

            let settingOn (step : BoundPathStep) (variable : VariableParameter) : ScanSetting option =
                settings |> List.tryFindBack (fun setting -> setting.elementId = step.slot.id && setting.variable = variable)

            let w : WaveLength =
                match waveLengthOpt, settings |> List.tryFindBack (fun setting -> setting.variable = VaryWaveLength) with
                | Some requested, _ -> requested
                | None, Some setting -> Units.toWaveLength Units.Nanometer setting.value.toUnitMagnitude
                | None, None -> this.operatingWaveLength

            // A raw reading turns its slot as `sign · (reading − zero)`, a fixed mounting as the reading (spec 0050 S11.2; step 255).
            let throughDial (step : BoundPathStep) (reading : Angle) : Angle =
                match step.dial with
                | DialMounted sign -> sign.physicalAngleOf step.dialZero reading
                | FixedMounting -> reading

            let rotationOf (step : BoundPathStep) : Angle =
                match settingOn step VaryR1, settingOn step VaryDialReading, step.slot.content with
                | Some setting, _, _ -> degreesOf setting.value
                | None, Some setting, _ -> throughDial step (degreesOf setting.value)
                | None, None, ItemInSlot (_, DialTurn reading) -> throughDial step reading
                | None, None, ItemInSlot (_, PlacementTurn)
                | None, None, EmptySlotContent -> step.slot.placement.r1

            let incidenceOf (step : BoundPathStep) : IncidenceAngle =
                match settingOn step VaryR2 with
                | Some setting -> IncidenceAngle.create (degreesOf setting.value)
                | None -> step.drawnIncidence

            this.path
            |> List.map (fun step ->
                let orientation : ElementOrientation = { rotation = rotationOf step; incidence = incidenceOf step; branch = step.branch }
                step.element.muellerAt orientation w |> Result.map step.slot.runStepOf)
            |> ItemEvaluation.sequenced
            |> Result.mapError (SlotEvaluationErr >> ForwardSlotErr)
            |> Result.map (fun steps ->
                // S₀ and Ψ, Δ of the arriving state, the readings `ExperimentRun.read` projects (spec 0050 S11.2; step 252).
                match this.readout with
                | IntensityDetectorReadout (gain, dark) ->
                    let s0 = (detectorStokes this.emission steps).intensity
                    RecordedSignal (MeasuredIntensity (s0.value * gain.value + dark.value))
                | EllipsometerDetectorReadout -> RecordedPsiDelta (detectorStokes this.emission steps).psiDelta
                | PolarimeterDetectorReadout -> RecordedMueller (RunStep.productOf steps))

    type MeasurementConfiguration with

        /// The configuration as a cut plate's orientation, its azimuth the rotation and its observable the branch, so
        /// `sampleAzimuth` and `observable` give its own back (spec 0050 S11.2; 001 §N).
        member this.elementOrientation : ElementOrientation =
            let branch : Propagation.Branch =
                match this.observable with
                | TransmittedMueller -> Propagation.BranchTransmitted
                | ReflectedMueller -> Propagation.BranchReflected

            { rotation = this.azimuth.angle; incidence = this.incidenceAngle; branch = branch }

    type BoundExperiment with

        /// The detector's predicted reading of a measured row: a settings row's by `readingAt`, a Mueller observation's as
        /// the cut plate of the observed crystal through the spec 0044 forward at its own configuration (spec 0050 S11.2;
        /// 001 §K, §N).
        member this.predictedReadingOf (row : MeasuredRow) : Result<RecordedReading, StudyForwardError> =
            match row with
            | SettingsRow (settings, waveLengthOpt, _) -> this.readingAt settings waveLengthOpt
            | ObservationRow observation ->
                let configuration = observation.configuration
                let name = this.experiment.name.value

                match this.readout, this.observedCrystalOpt with
                | PolarimeterDetectorReadout, Some crystal ->
                    let forward = createBerremanForward (fun (c : CrystalParameters) -> c.opticalProperties) this.solverParameters
                    let plate = BoundCutPlate (CutPlate.ofForward forward crystal configuration.cut configuration.thickness)

                    plate.muellerAt configuration.elementOrientation configuration.waveLength
                    |> Result.map RecordedMueller
                    |> Result.mapError (SlotEvaluationErr >> ForwardSlotErr)
                | PolarimeterDetectorReadout, None -> Error (ObservationPlateErr $"{this.experiment.setup.describe}, which {name} references")
                | IntensityDetectorReadout _, _ -> Error (ReadingMismatchErr $"{name} is read by an intensity detector, which records no Mueller matrix")
                | EllipsometerDetectorReadout, _ -> Error (ReadingMismatchErr $"{name} is read by an ellipsometer, which records no Mueller matrix")

    /// One measured row's residual entries, the prediction less the measurement, with a Mueller row's log-throughput
    /// ratio kept apart from its 15 normalized differences (spec 0050 S11.2; 001 §K).
    type RowResidual =
        {
            elements : ResidualVector
            throughputOpt : ResidualVector option
        }

    type RecordedReading with

        /// This prediction's residual against its row's measured reading: a signal's difference, Ψ's and Δ's, Δ's taken
        /// on the circle in radians, or a Mueller matrix's `observationResidual` and `throughputResidual` (spec 0050 S11.2).
        member this.residualAgainst (measured : RecordedReading) : Result<RowResidual, StudyForwardError> =
            let kindOf (reading : RecordedReading) : string =
                match reading with
                | RecordedSignal _ -> "a signal"
                | RecordedPsiDelta _ -> "Ψ and Δ"
                | RecordedMueller _ -> "a Mueller matrix"

            match this, measured with
            | RecordedSignal predicted, RecordedSignal recorded ->
                let elements : ResidualVector = ResidualVector [| predicted.value - recorded.value |]
                Ok { elements = elements; throughputOpt = None }
            | RecordedPsiDelta predicted, RecordedPsiDelta recorded ->
                // A Δ near ±180° on both sides is a small difference, not a 2π jump.
                let elements : ResidualVector =
                    ResidualVector
                        [|
                            predicted.psi.value - recorded.psi.value
                            System.Math.IEEERemainder (predicted.delta.value - recorded.delta.value, 2.0 * System.Math.PI)
                        |]

                Ok { elements = elements; throughputOpt = None }
            | RecordedMueller predicted, RecordedMueller recorded ->
                observationResidual recorded predicted
                |> Result.bind (fun elements ->
                    throughputResidual recorded predicted
                    |> Result.map (fun throughput -> { elements = elements; throughputOpt = Some throughput }))
                |> Result.mapError RowResidualErr
            | RecordedSignal _, _
            | RecordedPsiDelta _, _
            | RecordedMueller _, _ ->
                Error (ReadingMismatchErr $"the prediction is {kindOf this}, and the measured reading {kindOf measured}")
