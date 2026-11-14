namespace OpticalConstructor.Optimization

open System.Threading
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.StudyForward
open OpticalConstructor.Domain.FitRuns
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.MuellerFitting
open OpticalConstructor.Optimization.BenchNoise
open OpticalConstructor.Optimization.StagedMuellerPath

/// The study engine: its automatic choice between the linear path (S10.3's staged Mueller path) and the nonlinear one
/// (the multi-start study residual), and the fit (spec 0050 S11.3; 001 §L).
module StudyEngine =

    /// Why a study fit refused (spec 0050 S11.3).
    type StudyFitError =
        /// The study's forward model, its pre-fit or its data refused (spec 0050 S11.3).
        | StudyForwardFitErr of error : StudyForwardError
        /// The study could not be staged for the fit (spec 0050 S11.3).
        | FitStageErr of error : FitRunError
        /// A fitted run's session takes its gain from AIR runs, which the linear engine derives (spec 0050 S11.3).
        | SessionGainNeedsLinearEngineErr of reason : string
        /// Every multi-start member of the nonlinear fit failed (spec 0050 S11.3).
        | NoConvergedMemberErr of reason : string
        /// The fit's watch asked it to stop (spec 0050 S11.3).
        | FitCancelledErr of reason : string
        /// S10.3's staged Mueller path refused the linear engine's rows (spec 0050 S11.3).
        | StagedPathFitErr of error : StagedMuellerError

        member this.describe : string =
            match this with
            | StudyForwardFitErr error -> $"the study's model or data refused the fit: {error.describe}"
            | FitStageErr error -> $"the stage refused the fit: {error.describe}"
            | SessionGainNeedsLinearEngineErr reason -> $"the nonlinear engine fits a session's gain as its detector's parameter: {reason}"
            | NoConvergedMemberErr reason -> $"no start of the fit converged: {reason}"
            | FitCancelledErr reason -> $"the fit was cancelled: {reason}"
            | StagedPathFitErr error -> $"the staged Mueller path refused the fit: {error.describe}"

    /// The reference families' two polarizers, linear or LP-then-retarder circular; derived, never stored, so it carries
    /// no tag (spec 0050 S11.3; `MuellerReconstruction.Family`).
    type CalibrationPolarizer =
        | LinearCalibrationPolarizer
        | CircularCalibrationPolarizer

        /// The family this preparing polarizer makes with `analyzer` (spec 0050 S11.3).
        member this.familyWith (analyzer : CalibrationPolarizer) : Family =
            match this, analyzer with
            | LinearCalibrationPolarizer, LinearCalibrationPolarizer -> LpLp
            | LinearCalibrationPolarizer, CircularCalibrationPolarizer -> LpCpl
            | CircularCalibrationPolarizer, LinearCalibrationPolarizer -> CplLp
            | CircularCalibrationPolarizer, CircularCalibrationPolarizer -> CplCpl

    // The built-in circular polarizer's retarder: its second component, at 45° within it (spec 0050 S8.1, step 210).
    let private retarderComponent : ParameterNameIndex = ParameterNameIndex 2

    let private retarderComponentOffset : Angle = Angle.degree 45.0<Berreman.Constants.deg>

    type ItemModelChoice with

        /// The reference-family polarizer the choice is: the ideal-LP preset or a real linear polarizer is linear, the
        /// built-in LP-then-retarder compound circular, anything else none; for calibration only (spec 0050 S11.3).
        member this.calibrationPolarizerOpt : CalibrationPolarizer option =
            match this with
            | PolarizerPresetChoice (_, polarizer) ->
                match polarizer.behavior with
                | Library.ComputedIdeal Library.IdealLinear -> Some LinearCalibrationPolarizer
                | Library.ComputedIdeal (Library.IdealCircularLeft | Library.IdealCircularRight)
                | Library.ConstantMueller _ -> None
            | LibraryModelChoice model ->
                match model.entry.model with
                | OpticalElementModel (RealPolarizerModel _) -> Some LinearCalibrationPolarizer
                | OpticalElementModel (CompoundModel [ { CompoundComponent.form = RealPolarizerModel _; offset = polarizerOffset }
                                                       { CompoundComponent.form = RetarderPlateModel _; offset = retarderOffset } ])
                    when polarizerOffset = Angle.zero && retarderOffset = retarderComponentOffset -> Some CircularCalibrationPolarizer
                | OpticalElementModel (RetarderPlateModel _ | BlackBoxMuellerModel _ | CompoundModel _)
                | SourcePresetItemModel
                | IdealPolarizerItemModel
                | StackSampleItemModel
                | EmptySlotModel
                | IntensityDetectorModel _
                | MuellerPolarimeterModel _
                | EllipsometerModel _ -> None
            | SourcePresetChoice _
            | StackSampleChoice _ -> None

    // Declared ahead of `calibrateLinear` because the stage-1 constants below read them (spec 0050 S11.3).
    type Angle with

        /// The angle in radians as a parameter's number; `ParameterValue.angle` is the reading the other way (spec 0050
        /// S11.3).
        member this.parameterValue : ParameterValue = ParameterValue this.value

    type Retardance with

        /// The retardance in radians as a parameter's number (spec 0050 S11.3).
        member this.parameterValue : ParameterValue = ParameterValue this.value

    /// One stage-1 constant: the parameter it gives and its value in stage one's answer (spec 0050 S11.3).
    type private StageOneConstant =
        {
            constantParameter : StudyParameterRef
            constantIn : StageOneCalibrationData -> ParameterValue
        }

    /// A Calibration run's reference family and the items preparing and analyzing it (spec 0050 S11.3).
    type private CalibrationRunFamily =
        {
            runFamily : Family
            prepItem : ItemId
            analyzerItem : ItemId
        }

        /// The parameters stage one fits on a run of this family and the reference stage-1 constant each takes; the CPL
        /// source's retarder sits at 45° plus its offset (spec 0050 S11.3; `StagedMuellerPath.calibrateStageOne`).
        member this.stageOneConstants : StageOneConstant list =
            let retardance = ParameterName.retardance.inComponent retarderComponent

            match this.runFamily with
            | LpLp ->
                [ { constantParameter = ItemParameterRef (this.analyzerItem, ParameterName.dialZero); constantIn = fun stageOne -> stageOne.lpZero.parameterValue } ]
            | LpCpl ->
                [
                    { constantParameter = ItemParameterRef (this.analyzerItem, ParameterName.dialZero); constantIn = fun stageOne -> stageOne.cplZero.parameterValue }
                    {
                        constantParameter = ItemParameterRef (this.analyzerItem, retardance)
                        constantIn = fun stageOne -> stageOne.cplAnalyzerRetardance.parameterValue
                    }
                ]
            | CplLp ->
                [
                    {
                        constantParameter = ItemParameterRef (this.prepItem, ParameterName.fastAxisOffset.inComponent retarderComponent)
                        constantIn = fun stageOne -> (stageOne.cplSourceModel.thetaRel - retarderComponentOffset).parameterValue
                    }
                    {
                        constantParameter = ItemParameterRef (this.prepItem, retardance)
                        constantIn = fun stageOne -> stageOne.cplSourceModel.retardance.parameterValue
                    }
                ]
            | CplCpl -> []

    /// A run's held items in light order with their models among `items`; `None` when its setup, an item or a model does
    /// not resolve (spec 0050 S11.3).
    let private heldItemsIn (context : StudyForwardContext) (items : Item list) (run : Experiment) : (SlotRole * Item * ItemModelChoice) list option =
        match run.slotsIn context.kit.setups with
        | Error _ -> None
        | Ok slots ->
            let roledItems = SetupSlot.inLightOrder slots |> List.choose (fun roled -> roled.slot.content.itemOpt |> Option.map (fun id -> roled.role, id))

            let resolved =
                roledItems
                |> List.choose (fun (role, id) ->
                    items
                    |> List.tryFind (fun item -> item.id = id)
                    |> Option.bind (fun item -> context.choiceOf item |> Option.map (fun model -> role, item, model)))

            if resolved.Length = roledItems.Length then Some resolved else None

    /// The staged study's fitted runs that resolve, in study order, each with its held items (spec 0050 S11.3).
    let private fittedHeldIn (staged : StagedStudy) (context : StudyForwardContext) : (ExperimentName * (SlotRole * Item * ItemModelChoice) list) list =
        staged.fittedRuns
        |> List.choose (fun run ->
            context.kit.experiments
            |> List.tryFind (fun experiment -> experiment.name = run)
            |> Option.bind (heldItemsIn context staged.items)
            |> Option.map (fun held -> run, held))

    /// An AIR run's family: no object but an empty-slot item, one reference polarizer preparing and one analyzing (spec
    /// 0050 S11.3).
    let private calibrationRunOf (held : (SlotRole * Item * ItemModelChoice) list) : CalibrationRunFamily option =
        let objectsEmpty =
            held
            |> List.forall (fun (role, _, model) ->
                match role, model with
                | ObjectSlotRole _, LibraryModelChoice version ->
                    match version.entry.model with
                    | EmptySlotModel -> true
                    | _ -> false
                | ObjectSlotRole _, _ -> false
                | (SourceSlotRole | PrepSlotRole _ | AnalyzerSlotRole _ | DetectorSlotRole _), _ -> true)

        let preps = held |> List.filter (fun (role, _, _) -> match role with PrepSlotRole _ -> true | _ -> false)
        let analyzers = held |> List.filter (fun (role, _, _) -> match role with AnalyzerSlotRole _ -> true | _ -> false)

        match objectsEmpty, preps, analyzers with
        | true, [ (_, prep, prepModel) ], [ (_, analyzer, analyzerModel) ] ->
            match prepModel.calibrationPolarizerOpt, analyzerModel.calibrationPolarizerOpt with
            | Some prepPolarizer, Some analyzerPolarizer ->
                Some { runFamily = prepPolarizer.familyWith analyzerPolarizer; prepItem = prep.id; analyzerItem = analyzer.id }
            | _ -> None
        | _ -> None

    /// The sixteen-element black box's coefficients m00 … m33, its form's own axes (spec 0050 S8.1).
    let private muellerCoefficients : ParameterName list =
        (BlackBoxMuellerModel SixteenElementMueller).parameterAxes |> List.map (fun axis -> axis.name)

    type StagedStudy with

        /// Linear for the reference AIR calibration under stage 1, or for sixteen-element black boxes, one per fitted path,
        /// behind fixed optics under stage 2 or joint; nonlinear otherwise. Structural: no score, threshold or rank is read
        /// (spec 0050 S11.3; 001 §L).
        member this.engineWith (context : StudyForwardContext) (prefits : ExperimentPrefit list) : StudyEngineKind =
            let kit = { context.kit with items = this.items }
            let entries = (this.study.parameterLayoutWith { context with kit = kit }).entries
            let fittedParameters = entries |> List.map (fun entry -> entry.unknown.parameter)

            // A link's follower takes its leader's value, so following a fitted leader makes it fitted too (spec 0050 S11.1).
            let fittedFollowers : StudyParameterRef list =
                this.study.links
                |> List.filter (fun link -> List.contains link.leader fittedParameters)
                |> List.map (fun link -> link.follower)

            let bearsUnknown (item : ItemId) : bool =
                fittedParameters @ fittedFollowers
                |> List.exists (fun parameter ->
                    match parameter with
                    | ItemParameterRef (owner, _) -> owner = item
                    | MaterialParameterRef _ -> false)

            let itemOwned (entry : StudyVectorEntry) : bool =
                match entry.owner, entry.unknown.parameter with
                | ItemOwner owner, ItemParameterRef (item, _) -> owner = item
                | (ItemOwner _ | MaterialOwner _ | SessionOwner _ | ExperimentOwner _), _ -> false

            let fitted : (ExperimentName * (SlotRole * Item * ItemModelChoice) list) list = fittedHeldIn this context

            let everyRunResolves = fitted.Length = this.fittedRuns.Length

            // The rows of a Phone ROI CSV, step 300's capture rows (spec 0050 S9.2).
            let readsCaptureRows (run : ExperimentName) : bool =
                match prefits |> List.tryFind (fun prefit -> prefit.experiment.name = run) |> Option.map (fun prefit -> prefit.check) with
                | Some (Ok (CapturePoints _)) -> true
                | Some (Ok (SeriesPoints _ | ObservationPoints _ | ColumnPoints _))
                | Some (Error _)
                | None -> false

            // The detector that reads the run, as `boundWith` finds it: the first detector-slot item of a detector form.
            let readByIntensityDetector (held : (SlotRole * Item * ItemModelChoice) list) : bool =
                let detectorForms : ItemModelForm list =
                    held
                    |> List.choose (fun (role, _, model) ->
                        match role, model with
                        | DetectorSlotRole _, LibraryModelChoice version ->
                            match version.entry.model with
                            | IntensityDetectorModel _
                            | MuellerPolarimeterModel _
                            | EllipsometerModel _ -> Some version.entry.model
                            | SourcePresetItemModel
                            | IdealPolarizerItemModel
                            | StackSampleItemModel
                            | OpticalElementModel _
                            | EmptySlotModel -> None
                        | DetectorSlotRole _, (SourcePresetChoice _ | PolarizerPresetChoice _ | StackSampleChoice _)
                        | (SourceSlotRole | PrepSlotRole _ | ObjectSlotRole _ | AnalyzerSlotRole _), _ -> None)

                match detectorForms with
                | IntensityDetectorModel _ :: _ -> true
                | _ -> false

            let readsCapturesByIntensity (run : ExperimentName, held : (SlotRole * Item * ItemModelChoice) list) : bool =
                readsCaptureRows run && readByIntensityDetector held

            // (a) Stage 1 of the reference: LP-LP, LP-CPL and CPL-LP AIR runs over capture rows, fitting only their
            // stage-1 constants.
            let calibrationShape () : bool =
                let families =
                    fitted
                    |> List.choose (fun (run, held) -> if readsCapturesByIntensity (run, held) then calibrationRunOf held else None)

                let constants =
                    families |> List.collect (fun family -> family.stageOneConstants |> List.map (fun constant -> constant.constantParameter))

                let present = families |> List.map (fun family -> family.runFamily)

                everyRunResolves
                && families.Length = fitted.Length
                && [ LpLp; LpCpl; CplLp ] |> List.forall (fun family -> List.contains family present)
                && entries |> List.forall (fun entry -> itemOwned entry && List.contains entry.unknown.parameter constants)
                && List.isEmpty fittedFollowers

            let isCoefficient (parameter : StudyParameterRef) : bool =
                match parameter with
                | ItemParameterRef (id, name) when List.contains name muellerCoefficients ->
                    match this.items |> List.tryFind (fun item -> item.id = id) |> Option.bind context.choiceOf with
                    | Some (LibraryModelChoice model) ->
                        match model.entry.model with
                        | OpticalElementModel (BlackBoxMuellerModel SixteenElementMueller) -> true
                        | _ -> false
                    | Some (SourcePresetChoice _ | PolarizerPresetChoice _ | StackSampleChoice _)
                    | None -> false
                | ItemParameterRef _
                | MaterialParameterRef _ -> false

            // (b) Black-box coefficients, linked or not, each fitted path bearing at most one unknown matrix: every other
            // element on it, its gain included, is fixed.
            let blackBoxShape () : bool =
                // Counted per slot: one free item held in two slots is a double pass, quadratic in its coefficients.
                let oneUnknownMatrix (held : (SlotRole * Item * ItemModelChoice) list) : bool =
                    let bearing = held |> List.filter (fun (_, item, _) -> bearsUnknown item.id)
                    bearing.Length <= 1

                everyRunResolves
                && entries |> List.forall (fun entry -> itemOwned entry && isCoefficient entry.unknown.parameter)
                && fittedFollowers |> List.forall isCoefficient
                && fitted |> List.forall (fun (run, held) -> readsCapturesByIntensity (run, held) && oneUnknownMatrix held)

            let linear =
                match this.stage with
                | CalibrationStage -> calibrationShape ()
                | ObjectStage
                | JointStage -> blackBoxShape ()

            if linear then LinearStudyEngine else NonlinearStudyEngine

    /// What every engine answers and a fit run records: the engine, the layout it fitted, its members, the fitted vector
    /// and its object solves (spec 0050 S11.3).
    type StudyFitSolution =
        {
            engine : StudyEngineKind
            layout : StudyParameterLayout
            members : FitRunMember list
            fitted : StudyParameterVector
            objectMatrices : ObjectMatrixFit list
        }

    /// The nonlinear fit's box: an entry's scaled unit is its bounds' span over twice this half-width, so ±50 units span
    /// its bounds, as every inverse suite searches, and ALGLIB's fixed 1e-6 step stays linear (spec 0050 S11.3).
    let studySearchBox : SearchBox = SearchBox 50.0

    /// The study residual, plus one row per prior, over a vector scaled about each entry's start; total, as ALGLIB drives
    /// it with vectors the forward model may refuse (spec 0050 S11.3).
    [<ReferenceEquality>]
    type ScaledStudyProblem =
        {
            layout : StudyParameterLayout
            centre : StudyParameterVector
            unit : StudyParameterVector
            search : ScaledSearch
            residual : ScaledVector -> ResidualVector
        }

        /// The physical vector a scaled one stands for: entry k is centreₖ + unitₖ · scaledₖ (spec 0050 S11.3).
        member this.physicalOf (scaled : ScaledVector) : StudyParameterVector =
            Array.map3 (fun (centre : float) (unit : float) (coordinate : float) -> centre + unit * coordinate) this.centre.value this.unit.value scaled.value
            |> StudyParameterVector

    type StagedStudy with

        /// The staged study's residual over its layout, scaled about the parameters' starts within their own bounds, plus
        /// one row (xₖ − centre) / sigma per entry with a prior. Refused: a fitted run whose session's gain comes from AIR
        /// runs, then a residual refused at the start (spec 0050 S11.3; 001 §L).
        member this.scaledProblemWith (context : StudyForwardContext) (data : StudyData) : Result<ScaledStudyProblem, StudyFitError> =
            let staged = { context with kit = { context.kit with items = this.items } }
            let layout = this.study.parameterLayoutWith staged
            let unknowns = layout.entries |> List.map (fun entry -> entry.unknown.unknown)

            let unit =
                unknowns
                |> List.map (fun unknown -> (unknown.bounds.upper.value - unknown.bounds.lower.value) / (2.0 * studySearchBox.halfWidth))
                |> Array.ofList
                |> StudyParameterVector

            let scaledBound (boundOf : UnknownParameterData -> ParameterValue) : ScaledVector =
                Array.map2 (fun (unknown : UnknownParameterData) (scale : float) -> ((boundOf unknown).value - unknown.start.value) / scale) (Array.ofList unknowns) unit.value
                |> ScaledVector

            let search : ScaledSearch =
                {
                    start = Array.zeroCreate unknowns.Length |> ScaledVector
                    lowerBounds = scaledBound (fun unknown -> unknown.bounds.lower)
                    upperBounds = scaledBound (fun unknown -> unknown.bounds.upper)
                }

            let fittedData = { data with runs = data.runs |> List.filter (fun run -> List.contains run.experiment.name this.fittedRuns) }

            let priors : (StudyVectorIndex * ParameterPrior) list =
                layout.entries
                |> List.mapi (fun k entry -> entry.unknown.unknown.priorOpt |> Option.map (fun prior -> StudyVectorIndex k, prior))
                |> List.choose id

            // This engine fits a detector's gain as its parameter (step 374); a gain derived from AIR runs is the linear
            // engine's (step 390).
            let derivedGainOpt : StudyFitError option =
                layout.sessions
                |> List.filter (fun session -> session.experiments |> List.exists (fun run -> List.contains run this.fittedRuns))
                |> List.tryPick (fun session ->
                    let name = session.name.value

                    match this.study.gainModelOf session.name with
                    | FreeSessionGain -> None
                    | AnchoredSessionGain anchors ->
                        let runs = anchors |> List.map (fun run -> run.value) |> String.concat ", "
                        Some (SessionGainNeedsLinearEngineErr $"'{name}' anchors its gain to {runs}")
                    | InterpolatedSessionGain repeats ->
                        Some (SessionGainNeedsLinearEngineErr $"'{name}' interpolates its gain from {repeats.opening.value} to {repeats.closing.value}"))

            // The residual reads its scaled vector through the problem's own `physicalOf`, so the frame is made first.
            let frame : ScaledStudyProblem =
                {
                    layout = layout
                    centre = layout.startVector
                    unit = unit
                    search = search
                    residual = fun _ -> ResidualVector [||]
                }

            let residualAt (scaled : ScaledVector) : Result<ResidualVector, StudyForwardError> =
                let physical = frame.physicalOf scaled

                let priorRows =
                    priors
                    |> List.map (fun (index, prior) -> ((physical.valueAt index).value - prior.centre.value) / prior.sigma.value)
                    |> Array.ofList

                this.study.residualWith staged layout fittedData physical
                |> Result.map (fun residual -> Array.append residual.value priorRows |> ResidualVector)

            match derivedGainOpt with
            | Some refusal -> Error refusal
            | None ->
                match residualAt search.start with
                | Error error -> Error (StudyForwardFitErr error)
                | Ok atStart ->
                    let width = atStart.value.Length

                    // `MuellerFitting.residualFor`'s full-width penalty: a refused vector never changes the problem minimized.
                    let total (scaled : ScaledVector) : ResidualVector =
                        match residualAt scaled with
                        | Ok residual -> residual
                        | Error _ -> Array.create width 1.0e3 |> ResidualVector

                    Ok { frame with residual = total }

    type StagedStudy with

        /// The scaled problem minimised by ALGLIB Levenberg–Marquardt from the parameters' starts, then from deviate-placed
        /// starts within the bounds, under one watch counting every iteration; the converged member of least χ² is the fit,
        /// the earliest on a tie (spec 0050 S11.3; 001 §L).
        member this.fitNonlinear
            (context : StudyForwardContext)
            (data : StudyData)
            (deviates : UniformDeviateProvider)
            (watch : IterationWatch)
            : Result<StudyFitSolution, StudyFitError> =
            this.scaledProblemWith context data
            |> Result.bind (fun problem ->
                let count = this.study.settings.starts.value
                let search = problem.search

                let cancellation : CancellationToken =
                    match watch with
                    | WatchedIterations observer -> observer.cancellation
                    | UnwatchedIterations -> CancellationToken.None

                let valuesOf (vector : StudyParameterVector) : ParameterValue list =
                    List.init vector.value.Length (fun k -> vector.valueAt (StudyVectorIndex k))

                // Uniform on [−1, 1] placed across each coordinate's box, drawn entry after entry.
                let drawnStart () : ScaledVector =
                    Array.map2
                        (fun (lower : float) (upper : float) -> lower + ((deviates.nextDeviate ()).value + 1.0) / 2.0 * (upper - lower))
                        search.lowerBounds.value
                        search.upperBounds.value
                    |> ScaledVector

                // A member's watch counts on from the iterations the earlier members were told of, so the progress runs
                // 1, 2, … over the whole fit; the count is kept where the solver tells it, its one effect.
                let watchAfter (earlier : IterationCount) : IterationWatch * (unit -> IterationCount) =
                    match watch with
                    | UnwatchedIterations -> UnwatchedIterations, (fun () -> earlier)
                    | WatchedIterations observer ->
                        let told = ref earlier

                        let counting : OptimizerIterationObserver =
                            {
                                reportIteration =
                                    fun own ->
                                        told.Value <- IterationCount (earlier.value + own.value)
                                        observer.reportIteration told.Value
                                cancellation = observer.cancellation
                            }

                        WatchedIterations counting, (fun () -> told.Value)

                let rec membersFrom
                    (k : int)
                    (earlier : IterationCount)
                    (ran : (FitRunMember * (StudyParameterVector * FitChiSquared) option) list)
                    : Result<(FitRunMember * (StudyParameterVector * FitChiSquared) option) list, StudyFitError> =
                    if k > count then
                        Ok (List.rev ran)
                    elif cancellation.IsCancellationRequested then
                        Error (FitCancelledErr $"{k - 1} of {count} member(s) ran")
                    else
                        let start = if k = 1 then search.start else drawnStart ()
                        let memberWatch, toldOf = watchAfter earlier
                        let solved = { search with start = start }.solveWith memberWatch problem.residual

                        if cancellation.IsCancellationRequested then
                            Error (FitCancelledErr $"{k} of {count} member(s) ran, the last one's result dropped")
                        else
                            let outcome, convergedOpt =
                                match solved with
                                | Ok solution ->
                                    let fitted = problem.physicalOf solution.solution
                                    let chiSquared = FitChiSquared solution.finalResiduals.sumOfSquares
                                    MemberConverged (valuesOf fitted, Some chiSquared), Some (fitted, chiSquared)
                                | Error error -> MemberFailed error.failureText, None

                            let ran' = ({ start = valuesOf (problem.physicalOf start); outcome = outcome }, convergedOpt) :: ran
                            membersFrom (k + 1) (toldOf ()) ran'

                membersFrom 1 (IterationCount 0) []
                |> Result.bind (fun outcomes ->
                    let members = outcomes |> List.map fst

                    // Strictly less replaces the held best, so the earliest member wins a tie.
                    let bestOpt =
                        outcomes
                        |> List.choose snd
                        |> List.fold
                            (fun (heldOpt : (StudyParameterVector * FitChiSquared) option) (vector, chiSquared) ->
                                match heldOpt with
                                | Some (_, held) when held.value <= chiSquared.value -> heldOpt
                                | Some _
                                | None -> Some (vector, chiSquared))
                            None

                    match bestOpt with
                    | Some (fitted, _) ->
                        Ok
                            {
                                engine = NonlinearStudyEngine
                                layout = problem.layout
                                members = members
                                fitted = fitted
                                objectMatrices = []
                            }
                    | None ->
                        let failures =
                            members
                            |> List.mapi (fun k fitMember ->
                                match fitMember.outcome with
                                | MemberFailed failure -> $"member {k + 1}: {failure.value}"
                                | MemberConverged _ -> $"member {k + 1}: converged")
                            |> String.concat "; "

                        Error (NoConvergedMemberErr failures)))

    type StagedStudy with

        /// Stage 1 by the reference AIR calibration, where `engineWith` finds the calibration shape: S10.3's stage one over
        /// every Dark run's capture rows and each family's Calibration runs', its constants the instruments' values; the
        /// cosine fits report no common χ² (spec 0050 S11.3; 001 §L).
        member this.calibrateLinear
            (context : StudyForwardContext)
            (prefits : ExperimentPrefit list)
            (solver : MuellerSolverProxy)
            : Result<StudyFitSolution, StudyFitError> =
            let staged = { context with kit = { context.kit with items = this.items } }
            let layout = this.study.parameterLayoutWith staged

            let families : (ExperimentName * CalibrationRunFamily) list =
                fittedHeldIn this staged |> List.choose (fun (run, held) -> calibrationRunOf held |> Option.map (fun family -> run, family))

            let isDark (prefit : ExperimentPrefit) : bool =
                match prefit.experiment.role with
                | DarkRun -> true
                | ScienceRun
                | CalibrationRun
                | DriftCheckRun
                | DiagnosticRun
                | IgnoredRun
                | PlannedRun -> false

            let ofFamily (family : Family) (prefit : ExperimentPrefit) : bool =
                families |> List.exists (fun (run, calibration) -> run = prefit.experiment.name && calibration.runFamily = family)

            // The pre-fit check has kept its points by the attachment's exclusions (`MappedPoints.keptBy`, step 314); a run
            // stage one reads is refused, never dropped, when its check failed or holds no capture rows.
            let captureRowsOf (prefit : ExperimentPrefit) : Result<MuellerRawRow list, StudyFitError> =
                let name = prefit.experiment.name

                match prefit.check with
                | Ok (CapturePoints rows) -> Ok rows
                | Ok (SeriesPoints _ | ObservationPoints _ | ColumnPoints _) ->
                    Error (StudyForwardFitErr (ReadingMismatchErr $"{name.value} holds no capture rows, which the staged Mueller path reads"))
                | Error error -> Error (StudyForwardFitErr (PrefitFailedErr (name, error)))

            // Prefits are in study order, so each selection keeps it.
            let rowsOf (selected : ExperimentPrefit -> bool) : Result<MuellerRawRow list, StudyFitError> =
                prefits |> List.filter selected |> List.map captureRowsOf |> ItemEvaluation.sequenced |> Result.map List.concat

            rowsOf isDark
            |> Result.bind (fun darknessRows ->
                rowsOf (ofFamily LpLp)
                |> Result.bind (fun lpLpAirRows ->
                    rowsOf (ofFamily LpCpl)
                    |> Result.bind (fun lpCplAirRows ->
                        rowsOf (ofFamily CplLp)
                        |> Result.bind (fun cplLpAirRows ->
                            let rows : StageOneAirRows =
                                {
                                    darknessRows = darknessRows
                                    lpLpAirRows = lpLpAirRows
                                    lpCplAirRows = lpCplAirRows
                                    cplLpAirRows = cplLpAirRows
                                }

                            calibrateFromAirRows solver rows |> Result.mapError StagedPathFitErr))))
            |> Result.map (fun stageOne ->
                let constants = families |> List.collect (fun (_, family) -> family.stageOneConstants)

                // Under the calibration shape every entry is a constant; any other keeps its start.
                let valueOf (entry : StudyVectorEntry) : ParameterValue =
                    match constants |> List.tryFind (fun constant -> constant.constantParameter = entry.unknown.parameter) with
                    | Some constant -> constant.constantIn stageOne
                    | None -> entry.unknown.unknown.start

                let starts = layout.entries |> List.map (fun entry -> entry.unknown.unknown.start)
                let values = layout.entries |> List.map valueOf

                {
                    engine = LinearStudyEngine
                    layout = layout
                    members = [ ({ start = starts; outcome = MemberConverged (values, None) } : FitRunMember) ]
                    fitted = StudyParameterVector.ofParameterValues values
                    objectMatrices = []
                })
