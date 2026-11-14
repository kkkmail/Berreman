namespace OpticalConstructor.Optimization

open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Domain.PrefitChecks
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.StudyForward
open OpticalConstructor.Domain.FitRuns
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.ItemEvaluation
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.MuellerFitting
open OpticalConstructor.Optimization.BenchNoise
open OpticalConstructor.Optimization.StagedMuellerPath
open OpticalConstructor.Optimization.FittedProblems

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
        /// A fitted run's session gain model its AIR runs' gains cannot resolve (spec 0050 S11.3).
        | SessionGainFitErr of error : StudyError
        /// The least-squares solve of these black-box objects refused (spec 0050 S11.3).
        | ObjectSolveFitErr of items : ItemId list * error : SolverError
        /// The fit names a study the kit does not hold (spec 0050 S11.3).
        | UnknownFitStudyErr of reason : string
        /// The kit refused to read the study's fit runs or to write the new one (spec 0050 S11.3).
        | FitRunKitErr of error : KitError
        /// The fit job raised, or answered no single result, on its worker (spec 0050 S11.3).
        | FitRunRaisedErr of reason : string

        member this.describe : string =
            match this with
            | StudyForwardFitErr error -> $"the study's model or data refused the fit: {error.describe}"
            | FitStageErr error -> $"the stage refused the fit: {error.describe}"
            | SessionGainNeedsLinearEngineErr reason -> $"the nonlinear engine fits a session's gain as its detector's parameter: {reason}"
            | NoConvergedMemberErr reason -> $"no start of the fit converged: {reason}"
            | FitCancelledErr reason -> $"the fit was cancelled: {reason}"
            | StagedPathFitErr error -> $"the staged Mueller path refused the fit: {error.describe}"
            | SessionGainFitErr error -> $"a session gain model was refused: {error.describe}"
            | ObjectSolveFitErr (items, error) ->
                let named = items |> List.map (fun item -> $"{item.value}") |> String.concat ", "
                $"the objects could not be reconstructed: {named} — {error.describe}"
            | UnknownFitStudyErr reason -> $"the kit has no such study: {reason}"
            | FitRunKitErr error -> $"the kit refused the fit run: {error.describe}"
            | FitRunRaisedErr reason -> $"the fit raised: {reason}"

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

    /// The same coefficients in `MuellerMatrix.vecColumnMajor` order, m[row, column] at 4·column + row (spec 0050 S11.3).
    let private vecOrderCoefficients : ParameterName list =
        List.init 16 (fun place -> List.item (4 * (place % 4) + place / 4) muellerCoefficients)

    /// A link's follower takes its leader's value, so following a fitted leader makes it fitted too (spec 0050 S11.1,
    /// S11.3).
    let private fittedFollowersIn (study : Study) (fitted : StudyParameterRef list) : StudyParameterRef list =
        study.links |> List.filter (fun link -> List.contains link.leader fitted) |> List.map (fun link -> link.follower)

    /// The held items owning a moved parameter, in their order and once per slot holding them (spec 0050 S11.3).
    let private bearersAmong (moved : StudyParameterRef list) (held : ItemId list) : ItemId list =
        held
        |> List.filter (fun item ->
            moved
            |> List.exists (fun parameter ->
                match parameter with
                | ItemParameterRef (owner, _) -> owner = item
                | MaterialParameterRef _ -> false))

    type StagedStudy with

        /// Linear for the reference AIR calibration under stage 1, or for sixteen-element black boxes, one per fitted path,
        /// behind fixed optics under stage 2 or joint; nonlinear otherwise. Structural: no score, threshold or rank is read
        /// (spec 0050 S11.3; 001 §L).
        member this.engineWith (context : StudyForwardContext) (prefits : ExperimentPrefit list) : StudyEngineKind =
            let kit = { context.kit with items = this.items }
            let entries = (this.study.parameterLayoutWith { context with kit = kit }).entries
            let fittedParameters = entries |> List.map (fun entry -> entry.unknown.parameter)
            let fittedFollowers : StudyParameterRef list = fittedFollowersIn this.study fittedParameters

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
                    let bearing = bearersAmong (fittedParameters @ fittedFollowers) (held |> List.map (fun (_, item, _) -> item.id))
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

                let cancellation = watch.cancellation

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
                let told = ref (IterationCount 0)
                let telling = watch.tellingAlso (fun count -> told.Value <- count)

                let rec membersFrom
                    (k : int)
                    (ran : (FitRunMember * (StudyParameterVector * FitChiSquared) option) list)
                    : Result<(FitRunMember * (StudyParameterVector * FitChiSquared) option) list, StudyFitError> =
                    if k > count then
                        Ok (List.rev ran)
                    elif cancellation.IsCancellationRequested then
                        Error (FitCancelledErr $"{k - 1} of {count} member(s) ran")
                    else
                        let start = if k = 1 then search.start else drawnStart ()
                        let solved = { search with start = start }.solveWith (telling.after told.Value) problem.residual

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
                            membersFrom (k + 1) ran'

                membersFrom 1 []
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

    type ExperimentPrefit with

        /// The run's capture rows as its pre-fit check kept them by the attachment's exclusions (`MappedPoints.keptBy`,
        /// step 314); a run the linear engine reads is refused, never dropped, when its check failed or holds no capture
        /// rows (spec 0050 S11.3).
        member private this.captureRows : Result<MuellerRawRow list, StudyFitError> =
            let name = this.experiment.name

            match this.check with
            | Ok (CapturePoints rows) -> Ok rows
            | Ok (SeriesPoints _ | ObservationPoints _ | ColumnPoints _) ->
                Error (StudyForwardFitErr (ReadingMismatchErr $"{name.value} holds no capture rows, which the staged Mueller path reads"))
            | Error error -> Error (StudyForwardFitErr (PrefitFailedErr (name, error)))

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

            // Prefits are in study order, so each selection keeps it.
            let rowsOf (selected : ExperimentPrefit -> bool) : Result<MuellerRawRow list, StudyFitError> =
                prefits |> List.filter selected |> List.map (fun prefit -> prefit.captureRows) |> ItemEvaluation.sequenced |> Result.map List.concat

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

    type ParameterValue with

        /// The number as a detector's gain, the scale a row's dark-subtracted signal is divided by (spec 0050 S11.3).
        member this.airGain : AirGain =
            let (ParameterValue gain) = this
            AirGain gain

    type MuellerMatrix with

        /// Its sixteen elements m00 … m33, row-major, as parameter numbers (spec 0050 S11.3).
        member this.rowMajorValues : ParameterValue list =
            [
                for row in 0 .. 3 do
                    for column in 0 .. 3 -> ParameterValue (this.element row column)
            ]

    /// One capture row as the run law reads it about its run's unknown segment, every step at the row's own orientation
    /// and wavelength: the state reaching the segment, the detector's S₀ row through the steps past it, and the segment's
    /// rotation; the whole path, unrotated, for a run with none (spec 0050 S11.3).
    type private SegmentProjection =
        {
            arriving : StokesVector
            analyzing : RealVector4
            segmentRotation : Angle
            captured : MuellerRawRow
        }

    /// A run's projected capture rows, the items of its unknown segment in light order, and the dark level and held gain
    /// its detector reads with (spec 0050 S11.3).
    type private ProjectedRun =
        {
            projectedRun : ExperimentName
            bearers : ItemId list
            runDark : DarkMean
            heldGain : ParameterValue
            projections : SegmentProjection list
        }

    /// An object's sixteen coefficients in `vecColumnMajor` order as the affine image m = k + P·x of the fitted vector:
    /// `fixedPart` is k, and each free column is P's column of the entry it names (spec 0050 S11.3).
    type private CoefficientMap =
        {
            fixedPart : RealVector
            freeColumns : (StudyVectorIndex * RealVector) list
        }

    /// A lone object's fitted runs: each run's full system over its corrected rows, with its coefficient map (spec 0050
    /// S11.3).
    type private LoneObjectRuns =
        {
            loneItem : ItemId
            runSystems : (LeastSquaresSystem * CoefficientMap) list
        }

        /// The entries any of its runs fits it in.
        member this.columns : StudyVectorIndex list =
            this.runSystems |> List.collect (fun (_, map) -> map.freeColumns |> List.map fst) |> List.distinct

    /// One group's solve: its object solve, its complete matrix, and its own rows, which its share of the χ² counts (spec
    /// 0050 S11.3).
    type private GroupSolve =
        {
            matrixFit : ObjectMatrixFit
            solvedMatrix : MuellerMatrix
            ownRows : LeastSquaresSystem
        }

    /// An item evaluation's refusal as the study fit's, the one mapping the object stage's systems and solves share (spec
    /// 0050 S11.3, S11.4).
    let private evaluatedForFit (result : Result<'Value, ItemEvaluation.ItemEvaluationError>) : Result<'Value, StudyFitError> =
        result |> Result.mapError (fun error -> StudyForwardFitErr (ForwardSlotErr (ItemEvaluation.SlotEvaluationErr error)))

    /// One fitted run's capture rows, each projected by the run law about its unknown segment and divided by its session's
    /// gain at its time (spec 0050 S11.4).
    type ObjectRunSystem =
        {
            run : ExperimentName
            system : LeastSquaresSystem
        }

    /// The fitted runs whose unknown segment holds these items in light order, in study order (spec 0050 S11.4).
    type ObjectGroupSystem =
        {
            items : ItemId list
            runs : ObjectRunSystem list
        }

        /// Its runs' rows stacked in run order, the system its object solve is over (spec 0050 S11.4).
        member this.system : LeastSquaresSystem = this.runs |> List.map (fun run -> run.system) |> LeastSquaresSystem.concat

    type StagedStudy with

        /// The object stage's systems: each fitted run's capture rows projected by the run law about its unknown segment and
        /// divided by its session's gain at its time, grouped by the segment's items in first-run order (spec 0050 S11.3,
        /// S11.4).
        member this.objectSystemsWith
            (context : StudyForwardContext)
            (prefits : ExperimentPrefit list)
            (data : StudyData)
            : Result<ObjectGroupSystem list, StudyFitError> =
            let staged = { context with kit = { context.kit with items = this.items } }
            let layout = this.study.parameterLayoutWith staged
            let items = staged.kit.items
            let fittedParameters = layout.entries |> List.map (fun entry -> entry.unknown.parameter)
            let moved = fittedParameters @ fittedFollowersIn this.study fittedParameters

            let sessionOf (run : ExperimentName) : StudySession option =
                layout.sessions |> List.tryFind (fun session -> List.contains run session.experiments)

            // A run whose session reads no Dark run keeps its detector's own dark offset, as the residual does (step 374).
            let darkOf (run : ExperimentName) : DarkMean option =
                sessionOf run |> Option.bind (fun session -> data.darkOf session.name)

            // Bound once at the start vector: an unknown is never evaluated, and every fixed item holds its own value.
            let projectedOf (prefit : ExperimentPrefit) : Result<ProjectedRun, StudyFitError> =
                let experiment = prefit.experiment
                let name = experiment.name

                prefit.captureRows
                |> Result.bind (fun rows ->
                    layout.assignmentFor this.study items name layout.startVector
                    |> Result.bind (fun assignment -> experiment.boundWith staged this.study assignment (darkOf name))
                    |> Result.bind (fun bound ->
                        (CapturePoints rows).measuredRowsFor experiment.scan |> Result.map (fun measured -> bound, List.zip rows measured))
                    |> Result.mapError StudyForwardFitErr)
                |> Result.bind (fun (bound, placed) ->
                    match bound.readout with
                    | IntensityDetectorReadout (gain, dark) ->
                        let bearers = bearersAmong moved (bound.path |> List.choose (fun step -> step.slot.content.itemOpt))

                        let bearing (step : BoundPathStep) : bool =
                            match step.slot.content.itemOpt with
                            | Some item -> List.contains item bearers
                            | None -> false

                        // The segment runs from the first unknown-bearing step to the last; a black box is a sample's
                        // object step, which `emittedInto` never takes as the reference, so the steps before it fold the
                        // emission as the whole path does (spec 0050 S8.1).
                        let before, segmentOpt, after =
                            match List.tryFindIndex bearing bound.path, List.tryFindIndexBack bearing bound.path with
                            | Some first, Some last -> List.take first bound.path, Some (List.item first bound.path), List.skip (last + 1) bound.path
                            | _ -> bound.path, None, []

                        let projectionOf (row : MuellerRawRow, measured : MeasuredRow) : Result<SegmentProjection, StudyFitError> =
                            match measured with
                            | SettingsRow (settings, waveLengthOpt, _) ->
                                let w = bound.waveLengthAt settings waveLengthOpt

                                let stepsOf (steps : BoundPathStep list) : Result<ExperimentRun.RunStep list, ItemEvaluation.ItemEvaluationError> =
                                    steps
                                    |> List.map (fun step -> step.element.muellerAt (bound.orientationAt settings step) w |> Result.map step.slot.runStepOf)
                                    |> ItemEvaluation.sequenced

                                stepsOf before
                                |> Result.bind (fun beforeSteps ->
                                    stepsOf after
                                    |> Result.map (fun afterSteps ->
                                        let past = ExperimentRun.RunStep.productOf afterSteps

                                        let rotation =
                                            match segmentOpt with
                                            | Some step -> (bound.orientationAt settings step).rotation
                                            | None -> Angle.zero

                                        let projection : SegmentProjection =
                                            {
                                                arriving = ExperimentRun.detectorStokes bound.emission beforeSteps
                                                analyzing = RealVector4.create [ for column in 0 .. 3 -> past.element 0 column ]
                                                segmentRotation = rotation
                                                captured = row
                                            }

                                        projection))
                                |> evaluatedForFit
                            | ObservationRow _ ->
                                Error (StudyForwardFitErr (ReadingMismatchErr $"{name.value} holds a Mueller observation, which the linear path does not read"))

                        placed
                        |> List.map projectionOf
                        |> ItemEvaluation.sequenced
                        |> Result.map (fun projections ->
                            let projected : ProjectedRun =
                                {
                                    projectedRun = name
                                    bearers = bearers
                                    runDark = dark
                                    heldGain = gain
                                    projections = projections
                                }

                            projected)
                    | PolarimeterDetectorReadout
                    | EllipsometerDetectorReadout ->
                        Error (StudyForwardFitErr (ReadingMismatchErr $"{name.value} is not read by an intensity detector, which the linear path reads")))

            // An AIR run's gain from its own rows, projected by the run law and dark-corrected as its detector reads them.
            let anchorOf (prefit : ExperimentPrefit) : Result<AirAnchorGain, StudyFitError> =
                projectedOf prefit
                |> Result.bind (fun air ->
                    air.projections
                    |> List.map (fun projection -> projection.arriving, projection.analyzing, projection.captured)
                    |> airAnchorGainOver air.runDark air.projectedRun
                    |> Result.mapError StagedPathFitErr)

            // A free session divides by the held detector gain, an anchored or interpolated one by its rule (spec 0050 S11.1).
            let ruleOf (run : ExperimentName) : Result<SessionGainRule option, StudyFitError> =
                let model =
                    match sessionOf run with
                    | Some session -> this.study.gainModelOf session.name
                    | None -> FreeSessionGain

                let anchoredBy (airRuns : ExperimentName list) : Result<SessionGainRule option, StudyFitError> =
                    prefits
                    |> List.filter (fun prefit -> List.contains prefit.experiment.name airRuns)
                    |> List.map anchorOf
                    |> ItemEvaluation.sequenced
                    |> Result.bind (fun anchors -> model.ruleWith anchors |> Result.mapError SessionGainFitErr)
                    |> Result.map Some

                match model with
                | FreeSessionGain -> Ok None
                | AnchoredSessionGain anchors -> anchoredBy anchors
                | InterpolatedSessionGain repeats -> anchoredBy [ repeats.opening; repeats.closing ]

            // Each row as (s, a, (avg − dark) / gain at its capture time) in its object's frame, its full row `designRow s a`.
            let fullSystemOf (projected : ProjectedRun) : Result<LeastSquaresSystem, StudyFitError> =
                ruleOf projected.projectedRun
                |> Result.map (fun ruleOpt ->
                    projected.projections
                    |> List.map (fun projection ->
                        let gain =
                            match ruleOpt with
                            | Some rule -> rule.gainAt projection.captured.capturedAt
                            | None -> projected.heldGain.airGain

                        projectCaptureRowFrom projection.arriving projection.analyzing projection.segmentRotation projected.runDark gain projection.captured)
                    |> LeastSquaresSystem.ofProjections)

            prefits
            |> List.filter (fun prefit -> List.contains prefit.experiment.name this.fittedRuns)
            |> List.map projectedOf
            |> ItemEvaluation.sequenced
            |> Result.bind (fun projectedRuns ->
                projectedRuns
                |> List.filter (fun projected -> not (List.isEmpty projected.bearers))
                |> List.map (fun projected ->
                    fullSystemOf projected
                    |> Result.map (fun system -> projected.bearers, ({ run = projected.projectedRun; system = system } : ObjectRunSystem)))
                |> ItemEvaluation.sequenced)
            |> Result.map (fun systems ->
                // By the items of each run's unknown segment, in first-run order.
                systems
                |> List.groupBy fst
                |> List.map (fun (key, runs) -> ({ items = key; runs = List.map snd runs } : ObjectGroupSystem)))

    type StagedStudy with

        /// Stage 2, or a joint fit, by the reference linear path where `engineWith` finds the black-box shape: each capture
        /// row projected by the run law about its unknown object and divided by its session's gain at its time, then solved
        /// by `reconstruct` or on its free columns through the same proxy (spec 0050 S11.3; 001 §L).
        member this.reconstructLinear
            (context : StudyForwardContext)
            (prefits : ExperimentPrefit list)
            (data : StudyData)
            (solver : MuellerSolverProxy)
            : Result<StudyFitSolution, StudyFitError> =
            let staged = { context with kit = { context.kit with items = this.items } }
            let layout = this.study.parameterLayoutWith staged
            let items = staged.kit.items
            let count = layout.entries.Length

            // The study's assignment is affine in the vector, each value a copy of an entry, of a leader or its own: its image
            // of the zero vector is k, and each unit vector's change is P's column of that entry (spec 0050 S11.1, S11.3).
            let coefficientMapOf (run : ExperimentName) (item : ItemId) : Result<CoefficientMap, StudyFitError> =
                let coefficientsAt (vector : StudyParameterVector) : Result<float[], StudyFitError> =
                    layout.assignmentFor this.study items run vector
                    |> Result.mapError StudyForwardFitErr
                    |> Result.bind (fun assignment ->
                        match items |> List.tryFind (fun held -> held.id = item) with
                        | Some held ->
                            let values = assignment.valuesOf held

                            vecOrderCoefficients
                            |> List.map values.valueOf
                            |> ItemEvaluation.sequenced
                            |> Result.map (fun coefficients -> coefficients |> List.map (fun coefficient -> coefficient.value) |> Array.ofList)
                            |> evaluatedForFit
                        | None -> Error (StudyForwardFitErr (ForwardSlotErr (ItemEvaluation.SlotItemErr (UnknownSlotItemErr $"the kit holds no item {item.value}")))))

                let unitAt (entry : int) : StudyParameterVector = Array.init count (fun k -> if k = entry then 1.0 else 0.0) |> StudyParameterVector

                coefficientsAt (Array.zeroCreate count |> StudyParameterVector)
                |> Result.bind (fun fixedPart ->
                    List.init count (fun entry -> coefficientsAt (unitAt entry) |> Result.map (fun image -> StudyVectorIndex entry, Array.map2 (-) image fixedPart))
                    |> ItemEvaluation.sequenced
                    |> Result.map (fun columns ->
                        let map : CoefficientMap =
                            {
                                fixedPart = RealVector.create fixedPart
                                freeColumns =
                                    columns
                                    |> List.filter (fun (_, column) -> column |> Array.exists (fun weight -> weight <> 0.0))
                                    |> List.map (fun (index, column) -> index, RealVector.create column)
                            }

                        map))

            let dot (row : DesignRow) (vector : RealVector) : float = row.value |> Array.mapi (fun i entry -> entry * vector.[i]) |> Array.sum

            let columnIn (map : CoefficientMap) (index : StudyVectorIndex) : RealVector option =
                map.freeColumns |> List.tryFind (fun (held, _) -> held = index) |> Option.map snd

            // A run's rows over the system's columns: A·P, a linked column the sum of its coefficients' entries, and b − A·k.
            let reducedOver (columns : StudyVectorIndex list) (system : LeastSquaresSystem, map : CoefficientMap) : LeastSquaresSystem =
                let weights = columns |> List.map (columnIn map)

                {
                    design =
                        system.design
                        |> Array.map (fun row ->
                            weights
                            |> List.map (fun weightOpt ->
                                match weightOpt with
                                | Some column -> dot row column
                                | None -> 0.0)
                            |> Array.ofList
                            |> DesignRow)
                    rhs = Array.map2 (fun (row : DesignRow) (target : float) -> target - dot row map.fixedPart) system.design system.rhs
                }

            // m = k + P·x assembled whole before it is un-vec'd; a coefficient no column carries keeps its fixed value exactly.
            let assembledOf (columns : StudyVectorIndex list) (solution : RealVector) (map : CoefficientMap) : MuellerMatrix =
                let carried =
                    columns |> List.mapi (fun position index -> columnIn map index |> Option.map (fun column -> column, solution.[position])) |> List.choose id

                let values =
                    Array.init 16 (fun place ->
                        match carried |> List.filter (fun (column, _) -> column.[place] <> 0.0) with
                        | [] -> map.fixedPart.[place]
                        | weighted -> map.fixedPart.[place] + (weighted |> List.sumBy (fun (column, value) -> column.[place] * value)))

                (RealVector.create values).columnMajorMueller

            let solveOf (solvedItems : ItemId list) (matrix : MuellerMatrix) (rank : MatrixRank) (rmse : ResidualRmse) (rows : LeastSquaresSystem) : GroupSolve =
                {
                    matrixFit = { items = solvedItems; elements = matrix.rowMajorValues; rank = rank; rmse = rmse }
                    solvedMatrix = matrix
                    ownRows = rows
                }

            let ownedBy (item : ItemId) (index : StudyVectorIndex) : bool =
                match (List.item index.value layout.entries).unknown.parameter with
                | ItemParameterRef (owner, _) -> owner = item
                | MaterialParameterRef _ -> false

            // One system of lone objects, its columns in layout order: a lone item fitting all sixteen of its own by
            // `reconstruct`, any other through the proxy on A·P·x = b − A·k (spec 0050 S11.3).
            let solveLone (system : LoneObjectRuns list) : Result<GroupSolve list, StudyFitError> =
                let solvedItems = system |> List.map (fun lone -> lone.loneItem)
                let columns = system |> List.collect (fun lone -> lone.columns) |> List.distinct |> List.sortBy (fun index -> index.value)
                let rowsOf (lone : LoneObjectRuns) : LeastSquaresSystem = lone.runSystems |> List.map fst |> LeastSquaresSystem.concat

                match system with
                | [ lone ] when columns.Length = 16 && columns |> List.forall (ownedBy lone.loneItem) ->
                    let rows = rowsOf lone

                    reconstruct solver rows
                    |> Result.mapError (fun error -> ObjectSolveFitErr (solvedItems, error))
                    |> Result.map (fun reconstructed -> [ solveOf solvedItems reconstructed.matrix reconstructed.rank reconstructed.rmse rows ])
                | _ ->
                    system
                    |> List.collect (fun lone -> lone.runSystems |> List.map (reducedOver columns))
                    |> LeastSquaresSystem.concat
                    |> solver.solveLinearLeastSquares
                    |> Result.mapError (fun error -> ObjectSolveFitErr (solvedItems, error))
                    |> Result.map (fun solution ->
                        system
                        |> List.choose (fun lone ->
                            match lone.runSystems with
                            | (_, map) :: _ ->
                                let matrix = assembledOf columns solution.solution map
                                Some (solveOf [ lone.loneItem ] matrix solution.rank solution.rmse (rowsOf lone))
                            | [] -> None))

            // A series' combined matrix, its first unknown to its last, over its own rows: a diagnostic beside its
            // constituents' fits, never their coefficients (spec 0050 S11.3, S11.4).
            let solveSeries (series : ObjectGroupSystem) : Result<GroupSolve, StudyFitError> =
                let rows = series.system

                reconstruct solver rows
                |> Result.mapError (fun error -> ObjectSolveFitErr (series.items, error))
                |> Result.map (fun reconstructed -> solveOf series.items reconstructed.matrix reconstructed.rank reconstructed.rmse rows)

            this.objectSystemsWith context prefits data
            |> Result.bind (fun groups ->
                groups
                |> List.choose (fun group ->
                    match group.items with
                    | [ item ] -> Some (item, group.runs)
                    | _ -> None)
                |> List.map (fun (item, runs) ->
                    runs
                    |> List.map (fun run -> coefficientMapOf run.run item |> Result.map (fun map -> run.system, map))
                    |> ItemEvaluation.sequenced
                    |> Result.map (fun runSystems -> ({ loneItem = item; runSystems = runSystems } : LoneObjectRuns)))
                |> ItemEvaluation.sequenced
                |> Result.bind (fun lones ->
                    let firstRunOrder (lone : LoneObjectRuns) : int = lones |> List.findIndex (fun held -> held.loneItem = lone.loneItem)

                    // Lone objects sharing a fitted entry, a link's leader, are one system (spec 0050 S11.1, S11.3).
                    let loneSystems : LoneObjectRuns list list =
                        lones
                        |> List.fold
                            (fun (held : LoneObjectRuns list list) lone ->
                                let shares (system : LoneObjectRuns list) : bool =
                                    system |> List.exists (fun other -> other.columns |> List.exists (fun index -> List.contains index lone.columns))

                                let sharing, apart = held |> List.partition shares
                                apart @ [ List.concat sharing @ [ lone ] |> List.sortBy firstRunOrder ])
                            []

                    loneSystems
                    |> List.map solveLone
                    |> ItemEvaluation.sequenced
                    |> Result.map List.concat
                    |> Result.bind (fun loneSolves ->
                        groups
                        |> List.choose (fun group ->
                            match group.items with
                            | _ :: _ :: _ -> Some (solveSeries group)
                            | []
                            | [ _ ] -> None)
                        |> ItemEvaluation.sequenced
                        |> Result.map (fun seriesSolves ->
                            let solves =
                                groups
                                |> List.choose (fun group -> loneSolves @ seriesSolves |> List.tryFind (fun solve -> solve.matrixFit.items = group.items))

                            let startValues = layout.entries |> List.map (fun entry -> entry.unknown.unknown.start)

                            // A lone object fills only its own entries, each from its complete matrix.
                            let fittedValues =
                                layout.entries
                                |> List.map (fun entry ->
                                    match entry.unknown.parameter with
                                    | ItemParameterRef (item, name) ->
                                        let solveOpt = solves |> List.tryFind (fun solve -> solve.matrixFit.items = [ item ])

                                        match solveOpt, vecOrderCoefficients |> List.tryFindIndex (fun held -> held = name) with
                                        | Some solve, Some place -> ParameterValue solve.solvedMatrix.vecColumnMajor.[place]
                                        | _ -> entry.unknown.unknown.start
                                    | MaterialParameterRef _ -> entry.unknown.unknown.start)

                            let chiSquared =
                                solves
                                |> List.sumBy (fun solve -> solve.matrixFit.rmse.value * solve.matrixFit.rmse.value * float solve.ownRows.design.Length)
                                |> FitChiSquared

                            {
                                engine = LinearStudyEngine
                                layout = layout
                                members = [ ({ start = startValues; outcome = MemberConverged (fittedValues, Some chiSquared) } : FitRunMember) ]
                                fitted = StudyParameterVector.ofParameterValues fittedValues
                                objectMatrices = solves |> List.map (fun solve -> solve.matrixFit)
                            }))))

    /// What a fit's results are read from, each engine giving its own: its solves, each fitted run's residual and each
    /// series' product check (spec 0050 S11.4).
    [<ReferenceEquality>]
    type EngineProblems =
        {
            problems : FittedProblem list
            runResiduals : RunResidual list
            productChecks : SeriesProductCheck list
        }

    type StagedStudy with

        /// For a solution `fitNonlinear` answered: the study residual and its priors over the scaled coordinates it searched,
        /// at the fitted vector, and each fitted run's study residual over that run alone; it solves no series (spec 0050
        /// S11.4; 001 §L).
        member this.nonlinearProblemsWith
            (context : StudyForwardContext)
            (data : StudyData)
            (solution : StudyFitSolution)
            : Result<EngineProblems, StudyFitError> =
            let staged = { context with kit = { context.kit with items = this.items } }

            this.scaledProblemWith context data
            |> Result.bind (fun problem ->
                let atFitted : ScaledVector =
                    Array.map3
                        (fun (fitted : float) (centre : float) (unit : float) -> (fitted - centre) / unit)
                        solution.fitted.value
                        problem.centre.value
                        problem.unit.value
                    |> ScaledVector

                let solved : FittedProblem =
                    {
                        subject = StudyEntriesProblem
                        coordinates =
                            problem.layout.entries
                            |> List.map (fun entry -> EntryCoordinate [ ({ parameter = entry.unknown.parameter; owner = entry.owner } : FitEntryRef) ])
                        residual = problem.residual
                        search = { problem.search with start = atFitted }
                        centre = problem.centre
                        unit = problem.unit
                    }

                // The study residual alone, without the priors' rows, so its entries are the run's weighted ones.
                let runResidualOf (run : StudyRun) : Result<RunResidual, StudyFitError> =
                    this.study.residualWith staged problem.layout { data with runs = [ run ] } solution.fitted
                    |> Result.mapError StudyForwardFitErr
                    |> Result.map (fun residuals ->
                        let count = residuals.value.Length

                        ({
                            run = run.experiment.name
                            residuals = residuals
                            rmse = if count = 0 then ResidualRmse 0.0 else ResidualRmse (sqrt (residuals.sumOfSquares / float count))
                        } : RunResidual))

                data.runs
                |> List.filter (fun run -> List.contains run.experiment.name this.fittedRuns)
                |> List.map runResidualOf
                |> ItemEvaluation.sequenced
                |> Result.map (fun runResiduals ->
                    ({ problems = [ solved ]; runResiduals = runResiduals; productChecks = [] } : EngineProblems)))

    type ProductCheckData with

        /// The check as a fit run stores it for the series of these items: its cascade's sixteen elements row-major, its
        /// difference and its two RMSEs (spec 0050 S11.4).
        member this.storedFor (items : ItemId list) : SeriesProductCheck =
            {
                items = items
                cascade = this.cascade.rowMajorValues
                difference = this.difference
                fitRmse = this.fitRmse
                productRmse = this.productRmse
            }

    type SeriesProductCheck with

        /// The stored check's `productRmse / fitRmse`, the one place its ratio is formed (spec 0050 S11.4).
        member this.rmseRatio : ProductRmseRatio = ProductRmseRatio.between this.productRmse this.fitRmse

    type StagedStudy with

        /// For a solution `reconstructLinear` answered: each group's `reconstruct` over its own rows as its design over the
        /// matrix's sixteen elements, each fitted run's own rows' residual at its group's solve, and each series' check
        /// against the cascade of its objects' own solves (spec 0050 S11.4; 001 §L).
        member this.objectProblemsWith
            (context : StudyForwardContext)
            (prefits : ExperimentPrefit list)
            (data : StudyData)
            (solver : MuellerSolverProxy)
            : Result<EngineProblems, StudyFitError> =
            let staged = { context with kit = { context.kit with items = this.items } }
            let layout = this.study.parameterLayoutWith staged

            // The bounds of the item's own entry of that coefficient, the one its `ItemOwner` holds.
            let boundsOf (item : ItemId) (name : ParameterName) : ParameterInterval option =
                layout.entries
                |> List.tryPick (fun entry ->
                    match entry.owner, entry.unknown.parameter with
                    | ItemOwner owner, ItemParameterRef (held, coefficient) when owner = item && held = item && coefficient = name ->
                        Some entry.unknown.unknown.bounds
                    | (ItemOwner _ | MaterialOwner _ | SessionOwner _ | ExperimentOwner _), _ -> None)

            let residualsOf (system : LeastSquaresSystem) (solution : RealVector) : ResidualVector =
                let residuals = system.residualsAt solution
                Array.init system.design.Length (fun row -> residuals.[row]) |> ResidualVector

            // Coordinate k = 4c + r is the element (r, c), the order `vecColumnMajor` and the solve share.
            let problemOf (group : ObjectGroupSystem) (solved : ReconstructedMatrix) : FittedProblem =
                let system = group.system
                let solution = solved.matrix.vecColumnMajor

                let coordinates, boundsOpts =
                    match group.items with
                    | [ item ] ->
                        vecOrderCoefficients
                        |> List.map (fun name ->
                            EntryCoordinate [ ({ parameter = ItemParameterRef (item, name); owner = ItemOwner item } : FitEntryRef) ], boundsOf item name)
                        |> List.unzip
                    | _ -> List.init 16 (fun place -> ElementCoordinate (MuellerRowIndex (place % 4), MuellerColumnIndex (place / 4)), None) |> List.unzip

                let sideOf (side : ParameterInterval -> ParameterValue) (unbounded : float) : ScaledVector =
                    boundsOpts
                    |> List.map (fun boundsOpt ->
                        match boundsOpt with
                        | Some bounds -> (side bounds).value
                        | None -> unbounded)
                    |> Array.ofList
                    |> ScaledVector

                let search : ScaledSearch =
                    {
                        start = Array.init 16 (fun place -> solution.[place]) |> ScaledVector
                        lowerBounds = sideOf (fun bounds -> bounds.lower) -infinity
                        upperBounds = sideOf (fun bounds -> bounds.upper) infinity
                    }

                {
                    subject = ObjectSolveProblem group.items
                    coordinates = coordinates
                    residual = fun (ScaledVector scaled) -> residualsOf system (RealVector.create scaled)
                    search = search
                    centre = Array.zeroCreate 16 |> StudyParameterVector
                    unit = Array.create 16 1.0 |> StudyParameterVector
                }

            let runResidualsOf (group : ObjectGroupSystem) (solved : ReconstructedMatrix) : RunResidual list =
                let solution = solved.matrix.vecColumnMajor

                group.runs
                |> List.map (fun run ->
                    ({
                        run = run.run
                        residuals = residualsOf run.system solution
                        rmse = if run.system.design.Length = 0 then ResidualRmse 0.0 else run.system.residualRmseAt solution
                    } : RunResidual))

            this.objectSystemsWith context prefits data
            |> Result.bind (fun groups ->
                groups
                |> List.map (fun group ->
                    reconstruct solver group.system
                    |> Result.mapError (fun error -> ObjectSolveFitErr (group.items, error))
                    |> Result.map (fun solved -> group, solved))
                |> ItemEvaluation.sequenced)
            |> Result.map (fun solves ->
                let runResiduals = solves |> List.collect (fun (group, solved) -> runResidualsOf group solved)

                let ownMatrixOf (item : ItemId) : MuellerMatrix option =
                    solves
                    |> List.tryPick (fun (group, solved) ->
                        match group.items with
                        | [ own ] when own = item -> Some solved.matrix
                        | _ -> None)

                // Mₖ ⋯ M₂ cascaded onto M₁, so `productCheckOf`'s cascade is Mₖ ⋯ M₂ · M₁: for [QZ; LR], M_LR · M_QZ.
                let productCheckOpt (group : ObjectGroupSystem) (series : ReconstructedMatrix) : SeriesProductCheck option =
                    let ownsOpt =
                        group.items
                        |> List.fold
                            (fun (heldOpt : MuellerMatrix list option) item ->
                                match heldOpt, ownMatrixOf item with
                                | Some held, Some own -> Some (held @ [ own ])
                                | _ -> None)
                            (Some [])

                    match ownsOpt with
                    | Some (first :: second :: rest) ->
                        let later = rest |> List.fold (fun soFar next -> cascadeProduct next soFar) second
                        Some ((productCheckOf later first series group.system).storedFor group.items)
                    | Some [ _ ]
                    | Some []
                    | None -> None

                ({
                    problems = solves |> List.map (fun (group, solved) -> problemOf group solved)
                    runResiduals = this.fittedRuns |> List.choose (fun name -> runResiduals |> List.tryFind (fun residual -> residual.run = name))
                    productChecks = solves |> List.choose (fun (group, solved) -> productCheckOpt group solved)
                } : EngineProblems))

    /// What `RunFit` asks: the study, the stage (Run joint is `JointStage`, Run stage `CalibrationStage` or `ObjectStage`)
    /// and how its calibrations are used (spec 0050 S11.3, S11.5).
    type StudyFitRequest =
        {
            study : StudyName
            stage : FitStage
            calibrationUse : CalibrationUse
        }

    /// What one fit reads: the kit as the study sees it, the clock stamping its run, the SVD proxy, the deviates placing
    /// further starts, and the study's fit runs as the kit last read them (spec 0050 S11.3).
    [<ReferenceEquality>]
    type StudyFitParam =
        {
            forward : StudyForwardContext
            clock : ClockProvider
            linearSolver : MuellerSolverProxy
            deviates : UniformDeviateProvider
            fitRuns : FitRun list
        }

    type StudyFitParam with

        /// One fit: the study staged, its data checked and read, the engine `engineWith` chooses run, and the fit run
        /// stamped by the clock, read once, holding the runs, the data hashes, every parameter read and the vector used and
        /// produced; nothing is written (spec 0050 S11.3; 001 §E, §P).
        member this.runFit (request : StudyFitRequest) (watch : IterationWatch) : Result<FitRun, StudyFitError> =
            let kit = this.forward.kit

            match kit.studies |> List.tryFind (fun study -> study.name = request.study) with
            | None -> Error (UnknownFitStudyErr $"'{request.study.value}', which the fit names, is not among the kit's {kit.studies.Length} studies")
            | Some study ->
                study.stageWith kit request.stage request.calibrationUse
                |> Result.mapError FitStageErr
                |> Result.bind (fun staged ->
                    let stagedContext = { this.forward with kit = { kit with items = staged.items } }

                    // What the study's latest fit run read, which a changed hash is flagged against (step 370).
                    let lastRead : DataAttachment list =
                        match this.fitRuns |> List.filter (fun run -> run.study = study.name) with
                        | [] -> []
                        | runs -> (runs |> List.maxBy (fun run -> run.stamp)).dataRead

                    // A missing file is refused rather than recorded at a hash nobody read.
                    let readOf (prefit : ExperimentPrefit) : Result<DataAttachment option, StudyFitError> =
                        match prefit.experiment.dataOpt, prefit.changeOpt with
                        | None, _ -> Ok None
                        | Some data, Some DataUnchangedFlag -> Ok (Some data.attachment)
                        | Some data, Some (ChangedSinceAttachFlag current | ChangedSinceFitFlag current) ->
                            Ok (Some { data.attachment with contentHash = current })
                        | Some data, (Some MissingDataFileFlag | None) ->
                            let reason = $"{data.attachment.path.value} is missing, so the fit records no hash of it"
                            Error (StudyForwardFitErr (PrefitFailedErr (prefit.experiment.name, MissingDataErr reason)))

                    let fittedExperiments =
                        staged.fittedRuns |> List.choose (fun run -> kit.experiments |> List.tryFind (fun experiment -> experiment.name = run))

                    let parametersRead : Result<FitRunParameter list, StudyFitError> =
                        Experiment.itemsUsedBy fittedExperiments kit.setups staged.items
                        |> Result.mapError (fun error -> StudyForwardFitErr (ForwardSetupErr error))
                        |> Result.map (fun used ->
                            let ofItems =
                                used
                                |> List.collect (fun item ->
                                    item.parameters
                                    |> List.map (fun parameter -> ({ parameter = ItemParameterRef (item.id, parameter.name); state = parameter.state } : FitRunParameter)))

                            let ofMaterials =
                                staged.study.materials
                                |> List.collect (fun material ->
                                    material.parameters
                                    |> List.map (fun parameter ->
                                        ({ parameter = MaterialParameterRef (material.version, parameter.name); state = parameter.state } : FitRunParameter)))

                            ofItems @ ofMaterials)

                    // Every part read from the kit is settled before an engine runs, so a refusal never costs a fit.
                    staged.study.prefitWith stagedContext lastRead
                    |> Result.bind (fun prefits -> staged.study.dataWith stagedContext prefits |> Result.map (fun data -> prefits, data))
                    |> Result.mapError StudyForwardFitErr
                    |> Result.bind (fun (prefits, data) ->
                        prefits
                        |> List.map readOf
                        |> ItemEvaluation.sequenced
                        |> Result.bind (fun reads -> parametersRead |> Result.map (fun parameters -> List.choose id reads, parameters))
                        |> Result.bind (fun (dataRead, parameters) ->
                            let solved =
                                match staged.engineWith this.forward prefits, staged.stage with
                                | LinearStudyEngine, CalibrationStage -> staged.calibrateLinear this.forward prefits this.linearSolver
                                | LinearStudyEngine, (ObjectStage | JointStage) -> staged.reconstructLinear this.forward prefits data this.linearSolver
                                | NonlinearStudyEngine, _ -> staged.fitNonlinear this.forward data this.deviates watch

                            solved
                            |> Result.map (fun solution ->
                                let at = this.clock.now ()
                                let starts = solution.layout.startVector

                                let entries =
                                    solution.layout.entries
                                    |> List.mapi (fun k entry ->
                                        let index = StudyVectorIndex k

                                        ({
                                            parameter = entry.unknown.parameter
                                            owner = entry.owner
                                            start = starts.valueAt index
                                            fitted = solution.fitted.valueAt index
                                        } : FitRunEntry))

                                ({
                                    schemaVersion = FitRun.currentSchemaVersion
                                    name = study.name.fitRunNameAt at
                                    study = study.name
                                    stamp = at
                                    mode = study.settings.mode
                                    stage = request.stage
                                    calibrationUse = request.calibrationUse
                                    engine = solution.engine
                                    runs = staged.fittedRuns
                                    dataRead = dataRead
                                    parameters = parameters
                                    entries = entries
                                    members = solution.members
                                    objectMatrices = solution.objectMatrices
                                    notes = FitRunNotes.none
                                } : FitRun)))))
