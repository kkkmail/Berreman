namespace OpticalConstructor.Domain

open System.Globalization
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.StudyForward

/// The record of one study fit and the stage it fits (spec 0050 S11.3).
module FitRuns =

    /// What a fit run fits: every unknown at once, or one stage of a staged fit; its disk tag is an int, 1 to 3 in case
    /// order (spec 0050 S11.3; 001 §L).
    type FitStage =
        /// Every unknown at once (spec 0050 S11.3).
        | JointStage
        /// Stage 1: the instruments on the Calibration runs (spec 0050 S11.3).
        | CalibrationStage
        /// Stage 2: the objects behind the published calibration (spec 0050 S11.3).
        | ObjectStage

        member this.value : int =
            match this with
            | JointStage -> 1
            | CalibrationStage -> 2
            | ObjectStage -> 3

        /// The stage a disk tag names; any other tag is `None`, the caller's typed error (spec 0050 S11.3).
        static member tryOfWire (tag : int) : FitStage option =
            match tag with
            | 1 -> Some JointStage
            | 2 -> Some CalibrationStage
            | 3 -> Some ObjectStage
            | _ -> None

        member this.text : string =
            match this with
            | JointStage -> "Joint"
            | CalibrationStage -> "Stage 1 (calibration)"
            | ObjectStage -> "Stage 2 (objects)"

        /// Every stage in tag order (spec 0050 S11.3).
        static member all : FitStage list = [ JointStage; CalibrationStage; ObjectStage ]

    /// Whether a `CalibratedParameter` is held at its value or freed with a prior of its value and sigma; its disk tag
    /// is an int, 1 to 2 in case order (spec 0050 S11.3; 001 §L).
    type CalibrationUse =
        | CalibrationHeldFixed
        | CalibrationAsPrior

        member this.value : int =
            match this with
            | CalibrationHeldFixed -> 1
            | CalibrationAsPrior -> 2

        /// The use a disk tag names; any other tag is `None`, the caller's typed error (spec 0050 S11.3).
        static member tryOfWire (tag : int) : CalibrationUse option =
            match tag with
            | 1 -> Some CalibrationHeldFixed
            | 2 -> Some CalibrationAsPrior
            | _ -> None

        member this.text : string =
            match this with
            | CalibrationHeldFixed -> "fixed"
            | CalibrationAsPrior -> "as priors"

        /// Every use in tag order (spec 0050 S11.3).
        static member all : CalibrationUse list = [ CalibrationHeldFixed; CalibrationAsPrior ]

    /// The engine the automatic choice took; its disk tag is an int, 1 to 2 in case order (spec 0050 S11.3; 001 §L).
    type StudyEngineKind =
        | LinearStudyEngine
        | NonlinearStudyEngine

        member this.value : int =
            match this with
            | LinearStudyEngine -> 1
            | NonlinearStudyEngine -> 2

        /// The engine a disk tag names; any other tag is `None`, the caller's typed error (spec 0050 S11.3).
        static member tryOfWire (tag : int) : StudyEngineKind option =
            match tag with
            | 1 -> Some LinearStudyEngine
            | 2 -> Some NonlinearStudyEngine
            | _ -> None

        member this.text : string =
            match this with
            | LinearStudyEngine -> "linear"
            | NonlinearStudyEngine -> "nonlinear"

        /// Every engine in tag order (spec 0050 S11.3).
        static member all : StudyEngineKind list = [ LinearStudyEngine; NonlinearStudyEngine ]

    /// A reviewer's free-text notes on a fit run, empty when there are none (spec 0050 S11.3).
    type FitRunNotes =
        | FitRunNotes of string

        member this.value : string = let (FitRunNotes text) = this in text

        static member none : FitRunNotes = FitRunNotes ""

    /// The sum of a solution's squared residual entries (spec 0050 S11.3).
    type FitChiSquared =
        | FitChiSquared of float

        member this.value : float = let (FitChiSquared chiSquared) = this in chiSquared

    /// Why a multi-start member failed, as rendered (spec 0050 S11.3).
    type FitMemberFailure =
        | FitMemberFailure of string

        member this.value : string = let (FitMemberFailure text) = this in text

    /// Where a multi-start member ended: its fitted vector and its χ², or why it failed; stored in the serializer's case
    /// form, so it carries no tag (spec 0050 S11.3).
    type FitMemberOutcome =
        | MemberConverged of fitted : ParameterValue list * chiSquaredOpt : FitChiSquared option
        | MemberFailed of failure : FitMemberFailure

    /// One multi-start member: the vector it started from and where it ended (spec 0050 S11.3; 001 §L).
    type FitRunMember =
        {
            start : ParameterValue list
            outcome : FitMemberOutcome
        }

    /// One value of the fitted vector, in the layout's order: its parameter, the owner it is shared by, and where it
    /// started and ended (spec 0050 S11.3; 001 §E).
    type FitRunEntry =
        {
            parameter : StudyParameterRef
            owner : UnknownOwner
            start : ParameterValue
            fitted : ParameterValue
        }

    /// One parameter as the fit read it, with its state (spec 0050 S11.3; 001 §I).
    type FitRunParameter =
        {
            parameter : StudyParameterRef
            state : ParameterState
        }

    /// One linear object solve: the black-box items it reconstructs in light order, its sixteen elements m00 … m33
    /// row-major, and the solver's rank and RMSE (spec 0050 S11.3).
    type ObjectMatrixFit =
        {
            items : ItemId list
            elements : ParameterValue list
            rank : MatrixRank
            rmse : ResidualRmse
        }

    /// One entry of the fitted vector, as `FitRunEntry` names it: its parameter and the owner it is shared by
    /// (spec 0050 S11.4).
    type FitEntryRef =
        {
            parameter : StudyParameterRef
            owner : UnknownOwner
        }

    /// What one coordinate of a solve is the value of; stored in the serializer's case form, so it carries no tag
    /// (spec 0050 S11.4).
    type FitCoordinate =
        /// The entries it is the value of: one in the nonlinear and object solves, every entry taking that stage-one
        /// constant in a calibration fit (spec 0050 S11.4).
        | EntryCoordinate of entries : FitEntryRef list
        /// An element of a series' combined matrix, which no entry holds (spec 0050 S11.4).
        | ElementCoordinate of row : MuellerRowIndex * column : MuellerColumnIndex
        /// The free visibility of the LP-LP AIR fringe, which stage one fits and no entry holds (spec 0050 S11.4).
        | FringeVisibilityCoordinate

    /// The solve a quality is read from; stored in the serializer's case form, so it carries no tag (spec 0050 S11.4).
    type FitProblemSubject =
        /// The nonlinear fit of the study residual and its priors (spec 0050 S11.4).
        | StudyEntriesProblem
        /// One linear object solve, a series' combined matrix when it names more than one item (spec 0050 S11.4).
        | ObjectSolveProblem of items : ItemId list
        /// One stage-one AIR fringe fit of those runs (spec 0050 S11.4).
        | AirFringeProblem of runs : ExperimentName list

    /// A solve's Jacobian column norms, one per coordinate, read whole from the `ResidualJacobian` carrier
    /// (spec 0050 S11.4).
    [<Softellect.Sys.NumericCarrier("a solve's Jacobian column norms (R3): one per coordinate, read whole from the ResidualJacobian carrier and recorded whole in the fit run")>]
    type JacobianColumnNorms =
        | JacobianColumnNorms of float[]

        member this.value : float[] = let (JacobianColumnNorms norms) = this in norms

    /// A solve's parameter correlation matrix over its coordinates, read whole from the `ResidualJacobian` carrier
    /// (spec 0050 S11.4).
    [<Softellect.Sys.NumericCarrier("a solve's parameter correlation matrix (R3): one row and one column per coordinate, read whole from the ResidualJacobian carrier and recorded whole in the fit run")>]
    type CorrelationMatrix =
        | CorrelationMatrix of float[][]

        member this.value : float[][] = let (CorrelationMatrix correlation) = this in correlation

    /// One solve's identifiability in its own coordinates (the nonlinear fit's scaled ones); the condition and the
    /// correlation are present only where its Jacobian has full rank (spec 0050 S11.4; 001 §L).
    type FitProblemQuality =
        {
            subject : FitProblemSubject
            coordinates : FitCoordinate list
            rank : MatrixRank
            conditionOpt : ConditionNumber option
            columnNorms : JacobianColumnNorms
            correlationOpt : CorrelationMatrix option
            chiSquared : FitChiSquared
            rmse : ResidualRmse
        }

    /// One fitted value's ± σ, `None` where its solve leaves it undetermined (spec 0050 S11.4; 001 §L).
    type FitEntrySigma =
        {
            entry : FitEntryRef
            sigmaOpt : ParameterSigma option
        }

    /// One fitted run's residual entries, in the units of the solve that fitted it, and their root mean square
    /// (spec 0050 S11.4; 001 §L).
    type RunResidual =
        {
            run : ExperimentName
            residuals : ResidualVector
            rmse : ResidualRmse
        }

    /// Whether some optical element can produce a fitted Mueller matrix. Derived, never stored, so it carries no tag
    /// (spec 0050 S11.4).
    type MuellerRealizability =
        | RealizableMueller
        | UnrealizableMueller

    /// One black-box object's four Cloude coherency eigenvalues at its fitted values (spec 0050 S11.4; 001 §L).
    type ObjectRealizability =
        {
            item : ItemId
            eigenvalues : CoherencyEigenvalue list
        }

        /// Realizable when every eigenvalue is at least −1e-12 times their sum (the coherency trace, m₀₀): a round-off
        /// allowance on the positive semi-definite test `cloudeEigenvalues` documents (spec 0050 S11.4).
        member this.realizability : MuellerRealizability =
            let floor = -1.0e-12 * (this.eigenvalues |> List.sumBy (fun eigenvalue -> eigenvalue.value))

            if this.eigenvalues |> List.forall (fun eigenvalue -> eigenvalue.value >= floor) then RealizableMueller
            else UnrealizableMueller

    /// A series' combined matrix against the cascade of its objects' own matrices, the cascade's sixteen elements
    /// row-major, with step 352's readouts (spec 0050 S11.4; 001 §L).
    type SeriesProductCheck =
        {
            items : ItemId list
            cascade : ParameterValue list
            difference : MatrixDiff
            fitRmse : ResidualRmse
            productRmse : ResidualRmse
        }

    /// Why a fitted entry is suspect, and so profiled; stored in the serializer's case form, so it carries no tag
    /// (spec 0050 S11.4).
    type SuspectReason =
        /// Its solve leaves its σ undetermined (spec 0050 S11.4).
        | UndeterminedSigmaReason
        /// Its σ over its bounds' span, past the study's threshold (spec 0050 S11.4).
        | WideSigmaReason of fraction : SigmaFraction
        /// Its correlation with that entry, past the study's threshold (spec 0050 S11.4).
        | CorrelatedEntryReason of partner : FitEntryRef * correlation : FitCorrelation

    /// Where one profile re-fit ended: its χ², or why it failed; stored in the serializer's case form, so it carries no
    /// tag (spec 0050 S11.4).
    type ProfileOutcome =
        | ProfileConverged of chiSquared : FitChiSquared
        | ProfileFailed of failure : FitMemberFailure

    /// One re-fit with the profiled parameter held at `value` (spec 0050 S11.4).
    type ProfilePoint =
        {
            value : ParameterValue
            outcome : ProfileOutcome
        }

    /// A suspect entry's χ² profile: why it is suspect, and its re-fits (spec 0050 S11.4; 001 §L).
    type ParameterProfile =
        {
            entry : FitEntryRef
            reasons : SuspectReason list
            points : ProfilePoint list
        }

    /// A fit's results: each value's ± σ, each solve's quality, each fitted run's residuals, each black-box object's
    /// realizability, each series' product check and each suspect entry's profile (spec 0050 S11.4; 001 §L).
    type FitRunResults =
        {
            sigmas : FitEntrySigma list
            problems : FitProblemQuality list
            runResiduals : RunResidual list
            realizability : ObjectRealizability list
            productChecks : SeriesProductCheck list
            profiles : ParameterProfile list
        }

    /// One fit, append-only: the runs and data it read, the full parameter vector it used and produced, its members,
    /// its object solves and its results (spec 0050 S11.3, S11.4; 001 §E, §I, §L).
    type FitRun =
        {
            schemaVersion : FileSchemaVersion
            name : FitRunName
            study : StudyName
            stamp : ClockTime
            mode : FitMode
            stage : FitStage
            calibrationUse : CalibrationUse
            engine : StudyEngineKind
            runs : ExperimentName list
            dataRead : DataAttachment list
            parameters : FitRunParameter list
            entries : FitRunEntry list
            members : FitRunMember list
            objectMatrices : ObjectMatrixFit list
            results : FitRunResults
            notes : FitRunNotes
        }

        /// The only supported fit run file schema version (spec 0050 S11.3).
        static member currentSchemaVersion : FileSchemaVersion = FileSchemaVersion 1

    type StudyName with

        /// `<the study's file stem>@<at, UTC, as yyyyMMdd'T'HHmmss'Z'>`, e.g. `QZ-LR@20261101T120000Z`
        /// (spec 0050 S11.3; 001 §A).
        member this.fitRunNameAt (at : ClockTime) : FitRunName =
            let stamp = at.value.ToUniversalTime().ToString("yyyyMMdd'T'HHmmss'Z'", CultureInfo.InvariantCulture)
            FitRunName $"{this.fileStem.value}@{stamp}"

    type FitRunName with

        /// The stem of the run's `.fitrun-json`: its name, made filesystem-safe (spec 0050 S11.3; 001 §A).
        member this.fileStem : FileStem = FileStem.ofText this.value

    type NonlinearSolverError with

        /// Why a multi-start member failed, with the solver's reason (spec 0050 S11.3).
        member this.failureText : FitMemberFailure =
            match this with
            | DidNotConvergeErr reason -> FitMemberFailure $"the solver did not converge: {reason}"
            | InvalidNonlinearRequestErr reason -> FitMemberFailure $"the solve request is not valid: {reason}"

    /// Why a study cannot be staged for one fit (spec 0050 S11.3).
    type FitRunError =
        /// Stage 2 meets an instrument parameter that stage 1 has not published (spec 0050 S11.3; 001 §L).
        | InstrumentNotCalibratedErr of reason : string
        /// The stage has no run to fit (spec 0050 S11.3).
        | NoStageRunErr of reason : string
        /// The stage has nothing to fit (spec 0050 S11.3).
        | NoStageUnknownErr of reason : string

        member this.describe : string =
            match this with
            | InstrumentNotCalibratedErr reason -> $"an instrument parameter is not calibrated yet: {reason}"
            | NoStageRunErr reason -> $"the stage has no run to fit: {reason}"
            | NoStageUnknownErr reason -> $"the stage has nothing to fit: {reason}"

    /// The study as one stage reads it, the kit's items as the stage holds them, and the runs whose residual rows it
    /// fits; never stored, as the fit run records its stage (spec 0050 S11.3).
    type StagedStudy =
        {
            study : Study
            items : Item list
            stage : FitStage
            calibrationUse : CalibrationUse
            fittedRuns : ExperimentName list
        }

    type Study with

        /// The study staged for one fit through its own levers: runs outside the stage disabled, unknowns outside it held
        /// at their start, calibrations freed as priors within fifty sigma when asked. Refused: no run fitted, then an
        /// instrument's item-scoped unknown under stage 2, then nothing left to fit (spec 0050 S11.3; 001 §L).
        member this.stageWith (context : StudyContext) (stage : FitStage) (calibrationUse : CalibrationUse) : Result<StagedStudy, FitRunError> =
            let residualRuns = this.fittedRunsIn context.experiments

            let fitted =
                residualRuns
                |> List.filter (fun experiment ->
                    match stage, experiment.role with
                    | JointStage, _
                    | CalibrationStage, CalibrationRun
                    | ObjectStage, ScienceRun
                    | ObjectStage, DriftCheckRun -> true
                    | CalibrationStage, _
                    | ObjectStage, _ -> false)

            let fittedRuns = fitted |> List.map (fun experiment -> experiment.name)

            // Step 362's lookup of the items a study's experiments use, over the study narrowed to `runs`.
            let itemsUsedIn (runs : Experiment list) : Item list =
                context.itemsUsedBy ({ this with experiments = runs |> List.map (fun experiment -> experiment.name) } : Study)

            let isAmong (items : Item list) (item : Item) : bool = items |> List.exists (fun held -> held.id = item.id)
            let instruments = itemsUsedIn (residualRuns |> List.filter (fun experiment -> experiment.role = CalibrationRun))

            let uncalibratedOpt : FitRunError option =
                instruments
                |> List.tryPick (fun item ->
                    item.parameters
                    |> List.tryPick (fun parameter ->
                        match parameter.state, this.scopeOf (ItemParameterRef (item.id, parameter.name)) with
                        | UnknownParameter _, ItemScope ->
                            let named = $"'{item.label.value}' {parameter.name.value}"
                            Some (InstrumentNotCalibratedErr $"{named} is unknown per item, and {CalibrationStage.text} publishes it first")
                        | _ -> None))

            match fitted, stage, uncalibratedOpt with
            | [], _, _ -> Error (NoStageRunErr $"'{this.name.value}' has no enabled run that {stage.text} fits")
            | _, ObjectStage, Some uncalibrated -> Error uncalibrated
            | _ ->
                let airRuns =
                    this.sessionsIn context.experiments
                    |> List.filter (fun session -> session.experiments |> List.exists (fun run -> List.contains run fittedRuns))
                    |> List.collect (fun session ->
                        match this.gainModelOf session.name with
                        | FreeSessionGain -> []
                        | AnchoredSessionGain anchors -> anchors
                        | InterpolatedSessionGain repeats -> [ repeats.opening; repeats.closing ])

                // A Dark run gives no residual rows, so it is never disabled; each run disabled is the study's own, which
                // `withExperimentEnablement` never refuses.
                let runsStaged =
                    residualRuns
                    |> List.map (fun experiment -> experiment.name)
                    |> List.filter (fun run -> not (List.contains run fittedRuns || List.contains run airRuns))
                    |> List.fold (fun (study : Study) run -> study.withExperimentEnablement run DisabledInStudy |> Result.defaultValue study) this

                // Each component held is the opened copy's own, at its own validated start, which is never refused.
                let staged =
                    match stage with
                    | CalibrationStage ->
                        this.materials
                        |> List.fold (fun (study : Study) material ->
                            material.parameters
                            |> List.fold (fun (held : Study) parameter ->
                                match parameter.state with
                                | UnknownParameter unknown ->
                                    held.withMaterialParameterState material.version parameter.name (KnownParameter unknown.start)
                                    |> Result.defaultValue held
                                | KnownParameter _
                                | CalibratedParameter _ -> held) study) runsStaged
                    | JointStage
                    | ObjectStage -> runsStaged

                let withStates (stateOf : ParameterState -> ParameterState) (item : Item) : Item =
                    { item with parameters = item.parameters |> List.map (fun (parameter : Parameter) -> { parameter with state = stateOf parameter.state }) }

                let heldAtStart (state : ParameterState) : ParameterState =
                    match state with
                    | UnknownParameter unknown -> KnownParameter unknown.start
                    | KnownParameter _
                    | CalibratedParameter _ -> state

                // The inverse suites search `SearchBox 50.0` in scaled units; a calibration's scale is its sigma (step 387).
                let boxSigmas = 50.0

                let freedAsPrior (state : ParameterState) : ParameterState =
                    match state with
                    | CalibratedParameter calibrated ->
                        let halfWidth = boxSigmas * calibrated.sigma.value

                        UnknownParameter
                            ({
                                start = calibrated.value
                                bounds =
                                    ({
                                        lower = ParameterValue (calibrated.value.value - halfWidth)
                                        upper = ParameterValue (calibrated.value.value + halfWidth)
                                    } : ParameterInterval)
                                priorOpt = Some ({ centre = calibrated.value; sigma = calibrated.sigma } : ParameterPrior)
                            } : UnknownParameterData)
                    | KnownParameter _
                    | UnknownParameter _ -> state

                let usedByFitted = itemsUsedIn fitted

                let items =
                    context.items
                    |> List.map (fun item ->
                        match stage with
                        | CalibrationStage when not (isAmong instruments item) -> withStates heldAtStart item
                        | CalibrationStage
                        | ObjectStage
                        | JointStage -> item)
                    |> List.map (fun item ->
                        match calibrationUse with
                        | CalibrationAsPrior when isAmong usedByFitted item -> withStates freedAsPrior item
                        | CalibrationAsPrior
                        | CalibrationHeldFixed -> item)

                let stagedContext = { context with items = items }

                match staged.unknownsWith (stagedContext.itemsUsedBy staged) with
                | [] -> Error (NoStageUnknownErr $"'{this.name.value}' holds no unknown that {stage.text} fits")
                | _ ->
                    Ok
                        ({
                            study = staged
                            items = items
                            stage = stage
                            calibrationUse = calibrationUse
                            fittedRuns = fittedRuns
                        } : StagedStudy)
