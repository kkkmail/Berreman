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

    /// One fit, append-only: the runs and data it read, the full parameter vector it used and produced, its members
    /// and its object solves (spec 0050 S11.3; 001 §E, §I).
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
