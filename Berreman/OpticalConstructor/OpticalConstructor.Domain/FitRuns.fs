namespace OpticalConstructor.Domain

open System.Globalization
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.Studies

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
