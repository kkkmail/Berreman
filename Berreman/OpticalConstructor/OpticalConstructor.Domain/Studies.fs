namespace OpticalConstructor.Domain

open System
open System.Globalization
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.MuellerReconstruction

/// A study: a named set of one kit's experiments fitted together, with its unknowns, scopes, links, sessions and
/// opened materials (spec 0050 S11.1).
module Studies =

    /// Why a study, or a part of one, cannot be used (spec 0050 S11.1).
    type StudyError =
        /// An anchored gain model names no AIR run.
        | EmptyGainAnchorsErr of reason : string
        /// A gain model names an AIR run whose gain is not given.
        | MissingAnchorGainErr of reason : string
        /// A free session gain is fitted, so no AIR rule derives it.
        | FreeSessionGainErr of reason : string
        /// A blank study name.
        | InvalidStudyNameErr of reason : string
        /// A study name whose file another study of the kit already holds, ignoring case.
        | DuplicateStudyNameErr of reason : string
        /// A study names an experiment the kit does not hold.
        | UnknownStudyExperimentErr of reason : string
        /// A residual weight that is not finite or not above zero.
        | InvalidResidualWeightErr of reason : string

        member this.describe : string =
            match this with
            | EmptyGainAnchorsErr reason -> $"the gain model names no AIR run: {reason}"
            | MissingAnchorGainErr reason -> $"the AIR run's gain is not known: {reason}"
            | FreeSessionGainErr reason -> $"the session gain is fitted, not derived from AIR runs: {reason}"
            | InvalidStudyNameErr reason -> $"the study name is not valid: {reason}"
            | DuplicateStudyNameErr reason -> $"the kit already has a study of that name: {reason}"
            | UnknownStudyExperimentErr reason -> $"the kit has no such experiment: {reason}"
            | InvalidResidualWeightErr reason -> $"the residual weight is not valid: {reason}"

    /// The AIR runs that open and close a drifting session (spec 0050 S11.1).
    type AirRepeatPair =
        {
            opening : ExperimentName
            closing : ExperimentName
        }

    /// How a session's gain is known: fitted, anchored to its AIR runs, or interpolated between two AIR repeats
    /// (spec 0050 S11.1; 001 §L).
    type SessionGainModel =
        | FreeSessionGain
        | AnchoredSessionGain of anchors : ExperimentName list
        | InterpolatedSessionGain of repeats : AirRepeatPair

    /// One AIR run's fitted gain and the mean capture time of its rows, the reference `fit_air_gain` (spec 0050 S11.1).
    type AirAnchorGain =
        {
            run : ExperimentName
            gain : AirGain
            capturedAt : System.DateTimeOffset
        }

    /// A gain interpolated in time: `gainStart` at the opening AIR run's mean capture time `timeStart`, `gainEnd` at
    /// the closing run's `timeEnd` (spec 0050 S11.1).
    type GainInterpolation =
        {
            gainStart : AirGain
            timeStart : System.DateTimeOffset
            gainEnd : AirGain
            timeEnd : System.DateTimeOffset
        }

    /// The rule a session's rows are divided by: one steady gain, or one interpolated in time (spec 0050 S11.1).
    type SessionGainRule =
        | SteadyGainRule of gain : AirGain
        | InterpolatedGainRule of interpolation : GainInterpolation

        /// The gain at capture time `t`. The interpolation's weight is clipped to [0, 1] (matrix_fit_linear.py:108) and
        /// is 0 over a span that is not positive, where the reference raises (spec 0050 S11.1).
        member this.gainAt (t : System.DateTimeOffset) : AirGain =
            match this with
            | SteadyGainRule gain -> gain
            | InterpolatedGainRule interpolation ->
                let span = (interpolation.timeEnd - interpolation.timeStart).TotalSeconds
                let weight =
                    if span <= 0.0 then 0.0
                    else max 0.0 (min 1.0 ((t - interpolation.timeStart).TotalSeconds / span))
                AirGain ((1.0 - weight) * interpolation.gainStart.value + weight * interpolation.gainEnd.value)

    type SessionGainModel with

        /// The session's rule over its AIR runs' gains, each found by its run: one run's gain, the mean of more, or the
        /// interpolation from the opening run to the closing one. A free gain, no run or a run `anchors` lacks (the
        /// first, in model order) is refused (spec 0050 S11.1).
        member this.ruleWith (anchors : AirAnchorGain list) : Result<SessionGainRule, StudyError> =
            let anchorOf (run : ExperimentName) : Result<AirAnchorGain, StudyError> =
                match anchors |> List.tryFind (fun anchor -> anchor.run = run) with
                | Some anchor -> Ok anchor
                | None -> Error (MissingAnchorGainErr $"'{run.value}' is not among the {anchors.Length} AIR gains given")

            match this with
            | FreeSessionGain -> Error (FreeSessionGainErr "a free session gain is fitted with the study's unknowns")
            | AnchoredSessionGain [] -> Error (EmptyGainAnchorsErr "an anchored session gain lists no AIR run")
            | AnchoredSessionGain runs ->
                let found = runs |> List.map anchorOf

                match found |> List.tryPick (fun result -> match result with Error error -> Some error | Ok _ -> None) with
                | Some error -> Error error
                | None ->
                    match found |> List.choose Result.toOption |> List.map (fun anchor -> anchor.gain) with
                    | [ gain ] -> Ok (SteadyGainRule gain)
                    | [ a; b ] -> Ok (SteadyGainRule (AirGain (0.5 * (a.value + b.value))))
                    | gains -> Ok (SteadyGainRule (AirGain ((gains |> List.sumBy (fun gain -> gain.value)) / float gains.Length)))
            | InterpolatedSessionGain repeats ->
                anchorOf repeats.opening
                |> Result.bind (fun opening ->
                    anchorOf repeats.closing
                    |> Result.map (fun closing ->
                        InterpolatedGainRule
                            {
                                gainStart = opening.gain
                                timeStart = opening.capturedAt
                                gainEnd = closing.gain
                                timeEnd = closing.capturedAt
                            }))

    /// One session block of a study: its name and its experiments in list order (spec 0050 S11.1; 001 §L).
    type StudySession =
        {
            name : SessionName
            experiments : ExperimentName list
        }

        /// The sessions of `experiments`, in order: a hand-set session gathers its runs, the rest split at each Diagnostic
        /// run into `Session <k>` blocks, `k` the least name still free; ordered by first experiment (spec 0050 S11.1).
        static member splitAll (experiments : Experiment list) : StudySession list =
            let handSet = experiments |> List.choose (fun experiment -> experiment.sessionOpt)

            let firstFree (taken : SessionName list) : SessionName =
                let rec fromNumber (number : int) : SessionName =
                    let candidate = SessionName $"Session {number.ToString(System.Globalization.CultureInfo.InvariantCulture)}"
                    if List.contains candidate taken then fromNumber (number + 1) else candidate

                fromNumber 1

            let joined (session : SessionName) (run : ExperimentName) (sessions : StudySession list) : StudySession list =
                if sessions |> List.exists (fun held -> held.name = session) then
                    sessions
                    |> List.map (fun held -> if held.name = session then { held with experiments = held.experiments @ [ run ] } else held)
                else
                    sessions @ [ ({ name = session; experiments = [ run ] } : StudySession) ]

            // Every hand-set name is known before the fold, so a block is named when its first run opens it.
            experiments
            |> List.fold (fun (sessions : StudySession list, openBlockOpt : SessionName option) experiment ->
                match experiment.sessionOpt, experiment.role, openBlockOpt with
                | Some session, _, _ -> joined session experiment.name sessions, openBlockOpt
                | None, DiagnosticRun, _ -> sessions, None
                | None, _, Some block -> joined block experiment.name sessions, openBlockOpt
                | None, _, None ->
                    let block = firstFree (handSet @ (sessions |> List.map (fun held -> held.name)))
                    joined block experiment.name sessions, Some block) ([], None)
            |> fst

        /// The session's Dark runs in its order, or, when it holds none, every Dark run of `experiments`: one darkness
        /// file can serve every session (spec 0050 S11.1).
        member this.darkRunsIn (experiments : Experiment list) : ExperimentName list =
            let darkRuns =
                experiments
                |> List.filter (fun experiment -> experiment.role = DarkRun)
                |> List.map (fun experiment -> experiment.name)

            match this.experiments |> List.filter (fun run -> List.contains run darkRuns) with
            | [] -> darkRuns
            | own -> own

    /// A study's name, kept as given; its `<name>.study-json` stem follows the shared name rule (spec 0050 S11.1; 001 §A).
    type StudyName =
        | StudyName of string

        member this.value : string = let (StudyName text) = this in text

        static member tryCreate (text : string) : Result<StudyName, StudyError> =
            if String.IsNullOrWhiteSpace text then
                Error (InvalidStudyNameErr $"'{text}': a study name must not be blank")
            else
                Ok (StudyName text)

        member this.fileStem : FileStem = FileStem.ofText this.value

    /// A reviewer's free-text notes on a study; they live in its file (spec 0050 S11.1; 001 §E).
    type StudyNotes =
        | StudyNotes of string

        member this.value : string = let (StudyNotes text) = this in text

        static member none : StudyNotes = StudyNotes ""

    /// The sharing rule of an unknown: one value per item, per material version, per session block or per experiment;
    /// its disk tag is an int, 1 to 4 in case order (spec 0050 S11.1; 001 §L).
    type ParameterScope =
        | ItemScope
        | MaterialScope
        | SessionBlockScope
        | ExperimentScope

        member this.value : int =
            match this with
            | ItemScope -> 1
            | MaterialScope -> 2
            | SessionBlockScope -> 3
            | ExperimentScope -> 4

        /// The scope a disk tag names; any other tag is `None`, the caller's typed error (spec 0050 S11.1).
        static member tryOfWire (tag : int) : ParameterScope option =
            match tag with
            | 1 -> Some ItemScope
            | 2 -> Some MaterialScope
            | 3 -> Some SessionBlockScope
            | 4 -> Some ExperimentScope
            | _ -> None

        member this.text : string =
            match this with
            | ItemScope -> "Item"
            | MaterialScope -> "Material"
            | SessionBlockScope -> "Session"
            | ExperimentScope -> "Experiment"

        /// Every scope in tag order (spec 0050 S11.1).
        static member all : ParameterScope list = [ ItemScope; MaterialScope; SessionBlockScope; ExperimentScope ]

    /// A study's unknown by its owner: an item's parameter or an opened material version's (spec 0050 S11.1; 001 §L).
    type StudyParameterRef =
        | ItemParameterRef of item : ItemId * name : ParameterName
        | MaterialParameterRef of material : MaterialVersionId * name : ParameterName

        member this.name : ParameterName =
            match this with
            | ItemParameterRef (_, name) -> name
            | MaterialParameterRef (_, name) -> name

        /// `item <GUID> <name>` or `material <GUID> v<n> <name>` (spec 0050 S11.1).
        member this.describe : string =
            match this with
            | ItemParameterRef (item, name) -> $"item {item.value} {name.value}"
            | MaterialParameterRef (material, name) ->
                let version = material.version.value.ToString(CultureInfo.InvariantCulture)
                $"material {material.materialId.value} v{version} {name.value}"

    /// A scope the study sets on one of its unknowns (spec 0050 S11.1; 001 §L).
    type ScopedParameter =
        {
            parameter : StudyParameterRef
            scope : ParameterScope
        }

    /// An explicit constraint, `CPL2.δ = CPL1.δ`: the follower takes the leader's value (spec 0050 S11.1; 001 §L).
    type ParameterLink =
        {
            follower : StudyParameterRef
            leader : StudyParameterRef
        }

    /// How one session block's gain is known, as the study sets it (spec 0050 S11.1).
    type SessionGainSetting =
        {
            session : SessionName
            model : SessionGainModel
        }

    /// How a study's unknowns are fitted, all at once or stage by stage; its disk tag is an int, 1 to 2 in case order
    /// (spec 0050 S11.1; 001 §L; S11.3 runs them).
    type FitMode =
        | JointFit
        | StagedFit

        member this.value : int =
            match this with
            | JointFit -> 1
            | StagedFit -> 2

        /// The mode a disk tag names; any other tag is `None`, the caller's typed error (spec 0050 S11.1).
        static member tryOfWire (tag : int) : FitMode option =
            match tag with
            | 1 -> Some JointFit
            | 2 -> Some StagedFit
            | _ -> None

        member this.text : string =
            match this with
            | JointFit -> "Joint"
            | StagedFit -> "Staged"

        /// Every mode in tag order (spec 0050 S11.1).
        static member all : FitMode list = [ JointFit; StagedFit ]

    /// The factor a residual is weighted by: finite and above zero (spec 0050 S11.1; 001 §L).
    type ResidualWeight =
        | ResidualWeight of float

        member this.value : float = let (ResidualWeight weight) = this in weight

        static member tryCreate (value : float) : Result<ResidualWeight, StudyError> =
            if Double.IsFinite value && value > 0.0 then
                Ok (ResidualWeight value)
            else
                let rendered = value.ToString("R", CultureInfo.InvariantCulture)
                Error (InvalidResidualWeightErr $"{rendered} is not a finite number above zero")

        static member unit : ResidualWeight = ResidualWeight 1.0

    /// One experiment's residual weight in a study (spec 0050 S11.1; 001 §L).
    type ExperimentWeightSetting =
        {
            experiment : ExperimentName
            weight : ResidualWeight
        }

    /// One detector's residual weight in a study (spec 0050 S11.1; 001 §L).
    type DetectorWeightSetting =
        {
            detector : ItemId
            weight : ResidualWeight
        }

    /// A study's fit mode, its residual weights and the experiments it leaves out (spec 0050 S11.1; 001 §L).
    type StudyFitSettings =
        {
            mode : FitMode
            experimentWeights : ExperimentWeightSetting list
            detectorWeights : DetectorWeightSetting list
            disabled : ExperimentName list
        }

        /// A joint fit with no weight set and nothing left out (spec 0050 S11.1).
        static member initial : StudyFitSettings =
            {
                mode = JointFit
                experimentWeights = []
                detectorWeights = []
                disabled = []
            }

    /// The study-local working copy of an opened material version, stored in the study file (spec 0050 S11.1; 001 §I).
    type OpenedMaterial =
        {
            version : MaterialVersionId
            name : EntryName
            crystal : CrystalParameters
            parameters : Parameter list
        }

    /// A named set of one kit's experiments fitted together, with its opened materials, scopes, links, session gains,
    /// fit settings and notes; its fit runs are their own files (spec 0050 S11.1, S11.3; 001 §L).
    type Study =
        {
            name : StudyName
            experiments : ExperimentName list
            materials : OpenedMaterial list
            scopes : ScopedParameter list
            links : ParameterLink list
            sessionGains : SessionGainSetting list
            settings : StudyFitSettings
            notes : StudyNotes
        }

        /// CreateStudy's pure half: the study of `experiments` in order, each once, nothing opened, scoped or linked.
        /// Refused: a name whose `<name>.study-json` a `held` study's equals ignoring case, as every study file shares
        /// the kit root; then the first experiment `kitExperiments` lacks (spec 0050 S11.1; 001 §P).
        static member create
            (name : StudyName)
            (experiments : ExperimentName list)
            (held : Study list)
            (kitExperiments : Experiment list)
            : Result<Study, StudyError> =
            let sameFile (study : Study) : bool =
                String.Equals(study.name.fileStem.value, name.fileStem.value, StringComparison.OrdinalIgnoreCase)

            match held |> List.tryFind sameFile, Study.unknownExperimentAmong experiments kitExperiments with
            | Some study, _ ->
                let file = StudyKitFile.fileNameOf study.name.fileStem
                Error (DuplicateStudyNameErr $"'{name.value}' beside '{study.name.value}': both are stored as {file.value}, ignoring case")
            | None, Some unknown -> Error unknown
            | None, None ->
                Ok
                    {
                        name = name
                        experiments = List.distinct experiments
                        materials = []
                        scopes = []
                        links = []
                        sessionGains = []
                        settings = StudyFitSettings.initial
                        notes = StudyNotes.none
                    }

        /// The first of `runs` that `kitExperiments` lacks, as the refusal a study answers for it (spec 0050 S11.1).
        static member private unknownExperimentAmong (runs : ExperimentName list) (kitExperiments : Experiment list) : StudyError option =
            runs
            |> List.tryFind (fun run -> not (kitExperiments |> List.exists (fun experiment -> experiment.name = run)))
            |> Option.map (fun missing -> UnknownStudyExperimentErr $"'{missing.value}' is not among the kit's {kitExperiments.Length} experiments")

        /// AddExperimentsToStudy's pure half: the study's experiments in place, then each of `names` it lacks, in order and
        /// once, every other field kept; refused, with nothing changed, at the first name `kitExperiments` lacks
        /// (spec 0050 S11.1; 001 §P).
        member this.withExperiments (names : ExperimentName list) (kitExperiments : Experiment list) : Result<Study, StudyError> =
            match Study.unknownExperimentAmong names kitExperiments with
            | Some unknown -> Error unknown
            | None ->
                let added = names |> List.filter (fun run -> not (List.contains run this.experiments)) |> List.distinct
                Ok { this with experiments = this.experiments @ added }

        /// `StudySession.splitAll` of the study's experiments as `kitExperiments` holds them, in study order
        /// (spec 0050 S11.1).
        member this.sessionsIn (kitExperiments : Experiment list) : StudySession list =
            this.experiments
            |> List.choose (fun run -> kitExperiments |> List.tryFind (fun experiment -> experiment.name = run))
            |> StudySession.splitAll

        /// The session's gain model as the study sets it, else a free gain fitted with the unknowns (spec 0050 S11.1).
        member this.gainModelOf (session : SessionName) : SessionGainModel =
            match this.sessionGains |> List.tryFind (fun setting -> setting.session = session) with
            | Some setting -> setting.model
            | None -> FreeSessionGain

        /// What the kit snapshots and pins when it records the study: each opened material's exact version, in study
        /// order (spec 0050 S11.1; 001 §E).
        member this.libraryReferences : EntryReference list =
            this.materials
            |> List.map (fun material ->
                EntryVersionReference
                    {
                        entryId = material.version.materialId.toLibraryEntryId
                        version = material.version.version
                    })

    /// One study's versioned `<name>.study-json` content; a file is read back as its `study` (spec 0050 S11.1; 001 §A).
    type StudyFile =
        {
            schemaVersion : FileSchemaVersion
            study : Study
        }

        /// The only supported study file schema version (spec 0050 S11.1).
        static member currentSchemaVersion : FileSchemaVersion = FileSchemaVersion 1

    type Study with

        /// The study in the current study file schema (spec 0050 S11.1).
        member this.studyFile : StudyFile =
            {
                schemaVersion = StudyFile.currentSchemaVersion
                study = this
            }
