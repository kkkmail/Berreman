namespace OpticalConstructor.Domain

open System
open System.Globalization
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.MuellerInverse

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
        /// A study names an experiment the kit does not hold, or a setting one the study does not.
        | UnknownStudyExperimentErr of reason : string
        /// A residual weight that is not finite or not above zero.
        | InvalidResidualWeightErr of reason : string
        /// A material whose physics is a complexity tree, which publishes no parameter axis.
        | MaterialWithoutParametersErr of reason : string
        /// A material version the study has already opened.
        | MaterialAlreadyOpenErr of reason : string
        /// A material version the study has not opened.
        | UnknownStudyMaterialErr of reason : string
        /// A parameter name that no component of an opened material has.
        | UnknownStudyParameterErr of reason : string
        /// A parameter state that breaks its rules.
        | InvalidStudyParameterErr of reason : string
        /// A scope the parameter's kind does not take, or one set on a linked parameter.
        | ScopeNotAllowedErr of reason : string
        /// A link to itself, a chain, a second link of one follower, or two parameters of differing scopes.
        | InvalidParameterLinkErr of reason : string
        /// A gain model set for a session the study's experiments do not form.
        | UnknownStudySessionErr of reason : string
        /// A gain model's AIR run that is no experiment of its session.
        | GainAnchorOutsideSessionErr of reason : string
        /// A detector weight for an item the study's experiments do not use.
        | UnknownStudyItemErr of reason : string

        member this.describe : string =
            match this with
            | EmptyGainAnchorsErr reason -> $"the gain model names no AIR run: {reason}"
            | MissingAnchorGainErr reason -> $"the AIR run's gain is not known: {reason}"
            | FreeSessionGainErr reason -> $"the session gain is fitted, not derived from AIR runs: {reason}"
            | InvalidStudyNameErr reason -> $"the study name is not valid: {reason}"
            | DuplicateStudyNameErr reason -> $"the kit already has a study of that name: {reason}"
            | UnknownStudyExperimentErr reason -> $"the kit has no such experiment: {reason}"
            | InvalidResidualWeightErr reason -> $"the residual weight is not valid: {reason}"
            | MaterialWithoutParametersErr reason -> $"the material publishes no parameter to fit: {reason}"
            | MaterialAlreadyOpenErr reason -> $"the study already opens that material version: {reason}"
            | UnknownStudyMaterialErr reason -> $"the study opens no such material version: {reason}"
            | UnknownStudyParameterErr reason -> $"the study has no such parameter: {reason}"
            | InvalidStudyParameterErr reason -> $"the parameter state is not valid: {reason}"
            | ScopeNotAllowedErr reason -> $"the parameter cannot take that scope: {reason}"
            | InvalidParameterLinkErr reason -> $"the link is not valid: {reason}"
            | UnknownStudySessionErr reason -> $"the study has no such session: {reason}"
            | GainAnchorOutsideSessionErr reason -> $"the AIR run is not in that session: {reason}"
            | UnknownStudyItemErr reason -> $"the study uses no such item: {reason}"

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

        /// `<follower> = <leader>`, each by its `describe` (spec 0050 S11.1).
        member this.describe : string = $"{this.follower.describe} = {this.leader.describe}"

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

    /// Whether a study fits an experiment or leaves it out (ablation). Not stored: the study keeps its `disabled` list
    /// (spec 0050 S11.1; 001 §L).
    type ExperimentEnablement =
        | EnabledInStudy
        | DisabledInStudy

    type ParameterAxis<'Parameters> with

        /// The axis's read of `p` as a parameter's number: the one conversion from an axis read (spec 0050 S11.1).
        member this.parameterValueOf (p : 'Parameters) : ParameterValue = ParameterValue (this.read p)

        /// `p` with the axis written at a parameter's number: the one conversion back (spec 0050 S11.1).
        member this.writeValue (value : ParameterValue) (p : 'Parameters) : 'Parameters = this.write value.value p

    type CrystalParameters with

        /// `axis` of a record lifted onto the crystal: the same name and field case, reading the record `recordOf`
        /// finds, else `own`, and writing back `wrap` of it; a crystal of another case is left unchanged (spec 0050 S11.1).
        static member private liftedAxis
            (own : 'Record)
            (recordOf : CrystalParameters -> 'Record option)
            (wrap : 'Record -> CrystalParameters)
            (axis : ParameterAxis<'Record>)
            : ParameterAxis<CrystalParameters> =
            let lifted (access : ParameterAccess<'Record, 'Value>) : ParameterAccess<CrystalParameters, 'Value> =
                {
                    read = fun crystal -> access.read (recordOf crystal |> Option.defaultValue own)
                    write =
                        fun value crystal ->
                            match recordOf crystal with
                            | Some record -> wrap (access.write value record)
                            | None -> crystal
                }

            {
                name = axis.name
                field =
                    match axis.field with
                    | RefractionIndexField access -> RefractionIndexField (lifted access)
                    | RhoValueField access -> RhoValueField (lifted access)
                    | EpsValueField access -> EpsValueField (lifted access)
            }

        static member private uniaxialAxis (own : UniaxialParameters) (axis : ParameterAxis<UniaxialParameters>) : ParameterAxis<CrystalParameters> =
            let recordOf (crystal : CrystalParameters) : UniaxialParameters option =
                match crystal with
                | UniaxialCrystal parameters -> Some parameters
                | TriclinicCrystal _
                | AbsorbingTriclinicCrystal _ -> None

            CrystalParameters.liftedAxis own recordOf UniaxialCrystal axis

        static member private triclinicAxis (own : TriclinicParameters) (axis : ParameterAxis<TriclinicParameters>) : ParameterAxis<CrystalParameters> =
            let recordOf (crystal : CrystalParameters) : TriclinicParameters option =
                match crystal with
                | TriclinicCrystal parameters -> Some parameters
                | UniaxialCrystal _
                | AbsorbingTriclinicCrystal _ -> None

            CrystalParameters.liftedAxis own recordOf TriclinicCrystal axis

        static member private absorbingTriclinicAxis
            (own : AbsorbingTriclinicParameters)
            (axis : ParameterAxis<AbsorbingTriclinicParameters>)
            : ParameterAxis<CrystalParameters> =
            let recordOf (crystal : CrystalParameters) : AbsorbingTriclinicParameters option =
                match crystal with
                | AbsorbingTriclinicCrystal parameters -> Some parameters
                | UniaxialCrystal _
                | TriclinicCrystal _ -> None

            CrystalParameters.liftedAxis own recordOf AbsorbingTriclinicCrystal axis

        /// The record's axes in their order, generated onto the crystal; a study applies them to its own working copy
        /// only (spec 0050 S11.1; 001 §N).
        member this.parameterAxes : ParameterAxis<CrystalParameters> list =
            match this with
            | UniaxialCrystal own -> UniaxialParameters.axes |> List.map (CrystalParameters.uniaxialAxis own)
            | TriclinicCrystal own -> TriclinicParameters.axes |> List.map (CrystalParameters.triclinicAxis own)
            | AbsorbingTriclinicCrystal own -> AbsorbingTriclinicParameters.axes |> List.map (CrystalParameters.absorbingTriclinicAxis own)

    /// The study-local working copy of an opened material version, stored in the study file (spec 0050 S11.1; 001 §I).
    type OpenedMaterial =
        {
            version : MaterialVersionId
            name : EntryName
            crystal : CrystalParameters
            parameters : Parameter list
        }

        /// OpenMaterial's working copy: one known parameter per generated axis of the crystal, in order; a complexity
        /// tree publishes no axis and is refused (spec 0050 S11.1; 001 §I, §P).
        static member openFrom (version : MaterialVersionId) (entry : MaterialEntry) : Result<OpenedMaterial, StudyError> =
            match entry.physics with
            | CrystalParameterPhysics crystal ->
                Ok
                    {
                        version = version
                        name = entry.name
                        crystal = crystal
                        parameters =
                            crystal.parameterAxes
                            |> List.map (fun axis -> ({ name = axis.name; state = KnownParameter (axis.parameterValueOf crystal) } : Parameter))
                    }
            | ComplexityPhysics _ ->
                Error (MaterialWithoutParametersErr $"'{entry.name.value}' ({entry.id.value}) holds a complexity tree, not a crystal-parameter record")

        /// The copy with that component's state replaced in place, so freeing one is its unknown state; refused when no
        /// component has the name or the state breaks its rules (spec 0050 S11.1; 001 §I).
        member this.withParameterState (name : ParameterName) (state : ParameterState) : Result<OpenedMaterial, StudyError> =
            match this.parameters |> List.tryFind (fun parameter -> parameter.name = name) with
            | None ->
                let reference = MaterialParameterRef (this.version, name)
                Error (UnknownStudyParameterErr $"{reference.describe} is not among the {this.parameters.Length} parameters of '{this.name.value}'")
            | Some _ ->
                ({ name = name; state = state } : Parameter).validate
                |> Result.mapError (fun error -> InvalidStudyParameterErr error.describe)
                |> Result.map (fun parameter ->
                    { this with parameters = this.parameters |> List.map (fun held -> if held.name = name then parameter else held) })

        /// The crystal with each component written through its generated axis at its parameter's current number, in
        /// order: a known or calibrated value, an unknown's start (spec 0050 S11.1).
        member this.currentCrystal : CrystalParameters =
            let axes = this.crystal.parameterAxes

            this.parameters
            |> List.fold (fun (crystal : CrystalParameters) (parameter : Parameter) ->
                match axes |> List.tryFind (fun axis -> axis.name = parameter.name) with
                | Some axis -> axis.writeValue parameter.state.currentValue crystal
                | None -> crystal) this.crystal

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

        /// `entry` in place of the entry of its key, else appended: how a study sets a scope or a setting
        /// (spec 0050 S11.1).
        static member private replacedOrAppended<'Entry, 'Key when 'Key : equality>
            (keyOf : 'Entry -> 'Key)
            (entry : 'Entry)
            (entries : 'Entry list)
            : 'Entry list =
            match entries |> List.tryFindIndex (fun held -> keyOf held = keyOf entry) with
            | Some index -> entries |> List.updateAt index entry
            | None -> entries @ [ entry ]

        /// The refusal of a setting for an experiment the study does not hold, else `None` (spec 0050 S11.1).
        member private this.unheldExperimentOpt (experiment : ExperimentName) : StudyError option =
            if List.contains experiment this.experiments then
                None
            else
                Some (UnknownStudyExperimentErr $"'{experiment.value}' is not among the study's {this.experiments.Length} experiments")

        /// AddExperimentsToStudy's pure half: the study's experiments in place, then each of `names` it lacks, in order and
        /// once, every other field kept; refused, with nothing changed, at the first name `kitExperiments` lacks
        /// (spec 0050 S11.1; 001 §P).
        member this.withExperiments (names : ExperimentName list) (kitExperiments : Experiment list) : Result<Study, StudyError> =
            match Study.unknownExperimentAmong names kitExperiments with
            | Some unknown -> Error unknown
            | None ->
                let added = names |> List.filter (fun run -> not (List.contains run this.experiments)) |> List.distinct
                Ok { this with experiments = this.experiments @ added }

        /// OpenMaterial's pure half: the study with the version's working copy appended; refused when the study already
        /// opens the version or `entry` publishes no parameter (spec 0050 S11.1; 001 §I, §P).
        member this.openMaterial (version : MaterialVersionId) (entry : MaterialEntry) : Result<Study, StudyError> =
            match this.materials |> List.tryFind (fun material -> material.version = version) with
            | Some held -> Error (MaterialAlreadyOpenErr $"material {version.materialId.value} v{version.version.value} is opened as '{held.name.value}'")
            | None ->
                OpenedMaterial.openFrom version entry
                |> Result.map (fun material -> { this with materials = this.materials @ [ material ] })

        /// The study with that opened version's component state set in place by its working copy; refused when the
        /// study does not open the version (spec 0050 S11.1; 001 §I).
        member this.withMaterialParameterState
            (version : MaterialVersionId)
            (name : ParameterName)
            (state : ParameterState)
            : Result<Study, StudyError> =
            match this.materials |> List.tryFind (fun material -> material.version = version) with
            | None ->
                Error (UnknownStudyMaterialErr $"material {version.materialId.value} v{version.version.value} is not among the study's {this.materials.Length} opened materials")
            | Some material ->
                material.withParameterState name state
                |> Result.map (fun changed ->
                    { this with materials = this.materials |> List.map (fun held -> if held.version = version then changed else held) })

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

        /// The study with that session's gain model replaced in place, else appended. Refused, nothing changed: a session
        /// `sessionsIn kitExperiments` lacks, then the first AIR run of the model (an anchor, or the opening then the
        /// closing repeat) that is no experiment of the session (spec 0050 S11.1; 001 §L).
        member this.withSessionGainModel
            (session : SessionName)
            (model : SessionGainModel)
            (kitExperiments : Experiment list)
            : Result<Study, StudyError> =
            let sessions = this.sessionsIn kitExperiments

            match sessions |> List.tryFind (fun held -> held.name = session) with
            | None -> Error (UnknownStudySessionErr $"'{session.value}' is not among the study's {sessions.Length} sessions")
            | Some held ->
                let runs : ExperimentName list =
                    match model with
                    | FreeSessionGain -> []
                    | AnchoredSessionGain anchors -> anchors
                    | InterpolatedSessionGain repeats -> [ repeats.opening; repeats.closing ]

                match runs |> List.tryFind (fun run -> not (List.contains run held.experiments)) with
                | Some outside ->
                    Error (GainAnchorOutsideSessionErr $"'{outside.value}' is not among the {held.experiments.Length} experiments of '{session.value}'")
                | None ->
                    let setting : SessionGainSetting = { session = session; model = model }
                    let gains = this.sessionGains |> Study.replacedOrAppended (fun (entry : SessionGainSetting) -> entry.session) setting
                    Ok { this with sessionGains = gains }

        /// The study fitted jointly or in stages (spec 0050 S11.1, S11.3).
        member this.withFitMode (mode : FitMode) : Study = { this with settings = { this.settings with mode = mode } }

        /// The study with the experiment's residual weight replaced in place, else appended; refused, nothing changed,
        /// for an experiment the study does not hold (spec 0050 S11.1; 001 §L).
        member this.withExperimentWeight (experiment : ExperimentName) (weight : ResidualWeight) : Result<Study, StudyError> =
            match this.unheldExperimentOpt experiment with
            | Some unheld -> Error unheld
            | None ->
                let setting : ExperimentWeightSetting = { experiment = experiment; weight = weight }
                let weights =
                    this.settings.experimentWeights
                    |> Study.replacedOrAppended (fun (entry : ExperimentWeightSetting) -> entry.experiment) setting

                Ok { this with settings = { this.settings with experimentWeights = weights } }

        /// The study with the experiment left out (listed in `disabled` once) or fitted again (no longer listed); refused,
        /// nothing changed, for an experiment the study does not hold (spec 0050 S11.1; 001 §L).
        member this.withExperimentEnablement (experiment : ExperimentName) (enablement : ExperimentEnablement) : Result<Study, StudyError> =
            match this.unheldExperimentOpt experiment with
            | Some unheld -> Error unheld
            | None ->
                let disabled =
                    match enablement with
                    | DisabledInStudy when List.contains experiment this.settings.disabled -> this.settings.disabled
                    | DisabledInStudy -> this.settings.disabled @ [ experiment ]
                    | EnabledInStudy -> this.settings.disabled |> List.filter (fun run -> run <> experiment)

                Ok { this with settings = { this.settings with disabled = disabled } }

        /// The study with the detector's residual weight replaced in place, else appended; refused, nothing changed, for
        /// an item outside `used`, the items the study's experiments use (spec 0050 S11.1; 001 §L).
        member this.withDetectorWeight (detector : ItemId) (weight : ResidualWeight) (used : Item list) : Result<Study, StudyError> =
            if used |> List.exists (fun item -> item.id = detector) then
                let setting : DetectorWeightSetting = { detector = detector; weight = weight }
                let weights =
                    this.settings.detectorWeights
                    |> Study.replacedOrAppended (fun (entry : DetectorWeightSetting) -> entry.detector) setting

                Ok { this with settings = { this.settings with detectorWeights = weights } }
            else
                Error (UnknownStudyItemErr $"item {detector.value}: no item of the {used.Length} the study uses has that id")

        /// The study with the reviewer's notes in place of its own (spec 0050 S11.1; 001 §E).
        member this.withNotes (notes : StudyNotes) : Study = { this with notes = notes }

        /// The study's experiments without the disabled ones, in study order (spec 0050 S11.1; 001 §L).
        member this.enabledExperiments : ExperimentName list =
            this.experiments |> List.filter (fun run -> not (List.contains run this.settings.disabled))

        /// The weight the residual rows of the experiment read by the detector take: the experiment's weight times the
        /// detector's, each `ResidualWeight.unit` when unset (spec 0050 S11.1, S11.2).
        member this.residualWeightOf (experiment : ExperimentName) (detector : ItemId) : ResidualWeight =
            let experimentWeight =
                match this.settings.experimentWeights |> List.tryFind (fun setting -> setting.experiment = experiment) with
                | Some setting -> setting.weight
                | None -> ResidualWeight.unit

            let detectorWeight =
                match this.settings.detectorWeights |> List.tryFind (fun setting -> setting.detector = detector) with
                | Some setting -> setting.weight
                | None -> ResidualWeight.unit

            ResidualWeight (experimentWeight.value * detectorWeight.value)

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

    type Sample with

        /// The study's working copies of the material versions the sample is made of, in study order: every sample of an
        /// opened material reads the same copy (`MaterialScope`) (spec 0050 S11.1; 001 §I).
        member this.openedMaterialsIn (study : Study) : OpenedMaterial list =
            let referenced = this.structure.referencedMaterialVersions
            study.materials |> List.filter (fun material -> referenced.Contains material.version)

    /// What holds one value of an unknown: its item, its material version, a session block or an experiment. Derived,
    /// never stored, so it carries no tag (spec 0050 S11.1; 001 §L).
    type UnknownOwner =
        | ItemOwner of ItemId
        | MaterialOwner of MaterialVersionId
        | SessionOwner of SessionName
        | ExperimentOwner of ExperimentName

    /// One unknown of a study: its parameter, the scope its values are shared by, and its start, bounds and prior
    /// (spec 0050 S11.1; 001 §L).
    type StudyUnknown =
        {
            parameter : StudyParameterRef
            scope : ParameterScope
            unknown : UnknownParameterData
        }

    type Study with

        /// The study's scope for the parameter, else the default: a material component's is `MaterialScope`, a
        /// detector's `gain` `SessionBlockScope`, any other item parameter's `ItemScope` (spec 0050 S11.1; 001 §I, §L).
        member this.scopeOf (parameter : StudyParameterRef) : ParameterScope =
            match this.scopes |> List.tryFind (fun scoped -> scoped.parameter = parameter) with
            | Some scoped -> scoped.scope
            | None ->
                match parameter with
                | MaterialParameterRef _ -> MaterialScope
                | ItemParameterRef (_, name) when name = ParameterName.gain -> SessionBlockScope
                | ItemParameterRef _ -> ItemScope

        /// The refusal of a parameter that no item of `used` with its id, or no opened copy of its version, holds; else
        /// `None` (spec 0050 S11.1).
        member private this.missingParameterOpt (reference : StudyParameterRef) (used : Item list) : StudyError option =
            match reference with
            | ItemParameterRef (itemId, name) ->
                match used |> List.tryFind (fun item -> item.id = itemId) with
                | None -> Some (UnknownStudyParameterErr $"{reference.describe}: no item of the {used.Length} the study uses has that id")
                | Some item ->
                    match item.tryFindParameter name with
                    | Some _ -> None
                    | None ->
                        Some (UnknownStudyParameterErr $"{reference.describe} is not among the {item.parameters.Length} parameters of '{item.label.value}'")
            | MaterialParameterRef (version, name) ->
                match this.materials |> List.tryFind (fun material -> material.version = version) with
                | None -> Some (UnknownStudyParameterErr $"{reference.describe}: the study opens no copy of that version among its {this.materials.Length}")
                | Some material ->
                    match material.parameters |> List.tryFind (fun parameter -> parameter.name = name) with
                    | Some _ -> None
                    | None ->
                        Some (UnknownStudyParameterErr $"{reference.describe} is not among the {material.parameters.Length} parameters of '{material.name.value}'")

        /// SetParameterScope's pure half: `scoped` in place of the parameter's entry, else appended. Refused, nothing changed:
        /// a parameter of no item of `used` or opened copy, a scope its kind does not take (an item's Item, Session or
        /// Experiment; a material's Material) and a linked one, as a link joins one scope (spec 0050 S11.1; 001 §L, §P).
        member this.withParameterScope (scoped : ScopedParameter) (used : Item list) : Result<Study, StudyError> =
            let reference = scoped.parameter

            let allowed : ParameterScope list =
                match reference with
                | ItemParameterRef _ -> [ ItemScope; SessionBlockScope; ExperimentScope ]
                | MaterialParameterRef _ -> [ MaterialScope ]

            let linkOpt = this.links |> List.tryFind (fun link -> link.follower = reference || link.leader = reference)

            match this.missingParameterOpt reference used, linkOpt with
            | Some unknown, _ -> Error unknown
            | None, _ when not (List.contains scoped.scope allowed) ->
                let taken = allowed |> List.map (fun scope -> scope.text) |> String.concat ", "
                Error (ScopeNotAllowedErr $"{reference.describe} takes {taken}, not {scoped.scope.text}")
            | None, Some link ->
                Error (ScopeNotAllowedErr $"{reference.describe} is linked, {link.describe}, and a link joins parameters of one scope")
            | None, None ->
                let scopes = this.scopes |> Study.replacedOrAppended (fun (entry : ScopedParameter) -> entry.parameter) scoped
                Ok { this with scopes = scopes }

        /// LinkParameters' pure half: the study with `link` appended. Refused, nothing changed: a parameter it lacks, then a
        /// self link, a follower already linked, a leader that follows (no chain, so a follower reads its leader directly)
        /// and differing scopes, as a link equates one value per owner (spec 0050 S11.1; 001 §L, §P).
        member this.withLink (link : ParameterLink) (used : Item list) : Result<Study, StudyError> =
            let refused (rule : string) : Result<Study, StudyError> = Error (InvalidParameterLinkErr $"{link.describe}: {rule}")
            let followerHeldOpt = this.links |> List.tryFind (fun held -> held.follower = link.follower || held.leader = link.follower)
            let leaderHeldOpt = this.links |> List.tryFind (fun held -> held.follower = link.leader)

            match this.missingParameterOpt link.follower used, this.missingParameterOpt link.leader used with
            | Some missing, _
            | None, Some missing -> Error missing
            | None, None when link.follower = link.leader -> refused "a parameter cannot follow itself"
            | None, None ->
                match followerHeldOpt, leaderHeldOpt with
                | Some held, _ when held.follower = link.follower -> refused $"the follower already follows in {held.describe}, and a follower takes one leader"
                | Some held, _ -> refused $"the follower leads {held.describe}, and a leader follows no one"
                | None, Some held -> refused $"the leader follows in {held.describe}, and a link does not chain"
                | None, None ->
                    match this.scopeOf link.follower, this.scopeOf link.leader with
                    | followerScope, leaderScope when followerScope <> leaderScope ->
                        refused $"the follower is per {followerScope.text}, the leader per {leaderScope.text}, and a link equates one value per owner"
                    | _ -> Ok { this with links = this.links @ [ link ] }

        /// The unknown parameters of `used` (the items the study's experiments use), each item's in order, then each
        /// opened material's, with their scopes; a link's follower takes its leader's value, so it is left out
        /// (spec 0050 S11.1; 001 §L).
        member this.unknownsWith (used : Item list) : StudyUnknown list =
            let unknownOf (reference : StudyParameterRef) (parameter : Parameter) : StudyUnknown option =
                match parameter.state with
                | UnknownParameter unknown when not (this.links |> List.exists (fun link -> link.follower = reference)) ->
                    Some ({ parameter = reference; scope = this.scopeOf reference; unknown = unknown } : StudyUnknown)
                | UnknownParameter _
                | KnownParameter _
                | CalibratedParameter _ -> None

            let itemUnknowns =
                used
                |> List.collect (fun item ->
                    item.parameters |> List.choose (fun parameter -> unknownOf (ItemParameterRef (item.id, parameter.name)) parameter))

            let materialUnknowns =
                this.materials
                |> List.collect (fun material ->
                    material.parameters
                    |> List.choose (fun parameter -> unknownOf (MaterialParameterRef (material.version, parameter.name)) parameter))

            itemUnknowns @ materialUnknowns

    type StudyUnknown with

        /// One owner per value the fitter holds: an item or material scope's item or material; per session block each of
        /// `sessions` holding one of `users`, in order; per experiment each of `users` once, in order. `users` are the
        /// enabled experiments whose setups use the item or a sample of its material (spec 0050 S11.1, S11.2).
        member this.owners (sessions : StudySession list) (users : ExperimentName list) : UnknownOwner list =
            match this.scope with
            | ItemScope
            | MaterialScope ->
                match this.parameter with
                | ItemParameterRef (item, _) -> [ ItemOwner item ]
                | MaterialParameterRef (material, _) -> [ MaterialOwner material ]
            | SessionBlockScope ->
                sessions
                |> List.filter (fun session -> session.experiments |> List.exists (fun run -> List.contains run users))
                |> List.map (fun session -> SessionOwner session.name)
            | ExperimentScope -> users |> List.distinct |> List.map ExperimentOwner
