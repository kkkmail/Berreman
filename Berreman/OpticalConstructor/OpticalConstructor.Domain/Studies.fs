namespace OpticalConstructor.Domain

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

        member this.describe : string =
            match this with
            | EmptyGainAnchorsErr reason -> $"the gain model names no AIR run: {reason}"
            | MissingAnchorGainErr reason -> $"the AIR run's gain is not known: {reason}"
            | FreeSessionGainErr reason -> $"the session gain is fitted, not derived from AIR runs: {reason}"

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
