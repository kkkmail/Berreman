namespace OpticalConstructor.Tests

open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.Studies
open Xunit

/// Spec 0050 S11.1 — a session's gain model resolves to the per-family AIR gain rules it replaced, over fixed-literal
/// AIR gains and capture times; a study's experiments split into sessions, each with its dark runs.
module StudySessionTests =

    /// A run over `Bench` revision 1 turning one polarizer, as `ExperimentProxyTests`' E1; the session tests vary only its
    /// name, role and session.
    let private benchRun : Experiment =
        {
            id = ExperimentId 1
            name = ExperimentName "E1"
            setup = { name = SetupName "Bench"; revision = SetupRevision 1 }
            scan =
                ExperimentScan.oneAxis
                    {
                        elementId = ElementId "lp2"
                        label = ElementLabel "Linear polarizer #4"
                        variable = VaryR1
                        range = VariableRange.forVariable VaryR1
                    }
                    CaptureTransmitted
                    None
            role = ScienceRun
            sessionOpt = None
            notes = ExperimentNotes.none
            dataOpt = None
            corrections = []
        }

    let private run (name : ExperimentName) (role : ExperimentRole) (sessionOpt : SessionName option) : Experiment =
        { benchRun with name = name; role = role; sessionOpt = sessionOpt }

    let private sessionOf (name : SessionName) (experiments : ExperimentName list) : StudySession =
        { name = name; experiments = experiments }

    let private e1 = ExperimentName "E1"
    let private e2 = ExperimentName "E2"
    let private d1 = ExperimentName "D1"
    let private e3 = ExperimentName "E3"
    let private e4 = ExperimentName "E4"
    let private e5 = ExperimentName "E5"
    let private d2 = ExperimentName "D2"
    let private session1 = SessionName "Session 1"
    let private session2 = SessionName "Session 2"
    let private session3 = SessionName "Session 3"
    let private day2 = SessionName "Day 2"

    /// E1 (Science), E2 (Calibration), D1 (Diagnostic), E3 (Science), E4 (Science, hand-set `Day 2`), E5 (Dark) and D2
    /// (Diagnostic), E1 in `e1Session` when one is given.
    let private studyRuns (e1Session : SessionName option) : Experiment list =
        [ run e1 ScienceRun e1Session
          run e2 CalibrationRun None
          run d1 DiagnosticRun None
          run e3 ScienceRun None
          run e4 ScienceRun (Some day2)
          run e5 DarkRun None
          run d2 DiagnosticRun None ]

    let private atHour (hour : int) : System.DateTimeOffset =
        System.DateTimeOffset(2026, 7, 15, hour, 0, 0, System.TimeSpan.Zero)

    let private anchorOf (run : ExperimentName) (gain : AirGain) (capturedAt : System.DateTimeOffset) : AirAnchorGain =
        { run = run; gain = gain; capturedAt = capturedAt }

    let private ruleOf (model : SessionGainModel) (anchors : AirAnchorGain list) : SessionGainRule =
        match model.ruleWith anchors with
        | Ok rule -> rule
        | Error error -> failwith $"expected a gain rule, got {error.describe}"

    let private steadyGainOf (model : SessionGainModel) (anchors : AirAnchorGain list) : AirGain =
        match ruleOf model anchors with
        | SteadyGainRule gain -> gain
        | InterpolatedGainRule interpolation -> failwith $"expected a steady gain, got one from {interpolation.gainStart.value} to {interpolation.gainEnd.value}"

    [<Fact>]
    let ``a session anchored to one AIR run takes its gain, and to more the mean of their gains`` () : unit =
        let noon = atHour 12
        let lpLp = ExperimentName "LP-(AIR#0)-LP-2"
        let lpLpRepeat = ExperimentName "LP-(AIR#0)-LP-2R"
        let lpCpl = ExperimentName "LP-(AIR#0)-CPL-2"
        let lpCplRepeat = ExperimentName "LP-(AIR#0)-CPL-2R"
        let cplCplDay2 = ExperimentName "CPL-(AIR#0)-CPL-2"
        let first = ExperimentName "AIR first"
        let second = ExperimentName "AIR second"
        let third = ExperimentName "AIR third"
        let cplLpPre = ExperimentName "CPL-(AIR#0)-LP-2"
        let cplLpPost = ExperimentName "CPL-(AIR#0)-LP-2R"
        let anchors =
            [ anchorOf lpLp (AirGain 10.0) noon
              anchorOf lpLpRepeat (AirGain 20.0) noon
              anchorOf lpCpl (AirGain 4.0) noon
              anchorOf lpCplRepeat (AirGain 6.0) noon
              anchorOf cplCplDay2 (AirGain 42.0) noon
              anchorOf first (AirGain 1.0) noon
              anchorOf second (AirGain 2.0) noon
              anchorOf third (AirGain 6.0) noon
              anchorOf cplLpPre (AirGain 100.0) (atHour 10)
              anchorOf cplLpPost (AirGain 200.0) (atHour 14) ]

        let lpLpGain = steadyGainOf (AnchoredSessionGain [ lpLp; lpLpRepeat ]) anchors
        Assert.True((lpLpGain = AirGain (0.5 * (10.0 + 20.0))), $"LP-LP mean = {lpLpGain.value}")
        Assert.Equal(AirGain 15.0, lpLpGain)
        Assert.Equal(AirGain 5.0, steadyGainOf (AnchoredSessionGain [ lpCpl; lpCplRepeat ]) anchors)
        Assert.Equal(AirGain 42.0, steadyGainOf (AnchoredSessionGain [ cplCplDay2 ]) anchors)
        Assert.Equal(AirGain 3.0, steadyGainOf (AnchoredSessionGain [ first; second; third ]) anchors)

        // CPL-LP's split is two one-run sessions, each steady whatever the capture time.
        let preSplit = ruleOf (AnchoredSessionGain [ cplLpPre ]) anchors
        let postSplit = ruleOf (AnchoredSessionGain [ cplLpPost ]) anchors
        for hour in [ 8; 10; 12; 14; 16 ] do
            Assert.Equal(AirGain 100.0, preSplit.gainAt (atHour hour))
            Assert.Equal(AirGain 200.0, postSplit.gainAt (atHour hour))

    [<Fact>]
    let ``a session interpolated between two AIR repeats weighs their gains by time and clamps outside them`` () : unit =
        let opening = ExperimentName "CPL-(AIR#0)-CPL"
        let closing = ExperimentName "CPL-(AIR#0)-CPL-BR"
        let anchors = [ anchorOf opening (AirGain 30.0) (atHour 10); anchorOf closing (AirGain 50.0) (atHour 14) ]
        let rule = ruleOf (InterpolatedSessionGain { opening = opening; closing = closing }) anchors

        Assert.Equal(40.0, (rule.gainAt (atHour 12)).value, 1e-12)
        Assert.Equal(30.0, (rule.gainAt (atHour 10)).value, 1e-12)
        Assert.Equal(50.0, (rule.gainAt (atHour 14)).value, 1e-12)
        Assert.Equal(30.0, (rule.gainAt (atHour 9)).value, 1e-12)
        Assert.Equal(50.0, (rule.gainAt (atHour 15)).value, 1e-12)

    [<Fact>]
    let ``a free gain has no AIR rule, and an anchored model with no run or a missing run is refused`` () : unit =
        let present = ExperimentName "CPL-(AIR#0)-CPL"
        let absent = ExperimentName "CPL-(AIR#0)-CPL-BR"
        let anchors = [ anchorOf present (AirGain 30.0) (atHour 10) ]
        let free : SessionGainModel = FreeSessionGain

        match free.ruleWith anchors with
        | Error (FreeSessionGainErr _) -> ()
        | other -> Assert.Fail($"expected Error (FreeSessionGainErr _), got %A{other}")

        match (AnchoredSessionGain []).ruleWith anchors with
        | Error (EmptyGainAnchorsErr _) -> ()
        | other -> Assert.Fail($"expected Error (EmptyGainAnchorsErr _), got %A{other}")

        match (AnchoredSessionGain [ present; absent ]).ruleWith anchors with
        | Error (MissingAnchorGainErr reason) -> Assert.Contains(absent.value, reason)
        | other -> Assert.Fail($"expected Error (MissingAnchorGainErr _) naming {absent.value}, got %A{other}")

        match (InterpolatedSessionGain { opening = present; closing = absent }).ruleWith anchors with
        | Error (MissingAnchorGainErr reason) -> Assert.Contains(absent.value, reason)
        | other -> Assert.Fail($"expected Error (MissingAnchorGainErr _) naming {absent.value}, got %A{other}")

    [<Fact>]
    let ``every study error renders its condition and its payload`` () : unit =
        let emptyReason = "the CPL-LP session lists no run"
        let missingReason = "'CPL-(AIR#0)-CPL-BR' is not among the 1 AIR gains given"
        let freeReason = "the LP-LP session gain is an unknown"
        let nameReason = "'  ': a study name must not be blank"
        let duplicateReason = "'QZ-LR' beside 'qz-lr': both are stored as qz-lr.study-json, ignoring case"
        let experimentReason = "'E9' is not among the kit's 3 experiments"
        let weightReason = "-1 is not a finite number above zero"
        let complexityReason = "'Constant glass' (3e7a1c5b-9d2f-4a6e-b8c0-000000000001) holds a complexity tree, not a crystal-parameter record"
        let openReason = "material 36100000-0000-4000-8000-000000000001 v1 is opened as 'Probe absorbing crystal'"
        let materialReason = "material 36100000-0000-4000-8000-000000000002 v1 is not among the study's 1 opened materials"
        let parameterReason = "material 36100000-0000-4000-8000-000000000001 v1 n_o is not among the 15 parameters of 'Probe absorbing crystal'"
        let stateReason = "the parameter state is not valid: n3: the lower bound 1.8 is not below the upper bound 1.6"
        let scopeReason = "material 36100000-0000-4000-8000-000000000001 v1 n1 takes Material, not Item"
        let linkReason = "item 36200000-0000-4000-8000-000000000011 c2.retardance = item 36200000-0000-4000-8000-000000000011 c2.retardance: a parameter cannot follow itself"
        let sessionReason = "'Day 9' is not among the study's 2 sessions"
        let outsideReason = "'E3' is not among the 2 experiments of 'Session 1'"
        let itemReason = "item 36200000-0000-4000-8000-000000000013: no item of the 2 the study uses has that id"
        let studyReason = "'LR' is not among the kit's 1 studies"
        let unavailableReason = "material 36600000-0000-4000-8000-000000000009 v1 is not in the library"
        let startsReason = "0 is fewer than one start"
        let suspectReason = "the correlation 1.5 is not above zero and at most one"
        let rendered =
            [ (EmptyGainAnchorsErr emptyReason).describe
              (MissingAnchorGainErr missingReason).describe
              (FreeSessionGainErr freeReason).describe
              (InvalidStudyNameErr nameReason).describe
              (DuplicateStudyNameErr duplicateReason).describe
              (UnknownStudyExperimentErr experimentReason).describe
              (InvalidResidualWeightErr weightReason).describe
              (MaterialWithoutParametersErr complexityReason).describe
              (MaterialAlreadyOpenErr openReason).describe
              (UnknownStudyMaterialErr materialReason).describe
              (UnknownStudyParameterErr parameterReason).describe
              (InvalidStudyParameterErr stateReason).describe
              (ScopeNotAllowedErr scopeReason).describe
              (InvalidParameterLinkErr linkReason).describe
              (UnknownStudySessionErr sessionReason).describe
              (GainAnchorOutsideSessionErr outsideReason).describe
              (UnknownStudyItemErr itemReason).describe
              (UnknownStudyErr studyReason).describe
              (MaterialUnavailableErr unavailableReason).describe
              (InvalidFitStartCountErr startsReason).describe
              (InvalidSuspectThresholdErr suspectReason).describe ]

        Assert.Equal($"the gain model names no AIR run: {emptyReason}", rendered.[0])
        Assert.Equal($"the AIR run's gain is not known: {missingReason}", rendered.[1])
        Assert.Equal($"the session gain is fitted, not derived from AIR runs: {freeReason}", rendered.[2])
        Assert.Equal($"the study name is not valid: {nameReason}", rendered.[3])
        Assert.Equal($"the kit already has a study of that name: {duplicateReason}", rendered.[4])
        Assert.Equal($"the kit has no such experiment: {experimentReason}", rendered.[5])
        Assert.Equal($"the residual weight is not valid: {weightReason}", rendered.[6])
        Assert.Equal($"the material publishes no parameter to fit: {complexityReason}", rendered.[7])
        Assert.Equal($"the study already opens that material version: {openReason}", rendered.[8])
        Assert.Equal($"the study opens no such material version: {materialReason}", rendered.[9])
        Assert.Equal($"the study has no such parameter: {parameterReason}", rendered.[10])
        Assert.Equal($"the parameter state is not valid: {stateReason}", rendered.[11])
        Assert.Equal($"the parameter cannot take that scope: {scopeReason}", rendered.[12])
        Assert.Equal($"the link is not valid: {linkReason}", rendered.[13])
        Assert.Equal($"the study has no such session: {sessionReason}", rendered.[14])
        Assert.Equal($"the AIR run is not in that session: {outsideReason}", rendered.[15])
        Assert.Equal($"the study uses no such item: {itemReason}", rendered.[16])
        Assert.Equal($"the kit has no such study: {studyReason}", rendered.[17])
        Assert.Equal($"the material version could not be read: {unavailableReason}", rendered.[18])
        Assert.Equal($"the fit start count is not valid: {startsReason}", rendered.[19])
        Assert.Equal($"the suspect thresholds are not valid: {suspectReason}", rendered.[20])
        Assert.Equal(rendered.Length, rendered |> List.distinct |> List.length)

    [<Fact>]
    let ``experiments split into sessions at each diagnostic run, and a hand-set session gathers its own`` () : unit =
        Assert.Equal<StudySession list>(
            [ sessionOf session1 [ e1; e2 ]; sessionOf session2 [ e3; e5 ]; sessionOf day2 [ e4 ] ],
            StudySession.splitAll (studyRuns None))

        // A hand-set `Session 1` takes that name from the first block, which becomes `Session 2`.
        Assert.Equal<StudySession list>(
            [ sessionOf session1 [ e1 ]; sessionOf session2 [ e2 ]; sessionOf session3 [ e3; e5 ]; sessionOf day2 [ e4 ] ],
            StudySession.splitAll (studyRuns (Some session1)))

        Assert.Equal<StudySession list>([], StudySession.splitAll [ run d1 DiagnosticRun None; run d2 DiagnosticRun None ])

    [<Fact>]
    let ``a session's dark comes from its own dark runs, or else from every dark run of the study`` () : unit =
        let darkRunsOf (experiments : Experiment list) (session : SessionName) : ExperimentName list =
            match StudySession.splitAll experiments |> List.tryFind (fun held -> held.name = session) with
            | Some held -> held.darkRunsIn experiments
            | None -> failwith $"expected a session named {session.value}"

        let study = studyRuns None
        Assert.Equal<ExperimentName list>([ e5 ], darkRunsOf study session2)
        Assert.Equal<ExperimentName list>([ e5 ], darkRunsOf study session1)
        Assert.Equal<ExperimentName list>([ e5 ], darkRunsOf study day2)

        // E6 (Dark) beside E5 in `Session 2`, and E7 (Dark) after D2 opening `Session 3`.
        let e6 = ExperimentName "E6"
        let e7 = ExperimentName "E7"
        let (beforeD2, fromD2) = List.splitAt 6 study
        let twoDark = beforeD2 @ [ run e6 DarkRun None ] @ fromD2 @ [ run e7 DarkRun None ]
        Assert.Equal<ExperimentName list>([ e5; e6 ], darkRunsOf twoDark session2)
        Assert.Equal<ExperimentName list>([ e7 ], darkRunsOf twoDark session3)
        Assert.Equal<ExperimentName list>([ e5; e6; e7 ], darkRunsOf twoDark session1)
