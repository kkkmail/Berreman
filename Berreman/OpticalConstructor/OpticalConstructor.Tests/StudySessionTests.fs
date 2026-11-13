namespace OpticalConstructor.Tests

open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.Studies
open Xunit

/// Spec 0050 S11.1 — a session's gain model resolves to the per-family AIR gain rules it replaced, over fixed-literal
/// AIR gains and capture times.
module StudySessionTests =

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
        let rendered =
            [ (EmptyGainAnchorsErr emptyReason).describe
              (MissingAnchorGainErr missingReason).describe
              (FreeSessionGainErr freeReason).describe ]

        Assert.Equal($"the gain model names no AIR run: {emptyReason}", rendered.[0])
        Assert.Equal($"the AIR run's gain is not known: {missingReason}", rendered.[1])
        Assert.Equal($"the session gain is fitted, not derived from AIR runs: {freeReason}", rendered.[2])
        Assert.Equal(rendered.Length, rendered |> List.distinct |> List.length)
