namespace OpticalConstructor.Tests

open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Studies
open Xunit

/// Spec 0050 S11.1 — a study's shape: its name and file stem, its scope and fit-mode tags, its residual weights,
/// CreateStudy's and AddExperimentsToStudy's pure halves over a kit's experiments, and the sessions and gain models a
/// study reads from them.
module StudyTests =

    /// A run over `Bench` revision 1 turning one polarizer, as `StudySessionTests`' is; the study tests vary only its
    /// name and role.
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

    let private run (name : ExperimentName) (role : ExperimentRole) : Experiment =
        { benchRun with name = name; role = role }

    let private e1 = ExperimentName "E1"
    let private e2 = ExperimentName "E2"
    let private e3 = ExperimentName "E3"
    let private e9 = ExperimentName "E9"
    let private d1 = ExperimentName "D1"
    let private session1 = SessionName "Session 1"
    let private session2 = SessionName "Session 2"

    /// E1, E2 and E3 (Science), the kit's experiments in its order.
    let private kitRuns : Experiment list = [ run e1 ScienceRun; run e2 ScienceRun; run e3 ScienceRun ]

    let private studyOf (result : Result<Study, StudyError>) : Study =
        match result with
        | Ok study -> study
        | Error error -> failwith $"expected a study, got {error.describe}"

    [<Fact>]
    let ``a study name keeps its text, refuses a blank one and takes its file stem from the shared name rule`` () : unit =
        match StudyName.tryCreate "QZ:LR" with
        | Ok name ->
            Assert.Equal("QZ:LR", name.value)
            Assert.Equal(FileStem "QZ_LR", name.fileStem)
            Assert.Equal(FileStem.ofText "QZ:LR", name.fileStem)
        | Error error -> Assert.Fail($"expected a study name, got {error.describe}")

        for blank in [ ""; "  " ] do
            match StudyName.tryCreate blank with
            | Error (InvalidStudyNameErr _) -> ()
            | other -> Assert.Fail($"expected Error (InvalidStudyNameErr _) for '{blank}', got %A{other}")

    [<Fact>]
    let ``a parameter scope and a fit mode round-trip their wire tags and render their text`` () : unit =
        Assert.Equal<ParameterScope list>([ ItemScope; MaterialScope; SessionBlockScope; ExperimentScope ], ParameterScope.all)
        Assert.Equal<int list>([ 1; 2; 3; 4 ], ParameterScope.all |> List.map (fun scope -> scope.value))
        Assert.Equal<string list>([ "Item"; "Material"; "Session"; "Experiment" ], ParameterScope.all |> List.map (fun scope -> scope.text))

        for scope in ParameterScope.all do
            Assert.Equal<ParameterScope option>(Some scope, ParameterScope.tryOfWire scope.value)

        for tag in [ 0; 5 ] do
            Assert.Equal<ParameterScope option>(None, ParameterScope.tryOfWire tag)

        Assert.Equal<FitMode list>([ JointFit; StagedFit ], FitMode.all)
        Assert.Equal<int list>([ 1; 2 ], FitMode.all |> List.map (fun mode -> mode.value))
        Assert.Equal<string list>([ "Joint"; "Staged" ], FitMode.all |> List.map (fun mode -> mode.text))

        for mode in FitMode.all do
            Assert.Equal<FitMode option>(Some mode, FitMode.tryOfWire mode.value)

        for tag in [ 0; 3 ] do
            Assert.Equal<FitMode option>(None, FitMode.tryOfWire tag)

    [<Fact>]
    let ``a residual weight refuses a non-finite or non-positive value`` () : unit =
        Assert.Equal(1.0, ResidualWeight.unit.value)

        for kept in [ 0.5; 2.0 ] do
            match ResidualWeight.tryCreate kept with
            | Ok weight -> Assert.Equal(kept, weight.value)
            | Error error -> Assert.Fail($"expected the weight {kept} kept, got {error.describe}")

        for refused in [ nan; infinity; 0.0; -1.0 ] do
            match ResidualWeight.tryCreate refused with
            | Error (InvalidResidualWeightErr _) -> ()
            | other -> Assert.Fail($"expected Error (InvalidResidualWeightErr _) for {refused}, got %A{other}")

    [<Fact>]
    let ``a study is created over the kit's experiments in order, each once, with the initial settings, and refuses a held name or an experiment the kit lacks`` () : unit =
        let name = StudyName "QZ-LR"
        let study = studyOf (Study.create name [ e2; e1; e2 ] [] kitRuns)

        Assert.Equal(name, study.name)
        Assert.Equal<ExperimentName list>([ e2; e1 ], study.experiments)
        Assert.Empty(study.materials)
        Assert.Empty(study.scopes)
        Assert.Empty(study.links)
        Assert.Empty(study.sessionGains)
        Assert.Equal(StudyFitSettings.initial, study.settings)
        Assert.Equal(JointFit, study.settings.mode)
        Assert.Empty(study.settings.experimentWeights)
        Assert.Empty(study.settings.detectorWeights)
        Assert.Empty(study.settings.disabled)
        Assert.Equal(StudyNotes.none, study.notes)

        let held = studyOf (Study.create (StudyName "qz-lr") [ e3 ] [] kitRuns)

        match Study.create name [ e1 ] [ held ] kitRuns with
        | Error (DuplicateStudyNameErr reason) ->
            Assert.Contains("'QZ-LR'", reason)
            Assert.Contains("'qz-lr'", reason)
        | other -> Assert.Fail($"expected Error (DuplicateStudyNameErr _), got %A{other}")

        match Study.create name [ e1; e9 ] [] kitRuns with
        | Error (UnknownStudyExperimentErr reason) -> Assert.Contains($"'{e9.value}'", reason)
        | other -> Assert.Fail($"expected Error (UnknownStudyExperimentErr _) naming {e9.value}, got %A{other}")

    [<Fact>]
    let ``adding experiments appends the new ones in order and keeps those the study holds`` () : unit =
        // QZ-LR sets every other field, so a field reset to its initial value shows.
        let study = { StudyFileFixtures.qzLrStudyOver StudyFileFixtures.studyProbeMaterial with experiments = [ e2 ] }
        let added = studyOf (study.withExperiments [ e3; e2; e1; e3 ] kitRuns)

        Assert.Equal<ExperimentName list>([ e2; e3; e1 ], added.experiments)
        Assert.Equal({ study with experiments = [ e2; e3; e1 ] }, added)

    [<Fact>]
    let ``adding an experiment the kit lacks changes nothing`` () : unit =
        let study = studyOf (Study.create (StudyName "QZ-LR") [ e2 ] [] kitRuns)

        match study.withExperiments [ e1; e9 ] kitRuns with
        | Error (UnknownStudyExperimentErr reason) ->
            Assert.Contains($"'{e9.value}'", reason)
            Assert.DoesNotContain($"'{e1.value}'", reason)
        | other -> Assert.Fail($"expected Error (UnknownStudyExperimentErr _) naming {e9.value}, got %A{other}")

    [<Fact>]
    let ``a study's sessions split its experiments in study order, and a session without a setting has a free gain`` () : unit =
        // The kit holds them in another order, E1 first, so the split follows the study.
        let kit = kitRuns @ [ run d1 DiagnosticRun ]
        let study = studyOf (Study.create (StudyName "QZ-LR") [ e3; d1; e1 ] [] kit)

        Assert.Equal<StudySession list>(
            [ ({ name = session1; experiments = [ e3 ] } : StudySession); ({ name = session2; experiments = [ e1 ] } : StudySession) ],
            study.sessionsIn kit)

        Assert.Equal(FreeSessionGain, study.gainModelOf session1)
        Assert.Equal(FreeSessionGain, study.gainModelOf session2)

        let anchored = AnchoredSessionGain [ e3 ]
        let withSetting = { study with sessionGains = [ { session = session1; model = anchored } ] }
        Assert.Equal(anchored, withSetting.gainModelOf session1)
        Assert.Equal(FreeSessionGain, withSetting.gainModelOf session2)
