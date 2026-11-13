namespace OpticalConstructor.Tests

open System
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Studies
open Xunit
open CrystalMaterialFixtures

/// Spec 0050 S11.1 — a study's shape: its name and file stem, its scope and fit-mode tags, its residual weights,
/// CreateStudy's and AddExperimentsToStudy's pure halves over a kit's experiments, the sessions and gain models a
/// study reads from them, and its unknowns with their scopes and owners.
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

    let private retardance : ParameterName = ParameterName "c2.retardance"
    let private fastAxisOffset : ParameterName = ParameterName "c2.fastAxisOffset"
    let private n1 : ParameterName = ParameterName "n1"
    let private e11 : ParameterName = ParameterName "e11"

    /// An unknown starting at `start` within [`lower`, `upper`], with no prior.
    let private unknownFrom (start : ParameterValue) (lower : ParameterValue) (upper : ParameterValue) : UnknownParameterData =
        {
            start = start
            bounds = { lower = lower; upper = upper }
            priorOpt = None
        }

    let private cpl1RetardanceData : UnknownParameterData = unknownFrom (ParameterValue 1.55) (ParameterValue 1.40) (ParameterValue 1.75)
    let private cpl2RetardanceData : UnknownParameterData = unknownFrom (ParameterValue 1.60) (ParameterValue 1.45) (ParameterValue 1.70)
    let private fastAxisOffsetData : UnknownParameterData = unknownFrom (ParameterValue 0.0) (ParameterValue -0.1) (ParameterValue 0.1)
    let private gainData : UnknownParameterData = unknownFrom (ParameterValue 1.0) (ParameterValue 0.5) (ParameterValue 2.0)
    let private n1Data : UnknownParameterData = unknownFrom (ParameterValue 1.71) (ParameterValue 1.65) (ParameterValue 1.77)
    let private e11Data : UnknownParameterData = unknownFrom (ParameterValue 2.0e-3) (ParameterValue 0.0) (ParameterValue 1.0e-2)

    /// The draft's new item with each named parameter freed in order, failing with the refusal's text.
    let private itemFreeing (draft : ItemDraft) (freed : (ParameterName * UnknownParameterData) list) : Item =
        freed
        |> List.fold (fun (item : Item) (name, unknown) ->
            match item.withParameterState name (UnknownParameter unknown) with
            | Ok changed -> changed
            | Error error -> failwith $"'{draft.label.value}' refused {name.value}: {error.describe}") draft.toItem

    let private circularPolarizer : ItemModelChoice = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId)

    let private cpl1 : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "36200000-0000-4000-8000-000000000011")
                label = ItemLabel "CPL1"
                model = circularPolarizer
                dial = DialMounted DialPositive
            }
        itemFreeing draft [ retardance, cpl1RetardanceData ]

    let private cpl2 : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "36200000-0000-4000-8000-000000000012")
                label = ItemLabel "CPL2"
                model = circularPolarizer
                dial = DialMounted DialPositive
            }
        itemFreeing draft [ retardance, cpl2RetardanceData; fastAxisOffset, fastAxisOffsetData ]

    let private phone : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "36200000-0000-4000-8000-000000000013")
                label = ItemLabel "Phone"
                model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.intensityDetectorModelId)
                dial = FixedMounting
            }
        itemFreeing draft [ ParameterName.gain, gainData ]

    /// The test-local version of the absorbing crystal material the unknowns' study opens.
    let private openedVersion : MaterialVersionId =
        MaterialVersionId.firstOf (MaterialId (Guid.Parse "36200000-0000-4000-8000-000000000001"))

    let private absorbingEntry : MaterialEntry =
        {
            id = openedVersion.materialId
            name = EntryName "Probe absorbing crystal"
            category = CategoryIds.crystal
            description = None
            physics = CrystalParameterPhysics (AbsorbingTriclinicCrystal absorbingTriclinicParameters)
        }

    /// QZ-LR over E1, E2 and E3 with the absorbing crystal opened, its `n1` and `e11` freed and every other component
    /// known; nothing scoped or linked.
    let private unknownsStudy : Study =
        let created = studyOf (Study.create (StudyName "QZ-LR") [ e1; e2; e3 ] [] kitRuns)
        let opened = studyOf (created.openMaterial openedVersion absorbingEntry)
        let withN1 = studyOf (opened.withMaterialParameterState openedVersion n1 (UnknownParameter n1Data))
        studyOf (withN1.withMaterialParameterState openedVersion e11 (UnknownParameter e11Data))

    [<Fact>]
    let ``the study's unknowns are its items' and opened materials' unknown parameters, with their scopes, leaving out link followers`` () : unit =
        let cpl1Retardance = ItemParameterRef (cpl1.id, retardance)
        let cpl2Retardance = ItemParameterRef (cpl2.id, retardance)
        let phoneGain = ItemParameterRef (phone.id, ParameterName.gain)

        let expected : StudyUnknown list =
            [
                { parameter = cpl1Retardance; scope = ItemScope; unknown = cpl1RetardanceData }
                { parameter = cpl2Retardance; scope = ItemScope; unknown = cpl2RetardanceData }
                { parameter = ItemParameterRef (cpl2.id, fastAxisOffset); scope = ItemScope; unknown = fastAxisOffsetData }
                { parameter = phoneGain; scope = SessionBlockScope; unknown = gainData }
                { parameter = MaterialParameterRef (openedVersion, n1); scope = MaterialScope; unknown = n1Data }
                { parameter = MaterialParameterRef (openedVersion, e11); scope = MaterialScope; unknown = e11Data }
            ]

        Assert.Equal<StudyUnknown list>(expected, unknownsStudy.unknownsWith [ cpl1; cpl2; phone ])

        // Only the used items' unknowns, in the order given; the opened material's stay.
        Assert.Equal<StudyUnknown list>(expected.[3 ..], unknownsStudy.unknownsWith [ phone ])
        Assert.Equal<StudyUnknown list>(expected.[3 .. 3] @ expected.[0 .. 0] @ expected.[4 ..], unknownsStudy.unknownsWith [ phone; cpl1 ])

        // Only a detector's gain is per session by default; its other parameters are per item.
        Assert.Equal(ItemScope, unknownsStudy.scopeOf (ItemParameterRef (phone.id, ParameterName.darkOffset)))

        let scoped =
            {
                unknownsStudy with
                    scopes =
                        [
                            ({ parameter = cpl1Retardance; scope = ExperimentScope } : ScopedParameter)
                            ({ parameter = phoneGain; scope = ItemScope } : ScopedParameter)
                        ]
            }

        Assert.Equal(ExperimentScope, scoped.scopeOf cpl1Retardance)
        Assert.Equal(ItemScope, scoped.scopeOf phoneGain)

        let rescoped =
            expected
            |> List.map (fun unknown ->
                if unknown.parameter = cpl1Retardance then { unknown with scope = ExperimentScope }
                elif unknown.parameter = phoneGain then { unknown with scope = ItemScope }
                else unknown)

        Assert.Equal<StudyUnknown list>(rescoped, scoped.unknownsWith [ cpl1; cpl2; phone ])

        let link : ParameterLink =
            {
                follower = cpl2Retardance
                leader = cpl1Retardance
            }

        let linked = { unknownsStudy with links = [ link ] }

        Assert.Equal<StudyUnknown list>(
            expected |> List.filter (fun unknown -> unknown.parameter <> cpl2Retardance),
            linked.unknownsWith [ cpl1; cpl2; phone ])

    [<Fact>]
    let ``an unknown's owners are its item or material, one per session holding a user, or one per user`` () : unit =
        let sessions : StudySession list =
            [
                ({ name = session1; experiments = [ e1; e2 ] } : StudySession)
                ({ name = session2; experiments = [ e3 ] } : StudySession)
            ]

        let cpl1Retardance = ItemParameterRef (cpl1.id, retardance)
        let perExperiment = { unknownsStudy with scopes = [ ({ parameter = cpl1Retardance; scope = ExperimentScope } : ScopedParameter) ] }

        let unknownIn (study : Study) (parameter : StudyParameterRef) : StudyUnknown =
            study.unknownsWith [ cpl1; cpl2; phone ] |> List.find (fun unknown -> unknown.parameter = parameter)

        let cpl1Unknown = unknownIn unknownsStudy cpl1Retardance
        let n1Unknown = unknownIn unknownsStudy (MaterialParameterRef (openedVersion, n1))
        let gainUnknown = unknownIn unknownsStudy (ItemParameterRef (phone.id, ParameterName.gain))
        let experimentUnknown = unknownIn perExperiment cpl1Retardance

        // An item or material scope has its one owner, whoever uses it.
        Assert.Equal<UnknownOwner list>([ ItemOwner cpl1.id ], cpl1Unknown.owners sessions [ e1; e3 ])
        Assert.Equal<UnknownOwner list>([ ItemOwner cpl1.id ], cpl1Unknown.owners sessions [])
        Assert.Equal<UnknownOwner list>([ MaterialOwner openedVersion ], n1Unknown.owners sessions [ e2 ])

        // A session scope has one owner per session holding a user, in session order.
        Assert.Equal<UnknownOwner list>([ SessionOwner session1; SessionOwner session2 ], gainUnknown.owners sessions [ e1; e3 ])
        Assert.Equal<UnknownOwner list>([ SessionOwner session1; SessionOwner session2 ], gainUnknown.owners sessions [ e3; e1 ])
        Assert.Equal<UnknownOwner list>([ SessionOwner session1 ], gainUnknown.owners sessions [ e2 ])
        Assert.Equal<UnknownOwner list>([ SessionOwner session1 ], gainUnknown.owners sessions [ e1; e2 ])
        Assert.Empty(gainUnknown.owners sessions [ e9 ])

        // An experiment scope has one owner per user, in order, each once.
        Assert.Equal<UnknownOwner list>([ ExperimentOwner e3; ExperimentOwner e1 ], experimentUnknown.owners sessions [ e3; e1 ])
        Assert.Equal<UnknownOwner list>([ ExperimentOwner e3; ExperimentOwner e1 ], experimentUnknown.owners sessions [ e3; e1; e3 ])
        Assert.Empty(experimentUnknown.owners sessions [])
