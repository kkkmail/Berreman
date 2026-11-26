namespace OpticalConstructor.Tests

open System
open Berreman.Constants
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.FitRuns
open Xunit

/// Spec 0050 S11.3 — a fit run's shape: its name from its study and the clock, its stage, calibration-use and engine
/// tags, the failure a multi-start member records, and the study one stage of a staged or joint fit reads.
module FitRunTests =

    [<Fact>]
    let ``a fit run is named by its study's file stem and the clock's instant`` () : unit =
        let name = (StudyName "QZ:LR").fitRunNameAt (ClockTime (DateTime(2026, 11, 1, 12, 0, 0, DateTimeKind.Utc)))

        Assert.Equal(FitRunName "QZ_LR@20261101T120000Z", name)
        Assert.Equal(FileStem "QZ_LR@20261101T120000Z", name.fileStem)
        Assert.Equal(FileName "QZ_LR@20261101T120000Z.fitrun-json", FitRunKitFile.fileNameOf name.fileStem)

    [<Fact>]
    let ``a fit stage, a calibration use and an engine round-trip their wire tags and render their text`` () : unit =
        Assert.Equal<FitStage list>([ JointStage; CalibrationStage; ObjectStage ], FitStage.all)
        Assert.Equal<int list>([ 1; 2; 3 ], FitStage.all |> List.map (fun stage -> stage.value))
        Assert.Equal<string list>(
            [ "Joint"; "Stage 1 (calibration)"; "Stage 2 (objects)" ],
            FitStage.all |> List.map (fun stage -> stage.text))

        for stage in FitStage.all do
            Assert.Equal<FitStage option>(Some stage, FitStage.tryOfWire stage.value)

        for tag in [ 0; 4 ] do
            Assert.Equal<FitStage option>(None, FitStage.tryOfWire tag)

        Assert.Equal<CalibrationUse list>([ CalibrationHeldFixed; CalibrationAsPrior ], CalibrationUse.all)
        Assert.Equal<int list>([ 1; 2 ], CalibrationUse.all |> List.map (fun calibrationUse -> calibrationUse.value))
        Assert.Equal<string list>([ "fixed"; "as priors" ], CalibrationUse.all |> List.map (fun calibrationUse -> calibrationUse.text))

        for calibrationUse in CalibrationUse.all do
            Assert.Equal<CalibrationUse option>(Some calibrationUse, CalibrationUse.tryOfWire calibrationUse.value)

        for tag in [ 0; 3 ] do
            Assert.Equal<CalibrationUse option>(None, CalibrationUse.tryOfWire tag)

        Assert.Equal<StudyEngineKind list>([ LinearStudyEngine; NonlinearStudyEngine ], StudyEngineKind.all)
        Assert.Equal<int list>([ 1; 2 ], StudyEngineKind.all |> List.map (fun engine -> engine.value))
        Assert.Equal<string list>([ "linear"; "nonlinear" ], StudyEngineKind.all |> List.map (fun engine -> engine.text))

        for engine in StudyEngineKind.all do
            Assert.Equal<StudyEngineKind option>(Some engine, StudyEngineKind.tryOfWire engine.value)

        for tag in [ 0; 3 ] do
            Assert.Equal<StudyEngineKind option>(None, StudyEngineKind.tryOfWire tag)

    [<Fact>]
    let ``a solver failure renders the reason a multi-start member failed`` () : unit =
        let notConverged = (DidNotConvergeErr "step too small").failureText
        let invalid = (InvalidNonlinearRequestErr "empty").failureText

        Assert.Equal(FitMemberFailure "the solver did not converge: step too small", notConverged)
        Assert.Equal(FitMemberFailure "the solve request is not valid: empty", invalid)
        Assert.EndsWith("step too small", notConverged.value)
        Assert.EndsWith("empty", invalid.value)

    // ================================ spec 0050 S11.3: staging a study for one fit ================================

    let private retardance : ParameterName = ParameterName "c2.retardance"
    let private m00 : ParameterName = ParameterName "m00"
    let private n3 : ParameterName = ParameterName "n3"

    let private linearPolarizerChoice : ItemModelChoice =
        match BuiltinCatalogue.preset Library.PresetIds.polarizerLp with
        | Library.PolarizerItem polarizer -> PolarizerPresetChoice (BuiltinCatalogue.itemModel ItemModelIds.idealPolarizerItemModelId, polarizer)
        | Library.SampleItem _ | Library.SourceItem _ | Library.DetectorItem _ -> failwith "The Built-in LP preset is not a polarizer."

    let private circularPolarizerChoice : ItemModelChoice = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId)
    let private blackBoxChoice : ItemModelChoice = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.blackBoxMuellerModelId)
    let private intensityDetectorChoice : ItemModelChoice = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.intensityDetectorModelId)

    // Every model is a shipped entry of step 210.
    let private lp1Draft : ItemDraft =
        {
            id = ItemId (Guid.Parse "38500000-0000-4000-8000-000000000001")
            label = ItemLabel "LP1"
            model = linearPolarizerChoice
            dial = FixedMounting
        }

    let private cpl1Draft : ItemDraft =
        {
            id = ItemId (Guid.Parse "38500000-0000-4000-8000-000000000002")
            label = ItemLabel "CPL1"
            model = circularPolarizerChoice
            dial = FixedMounting
        }

    let private qzDraft : ItemDraft =
        {
            id = ItemId (Guid.Parse "38500000-0000-4000-8000-000000000003")
            label = ItemLabel "QZ"
            model = blackBoxChoice
            dial = FixedMounting
        }

    let private phoneDraft : ItemDraft =
        {
            id = ItemId (Guid.Parse "38500000-0000-4000-8000-000000000004")
            label = ItemLabel "Phone"
            model = intensityDetectorChoice
            dial = FixedMounting
        }

    /// The draft's new item, failing with the refusal's text.
    let private itemOf (draft : ItemDraft) : Item =
        match draft.addTo [] with
        | Ok item -> item
        | Error error -> failwith $"'{draft.label.value}' was refused: {error.describe}"

    /// The item with one parameter in that state, failing with the refusal's text.
    let private stating (name : ParameterName) (state : ParameterState) (item : Item) : Item =
        match item.withParameterState name state with
        | Ok stated -> stated
        | Error error -> failwith $"'{item.label.value}' refused {name.value}: {error.describe}"

    /// CPL1's retardance: an unknown from 1.5 within [1, 2].
    let private retardanceData : UnknownParameterData =
        {
            start = ParameterValue 1.5
            bounds = { lower = ParameterValue 1.0; upper = ParameterValue 2.0 }
            priorOpt = None
        }

    /// QZ's m00: an unknown from 1 within [0, 2].
    let private m00Data : UnknownParameterData =
        {
            start = ParameterValue 1.0
            bounds = { lower = ParameterValue 0.0; upper = ParameterValue 2.0 }
            priorOpt = None
        }

    let private lp1 : Item = itemOf lp1Draft
    let private cpl1 : Item = itemOf cpl1Draft |> stating retardance (UnknownParameter retardanceData)
    let private qz : Item = itemOf qzDraft |> stating m00 (UnknownParameter m00Data)
    let private phone : Item = itemOf phoneDraft |> stating ParameterName.gain (KnownParameter (ParameterValue 1000.0))

    /// CPL1's retardance as stage 1 publishes it.
    let private calibration : CalibratedParameterData =
        {
            value = ParameterValue 1.55
            sigma = ParameterSigma 0.002
            fitRun = FitRunName "S@20261101T120000Z"
        }

    /// CPL1 at its next revision, its retardance published as `calibration`.
    let private calibratedCpl1 : Item =
        match cpl1.publishRevision [ { name = retardance; state = CalibratedParameter calibration } ] with
        | Ok published -> published
        | Error error -> failwith $"CPL1 refused its calibration: {error.describe}"

    let private emptySlot (id : Library.ElementId) (kind : CatalogueKind) (point : TablePoint) : SetupSlot =
        {
            id = id
            placement = ElementPlacement.create kind point
            zoom = BeamLayout.ElementZoom 5.0
            beam = BeamLayout.OnMainBeam
            content = EmptySlotContent
        }

    let private assigned (item : Item) (model : ItemModelChoice) (slot : SetupSlot) : SetupSlot =
        match slot.withItem item model with
        | Ok held -> held
        | Error error -> failwith $"'{item.label.value}' did not fit '{slot.id.value}': {error.describe}"

    let private src : SetupSlot = emptySlot (Library.ElementId "src") LightSource RayModel.defaultSourcePoint

    let private prep : SetupSlot =
        emptySlot (Library.ElementId "prep") CircularPolarizer { x = -0.4<meter>; y = 0.0<meter> } |> assigned cpl1 circularPolarizerChoice

    let private objectSlot : SetupSlot = emptySlot (Library.ElementId "object") Sample { x = 0.0<meter>; y = 0.0<meter> }

    let private analyzer : SetupSlot =
        emptySlot (Library.ElementId "analyzer") LinearPolarizer { x = 0.4<meter>; y = 0.0<meter> } |> assigned lp1 linearPolarizerChoice

    let private det : SetupSlot = emptySlot (Library.ElementId "det") Detector RayModel.defaultDetectorPoint |> assigned phone intensityDetectorChoice

    let private setupOf (name : SetupName) (slots : SetupSlot list) : Setup =
        match Setup.create name slots [] with
        | Ok setup -> setup
        | Error error -> failwith $"'{name.value}' was refused: {error.describe}"

    /// A: CPL1 preparing, the object slot empty, LP1 analyzing and the Phone.
    let private setupA : Setup = setupOf (SetupName "A") [ src; prep; objectSlot; analyzer; det ]

    /// Q: A with QZ in its object slot.
    let private setupQ : Setup = setupOf (SetupName "Q") [ src; prep; objectSlot |> assigned qz blackBoxChoice; analyzer; det ]

    let private a1 : ExperimentName = ExperimentName "A1"
    let private a2 : ExperimentName = ExperimentName "A2"
    let private e1 : ExperimentName = ExperimentName "E1"
    let private k1 : ExperimentName = ExperimentName "K1"

    /// A run over the setup turning LP1.
    let private runOver (setup : Setup) (id : ExperimentId) (name : ExperimentName) (role : ExperimentRole) : Experiment =
        {
            id = id
            name = name
            setup = setup.reference
            scan =
                ExperimentScan.oneAxis
                    {
                        elementId = analyzer.id
                        label = ElementLabel "LP1"
                        variable = VaryR1
                        range = VariableRange.forVariable VaryR1
                    }
                    CaptureTransmitted
                    None
            role = role
            sessionOpt = None
            notes = ExperimentNotes.none
            dataOpt = None
            corrections = []
        }

    /// A1, A2 (Calibration, over A), E1 (Science, over Q) and K1 (Dark, over A), the kit's experiments in its order.
    let private kitRuns : Experiment list =
        [
            runOver setupA (ExperimentId 1) a1 CalibrationRun
            runOver setupA (ExperimentId 2) a2 CalibrationRun
            runOver setupQ (ExperimentId 3) e1 ScienceRun
            runOver setupA (ExperimentId 4) k1 DarkRun
        ]

    let private studyOf (result : Result<Study, StudyError>) : Study =
        match result with
        | Ok study -> study
        | Error error -> failwith $"expected a study, got {error.describe}"

    /// The shared probe crystal, its `n3` an unknown under a prior.
    let private probe : OpenedMaterial = StudyFileFixtures.studyProbeMaterial

    /// QZ-LR over the kit's four runs, with the probe crystal opened.
    let private qzLr : Study =
        let created = studyOf (Study.create (StudyName "QZ-LR") [ a1; a2; e1; k1 ] [] kitRuns)
        { created with materials = [ probe ] }

    /// The kit as the study messages read it, holding these items.
    let private contextOver (items : Item list) (study : Study) : StudyContext =
        {
            studies = [ study ]
            experiments = kitRuns
            setups = [ setupA; setupQ ]
            items = items
            resolveMaterial = fun version -> Error (MaterialUnavailableErr $"material {version.materialId.value} is not the test's")
        }

    let private stagedOf (result : Result<StagedStudy, FitRunError>) : StagedStudy =
        match result with
        | Ok staged -> staged
        | Error error -> failwith $"expected a staged study, got {error.describe}"

    /// The state of the item's parameter as the staged study holds it.
    let private stateIn (staged : StagedStudy) (item : Item) (name : ParameterName) : ParameterState =
        match staged.items |> List.tryFind (fun held -> held.id = item.id) |> Option.bind (fun held -> held.tryFindParameter name) with
        | Some parameter -> parameter.state
        | None -> failwith $"the staged items hold no {name.value} of '{item.label.value}'"

    /// The parameters a study frees over the items its experiments use, as `context` holds them.
    let private unknownsIn (context : StudyContext) (study : Study) : StudyParameterRef list =
        study.unknownsWith (context.itemsUsedBy study) |> List.map (fun unknown -> unknown.parameter)

    /// The parameters the staged study frees, over the items as the stage holds them.
    let private stagedUnknowns (context : StudyContext) (staged : StagedStudy) : StudyParameterRef list =
        unknownsIn { context with items = staged.items } staged.study

    [<Fact>]
    let ``stage one fits the calibration runs and frees only the instruments' unknowns`` () : unit =
        let context = contextOver [ lp1; cpl1; qz; phone ] qzLr
        let staged = stagedOf (qzLr.stageWith context CalibrationStage CalibrationHeldFixed)

        Assert.Equal(CalibrationStage, staged.stage)
        Assert.Equal(CalibrationHeldFixed, staged.calibrationUse)
        Assert.Equal<ExperimentName list>([ a1; a2 ], staged.fittedRuns)
        Assert.Equal<ExperimentName list>([ e1 ], staged.study.settings.disabled)
        Assert.Equal<ExperimentName list>([ a1; a2; k1 ], staged.study.enabledExperiments)

        Assert.Equal(KnownParameter (ParameterValue 1.0), stateIn staged qz m00)
        Assert.Equal(UnknownParameter retardanceData, stateIn staged cpl1 retardance)
        Assert.Equal(KnownParameter (ParameterValue 1000.0), stateIn staged phone ParameterName.gain)

        // The opened crystal is no instrument's, so stage 1 holds its unknown at its start too.
        match staged.study.materials |> List.collect (fun material -> material.parameters) |> List.tryFind (fun parameter -> parameter.name = n3) with
        | Some parameter -> Assert.Equal(KnownParameter (ParameterValue 1.78), parameter.state)
        | None -> Assert.Fail("the staged study holds no n3")

        Assert.Equal<StudyParameterRef list>(
            [ ItemParameterRef (cpl1.id, retardance); ItemParameterRef (qz.id, m00); MaterialParameterRef (probe.version, n3) ],
            unknownsIn context qzLr)

        Assert.Equal<StudyParameterRef list>([ ItemParameterRef (cpl1.id, retardance) ], stagedUnknowns context staged)

    [<Fact>]
    let ``stage two refuses an uncalibrated instrument and holds a calibrated one`` () : unit =
        match qzLr.stageWith (contextOver [ lp1; cpl1; qz; phone ] qzLr) ObjectStage CalibrationHeldFixed with
        | Error (InstrumentNotCalibratedErr reason) ->
            Assert.Contains("'CPL1'", reason)
            Assert.Contains(retardance.value, reason)
        | other -> Assert.Fail($"expected Error (InstrumentNotCalibratedErr _) naming CPL1, got %A{other}")

        let context = contextOver [ lp1; calibratedCpl1; qz; phone ] qzLr
        let staged = stagedOf (qzLr.stageWith context ObjectStage CalibrationHeldFixed)

        Assert.Equal(ObjectStage, staged.stage)
        Assert.Equal<ExperimentName list>([ e1 ], staged.fittedRuns)
        Assert.Equal<ExperimentName list>([ a1; a2 ], staged.study.settings.disabled)
        Assert.Equal<ExperimentName list>([ e1; k1 ], staged.study.enabledExperiments)

        Assert.Equal(CalibratedParameter calibration, stateIn staged calibratedCpl1 retardance)
        Assert.Equal(UnknownParameter m00Data, stateIn staged qz m00)
        Assert.Equal<Study>({ qzLr with settings = { qzLr.settings with disabled = [ a1; a2 ] } }, staged.study)

        Assert.Equal<StudyParameterRef list>(
            [ ItemParameterRef (qz.id, m00); MaterialParameterRef (probe.version, n3) ],
            stagedUnknowns context staged)

    [<Fact>]
    let ``as priors, a calibrated parameter is freed with its value as start and prior, within fifty sigma`` () : unit =
        let context = contextOver [ lp1; calibratedCpl1; qz; phone ] qzLr
        let staged = stagedOf (qzLr.stageWith context ObjectStage CalibrationAsPrior)

        Assert.Equal(CalibrationAsPrior, staged.calibrationUse)
        Assert.Equal<ExperimentName list>([ e1 ], staged.fittedRuns)

        match stateIn staged calibratedCpl1 retardance with
        | UnknownParameter freed ->
            Assert.Equal(1.55, freed.start.value, 1e-12)
            Assert.Equal(1.45, freed.bounds.lower.value, 1e-12)
            Assert.Equal(1.65, freed.bounds.upper.value, 1e-12)

            match freed.priorOpt with
            | Some prior ->
                Assert.Equal(1.55, prior.centre.value, 1e-12)
                Assert.Equal(0.002, prior.sigma.value, 1e-12)
            | None -> Assert.Fail("the freed calibration carries no prior")
        | other -> Assert.Fail($"expected CPL1's retardance freed as an unknown, got %A{other}")

        Assert.Equal(UnknownParameter m00Data, stateIn staged qz m00)

        Assert.Equal<StudyParameterRef list>(
            [ ItemParameterRef (calibratedCpl1.id, retardance); ItemParameterRef (qz.id, m00); MaterialParameterRef (probe.version, n3) ],
            stagedUnknowns context staged)

    [<Fact>]
    let ``a stage with no run to fit or nothing to fit is refused`` () : unit =
        let calibrationOnly = studyOf (Study.create (StudyName "Calibration only") [ a1; a2; k1 ] [] kitRuns)

        // CPL1 is uncalibrated as well, but a stage with no run to fit is refused for that first.
        match calibrationOnly.stageWith (contextOver [ lp1; cpl1; qz; phone ] calibrationOnly) ObjectStage CalibrationHeldFixed with
        | Error (NoStageRunErr reason) ->
            Assert.Contains("'Calibration only'", reason)
            Assert.Contains(ObjectStage.text, reason)
        | other -> Assert.Fail($"expected Error (NoStageRunErr _), got %A{other}")

        let allKnown = studyOf (Study.create (StudyName "All known") [ a1; a2; e1; k1 ] [] kitRuns)

        match allKnown.stageWith (contextOver [ lp1; itemOf cpl1Draft; itemOf qzDraft; phone ] allKnown) JointStage CalibrationHeldFixed with
        | Error (NoStageUnknownErr reason) ->
            Assert.Contains("'All known'", reason)
            Assert.Contains(JointStage.text, reason)
        | other -> Assert.Fail($"expected Error (NoStageUnknownErr _), got %A{other}")

    [<Fact>]
    let ``every fit-run error renders its condition and its payload`` () : unit =
        let cases : (FitRunError * string) list =
            [
                InstrumentNotCalibratedErr "'CPL1' c2.retardance", "an instrument parameter is not calibrated yet: 'CPL1' c2.retardance"
                NoStageRunErr "'QZ-LR' has no Science run", "the stage has no run to fit: 'QZ-LR' has no Science run"
                NoStageUnknownErr "every parameter is known", "the stage has nothing to fit: every parameter is known"
            ]

        for error, expected in cases do
            Assert.Equal<string>(expected, error.describe)

        let descriptions = cases |> List.map (fun (error, _) -> error.describe)
        Assert.Equal(3, descriptions |> List.distinct |> List.length)
