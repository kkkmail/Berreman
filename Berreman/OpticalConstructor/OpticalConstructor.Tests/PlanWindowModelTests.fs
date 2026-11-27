namespace OpticalConstructor.Tests

open System
open System.Collections.Concurrent
open System.Threading
open Xunit
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.MeasurementPlanner
open OpticalConstructor.Storage
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Storage.PlannerCacheStore
open OpticalConstructor.Optimization.PlannerInformation
open OpticalConstructor.Optimization.PlannerRuns
open OpticalConstructor.Ui.JobRunner
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.PlanJob
open OpticalConstructor.Ui.TableAndElementRotationView
open PlannerInputsTests
open PlanRunTests
open TabulatedStudyFixtures

/// The constructor window plans a study's crystal material through its shell's plan host, one plan at a time under a
/// read-write or read-only kit, and shows the committed report of the chosen study and material (spec 0050 S14.2; 001 §P).
module PlanWindowModelTests =

    let private kitValue (result : Result<'Value, KitError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private libraryValue (result : Result<'Value, KitLibraryError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private studyValue (result : Result<Study, StudyError>) : Study =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private plannerValue (result : Result<'Value, PlannerError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private qz : StudyName = StudyName "QZ"
    let private glassAndQz : StudyName = StudyName "Glass and QZ"
    let private glass : StudyName = StudyName "Glass"

    /// `PlanRunTests`' study, g11 and g33 unknown, named QZ.
    let private qzStudy : Study = { planStudy with name = qz }

    /// The tabulated glass opened first, then the probe quartz: one crystal among the study's two materials.
    let private glassAndQzStudy : Study =
        Study.create glassAndQz [] [] []
        |> Result.bind (fun (study : Study) -> study.openMaterial tableVersion uncertainTableEntry)
        |> Result.bind (fun (study : Study) -> study.openMaterial crystalVersion crystalEntry)
        |> studyValue

    /// The tabulated glass alone: no crystal to plan.
    let private glassStudy : Study =
        Study.create glass [] [] [] |> Result.bind (fun (study : Study) -> study.openMaterial tableVersion uncertainTableEntry) |> studyValue

    /// A version no study of the kit opens.
    let private unopenedVersion : MaterialVersionId =
        MaterialVersionId.firstOf (MaterialId (Guid.Parse "48800000-0000-4000-8000-000000000002"))

    /// The plate sample QZ-Z's item binds.
    let private qzZSample : Sample =
        match qzZDraft.model with
        | StackSampleChoice (_, _, sample) -> sample
        | LibraryModelChoice _
        | SourcePresetChoice _
        | PolarizerPresetChoice _ -> failwith "QZ-Z's draft binds no sample."

    let private request : PlanRequest =
        {
            study = qz
            material = crystalVersion
            criterion = DeterminantPlanCriterion
        }

    /// A host that records each plan it is given, never runs it, and answers a handle recording its cancel.
    let private recordingPlanHost
        (jobs : ResizeArray<PlanObserver -> Result<PlanReport, PlannerError>>)
        (cancel : unit -> unit)
        : PlanJobHost =
        {
            startPlan =
                fun plan ->
                    jobs.Add plan
                    {
                        cancelPlan = cancel
                    }
        }

    let private withPlanRuns (host : PlanJobHost) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        {
            model with
                kitShell =
                    {
                        model.kitShell with planRuns = host
                    }
        }

    let private chosenStudy (model : TableAndElementRotationModel) : Study =
        match model.selectedStudyOpt with
        | Some study -> study
        | None -> failwith "The window has no chosen study."

    /// An observer whose token is cancelled before the plan starts, so a plan stops before its first candidate.
    let private cancelledObserver : PlanObserver =
        {
            reportCandidate = ignore
            cancellation = CancellationToken true
        }

    /// A recorded plan run under `cancelledObserver`: it reads the window's study and stops before solving anything.
    let private assertPlansTwoCandidates (plan : PlanObserver -> Result<PlanReport, PlannerError>) : unit =
        match plan cancelledObserver with
        | Error (PlanCancelledErr reason) -> Assert.Equal("0 of 2 candidates were evaluated", reason)
        | Error error -> Assert.Fail($"expected the cancelled plan of QZ-Z's two candidates, got {error.describe}")
        | Ok _ -> Assert.Fail "a plan under a cancelled token must not complete"

    let private pathsOf (folder : InMemoryKitFolder) : string list = folder.paths () |> List.map (fun path -> path.value)

    let private cacheFilesOf (folder : InMemoryKitFolder) : string list =
        pathsOf folder |> List.filter (fun path -> path.EndsWith(".candidate-json", StringComparison.Ordinal))

    /// Runs over `initInverse` loaded from an in-memory kit opened with `access`. The kit holds Laser, Polarimeter and
    /// QZ-Z, the probe quartz and the tabulated glass, and the studies QZ, Glass and QZ, and Glass; the catalogue store
    /// holds both materials and QZ-Z's sample, so the kit snapshots them and the window resolves QZ-Z.
    let private withPlanWindow
        (access : KitAccess)
        (run : InMemoryKitFolder -> TableAndElementRotationModel -> unit)
        : unit =
        let stores = BuiltinCatalogue.freshStores ()
        let imports = CatalogueImportContext.create stores.proxy

        BuiltinCatalogue.putSealedEntries
            imports
            [
                LibraryEntryContent.ofMaterialEntry VersionNumber.first UserProvenance crystalEntry
                LibraryEntryContent.ofMaterialEntry VersionNumber.first UserProvenance uncertainTableEntry
                LibraryEntryContent.ofSample VersionNumber.first UserProvenance qzZSample
            ]

        let folder = InMemoryKitFolder.create ()
        let clock = ClockProvider.system
        let kitFile = KitFile.create (KitId (Guid.Parse "48800000-0000-4000-8000-000000000001")) (KitName "Plan window") (KitAuthor "Plan window tests") (clock.now ())
        (KitProxy.createInMemory folder).initialiseKit kitFile |> kitValue

        let created : KitLibraryContext =
            {
                kit = KitContext.openKit (KitProxy.createInMemory folder) ReadWriteKitAccess clock |> kitValue
                imports = imports
            }

        try
            for item in [ laser; polarimeter; qzZ ] do created.recordItem item |> libraryValue
            for study in [ qzStudy; glassAndQzStudy; glassStudy ] do created.recordStudy study |> libraryValue
        finally created.kit.close () |> kitValue

        let library : KitLibraryContext =
            {
                kit = KitContext.openKit (KitProxy.createInMemory folder) access clock |> kitValue
                imports = imports
            }

        try
            let proxy = library.kit.proxy
            let opened : OpenedKit =
                {
                    folder = folder.kitFolder
                    library = library
                    experimentData = ExperimentDataStore.createFileBacked proxy
                    experiments = proxy.readExperiments () |> kitValue
                    items = proxy.readItems () |> kitValue
                    studies = proxy.readStudies () |> kitValue
                    fitRuns = []
                    setups = proxy.readSetups () |> kitValue
                    decisions = library.resolveSnapshots () |> libraryValue
                    overlay = library.openOverlay [] |> libraryValue
                }

            let sources = SourceProxy.createInMemory ()
            let live = liveLibrary stores.libraryEntries stores.samples sources
            let initial = initInverse live (createInMemoryExperiments ()) stores.materials stores.samples stores.categories |> kitValue

            try
                let model =
                    {
                        initial with
                            sources = sources
                            catalogueImports = imports
                            itemModels = stores.itemModels
                    }

                run folder (model.loadKit opened)
            finally initial.kit.close () |> kitValue
        finally library.kit.close () |> kitValue

    [<Fact>]
    let ``RunPlan starts one plan through the shell's host, Cancel cancels it, and a read-only kit plans too`` () : unit =
        let jobs = ResizeArray<PlanObserver -> Result<PlanReport, PlannerError>> ()
        let cancels = ResizeArray<unit> ()
        let host = recordingPlanHost jobs cancels.Add

        withPlanWindow ReadWriteKitAccess (fun folder loaded ->
            Assert.Equal<PlanJobState>(PlanJobState.idle, loaded.planJob)
            let ready = loaded |> withPlanRuns host |> updateTableAndElementRotation (SelectStudy qz)
            let before = pathsOf folder

            let running = ready |> updateTableAndElementRotation (StudyPlan (RunPlan request))
            Assert.True running.planJob.job.isRunning
            Assert.Single jobs |> ignore
            Assert.Empty cancels
            Assert.Equal<ControlCaption option>(None, running.kitRefusalOpt)
            assertPlansTwoCandidates jobs.[0]

            // One plan at a time: a second RunPlan starts nothing and changes nothing.
            let still = running |> updateTableAndElementRotation (StudyPlan (RunPlan { request with criterion = TargetTracePlanCriterion }))
            Assert.Equal<TableAndElementRotationModel>(running, still)
            Assert.Single jobs |> ignore

            let progress : JobProgress =
                {
                    completed = JobStepCount 1
                    total = Some (JobStepCount 2)
                    elapsed = JobElapsed (TimeSpan.FromMilliseconds 25.0)
                }

            let progressed = still |> updateTableAndElementRotation (StudyPlan (PlanProgressed progress))
            Assert.Equal<JobProgress option>(Some progress, progressed.planJob.job.progress)
            let cancelling = progressed |> updateTableAndElementRotation (StudyPlan CancelPlan)
            Assert.Single cancels |> ignore
            Assert.True cancelling.planJob.job.isRunning

            let cancelled =
                cancelling |> updateTableAndElementRotation (StudyPlan (PlanEnded (Error (PlanCancelledErr "cancelled by the recording host"))))

            Assert.Equal<JobStatus>(Cancelled, cancelled.planJob.job.status)
            Assert.Equal<PlanJobHandle option>(None, cancelled.planJob.handleOpt)
            Assert.Equal<PlannerError option>(None, cancelled.planJob.errorOpt)
            Assert.Equal<PlanReport option>(None, cancelled.planReportOpt)
            Assert.Equal<string list>(before, pathsOf folder))

        jobs.Clear ()
        cancels.Clear ()

        // A plan writes no kit file, so a read-only kit plans as a read-write one does.
        withPlanWindow ReadOnlyKitAccess (fun folder loaded ->
            let ready = loaded |> withPlanRuns host |> updateTableAndElementRotation (SelectStudy qz)
            Assert.Equal(ReadOnlyKitAccess, ready.kit.access)
            let before = pathsOf folder

            let running = ready |> updateTableAndElementRotation (StudyPlan (RunPlan request))
            Assert.True running.planJob.job.isRunning
            Assert.Single jobs |> ignore
            Assert.Equal<ControlCaption option>(None, running.kitRefusalOpt)
            assertPlansTwoCandidates jobs.[0]

            let cancelling = running |> updateTableAndElementRotation (StudyPlan CancelPlan)
            Assert.Single cancels |> ignore
            Assert.True cancelling.planJob.job.isRunning
            Assert.Equal<string list>(before, pathsOf folder))

    [<Fact>]
    let ``the Plan step plans the chosen study's opened crystal material, the first by default`` () : unit =
        withPlanWindow ReadWriteKitAccess (fun folder loaded ->
            let before = pathsOf folder
            let initial : PlanSurfaceState =
                {
                    materialOpt = None
                    criterion = DeterminantPlanCriterion
                }

            Assert.Equal<PlanSurfaceState>(initial, loaded.planSurface)

            // The study opens the table first; the crystal alone can be planned, and it is planned by default.
            let chosen = loaded |> updateTableAndElementRotation (SelectStudy glassAndQz)
            let study = chosenStudy chosen
            Assert.Equal<MaterialVersionId list>([ tableVersion; crystalVersion ], study.materials |> List.map (fun material -> material.version))
            Assert.Equal<OpenedMaterial list>(study.materials |> List.filter (fun material -> material.version = crystalVersion), chosen.planMaterials)
            Assert.Equal<MaterialVersionId option>(Some crystalVersion, chosen.planMaterialOpt)

            // A version the study opens as a table, or does not open, changes nothing.
            for version in [ tableVersion; unopenedVersion ] do
                Assert.Equal<TableAndElementRotationModel>(chosen, chosen |> updateTableAndElementRotation (SetPlanMaterial version))

            let picked = chosen |> updateTableAndElementRotation (SetPlanMaterial crystalVersion)
            Assert.Equal<MaterialVersionId option>(Some crystalVersion, picked.planSurface.materialOpt)
            Assert.Equal<MaterialVersionId option>(Some crystalVersion, picked.planMaterialOpt)

            let ranked = picked |> updateTableAndElementRotation (SetPlanCriterion TargetTracePlanCriterion)
            let expected : PlanSurfaceState =
                {
                    materialOpt = Some crystalVersion
                    criterion = TargetTracePlanCriterion
                }

            Assert.Equal<PlanSurfaceState>(expected, ranked.planSurface)
            Assert.Equal<TableAndElementRotationModel>({ picked with planSurface = expected }, ranked)

            // QZ's working copy is QZ's own; a study opening no crystal plans nothing, whatever the surface holds.
            let onQz = ranked |> updateTableAndElementRotation (SelectStudy qz)
            Assert.Equal<OpenedMaterial list>((chosenStudy onQz).materials, onQz.planMaterials)
            Assert.Equal<MaterialVersionId option>(Some crystalVersion, onQz.planMaterialOpt)
            let onGlass = onQz |> updateTableAndElementRotation (SelectStudy glass)
            Assert.Empty onGlass.planMaterials
            Assert.Equal<MaterialVersionId option>(None, onGlass.planMaterialOpt)
            Assert.Equal<PlanSurfaceState>(expected, onGlass.planSurface)
            Assert.Equal<string list>(before, pathsOf folder))

    [<Fact>]
    let ``a plan's end commits its report, which the Plan step shows for its study and material`` () : unit =
        withPlanWindow ReadWriteKitAccess (fun folder loaded ->
            use ended = new ManualResetEventSlim (false)
            let messages = ConcurrentQueue<TableAndElementRotationMsg> ()

            let host =
                PlanJobHost.posting
                    (fun action -> action ())
                    (fun progress -> messages.Enqueue (StudyPlan (PlanProgressed progress)))
                    (fun result ->
                        messages.Enqueue (StudyPlan (PlanEnded result))
                        ended.Set ())

            let ready = loaded |> withPlanRuns host |> updateTableAndElementRotation (SelectStudy qz)

            // The kit's cache answers each candidate with literal rows under the key the window's own forward context
            // gives, so the plan solves nothing: its subject is the window's wiring, the solve is `PlanJobTests`'.
            let forward = ready.studyForwardContext
            let inputs = (chosenStudy ready).plannerInputsWith forward.kit forward.choiceOf crystalVersion |> plannerValue
            Assert.Equal(2, inputs.candidates.Length)

            let rowsOn (observable : Observable) : CandidateRows =
                match observable with
                | TransmittedMueller -> { jacobian = ResidualJacobian [| [| 1.0; 0.0 |] |]; noise = NoiseVariance 1.0e-4 }
                | ReflectedMueller -> { jacobian = ResidualJacobian [| [| 0.0; 2.0 |] |]; noise = NoiseVariance 1.0e-4 }

            for candidate in inputs.candidates do
                let key = inputs.cacheKeyWith forward.kit.items candidate |> plannerValue
                ready.kit.cachedPlannerRows key (fun () -> Ok (rowsOn candidate.observable)) |> plannerValue |> ignore

            let cached = cacheFilesOf folder
            Assert.Equal(2, cached.Length)

            let running = ready |> updateTableAndElementRotation (StudyPlan (RunPlan request))
            Assert.True running.planJob.job.isRunning
            Assert.Equal<PlanReport option>(None, running.planReportOpt)
            Assert.True(ended.Wait(TimeSpan.FromSeconds 10.0), "the plan did not end within ten seconds")

            let finished =
                messages.ToArray ()
                |> Array.fold (fun (model : TableAndElementRotationModel) (message : TableAndElementRotationMsg) -> updateTableAndElementRotation message model) running

            Assert.Equal<JobStatus>(Completed, finished.planJob.job.status)
            Assert.Equal<PlannerError option>(None, finished.planJob.errorOpt)

            match finished.planJob.job.progressText with
            | Some text -> Assert.StartsWith("2 / 2", text)
            | None -> Assert.Fail "the completed plan shows no progress text"

            match finished.planReportOpt with
            | Some report ->
                Assert.Equal<PlanReport option>(Some report, finished.planJob.job.committedResults)
                Assert.Equal(qz, report.study)
                Assert.Equal(crystalVersion, report.material)
                Assert.Equal(DeterminantPlanCriterion, report.criterion)

                // QZ-Z's two candidates, its transmitted and its reflected branch, both open and both ranked.
                let ranked = report.ranked |> List.map (fun (step : RankedPlanStep) -> step.candidate)
                Assert.Equal(2, ranked.Length)
                for candidate in inputs.candidates do Assert.Contains(candidate, ranked)
                Assert.All(ranked, fun (candidate : PlanCandidate) -> Assert.Equal(qzZ.id, candidate.plate.item))
            | None -> Assert.Fail "the completed plan of the chosen study and material shows no report"

            // The plan read the cache and wrote nothing.
            Assert.Equal<string list>(cached, cacheFilesOf folder)

            // Another study, even one opening the same crystal, shows no report; QZ shows it again.
            for other in [ glassAndQz; glass ] do
                let elsewhere = finished |> updateTableAndElementRotation (SelectStudy other)
                Assert.Equal<PlanReport option>(None, elsewhere.planReportOpt)
                Assert.Equal<PlanReport option>(finished.planJob.job.committedResults, elsewhere.planJob.job.committedResults)
                Assert.Equal<PlanReport option>(finished.planReportOpt, (elsewhere |> updateTableAndElementRotation (SelectStudy qz)).planReportOpt))

    // Spec 0050 S14.2: Add to kit records a candidate as the grid generator records a combination, a PlannedRun.

    let private sampleSlot : ElementId = ElementId "sample"
    let private detectorSlot : ElementId = ElementId "det"

    /// The inverse table under QZ, the Polarimeter in `det`; its draft varies the sample, so a draft put back shows.
    let private planTable (loaded : TableAndElementRotationModel) : TableAndElementRotationModel =
        let ready =
            loaded
            |> updateTableAndElementRotation (SelectStudy qz)
            |> updateTableAndElementRotation (AssignSlot (detectorSlot, Some polarimeter.id))
            |> updateTableAndElementRotation (ExpChooseElement sampleSlot)

        Assert.Equal<ControlCaption option>(None, ready.kitRefusalOpt)
        Assert.Equal<ItemId option>(Some polarimeter.id, (ready.elements |> List.find (fun slot -> slot.id = detectorSlot)).content.itemOpt)
        Assert.Equal<ElementId option>(Some sampleSlot, ready.experimentCollection.draft.elementId)
        ready

    /// QZ-Z's candidate on that branch, as the window's study QZ plans its quartz.
    let private candidateOn (observable : Observable) (model : TableAndElementRotationModel) : PlanCandidate =
        let forward = model.studyForwardContext
        let inputs = (chosenStudy model).plannerInputsWith forward.kit forward.choiceOf crystalVersion |> plannerValue
        inputs.candidates |> List.find (fun (candidate : PlanCandidate) -> candidate.observable = observable)

    let private setupsOf (model : TableAndElementRotationModel) : Setups.Setup list = model.kit.proxy.readSetups () |> kitValue
    let private experimentsOf (model : TableAndElementRotationModel) : Experiment list = model.kit.proxy.readExperiments () |> kitValue

    [<Fact>]
    let ``Add to kit records the candidate's setup and a planned experiment sweeping its angles on its branch`` () : unit =
        withPlanWindow ReadWriteKitAccess (fun _ loaded ->
            let ready = planTable loaded
            let candidate = candidateOn TransmittedMueller ready
            Assert.Equal(qzZ.id, candidate.plate.item)
            Assert.Equal(CaptureTransmitted, candidate.capture)
            Assert.Empty ready.kitSetups
            Assert.Empty ready.experimentCollection.experiments
            Assert.Equal(CandidateNotInKit, candidate.coverageIn ready.kitSetups ready.experimentCollection.experiments)

            let added = ready |> updateTableAndElementRotation (AddPlannedExperiment candidate)

            // The kit holds the table with QZ-Z in its sample slot, at R1 0, and the Polarimeter in `det`.
            let setups = setupsOf added
            let setup = Assert.Single setups
            Assert.Equal<Setups.Setup list>(setups, added.kitSetups)
            let slotOf (id : ElementId) : Setups.SetupSlot = setup.slots |> List.find (fun slot -> slot.id = id)
            Assert.Equal<ItemId option>(Some qzZ.id, (slotOf sampleSlot).content.itemOpt)
            Assert.Equal(0.0, (slotOf sampleSlot).placement.r1.value)
            Assert.Equal<ItemId option>(Some polarimeter.id, (slotOf detectorSlot).content.itemOpt)

            // Over it one planned experiment, whose one block reads the transmitted branch while the sample's R2 sweeps the
            // incidence and then its R1 the azimuth, so the planner finds the candidate in the kit.
            let experiments = experimentsOf added
            let experiment = Assert.Single experiments
            Assert.Equal<Experiment list>(experiments, added.experimentCollection.experiments)
            Assert.Equal(PlannedRun, experiment.role)
            Assert.Equal<SetupReference>(setup.reference, experiment.setup)
            Assert.Equal<Berreman.Fields.WaveLength option>(None, experiment.scan.sourceWaveLengthOpt)
            let block = Assert.Single experiment.scan.blocks
            Assert.Equal(CaptureTransmitted, block.capture)
            Assert.Equal<(ElementId * VariableParameter * VariableRange) list>(
                [
                    sampleSlot, VaryR2, PlanCandidate.incidenceRange
                    sampleSlot, VaryR1, PlanCandidate.azimuthRange
                ],
                block.axes |> List.map (fun axis -> axis.elementId, axis.variable, axis.range))
            Assert.Equal(CandidateInKit experiment.name, candidate.coverageIn added.kitSetups added.experimentCollection.experiments)
            Assert.Equal(CandidateInKit experiment.name, candidate.coverageIn setups experiments)

            match added.kitStatusCaption with
            | Some status ->
                Assert.StartsWith("Planned ", status.value)
                Assert.Equal($"Planned {experiment.name.value} over {setup.reference.describe}.", status.value)
            | None -> Assert.Fail "Add to kit must tell what it planned in the kit status"

            // The window's own draft and table stay as they were.
            Assert.Equal<ExperimentDraft>(ready.experimentCollection.draft, added.experimentCollection.draft)
            Assert.Equal<Setups.SetupSlot list>(ready.elements, added.elements))

    [<Fact>]
    let ``a candidate the kit already holds is not added again`` () : unit =
        withPlanWindow ReadWriteKitAccess (fun folder loaded ->
            let ready = planTable loaded
            let candidate = candidateOn TransmittedMueller ready
            let added = ready |> updateTableAndElementRotation (AddPlannedExperiment candidate)
            let experiment = Assert.Single added.experimentCollection.experiments
            let paths = pathsOf folder
            let setups = setupsOf added
            let experiments = experimentsOf added
            Assert.Single setups |> ignore
            Assert.Single experiments |> ignore

            let again = added |> updateTableAndElementRotation (AddPlannedExperiment candidate)

            Assert.Equal<ControlCaption option>(Some (ControlCaption $"Not added — {experiment.name.value} already plans it."), again.kitStatusCaption)
            Assert.Equal<string list>(paths, pathsOf folder)
            Assert.Equal<Setups.Setup list>(setups, setupsOf again)
            Assert.Equal<Experiment list>(experiments, experimentsOf again)
            Assert.Equal<TableAndElementRotationModel>({ added with kitStatus = again.kitStatus }, again))

    [<Fact>]
    let ``a candidate the table cannot capture, or a table with no sample slot, adds nothing, and a read-only kit adds nothing`` () : unit =
        withPlanWindow ReadWriteKitAccess (fun folder loaded ->
            let ready = planTable loaded
            let before = pathsOf folder

            // `det` sits on the main beam behind the sample, so it reads the transmitted light alone.
            Assert.Equal<MeasurementMode list>([ CaptureTransmitted ], availableCaptures ready sampleSlot)
            let refused = ready |> updateTableAndElementRotation (AddPlannedExperiment (candidateOn ReflectedMueller ready))
            Assert.Equal<ControlCaption option>(
                Some (ControlCaption "Not added — no detector of the table captures the reflected light of the sample."),
                refused.kitStatusCaption)
            Assert.Equal<TableAndElementRotationModel>({ ready with kitStatus = refused.kitStatus }, refused)
            Assert.Equal<string list>(before, pathsOf folder)

            // Without its sample the table has no Object 1 slot to hold the plate.
            let sampleIndex = ready.elements |> List.findIndex (fun slot -> slot.id = sampleSlot)
            let bare =
                { ready with selection = ElementSelected (SceneElementIndex sampleIndex) }
                |> updateTableAndElementRotation RemoveSelectedElement
            Assert.DoesNotContain<Setups.SetupSlot>(bare.elements, fun slot -> slot.id = sampleSlot)
            let unslotted = bare |> updateTableAndElementRotation (AddPlannedExperiment (candidateOn TransmittedMueller ready))
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Not added — the table has no sample slot."), unslotted.kitStatusCaption)
            Assert.Equal<TableAndElementRotationModel>({ bare with kitStatus = unslotted.kitStatus }, unslotted)
            Assert.Equal<string list>(before, pathsOf folder)
            Assert.Empty(setupsOf unslotted)
            Assert.Empty(experimentsOf unslotted))

        // A read-only kit is refused by the write guard before anything is asked.
        withPlanWindow ReadOnlyKitAccess (fun folder loaded ->
            let ready = planTable loaded
            Assert.Equal(ReadOnlyKitAccess, ready.kit.access)
            let before = pathsOf folder

            let refused = ready |> updateTableAndElementRotation (AddPlannedExperiment (candidateOn TransmittedMueller ready))

            Assert.Equal<ControlCaption option>(Some (ControlCaption "The kit is open read-only: nothing is saved."), refused.kitStatusCaption)
            Assert.Equal<TableAndElementRotationModel>({ ready with kitStatus = refused.kitStatus }, refused)
            Assert.Equal<string list>(before, pathsOf folder))
