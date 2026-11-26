namespace OpticalConstructor.Tests

open System
open System.Threading
open Xunit
open Berreman.Solvers
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Propagation
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.StudyForward
open OpticalConstructor.Domain.PlannerSymmetry
open OpticalConstructor.Domain.MeasurementPlanner
open OpticalConstructor.Storage
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Storage.PlannerCacheStore
open OpticalConstructor.Optimization.StudyEngine
open OpticalConstructor.Optimization.PlannerInformation
open OpticalConstructor.Optimization.PlannerRuns
open PlannerInputsTests

/// The Measurement Planner's run over a kit: each candidate's rows with progress and Cancel, the targets' priors and
/// the candidates the kit's experiments hold as the baseline, the open candidates ranked with a bound per target, and
/// the structural and instrument findings (spec 0050 S14.2; 001 §M).
module PlanRunTests =

    let private studyValue (result : Result<Study, StudyError>) : Study =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private reportValue (result : Result<PlanReport, PlannerError>) : PlanReport =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private rowsValue (result : Result<CandidateRows, PlannerError>) : CandidateRows =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    /// The study's copy of the probe quartz.
    let private copyOf (study : Study) : OpenedMaterial =
        study.materials |> List.find (fun (opened : OpenedMaterial) -> opened.version = crystalVersion)

    /// The study with the named component an unknown starting at its opened value, within +-50 % of it, under
    /// `priorOpt`.
    let private freedWithin (priorOpt : ParameterPrior option) (study : Study) (name : ParameterName) : Study =
        match (copyOf study).physics.componentValue name with
        | Some (ParameterValue value) ->
            let ends : float list = [ 0.5 * value; 1.5 * value ]

            let freed : UnknownParameterData =
                {
                    start = ParameterValue value
                    bounds = { lower = ParameterValue (List.min ends); upper = ParameterValue (List.max ends) }
                    priorOpt = priorOpt
                }

            study.withMaterialParameterState crystalVersion name (UnknownParameter freed) |> studyValue
        | None -> failwith $"The crystal's copy has no component {name.value}."

    /// A study of no experiment that opens the probe quartz.
    let private openedStudy : Study =
        Study.create (StudyName "Probe quartz plan") [] [] []
        |> Result.bind (fun (study : Study) -> study.openMaterial crystalVersion crystalEntry)
        |> studyValue

    /// The study freeing g11 and g33, neither under a prior.
    let planStudy : Study = [ g11; g33 ] |> List.fold (freedWithin None) openedStudy

    /// Laser, Polarimeter and QZ-Z.
    let private items : Item list = [ laser; polarimeter; qzZ ]

    /// The study's forward context over a kit of these items, setups and experiments: the Built-in library and
    /// materials, the data store of an empty in-memory kit, and the default solver parameters.
    let contextOver
        (study : Study)
        (kitItems : Item list)
        (setups : Setup list)
        (experiments : Experiment list)
        : StudyForwardContext =
        let kit : StudyContext =
            {
                studies = [ study ]
                experiments = experiments
                setups = setups
                items = kitItems
                resolveMaterial =
                    fun version ->
                        if version = crystalVersion then
                            Ok crystalEntry
                        else
                            let reason : string = $"material {version.materialId.value} is not the test's crystal"
                            Error (MaterialUnavailableErr reason)
            }

        let noData : ExperimentData.ExperimentDataProxy =
            KitProxy.createInMemory (InMemoryKitFolder.create ()) |> ExperimentDataStore.createFileBacked

        {
            kit = kit
            library = BuiltinCatalogue.library ()
            materials = BuiltinCatalogue.materialStore (fun _ -> [])
            experimentData = noData
            choiceOf = choiceOf
            solverParameters = SolverParameters.defaultValue
        }

    /// The planner's inputs of `context`'s study for the probe quartz.
    let private inputsIn (context : StudyForwardContext) : PlannerInputs =
        (List.head context.kit.studies).plannerInputsWith context.kit choiceOf crystalVersion |> inputsValue

    /// The D-optimal plan of `inputs` over `context`, for its study and the probe quartz.
    let private planOver
        (context : StudyForwardContext)
        (inputs : PlannerInputs)
        (rowsOf : PlanCandidate -> Result<CandidateRows, PlannerError>)
        (observer : PlanObserver)
        : Result<PlanReport, PlannerError> =
        let study : StudyName = (List.head context.kit.studies).name
        inputs.planWith context study crystalVersion DeterminantPlanCriterion rowsOf observer

    let private transmittedRows : CandidateRows =
        {
            jacobian = ResidualJacobian [| [| 1.0; 0.0 |] |]
            noise = NoiseVariance 1.0e-4
        }

    let private reflectedRows : CandidateRows =
        {
            jacobian = ResidualJacobian [| [| 0.0; 2.0 |] |]
            noise = NoiseVariance 1.0e-4
        }

    /// Literal rows by branch, counting the calls in `calls`.
    let private literalRows (calls : int ref) (candidate : PlanCandidate) : Result<CandidateRows, PlannerError> =
        calls.Value <- calls.Value + 1

        match candidate.observable with
        | TransmittedMueller -> Ok transmittedRows
        | ReflectedMueller -> Ok reflectedRows

    /// An observer recording what it is told into `told`, under a token never cancelled.
    let private recordingInto (told : ResizeArray<PlanProgress>) : PlanObserver =
        { reportCandidate = told.Add; cancellation = CancellationToken.None }

    /// An observer nothing reads.
    let private unread () : PlanObserver = recordingInto (ResizeArray<PlanProgress>())

    let private progressOf (evaluated : int) (total : int) : PlanProgress =
        { evaluated = PlanCandidateCount evaluated; total = PlanCandidateCount total }

    let private candidateOn (inputs : PlannerInputs) (observable : Observable) : PlanCandidate =
        inputs.candidates |> List.find (fun (candidate : PlanCandidate) -> candidate.observable = observable)

    let private stepCandidates (report : PlanReport) : PlanCandidate list =
        report.ranked |> List.map (fun (step : RankedPlanStep) -> step.candidate)

    let private cancelledReasonOf (result : Result<PlanReport, PlannerError>) : string =
        match result with
        | Error (PlanCancelledErr reason) -> reason
        | Error error -> failwith $"Expected a cancelled plan, got: {error.describe}"
        | Ok _ -> failwith "Expected a cancelled plan, got its report."

    [<Fact>]
    let ``a plan reports each candidate as it is evaluated and stops when cancelled`` () : unit =
        let context = contextOver planStudy items [] []
        let inputs = inputsIn context
        Assert.Equal(2, inputs.candidates.Length)

        let calls = ref 0
        let told = ResizeArray<PlanProgress>()
        let report = planOver context inputs (literalRows calls) (recordingInto told) |> reportValue

        Assert.Equal<PlanProgress list>([ progressOf 1 2; progressOf 2 2 ], List.ofSeq told)
        Assert.Equal(2, calls.Value)
        Assert.Equal(planStudy.name, report.study)
        Assert.Equal(crystalVersion, report.material)
        Assert.Equal(DeterminantPlanCriterion, report.criterion)

        // Both open, the larger determinant first: the reflected row's singular value is 2, the transmitted one's 1.
        Assert.Empty(report.inKit)
        Assert.Equal<PlanCandidate list>(List.rev inputs.candidates, stepCandidates report)
        Assert.Equal(MatrixRank 2, (List.last report.ranked).standing.information.rank)

        // A token the first report cancels stops the run before the second candidate.
        use source = new CancellationTokenSource()
        let cancelledCalls = ref 0
        let cancelledTold = ResizeArray<PlanProgress>()

        let cancelling : PlanObserver =
            {
                reportCandidate =
                    fun (progress : PlanProgress) ->
                        cancelledTold.Add progress
                        source.Cancel()
                cancellation = source.Token
            }

        let cancelled = planOver context inputs (literalRows cancelledCalls) cancelling
        Assert.Equal("1 of 2 candidates were evaluated", cancelledReasonOf cancelled)

        match cancelled with
        | Error error -> Assert.Equal("the plan was cancelled: 1 of 2 candidates were evaluated", error.describe)
        | Ok _ -> Assert.Fail "Expected a cancelled plan, got its report."

        Assert.Equal(1, cancelledCalls.Value)
        Assert.Equal<PlanProgress list>([ progressOf 1 2 ], List.ofSeq cancelledTold)

        // A token cancelled before the run evaluates nothing.
        let unreadCalls = ref 0
        let silent = ResizeArray<PlanProgress>()
        let alreadyCancelled : PlanObserver = { recordingInto silent with cancellation = source.Token }
        let beforeAny = planOver context inputs (literalRows unreadCalls) alreadyCancelled
        Assert.Equal("0 of 2 candidates were evaluated", cancelledReasonOf beforeAny)
        Assert.Equal(0, unreadCalls.Value)
        Assert.Empty(silent)

        // The first refusal of `rowsOf` stops the run, nothing reported.
        let refusing (_ : PlanCandidate) : Result<CandidateRows, PlannerError> = Error (PlannerForwardErr "unsolved")
        let refusedTold = ResizeArray<PlanProgress>()

        match planOver context inputs refusing (recordingInto refusedTold) with
        | Error error -> Assert.Equal(PlannerForwardErr "unsolved", error)
        | Ok _ -> Assert.Fail "Expected the forward's refusal, got a report."

        Assert.Empty(refusedTold)

    [<Fact>]
    let ``a candidate a kit experiment already holds joins the baseline and is not proposed again`` () : unit =
        let bench : Setup = benchHolding qzZDraft

        let sweep : ScanAxis list =
            [ sampleAxisOf VaryR2 PlanCandidate.incidenceRange; sampleAxisOf VaryR1 PlanCandidate.azimuthRange ]

        let e1 : Experiment = runOver "E1" bench sweep CaptureTransmitted
        let context = contextOver planStudy items [ bench ] [ e1 ]
        let inputs = inputsIn context
        let transmitted = candidateOn inputs TransmittedMueller
        let reflected = candidateOn inputs ReflectedMueller
        Assert.Equal(CandidateInKit e1.name, transmitted.coverageIn [ bench ] [ e1 ])

        let calls = ref 0
        let told = ResizeArray<PlanProgress>()
        let report = planOver context inputs (literalRows calls) (recordingInto told) |> reportValue

        // The held candidate's rows are still computed: they are what the kit already knows.
        Assert.Equal(2, calls.Value)
        Assert.Equal<PlanProgress list>([ progressOf 1 2; progressOf 2 2 ], List.ofSeq told)
        Assert.Equal<PlanCandidate list>([ transmitted ], report.inKit)
        Assert.Equal<PlanCandidate list>([ reflected ], stepCandidates report)

        Assert.Equal(MatrixRank 1, report.baseline.information.rank)
        Assert.Equal(transmittedRows.jacobian, report.baseline.information.rows)
        Assert.Equal<TargetBound list option>(None, report.baseline.boundsOpt)
        Assert.Equal(MatrixRank 2, (List.head report.ranked).standing.information.rank)

        // A planned run holds it as well.
        let planned = contextOver planStudy items [ bench ] [ e1.withRole PlannedRun ]
        let plannedReport = planOver planned inputs (literalRows calls) (unread ()) |> reportValue
        Assert.Equal<PlanCandidate list>([ transmitted ], plannedReport.inKit)

    [<Fact>]
    let ``a target's prior is information the kit already holds`` () : unit =
        let g33Centre : ParameterValue =
            match (copyOf openedStudy).physics.componentValue g33 with
            | Some value -> value
            | None -> failwith "The crystal's copy has no g33."

        let prior : ParameterPrior = { centre = g33Centre; sigma = ParameterSigma 1.0e-6 }
        let priorStudy : Study = freedWithin (Some prior) (freedWithin None openedStudy g11) g33
        let context = contextOver priorStudy items [] []
        let inputs = inputsIn context

        let g33Unit : ParameterValue =
            match (copyOf priorStudy).parameters |> List.find (fun (parameter : Parameter) -> parameter.name = g33) with
            | { state = UnknownParameter unknown } -> unknown.bounds.searchUnit
            | _ -> failwith "g33 is not an unknown of the study."

        // g33 = -1e-5 within +-50 %: a span of 1e-5, so its unit is 1e-7 and its row 0.1.
        let (ParameterValue unit) = g33Unit
        Assert.Equal(1.0e-7, unit, 1e-20)

        let expectedRow : CandidateRows =
            {
                jacobian = ResidualJacobian [| [| 0.0; unit / 1.0e-6 |] |]
                noise = NoiseVariance 1.0
            }

        let unknowing = contextOver planStudy items [] []
        Assert.Equal<Result<CandidateRows list, PlannerError>>(Ok [ expectedRow ], inputs.priorRows)
        Assert.Equal<Result<CandidateRows list, PlannerError>>(Ok [], (inputsIn unknowing).priorRows)

        let calls = ref 0
        let report = planOver context inputs (literalRows calls) (unread ()) |> reportValue

        // Before any candidate the prior makes g33 identifiable; the transmitted row, g11's, then completes the rank.
        Assert.Empty(report.inKit)
        Assert.Equal(MatrixRank 1, report.baseline.information.rank)
        Assert.Equal(expectedRow.jacobian, report.baseline.information.rows)
        Assert.Equal(candidateOn inputs TransmittedMueller, (List.head report.ranked).candidate)
        Assert.Equal(MatrixRank 2, (List.head report.ranked).standing.information.rank)

        // Without the prior the baseline knows nothing.
        let unknowingReport = planOver unknowing (inputsIn unknowing) (literalRows calls) (unread ()) |> reportValue
        Assert.Equal(MatrixRank 0, unknowingReport.baseline.information.rank)

        // A target the copy holds known is no target to scale.
        let knownStudy : Study =
            priorStudy.withMaterialParameterState crystalVersion g33 (KnownParameter g33Centre) |> studyValue

        let knownTarget : PlannerInputs = { inputs with prior = { inputs.prior with material = copyOf knownStudy } }

        match knownTarget.scalingWith with
        | Error (UnknownPlannerTargetErr reason) -> Assert.Equal("g33 is not an unknown of 'Probe quartz'", reason)
        | Error error -> Assert.Fail $"Expected an unknown target, got: {error.describe}"
        | Ok _ -> Assert.Fail "Expected an unknown target, got a scaling."

    [<Fact>]
    let ``a plan over a z-cut plate reports its structural findings, the rank its candidates raise and a bound on each target`` () : unit =
        let context = contextOver planStudy items [] []
        let inputs = inputsIn context
        let told = ResizeArray<PlanProgress>()

        let report =
            planOver context inputs (inputs.candidateRowsWith SolverParameters.defaultValue) (recordingInto told)
            |> reportValue

        Assert.Equal<PlanProgress list>([ progressOf 1 2; progressOf 2 2 ], List.ofSeq told)

        // Every plate a z-cut: g33 lies along every normal, and only an x-cut lays it across the wave.
        match report.structural.offAxisCutOpt with
        | Some need ->
            let onNormal : ParameterName list =
                need.targets |> List.map (fun (target : CrystalComponent) -> target.name)

            Assert.Equal<ParameterName list>([ g33 ], onNormal)
            Assert.Equal<SampleCut list>([ XCut ], need.cuts)
        | None -> Assert.Fail "Expected the off-axis-cut finding of a z-cut-only kit."

        Assert.Equal<Result<RecoverabilityReport, PlannerError>>(Ok report.structural, inputs.structuralReport)
        Assert.Equal(inputs.prior.crystal, report.nominal)
        Assert.Equal(MatrixRank 0, report.baseline.information.rank)
        Assert.Empty(report.inKit)
        Assert.Equal(2, report.ranked.Length)

        let last : RankedPlanStep = List.last report.ranked
        Assert.Equal(MatrixRank 2, last.standing.information.rank)

        match last.standing.boundsOpt with
        | Some bounds ->
            Assert.Equal<ParameterName list>([ g11; g33 ], bounds |> List.map (fun (bound : TargetBound) -> bound.name))

            for bound in bounds do
                let (ParameterSigma sigma) = bound.sigma
                let message : string = $"{bound.name.value}'s bound {sigma} is finite and above zero"
                Assert.True(Double.IsFinite sigma && sigma > 0.0, message)
        | None -> Assert.Fail "Expected a bound on each target at rank 2."

        Assert.Empty(report.instrument)
        Assert.Empty(report.unplacedPlates)
        Assert.Equal(inputs.polarimeterOpt, report.polarimeterOpt)

        // An edited detector noise reaches the candidate's information: the Jacobian is unchanged and the noise
        // variance rises by the element variance's rise over the 15 normalized of every 16 entries.
        let transmitted = candidateOn inputs TransmittedMueller
        let defaultRows = inputs.candidateRowsWith SolverParameters.defaultValue transmitted |> rowsValue

        let defaultElement : float =
            match inputs.polarimeterOpt with
            | Some detector ->
                match detector.noise.valueOf ParameterName.elementUncertainty with
                | Ok (ParameterValue element) -> element
                | Error error -> failwith error.describe
            | None -> failwith "Expected the kit's polarimeter."

        let editedElement : float = 2.0 * defaultElement

        let editedState : ParameterState = KnownParameter (ParameterValue editedElement)

        let editedPolarimeter : Item =
            polarimeter.withParameterState ParameterName.elementUncertainty editedState |> itemValue

        let edited = inputsIn (contextOver planStudy [ laser; editedPolarimeter; qzZ ] [] [])
        let editedRows = edited.candidateRowsWith SolverParameters.defaultValue transmitted |> rowsValue
        Assert.Equal(defaultRows.jacobian, editedRows.jacobian)

        let (NoiseVariance defaultNoise) = defaultRows.noise
        let (NoiseVariance editedNoise) = editedRows.noise
        let rise : float = editedNoise - defaultNoise
        let expectedRise : float = 15.0 / 16.0 * (editedElement * editedElement - defaultElement * defaultElement) / 3.0
        Assert.True(abs (rise - expectedRise) <= 1e-9 * expectedRise, $"the noise rose by {rise}, not {expectedRise}")

        // A budget its noise values refuse names the detector and the axis; no polarimeter has no budget.
        let brokenPolarimeter : Item =
            polarimeter.withParameterState ParameterName.rotationUncertainty (KnownParameter (ParameterValue -0.01))
            |> itemValue

        let broken = inputsIn (contextOver planStudy [ laser; brokenPolarimeter; qzZ ] [] [])

        match broken.candidateRowsWith SolverParameters.defaultValue transmitted with
        | Error (UnresolvedPlannerItemErr reason) ->
            Assert.StartsWith("Polarimeter: ", reason)
            Assert.Contains("rotationUncertainty = -0.01", reason)
        | Error error -> Assert.Fail $"Expected the detector's refused budget, got: {error.describe}"
        | Ok _ -> Assert.Fail "Expected the detector's refused budget, got rows."

        let undetected = inputsIn (contextOver planStudy [ laser; qzZ ] [] [])

        match undetected.candidateRowsWith SolverParameters.defaultValue transmitted with
        | Error (UnresolvedPlannerItemErr reason) ->
            Assert.Equal("no detector of the kit records a full Mueller matrix", reason)
        | Error error -> Assert.Fail $"Expected no polarimeter, got: {error.describe}"
        | Ok _ -> Assert.Fail "Expected no polarimeter, got rows."

    [<Fact>]
    let ``a plate added to the kit computes only its own candidates`` () : unit =
        let kitContext : KitContext =
            PlannerCacheTests.openOver (PlannerCacheTests.newKitFolder ()) ReadWriteKitAccess

        let computed = ref 0

        let planOf (kitItems : Item list) : PlanReport =
            let context = contextOver planStudy kitItems [] []
            let inputs = inputsIn context

            let cachedRowsOf (candidate : PlanCandidate) : Result<CandidateRows, PlannerError> =
                inputs.cacheKeyWith kitItems candidate
                |> Result.bind (fun (key : PlannerCacheKey) ->
                    kitContext.cachedPlannerRows key (fun () ->
                        computed.Value <- computed.Value + 1
                        inputs.candidateRowsWith SolverParameters.defaultValue candidate))

            planOver context inputs cachedRowsOf (unread ()) |> reportValue

        let first = planOf items
        Assert.Equal(2, computed.Value)
        Assert.Equal(2, first.ranked.Length)

        let second = planOf [ laser; polarimeter; qzZ; qzX ]
        Assert.Equal(3, computed.Value)
        Assert.Equal(3, second.ranked.Length)

        // QZ-X lays g33 across the wave, so the kit no longer needs an off-axis cut.
        Assert.Equal<OffAxisCutNeed option>(None, second.structural.offAxisCutOpt)
        Assert.Equal(MatrixRank 2, (List.last second.ranked).standing.information.rank)
