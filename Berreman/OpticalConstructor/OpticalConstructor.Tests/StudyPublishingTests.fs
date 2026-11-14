namespace OpticalConstructor.Tests

open System
open OpticalConstructor.Domain
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.FitRuns
open OpticalConstructor.Domain.StudyPublishing
open Xunit

/// Spec 0050 S11.5 — Publish calibration's pure half: a fit run's item-owned values become its items' next revisions,
/// a link's follower takes its leader's, and a run with nothing new, of another study, over an item the kit lacks or
/// with an undetermined sigma is refused.
module StudyPublishingTests =

    let private retardance : ParameterName = ParameterName "c2.retardance"

    /// The draft's new item with `name` freed as `unknown`, failing with the refusal's text.
    let private itemFreeing (draft : ItemDraft) (name : ParameterName) (unknown : UnknownParameterData) : Item =
        match draft.toItem.withParameterState name (UnknownParameter unknown) with
        | Ok item -> item
        | Error error -> failwith $"'{draft.label.value}' refused {name.value}: {error.describe}"

    /// A retardance unknown from 1.5 within [1, 2], with no prior.
    let private retardanceData : UnknownParameterData =
        {
            start = ParameterValue 1.5
            bounds = { lower = ParameterValue 1.0; upper = ParameterValue 2.0 }
            priorOpt = None
        }

    /// A gain unknown from 1000 within [100, 10000], with no prior.
    let private gainData : UnknownParameterData =
        {
            start = ParameterValue 1000.0
            bounds = { lower = ParameterValue 100.0; upper = ParameterValue 10000.0 }
            priorOpt = None
        }

    let private circularPolarizer : ItemModelChoice = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId)

    let private cpl1 : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "40600000-0000-4000-8000-000000000011")
                label = ItemLabel "CPL1"
                model = circularPolarizer
                dial = DialMounted DialPositive
            }
        itemFreeing draft retardance retardanceData

    let private cpl2 : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "40600000-0000-4000-8000-000000000012")
                label = ItemLabel "CPL2"
                model = circularPolarizer
                dial = DialMounted DialPositive
            }
        itemFreeing draft retardance retardanceData

    let private phone : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "40600000-0000-4000-8000-000000000013")
                label = ItemLabel "Phone"
                model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.intensityDetectorModelId)
                dial = FixedMounting
            }
        itemFreeing draft ParameterName.gain gainData

    let private cpl1Retardance : StudyParameterRef = ItemParameterRef (cpl1.id, retardance)

    /// QZ-LR, whose one link makes CPL2's retardance follow CPL1's.
    let private qzLr : Study =
        {
            name = StudyName "QZ-LR"
            experiments = [ ExperimentName "E1" ]
            materials = []
            scopes = []
            links = [ { follower = ItemParameterRef (cpl2.id, retardance); leader = cpl1Retardance } ]
            sessionGains = []
            settings = StudyFitSettings.initial
            notes = StudyNotes.none
        }

    let private runName : FitRunName = FitRunName "QZ-LR@20261101T120000Z"

    let private retardanceEntry : FitRunEntry =
        {
            parameter = cpl1Retardance
            owner = ItemOwner cpl1.id
            start = ParameterValue 1.5
            fitted = ParameterValue 1.55
        }

    let private gainEntry : FitRunEntry =
        {
            parameter = ItemParameterRef (phone.id, ParameterName.gain)
            owner = SessionOwner (SessionName "Session 1")
            start = ParameterValue 1000.0
            fitted = ParameterValue 900.0
        }

    let private sigmaOf (entry : FitRunEntry) (sigmaOpt : ParameterSigma option) : FitEntrySigma =
        {
            entry = ({ parameter = entry.parameter; owner = entry.owner } : FitEntryRef)
            sigmaOpt = sigmaOpt
        }

    /// QZ-LR's stage-one run: CPL1's retardance fitted at 1.55 ± 0.002 per item, the Phone's gain at 900 per session with
    /// its sigma undetermined.
    let private calibrationRun : FitRun =
        {
            schemaVersion = FitRun.currentSchemaVersion
            name = runName
            study = StudyName "QZ-LR"
            stamp = ClockTime (DateTime(2026, 11, 1, 12, 0, 0, DateTimeKind.Utc))
            mode = StagedFit
            stage = CalibrationStage
            calibrationUse = CalibrationHeldFixed
            engine = NonlinearStudyEngine
            runs = [ ExperimentName "E1" ]
            dataRead = []
            parameters = []
            entries = [ retardanceEntry; gainEntry ]
            members = []
            objectMatrices = []
            results =
                {
                    sigmas = [ sigmaOf retardanceEntry (Some (ParameterSigma 0.002)); sigmaOf gainEntry None ]
                    problems = []
                    runResiduals = []
                    realizability = []
                    productChecks = []
                    profiles = []
                }
            notes = FitRunNotes.none
        }

    /// CPL1's retardance as the run calibrates it.
    let private calibratedRetardance : Parameter =
        {
            name = retardance
            state = CalibratedParameter { value = ParameterValue 1.55; sigma = ParameterSigma 0.002; fitRun = runName }
        }

    let private publishedOf (result : Result<Item list, PublishError>) : Item list =
        match result with
        | Ok items -> items
        | Error error -> failwith $"expected the published items, got {error.describe}"

    [<Fact>]
    let ``publishing a calibration writes each item-owned fitted value with its sigma and fit run into the item's next revision, and a linked follower takes its leader's`` () : unit =
        let published = publishedOf (qzLr.calibratedItemsWith calibrationRun [ cpl1; cpl2; phone ])

        // The Phone's gain is per session, so the Phone is not answered.
        Assert.Equal<ItemId list>([ cpl1.id; cpl2.id ], published |> List.map (fun item -> item.id))

        let othersOf (item : Item) : Parameter list = item.parameters |> List.filter (fun parameter -> parameter.name <> retardance)

        for original, revised in List.zip [ cpl1; cpl2 ] published do
            Assert.Equal<Parameter option>(Some ({ name = retardance; state = UnknownParameter retardanceData } : Parameter), original.tryFindParameter retardance)
            Assert.Equal(ItemRevision 2, revised.revision)
            Assert.Equal<ItemRevisionRecord list>([ ({ revision = ItemRevision.first; parameters = original.parameters } : ItemRevisionRecord) ], revised.history)
            Assert.Equal<Parameter option>(Some calibratedRetardance, revised.tryFindParameter retardance)
            Assert.Equal<ParameterName list>(original.parameters |> List.map (fun parameter -> parameter.name), revised.parameters |> List.map (fun parameter -> parameter.name))
            Assert.Equal<Parameter list>(othersOf original, othersOf revised)
            Assert.Equal(original, { revised with parameters = original.parameters; revision = original.revision; history = original.history })

    [<Fact>]
    let ``a session-owned value is no item calibration, and a run that calibrates nothing or only what is published is refused`` () : unit =
        match qzLr.calibratedItemsWith { calibrationRun with entries = [ gainEntry ] } [ cpl1; cpl2; phone ] with
        | Error (NoCalibrationErr reason) -> Assert.Contains($"'{runName.value}'", reason)
        | other -> Assert.Fail($"expected Error (NoCalibrationErr _), got %A{other}")

        let published = publishedOf (qzLr.calibratedItemsWith calibrationRun [ cpl1; cpl2; phone ])

        match qzLr.calibratedItemsWith calibrationRun published with
        | Error (CalibrationAlreadyPublishedErr reason) -> Assert.Contains($"'{runName.value}'", reason)
        | other -> Assert.Fail($"expected Error (CalibrationAlreadyPublishedErr _), got %A{other}")

        // CPL1 already holds its calibration, so only the follower is published, still taking its leader's.
        match published with
        | [ publishedCpl1; _ ] ->
            match qzLr.calibratedItemsWith calibrationRun [ publishedCpl1; cpl2 ] with
            | Ok [ revised ] ->
                Assert.Equal(cpl2.id, revised.id)
                Assert.Equal(ItemRevision 2, revised.revision)
                Assert.Equal<Parameter option>(Some calibratedRetardance, revised.tryFindParameter retardance)
            | other -> Assert.Fail($"expected CPL2 alone published, got %A{other}")
        | other -> Assert.Fail($"expected CPL1 and CPL2 published, got %A{other}")

    [<Fact>]
    let ``a value without a sigma, a run of another study or an item the kit lacks publishes nothing`` () : unit =
        let unrecorded = { calibrationRun with results = { calibrationRun.results with sigmas = [ sigmaOf gainEntry None ] } }
        let undetermined = { calibrationRun with results = { calibrationRun.results with sigmas = [ sigmaOf retardanceEntry None; sigmaOf gainEntry None ] } }

        for run in [ undetermined; unrecorded ] do
            match qzLr.calibratedItemsWith run [ cpl1; cpl2; phone ] with
            | Error (UndeterminedSigmaErr reason) ->
                Assert.Contains("'CPL1'", reason)
                Assert.Contains("c2.retardance", reason)
                Assert.Contains($"'{runName.value}'", reason)
            | other -> Assert.Fail($"expected Error (UndeterminedSigmaErr _), got %A{other}")

        match qzLr.calibratedItemsWith { calibrationRun with study = StudyName "LR" } [ cpl1; cpl2; phone ] with
        | Error (ForeignFitRunErr reason) ->
            Assert.Contains("'LR'", reason)
            Assert.Contains("'QZ-LR'", reason)
        | other -> Assert.Fail($"expected Error (ForeignFitRunErr _), got %A{other}")

        match qzLr.calibratedItemsWith calibrationRun [ cpl2; phone ] with
        | Error (UnknownPublishedItemErr reason) ->
            Assert.Contains("40600000-0000-4000-8000-000000000011", reason)
            Assert.Contains($"'{runName.value}'", reason)
        | other -> Assert.Fail($"expected Error (UnknownPublishedItemErr _), got %A{other}")

    [<Fact>]
    let ``every publish error renders its condition and its payload`` () : unit =
        let cases : (PublishError * string) list =
            [
                ForeignFitRunErr "foreign run", "foreign run"
                UnknownPublishedItemErr "missing item", "missing item"
                UndeterminedSigmaErr "open sigma", "open sigma"
                NoCalibrationErr "session gain", "session gain"
                CalibrationAlreadyPublishedErr "held values", "held values"
                PublishItemErr (UnknownItemParameterErr "absent axis"), "absent axis"
            ]

        let texts = cases |> List.map (fun (error, _) -> error.describe)

        Assert.Equal<string list>(
            [
                "the fit run belongs to another study: foreign run"
                "the kit has no such item: missing item"
                "the fit leaves a sigma undetermined: open sigma"
                "the fit run calibrates no item: session gain"
                "the calibration is already published: held values"
                "the item refused the calibration: the item has no such parameter: absent axis"
            ],
            texts)

        for error, payload in cases do
            Assert.Contains(payload, error.describe)

        Assert.Equal(cases.Length, (List.distinct texts).Length)
