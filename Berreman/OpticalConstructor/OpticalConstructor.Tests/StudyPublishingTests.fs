namespace OpticalConstructor.Tests

open System
open System.Text
open Softellect.Sys.Primitives
open Berreman.Geometry
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.ItemEvaluation
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.FitRuns
open OpticalConstructor.Domain.StudyPublishing
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Storage.LibraryDatabaseStore
open OpticalConstructor.Storage.MaterialDatabaseStore
open Xunit

/// Spec 0050 S11.5 — calibration revisions and library promotion, preserving fit snapshots, identity,
/// provenance and pinned versions, and refusing read-only writes or values without one fitted result.
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
                UnfittedPromotionErr "unfitted material", "unfitted material"
                ScopedPromotionErr "per-run n1", "per-run n1"
                NotBlackBoxErr "CPL1", "CPL1"
                PromotionStudyErr (UnknownStudyParameterErr "missing n1"), "missing n1"
                PromotionEvaluationErr (MissingParameterValueErr "missing m00"), "missing m00"
                PromotionImportErr (ImportStoreUnavailableErr "closed library"), "closed library"
                PromotionEntryErr (EntryIdNotGuidErr "invalid preset identity"), "invalid preset identity"
                PromotionKitErr (KitLibraryKitErr (KitReadOnlyErr "promoteToLibrary")), "promoteToLibrary"
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
                "the fit run fitted none of its values: unfitted material"
                "the fit gives it no single value: per-run n1"
                "the item is not a black-box Mueller matrix: CPL1"
                "the study refused the promoted value: the study has no such parameter: missing n1"
                "the fitted matrix could not be evaluated: the values hold no number for the parameter: missing m00"
                "the library refused the promoted version: the import store is unavailable: closed library"
                "the promoted entry could not be built: the entry id is not a canonical GUID: invalid preset identity"
                "the kit could not snapshot the promoted version: the kit refused the library snapshot: the kit is open read-only: promoteToLibrary"
            ],
            texts)

        for error, payload in cases do
            Assert.Contains(payload, error.describe)

        Assert.Equal(cases.Length, (List.distinct texts).Length)

    let private n1 : ParameterName = ParameterName "n1"
    let private n2 : ParameterName = ParameterName "n2"
    let private e11 : ParameterName = ParameterName "e11"

    let private promotionVersion : MaterialVersionId =
        MaterialVersionId.firstOf (MaterialId (Guid.Parse "40700000-0000-4000-8000-000000000001"))

    let private promotionCrystal : CrystalParameters =
        AbsorbingTriclinicCrystal CrystalMaterialFixtures.absorbingTriclinicParameters

    let private promotionEntry : MaterialEntry =
        {
            id = promotionVersion.materialId
            name = EntryName "Promotion probe"
            category = CategoryIds.crystal
            description = Some (EntryDescription "Fifteen distinct crystal components")
            physics = CrystalParameterPhysics promotionCrystal
        }

    let private publishingStudyOf (result : Result<Study, StudyError>) : Study =
        match result with
        | Ok study -> study
        | Error error -> failwith error.describe

    let private materialStudy : Study =
        let freeComponent (name : ParameterName) (study : Study) : Study =
            let axis = promotionCrystal.parameterAxes |> List.find (fun axis -> axis.name = name)
            let unknown =
                UnknownParameter
                    {
                        start = axis.parameterValueOf promotionCrystal
                        bounds =
                            {
                                lower = ParameterValue 0.0
                                upper = ParameterValue 3.0
                            }
                        priorOpt = None
                    }
            study.withMaterialParameterState promotionVersion name unknown |> publishingStudyOf

        qzLr.openMaterial promotionVersion promotionEntry
        |> publishingStudyOf
        |> freeComponent n1
        |> freeComponent e11

    let private fittedMaterialEntry (name : ParameterName) (fitted : ParameterValue) : FitRunEntry =
        let axis = promotionCrystal.parameterAxes |> List.find (fun axis -> axis.name = name)
        {
            parameter = MaterialParameterRef (promotionVersion, name)
            owner = MaterialOwner promotionVersion
            start = axis.parameterValueOf promotionCrystal
            fitted = fitted
        }

    let private n1Entry : FitRunEntry = fittedMaterialEntry n1 (ParameterValue 1.7)
    let private e11Entry : FitRunEntry = fittedMaterialEntry e11 (ParameterValue 0.02)

    let private materialRun : FitRun =
        {
            calibrationRun with
                entries = [ n1Entry; e11Entry ]
                parameters =
                    materialStudy.materials
                    |> List.collect (fun material ->
                        material.parameters
                        |> List.map (fun parameter ->
                            {
                                parameter = MaterialParameterRef (material.version, parameter.name)
                                state = parameter.state
                            }))
        }

    [<Fact>]
    let ``a promoted material is its opened version's entry with the crystal its fit run produced`` () : unit =
        // Spec 0050 S11.5: promotion must read the fit's snapshot after the working copy changes.
        let editedStudy =
            materialStudy.withMaterialParameterState promotionVersion n2 (KnownParameter (ParameterValue 2.8))
            |> publishingStudyOf

        match editedStudy.promotedMaterialWith materialRun promotionVersion promotionEntry with
        | Ok promoted ->
            let restoredPhysics =
                {
                    promoted with
                        physics = promotionEntry.physics
                }
            Assert.Equal(promotionEntry, restoredPhysics)
            match promoted.physics with
            | CrystalParameterPhysics crystal ->
                Assert.Equal(15, crystal.parameterAxes.Length)
                for axis in crystal.parameterAxes do
                    let expected =
                        if axis.name = n1 then ParameterValue 1.7
                        elif axis.name = e11 then ParameterValue 0.02
                        else axis.parameterValueOf promotionCrystal
                    Assert.Equal(expected, axis.parameterValueOf crystal)
            | ComplexityPhysics _ -> Assert.Fail "expected crystal-parameter physics"
        | Error error -> Assert.Fail error.describe

        let editedCopy = editedStudy.materials |> List.find (fun material -> material.version = promotionVersion)
        let heldN2 = editedCopy.parameters |> List.find (fun parameter -> parameter.name = n2)
        Assert.Equal(KnownParameter (ParameterValue 2.8), heldN2.state)

    [<Fact>]
    let ``a material component fitted per run, or a material the study did not open or the run did not fit, is not promoted`` () : unit =
        let unopened =
            {
                promotionVersion with
                    version = VersionNumber 2
            }
        for owner in [ ExperimentOwner (ExperimentName "E1"); SessionOwner (SessionName "Session 1"); MaterialOwner unopened ] do
            let scopedEntry =
                {
                    n1Entry with
                        owner = owner
                }
            let scoped =
                {
                    materialRun with
                        entries = [ scopedEntry; e11Entry ]
                }
            match materialStudy.promotedMaterialWith scoped promotionVersion promotionEntry with
            | Error (ScopedPromotionErr reason) -> Assert.Contains("n1", reason)
            | Error error -> Assert.Fail error.describe
            | Ok _ -> Assert.Fail "expected scoped material promotion to be refused"

        match materialStudy.promotedMaterialWith materialRun unopened promotionEntry with
        | Error (UnfittedPromotionErr reason) ->
            Assert.Contains($"{unopened.materialId.value}", reason)
            Assert.Contains("v2", reason)
        | Error error -> Assert.Fail error.describe
        | Ok _ -> Assert.Fail "expected unopened version to be refused"

        let unfitted =
            {
                materialRun with
                    entries = [ retardanceEntry ]
            }
        match materialStudy.promotedMaterialWith unfitted promotionVersion promotionEntry with
        | Error (UnfittedPromotionErr reason) ->
            Assert.Contains(promotionEntry.name.value, reason)
            Assert.Contains(runName.value, reason)
        | Error error -> Assert.Fail error.describe
        | Ok _ -> Assert.Fail "expected unfitted material to be refused"

        let foreign =
            {
                materialRun with
                    study = StudyName "Foreign"
            }
        match materialStudy.promotedMaterialWith foreign promotionVersion promotionEntry with
        | Error (ForeignFitRunErr reason) ->
            Assert.Contains("Foreign", reason)
            Assert.Contains(materialStudy.name.value, reason)
        | Error error -> Assert.Fail error.describe
        | Ok _ -> Assert.Fail "expected foreign fit run to be refused"

        let invalidEntry =
            {
                n1Entry with
                    fitted = ParameterValue Double.NaN
            }
        let invalidRun =
            {
                materialRun with
                    entries = [ invalidEntry; e11Entry ]
            }
        match materialStudy.promotedMaterialWith invalidRun promotionVersion promotionEntry with
        | Error (PromotionStudyErr (InvalidStudyParameterErr reason)) -> Assert.Contains("n1", reason)
        | Error error -> Assert.Fail error.describe
        | Ok _ -> Assert.Fail "expected invalid fitted component to be refused"

        let missingSnapshot =
            {
                materialRun with
                    parameters = materialRun.parameters |> List.filter (fun parameter -> parameter.parameter.name <> n2)
            }
        match materialStudy.promotedMaterialWith missingSnapshot promotionVersion promotionEntry with
        | Error (PromotionStudyErr (UnknownStudyParameterErr reason)) -> Assert.Contains("n2", reason)
        | Error error -> Assert.Fail error.describe
        | Ok _ -> Assert.Fail "expected a missing fit snapshot component to be refused"

    let private blackBoxChoice : ItemModelChoice = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.blackBoxMuellerModelId)
    let private m00 : ParameterName = ParameterName "m00"
    let private m01 : ParameterName = ParameterName "m01"
    let private m22 : ParameterName = ParameterName "m22"

    let private qz : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "40700000-0000-4000-8000-000000000002")
                label = ItemLabel "QZ"
                model = blackBoxChoice
                dial = DialMounted DialPositive
            }
        draft.toItem

    let private m00Entry : FitRunEntry =
        {
            parameter = ItemParameterRef (qz.id, m00)
            owner = ItemOwner qz.id
            start = ParameterValue 1.0
            fitted = ParameterValue 0.9
        }

    let private m01Entry : FitRunEntry =
        {
            parameter = ItemParameterRef (qz.id, m01)
            owner = ItemOwner qz.id
            start = ParameterValue 0.0
            fitted = ParameterValue 0.25
        }

    let private blackBoxRun : FitRun =
        {
            calibrationRun with
                entries = [ m00Entry; m01Entry ]
                parameters =
                    qz.parameters
                    |> List.map (fun parameter ->
                        {
                            parameter = ItemParameterRef (qz.id, parameter.name)
                            state = parameter.state
                        })
        }

    [<Fact>]
    let ``a promoted black box is a custom-Mueller polarizer preset of its fitted matrix under the item's GUID and label`` () : unit =
        let editedItem =
            match qz.withParameterState m22 (KnownParameter (ParameterValue 0.3)) with
            | Ok item -> item
            | Error error -> failwith error.describe

        match blackBoxRun.promotedBlackBoxWith editedItem blackBoxChoice with
        | Ok preset ->
            Assert.Equal("40700000-0000-4000-8000-000000000002", preset.id.value)
            Assert.Equal(qz.id.value, qz.id.toLibraryEntryId.value)
            Assert.Equal(qz.id.toElementValueId, preset.id)
            Assert.Equal(EntryName "QZ", preset.name)
            Assert.Equal(qz.label.entryName, preset.name)
            Assert.Equal(CustomMueller, preset.category)
            Assert.Equal(UserManaged, preset.protection)
            match preset.behavior with
            | ConstantMueller [ muellerComponent ] ->
                Assert.Equal(Angle.zero, muellerComponent.offset)
                for row in 0 .. 3 do
                    for column in 0 .. 3 do
                        let expected =
                            match row, column with
                            | 0, 0 -> ParameterValue 0.9
                            | 0, 1 -> ParameterValue 0.25
                            | _ when row = column -> ParameterValue 1.0
                            | _ -> ParameterValue 0.0
                        Assert.Equal(expected.value, muellerComponent.matrix.element row column, 1.0e-15)
            | _ -> Assert.Fail "expected one constant Mueller component"
        | Error error -> Assert.Fail error.describe

        match blackBoxRun.promotedBlackBoxWith cpl1 circularPolarizer with
        | Error (NotBlackBoxErr reason) -> Assert.Contains("CPL1", reason)
        | Error error -> Assert.Fail error.describe
        | Ok _ -> Assert.Fail "expected circular-polarizer model to be refused"

        for owner in [ ExperimentOwner (ExperimentName "E1"); SessionOwner (SessionName "Session 1"); ItemOwner cpl1.id ] do
            let scopedEntry =
                {
                    m01Entry with
                        owner = owner
                }
            let scoped =
                {
                    blackBoxRun with
                        entries = [ m00Entry; scopedEntry ]
                }
            match scoped.promotedBlackBoxWith qz blackBoxChoice with
            | Error (ScopedPromotionErr reason) -> Assert.Contains("m01", reason)
            | Error error -> Assert.Fail error.describe
            | Ok _ -> Assert.Fail "expected scoped Mueller element to be refused"

        let unfitted =
            {
                blackBoxRun with
                    entries = [ retardanceEntry ]
            }
        match unfitted.promotedBlackBoxWith qz blackBoxChoice with
        | Error (UnfittedPromotionErr reason) ->
            Assert.Contains("QZ", reason)
            Assert.Contains(runName.value, reason)
        | Error error -> Assert.Fail error.describe
        | Ok _ -> Assert.Fail "expected unfitted black box to be refused"

        let invalidEntry =
            {
                m00Entry with
                    fitted = ParameterValue Double.NaN
            }
        let invalid =
            {
                blackBoxRun with
                    entries = [ invalidEntry; m01Entry ]
            }
        match invalid.promotedBlackBoxWith qz blackBoxChoice with
        | Error (PromotionEvaluationErr (ParameterOutOfRangeErr reason)) -> Assert.Contains("m00", reason)
        | Error error -> Assert.Fail error.describe
        | Ok _ -> Assert.Fail "expected invalid fitted matrix element to be refused"

        let missingSnapshot =
            {
                blackBoxRun with
                    parameters = blackBoxRun.parameters |> List.filter (fun parameter -> parameter.parameter.name <> m22)
            }
        match missingSnapshot.promotedBlackBoxWith qz blackBoxChoice with
        | Error (PromotionEvaluationErr (MissingParameterValueErr reason)) -> Assert.Contains("m22", reason)
        | Error error -> Assert.Fail error.describe
        | Ok _ -> Assert.Fail "expected a missing fit snapshot element to be refused"

    [<Fact>]
    let ``a fit run's provenance names its kit, its study's file and its own file`` () : unit =
        let kit = KitId (Guid.Parse "40700000-0000-4000-8000-000000000003")
        let expected =
            FitProvenance
                {
                    kit = kit
                    studyStem = FileStem "QZ-LR"
                    fitRunStem = FileStem "QZ-LR@20261101T120000Z"
                }
        Assert.Equal(expected, calibrationRun.provenanceIn kit)

        let unsafeNames =
            {
                calibrationRun with
                    study = StudyName "QZ/LR"
                    name = FitRunName "QZ/LR@12:00"
            }
        let safeProvenance =
            FitProvenance
                {
                    kit = kit
                    studyStem = FileStem "QZ_LR"
                    fitRunStem = FileStem "QZ_LR@12_00"
                }
        Assert.Equal(safeProvenance, unsafeNames.provenanceIn kit)

    [<Fact>]
    let ``a fit run offers each opened material and black box it fitted for promotion`` () : unit =
        let choiceOf (item : Item) : ItemModelChoice option =
            if item.id = cpl1.id then Some circularPolarizer
            elif item.id = qz.id then Some blackBoxChoice
            else None

        let run =
            {
                materialRun with
                    entries = [ m00Entry; retardanceEntry; n1Entry ]
            }
        Assert.Equal<PromotionSubject list>(
            [ MaterialPromotion promotionVersion; BlackBoxPromotion qz.id ],
            run.promotionSubjectsWith materialStudy [ cpl1; qz; phone ] choiceOf)

        let onlyCpl1 =
            {
                run with
                    entries = [ retardanceEntry ]
            }
        Assert.Empty(onlyCpl1.promotionSubjectsWith materialStudy [ cpl1; qz ] choiceOf)

        let secondVersion =
            {
                promotionVersion with
                    version = VersionNumber 2
            }
        let firstCopy = materialStudy.materials |> List.head
        let secondCopy =
            {
                firstCopy with
                    version = secondVersion
            }
        let secondItem =
            {
                qz with
                    id = ItemId (Guid.Parse "40700000-0000-4000-8000-000000000004")
                    label = ItemLabel "QZ2"
            }
        let bothStudy =
            {
                materialStudy with
                    materials = [ secondCopy; firstCopy ]
            }
        let bothRun =
            {
                run with
                    entries =
                        run.entries @
                        [
                            e11Entry
                            {
                                n1Entry with
                                    parameter = MaterialParameterRef (secondVersion, n1)
                                    owner = MaterialOwner secondVersion
                            }
                            {
                                m00Entry with
                                    parameter = ItemParameterRef (secondItem.id, m00)
                                    owner = ItemOwner secondItem.id
                            }
                            m01Entry
                        ]
            }
        let bothChoices (item : Item) : ItemModelChoice option =
            if item.id = secondItem.id then Some blackBoxChoice else choiceOf item

        Assert.Equal<PromotionSubject list>(
            [ MaterialPromotion secondVersion; MaterialPromotion promotionVersion; BlackBoxPromotion secondItem.id; BlackBoxPromotion qz.id ],
            bothRun.promotionSubjectsWith bothStudy [ secondItem; cpl1; qz; phone ] bothChoices)

        Assert.Equal<PromotionSubject list>(
            [ MaterialPromotion secondVersion; MaterialPromotion promotionVersion ],
            bothRun.promotionSubjectsWith bothStudy [ secondItem; qz ] (fun _ -> None))

    type private PromotionKitFixture =
        {
            folder : InMemoryKitFolder
            library : KitLibraryContext
            materials : MaterialProxy
        }

    let private promotionKit (database : LibraryDatabase) : PromotionKitFixture =
        let folder = InMemoryKitFolder.create ()
        let proxy = KitProxy.createInMemory folder
        let mutable next = ClockTime (DateTime(2026, 11, 14, 12, 0, 0, DateTimeKind.Utc))
        let clock : ClockProvider =
            {
                now = fun () ->
                    let current = next
                    next <- ClockTime (current.value.AddSeconds 1.0)
                    current
            }
        let kitId = KitId (Guid.Parse "40800000-0000-4000-8000-000000000001")
        let kitName = KitName "Promotion kit"
        let author = KitAuthor "Study publishing tests"
        let kitFile = KitFile.create kitId kitName author (clock.now ())
        proxy.initialiseKit kitFile |> Result.defaultWith (fun error -> failwith error.describe)

        let library : KitLibraryContext =
            {
                kit = KitContext.openKit proxy ReadWriteKitAccess clock |> Result.defaultWith (fun error -> failwith error.describe)
                imports = CatalogueImportContext.create (CatalogueImportProxy.createInDatabase database)
            }
        let materials = MaterialProxy.createInDatabase database
        Assert.Equal<Result<unit, MaterialError>>(Ok (), materials.saveMaterial promotionEntry)
        library.recordStudy materialStudy |> Result.defaultWith (fun error -> failwith error.describe)
        {
            folder = folder
            library = library
            materials = materials
        }

    let private promotedVersionOf (result : Result<LibraryVersionRef, PublishError>) : LibraryVersionRef =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private promotionEnvelope (library : KitLibraryContext) (version : LibraryVersionRef) : LibraryEntryEnvelope =
        match library.imports.proxy.tryHeldEnvelope version.entryId version.version with
        | Ok (Some envelope) -> envelope
        | Ok None -> failwith $"the library lost {version.describe}"
        | Error error -> failwith error.describe

    let private assertPromotionPins (database : LibraryDatabase) (versions : LibraryVersionRef list) : unit =
        database.run (fun context ->
            for version in versions do
                match context.tryStoredVersion version.entryId version.version with
                | Ok (Some stored) -> Assert.Equal(VersionPinned, stored.state.pin)
                | Ok None -> Assert.Fail($"the library lost {version.describe}")
                | Error error -> Assert.Fail(error.describe))
        |> Result.defaultWith (fun error -> failwith error.describe)

    // In-memory snapshots are envelopes, while the other files retain their bytes (spec 0050 S11.5).
    let private promotionKitBytes (fixture : PromotionKitFixture) : Map<KitRelativePath, FileByteContent> =
        let snapshots =
            fixture.library.kit.proxy.listSnapshots ()
            |> Result.defaultWith (fun error -> failwith error.describe)
            |> List.map (fun envelope -> envelope.snapshotPath, envelope)
            |> Map.ofList
        let folders = [ KitRelativePath "data"; KitRelativePath "library"; KitRelativePath.appCacheFolder ]
        fixture.folder.paths ()
        |> List.except folders
        |> List.map (fun path ->
            let bytes =
                match Map.tryFind path snapshots with
                | Some envelope ->
                    let text = envelope.fileText |> Result.defaultWith (fun error -> failwith error.describe)
                    FileByteContent (Encoding.UTF8.GetBytes text.value)
                | None ->
                    let source = fixture.library.kit.proxy.readLabFile path |> Result.defaultWith (fun error -> failwith error.describe)
                    source.content
            path, bytes)
        |> Map.ofList

    [<Fact>]
    let ``promoting a fitted material mints its next library version under the fit's provenance and snapshots it into the kit`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let fixture = promotionKit database
        let library = fixture.library
        let original : LibraryVersionRef =
            {
                entryId = promotionVersion.materialId.toLibraryEntryId
                version = promotionVersion.version
            }
        let originalEnvelope = promotionEnvelope library original
        let before = promotionKitBytes fixture
        assertPromotionPins database [ original ]

        let promoted = library.promoteMaterial materialStudy materialRun promotionVersion promotionEntry |> promotedVersionOf
        let expectedVersion =
            {
                original with
                    version = VersionNumber 2
            }
        Assert.Equal(expectedVersion, promoted)
        let promotedMaterialVersion =
            {
                promotionVersion with
                    version = promoted.version
            }
        let expected =
            materialStudy.promotedMaterialWith materialRun promotionVersion promotionEntry
            |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some expected), fixture.materials.resolveVersion promotedMaterialVersion)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some promotionEntry), fixture.materials.resolveVersion promotionVersion)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some expected), fixture.materials.tryGetMaterial promotionVersion.materialId)

        let envelope = promotionEnvelope library promoted
        let provenance =
            FitProvenance
                {
                    kit = library.kit.kitFile.id
                    studyStem = FileStem "QZ-LR"
                    fitRunStem = materialRun.name.fileStem
                }
        Assert.Equal(provenance, envelope.content.provenance)
        match envelope.content.payload, expected.physics with
        | CrystalMaterialPayload crystal, CrystalParameterPhysics expectedCrystal -> Assert.Equal(expectedCrystal, crystal)
        | _ -> Assert.Fail "the promoted material must retain its crystal payload"
        Assert.NotEqual(originalEnvelope.contentHash, envelope.contentHash)
        Assert.Equal(originalEnvelope, promotionEnvelope library original)
        let firstPath = KitRelativePath $"library/{promotionVersion.materialId.value:D}@v1.material-json"
        let secondPath = KitRelativePath $"library/{promotionVersion.materialId.value:D}@v2.material-json"
        Assert.Contains(firstPath, fixture.folder.paths ())
        Assert.Contains(secondPath, fixture.folder.paths ())
        let snapshots = library.kit.proxy.listSnapshots () |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Contains<LibraryEntryEnvelope>(envelope, snapshots)
        Assert.Contains<LibraryEntryEnvelope>(originalEnvelope, snapshots)
        assertPromotionPins database [ original; promoted ]
        let storedStudy = library.kit.proxy.readStudies () |> Result.defaultWith (fun error -> failwith error.describe) |> Assert.Single
        Assert.Equal(materialStudy, storedStudy)
        let after = promotionKitBytes fixture
        for KeyValue(path, bytes) in before do
            Assert.Equal(bytes, Map.find path after)

    [<Fact>]
    let ``promoting a black box adds a custom-Mueller preset at version 1, and promoting it again mints version 2`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let fixture = promotionKit database
        let library = fixture.library
        Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(Ok None, library.imports.proxy.tryHeldEntry qz.id.toLibraryEntryId)
        let first = library.promoteBlackBox blackBoxRun qz blackBoxChoice |> promotedVersionOf
        Assert.Equal(qz.id.toLibraryEntryId, first.entryId)
        Assert.Equal(VersionNumber.first, first.version)
        let firstEnvelope = promotionEnvelope library first
        let firstPath = KitRelativePath $"library/{qz.id.value:D}@v1.polarizer-json"
        Assert.Contains(firstPath, fixture.folder.paths ())
        assertPromotionPins database [ first ]
        let before = promotionKitBytes fixture

        let secondM00 =
            {
                m00Entry with
                    fitted = ParameterValue 0.8
            }
        let secondRun =
            {
                blackBoxRun with
                    name = FitRunName "QZ-LR@20261101T120100Z"
                    stamp = ClockTime (DateTime(2026, 11, 1, 12, 1, 0, DateTimeKind.Utc))
                    entries = [ secondM00; m01Entry ]
            }
        let second = library.promoteBlackBox secondRun qz blackBoxChoice |> promotedVersionOf
        let expectedSecond =
            {
                first with
                    version = VersionNumber 2
            }
        Assert.Equal(expectedSecond, second)
        let secondPath = KitRelativePath $"library/{qz.id.value:D}@v2.polarizer-json"
        Assert.Contains(firstPath, fixture.folder.paths ())
        Assert.Contains(secondPath, fixture.folder.paths ())
        let snapshots = library.kit.proxy.listSnapshots () |> Result.defaultWith (fun error -> failwith error.describe)
        for version, run in [ first, blackBoxRun; second, secondRun ] do
            let envelope = promotionEnvelope library version
            Assert.Equal(run.provenanceIn library.kit.kitFile.id, envelope.content.provenance)
            let preset = envelope.content.toPolarizerPreset |> Result.defaultWith (fun error -> failwith error.describe)
            let expected = run.promotedBlackBoxWith qz blackBoxChoice |> Result.defaultWith (fun error -> failwith error.describe)
            Assert.Equal(expected, preset)
            Assert.Equal(CustomMueller, preset.category)
            Assert.Equal(qz.id.toElementValueId, preset.id)
            Assert.Contains<LibraryEntryEnvelope>(envelope, snapshots)
        Assert.Equal(firstEnvelope, promotionEnvelope library first)
        Assert.NotEqual(firstEnvelope.contentHash, (promotionEnvelope library second).contentHash)
        match library.imports.proxy.tryHeldEntry first.entryId with
        | Ok (Some held) ->
            Assert.Equal(second.version, held.latest)
            Assert.Equal<VersionNumber list>([ first.version; second.version ], held.versions |> List.map _.version)
        | Ok None -> Assert.Fail "the promoted black box is absent"
        | Error error -> Assert.Fail error.describe
        assertPromotionPins database [ first; second ]
        let after = promotionKitBytes fixture
        for KeyValue(path, bytes) in before do
            Assert.Equal(bytes, Map.find path after)

    [<Fact>]
    let ``a read-only kit promotes nothing`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let fixture = promotionKit database
        let writable = fixture.library
        let readOnlyKit =
            KitContext.openKit (KitProxy.createInMemory fixture.folder) ReadOnlyKitAccess writable.kit.clock
            |> Result.defaultWith (fun error -> failwith error.describe)
        let library =
            {
                writable with
                    kit = readOnlyKit
            }
        let entryIds = [ promotionVersion.materialId.toLibraryEntryId; qz.id.toLibraryEntryId ]
        let heldBefore = entryIds |> List.map writable.imports.proxy.tryHeldEntry
        let pathsBefore = fixture.folder.paths ()
        let bytesBefore = promotionKitBytes fixture
        let expected : Result<LibraryVersionRef, PublishError> = Error (PromotionKitErr (KitLibraryKitErr (KitReadOnlyErr "promoteToLibrary")))
        Assert.Equal<Result<LibraryVersionRef, PublishError>>(expected, library.promoteMaterial materialStudy materialRun promotionVersion promotionEntry)
        Assert.Equal<Result<LibraryVersionRef, PublishError>>(expected, library.promoteBlackBox blackBoxRun qz blackBoxChoice)

        // A refusal must precede even reads and sealing, not merely rely on the kit's write guard (spec 0050 S11.5).
        let unexpectedIo () : 'Value = failwith "read-only promotion performed IO"
        let guardedImports =
            {
                library.imports with
                    sealEntry = fun _ -> unexpectedIo ()
                    proxy =
                        {
                            library.imports.proxy with
                                tryHeldEntry = fun _ -> unexpectedIo ()
                                tryHeldEnvelope = fun _ _ -> unexpectedIo ()
                                putEntryVersion = fun _ -> unexpectedIo ()
                                pinEntryVersion = fun _ _ -> unexpectedIo ()
                        }
            }
        let guarded =
            {
                library with
                    imports = guardedImports
                    kit =
                        {
                            readOnlyKit with
                                proxy =
                                    {
                                        readOnlyKit.proxy with
                                            listSnapshots = fun () -> unexpectedIo ()
                                            writeSnapshot = fun _ -> unexpectedIo ()
                                    }
                        }
            }
        Assert.Equal<Result<LibraryVersionRef, PublishError>>(expected, guarded.promoteMaterial materialStudy materialRun promotionVersion promotionEntry)
        Assert.Equal<Result<LibraryVersionRef, PublishError>>(expected, guarded.promoteBlackBox blackBoxRun qz blackBoxChoice)
        Assert.Equal<Result<HeldEntry option, CatalogueImportError> list>(heldBefore, entryIds |> List.map writable.imports.proxy.tryHeldEntry)
        Assert.Equal<KitRelativePath list>(pathsBefore, fixture.folder.paths ())
        Assert.Equal<Map<KitRelativePath, FileByteContent>>(bytesBefore, promotionKitBytes fixture)
