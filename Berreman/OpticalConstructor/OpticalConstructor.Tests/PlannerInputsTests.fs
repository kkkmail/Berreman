namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Propagation
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.MaterialStore
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.ItemEvaluation
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.PlannerSymmetry
open OpticalConstructor.Domain.MeasurementPlanner
open CrystalMaterialFixtures

/// The Measurement Planner's inputs: the lab profile (the kit's sources, optics and detectors read from their models,
/// and what an intensity detector reaches beside them), the kit's plates of a material, and the study's prior and targets;
/// then the kit's looks and its structural report, its candidates, and its instrument's polarizer and compensator states
/// (spec 0050 S14.1, S14.2; 001 §M).
module PlannerInputsTests =

    let private itemValue (result : Result<Item, ItemError>) : Item =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private profileValue (result : Result<LabProfile, PlannerError>) : LabProfile =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    /// The Built-in polarizer preset of that id, failing when the catalogue holds another kind under it.
    let private polarizerPresetOf (id : ElementValueId) : Library.PolarizerPreset =
        match BuiltinCatalogue.preset id with
        | Library.PolarizerItem polarizer -> polarizer
        | Library.SampleItem _
        | Library.SourceItem _
        | Library.DetectorItem _ -> failwith $"The Built-in preset '{id.value}' is not a polarizer."

    /// A fixed item of that id and label over the model choice.
    let private fixedDraft (id : string) (label : string) (model : ItemModelChoice) : ItemDraft =
        {
            id = ItemId (Guid.Parse id)
            label = ItemLabel label
            model = model
            dial = FixedMounting
        }

    /// A fixed item over the shipped standalone item model of that id (step 210).
    let private libraryDraft (id : string) (label : string) (modelId : ItemModelId) : ItemDraft =
        fixedDraft id label (LibraryModelChoice (BuiltinCatalogue.itemModel modelId))

    let private linearPolarizerChoice : ItemModelChoice =
        PolarizerPresetChoice (
            BuiltinCatalogue.itemModel ItemModelIds.idealPolarizerItemModelId,
            polarizerPresetOf Library.PresetIds.polarizerLp
        )

    let private laserDraft : ItemDraft =
        fixedDraft
            "47000000-0000-4000-8000-000000000001"
            "Laser"
            (SourcePresetChoice (
                BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId,
                BuiltinCatalogue.source Library.PresetIds.heNe633
            ))

    let private lp1Draft : ItemDraft = fixedDraft "47000000-0000-4000-8000-000000000002" "LP1" linearPolarizerChoice

    let private lp2Draft : ItemDraft = fixedDraft "47000000-0000-4000-8000-000000000003" "LP2" linearPolarizerChoice

    let private cpl1Draft : ItemDraft =
        libraryDraft "47000000-0000-4000-8000-000000000004" "CPL1" ItemModelIds.circularPolarizerModelId

    let private retDraft : ItemDraft =
        libraryDraft "47000000-0000-4000-8000-000000000005" "RET" ItemModelIds.retarderPlateModelId

    let private phoneDraft : ItemDraft =
        libraryDraft "47000000-0000-4000-8000-000000000006" "Phone" ItemModelIds.intensityDetectorModelId

    let private polarimeterDraft : ItemDraft =
        libraryDraft "47000000-0000-4000-8000-000000000007" "Polarimeter" ItemModelIds.muellerPolarimeterModelId

    let private ellipsometerDraft : ItemDraft =
        libraryDraft "47000000-0000-4000-8000-000000000008" "Ellipsometer" ItemModelIds.ellipsometerModelId

    let private glDraft : ItemDraft =
        fixedDraft
            "47000000-0000-4000-8000-000000000009"
            "GL"
            (StackSampleChoice (
                BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId,
                Library.SampleVersionId.firstOf Library.SampleIds.glassPlate1mmId,
                BuiltinCatalogue.sample Library.SampleIds.glassPlate1mmId
            ))

    /// The test-local uniaxial crystal the plates here are cut from.
    let private crystalVersion : MaterialVersionId =
        MaterialVersionId.firstOf (MaterialId (Guid.Parse "47100000-0000-4000-8000-000000000001"))

    let private crystalEntry : MaterialEntry =
        {
            id = crystalVersion.materialId
            name = EntryName "Probe quartz"
            category = CategoryIds.crystal
            description = None
            physics = CrystalParameterPhysics (UniaxialCrystal uniaxialParameters)
        }

    let private plateThickness : Thickness = Thickness.mm 0.5<mm>

    let private crystalLayerOf (thickness : Thickness) (orientation : CrystalOrientation) : SampleLayer =
        {
            materialId = crystalVersion
            thickness = thickness
            orientation = orientation
        }

    /// A thick plate of the crystal whose substrate layer is `layer`.
    let private plateSampleOf (id : string) (name : string) (layer : SampleLayer) (emission : Emission) : Sample =
        {
            id = SampleId (Guid.Parse id)
            name = EntryName name
            structure = { films = []; substrate = Some layer; lower = None }
            substrate = ThickPlate
            description = EntryDescription $"A plate of the probe quartz: {name}"
            supportedEmission = emission
        }

    let private qzZSample : Sample =
        plateSampleOf "47100000-0000-4000-8000-000000000011" "QZ-Z" (crystalLayerOf plateThickness PrimaryAxes) EmitBoth

    /// The x-cut: theta = 90 degrees under ZmYpXp carries the first principal axis onto the normal.
    let private xCutOrientation : CrystalOrientation =
        EulerRotation { EulerAngles.convention = ZmYpXp; phi = Angle.zero; theta = Angle.degree 90.0<deg>; psi = Angle.zero }

    let private qzXSample : Sample =
        plateSampleOf "47100000-0000-4000-8000-000000000012" "QZ-X" (crystalLayerOf plateThickness xCutOrientation) EmitTransmittedOnly

    /// A thin film whose single film layer is the crystal, on no substrate plate.
    let private qzFilmSample : Sample =
        {
            qzZSample with
                id = SampleId (Guid.Parse "47100000-0000-4000-8000-000000000013")
                name = EntryName "QZ film"
                structure = { films = [ SingleLayer (crystalLayerOf plateThickness PrimaryAxes) ]; substrate = None; lower = None }
                substrate = ThinFilm
                description = EntryDescription "A film of the probe quartz"
        }

    let private qzInfiniteSample : Sample =
        plateSampleOf "47100000-0000-4000-8000-000000000014" "QZ-inf" (crystalLayerOf Thickness.Infinity PrimaryAxes) EmitBoth

    /// A tilted cut: theta = 30 degrees under ZmYpXp puts no principal axis on the normal.
    let private tiltedOrientation : CrystalOrientation =
        EulerRotation { EulerAngles.convention = ZmYpXp; phi = Angle.zero; theta = Angle.degree 30.0<deg>; psi = Angle.zero }

    let private qzTSample : Sample =
        plateSampleOf "47100000-0000-4000-8000-000000000015" "QZ-T" (crystalLayerOf plateThickness tiltedOrientation) EmitBoth

    /// A fixed item over the shipped stack-sample model bound to version one of `sample` (steps 210, 216).
    let private sampleDraft (id : string) (sample : Sample) : ItemDraft =
        fixedDraft
            id
            sample.name.value
            (StackSampleChoice (
                BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId,
                SampleVersionId.firstOf sample.id,
                sample
            ))

    let private qzZDraft : ItemDraft = sampleDraft "47100000-0000-4000-8000-000000000021" qzZSample
    let private qzXDraft : ItemDraft = sampleDraft "47100000-0000-4000-8000-000000000022" qzXSample
    let private qzFilmDraft : ItemDraft = sampleDraft "47100000-0000-4000-8000-000000000023" qzFilmSample
    let private qzInfiniteDraft : ItemDraft = sampleDraft "47100000-0000-4000-8000-000000000024" qzInfiniteSample
    let private qzTDraft : ItemDraft = sampleDraft "47100000-0000-4000-8000-000000000025" qzTSample

    let private drafts : ItemDraft list =
        [
            laserDraft
            lp1Draft
            lp2Draft
            cpl1Draft
            retDraft
            phoneDraft
            polarimeterDraft
            ellipsometerDraft
            glDraft
            qzZDraft
            qzXDraft
            qzFilmDraft
            qzInfiniteDraft
            qzTDraft
        ]

    /// Each item's draft model, as the library would resolve it.
    let private choiceOf (item : Item) : ItemModelChoice option =
        drafts |> List.tryFind (fun draft -> draft.id = item.id) |> Option.map (fun draft -> draft.model)

    let private laser : Item = laserDraft.toItem
    let private lp1 : Item = lp1Draft.toItem
    let private lp2 : Item = lp2Draft.toItem
    let private cpl1 : Item = cpl1Draft.toItem
    let private ret : Item = retDraft.toItem
    let private phone : Item = phoneDraft.toItem
    let private polarimeter : Item = polarimeterDraft.toItem
    let private ellipsometer : Item = ellipsometerDraft.toItem
    let private gl : Item = glDraft.toItem
    let private qzZ : Item = qzZDraft.toItem
    let private qzX : Item = qzXDraft.toItem
    let private qzFilm : Item = qzFilmDraft.toItem
    let private qzInfinite : Item = qzInfiniteDraft.toItem
    let private qzT : Item = qzTDraft.toItem

    let private opticOf (item : Item) (role : OpticRole) : PlannerOptic = { item = item.id; label = item.label; role = role }

    let private detectorOf (item : Item) (recording : DetectorRecording) : PlannerDetector =
        {
            item = item.id
            label = item.label
            recording = recording
            noise = ParameterValues.ofParameters item.parameters
        }

    /// The reach of `detector` in the lab profile of `items`.
    let private reachIn (items : Item list) (detector : Item) : DetectorReach =
        let profile = LabProfile.ofItems items choiceOf |> profileValue
        profile.reachOf (profile.detectors |> List.find (fun (held : PlannerDetector) -> held.item = detector.id))

    /// The reason of an `UnresolvedPlannerItemErr`, failing on any other answer.
    let private unresolvedReasonOf (result : Result<LabProfile, PlannerError>) : string =
        match result with
        | Error (UnresolvedPlannerItemErr reason) -> reason
        | Error (UnknownPlannerTargetErr _ | UnopenedPlannerMaterialErr _ | NonCrystalPlannerMaterialErr _ | NoiselessPlannerCandidateErr _ | PlannerShapeErr _ | InstrumentEvaluationErr _ as error) ->
            failwith $"Expected an unresolved item, got: {error.describe}"
        | Ok _ -> failwith "Expected an unresolved item, got a lab profile."

    [<Fact>]
    let ``the lab profile reads the kit's sources, optics and detectors from their models`` () =
        let rotationAxis : ItemParameterAxis =
            (BuiltinCatalogue.itemModel ItemModelIds.muellerPolarimeterModelId).entry.model.parameterAxes
            |> List.find (fun axis -> axis.name = ParameterName.rotationUncertainty)

        let libraryRotation : ParameterValue = rotationAxis.defaultValue

        let editedRotation : ParameterValue = ParameterValue 0.0125
        Assert.NotEqual(libraryRotation, editedRotation)

        let editedPolarimeter : Item =
            polarimeter.withParameterState ParameterName.rotationUncertainty (KnownParameter editedRotation) |> itemValue

        let items : Item list = [ laser; lp1; lp2; cpl1; ret; phone; editedPolarimeter; ellipsometer; gl ]
        let profile : LabProfile = LabProfile.ofItems items choiceOf |> profileValue
        let heNe : Library.SourcePreset = BuiltinCatalogue.source Library.PresetIds.heNe633

        let expectedSource : PlannerSource =
            {
                item = laser.id
                label = laser.label
                waveLength = heNe.waveLength
                tunableLimitsOpt = None
            }

        Assert.Equal<PlannerSource list>([ expectedSource ], profile.sources)

        Assert.Equal<PlannerOptic list>(
            [
                opticOf lp1 PolarizerOptic
                opticOf lp2 PolarizerOptic
                opticOf cpl1 CompoundOptic
                opticOf ret CompensatorOptic
            ],
            profile.optics
        )

        Assert.Equal<PlannerDetector list>(
            [
                detectorOf phone RecordsIntensity
                detectorOf editedPolarimeter RecordsMuellerMatrix
                detectorOf ellipsometer RecordsPsiDelta
            ],
            profile.detectors
        )

        let polarimeterDetector : PlannerDetector =
            profile.detectors |> List.find (fun (held : PlannerDetector) -> held.item = editedPolarimeter.id)

        match polarimeterDetector.noise.valueOf ParameterName.rotationUncertainty with
        | Ok rotation -> Assert.Equal(editedRotation, rotation)
        | Error error -> failwith error.describe

        let profiled : ItemId list =
            (profile.sources |> List.map (fun (held : PlannerSource) -> held.item))
            @ (profile.optics |> List.map (fun (held : PlannerOptic) -> held.item))
            @ (profile.detectors |> List.map (fun (held : PlannerDetector) -> held.item))

        Assert.DoesNotContain(gl.id, profiled)

    [<Fact>]
    let ``an intensity detector reaches a full Mueller matrix only between a polarizer and an analyzer`` () =
        Assert.Equal(FullMuellerReach, reachIn [ phone; lp1; cpl1 ] phone)
        Assert.Equal(FullMuellerReach, reachIn [ phone; lp1; lp2 ] phone)
        Assert.Equal(IntensityOnlyReach, reachIn [ phone; lp1 ] phone)
        Assert.Equal(IntensityOnlyReach, reachIn [ phone; lp1; ret ] phone)
        Assert.Equal(IntensityOnlyReach, reachIn [ phone ] phone)

        let profiles : Item list list = [ []; [ lp1 ]; [ lp1; ret ]; [ lp1; cpl1 ]; [ lp1; lp2; cpl1; ret ] ]

        for optics in profiles do
            Assert.Equal(FullMuellerReach, reachIn (polarimeter :: optics) polarimeter)
            Assert.Equal(EllipsometricReach, reachIn (ellipsometer :: optics) ellipsometer)

        let mixed : LabProfile = LabProfile.ofItems [ phone; lp1; ellipsometer ] choiceOf |> profileValue
        Assert.Equal<DetectorReach list>([ EllipsometricReach; IntensityOnlyReach ], mixed.reaches)

        let whole : LabProfile =
            LabProfile.ofItems [ laser; lp1; lp2; cpl1; ret; phone; polarimeter; ellipsometer; gl ] choiceOf |> profileValue

        Assert.Equal<DetectorReach list>([ FullMuellerReach; EllipsometricReach ], whole.reaches)

    [<Fact>]
    let ``an item whose model the library does not resolve refuses the lab profile, naming it`` () =
        let items : Item list = [ laser; lp1; lp2; cpl1; ret; phone; polarimeter; ellipsometer; gl ]
        let withoutLp2 (item : Item) : ItemModelChoice option = if item.id = lp2.id then None else choiceOf item
        let refusal : Result<LabProfile, PlannerError> = LabProfile.ofItems items withoutLp2
        Assert.Equal("LP2", unresolvedReasonOf refusal)

        match refusal with
        | Error error -> Assert.StartsWith("the item's model is not in the library: ", error.describe)
        | Ok _ -> failwith "Expected a refusal, got a lab profile."

        // A detector whose noise value its model's axes refuse is unresolved too, naming the item and the axis.
        let outOfRange : Item =
            ellipsometer.withParameterState ParameterName.elementUncertainty (KnownParameter (ParameterValue 1.5)) |> itemValue

        let rangeReason : string = LabProfile.ofItems [ lp1; outOfRange ] choiceOf |> unresolvedReasonOf
        Assert.StartsWith("Ellipsometer: ", rangeReason)
        Assert.Contains("elementUncertainty", rangeReason)

        let missing : Item =
            {
                phone with
                    parameters = phone.parameters |> List.filter (fun held -> held.name <> ParameterName.throughputUncertainty)
            }

        let missingReason : string = LabProfile.ofItems [ missing; lp1 ] choiceOf |> unresolvedReasonOf
        Assert.StartsWith("Phone: ", missingReason)
        Assert.Contains("throughputUncertainty", missingReason)

    let private studyValue (result : Result<Study, StudyError>) : Study =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private inputsValue (result : Result<PlannerInputs, PlannerError>) : PlannerInputs =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private nE : ParameterName = ParameterName "n_e"

    let private g33 : ParameterName = ParameterName "g33"

    /// The study's copy of `version`.
    let private copyIn (study : Study) (version : MaterialVersionId) : OpenedMaterial =
        study.materials |> List.find (fun (opened : OpenedMaterial) -> opened.version = version)

    /// The study with the named component of the crystal an unknown starting at its opened value, within +-5 % of it.
    let private freedNear (study : Study) (name : ParameterName) : Study =
        match (copyIn study crystalVersion).physics.componentValue name with
        | Some (ParameterValue value) ->
            let ends : float list = [ 0.95 * value; 1.05 * value ]

            let freed : UnknownParameterData =
                {
                    start = ParameterValue value
                    bounds = { lower = ParameterValue (List.min ends); upper = ParameterValue (List.max ends) }
                    priorOpt = None
                }

            study.withMaterialParameterState crystalVersion name (UnknownParameter freed) |> studyValue
        | None -> failwith $"The crystal's copy has no component {name.value}."

    /// A study of no experiment that opens the crystal and frees n_e and g33.
    let private crystalStudy : Study =
        let opened : Study =
            Study.create (StudyName "Probe quartz plates") [] [] []
            |> Result.bind (fun study -> study.openMaterial crystalVersion crystalEntry)
            |> studyValue

        [ nE; g33 ] |> List.fold freedNear opened

    /// The kit of `items` holding `study` and no experiment.
    let private contextOf (items : Item list) (study : Study) : StudyContext =
        {
            studies = [ study ]
            experiments = []
            setups = []
            items = items
            resolveMaterial = fun version -> Error (MaterialUnavailableErr $"material {version.materialId.value} is not the test's")
        }

    /// The planner's inputs for `material` over the kit of `items` holding `study`.
    let private plannerInputsOf
        (study : Study)
        (items : Item list)
        (choiceOf : Item -> ItemModelChoice option)
        (material : MaterialVersionId)
        : Result<PlannerInputs, PlannerError> =
        study.plannerInputsWith (contextOf items study) choiceOf material

    /// The expected 0.5 mm thick plate of `item`, version one of `sample`.
    let private plateOf (item : Item) (sample : Sample) (emission : Emission) (normalAxis : CrystalAxis) : PlannerPlate =
        {
            item = item.id
            label = item.label
            sample = SampleVersionId.firstOf sample.id
            thickness = Thickness.mm 0.5<mm>
            substrate = ThickPlate
            emission = emission
            normalAxisOpt = Some normalAxis
        }

    [<Fact>]
    let ``the planner's inputs are the kit's items, its plates of the material, the opened copy and the study's unknowns of it`` () =
        let items : Item list = [ laser; lp1; cpl1; polarimeter; qzZ; qzX; gl ]
        let inputs : PlannerInputs = plannerInputsOf crystalStudy items choiceOf crystalVersion |> inputsValue

        Assert.Equal(LabProfile.ofItems items choiceOf |> profileValue, inputs.lab)

        Assert.Equal<PlannerPlate list>(
            [
                plateOf qzZ qzZSample EmitBoth ThirdCrystalAxis
                plateOf qzX qzXSample EmitTransmittedOnly FirstCrystalAxis
            ],
            inputs.plates
        )

        let copy : OpenedMaterial = copyIn crystalStudy crystalVersion
        Assert.Equal(copy, inputs.prior.material)

        match copy.currentPhysics.crystalOpt with
        | Some crystal -> Assert.Equal(crystal, inputs.prior.crystal)
        | None -> failwith "The opened copy has no crystal."

        Assert.Equal(UniaxialCrystal uniaxialParameters, inputs.prior.crystal)
        Assert.Equal<ParameterName list>([ nE; g33 ], inputs.targets)

        // n_o and g11 stay known: they are prior, not targets.
        let knownNames : ParameterName list =
            inputs.prior.material.parameters
            |> List.choose (fun (parameter : Parameter) ->
                match parameter.state with
                | KnownParameter _ -> Some parameter.name
                | UnknownParameter _
                | CalibratedParameter _ -> None)

        Assert.Equal<ParameterName list>([ ParameterName "n_o"; ParameterName "g11" ], knownNames)

    [<Fact>]
    let ``a material the study does not open, or a tabulated one, gives no planner inputs`` () =
        let items : Item list = [ laser; lp1; polarimeter; qzZ ]
        let unopened : MaterialVersionId = { crystalVersion with version = crystalVersion.version.next }

        match plannerInputsOf crystalStudy items choiceOf unopened with
        | Error (UnopenedPlannerMaterialErr reason as error) ->
            Assert.Contains($"material {crystalVersion.materialId.value} v2", reason)
            Assert.StartsWith("the study opens no such material version: ", error.describe)
        | Error error -> failwith $"Expected an unopened material, got: {error.describe}"
        | Ok _ -> failwith "Expected an unopened material, got planner inputs."

        let tableVersion : MaterialVersionId = TabulatedStudyFixtures.tableVersion
        let tableEntry : MaterialEntry = TabulatedStudyFixtures.uncertainTableEntry

        let tableStudy : Study =
            Study.create (StudyName "Probe tabulated glass") [] [] []
            |> Result.bind (fun study -> study.openMaterial tableVersion tableEntry)
            |> studyValue

        match plannerInputsOf tableStudy items choiceOf tableVersion with
        | Error (NonCrystalPlannerMaterialErr reason as error) ->
            Assert.Contains($"'{tableEntry.name.value}'", reason)
            Assert.Contains($"material {tableVersion.materialId.value} v1", reason)
            Assert.StartsWith("the opened material is not a crystal: ", error.describe)
        | Error error -> failwith $"Expected a non-crystal material, got: {error.describe}"
        | Ok _ -> failwith "Expected a non-crystal material, got planner inputs."

        // The study's refusal comes before the lab profile's, which otherwise passes through.
        let withoutLp1 (item : Item) : ItemModelChoice option = if item.id = lp1.id then None else choiceOf item

        match plannerInputsOf crystalStudy items withoutLp1 unopened with
        | Error (UnopenedPlannerMaterialErr _) -> ()
        | Error error -> failwith $"Expected an unopened material, got: {error.describe}"
        | Ok _ -> failwith "Expected an unopened material, got planner inputs."

        match plannerInputsOf crystalStudy items withoutLp1 crystalVersion with
        | Error (UnresolvedPlannerItemErr reason) -> Assert.Equal("LP1", reason)
        | Error error -> failwith $"Expected an unresolved item, got: {error.describe}"
        | Ok _ -> failwith "Expected an unresolved item, got planner inputs."

    [<Fact>]
    let ``a film of the material, or a substrate of infinite thickness, is no plate`` () =
        let items : Item list = [ laser; lp1; polarimeter; qzFilm; qzZ; qzInfinite; gl ]
        let inputs : PlannerInputs = plannerInputsOf crystalStudy items choiceOf crystalVersion |> inputsValue
        Assert.Equal<PlannerPlate list>([ plateOf qzZ qzZSample EmitBoth ThirdCrystalAxis ], inputs.plates)

        let withoutPlate : PlannerInputs =
            plannerInputsOf crystalStudy [ laser; lp1; polarimeter; qzFilm; qzInfinite; gl ] choiceOf crystalVersion
            |> inputsValue

        Assert.Equal<PlannerPlate list>([], withoutPlate.plates)
        Assert.Equal<ParameterName list>([ nE; g33 ], withoutPlate.targets)

    let private nO : ParameterName = ParameterName "n_o"

    let private g11 : ParameterName = ParameterName "g11"

    /// The crystal the probe quartz's copy opens as.
    let private probeCrystal : CrystalParameters = UniaxialCrystal uniaxialParameters

    /// The probe crystal's component named `name`.
    let private probeComponentOf (name : ParameterName) : CrystalComponent =
        probeCrystal.components |> List.find (fun (held : CrystalComponent) -> held.name = name)

    /// Every component of the probe crystal, in its order.
    let private everyProbeComponent : CrystalComponent list = [ nO; nE; g11; g33 ] |> List.map probeComponentOf

    /// The crystal study with n_o and g11 freed too, so every component is a target.
    let private everyTargetStudy : Study = [ nO; g11 ] |> List.fold freedNear crystalStudy

    /// The planner's inputs over the kit of `items` holding the study that frees every component.
    let private everyTargetInputsOver (items : Item list) : PlannerInputs =
        plannerInputsOf everyTargetStudy items choiceOf crystalVersion |> inputsValue

    /// The kit's structural report; a refusal fails the fact.
    let private kitReportOf (inputs : PlannerInputs) : RecoverabilityReport =
        inputs.structuralReport |> Result.defaultWith (fun error -> failwith error.describe)

    /// The steepest incidence the table takes, written out.
    let private steepestIncidence : IncidenceAngle = IncidenceAngle.create (Angle.degree 89.0<deg>)

    /// A look at a plate that puts `normalAxis` on its normal.
    let private lookAt
        (normalAxis : CrystalAxis)
        (reach : DetectorReach)
        (incidence : IncidenceAngle)
        (observable : Observable)
        : PlannerLook =
        {
            normalAxisOpt = Some normalAxis
            incidence = incidence
            observable = observable
            reach = reach
        }

    [<Fact>]
    let ``a kit's looks are each plate under each reach, at normal and steepest incidence, on each branch its emission allows`` () =
        let inputs : PlannerInputs = everyTargetInputsOver [ laser; polarimeter; ellipsometer; qzZ; qzX ]
        let normal : IncidenceAngle = IncidenceAngle.normal

        Assert.Equal<ParameterName list>([ nO; nE; g11; g33 ], inputs.targets)
        Assert.Equal<DetectorReach list>([ FullMuellerReach; EllipsometricReach ], inputs.lab.reaches)
        Assert.Equal(steepestIncidence, IncidenceAngle.maxValue)

        Assert.Equal<PlannerLook list>(
            [
                lookAt ThirdCrystalAxis FullMuellerReach normal TransmittedMueller
                lookAt ThirdCrystalAxis FullMuellerReach normal ReflectedMueller
                lookAt ThirdCrystalAxis FullMuellerReach steepestIncidence TransmittedMueller
                lookAt ThirdCrystalAxis FullMuellerReach steepestIncidence ReflectedMueller
                lookAt ThirdCrystalAxis EllipsometricReach normal TransmittedMueller
                lookAt ThirdCrystalAxis EllipsometricReach normal ReflectedMueller
                lookAt ThirdCrystalAxis EllipsometricReach steepestIncidence TransmittedMueller
                lookAt ThirdCrystalAxis EllipsometricReach steepestIncidence ReflectedMueller
                lookAt FirstCrystalAxis FullMuellerReach normal TransmittedMueller
                lookAt FirstCrystalAxis FullMuellerReach steepestIncidence TransmittedMueller
                lookAt FirstCrystalAxis EllipsometricReach normal TransmittedMueller
                lookAt FirstCrystalAxis EllipsometricReach steepestIncidence TransmittedMueller
            ],
            inputs.looks
        )

        // A plate that only reflects is looked at on its reflected branch alone.
        let reflecting : PlannerInputs = { inputs with plates = [ plateOf qzZ qzZSample EmitReflectedOnly ThirdCrystalAxis ] }

        Assert.Equal<PlannerLook list>(
            [
                lookAt ThirdCrystalAxis FullMuellerReach normal ReflectedMueller
                lookAt ThirdCrystalAxis FullMuellerReach steepestIncidence ReflectedMueller
                lookAt ThirdCrystalAxis EllipsometricReach normal ReflectedMueller
                lookAt ThirdCrystalAxis EllipsometricReach steepestIncidence ReflectedMueller
            ],
            reflecting.looks
        )

        // No detector, no reach, and so no look.
        Assert.Equal<PlannerLook list>([], (everyTargetInputsOver [ laser; lp1; qzZ; qzX ]).looks)

    [<Fact>]
    let ``a kit of z-cut plates needs an off-axis cut, and one without a full Mueller reach never recovers the gyration`` () =
        let zCutKit : RecoverabilityReport = everyTargetInputsOver [ laser; polarimeter; qzZ ] |> kitReportOf

        let need : OffAxisCutNeed =
            {
                targets = [ probeComponentOf nE; probeComponentOf g33 ]
                capAngle = probeCrystal.snellCap
                cuts = [ XCut ]
            }

        Assert.Equal<CrystalComponent list>(everyProbeComponent, zCutKit.reachable)
        Assert.Equal<PlannerFinding list>([ OffAxisCutFinding need ], zCutKit.findings)

        Assert.Equal<string list>(
            [
                "You need an off-axis cut: n_e, g33 lie along the normal of every plate, and a tilt carries the wave at most 40.4° off it; add one x-cut plate."
            ],
            zCutKit.findings |> List.map (fun (finding : PlannerFinding) -> finding.text)
        )

        let bothCutsKit : RecoverabilityReport = everyTargetInputsOver [ laser; polarimeter; qzZ; qzX ] |> kitReportOf

        Assert.Equal<CrystalComponent list>(everyProbeComponent, bothCutsKit.reachable)
        Assert.Equal<OffAxisCutNeed option>(None, bothCutsKit.offAxisCutOpt)
        Assert.Equal<PlannerFinding list>([], bothCutsKit.findings)

        // A phone behind a single polarizer records S0 alone.
        let intensityInputs : PlannerInputs = everyTargetInputsOver [ laser; lp1; phone; qzZ; qzX ]
        Assert.Equal<DetectorReach list>([ IntensityOnlyReach ], intensityInputs.lab.reaches)

        let intensityKit : RecoverabilityReport = kitReportOf intensityInputs

        Assert.Equal<CrystalComponent list>([ probeComponentOf nO; probeComponentOf nE ], intensityKit.reachable)

        Assert.Equal<PlannerFinding list>(
            [
                UnrecoverableTargetFinding
                    {
                        target = probeComponentOf g11
                        reason = FullMuellerNeededReason
                        remedies = [ FullMuellerRemedy ]
                    }
                UnrecoverableTargetFinding
                    {
                        target = probeComponentOf g33
                        reason = FullMuellerNeededReason
                        remedies = [ FullMuellerRemedy ]
                    }
            ],
            intensityKit.findings
        )

    [<Fact>]
    let ``a kit with no plate of the material can never recover any target`` () =
        let inputs : PlannerInputs = everyTargetInputsOver [ laser; polarimeter; gl ]

        Assert.Equal<PlannerPlate list>([], inputs.plates)
        Assert.Equal<DetectorReach list>([ FullMuellerReach ], inputs.lab.reaches)
        Assert.Equal<PlannerLook list>([], inputs.looks)

        let report : RecoverabilityReport = kitReportOf inputs

        Assert.Equal<CrystalComponent list>([], report.reachable)
        Assert.Equal<CrystalComponent list>(everyProbeComponent, report.unrecoverable)
        Assert.Equal<OffAxisCutNeed option>(None, report.offAxisCutOpt)

        for finding in report.findings do
            match finding with
            | UnrecoverableTargetFinding targetFinding ->
                Assert.Equal(NoLookReason, targetFinding.reason)

                let addedPlates : SampleCut list =
                    targetFinding.remedies
                    |> List.choose (fun (remedy : PlannerRemedy) ->
                        match remedy with
                        | AddPlateRemedy cut -> Some cut
                        | ObliqueIncidenceRemedy _
                        | FullMuellerRemedy -> None)

                Assert.NotEmpty(addedPlates)

                Assert.Equal<PlannerRemedy list>(
                    (addedPlates |> List.map AddPlateRemedy) @ [ FullMuellerRemedy ],
                    targetFinding.remedies
                )
            | OffAxisCutFinding _ -> Assert.Fail "a kit with no plate of the material has no off-axis finding"

        Assert.Equal(
            "This kit can never recover n_o: no look of the kit measures a plate of it.",
            (List.head report.findings).text
        )

    /// The kit's Laser as its lab profile reads it.
    let private laserSource : PlannerSource =
        {
            item = laser.id
            label = laser.label
            waveLength = (BuiltinCatalogue.source Library.PresetIds.heNe633).waveLength
            tunableLimitsOpt = None
        }

    /// The candidate of `source`, `plate`, `cut` and `observable`.
    let private candidateOf (source : PlannerSource) (plate : PlannerPlate) (cut : SampleCut) (observable : Observable) : PlanCandidate =
        {
            source = source
            plate = plate
            cut = cut
            observable = observable
        }

    [<Fact>]
    let ``a kit's candidates are each placed plate on each branch it emits at each source's line, read by its polarimeter`` () =
        let inputs : PlannerInputs =
            plannerInputsOf crystalStudy [ laser; polarimeter; qzZ; qzX; qzT ] choiceOf crystalVersion |> inputsValue

        let qzZPlate : PlannerPlate = plateOf qzZ qzZSample EmitBoth ThirdCrystalAxis
        let qzXPlate : PlannerPlate = plateOf qzX qzXSample EmitTransmittedOnly FirstCrystalAxis
        let qzTPlate : PlannerPlate = { plateOf qzT qzTSample EmitBoth ThirdCrystalAxis with normalAxisOpt = None }

        Assert.Equal<PlannerPlate list>([ qzZPlate; qzXPlate; qzTPlate ], inputs.plates)
        Assert.Equal<PlannerSource list>([ laserSource ], inputs.lab.sources)
        Assert.Equal<PlannerDetector option>(Some (detectorOf polarimeter RecordsMuellerMatrix), inputs.polarimeterOpt)
        Assert.Equal<PlannerPlate list>([ qzTPlate ], inputs.unplacedPlates)

        Assert.Equal<PlanCandidate list>(
            [
                candidateOf laserSource qzZPlate ZCut TransmittedMueller
                candidateOf laserSource qzZPlate ZCut ReflectedMueller
                candidateOf laserSource qzXPlate XCut TransmittedMueller
            ],
            inputs.candidates
        )

        Assert.Equal<string list>(
            [
                "QZ-Z (z-cut plate), transmitted, 632.8 nm"
                "QZ-Z (z-cut plate), reflected, 632.8 nm"
                "QZ-X (x-cut plate), transmitted, 632.8 nm"
            ],
            inputs.candidates |> List.map (fun (candidate : PlanCandidate) -> candidate.caption)
        )

        Assert.Equal<MeasurementMode list>(
            [ CaptureTransmitted; CaptureReflected; CaptureTransmitted ],
            inputs.candidates |> List.map (fun (candidate : PlanCandidate) -> candidate.capture)
        )

        Assert.Equal<SweptValue list>(
            [ SweptValue 0.0; SweptValue 20.0; SweptValue 40.0; SweptValue 60.0 ],
            PlanCandidate.incidenceRange.sweptValues
        )

        Assert.Equal<SweptValue list>(
            [ SweptValue 0.0; SweptValue 22.5; SweptValue 45.0; SweptValue 67.5 ],
            PlanCandidate.azimuthRange.sweptValues
        )

        // A second line repeats every placed plate's candidates after the first source's.
        let greenSource : PlannerSource =
            {
                item = ItemId (Guid.Parse "47000000-0000-4000-8000-000000000010")
                label = ItemLabel "Green"
                waveLength = WaveLength.nm 532.0<nm>
                tunableLimitsOpt = None
            }

        let twoLines : PlannerInputs = { inputs with lab = { inputs.lab with sources = [ laserSource; greenSource ] } }

        Assert.Equal<PlanCandidate list>(
            inputs.candidates
            @ [
                candidateOf greenSource qzZPlate ZCut TransmittedMueller
                candidateOf greenSource qzZPlate ZCut ReflectedMueller
                candidateOf greenSource qzXPlate XCut TransmittedMueller
            ],
            twoLines.candidates
        )

        Assert.Equal("QZ-X (x-cut plate), transmitted, 532.0 nm", (List.last twoLines.candidates).caption)

        // The Phone in place of the Polarimeter reads no Mueller matrix: no candidate, the tilted plate still unplaced.
        let phoneInputs : PlannerInputs =
            plannerInputsOf crystalStudy [ laser; phone; qzZ; qzX; qzT ] choiceOf crystalVersion |> inputsValue

        Assert.Equal<PlannerDetector option>(None, phoneInputs.polarimeterOpt)
        Assert.Equal<PlanCandidate list>([], phoneInputs.candidates)
        Assert.Equal<PlannerPlate list>([ qzTPlate ], phoneInputs.unplacedPlates)

    let private benchSlotOf (id : string) (kind : CatalogueKind) (point : TablePoint) : SetupSlot =
        {
            id = ElementId id
            placement = ElementPlacement.create kind point
            zoom = BeamLayout.ElementZoom 5.0
            beam = BeamLayout.OnMainBeam
            content = EmptySlotContent
        }

    let private srcSlot : SetupSlot = benchSlotOf "src" CatalogueKind.LightSource RayModel.defaultSourcePoint
    let private sampleSlot : SetupSlot = benchSlotOf "sample" CatalogueKind.Sample { x = 0.0<meter>; y = 0.0<meter> }
    let private detSlot : SetupSlot = benchSlotOf "det" CatalogueKind.Detector RayModel.defaultDetectorPoint

    /// The slot holding the draft's item, as the table assigns it.
    let private assigned (draft : ItemDraft) (slot : SetupSlot) : SetupSlot =
        match slot.withItem draft.toItem draft.model with
        | Ok held -> held
        | Error error -> failwith $"'{draft.label.value}' did not fit '{slot.id.value}': {error.describe}"

    /// `Bench` at its first revision: the Laser in `src`, the plate in `sample` and the Polarimeter in `det`.
    let private benchHolding (plate : ItemDraft) : Setup =
        let slots : SetupSlot list = [ assigned laserDraft srcSlot; assigned plate sampleSlot; assigned polarimeterDraft detSlot ]

        match Setup.create (SetupName "Bench") slots [] with
        | Ok setup -> setup
        | Error error -> failwith $"'Bench' was refused: {error.describe}"

    /// The sample slot's `variable` over `range`, under a label the coverage does not compare.
    let private sampleAxisOf (variable : VariableParameter) (range : VariableRange) : ScanAxis =
        {
            elementId = sampleSlot.id
            label = ElementLabel "Sample #1"
            variable = variable
            range = range
        }

    /// A science run named `name` over `setup`, one block of `axes` read on `capture`, at the source's own line.
    let private runOver (name : string) (setup : Setup) (axes : ScanAxis list) (capture : MeasurementMode) : Experiment =
        let block : ScanBlock = { axes = axes; capture = capture }

        {
            id = ExperimentId 1
            name = ExperimentName name
            setup = setup.reference
            scan = { blocks = [ block ]; sourceWaveLengthOpt = None }
            role = ScienceRun
            sessionOpt = None
            notes = ExperimentNotes.none
            dataOpt = None
            corrections = []
        }

    [<Fact>]
    let ``a candidate is in the kit once an experiment over its plate sweeps its angles on its branch`` () =
        let inputs : PlannerInputs =
            plannerInputsOf crystalStudy [ laser; polarimeter; qzZ; qzX ] choiceOf crystalVersion |> inputsValue

        let candidateOn (plate : Item) (observable : Observable) : PlanCandidate =
            inputs.candidates
            |> List.find (fun (candidate : PlanCandidate) -> candidate.plate.item = plate.id && candidate.observable = observable)

        let transmitted : PlanCandidate = candidateOn qzZ TransmittedMueller
        let reflected : PlanCandidate = candidateOn qzZ ReflectedMueller
        let bench : Setup = benchHolding qzZDraft
        let e1 : ExperimentName = ExperimentName "E1"

        let sweep : ScanAxis list =
            [ sampleAxisOf VaryR2 PlanCandidate.incidenceRange; sampleAxisOf VaryR1 PlanCandidate.azimuthRange ]

        let transmittedRun : Experiment = runOver "E1" bench sweep CaptureTransmitted
        Assert.Equal(CandidateInKit e1, transmitted.coverageIn [ bench ] [ transmittedRun ])
        Assert.Equal(CandidateNotInKit, reflected.coverageIn [ bench ] [ transmittedRun ])

        // Whatever its role: a planned run already holds the candidate.
        Assert.Equal(CandidateInKit e1, transmitted.coverageIn [ bench ] [ transmittedRun.withRole PlannedRun ])

        let bothRun : Experiment = runOver "E1" bench sweep CaptureBoth
        Assert.Equal(CandidateInKit e1, transmitted.coverageIn [ bench ] [ bothRun ])
        Assert.Equal(CandidateInKit e1, reflected.coverageIn [ bench ] [ bothRun ])

        // The first experiment that holds it is named.
        let swappedE0 : Experiment = runOver "E0" bench (List.rev sweep) CaptureBoth
        let bothE2 : Experiment = runOver "E2" bench sweep CaptureBoth

        Assert.Equal(
            CandidateInKit e1,
            reflected.coverageIn [ bench ] [ swappedE0; runOver "E1" bench sweep CaptureReflected; bothE2 ]
        )

        // The axes in the other order, another range, or a setup holding QZ-X: it holds neither.
        let otherRange : ScanAxis list =
            [
                sampleAxisOf VaryR2 PlanCandidate.incidenceRange
                sampleAxisOf VaryR1 { PlanCandidate.azimuthRange with points = CurvePointCount 5 }
            ]

        let xBench : Setup = benchHolding qzXDraft
        let xRun : Experiment = runOver "E1" xBench sweep CaptureBoth

        for setups, run in [ [ bench ], swappedE0; [ bench ], runOver "E1" bench otherRange CaptureBoth; [ xBench ], xRun ] do
            Assert.Equal(CandidateNotInKit, transmitted.coverageIn setups [ run ])
            Assert.Equal(CandidateNotInKit, reflected.coverageIn setups [ run ])

        Assert.Equal(CandidateInKit e1, (candidateOn qzX TransmittedMueller).coverageIn [ xBench ] [ xRun ])

        // A setup the kit does not hold, or a set line, holds nothing.
        Assert.Equal(CandidateNotInKit, transmitted.coverageIn [] [ bothRun ])

        let setLine : Experiment =
            { bothRun with scan = { bothRun.scan with sourceWaveLengthOpt = Some laserSource.waveLength } }

        Assert.Equal(CandidateNotInKit, transmitted.coverageIn [ bench ] [ setLine ])

    /// An empty material store: no optic here is a stack sample.
    let private noMaterials : MaterialProxy = MaterialProxy.createInMemory (fun _ -> [])

    /// The instrument findings of the kit of `items`, its inputs read through `choiceOf` and its optics through
    /// `evaluatedBy`.
    let private instrumentFindingsOver
        (items : Item list)
        (evaluatedBy : Item -> ItemModelChoice option)
        : Result<InstrumentFinding list, PlannerError> =
        let inputs : PlannerInputs = plannerInputsOf crystalStudy items choiceOf crystalVersion |> inputsValue
        inputs.instrumentFindingsWith items evaluatedBy noMaterials

    let private findingsValue (result : Result<InstrumentFinding list, PlannerError>) : InstrumentFinding list =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    /// The draft's Mueller matrix through the item evaluator at `rotation`, at the Laser's line.
    let private evaluatedMuellerOf (draft : ItemDraft) (rotation : Angle) : MuellerMatrix =
        let item : Item = draft.toItem

        draft.model.boundWith noMaterials (ParameterValues.ofParameters item.parameters)
        |> Result.bind (fun (bound : BoundOpticalElement) ->
            bound.muellerAt (ElementOrientation.transmittingAt rotation) laserSource.waveLength)
        |> Result.defaultWith (fun error -> failwith error.describe)

    /// The condition number of the matrix of these rows.
    let private conditionOfRows (rows : float[] list) : float = (rows |> List.toArray |> ResidualJacobian).conditionNumber.value

    [<Fact>]
    let ``a polarizer and a retarder give the four compensator angles of least generator condition, and the instrument's condition follows`` () =
        let choice : InstrumentChoice =
            match instrumentFindingsOver [ laser; lp1; ret ] choiceOf |> findingsValue with
            | [ ConditionedInstrumentFinding held ] -> held
            | findings ->
                let texts : string = findings |> List.map (fun (finding : InstrumentFinding) -> finding.text) |> String.concat " | "
                failwith $"Expected one conditioned finding, got: {texts}"

        Assert.Equal(opticOf lp1 PolarizerOptic, choice.polarizer)
        Assert.Equal(opticOf ret CompensatorOptic, choice.compensator)

        let grid : Angle list = InstrumentChoice.compensatorAngleGrid
        Assert.Equal<Angle list>([ for step in 0..11 -> Angle.degree (15.0<deg> * float step) ], grid)

        // Four distinct grid angles, in the grid's order.
        Assert.Equal(4, choice.angles.Length)
        Assert.Equal<Angle list>(grid |> List.filter (fun (angle : Angle) -> List.contains angle choice.angles), choice.angles)

        // The generator: LP1 at 0 then RET turned to θ, over unpolarized light; the analyzer reads row 0 of LP1 · RET(θ).
        let polarizing : MuellerMatrix = evaluatedMuellerOf lp1Draft Angle.zero
        let polarized : StokesVector = polarizing * Propagation.unpolarizedStokes
        let stateAt (angle : Angle) : StokesVector = evaluatedMuellerOf retDraft angle * polarized

        let analyzerRowAt (angle : Angle) : RealVector4 =
            let analyzing : MuellerMatrix = polarizing * evaluatedMuellerOf retDraft angle
            RealVector4.create [ for j in 0..3 -> analyzing.element 0 j ]

        let generatorConditionAt (angles : Angle list) : float =
            angles
            |> List.map (fun (angle : Angle) ->
                let state : StokesVector = stateAt angle
                [| state.s0; state.s1; state.s2; state.s3 |])
            |> conditionOfRows

        let analyzerConditionAt (angles : Angle list) : float =
            angles
            |> List.map (fun (angle : Angle) ->
                let row : RealVector4 = analyzerRowAt angle
                [| row.[0]; row.[1]; row.[2]; row.[3] |])
            |> conditionOfRows

        let sets : Angle list list =
            [
                for i in 0..11 do
                    for j in i + 1 .. 11 do
                        for k in j + 1 .. 11 do
                            for l in k + 1 .. 11 do
                                [ List.item i grid; List.item j grid; List.item k grid; List.item l grid ]
            ]

        Assert.Equal(495, sets.Length)

        let conditions : float list = sets |> List.map generatorConditionAt
        let chosenCondition : float = generatorConditionAt choice.angles

        for (set, condition) in List.zip sets conditions do
            Assert.True(chosenCondition <= condition, $"{set} has generator condition {condition}, below the chosen {chosenCondition}")

        // A tie keeps the earliest set in the grid's lexicographic order.
        let least : float = List.min conditions
        Assert.Equal<Angle list>(sets |> List.find (fun (set : Angle list) -> generatorConditionAt set = least), choice.angles)

        // The instrument matrix is the Kronecker product of the generator and analyzer matrices: its condition is theirs
        // multiplied, each at least √3.
        let kappa : float = choice.condition.value
        Assert.True(Double.IsFinite kappa, $"the instrument's condition {kappa} is not finite")
        Assert.True(kappa >= 3.0, $"the instrument's condition {kappa} is below 3")
        let expectedKappa : float = chosenCondition * analyzerConditionAt choice.angles
        Assert.Equal(expectedKappa, kappa, 1.0e-9 * expectedKappa)

        let anglesText : string =
            choice.angles
            |> List.map (fun (angle : Angle) -> angle.degrees.ToString("F1", Globalization.CultureInfo.InvariantCulture))
            |> String.concat ", "

        let kappaText : string = kappa.ToString("F2", Globalization.CultureInfo.InvariantCulture)

        Assert.Equal(
            $"LP1 with RET at {anglesText}°: the instrument matrix's condition number is {kappaText}.",
            (ConditionedInstrumentFinding choice).text
        )

        // Each polarizer in order, with each compensator: LP2 is the same preset, so its choice is LP1's.
        Assert.Equal<InstrumentFinding list>(
            [
                ConditionedInstrumentFinding choice
                ConditionedInstrumentFinding { choice with polarizer = opticOf lp2 PolarizerOptic }
            ],
            instrumentFindingsOver [ laser; lp1; lp2; ret ] choiceOf |> findingsValue
        )

    [<Fact>]
    let ``a polarizer alone is told it needs a compensator, and a compound optic is not paired`` () =
        let missing : InstrumentFinding = CompensatorMissingFinding (opticOf lp1 PolarizerOptic)

        for items in [ [ laser; lp1 ]; [ laser; lp1; cpl1 ] ] do
            Assert.Equal<InstrumentFinding list>([ missing ], instrumentFindingsOver items choiceOf |> findingsValue)

        Assert.Equal(
            "LP1 has no compensator to pair: it generates linear states only, so a full Mueller matrix needs a retarder.",
            missing.text
        )

        // Beside a retarder, CPL1 is still paired with nothing: LP1 alone is, with RET.
        match instrumentFindingsOver [ laser; lp1; cpl1; ret ] choiceOf |> findingsValue with
        | [ ConditionedInstrumentFinding choice ] ->
            Assert.Equal(opticOf lp1 PolarizerOptic, choice.polarizer)
            Assert.Equal(opticOf ret CompensatorOptic, choice.compensator)
        | findings -> failwith $"Expected LP1 with RET alone, got {findings.Length} findings."

        // No source, no finding.
        Assert.Equal<InstrumentFinding list>([], instrumentFindingsOver [ lp1 ] choiceOf |> findingsValue)
        Assert.Equal<InstrumentFinding list>([], instrumentFindingsOver [ lp1; cpl1; ret ] choiceOf |> findingsValue)

    [<Fact>]
    let ``an optic the library does not resolve refuses the instrument findings, naming it`` () =
        let withoutRet (item : Item) : ItemModelChoice option = if item.id = ret.id then None else choiceOf item

        let refusalOf (result : Result<InstrumentFinding list, PlannerError>) : string =
            match result with
            | Error (InstrumentEvaluationErr reason as error) ->
                Assert.StartsWith("an optic of the kit cannot be evaluated: ", error.describe)
                reason
            | Error error -> failwith $"Expected an optic the evaluator refuses, got: {error.describe}"
            | Ok _ -> failwith "Expected an optic the evaluator refuses, got instrument findings."

        Assert.Equal("RET: its model is not in the library", instrumentFindingsOver [ laser; lp1; ret ] withoutRet |> refusalOf)

        // An optic missing from the items, or one the evaluator refuses, is named likewise.
        let inputs : PlannerInputs = plannerInputsOf crystalStudy [ laser; lp1; ret ] choiceOf crystalVersion |> inputsValue

        Assert.Equal(
            "RET: the kit holds no such item",
            inputs.instrumentFindingsWith [ laser; lp1 ] choiceOf noMaterials |> refusalOf
        )

        let withoutOffset : Item =
            { ret with parameters = ret.parameters |> List.filter (fun held -> held.name <> ParameterName.fastAxisOffset) }

        let evaluatorReason : string = inputs.instrumentFindingsWith [ laser; lp1; withoutOffset ] choiceOf noMaterials |> refusalOf
        Assert.StartsWith("RET: the values hold no number for the parameter: ", evaluatorReason)
        Assert.Contains(ParameterName.fastAxisOffset.value, evaluatorReason)
