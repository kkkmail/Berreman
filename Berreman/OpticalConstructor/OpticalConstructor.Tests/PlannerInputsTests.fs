namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary
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
/// then the kit's looks and its structural report (spec 0050 S14.1; 001 §M).
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
        | Error (UnknownPlannerTargetErr _ | UnopenedPlannerMaterialErr _ | NonCrystalPlannerMaterialErr _ | NoiselessPlannerCandidateErr _ | PlannerShapeErr _ as error) ->
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
