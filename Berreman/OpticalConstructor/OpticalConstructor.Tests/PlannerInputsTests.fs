namespace OpticalConstructor.Tests

open System
open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.ItemEvaluation
open OpticalConstructor.Domain.MeasurementPlanner

/// The Measurement Planner's lab profile: the kit's sources, optics and detectors read from their models, and what an
/// intensity detector reaches beside them (spec 0050 S14.1; 001 §M).
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

    let private drafts : ItemDraft list =
        [ laserDraft; lp1Draft; lp2Draft; cpl1Draft; retDraft; phoneDraft; polarimeterDraft; ellipsometerDraft; glDraft ]

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
        | Error (UnknownPlannerTargetErr _ as error) -> failwith $"Expected an unresolved item, got: {error.describe}"
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
