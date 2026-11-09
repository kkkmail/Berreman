namespace OpticalConstructor.Tests

open System
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit

/// An item's identity and the model it instantiates in `Kit` (spec 0050 S7.1, 001 §I).
module KitItemTests =

    /// The exact entry version of an item model, built from its GUID and version rather than through the member under test.
    let private exactModelVersion (model : ItemModelVersionId) : EntryReference =
        EntryVersionReference { entryId = model.itemModelId.toLibraryEntryId; version = model.version }

    let private assertReferences (instance : ItemModelInstance) (model : ItemModelVersionId) (binding : EntryReference option) : unit =
        Assert.Equal<EntryReference option>(Some (exactModelVersion model), instance.entryReferenceOpt)
        Assert.Equal<EntryReference option>(Some model.entryReference, instance.entryReferenceOpt)
        Assert.Equal<EntryReference option>(binding, instance.bindingReferenceOpt)

    [<Fact>]
    let ``an item label keeps its text, refuses a blank one and takes its file stem from the shared name rule`` () : unit =
        match ItemLabel.tryCreate "CPL:2" with
        | Ok label ->
            Assert.Equal<ItemLabel>(ItemLabel "CPL:2", label)
            Assert.Equal<string>("CPL:2", label.value)
            Assert.Equal<FileStem>(FileStem "CPL_2", label.fileStem)
            Assert.Equal<FileStem>(FileStem.ofText "CPL:2", label.fileStem)
            Assert.Equal<FileName>(FileName "CPL_2.item-json", ItemKitFile.fileNameOf label.fileStem)
        | Error error -> Assert.Fail(error.describe)

        match ItemLabel.tryCreate " QZ " with
        | Ok label -> Assert.Equal<string>(" QZ ", label.value)
        | Error error -> Assert.Fail(error.describe)

        for text in [ ""; "  " ] do
            match ItemLabel.tryCreate text with
            | Error (InvalidItemLabelErr reason) -> Assert.Contains($"'{text}'", reason)
            | Error error -> Assert.Fail($"expected an invalid item label: {error.describe}")
            | Ok label -> Assert.Fail($"accepted a blank item label: '{label.value}'")

    [<Fact>]
    let ``an item revision starts at one and counts up`` () : unit =
        Assert.Equal<ItemRevision>(ItemRevision 1, ItemRevision.first)
        Assert.Equal(1, ItemRevision.first.value)
        Assert.Equal<ItemRevision>(ItemRevision 2, ItemRevision.first.next)
        Assert.Equal<ItemRevision>(ItemRevision 3, ItemRevision.first.next.next)
        Assert.Equal(3, ItemRevision.first.next.next.value)

    [<Fact>]
    let ``an item's model is referenced as the library snapshots it`` () : unit =
        let sourceModel = (BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId).versionId
        let polarizerModel = (BuiltinCatalogue.itemModel ItemModelIds.idealPolarizerItemModelId).versionId
        let stackModel = (BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId).versionId
        let standaloneModel = (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId).versionId
        let sampleVersion = SampleVersionId.firstOf SampleIds.glassPlate1mmId
        let presetAtLatest (preset : ElementValueId) : EntryReference = EntryLatestReference (LibraryEntryId (Guid.Parse preset.value))
        let sampleAtVersion : EntryReference =
            EntryVersionReference { entryId = SampleIds.glassPlate1mmId.toLibraryEntryId; version = VersionNumber.first }

        let bound : (ItemModelInstance * ItemModelVersionId * EntryReference) list =
            [
                PresetInstance (sourceModel, PresetIds.heNe633), sourceModel, presetAtLatest PresetIds.heNe633
                PresetInstance (polarizerModel, PresetIds.polarizerLp), polarizerModel, presetAtLatest PresetIds.polarizerLp
                SampleInstance (stackModel, sampleVersion), stackModel, sampleAtVersion
            ]

        for instance, model, binding in bound do
            assertReferences instance model (Some binding)
            Assert.NotEqual<EntryReference option>(instance.entryReferenceOpt, instance.bindingReferenceOpt)

        assertReferences (ItemModelVersionInstance standaloneModel) standaloneModel None

        // A preset id that is not a canonical GUID binds nothing, and the model version is still referenced.
        for text in [ "not-a-guid"; PresetIds.heNe633.value.ToUpperInvariant() ] do
            assertReferences (PresetInstance (sourceModel, ElementValueId text)) sourceModel None

    [<Fact>]
    let ``every item error renders its condition and its payload`` () : unit =
        let cases : (ItemError * string) list =
            [
                InvalidItemLabelErr "'  ': blank", "the item label is not valid: '  ': blank"
                DuplicateItemLabelErr "LP:1 beside LP_1", "the kit already has an item of that label: LP:1 beside LP_1"
                DuplicateItemIdErr "LP1 (3f0c)", "the kit already has an item of that id: LP1 (3f0c)"
                UnknownItemParameterErr "LP1.nothing", "the item has no such parameter: LP1.nothing"
                InvalidParameterStateErr "QZ.retardance: NaN", "the parameter state is not valid: QZ.retardance: NaN"
                InvalidItemModelBindingErr "source model v1 over not-a-guid", "the item model binding is not valid: source model v1 over not-a-guid"
                UnknownKitItemErr "CPL2 (9a1e)", "the kit has no item of that id: CPL2 (9a1e)"
            ]

        for error, expected in cases do
            Assert.Equal<string>(expected, error.describe)

        let descriptions = cases |> List.map (fun (error, _) -> error.describe)
        Assert.Equal(7, descriptions |> List.distinct |> List.length)

    [<Fact>]
    let ``a parameter is created known at its axis default`` () : unit =
        let axes = (OpticalElementModel (RetarderPlateModel ConstantRetardance)).parameterAxes

        match axes |> List.tryFind (fun axis -> axis.name = ParameterName "retardance") with
        | Some axis ->
            let expected : Parameter =
                {
                    name = ParameterName "retardance"
                    state = KnownParameter (ParameterValue (Math.PI / 2.0))
                }

            Assert.Equal<Parameter>(expected, Parameter.knownAtDefault axis)
        | None -> Assert.Fail("the constant retarder plate publishes no retardance axis")

    [<Fact>]
    let ``a parameter whose state keeps its rules is kept, and one that breaks them is InvalidParameterStateErr`` () : unit =
        let parameterOf (state : ParameterState) : Parameter = { name = ParameterName "retardance"; state = state }
        let unitInterval : ParameterInterval = { lower = ParameterValue 0.0; upper = ParameterValue 1.0 }

        let unknownWithin (start : ParameterValue) (bounds : ParameterInterval) (priorOpt : ParameterPrior option) : ParameterState =
            UnknownParameter { start = start; bounds = bounds; priorOpt = priorOpt }

        let priorOf (sigma : ParameterSigma) : ParameterPrior = { centre = ParameterValue 0.5; sigma = sigma }

        let calibratedWith (sigma : ParameterSigma) : ParameterState =
            let calibration : CalibratedParameterData =
                {
                    value = ParameterValue 0.25
                    sigma = sigma
                    fitRun = FitRunName "QZ-LR@20261101T120000Z"
                }

            CalibratedParameter calibration

        let kept : ParameterState list =
            [
                KnownParameter (ParameterValue 0.1)
                unknownWithin (ParameterValue 0.5) unitInterval None
                unknownWithin (ParameterValue 0.5) unitInterval (Some (priorOf (ParameterSigma 0.1)))
                unknownWithin (ParameterValue 0.0) unitInterval None
                calibratedWith (ParameterSigma 0.01)
            ]

        for state in kept do
            let parameter = parameterOf state

            match parameter.validate with
            | Ok validated -> Assert.Equal<Parameter>(parameter, validated)
            | Error error -> Assert.Fail($"refused a parameter that keeps its rules: {error.describe}")

        let unboundedAbove : ParameterInterval = { lower = ParameterValue 0.0; upper = ParameterValue Double.PositiveInfinity }
        let empty : ParameterInterval = { lower = ParameterValue 1.0; upper = ParameterValue 1.0 }

        // Each refused state names the rule it breaks.
        let refused : (ParameterState * string) list =
            [
                KnownParameter (ParameterValue Double.NaN), "the known value NaN is not finite"
                unknownWithin (ParameterValue 0.5) unboundedAbove None, "the upper bound Infinity is not finite"
                unknownWithin (ParameterValue 1.0) empty None, "the lower bound 1 is not below the upper bound 1"
                unknownWithin (ParameterValue (-0.5)) unitInterval None, "the start -0.5 lies outside [0, 1]"
                unknownWithin (ParameterValue 0.5) unitInterval (Some (priorOf (ParameterSigma 0.0))), "the prior sigma 0 is not above zero"
                calibratedWith (ParameterSigma (-1.0)), "the calibration sigma -1 is not above zero"
            ]

        for state, rule in refused do
            match (parameterOf state).validate with
            | Error (InvalidParameterStateErr reason) -> Assert.Equal<string>($"retardance: {rule}", reason)
            | Error error -> Assert.Fail($"expected an invalid parameter state: {error.describe}")
            | Ok parameter -> Assert.Fail($"kept a parameter that breaks its rules: {parameter.name.value}")
