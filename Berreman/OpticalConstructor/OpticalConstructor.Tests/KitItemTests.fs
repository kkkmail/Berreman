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
