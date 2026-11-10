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

    /// A preset's binding at its latest version, built from its GUID text rather than through the member under test.
    let private presetAtLatest (preset : ElementValueId) : EntryReference = EntryLatestReference (LibraryEntryId (Guid.Parse preset.value))

    let private sourceItemModel : ItemModelVersion = BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId
    let private polarizerItemModel : ItemModelVersion = BuiltinCatalogue.itemModel ItemModelIds.idealPolarizerItemModelId
    let private stackItemModel : ItemModelVersion = BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId
    let private sourceModel : ItemModelVersionId = sourceItemModel.versionId
    let private polarizerModel : ItemModelVersionId = polarizerItemModel.versionId
    let private stackModel : ItemModelVersionId = stackItemModel.versionId
    let private sampleVersion : SampleVersionId = SampleVersionId.firstOf SampleIds.glassPlate1mmId

    /// The glass plate's first version as a reference, built from its GUID rather than through the member under test.
    let private sampleAtVersion : EntryReference =
        EntryVersionReference { entryId = SampleIds.glassPlate1mmId.toLibraryEntryId; version = VersionNumber.first }

    /// The Built-in polarizer preset of that id, failing when the catalogue holds another kind under it.
    let private polarizerPreset (id : ElementValueId) : PolarizerPreset =
        match BuiltinCatalogue.preset id with
        | PolarizerItem polarizer -> polarizer
        | SampleItem _ | SourceItem _ | DetectorItem _ -> failwith $"The Built-in preset '{id.value}' is not a polarizer."

    let private knownAt (name : ParameterName) (value : ParameterValue) : Parameter = { name = name; state = KnownParameter value }

    /// The draft's item, failing with the refusal's text when `addTo` refuses it.
    let private added (items : Item list) (draft : ItemDraft) : Item =
        match draft.addTo items with
        | Ok item -> item
        | Error error -> failwith $"'{draft.label.value}' was refused: {error.describe}"

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
        let standaloneModel = (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId).versionId

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

    [<Fact>]
    let ``an item references its model's library version once and finds its parameters by name`` () : unit =
        let circularPolarizerV1 : ItemModelVersionId =
            {
                itemModelId = ItemModelIds.circularPolarizerModelId
                version = VersionNumber.first
            }

        let dialZero : Parameter = { name = ParameterName "dialZero"; state = KnownParameter (ParameterValue 0.0) }
        let retardanceAt (value : ParameterValue) : Parameter = { name = ParameterName "c2.retardance"; state = KnownParameter value }

        let revisionOne : ItemRevisionRecord =
            {
                revision = ItemRevision.first
                parameters = [ retardanceAt (ParameterValue (Math.PI / 2.0)); dialZero ]
            }

        let cpl1 : Item =
            {
                id = ItemId (Guid.NewGuid())
                label = ItemLabel "CPL1"
                model = ItemModelVersionInstance circularPolarizerV1
                parameters = [ retardanceAt (ParameterValue 1.55); dialZero ]
                dial = DialMounted DialPositive
                photoOpt = Some (KitRelativePath "data/photos/CPL1.jpg")
                notes = ItemNotes "the left-handed circular polarizer"
                revision = ItemRevision.first.next
                history = [ revisionOne ]
            }

        Assert.Equal<EntryReference list>([ exactModelVersion circularPolarizerV1 ], cpl1.libraryReferences)
        Assert.Equal<EntryReference list>([ circularPolarizerV1.entryReference ], cpl1.libraryReferences)
        Assert.Equal<Parameter option>(Some dialZero, cpl1.tryFindParameter (ParameterName "dialZero"))
        Assert.Equal<Parameter option>(Some (retardanceAt (ParameterValue 1.55)), cpl1.tryFindParameter (ParameterName "c2.retardance"))
        Assert.Equal<Parameter option>(None, cpl1.tryFindParameter (ParameterName "nothing"))

        let itemOver (label : ItemLabel) (model : ItemModelInstance) : Item =
            {
                id = ItemId (Guid.NewGuid())
                label = label
                model = model
                parameters = []
                dial = FixedMounting
                photoOpt = None
                notes = ItemNotes ""
                revision = ItemRevision.first
                history = []
            }

        let bound : (Item * EntryReference list) list =
            [
                itemOver (ItemLabel "Laser") (PresetInstance (sourceModel, PresetIds.heNe633)),
                [ exactModelVersion sourceModel; presetAtLatest PresetIds.heNe633 ]
                itemOver (ItemLabel "LP1") (PresetInstance (polarizerModel, PresetIds.polarizerLp)),
                [ exactModelVersion polarizerModel; presetAtLatest PresetIds.polarizerLp ]
                itemOver (ItemLabel "QZ") (SampleInstance (stackModel, sampleVersion)),
                [ exactModelVersion stackModel; sampleAtVersion ]
            ]

        for item, references in bound do
            Assert.Equal<EntryReference list>(references, item.libraryReferences)

        // A binding naming the model's own GUID and version is the same reference, so it is named once.
        let sharedEntry = Guid.Parse "6f1d2c3b-4a59-4e68-9d7c-8b0a1f2e3d4c"
        let sharedModel : ItemModelVersionId = { itemModelId = ItemModelId sharedEntry; version = VersionNumber.first }
        let sharedSample : SampleVersionId = { sampleId = SampleId sharedEntry; version = VersionNumber.first }
        let selfBound = itemOver (ItemLabel "QZ2") (SampleInstance (sharedModel, sharedSample))
        Assert.Equal<EntryReference list>([ exactModelVersion sharedModel ], selfBound.libraryReferences)

    [<Fact>]
    let ``a dial-mounted item publishes its dial zero and a fixed one nothing`` () : unit =
        let dialZero : ItemParameterAxis =
            {
                name = ParameterName "dialZero"
                kind = AngleParameter
                defaultValue = ParameterValue 0.0
            }

        Assert.Equal<ItemParameterAxis list>([ dialZero ], (DialMounted DialNegative).parameterAxes)
        Assert.Equal<ItemParameterAxis list>([ dialZero ], (DialMounted DialPositive).parameterAxes)
        Assert.Equal<ItemParameterAxis list>([], FixedMounting.parameterAxes)

    [<Fact>]
    let ``adding an item from a library model creates its parameters known at the model's defaults and its dial zero`` () : unit =
        let circularPolarizer = BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId
        let cpl1Id = ItemId (Guid.Parse "7d613690-0061-4b67-802e-4d443d689476")

        let cpl1 : ItemDraft =
            {
                id = cpl1Id
                label = ItemLabel "CPL1"
                model = LibraryModelChoice circularPolarizer
                dial = DialMounted DialPositive
            }

        let circularPolarizerV1 : ItemModelVersionId =
            {
                itemModelId = ItemModelIds.circularPolarizerModelId
                version = VersionNumber.first
            }

        let expected : Item =
            {
                id = cpl1Id
                label = ItemLabel "CPL1"
                model = ItemModelVersionInstance circularPolarizerV1
                parameters =
                    [
                        knownAt (ParameterName "c1.extinctionRatio") (ParameterValue 0.0)
                        knownAt (ParameterName "c1.axisOffset") (ParameterValue 0.0)
                        knownAt (ParameterName "c2.retardance") (ParameterValue (Math.PI / 2.0))
                        knownAt (ParameterName "c2.fastAxisOffset") (ParameterValue 0.0)
                        knownAt (ParameterName "dialZero") (ParameterValue 0.0)
                    ]
                dial = DialMounted DialPositive
                photoOpt = None
                notes = ItemNotes ""
                revision = ItemRevision 1
                history = []
            }

        let item = cpl1.toItem
        Assert.Equal<Item>(expected, item)
        Assert.Equal<ItemModelInstance>(ItemModelVersionInstance circularPolarizer.versionId, item.model)

        // Every parameter is its axis's default, the model's axes first and the dial's last.
        let axes = circularPolarizer.entry.model.parameterAxes @ (DialMounted DialPositive).parameterAxes
        Assert.Equal<Parameter list>(axes |> List.map Parameter.knownAtDefault, item.parameters)

        match cpl1.addTo [] with
        | Ok addedItem -> Assert.Equal<Item>(expected, addedItem)
        | Error error -> Assert.Fail($"an empty kit refused CPL1: {error.describe}")

    [<Fact>]
    let ``a source preset, an ideal polarizer and a sample give an item their own parameters`` () : unit =
        let heNe = BuiltinCatalogue.source PresetIds.heNe633
        let linearPolarizer = polarizerPreset PresetIds.polarizerLp
        let glassPlate = BuiltinCatalogue.sample SampleIds.glassPlate1mmId

        let laserDraft : ItemDraft =
            {
                id = ItemId (Guid.Parse "f08359e8-6ea0-485d-bb95-dcb2ff4c9023")
                label = ItemLabel "Laser"
                model = SourcePresetChoice (sourceItemModel, heNe)
                dial = FixedMounting
            }

        let lp1Draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "b8492f97-06ee-40b2-a563-ce9970e1f8ab")
                label = ItemLabel "LP1"
                model = PolarizerPresetChoice (polarizerItemModel, linearPolarizer)
                dial = DialMounted DialNegative
            }

        let plateDraft : ItemDraft =
            {
                id = ItemId (Guid.Parse "6b1c045e-8e01-47cf-a319-a6c2e3567071")
                label = ItemLabel "Plate"
                model = StackSampleChoice (stackItemModel, sampleVersion, glassPlate)
                dial = FixedMounting
            }

        let laser = added [] laserDraft
        let lp1 = added [ laser ] lp1Draft
        let plate = added [ laser; lp1 ] plateDraft

        // Each names the shipped item model's exact version and binds its preset or sample as typed physics.
        let modelV1 (id : ItemModelId) : ItemModelVersionId = { itemModelId = id; version = VersionNumber.first }
        let laserModel = modelV1 ItemModelIds.sourcePresetItemModelId
        let lp1Model = modelV1 ItemModelIds.idealPolarizerItemModelId
        let plateModel = modelV1 ItemModelIds.stackSampleItemModelId

        let instances : (Item * ItemModelInstance * ItemModelVersionId) list =
            [
                laser, PresetInstance (laserModel, PresetIds.heNe633), laserModel
                lp1, PresetInstance (lp1Model, PresetIds.polarizerLp), lp1Model
                plate, SampleInstance (plateModel, sampleVersion), plateModel
            ]

        for item, instance, model in instances do
            Assert.Equal<ItemModelInstance>(instance, item.model)
            Assert.Equal<EntryReference option>(Some (exactModelVersion model), item.model.entryReferenceOpt)

        Assert.Equal<Parameter list>(
            [
                knownAt (ParameterName "azimuth") (ParameterValue 0.0)
                knownAt (ParameterName "ellipticity") (ParameterValue 0.0)
                knownAt (ParameterName "degreeOfPolarization") (ParameterValue 0.0)
                knownAt (ParameterName "intensityScale") (ParameterValue 1.0)
            ],
            laser.parameters)

        Assert.Equal<Parameter list>([ knownAt (ParameterName "dialZero") (ParameterValue 0.0) ], lp1.parameters)

        match plate.parameters with
        | [ { name = name; state = KnownParameter thickness } ] ->
            Assert.Equal<ParameterName>(ParameterName "substrate.thickness", name)
            Assert.Equal(1e-3, thickness.value, 1e-15)
        | parameters -> Assert.Fail($"the plate holds {parameters.Length} parameters, not its substrate thickness alone")

        // A model that does not take the choice's binding, a preset id that is no GUID, a sample version of another
        // sample and a binding model chosen without its binding are each refused, and the kit keeps what it held.
        let kit = [ laser; lp1; plate ]
        let circularPolarizer = BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId
        let notAGuid : PolarizerPreset = { linearPolarizer with id = ElementValueId "not-a-guid" }

        let refused : (ItemModelChoice * ItemModelVersion) list =
            [
                SourcePresetChoice (polarizerItemModel, heNe), polarizerItemModel
                SourcePresetChoice (circularPolarizer, heNe), circularPolarizer
                PolarizerPresetChoice (stackItemModel, linearPolarizer), stackItemModel
                PolarizerPresetChoice (polarizerItemModel, notAGuid), polarizerItemModel
                StackSampleChoice (stackItemModel, SampleVersionId.firstOf SampleIds.glassFilm600Id, glassPlate), stackItemModel
                StackSampleChoice (sourceItemModel, sampleVersion, glassPlate), sourceItemModel
                LibraryModelChoice sourceItemModel, sourceItemModel
                LibraryModelChoice stackItemModel, stackItemModel
            ]

        for choice, model in refused do
            let draft : ItemDraft =
                {
                    id = ItemId (Guid.Parse "540fdf04-d6ca-47f4-a374-49a16f028b1f")
                    label = ItemLabel "Refused"
                    model = choice
                    dial = FixedMounting
                }

            match draft.addTo kit with
            | Error (InvalidItemModelBindingErr reason) -> Assert.Contains($"'{model.entry.name.value}'", reason)
            | Error error -> Assert.Fail($"expected an invalid item model binding: {error.describe}")
            | Ok item -> Assert.Fail($"recorded an item over a model that does not take its binding: {item.label.value}")

        Assert.Equal<Item list>([ laser; lp1; plate ], kit)

    [<Fact>]
    let ``two items of one model are two items with equal starting parameters`` () : unit =
        let realLinearPolarizer = BuiltinCatalogue.itemModel ItemModelIds.realLinearPolarizerModelId

        let draftOf (id : ItemId) (label : ItemLabel) : ItemDraft =
            {
                id = id
                label = label
                model = LibraryModelChoice realLinearPolarizer
                dial = DialMounted DialPositive
            }

        let lp1 = added [] (draftOf (ItemId (Guid.Parse "28f940f8-6b2e-48c5-8e93-65dc011e8780")) (ItemLabel "LP1"))
        let lp2 = added [ lp1 ] (draftOf (ItemId (Guid.Parse "c52c3d48-ecc5-4019-bf1e-1a5b3df11240")) (ItemLabel "LP2"))

        Assert.NotEqual<ItemId>(lp1.id, lp2.id)
        Assert.NotEqual<ItemLabel>(lp1.label, lp2.label)
        Assert.Equal<ItemModelInstance>(ItemModelVersionInstance realLinearPolarizer.versionId, lp1.model)
        Assert.Equal<ItemModelInstance>(lp1.model, lp2.model)
        Assert.Equal<Parameter list>(lp1.parameters, lp2.parameters)

        Assert.Equal<Parameter list>(
            [
                knownAt (ParameterName "extinctionRatio") (ParameterValue 0.0)
                knownAt (ParameterName "axisOffset") (ParameterValue 0.0)
                knownAt (ParameterName "dialZero") (ParameterValue 0.0)
            ],
            lp2.parameters)

    [<Fact>]
    let ``adding an item whose id or label file name the kit already holds is refused`` () : unit =
        let realLinearPolarizer = BuiltinCatalogue.itemModel ItemModelIds.realLinearPolarizerModelId
        let lp1Id = ItemId (Guid.Parse "c4c3c64b-0c6f-4f68-a2e3-0b611726ac81")

        let draftOf (id : ItemId) (label : ItemLabel) : ItemDraft =
            {
                id = id
                label = label
                model = LibraryModelChoice realLinearPolarizer
                dial = DialMounted DialPositive
            }

        let lp1 = added [] (draftOf lp1Id (ItemLabel "LP1"))
        let lpUnderscore1 = added [ lp1 ] (draftOf (ItemId (Guid.Parse "82f8374a-12e6-48c0-8b07-c1f706a7688e")) (ItemLabel "LP_1"))
        let kit = [ lp1; lpUnderscore1 ]

        // On Windows `lp1.item-json` is the file `LP1.item-json`, and `LP:1` is stored as `LP_1.item-json`.
        let sameFile : (ItemLabel * ItemLabel) list =
            [
                ItemLabel "lp1", ItemLabel "LP1"
                ItemLabel "LP:1", ItemLabel "LP_1"
            ]

        for label, held in sameFile do
            match (draftOf (ItemId (Guid.Parse "f09049d9-b228-4c69-ab7e-d655d97c66e0")) label).addTo kit with
            | Error (DuplicateItemLabelErr reason) ->
                Assert.Contains($"'{label.value}'", reason)
                Assert.Contains($"'{held.value}'", reason)
                Assert.Contains((ItemKitFile.fileNameOf held.fileStem).value, reason)
            | Error error -> Assert.Fail($"expected a duplicate item label: {error.describe}")
            | Ok item -> Assert.Fail($"recorded '{item.label.value}' beside '{held.value}'")

        match (draftOf lp1Id (ItemLabel "LP3")).addTo kit with
        | Error (DuplicateItemIdErr reason) ->
            Assert.Contains("'LP3'", reason)
            Assert.Contains("'LP1'", reason)
            Assert.Contains(lp1Id.value.ToString(), reason)
        | Error error -> Assert.Fail($"expected a duplicate item id: {error.describe}")
        | Ok item -> Assert.Fail($"recorded '{item.label.value}' under LP1's id")

        // A new id under a new file name is added beside them.
        let lp2 = added kit (draftOf (ItemId (Guid.Parse "c91baa09-e606-4a43-bf77-259fd3db1a05")) (ItemLabel "LP2"))
        Assert.Equal<ItemLabel>(ItemLabel "LP2", lp2.label)

    // ============================ spec 0050 S7.2: the item mapping of a copy ============================

    /// An item of a shipped library model, built from its resolved version, failing when `addTo` refuses it.
    let private modelItem (model : ItemModelVersion) (id : ItemId) (label : ItemLabel) : Item =
        let draft : ItemDraft =
            {
                id = id
                label = label
                model = LibraryModelChoice model
                dial = DialMounted DialPositive
            }

        added [] draft

    let private realLinearPolarizerModel : ItemModelVersion = BuiltinCatalogue.itemModel ItemModelIds.realLinearPolarizerModelId
    let private circularPolarizerModel : ItemModelVersion = BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId

    let private heldLp1 : Item = modelItem realLinearPolarizerModel (ItemId (Guid.Parse "1e6b9c3a-4f20-4d8e-a7b1-5c0d2e9f3a61")) (ItemLabel "LP1")
    let private heldCpl1 : Item = modelItem circularPolarizerModel (ItemId (Guid.Parse "2f7cad4b-5031-4e9f-b8c2-6d1e3f0a4b72")) (ItemLabel "CPL1")

    /// A glass plate on the stack-sample item model: the kit's `QZ` and a copy's `QZ` are two pieces of one model.
    let private plateItem (id : ItemId) (label : ItemLabel) : Item =
        let draft : ItemDraft =
            {
                id = id
                label = label
                model = StackSampleChoice (stackItemModel, sampleVersion, BuiltinCatalogue.sample SampleIds.glassPlate1mmId)
                dial = FixedMounting
            }

        added [] draft

    [<Fact>]
    let ``an added item's label takes the next free suffix among the labels the kit holds`` () : unit =
        let cases : (ItemLabel * ItemLabel list * ItemLabel) list =
            [
                ItemLabel "CPL1", [ ItemLabel "cpl1"; ItemLabel "CPL1 (2)" ], ItemLabel "CPL1 (3)"
                ItemLabel "CPL1", [], ItemLabel "CPL1"
                // `LP:1` is stored as `LP_1.item-json`, the file `LP_1` already names.
                ItemLabel "LP:1", [ ItemLabel "LP_1" ], ItemLabel "LP:1 (2)"
                ItemLabel "QZ", [ ItemLabel "LP1"; ItemLabel "QZ (2)" ], ItemLabel "QZ"
            ]

        for label, taken, expected in cases do
            Assert.Equal<ItemLabel>(expected, label.nextFreeAmong taken)

    [<Fact>]
    let ``an item maps by default onto the kit's item of its id, then of its model and label, and is otherwise added`` () : unit =
        let held = [ heldLp1; heldCpl1 ]

        let cases : (Item * ItemCopyChoice) list =
            [
                // One id is one physical piece, whatever its label or model in the other kit.
                modelItem circularPolarizerModel heldLp1.id (ItemLabel "Polarizer A"), MapOntoKitItem heldLp1.id
                modelItem circularPolarizerModel (ItemId (Guid.Parse "3a8dbe5c-6142-4fa0-89d3-7e2f4a1b5c83")) (ItemLabel "CPL1"), MapOntoKitItem heldCpl1.id
                modelItem realLinearPolarizerModel (ItemId (Guid.Parse "4b9ecf6d-7253-40b1-9ae4-8f3a5b2c6d94")) (ItemLabel "CPL1"), AddCopiedItem
                modelItem circularPolarizerModel (ItemId (Guid.Parse "5cafd07e-8364-41c2-abf5-9a4b6c3d7ea5")) (ItemLabel "CPL2"), AddCopiedItem
                // The label is matched ordinally.
                modelItem circularPolarizerModel (ItemId (Guid.Parse "6db0e18f-9475-42d3-bc06-ab5c7d4e8fb6")) (ItemLabel "cpl1"), AddCopiedItem
            ]

        for source, expected in cases do
            Assert.Equal<ItemCopyChoice>(expected, source.defaultCopyChoice held)

        Assert.Equal<ItemCopyChoice>(AddCopiedItem, heldLp1.defaultCopyChoice [])

    [<Fact>]
    let ``the mapping rows follow the used items and keep the choices already made`` () : unit =
        let sourceQz = plateItem (ItemId (Guid.Parse "7ec1f2a0-a586-43e4-8d17-bc6d8e5f9ac7")) (ItemLabel "QZ")
        let heldQz = plateItem (ItemId (Guid.Parse "8fd203b1-b697-44f5-9e28-cd7e9f6a0bd8")) (ItemLabel "QZ")
        let unused = modelItem circularPolarizerModel (ItemId (Guid.Parse "90e314c2-c7a8-4506-af39-de8fa07b1ce9")) (ItemLabel "CPL9")
        let held = [ heldLp1; heldQz ]
        let used = [ heldLp1; sourceQz ]

        let defaults : ItemCopyRow list =
            [
                { sourceItem = heldLp1; choice = MapOntoKitItem heldLp1.id }
                { sourceItem = sourceQz; choice = MapOntoKitItem heldQz.id }
            ]

        Assert.Equal<ItemCopyRow list>(defaults, ItemCopyRow.copyRowsFor used held [])

        let chosen : ItemCopyRow list =
            [
                { sourceItem = unused; choice = AddCopiedItem }
                { sourceItem = sourceQz; choice = AddCopiedItem }
            ]

        let expected : ItemCopyRow list =
            [
                { sourceItem = heldLp1; choice = MapOntoKitItem heldLp1.id }
                { sourceItem = sourceQz; choice = AddCopiedItem }
            ]

        Assert.Equal<ItemCopyRow list>(expected, ItemCopyRow.copyRowsFor used held chosen)
        Assert.Equal<ItemCopyRow list>(List.rev expected, ItemCopyRow.copyRowsFor (List.rev used) held chosen)
        Assert.Empty(ItemCopyRow.copyRowsFor [] held chosen)

    [<Fact>]
    let ``the items to add are checked before any is answered, and each clashing label is suffixed`` () : unit =
        let held = [ heldCpl1 ]
        let firstCopy = modelItem circularPolarizerModel (ItemId (Guid.Parse "a1f425d3-d8b9-4617-b04a-ef9ab18c2dfa")) (ItemLabel "CPL1")
        let secondCopy = modelItem circularPolarizerModel (ItemId (Guid.Parse "b2a536e4-e9ca-4728-815b-f0abc29d3e0b")) (ItemLabel "CPL1")
        let mapped = modelItem circularPolarizerModel (ItemId (Guid.Parse "c3b647f5-fadb-4839-926c-01bcd3ae4f1c")) (ItemLabel "CPL7")

        let rows : ItemCopyRow list =
            [
                { sourceItem = firstCopy; choice = AddCopiedItem }
                { sourceItem = mapped; choice = MapOntoKitItem heldCpl1.id }
                { sourceItem = secondCopy; choice = AddCopiedItem }
            ]

        let expected : Item list =
            [
                { firstCopy with label = ItemLabel "CPL1 (2)" }
                { secondCopy with label = ItemLabel "CPL1 (3)" }
            ]

        Assert.Equal<Result<Item list, ItemError>>(Ok expected, ItemCopyRow.itemsToAdd rows held)
        Assert.Equal<Result<Item list, ItemError>>(Ok [], ItemCopyRow.itemsToAdd [ { sourceItem = mapped; choice = MapOntoKitItem heldCpl1.id } ] held)

        // A refusing row after the valid ones still answers no item.
        let addsHeldPiece = rows @ [ { sourceItem = { heldCpl1 with label = ItemLabel "Circular A" }; choice = AddCopiedItem } ]

        match ItemCopyRow.itemsToAdd addsHeldPiece held with
        | Error (DuplicateItemIdErr reason) ->
            Assert.Contains("'Circular A'", reason)
            Assert.Contains("'CPL1'", reason)
        | other -> Assert.Fail($"expected DuplicateItemIdErr, got %A{other}")

        let missing = ItemId (Guid.Parse "d4c75806-0bec-4a4d-a37d-12cde4bf502d")
        let mapsOntoMissing = rows @ [ { sourceItem = mapped; choice = MapOntoKitItem missing } ]

        match ItemCopyRow.itemsToAdd mapsOntoMissing held with
        | Error (UnknownKitItemErr reason) ->
            Assert.Contains("'CPL7'", reason)
            Assert.Contains(missing.value.ToString(), reason)
        | other -> Assert.Fail($"expected UnknownKitItemErr, got %A{other}")

    // ============================ spec 0050 S7.1: setting one parameter's state ============================

    let private retardance : ParameterName = ParameterName "c2.retardance"

    /// An unknown retardance a fit starts at π / 2 and searches within the given bounds, with no prior.
    let private unknownRetardance (lower : ParameterValue) (upper : ParameterValue) : ParameterState =
        UnknownParameter
            {
                start = ParameterValue (Math.PI / 2.0)
                bounds = { lower = lower; upper = upper }
                priorOpt = None
            }

    /// What the circular polarizer and its dial publish, every parameter known at its default (step 216).
    let private circularPolarizerDefaults : Parameter list =
        circularPolarizerModel.entry.model.parameterAxes @ (DialMounted DialPositive).parameterAxes
        |> List.map Parameter.knownAtDefault

    [<Fact>]
    let ``setting a parameter's state replaces it in place and keeps the revision`` () : unit =
        let unknown = unknownRetardance (ParameterValue (Math.PI / 4.0)) (ParameterValue (3.0 * Math.PI / 4.0))

        match heldCpl1.withParameterState retardance unknown with
        | Ok cpl1 ->
            Assert.Equal<Parameter list>(
                [
                    knownAt (ParameterName "c1.extinctionRatio") (ParameterValue 0.0)
                    knownAt (ParameterName "c1.axisOffset") (ParameterValue 0.0)
                    { name = retardance; state = unknown }
                    knownAt (ParameterName "c2.fastAxisOffset") (ParameterValue 0.0)
                    knownAt (ParameterName "dialZero") (ParameterValue 0.0)
                ],
                cpl1.parameters)

            // Only the parameter list changes: the id, label, model, dial, notes, revision and history are the item's.
            Assert.Equal<Item>({ heldCpl1 with parameters = cpl1.parameters }, cpl1)
            Assert.Equal<ItemRevision>(ItemRevision 1, cpl1.revision)
            Assert.Empty(cpl1.history)

            // A second edit stays in the working revision: restoring the default gives the item back as it was made.
            let atDefault = KnownParameter (ParameterValue (Math.PI / 2.0))
            Assert.Equal<Result<Item, ItemError>>(Ok heldCpl1, cpl1.withParameterState retardance atDefault)
        | Error error -> Assert.Fail($"CPL1 refused an unknown retardance within its bounds: {error.describe}")

    [<Fact>]
    let ``setting the state of a parameter the item lacks, or one that breaks its rules, is refused`` () : unit =
        match heldCpl1.withParameterState (ParameterName "nothing") (KnownParameter (ParameterValue 0.0)) with
        | Error (UnknownItemParameterErr reason) ->
            Assert.Contains("'nothing'", reason)
            Assert.Contains("'CPL1'", reason)
        | Error error -> Assert.Fail($"expected an unknown item parameter: {error.describe}")
        | Ok item -> Assert.Fail($"'{item.label.value}' took a state for a parameter it lacks")

        let reversed = unknownRetardance (ParameterValue (3.0 * Math.PI / 4.0)) (ParameterValue (Math.PI / 4.0))
        let reversedRetardance : Parameter = { name = retardance; state = reversed }

        match heldCpl1.withParameterState retardance reversed with
        | Error (InvalidParameterStateErr reason) ->
            Assert.StartsWith("c2.retardance: the lower bound ", reason)
            Assert.Contains(" is not below the upper bound ", reason)
            Assert.Equal<Result<Parameter, ItemError>>(Error (InvalidParameterStateErr reason), reversedRetardance.validate)
        | Error error -> Assert.Fail($"expected an invalid parameter state: {error.describe}")
        | Ok item -> Assert.Fail($"'{item.label.value}' took an unknown whose bounds are reversed")

        Assert.Equal<Item>(modelItem circularPolarizerModel heldCpl1.id heldCpl1.label, heldCpl1)
        Assert.Equal<Parameter list>(circularPolarizerDefaults, heldCpl1.parameters)

    [<Fact>]
    let ``two items of one model keep independent parameters`` () : unit =
        let cpl2 = modelItem circularPolarizerModel (ItemId (Guid.Parse "e4d8692b-1c3e-4a5f-8b6d-7f0e2a9c3b14")) (ItemLabel "CPL2")
        let fastAxisOffset = ParameterName "c2.fastAxisOffset"
        let offset = KnownParameter (ParameterValue 0.01)

        match heldCpl1.withParameterState fastAxisOffset offset with
        | Ok cpl1 ->
            Assert.Equal<Parameter option>(Some { name = fastAxisOffset; state = offset }, cpl1.tryFindParameter fastAxisOffset)
            Assert.Equal<Parameter list>(circularPolarizerDefaults, cpl2.parameters)
            Assert.Equal<ItemModelInstance>(cpl1.model, cpl2.model)
            Assert.NotEqual<Parameter list>(cpl1.parameters, cpl2.parameters)
        | Error error -> Assert.Fail($"CPL1 refused a known fast-axis offset: {error.describe}")
