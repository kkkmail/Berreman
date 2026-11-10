namespace OpticalConstructor.Tests

open System
open System.Globalization
open System.IO
open System.Text
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Storage
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Ui.TableAndElementRotationView

/// The Items panel's state and AddItem, as semantic messages of the constructor window (spec 0050 S7.2).
module ItemsPanelModelTests =

    let private firstItemId : ItemId = ItemId (Guid.Parse "23900000-0000-4000-8000-000000000001")

    let private secondItemId : ItemId = ItemId (Guid.Parse "23900000-0000-4000-8000-000000000002")

    let private circularModel : ItemModelVersion = BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId

    let private circularChoice : ItemModelChoice = LibraryModelChoice circularModel

    let private cpl1Label : ControlCaption = ControlCaption "CPL1"

    let private cpl1Dial : ItemDial = DialMounted DialPositive

    let private readOnlyStatus : ControlCaption option = Some (ControlCaption "The kit is open read-only: nothing is saved.")

    let private kitValue (result : Result<'Value, KitError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private libraryValue (result : Result<'Value, LibraryError>) : 'Value =
        match result with
        | Ok value -> value
        | Error (UnknownEntryIdErr reason | NoEntriesForKindErr reason | LibraryUnavailableErr reason | PresetRefusedErr reason) ->
            failwith $"The library listing failed: {reason}"

    /// A provider answering the ids in turn; a mint past the last fails the fact.
    let private fixedItemIds (ids : ItemId list) : ItemIdProvider =
        let remaining = ref ids
        {
            mintItemId =
                fun () ->
                    match remaining.Value with
                    | next :: rest ->
                        remaining.Value <- rest
                        next
                    | [] -> failwith "The fact minted more item ids than it supplied."
        }

    /// Runs over a fresh Main scene in its scratch kit over the Built-in stores, minting the fixed ids in turn; the
    /// scratch kit is closed at the end.
    let private withScene (run : TableAndElementRotationModel -> unit) : unit =
        let scene = initMain () |> kitValue
        try run { scene with itemIds = fixedItemIds [ firstItemId; secondItemId ] }
        finally scene.kit.close () |> kitValue

    let private openForm (model : TableAndElementRotationModel) : AddItemForm =
        match model.addItemFormOpt with
        | Some form -> form
        | None -> failwith "The add form must be open."

    /// The open add form filled with the CPL1 circular polarizer on a positive dial.
    let private fillForm (opened : TableAndElementRotationModel) : TableAndElementRotationModel =
        opened
        |> updateTableAndElementRotation (ChooseNewItemModel circularChoice)
        |> updateTableAndElementRotation (SetNewItemLabel cpl1Label)
        |> updateTableAndElementRotation (SetNewItemDial cpl1Dial)

    /// The scene with the add form opened under the next minted id and filled by `fillForm`.
    let private filledForm (scene : TableAndElementRotationModel) : TableAndElementRotationModel =
        scene |> updateTableAndElementRotation OpenAddItemForm |> fillForm

    let private cpl1Draft : ItemDraft =
        {
            id = firstItemId
            label = ItemLabel "CPL1"
            model = circularChoice
            dial = cpl1Dial
        }

    let private circularEnvelope : LibraryEntryEnvelope =
        (BuiltinCatalogue.catalogueData ()).envelopes
        |> List.find (fun envelope -> envelope.content.id = ItemModelIds.circularPolarizerModelId.toLibraryEntryId)

    [<Fact>]
    let ``Add item from library model lists the item models, the presets and the samples, and mints the item's id`` () : unit =
        withScene (fun scene ->
            Assert.Equal<AddItemForm option>(None, scene.addItemFormOpt)
            let opened = scene |> updateTableAndElementRotation OpenAddItemForm
            let form = openForm opened
            Assert.Equal<ItemId>(firstItemId, form.itemId)
            Assert.Equal<ItemModelChoice option>(None, form.chosenOpt)
            Assert.Equal<ControlCaption>(ControlCaption "", form.labelText)
            Assert.Equal<ItemDial>(FixedMounting, form.dial)
            Assert.Equal<ItemDraft option>(None, form.itemDraftOpt)
            Assert.Equal<ControlCaption option>(None, opened.kitStatus)
            Assert.Equal<TableAndElementRotationModel>({ scene with addItemFormOpt = opened.addItemFormOpt }, opened)

            // The binding models are the shipped entries of step 210, never made here.
            let sourceModel = BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId
            let polarizerModel = BuiltinCatalogue.itemModel ItemModelIds.idealPolarizerItemModelId
            let sampleModel = BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId
            let standalone =
                BuiltinCatalogue.itemModels ()
                |> List.filter (fun held ->
                    match held.entry.model.bindingKindOpt with
                    | Some _ -> false
                    | None -> true)
                |> List.sortBy (fun held -> held.entry.name)
            Assert.Equal(8, standalone.Length)

            let sourcePresets =
                scene.library.entriesForKind CatalogueKind.LightSource
                |> libraryValue
                |> List.choose (fun entry ->
                    match entry with
                    | SourceItem source -> Some source
                    | SampleItem _ | DetectorItem _ | PolarizerItem _ -> None)
            let linearListing = scene.library.entriesForKind CatalogueKind.LinearPolarizer |> libraryValue
            let circularListing = scene.library.entriesForKind CatalogueKind.CircularPolarizer |> libraryValue
            let polarizerPresets =
                linearListing @ circularListing
                |> List.distinctBy (fun entry -> entry.entryId)
                |> List.choose (fun entry ->
                    match entry with
                    | PolarizerItem polarizer -> Some polarizer
                    | SampleItem _ | SourceItem _ | DetectorItem _ -> None)
            let samples =
                scene.library.entriesForKind CatalogueKind.Sample
                |> libraryValue
                |> List.choose (fun entry ->
                    match entry with
                    | SampleItem sample -> Some sample
                    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> None)

            // Every Built-in source, polarizer and sample is offered, so the listings above are not empty.
            let builtInPolarizers =
                BuiltinCatalogue.presets ()
                |> List.choose (fun entry ->
                    match entry with
                    | PolarizerItem polarizer -> Some polarizer.id
                    | SampleItem _ | SourceItem _ | DetectorItem _ -> None)
            Assert.Equal<Set<ElementValueId>>(BuiltinCatalogue.sources () |> List.map _.id |> Set.ofList, sourcePresets |> List.map _.id |> Set.ofList)
            Assert.Equal<Set<ElementValueId>>(Set.ofList builtInPolarizers, polarizerPresets |> List.map _.id |> Set.ofList)
            Assert.Equal<Set<SampleId>>(BuiltinCatalogue.samples () |> List.map _.id |> Set.ofList, samples |> List.map _.id |> Set.ofList)
            Assert.Equal(List.length polarizerPresets, polarizerPresets |> List.distinctBy _.id |> List.length)

            let expected : ItemModelChoice list =
                (standalone |> List.map LibraryModelChoice)
                @ (sourcePresets |> List.map (fun source -> SourcePresetChoice (sourceModel, source)))
                @ (polarizerPresets |> List.map (fun polarizer -> PolarizerPresetChoice (polarizerModel, polarizer)))
                @ (samples |> List.map (fun sample -> StackSampleChoice (sampleModel, SampleVersionId.firstOf sample.id, sample)))
            Assert.Equal<ItemModelChoice list>(expected, form.offers)

            for offer in form.offers do
                match offer with
                | StackSampleChoice (_, version, sample) ->
                    Assert.Equal<SampleId>(sample.id, version.sampleId)
                    Assert.Equal<VersionNumber>(VersionNumber.first, version.version)
                | SourcePresetChoice _ | PolarizerPresetChoice _ | LibraryModelChoice _ -> ()

            // Each opening mints the next id.
            let reopened = opened |> updateTableAndElementRotation OpenAddItemForm
            Assert.Equal<ItemId>(secondItemId, (openForm reopened).itemId)
            Assert.Equal<ItemModelChoice list>(expected, (openForm reopened).offers)

            // A compound polarizer serves both kinds, so both listings name it; it is offered once.
            let compound : PolarizerPreset =
                {
                    List.head polarizerPresets with
                        id = ElementValueId "23900000-0000-4000-8000-0000000000c1"
                        name = EntryName "Compound polarizer (LP then CP)"
                        category = LpCpCategory
                }
            let bothKinds : LibraryProxy =
                {
                    scene.library with
                        entriesForKind =
                            fun kind ->
                                scene.library.entriesForKind kind
                                |> Result.map (fun entries ->
                                    match kind with
                                    | CatalogueKind.LinearPolarizer
                                    | CatalogueKind.CircularPolarizer -> entries @ [ PolarizerItem compound ]
                                    | CatalogueKind.LightSource
                                    | CatalogueKind.Sample
                                    | CatalogueKind.Lens
                                    | CatalogueKind.FlatMirror
                                    | CatalogueKind.CurvedMirror
                                    | CatalogueKind.Detector -> entries)
                }
            let doubled =
                { scene with library = bothKinds; itemIds = fixedItemIds [ firstItemId ] }
                |> updateTableAndElementRotation OpenAddItemForm
            let compoundOffers =
                (openForm doubled).offers
                |> List.filter (fun offer ->
                    match offer with
                    | PolarizerPresetChoice (_, polarizer) -> polarizer.id = compound.id
                    | SourcePresetChoice _ | StackSampleChoice _ | LibraryModelChoice _ -> false)
            Assert.Equal<ItemModelChoice list>([ PolarizerPresetChoice (polarizerModel, compound) ], compoundOffers))

    [<Fact>]
    let ``the add form takes a model, a label and a dial, and Cancel closes it writing nothing`` () : unit =
        withScene (fun scene ->
            // Without a form, the form's messages change nothing.
            for message in [ ChooseNewItemModel circularChoice; SetNewItemLabel cpl1Label; SetNewItemDial cpl1Dial; CancelAddItemForm ] do
                Assert.Equal<TableAndElementRotationModel>(scene, scene |> updateTableAndElementRotation message)

            let opened = scene |> updateTableAndElementRotation OpenAddItemForm
            Assert.Contains(circularChoice, (openForm opened).offers)
            let filled = fillForm opened
            let form = openForm filled
            Assert.Equal<ItemModelChoice option>(Some circularChoice, form.chosenOpt)
            Assert.Equal<ControlCaption>(cpl1Label, form.labelText)
            Assert.Equal<ItemDial>(cpl1Dial, form.dial)
            Assert.Equal<ItemDraft option>(Some cpl1Draft, form.itemDraftOpt)
            Assert.Equal<TableAndElementRotationModel>({ opened with addItemFormOpt = filled.addItemFormOpt }, filled)

            let blank = filled |> updateTableAndElementRotation (SetNewItemLabel (ControlCaption "  "))
            Assert.Equal<ItemDraft option>(None, (openForm blank).itemDraftOpt)
            let unchosen = { filled with addItemFormOpt = Some { form with chosenOpt = None } }
            Assert.Equal<ItemDraft option>(None, (openForm unchosen).itemDraftOpt)

            let cancelled = filled |> updateTableAndElementRotation CancelAddItemForm
            Assert.Equal<AddItemForm option>(None, cancelled.addItemFormOpt)
            Assert.Equal<TableAndElementRotationModel>({ filled with addItemFormOpt = None }, cancelled)
            Assert.Empty(cancelled.kitItems)
            Assert.Equal<Item list>([], cancelled.kit.proxy.readItems () |> kitValue)
            Assert.Empty(cancelled.kit.proxy.listSnapshots () |> kitValue))

    [<Fact>]
    let ``AddItem records the item in the kit with its model's snapshot and selects it`` () : unit =
        withScene (fun scene ->
            let filled = filledForm scene
            let draft =
                match (openForm filled).itemDraftOpt with
                | Some draft -> draft
                | None -> failwith "The filled add form must hold a draft."
            let added = filled |> updateTableAndElementRotation (AddItem draft)
            let cpl1Item = cpl1Draft.toItem
            Assert.Equal<Item list>([ cpl1Item ], added.kitItems)
            Assert.Equal<ItemId option>(Some firstItemId, added.selectedItemOpt)
            Assert.Equal<AddItemForm option>(None, added.addItemFormOpt)
            Assert.Equal<ControlCaption option>(None, added.kitStatus)
            Assert.Equal<Item list>([ cpl1Item ], added.kit.proxy.readItems () |> kitValue)
            Assert.Equal<LibraryEntryEnvelope list>([ circularEnvelope ], added.kit.proxy.listSnapshots () |> kitValue)
            Assert.Equal<VersionNumber>(VersionNumber.first, circularEnvelope.content.version)

            // A later item sorts by its label, ordinal, and becomes the selected one.
            let laterDraft : ItemDraft =
                {
                    id = secondItemId
                    label = ItemLabel "A1"
                    model = circularChoice
                    dial = FixedMounting
                }
            let both = added |> updateTableAndElementRotation (AddItem laterDraft)
            Assert.Equal<Item list>([ laterDraft.toItem; cpl1Item ], both.kitItems)
            Assert.Equal<ItemId option>(Some secondItemId, both.selectedItemOpt)
            Assert.Equal<Item list>([ laterDraft.toItem; cpl1Item ], both.kit.proxy.readItems () |> kitValue)

            // A press of an item row selects it; an id the kit does not hold selects nothing.
            let pressed = both |> updateTableAndElementRotation (SelectKitItem firstItemId)
            Assert.Equal<TableAndElementRotationModel>({ both with selectedItemOpt = Some firstItemId }, pressed)
            let stranger = ItemId (Guid.Parse "23900000-0000-4000-8000-0000000000ff")
            Assert.Equal<TableAndElementRotationModel>(pressed, pressed |> updateTableAndElementRotation (SelectKitItem stranger)))

    [<Fact>]
    let ``AddItem of a label the kit holds is told and records nothing`` () : unit =
        withScene (fun scene ->
            let added = scene |> updateTableAndElementRotation (AddItem cpl1Draft)
            Assert.Equal<Item list>([ cpl1Draft.toItem ], added.kitItems)
            let clash : ItemDraft =
                {
                    cpl1Draft with
                        id = secondItemId
                        label = ItemLabel "cpl1"
                }
            let refused = added |> updateTableAndElementRotation (AddItem clash)
            match refused.kitStatus with
            | Some status ->
                Assert.StartsWith("The item was not added — ", status.value)
                Assert.Contains("already has an item of that label", status.value)
            | None -> Assert.Fail("A refused AddItem must tell why.")
            Assert.Equal<TableAndElementRotationModel>({ added with kitStatus = refused.kitStatus }, refused)
            Assert.Equal<Item list>(added.kitItems, refused.kitItems)
            Assert.Equal<Item list>([ cpl1Draft.toItem ], refused.kit.proxy.readItems () |> kitValue))

    [<Fact>]
    let ``a kit opened read-only adds no item`` () : unit =
        withScene (fun scene ->
            let readOnly =
                {
                    scene with
                        kit =
                            {
                                scene.kit with
                                    proxy = scene.kit.proxy.readOnlyView
                                    access = ReadOnlyKitAccess
                            }
                }
            let filled = filledForm readOnly
            let refused = filled |> updateTableAndElementRotation (AddItem cpl1Draft)
            Assert.Equal<ControlCaption option>(readOnlyStatus, refused.kitStatus)
            Assert.Equal<TableAndElementRotationModel>({ filled with kitStatus = readOnlyStatus }, refused)
            Assert.Empty(refused.kitItems)
            Assert.Equal<Item list>([], refused.kit.proxy.readItems () |> kitValue)
            Assert.Equal<Item list>([], scene.kit.proxy.readItems () |> kitValue)
            Assert.Empty(scene.kit.proxy.listSnapshots () |> kitValue))

    // Step 0240: the parameter editor and the label, notes and photo edits (spec 0050 S7.2).

    let private retardance : ParameterName = ParameterName "c2.retardance"

    let private lp1Draft : ItemDraft =
        {
            id = secondItemId
            label = ItemLabel "LP1"
            model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.realLinearPolarizerModelId)
            dial = FixedMounting
        }

    let private strangerId : ItemId = ItemId (Guid.Parse "24000000-0000-4000-8000-0000000000ff")

    /// Unknown, starting at π / 2 within [π / 4, 3π / 4], without a prior.
    let private retardanceUnknown : ParameterState =
        UnknownParameter
            {
                start = ParameterValue (Math.PI / 2.0)
                bounds =
                    {
                        lower = ParameterValue (Math.PI / 4.0)
                        upper = ParameterValue (3.0 * Math.PI / 4.0)
                    }
                priorOpt = None
            }

    let private photoBytes : FileByteContent = FileByteContent (Encoding.UTF8.GetBytes "CPL1 on its mount")

    let private itemValue (result : Result<Item, ItemError>) : Item =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    /// Runs over `withScene`'s scene with its kit over a folder the fact holds, so the fact can list the kit's paths;
    /// the kit keeps the scratch kit's file, access and clock, and the scene attaches files through it.
    let private withKitScene (run : InMemoryKitFolder -> TableAndElementRotationModel -> unit) : unit =
        withScene (fun scene ->
            let folder = InMemoryKitFolder.create ()
            let proxy = KitProxy.createInMemory folder
            proxy.initialiseKit scene.kit.kitFile |> kitValue
            let kit : KitContext = { scene.kit with proxy = proxy }
            run folder { scene with kit = kit; experimentData = ExperimentDataStore.createFileBacked proxy })

    /// Writes `CPL1.jpg` into a new folder under the test run's output, outside every kit, and deletes that folder at
    /// the end.
    let private withPhotoFile (run : FileName -> unit) : unit =
        let folder = Path.Combine(System.AppContext.BaseDirectory, "ItemsPanelModelTests", Guid.NewGuid().ToString())
        Directory.CreateDirectory folder |> ignore
        let photo = FileName (Path.Combine(folder, "CPL1.jpg"))
        File.WriteAllBytes(photo.value, photoBytes.value)
        try run photo
        finally Directory.Delete(folder, true)

    let private openDraft (model : TableAndElementRotationModel) : ParameterEditDraft =
        match model.parameterEditOpt with
        | Some draft -> draft
        | None -> failwith "The parameter editor must be open."

    /// The kit's item files, as its folder lists them.
    let private itemFilesOf (folder : InMemoryKitFolder) : KitRelativePath list =
        folder.paths ()
        |> List.filter (fun path -> path.value.EndsWith(ItemKitFile.extension.value, StringComparison.Ordinal))

    let private statusOf (model : TableAndElementRotationModel) : string =
        match model.kitStatus with
        | Some status -> status.value
        | None -> failwith "A refusal must tell why."

    [<Fact>]
    let ``SetParameterState sets an item's parameter and records it`` () : unit =
        withScene (fun scene ->
            let added = scene |> updateTableAndElementRotation (AddItem cpl1Draft)
            let editing = added |> updateTableAndElementRotation (EditItemParameter retardance)
            Assert.Equal<ParameterName>(retardance, (openDraft editing).name)
            let set = editing |> updateTableAndElementRotation (SetParameterState (firstItemId, retardance, retardanceUnknown))
            let expected = cpl1Draft.toItem.withParameterState retardance retardanceUnknown |> itemValue
            Assert.Equal<Item list>([ expected ], set.kitItems)
            Assert.Equal<Item list>([ expected ], set.kit.proxy.readItems () |> kitValue)
            let recorded = List.exactlyOne (set.kit.proxy.readItems () |> kitValue)
            match recorded.tryFindParameter retardance with
            | Some parameter -> Assert.Equal<ParameterState>(retardanceUnknown, parameter.state)
            | None -> Assert.Fail("CPL1 must hold c2.retardance.")
            Assert.Equal<ItemRevision>(ItemRevision.first, recorded.revision)
            Assert.Empty(recorded.history)
            // The other parameters keep their known defaults.
            Assert.Equal<Parameter list>(
                cpl1Draft.toItem.parameters |> List.filter (fun parameter -> parameter.name <> retardance),
                recorded.parameters |> List.filter (fun parameter -> parameter.name <> retardance))
            // The editor closes once the state is recorded; nothing else changes.
            Assert.Equal<ControlCaption option>(None, set.kitStatus)
            Assert.Equal<TableAndElementRotationModel>({ editing with kitItems = set.kitItems; parameterEditOpt = None }, set))

    [<Fact>]
    let ``a parameter state that breaks its rules is told and nothing is recorded`` () : unit =
        withScene (fun scene ->
            let added = scene |> updateTableAndElementRotation (AddItem cpl1Draft)
            let editing = added |> updateTableAndElementRotation (EditItemParameter retardance)
            let reversed =
                UnknownParameter
                    {
                        start = ParameterValue (Math.PI / 2.0)
                        bounds =
                            {
                                lower = ParameterValue (3.0 * Math.PI / 4.0)
                                upper = ParameterValue (Math.PI / 4.0)
                            }
                        priorOpt = None
                    }
            let refused = editing |> updateTableAndElementRotation (SetParameterState (firstItemId, retardance, reversed))
            Assert.StartsWith("The item was not changed — ", statusOf refused)
            Assert.Contains("the parameter state is not valid", statusOf refused)
            Assert.Contains("c2.retardance", statusOf refused)
            // The editor stays open on the refused state, and nothing but the status changes.
            Assert.Equal<TableAndElementRotationModel>({ editing with kitStatus = refused.kitStatus }, refused)
            Assert.Equal<Item list>([ cpl1Draft.toItem ], refused.kitItems)
            Assert.Equal<Item list>([ cpl1Draft.toItem ], refused.kit.proxy.readItems () |> kitValue)

            // A parameter the item lacks is told; an id the kit does not hold changes nothing.
            let unknownName = added |> updateTableAndElementRotation (SetParameterState (firstItemId, ParameterName "c9.retardance", retardanceUnknown))
            Assert.StartsWith("The item was not changed — the item has no such parameter", statusOf unknownName)
            Assert.Equal<Item list>([ cpl1Draft.toItem ], unknownName.kit.proxy.readItems () |> kitValue)
            Assert.Equal<TableAndElementRotationModel>(added, added |> updateTableAndElementRotation (SetParameterState (strangerId, retardance, retardanceUnknown))))

    [<Fact>]
    let ``the parameter editor starts from the parameter's state and reads the state it sets`` () : unit =
        withScene (fun scene ->
            let added = scene |> updateTableAndElementRotation (AddItem cpl1Draft)

            // Without a selected item, a parameter the item lacks or an open editor, the editor's messages change nothing.
            Assert.Equal<TableAndElementRotationModel>(scene, scene |> updateTableAndElementRotation (EditItemParameter retardance))
            Assert.Equal<TableAndElementRotationModel>(added, added |> updateTableAndElementRotation (EditItemParameter (ParameterName "c9.retardance")))
            for message in [ SetParameterEditChoice UnknownStateChoice; SetParameterEditText (LowerEditField, ControlCaption "0.7"); CancelParameterEdit ] do
                Assert.Equal<TableAndElementRotationModel>(added, added |> updateTableAndElementRotation message)

            let opened = added |> updateTableAndElementRotation (EditItemParameter (ParameterName "c2.retardance"))
            let draft = openDraft opened
            Assert.Equal<TableAndElementRotationModel>({ added with parameterEditOpt = opened.parameterEditOpt }, opened)
            Assert.Equal<ItemId>(firstItemId, draft.itemId)
            Assert.Equal<ParameterName>(retardance, draft.name)
            Assert.Equal<ParameterStateChoice>(KnownStateChoice, draft.choice)
            Assert.Equal<double>(Math.PI / 2.0, Double.Parse(draft.valueText.value, NumberStyles.Float, CultureInfo.InvariantCulture))
            for empty in [ draft.lowerText; draft.upperText; draft.priorSigmaText ] do
                Assert.Equal<ControlCaption>(ControlCaption "", empty)
            Assert.Equal<Result<ParameterState, ItemError>>(Ok (KnownParameter (ParameterValue (Math.PI / 2.0))), draft.tryState)

            let typed =
                opened
                |> updateTableAndElementRotation (SetParameterEditChoice UnknownStateChoice)
                |> updateTableAndElementRotation (SetParameterEditText (LowerEditField, ControlCaption "0.7"))
                |> updateTableAndElementRotation (SetParameterEditText (UpperEditField, ControlCaption "2.4"))
                |> updateTableAndElementRotation (SetParameterEditText (PriorSigmaEditField, ControlCaption "0.1"))
            let unknownDraft = openDraft typed
            Assert.Equal<ParameterStateChoice>(UnknownStateChoice, unknownDraft.choice)
            let expected =
                UnknownParameter
                    {
                        start = ParameterValue (Math.PI / 2.0)
                        bounds =
                            {
                                lower = ParameterValue 0.7
                                upper = ParameterValue 2.4
                            }
                        priorOpt =
                            Some
                                {
                                    centre = ParameterValue (Math.PI / 2.0)
                                    sigma = ParameterSigma 0.1
                                }
                    }
            Assert.Equal<Result<ParameterState, ItemError>>(Ok expected, unknownDraft.tryState)
            // Typing writes nothing.
            Assert.Equal<TableAndElementRotationModel>({ added with parameterEditOpt = typed.parameterEditOpt }, typed)
            Assert.Equal<Item list>([ cpl1Draft.toItem ], typed.kit.proxy.readItems () |> kitValue)

            match (unknownDraft.withFieldText LowerEditField (ControlCaption "abc")).tryState with
            | Error (InvalidParameterStateErr reason) ->
                Assert.Contains("c2.retardance", reason)
                Assert.Contains("lower", reason)
                Assert.Contains("abc", reason)
            | other -> Assert.Fail($"The lower text abc must be InvalidParameterStateErr, not %A{other}.")
            match (unknownDraft.withFieldText PriorSigmaEditField (ControlCaption "x")).tryState with
            | Error (InvalidParameterStateErr reason) -> Assert.Contains("prior sigma", reason)
            | other -> Assert.Fail($"The sigma text x must be InvalidParameterStateErr, not %A{other}.")

            let noPrior =
                UnknownParameter
                    {
                        start = ParameterValue (Math.PI / 2.0)
                        bounds =
                            {
                                lower = ParameterValue 0.7
                                upper = ParameterValue 2.4
                            }
                        priorOpt = None
                    }
            Assert.Equal<Result<ParameterState, ItemError>>(Ok noPrior, (unknownDraft.withFieldText PriorSigmaEditField (ControlCaption "")).tryState)

            // An unknown opens with its start, bounds and sigma, and reads back as itself.
            let reopened = ParameterEditDraft.ofParameter firstItemId { name = retardance; state = expected }
            Assert.Equal<ParameterEditDraft>(unknownDraft, reopened)
            Assert.Equal<Result<ParameterState, ItemError>>(Ok expected, reopened.tryState)
            Assert.Equal<ControlCaption>(ControlCaption "", (ParameterEditDraft.ofParameter firstItemId { name = retardance; state = noPrior }).priorSigmaText)

            let calibration : Parameter =
                {
                    name = retardance
                    state =
                        CalibratedParameter
                            {
                                value = ParameterValue 1.55
                                sigma = ParameterSigma 0.01
                                fitRun = FitRunName "CPL1 study@20261109T120000Z"
                            }
                }
            let calibrated = ParameterEditDraft.ofParameter firstItemId calibration
            Assert.Equal<ParameterStateChoice>(KnownStateChoice, calibrated.choice)
            Assert.Equal<ControlCaption>(ControlCaption "1.55", calibrated.valueText)
            Assert.Equal<Result<ParameterState, ItemError>>(Ok (KnownParameter (ParameterValue 1.55)), calibrated.tryState)

            let cancelled = typed |> updateTableAndElementRotation CancelParameterEdit
            Assert.Equal<ParameterEditDraft option>(None, cancelled.parameterEditOpt)
            Assert.Equal<TableAndElementRotationModel>({ typed with parameterEditOpt = None }, cancelled)
            // Pressing an item row closes the editor too.
            Assert.Equal<ParameterEditDraft option>(None, (typed |> updateTableAndElementRotation (SelectKitItem firstItemId)).parameterEditOpt))

    [<Fact>]
    let ``relabelling an item renames its kit file, and a label another item holds is told`` () : unit =
        withKitScene (fun folder scene ->
            let added = scene |> updateTableAndElementRotation (AddItem cpl1Draft)
            Assert.Equal<KitRelativePath list>([ KitRelativePath "CPL1.item-json" ], itemFilesOf folder)

            let relabelled = added |> updateTableAndElementRotation (RelabelKitItem (firstItemId, ItemLabel "CPL 1"))
            let cpl1 = { cpl1Draft.toItem with label = ItemLabel "CPL 1" }
            Assert.Equal<ControlCaption option>(None, relabelled.kitStatus)
            Assert.Equal<Item list>([ cpl1 ], relabelled.kitItems)
            Assert.Equal<Item list>([ cpl1 ], relabelled.kit.proxy.readItems () |> kitValue)
            Assert.Equal<KitRelativePath list>([ KitRelativePath "CPL 1.item-json" ], itemFilesOf folder)
            Assert.Equal<TableAndElementRotationModel>({ added with kitItems = relabelled.kitItems }, relabelled)

            let withLp1 = relabelled |> updateTableAndElementRotation (AddItem lp1Draft)
            Assert.Equal<ControlCaption option>(None, withLp1.kitStatus)
            let files = itemFilesOf folder
            Assert.Equal<KitRelativePath list>([ KitRelativePath "CPL 1.item-json"; KitRelativePath "LP1.item-json" ], files)

            let clash = withLp1 |> updateTableAndElementRotation (RelabelKitItem (firstItemId, ItemLabel "lp1"))
            Assert.StartsWith("The item was not changed — ", statusOf clash)
            Assert.Contains("already has an item of that label", statusOf clash)
            Assert.Equal<TableAndElementRotationModel>({ withLp1 with kitStatus = clash.kitStatus }, clash)
            Assert.Equal<Item list>(withLp1.kitItems, clash.kit.proxy.readItems () |> kitValue)
            Assert.Equal<KitRelativePath list>(files, itemFilesOf folder)

            // A change of case alone renames the one file; an id the kit does not hold changes nothing.
            let recased = withLp1 |> updateTableAndElementRotation (RelabelKitItem (firstItemId, ItemLabel "cpl 1"))
            Assert.Equal<ControlCaption option>(None, recased.kitStatus)
            Assert.Equal<KitRelativePath list>([ KitRelativePath "LP1.item-json"; KitRelativePath "cpl 1.item-json" ], itemFilesOf folder)
            Assert.Equal<Item list>(recased.kitItems, recased.kit.proxy.readItems () |> kitValue)
            Assert.Equal<TableAndElementRotationModel>(recased, recased |> updateTableAndElementRotation (RelabelKitItem (strangerId, ItemLabel "QZ"))))

    [<Fact>]
    let ``notes and a photo are recorded with the item, and removing the photo keeps its file`` () : unit =
        withKitScene (fun folder scene ->
            let added = scene |> updateTableAndElementRotation (AddItem cpl1Draft)
            let noted = added |> updateTableAndElementRotation (SetKitItemNotes (firstItemId, ItemNotes "Mounted"))
            let notedItem = cpl1Draft.toItem.withNotes (ItemNotes "Mounted")
            Assert.Equal<ControlCaption option>(None, noted.kitStatus)
            Assert.Equal<Item list>([ notedItem ], noted.kitItems)
            Assert.Equal<Item list>([ notedItem ], noted.kit.proxy.readItems () |> kitValue)
            Assert.Equal<ItemNotes>(ItemNotes "Mounted", (List.exactlyOne (noted.kit.proxy.readItems () |> kitValue)).notes)

            withPhotoFile (fun photo ->
                let photoPath = KitRelativePath "data/CPL1.jpg"
                let photographed = noted |> updateTableAndElementRotation (SetKitItemPhoto (firstItemId, photo))
                let photographedItem = notedItem.withPhoto (Some photoPath)
                Assert.Equal<ControlCaption option>(None, photographed.kitStatus)
                Assert.Equal<Item list>([ photographedItem ], photographed.kitItems)
                Assert.Equal<Item list>([ photographedItem ], photographed.kit.proxy.readItems () |> kitValue)
                Assert.Contains(photoPath, folder.paths ())
                let copied = photographed.kit.proxy.readDataFile photoPath |> kitValue
                Assert.Equal<ContentHash>(ContentHash.ofBytes photoBytes, copied.contentHash)
                Assert.True(File.Exists photo.value, "The picked photo is copied, never moved.")

                let paths = folder.paths ()
                let removed = photographed |> updateTableAndElementRotation (RemoveKitItemPhoto firstItemId)
                Assert.Equal<ControlCaption option>(None, removed.kitStatus)
                Assert.Equal<Item list>([ notedItem ], removed.kitItems)
                Assert.Equal<Item list>([ notedItem ], removed.kit.proxy.readItems () |> kitValue)
                Assert.Contains(photoPath, folder.paths ())
                Assert.Equal<KitRelativePath list>(paths, folder.paths ())

                // A photo that cannot be attached is told, and nothing is written.
                let missing = FileName (Path.Combine(Path.GetDirectoryName photo.value, "missing.jpg"))
                let refused = removed |> updateTableAndElementRotation (SetKitItemPhoto (firstItemId, missing))
                Assert.StartsWith("The photo was not attached — ", statusOf refused)
                Assert.Equal<TableAndElementRotationModel>({ removed with kitStatus = refused.kitStatus }, refused)
                Assert.Equal<KitRelativePath list>(paths, folder.paths ())
                Assert.Equal<Item list>([ notedItem ], refused.kit.proxy.readItems () |> kitValue)))

    [<Fact>]
    let ``a kit opened read-only edits no item and writes nothing`` () : unit =
        withKitScene (fun folder scene ->
            scene |> updateTableAndElementRotation (AddItem cpl1Draft) |> ignore
            let readOnlyProxy = scene.kit.proxy.readOnlyView
            let readOnly =
                {
                    scene with
                        kit =
                            {
                                scene.kit with
                                    proxy = readOnlyProxy
                                    access = ReadOnlyKitAccess
                            }
                        experimentData = ExperimentDataStore.createFileBacked readOnlyProxy
                        kitItems = readOnlyProxy.readItems () |> kitValue
                }
            Assert.Equal<Item list>([ cpl1Draft.toItem ], readOnly.kitItems)
            let paths = folder.paths ()

            withPhotoFile (fun photo ->
                // The Items panel's six writes: step 239's AddItem and this step's five item edits.
                let writes =
                    [
                        AddItem lp1Draft
                        SetParameterState (firstItemId, retardance, retardanceUnknown)
                        RelabelKitItem (firstItemId, ItemLabel "CPL 1")
                        SetKitItemNotes (firstItemId, ItemNotes "Mounted")
                        SetKitItemPhoto (firstItemId, photo)
                        RemoveKitItemPhoto firstItemId
                    ]
                for message in writes do
                    let refused = readOnly |> updateTableAndElementRotation message
                    Assert.Equal<ControlCaption option>(readOnlyStatus, refused.kitStatus)
                    Assert.Equal<TableAndElementRotationModel>({ readOnly with kitStatus = readOnlyStatus }, refused)
                    Assert.Equal<KitRelativePath list>(paths, folder.paths ())
                Assert.Equal<Item list>([ cpl1Draft.toItem ], readOnlyProxy.readItems () |> kitValue)))
