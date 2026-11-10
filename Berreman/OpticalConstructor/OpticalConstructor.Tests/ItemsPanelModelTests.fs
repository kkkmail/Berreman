namespace OpticalConstructor.Tests

open System
open Xunit
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Storage.KitContentFiles
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
