namespace OpticalConstructor.Ui.Tests

open System
open System.IO
open System.Text
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.FuncUI.Types
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Media
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Storage
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Ui.ItemsPanelView
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open global.Elmish

/// The Items panel's view over the constructor window's model, driven by its messages (spec 0050 S7.2).
module ItemsPanelTests =

    let private cpl1Id : ItemId = ItemId (Guid.Parse "24100000-0000-4000-8000-000000000001")

    let private lp1Id : ItemId = ItemId (Guid.Parse "24100000-0000-4000-8000-000000000002")

    let private circularModel : ItemModelVersion = BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId

    let private circularChoice : ItemModelChoice = LibraryModelChoice circularModel

    let private circularCaption : string = "Circular polarizer (linear polarizer and retarder) v1"

    let private retardance : ParameterName = ParameterName "c2.retardance"

    let private fastAxisOffset : ParameterName = ParameterName "c2.fastAxisOffset"

    let private cpl1Draft : ItemDraft =
        {
            id = cpl1Id
            label = ItemLabel "CPL1"
            model = circularChoice
            dial = DialMounted DialPositive
        }

    let private lp1Draft : ItemDraft =
        {
            id = lp1Id
            label = ItemLabel "LP1"
            model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.realLinearPolarizerModelId)
            dial = FixedMounting
        }

    let private update (msg : TableAndElementRotationMsg) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        updateTableAndElementRotation msg model

    let private itemValue (result : Result<Item, ItemError>) : Item =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    /// Runs over a fresh Main scene in its scratch kit, minting CPL1's id for the add form; the kit is closed at the end.
    let private withScene (run : TableAndElementRotationModel -> unit) : unit =
        let scene = initMain () |> SceneTestFixture.kitSuccess
        try run { scene with itemIds = { mintItemId = fun () -> cpl1Id } }
        finally scene.kit.close () |> SceneTestFixture.kitSuccess

    /// Shows `itemsPanelView` over `model` headless at 1000 × 700, as `ScrollExtentTests.constructorWindow` mounts a
    /// view, runs `check` over the window, then closes it.
    let private withItemsPanel (model : TableAndElementRotationModel) (check : Window -> unit) : unit =
        let window = HostWindow()
        Program.mkSimple (fun () -> model) updateTableAndElementRotation itemsPanelView |> Program.withHost window |> Program.run
        window.Width <- 1000.0
        window.Height <- 700.0
        try
            window.Show()
            Dispatcher.UIThread.RunJobs()
            check window
        finally
            window.Close()
            Dispatcher.UIThread.RunJobs()

    /// Every control under `root` carrying `id` as its AutomationId.
    let private controlsUnder (root : Visual) (id : ConstructorUiId) : Control list =
        root.GetVisualDescendants()
        |> Seq.choose (function :? Control as control -> Some control | _ -> None)
        |> Seq.filter (fun control -> AutomationProperties.GetAutomationId control = id.value)
        |> List.ofSeq

    let private tryControlById (window : Window) (id : ConstructorUiId) : Control option =
        controlsUnder window id |> List.tryHead

    let private controlById (window : Window) (id : ConstructorUiId) : Control =
        tryControlById window id |> Option.defaultWith (fun () -> failwith $"{id.value} must be in the window")

    let private assertAbsent (window : Window) (id : ConstructorUiId) : unit =
        match tryControlById window id with
        | Some _ -> Assert.Fail $"{id.value} must be absent"
        | None -> ()

    let private textOf (window : Window) (id : ConstructorUiId) : string =
        Assert.IsType<TextBlock>(controlById window id).Text

    /// The label of the verb or action box carrying `id`: the text of the box's child.
    let private boxLabelOf (window : Window) (id : ConstructorUiId) : string =
        Assert.IsType<TextBlock>(Assert.IsType<Border>(controlById window id).Child).Text

    /// The AutomationId a list child carries, or the one of the box it holds at its caption's width.
    let private listIdOf (child : Control) : string option =
        let carried (control : Control) : string option =
            match AutomationProperties.GetAutomationId control with
            | null | "" -> None
            | id -> Some id
        match carried child, child with
        | Some id, _ -> Some id
        | None, (:? Border as holder) ->
            match holder.Child with
            | null -> None
            | box -> carried box
        | None, _ -> None

    /// The ids of `panel`'s children that carry one, in order.
    let private childIds (panel : Panel) : string list =
        panel.Children |> Seq.choose listIdOf |> List.ofSeq

    /// The window position of the control's top-left corner.
    let private topLeftOf (window : Window) (control : Control) : Point =
        match control.TranslatePoint(Point(0.0, 0.0), window) |> Option.ofNullable with
        | Some point -> point
        | None -> failwith $"{AutomationProperties.GetAutomationId control} has no position in its window"

    /// The two boxes share a row, `right` beyond `left`'s right edge.
    let private assertOneRowRightOf (window : Window) (left : ConstructorUiId) (right : ConstructorUiId) : unit =
        let leftBox = controlById window left
        let rightBox = controlById window right
        let leftAt = topLeftOf window leftBox
        let rightAt = topLeftOf window rightBox
        Assert.Equal(leftAt.Y, rightAt.Y, 6)
        Assert.True(rightAt.X > leftAt.X + leftBox.Bounds.Width, $"{right.value} must sit right of {left.value}, past a gap")

    /// Presses the box carrying `id` at its centre, as `LibraryWindowTests.clickOn` does.
    let private pressBox (window : Window) (id : ConstructorUiId) : unit =
        let pressed = Assert.IsType<Border>(controlById window id)
        Assert.True(pressed.IsEffectivelyVisible, $"{id.value} must be visible")
        pressed.BringIntoView() |> ignore
        Dispatcher.UIThread.RunJobs()
        match pressed.TranslatePoint(Point(pressed.Bounds.Width / 2.0, pressed.Bounds.Height / 2.0), window) |> Option.ofNullable with
        | Some centre ->
            window.MouseDown(centre, MouseButton.Left, RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
            window.MouseUp(centre, MouseButton.Left, RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
        | None -> Assert.Fail $"{id.value} has no position in its window"

    /// The texts of the grid cells right of the parameter's name box, in column order.
    let private gridCellsOf (window : Window) (name : ParameterName) : string list =
        let grid = Assert.IsType<Grid>(controlById window ConstructorUiId.itemParameterGrid)
        let nameBox = controlById window (ConstructorUiId.itemParameterRow name)
        let row =
            match nameBox.Parent with
            | :? Control as cell -> Grid.GetRow cell
            | _ -> failwith $"{name.value}'s box must sit in a grid cell"
        grid.Children
        |> Seq.filter (fun cell -> Grid.GetRow cell = row && Grid.GetColumn cell > 0)
        |> Seq.sortBy Grid.GetColumn
        |> Seq.map (fun cell -> Assert.IsType<TextBlock>(cell).Text)
        |> List.ofSeq

    /// Writes `CPL1.jpg` into a new folder under the test run's output, outside every kit, and deletes it at the end.
    let private withPhotoFile (run : FileName -> unit) : unit =
        let folder = Path.Combine(System.AppContext.BaseDirectory, "ItemsPanelTests", Guid.NewGuid().ToString())
        Directory.CreateDirectory folder |> ignore
        let photo = FileName (Path.Combine(folder, "CPL1.jpg"))
        File.WriteAllBytes(photo.value, Encoding.UTF8.GetBytes "CPL1 on its mount")
        try run photo
        finally Directory.Delete(folder, true)

    [<Fact>]
    let ``the Items panel UiIds are the stable intent-named ids`` () : unit =
        let fixedIds : (ConstructorUiId * string) list =
            [
                ConstructorUiId.itemsPanel, "ItemsPanel"
                ConstructorUiId.kitItemList, "KitItemList"
                ConstructorUiId.addItemButton, "AddItemButton"
                ConstructorUiId.addItemOfferList, "AddItemOfferList"
                ConstructorUiId.newItemLabelField, "NewItemLabelField"
                ConstructorUiId.addItemConfirmButton, "AddItemConfirmButton"
                ConstructorUiId.addItemCancelButton, "AddItemCancelButton"
                ConstructorUiId.itemLabelField, "ItemLabelField"
                ConstructorUiId.itemModelText, "ItemModelText"
                ConstructorUiId.itemDialText, "ItemDialText"
                ConstructorUiId.itemNotesField, "ItemNotesField"
                ConstructorUiId.itemPhotoText, "ItemPhotoText"
                ConstructorUiId.itemChoosePhotoButton, "ItemChoosePhotoButton"
                ConstructorUiId.itemRemovePhotoButton, "ItemRemovePhotoButton"
                ConstructorUiId.itemParameterGrid, "ItemParameterGrid"
                ConstructorUiId.parameterEditPanel, "ParameterEditPanel"
                ConstructorUiId.parameterEditKnownChoice, "ParameterEditKnownChoice"
                ConstructorUiId.parameterEditUnknownChoice, "ParameterEditUnknownChoice"
                ConstructorUiId.parameterEditValueField, "ParameterEditValueField"
                ConstructorUiId.parameterEditLowerField, "ParameterEditLowerField"
                ConstructorUiId.parameterEditUpperField, "ParameterEditUpperField"
                ConstructorUiId.parameterEditPriorSigmaField, "ParameterEditPriorSigmaField"
                ConstructorUiId.parameterEditSetButton, "ParameterEditSetButton"
                ConstructorUiId.parameterEditCancelButton, "ParameterEditCancelButton"
                ConstructorUiId.itemRevisionList, "ItemRevisionList"
            ]
        for id, text in fixedIds do
            Assert.Equal(text, id.value)
        Assert.Equal(fixedIds.Length, fixedIds |> List.distinctBy (fun (id, _) -> id) |> List.length)

        let testItem = ItemId (Guid.Parse "24100000-0000-4000-8000-0000000000aa")
        Assert.Equal("KitItemRow_24100000-0000-4000-8000-0000000000aa", (ConstructorUiId.kitItemRow testItem).value)
        Assert.Equal("AddItemOffer_33486eec-836a-4cbf-ac79-e3f14439ad4b_v1", (ConstructorUiId.addItemOffer circularChoice).value)
        Assert.Equal("NewItemDial_Negative", (ConstructorUiId.newItemDialChoice (DialMounted DialNegative)).value)
        Assert.Equal("NewItemDial_Positive", (ConstructorUiId.newItemDialChoice (DialMounted DialPositive)).value)
        Assert.Equal("NewItemDial_Fixed", (ConstructorUiId.newItemDialChoice FixedMounting).value)
        Assert.Equal("ItemParameterRow_c2.retardance", (ConstructorUiId.itemParameterRow retardance).value)
        Assert.Equal("ItemRevisionRow_1", (ConstructorUiId.itemRevisionRow ItemRevision.first).value)

        // An offer's id and caption carry its model's GUID and version, and a bound one its binding, so two presets
        // or samples under one model stay distinct. The binding models are step 210's shipped entries.
        let sourceModel = BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId
        let polarizerModel = BuiltinCatalogue.itemModel ItemModelIds.idealPolarizerItemModelId
        let sampleModel = BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId
        let polarizers =
            BuiltinCatalogue.presets ()
            |> List.choose (fun entry ->
                match entry with
                | PolarizerItem polarizer -> Some polarizer
                | SampleItem _ | SourceItem _ | DetectorItem _ -> None)
        let sourceOffers = BuiltinCatalogue.sources () |> List.truncate 2 |> List.map (fun source -> SourcePresetChoice (sourceModel, source))
        let polarizerOffers = polarizers |> List.truncate 2 |> List.map (fun polarizer -> PolarizerPresetChoice (polarizerModel, polarizer))
        let sampleOffers =
            BuiltinCatalogue.samples ()
            |> List.truncate 2
            |> List.map (fun sample -> StackSampleChoice (sampleModel, SampleVersionId.firstOf sample.id, sample))
        for offers in [ sourceOffers; polarizerOffers; sampleOffers ] do
            Assert.Equal(2, offers.Length)
            Assert.NotEqual<ConstructorUiId>(ConstructorUiId.addItemOffer offers.[0], ConstructorUiId.addItemOffer offers.[1])
            Assert.NotEqual<ControlCaption>(offers.[0].caption, offers.[1].caption)
        match sourceOffers.[0] with
        | SourcePresetChoice (_, source) ->
            Assert.Equal($"AddItemOffer_239b0cfb-9938-5f3b-a767-c4252ea671b6_v1_{source.id.value}", (ConstructorUiId.addItemOffer sourceOffers.[0]).value)
            Assert.Equal(
                $"{source.name.value} ({source.id.value}) — Light source (preset binding) v1 (239b0cfb-9938-5f3b-a767-c4252ea671b6)",
                sourceOffers.[0].caption.value)
        | other -> Assert.Fail $"The first source offer must bind a source preset, not {(ConstructorUiId.addItemOffer other).value}."
        match polarizerOffers.[0] with
        | PolarizerPresetChoice (_, polarizer) ->
            Assert.Equal($"AddItemOffer_d47e5ce3-3e74-5058-848e-3aa54930e377_v1_{polarizer.id.value}", (ConstructorUiId.addItemOffer polarizerOffers.[0]).value)
            Assert.EndsWith("v1 (d47e5ce3-3e74-5058-848e-3aa54930e377)", polarizerOffers.[0].caption.value)
            Assert.StartsWith($"{polarizer.name.value} ({polarizer.id.value}) — ", polarizerOffers.[0].caption.value)
        | other -> Assert.Fail $"The first polarizer offer must bind a polarizer preset, not {(ConstructorUiId.addItemOffer other).value}."
        match sampleOffers.[0] with
        | StackSampleChoice (_, version, sample) ->
            Assert.Equal(
                $"AddItemOffer_4d13ecb1-2006-576f-99e7-e8bc266e8624_v1_{sample.id.value}_v1",
                (ConstructorUiId.addItemOffer sampleOffers.[0]).value)
            Assert.Equal(
                $"{sample.name.value} v1 ({version.sampleId.value}) — Sample by stack v1 (4d13ecb1-2006-576f-99e7-e8bc266e8624)",
                sampleOffers.[0].caption.value)
        | other -> Assert.Fail $"The first sample offer must bind a sample, not {(ConstructorUiId.addItemOffer other).value}."
        Assert.Equal($"{circularCaption} (33486eec-836a-4cbf-ac79-e3f14439ad4b)", circularChoice.caption.value)

    [<Fact>]
    let ``the Items panel lists the kit's items and shows the selected item's model, dial, notes, photo, parameters and revisions`` () : unit =
        HeadlessSession.run (fun () ->
            withScene (fun scene ->
                let added = scene |> update (AddItem cpl1Draft) |> update (AddItem lp1Draft)
                Assert.Equal<ControlCaption option>(None, added.kitStatus)
                let calibration : Parameter =
                    {
                        name = fastAxisOffset
                        state =
                            CalibratedParameter
                                {
                                    value = ParameterValue 0.01
                                    sigma = ParameterSigma 0.002
                                    fitRun = FitRunName "QZ-LR@20261101T120000Z"
                                }
                    }
                let published =
                    let cpl1 = added.kitItems |> List.find (fun item -> item.id = cpl1Id)
                    let revised = cpl1.publishRevision [ calibration ] |> itemValue
                    (revised.withNotes (ItemNotes "On the analyzer dial")).withPhoto (Some (KitRelativePath "data/CPL1.jpg"))
                let model =
                    {
                        added with kitItems = added.kitItems |> List.map (fun item -> if item.id = cpl1Id then published else item)
                    }
                    |> update (SelectKitItem cpl1Id)
                withItemsPanel model (fun window ->
                    let list = Assert.IsType<StackPanel>(controlById window ConstructorUiId.kitItemList)
                    Assert.Equal<string list>(
                        [ (ConstructorUiId.kitItemRow cpl1Id).value; (ConstructorUiId.kitItemRow lp1Id).value ],
                        childIds list)
                    Assert.Equal($"CPL1 — {circularCaption} — revision 2", boxLabelOf window (ConstructorUiId.kitItemRow cpl1Id))
                    Assert.Equal("LP1 — Real linear polarizer v1 — revision 1", boxLabelOf window (ConstructorUiId.kitItemRow lp1Id))
                    // The selected row is filled; the other is a verb.
                    let fillOf (id : ConstructorUiId) : Color =
                        Assert.IsAssignableFrom<ISolidColorBrush>(Assert.IsType<Border>(controlById window id).Background).Color
                    Assert.Equal(MaterialsWindowView.positiveBackground, fillOf (ConstructorUiId.kitItemRow cpl1Id))
                    Assert.NotEqual(MaterialsWindowView.positiveBackground, fillOf (ConstructorUiId.kitItemRow lp1Id))

                    let label = Assert.IsType<TextBox>(controlById window ConstructorUiId.itemLabelField)
                    Assert.Equal("CPL1", label.Text)
                    Assert.True(label.IsEnabled, "a kit that takes writes offers the label")
                    Assert.Equal($"Model: {circularCaption}", textOf window ConstructorUiId.itemModelText)
                    Assert.Equal("Dial: On a dial, sign +1", textOf window ConstructorUiId.itemDialText)
                    let notes = Assert.IsType<TextBox>(controlById window ConstructorUiId.itemNotesField)
                    Assert.Equal("On the analyzer dial", notes.Text)
                    Assert.True(notes.AcceptsReturn, "the notes take returns")
                    Assert.True(notes.IsEnabled, "a kit that takes writes offers the notes")
                    Assert.Equal("Photo: data/CPL1.jpg", textOf window ConstructorUiId.itemPhotoText)
                    Assert.Equal("Choose photo…", boxLabelOf window ConstructorUiId.itemChoosePhotoButton)
                    Assert.Equal("Remove photo", boxLabelOf window ConstructorUiId.itemRemovePhotoButton)
                    assertAbsent window ConstructorUiId.addItemConfirmButton

                    let grid = Assert.IsType<Grid>(controlById window ConstructorUiId.itemParameterGrid)
                    let parameterRows =
                        published.parameters |> List.map (fun parameter -> ConstructorUiId.itemParameterRow parameter.name)
                    Assert.Equal(5, parameterRows.Length)
                    for id in parameterRows do
                        Assert.Single(controlsUnder grid id) |> ignore
                    Assert.Equal<string list>(
                        [ "c1.extinctionRatio"; "c1.axisOffset"; "c2.retardance"; "c2.fastAxisOffset"; "dialZero" ],
                        published.parameters |> List.map (fun parameter -> boxLabelOf window (ConstructorUiId.itemParameterRow parameter.name)))
                    Assert.Equal<string list>([ "Calibrated"; "0.01"; "—"; "0.002"; "Fit run QZ-LR@20261101T120000Z" ], gridCellsOf window fastAxisOffset)
                    Assert.Equal<string list>([ "Known"; "1.5708"; "—"; "—"; "Given" ], gridCellsOf window retardance)
                    let headers =
                        grid.Children
                        |> Seq.filter (fun cell -> Grid.GetRow cell = 0)
                        |> Seq.sortBy Grid.GetColumn
                        |> Seq.map (fun cell -> Assert.IsType<TextBlock>(cell).Text)
                        |> List.ofSeq
                    Assert.Equal<string list>([ "Parameter"; "State"; "Value"; "Bounds"; "σ"; "Provenance" ], headers)

                    let revisions = Assert.IsType<StackPanel>(controlById window ConstructorUiId.itemRevisionList)
                    Assert.Equal<string list>(
                        [ (ConstructorUiId.itemRevisionRow (ItemRevision 2)).value; (ConstructorUiId.itemRevisionRow ItemRevision.first).value ],
                        childIds revisions)
                    Assert.Equal("Revision 2 (current)", textOf window (ConstructorUiId.itemRevisionRow (ItemRevision 2)))
                    Assert.Equal(
                        "Revision 1: c1.extinctionRatio 0, c1.axisOffset 0, c2.retardance 1.5708, c2.fastAxisOffset 0, dialZero 0",
                        textOf window (ConstructorUiId.itemRevisionRow ItemRevision.first)))

                // A bound item names its preset or sample beside its stored model, and a model the store does not
                // hold is the missing-model condition, never the preset's name alone.
                let source = List.head (BuiltinCatalogue.sources ())
                let sample = List.head (BuiltinCatalogue.samples ())
                let sourceItemId = ItemId (Guid.Parse "24100000-0000-4000-8000-000000000003")
                let sampleItemId = ItemId (Guid.Parse "24100000-0000-4000-8000-000000000004")
                let sourceDraft : ItemDraft =
                    {
                        id = sourceItemId
                        label = ItemLabel "SRC1"
                        model = SourcePresetChoice (BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId, source)
                        dial = FixedMounting
                    }
                let sampleDraft : ItemDraft =
                    {
                        id = sampleItemId
                        label = ItemLabel "S1"
                        model = StackSampleChoice (BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId, SampleVersionId.firstOf sample.id, sample)
                        dial = FixedMounting
                    }
                let bound = model |> update (AddItem sourceDraft) |> update (AddItem sampleDraft) |> update (SelectKitItem sourceItemId)
                Assert.Equal<ControlCaption option>(None, bound.kitStatus)
                withItemsPanel bound (fun window ->
                    Assert.Equal(
                        $"SRC1 — {source.name.value} (Light source (preset binding) v1) — revision 1",
                        boxLabelOf window (ConstructorUiId.kitItemRow sourceItemId))
                    Assert.Equal($"S1 — {sample.name.value} v1 (Sample by stack v1) — revision 1", boxLabelOf window (ConstructorUiId.kitItemRow sampleItemId))
                    Assert.Equal($"Model: {source.name.value} (Light source (preset binding) v1)", textOf window ConstructorUiId.itemModelText)
                    Assert.Equal("Dial: Fixed mounting", textOf window ConstructorUiId.itemDialText))
                // The revisions list the current one, then the earlier ones newest first.
                let twicePublished =
                    let sourceItem = bound.kitItems |> List.find (fun item -> item.id = sourceItemId)
                    sourceItem.publishRevision [] |> Result.bind (fun revised -> revised.publishRevision []) |> itemValue
                let republished =
                    {
                        bound with kitItems = bound.kitItems |> List.map (fun item -> if item.id = sourceItemId then twicePublished else item)
                    }
                withItemsPanel republished (fun window ->
                    let revisions = Assert.IsType<StackPanel>(controlById window ConstructorUiId.itemRevisionList)
                    Assert.Equal<string list>(
                        [ 3; 2; 1 ] |> List.map (fun revision -> (ConstructorUiId.itemRevisionRow (ItemRevision revision)).value),
                        childIds revisions)
                    Assert.Equal("Revision 3 (current)", textOf window (ConstructorUiId.itemRevisionRow (ItemRevision 3)))
                    Assert.StartsWith("Revision 2: azimuth 0, ", textOf window (ConstructorUiId.itemRevisionRow (ItemRevision 2))))
                let unheld = { bound with itemModels = ItemModelProxy.createInMemory () }
                withItemsPanel unheld (fun window ->
                    Assert.Equal("CPL1 — (model not in the library) — revision 2", boxLabelOf window (ConstructorUiId.kitItemRow cpl1Id))
                    for id, label in [ lp1Id, "LP1"; sourceItemId, "SRC1"; sampleItemId, "S1" ] do
                        Assert.Equal($"{label} — (model not in the library) — revision 1", boxLabelOf window (ConstructorUiId.kitItemRow id))
                    Assert.Equal("Model: (model not in the library)", textOf window ConstructorUiId.itemModelText))

                // With no item, the list says so and no item is shown.
                withItemsPanel scene (fun window ->
                    let list = Assert.IsType<StackPanel>(controlById window ConstructorUiId.kitItemList)
                    Assert.Equal("This kit holds no item yet.", Assert.IsType<TextBlock>(Assert.Single(list.Children)).Text)
                    Assert.Equal("Add item from library model…", boxLabelOf window ConstructorUiId.addItemButton)
                    for absent in [ ConstructorUiId.itemLabelField; ConstructorUiId.itemParameterGrid; ConstructorUiId.itemRevisionList; ConstructorUiId.addItemOfferList ] do
                        assertAbsent window absent)))

    [<Fact>]
    let ``the add form lists the library models by kind and offers Add and Cancel on one row, Cancel to the right`` () : unit =
        HeadlessSession.run (fun () ->
            withScene (fun scene ->
                let opened = scene |> update OpenAddItemForm
                withItemsPanel opened (fun window ->
                    let offerList = Assert.IsType<ScrollViewer>(controlById window ConstructorUiId.addItemOfferList)
                    Assert.Single(controlsUnder offerList (ConstructorUiId.addItemOffer circularChoice)) |> ignore
                    let offers = Assert.IsType<StackPanel>(offerList.Content)
                    let headings =
                        offers.Children
                        |> Seq.choose (function :? TextBlock as heading -> Some heading.Text | _ -> None)
                        |> List.ofSeq
                    Assert.Equal<string list>([ "Item models"; "Light sources"; "Polarizers"; "Samples" ], headings)
                    // Each offer sits under its kind's heading, in the form's order.
                    let form = opened.addItemFormOpt |> Option.defaultWith (fun () -> failwith "The add form must be open.")
                    let offerIds = form.offers |> List.map (fun offer -> (ConstructorUiId.addItemOffer offer).value)
                    Assert.Equal<string list>(offerIds, childIds offers)
                    let position (text : string) : int =
                        offers.Children |> Seq.findIndex (fun child -> listIdOf child = Some text)
                    let headingAt (text : string) : int =
                        offers.Children |> Seq.findIndex (function :? TextBlock as heading -> heading.Text = text | _ -> false)
                    let circularAt = position (ConstructorUiId.addItemOffer circularChoice).value
                    Assert.True(headingAt "Item models" < circularAt && circularAt < headingAt "Light sources", "the circular polarizer is an item model")
                    assertAbsent window ConstructorUiId.addItemConfirmButton
                    assertAbsent window ConstructorUiId.addItemButton
                    Assert.Equal("Cancel", boxLabelOf window ConstructorUiId.addItemCancelButton)
                    Assert.Equal("Fixed mounting", boxLabelOf window (ConstructorUiId.newItemDialChoice FixedMounting))
                    Assert.Equal("On a dial, sign −1", boxLabelOf window (ConstructorUiId.newItemDialChoice (DialMounted DialNegative))))

                let filled =
                    opened
                    |> update (ChooseNewItemModel circularChoice)
                    |> update (SetNewItemLabel (ControlCaption "CPL1"))
                withItemsPanel filled (fun window ->
                    Assert.Equal("CPL1", Assert.IsType<TextBox>(controlById window ConstructorUiId.newItemLabelField).Text)
                    Assert.Equal("Add", boxLabelOf window ConstructorUiId.addItemConfirmButton)
                    assertOneRowRightOf window ConstructorUiId.addItemConfirmButton ConstructorUiId.addItemCancelButton)))

    [<Fact>]
    let ``the parameter editor offers Known and Unknown, and Set and Cancel on one row`` () : unit =
        HeadlessSession.run (fun () ->
            withScene (fun scene ->
                let known = scene |> update (AddItem cpl1Draft) |> update (EditItemParameter retardance)
                withItemsPanel known (fun window ->
                    Assert.Single(controlsUnder window ConstructorUiId.parameterEditPanel) |> ignore
                    Assert.Equal("Known", boxLabelOf window ConstructorUiId.parameterEditKnownChoice)
                    Assert.Equal("Unknown", boxLabelOf window ConstructorUiId.parameterEditUnknownChoice)
                    Assert.Single(controlsUnder window ConstructorUiId.parameterEditValueField) |> ignore
                    for absent in [ ConstructorUiId.parameterEditLowerField; ConstructorUiId.parameterEditUpperField; ConstructorUiId.parameterEditPriorSigmaField ] do
                        assertAbsent window absent
                    assertOneRowRightOf window ConstructorUiId.parameterEditSetButton ConstructorUiId.parameterEditCancelButton)

                let unknown = known |> update (SetParameterEditChoice UnknownStateChoice)
                let typed =
                    unknown
                    |> update (SetParameterEditText (LowerEditField, ControlCaption "abc"))
                    |> update (SetParameterEditText (UpperEditField, ControlCaption "2.4"))
                withItemsPanel typed (fun window ->
                    for present in [ ConstructorUiId.parameterEditLowerField; ConstructorUiId.parameterEditUpperField; ConstructorUiId.parameterEditPriorSigmaField ] do
                        Assert.Single(controlsUnder window present) |> ignore
                    Assert.Equal("abc", Assert.IsType<TextBox>(controlById window ConstructorUiId.parameterEditLowerField).Text)
                    assertAbsent window ConstructorUiId.parameterEditSetButton
                    let panel = controlById window ConstructorUiId.parameterEditPanel
                    let told =
                        panel.GetVisualDescendants()
                        |> Seq.choose (function :? TextBlock as line -> Option.ofObj line.Text | _ -> None)
                        |> Seq.filter (fun text -> text.Contains "abc" && text.Contains "lower")
                        |> List.ofSeq
                    Assert.Single(told) |> ignore
                    Assert.Equal("Cancel", boxLabelOf window ConstructorUiId.parameterEditCancelButton))

                let valid = typed |> update (SetParameterEditText (LowerEditField, ControlCaption "0.7"))
                withItemsPanel valid (fun window ->
                    Assert.Equal("Set", boxLabelOf window ConstructorUiId.parameterEditSetButton)
                    assertOneRowRightOf window ConstructorUiId.parameterEditSetButton ConstructorUiId.parameterEditCancelButton)))

    [<Fact>]
    let ``a kit opened read-only offers no item write in the Items panel`` () : unit =
        HeadlessSession.run (fun () ->
            withScene (fun scene ->
                withPhotoFile (fun photo ->
                    let photographed = scene |> update (AddItem cpl1Draft) |> update (SetKitItemPhoto (cpl1Id, photo))
                    Assert.Equal<ControlCaption option>(None, photographed.kitStatus)
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
                                kitItems = readOnlyProxy.readItems () |> SceneTestFixture.kitSuccess
                                selectedItemOpt = Some cpl1Id
                        }
                    let held : Item = Assert.Single(readOnly.kitItems)
                    Assert.Equal<KitRelativePath option>(Some (KitRelativePath "data/CPL1.jpg"), held.photoOpt)
                    withItemsPanel readOnly (fun window ->
                        Assert.Single(controlsUnder window (ConstructorUiId.kitItemRow cpl1Id)) |> ignore
                        Assert.Equal("Photo: data/CPL1.jpg", textOf window ConstructorUiId.itemPhotoText)
                        for absent in [ ConstructorUiId.addItemButton; ConstructorUiId.itemChoosePhotoButton; ConstructorUiId.itemRemovePhotoButton ] do
                            assertAbsent window absent
                        for disabled in [ ConstructorUiId.itemLabelField; ConstructorUiId.itemNotesField ] do
                            Assert.False((controlById window disabled).IsEnabled, $"{disabled.value} must be disabled"))

                    // A form or an editor opened by its message still offers no Add or Set.
                    let editing =
                        readOnly
                        |> update OpenAddItemForm
                        |> update (ChooseNewItemModel circularChoice)
                        |> update (SetNewItemLabel (ControlCaption "CPL2"))
                        |> update (EditItemParameter retardance)
                    withItemsPanel editing (fun window ->
                        Assert.Single(controlsUnder window ConstructorUiId.addItemCancelButton) |> ignore
                        Assert.Single(controlsUnder window ConstructorUiId.parameterEditCancelButton) |> ignore
                        assertAbsent window ConstructorUiId.addItemConfirmButton
                        assertAbsent window ConstructorUiId.parameterEditSetButton))))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``pressing Add item, a model, a label and Add records the item in the kit`` () : unit =
        HeadlessSession.run (fun () ->
            withScene (fun scene ->
                withItemsPanel scene (fun window ->
                    pressBox window ConstructorUiId.addItemButton
                    pressBox window (ConstructorUiId.addItemOffer circularChoice)
                    // Typed in two steps: Add, shown at the first, must send the label as it reads at the press.
                    let field = Assert.IsType<TextBox>(controlById window ConstructorUiId.newItemLabelField)
                    field.Text <- "CP"
                    Dispatcher.UIThread.RunJobs()
                    Assert.Equal("Add", boxLabelOf window ConstructorUiId.addItemConfirmButton)
                    field.Text <- "CPL1"
                    Dispatcher.UIThread.RunJobs()
                    pressBox window ConstructorUiId.addItemConfirmButton
                    Assert.Equal($"CPL1 — {circularCaption} — revision 1", boxLabelOf window (ConstructorUiId.kitItemRow cpl1Id))
                    assertAbsent window ConstructorUiId.addItemOfferList
                    let recorded : Item = Assert.Single(scene.kit.proxy.readItems () |> SceneTestFixture.kitSuccess)
                    Assert.Equal<ItemId>(cpl1Id, recorded.id)
                    Assert.Equal<ItemLabel>(ItemLabel "CPL1", recorded.label)
                    Assert.Equal<ItemModelInstance>(circularChoice.instance, recorded.model)
                    Assert.Equal<ItemDial>(FixedMounting, recorded.dial))))
