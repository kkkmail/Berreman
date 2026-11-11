namespace OpticalConstructor.Ui.Tests

open System
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open global.Elmish

/// The slot bar above the Main table, mounted in `mainView` and driven through its messages, its keyboard and its
/// presses: a chip per slot in light order, the open chip's fitting items, Tab and letters, swaps and Record experiment
/// (spec 0050 S8.4; 001 §J, §O).
module SlotBarTests =

    let private update (msg : TableAndElementRotationMsg) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        updateTableAndElementRotation msg model

    /// The seeded light source's slot.
    let private source : Library.ElementId = Library.ElementId "src"

    /// The seeded detector's slot.
    let private detector : Library.ElementId = Library.ElementId "det"

    let private prepRole : SlotRole = PrepSlotRole (SlotPosition 1)

    let private objectRole : SlotRole = ObjectSlotRole (SlotPosition 1)

    let private laserChoice : ItemModelChoice =
        SourcePresetChoice (BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId, BuiltinCatalogue.source Library.PresetIds.heNe633)

    /// The ideal linear polarizer preset on the shipped ideal-polarizer item model.
    let private linearPolarizerChoice : ItemModelChoice =
        match BuiltinCatalogue.preset Library.PresetIds.polarizerLp with
        | Library.PolarizerItem polarizer ->
            PolarizerPresetChoice (BuiltinCatalogue.itemModel ItemModelIds.idealPolarizerItemModelId, polarizer)
        | Library.SampleItem _
        | Library.SourceItem _
        | Library.DetectorItem _ -> failwith "The Built-in linear polarizer preset must be a polarizer."

    /// The glass plate at its first version on the shipped stack-sample item model.
    let private plateChoice (plate : Library.SampleId) : ItemModelChoice =
        StackSampleChoice (
            BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId,
            Library.SampleVersionId.firstOf plate,
            BuiltinCatalogue.sample plate
        )

    /// Runs over a fresh Main scene in its scratch kit; the kit is closed at the end.
    let private withScene (run : TableAndElementRotationModel -> unit) : unit =
        let scene = initMain () |> SceneTestFixture.kitSuccess
        try run scene
        finally scene.kit.close () |> SceneTestFixture.kitSuccess

    /// The model with an item of `choice` labelled `label`, added through the Items panel's add form (spec 0050 S7.2).
    let private withItemAdded (label : ControlCaption) (choice : ItemModelChoice) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let filled =
            model
            |> update OpenAddItemForm
            |> update (ChooseNewItemModel choice)
            |> update (SetNewItemLabel label)
        match filled.addItemFormOpt |> Option.bind (fun form -> form.itemDraftOpt) with
        | Some draft -> filled |> update (AddItem draft)
        | None -> failwith $"The add form must send {label.value}."

    /// The id of the kit's item labelled `label`.
    let private itemLabelled (label : ItemLabel) (model : TableAndElementRotationModel) : ItemId =
        match model.kitItems |> List.tryFind (fun item -> item.label = label) with
        | Some item -> item.id
        | None -> failwith $"The kit must hold {label.value}."

    /// The slot-bar facts' table and the slot ids of its polarizer and its sample.
    type private BarTable =
        {
            table : TableAndElementRotationModel
            polarizer : Library.ElementId
            sample : Library.ElementId
        }

    /// Over the scratch kit stocked with Laser, LP1, LP2, QZ and QZ2: a linear polarizer and a sample added, which the
    /// light meets in that order before the seeded detector, the sample turned to R1 45°, and Laser, LP1 and QZ assigned
    /// to the source, the polarizer and the sample.
    let private barTable (scene : TableAndElementRotationModel) : BarTable =
        let stocked =
            scene
            |> withItemAdded (ControlCaption "Laser") laserChoice
            |> withItemAdded (ControlCaption "LP1") linearPolarizerChoice
            |> withItemAdded (ControlCaption "LP2") linearPolarizerChoice
            |> withItemAdded (ControlCaption "QZ") (plateChoice Library.SampleIds.glassPlate1mmId)
            |> withItemAdded (ControlCaption "QZ2") (plateChoice Library.SampleIds.glassPlate2mmId)
        Assert.Equal<ControlCaption option>(None, stocked.kitStatus)
        Assert.Equal<ItemLabel list>(
            [ ItemLabel "LP1"; ItemLabel "LP2"; ItemLabel "Laser"; ItemLabel "QZ"; ItemLabel "QZ2" ],
            stocked.kitItems |> List.map (fun item -> item.label))
        let withPolarizer = stocked |> update (AddElement LinearPolarizer)
        let polarizer = (withPolarizer.elements |> List.find (fun slot -> slot.placement.catalogueKind = LinearPolarizer)).id
        let withSample = withPolarizer |> update (AddElement Sample)
        let sample = (withSample.elements |> List.find (fun slot -> slot.placement.catalogueKind = Sample)).id
        // The add selects the sample, so the turn is its own.
        let table =
            withSample
            |> update (RotSetAxis (RotationControls.R1, BarDegrees 45.0))
            |> update (AssignSlot (source, Some (itemLabelled (ItemLabel "Laser") stocked)))
            |> update (AssignSlot (polarizer, Some (itemLabelled (ItemLabel "LP1") stocked)))
            |> update (AssignSlot (sample, Some (itemLabelled (ItemLabel "QZ") stocked)))
        Assert.Equal<ControlCaption option>(None, table.kitStatus)
        {
            table = table
            polarizer = polarizer
            sample = sample
        }

    /// Shows `mainView` over `model` headless at 1000 × 900, as `ScrollExtentTests.constructorWindow` mounts it, keeping
    /// every model the program answers in order; runs `check` over the window and those models, then closes it.
    let private withMainWindow
        (model : TableAndElementRotationModel)
        (check : Window -> ResizeArray<TableAndElementRotationModel> -> unit)
        : unit =
        let answered = ResizeArray<TableAndElementRotationModel> ()
        let recordingUpdate (msg : TableAndElementRotationMsg) (current : TableAndElementRotationModel) : TableAndElementRotationModel =
            let next = updateTableAndElementRotation msg current
            answered.Add next
            next
        let window = HostWindow()
        Program.mkSimple (fun () -> model) recordingUpdate mainView |> Program.withHost window |> Program.run
        window.Width <- 1000.0
        window.Height <- 900.0
        try
            window.Show()
            Dispatcher.UIThread.RunJobs()
            check window answered
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

    /// The label of the chip or box carrying `id`: the text of the box's child.
    let private boxLabelOf (window : Window) (id : ConstructorUiId) : string =
        Assert.IsType<TextBlock>(Assert.IsType<Border>(controlById window id).Child).Text

    /// The window position of the control's top-left corner.
    let private topLeftOf (window : Window) (control : Control) : Point =
        match control.TranslatePoint(Point(0.0, 0.0), window) |> Option.ofNullable with
        | Some point -> point
        | None -> failwith $"{AutomationProperties.GetAutomationId control} has no position in its window"

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

    /// The ids the panel's children carry, in order.
    let private childIds (panel : Panel) : string list =
        panel.Children |> Seq.map AutomationProperties.GetAutomationId |> List.ofSeq

    /// The element the window's keyboard focus is on.
    let private focusedIn (window : Window) : obj =
        match window.FocusManager with
        | null -> failwith "The window must have a focus manager."
        | focus -> box (focus.GetFocusedElement())

    /// The latest model the program answered, else the one it started from.
    let private latestOf (start : TableAndElementRotationModel) (answered : ResizeArray<TableAndElementRotationModel>) : TableAndElementRotationModel =
        match Seq.tryLast answered with
        | Some latest -> latest
        | None -> start

    /// The item the table's slot of that id holds.
    let private heldIn (slotId : Library.ElementId) (model : TableAndElementRotationModel) : ItemId option =
        (model.elements |> List.find (fun slot -> slot.id = slotId)).content.itemOpt

    [<Fact>]
    let ``the slot bar UiIds are the stable intent-named ids`` () : unit =
        Assert.Equal("SlotBar", ConstructorUiId.slotBar.value)
        Assert.Equal("SlotChipChoiceList", ConstructorUiId.slotChipChoiceList.value)
        Assert.Equal("RecordExperimentButton", ConstructorUiId.recordExperimentButton.value)
        Assert.Equal("SlotChip_Source", (ConstructorUiId.slotChip SourceSlotRole).value)
        Assert.Equal("SlotChip_Prep1", (ConstructorUiId.slotChip (PrepSlotRole (SlotPosition 1))).value)
        Assert.Equal("SlotChip_Obj2", (ConstructorUiId.slotChip (ObjectSlotRole (SlotPosition 2))).value)
        Assert.Equal("SlotChip_Analyzer1", (ConstructorUiId.slotChip (AnalyzerSlotRole (SlotPosition 1))).value)
        Assert.Equal("SlotChip_Detector1", (ConstructorUiId.slotChip (DetectorSlotRole (SlotPosition 1))).value)
        let item = ItemId (Guid.Parse "28000000-0000-4000-8000-000000000001")
        Assert.Equal("SlotChipChoice_Obj1_28000000-0000-4000-8000-000000000001", (ConstructorUiId.slotChipChoice objectRole item).value)

    [<Fact>]
    let ``the slot bar shows one chip per slot in light order above the table, each naming its item and an object its angle`` () : unit =
        HeadlessSession.run (fun () ->
            withScene (fun scene ->
                let held = barTable scene
                withMainWindow held.table (fun window _ ->
                    let chips =
                        [ SourceSlotRole; prepRole; objectRole; DetectorSlotRole (SlotPosition 1) ]
                        |> List.map ConstructorUiId.slotChip
                    let bar = controlById window ConstructorUiId.slotBar
                    let shown =
                        bar.GetVisualDescendants()
                        |> Seq.choose (function :? Border as border -> Some (AutomationProperties.GetAutomationId border) | _ -> None)
                        |> Seq.filter (fun id -> not (isNull id) && id.StartsWith "SlotChip_")
                        |> List.ofSeq
                    Assert.Equal<string list>(chips |> List.map (fun chip -> chip.value), shown)
                    Assert.Equal<string list>(
                        [ "Source: Laser ▾"; "Prep 1: LP1 ▾"; "Obj 1: QZ @ 45° ▾"; "Detector 1: — ▾" ],
                        chips |> List.map (boxLabelOf window))
                    for chip in chips do
                        let chipBox = controlById window chip
                        Assert.True(chipBox.Focusable && KeyboardNavigation.GetIsTabStop chipBox, $"{chip.value} must be a focusable tab stop")
                    // One row, left to right in light order.
                    let corners = chips |> List.map (fun chip -> topLeftOf window (controlById window chip))
                    let rowTop = (List.head corners).Y
                    Assert.True(corners |> List.forall (fun corner -> abs (corner.Y - rowTop) < 0.5), $"the chips must share one row: %A{corners}")
                    Assert.True(corners |> List.pairwise |> List.forall (fun (left, right) -> left.X < right.X), $"the chips must run left to right: %A{corners}")
                    let canvas =
                        window.GetVisualDescendants()
                        |> Seq.pick (function :? Canvas as canvas when canvas.Name = ConstructorUiId.tableElementCanvas.value -> Some canvas | _ -> None)
                    let barBottom = (topLeftOf window bar).Y + bar.Bounds.Height
                    let tableTop = (topLeftOf window canvas).Y
                    Assert.True(barBottom <= tableTop + 0.5, $"the bar's bottom {barBottom} must be above the table's top {tableTop}"))))

    [<Fact>]
    let ``an open chip lists only the kit's items that fit its slot`` () : unit =
        HeadlessSession.run (fun () ->
            withScene (fun scene ->
                let held = barTable scene
                let itemOf (label : string) : ItemId = itemLabelled (ItemLabel label) held.table
                withMainWindow (held.table |> update (ToggleSlotChip held.polarizer)) (fun window _ ->
                    let list = Assert.IsType<WrapPanel>(controlById window ConstructorUiId.slotChipChoiceList)
                    let listed = [ itemOf "LP1"; itemOf "LP2" ] |> List.map (ConstructorUiId.slotChipChoice prepRole)
                    Assert.Equal<string list>(listed |> List.map (fun id -> id.value), childIds list)
                    for unfit in [ "Laser"; "QZ"; "QZ2" ] do
                        assertAbsent window (ConstructorUiId.slotChipChoice prepRole (itemOf unfit))
                    Assert.Equal<string list>([ "LP1"; "LP2" ], listed |> List.map (boxLabelOf window))
                    // The open chip and the item its slot holds are on the chosen-option fill.
                    let fillOf (id : ConstructorUiId) : Media.IBrush = Assert.IsType<Border>(controlById window id).Background
                    let chosen = Assert.IsAssignableFrom<Media.ISolidColorBrush>(fillOf (ConstructorUiId.slotChip prepRole))
                    Assert.Equal(MaterialsWindowView.positiveBackground, chosen.Color)
                    let heldFill = Assert.IsAssignableFrom<Media.ISolidColorBrush>(fillOf (List.head listed))
                    Assert.Equal(MaterialsWindowView.positiveBackground, heldFill.Color)
                    let otherFill = Assert.IsAssignableFrom<Media.ISolidColorBrush>(fillOf (List.item 1 listed))
                    Assert.NotEqual(MaterialsWindowView.positiveBackground, otherFill.Color))
                withMainWindow (held.table |> update (ToggleSlotChip detector)) (fun window _ ->
                    let list = Assert.IsType<WrapPanel>(controlById window ConstructorUiId.slotChipChoiceList)
                    Assert.Equal("No item of this kit fits Detector 1.", Assert.IsType<TextBlock>(Assert.Single(list.Children)).Text))))

    [<Fact>]
    let ``Tab moves between the chips in light order, and a typed letter picks the next fitting item`` () : unit =
        HeadlessSession.run (fun () ->
            withScene (fun scene ->
                let held = barTable scene
                let itemOf (label : string) : ItemId = itemLabelled (ItemLabel label) held.table
                withMainWindow held.table (fun window answered ->
                    let chip (role : SlotRole) : Control = controlById window (ConstructorUiId.slotChip role)
                    let tab () : unit =
                        window.KeyPressQwerty(PhysicalKey.Tab, RawInputModifiers.None)
                        Dispatcher.UIThread.RunJobs()
                        window.KeyReleaseQwerty(PhysicalKey.Tab, RawInputModifiers.None)
                        Dispatcher.UIThread.RunJobs()
                    let focusOn (role : SlotRole) : unit =
                        Assert.True((chip role).Focus(), $"the {role.text} chip must take the focus")
                        Dispatcher.UIThread.RunJobs()
                    let typeOnObject (text : string) : unit =
                        focusOn objectRole
                        window.KeyTextInput text
                        Dispatcher.UIThread.RunJobs()
                    let sampleHolds () : ItemId option = heldIn held.sample (latestOf held.table answered)
                    focusOn prepRole
                    tab ()
                    Assert.Same(box (chip objectRole), focusedIn window)
                    tab ()
                    Assert.Same(box (chip (DetectorSlotRole (SlotPosition 1))), focusedIn window)
                    typeOnObject "q"
                    Assert.Equal<ItemId option>(Some (itemOf "QZ2"), sampleHolds ())
                    typeOnObject "q"
                    Assert.Equal<ItemId option>(Some (itemOf "QZ"), sampleHolds ())
                    let answeredBefore = answered.Count
                    typeOnObject "x"
                    Assert.Equal(answeredBefore, answered.Count)
                    Assert.Equal<ItemId option>(Some (itemOf "QZ"), sampleHolds ()))))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``pressing a chip and an item swaps the table to a kit setup that holds it`` () : unit =
        HeadlessSession.run (fun () ->
            withScene (fun scene ->
                let held = barTable scene
                let qz2 = itemLabelled (ItemLabel "QZ2") held.table
                withMainWindow held.table (fun window _ ->
                    assertAbsent window ConstructorUiId.slotChipChoiceList
                    pressBox window (ConstructorUiId.slotChip objectRole)
                    Assert.Same(box (controlById window (ConstructorUiId.slotChip objectRole)), focusedIn window)
                    Assert.IsType<WrapPanel>(controlById window ConstructorUiId.slotChipChoiceList) |> ignore
                    pressBox window (ConstructorUiId.slotChipChoice objectRole qz2)
                    assertAbsent window ConstructorUiId.slotChipChoiceList
                    Assert.Equal("Obj 1: QZ2 @ 45° ▾", boxLabelOf window (ConstructorUiId.slotChip objectRole)))
                let setup = Assert.Single(held.table.kit.proxy.readSetups () |> SceneTestFixture.kitSuccess)
                let sampleSlot = setup.slots |> List.find (fun slot -> slot.id = held.sample)
                Assert.Equal<ItemId option>(Some qz2, sampleSlot.content.itemOpt)))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``pressing Record experiment twice records two experiments over the active setup`` () : unit =
        HeadlessSession.run (fun () ->
            withScene (fun scene ->
                let held = barTable scene
                let ready =
                    held.table
                    |> update (SetSetupName (ControlCaption "Bench"))
                    |> update CreateSetup
                    |> update (ExpChooseElement held.polarizer)
                Assert.Equal<SetupName option>(Some (SetupName "Bench"), ready.activeSetupOpt |> Option.map (fun setup -> setup.name))
                withMainWindow ready (fun window _ ->
                    pressBox window ConstructorUiId.recordExperimentButton
                    pressBox window ConstructorUiId.recordExperimentButton)
                let recorded = ready.kit.proxy.readExperiments () |> SceneTestFixture.kitSuccess
                Assert.Equal<Experiments.ExperimentId list>(
                    [ Experiments.ExperimentId 1; Experiments.ExperimentId 2 ],
                    recorded |> List.map (fun experiment -> experiment.id))
                Assert.Equal<string list>(
                    [ "'Bench' revision 1"; "'Bench' revision 1" ],
                    recorded |> List.map (fun experiment -> experiment.setup.describe))))
