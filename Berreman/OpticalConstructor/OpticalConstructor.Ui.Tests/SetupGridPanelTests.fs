namespace OpticalConstructor.Ui.Tests

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Media
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.SetupGrids
open OpticalConstructor.Storage
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView

/// The grid generator's panel in `mainView`, between the slot bar and the table, driven through its messages and its
/// presses: each slot's fitting items and R1 angles, the combinations ticked, unticked or impossible, Generate beside
/// Close, and a read-only kit (spec 0050 S8.4; 001 §J, §O). It mounts the window as `SlotBarTests` does.
module SetupGridPanelTests =

    let private update (msg : TableAndElementRotationMsg) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        updateTableAndElementRotation msg model

    /// The seeded light source's slot.
    let private source : Library.ElementId = Library.ElementId "src"

    /// The seeded detector's slot.
    let private detector : Library.ElementId = Library.ElementId "det"

    let private prepRole : SlotRole = PrepSlotRole (SlotPosition 1)

    let private objectRole : SlotRole = ObjectSlotRole (SlotPosition 1)

    let private analyzerRole : SlotRole = AnalyzerSlotRole (SlotPosition 1)

    let private detectorRole : SlotRole = DetectorSlotRole (SlotPosition 1)

    /// The R1 the grid tries for `degrees` typed on the bar: wrapped as `AddSetupGridAngle` wraps it.
    let private triedAt (degrees : float) : Angle = (BarDegrees degrees).wrappedAngle

    /// The grid facts' table and the slot ids of its first polarizer, its sample and its second polarizer.
    type private GridTable =
        {
            table : TableAndElementRotationModel
            polarizer : Library.ElementId
            sample : Library.ElementId
            analyzer : Library.ElementId
        }

    /// Over the scratch kit stocked with LP1, LP2, QZ and QZ2: a linear polarizer, a sample and a second linear
    /// polarizer added, which the light meets in that order before the seeded detector, holding LP1 and LP2.
    let private gridTable (scene : TableAndElementRotationModel) : GridTable =
        let stocked =
            scene
            |> SlotBarTests.withItemAdded (ControlCaption "LP1") SlotBarTests.linearPolarizerChoice
            |> SlotBarTests.withItemAdded (ControlCaption "LP2") SlotBarTests.linearPolarizerChoice
            |> SlotBarTests.withItemAdded (ControlCaption "QZ") (SlotBarTests.plateChoice Library.SampleIds.glassPlate1mmId)
            |> SlotBarTests.withItemAdded (ControlCaption "QZ2") (SlotBarTests.plateChoice Library.SampleIds.glassPlate2mmId)
        Assert.Equal<ControlCaption option>(None, stocked.kitStatus)
        Assert.Equal<ItemLabel list>(
            [ ItemLabel "LP1"; ItemLabel "LP2"; ItemLabel "QZ"; ItemLabel "QZ2" ],
            stocked.kitItems |> List.map (fun item -> item.label))
        let withPolarizer = stocked |> update (AddElement LinearPolarizer)
        let polarizer = (withPolarizer.elements |> List.find (fun slot -> slot.placement.catalogueKind = LinearPolarizer)).id
        let withSample = withPolarizer |> update (AddElement Sample)
        let sample = (withSample.elements |> List.find (fun slot -> slot.placement.catalogueKind = Sample)).id
        let withAnalyzer = withSample |> update (AddElement LinearPolarizer)
        let analyzer =
            (withAnalyzer.elements |> List.find (fun slot -> slot.placement.catalogueKind = LinearPolarizer && slot.id <> polarizer)).id
        let table =
            withAnalyzer
            |> update (AssignSlot (polarizer, Some (SlotBarTests.itemLabelled (ItemLabel "LP1") stocked)))
            |> update (AssignSlot (analyzer, Some (SlotBarTests.itemLabelled (ItemLabel "LP2") stocked)))
        Assert.Equal<ControlCaption option>(None, table.kitStatus)
        Assert.Equal<(SlotRole * Library.ElementId) list>(
            [ SourceSlotRole, source; prepRole, polarizer; objectRole, sample; analyzerRole, analyzer; detectorRole, detector ],
            SetupSlot.inLightOrder table.elements |> List.map (fun roled -> roled.role, roled.slot.id))
        {
            table = table
            polarizer = polarizer
            sample = sample
            analyzer = analyzer
        }

    /// The R1 caption of the table's slot of that id, as a combination's caption writes it.
    let private r1Of (slotId : Library.ElementId) (model : TableAndElementRotationModel) : string =
        (model.elements |> List.find (fun slot -> slot.id = slotId)).placement.r1.degreeCaption.value

    /// The lines of the combinations list in order: a box's caption, or an impossible line's text.
    let private combinationLines (window : Window) : Control list =
        let list = Assert.IsType<StackPanel>(SlotBarTests.controlById window ConstructorUiId.setupGridCombinationList)
        list.Children |> List.ofSeq

    /// The text a combination line shows.
    let private lineText (line : Control) : string =
        match line with
        | :? Border as box -> Assert.IsType<TextBlock>(box.Child).Text
        | :? TextBlock as text -> text.Text
        | other -> failwith $"a combination line must be a box or a text, not a {other.GetType().Name}"

    /// The fill of the box carrying `id`.
    let private fillOf (window : Window) (id : ConstructorUiId) : Color =
        let box = Assert.IsType<Border>(SlotBarTests.controlById window id)
        (Assert.IsAssignableFrom<ISolidColorBrush>(box.Background)).Color

    [<Fact>]
    let ``the setup grid UiIds are the stable intent-named ids`` () : unit =
        Assert.Equal("SetupGridOpenButton", ConstructorUiId.setupGridOpenButton.value)
        Assert.Equal("SetupGridPanel", ConstructorUiId.setupGridPanel.value)
        Assert.Equal("SetupGridCombinationList", ConstructorUiId.setupGridCombinationList.value)
        Assert.Equal("SetupGridGenerateButton", ConstructorUiId.setupGridGenerateButton.value)
        Assert.Equal("SetupGridCloseButton", ConstructorUiId.setupGridCloseButton.value)
        Assert.Equal("SetupGridCombination_2", (ConstructorUiId.setupGridCombination (ConstructorUiPosition 2)).value)
        let item = ItemId (Guid.Parse "28300000-0000-4000-8000-000000000001")
        Assert.Equal("SetupGridItem_Obj1_28300000-0000-4000-8000-000000000001", (ConstructorUiId.setupGridItemChoice objectRole item).value)
        Assert.Equal("SetupGridAngleField_Analyzer1", (ConstructorUiId.setupGridAngleField analyzerRole).value)
        Assert.Equal("SetupGridAngle_Obj1_22.5", (ConstructorUiId.setupGridAngle objectRole (Angle.degree 22.5<deg>)).value)

    [<Fact>]
    let ``the grid panel sits between the slot bar and the table, offers each slot's fitting items and angles, and marks each combination ticked, unticked or impossible`` () : unit =
        HeadlessSession.run (fun () ->
            SlotBarTests.withScene (fun scene ->
                let held = gridTable scene
                let itemOf (label : string) : ItemId = SlotBarTests.itemLabelled (ItemLabel label) held.table
                let opened = held.table |> update OpenSetupGrid
                SlotBarTests.withMainWindow opened (fun window _ ->
                    let bar = SlotBarTests.controlById window ConstructorUiId.slotBar
                    let panel = SlotBarTests.controlById window ConstructorUiId.setupGridPanel
                    let canvas =
                        window.GetVisualDescendants()
                        |> Seq.pick (function :? Canvas as canvas when canvas.Name = ConstructorUiId.tableElementCanvas.value -> Some canvas | _ -> None)
                    let barBottom = (SlotBarTests.topLeftOf window bar).Y + bar.Bounds.Height
                    let panelTop = (SlotBarTests.topLeftOf window panel).Y
                    let panelBottom = panelTop + panel.Bounds.Height
                    let tableTop = (SlotBarTests.topLeftOf window canvas).Y
                    Assert.True(barBottom <= panelTop + 0.5, $"the panel's top {panelTop} must be below the bar's bottom {barBottom}")
                    Assert.True(panelBottom <= tableTop + 0.5, $"the panel's bottom {panelBottom} must be above the table's top {tableTop}")
                    // A row per slot, in light order, each with its R1 field.
                    let roleLabels = [ "Source:"; "Prep 1:"; "Obj 1:"; "Analyzer 1:"; "Detector 1:" ]
                    let shownLabels =
                        panel.GetVisualDescendants()
                        |> Seq.choose (function :? TextBlock as text when List.contains text.Text roleLabels -> Some text.Text | _ -> None)
                        |> List.ofSeq
                    Assert.Equal<string list>(roleLabels, shownLabels)
                    for role in [ SourceSlotRole; prepRole; objectRole; analyzerRole; detectorRole ] do
                        Assert.IsType<TextBox>(SlotBarTests.controlById window (ConstructorUiId.setupGridAngleField role)) |> ignore
                    // Each slot offers the kit's items that fit it, and only those.
                    let offered (role : SlotRole) (labels : string list) : string list =
                        labels |> List.map (fun label -> SlotBarTests.boxLabelOf window (ConstructorUiId.setupGridItemChoice role (itemOf label)))
                    Assert.Equal<string list>([ "QZ"; "QZ2" ], offered objectRole [ "QZ"; "QZ2" ])
                    Assert.Equal<string list>([ "LP1"; "LP2" ], offered prepRole [ "LP1"; "LP2" ])
                    for unfit in [ "LP1"; "LP2" ] do
                        SlotBarTests.assertAbsent window (ConstructorUiId.setupGridItemChoice objectRole (itemOf unfit))
                    for unfit in [ "QZ"; "QZ2" ] do
                        SlotBarTests.assertAbsent window (ConstructorUiId.setupGridItemChoice prepRole (itemOf unfit))
                    // Nothing tried yet: the one combination is the table as it is.
                    Assert.Equal<string list>([ "☑ The table as it is" ], combinationLines window |> List.map lineText))

                let qz2At45 : GridCombination =
                    {
                        picks = [ ({ slot = held.sample; choice = ({ itemOpt = Some (itemOf "QZ2"); r1 = triedAt 45.0 } : SlotChoice) } : SlotPick) ]
                    }
                let tried =
                    opened
                    |> update (ToggleSetupGridItem (held.sample, itemOf "QZ"))
                    |> update (ToggleSetupGridItem (held.sample, itemOf "QZ2"))
                    |> update (AddSetupGridAngle (held.sample, BarDegrees 0.0))
                    |> update (AddSetupGridAngle (held.sample, BarDegrees 45.0))
                    |> update (ToggleSetupGridCombination qz2At45)
                SlotBarTests.withMainWindow tried (fun window _ ->
                    let lines = combinationLines window
                    Assert.Equal<string list>(
                        [ "☑ Obj 1: QZ @ 0°"; "☑ Obj 1: QZ @ 45°"; "☑ Obj 1: QZ2 @ 0°"; "☐ Obj 1: QZ2 @ 45°" ],
                        lines |> List.map lineText)
                    Assert.Equal<string list>(
                        [ 1 .. 4 ] |> List.map (fun position -> (ConstructorUiId.setupGridCombination (ConstructorUiPosition position)).value),
                        lines |> List.map Automation.AutomationProperties.GetAutomationId)
                    Assert.Equal<string list>(
                        [ "0° ×"; "45° ×" ],
                        [ 0.0; 45.0 ] |> List.map (fun degrees -> SlotBarTests.boxLabelOf window (ConstructorUiId.setupGridAngle objectRole (triedAt degrees))))
                    // The tried items are on the chosen-option fill; an untried one is not.
                    for label in [ "QZ"; "QZ2" ] do
                        Assert.Equal(MaterialsWindowView.positiveBackground, fillOf window (ConstructorUiId.setupGridItemChoice objectRole (itemOf label)))
                    Assert.NotEqual(MaterialsWindowView.positiveBackground, fillOf window (ConstructorUiId.setupGridItemChoice prepRole (itemOf "LP1"))))

                let crossed =
                    tried
                    |> update (ToggleSetupGridItem (held.polarizer, itemOf "LP1"))
                    |> update (ToggleSetupGridItem (held.polarizer, itemOf "LP2"))
                    |> update (ToggleSetupGridItem (held.analyzer, itemOf "LP2"))
                SlotBarTests.withMainWindow crossed (fun window _ ->
                    let lines = combinationLines window
                    Assert.Equal(8, lines.Length)
                    let prep = r1Of held.polarizer crossed
                    let analyzer = r1Of held.analyzer crossed
                    Assert.Equal($"☑ Prep 1: LP1 @ {prep}, Obj 1: QZ @ 0°, Analyzer 1: LP2 @ {analyzer}", lineText (List.head lines))
                    let twice = $"the item is in two slots: 'LP2' in '{held.polarizer.value}' and '{held.analyzer.value}'"
                    let impossible, possible = lines |> List.partition (fun line -> (lineText line).Contains "Prep 1: LP2")
                    Assert.Equal(4, impossible.Length)
                    for line in impossible do
                        // An impossible line is text in the refusal colour, which takes no press.
                        let text = Assert.IsType<TextBlock>(line)
                        Assert.StartsWith("✗ ", text.Text)
                        Assert.EndsWith(twice, text.Text)
                        Assert.Equal(MaterialsWindowView.messageColor, (Assert.IsAssignableFrom<ISolidColorBrush>(text.Foreground)).Color)
                    for line in possible do
                        Assert.IsType<Border>(line) |> ignore
                        Assert.StartsWith("☑ ", lineText line))))

    [<Fact>]
    let ``Generate and Close share a row, Close to the right, and Generate needs a ticked combination over a slot`` () : unit =
        HeadlessSession.run (fun () ->
            SlotBarTests.withScene (fun scene ->
                let held = gridTable scene
                let qz = SlotBarTests.itemLabelled (ItemLabel "QZ") held.table
                let opened = held.table |> update OpenSetupGrid
                SlotBarTests.withMainWindow opened (fun window _ ->
                    SlotBarTests.assertAbsent window ConstructorUiId.setupGridGenerateButton
                    Assert.Equal("Close", SlotBarTests.boxLabelOf window ConstructorUiId.setupGridCloseButton))

                let tried = opened |> update (ToggleSetupGridItem (held.sample, qz))
                SlotBarTests.withMainWindow tried (fun window _ ->
                    let generate = SlotBarTests.controlById window ConstructorUiId.setupGridGenerateButton
                    let close = SlotBarTests.controlById window ConstructorUiId.setupGridCloseButton
                    Assert.Equal("Generate", SlotBarTests.boxLabelOf window ConstructorUiId.setupGridGenerateButton)
                    let generateCorner = SlotBarTests.topLeftOf window generate
                    let closeCorner = SlotBarTests.topLeftOf window close
                    Assert.True(abs (generateCorner.Y - closeCorner.Y) < 0.5, $"Generate {generateCorner} and Close {closeCorner} must share a row")
                    let generateRight = generateCorner.X + generate.Bounds.Width
                    Assert.True(closeCorner.X > generateRight, $"Close's left {closeCorner.X} must be beyond Generate's right {generateRight}")
                    Assert.Equal(MaterialsWindowView.positiveBackground, fillOf window ConstructorUiId.setupGridGenerateButton)
                    Assert.Equal(MaterialsWindowView.negativeBackground, fillOf window ConstructorUiId.setupGridCloseButton))

                // Its one combination unticked, the grid has nothing to generate.
                let only =
                    match tried.setupGrid with
                    | Some grid -> Assert.Single(grid.combinations)
                    | None -> failwith "The grid panel must be open."
                SlotBarTests.withMainWindow (tried |> update (ToggleSetupGridCombination only)) (fun window _ ->
                    Assert.StartsWith("☐ ", lineText (List.exactlyOne (combinationLines window)))
                    SlotBarTests.assertAbsent window ConstructorUiId.setupGridGenerateButton)

                SlotBarTests.withMainWindow tried (fun window answered ->
                    SlotBarTests.pressBox window ConstructorUiId.setupGridCloseButton
                    SlotBarTests.assertAbsent window ConstructorUiId.setupGridPanel
                    Assert.Equal<SetupGridDraft option>(None, (SlotBarTests.latestOf tried answered).setupGridOpt))))

    [<Fact>]
    let ``a kit opened read-only offers the grid but no Generate`` () : unit =
        HeadlessSession.run (fun () ->
            SlotBarTests.withScene (fun scene ->
                let held = gridTable scene
                let qz = SlotBarTests.itemLabelled (ItemLabel "QZ") held.table
                let readOnlyProxy = held.table.kit.proxy.readOnlyView
                let readOnly =
                    {
                        held.table with
                            kit =
                                {
                                    held.table.kit with
                                        proxy = readOnlyProxy
                                        access = ReadOnlyKitAccess
                                }
                            experimentData = ExperimentDataStore.createFileBacked readOnlyProxy
                            kitItems = readOnlyProxy.readItems () |> SceneTestFixture.kitSuccess
                    }
                let gridOpen = readOnly |> update OpenSetupGrid |> update (ToggleSetupGridItem (held.sample, qz))
                Assert.Equal<ControlCaption option>(None, gridOpen.kitStatus)
                SlotBarTests.withMainWindow gridOpen (fun window _ ->
                    SlotBarTests.controlById window ConstructorUiId.setupGridOpenButton |> ignore
                    SlotBarTests.controlById window ConstructorUiId.setupGridPanel |> ignore
                    Assert.Equal(MaterialsWindowView.positiveBackground, fillOf window (ConstructorUiId.setupGridItemChoice objectRole qz))
                    Assert.Equal<string list>(
                        [ $"☑ Obj 1: QZ @ {r1Of held.sample gridOpen}" ],
                        combinationLines window |> List.map lineText)
                    SlotBarTests.assertAbsent window ConstructorUiId.setupGridGenerateButton
                    Assert.Equal("Close", SlotBarTests.boxLabelOf window ConstructorUiId.setupGridCloseButton))))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``pressing Setup grid, items, an angle and Generate records the grid's setups in the kit`` () : unit =
        HeadlessSession.run (fun () ->
            SlotBarTests.withScene (fun scene ->
                let held = gridTable scene
                let itemOf (label : string) : ItemId = SlotBarTests.itemLabelled (ItemLabel label) held.table
                SlotBarTests.withMainWindow held.table (fun window _ ->
                    SlotBarTests.assertAbsent window ConstructorUiId.setupGridPanel
                    SlotBarTests.pressBox window ConstructorUiId.setupGridOpenButton
                    SlotBarTests.pressBox window (ConstructorUiId.setupGridItemChoice objectRole (itemOf "QZ"))
                    SlotBarTests.pressBox window (ConstructorUiId.setupGridItemChoice objectRole (itemOf "QZ2"))
                    let field = Assert.IsType<TextBox>(SlotBarTests.controlById window (ConstructorUiId.setupGridAngleField objectRole))
                    Assert.True(field.Focus(), "Obj 1's angle field must take the focus")
                    Dispatcher.UIThread.RunJobs()
                    window.KeyTextInput "45"
                    Dispatcher.UIThread.RunJobs()
                    window.KeyPressQwerty(PhysicalKey.Enter, RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    window.KeyReleaseQwerty(PhysicalKey.Enter, RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    Assert.True(String.IsNullOrEmpty field.Text, $"the read angle must clear the field, not leave '{field.Text}'")
                    Assert.Equal("45° ×", SlotBarTests.boxLabelOf window (ConstructorUiId.setupGridAngle objectRole (triedAt 45.0)))
                    SlotBarTests.pressBox window ConstructorUiId.setupGridGenerateButton)
                let names =
                    held.table.kit.proxy.readSetups ()
                    |> SceneTestFixture.kitSuccess
                    |> List.map (fun setup -> setup.name.value)
                    |> List.sort
                Assert.Equal<string list>([ "LP1-(QZ#45)-LP2"; "LP1-(QZ2#45)-LP2" ], names)))
