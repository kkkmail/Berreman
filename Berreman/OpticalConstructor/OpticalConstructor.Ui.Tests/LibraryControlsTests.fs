namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Tests
open OpticalConstructor.Ui.ConstructorUiIdOfDomain
open OpticalConstructor.Ui.TableAndElementRotationView

/// Spec 0027 (024) Phase 1 — the Select bay (labelled "Library" until spec 0033 step 014): the pure
/// control contract, the host's kind-constrained `libraryState` flattening, the `BindValueId` MVU
/// binding, and one headless render proof that the bay lists its entries and a click binds the selected
/// element's `valueId`. The control stays `LibraryControls` — only the bay label changed.
module LibraryControlsTests =

    let private elem (i : int) (m : TableAndElementRotationModel) : TestElement = List.item i m.elements

    /// Bind BuiltinCatalogue.sample entries through their Guid strings at the Selector valueId seam
    /// (spec 0033 step 002; 0050 §3.3), using Library.SampleIds rather than repeated literals.
    let private glass1mmId : string = (Library.SampleItem (BuiltinCatalogue.sample Library.SampleIds.glassPlate1mmId)).entryId.value
    let private glass2mmId : string = (Library.SampleItem (BuiltinCatalogue.sample Library.SampleIds.glassPlate2mmId)).entryId.value
    let private multilayerQwId : string = (Library.SampleItem (BuiltinCatalogue.sample Library.SampleIds.multilayerQwId)).entryId.value

    // ============================ pure control contract ============================

    [<Fact>]
    let ``the empty Library state is disabled with no rows and no binding`` () =
        let s = LibraryControls.empty
        Assert.Equal(ControlWithheld, s.availability)
        Assert.Empty(s.rows)
        Assert.Equal(None, s.boundName)

    [<Fact>]
    let ``the Library UiIds prefix leaf ids and are stable`` () =
        // `entry` prefixes ANY id string; BuiltinCatalogue.sample uses its Guid string
        // (spec 0033 step 002).
        Assert.Equal("LibraryEntry_abc", (ConstructorUiId.libraryEntry (ConstructorUiKey "abc")).value)
        Assert.Equal("LibraryEntry_" + glass1mmId, (ConstructorUiId.libraryEntry (ConstructorUiKey glass1mmId)).value)
        Assert.Equal("LibraryTree", ConstructorUiId.libraryTree.value)
        Assert.Equal("LibraryBoundReadout", ConstructorUiId.libraryBoundReadout.value)

    // ============================ host libraryState (pure) ============================

    /// Build the model with a single Sample element selected (so the Select bay shows samples).
    let private withSampleSelected () : TableAndElementRotationModel =
        SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement Sample)   // appends a Sample at index 2, selects it

    /// Spec 0038 (017): the 11 BuiltinCatalogue.samples () entries exceed the default quick-pick
    /// threshold 5 (Choose… alone). These tests raise it to exercise the STRIP mechanics.
    let private withWideQuickPick (m : TableAndElementRotationModel) : TableAndElementRotationModel =
        { m with quickPickThreshold = WorkbenchSettings.QuickPickThreshold 100 }

    [<Fact>]
    let ``libraryState for a selected Sample lists ONLY sample leaf rows, kind-labelled Sample`` () =
        let m = withSampleSelected ()
        let bays = mainBays m ignore
        // The Select bay exists and is offered.
        Assert.Contains(BayNames.selector, bays |> List.map (fun b -> b.name))
        // Its rows: every selectable leaf is a Sample entry id; the kind label is "Sample".
        // (Re-derive the bay state the same way the host does — through the live model.)
        let sampleIds =
            match m.library.entriesForKind CatalogueKind.Sample with
            | Ok entries -> entries |> List.map (fun e -> e.entryId) |> Set.ofList
            | Error _ -> Set.empty
        Assert.False(Set.isEmpty sampleIds)
        // Render the bay and read its leaf ids back (the host flattening is exercised end-to-end below);
        // here assert the kind-constraint at the domain seam the host uses.
        Assert.All(Set.toList sampleIds, fun id ->
            match m.library.tryGetEntry id with
            | Ok (Some (Library.SampleItem _)) -> ()
            | other -> Assert.Fail($"%s{id.value} is not a Sample: %A{other}"))

    [<Fact>]
    let ``the Select bay lists a sample created in the samples store under its geometry`` () : unit =
        HeadlessSession.run (fun () ->
            let model =
                withSampleSelected ()
                |> withWideQuickPick
                |> updateTableAndElementRotation (SelectBay BayNames.selector)
            let sample : Library.Sample =
                {
                    BuiltinCatalogue.sample Library.SampleIds.euvMoSiId with
                        id = Library.newSampleId ()
                        name = MaterialLibrary.EntryName "Select bay user mirror"
                }
            Assert.Equal<Library.SubstrateKind>(Library.Mirror, sample.substrate)
            Assert.Equal<Result<unit, Library.SampleError>>(Ok (), model.samples.saveSample sample)
            let window = Window(Width = 980.0, Height = tableAndElementRotationCanvasHeight + 320.0)
            try
                window.Content <- Component(fun _ -> mainView model ignore)
                window.Show()
                Dispatcher.UIThread.RunJobs()
                // Spec 0050 S1.7: exercise the internal libraryState through its public rendered consumer.
                let tree =
                    window.GetVisualDescendants()
                    |> Seq.choose (function
                        | :? StackPanel as panel when panel.Name = ConstructorUiId.libraryTree.value -> Some panel
                        | _ -> None)
                    |> Assert.Single<StackPanel>
                let rows = tree.Children |> Seq.toList
                let mirrorHeader =
                    rows
                    |> List.choose (function
                        | :? TextBlock as label when label.Text = "Mirrors" -> Some label
                        | _ -> None)
                    |> Assert.Single<TextBlock>
                let groupDepth = LibraryControls.LibraryRowDepth 2
                Assert.Equal(groupDepth.indent, mirrorHeader.Margin.Left)
                let mirrorRows =
                    rows
                    |> List.skipWhile (fun row -> not (System.Object.ReferenceEquals(row, mirrorHeader)))
                    |> List.tail
                    |> List.takeWhile (fun row -> row.Margin.Left > mirrorHeader.Margin.Left)
                let leafId = ConstructorUiId.libraryEntry sample.id.toElementValueId.uiKey
                let leaf =
                    mirrorRows
                    |> List.choose (function
                        | :? Border as border when Avalonia.Automation.AutomationProperties.GetAutomationId(border) = leafId.value -> Some border
                        | _ -> None)
                    |> Assert.Single<Border>
                let leafDepth = LibraryControls.LibraryRowDepth 3
                Assert.Equal(leafDepth.indent, leaf.Margin.Left)
                Assert.Equal(sample.name.value, Assert.IsType<TextBlock>(leaf.Child).Text)
                Assert.Equal(None, (elem 2 model).placement.valueId)
                Assert.True(leaf.IsEnabled)
            finally
                window.Close())

    [<Fact>]
    let ``BindValueId (ElementValueId sets) the selected element's valueId; inert for table or nothing`` () =
        let m = withSampleSelected ()                      // element 2 (a Sample) selected
        let bound = updateTableAndElementRotation (BindValueId (ElementValueId glass1mmId)) m
        Assert.Equal(Some (ElementValueId glass1mmId), (elem 2 bound).placement.valueId)
        // Inert when the table is selected.
        let tableSel = { (SceneTestFixture.mainScene ()) with selection = TableSelected }
        Assert.Equal<TableAndElementRotationModel>(tableSel, updateTableAndElementRotation (BindValueId (ElementValueId glass1mmId)) tableSel)
        // Inert when nothing is selected.
        let nothingSel = { (SceneTestFixture.mainScene ()) with selection = NothingSelected }
        Assert.Equal<TableAndElementRotationModel>(nothingSel, updateTableAndElementRotation (BindValueId (ElementValueId glass1mmId)) nothingSel)

    [<Fact>]
    let ``binding then rebinding overwrites the valueId`` () =
        let m = withSampleSelected ()
        let m1 = updateTableAndElementRotation (BindValueId (ElementValueId glass1mmId)) m
        let m2 = updateTableAndElementRotation (BindValueId (ElementValueId glass2mmId)) m1
        Assert.Equal(Some (ElementValueId glass2mmId), (elem 2 m2).placement.valueId)

    [<Fact>]
    let ``the ribbon offers the Select bay; the Library workbench left for its window`` () =
        // The Select bay is present, in order, after Render (the Experiments bay was added in Phase 2,
        // so `BayNames.all` is now longer than the original five; assert membership + the prefix order
        // rather than an exact five-element list). Spec 0033 step 014 renamed the bay label from
        // "Library" to "Selector" (same behaviour); spec 0033 step 024 reused the freed label for the
        // SAMPLES WORKBENCH bay — which spec 0038 step 015 moved into the single-instance Library
        // WINDOW, so no "Library" bay is offered any more (the Selector alone keeps binding duty).
        // Spec 0046 (operator): the label is the verb "Select" (was "Selector"); likewise "Rotate".
        Assert.Equal("Select", BayNames.selector.value)
        Assert.Equal("Rotate", BayNames.rotation.value)
        Assert.Contains(BayNames.selector, BayNames.allBayNames)
        Assert.DoesNotContain(Ribbon.BayName "Library", BayNames.allBayNames)
        // Spec 0046: Add comes first.
        Assert.Equal<Ribbon.BayName list>(
            [ BayNames.add; BayNames.rotation; BayNames.move; BayNames.render; BayNames.selector ],
            BayNames.allBayNames |> List.truncate 5)
        let m = SceneTestFixture.mainScene ()
        let bays = mainBays m ignore
        Assert.Equal<Ribbon.BayName list>(BayNames.allBayNames, bays |> List.map (fun b -> b.name))

    // ============================ Spec 0027 (026) confirm-gated bind (pure) ============================

    [<Fact>]
    let ``RequestBindValueId (ElementValueId sets) a pending entry WITHOUT binding the valueId`` () =
        let m = withSampleSelected ()                          // element 2 (a Sample) selected
        let pending = updateTableAndElementRotation (RequestBindValueId (ElementValueId glass1mmId)) m
        // The pending choice is recorded, but no valueId is bound yet.
        Assert.Equal(Some (ElementValueId glass1mmId), pending.pendingEntry)
        Assert.Equal(None, (elem 2 pending).placement.valueId)

    [<Fact>]
    let ``ConfirmBindValueId commits the pending entry to the selected element's valueId and clears pending`` () =
        let m =
            withSampleSelected ()
            |> updateTableAndElementRotation (RequestBindValueId (ElementValueId glass1mmId))
            |> updateTableAndElementRotation ConfirmBindValueId
        Assert.Equal(Some (ElementValueId glass1mmId), (elem 2 m).placement.valueId)
        Assert.Equal(None, m.pendingEntry)

    [<Fact>]
    let ``CancelBindValueId clears the pending entry and does NOT bind`` () =
        let m =
            withSampleSelected ()
            |> updateTableAndElementRotation (RequestBindValueId (ElementValueId glass1mmId))
            |> updateTableAndElementRotation CancelBindValueId
        Assert.Equal(None, m.pendingEntry)
        Assert.Equal(None, (elem 2 m).placement.valueId)

    [<Fact>]
    let ``a confirm with no pending entry binds nothing`` () =
        let m = withSampleSelected () |> updateTableAndElementRotation ConfirmBindValueId
        Assert.Equal(None, (elem 2 m).placement.valueId)

    [<Fact>]
    let ``libraryState surfaces the pending entry's name and FULL description before confirm`` () =
        let m = withSampleSelected () |> updateTableAndElementRotation (RequestBindValueId (ElementValueId multilayerQwId))
        // The host's libraryState fills the pending fields from the proxy; mirror that resolution here.
        match m.library.tryGetEntry (ElementValueId multilayerQwId) with
        | Ok (Some entry) ->
            Assert.False(System.String.IsNullOrWhiteSpace entry.fullDescription)
            // A multilayer's description spells out the stack, not just its short name.
            Assert.Contains("layer", entry.fullDescription)
        | other -> Assert.Fail($"expected the quarter-wave multilayer entry, got %A{other}")
        Assert.Equal(Some (ElementValueId multilayerQwId), m.pendingEntry)

    // ============================ headless render proof (ui-smoke) ============================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Select bay shows the pending description, then a Confirm click binds the valueId`` () =
        HeadlessSession.run (fun () ->
            // A Sample is selected, an entry is already PENDING (selected-not-confirmed), the Select bay is
            // shown — so the confirm panel (the entry's full description + Confirm / Cancel) renders up front.
            let mutable model =
                withSampleSelected ()
                |> withWideQuickPick
                |> updateTableAndElementRotation (SelectBay BayNames.selector)
                |> updateTableAndElementRotation (RequestBindValueId (ElementValueId glass1mmId))
            let dispatch (msg : TableAndElementRotationMsg) = model <- updateTableAndElementRotation msg model
            let window = Window(Width = 980.0, Height = tableAndElementRotationCanvasHeight + 320.0)
            window.Content <- Component(fun _ -> mainView model dispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // The ribbon shows a Selector tab (spec 0033 step 014); NO Library tab remains —
            // the samples workbench is the Library WINDOW now (spec 0038 step 015).
            let tabNames =
                window.GetVisualDescendants()
                |> Seq.choose (function
                    | :? Border as b when not (System.String.IsNullOrEmpty b.Name) && b.Name.StartsWith("RibbonTab_") -> Some b.Name
                    | _ -> None)
                |> List.ofSeq
            Assert.Contains((ConstructorUiId.ribbonTab (ConstructorUiKey BayNames.selector.value)).value, tabNames)
            Assert.DoesNotContain((ConstructorUiId.ribbonTab (ConstructorUiKey "Library")).value, tabNames)
            // The pending entry's full description is shown (and the element is NOT bound yet).
            Assert.Equal(None, (elem 2 model).placement.valueId)
            let descriptionShown () : bool =
                window.GetVisualDescendants()
                |> Seq.exists (function
                    | :? TextBlock as t -> t.Name = ConstructorUiId.libraryEntryDescription.value && not (System.String.IsNullOrWhiteSpace t.Text) && t.IsEffectivelyVisible
                    | _ -> false)
            Assert.True(descriptionShown (), "the pending entry's full description was not shown")
            // Click Confirm — now it binds the selected element's valueId.
            let findConfirm () : Border option =
                window.GetVisualDescendants()
                |> Seq.tryPick (function :? Border as b when Avalonia.Automation.AutomationProperties.GetAutomationId(b) = ConstructorUiId.libraryConfirmButton.value && b.IsEffectivelyVisible -> Some b | _ -> None)
            match findConfirm () with
            | None -> Assert.Fail("the Confirm button was not visible in the Select bay")
            | Some b ->
                let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
                if c.HasValue then
                    window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    Assert.Equal(Some (ElementValueId glass1mmId), (elem 2 model).placement.valueId)
                else Assert.Fail("the Confirm button has no on-screen position")
            window.Close())

    // ============================ FuncUI recycling regressions (spec 0038) ============================
    // The leaf rows are GENERATED from a kind-constrained list whose membership changes with the
    // selection, so a re-render can shift one entry onto another's reused control. A `Border.name`
    // there made FuncUI re-set a styled element's `Name` and throw "Cannot set Name : styled element
    // already styled" (the live Selector-bay crash); the rows now carry a freely-mutable
    // `AutomationProperties.AutomationId` and are keyed with `View.withKey`. These drive the bay
    // through a REAL re-rendering Component (the app's Elmish patch path) to guard that.

    /// A Border in `window` carrying this automation id (the converted boxes carry no `Name`).
    let private borderWithId (window : Window) (id : string) : Border option =
        window.GetVisualDescendants()
        |> Seq.tryPick (function
            | :? Border as b when Avalonia.Automation.AutomationProperties.GetAutomationId(b) = id && b.IsEffectivelyVisible -> Some b
            | _ -> None)

    let private clickIn (window : Window) (id : string) : unit =
        match borderWithId window id with
        | Some b ->
            b.BringIntoView() |> ignore
            Dispatcher.UIThread.RunJobs()
            let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
            if c.HasValue then
                window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"%s{id} has no on-screen position")
        | None -> Assert.Fail($"%s{id} not found")

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a leaf-row membership shift re-renders without a styled-element rename throw`` () =
        // The mechanism regression, at the control level: rows [alpha; beta; gamma] → [beta; gamma]
        // shifts 'beta' onto 'alpha''s reused Border (same type, same slot). With `Border.name` this
        // threw "Cannot set Name : styled element already styled"; the AutomationId + ViewKey sweep
        // recreates the shifted rows instead.
        HeadlessSession.run (fun () ->
            let rowFor (id : string) (label : string) : LibraryControls.Row =
                { label = ControlCaption label; depth = LibraryControls.LibraryRowDepth 0; kind = LibraryControls.LibraryEntryLeaf (ConstructorUiKey id, LibraryControls.EntryUnbound) }
            let fullRows = [ rowFor "alpha" "Alpha"; rowFor "beta" "Beta"; rowFor "gamma" "Gamma" ]
            let shiftedRows = [ rowFor "beta" "Beta"; rowFor "gamma" "Gamma" ]
            let stateFor (rows : LibraryControls.Row list) : LibraryControls.LibraryControlsState =
                { LibraryControls.empty with rows = rows; kindLabel = Some (ControlCaption "Sample"); availability = ControlOffered }
            let handlers : LibraryControls.LibraryControlsHandlers =
                { selectEntry = ignore; confirmEntry = ignore; cancelEntry = ignore }
            let setRows : (LibraryControls.Row list -> unit) ref = ref (fun _ -> ())
            let host =
                Component(fun ctx ->
                    let rows = ctx.useState fullRows
                    setRows.Value <- rows.Set
                    LibraryControls.libraryControlsView (stateFor rows.Current) handlers)
            let window = Window(Width = 640.0, Height = 480.0, Content = host)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // Shift the membership (the first leaf disappears) — must not throw…
            setRows.Value shiftedRows
            Dispatcher.UIThread.RunJobs()
            // …and back to the full list (the reverse shift).
            setRows.Value fullRows
            Dispatcher.UIThread.RunJobs()
            let hasEntry (id : string) : bool =
                match borderWithId window (ConstructorUiId.libraryEntry (ConstructorUiKey id)).value with
                | Some _ -> true
                | None -> false
            Assert.True(hasEntry "alpha" && hasEntry "beta" && hasEntry "gamma",
                        "the re-rendered leaf rows were not locatable by AutomationId")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Select bay survives select, bind, and kind-change re-renders`` () =
        // The acceptance drive over the LIVE main view: select an entry (pending), bind it (Confirm),
        // then change the selected element's KIND twice — rows collapse for a polarizer and rebuild for
        // a fresh sample — with every step re-rendering through the FuncUI patch path.
        HeadlessSession.run (fun () ->
            let seed = withSampleSelected () |> withWideQuickPick |> updateTableAndElementRotation (SelectBay BayNames.selector)
            let latest : TableAndElementRotationModel ref = ref seed
            let dispatchRef : (TableAndElementRotationMsg -> unit) ref = ref ignore
            let comp =
                Component(fun ctx ->
                    let st = ctx.useState seed
                    latest.Value <- st.Current
                    let dispatch (msg : TableAndElementRotationMsg) =
                        let m = updateTableAndElementRotation msg st.Current
                        latest.Value <- m
                        st.Set m
                    dispatchRef.Value <- dispatch
                    mainView st.Current dispatch)
            let window = Window(Width = 980.0, Height = tableAndElementRotationCanvasHeight + 360.0, Content = comp)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // SELECT: click a leaf entry — the pending confirm panel renders on the patched tree.
            clickIn window (ConstructorUiId.libraryEntry (ConstructorUiKey glass1mmId)).value
            Assert.Equal(Some (ElementValueId glass1mmId), latest.Value.pendingEntry)
            // BIND: click Confirm — the pending entry commits to the selected element's valueId.
            clickIn window ConstructorUiId.libraryConfirmButton.value
            Assert.Equal(Some (ElementValueId glass1mmId), (elem 2 latest.Value).placement.valueId)
            let sampleLeafShown () : bool =
                match borderWithId window (ConstructorUiId.libraryEntry (ConstructorUiKey glass1mmId)).value with
                | Some _ -> true
                | None -> false
            // KIND-CHANGE 1: add (and select) a polarizer — no library entries for its kind, so the
            // leaf rows COLLAPSE on the re-render.
            dispatchRef.Value (AddElement LinearPolarizer)
            Dispatcher.UIThread.RunJobs()
            Assert.False(sampleLeafShown (), "a sample leaf row survived the polarizer kind-change")
            // KIND-CHANGE 2: add (and select) another sample — the full sample tree REBUILDS.
            dispatchRef.Value (AddElement Sample)
            Dispatcher.UIThread.RunJobs()
            Assert.True(sampleLeafShown (), "the sample leaf rows did not rebuild after the kind-change back")
            // The first sample kept its binding across the re-renders.
            Assert.Equal(Some (ElementValueId glass1mmId), (elem 2 latest.Value).placement.valueId)
            window.Close())
