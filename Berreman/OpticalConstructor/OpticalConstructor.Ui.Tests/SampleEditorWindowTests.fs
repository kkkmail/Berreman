namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open Xunit
open Berreman.Constants
open Berreman.Media
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.MaterialStore
open OpticalConstructor.Domain.SampleStore
open OpticalConstructor.Domain.SampleStackEditor
open OpticalConstructor.Domain.WindowMode
open OpticalConstructor.Controls
open OpticalConstructor.Ui
open OpticalConstructor.Ui.SampleEditorView

/// Spec 0033 (022) — the SampleEditorWindow component (UICOMP_XDUO_0003): the Sample editor
/// window in OpticalConstructor.Ui over the step-21 Domain `SampleStackEditor`.
/// Two layers, the 016 precedent: pure contract tests for the model / update / helpers, and
/// headless semantic-tree proofs that DRIVE THE REAL WINDOW BY ITS UiIds — the slice
/// acceptance: repeating a 2-layer selection K times shows a structure expanding to `2*K`
/// films, select-by-material bulk set-thickness updates only the matching layers, and Save
/// persists through `SampleProxy` (add for new, update for existing). Spec 0038 step 019
/// adds the material-picking suite: the per-layer Choose material… verb opens (or re-targets)
/// the REAL Materials window in Select state targeted at the row's `LayerPosition`, the
/// return binds THAT row (and becomes the toolbar's chosen material), a deleted row makes the
/// return a no-op plus the status line, and the material list re-queries the live proxy on
/// every Select return and window activation.
module SampleEditorWindowTests =

    /// A control matches `id` by its `Name` OR its `AutomationProperties.AutomationId` (the
    /// stack rows / material options / steppers live in variable-membership lists, so they
    /// carry an AutomationId — the SampleLibraryControls precedent).
    let private matchesId (id : string) (c : Control) : bool =
        c.Name = id || Avalonia.Automation.AutomationProperties.GetAutomationId(c) = id

    let private tryFindControl (window : Window) (id : string) : Control option =
        window.GetVisualDescendants()
        |> Seq.tryPick (function :? Control as c when matchesId id c -> Some c | _ -> None)

    let private isPresent (window : Window) (id : string) : bool =
        match tryFindControl window id with
        | Some _ -> true
        | None -> false

    /// Click the centre of the clickable Border carrying `id` (by Name or AutomationId).
    let private clickOn (window : Window) (id : string) : unit =
        let found =
            window.GetVisualDescendants()
            |> Seq.tryPick (function :? Border as b when matchesId id b && b.IsEffectivelyVisible -> Some b | _ -> None)
        match found with
        | None -> Assert.Fail($"%s{id} was not found (or not visible)")
        | Some b ->
            // Spec 0046: scroll the box into view first (the facet rows now WRAP, so the tree can sit below
            // the visible area of its scroller, where a click at the box's centre lands nowhere).
            b.BringIntoView() |> ignore
            Dispatcher.UIThread.RunJobs()
            let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
            if c.HasValue then
                window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                // A Save / Cancel click closes the window during the press — skip the release then.
                if window.IsVisible then
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"%s{id} has no on-screen position")

    /// Click the centre of ANY control carrying `id` (not only a Border) — a CheckBox toggles and
    /// raises its Click on this pointer press/release exactly as a user interaction does (the
    /// emission R/T boxes, spec 0040 Part D.2 step 010).
    let private clickControl (window : Window) (id : string) : unit =
        let found =
            window.GetVisualDescendants()
            |> Seq.tryPick (function :? Control as c when matchesId id c && c.IsEffectivelyVisible -> Some c | _ -> None)
        match found with
        | None -> Assert.Fail($"%s{id} was not found (or not visible)")
        | Some c ->
            let p = c.TranslatePoint(Point(c.Bounds.Width / 2.0, c.Bounds.Height / 2.0), window)
            if p.HasValue then
                window.MouseDown(p.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                window.MouseUp(p.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"%s{id} has no on-screen position")

    /// The CheckBox carrying `id` (fails loudly when absent or a different control) — the tests
    /// read its `IsChecked` / `IsEnabled` directly.
    let private checkBox (window : Window) (id : string) : CheckBox =
        match tryFindControl window id with
        | Some (:? CheckBox as cb) -> cb
        | Some c -> failwith $"%s{id} is a %s{c.GetType().Name}, not a CheckBox"
        | None -> failwith $"%s{id} was not found"

    /// Set the text of the TextBox carrying `id` (fires the property-change subscription the
    /// view's `onTextChanged` binds — still driving the control found by its UiId).
    let private setText (window : Window) (id : string) (text : string) : unit =
        match tryFindControl window id with
        | Some (:? TextBox as tb) ->
            tb.Text <- text
            Dispatcher.UIThread.RunJobs()
        | Some c -> Assert.Fail($"%s{id} is a %s{c.GetType().Name}, not a TextBox")
        | None -> Assert.Fail($"%s{id} was not found")

    /// The display text under the control carrying `id` (the control itself when it is a
    /// TextBlock, its first TextBlock descendant otherwise — a layer row's first block is its
    /// material name).
    let private textOf (window : Window) (id : string) : string =
        match tryFindControl window id with
        | None -> ""
        | Some (:? TextBlock as t) -> (if isNull t.Text then "" else t.Text)
        | Some c ->
            c.GetVisualDescendants()
            |> Seq.tryPick (function :? TextBlock as t when not (isNull t.Text) -> Some t.Text | _ -> None)
            |> Option.defaultValue ""

    /// Commit `text` through the REAL faceted filter box of a Materials window (the control
    /// commits on Enter — the MaterialsWindowTests driving shape).
    let private commitFilter (window : Window) (text : string) : unit =
        match tryFindControl window ConstructorUiId.facetFilterBox.value with
        | Some (:? TextBox as tb) ->
            tb.Focus() |> ignore
            Dispatcher.UIThread.RunJobs()
            tb.Text <- text
            Dispatcher.UIThread.RunJobs()
            window.KeyPressQwerty(PhysicalKey.Enter, RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
            window.KeyReleaseQwerty(PhysicalKey.Enter, RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
        | Some c -> Assert.Fail($"the filter box is a %s{c.GetType().Name}, not a TextBox")
        | None -> Assert.Fail("the filter box was not found")

    let private close (a : float) (b : float) : bool = abs (a - b) <= 1.0e-9

    /// Fresh, isolated in-memory stores per test — the SAME composition the App performs
    /// (samples first, then materials whose remove-block consults the LIVE samples, then
    /// categories whose remove-block consults the LIVE materials).
    let private freshProxies () : MaterialProxy * SampleProxy * CategoryProxy =
        let samples = SampleProxy.createInMemory VersionsInUse.empty
        let materials = MaterialProxy.createInMemory (samplesReferencing samples) VersionsInUse.empty
        let categories = CategoryProxy.createInMemory (materialsReferencingCategory materials)
        materials, samples, categories

    let private builtIn (id : MaterialId) : MaterialEntry =
        builtInEntries |> List.find (fun e -> e.id = id)

    /// A stub materials read-seam over the STATIC built-in list (the pure tests' re-query
    /// source; the write surface is inert).
    let private stubMaterialsProxy : MaterialProxy =
        {
            listMaterials = fun _ -> Ok builtInEntries
            searchMaterials = fun _ -> Ok builtInEntries
            tryGetMaterial = fun id -> Ok (builtInEntries |> List.tryFind (fun e -> e.id = id))
            resolveVersion = fun mvid -> Ok (builtInEntries |> List.tryFind (fun e -> e.id = mvid.materialId))
            saveMaterial = fun _ -> Ok ()
            markMaterialInactive = fun _ -> Ok ()
            markMaterialActive = fun _ -> Ok ()
            supersedeMaterial = fun _ -> Ok ()
            removeMaterial = fun _ -> Ok ()
        }

    /// A recording stub context (the functional-proxy seam — the test substitutes in-memory
    /// stubs of the SAME shape and observes which proxy function a Save/Cancel reached, and
    /// whether a Choose material… verb requested the Materials Select window).
    let private recordingContext () : ResizeArray<string> * SampleEditorContext =
        let calls = ResizeArray<string>()
        let stub : SampleProxy =
            {
                listSamples = fun _ -> Ok []
                searchSamples = fun _ -> Ok []
                tryGetSample = fun _ -> Ok None
                resolveVersion = fun _ -> Ok None
                // Spec 0038 step 022: the ONE versioned `saveSample` records the Save (the mirror of
                // the material editor's collapse — `addSample`/`updateSample` are gone).
                saveSample = fun s -> calls.Add("save:" + s.name.value); Ok ()
                markSampleInactive = fun _ -> Ok ()
                markSampleActive = fun _ -> Ok ()
                supersedeSample = fun _ -> Ok ()
                removeSample = fun _ -> Ok ()
            }
        calls,
        {
            materials = stubMaterialsProxy
            samples = stub
            openMaterialsSelect = fun _ _ -> calls.Add "materials-select-open"
            requestClose = fun () -> calls.Add "close"
        }

    let private nmT (t : float) : Thickness = Thickness.nm (t * 1.0<nm>)

    // The layer pins version one of the material (spec 0038 step 022 — a `SampleLayer` carries a
    // `MaterialVersionId`); `.materialId.materialId` recovers the identity in the assertions below.
    let private layerOf (materialId : MaterialId) (thicknessNm : float) : SampleLayer =
        { materialId = MaterialVersionId.firstOf materialId; thickness = nmT thicknessNm; orientation = PrimaryAxes }

    /// films = [ glass 100 nm; vacuum 50 nm; glass 100 nm ] — the select-by-material shape.
    let private threeFilmSample () : Sample =
        {
            id = newSampleId ()
            name = EntryName "Three films"
            structure =
                {
                    films =
                        [
                            SingleLayer (layerOf MaterialIds.glass152 100.0)
                            SingleLayer (layerOf MaterialIds.vacuumMaterial 50.0)
                            SingleLayer (layerOf MaterialIds.glass152 100.0)
                        ]
                    substrate = None
                    lower = None
                }
            substrate = ThinFilm
            description = EntryDescription "test three-film stack"
            supportedEmission = ThinFilm.defaultSupportedEmission
        }

    /// A `Plate` variant of the three-film sample (geometry `Plate`, default `EmitBoth`) — the
    /// emission-editing fixture (spec 0040 Part D.2 step 010): a Plate exposes the R and T
    /// checkboxes, a ThinFilm pins R on.
    let private threeFilmPlate () : Sample =
        { threeFilmSample () with substrate = ThickPlate; supportedEmission = ThickPlate.defaultSupportedEmission }

    /// The same stack as a MIRROR — spec 0046: the only geometry pinned to reflected-only.
    let private threeFilmMirror () : Sample =
        { threeFilmSample () with substrate = Mirror; supportedEmission = Mirror.defaultSupportedEmission }

    let private newModel () : SampleEditorModel =
        let _, context = recordingContext ()
        // The Add-open shape (spec 0038 step 008): the id is minted AT WINDOW OPEN.
        initSampleEditor context builtInEntries (NewBlankSample (newSampleId ()))

    // ============================ pure control contract ============================

    [<Fact>]
    let ``the Sample editor UiIds are the slice-mandated stable ids`` () =
        Assert.Equal("SampleEditorWindow", ConstructorUiId.sampleEditorWindow.value)
        Assert.Equal("SampleNameBox", ConstructorUiId.sampleNameBox.value)
        Assert.Equal("AddLayerButton", ConstructorUiId.addLayerButton.value)
        Assert.Equal("MakeRepeatBlockButton", ConstructorUiId.makeRepeatBlockButton.value)
        Assert.Equal("SelectByMaterialButton", ConstructorUiId.selectByMaterialButton.value)
        Assert.Equal("SetLayerHeightButton", ConstructorUiId.setLayerHeightButton.value)
        Assert.Equal("SetLayerMaterialButton", ConstructorUiId.setLayerMaterialButton.value)
        Assert.Equal("SetOrientationOfSelectedButton", ConstructorUiId.setOrientationOfSelectedButton.value)
        Assert.Equal("RemoveSelectedLayersButton", ConstructorUiId.removeSelectedLayersButton.value)
        Assert.Equal("RepeatCountStepper", ConstructorUiId.repeatCountStepper.value)
        Assert.Equal("QwotEntryBox", ConstructorUiId.qwotEntryBox.value)
        Assert.Equal("SampleEditorSaveButton", ConstructorUiId.sampleEditorSaveButton.value)
        Assert.Equal("SampleEditorCancelButton", ConstructorUiId.sampleEditorCancelButton.value)
        // The supported-emission R/T checkboxes (spec 0040 Part D.2 step 010).
        Assert.Equal("SampleEmitReflectedCheck", ConstructorUiId.sampleEmitReflectedCheck.value)
        Assert.Equal("SampleEmitTransmittedCheck", ConstructorUiId.sampleEmitTransmittedCheck.value)
        // The substrate Set… verb reuses this id (spec 0040 Part D.4 step 011 — it replaced the
        // old chosen-gated 'Set from chosen' substrate button as the substrate pick path).
        Assert.Equal("SetSubstrateButton", ConstructorUiId.setSubstrateButton.value)
        // The derived per-row / per-group id families are prefixed so they cannot collide.
        Assert.Equal("SampleLayerRow_1", (ConstructorUiId.sampleLayerRow (ConstructorUiPosition 1)).value)
        Assert.Equal("SampleLayerRow_0_1", (ConstructorUiId.sampleCellLayerRow (ConstructorUiPosition 0) (ConstructorUiPosition 1)).value)
        Assert.Equal("SampleGroupRow_0", (ConstructorUiId.sampleGroupRow (ConstructorUiPosition 0)).value)
        Assert.Equal("RepeatCountStepperPlus_0", (ConstructorUiId.repeatCountStepperPlusOfGroup (ConstructorUiPosition 0)).value)
        // The per-layer Choose material… verb family (spec 0038 step 019).
        Assert.Equal("ChooseMaterialButton_1", (ConstructorUiId.chooseMaterialButton (ConstructorUiPosition 1)).value)
        Assert.Equal("ChooseMaterialButton_0_1", (ConstructorUiId.cellChooseMaterialButton (ConstructorUiPosition 0) (ConstructorUiPosition 1)).value)

    [<Fact>]
    let ``a new sample opens empty: thin film, no films, nothing selected, fold count 2`` () =
        // The Add path (spec 0038 step 008): the upfront-minted id rides the target as a
        // NewUnsaved entry — the SAME id the launcher's registry keys the window by.
        let minted = newSampleId ()
        let _, context = recordingContext ()
        let m = initSampleEditor context builtInEntries (NewBlankSample minted)
        Assert.Equal(EntryName "", m.name)
        Assert.Equal(EntryDescription "", m.description)
        Assert.Equal(ThinFilm, m.substrate)
        Assert.Empty(m.editor.structure.films)
        Assert.Empty(m.editor.selection)
        Assert.Equal(PeriodCount 2, m.foldCount)
        Assert.Equal({ sampleId = minted; freshness = WindowLauncher.NewUnsaved }, m.target)
        match m.status with
        | None -> ()
        | Some s -> Assert.Fail($"expected no status, got '%s{s.value}'")

    [<Fact>]
    let ``an existing sample seeds the editor and save targets its id`` () =
        let sample = threeFilmSample ()
        let _, context = recordingContext ()
        let m = initSampleEditor context builtInEntries (EditSample sample)
        Assert.Equal(sample.name, m.name)
        Assert.Equal(sample.description, m.description)
        Assert.Equal(sample.substrate, m.substrate)
        Assert.Equal<SampleStructure>(sample.structure, m.editor.structure)
        Assert.Equal({ sampleId = sample.id; freshness = WindowLauncher.Persisted }, m.target)

    [<Fact>]
    let ``Make-multilayer opens a NEW sample seeded with a foldable 2-layer period, distinct from the blank Add`` () =
        let _, context = recordingContext ()
        let minted = newSampleId ()
        let seeded = initSampleEditor context builtInEntries (NewSeededMultilayer minted)
        // A NEW sample under the id minted at open (Save routes addSample on the NewUnsaved
        // freshness — spec 0038 step 008), NOT an existing one to update in place.
        Assert.Equal({ sampleId = minted; freshness = WindowLauncher.NewUnsaved }, seeded.target)
        Assert.Equal(EntryName "", seeded.name)
        // Exactly ONE Repeated period group of a 2-layer cell — the foldable starter the K-stepper
        // (SetRepeatCount) then builds up into a full stack.
        match seeded.editor.structure.films with
        | [ Repeated g ] ->
            Assert.Equal(2, List.length g.cell)
            Assert.True(g.count.value >= 1, "the starter period repeats at least once")
        | films -> Assert.Fail($"expected one Repeated 2-layer period, got %A{films}")
        // The blank Add path opens onto NOTHING — the two NEW intents are genuinely distinct.
        Assert.Empty((initSampleEditor context builtInEntries (NewBlankSample (newSampleId ()))).editor.structure.films)

    [<Fact>]
    let ``toSample builds the sample from the model under the given id`` () =
        let m =
            newModel ()
            |> updateSampleEditor (SetName (EntryName "Built"))
            |> updateSampleEditor (SetDescription (EntryDescription "built by test"))
            |> updateSampleEditor (SetSampleSubstrate ThickPlate)
        let id = newSampleId ()
        let s = toSample id m
        Assert.Equal(id, s.id)
        Assert.Equal("Built", s.name.value)
        Assert.Equal("built by test", s.description.value)
        Assert.Equal(ThickPlate, s.substrate)
        Assert.Equal<SampleStructure>(m.editor.structure, s.structure)

    [<Fact>]
    let ``clicking a layer row toggles it in and out of the multi-selection`` () =
        let _, context = recordingContext ()
        let m = initSampleEditor context builtInEntries (EditSample (threeFilmSample ()))
        let selected = m |> updateSampleEditor (ToggleLayer (AtSingleLayer (FilmItemIndex 0))) |> updateSampleEditor (ToggleLayer (AtSingleLayer (FilmItemIndex 1)))
        Assert.Equal<Set<LayerPosition>>(Set.ofList [ AtSingleLayer (FilmItemIndex 0); AtSingleLayer (FilmItemIndex 1) ], selected.editor.selection)
        let toggledOff = selected |> updateSampleEditor (ToggleLayer (AtSingleLayer (FilmItemIndex 0)))
        Assert.Equal<Set<LayerPosition>>(Set.ofList [ AtSingleLayer (FilmItemIndex 1) ], toggledOff.editor.selection)

    [<Fact>]
    let ``AddLayer appends a layer of the chosen material with the default thickness`` () =
        let m = newModel () |> updateSampleEditor (ChooseMaterial MaterialIds.glass175) |> updateSampleEditor AddLayerClicked
        match m.editor.structure.films with
        | [ SingleLayer l ] ->
            Assert.Equal(MaterialIds.glass175, l.materialId.materialId)
            Assert.Equal<Thickness>(defaultLayerThickness, l.thickness)
            Assert.Equal(PrimaryAxes, l.orientation)
        | films -> Assert.Fail($"expected one single layer, got %A{films}")

    [<Fact>]
    let ``the fold-count stepper steps and clamps at 1`` () =
        let m = newModel ()
        Assert.Equal(PeriodCount 3, (m |> updateSampleEditor (FoldCountBy (PeriodCountStep 1))).foldCount)
        Assert.Equal(PeriodCount 1, (m |> updateSampleEditor (FoldCountBy (PeriodCountStep -5))).foldCount)

    [<Fact>]
    let ``a rejected repeat operation surfaces the typed error's reason as the status`` () =
        // MakeRepeatBlock with nothing selected → SelectionNotFoldable.
        let m = newModel () |> updateSampleEditor MakeRepeatBlockClicked
        match m.status with
        | Some reason -> Assert.Contains("selected", reason.value)
        | None -> Assert.Fail("expected a status reason for the empty-selection fold")
        // Stepping a group's count below 1 → InvalidRepeatCount, count unchanged.
        let _, context = recordingContext ()
        let grouped =
            initSampleEditor context builtInEntries (EditSample (threeFilmSample ()))
            |> updateSampleEditor (ToggleLayer (AtSingleLayer (FilmItemIndex 0)))
            |> updateSampleEditor (ToggleLayer (AtSingleLayer (FilmItemIndex 1)))
            |> updateSampleEditor MakeRepeatBlockClicked
        let below = grouped |> updateSampleEditor (GroupCountBy (FilmItemIndex 0, PeriodCountStep -2))
        match below.status with
        | Some reason -> Assert.Contains("at least 1", reason.value)
        | None -> Assert.Fail("expected a status reason for the count-below-1 step")
        match below.editor.structure.films with
        | Repeated g :: _ -> Assert.Equal(2, g.count.value)
        | films -> Assert.Fail($"expected the repeat group to survive, got %A{films}")

    [<Fact>]
    let ``isAnisotropicEntry separates the anisotropic built-ins from the isotropic ones`` () =
        Assert.False((builtIn MaterialIds.glass152).isAnisotropic)
        Assert.False((builtIn MaterialIds.vacuumMaterial).isAnisotropic)
        Assert.False((builtIn MaterialIds.silicon).isAnisotropic)
        Assert.False((builtIn MaterialIds.euvMolybdenum).isAnisotropic)
        Assert.True((builtIn MaterialIds.uniaxialCrystal).isAnisotropic)
        Assert.True((builtIn MaterialIds.biaxialCrystal).isAnisotropic)
        Assert.True((builtIn MaterialIds.activeCrystal).isAnisotropic)
        Assert.True((builtIn MaterialIds.langasite).isAnisotropic)

    [<Fact>]
    let ``QWOT derives t = lambda over 4n into canonical metres`` () =
        // λ = 600 nm on glass n = 1.52 → t = 600/(4·1.52) nm ≈ 9.868e-8 m (the DBR λ/4 shape).
        match qwotThickness (Berreman.Fields.WaveLength.nm 600.0<nm>) (Berreman.MaterialProperties.RefractionIndex 1.52) with
        | Thickness meters -> Assert.True(close (float meters) (600.0 / (4.0 * 1.52) * 1.0e-9), $"t = %A{meters} m")
        | Infinity -> Assert.Fail("QWOT must derive a finite thickness")
        // The derivation needs a chosen material and a parsable positive wavelength.
        let m = newModel () |> updateSampleEditor (SetQwotText (ControlCaption "600"))
        match m.qwotDerived with
        | Some _ -> Assert.Fail("no material chosen — nothing to take n from")
        | None -> ()
        let chosen = m |> updateSampleEditor (ChooseMaterial MaterialIds.glass152)
        match chosen.qwotDerived with
        | Some (Thickness meters) -> Assert.True(close (float meters) (600.0 / (4.0 * 1.52) * 1.0e-9))
        | Some Infinity | None -> Assert.Fail("expected the derived QWOT thickness")

    [<Fact>]
    let ``nanometreCaption renders nanometres and the infinite case`` () =
        let infinite : Thickness = Infinity
        Assert.Equal(ControlCaption "100 nm", (nmT 100.0).nanometreCaption)
        Assert.Equal(ControlCaption "∞", infinite.nanometreCaption)

    [<Fact>]
    let ``Save persists a NEW sample and an EXISTING one through saveSample, and Cancel writes nothing`` () =
        // NewUnsaved → saveSample under the UPFRONT-minted id (spec 0038 steps 008/022 — the mint
        // happened at window open; the store inserts version 1), then close.
        let calls, context = recordingContext ()
        initSampleEditor context builtInEntries (NewBlankSample (newSampleId ()))
        |> updateSampleEditor (SetName (EntryName "Fresh"))
        |> updateSampleEditor SaveClicked
        |> ignore
        Assert.Equal<string list>([ "save:Fresh"; "close" ], List.ofSeq calls)
        // Existing → the SAME saveSample (the store runs the decision rule), then close.
        let calls2, context2 = recordingContext ()
        initSampleEditor context2 builtInEntries (EditSample (threeFilmSample ()))
        |> updateSampleEditor (SetName (EntryName "Edited"))
        |> updateSampleEditor SaveClicked
        |> ignore
        Assert.Equal<string list>([ "save:Edited"; "close" ], List.ofSeq calls2)
        // Cancel on a DIRTY editor no longer closes silently (spec 0038 step 033): it shows the
        // discard confirm and reaches neither saveSample nor requestClose; Discard then closes.
        let calls3, context3 = recordingContext ()
        let confirming =
            initSampleEditor context3 builtInEntries (EditSample (threeFilmSample ()))
            |> updateSampleEditor (SetName (EntryName "Discarded"))
            |> updateSampleEditor CancelClicked
        Assert.Empty(calls3)
        match confirming.exit with
        | ConfirmingDiscard -> ()
        | Editing -> Assert.Fail("a dirty Cancel must show the discard confirm, not close")
        updateSampleEditor DiscardConfirmed confirming |> ignore
        Assert.Equal<string list>([ "close" ], List.ofSeq calls3)

    [<Fact>]
    let ``spec 0038 (033): a pristine editor closes silently, a dirty one gates Cancel behind the discard confirm`` () =
        // Pristine: a freshly opened blank sample is not dirty and Cancel closes silently.
        let calls, context = recordingContext ()
        let fresh = initSampleEditor context builtInEntries (NewBlankSample (newSampleId ()))
        Assert.False(fresh.isDirty, "a freshly opened editor is pristine")
        let afterCancel = updateSampleEditor CancelClicked fresh
        Assert.Equal<string list>([ "close" ], List.ofSeq calls)
        Assert.Equal(Editing, afterCancel.exit)
        // Dirty: a name edit makes it dirty; Cancel shows the confirm and reaches nothing.
        let calls2, context2 = recordingContext ()
        let edited = initSampleEditor context2 builtInEntries (NewBlankSample (newSampleId ())) |> updateSampleEditor (SetName (EntryName "X"))
        Assert.True(edited.isDirty, "a name edit makes the editor dirty")
        let confirming = updateSampleEditor CancelClicked edited
        Assert.Equal(ConfirmingDiscard, confirming.exit)
        Assert.Empty(calls2)
        // Keep editing dismisses the confirm without closing.
        let kept = updateSampleEditor KeepEditing confirming
        Assert.Equal(Editing, kept.exit)
        Assert.Empty(calls2)
        // Discard closes without ever reaching saveSample.
        updateSampleEditor DiscardConfirmed confirming |> ignore
        Assert.Equal<string list>([ "close" ], List.ofSeq calls2)

    [<Fact>]
    let ``spec 0038 (033): a structural stack edit dirties the editor and a seeded starter opens pristine`` () =
        // A NewSeededMultilayer opens onto the Domain starter period but is NOT a user edit — it
        // opens pristine and closes silently.
        let calls, context = recordingContext ()
        let seeded = initSampleEditor context builtInEntries (NewSeededMultilayer (newSampleId ()))
        Assert.False(seeded.isDirty, "a seeded starter is not a user edit")
        // A stack structural edit (add a layer) dirties it; the selection alone would not.
        let dirtied = updateSampleEditor AddLayerClicked seeded
        Assert.True(dirtied.isDirty, "adding a layer changes the stack structure — dirty")
        // Cancel then gates behind the confirm rather than closing.
        let confirming = updateSampleEditor CancelClicked dirtied
        Assert.Equal(ConfirmingDiscard, confirming.exit)
        Assert.Empty(calls)

    [<Fact>]
    let ``a failing save keeps the window open and surfaces the proxy's reason`` () =
        let closes = ResizeArray<string>()
        let failing : SampleProxy =
            {
                listSamples = fun _ -> Ok []
                searchSamples = fun _ -> Ok []
                tryGetSample = fun _ -> Ok None
                resolveVersion = fun _ -> Ok None
                saveSample = fun _ -> Error (InvalidSampleErr "the name is blank")
                markSampleInactive = fun _ -> Ok ()
                markSampleActive = fun _ -> Ok ()
                supersedeSample = fun _ -> Ok ()
                removeSample = fun _ -> Ok ()
            }
        let context : SampleEditorContext =
            {
                materials = stubMaterialsProxy
                samples = failing
                openMaterialsSelect = fun _ _ -> ()
                requestClose = fun () -> closes.Add "close"
            }
        let m = initSampleEditor context builtInEntries (NewBlankSample (newSampleId ())) |> updateSampleEditor SaveClicked
        Assert.Empty(closes)
        match m.status with
        | Some reason -> Assert.Equal("the name is blank", reason.value)
        | None -> Assert.Fail("expected the proxy's typed reason as the status")

    // ========== step 019 — the Select-flow material picking (pure) ==========

    [<Fact>]
    let ``RefreshMaterials re-queries the LIVE proxy — a material added elsewhere appears, replacing the load-once snapshot`` () =
        let materials, samples, _ = freshProxies ()
        let context : SampleEditorContext =
            {
                materials = materials
                samples = samples
                openMaterialsSelect = fun _ _ -> ()
                requestClose = fun () -> ()
            }
        let opened =
            match materials.listMaterials ActiveOnly with
            | Ok entries -> entries
            | Error e -> failwith $"listMaterials failed: %A{e}"
        let m = initSampleEditor context opened (NewBlankSample (newSampleId ()))
        // Another window writes through the SHARED store while this editor is open (no live
        // notification reaches the editor — out of scope).
        let added = { builtIn MaterialIds.glass152 with id = newMaterialId (); name = EntryName "Added elsewhere" }
        match materials.saveMaterial added with
        | Ok () -> ()
        | Error e -> failwith $"saveMaterial failed: %A{e}"
        Assert.False(m.materials |> List.exists (fun e -> e.id = added.id), "the open-time snapshot cannot hold the later write")
        // The re-query (a Select-session return or a window activation dispatches it)
        // surfaces the write; a pending status is left alone (RefreshMaterials rides the same
        // return as a possible vanished-row message).
        let refreshed = updateSampleEditor RefreshMaterials { m with status = Some (ControlCaption "pending") }
        Assert.True(refreshed.materials |> List.exists (fun e -> e.id = added.id), "RefreshMaterials must surface the added material")
        Assert.Equal<ControlCaption option>(Some (ControlCaption "pending"), refreshed.status)

    [<Fact>]
    let ``BindMaterialToLayer records the picked material as the CHOSEN one — a vanished row chooses nothing`` () =
        // Step 019: the Select-flow pick is the toolbar bulk verbs' one remaining
        // chosen-material source now that the inline picker is gone.
        let _, context = recordingContext ()
        let m = initSampleEditor context builtInEntries (EditSample (threeFilmSample ()))
        let bound = updateSampleEditor (BindMaterialToLayer (AtSingleLayer (FilmItemIndex 1), MaterialIds.glass175)) m
        Assert.Equal<MaterialId option>(Some MaterialIds.glass175, bound.chosenMaterial)
        // The vanished-row return stays a strict no-op plus the status line.
        let vanished = updateSampleEditor (BindMaterialToLayer (AtSingleLayer (FilmItemIndex 7), MaterialIds.glass175)) m
        Assert.Equal<MaterialId option>(None, vanished.chosenMaterial)

    // ========== step 010 — the supported-emission R/T constraint (pure) ==========

    [<Fact>]
    let ``a Plate editor toggles T off leaving R on and marks the editor dirty`` () =
        let _, context = recordingContext ()
        let m = initSampleEditor context builtInEntries (EditSample (threeFilmPlate ()))
        // A freshly opened Plate editor is pristine and emits BOTH groups by default.
        Assert.False(m.isDirty, "a freshly opened editor is pristine")
        Assert.True(m.supportedEmission.emitsReflected, "a Plate defaults to emitting the reflected group")
        Assert.True(m.supportedEmission.emitsTransmitted, "a Plate defaults to emitting the transmitted group")
        // Turning T off leaves R on (never neither) and dirties the editor.
        let tOff = updateSampleEditor (SetTransmittedEmission OpticalConstructor.Domain.Placement.RayGroupOff) m
        Assert.True(tOff.supportedEmission.emitsReflected, "R stays on when T is cleared")
        Assert.False(tOff.supportedEmission.emitsTransmitted, "T is now off")
        Assert.True(tOff.isDirty, "an emission change marks the editor dirty")
        // Symmetrically, a Plate can be constrained to T-only (clear R while T is on).
        let rOff = updateSampleEditor (SetReflectedEmission OpticalConstructor.Domain.Placement.RayGroupOff) m
        Assert.False(rOff.supportedEmission.emitsReflected, "R is now off")
        Assert.True(rOff.supportedEmission.emitsTransmitted, "T stays on when R is cleared")

    [<Fact>]
    let ``clearing both emission groups is impossible on a Plate`` () =
        let _, context = recordingContext ()
        let m = initSampleEditor context builtInEntries (EditSample (threeFilmPlate ()))
        // Clear R (T forced on), then clear the last remaining group: R is forced back on — the
        // both-off state is unrepresentable through the smart setters.
        let rOff = updateSampleEditor (SetReflectedEmission OpticalConstructor.Domain.Placement.RayGroupOff) m
        Assert.False(rOff.supportedEmission.emitsReflected)
        Assert.True(rOff.supportedEmission.emitsTransmitted, "clearing R forces T on")
        let lastCleared = updateSampleEditor (SetTransmittedEmission OpticalConstructor.Domain.Placement.RayGroupOff) rOff
        Assert.True(lastCleared.supportedEmission.emitsReflected, "clearing the last group forces the other back on")
        Assert.False(lastCleared.supportedEmission.emitsTransmitted)

    [<Fact>]
    let ``a Mirror editor pins R on under any emission message and a geometry flip re-imposes it`` () =
        let _, context = recordingContext ()
        // Spec 0046: a Mirror is EmitReflectedOnly at open — R on, T off.
        let m = initSampleEditor context builtInEntries (EditSample (threeFilmMirror ()))
        Assert.True(m.supportedEmission.emitsReflected)
        Assert.False(m.supportedEmission.emitsTransmitted)
        // A stray T-on or R-off message cannot escape the Mirror constraint.
        let tryGainT = updateSampleEditor (SetTransmittedEmission OpticalConstructor.Domain.Placement.RayGroupOn) m
        Assert.True(tryGainT.supportedEmission.emitsReflected)
        Assert.False(tryGainT.supportedEmission.emitsTransmitted, "a Mirror can never gain a transmitted branch")
        let tryClearR = updateSampleEditor (SetReflectedEmission OpticalConstructor.Domain.Placement.RayGroupOff) m
        Assert.True(tryClearR.supportedEmission.emitsReflected, "a Mirror's reflected branch is unclearable")
        // A Plate constrained to T-only, then flipped to Mirror, re-pins R-only ...
        let plate = initSampleEditor context builtInEntries (EditSample (threeFilmPlate ()))
        let tOnly = updateSampleEditor (SetReflectedEmission OpticalConstructor.Domain.Placement.RayGroupOff) plate
        Assert.True(tOnly.supportedEmission.emitsTransmitted && not tOnly.supportedEmission.emitsReflected, "the Plate is T-only")
        let flipped = updateSampleEditor (SetSampleSubstrate Mirror) tOnly
        Assert.True(flipped.supportedEmission.emitsReflected, "flipping to Mirror re-pins the reflected branch on")
        Assert.False(flipped.supportedEmission.emitsTransmitted, "flipping to Mirror clears the transmitted branch")
        // ... and leaving Mirror for a ThinFilm restores both branches (a thin film reflects AND transmits).
        let film = updateSampleEditor (SetSampleSubstrate ThinFilm) flipped
        Assert.True(film.supportedEmission.emitsReflected && film.supportedEmission.emitsTransmitted, "a ThinFilm emits both")
        // A ThinFilm is constrainable like a Plate.
        let thin = initSampleEditor context builtInEntries (EditSample (threeFilmSample ()))
        Assert.True(thin.supportedEmission.emitsReflected && thin.supportedEmission.emitsTransmitted, "a ThinFilm opens emitting both")
        let thinTOnly = updateSampleEditor (SetReflectedEmission OpticalConstructor.Domain.Placement.RayGroupOff) thin
        Assert.True(thinTOnly.supportedEmission.emitsTransmitted && not thinTOnly.supportedEmission.emitsReflected, "a ThinFilm can be T-only")

    [<Fact>]
    let ``toSample carries the edited supported emission and init seeds it from the sample`` () =
        let _, context = recordingContext ()
        // An edited Plate sample's emission is seeded into the model and preserved through toSample.
        let m = initSampleEditor context builtInEntries (EditSample (threeFilmPlate ()))
        let tOnly = updateSampleEditor (SetReflectedEmission OpticalConstructor.Domain.Placement.RayGroupOff) m
        let saved = toSample (newSampleId ()) tOnly
        Assert.True(saved.supportedEmission.emitsTransmitted && not saved.supportedEmission.emitsReflected, "toSample carries the T-only choice")
        // A Mirror always saves EmitReflectedOnly, whatever messages arrived (spec 0046).
        let mirror = initSampleEditor context builtInEntries (EditSample (threeFilmMirror ())) |> updateSampleEditor (SetTransmittedEmission OpticalConstructor.Domain.Placement.RayGroupOn)
        let savedMirror = toSample (newSampleId ()) mirror
        Assert.True(savedMirror.supportedEmission.emitsReflected && not savedMirror.supportedEmission.emitsTransmitted, "a Mirror saves reflected-only")

    // ============================ headless semantic-tree proofs ============================

    /// Every fixed slice-mandated UiId (the window's own id is asserted on the window itself).
    let private mandatedIds : string list =
        [
            ConstructorUiId.sampleNameBox.value
            ConstructorUiId.addLayerButton.value
            ConstructorUiId.makeRepeatBlockButton.value
            ConstructorUiId.selectByMaterialButton.value
            ConstructorUiId.setLayerHeightButton.value
            ConstructorUiId.setLayerMaterialButton.value
            ConstructorUiId.setOrientationOfSelectedButton.value
            ConstructorUiId.removeSelectedLayersButton.value
            ConstructorUiId.repeatCountStepper.value
            ConstructorUiId.qwotEntryBox.value
            ConstructorUiId.sampleEditorSaveButton.value
            ConstructorUiId.sampleEditorCancelButton.value
        ]

    /// Mount the REAL Sample-editor MVU loop headless WITH a captured dispatch (the window
    /// host's own `Cmd.ofEffect` shape) — for the tests that need a model-level message (the
    /// unchanged by-MaterialId ChooseMaterial contract) beside the id-driven clicks.
    let private mountEditorLoop (materials : MaterialProxy) (samples : SampleProxy) (intent : SampleEditorIntent) : HostWindow * (SampleEditorMsg -> unit) =
        let entries =
            match materials.listMaterials ActiveOnly with
            | Ok list -> list
            | Error e -> failwith $"listMaterials failed: %A{e}"
        let context : SampleEditorContext =
            {
                materials = materials
                samples = samples
                openMaterialsSelect = fun _ _ -> ()
                requestClose = fun () -> ()
            }
        let window = HostWindow(Width = 1080.0, Height = 900.0)
        let mutable dispatchRef : SampleEditorMsg -> unit = ignore
        Program.mkProgram
            (fun () -> initSampleEditor context entries intent, Cmd.ofEffect (fun d -> dispatchRef <- d))
            (fun msg m -> updateSampleEditor msg m, Cmd.none)
            sampleEditorView
        |> Program.withHost window
        |> Program.run
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window, (fun msg -> dispatchRef msg; Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the window mounts with every slice-mandated UiId present`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshProxies ()
            let window = SampleEditorWindow(materials, samples, categories, NewBlankSample (newSampleId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(matchesId ConstructorUiId.sampleEditorWindow.value window, "the window itself carries the SampleEditorWindow id")
            for id in mandatedIds do
                Assert.True(isPresent window id, $"%s{id} is missing from the mounted window")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: repeating a 2-layer selection K times expands the structure to 2K films`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshProxies ()
            let window = SampleEditorWindow(materials, samples, categories, NewBlankSample (newSampleId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // Build a 2-layer stack by UiIds (Add layer takes the first listed material now
            // that the inline picker is gone — the fold cares about the count, not the kind).
            clickOn window ConstructorUiId.addLayerButton.value
            clickOn window ConstructorUiId.addLayerButton.value
            Assert.Equal("2", textOf window ConstructorUiId.sampleFilmsCount.value)
            // Select BOTH layers, step the fold count to K = 3, and fold.
            clickOn window (ConstructorUiId.sampleLayerRow (ConstructorUiPosition 0)).value
            clickOn window (ConstructorUiId.sampleLayerRow (ConstructorUiPosition 1)).value
            clickOn window ConstructorUiId.repeatCountStepperPlus.value
            clickOn window ConstructorUiId.makeRepeatBlockButton.value
            // 2 layers × K=3 periods = 6 films, shown by the structure readout.
            Assert.Equal("6", textOf window ConstructorUiId.sampleFilmsCount.value)
            // The group renders as ONE collapsible super-row with its cell layers nested beneath.
            Assert.True(isPresent window (ConstructorUiId.sampleGroupRow (ConstructorUiPosition 0)).value, "the period super-row is missing")
            Assert.True(isPresent window (ConstructorUiId.sampleGroupExpander (ConstructorUiPosition 0)).value, "the rotating-triangle expander is missing")
            Assert.True(isPresent window (ConstructorUiId.sampleCellLayerRow (ConstructorUiPosition 0) (ConstructorUiPosition 0)).value, "cell layer 0 is missing")
            Assert.True(isPresent window (ConstructorUiId.sampleCellLayerRow (ConstructorUiPosition 0) (ConstructorUiPosition 1)).value, "cell layer 1 is missing")
            Assert.False(isPresent window (ConstructorUiId.sampleLayerRow (ConstructorUiPosition 0)).value, "the folded singles must no longer render as top-level rows")
            // The group's INLINE stepper resizes by whole periods: 3 → 4 ⇒ 8 films.
            clickOn window (ConstructorUiId.repeatCountStepperPlusOfGroup (ConstructorUiPosition 0)).value
            Assert.Equal("8", textOf window ConstructorUiId.sampleFilmsCount.value)
            // Collapsing the super-row removes the nested cell rows (the expander toggles).
            clickOn window (ConstructorUiId.sampleGroupExpander (ConstructorUiPosition 0)).value
            Assert.False(isPresent window (ConstructorUiId.sampleCellLayerRow (ConstructorUiPosition 0) (ConstructorUiPosition 0)).value, "a collapsed group must hide its cell rows")
            Assert.Equal("8", textOf window ConstructorUiId.sampleFilmsCount.value)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: select-by-material bulk set-thickness updates only the matching layers`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, _ = freshProxies ()
            // The mounted loop's captured dispatch stands in for a Select-flow pick here (the
            // unchanged by-MaterialId ChooseMaterial contract — the full Choose material…
            // loop is the step-019 acceptance below).
            let window, dispatch = mountEditorLoop materials samples (EditSample (threeFilmSample ()))
            // Choose glass, select every glass layer, and bulk-set the thickness to 5 nm.
            dispatch (ChooseMaterial MaterialIds.glass152)
            clickOn window ConstructorUiId.selectByMaterialButton.value
            setText window ConstructorUiId.sampleLayerHeightBox.value "5"
            clickOn window ConstructorUiId.setLayerHeightButton.value
            // The two glass rows changed; the vacuum row between them did not.
            Assert.Equal("5 nm", textOf window (ConstructorUiId.sampleLayerThickness (ConstructorUiPosition 0)).value)
            Assert.Equal("50 nm", textOf window (ConstructorUiId.sampleLayerThickness (ConstructorUiPosition 1)).value)
            Assert.Equal("5 nm", textOf window (ConstructorUiId.sampleLayerThickness (ConstructorUiPosition 2)).value)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: Save persists a NEW sample through SampleProxy.addSample and closes`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshProxies ()
            let seededCount =
                match samples.listSamples ActiveOnly with
                | Ok all -> List.length all
                | Error e -> failwith $"seed listing failed: %A{e}"
            let window = SampleEditorWindow(materials, samples, categories, NewBlankSample (newSampleId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            setText window ConstructorUiId.sampleNameBox.value "Headless stack"
            setText window ConstructorUiId.sampleDescriptionBox.value "made by the headless test"
            clickOn window ConstructorUiId.addLayerButton.value
            clickOn window ConstructorUiId.sampleEditorSaveButton.value
            Assert.False(window.IsVisible)
            match samples.listSamples ActiveOnly with
            | Ok all ->
                Assert.Equal(seededCount + 1, List.length all)
                match all |> List.tryFind (fun s -> s.name.value = "Headless stack") with
                | Some saved ->
                    Assert.Equal("made by the headless test", saved.description.value)
                    Assert.Equal(1, List.length saved.structure.expandedFilms)
                | None -> Assert.Fail("the new sample was not persisted")
            | Error e -> Assert.Fail($"listSamples failed: %A{e}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: Make-multilayer opens the editor showing the seeded 2-layer period and Save persists a NEW sample`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshProxies ()
            let seededCount =
                match samples.listSamples ActiveOnly with
                | Ok all -> List.length all
                | Error e -> failwith $"seed listing failed: %A{e}"
            // The distinct Make-multilayer path: a NEW sample seeded with the foldable starter period.
            let window = SampleEditorWindow(materials, samples, categories, NewSeededMultilayer (newSampleId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // The seeded 2-layer period renders as ONE collapsible super-row with two cell layers
            // (distinct from a blank Add, which shows no rows).
            Assert.True(isPresent window (ConstructorUiId.sampleGroupRow (ConstructorUiPosition 0)).value, "the seeded period super-row must render")
            Assert.True(isPresent window (ConstructorUiId.sampleCellLayerRow (ConstructorUiPosition 0) (ConstructorUiPosition 0)).value, "seeded cell layer 0 must render")
            Assert.True(isPresent window (ConstructorUiId.sampleCellLayerRow (ConstructorUiPosition 0) (ConstructorUiPosition 1)).value, "seeded cell layer 1 must render")
            // Name it (the structure is already non-empty) and Save — the NEW sample persists
            // through SampleProxy.addSample under a freshly minted SampleId.
            setText window ConstructorUiId.sampleNameBox.value "Headless multilayer"
            clickOn window ConstructorUiId.sampleEditorSaveButton.value
            Assert.False(window.IsVisible)
            match samples.listSamples ActiveOnly with
            | Ok all ->
                Assert.Equal(seededCount + 1, List.length all)
                match all |> List.tryFind (fun s -> s.name.value = "Headless multilayer") with
                | Some saved ->
                    match saved.structure.films with
                    | [ Repeated g ] -> Assert.Equal(2, List.length g.cell)
                    | films -> Assert.Fail($"expected the seeded 2-layer period to persist, got %A{films}")
                | None -> Assert.Fail("the new multilayer sample was not persisted")
            | Error e -> Assert.Fail($"listSamples failed: %A{e}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: Save UPDATES an existing sample in place through SampleProxy.updateSample`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshProxies ()
            let existing = SeedSamples.glassFilm200
            let seededCount =
                match samples.listSamples ActiveOnly with
                | Ok all -> List.length all
                | Error e -> failwith $"seed listing failed: %A{e}"
            let window = SampleEditorWindow(materials, samples, categories, EditSample existing)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            setText window ConstructorUiId.sampleNameBox.value "Renamed film"
            clickOn window ConstructorUiId.sampleEditorSaveButton.value
            Assert.False(window.IsVisible)
            match samples.tryGetSample existing.id with
            | Ok (Some updated) -> Assert.Equal("Renamed film", updated.name.value)
            | Ok None -> Assert.Fail("the existing sample vanished")
            | Error e -> Assert.Fail($"tryGetSample failed: %A{e}")
            match samples.listSamples ActiveOnly with
            | Ok all -> Assert.Equal(seededCount, List.length all)
            | Error e -> Assert.Fail($"listSamples failed: %A{e}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``Cancel on a dirty editor shows the discard confirm, and Discard closes without touching the store`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshProxies ()
            let existing = SeedSamples.glassFilm200
            let window = SampleEditorWindow(materials, samples, categories, EditSample existing)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            setText window ConstructorUiId.sampleNameBox.value "Should not persist"
            // Cancel on the now-dirty editor shows the confirm instead of closing (spec 0038 step 033).
            clickOn window ConstructorUiId.sampleEditorCancelButton.value
            Assert.True(window.IsVisible, "a dirty Cancel must not close silently")
            Assert.True(isPresent window ConstructorUiId.sampleEditorExitConfirm.value, "the discard confirm must appear")
            Assert.False(isPresent window ConstructorUiId.sampleEditorSaveButton.value, "Save/Cancel are replaced by the confirm")
            // Discard closes WITHOUT persisting the edit.
            clickOn window ConstructorUiId.sampleEditorDiscardButton.value
            Assert.False(window.IsVisible)
            match samples.tryGetSample existing.id with
            | Ok (Some kept) -> Assert.Equal(existing.name, kept.name)
            | Ok None -> Assert.Fail("the existing sample vanished")
            | Error e -> Assert.Fail($"tryGetSample failed: %A{e}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``spec 0038 (033): a pristine editor's Cancel closes immediately`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshProxies ()
            let window = SampleEditorWindow(materials, samples, categories, NewBlankSample (newSampleId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.False(isPresent window ConstructorUiId.sampleEditorExitConfirm.value, "no confirm surface before any edit")
            clickOn window ConstructorUiId.sampleEditorCancelButton.value
            Assert.False(window.IsVisible, "a pristine Cancel closes immediately"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``spec 0038 (033): a dirty Cancel confirm — Keep editing returns, then Discard closes`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshProxies ()
            let window = SampleEditorWindow(materials, samples, categories, NewBlankSample (newSampleId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            setText window ConstructorUiId.sampleNameBox.value "Unsaved sample"
            clickOn window ConstructorUiId.sampleEditorCancelButton.value
            Assert.True(window.IsVisible, "a dirty Cancel must not close the window")
            Assert.True(isPresent window ConstructorUiId.sampleEditorExitConfirm.value, "the discard confirm must appear")
            Assert.True(isPresent window ConstructorUiId.sampleEditorDiscardButton.value)
            Assert.True(isPresent window ConstructorUiId.sampleEditorKeepEditingButton.value)
            // Keep editing returns to the editor.
            clickOn window ConstructorUiId.sampleEditorKeepEditingButton.value
            Assert.True(window.IsVisible)
            Assert.False(isPresent window ConstructorUiId.sampleEditorExitConfirm.value, "Keep editing dismisses the confirm")
            Assert.True(isPresent window ConstructorUiId.sampleEditorSaveButton.value, "the Save action returns")
            // Cancel again → Discard closes for real.
            clickOn window ConstructorUiId.sampleEditorCancelButton.value
            Assert.True(isPresent window ConstructorUiId.sampleEditorExitConfirm.value)
            clickOn window ConstructorUiId.sampleEditorDiscardButton.value
            Assert.False(window.IsVisible, "Discard closes the window"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``spec 0038 (033): the window chrome (OnClosing) is equally gated`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshProxies ()
            let window = SampleEditorWindow(materials, samples, categories, NewBlankSample (newSampleId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // The OS title-bar X routes through ChromeCloseIntercepted — the OnClosing override
            // delegates to it (the OS chrome is not reachable through the headless input surface).
            // A pristine editor's chrome close proceeds: nothing intercepted, no confirm.
            Assert.False(window.ChromeCloseIntercepted(), "a pristine chrome close proceeds")
            Assert.False(isPresent window ConstructorUiId.sampleEditorExitConfirm.value, "no confirm on a pristine chrome close")
            // A dirty editor's chrome close is intercepted and shows the SAME discard confirm the
            // Cancel button raises — the window stays open.
            setText window ConstructorUiId.sampleNameBox.value "Chrome edit"
            Assert.True(window.ChromeCloseIntercepted(), "a dirty chrome close must be intercepted")
            Dispatcher.UIThread.RunJobs()
            Assert.True(window.IsVisible, "an intercepted chrome close leaves the window open")
            Assert.True(isPresent window ConstructorUiId.sampleEditorExitConfirm.value, "the chrome close shows the discard confirm")
            // Discard from the confirm then closes for real.
            clickOn window ConstructorUiId.sampleEditorDiscardButton.value
            Assert.False(window.IsVisible, "Discard closes the chrome-gated window"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the per-layer orientation editor exists for anisotropic layers and is REMOVED for isotropic ones`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshProxies ()
            let sample =
                {
                    id = newSampleId ()
                    name = EntryName "Orientation visibility"
                    structure =
                        {
                            films =
                                [
                                    SingleLayer (layerOf MaterialIds.uniaxialCrystal 1000.0)
                                    SingleLayer (layerOf MaterialIds.glass152 100.0)
                                ]
                            substrate = None
                            lower = None
                        }
                    substrate = ThinFilm
                    description = EntryDescription "uniaxial over glass"
                    supportedEmission = ThinFilm.defaultSupportedEmission
                }
            let window = SampleEditorWindow(materials, samples, categories, EditSample sample)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(isPresent window (ConstructorUiId.sampleLayerOrientation (ConstructorUiPosition 0)).value, "the anisotropic layer must carry its orientation editor")
            Assert.False(isPresent window (ConstructorUiId.sampleLayerOrientation (ConstructorUiPosition 1)).value, "the isotropic layer's orientation editor must be REMOVED, not greyed")
            window.Close())

    // ========== step 010 — the supported-emission checkboxes (headless) ==========

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (010): a Plate editor renders both emission checkboxes; a T click clears T, leaves R, and dirties the editor`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshProxies ()
            let window = SampleEditorWindow(materials, samples, categories, EditSample (threeFilmPlate ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // Both R and T checkboxes render for a Plate, both checked (EmitBoth), both enabled.
            Assert.True(isPresent window ConstructorUiId.sampleEmitReflectedCheck.value, "the R checkbox must render")
            Assert.True(isPresent window ConstructorUiId.sampleEmitTransmittedCheck.value, "the T checkbox must render for a Plate")
            Assert.True((checkBox window ConstructorUiId.sampleEmitReflectedCheck.value).IsChecked.GetValueOrDefault false, "R starts checked")
            Assert.True((checkBox window ConstructorUiId.sampleEmitTransmittedCheck.value).IsChecked.GetValueOrDefault false, "T starts checked")
            Assert.True((checkBox window ConstructorUiId.sampleEmitTransmittedCheck.value).IsEnabled, "the Plate T checkbox is enabled")
            // A user click on the T box clears T; R stays checked (never neither).
            clickControl window ConstructorUiId.sampleEmitTransmittedCheck.value
            Assert.False((checkBox window ConstructorUiId.sampleEmitTransmittedCheck.value).IsChecked.GetValueOrDefault true, "the T click cleared T")
            Assert.True((checkBox window ConstructorUiId.sampleEmitReflectedCheck.value).IsChecked.GetValueOrDefault false, "R remains on after clearing T")
            // The emission edit dirtied the editor: Cancel is gated behind the discard confirm.
            clickOn window ConstructorUiId.sampleEditorCancelButton.value
            Assert.True(window.IsVisible, "an emission edit makes a dirty Cancel show the confirm, not close")
            Assert.True(isPresent window ConstructorUiId.sampleEditorExitConfirm.value, "the emission change marks the editor dirty")
            clickOn window ConstructorUiId.sampleEditorDiscardButton.value
            Assert.False(window.IsVisible))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (010): a Mirror editor shows the R emission checkbox fixed on and disabled, with no T control`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshProxies ()
            let window = SampleEditorWindow(materials, samples, categories, EditSample (threeFilmMirror ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(isPresent window ConstructorUiId.sampleEmitReflectedCheck.value, "the R checkbox must render for a Mirror")
            let rBox = checkBox window ConstructorUiId.sampleEmitReflectedCheck.value
            Assert.True(rBox.IsChecked.GetValueOrDefault false, "the Mirror R checkbox is fixed on")
            Assert.False(rBox.IsEnabled, "the Mirror R checkbox is disabled (fixed)")
            Assert.False(isPresent window ConstructorUiId.sampleEmitTransmittedCheck.value, "a Mirror shows no T control")
            window.Close()
            // Spec 0046: a ThinFilm offers BOTH boxes, enabled.
            let thinWindow = SampleEditorWindow(materials, samples, categories, EditSample (threeFilmSample ()))
            thinWindow.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True((checkBox thinWindow ConstructorUiId.sampleEmitReflectedCheck.value).IsEnabled, "a ThinFilm's R box is switchable")
            Assert.True(isPresent thinWindow ConstructorUiId.sampleEmitTransmittedCheck.value, "a ThinFilm shows the T control")
            thinWindow.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the QWOT entry derives the read-only thickness in nanometres and Set thickness applies it`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, _ = freshProxies ()
            // The captured dispatch stands in for a Select-flow pick (the unchanged
            // by-MaterialId ChooseMaterial contract): QWOT takes n from the CHOSEN glass.
            let window, dispatch = mountEditorLoop materials samples (NewBlankSample (newSampleId ()))
            dispatch (ChooseMaterial MaterialIds.glass152)
            clickOn window ConstructorUiId.addLayerButton.value
            clickOn window (ConstructorUiId.sampleLayerRow (ConstructorUiPosition 0)).value
            setText window ConstructorUiId.qwotEntryBox.value "600"
            // t = λ/(4n) = 600/(4·1.52) nm, shown read-only in DISPLAY nanometres (spec 0033
            // gap G14.1 — no longer raw metres) though stored canonical-SI.
            Assert.Equal($"%g{600.0 / (4.0 * 1.52)} nm", textOf window ConstructorUiId.qwotDerivedText.value)
            clickOn window ConstructorUiId.setLayerHeightButton.value
            Assert.Equal($"%g{600.0 / (4.0 * 1.52)} nm", textOf window (ConstructorUiId.sampleLayerThickness (ConstructorUiPosition 0)).value)
            window.Close())

    // ========== step 019 — the per-layer Choose material… Select flow (headless) ==========

    /// Subscribe to the windows the REAL launcher opens (the WindowOpenedEvent seam the
    /// desktop lifetime itself uses).
    let private observeOpenedWindows () : ResizeArray<Window> * System.IDisposable =
        let opened = ResizeArray<Window>()
        let subscription =
            Window.WindowOpenedEvent.Raised
            |> Observable.subscribe (fun (struct (sender, _args)) ->
                match sender with
                | :? Window as w -> opened.Add w
                | _ -> ())
        opened, subscription

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (019): Choose material… opens the Materials window in Select state targeted at the row — the return binds it and becomes the chosen material`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshProxies ()
            let window = SampleEditorWindow(materials, samples, categories, EditSample (threeFilmSample ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let opened, subscription = observeOpenedWindows ()
            use _sub = subscription
            // Row 1 (the vacuum film) asks for a material: the MATERIALS window opens in
            // SELECT state — the Select/Close pair and the fixed sample-layer banner.
            clickOn window (ConstructorUiId.chooseMaterialButton (ConstructorUiPosition 1)).value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            let materialsWindow = opened.[0]
            Assert.True(matchesId ConstructorUiId.materialsWindow.value materialsWindow, "the opened window must be the Materials window")
            Assert.True(isPresent materialsWindow ConstructorUiId.materialsSelectButton.value, "the Select verb must render — the window is in Select state")
            Assert.Contains("sample layer", textOf materialsWindow ConstructorUiId.materialsSelectConstraint.value)
            // Pick the 1.75 glass: filter → highlight the leaf → Select. The TARGETED return
            // re-materials ROW 1 (never "the current selection") and closes the window.
            commitFilter materialsWindow "1.75"
            clickOn materialsWindow (ConstructorUiId.facetTreeChevron (ConstructorUiKey "entries")).value
            clickOn materialsWindow ((ConstructorUiId.materialsEntryNode MaterialIds.glass175).value)
            clickOn materialsWindow ConstructorUiId.materialsSelectButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.False(materialsWindow.IsVisible, "Select must close the Materials window")
            Assert.Contains("Transparent glass (n = 1.75)", textOf window (ConstructorUiId.sampleLayerRow (ConstructorUiPosition 1)).value)
            Assert.Equal("", textOf window ConstructorUiId.sampleEditorStatus.value)
            // The picked id also became the CHOSEN material — the toolbar bulk verbs' source:
            // Set material re-materials the row-0 selection with it.
            clickOn window (ConstructorUiId.sampleLayerRow (ConstructorUiPosition 0)).value
            clickOn window ConstructorUiId.setLayerMaterialButton.value
            Assert.Contains("Transparent glass (n = 1.75)", textOf window (ConstructorUiId.sampleLayerRow (ConstructorUiPosition 0)).value)
            window.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (019): a second Choose material… RE-TARGETS the live Materials window at the new row`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshProxies ()
            let window = SampleEditorWindow(materials, samples, categories, EditSample (threeFilmSample ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let opened, subscription = observeOpenedWindows ()
            use _sub = subscription
            clickOn window (ConstructorUiId.chooseMaterialButton (ConstructorUiPosition 0)).value
            Dispatcher.UIThread.RunJobs()
            clickOn window (ConstructorUiId.chooseMaterialButton (ConstructorUiPosition 2)).value
            Dispatcher.UIThread.RunJobs()
            // ONE window: the second Choose re-pointed the live single instance (the step-016
            // RetargetedWindow semantics), it did not stack a copy.
            Assert.Equal(1, opened.Count)
            let materialsWindow = opened.[0]
            // The re-pointed session serves ROW 2: the return re-materials it; the superseded
            // row-0 session binds nothing.
            commitFilter materialsWindow "1.75"
            clickOn materialsWindow (ConstructorUiId.facetTreeChevron (ConstructorUiKey "entries")).value
            clickOn materialsWindow ((ConstructorUiId.materialsEntryNode MaterialIds.glass175).value)
            clickOn materialsWindow ConstructorUiId.materialsSelectButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Contains("Transparent glass (n = 1.75)", textOf window (ConstructorUiId.sampleLayerRow (ConstructorUiPosition 2)).value)
            Assert.Contains("Transparent glass (n = 1.52)", textOf window (ConstructorUiId.sampleLayerRow (ConstructorUiPosition 0)).value)
            Assert.Contains("Vacuum", textOf window (ConstructorUiId.sampleLayerRow (ConstructorUiPosition 1)).value)
            window.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (019): deleting the target row first makes the Select return a NO-OP plus the status line`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshProxies ()
            let window = SampleEditorWindow(materials, samples, categories, EditSample (threeFilmSample ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let opened, subscription = observeOpenedWindows ()
            use _sub = subscription
            clickOn window (ConstructorUiId.chooseMaterialButton (ConstructorUiPosition 2)).value
            Dispatcher.UIThread.RunJobs()
            let materialsWindow = opened.[0]
            // The user deletes the target row while the modeless Select window is open.
            clickOn window (ConstructorUiId.sampleLayerRow (ConstructorUiPosition 2)).value
            clickOn window ConstructorUiId.removeSelectedLayersButton.value
            Assert.Equal("2", textOf window ConstructorUiId.sampleFilmsCount.value)
            // The Select return now targets a vanished row: a NO-OP plus the status line —
            // never a throw, and no surviving row takes the returned material.
            commitFilter materialsWindow "1.75"
            clickOn materialsWindow (ConstructorUiId.facetTreeChevron (ConstructorUiKey "entries")).value
            clickOn materialsWindow ((ConstructorUiId.materialsEntryNode MaterialIds.glass175).value)
            clickOn materialsWindow ConstructorUiId.materialsSelectButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.False(materialsWindow.IsVisible, "the Select window still closes after its return")
            Assert.Equal("2", textOf window ConstructorUiId.sampleFilmsCount.value)
            Assert.Contains("no longer in the stack", textOf window ConstructorUiId.sampleEditorStatus.value)
            Assert.Contains("Transparent glass (n = 1.52)", textOf window (ConstructorUiId.sampleLayerRow (ConstructorUiPosition 0)).value)
            Assert.Contains("Vacuum", textOf window (ConstructorUiId.sampleLayerRow (ConstructorUiPosition 1)).value)
            window.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (019): a material saved through the Material editor appears after the sample editor's ACTIVATION re-query`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshProxies ()
            let mintedId = newMaterialId ()
            let sample =
                {
                    id = newSampleId ()
                    name = EntryName "Awaiting the fresh oxide"
                    structure =
                        {
                            films = [ SingleLayer { materialId = MaterialVersionId.firstOf mintedId; thickness = nmT 100.0; orientation = PrimaryAxes } ]
                            substrate = None
                            lower = None
                        }
                    substrate = ThinFilm
                    description = EntryDescription "step-019 activation re-query"
                    supportedEmission = ThinFilm.defaultSupportedEmission
                }
            let window = SampleEditorWindow(materials, samples, categories, EditSample sample)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // The material does not exist yet: the row shows the unresolved-id readout.
            Assert.Contains("unknown material", textOf window (ConstructorUiId.sampleLayerRow (ConstructorUiPosition 0)).value)
            // ANOTHER window adds it — the REAL Material editor over the SHARED store; no
            // live notification reaches the sample editor (out of scope).
            let materialEditor = MaterialEditorWindow(materials, MaterialEditorView.NewMaterial mintedId, categories = categories)
            materialEditor.Show()
            Dispatcher.UIThread.RunJobs()
            setText materialEditor ConstructorUiId.materialNameBox.value "Fresh oxide"
            clickOn materialEditor ConstructorUiId.materialEditorSaveButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.False(materialEditor.IsVisible, "Save must close the Material editor")
            // Re-ACTIVATING the sample editor re-queries the list: the row resolves its name.
            window.Activate()
            Dispatcher.UIThread.RunJobs()
            Assert.Contains("Fresh oxide", textOf window (ConstructorUiId.sampleLayerRow (ConstructorUiPosition 0)).value)
            window.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (019): a material added while the editor is open appears through the SELECT-RETURN re-query`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshProxies ()
            let window = SampleEditorWindow(materials, samples, categories, EditSample (threeFilmSample ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // A material lands in the SHARED store while the editor is open (any other
            // window's save path) — the editor's open-time snapshot cannot know it.
            let added = { builtIn MaterialIds.glass152 with id = newMaterialId (); name = EntryName "Mid-session titania" }
            match materials.saveMaterial added with
            | Ok () -> ()
            | Error e -> failwith $"saveMaterial failed: %A{e}"
            let opened, subscription = observeOpenedWindows ()
            use _sub = subscription
            clickOn window (ConstructorUiId.chooseMaterialButton (ConstructorUiPosition 0)).value
            Dispatcher.UIThread.RunJobs()
            let materialsWindow = opened.[0]
            // The Materials window projects the LIVE store, so the new entry is offered;
            // picking it can only NAME the row if the editor re-queried its own list on the
            // Select return.
            commitFilter materialsWindow "Mid-session titania"
            clickOn materialsWindow (ConstructorUiId.facetTreeChevron (ConstructorUiKey "entries")).value
            clickOn materialsWindow ((ConstructorUiId.materialsEntryNode added.id).value)
            clickOn materialsWindow ConstructorUiId.materialsSelectButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Contains("Mid-session titania", textOf window (ConstructorUiId.sampleLayerRow (ConstructorUiPosition 0)).value)
            Assert.Equal("", textOf window ConstructorUiId.sampleEditorStatus.value)
            window.Close()
            Dispatcher.UIThread.RunJobs())

    // ========== step 016 — the TARGETED Select return (BindMaterialToLayer, pure) ==========

    [<Fact>]
    let ``BindMaterialToLayer re-materials exactly the targeted position and preserves the user selection`` () =
        let _, context = recordingContext ()
        let m =
            initSampleEditor context builtInEntries (EditSample (threeFilmSample ()))
            |> updateSampleEditor (ToggleLayer (AtSingleLayer (FilmItemIndex 2)))
        // The Materials window's Select session returns vacuum for row 1 — NOT the selection.
        let bound = updateSampleEditor (BindMaterialToLayer (AtSingleLayer (FilmItemIndex 1), MaterialIds.glass175)) m
        let materialAt (i : int) (model : SampleEditorModel) : MaterialId =
            match List.item i model.editor.structure.films with
            | SingleLayer layer -> layer.materialId.materialId
            | Repeated _ -> failwith $"films item %d{i} is unexpectedly a repeat group"
        Assert.Equal(MaterialIds.glass175, materialAt 1 bound)
        Assert.Equal(MaterialIds.glass152, materialAt 0 bound)
        Assert.Equal(MaterialIds.glass152, materialAt 2 bound)
        // The user's own multi-selection survives the targeted transform (the
        // SetLayerOrientation dance), and the status stays clean.
        Assert.Equal<Set<LayerPosition>>(Set.ofList [ AtSingleLayer (FilmItemIndex 2) ], bound.editor.selection)
        Assert.Equal<ControlCaption option>(None, bound.status)

    [<Fact>]
    let ``BindMaterialToLayer on a vanished row is a NO-OP plus the status line — never a throw`` () =
        let _, context = recordingContext ()
        // A blank NEW sample has no row 0 — the deleted-row race the targeted return must
        // survive (spec 0038 step 016; step 019 wires the Choose material… verb).
        let m = initSampleEditor context builtInEntries (NewBlankSample (newSampleId ()))
        let vanished = updateSampleEditor (BindMaterialToLayer (AtSingleLayer (FilmItemIndex 0), MaterialIds.glass152)) m
        Assert.Equal<SampleStructure>(m.editor.structure, vanished.editor.structure)
        match vanished.status with
        | Some text -> Assert.Contains("no longer in the stack", text.value)
        | None -> Assert.Fail "the vanished row must surface the status line"
        // A cell-slot position of a non-group item is equally vanished (total, typed, no throw).
        let three = initSampleEditor context builtInEntries (EditSample (threeFilmSample ()))
        let cellMiss = updateSampleEditor (BindMaterialToLayer (AtCellLayer (FilmItemIndex 0, CellLayerIndex 0), MaterialIds.glass152)) three
        Assert.Equal<SampleStructure>(three.editor.structure, cellMiss.editor.structure)
        match cellMiss.status with
        | Some text -> Assert.Contains("no longer in the stack", text.value)
        | None -> Assert.Fail "the mismatched cell position must surface the status line"

    // ========== step 011 — the substrate Select return (D.4: a film-less Plate) ==========

    /// A `Plate` with NO film layers and no substrate plate — the D.4 gap: it has no film row,
    /// hence no Choose material… verb, so before this slice it had no route to pick a substrate
    /// material at all. Its ONLY path to the Materials window is the substrate Set… verb.
    let private filmlessPlate () : Sample =
        {
            id = newSampleId ()
            name = EntryName "Bare plate"
            structure = { films = []; substrate = None; lower = None }
            substrate = ThickPlate
            description = EntryDescription "film-less Plate — substrate set via the Materials Select window"
            supportedEmission = ThickPlate.defaultSupportedEmission
        }

    [<Fact>]
    let ``BindMaterialToSubstrate sets the substrate plate to the picked material and records it as chosen`` () =
        let _, context = recordingContext ()
        let m = initSampleEditor context builtInEntries (EditSample (filmlessPlate ()))
        // The bare plate opens with no substrate plate.
        Assert.Equal<SampleLayer option>(None, m.editor.structure.substrate)
        // The substrate Select session returns 1.75 glass for the substrate slot.
        let bound = updateSampleEditor (BindMaterialToSubstrate MaterialIds.glass175) m
        match bound.editor.structure.substrate with
        | Some layer -> Assert.Equal(MaterialIds.glass175, layer.materialId.materialId)
        | None -> Assert.Fail "the substrate plate must be set to the picked material"
        // The picked id also becomes the toolbar's chosen material (the BindMaterialToLayer
        // precedent), the status stays clean, and no films were fabricated by the pick.
        Assert.Equal<MaterialId option>(Some MaterialIds.glass175, bound.chosenMaterial)
        Assert.Equal<ControlCaption option>(None, bound.status)
        Assert.Equal<int>(0, List.length bound.editor.structure.films)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (011): a film-less Plate sets its substrate through the Materials Select window`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshProxies ()
            let window = SampleEditorWindow(materials, samples, categories, EditSample (filmlessPlate ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // A film-less Plate: no substrate plate yet, and no film rows to pick a material from.
            Assert.Equal("none", textOf window ConstructorUiId.sampleSubstrateSummary.value)
            Assert.False(isPresent window (ConstructorUiId.sampleLayerRow (ConstructorUiPosition 0)).value, "a film-less Plate has no layer rows")
            let opened, subscription = observeOpenedWindows ()
            use _sub = subscription
            // The substrate Set… verb opens the MATERIALS window in SELECT state — the Select
            // pair renders and the banner names the substrate plate (the new SampleSubstrateTarget
            // target description).
            clickOn window ConstructorUiId.setSubstrateButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            let materialsWindow = opened.[0]
            Assert.True(matchesId ConstructorUiId.materialsWindow.value materialsWindow, "the opened window must be the Materials window")
            Assert.True(isPresent materialsWindow ConstructorUiId.materialsSelectButton.value, "the Select verb must render — the window is in Select state")
            Assert.Contains("substrate", textOf materialsWindow ConstructorUiId.materialsSelectConstraint.value)
            // Pick the 1.75 glass: filter → highlight the leaf → Select. The TARGETED return sets
            // the substrate plate and closes the window.
            commitFilter materialsWindow "1.75"
            clickOn materialsWindow (ConstructorUiId.facetTreeChevron (ConstructorUiKey "entries")).value
            clickOn materialsWindow ((ConstructorUiId.materialsEntryNode MaterialIds.glass175).value)
            clickOn materialsWindow ConstructorUiId.materialsSelectButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.False(materialsWindow.IsVisible, "Select must close the Materials window")
            // The returned material became the substrate — headless-verified.
            Assert.Contains("Transparent glass (n = 1.75)", textOf window ConstructorUiId.sampleSubstrateSummary.value)
            Assert.Equal("", textOf window ConstructorUiId.sampleEditorStatus.value)
            window.Close()
            Dispatcher.UIThread.RunJobs())
