namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open Berreman.Constants
open Berreman.Media
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.SampleStackEditor
open OpticalConstructor.TestWindows
open OpticalConstructor.TestWindows.SampleEditorView

/// Spec 0033 (022) — the SampleEditorWindow component (UICOMP_XDUO_0003): the Sample editor
/// window in OpticalConstructor.TestWindows over the step-21 Domain `SampleStackEditor`.
/// Two layers, the 016 precedent: pure contract tests for the model / update / helpers, and
/// headless semantic-tree proofs that DRIVE THE REAL WINDOW BY ITS UiIds — the slice
/// acceptance: repeating a 2-layer selection K times shows a structure expanding to `2*K`
/// films, select-by-material bulk set-thickness updates only the matching layers, and Save
/// persists through `SampleProxy` (add for new, update for existing). The component is
/// declared, not wired — no parent view opens it here.
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
        | None -> Assert.Fail(sprintf "%s was not found (or not visible)" id)
        | Some b ->
            let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
            if c.HasValue then
                window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                // A Save / Cancel click closes the window during the press — skip the release then.
                if window.IsVisible then
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
            else Assert.Fail(sprintf "%s has no on-screen position" id)

    /// Set the text of the TextBox carrying `id` (fires the property-change subscription the
    /// view's `onTextChanged` binds — still driving the control found by its UiId).
    let private setText (window : Window) (id : string) (text : string) : unit =
        match tryFindControl window id with
        | Some (:? TextBox as tb) ->
            tb.Text <- text
            Dispatcher.UIThread.RunJobs()
        | Some c -> Assert.Fail(sprintf "%s is a %s, not a TextBox" id (c.GetType().Name))
        | None -> Assert.Fail(sprintf "%s was not found" id)

    /// The text of the TextBlock carrying `id`.
    let private textOf (window : Window) (id : string) : string =
        match tryFindControl window id with
        | Some (:? TextBlock as tb) -> tb.Text
        | Some c -> failwith (sprintf "%s is a %s, not a TextBlock" id (c.GetType().Name))
        | None -> failwith (sprintf "%s was not found in the visual tree" id)

    let private close (a : float) (b : float) : bool = abs (a - b) <= 1.0e-9

    /// Fresh, isolated in-memory stores per test (the composition the App would perform).
    let private freshProxies () : MaterialProxy * SampleProxy =
        let samples = SampleProxy.createInMemory ()
        let materials = MaterialProxy.createInMemory (samplesReferencing samples)
        materials, samples

    let private builtIn (id : MaterialId) : MaterialEntry =
        builtInEntries |> List.find (fun e -> e.id = id)

    /// A recording stub context (the functional-proxy seam — the test substitutes in-memory
    /// stubs of the SAME shape and observes which proxy function a Save/Cancel reached).
    let private recordingContext () : ResizeArray<string> * SampleEditorContext =
        let calls = ResizeArray<string>()
        let stub : SampleProxy =
            {
                listSamples = fun () -> Ok []
                searchSamples = fun _ -> Ok []
                tryGetSample = fun _ -> Ok None
                addSample = fun s -> calls.Add("add:" + s.name); Ok ()
                updateSample = fun s -> calls.Add("update:" + s.name); Ok ()
                removeSample = fun _ -> Ok ()
            }
        calls, { samples = stub; requestClose = fun () -> calls.Add "close" }

    let private nmT (t : float) : Thickness = Thickness.nm (t * 1.0<nm>)

    let private layerOf (materialId : MaterialId) (thicknessNm : float) : SampleLayer =
        { materialId = materialId; thickness = nmT thicknessNm; orientation = PrimaryAxes }

    /// films = [ glass 100 nm; vacuum 50 nm; glass 100 nm ] — the select-by-material shape.
    let private threeFilmSample () : Sample =
        {
            id = newSampleId ()
            name = "Three films"
            structure =
                {
                    films =
                        [
                            SingleLayer (layerOf MaterialIds.glass152 100.0)
                            SingleLayer (layerOf MaterialIds.vacuum 50.0)
                            SingleLayer (layerOf MaterialIds.glass152 100.0)
                        ]
                    substrate = None
                    lower = None
                }
            substrate = ThinFilm
            description = "test three-film stack"
        }

    let private newModel () : Model =
        let _, context = recordingContext ()
        init context builtInEntries None

    // ============================ pure control contract ============================

    [<Fact>]
    let ``the Sample editor UiIds are the slice-mandated stable ids`` () =
        Assert.Equal("SampleEditorWindow", UiIds.window)
        Assert.Equal("SampleNameBox", UiIds.nameBox)
        Assert.Equal("AddLayerButton", UiIds.addLayerButton)
        Assert.Equal("MakeRepeatBlockButton", UiIds.makeRepeatBlockButton)
        Assert.Equal("SelectByMaterialButton", UiIds.selectByMaterialButton)
        Assert.Equal("SetLayerHeightButton", UiIds.setLayerHeightButton)
        Assert.Equal("SetLayerMaterialButton", UiIds.setLayerMaterialButton)
        Assert.Equal("SetOrientationOfSelectedButton", UiIds.setOrientationOfSelectedButton)
        Assert.Equal("RemoveSelectedLayersButton", UiIds.removeSelectedLayersButton)
        Assert.Equal("RepeatCountStepper", UiIds.repeatCountStepper)
        Assert.Equal("QwotEntryBox", UiIds.qwotEntryBox)
        Assert.Equal("SampleEditorSaveButton", UiIds.saveButton)
        Assert.Equal("SampleEditorCancelButton", UiIds.cancelButton)
        // The derived per-row / per-group id families are prefixed so they cannot collide.
        Assert.Equal("SampleLayerRow_1", UiIds.layerRow 1)
        Assert.Equal("SampleLayerRow_0_1", UiIds.cellLayerRow 0 1)
        Assert.Equal("SampleGroupRow_0", UiIds.groupRow 0)
        Assert.Equal("RepeatCountStepperPlus_0", UiIds.groupStepperPlus 0)

    [<Fact>]
    let ``a new sample opens empty: thin film, no films, nothing selected, fold count 2`` () =
        let m = newModel ()
        Assert.Equal("", m.name)
        Assert.Equal("", m.description)
        Assert.Equal(ThinFilm, m.substrate)
        Assert.Empty(m.editor.structure.films)
        Assert.Empty(m.editor.selection)
        Assert.Equal(2, m.foldCount)
        Assert.Equal(NewSample, m.target)
        match m.status with
        | None -> ()
        | Some s -> Assert.Fail(sprintf "expected no status, got '%s'" s)

    [<Fact>]
    let ``an existing sample seeds the editor and save targets its id`` () =
        let sample = threeFilmSample ()
        let _, context = recordingContext ()
        let m = init context builtInEntries (Some sample)
        Assert.Equal(sample.name, m.name)
        Assert.Equal(sample.description, m.description)
        Assert.Equal(sample.substrate, m.substrate)
        Assert.Equal<SampleStructure>(sample.structure, m.editor.structure)
        Assert.Equal(ExistingSample sample.id, m.target)

    [<Fact>]
    let ``toSample builds the sample from the model under the given id`` () =
        let m =
            newModel ()
            |> update (SetName "Built")
            |> update (SetDescription "built by test")
            |> update (SetSubstrate Plate)
        let id = newSampleId ()
        let s = toSample id m
        Assert.Equal(id, s.id)
        Assert.Equal("Built", s.name)
        Assert.Equal("built by test", s.description)
        Assert.Equal(Plate, s.substrate)
        Assert.Equal<SampleStructure>(m.editor.structure, s.structure)

    [<Fact>]
    let ``clicking a layer row toggles it in and out of the multi-selection`` () =
        let _, context = recordingContext ()
        let m = init context builtInEntries (Some (threeFilmSample ()))
        let selected = m |> update (ToggleLayer (AtSingleLayer 0)) |> update (ToggleLayer (AtSingleLayer 1))
        Assert.Equal<Set<LayerPosition>>(Set.ofList [ AtSingleLayer 0; AtSingleLayer 1 ], selected.editor.selection)
        let toggledOff = selected |> update (ToggleLayer (AtSingleLayer 0))
        Assert.Equal<Set<LayerPosition>>(Set.ofList [ AtSingleLayer 1 ], toggledOff.editor.selection)

    [<Fact>]
    let ``AddLayer appends a layer of the chosen material with the default thickness`` () =
        let m = newModel () |> update (ChooseMaterial MaterialIds.glass175) |> update AddLayerClicked
        match m.editor.structure.films with
        | [ SingleLayer l ] ->
            Assert.Equal(MaterialIds.glass175, l.materialId)
            Assert.Equal<Thickness>(defaultLayerThickness, l.thickness)
            Assert.Equal(PrimaryAxes, l.orientation)
        | films -> Assert.Fail(sprintf "expected one single layer, got %A" films)

    [<Fact>]
    let ``the fold-count stepper steps and clamps at 1`` () =
        let m = newModel ()
        Assert.Equal(3, (m |> update (FoldCountBy 1)).foldCount)
        Assert.Equal(1, (m |> update (FoldCountBy -5)).foldCount)

    [<Fact>]
    let ``a rejected repeat operation surfaces the typed error's reason as the status`` () =
        // MakeRepeatBlock with nothing selected → SelectionNotFoldable.
        let m = newModel () |> update MakeRepeatBlockClicked
        match m.status with
        | Some reason -> Assert.Contains("selected", reason)
        | None -> Assert.Fail("expected a status reason for the empty-selection fold")
        // Stepping a group's count below 1 → InvalidRepeatCount, count unchanged.
        let _, context = recordingContext ()
        let grouped =
            init context builtInEntries (Some (threeFilmSample ()))
            |> update (ToggleLayer (AtSingleLayer 0))
            |> update (ToggleLayer (AtSingleLayer 1))
            |> update MakeRepeatBlockClicked
        let below = grouped |> update (GroupCountBy (0, -2))
        match below.status with
        | Some reason -> Assert.Contains("at least 1", reason)
        | None -> Assert.Fail("expected a status reason for the count-below-1 step")
        match below.editor.structure.films with
        | Repeated g :: _ -> Assert.Equal(2, g.count)
        | films -> Assert.Fail(sprintf "expected the repeat group to survive, got %A" films)

    [<Fact>]
    let ``isAnisotropicEntry separates the anisotropic built-ins from the isotropic ones`` () =
        Assert.False(isAnisotropicEntry (builtIn MaterialIds.glass152))
        Assert.False(isAnisotropicEntry (builtIn MaterialIds.vacuum))
        Assert.False(isAnisotropicEntry (builtIn MaterialIds.silicon))
        Assert.False(isAnisotropicEntry (builtIn MaterialIds.euvMolybdenum))
        Assert.True(isAnisotropicEntry (builtIn MaterialIds.uniaxialCrystal))
        Assert.True(isAnisotropicEntry (builtIn MaterialIds.biaxialCrystal))
        Assert.True(isAnisotropicEntry (builtIn MaterialIds.activeCrystal))
        Assert.True(isAnisotropicEntry (builtIn MaterialIds.langasite))

    [<Fact>]
    let ``QWOT derives t = lambda over 4n into canonical metres`` () =
        // λ = 600 nm on glass n = 1.52 → t = 600/(4·1.52) nm ≈ 9.868e-8 m (the DBR λ/4 shape).
        match qwotThickness 600.0 1.52 with
        | Thickness meters -> Assert.True(close (float meters) (600.0 / (4.0 * 1.52) * 1.0e-9), sprintf "t = %A m" meters)
        | Infinity -> Assert.Fail("QWOT must derive a finite thickness")
        // The derivation needs a chosen material and a parsable positive wavelength.
        let m = newModel () |> update (SetQwotText "600")
        match qwotDerived m with
        | Some _ -> Assert.Fail("no material chosen — nothing to take n from")
        | None -> ()
        let chosen = m |> update (ChooseMaterial MaterialIds.glass152)
        match qwotDerived chosen with
        | Some (Thickness meters) -> Assert.True(close (float meters) (600.0 / (4.0 * 1.52) * 1.0e-9))
        | Some Infinity | None -> Assert.Fail("expected the derived QWOT thickness")

    [<Fact>]
    let ``thicknessLabel renders nanometres and the infinite case`` () =
        Assert.Equal("100 nm", thicknessLabel (nmT 100.0))
        Assert.Equal("∞", thicknessLabel Infinity)

    [<Fact>]
    let ``Save adds a NEW sample, updates an EXISTING one, and Cancel writes nothing`` () =
        // New → addSample, then close.
        let calls, context = recordingContext ()
        init context builtInEntries None
        |> update (SetName "Fresh")
        |> update SaveClicked
        |> ignore
        Assert.Equal<string list>([ "add:Fresh"; "close" ], List.ofSeq calls)
        // Existing → updateSample, then close.
        let calls2, context2 = recordingContext ()
        init context2 builtInEntries (Some (threeFilmSample ()))
        |> update (SetName "Edited")
        |> update SaveClicked
        |> ignore
        Assert.Equal<string list>([ "update:Edited"; "close" ], List.ofSeq calls2)
        // Cancel → close only; the proxy is never reached.
        let calls3, context3 = recordingContext ()
        init context3 builtInEntries (Some (threeFilmSample ()))
        |> update (SetName "Discarded")
        |> update CancelClicked
        |> ignore
        Assert.Equal<string list>([ "close" ], List.ofSeq calls3)

    [<Fact>]
    let ``a failing save keeps the window open and surfaces the proxy's reason`` () =
        let closes = ResizeArray<string>()
        let failing : SampleProxy =
            {
                listSamples = fun () -> Ok []
                searchSamples = fun _ -> Ok []
                tryGetSample = fun _ -> Ok None
                addSample = fun _ -> Error (InvalidSample "the name is blank")
                updateSample = fun _ -> Error (InvalidSample "the name is blank")
                removeSample = fun _ -> Ok ()
            }
        let context : SampleEditorContext = { samples = failing; requestClose = fun () -> closes.Add "close" }
        let m = init context builtInEntries None |> update SaveClicked
        Assert.Empty(closes)
        match m.status with
        | Some reason -> Assert.Equal("the name is blank", reason)
        | None -> Assert.Fail("expected the proxy's typed reason as the status")

    // ============================ headless semantic-tree proofs ============================

    /// Every fixed slice-mandated UiId (the window's own id is asserted on the window itself).
    let private mandatedIds : string list =
        [
            UiIds.nameBox
            UiIds.addLayerButton
            UiIds.makeRepeatBlockButton
            UiIds.selectByMaterialButton
            UiIds.setLayerHeightButton
            UiIds.setLayerMaterialButton
            UiIds.setOrientationOfSelectedButton
            UiIds.removeSelectedLayersButton
            UiIds.repeatCountStepper
            UiIds.qwotEntryBox
            UiIds.saveButton
            UiIds.cancelButton
        ]

    let private materialOptionId (id : MaterialId) : string = UiIds.materialOption (string id.value)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the window mounts with every slice-mandated UiId present`` () =
        HeadlessSession.run (fun () ->
            let materials, samples = freshProxies ()
            let window = SampleEditorWindow(materials, samples, None)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(matchesId UiIds.window window, "the window itself carries the SampleEditorWindow id")
            for id in mandatedIds do
                Assert.True(isPresent window id, sprintf "%s is missing from the mounted window" id)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: repeating a 2-layer selection K times expands the structure to 2K films`` () =
        HeadlessSession.run (fun () ->
            let materials, samples = freshProxies ()
            let window = SampleEditorWindow(materials, samples, None)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // Build a 2-layer stack by UiIds: glass then vacuum.
            clickOn window (materialOptionId MaterialIds.glass152)
            clickOn window UiIds.addLayerButton
            clickOn window (materialOptionId MaterialIds.vacuum)
            clickOn window UiIds.addLayerButton
            Assert.Equal("2", textOf window UiIds.filmsCount)
            // Select BOTH layers, step the fold count to K = 3, and fold.
            clickOn window (UiIds.layerRow 0)
            clickOn window (UiIds.layerRow 1)
            clickOn window UiIds.repeatCountStepperPlus
            clickOn window UiIds.makeRepeatBlockButton
            // 2 layers × K=3 periods = 6 films, shown by the structure readout.
            Assert.Equal("6", textOf window UiIds.filmsCount)
            // The group renders as ONE collapsible super-row with its cell layers nested beneath.
            Assert.True(isPresent window (UiIds.groupRow 0), "the period super-row is missing")
            Assert.True(isPresent window (UiIds.groupExpander 0), "the rotating-triangle expander is missing")
            Assert.True(isPresent window (UiIds.cellLayerRow 0 0), "cell layer 0 is missing")
            Assert.True(isPresent window (UiIds.cellLayerRow 0 1), "cell layer 1 is missing")
            Assert.False(isPresent window (UiIds.layerRow 0), "the folded singles must no longer render as top-level rows")
            // The group's INLINE stepper resizes by whole periods: 3 → 4 ⇒ 8 films.
            clickOn window (UiIds.groupStepperPlus 0)
            Assert.Equal("8", textOf window UiIds.filmsCount)
            // Collapsing the super-row removes the nested cell rows (the expander toggles).
            clickOn window (UiIds.groupExpander 0)
            Assert.False(isPresent window (UiIds.cellLayerRow 0 0), "a collapsed group must hide its cell rows")
            Assert.Equal("8", textOf window UiIds.filmsCount)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: select-by-material bulk set-thickness updates only the matching layers`` () =
        HeadlessSession.run (fun () ->
            let materials, samples = freshProxies ()
            let window = SampleEditorWindow(materials, samples, Some (threeFilmSample ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // Choose glass, select every glass layer, and bulk-set the thickness to 5 nm.
            clickOn window (materialOptionId MaterialIds.glass152)
            clickOn window UiIds.selectByMaterialButton
            setText window UiIds.layerHeightBox "5"
            clickOn window UiIds.setLayerHeightButton
            // The two glass rows changed; the vacuum row between them did not.
            Assert.Equal("5 nm", textOf window (UiIds.layerThickness 0))
            Assert.Equal("50 nm", textOf window (UiIds.layerThickness 1))
            Assert.Equal("5 nm", textOf window (UiIds.layerThickness 2))
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: Save persists a NEW sample through SampleProxy.addSample and closes`` () =
        HeadlessSession.run (fun () ->
            let materials, samples = freshProxies ()
            let seededCount =
                match samples.listSamples () with
                | Ok all -> List.length all
                | Error e -> failwith (sprintf "seed listing failed: %A" e)
            let window = SampleEditorWindow(materials, samples, None)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            setText window UiIds.nameBox "Headless stack"
            setText window UiIds.descriptionBox "made by the headless test"
            clickOn window (materialOptionId MaterialIds.glass152)
            clickOn window UiIds.addLayerButton
            clickOn window UiIds.saveButton
            Assert.False(window.IsVisible)
            match samples.listSamples () with
            | Ok all ->
                Assert.Equal(seededCount + 1, List.length all)
                match all |> List.tryFind (fun s -> s.name = "Headless stack") with
                | Some saved ->
                    Assert.Equal("made by the headless test", saved.description)
                    Assert.Equal(1, List.length saved.structure.expandedFilms)
                | None -> Assert.Fail("the new sample was not persisted")
            | Error e -> Assert.Fail(sprintf "listSamples failed: %A" e))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: Save UPDATES an existing sample in place through SampleProxy.updateSample`` () =
        HeadlessSession.run (fun () ->
            let materials, samples = freshProxies ()
            let existing = SeedSamples.glassFilm200
            let seededCount =
                match samples.listSamples () with
                | Ok all -> List.length all
                | Error e -> failwith (sprintf "seed listing failed: %A" e)
            let window = SampleEditorWindow(materials, samples, Some existing)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            setText window UiIds.nameBox "Renamed film"
            clickOn window UiIds.saveButton
            Assert.False(window.IsVisible)
            match samples.tryGetSample existing.id with
            | Ok (Some updated) -> Assert.Equal("Renamed film", updated.name)
            | Ok None -> Assert.Fail("the existing sample vanished")
            | Error e -> Assert.Fail(sprintf "tryGetSample failed: %A" e)
            match samples.listSamples () with
            | Ok all -> Assert.Equal(seededCount, List.length all)
            | Error e -> Assert.Fail(sprintf "listSamples failed: %A" e))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``Cancel discards the edit and closes without touching the store`` () =
        HeadlessSession.run (fun () ->
            let materials, samples = freshProxies ()
            let existing = SeedSamples.glassFilm200
            let window = SampleEditorWindow(materials, samples, Some existing)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            setText window UiIds.nameBox "Should not persist"
            clickOn window UiIds.cancelButton
            Assert.False(window.IsVisible)
            match samples.tryGetSample existing.id with
            | Ok (Some kept) -> Assert.Equal(existing.name, kept.name)
            | Ok None -> Assert.Fail("the existing sample vanished")
            | Error e -> Assert.Fail(sprintf "tryGetSample failed: %A" e))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the per-layer orientation editor exists for anisotropic layers and is REMOVED for isotropic ones`` () =
        HeadlessSession.run (fun () ->
            let materials, samples = freshProxies ()
            let sample =
                {
                    id = newSampleId ()
                    name = "Orientation visibility"
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
                    description = "uniaxial over glass"
                }
            let window = SampleEditorWindow(materials, samples, Some sample)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(isPresent window (UiIds.layerOrientation 0), "the anisotropic layer must carry its orientation editor")
            Assert.False(isPresent window (UiIds.layerOrientation 1), "the isotropic layer's orientation editor must be REMOVED, not greyed")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the QWOT entry derives the read-only canonical-metres thickness and Set thickness applies it`` () =
        HeadlessSession.run (fun () ->
            let materials, samples = freshProxies ()
            let window = SampleEditorWindow(materials, samples, None)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            clickOn window (materialOptionId MaterialIds.glass152)
            clickOn window UiIds.addLayerButton
            clickOn window (UiIds.layerRow 0)
            setText window UiIds.qwotEntryBox "600"
            // t = λ/(4n) = 600/(4·1.52) nm, shown read-only in canonical metres.
            Assert.Equal(sprintf "%g m" (600.0 / (4.0 * 1.52) * 1.0e-9), textOf window UiIds.qwotDerivedText)
            clickOn window UiIds.setLayerHeightButton
            Assert.Equal(sprintf "%g nm" (600.0 / (4.0 * 1.52)), textOf window (UiIds.layerThickness 0))
            window.Close())
