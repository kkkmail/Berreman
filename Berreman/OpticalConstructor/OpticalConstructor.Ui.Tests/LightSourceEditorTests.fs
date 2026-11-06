namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Automation
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
open Berreman.Fields
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.MaterialStore
open OpticalConstructor.Domain.SampleStore
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.WindowMode
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Ui
open OpticalConstructor.Tests

/// Spec 0046 — the Library's light-source editor: its pure MVU (name, wavelength in a chosen unit, Save
/// through the user-sources store, the dirty exit gate), the Library window's Add source / Edit / Remove
/// verbs for user sources (a protected seeded source keeps no Edit and refuses Remove), a user or seeded
/// source bound on the Main table resolving through the live Library, and the editor window driven
/// headless.
module LightSourceEditorTests =

    module SE = LightSourceEditorView
    module LW = LibraryWindowView
    module Scene = TableAndElementRotationView

    let private nmOf (w : WaveLength) : float = w.value / nmToMeter / 1.0<nm>

    /// A recording editor context over `sources`: the close request appends a tag.
    let private editorContext (sources : SourceProxy) : ResizeArray<string> * SE.SourceEditorContext =
        let calls = ResizeArray<string>()
        calls, { sources = sources; requestClose = fun () -> calls.Add "close" }

    // ------------------------------ the editor (pure) ------------------------------

    [<Fact>]
    let ``a new source opens at 600 nm, pristine, and Save stores it user-managed under the minted id`` () =
        let sources = SourceProxy.createInMemory ()
        let calls, context = editorContext sources
        let m0 = SE.initLightSourceEditor context (SE.NewSource (Placement.ElementValueId "src-new-1"))
        Assert.False(m0.isDirty)
        Assert.Equal(Some 600.0, m0.waveLength |> Option.map nmOf)
        let saved =
            m0
            |> SE.updateLightSourceEditor (SE.SetLightSourceName (EntryName "My EUV line"))
            |> SE.updateLightSourceEditor (SE.SetWaveLengthText (ControlCaption "13.4"))
            |> SE.updateLightSourceEditor SE.LightSourceSaveClicked
        Assert.Equal(None, saved.status)
        Assert.Equal<string list>([ "close" ], List.ofSeq calls)
        match sources.tryGetSource (Placement.ElementValueId "src-new-1") with
        | Ok (Some s) ->
            Assert.Equal("My EUV line", s.name.value)
            Assert.True(abs (nmOf s.waveLength - 13.4) < 1.0e-9)
            Assert.Equal<EntryProtection>(UserManaged, s.protection)
        | other -> Assert.Fail($"expected the saved source, got %A{other}")

    [<Fact>]
    let ``switching the unit keeps the wavelength and shows it in the new unit`` () =
        let _, context = editorContext (SourceProxy.createInMemory ())
        let m = SE.initLightSourceEditor context (SE.NewSource (Placement.ElementValueId "src-x")) |> SE.updateLightSourceEditor (SE.SetUnit ElectronVolt)
        Assert.Equal(ElectronVolt, m.unit)
        Assert.Equal(Some 600.0, m.waveLength |> Option.map nmOf)
        Assert.Equal(SE.formatIn ElectronVolt (WaveLength.nm 600.0<nm>), m.waveLengthText)
        // The box echoing that text back keeps the stored value exactly (no rounding drift, not dirty).
        let echoed = SE.updateLightSourceEditor (SE.SetWaveLengthText m.waveLengthText) m
        Assert.False(echoed.isDirty)
        // A value typed in eV is read in eV: 91.84 eV is the 13.5 nm EUV line.
        let euv = SE.updateLightSourceEditor (SE.SetWaveLengthText (ControlCaption "91.84")) m
        Assert.True(abs ((euv.waveLength |> Option.map nmOf |> Option.defaultValue 0.0) - 13.5) < 1.0e-3)

    [<Fact>]
    let ``Save refuses a blank name or an unparsable wavelength and keeps the editor open`` () =
        let sources = SourceProxy.createInMemory ()
        let calls, context = editorContext sources
        let blank = SE.initLightSourceEditor context (SE.NewSource (Placement.ElementValueId "src-y")) |> SE.updateLightSourceEditor SE.LightSourceSaveClicked
        Assert.True(Option.isSome blank.status, "a blank name is refused with a reason")
        let bad = SE.initLightSourceEditor context (SE.NewSource (Placement.ElementValueId "src-y")) |> SE.updateLightSourceEditor (SE.SetLightSourceName (EntryName "X")) |> SE.updateLightSourceEditor (SE.SetWaveLengthText (ControlCaption "abc"))
        Assert.Equal(None, bad.waveLength)
        Assert.Contains("enter a positive wavelength", (SE.updateLightSourceEditor SE.LightSourceSaveClicked bad).status |> Option.map (fun reason -> reason.value) |> Option.defaultValue "")
        Assert.Empty(calls)
        Assert.Equal(Ok [], sources.listSources ())

    [<Fact>]
    let ``editing a user source replaces it; Cancel closes a pristine editor and confirms a dirty one`` () =
        let sources = SourceProxy.createInMemory ()
        let original : SourcePreset = { id = Placement.ElementValueId "src-u"; name = EntryName "Line A"; waveLength = WaveLength.nm 500.0<nm>; tuning = FixedLine; protection = UserManaged }
        Assert.Equal(Ok (), sources.saveSource original)
        let calls, context = editorContext sources
        let m0 = SE.initLightSourceEditor context (SE.EditSource original)
        Assert.Equal(WindowLauncher.Persisted, m0.freshness)
        // Pristine: Cancel closes at once.
        SE.updateLightSourceEditor SE.LightSourceCancelClicked m0 |> ignore
        Assert.Equal<string list>([ "close" ], List.ofSeq calls)
        calls.Clear()
        // Dirty: Cancel asks first; Keep editing stays; Discard closes WITHOUT saving.
        let dirty = SE.updateLightSourceEditor (SE.SetLightSourceName (EntryName "Line B")) m0
        let asking = SE.updateLightSourceEditor SE.LightSourceCancelClicked dirty
        Assert.Equal(SE.LightSourceConfirmingDiscard, asking.exit)
        Assert.Empty(calls)
        Assert.Equal(SE.LightSourceEditing, (SE.updateLightSourceEditor SE.LightSourceKeepEditing asking).exit)
        SE.updateLightSourceEditor SE.LightSourceDiscardConfirmed asking |> ignore
        Assert.Equal<string list>([ "close" ], List.ofSeq calls)
        Assert.Equal(Ok (Some original), sources.tryGetSource (Placement.ElementValueId "src-u"))
        // Save replaces it in place.
        SE.updateLightSourceEditor SE.LightSourceSaveClicked dirty |> ignore
        Assert.Equal(Ok (Some { original with name = EntryName "Line B" }), sources.tryGetSource (Placement.ElementValueId "src-u"))

    // ------------------------------ the Library window verbs (pure) ------------------------------

    let private libraryContext (sources : SourceProxy) : ResizeArray<string> * LW.LibraryWindowContext =
        let calls = ResizeArray<string>()
        let samples = SampleProxy.createInMemory VersionsInUse.empty
        let materials = MaterialProxy.createInMemory (samplesReferencing samples) VersionsInUse.empty
        calls,
        {
            library = Library.createInMemory ()
            samples = samples
            materials = materials
            categories = BuiltinCatalogue.categoryStore (fun _ -> [])
            treeAutoBuildThreshold = TreeAutoBuildThreshold.defaultValue
            thicknessBucketCap = ThicknessBucketCap.defaultValue
            openSampleEditor = fun _ -> calls.Add "sample-editor"
            sources = sources
            openSourceEditor =
                fun intent ->
                    calls.Add(
                        match intent with
                        | SE.NewSource mintedId -> "source-add:" + mintedId.value
                        | SE.EditSource s -> "source-edit:" + s.name.value)
            requestClose = fun () -> calls.Add "close"
        }

    [<Fact>]
    let ``Add source opens the editor on a freshly minted source id`` () =
        let calls, context = libraryContext (SourceProxy.createInMemory ())
        LW.updateLibraryWindow LW.AddSource (LW.initLibraryWindow context Browse) |> ignore
        Assert.Equal(1, calls.Count)
        // Spec 0050 §3.3: the minted id is the canonical string of a GUID, as a sample's is.
        let prefix = "source-add:"
        Assert.StartsWith(prefix, calls.[0])
        let minted = calls.[0].Substring(prefix.Length)
        match System.Guid.TryParse minted with
        | true, guid -> Assert.Equal(string guid, minted)
        | false, _ -> Assert.Fail $"the minted source id '{minted}' is not the string of a GUID"

    [<Fact>]
    let ``a user source joins the Library, offers Edit, and is removed through the confirm gate`` () =
        let sources = SourceProxy.createInMemory ()
        let mine : SourcePreset = { id = Placement.ElementValueId "src-mine"; name = EntryName "My line"; waveLength = WaveLength.nm 450.0<nm>; tuning = FixedLine; protection = UserManaged }
        Assert.Equal(Ok (), sources.saveSource mine)
        let calls, context = libraryContext sources
        let m0 = LW.initLibraryWindow context Browse
        Assert.Contains("src-mine", m0.filteredEntries |> List.map (fun e -> e.entryId.value))
        let selected = LW.updateLibraryWindow (LW.SelectLibraryEntry (OpticalConstructor.Domain.Placement.ElementValueId "src-mine")) m0
        Assert.Equal(Some mine, selected.editableSource)
        LW.updateLibraryWindow LW.EditSelectedEntry selected |> ignore
        Assert.Equal<string list>([ "source-edit:My line" ], List.ofSeq calls)
        let armed = LW.updateLibraryWindow LW.RequestRemoveSelectedEntry selected
        Assert.Equal(LW.PendingSourceRemove (OpticalConstructor.Domain.Placement.ElementValueId "src-mine"), armed.removeGate)
        let removed = LW.updateLibraryWindow LW.ConfirmEntryRemove armed
        Assert.Equal(LW.NoPendingLibraryRemove, removed.removeGate)
        Assert.Equal(None, removed.selectedEntryId)
        Assert.Equal(Ok [], sources.listSources ())

    [<Fact>]
    let ``a seeded source is protected: no Edit, and Remove refuses with the reason`` () =
        let calls, context = libraryContext (SourceProxy.createInMemory ())
        let selected = LW.updateLibraryWindow (LW.SelectLibraryEntry PresetIds.euv13p5) (LW.initLibraryWindow context Browse)
        Assert.Equal(None, selected.editableSource)
        LW.updateLibraryWindow LW.EditSelectedEntry selected |> ignore
        Assert.Empty(calls)
        match (LW.updateLibraryWindow LW.RequestRemoveSelectedEntry selected).lastError with
        | Some (LW.ProtectedEntryRefusedErr reason) -> Assert.Contains("EUV 13.5 nm", reason)
        | other -> Assert.Fail($"expected the protected refusal, got %A{other}")

    // ------------------------------ on the Main table (headless) ------------------------------

    /// The Main readout text for `model` (the scene rendered once headless).
    let private readoutOf (model : Scene.TableAndElementRotationModel) : string =
        let window = Window(Width = 980.0, Height = 900.0)
        window.Content <- Avalonia.FuncUI.Component(fun _ -> Scene.mainView model ignore)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        let text =
            window.GetVisualDescendants()
            |> Seq.tryPick (fun v ->
                match v with
                | :? TextBlock as t when t.Name = ConstructorUiId.teReadout.value -> Some t.Text
                | _ -> None)
            |> Option.defaultValue ""
        window.Close()
        text

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a user source and a seeded lithography source bind to the table's source and resolve`` () =
        HeadlessSession.run (fun () ->
            let m0 = { Scene.initMain () with selection = Scene.ElementSelected (Scene.SceneElementIndex 0) }
            let mine : SourcePreset = { id = Placement.ElementValueId "src-mine"; name = EntryName "My 13.4 nm line"; waveLength = WaveLength.nm 13.4<nm>; tuning = FixedLine; protection = UserManaged }
            Assert.Equal(Ok (), m0.sources.saveSource mine)
            let sourceId = (List.item 0 m0.elements).id
            let boundMine = Scene.updateTableAndElementRotation (Scene.BindValueIdTo (sourceId, Placement.ElementValueId "src-mine")) m0
            Assert.Equal(Some (Placement.ElementValueId "src-mine"), (List.item 0 boundMine.elements).placement.valueId)
            Assert.Contains("bound: My 13.4 nm line", readoutOf boundMine)
            let boundArf = Scene.updateTableAndElementRotation (Scene.BindValueIdTo (sourceId, PresetIds.arf193)) m0
            Assert.Contains("bound: ArF excimer 193 nm", readoutOf boundArf))

    // ------------------------------ the editor window (headless) ------------------------------

    let private clickOn (window : Window) (autoId : string) : unit =
        let found =
            window.GetVisualDescendants()
            |> Seq.tryPick (fun v ->
                match v with
                | :? Border as b when AutomationProperties.GetAutomationId b = autoId && b.IsEffectivelyVisible -> Some b
                | _ -> None)
        match found with
        | None -> Assert.Fail($"%s{autoId} was not found")
        | Some b ->
            match Option.ofNullable (b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)) with
            | Some c ->
                window.MouseDown(c, MouseButton.Left, RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                if window.IsVisible then
                    window.MouseUp(c, MouseButton.Left, RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
            | None -> Assert.Fail($"%s{autoId} has no on-screen position")

    let private typeInto (window : Window) (name : string) (text : string) : unit =
        match window.GetVisualDescendants() |> Seq.tryPick (fun v -> match v with :? TextBox as t when t.Name = name -> Some t | _ -> None) with
        | Some box ->
            box.Text <- text
            Dispatcher.UIThread.RunJobs()
        | None -> Assert.Fail($"text box %s{name} was not found")

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the editor window stores a typed source on Save and closes`` () =
        HeadlessSession.run (fun () ->
            let sources = SourceProxy.createInMemory ()
            let window = LightSourceEditorWindow(sources, SE.NewSource (Placement.ElementValueId "src-typed"))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            typeInto window ConstructorUiId.sourceNameBox.value "Typed KrF"
            clickOn window (ConstructorUiId.sourceWaveLengthUnit Nanometer.uiKey).value
            typeInto window ConstructorUiId.sourceWaveLengthBox.value "248.4"
            clickOn window ConstructorUiId.sourceSaveButton.value
            Assert.False(window.IsVisible, "Save closes the editor")
            match sources.tryGetSource (Placement.ElementValueId "src-typed") with
            | Ok (Some s) ->
                Assert.Equal("Typed KrF", s.name.value)
                Assert.True(abs (nmOf s.waveLength - 248.4) < 1.0e-9)
            | other -> Assert.Fail($"expected the typed source, got %A{other}"))
