namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Media
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Facets
open OpticalConstructor.Domain.LibraryFacets
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryBackup
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.WindowMode
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls
open OpticalConstructor.Ui
open OpticalConstructor.Ui.ImportDialogView
open OpticalConstructor.Tests

/// Spec 0038 Part F (step 015) — the Library window (UICOMP_XDUO_0010): the single-instance
/// window instantiating the step-012 `FacetedTreeControls` over the step-011 library facet
/// catalogue (default representation "By kind"), beside the view panel and the
/// Add sample / Make multilayer / Edit / Remove verbs rewired from the retired Library
/// (samples workbench) bay. Two layers, the repo precedent: pure tests for the MVU model, the
/// engine projection (incl. the FIRST numeric-facet host — film-thickness buckets + the manual
/// min–max range) and the protection/confirm discipline; headless proofs driving the REAL
/// window (and the REAL workbench strip button) by automation ids — the slice acceptance: the
/// strip button opens ONE window (a second click ACTIVATES it), the by-kind tree lists every
/// entry kind with counts, the sample verbs operate over the shared app-scoped stores, and
/// removing a `ProtectedBuiltIn` entry is refused with a typed reason and changes nothing.
/// Step 016 adds the Select-state suite: the Select/Close pair, the pre-applied NON-REMOVABLE
/// kind constraint, the targeted `onSelected` dispatch into the workbench, the
/// close-on-table-selection-change staleness, and the vanished-target no-op + status line.
/// Step 017 adds the Selector-bay Choose…/quick-pick suite: the threshold-gated inline strip,
/// Choose… opening the kind-constrained Select-state window through the REAL launcher seam,
/// the identical-valueId convergence of the two bind paths, and the re-target on a second
/// Choose… with the reference-keyed session handle.
module LibraryWindowTests =

    module LW = OpticalConstructor.Ui.LibraryWindowView
    module Scene = OpticalConstructor.Ui.TableAndElementRotationView

    // ============================ shared helpers ============================

    /// A control matches `id` by its `Name` OR its `AutomationProperties.AutomationId` (the
    /// window's rows / offers / verbs live in variable-membership lists, so they carry an
    /// AutomationId — the FacetedTreeControls discipline).
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
            // Scroll the box into view first — BuiltinCatalogue.sources () (spec 0046) puts
            // later rows below the window's visible area, where a click at their centre lands nowhere.
            b.BringIntoView() |> ignore
            Dispatcher.UIThread.RunJobs()
            let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
            if c.HasValue then
                window.MouseDown(c.Value, MouseButton.Left, RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                if window.IsVisible then
                    window.MouseUp(c.Value, MouseButton.Left, RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"%s{id} has no on-screen position")

    /// Expand the tree node with this code by clicking its disclosure chevron (spec 0040 step 002).
    /// The tree is collapsed by default, so a node's children (entry leaves, facet branches) render
    /// only after this — and the expansion PERSISTS across the window's later re-renders.
    let private expandNode (window : Window) (code : string) : unit =
        clickOn window (ConstructorUiId.facetTreeChevron (ConstructorUiKey code)).value

    /// Expand the "entries" group so its entry-leaf children render (the common precondition for the
    /// leaf-clicking headless proofs, which pre-date the collapsed-by-default tree).
    let private expandEntries (window : Window) : unit = expandNode window "entries"

    /// The display text under the control carrying `id` (the control itself when it is a
    /// TextBlock, its first TextBlock descendant otherwise).
    let private textOf (window : Window) (id : string) : string =
        match tryFindControl window id with
        | None -> ""
        | Some (:? TextBlock as t) -> (if isNull t.Text then "" else t.Text)
        | Some c ->
            c.GetVisualDescendants()
            |> Seq.tryPick (function :? TextBlock as t when not (isNull t.Text) -> Some t.Text | _ -> None)
            |> Option.defaultValue ""

    /// Set the text of the TextBox carrying `id` (the editor windows subscribe text changes;
    /// the faceted filter box does NOT — it is driven through `commitFilter` below).
    let private setText (window : Window) (id : string) (text : string) : unit =
        match tryFindControl window id with
        | Some (:? TextBox as tb) ->
            tb.Text <- text
            Dispatcher.UIThread.RunJobs()
        | Some c -> Assert.Fail($"%s{id} is a %s{c.GetType().Name}, not a TextBox")
        | None -> Assert.Fail($"%s{id} was not found")

    /// Commit `text` through the REAL faceted filter box: set the box text (no dispatch — the
    /// control has no text-change subscription) and press Enter, the control's commit gesture.
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

    /// The clickable label Border of the tree row carrying `id` (the row's `treeNode` id — the
    /// chevron carries a DISTINCT id, so this is unambiguously the label box).
    let private labelBorderOf (window : Window) (id : string) : Border =
        match window.GetVisualDescendants() |> Seq.tryPick (function :? Border as b when matchesId id b -> Some b | _ -> None) with
        | Some b -> b
        | None -> failwith $"%s{id} label border was not found"

    /// A Border's solid fill colour (None when it carries a non-solid brush).
    let private backgroundColorOf (b : Border) : Color option =
        match b.Background with
        | :? SolidColorBrush as s -> Some s.Color
        | _ -> None

    // The FacetedTreeControls idle / chosen row fills (kept private there; mirrored for the proof).
    let private idleFill = Color.FromRgb(232uy, 232uy, 232uy)
    /// Count BuiltinCatalogue.samples () and BuiltinCatalogue.presets () so catalogue additions
    /// retain the whole-corpus expectation (spec 0046; spec 0050 S1.7).
    let private seededCorpus : int = List.length (BuiltinCatalogue.samples ()) + List.length (BuiltinCatalogue.presets ())
    let private chosenFill = Color.FromRgb(150uy, 185uy, 235uy)

    /// BuiltinCatalogue.sampleStore precedes materialStore so removal consults LIVE samples
    /// (spec 0050 S1.7).
    let private freshStores () : LibraryProxy * SampleProxy * MaterialProxy =
        let samples = BuiltinCatalogue.sampleStore ()
        let materials = BuiltinCatalogue.materialStore (samplesReferencing samples)
        BuiltinCatalogue.library (), samples, materials

    /// A recording stub context (the functional-proxy seam): the launcher and the close
    /// request append tags, so a verb's request is observable without opening a window.
    let private stubContext (library : LibraryProxy) (samples : SampleProxy) (materials : MaterialProxy) : ResizeArray<string> * LW.LibraryWindowContext =
        let calls = ResizeArray<string>()
        let context : LW.LibraryWindowContext =
            {
                library = library
                samples = samples
                materials = materials
                categories = BuiltinCatalogue.categoryStore (fun _ -> [])
                libraryBackups = LibraryBackupProxy.createInMemory ()
                catalogueImports = CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy
                openImportDialog = fun _ -> calls.Add "import-requested"
                treeAutoBuildThreshold = TreeAutoBuildThreshold.defaultValue
                thicknessBucketCap = ThicknessBucketCap.defaultValue
                openSampleEditor =
                    fun intent ->
                        calls.Add(
                            match intent with
                            | SampleEditorView.NewBlankSample mintedId -> $"sample-add:{mintedId.value}"
                            | SampleEditorView.NewSeededMultilayer mintedId -> $"sample-multilayer:{mintedId.value}"
                            | SampleEditorView.EditSample s -> "sample-edit:" + s.name.value)
                // Spec 0046: a fresh user-sources store (reachable as `context.sources`) and a recording
                // source-editor launcher.
                sources = SourceProxy.createInMemory ()
                openSourceEditor =
                    fun intent ->
                        calls.Add(
                            match intent with
                            | LightSourceEditorView.NewSource mintedId -> "source-add:" + mintedId.value
                            | LightSourceEditorView.EditSource s -> "source-edit:" + s.name.value)
                requestClose = fun () -> calls.Add "close-requested"
            }
        calls, context

    let private freshModel () : ResizeArray<string> * LW.LibraryWindowModel =
        let library, samples, materials = freshStores ()
        let calls, context = stubContext library samples materials
        calls, LW.initLibraryWindow context Browse

    /// A fresh model PLUS its live stores (for tests that mutate behind the window's back).
    let private freshModelWithStores () : SampleProxy * LW.LibraryWindowModel =
        let library, samples, materials = freshStores ()
        let _, context = stubContext library samples materials
        samples, LW.initLibraryWindow context Browse

    /// A recording Select-session context (spec 0038 step 016): `onSelected` tags the chosen
    /// entry id, `onCancelled` tags the cancel — the window guarantees exactly one fires.
    let private selectContext (kind : CatalogueKind) (target : SelectionTarget) : ResizeArray<string> * SelectionContext<LibraryEntry> =
        let events = ResizeArray<string>()
        let context : SelectionContext<LibraryEntry> =
            {
                kindConstraint = KindConstraint kind
                currentEntryIdOpt = None
                target = target
                onSelected = fun entry -> events.Add ("selected:" + entry.entryId.value)
                onCancelled = fun () -> events.Add "cancelled"
            }
        events, context

    /// A fresh SELECT-state model over recording stubs: the session events, the context calls
    /// (incl. "close-requested"), and the model.
    let private freshSelectModel (kind : CatalogueKind) (target : SelectionTarget) : ResizeArray<string> * ResizeArray<string> * LW.LibraryWindowModel =
        let library, samples, materials = freshStores ()
        let calls, context = stubContext library samples materials
        let events, selectCtx = selectContext kind target
        events, calls, LW.initLibraryWindow context (Select selectCtx)

    let private filteredEntryIds (m : LW.LibraryWindowModel) : string list =
        m.filteredEntries |> List.map (fun e -> e.entryId.value)

    let private glassFilm600EntryId : ElementValueId = SampleIds.glassFilm600Id.toElementValueId

    /// The kind-facet constraint helper (a single discrete key).
    let private kindConstraint (kind : string) : LW.LibraryWindowMsg =
        LW.ApplyFacetConstraint (entryKindFacetKey, DiscreteSelection (Set.singleton (DiscreteKey kind)))

    let private catalogueFolderFixture () : FolderName * CatalogueData =
        let folder = FolderName (System.IO.Path.Combine(System.AppContext.BaseDirectory, "LibraryWindowTests", System.Guid.NewGuid().ToString("N")))
        let source =
            {
                BuiltinCatalogue.source PresetIds.source600 with
                    id = newSourceId ()
                    name = EntryName "Registered catalogue source"
                    protection = UserManaged
            }
        let content =
            LibraryEntryContent.ofSourcePreset VersionNumber.first UserProvenance source
            |> Result.defaultWith (fun error -> failwith error.describe)
        let envelope = content.``sealed`` |> Result.defaultWith (fun error -> failwith error.describe)
        let id = CatalogueId (System.Guid.NewGuid())
        let name = CatalogueName "Library window catalogue"
        let catalogue =
            CatalogueData.tryCreate id name VersionNumber.first [ envelope ]
            |> Result.defaultWith (fun error -> failwith error.describe)
        (CatalogueProxy.create folder).writeCatalogue catalogue
        |> Result.defaultWith (fun error -> failwith error.describe)
        folder, catalogue

    let private confirmCatalogueRegistration (context : CatalogueImportContext) (pending : PendingCatalogueImport) : unit =
        pending.decisions |> List.map _.byDefault |> context.settle pending
        |> Result.defaultWith (fun error -> failwith error.describe)
        |> ignore

    let private registeredCatalogueFixture (model : LW.LibraryWindowModel) (folder : FolderName) : LW.LibraryWindowModel =
        let pendingModel = LW.updateLibraryWindow (LW.RegisterCatalogueFolder folder) model
        let pending = Assert.Single pendingModel.pendingImports
        confirmCatalogueRegistration model.context.catalogueImports pending
        pendingModel

    let private freshImportModel () : ResizeArray<ImportDialogRequest> * LW.LibraryWindowModel =
        let _, model = freshModel ()
        let requests = ResizeArray<ImportDialogRequest>()
        let context =
            {
                model.context with
                    openImportDialog = requests.Add
            }
        requests, { model with context = context }

    let private droppedSourceFixture () : FileName * SourcePreset =
        let folder, catalogue = catalogueFolderFixture ()
        let envelope = Assert.Single catalogue.envelopes
        let source = envelope.content.toSourcePreset |> Result.defaultWith (fun error -> failwith error.describe)
        let text = envelope.fileText |> Result.defaultWith (fun error -> failwith error.describe)
        let leaf = FileName "dropped.source-json"
        let file = leaf.combine folder
        System.IO.File.WriteAllText(file.value, text.value)
        file, source

    let private freshLibraryExportContext () : InMemoryCatalogueStores * LW.LibraryWindowContext =
        let stores = CatalogueImportProxy.createInMemory ()
        let imports = CatalogueImportContext.create stores.proxy
        imports.importFrom (CatalogueProxy.createBuiltIn ())
        |> Result.bind (fun decisions -> imports.apply (decisions |> List.map _.byDefault))
        |> Result.defaultWith (fun error -> failwith error.describe)
        |> ignore
        let _, context = stubContext stores.libraryEntries stores.samples stores.materials
        stores,
        {
            context with
                categories = stores.categories
                catalogueImports = imports
        }

    let private freshLibraryExportFolder () : FolderName =
        FolderName (System.IO.Path.Combine(System.AppContext.BaseDirectory, "LibraryWindowTests", System.Guid.NewGuid().ToString("N")))

    let private mountLibraryExportView (window : HostWindow) (model : LW.LibraryWindowModel) : unit =
        Program.mkSimple (fun () -> model) LW.updateLibraryWindow LW.libraryWindowView
        |> Program.withHost window
        |> Program.run
        window.Show()
        Dispatcher.UIThread.RunJobs()

    let private assertLibraryExportPreservesState (before : LW.LibraryWindowModel) (after : LW.LibraryWindowModel) : unit =
        Assert.Same(before.context, after.context)
        Assert.Equal(before.selectedEntryId, after.selectedEntryId)
        Assert.Equal(before.selectedNodeOpt, after.selectedNodeOpt)
        Assert.Equal(before.viewedVersion, after.viewedVersion)
        Assert.Equal(before.removeGate, after.removeGate)
        Assert.Equal(before.lifecycleGate, after.lifecycleGate)
        Assert.Equal(before.restoreGate, after.restoreGate)
        Assert.Equal<AppliedConstraint list>(before.appliedFacets, after.appliedFacets)
        Assert.Equal(before.textFilter, after.textFilter)
        match before.mode, after.mode with
        | Browse, Browse -> ()
        | Select expected, Select actual -> Assert.Same(expected, actual)
        | _ -> Assert.Fail("An export must preserve the window mode.")

    [<Fact>]
    let ``the export and attribution UiIds of the Library window are the stable intent-named ids`` () : unit =
        Assert.Equal("LibraryExportButton", ConstructorUiId.libraryExportButton.value)
        Assert.Equal("LibraryExportShownButton", ConstructorUiId.libraryExportShownButton.value)
        Assert.Equal("LibraryExportMyLibraryButton", ConstructorUiId.libraryExportMyLibraryButton.value)
        Assert.Equal("LibraryExportStatusText", ConstructorUiId.libraryExportStatusText.value)
        Assert.Equal("LibraryAttributionPanel", ConstructorUiId.libraryAttributionPanel.value)
        HeadlessSession.run (fun () ->
            let _, context = freshLibraryExportContext ()
            let initial = LW.initLibraryWindow context Browse
            let selected = initial |> LW.updateLibraryWindow (LW.SelectLibraryEntry PresetIds.source600)
            let targetId = ElementId "export-selection"
            let target = TableElementTarget targetId
            let _, selection = selectContext LightSource target
            let selectModel = LW.initLibraryWindow context (Select selection) |> LW.updateLibraryWindow (LW.SelectLibraryEntry PresetIds.source600)
            let unmatched = initial |> LW.updateLibraryWindow (LW.CommitLibraryTextFilter (TextQuery "no matching library entry"))
            for model in [ initial; selected; selectModel; unmatched ] do
                let window = HostWindow(Width = 1150.0, Height = 800.0)
                try
                    mountLibraryExportView window model
                    match model.exportableSelection with
                    | Some _ ->
                        Assert.True(isPresent window ConstructorUiId.libraryExportButton.value)
                        Assert.Equal("Export…", textOf window ConstructorUiId.libraryExportButton.value)
                    | None -> Assert.False(isPresent window ConstructorUiId.libraryExportButton.value)
                    match model.mode with
                    | Browse ->
                        Assert.True(isPresent window ConstructorUiId.libraryExportMyLibraryButton.value)
                        Assert.Equal("Export my library…", textOf window ConstructorUiId.libraryExportMyLibraryButton.value)
                        Assert.Equal(not (List.isEmpty model.filteredEntries), isPresent window ConstructorUiId.libraryExportShownButton.value)
                    | Select _ ->
                        Assert.False(isPresent window ConstructorUiId.libraryExportShownButton.value)
                        Assert.False(isPresent window ConstructorUiId.libraryExportMyLibraryButton.value)
                    Assert.False(isPresent window ConstructorUiId.libraryExportStatusText.value)
                finally window.Close())

    [<Fact>]
    let ``ExportSelectedLibraryEntry writes the selected entry's file and pins its version`` () : unit =
        let stores, context = freshLibraryExportContext ()
        let materialId = newMaterialId ()
        let sampleId = newSampleId ()
        let materialName = EntryName "Lab glass"
        let sampleName = EntryName "Lab stack"
        let material = BuiltinCatalogue.userMaterialContent materialId materialName
        let sample = BuiltinCatalogue.sampleContentOver sampleId materialId sampleName
        BuiltinCatalogue.putSealedEntries context.catalogueImports [ material; sample ]
        let held =
            stores.proxy.tryLatestEnvelope sample.id
            |> Result.defaultWith (fun error -> failwith error.describe)
            |> Option.defaultWith (fun () -> failwith "Lab stack must be held.")
        let folder = freshLibraryExportFolder ()
        // Spec 0050 S3.3: a save-picker destination has an existing parent folder.
        System.IO.Directory.CreateDirectory folder.value |> ignore
        let leaf = FileName "Lab stack.sample-json"
        let file = leaf.combine folder
        let initial = LW.initLibraryWindow context Browse
        Assert.Equal<ControlCaption option>(None, initial.entryExportStatus)
        Assert.Same(initial, LW.updateLibraryWindow (LW.ExportSelectedLibraryEntry file) initial)
        Assert.False(System.IO.File.Exists file.value)
        let selected = initial |> LW.updateLibraryWindow (LW.SelectLibraryEntry sampleId.toElementValueId)
        Assert.Equal<LibraryEntryEnvelope option>(Some held, selected.exportableSelection)
        let removing = selected |> LW.updateLibraryWindow LW.RequestRemoveSelectedEntry
        let before =
            {
                removing with
                    lastError = Some (LW.ProtectedEntryRefusedErr "Previous refusal")
            }
        let exported = LW.updateLibraryWindow (LW.ExportSelectedLibraryEntry file) before
        Assert.True(System.IO.File.Exists file.value)
        let read = context.catalogueImports.readEntryFile SampleEntryKind file |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal(held, read)
        let expected = ControlCaption $"Exported Lab stack version 1 to {file.value}; that version is now pinned."
        Assert.Equal<ControlCaption option>(Some expected, exported.entryExportStatus)
        Assert.Equal<LW.LibraryWindowError option>(None, exported.lastError)
        assertLibraryExportPreservesState before exported
        match stores.samples.removeSample sampleId with
        | Error (SampleVersionPinnedErr _) -> ()
        | _ -> Assert.Fail("Removing an exported sample must return SampleVersionPinnedErr.")
        let targetId = ElementId "export-selection"
        let target = TableElementTarget targetId
        let _, selection = selectContext CatalogueKind.Sample target
        let selecting = LW.initLibraryWindow context (Select selection) |> LW.updateLibraryWindow (LW.SelectLibraryEntry sampleId.toElementValueId)
        Assert.Equal<LibraryEntryEnvelope option>(None, selecting.exportableSelection)
        Assert.Same(selecting, LW.updateLibraryWindow (LW.ExportSelectedLibraryEntry file) selecting)
        let unreadable =
            {
                selected with
                    context =
                        {
                            context with
                                catalogueImports =
                                    {
                                        context.catalogueImports with
                                            proxy =
                                                {
                                                    stores.proxy with
                                                        tryHeldEnvelope = fun _ _ -> Error (ImportStoreUnavailableErr "Envelope unavailable")
                                                }
                                    }
                        }
            }
        Assert.Equal<LibraryEntryEnvelope option>(None, unreadable.exportableSelection)
        Assert.Same(unreadable, LW.updateLibraryWindow (LW.ExportSelectedLibraryEntry file) unreadable)
        HeadlessSession.run (fun () ->
            let window = HostWindow(Width = 1150.0, Height = 800.0)
            try
                mountLibraryExportView window exported
                Assert.Equal(expected.value, textOf window ConstructorUiId.libraryExportStatusText.value)
                Assert.False(isPresent window ConstructorUiId.libraryWindowMessage.value)
            finally window.Close())

    [<Fact>]
    let ``ExportShownLibraryEntries writes the shown entries and the user materials they reference`` () : unit =
        let stores, context = freshLibraryExportContext ()
        let materialId = newMaterialId ()
        let sampleId = newSampleId ()
        let otherId = newMaterialId ()
        let materialName = EntryName "Lab glass"
        let sampleName = EntryName "Lab stack"
        let otherName = EntryName "Unshown glass"
        let material = BuiltinCatalogue.userMaterialContent materialId materialName
        let sample = BuiltinCatalogue.sampleContentOver sampleId materialId sampleName
        let other = BuiltinCatalogue.userMaterialContent otherId otherName
        BuiltinCatalogue.putSealedEntries context.catalogueImports [ material; sample; other ]
        let before =
            LW.initLibraryWindow context Browse
            |> LW.updateLibraryWindow (LW.CommitLibraryTextFilter (TextQuery "Lab stack"))
            |> LW.updateLibraryWindow (LW.SelectLibraryEntry sampleId.toElementValueId)
            |> LW.updateLibraryWindow LW.RequestMarkSampleInactive
        Assert.Equal(sampleId.toElementValueId, (Assert.Single before.filteredEntries).entryId)
        let folder = freshLibraryExportFolder ()
        let target = CatalogueExportTarget.ofFolder folder
        let exported = LW.updateLibraryWindow (LW.ExportShownLibraryEntries target) before
        let catalogue = (context.catalogueImports.openCatalogue folder).readCatalogue () |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal<Set<LibraryEntryId>>(Set.ofList [ sample.id; material.id ], catalogue.envelopes |> List.map _.content.id |> Set.ofList)
        for envelope in catalogue.envelopes do
            Assert.Equal<Result<LibraryEntryEnvelope option, CatalogueImportError>>(Ok (Some envelope), stores.proxy.tryLatestEnvelope envelope.content.id)
        let expected = ControlCaption $"Exported 2 entries to the catalogue folder {folder.value}; their versions are now pinned."
        Assert.Equal<ControlCaption option>(Some expected, exported.entryExportStatus)
        Assert.Equal<LW.LibraryWindowError option>(None, exported.lastError)
        assertLibraryExportPreservesState before exported

    [<Fact>]
    let ``ExportMyLibrary writes every user entry to a catalogue folder`` () : unit =
        let stores, context = freshLibraryExportContext ()
        let materialId = newMaterialId ()
        let sampleId = newSampleId ()
        let licensedId = newMaterialId ()
        let materialName = EntryName "Lab glass"
        let sampleName = EntryName "Lab stack"
        let licensedName = EntryName "PTB glass"
        let licence = EntryLicence "CC-BY-4.0"
        let citation = EntryCitation "A. Author, Test optical constants (2026), doi:10.0000/test"
        let material = BuiltinCatalogue.userMaterialContent materialId materialName
        let sample = BuiltinCatalogue.sampleContentOver sampleId materialId sampleName
        let licensed = BuiltinCatalogue.licensedMaterialContent licensedId licensedName licence citation
        BuiltinCatalogue.putSealedEntries context.catalogueImports [ material; sample; licensed ]
        let held = stores.proxy.heldUserEntries () |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal(3, held.Length)
        let before =
            {
                LW.initLibraryWindow context Browse with
                    textFilter = TextQuery "nothing shown"
                    restoreGate = LW.PendingLibraryRestore ([], None)
                    lastError = Some (LW.ProtectedEntryRefusedErr "Previous refusal")
            }
        Assert.Empty before.filteredEntries
        let folder = freshLibraryExportFolder ()
        let target = CatalogueExportTarget.ofFolder folder
        let exported = LW.updateLibraryWindow (LW.ExportMyLibrary target) before
        let catalogue = (context.catalogueImports.openCatalogue folder).readCatalogue () |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal<Set<LibraryEntryId>>(held |> List.map _.id |> Set.ofList, catalogue.envelopes |> List.map _.content.id |> Set.ofList)
        for envelope in catalogue.envelopes do
            match envelope.content.provenance with
            | BuiltInProvenance _ -> Assert.Fail("Export my library must contain only user-managed entries.")
            | _ -> ()
            Assert.Equal<Result<LibraryEntryEnvelope option, CatalogueImportError>>(Ok (Some envelope), stores.proxy.tryLatestEnvelope envelope.content.id)
        let expected = ControlCaption $"Exported 3 entries to the catalogue folder {folder.value}; their versions are now pinned."
        Assert.Equal<ControlCaption option>(Some expected, exported.entryExportStatus)
        Assert.Equal<LW.LibraryWindowError option>(None, exported.lastError)
        assertLibraryExportPreservesState before exported

    [<Fact>]
    let ``a refused Library export shows its reason as the window message`` () : unit =
        let _, context = freshLibraryExportContext ()
        let before =
            {
                LW.initLibraryWindow context Browse with
                    entryExportStatus = Some (ControlCaption "Previous export succeeded.")
                    restoreGate = LW.PendingLibraryRestore ([], None)
            }
        let folder = freshLibraryExportFolder ()
        let target = CatalogueExportTarget.ofFolder folder
        let refused = LW.updateLibraryWindow (LW.ExportMyLibrary target) before
        let reason =
            match refused.lastError with
            | Some (LW.LibraryExportRefusedErr (EmptyLibraryExportErr _ as error)) -> error.caption
            | _ -> failwith "An empty user library must return the typed export refusal."
        Assert.Equal<ControlCaption option>(None, refused.entryExportStatus)
        Assert.False(System.IO.Directory.Exists folder.value)
        assertLibraryExportPreservesState before refused
        HeadlessSession.run (fun () ->
            let window = HostWindow(Width = 1150.0, Height = 800.0)
            try
                mountLibraryExportView window refused
                Assert.Equal(reason.value, textOf window ConstructorUiId.libraryWindowMessage.value)
                Assert.False(isPresent window ConstructorUiId.libraryExportStatusText.value)
            finally window.Close())

    [<Fact>]
    let ``the Library window shows the citation of a CC-BY material of the selected sample`` () : unit =
        HeadlessSession.run (fun () ->
            let stores, context = freshLibraryExportContext ()
            let materialId = newMaterialId ()
            let sampleId = newSampleId ()
            let materialName = EntryName "PTB glass"
            let sampleName = EntryName "PTB stack"
            let licence = EntryLicence "CC-BY-4.0"
            let citation = EntryCitation "A. Author, Test optical constants (2026), doi:10.0000/test"
            let material = BuiltinCatalogue.licensedMaterialContent materialId materialName licence citation
            let sample = BuiltinCatalogue.sampleContentOver sampleId materialId sampleName
            BuiltinCatalogue.putSealedEntries context.catalogueImports [ material; sample ]
            let initial = LW.initLibraryWindow context Browse
            let window = HostWindow(Width = 1150.0, Height = 800.0)
            try
                mountLibraryExportView window initial
                commitFilter window "PTB stack"
                expandEntries window
                clickOn window (ConstructorUiId.libraryEntryNode sampleId.toElementValueId).value
                Assert.Equal("PTB glass: Licence CC-BY-4.0 — cite: A. Author, Test optical constants (2026), doi:10.0000/test",
                             textOf window ConstructorUiId.libraryAttributionPanel.value)
                match tryFindControl window ConstructorUiId.libraryAttributionPanel.value with
                | Some (:? StackPanel as panel) ->
                    Assert.Equal("Licence attribution", Avalonia.Automation.AutomationProperties.GetName panel)
                    let line = Assert.IsType<TextBlock>(Assert.Single panel.Children)
                    Assert.Equal(TextWrapping.Wrap, line.TextWrapping)
                | _ -> Assert.Fail("A sample's licensed material must have an attribution panel.")
                commitFilter window ""
                clickOn window (ConstructorUiId.libraryEntryNode PresetIds.source600).value
                Assert.False(isPresent window ConstructorUiId.libraryAttributionPanel.value)
            finally window.Close()
            let failedContext =
                {
                    context with
                        catalogueImports =
                            {
                                context.catalogueImports with
                                    proxy =
                                        {
                                            stores.proxy with
                                                tryHeldEntry = fun _ -> Error (ImportStoreUnavailableErr "Attribution unavailable")
                                        }
                            }
                }
            let failed = LW.initLibraryWindow failedContext Browse |> LW.updateLibraryWindow (LW.SelectLibraryEntry sampleId.toElementValueId)
            let failedWindow = HostWindow(Width = 1150.0, Height = 800.0)
            try
                mountLibraryExportView failedWindow failed
                Assert.True(isPresent failedWindow ConstructorUiId.libraryViewPanel.value)
                Assert.False(isPresent failedWindow ConstructorUiId.libraryAttributionPanel.value)
            finally failedWindow.Close())

    [<Fact>]
    let ``Export my library over the app library writes a catalogue folder that a new library registers with every user entry`` () : unit =
        let owner = FolderName "LibraryWindowExportTests"
        let firstLocations = TestAppScope.fileLocations owner
        let first = AppContext.create firstLocations WorkbenchSettings.defaults |> Result.defaultWith (fun error -> failwith error.describe)
        TestAppScope.settlePendingImports first
        let materialId = newMaterialId ()
        let sampleId = newSampleId ()
        let materialName = EntryName "Lab glass"
        let sampleName = EntryName "Lab stack"
        let material = (BuiltinCatalogue.userMaterialContent materialId materialName).toMaterialEntry |> Result.defaultWith (fun error -> failwith error.describe)
        let sample = (BuiltinCatalogue.sampleContentOver sampleId materialId sampleName).toSample |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal<Result<unit, MaterialError>>(Ok (), first.materials.saveMaterial material)
        Assert.Equal<Result<unit, SampleError>>(Ok (), first.samples.saveSample sample)
        let _, context = stubContext first.library first.samples first.materials
        let configured =
            {
                context with
                    categories = first.categories
                    sources = first.sources
                    libraryBackups = first.libraryBackups
                    catalogueImports = first.catalogueImports
            }
        let folder = freshLibraryExportFolder ()
        let target = CatalogueExportTarget.ofFolder folder
        let exported = LW.initLibraryWindow configured Browse |> LW.updateLibraryWindow (LW.ExportMyLibrary target)
        let expected = ControlCaption $"Exported 2 entries to the catalogue folder {folder.value}; their versions are now pinned."
        Assert.Equal<ControlCaption option>(Some expected, exported.entryExportStatus)
        Assert.Equal<LW.LibraryWindowError option>(None, exported.lastError)
        let secondLocations = TestAppScope.fileLocations owner
        let second = AppContext.create secondLocations WorkbenchSettings.defaults |> Result.defaultWith (fun error -> failwith error.describe)
        TestAppScope.settlePendingImports second
        let beforeLogs = second.catalogueImports.proxy.loggedImports () |> Result.defaultWith (fun error -> failwith error.describe)
        let beforeEntries = second.catalogueImports.proxy.heldUserEntries () |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Empty beforeEntries
        let pending =
            match second.catalogueImports.registerFolder folder with
            | Ok (CatalogueAwaitsChoices pending) -> pending
            | Ok _ -> failwith "A new catalogue must await confirmation of every row."
            | Error error -> failwith error.describe
        Assert.Equal(2, pending.decisions.Length)
        let incomingIds =
            pending.decisions |> List.map (fun decision ->
                match decision.status with
                | EntryIdNotHeld envelope -> envelope.content.id
                | _ -> failwith "Every exported entry must be a new id in the fresh library.")
        Assert.Equal<Set<LibraryEntryId>>(Set.ofList [ materialId.toLibraryEntryId; sampleId.toLibraryEntryId ], Set.ofList incomingIds)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok None, second.materials.tryGetMaterial materialId)
        Assert.Equal<Result<Sample option, SampleError>>(Ok None, second.samples.tryGetSample sampleId)
        Assert.Equal<Result<HeldEntry list, CatalogueImportError>>(Ok beforeEntries, second.catalogueImports.proxy.heldUserEntries ())
        Assert.Equal<Result<CatalogueImportLogEntry list, CatalogueImportError>>(Ok beforeLogs, second.catalogueImports.proxy.loggedImports ())
        confirmCatalogueRegistration second.catalogueImports pending
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some material), second.materials.tryGetMaterial materialId)
        Assert.Equal<Result<Sample option, SampleError>>(Ok (Some sample), second.samples.tryGetSample sampleId)
        let registered = second.catalogueImports.proxy.registeredFolders () |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal(folder, (Assert.Single registered).folder)

    [<Fact>]
    let ``dropping entry files on the Library window decides them and opens the import dialog, applying nothing`` () : unit =
        let file, source = droppedSourceFixture ()
        let entryId = source.id.tryToLibraryEntryId |> Option.defaultWith (fun () -> failwith "The fixture source must have a GUID identity.")
        let opened, initial = freshImportModel ()
        let before =
            {
                initial with
                    removeGate = LW.PendingSampleRemove SampleIds.glassFilm600Id
                    importStatus = Some (ControlCaption "Previous import")
                    lastError = Some (LW.CatalogueImportRefusedErr (DroppedFileRefusedErr "Previous refusal"))
            }
        Assert.Same(before, LW.updateLibraryWindow (LW.DropLibraryEntryFiles []) before)
        Assert.Empty opened
        let dropped = LW.updateLibraryWindow (LW.DropLibraryEntryFiles [ file ]) before
        let request = Assert.Single opened
        let decisions =
            match request with
            | ApplyDroppedEntries decisions -> decisions
            | SettlePendingCatalogue _ | ResolveKitSnapshot _ -> failwith "Dropped files must open their own decision rows."
        let row = Assert.Single decisions
        Assert.Equal(entryId, row.entryId)
        Assert.Equal(DroppedFileOrigin file.leafName, row.origin)
        match row.status with
        | EntryIdNotHeld incoming -> Assert.Equal(entryId, incoming.content.id)
        | _ -> Assert.Fail "A new source must be offered as an unheld entry."
        Assert.Equal<LW.LibraryWindowError option>(None, dropped.lastError)
        Assert.Equal<ControlCaption option>(None, dropped.importStatus)
        Assert.Equal(before.removeGate, dropped.removeGate)
        Assert.Equal(before.lifecycleGate, dropped.lifecycleGate)
        Assert.Equal(before.restoreGate, dropped.restoreGate)
        Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(Ok None, initial.context.catalogueImports.proxy.tryHeldEntry row.entryId)
        Assert.Equal<Result<SourcePreset option, SourceError>>(Ok None, initial.context.sources.tryGetSource source.id)
        let choices = decisions |> List.map _.byDefault
        let outcome = initial.context.catalogueImports.apply choices |> Result.defaultWith (fun error -> failwith error.describe)
        let applied : ImportDialogApplied =
            {
                request = request
                choices = choices
                outcome = outcome
            }
        let completed = LW.updateLibraryWindow (LW.LibraryImportApplied applied) dropped
        Assert.Equal(Some applied.summary, completed.importStatus)
        Assert.Equal<LW.LibraryWindowError option>(None, completed.lastError)
        Assert.Equal<PendingCatalogueImport list>(dropped.pendingImports, completed.pendingImports)
        Assert.Equal(dropped.removeGate, completed.removeGate)

    [<Fact>]
    let ``a refused drop on the Library window shows its reason as the window message`` () : unit =
        let folder, _ = catalogueFolderFixture ()
        let leaf = FileName "notes.txt"
        let file = leaf.combine folder
        System.IO.File.WriteAllText(file.value, "Not a library entry")
        let opened, initial = freshImportModel ()
        let before =
            {
                initial with
                    lifecycleGate = LW.PendingSampleLifecycle (SampleIds.glassFilm600Id, MaterialsWindowView.MarkInactiveAction)
                    importStatus = Some (ControlCaption "Previous import")
            }
        let refused = LW.updateLibraryWindow (LW.DropLibraryEntryFiles [ file ]) before
        let error =
            match refused.lastError with
            | Some (LW.CatalogueImportRefusedErr (DroppedFileRefusedErr reason) as error) ->
                Assert.Contains("notes.txt", reason)
                error
            | _ -> failwith "The unsupported file must surface DroppedFileRefusedErr."
        Assert.Empty opened
        Assert.Equal<ControlCaption option>(None, refused.importStatus)
        Assert.Equal(before.removeGate, refused.removeGate)
        Assert.Equal(before.lifecycleGate, refused.lifecycleGate)
        Assert.Equal(before.restoreGate, refused.restoreGate)
        HeadlessSession.run (fun () ->
            let window = HostWindow(Width = 1150.0, Height = 800.0)
            try
                Program.mkSimple (fun () -> refused) LW.updateLibraryWindow LW.libraryWindowView
                |> Program.withHost window
                |> Program.run
                window.Show()
                Dispatcher.UIThread.RunJobs()
                Assert.Equal(error.caption.value, textOf window ConstructorUiId.libraryWindowMessage.value)
            finally window.Close())

    [<Fact>]
    let ``a registration whose rows await a choice opens the import dialog, and Review imports reopens it`` () : unit =
        let opened, initial = freshImportModel ()
        Assert.Same(initial, LW.updateLibraryWindow LW.ReviewPendingImports initial)
        Assert.Empty opened
        let folder, _ = catalogueFolderFixture ()
        let registered = LW.updateLibraryWindow (LW.RegisterCatalogueFolder folder) initial
        let pending = Assert.Single registered.pendingImports
        Assert.Equal(SettlePendingCatalogue pending, Assert.Single opened)
        Assert.Same(registered, LW.updateLibraryWindow LW.ReviewPendingImports registered)
        Assert.Equal<ImportDialogRequest list>([ SettlePendingCatalogue pending; SettlePendingCatalogue pending ], List.ofSeq opened)
        Assert.Equal<Result<CatalogueImportLogEntry list, CatalogueImportError>>(Ok [], initial.context.catalogueImports.proxy.registeredFolders ())
        for decision in pending.decisions do
            Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(Ok None, initial.context.catalogueImports.proxy.tryHeldEntry decision.entryId)
        Assert.Equal("LibraryReviewImportsButton", ConstructorUiId.libraryReviewImportsButton.value)
        let events, selection = selectContext LightSource (TableElementTarget (ElementId "review-source"))
        HeadlessSession.run (fun () ->
            for mode in [ Browse; Select selection ] do
                let model =
                    {
                        registered with
                            mode = mode
                    }
                let window = HostWindow(Width = 1150.0, Height = 800.0)
                try
                    Program.mkSimple (fun () -> model) LW.updateLibraryWindow LW.libraryWindowView
                    |> Program.withHost window
                    |> Program.run
                    window.Show()
                    Dispatcher.UIThread.RunJobs()
                    Assert.Equal("Review imports…", textOf window ConstructorUiId.libraryReviewImportsButton.value)
                    clickOn window ConstructorUiId.libraryReviewImportsButton.value
                    Assert.Equal(SettlePendingCatalogue pending, Seq.last opened)
                finally window.Close())
        Assert.Equal(4, opened.Count)
        Assert.Empty events

    [<Fact>]
    let ``a settled registration leaves the pending list and its summary is the status`` () : unit =
        let opened, initial = freshImportModel ()
        let folder, _ = catalogueFolderFixture ()
        let registered = LW.updateLibraryWindow (LW.RegisterCatalogueFolder folder) initial
        let pending = Assert.Single registered.pendingImports
        let choices = pending.decisions |> List.map _.byDefault
        let outcome = registered.context.catalogueImports.settle pending choices |> Result.defaultWith (fun error -> failwith error.describe)
        let applied : ImportDialogApplied =
            {
                request = Assert.Single opened
                choices = choices
                outcome = outcome
            }
        let events, selection = selectContext LightSource (TableElementTarget (ElementId "settled-source"))
        let before =
            {
                registered with
                    mode = Select selection
                    selectedEntryId = Some PresetIds.source600
                    restoreGate = LW.PendingLibraryRestore ([], None)
                    lastError = Some (LW.CatalogueImportRefusedErr (DroppedFileRefusedErr "Previous refusal"))
            }
        let completed = LW.updateLibraryWindow (LW.LibraryImportApplied applied) before
        Assert.Empty completed.pendingImports
        Assert.Equal(Some applied.summary, completed.importStatus)
        Assert.Equal<LW.LibraryWindowError option>(None, completed.lastError)
        Assert.Equal(before.removeGate, completed.removeGate)
        Assert.Equal(before.lifecycleGate, completed.lifecycleGate)
        Assert.Equal(before.restoreGate, completed.restoreGate)
        Assert.Equal(before.selectedEntryId, completed.selectedEntryId)
        match completed.mode with
        | Select actual -> Assert.Same(selection, actual)
        | Browse -> Assert.Fail "Import completion must preserve the Select session."
        Assert.Empty events
        let entries = completed.context.catalogueImports.proxy.registeredFolders () |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal(folder, (Assert.Single entries).folder)
        let nextFolder, _ = catalogueFolderFixture ()
        let withNext = LW.updateLibraryWindow (LW.RegisterCatalogueFolder nextFolder) before
        let nextPending = List.last withNext.pendingImports
        let completedFirst = LW.updateLibraryWindow (LW.LibraryImportApplied applied) withNext
        Assert.Equal<PendingCatalogueImport list>([ nextPending ], completedFirst.pendingImports)
        HeadlessSession.run (fun () ->
            let window = HostWindow(Width = 1150.0, Height = 800.0)
            try
                Program.mkSimple (fun () -> completed) LW.updateLibraryWindow LW.libraryWindowView
                |> Program.withHost window
                |> Program.run
                window.Show()
                Dispatcher.UIThread.RunJobs()
                Assert.False(isPresent window ConstructorUiId.libraryReviewImportsButton.value)
                Assert.Equal(applied.summary.value, textOf window ConstructorUiId.libraryImportStatusText.value)
                Assert.True(isPresent window (ConstructorUiId.libraryCatalogueFolderRow pending.entry.catalogueId.uiKey).value)
            finally window.Close())

    [<Fact>]
    let ``the catalogue-folder UiIds are the stable intent-named ids`` () : unit =
        let catalogue = CatalogueId (System.Guid.Parse "7dd43058-04cf-448d-bf13-3e4e8e833942")
        Assert.Equal("LibraryRegisterCatalogueFolderButton", ConstructorUiId.libraryRegisterCatalogueFolderButton.value)
        Assert.Equal("LibraryCatalogueFoldersPanel", ConstructorUiId.libraryCatalogueFoldersPanel.value)
        Assert.Equal($"LibraryCatalogueFolderRow_{catalogue.value}", (ConstructorUiId.libraryCatalogueFolderRow catalogue.uiKey).value)
        Assert.Equal($"LibraryUnregisterCatalogueButton_{catalogue.value}", (ConstructorUiId.libraryUnregisterCatalogueButton catalogue.uiKey).value)
        Assert.Equal("LibraryImportStatusText", ConstructorUiId.libraryImportStatusText.value)

    [<Fact>]
    let ``RegisterCatalogueFolder holds every row for confirmation and names it in the status`` () : unit =
        let _, initial = freshModel ()
        Assert.Equal<ControlCaption option>(None, initial.importStatus)
        Assert.Empty initial.pendingImports
        let folder, catalogue = catalogueFolderFixture ()
        let model = LW.updateLibraryWindow (LW.RegisterCatalogueFolder folder) initial
        let pending = Assert.Single model.pendingImports
        Assert.Equal(catalogue.manifest.id, pending.entry.catalogueId)
        Assert.Equal<LibraryEntryId list>(catalogue.envelopes |> List.map _.content.id, pending.decisions |> List.map _.entryId)
        Assert.Equal<Result<CatalogueImportLogEntry list, CatalogueImportError>>(Ok [], model.context.catalogueImports.proxy.registeredFolders ())
        for envelope in catalogue.envelopes do
            Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(Ok None, model.context.catalogueImports.proxy.tryHeldEntry envelope.content.id)
        Assert.Equal(Some (ControlCaption $"Review {folder.value}; 1 entries await confirmation. Nothing has been imported or registered."), model.importStatus)
        Assert.Equal<LW.LibraryWindowError option>(None, model.lastError)
        let nextFolder, nextCatalogue = catalogueFolderFixture ()
        let next = LW.updateLibraryWindow (LW.RegisterCatalogueFolder nextFolder) model
        Assert.Equal<CatalogueId list>([ catalogue.manifest.id; nextCatalogue.manifest.id ], next.pendingImports |> List.map _.entry.catalogueId)
        Assert.Equal<Result<CatalogueImportLogEntry list, CatalogueImportError>>(Ok [], next.context.catalogueImports.proxy.registeredFolders ())

    [<Fact>]
    let ``a folder is held for the import dialog without imports or registration before confirmation`` () : unit =
        let _, initial = freshModel ()
        let folder, catalogue = catalogueFolderFixture ()
        let incoming = Assert.Single catalogue.envelopes
        let heldContent =
            {
                incoming.content with
                    id = LibraryEntryId (System.Guid.NewGuid())
            }
        let heldEnvelope = heldContent.``sealed`` |> Result.defaultWith (fun error -> failwith error.describe)
        let put =
            {
                envelope = heldEnvelope
                latest = MoveLatestVersion
            }
        initial.context.catalogueImports.proxy.putEntryVersion put
        |> Result.defaultWith (fun error -> failwith error.describe)
        let before = initial.context.catalogueImports.proxy.tryHeldEntry heldContent.id
        let model = LW.updateLibraryWindow (LW.RegisterCatalogueFolder folder) initial
        let pending = Assert.Single model.pendingImports
        let decision = Assert.Single pending.decisions
        match decision.status with
        | NameCollision (entry, held) ->
            Assert.Equal(incoming, entry)
            Assert.Equal(heldContent.id, (Assert.Single held).id)
        | _ -> Assert.Fail "The same-name source must remain a pending collision."
        Assert.Equal<Result<CatalogueImportLogEntry list, CatalogueImportError>>(Ok [], model.context.catalogueImports.proxy.registeredFolders ())
        Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(Ok None, model.context.catalogueImports.proxy.tryHeldEntry incoming.content.id)
        Assert.Equal(before, model.context.catalogueImports.proxy.tryHeldEntry heldContent.id)
        Assert.Equal(Some (ControlCaption $"Review {folder.value}; 1 entries await confirmation. Nothing has been imported or registered."), model.importStatus)

    [<Fact>]
    let ``a refused registration shows its reason as the window message`` () : unit =
        let _, initial = freshModel ()
        let folder, _ = catalogueFolderFixture ()
        let registered = registeredCatalogueFixture initial folder
        let model = LW.updateLibraryWindow (LW.RegisterCatalogueFolder folder) registered
        match model.lastError with
        | Some (LW.CatalogueImportRefusedErr (CatalogueAlreadyRegisteredErr reason as error)) ->
            Assert.Contains(folder.value, reason)
            Assert.Equal(error.caption, (LW.CatalogueImportRefusedErr error).caption)
        | _ -> Assert.Fail "The second registration must surface CatalogueAlreadyRegisteredErr."
        Assert.Equal<ControlCaption option>(None, model.importStatus)
        Assert.Equal<PendingCatalogueImport list>(registered.pendingImports, model.pendingImports)

    [<Fact>]
    let ``UnregisterCatalogueFolder forgets the folder and keeps its entries`` () : unit =
        let _, initial = freshModel ()
        let folder, catalogue = catalogueFolderFixture ()
        let registered = registeredCatalogueFixture initial folder
        let source = Assert.Single catalogue.envelopes
        let before = registered.context.catalogueImports.proxy.tryHeldEntry source.content.id
        match before with
        | Ok (Some _) -> ()
        | _ -> Assert.Fail "The fixture must explicitly settle its pending source."
        let model = LW.updateLibraryWindow (LW.UnregisterCatalogueFolder catalogue.manifest.id) registered
        Assert.Equal<Result<CatalogueImportLogEntry list, CatalogueImportError>>(Ok [], model.context.catalogueImports.proxy.registeredFolders ())
        Assert.Equal(before, model.context.catalogueImports.proxy.tryHeldEntry source.content.id)
        Assert.Equal(Some (ControlCaption $"Unregistered {folder.value}. Its entries stay in the library."), model.importStatus)
        Assert.Equal<LW.LibraryWindowError option>(None, model.lastError)
        let again = LW.updateLibraryWindow (LW.RegisterCatalogueFolder folder) model
        let pending = List.last again.pendingImports
        Assert.Equal(ReuseHeldEntry, (Assert.Single pending.decisions).defaultAction)
        Assert.Equal<Result<CatalogueImportLogEntry list, CatalogueImportError>>(Ok [], again.context.catalogueImports.proxy.registeredFolders ())
        Assert.Equal(before, again.context.catalogueImports.proxy.tryHeldEntry source.content.id)

    // ============================ pure: ids contract ============================

    [<Fact>]
    let ``the LibraryWindow UiIds are the stable intent-named ids`` () =
        Assert.Equal("LibraryWindow", ConstructorUiId.libraryWindow.value)
        Assert.Equal("LibraryFacetTreeHost", ConstructorUiId.libraryFacetTreeHost.value)
        Assert.Equal("LibraryViewPanel", ConstructorUiId.libraryViewPanel.value)
        Assert.Equal("LibraryAddSampleButton", ConstructorUiId.libraryAddSampleButton.value)
        Assert.Equal("LibraryMakeMultilayerButton", ConstructorUiId.libraryMakeMultilayerButton.value)
        Assert.Equal("LibraryEditButton", ConstructorUiId.libraryEditButton.value)
        Assert.Equal("LibraryRemoveButton", ConstructorUiId.libraryRemoveButton.value)
        Assert.Equal("LibraryRemoveConfirmButton", ConstructorUiId.libraryRemoveConfirmButton.value)
        Assert.Equal("LibraryRemoveCancelButton", ConstructorUiId.libraryRemoveCancelButton.value)
        Assert.Equal("LibraryWindowMessage", ConstructorUiId.libraryWindowMessage.value)
        // The entry-leaf id derives from the tree-node code family, prefixed so it cannot collide.
        Assert.Equal("FacetTreeNode_entry:" + PresetIds.source600.value, (ConstructorUiId.libraryEntryNode PresetIds.source600).value)
        // The constructor-side entry point: the ribbon strip's right-aligned button.
        Assert.Equal("OpenLibraryWindowButton", ConstructorUiId.openLibraryWindowButton.value)

    // ============================ pure: projection ============================

    [<Fact>]
    let ``the initial projection lists the WHOLE corpus — every sample and every preset — as selectable entry leaves`` () =
        let _, m = freshModel ()
        let state = LW.libraryFacetedState m
        Assert.Equal(seededCorpus, state.resultCount.value)
        Assert.Equal(FacetedTreeControls.TreeMaterialized, state.materialization)
        Assert.Empty(state.breadcrumbs)
        Assert.Equal("", state.filterDraft.value)
        let entries = List.head state.tree
        Assert.Equal("entries", entries.code.value)
        Assert.Equal(Some (FacetedTreeControls.FacetItemCount seededCorpus), entries.countOpt)
        Assert.Equal(seededCorpus, List.length entries.children)
        for leaf in entries.children do
            Assert.StartsWith("entry:", leaf.code.value)
        // Samples AND presets both lead as leaves.
        Assert.Contains(entries.children, fun (n : FacetedTreeControls.TreeNode) -> n.code.value = glassFilm600EntryId.libraryEntryCode.value)
        Assert.Contains(entries.children, fun (n : FacetedTreeControls.TreeNode) -> n.code.value = PresetIds.source600.libraryEntryCode.value)
        // The ONE default representation is "By kind", active by default.
        Assert.Equal<string list>([ "by-kind" ], state.representations |> List.map (fun r -> r.code.value))
        Assert.Equal<ConstructorUiKey option>(Some (ConstructorUiKey "by-kind"), state.activeRepresentationOpt)

    [<Fact>]
    let ``acceptance (pure): the by-kind tree lists every entry kind with counts`` () =
        let _, m = freshModel ()
        let state = LW.libraryFacetedState m
        let kindNode =
            state.tree |> List.find (fun n -> n.code.value = "facet:" + entryKindFacetKey.value)
        let branches =
            kindNode.children |> List.map (fun b -> b.label.value, b.countOpt |> Option.map (fun c -> c.value))
        // Every kind, with its count, in the engine's structural branch order.
        Assert.Equal<(string * int option) list>(
            [ "Detector", Some 2; "Polarizer", Some 3; "Sample", Some 11; "Source", Some (List.length (BuiltinCatalogue.sources ())) ],
            branches)
        // The kind facet is offered FIRST (the by-kind representation's head) with count-previews.
        let kindOffer = state.offers |> List.head
        Assert.Equal(entryKindFacetKey.value, kindOffer.code.value)
        Assert.Equal<(string * int) list>(
            [ "Detector", 2; "Polarizer", 3; "Sample", 11; "Source", List.length (BuiltinCatalogue.sources ()) ],
            kindOffer.values |> List.map (fun v -> v.code.value, v.previewCount.value))

    [<Fact>]
    let ``the projected tree lists entry leaves and facet groups in case-insensitive alphabetical label order`` () =
        // The window-projection half of the acceptance (operator 010/Q1): both the entry leaves
        // (corpus order overridden) and the top-level facet groups (representation order
        // overridden — "By kind" leads with Kind, not the alphabetical head) read alphabetically
        // by label. The numeric film-thickness BUCKETS keep their range order and are not asserted.
        let _, m = freshModel ()
        let state = LW.libraryFacetedState m
        let caseInsensitive (a : string) (b : string) : int =
            System.String.Compare(a, b, System.StringComparison.OrdinalIgnoreCase)
        let isSorted (labels : string list) : bool = labels = List.sortWith caseInsensitive labels
        // Level 1a — all BuiltinCatalogue.library () leaves, before alphabetical projection.
        let entries = List.head state.tree
        let leafLabels = entries.children |> List.map (fun n -> n.label.value)
        Assert.Equal(seededCorpus, List.length leafLabels)
        Assert.True(isSorted leafLabels, $"entry leaves must be alphabetical: %A{leafLabels}")
        // Level 1b — the top-level facet groups (Kind no longer leads by representation order).
        let facetLabels = state.tree |> List.tail |> List.map (fun n -> n.label.value)
        Assert.True(List.length facetLabels > 1, "the by-kind tree offers several facet groups")
        Assert.True(isSorted facetLabels, $"facet groups must be alphabetical: %A{facetLabels}")

    [<Fact>]
    let ``every facet group shows its total count and each branch expands to its member entry leaves`` () =
        // The operator gap (.manual/001-task): facet groups MUST show a total and branches MUST
        // expand to the entries they contain — including the numeric film-thickness buckets and the
        // multi-valued film-material facet. Pure projection proof; a UI-less assertion on
        // `facetedState` would have caught both before the window ever rendered.
        let _, m = freshModel ()
        let state = LW.libraryFacetedState m
        let facetGroups = state.tree |> List.tail   // head is the "entries" corpus group
        Assert.NotEmpty(facetGroups)
        for group in facetGroups do
            // 1) The group carries a positive total (never a bare, countless heading).
            match group.countOpt with
            | Some c -> Assert.True(c.value > 0, $"facet group %s{group.label.value} must show a positive total, got %d{c.value}")
            | None -> Assert.Fail($"facet group %s{group.label.value} must show a total count")
            // 2) Every branch expands to member entry leaves, the leaf tally matching its (count) badge.
            Assert.NotEmpty(group.children)
            let mutable distinct = Set.empty
            for branch in group.children do
                match branch.countOpt with
                | Some bc ->
                    Assert.True(bc.value > 0, $"branch %s{branch.label.value} must carry entries")
                    Assert.Equal(bc.value, List.length branch.children)
                | None -> Assert.Fail($"branch %s{branch.label.value} must show a count")
                for leaf in branch.children do
                    Assert.True(List.isEmpty leaf.children, "a branch entry leaf is terminal")
                    // Each branch-nested leaf is a SELECTABLE entry (its code resolves to an entryId).
                    match leaf.code.tryLibraryEntryId with
                    | Some entryId -> distinct <- Set.add entryId distinct
                    | None -> Assert.Fail($"branch entry leaf %s{leaf.label.value} (%s{leaf.code.value}) must resolve to an entry id")
            // 3) The group total is exactly the DISTINCT entries reachable by expanding its branches.
            Assert.Equal(Some (FacetedTreeControls.FacetItemCount (Set.count distinct)), group.countOpt)

    [<Fact>]
    let ``the single-valued Kind facet totals the whole population and its branch counts sum to it`` () =
        // Entry-kind is single-valued and total (every entry is exactly one of Sample / Source /
        // Detector / Polarizer), so its group count is the whole corpus and the branch counts
        // partition it — the operator's "the math must add up" for a facet.
        let _, m = freshModel ()
        let state = LW.libraryFacetedState m
        let kindGroup = state.tree |> List.find (fun n -> n.code.value = "facet:" + entryKindFacetKey.value)
        Assert.Equal(Some state.resultCount, kindGroup.countOpt)
        let branchSum = kindGroup.children |> List.sumBy (fun b -> match b.countOpt with Some c -> c.value | None -> 0)
        Assert.Equal(state.resultCount.value, branchSum)

    [<Fact>]
    let ``a branch-nested entry leaf code selects its entry, while bare branch and heading codes stay inert`` () =
        // The branch-nested leaf code `branch:<facet>:<value>:entry:<entryId>` resolves to its id (so
        // an entry selected THROUGH a facet branch works), yet the bare branch/heading codes remain
        // grouping-only — the selection contract after the fix.
        let leafCode = (ConstructorUiKey ("branch:" + entryKindFacetKey.value + ":Source")).libraryBranchLeaf PresetIds.source600
        Assert.Equal("branch:" + entryKindFacetKey.value + ":Source:entry:" + PresetIds.source600.value, leafCode.value)
        Assert.Equal(Some PresetIds.source600, leafCode.tryLibraryEntryId)
        Assert.Equal<ElementValueId option>(None, (ConstructorUiKey ("facet:" + entryKindFacetKey.value)).tryLibraryEntryId)
        Assert.Equal<ElementValueId option>(None, (ConstructorUiKey ("branch:" + entryKindFacetKey.value + ":Source")).tryLibraryEntryId)

    [<Fact>]
    let ``selecting an entry THROUGH a facet branch highlights that branch row, not the corpus-group copy`` () =
        // The operator gap (.manual/002-followup): clicking an entry under a facet branch used to
        // highlight the matching row in the "Library" corpus group instead of the clicked branch
        // row. The projected `selectedCode` must now be the EXACT clicked node code.
        let _, m = freshModel ()
        let leafCode = (ConstructorUiKey ("branch:" + entryKindFacetKey.value + ":Source")).libraryBranchLeaf PresetIds.source600
        let selected = LW.updateLibraryWindow (LW.SelectLibraryEntryNode (PresetIds.source600, leafCode)) m
        let state = LW.libraryFacetedState selected
        // The highlight is on the clicked branch row — NOT the flat corpus-group `entry:<id>` code.
        Assert.Equal<ConstructorUiKey option>(Some leafCode, state.selectedCodeOpt)
        Assert.NotEqual<ConstructorUiKey option>(Some PresetIds.source600.libraryEntryCode, state.selectedCodeOpt)
        // The domain selection still resolves (the view panel / verbs target the entry).
        Assert.Equal<ElementValueId option>(Some PresetIds.source600, selected.selectedEntryId)
        // The id-only path (programmatic / verb-driven) still highlights the corpus-group row.
        let viaId = LW.updateLibraryWindow (LW.SelectLibraryEntry PresetIds.source600) m
        Assert.Equal<ConstructorUiKey option>(Some PresetIds.source600.libraryEntryCode, (LW.libraryFacetedState viaId).selectedCodeOpt)

    [<Fact>]
    let ``the tree is collapsed by default and ToggleNode flips a node's expansion, re-projecting its children`` () =
        // Spec 0040 step 002: every top-level node opens CollapsedNode; the disclosure chevron's
        // ToggleNode records the code expanded (its children then render) and a second toggle collapses.
        let _, m = freshModel ()
        let entriesOf (model : LW.LibraryWindowModel) : FacetedTreeControls.TreeNode = List.head (LW.libraryFacetedState model).tree
        // First open: EVERY top-level node (the entries group and every facet group) is CollapsedNode.
        let opened = LW.libraryFacetedState m
        for node in opened.tree do
            Assert.Equal(FacetedTreeControls.CollapsedNode, node.expansion)
        // The children are still PROJECTED (collapse is a render concern) — only the flag changes.
        Assert.Equal(seededCorpus, List.length (entriesOf m).children)
        // Toggling "entries" records it expanded and the projected node flips to ExpandedNode.
        let expanded = LW.updateLibraryWindow (LW.ToggleLibraryNode (ConstructorUiKey "entries")) m
        Assert.Equal(FacetedTreeControls.ExpandedNode, expanded.expandedNodes.nodeExpansion (ConstructorUiKey "entries"))
        Assert.Equal(FacetedTreeControls.ExpandedNode, (entriesOf expanded).expansion)
        // A facet group toggles INDEPENDENTLY while entries stays expanded (the set holds both).
        let bothOpen = LW.updateLibraryWindow (LW.ToggleLibraryNode (ConstructorUiKey ("facet:" + entryKindFacetKey.value))) expanded
        Assert.Equal(FacetedTreeControls.ExpandedNode, bothOpen.expandedNodes.nodeExpansion (ConstructorUiKey "entries"))
        let kindNode = (LW.libraryFacetedState bothOpen).tree |> List.find (fun n -> n.code.value = "facet:" + entryKindFacetKey.value)
        Assert.Equal(FacetedTreeControls.ExpandedNode, kindNode.expansion)
        // A second toggle collapses entries again (persisted).
        let collapsed = LW.updateLibraryWindow (LW.ToggleLibraryNode (ConstructorUiKey "entries")) bothOpen
        Assert.Equal(FacetedTreeControls.CollapsedNode, collapsed.expandedNodes.nodeExpansion (ConstructorUiKey "entries"))
        Assert.Equal(FacetedTreeControls.CollapsedNode, (entriesOf collapsed).expansion)

    [<Fact>]
    let ``facetedState projects the selected entry's node code — empty when nothing is selected`` () =
        // Spec 0040 step 003: the control highlights the row whose code is `selectedCode`; the host
        // projects it from the Model's already-tracked selection (nothing selected → empty string).
        let _, m = freshModel ()
        Assert.Equal<ConstructorUiKey option>(None, (LW.libraryFacetedState m).selectedCodeOpt)
        let selected = LW.updateLibraryWindow (LW.SelectLibraryEntry PresetIds.source600) m
        Assert.Equal<ConstructorUiKey option>(Some PresetIds.source600.libraryEntryCode, (LW.libraryFacetedState selected).selectedCodeOpt)
        // A re-selection re-targets the highlight in the same projection.
        let retargeted = LW.updateLibraryWindow (LW.SelectLibraryEntry PresetIds.polarizerLp) selected
        Assert.Equal<ConstructorUiKey option>(Some PresetIds.polarizerLp.libraryEntryCode, (LW.libraryFacetedState retargeted).selectedCodeOpt)

    [<Fact>]
    let ``the committed text filter narrows the corpus over display names and echoes as the box draft`` () =
        let _, m = freshModel ()
        let narrowed = LW.updateLibraryWindow (LW.CommitLibraryTextFilter (TextQuery "glass")) m
        Assert.Equal(6, List.length (narrowed.filteredEntries))
        Assert.Contains(glassFilm600EntryId.value, filteredEntryIds narrowed)
        Assert.DoesNotContain(PresetIds.source600.value, filteredEntryIds narrowed)
        Assert.Equal("glass", (LW.libraryFacetedState narrowed).filterDraft.value)
        let restored = LW.updateLibraryWindow (LW.CommitLibraryTextFilter (TextQuery "")) narrowed
        Assert.Equal(seededCorpus, List.length (restored.filteredEntries))

    [<Fact>]
    let ``applying the kind facet narrows the corpus and takes a removable breadcrumb chip with its after-count`` () =
        let _, m = freshModel ()
        let constrained = LW.updateLibraryWindow (kindConstraint "Polarizer") m
        Assert.Equal(3, List.length (constrained.filteredEntries))
        Assert.Contains(PresetIds.polarizerLp.value, filteredEntryIds constrained)
        let chips = (LW.libraryFacetedState constrained).breadcrumbs
        Assert.Equal(1, List.length chips)
        Assert.Equal(entryKindFacetKey.value, (List.head chips).code.value)
        Assert.Equal("Kind: Polarizer", (List.head chips).label.value)
        Assert.Equal(3, (List.head chips).afterCount.value)
        let removed = LW.updateLibraryWindow (LW.RemoveLibraryFacet entryKindFacetKey) constrained
        Assert.Equal(seededCorpus, List.length (removed.filteredEntries))
        Assert.Empty((LW.libraryFacetedState removed).breadcrumbs)

    [<Fact>]
    let ``the polarizer-category facet rides polarizer populations only and the sample facets ride the sample subtree`` () =
        let _, m = freshModel ()
        let offerCodes (model : LW.LibraryWindowModel) : string list =
            (LW.libraryFacetedState model).offers |> List.map (fun g -> g.code.value)
        // Over the whole corpus both facet families are offered (polarizers and samples exist).
        Assert.Contains(polarizerCategoryFacetKey.value, offerCodes m)
        Assert.Contains(sampleFilmMaterialKey.value, offerCodes m)
        // Over polarizers only: the polarizer facet stays, every sample facet vanishes.
        let polarizers = LW.updateLibraryWindow (kindConstraint "Polarizer") m
        Assert.Contains(polarizerCategoryFacetKey.value, offerCodes polarizers)
        Assert.DoesNotContain(sampleFilmMaterialKey.value, offerCodes polarizers)
        Assert.DoesNotContain(sampleFilmThicknessKey.value, offerCodes polarizers)
        // Over a polarizer-free population the polarizer facet vanishes entirely.
        let sources = LW.updateLibraryWindow (kindConstraint "Source") m
        Assert.DoesNotContain(polarizerCategoryFacetKey.value, offerCodes sources)
        Assert.DoesNotContain(sampleFilmMaterialKey.value, offerCodes sources)
        // The facet offers BuiltinCatalogue.presets () categories with previews (1 LP, 2 CP).
        let polarizerOffer =
            (LW.libraryFacetedState polarizers).offers |> List.find (fun g -> g.code.value = polarizerCategoryFacetKey.value)
        Assert.Equal<(string * int) list>(
            [ "Circular", 2; "Linear", 1 ],
            polarizerOffer.values |> List.map (fun v -> v.code.value, v.previewCount.value))

    [<Fact>]
    let ``a lifted sample facet narrows to the samples carrying the constituent — any film layer matches`` () =
        let _, m = freshModel ()
        let constrained =
            LW.updateLibraryWindow (LW.ApplyFacetConstraint (sampleFilmMaterialKey, DiscreteSelection (Set.singleton (DiscreteKey "Transparent glass (n = 1.52)")))) m
        // BuiltinCatalogue.sample's glassFilm200 (single film) and multilayerQw (period + closing layer) carry
        // glass152 films; every other entry — incl. the plates whose SUBSTRATE is glass152 and
        // every preset — leaves the result set.
        Assert.Equal<string list>(
            ([ BuiltinCatalogue.sample SampleIds.glassFilm200Id; BuiltinCatalogue.sample SampleIds.multilayerQwId ] |> List.map (fun s -> string s.id.value) |> List.sort),
            filteredEntryIds constrained |> List.sort)

    // ============================ pure: the numeric facet (buckets + manual range) ==========

    [<Fact>]
    let ``the film-thickness facet offers step-010 buckets with the manual range box and a bucket click reproduces its count`` () =
        let _, m = freshModel ()
        let state = LW.libraryFacetedState m
        let thicknessOffer = state.offers |> List.find (fun g -> g.code.value = sampleFilmThicknessKey.value)
        Assert.Equal(FacetedTreeControls.ManualRangeOffered, thicknessOffer.manualRange)
        Assert.NotEmpty(thicknessOffer.values)
        // Every OTHER offer group is discrete: no manual box anywhere else.
        for group in state.offers do
            if group.code.value <> sampleFilmThicknessKey.value then
                Assert.Equal(FacetedTreeControls.NoManualRange, group.manualRange)
        // Applying a bucket through the control's token seam reproduces exactly its preview
        // count (the step-010 pin, now through the window's own handler boundary).
        let bucket = List.head thicknessOffer.values
        let mutable current = m
        let handlers = LW.libraryFacetedHandlers (fun msg -> current <- LW.updateLibraryWindow msg current)
        handlers.applyConstraint sampleFilmThicknessKey.uiKey bucket.code
        Assert.Equal(bucket.previewCount.value, List.length (current.filteredEntries))
        // The chip is removable and carries the bucket-shaped range label.
        let chips = (LW.libraryFacetedState current).breadcrumbs
        Assert.Equal(1, List.length chips)
        Assert.StartsWith("Film thickness:", (List.head chips).label.value)

    [<Fact>]
    let ``the tree's film-thickness branches are buckets, never raw magnitudes`` () =
        let _, m = freshModel ()
        let state = LW.libraryFacetedState m
        let thicknessNode =
            state.tree |> List.find (fun n -> n.code.value = "facet:" + sampleFilmThicknessKey.value)
        Assert.NotEmpty(thicknessNode.children)
        for branch in thicknessNode.children do
            // A bucket branch carries a range code and a unit-labelled range text with a count.
            Assert.StartsWith("branch:" + sampleFilmThicknessKey.value + ":", branch.code.value)
            match branch.countOpt with
            | Some count -> Assert.True(count.value > 0, "a bucket branch carries a non-zero count")
            | None -> Assert.Fail($"bucket branch %s{branch.code.value} must carry its count")
            Assert.Contains("m", branch.label.value)

    [<Fact>]
    let ``a manual min–max entry applies as an ordinary constraint chip and an unparseable commit is a no-op`` () =
        let _, m = freshModel ()
        let mutable current = m
        let handlers = LW.libraryFacetedHandlers (fun msg -> current <- LW.updateLibraryWindow msg current)
        // "160-260" nm: only the 200 nm glass film's layer lands inside the half-open range.
        handlers.applyManualRange sampleFilmThicknessKey.uiKey (ControlCaption "160-260")
        Assert.Equal<string list>(
            [ string SampleIds.glassFilm200Id.value ],
            filteredEntryIds current)
        Assert.Equal(1, List.length (LW.libraryFacetedState current).breadcrumbs)
        // An unparseable commit dispatches nothing — the constraint (and the model) survive.
        let before = current
        handlers.applyManualRange sampleFilmThicknessKey.uiKey (ControlCaption "not-a-range")
        handlers.applyManualRange sampleFilmThicknessKey.uiKey (ControlCaption "")
        Assert.Equal<LW.LibraryWindowModel>(before, current)
        // A single value parses as the degenerate exact-value range (never asserted against a
        // store magnitude — thicknesses round-trip through meters a few ulp off, step 011).
        Assert.Equal<NumericRange option>(Some { lower = Units.UnitMagnitude 42.0; upper = Units.UnitMagnitude 42.0 }, (ControlCaption "42").tryManualNumericRange)

    [<Fact>]
    let ``the numeric range codes round-trip and the range text reuses the step-010 unit switch`` () =
        let range : NumericRange = { lower = Units.UnitMagnitude 200.0; upper = Units.UnitMagnitude 500.0 }
        Assert.Equal<NumericRange option>(Some range, range.uiKey.tryNumericRange)
        Assert.Equal<NumericRange option>(None, (ConstructorUiKey "garbage").tryNumericRange)
        Assert.Equal("200-500 nm", range.caption.value)
        Assert.Equal("150 nm", ({ lower = Units.UnitMagnitude 150.0; upper = Units.UnitMagnitude 150.0 } : NumericRange).caption.value)
        Assert.Equal("500 nm-1 µm", ({ lower = Units.UnitMagnitude 500.0; upper = Units.UnitMagnitude 1000.0 } : NumericRange).caption.value)

    // ============================ pure: live corpus over the shared stores ============================

    [<Fact>]
    let ``the projection re-queries the LIVE samples store — a store write shows in the next pass`` () =
        let samples, m = freshModelWithStores ()
        Assert.Equal(seededCorpus, (LW.libraryFacetedState m).resultCount.value)
        // Remove a sample directly through the SHARED store (another window's write): the very
        // next projection of the UNCHANGED model no longer lists it.
        match samples.removeSample SampleIds.glassFilm200Id with
        | Ok () -> ()
        | Error e -> Assert.Fail($"removing the seeded sample must succeed, got %A{e}")
        Assert.Equal(seededCorpus - 1, (LW.libraryFacetedState m).resultCount.value)
        Assert.DoesNotContain(string SampleIds.glassFilm200Id.value, filteredEntryIds m)

    // ============================ pure: verbs + confirm gate ============================

    [<Fact>]
    let ``Add sample and Make multilayer mint fresh upfront ids per dispatch and reach the launcher`` () =
        // Spec 0038 step 008: the id-mint stays off the save path — each verb mints the
        // entity's Guid AT the window-open dispatch, so every Add opens its own registry-keyed
        // editor (the recorded call carries the minted id).
        let calls, m = freshModel ()
        LW.updateLibraryWindow LW.AddSample m |> ignore
        LW.updateLibraryWindow LW.AddSample m |> ignore
        match calls |> Seq.filter (fun c -> c.StartsWith "sample-add:") |> List.ofSeq with
        | [ a; b ] -> Assert.NotEqual<string>(a, b)
        | other -> Assert.Fail($"expected two sample Adds, got %A{other}")
        calls.Clear()
        // Make-multilayer is the DISTINCT seeded-period creation path (spec 0035 step 014).
        LW.updateLibraryWindow LW.MakeMultilayer m |> ignore
        match List.ofSeq calls with
        | [ one ] -> Assert.StartsWith("sample-multilayer:", one)
        | other -> Assert.Fail($"expected one Make-multilayer, got %A{other}")

    [<Fact>]
    let ``Edit reaches the launcher only for a SAMPLE selection — presets and no selection are inert`` () =
        let calls, m = freshModel ()
        m |> LW.updateLibraryWindow (LW.SelectLibraryEntry glassFilm600EntryId) |> LW.updateLibraryWindow LW.EditSelectedEntry |> ignore
        Assert.Equal<string list>([ "sample-edit:Glass thin film (n=1.75, 600 nm)" ], List.ofSeq calls)
        calls.Clear()
        // A preset has no editor — the verb is absent and the dispatch inert.
        m |> LW.updateLibraryWindow (LW.SelectLibraryEntry PresetIds.polarizerLp) |> LW.updateLibraryWindow LW.EditSelectedEntry |> ignore
        LW.updateLibraryWindow LW.EditSelectedEntry m |> ignore
        Assert.Empty(calls)

    [<Fact>]
    let ``acceptance (pure): Remove on a ProtectedBuiltIn entry surfaces the typed refusal and changes nothing`` () =
        let samples, m = freshModelWithStores ()
        let refused =
            m
            |> LW.updateLibraryWindow (LW.SelectLibraryEntry PresetIds.source600)
            |> LW.updateLibraryWindow LW.RequestRemoveSelectedEntry
        // The typed reason NAMES the entry; the confirm gate never arms.
        match refused.lastError with
        | Some (LW.ProtectedEntryRefusedErr reason) ->
            Assert.Contains("Monochromatic 600 nm", reason)
            Assert.Contains("protected built-in", reason)
        | other -> Assert.Fail($"expected ProtectedEntryRefused, got %A{other}")
        Assert.Equal(LW.NoPendingLibraryRemove, refused.removeGate)
        // Nothing changed: the corpus, the stores, the selection.
        Assert.Equal(seededCorpus, (LW.libraryFacetedState refused).resultCount.value)
        Assert.Contains(PresetIds.source600.value, filteredEntryIds refused)
        match samples.listSamples ActiveOnly with
        | Ok all -> Assert.Equal(11, List.length all)
        | Error e -> Assert.Fail($"listSamples failed: %A{e}")
        // A confirm after the refusal is inert too — there is no armed gate to fire.
        Assert.Equal<LW.LibraryWindowModel>(refused, LW.updateLibraryWindow LW.ConfirmEntryRemove refused)

    [<Fact>]
    let ``Remove on a sample is confirm-gated: request arms with the id, cancel disarms, nothing is removed; no selection is inert`` () =
        let _, m = freshModel ()
        Assert.Equal<LW.LibraryWindowModel>(m, LW.updateLibraryWindow LW.RequestRemoveSelectedEntry m)
        let armed =
            m
            |> LW.updateLibraryWindow (LW.SelectLibraryEntry glassFilm600EntryId)
            |> LW.updateLibraryWindow LW.RequestRemoveSelectedEntry
        Assert.Equal(LW.PendingSampleRemove SampleIds.glassFilm600Id, armed.removeGate)
        let cancelled = LW.updateLibraryWindow LW.CancelEntryRemove armed
        Assert.Equal(LW.NoPendingLibraryRemove, cancelled.removeGate)
        Assert.Equal(seededCorpus, List.length (cancelled.filteredEntries))

    [<Fact>]
    let ``a confirmed sample remove drops the entry from the projection and the shared store in the same pass`` () =
        let samples, m = freshModelWithStores ()
        let removed =
            m
            |> LW.updateLibraryWindow (LW.SelectLibraryEntry glassFilm600EntryId)
            |> LW.updateLibraryWindow LW.RequestRemoveSelectedEntry
            |> LW.updateLibraryWindow LW.ConfirmEntryRemove
        match removed.lastError with
        | None -> ()
        | Some e -> Assert.Fail($"expected no error, got %A{e}")
        Assert.DoesNotContain(glassFilm600EntryId.value, filteredEntryIds removed)
        Assert.Equal<ElementValueId option>(None, removed.selectedEntryId)
        match samples.listSamples ActiveOnly with
        | Ok all -> Assert.Equal(10, List.length all)
        | Error e -> Assert.Fail($"listSamples failed: %A{e}")

    [<Fact>]
    let ``a confirm whose sample vanished behind the window's back surfaces the typed store refusal and changes nothing`` () =
        let samples, m = freshModelWithStores ()
        let armed =
            m
            |> LW.updateLibraryWindow (LW.SelectLibraryEntry glassFilm600EntryId)
            |> LW.updateLibraryWindow LW.RequestRemoveSelectedEntry
        // The sample disappears through the SHARED store (another window's remove)…
        match samples.removeSample SampleIds.glassFilm600Id with
        | Ok () -> ()
        | Error e -> Assert.Fail($"the out-of-band remove must succeed, got %A{e}")
        // …so the armed confirm meets the typed UnknownSampleIdErr block, surfaced inline.
        let refused = LW.updateLibraryWindow LW.ConfirmEntryRemove armed
        match refused.lastError with
        | Some (LW.SampleRemoveRefusedErr (UnknownSampleIdErr reason)) -> Assert.Contains("unknown sample id", reason)
        | other -> Assert.Fail($"expected SampleRemoveRefused UnknownSampleIdErr, got %A{other}")
        Assert.Equal(LW.NoPendingLibraryRemove, refused.removeGate)
        Assert.Equal(seededCorpus - 1, (LW.libraryFacetedState refused).resultCount.value)

    [<Fact>]
    let ``a query or selection change disarms a pending remove and clears the refusal`` () =
        let armed () =
            let _, m = freshModel ()
            m |> LW.updateLibraryWindow (LW.SelectLibraryEntry glassFilm600EntryId) |> LW.updateLibraryWindow LW.RequestRemoveSelectedEntry
        Assert.Equal(LW.NoPendingLibraryRemove, (LW.updateLibraryWindow (LW.CommitLibraryTextFilter (TextQuery "film")) (armed ())).removeGate)
        Assert.Equal(LW.NoPendingLibraryRemove, (LW.updateLibraryWindow (kindConstraint "Sample") (armed ())).removeGate)
        Assert.Equal(LW.NoPendingLibraryRemove, (LW.updateLibraryWindow (LW.RemoveLibraryFacet entryKindFacetKey) (armed ())).removeGate)
        let retargeted = LW.updateLibraryWindow (LW.SelectLibraryEntry PresetIds.source600) (armed ())
        Assert.Equal(Some PresetIds.source600, retargeted.selectedEntryId)
        Assert.Equal(LW.NoPendingLibraryRemove, retargeted.removeGate)
        // The typed protected refusal clears on the next query edit (not on a mere cancel).
        let refused =
            let _, m = freshModel ()
            m |> LW.updateLibraryWindow (LW.SelectLibraryEntry PresetIds.source600) |> LW.updateLibraryWindow LW.RequestRemoveSelectedEntry
        match refused.lastError with
        | Some (LW.ProtectedEntryRefusedErr _) -> ()
        | other -> Assert.Fail($"expected ProtectedEntryRefused, got %A{other}")
        Assert.Equal(None, (LW.updateLibraryWindow (LW.CommitLibraryTextFilter (TextQuery "si")) refused).lastError)

    [<Fact>]
    let ``only an entry node code selects — branches and headings are grouping display`` () =
        Assert.Equal(Some PresetIds.source600, PresetIds.source600.libraryEntryCode.tryLibraryEntryId)
        Assert.Equal<ElementValueId option>(None, (ConstructorUiKey "branch:entry-kind:Sample").tryLibraryEntryId)
        Assert.Equal<ElementValueId option>(None, (ConstructorUiKey "entries").tryLibraryEntryId)
        let dispatched = ResizeArray<LW.LibraryWindowMsg>()
        let handlers = LW.libraryFacetedHandlers dispatched.Add
        handlers.selectNode PresetIds.source600.libraryEntryCode
        handlers.selectNode (ConstructorUiKey "branch:entry-kind:Sample")
        handlers.selectNode (ConstructorUiKey "entries")
        // Only the entry-leaf click selects — carrying the EXACT clicked code (a corpus-group leaf
        // here) so the highlight lands on the clicked row; the bare branch/heading codes are inert.
        Assert.Equal<LW.LibraryWindowMsg list>([ LW.SelectLibraryEntryNode (PresetIds.source600, PresetIds.source600.libraryEntryCode) ], List.ofSeq dispatched)
        // A discrete offer click lifts its tokens back to elevated engine values at the boundary.
        handlers.applyConstraint entryKindFacetKey.uiKey (ConstructorUiKey "Polarizer")
        Assert.Equal(kindConstraint "Polarizer", dispatched.[1])

    [<Fact>]
    let ``the view panel target follows the selection and resolves through the live corpus`` () =
        let samples, m = freshModelWithStores ()
        Assert.Equal<LibraryEntry option>(None, m.selectedEntry)
        // A preset selection: shown, protected, NOT editable.
        let source = LW.updateLibraryWindow (LW.SelectLibraryEntry PresetIds.source600) m
        match source.selectedEntry with
        | Some entry ->
            Assert.Equal("Monochromatic 600 nm", entry.displayName)
            Assert.Equal(EntryProtection.ProtectedBuiltIn, entry.protection)
        | None -> Assert.Fail "the selected source entry must resolve"
        Assert.Equal<Sample option>(None, source.editableSample)
        // A sample selection: shown AND editable.
        let sample = LW.updateLibraryWindow (LW.SelectLibraryEntry glassFilm600EntryId) m
        match sample.editableSample with
        | Some s -> Assert.Equal(SampleIds.glassFilm600Id, s.id)
        | None -> Assert.Fail "the selected sample must offer Edit"
        // A vanished sample's selection resolves to nothing (its panel vanishes with its row).
        match samples.removeSample SampleIds.glassFilm600Id with
        | Ok () -> ()
        | Error e -> Assert.Fail($"remove failed: %A{e}")
        Assert.Equal<LibraryEntry option>(None, sample.selectedEntry)

    [<Fact>]
    let ``a result count above the threshold gates the tree and Show-Search materializes it`` () =
        let library, samples, materials = freshStores ()
        let _, context = stubContext library samples materials
        let m = LW.initLibraryWindow { context with treeAutoBuildThreshold = TreeAutoBuildThreshold 1 } Browse
        let gated = LW.libraryFacetedState m
        Assert.Equal(FacetedTreeControls.TreeGated, gated.materialization)
        Assert.Empty(gated.tree)
        Assert.Equal(seededCorpus, gated.resultCount.value)
        Assert.NotEmpty(gated.offers)
        let shown = LW.libraryFacetedState (LW.updateLibraryWindow LW.RequestLibraryTreeBuild m)
        Assert.Equal(FacetedTreeControls.TreeMaterialized, shown.materialization)
        Assert.NotEmpty(shown.tree)

    // ============================ headless acceptance (ui-smoke) ============================

    /// Mount the REAL Main workbench MVU loop headless (the MainConstructorWindow shape with
    /// injectable stores), for the strip-button acceptance.
    let private mountMain (library : LibraryProxy) (samples : SampleProxy) (materials : MaterialProxy) : HostWindow =
        let categories = BuiltinCatalogue.categoryStore (materialsReferencingCategory materials)
        let model0 = Scene.initMainWith library (Experiments.createInMemoryExperiments ()) materials samples categories
        let window = HostWindow(Width = 980.0, Height = 1050.0)
        Program.mkSimple (fun () -> model0) Scene.updateTableAndElementRotation Scene.mainView
        |> Program.withHost window
        |> Program.run
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    let private mountLibraryWindow (library : LibraryProxy) (samples : SampleProxy) (materials : MaterialProxy) : LibraryWindow =
        let categories = BuiltinCatalogue.categoryStore (materialsReferencingCategory materials)
        let window = LibraryWindow(library, samples, SourceProxy.createInMemory (), materials, categories, LibraryBackupProxy.createInMemory (), CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        Assert.True(isPresent window ConstructorUiId.libraryRegisterCatalogueFolderButton.value)
        // The tree opens collapsed (spec 0040 step 002); expand the entries group so the leaf-
        // clicking proofs below find their rows (expansion persists across the window's lifetime).
        expandEntries window
        window

    [<Fact>]
    let ``the Library window accepts dropped files`` () : unit =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let window = mountLibraryWindow library samples materials
            try Assert.True(DragDrop.GetAllowDrop window)
            finally window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: a source file dropped on the Library window opens the import dialog, and Apply adds it to the app library`` () : unit =
        HeadlessSession.run (fun () ->
            let owner = FolderName "LibraryWindowDropTests"
            let ctx =
                AppContext.create (TestAppScope.fileLocations owner) WorkbenchSettings.defaults
                |> Result.defaultWith (fun error -> failwith error.describe)
            TestAppScope.settlePendingImports ctx
            let file, source = droppedSourceFixture ()
            let window = LibraryWindow(ctx.library, ctx.samples, ctx.sources, ctx.materials, ctx.categories, ctx.libraryBackups, ctx.catalogueImports)
            let opened = ResizeArray<ImportDialogWindow>()
            use _subscription =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _)) ->
                    match sender with
                    | :? ImportDialogWindow as dialog -> opened.Add dialog
                    | _ -> ())
            try
                window.Show()
                Dispatcher.UIThread.RunJobs()
                commitFilter window source.name.value
                Assert.False(isPresent window ConstructorUiId.libraryImportStatusText.value)
                window.DropEntryFiles [ file ]
                Dispatcher.UIThread.RunJobs()
                let dialog = Assert.Single opened
                Assert.True(dialog.IsVisible)
                Assert.Equal<Result<SourcePreset option, SourceError>>(Ok None, ctx.sources.tryGetSource source.id)
                Assert.False(isPresent window ConstructorUiId.libraryImportStatusText.value)
                clickOn dialog ConstructorUiId.importApplyButton.value
                Assert.False(dialog.IsVisible)
                Assert.Equal<Result<SourcePreset option, SourceError>>(Ok (Some source), ctx.sources.tryGetSource source.id)
                Assert.Equal("Imported the dropped entry files (decisions applied: 1).", textOf window ConstructorUiId.libraryImportStatusText.value)
                expandEntries window
                Assert.True(isPresent window (ConstructorUiId.libraryEntryNode source.id).value,
                            "Apply must refresh the Library tree without reopening the window.")
            finally
                for dialog in opened do dialog.Close()
                window.Close()
                Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the strip button opens ONE Library window and a second click ACTIVATES it`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let window = mountMain library samples materials
            Assert.True(isPresent window ConstructorUiId.openLibraryWindowButton.value,
                        "the ribbon strip row must carry the right-aligned Library… button")
            // Observe the windows the REAL defaults open (the WindowOpenedEvent seam the desktop
            // lifetime itself uses) — subscribed only after the Main window is shown.
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            clickOn window ConstructorUiId.openLibraryWindowButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            let libraryWindow = opened.[0]
            Assert.True(matchesId ConstructorUiId.libraryWindow.value libraryWindow, "the opened window must be the Library window")
            Assert.True(libraryWindow.IsVisible)
            // The single-instance acceptance: a second click ACTIVATES the live window — the
            // shared registry under LibraryWindowKey creates nothing new.
            clickOn window ConstructorUiId.openLibraryWindowButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            Assert.True(libraryWindow.IsVisible, "the activated window must still be the live one")
            // Close → the registry forgets the key, so the NEXT open creates afresh.
            libraryWindow.Close()
            Dispatcher.UIThread.RunJobs()
            clickOn window ConstructorUiId.openLibraryWindowButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(2, opened.Count)
            opened.[1].Close()
            Dispatcher.UIThread.RunJobs()
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the by-kind faceted tree renders every entry kind with counts over the whole corpus`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let window = mountLibraryWindow library samples materials
            Assert.Equal($"%d{seededCorpus} results", textOf window ConstructorUiId.facetResultCount.value)
            // Sample and preset leaves both render (entries is auto-expanded by the mount helper).
            Assert.True(isPresent window ((ConstructorUiId.libraryEntryNode glassFilm600EntryId).value))
            Assert.True(isPresent window ((ConstructorUiId.libraryEntryNode PresetIds.source600).value))
            // The kind facet's branches: every entry kind, each with its count. The facet group
            // opens collapsed (spec 0040 step 002); collapse the entries group first (so the facet
            // rows rise back to the top and the facet chevron is reachable), then expand the kind
            // facet to reveal its branches.
            expandEntries window
            expandNode window ("facet:" + entryKindFacetKey.value)
            for kind, count in [ "Sample", 11; "Source", List.length (BuiltinCatalogue.sources ()); "Detector", 2; "Polarizer", 3 ] do
                let branchId = (ConstructorUiId.facetTreeNode (ConstructorUiKey ("branch:" + entryKindFacetKey.value + ":" + kind))).value
                Assert.True(isPresent window branchId, $"the by-kind tree must list the %s{kind} branch")
                Assert.Equal($"%s{kind} (%d{count})", textOf window branchId)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: Add sample persists through the real editor into the shared store and the window lists it`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let window = mountLibraryWindow library samples materials
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            // Add sample → the REAL Sample editor over the SHARED stores (through the real
            // launcher under the Add-minted SampleEditorKey).
            clickOn window ConstructorUiId.libraryAddSampleButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            let editor = opened.[0]
            Assert.True(matchesId ConstructorUiId.sampleEditorWindow.value editor, "the opened window must be the Sample editor")
            // Name it and give it one layer (a valid stack — Add layer takes the first listed
            // material now that the inline picker is gone, step 019), then Save — NewUnsaved
            // → addSample.
            setText editor ConstructorUiId.sampleNameBox.value "Library window sample"
            clickOn editor ConstructorUiId.addLayerButton.value
            clickOn editor ConstructorUiId.sampleEditorSaveButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.False(editor.IsVisible, "Save must close the editor")
            // The save landed in the SHARED store…
            match samples.listSamples ActiveOnly with
            | Ok all -> Assert.Contains(all, fun (s : Sample) -> s.name.value = "Library window sample")
            | Error e -> Assert.Fail($"listSamples failed: %A{e}")
            // …and the window's next dispatch-driven render re-queries it: committing the
            // matching filter narrows the tree to the just-saved entry.
            commitFilter window "Library window sample"
            Assert.Equal("1 results", textOf window ConstructorUiId.facetResultCount.value)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: Remove on a protected entry surfaces the typed refusal and changes nothing`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let window = mountLibraryWindow library samples materials
            // Narrow to the protected source and select its leaf.
            commitFilter window "Monochromatic"
            clickOn window ((ConstructorUiId.libraryEntryNode PresetIds.source600).value)
            clickOn window ConstructorUiId.libraryRemoveButton.value
            // The typed refusal renders inline; the confirm gate never armed.
            let message = textOf window ConstructorUiId.libraryWindowMessage.value
            Assert.Contains("Monochromatic 600 nm", message)
            Assert.Contains("protected built-in", message)
            Assert.False(isPresent window ConstructorUiId.libraryRemoveConfirmButton.value,
                         "the confirm gate must never arm for a protected entry")
            // Nothing changed: the entry is still listed and the samples store untouched.
            Assert.True(isPresent window ((ConstructorUiId.libraryEntryNode PresetIds.source600).value))
            Assert.Equal("1 results", textOf window ConstructorUiId.facetResultCount.value)
            match samples.listSamples ActiveOnly with
            | Ok all -> Assert.Equal(11, List.length all)
            | Error e -> Assert.Fail($"listSamples failed: %A{e}")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless: removing a sample through the confirm gate drops its row in the same render pass`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let window = mountLibraryWindow library samples materials
            commitFilter window "n=1.75"
            clickOn window ((ConstructorUiId.libraryEntryNode glassFilm600EntryId).value)
            clickOn window ConstructorUiId.libraryRemoveButton.value
            clickOn window ConstructorUiId.libraryRemoveConfirmButton.value
            Assert.False(isPresent window ((ConstructorUiId.libraryEntryNode glassFilm600EntryId).value),
                         "the removed sample's row must leave the tree in the same render pass")
            match samples.listSamples ActiveOnly with
            | Ok all -> Assert.Equal(10, List.length all)
            | Error e -> Assert.Fail($"listSamples failed: %A{e}")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless: Edit opens the real Sample editor on the selected sample and Make multilayer opens the seeded editor`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let window = mountLibraryWindow library samples materials
            commitFilter window "n=1.75"
            clickOn window ((ConstructorUiId.libraryEntryNode glassFilm600EntryId).value)
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            clickOn window ConstructorUiId.libraryEditButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            Assert.True(matchesId ConstructorUiId.sampleEditorWindow.value opened.[0], "the opened window must be the Sample editor")
            Assert.Contains("Glass thin film", opened.[0].Title)
            opened.[0].Close()
            Dispatcher.UIThread.RunJobs()
            // Make multilayer: a NEW editor seeded with the foldable 2-layer period — one
            // super-row plus its two cell rows (spec 0035 step 014), and Save persists it.
            clickOn window ConstructorUiId.libraryMakeMultilayerButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(2, opened.Count)
            let editor = opened.[1]
            Assert.True(isPresent editor (ConstructorUiId.sampleGroupRow (ConstructorUiPosition 0)).value, "the seeded period super-row must render")
            Assert.True(isPresent editor (ConstructorUiId.sampleCellLayerRow (ConstructorUiPosition 0) (ConstructorUiPosition 0)).value, "seeded cell layer 0 must render")
            Assert.True(isPresent editor (ConstructorUiId.sampleCellLayerRow (ConstructorUiPosition 0) (ConstructorUiPosition 1)).value, "seeded cell layer 1 must render")
            setText editor ConstructorUiId.sampleNameBox.value "Window multilayer"
            clickOn editor ConstructorUiId.sampleEditorSaveButton.value
            Assert.False(editor.IsVisible)
            match samples.listSamples ActiveOnly with
            | Ok all -> Assert.Contains(all, fun (s : Sample) -> s.name.value = "Window multilayer")
            | Error e -> Assert.Fail($"listSamples failed: %A{e}")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless: selecting an entry leaf shows the view panel with the kind, protection and full description`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let window = mountLibraryWindow library samples materials
            Assert.False(isPresent window ConstructorUiId.libraryViewPanel.value, "no selection → no view panel")
            commitFilter window "Monochromatic"
            clickOn window ((ConstructorUiId.libraryEntryNode PresetIds.source600).value)
            Assert.True(isPresent window ConstructorUiId.libraryViewPanel.value, "the view panel must render for the selected entry")
            let panelText = textOf window ConstructorUiId.libraryViewPanel.value
            Assert.Contains("Monochromatic 600 nm", panelText)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless: a gated tree shows Show-Search with ZERO rows and the click materializes the entry leaves`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let _, context = stubContext library samples materials
            // Mount the REAL MVU loop over a tiny threshold (the window minus its launcher
            // composition — the FacetedTreeControlsTests mounting precedent).
            let window = HostWindow(Width = 900.0, Height = 760.0)
            Program.mkSimple
                (fun () -> LW.initLibraryWindow { context with treeAutoBuildThreshold = TreeAutoBuildThreshold 1 } Browse)
                LW.updateLibraryWindow
                LW.libraryWindowView
            |> Program.withHost window
            |> Program.run
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(isPresent window ConstructorUiId.facetShowTreeButton.value, "the Show/Search button must gate the tree")
            let treeRowCount () =
                window.GetVisualDescendants()
                |> Seq.filter (fun v ->
                    match v with
                    | :? Control as c ->
                        let autoId = Avalonia.Automation.AutomationProperties.GetAutomationId(c)
                        not (isNull autoId) && autoId.StartsWith("FacetTreeNode_")
                    | _ -> false)
                |> Seq.length
            Assert.Equal(0, treeRowCount ())
            Assert.Equal($"%d{seededCorpus} results", textOf window ConstructorUiId.facetResultCount.value)
            clickOn window ConstructorUiId.facetShowTreeButton.value
            Assert.True(treeRowCount () > 0, "the explicit build must materialize the tree")
            // The materialized tree opens collapsed (spec 0040 step 002) — expand entries to reach
            // the leaves.
            expandEntries window
            Assert.True(isPresent window ((ConstructorUiId.libraryEntryNode PresetIds.source600).value))
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (002): the tree opens collapsed and the entries chevron toggles its leaves in the same render pass`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let _, context = stubContext library samples materials
            // Mount the REAL MVU loop directly (NOT the auto-expanding helper) so the first render
            // is the collapsed default.
            let window = HostWindow(Width = 900.0, Height = 760.0)
            Program.mkSimple (fun () -> LW.initLibraryWindow context Browse) LW.updateLibraryWindow LW.libraryWindowView
            |> Program.withHost window
            |> Program.run
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // First open: the entries group renders a collapsed row WITH a chevron, but its leaves
            // do NOT render.
            Assert.True(isPresent window (ConstructorUiId.facetTreeNode (ConstructorUiKey "entries")).value, "the entries group row must render")
            Assert.True(isPresent window (ConstructorUiId.facetTreeChevron (ConstructorUiKey "entries")).value, "the entries group must carry a disclosure chevron")
            Assert.False(isPresent window ((ConstructorUiId.libraryEntryNode PresetIds.source600).value), "a collapsed tree must hide its entry leaves on first open")
            // Clicking the chevron expands the group — the leaves render in the same pass.
            expandEntries window
            Assert.True(isPresent window ((ConstructorUiId.libraryEntryNode PresetIds.source600).value), "the chevron click must reveal the entry leaves")
            // A second click collapses it again.
            expandEntries window
            Assert.False(isPresent window ((ConstructorUiId.libraryEntryNode PresetIds.source600).value), "a second chevron click must collapse the group")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (003): selecting an entry leaf highlights exactly that row with the chosen fill and a thicker border`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let window = mountLibraryWindow library samples materials
            // Narrow to the three ideal polarizers so both leaves render near the top and stay clickable.
            commitFilter window "polarizer"
            // Before any selection every leaf is idle (no row carries the chosen fill).
            Assert.Equal(Some idleFill, backgroundColorOf (labelBorderOf window ((ConstructorUiId.libraryEntryNode PresetIds.polarizerLp).value)))
            clickOn window ((ConstructorUiId.libraryEntryNode PresetIds.polarizerLp).value)
            let selected = labelBorderOf window ((ConstructorUiId.libraryEntryNode PresetIds.polarizerLp).value)
            let sibling = labelBorderOf window ((ConstructorUiId.libraryEntryNode PresetIds.polarizerCpLeft).value)
            // Exactly the selected row reads the chosen fill; the sibling stays idle.
            Assert.Equal(Some chosenFill, backgroundColorOf selected)
            Assert.Equal(Some idleFill, backgroundColorOf sibling)
            // …and the selected row carries the non-hue cue: a border strictly thicker than an idle
            // row's (a colourblind-safe cue that does not rely on colour alone).
            Assert.True(selected.BorderThickness.Top > sibling.BorderThickness.Top,
                        "the selected row's border must be thicker than an idle row's")
            window.Close())

    // ============================ step 016 — Select mode (pure) ============================

    [<Fact>]
    let ``the step-016 UiIds are the stable intent-named ids`` () =
        Assert.Equal("LibrarySelectButton", ConstructorUiId.librarySelectButton.value)
        Assert.Equal("LibrarySelectCloseButton", ConstructorUiId.librarySelectCloseButton.value)
        Assert.Equal("LibrarySelectConstraint", ConstructorUiId.librarySelectConstraint.value)
        Assert.Equal("WorkbenchSelectStatus", ConstructorUiId.workbenchSelectStatus.value)

    [<Fact>]
    let ``Select state pre-applies the kind constraint at the corpus seam — non-removable, no breadcrumb chip`` () =
        // Constrained to CircularPolarizer: exactly the two CP presets remain; the constraint
        // takes NO chip (nothing to remove — it lives below the facet engine) and the ordinary
        // window state (filter, offers, representation) is otherwise untouched.
        let _, _, m = freshSelectModel CircularPolarizer (TableElementTarget (ElementId "el-1"))
        Assert.Equal<string list>(
            List.sort [ PresetIds.polarizerCpLeft.value; PresetIds.polarizerCpRight.value ],
            filteredEntryIds m |> List.sort)
        let state = LW.libraryFacetedState m
        Assert.Equal(2, state.resultCount.value)
        Assert.Empty(state.breadcrumbs)
        // A LinearPolarizer constraint narrows to the one LP preset (forKinds, never re-derived).
        let _, _, lp = freshSelectModel LinearPolarizer (TableElementTarget (ElementId "el-1"))
        Assert.Equal<string list>([ PresetIds.polarizerLp.value ], filteredEntryIds lp)
        // The selection resolves through the CONSTRAINED corpus: an out-of-constraint id
        // resolves to nothing (Select can never return an entry outside its constraint).
        let stray = LW.updateLibraryWindow (LW.SelectLibraryEntry PresetIds.source600) lp
        Assert.Equal<LibraryEntry option>(None, stray.selectedEntry)
        // Browse mode is byte-for-byte the ordinary window corpus.
        let _, browse = freshModel ()
        Assert.Equal(seededCorpus, List.length (browse.filteredEntries))

    [<Fact>]
    let ``ConfirmSelect returns the HIGHLIGHTED entry through onSelected and closes — no highlight is inert`` () =
        let events, calls, m = freshSelectModel Detector (TableElementTarget (ElementId "det"))
        // No highlight → inert: no callback, no close, the model unchanged.
        Assert.Equal<LW.LibraryWindowModel>(m, LW.updateLibraryWindow LW.ConfirmLibrarySelect m)
        Assert.Empty(events)
        // Highlight an in-constraint entry, then Select: onSelected carries THAT entry, the
        // close request follows, and the mode flips to Browse (the resolved session cannot be
        // cancelled again by the host's dismissal hook).
        let resolved =
            m
            |> LW.updateLibraryWindow (LW.SelectLibraryEntry PresetIds.detectorIntensity)
            |> LW.updateLibraryWindow LW.ConfirmLibrarySelect
        Assert.Equal<string list>([ "selected:" + PresetIds.detectorIntensity.value ], List.ofSeq events)
        Assert.Contains("close-requested", calls)
        Assert.Equal<LibraryWindowMode<LibraryEntry>>(Browse, resolved.mode)
        Assert.Equal<LW.LibraryWindowModel>(resolved, LW.updateLibraryWindow LW.LibrarySelectDismissed resolved)
        Assert.Equal<string list>([ "selected:" + PresetIds.detectorIntensity.value ], List.ofSeq events)
        // Browse-mode Confirm/Cancel are inert (the pair does not exist there).
        let _, browse = freshModel ()
        Assert.Equal<LW.LibraryWindowModel>(browse, LW.updateLibraryWindow LW.ConfirmLibrarySelect browse)
        Assert.Equal<LW.LibraryWindowModel>(browse, LW.updateLibraryWindow LW.CancelLibrarySelect browse)

    [<Fact>]
    let ``CancelSelect and the host dismissal cancel a pending session exactly once`` () =
        // The Close verb: onCancelled, then the close request.
        let events, calls, m = freshSelectModel Detector (TableElementTarget (ElementId "det"))
        let cancelled = LW.updateLibraryWindow LW.CancelLibrarySelect m
        Assert.Equal<string list>([ "cancelled" ], List.ofSeq events)
        Assert.Contains("close-requested", calls)
        Assert.Equal<LibraryWindowMode<LibraryEntry>>(Browse, cancelled.mode)
        // A dismissal AFTER the cancel is a no-op — never a second onCancelled.
        LW.updateLibraryWindow LW.LibrarySelectDismissed cancelled |> ignore
        Assert.Equal<string list>([ "cancelled" ], List.ofSeq events)
        // The host dismissal alone (title-bar X / a staleness Close()): onCancelled once,
        // WITHOUT a close request (the window is already closing).
        let events2, calls2, m2 = freshSelectModel Detector (TableElementTarget (ElementId "det"))
        let dismissed = LW.updateLibraryWindow LW.LibrarySelectDismissed m2
        Assert.Equal<string list>([ "cancelled" ], List.ofSeq events2)
        Assert.DoesNotContain("close-requested", calls2)
        Assert.Equal<LibraryWindowMode<LibraryEntry>>(Browse, dismissed.mode)

    [<Fact>]
    let ``RetargetSelect cancels the superseded session, re-points the constraint and target, and clears the highlight`` () =
        let events, _, m = freshSelectModel LinearPolarizer (TableElementTarget (ElementId "el-1"))
        let highlighted = LW.updateLibraryWindow (LW.SelectLibraryEntry PresetIds.polarizerLp) m
        // A second Select open re-targets the live single instance: the FIRST session is
        // cancelled (a second Choose closes the first, logically) and the window now serves
        // the new constraint/target with a fresh highlight.
        let events2, retargetCtx = selectContext Detector (TableElementTarget (ElementId "det"))
        let retargeted = LW.updateLibraryWindow (LW.RetargetLibrarySelect retargetCtx) highlighted
        Assert.Equal<string list>([ "cancelled" ], List.ofSeq events)
        Assert.Empty(events2)
        Assert.Equal<ElementValueId option>(None, retargeted.selectedEntryId)
        Assert.Equal<string list>(
            List.sort [ PresetIds.detectorEllipsometer.value; PresetIds.detectorIntensity.value ],
            filteredEntryIds retargeted |> List.sort)
        // The re-pointed session then resolves through the NEW context.
        retargeted
        |> LW.updateLibraryWindow (LW.SelectLibraryEntry PresetIds.detectorEllipsometer)
        |> LW.updateLibraryWindow LW.ConfirmLibrarySelect
        |> ignore
        Assert.Equal<string list>([ "selected:" + PresetIds.detectorEllipsometer.value ], List.ofSeq events2)

    // ============================ step 016 — Select mode (headless) ============================

    let private mountSelectLibraryWindow (library : LibraryProxy) (samples : SampleProxy) (materials : MaterialProxy) (selectCtx : SelectionContext<LibraryEntry>) : LibraryWindow =
        let categories = BuiltinCatalogue.categoryStore (materialsReferencingCategory materials)
        let window = LibraryWindow(library, samples, SourceProxy.createInMemory (), materials, categories, LibraryBackupProxy.createInMemory (), CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy, mode = Select selectCtx)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        Assert.True(isPresent window ConstructorUiId.libraryRegisterCatalogueFolderButton.value)
        // The tree opens collapsed (spec 0040 step 002); expand entries so the Select proofs reach
        // their leaves (expansion persists across the window's lifetime).
        expandEntries window
        window

    /// Mount the REAL Main workbench MVU loop headless WITH a captured dispatch (the
    /// `Cmd.ofEffect` capture the window hosts themselves use), so a Select window's
    /// onSelected can dispatch the TARGETED bind into the live scene loop.
    let private mountMainWithDispatch (model0 : Scene.TableAndElementRotationModel) : HostWindow * (Scene.TableAndElementRotationMsg -> unit) =
        let window = HostWindow(Width = 980.0, Height = 1050.0)
        let mutable dispatchRef : Scene.TableAndElementRotationMsg -> unit = ignore
        Program.mkProgram
            (fun () -> model0, Cmd.ofEffect (fun d -> dispatchRef <- d))
            (fun msg m -> Scene.updateTableAndElementRotation msg m, Cmd.none)
            Scene.mainView
        |> Program.withHost window
        |> Program.run
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window, (fun msg -> dispatchRef msg)

    let private freshMainModel () : Scene.TableAndElementRotationModel =
        let library, samples, materials = freshStores ()
        let categories = BuiltinCatalogue.categoryStore (materialsReferencingCategory materials)
        Scene.initMainWith library (Experiments.createInMemoryExperiments ()) materials samples categories

    /// Click the shared table canvas at canvas-local coordinates (the pointer gestures live on
    /// the wrapping Border and read positions relative to the NAMED canvas).
    let private clickCanvasAt (window : Window) (sx : float) (sy : float) : unit =
        match tryFindControl window ConstructorUiId.tableElementCanvas.value with
        | Some canvas ->
            let p = canvas.TranslatePoint(Point(sx, sy), window)
            if p.HasValue then
                window.MouseDown(p.Value, MouseButton.Left, RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                window.MouseUp(p.Value, MouseButton.Left, RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
            else Assert.Fail "the canvas has no on-screen position"
        | None -> Assert.Fail "the table canvas was not found"

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (016): the Select state shows the Select-Close pair and the NON-REMOVABLE kind constraint — everything else IS the ordinary window`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let _, selectCtx = selectContext CircularPolarizer (TableElementTarget (ElementId "el-1"))
            let window = mountSelectLibraryWindow library samples materials selectCtx
            // Exactly the TWO buttons, one row, distinct positive/negative styling.
            Assert.True(isPresent window ConstructorUiId.librarySelectButton.value, "the Select button must render")
            Assert.True(isPresent window ConstructorUiId.librarySelectCloseButton.value, "the Close button must render")
            Assert.Equal("Select", textOf window ConstructorUiId.librarySelectButton.value)
            Assert.Equal("Close", textOf window ConstructorUiId.librarySelectCloseButton.value)
            // The pre-applied constraint: the banner NAMES the fixed kind, the corpus is
            // narrowed to it, and NO breadcrumb chip exists (nothing to remove).
            let banner = textOf window ConstructorUiId.librarySelectConstraint.value
            Assert.Contains("Circular polarizer", banner)
            Assert.Contains("fixed", banner)
            Assert.Equal("2 results", textOf window ConstructorUiId.facetResultCount.value)
            Assert.False(isPresent window (ConstructorUiId.facetBreadcrumbChip (ConstructorUiKey entryKindFacetKey.value)).value,
                         "the pre-applied constraint must take NO removable breadcrumb chip")
            Assert.True(isPresent window ((ConstructorUiId.libraryEntryNode PresetIds.polarizerCpLeft).value))
            Assert.False(isPresent window ((ConstructorUiId.libraryEntryNode PresetIds.source600).value), "an out-of-kind entry must not be listed")
            // Everything else IS the ordinary window: the add-on-the-fly verbs are all there.
            Assert.True(isPresent window ConstructorUiId.libraryAddSampleButton.value, "Add sample must survive Select state")
            Assert.True(isPresent window ConstructorUiId.libraryMakeMultilayerButton.value, "Make multilayer must survive Select state")
            Assert.True(isPresent window (ConstructorUiId.facetFilterBox.value), "the filter box must survive Select state")
            window.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (016): Select returns the highlighted entry through the TARGETED dispatch into the workbench and closes`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let model0 = freshMainModel ()
            // The detector is the selected element, so the readout shows ITS bound name.
            let mainWindow, dispatch = mountMainWithDispatch { model0 with selection = Scene.ElementSelected (Scene.SceneElementIndex 1) }
            // The Select session the step-017 Choose… flow will compose: onSelected dispatches
            // the TARGETED bind (by the element's serializable id) into the live scene loop.
            let cancels = ResizeArray<string>()
            let selectCtx : SelectionContext<LibraryEntry> =
                {
                    kindConstraint = KindConstraint Detector
                    currentEntryIdOpt = None
                    target = TableElementTarget (ElementId "det")
                    onSelected = fun entry -> dispatch (Scene.BindValueIdTo (ElementId "det", entry.entryId))
                    onCancelled = fun () -> cancels.Add "cancelled"
                }
            let selectWindow = mountSelectLibraryWindow library samples materials selectCtx
            clickOn selectWindow ((ConstructorUiId.libraryEntryNode PresetIds.detectorIntensity).value)
            clickOn selectWindow ConstructorUiId.librarySelectButton.value
            Dispatcher.UIThread.RunJobs()
            // The window closed itself after onSelected — and never cancelled.
            Assert.False(selectWindow.IsVisible, "Select must close the window after onSelected")
            Assert.Empty(cancels)
            // The TARGETED bind landed on the detector element: the workbench readout renders
            // the bound entry's display name in the same pass.
            Assert.Contains("bound: Intensity detector", textOf mainWindow ConstructorUiId.teReadout.value)
            Assert.False(isPresent mainWindow ConstructorUiId.workbenchSelectStatus.value, "a successful bind reports no staleness status")
            mainWindow.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (016): a changed table selection cancels and CLOSES the open Select-state Library window`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let events, selectCtx = selectContext Detector (TableElementTarget (ElementId "det"))
            let selectWindow = mountSelectLibraryWindow library samples materials selectCtx
            // The workbench holds the session handle (the step-017 Choose… flow populates it;
            // seeded here): selection sits on the detector element.
            let model0 = freshMainModel ()
            let session : Scene.SelectSession =
                {
                    target = ElementId "det"
                    cancelAndClose = fun () -> selectWindow.Close()
                }
            let mainWindow, _ =
                mountMainWithDispatch
                    { model0 with selection = Scene.ElementSelected (Scene.SceneElementIndex 1); activeSelect = Some session }
            Assert.True(selectWindow.IsVisible)
            // A REAL canvas click on the empty table changes the selection (element → table):
            // the staleness rule cancels and closes the open Select window in the same pass —
            // the pending-bind-clears precedent extended.
            clickCanvasAt mainWindow TableScene.center.sx TableScene.center.sy
            Assert.False(selectWindow.IsVisible, "the changed table selection must close the Select window")
            Assert.Equal<string list>([ "cancelled" ], List.ofSeq events)
            mainWindow.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (016): a vanished target element makes the Select return a NO-OP plus a status line — never a throw`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let model0 = freshMainModel ()
            let mainWindow, dispatch = mountMainWithDispatch { model0 with selection = Scene.ElementSelected (Scene.SceneElementIndex 1) }
            let selectCtx : SelectionContext<LibraryEntry> =
                {
                    kindConstraint = KindConstraint Detector
                    currentEntryIdOpt = None
                    target = TableElementTarget (ElementId "det")
                    onSelected = fun entry -> dispatch (Scene.BindValueIdTo (ElementId "det", entry.entryId))
                    onCancelled = fun () -> ()
                }
            let selectWindow = mountSelectLibraryWindow library samples materials selectCtx
            // The target element vanishes while the modeless window is open (no session handle
            // is registered here — the belt-and-braces race the targeted return must survive).
            dispatch Scene.RemoveSelectedElement
            Dispatcher.UIThread.RunJobs()
            // The return is a NO-OP plus the status line, never a throw.
            clickOn selectWindow ((ConstructorUiId.libraryEntryNode PresetIds.detectorIntensity).value)
            clickOn selectWindow ConstructorUiId.librarySelectButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.False(selectWindow.IsVisible, "the Select window still closes after its return")
            Assert.True(isPresent mainWindow ConstructorUiId.workbenchSelectStatus.value, "the vanished target must surface the status line")
            Assert.Contains("no longer on the table", textOf mainWindow ConstructorUiId.workbenchSelectStatus.value)
            Assert.DoesNotContain("bound: Intensity detector", textOf mainWindow ConstructorUiId.teReadout.value)
            mainWindow.Close()
            Dispatcher.UIThread.RunJobs())

    // ============== step 017 — the Select bay's Choose…/quick-pick flow (headless) ==============

    /// Mount the Main workbench through a re-rendering FuncUI Component that EXPOSES the latest
    /// model (the LibraryControlsTests drive pattern): the step-017 acceptance compares the
    /// model-level valueId the two bind paths land, so the readout text is not enough here.
    /// `latest` is the dispatch chain's source of truth; `state` only feeds the re-render.
    let private mountMainExposed (model0 : Scene.TableAndElementRotationModel) : Window * (Scene.TableAndElementRotationMsg -> unit) * (unit -> Scene.TableAndElementRotationModel) =
        let latest : Scene.TableAndElementRotationModel ref = ref model0
        let dispatchRef : (Scene.TableAndElementRotationMsg -> unit) ref = ref ignore
        let comp =
            Avalonia.FuncUI.Component(fun ctx ->
                let state = ctx.useState model0
                let dispatch (msg : Scene.TableAndElementRotationMsg) =
                    let m = Scene.updateTableAndElementRotation msg latest.Value
                    latest.Value <- m
                    state.Set m
                dispatchRef.Value <- dispatch
                Scene.mainView state.Current dispatch)
        let window = Window(Width = 980.0, Height = 1050.0, Content = comp)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window, (fun msg -> dispatchRef.Value msg), (fun () -> latest.Value)

    /// The Main scene over fresh stores with the detector element selected (2 detector entries
    /// < the default threshold 5 — the under-threshold fixture) and the Select bay shown.
    let private detectorSelectorModel () : Scene.TableAndElementRotationModel =
        { freshMainModel () with selection = Scene.ElementSelected (Scene.SceneElementIndex 1); ribbon = Scene.BayNames.selector }

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (017): an over-threshold kind offers Choose… alone — no quick-pick strip`` () =
        HeadlessSession.run (fun () ->
            // A Sample is selected: the 11 BuiltinCatalogue.samples () entries sit at/above the
            // default QuickPickThreshold 5, so the bay offers the Choose… verb ALONE.
            let model0 = Scene.updateTableAndElementRotation (Scene.AddElement Sample) (freshMainModel ())
            let window, _, getModel = mountMainExposed { model0 with ribbon = Scene.BayNames.selector }
            Assert.Equal<Scene.SelectorOffer>(Scene.ChooseAlone, (getModel ()).selectorOffer)
            Assert.True(isPresent window ConstructorUiId.selectorChooseButton.value, "Choose… must be offered for the over-threshold kind")
            Assert.False(isPresent window ConstructorUiId.selectorQuickPickStrip.value, "the quick-pick strip must NOT render at/above the threshold")
            Assert.False(isPresent window ConstructorUiId.libraryTree.value, "the strip's row tree must be gone with it")
            Assert.False(isPresent window (ConstructorUiId.libraryEntry glassFilm600EntryId.uiKey).value, "no inline sample row may remain")
            window.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (017): an under-threshold kind shows the quick-pick strip beside Choose…`` () =
        HeadlessSession.run (fun () ->
            let window, _, getModel = mountMainExposed (detectorSelectorModel ())
            Assert.Equal<Scene.SelectorOffer>(Scene.QuickPickAndChoose, (getModel ()).selectorOffer)
            Assert.True(isPresent window ConstructorUiId.selectorQuickPickStrip.value, "the strip must render below the threshold")
            Assert.True(isPresent window (ConstructorUiId.libraryEntry PresetIds.detectorIntensity.uiKey).value, "the kind-constrained rows render inline")
            Assert.True(isPresent window ConstructorUiId.selectorChooseButton.value, "Choose… is offered beside the strip")
            window.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (017): binding through the strip and through the Choose… Select window land the IDENTICAL valueId`` () =
        HeadlessSession.run (fun () ->
            // ---- Path 1: the inline quick-pick strip (under threshold). ----
            let stripWindow, _, stripModel = mountMainExposed (detectorSelectorModel ())
            clickOn stripWindow (ConstructorUiId.libraryEntry PresetIds.detectorIntensity.uiKey).value   // pending
            clickOn stripWindow ConstructorUiId.libraryConfirmButton.value                   // ConfirmBindValueId → the targeted bind
            let stripBound = ((stripModel ()).elements |> List.item 1).placement.valueId
            Assert.Equal(Some PresetIds.detectorIntensity, stripBound)
            stripWindow.Close()
            Dispatcher.UIThread.RunJobs()
            // ---- Path 2: the Choose… verb → the kind-constrained Select-state Library window. ----
            let mainWindow, _, getModel = mountMainExposed (detectorSelectorModel ())
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            clickOn mainWindow ConstructorUiId.selectorChooseButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            let selectWindow = opened.[0]
            Assert.True(matchesId ConstructorUiId.libraryWindow.value selectWindow, "Choose… must open the Library window")
            Assert.True(selectWindow.IsVisible)
            // Select state, constrained to the element's kind: the fixed no-chip banner names
            // Detector, only detector entries are offered, and the Select/Close pair is there.
            let banner = textOf selectWindow ConstructorUiId.librarySelectConstraint.value
            Assert.Contains("Detector", banner)
            Assert.Contains("fixed", banner)
            // The launcher-opened window opens collapsed (spec 0040 step 002) — expand its entries
            // group to reach the offered leaves.
            expandEntries selectWindow
            Assert.True(isPresent selectWindow ((ConstructorUiId.libraryEntryNode PresetIds.detectorIntensity).value))
            Assert.False(isPresent selectWindow ((ConstructorUiId.libraryEntryNode PresetIds.source600).value), "an out-of-kind entry must not be offered")
            Assert.True(isPresent selectWindow ConstructorUiId.librarySelectButton.value)
            // The workbench holds the session's staleness handle while the window is open.
            (match (getModel ()).activeSelect with
             | Some session -> Assert.Equal<ElementId>(ElementId "det", session.target)
             | None -> Assert.Fail "Choose… must register the Select session handle")
            // Bind through the window: highlight the entry, then Select.
            clickOn selectWindow ((ConstructorUiId.libraryEntryNode PresetIds.detectorIntensity).value)
            clickOn selectWindow ConstructorUiId.librarySelectButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.False(selectWindow.IsVisible, "the Select window closes after its return")
            let windowBound = ((getModel ()).elements |> List.item 1).placement.valueId
            // THE step acceptance: both paths committed the IDENTICAL valueId bind.
            Assert.Equal(Some PresetIds.detectorIntensity, windowBound)
            Assert.Equal<ElementValueId option>(stripBound, windowBound)
            (match (getModel ()).activeSelect with
             | None -> ()
             | Some _ -> Assert.Fail "the committed bind must end the Select session")
            Assert.False(isPresent mainWindow ConstructorUiId.workbenchSelectStatus.value, "a successful bind reports no staleness status")
            Assert.Contains("bound: Intensity detector", textOf mainWindow ConstructorUiId.teReadout.value)
            mainWindow.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``spec 0046: the element menu's Choose… specifies the LIGHT SOURCE through the same Select window as the Selector's`` () =
        HeadlessSession.run (fun () ->
            let model0 = freshMainModel ()
            let mainWindow, dispatch, getModel = mountMainExposed model0
            // A right click on the source selects it and opens its menu, which offers Choose….
            let sourcePoint = TableView.project TableScene.pixelsPerMeter TableScene.center model0.view (model0.snappedCentres.[Scene.SceneElementIndex 0])
            dispatch (Scene.OpenElementMenuAt sourcePoint)
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(Scene.ElementSelected (Scene.SceneElementIndex 0), (getModel ()).selection)
            Assert.Equal(None, ((getModel ()).elements |> List.item 0).placement.valueId)
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            clickOn mainWindow ConstructorUiId.elementMenuChoose.value
            Dispatcher.UIThread.RunJobs()
            // The menu closed and the Selector's Choose… flow ran: ONE Select-state Library window,
            // constrained to light sources, holding the session for the source.
            Assert.Equal(None, (getModel ()).elementMenu)
            Assert.Equal(1, opened.Count)
            let selectWindow = opened.[0]
            Assert.True(matchesId ConstructorUiId.libraryWindow.value selectWindow, "the menu's Choose… must open the Library window")
            Assert.Contains("Light source", textOf selectWindow ConstructorUiId.librarySelectConstraint.value)
            (match (getModel ()).activeSelect with
             | Some session -> Assert.Equal<ElementId>(ElementId "src", session.target)
             | None -> Assert.Fail "the menu's Choose… must register the Select session handle")
            expandEntries selectWindow
            Assert.True(isPresent selectWindow ((ConstructorUiId.libraryEntryNode PresetIds.source600).value))
            Assert.False(isPresent selectWindow ((ConstructorUiId.libraryEntryNode PresetIds.detectorIntensity).value), "an out-of-kind entry must not be offered")
            clickOn selectWindow ((ConstructorUiId.libraryEntryNode PresetIds.source600).value)
            clickOn selectWindow ConstructorUiId.librarySelectButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.False(selectWindow.IsVisible, "the Select window closes after its return")
            Assert.Equal(Some PresetIds.source600, ((getModel ()).elements |> List.item 0).placement.valueId)
            Assert.Contains("bound: Monochromatic 600 nm", textOf mainWindow ConstructorUiId.teReadout.value)
            mainWindow.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (017): a second Choose… RE-TARGETS the live Select window and the handle survives reference-keyed`` () =
        HeadlessSession.run (fun () ->
            let mainWindow, _, getModel = mountMainExposed (detectorSelectorModel ())
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            clickOn mainWindow ConstructorUiId.selectorChooseButton.value
            Dispatcher.UIThread.RunJobs()
            // A second Choose… on the same element RE-TARGETS the live single instance through
            // the launcher (no second window); the superseded session's cancel fires while the
            // successor is stored, and the reference-keyed end leaves the NEW handle in place.
            clickOn mainWindow ConstructorUiId.selectorChooseButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            let selectWindow = opened.[0]
            Assert.True(selectWindow.IsVisible, "the re-target keeps the one live window")
            (match (getModel ()).activeSelect with
             | Some session -> Assert.Equal<ElementId>(ElementId "det", session.target)
             | None -> Assert.Fail "the re-target must leave the NEW session handle in place")
            // Closing the window from ITS side (the title-bar X path) ends the session exactly
            // once and clears the workbench handle through the reference-keyed message.
            selectWindow.Close()
            Dispatcher.UIThread.RunJobs()
            (match (getModel ()).activeSelect with
             | None -> ()
             | Some _ -> Assert.Fail "the window-side close must clear the session handle")
            mainWindow.Close()
            Dispatcher.UIThread.RunJobs())

    // ============================ step 023 — lifecycle (pure) ============================

    /// BuiltinCatalogue.sample supplies glassFilm600 for two DISTINCT versions under ONE id —
    /// the multi-version history the live store never grows until step 25, proven here through a
    /// stub proxy: `resolveVersion` serves v1 and v2, `tryGetSample` / `listSamples` the latest.
    let private twoVersionSamples () : SampleId * Sample * Sample * SampleProxy =
        let template = BuiltinCatalogue.sample SampleIds.glassFilm600Id
        let id = newSampleId ()
        let v1 = { template with id = id; name = EntryName "Versioned sample v1" }
        let v2 = { template with id = id; name = EntryName "Versioned sample v2" }
        let proxy : SampleProxy =
            {
                listSamples = fun _ -> Ok [ v2 ]
                searchSamples = fun _ -> Ok [ v2 ]
                tryGetSample = fun sid -> Ok (if sid = id then Some v2 else None)
                resolveVersion =
                    fun svid ->
                        if svid.sampleId = id then
                            match svid.version.value with
                            | 1 -> Ok (Some v1)
                            | 2 -> Ok (Some v2)
                            | _ -> Ok None
                        else Ok None
                saveSample = fun _ -> Ok ()
                markSampleInactive = fun _ -> Ok ()
                markSampleActive = fun _ -> Ok ()
                supersedeSample = fun _ -> Ok ()
                pinSampleVersion = fun _ -> Ok ()
                removeSample = fun _ -> Ok ()
            }
        id, v1, v2, proxy

    [<Fact>]
    let ``the step-023 lifecycle UiIds are the stable intent-named ids`` () =
        Assert.Equal("LibraryShowInactiveToggle", ConstructorUiId.libraryShowInactiveToggle.value)
        Assert.Equal("LibraryMarkInactiveButton", ConstructorUiId.libraryMarkInactiveButton.value)
        Assert.Equal("LibraryMarkActiveButton", ConstructorUiId.libraryMarkActiveButton.value)
        Assert.Equal("LibrarySupersedeButton", ConstructorUiId.librarySupersedeButton.value)
        Assert.Equal("LibraryLifecycleConfirmButton", ConstructorUiId.libraryLifecycleConfirmButton.value)
        Assert.Equal("LibraryLifecycleCancelButton", ConstructorUiId.libraryLifecycleCancelButton.value)
        Assert.Equal("LibraryVersionsPanel", ConstructorUiId.libraryVersionsPanel.value)
        Assert.Equal("LibraryViewOnlyNote", ConstructorUiId.libraryViewOnlyNote.value)
        Assert.Equal("LibraryVersionRow_2", (ConstructorUiId.libraryVersionRow (VersionNumber 2)).value)

    [<Fact>]
    let ``no lifecycle verbs on a protected preset; a sample offers Mark inactive and Supersede`` () =
        let _, m = freshModel ()
        // A protected preset (ProtectedBuiltIn) offers NONE (removed, not greyed).
        let preset = LW.updateLibraryWindow (LW.SelectLibraryEntry PresetIds.source600) m
        Assert.Empty(preset.offeredLifecycleActions)
        // A UserManaged sample offers Mark inactive + Supersede…
        let sample = LW.updateLibraryWindow (LW.SelectLibraryEntry glassFilm600EntryId) m
        Assert.Equal<MaterialsWindowView.LifecycleAction list>([ MaterialsWindowView.MarkInactiveAction; MaterialsWindowView.SupersedeAction ], sample.offeredLifecycleActions)

    [<Fact>]
    let ``the show-inactive toggle scopes samples, badges retired ones, and keeps references resolving`` () =
        let samples, m = freshModelWithStores ()
        let retired =
            m
            |> LW.updateLibraryWindow (LW.SelectLibraryEntry glassFilm600EntryId)
            |> LW.updateLibraryWindow LW.RequestMarkSampleInactive
            |> LW.updateLibraryWindow LW.ConfirmSampleLifecycle
        Assert.Equal(LW.NoPendingSampleLifecycle, retired.lifecycleGate)
        // Default scope EXCLUDES the retired sample (one fewer); the badge count is 1.
        Assert.DoesNotContain(glassFilm600EntryId.value, filteredEntryIds retired)
        Assert.Equal(seededCorpus - 1, (LW.libraryFacetedState retired).resultCount.value)
        Assert.Equal(ItemCount 1, retired.inactiveCount)
        // The reference still resolves IGNORING lifecycle (the table keeps drawing it).
        match
            samples.resolveVersion
                {
                    sampleId = SampleIds.glassFilm600Id
                    version = VersionNumber.first
                }
        with
        | Ok (Some sample) -> Assert.Equal(SampleIds.glassFilm600Id, sample.id)
        | other -> Assert.Fail($"the retired sample's version must still resolve, got %A{other}")
        // The toggle reveals it, badged, in the tree.
        let shown = LW.updateLibraryWindow LW.ToggleShowInactiveEntries retired
        Assert.Equal(IncludeInactive, shown.showInactive)
        Assert.Contains(glassFilm600EntryId.value, filteredEntryIds shown)
        Assert.Equal(seededCorpus, (LW.libraryFacetedState shown).resultCount.value)
        let leaf =
            (LW.libraryFacetedState shown).tree
            |> List.head
            |> fun entries -> entries.children |> List.find (fun n -> n.code.value = glassFilm600EntryId.libraryEntryCode.value)
        Assert.Contains("inactive", leaf.label.value)
        // Presets never badge — a source leaf keeps its bare name.
        let srcLeaf =
            (LW.libraryFacetedState shown).tree
            |> List.head
            |> fun entries -> entries.children |> List.find (fun n -> n.code.value = PresetIds.source600.libraryEntryCode.value)
        Assert.DoesNotContain("inactive", srcLeaf.label.value)

    [<Fact>]
    let ``a sample lifecycle verb is confirm-gated and a vanished sample surfaces a typed refusal`` () =
        let samples, m = freshModelWithStores ()
        let armed =
            m
            |> LW.updateLibraryWindow (LW.SelectLibraryEntry glassFilm600EntryId)
            |> LW.updateLibraryWindow LW.RequestSupersedeSample
        Assert.Equal(LW.PendingSampleLifecycle (SampleIds.glassFilm600Id, MaterialsWindowView.SupersedeAction), armed.lifecycleGate)
        // A query change disarms.
        Assert.Equal(LW.NoPendingSampleLifecycle, (LW.updateLibraryWindow (LW.CommitLibraryTextFilter (TextQuery "x")) armed).lifecycleGate)
        // The sample vanishes behind the window's back, then Confirm meets the typed store block.
        match samples.removeSample SampleIds.glassFilm600Id with
        | Ok () -> ()
        | Error e -> Assert.Fail($"the out-of-band remove must succeed, got %A{e}")
        let refused = LW.updateLibraryWindow LW.ConfirmSampleLifecycle armed
        match refused.lastError with
        | Some (LW.SampleLifecycleRefusedErr (UnknownSampleIdErr reason)) -> Assert.Contains("unknown sample id", reason)
        | other -> Assert.Fail($"expected SampleLifecycleRefused UnknownSampleIdErr, got %A{other}")

    [<Fact>]
    let ``the view panel enumerates sample versions and an older version is view-only while Edit targets the latest`` () =
        let id, _, v2, samples = twoVersionSamples ()
        let library = BuiltinCatalogue.library ()
        let materials = BuiltinCatalogue.materialStore (samplesReferencing samples)
        let _, context = stubContext library samples materials
        let entryId = id.toElementValueId
        let selected = LW.initLibraryWindow context Browse |> LW.updateLibraryWindow (LW.SelectLibraryEntry entryId)
        Assert.Equal<int list>([ 1; 2 ], selected.selectedVersions |> List.map (fun (v, _) -> v.value))
        Assert.Equal<VersionNumber option>(None, selected.viewedVersion)
        // The Edit affordance targets the LATEST sample (v2).
        match selected.editableSample with
        | Some sample -> Assert.Equal(v2.name, sample.name)
        | None -> Assert.Fail "the latest version must remain editable"
        // Viewing an OLDER version records it; a new selection resets to the latest.
        let older = LW.updateLibraryWindow (LW.ViewSampleVersion (VersionNumber 1)) selected
        Assert.Equal<VersionNumber option>(Some (VersionNumber 1), older.viewedVersion)
        Assert.Equal<VersionNumber option>(None, (LW.updateLibraryWindow (LW.SelectLibraryEntry entryId) older).viewedVersion)

    // ============================ step 023 — lifecycle (headless) ============================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (023): a protected library entry shows NO lifecycle verbs; a sample shows them`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let window = mountLibraryWindow library samples materials
            // The protected source: none of the three lifecycle verbs render.
            commitFilter window "Monochromatic"
            clickOn window ((ConstructorUiId.libraryEntryNode PresetIds.source600).value)
            Assert.False(isPresent window ConstructorUiId.libraryMarkInactiveButton.value, "a protected entry shows no Mark inactive verb")
            Assert.False(isPresent window ConstructorUiId.librarySupersedeButton.value, "a protected entry shows no Supersede verb")
            Assert.False(isPresent window ConstructorUiId.libraryMarkActiveButton.value, "a protected entry shows no Mark active verb")
            // A sample: the lifecycle verbs are present (removed, not greyed — they exist here).
            commitFilter window "n=1.75"
            clickOn window ((ConstructorUiId.libraryEntryNode glassFilm600EntryId).value)
            Assert.True(isPresent window ConstructorUiId.libraryMarkInactiveButton.value, "a sample offers Mark inactive")
            Assert.True(isPresent window ConstructorUiId.librarySupersedeButton.value, "a sample offers Supersede")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (023): marking a sample inactive hides it, the toggle reveals it badged, and its reference still resolves`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let window = mountLibraryWindow library samples materials
            commitFilter window "n=1.75"
            clickOn window ((ConstructorUiId.libraryEntryNode glassFilm600EntryId).value)
            clickOn window ConstructorUiId.libraryMarkInactiveButton.value
            clickOn window ConstructorUiId.libraryLifecycleConfirmButton.value
            // Gone from the default (ActiveOnly) tree…
            commitFilter window ""
            Assert.False(isPresent window ((ConstructorUiId.libraryEntryNode glassFilm600EntryId).value),
                         "the retired sample must leave the default tree")
            // …but its version still resolves IGNORING lifecycle (the table keeps drawing it).
            match
                samples.resolveVersion
                    {
                        sampleId = SampleIds.glassFilm600Id
                        version = VersionNumber.first
                    }
            with
            | Ok (Some _) -> ()
            | other -> Assert.Fail($"the retired sample's version must still resolve, got %A{other}")
            // The show-inactive toggle carries the count badge and reveals the sample, badged.
            Assert.Contains("(1)", textOf window ConstructorUiId.libraryShowInactiveToggle.value)
            clickOn window ConstructorUiId.libraryShowInactiveToggle.value
            Assert.True(isPresent window ((ConstructorUiId.libraryEntryNode glassFilm600EntryId).value),
                        "the toggle must reveal the retired sample in the tree")
            Assert.Contains("inactive", textOf window ((ConstructorUiId.libraryEntryNode glassFilm600EntryId).value))
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (023): an older sample version opens VIEW-ONLY inline while the latest stays editable`` () =
        HeadlessSession.run (fun () ->
            let id, _, _, samples = twoVersionSamples ()
            let library = BuiltinCatalogue.library ()
            let materials = BuiltinCatalogue.materialStore (samplesReferencing samples)
            let _, context = stubContext library samples materials
            let entryId = id.toElementValueId
            // Mount the REAL MVU loop (the window minus its launcher composition — the gated-tree
            // mounting precedent) so the version-row clicks drive the pure update.
            let window = HostWindow(Width = 900.0, Height = 760.0)
            Program.mkSimple (fun () -> LW.initLibraryWindow context Browse) LW.updateLibraryWindow LW.libraryWindowView
            |> Program.withHost window
            |> Program.run
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // The tree opens collapsed (spec 0040 step 002) — expand entries to reach the leaf.
            expandEntries window
            clickOn window ((ConstructorUiId.libraryEntryNode entryId).value)
            // The version list renders both versions; the latest view carries NO view-only note.
            Assert.True(isPresent window ConstructorUiId.libraryVersionsPanel.value, "the view panel must list the sample's versions")
            Assert.True(isPresent window ((ConstructorUiId.libraryVersionRow (VersionNumber 1)).value))
            Assert.True(isPresent window ((ConstructorUiId.libraryVersionRow (VersionNumber 2)).value))
            Assert.False(isPresent window ConstructorUiId.libraryViewOnlyNote.value, "the latest version is editable — no view-only note")
            Assert.True(isPresent window ConstructorUiId.libraryEditButton.value, "the latest version keeps the Edit verb")
            // Clicking the OLDER version opens it view-only inline (no Save path).
            clickOn window ((ConstructorUiId.libraryVersionRow (VersionNumber 1)).value)
            Assert.True(isPresent window ConstructorUiId.libraryViewOnlyNote.value, "an older version must render the view-only note")
            Assert.Contains("view-only", textOf window ConstructorUiId.libraryViewOnlyNote.value)
            Assert.True(isPresent window ConstructorUiId.libraryEditButton.value, "the library still edits the latest version")
            window.Close())

    [<Fact>]
    let ``the backup and restore UiIds are the stable intent-named ids`` () : unit =
        let name = LibraryBackupName "library-20261106-120000.db"
        Assert.Equal("LibraryBackUpNowButton", ConstructorUiId.libraryBackUpNowButton.value)
        Assert.Equal("LibraryRestoreButton", ConstructorUiId.libraryRestoreButton.value)
        Assert.Equal("LibraryRestorePanel", ConstructorUiId.libraryRestorePanel.value)
        Assert.Equal("LibraryBackupRow_library-20261106-120000.db", (ConstructorUiId.libraryBackupRow name.uiKey).value)
        Assert.Equal("LibraryRestoreConfirmButton", ConstructorUiId.libraryRestoreConfirmButton.value)
        Assert.Equal("LibraryRestoreCancelButton", ConstructorUiId.libraryRestoreCancelButton.value)
        Assert.Equal("LibraryBackupStatusText", ConstructorUiId.libraryBackupStatusText.value)

    [<Fact>]
    let ``BackUpLibraryNow reaches the backup store and shows the backup's name`` () : unit =
        let _, initial = freshModel ()
        let error = LibraryBackupFailedErr "previous failure"
        let model =
            {
                initial with lastError = Some (LW.LibraryBackupRefusedErr error)
            }
        Assert.Equal(LW.NoPendingLibraryRestore, model.restoreGate)
        Assert.Equal<ControlCaption option>(None, model.backupStatus)
        let backedUp = LW.updateLibraryWindow LW.BackUpLibraryNow model
        let backups = model.context.libraryBackups.listBackups () |> Result.defaultWith (fun error -> failwith error.describe)
        let name = Assert.Single backups
        Assert.Equal(Some (ControlCaption $"Library backed up to {name.value}."), backedUp.backupStatus)
        Assert.Equal<LW.LibraryWindowError option>(None, backedUp.lastError)

    [<Fact>]
    let ``a refused backup shows its reason as the window message and no status`` () : unit =
        let _, initial = freshModel ()
        let error = LibraryBackupFailedErr "the backup folder is unavailable"
        let backups : LibraryBackupProxy =
            {
                backUpLibrary = fun () -> Error error
                listBackups = fun () -> Error error
                restoreLibrary = fun _ -> Error error
            }
        let model =
            {
                initial with
                    context =
                        {
                            initial.context with libraryBackups = backups
                        }
                    backupStatus = Some (ControlCaption "Previous backup succeeded.")
            }
        let refused = LW.updateLibraryWindow LW.BackUpLibraryNow model
        Assert.Equal(Some (LW.LibraryBackupRefusedErr error), refused.lastError)
        Assert.Equal<ControlCaption option>(None, refused.backupStatus)
        match refused.lastError with
        | Some failure -> Assert.Equal(error.describe, failure.caption.value)
        | None -> Assert.Fail "the backup refusal must carry the store's reason"
        let listRefused = LW.updateLibraryWindow LW.RequestLibraryRestore model
        Assert.Equal(Some (LW.LibraryBackupRefusedErr error), listRefused.lastError)
        Assert.Equal(LW.NoPendingLibraryRestore, listRefused.restoreGate)
        let name = LibraryBackupName "unavailable-backup"
        let picked =
            {
                model with restoreGate = LW.PendingLibraryRestore ([ name ], Some name)
            }
        let restoreRefused = LW.updateLibraryWindow LW.ConfirmLibraryRestore picked
        Assert.Equal(Some (LW.LibraryBackupRefusedErr error), restoreRefused.lastError)
        Assert.Equal(picked.restoreGate, restoreRefused.restoreGate)
        Assert.Equal(picked.selectedEntryId, restoreRefused.selectedEntryId)

    [<Fact>]
    let ``RequestLibraryRestore opens the panel over the backups, and Restore stays inert until one is picked`` () : unit =
        let _, model = freshModel ()
        let empty = LW.updateLibraryWindow LW.RequestLibraryRestore model
        Assert.Equal(LW.PendingLibraryRestore ([], None), empty.restoreGate)
        Assert.Equal(empty, LW.updateLibraryWindow LW.ConfirmLibraryRestore empty)
        let first = model.context.libraryBackups.backUpLibrary () |> Result.defaultWith (fun error -> failwith error.describe)
        let second = model.context.libraryBackups.backUpLibrary () |> Result.defaultWith (fun error -> failwith error.describe)
        let opened = LW.updateLibraryWindow LW.RequestLibraryRestore model
        Assert.Equal(LW.PendingLibraryRestore ([ second; first ], None), opened.restoreGate)
        Assert.Equal<ControlCaption option>(None, opened.backupStatus)
        Assert.Equal<LW.LibraryWindowError option>(None, opened.lastError)
        Assert.Equal(opened, LW.updateLibraryWindow LW.ConfirmLibraryRestore opened)
        let unlisted = LibraryBackupName "not-listed"
        Assert.Equal(opened, LW.updateLibraryWindow (LW.PickLibraryBackup unlisted) opened)
        Assert.Equal(model, LW.updateLibraryWindow (LW.PickLibraryBackup first) model)
        Assert.Equal(model, LW.updateLibraryWindow LW.ConfirmLibraryRestore model)
        Assert.Equal<Result<LibraryBackupName list, LibraryBackupError>>(Ok [ second; first ], model.context.libraryBackups.listBackups ())

    [<Fact>]
    let ``confirming a picked backup restores it, names the pre-restore backup, closes the panel and clears the selection`` () : unit =
        let _, model = freshModel ()
        let backup = model.context.libraryBackups.backUpLibrary () |> Result.defaultWith (fun error -> failwith error.describe)
        let selected =
            model
            |> LW.updateLibraryWindow (LW.SelectLibraryEntry glassFilm600EntryId)
            |> LW.updateLibraryWindow (LW.ViewSampleVersion VersionNumber.first)
        Assert.Equal(Some glassFilm600EntryId, selected.selectedEntryId)
        Assert.Equal(Some glassFilm600EntryId.libraryEntryCode, selected.selectedNodeOpt)
        Assert.Equal(Some VersionNumber.first, selected.viewedVersion)
        let picked =
            selected
            |> LW.updateLibraryWindow LW.RequestLibraryRestore
            |> LW.updateLibraryWindow (LW.PickLibraryBackup backup)
        Assert.Equal(LW.PendingLibraryRestore ([ backup ], Some backup), picked.restoreGate)
        let restored = LW.updateLibraryWindow LW.ConfirmLibraryRestore picked
        Assert.Equal(LW.NoPendingLibraryRestore, restored.restoreGate)
        Assert.Equal<ElementValueId option>(None, restored.selectedEntryId)
        Assert.Equal<ConstructorUiKey option>(None, restored.selectedNodeOpt)
        Assert.Equal<VersionNumber option>(None, restored.viewedVersion)
        Assert.Equal<LW.LibraryWindowError option>(None, restored.lastError)
        match model.context.libraryBackups.listBackups () with
        | Ok [ before; original ] ->
            Assert.Equal(backup, original)
            Assert.NotEqual(backup, before)
            Assert.Equal(Some (ControlCaption $"Library restored from {backup.value}. The library before the restore is backed up as {before.value}."), restored.backupStatus)
        | _ -> Assert.Fail "restore must put the pre-restore backup first, before the original"

    [<Fact>]
    let ``Cancel closes the restore panel and restores nothing, and arming Remove closes it too`` () : unit =
        let _, initial = freshModel ()
        let backup = initial.context.libraryBackups.backUpLibrary () |> Result.defaultWith (fun error -> failwith error.describe)
        let opened =
            initial
            |> LW.updateLibraryWindow (LW.SelectLibraryEntry glassFilm600EntryId)
            |> LW.updateLibraryWindow LW.RequestLibraryRestore
            |> LW.updateLibraryWindow (LW.PickLibraryBackup backup)
        let cancelled = LW.updateLibraryWindow LW.CancelLibraryRestore opened
        let expected =
            {
                opened with restoreGate = LW.NoPendingLibraryRestore
            }
        Assert.Equal(expected, cancelled)
        let removing = LW.updateLibraryWindow LW.RequestRemoveSelectedEntry opened
        Assert.Equal(LW.NoPendingLibraryRestore, removing.restoreGate)
        Assert.Equal(LW.PendingSampleRemove SampleIds.glassFilm600Id, removing.removeGate)
        let reopened = LW.updateLibraryWindow LW.RequestLibraryRestore removing
        Assert.Equal(LW.NoPendingLibraryRemove, reopened.removeGate)
        let retiring = LW.updateLibraryWindow LW.RequestMarkSampleInactive reopened
        Assert.Equal(LW.NoPendingLibraryRestore, retiring.restoreGate)
        Assert.Equal(LW.NoPendingLibraryRemove, retiring.removeGate)
        Assert.Equal(LW.PendingSampleLifecycle (SampleIds.glassFilm600Id, MaterialsWindowView.MarkInactiveAction), retiring.lifecycleGate)
        Assert.Equal(LW.NoPendingSampleLifecycle, (LW.updateLibraryWindow LW.RequestLibraryRestore retiring).lifecycleGate)
        Assert.Equal(LW.NoPendingSampleLifecycle, (LW.updateLibraryWindow LW.RequestRemoveSelectedEntry retiring).lifecycleGate)
        Assert.Equal<Result<LibraryBackupName list, LibraryBackupError>>(Ok [ backup ], initial.context.libraryBackups.listBackups ())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the Library window lists a registered catalogue folder, and Unregister removes it and keeps its entries`` () : unit =
        HeadlessSession.run (fun () ->
            let owner = FolderName "LibraryWindowCatalogueTests"
            let locations = TestAppScope.fileLocations owner
            let ctx =
                AppContext.create locations WorkbenchSettings.defaults
                |> Result.defaultWith (fun error -> failwith error.describe)
            TestAppScope.settlePendingImports ctx
            let folder, catalogue = catalogueFolderFixture ()
            let pending =
                match ctx.catalogueImports.registerFolder folder with
                | Ok (CatalogueAwaitsChoices pending) -> pending
                | Ok _ -> failwith "Every new catalogue row must await confirmation."
                | Error error -> failwith error.describe
            confirmCatalogueRegistration ctx.catalogueImports pending
            let source =
                (Assert.Single catalogue.envelopes).content.toSourcePreset
                |> Result.defaultWith (fun error -> failwith error.describe)
            let registered =
                ctx.catalogueImports.proxy.registeredFolders ()
                |> Result.defaultWith (fun error -> failwith error.describe)
                |> Assert.Single
            let rowId = ConstructorUiId.libraryCatalogueFolderRow catalogue.manifest.id.uiKey
            let window = LibraryWindow(ctx.library, ctx.samples, ctx.sources, ctx.materials, ctx.categories, ctx.libraryBackups, ctx.catalogueImports)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            try
                Assert.True(isPresent window ConstructorUiId.libraryRegisterCatalogueFolderButton.value)
                Assert.True(isPresent window ConstructorUiId.libraryCatalogueFoldersPanel.value)
                Assert.True(isPresent window rowId.value)
                Assert.Equal(folder.value, textOf window rowId.value)
                let importedAt = registered.importedAt.value.ToString("yyyy-MM-dd HH:mm", System.Globalization.CultureInfo.InvariantCulture)
                let texts = window.GetVisualDescendants() |> Seq.choose (function :? TextBlock as text -> Some text.Text | _ -> None) |> Seq.toList
                Assert.Contains($"imported {importedAt} UTC", texts)
                Assert.Contains("1 entries", texts)
                Assert.Equal<Result<SourcePreset option, SourceError>>(Ok (Some source), ctx.sources.tryGetSource source.id)
                clickOn window (ConstructorUiId.libraryUnregisterCatalogueButton catalogue.manifest.id.uiKey).value
                Assert.False(isPresent window rowId.value)
                Assert.False(isPresent window ConstructorUiId.libraryCatalogueFoldersPanel.value)
                Assert.Equal($"Unregistered {folder.value}. Its entries stay in the library.", textOf window ConstructorUiId.libraryImportStatusText.value)
                Assert.Equal<Result<CatalogueImportLogEntry list, CatalogueImportError>>(Ok [], ctx.catalogueImports.proxy.registeredFolders ())
                Assert.Equal<Result<SourcePreset option, SourceError>>(Ok (Some source), ctx.sources.tryGetSource source.id)
            finally
                window.Close()
                Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the Library window backs up and restores the app-scope library through its buttons`` () : unit =
        HeadlessSession.run (fun () ->
            let owner = FolderName "LibraryWindowBackupTests"
            let locations = TestAppScope.fileLocations owner
            let ctx =
                AppContext.create locations WorkbenchSettings.defaults
                |> Result.defaultWith (fun error -> failwith error.describe)
            TestAppScope.settlePendingImports ctx
            let window = LibraryWindow(ctx.library, ctx.samples, ctx.sources, ctx.materials, ctx.categories, ctx.libraryBackups, ctx.catalogueImports)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            try
                Assert.True(isPresent window ConstructorUiId.libraryBackUpNowButton.value)
                Assert.True(isPresent window ConstructorUiId.libraryRestoreButton.value)
                clickOn window ConstructorUiId.libraryBackUpNowButton.value
                let backups = ctx.libraryBackups.listBackups () |> Result.defaultWith (fun error -> failwith error.describe)
                let backup = Assert.Single backups
                Assert.Equal($"Library backed up to {backup.value}.", textOf window ConstructorUiId.libraryBackupStatusText.value)
                let sample =
                    {
                        BuiltinCatalogue.sample SampleIds.glassFilm600Id with
                            id = newSampleId ()
                            name = EntryName "Sample added after the backup"
                    }
                Assert.Equal<Result<unit, SampleError>>(Ok (), ctx.samples.saveSample sample)
                Assert.Equal<Result<Sample option, SampleError>>(Ok (Some sample), ctx.samples.tryGetSample sample.id)
                clickOn window ConstructorUiId.libraryRestoreButton.value
                Assert.True(isPresent window ConstructorUiId.libraryRestorePanel.value)
                Assert.Contains("The current library is backed up first.", textOf window ConstructorUiId.libraryRestorePanel.value)
                Assert.False(isPresent window ConstructorUiId.libraryBackupStatusText.value)
                clickOn window ConstructorUiId.libraryRestoreConfirmButton.value
                Assert.Equal<Result<Sample option, SampleError>>(Ok (Some sample), ctx.samples.tryGetSample sample.id)
                Assert.Equal<Result<LibraryBackupName list, LibraryBackupError>>(Ok [ backup ], ctx.libraryBackups.listBackups ())
                let rowId = ConstructorUiId.libraryBackupRow backup.uiKey
                let idle = backgroundColorOf (labelBorderOf window rowId.value)
                clickOn window rowId.value
                let picked = backgroundColorOf (labelBorderOf window rowId.value)
                Assert.NotEqual(idle, picked)
                let restoreButton = labelBorderOf window ConstructorUiId.libraryRestoreConfirmButton.value
                let cancelButton = labelBorderOf window ConstructorUiId.libraryRestoreCancelButton.value
                Assert.Equal(picked, backgroundColorOf restoreButton)
                Assert.NotEqual(backgroundColorOf restoreButton, backgroundColorOf cancelButton)
                Assert.Same(restoreButton.Parent, cancelButton.Parent)
                Assert.True(restoreButton.Bounds.Right < cancelButton.Bounds.Left, "Restore and Cancel must have a visible gap")
                clickOn window ConstructorUiId.libraryRestoreConfirmButton.value
                Assert.Equal<Result<Sample option, SampleError>>(Ok None, ctx.samples.tryGetSample sample.id)
                Assert.Equal<Result<LibraryEntry option, LibraryError>>(Ok None, ctx.library.tryGetEntry sample.id.toElementValueId)
                Assert.False(isPresent window ConstructorUiId.libraryRestorePanel.value)
                let restoredBackups = ctx.libraryBackups.listBackups () |> Result.defaultWith (fun error -> failwith error.describe)
                match restoredBackups with
                | [ before; original ] ->
                    Assert.Equal(backup, original)
                    Assert.NotEqual(backup, before)
                    Assert.Equal($"Library restored from {backup.value}. The library before the restore is backed up as {before.value}.", textOf window ConstructorUiId.libraryBackupStatusText.value)
                | _ -> Assert.Fail "the restore must leave the original and the pre-restore backup"
                let files = System.IO.Directory.GetFiles(locations.backupsFolder.value, "*.db")
                Assert.Equal(2, files.Length)
                for name in restoredBackups do
                    Assert.True(System.IO.File.Exists(System.IO.Path.Combine(locations.backupsFolder.value, name.value)))
            finally
                window.Close()
                Dispatcher.UIThread.RunJobs())
