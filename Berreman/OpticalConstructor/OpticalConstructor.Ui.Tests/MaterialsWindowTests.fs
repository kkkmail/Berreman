namespace OpticalConstructor.Ui.Tests

open System
open System.IO
open System.Collections.Generic
open System.Threading.Tasks
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.EntryFileJson
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Platform
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Media
open Avalonia.Platform.Storage
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Facets
open OpticalConstructor.Domain.LibraryFacets
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.SampleStackEditor
open OpticalConstructor.Domain.WindowMode
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls
open OpticalConstructor.Tests
open OpticalConstructor.Ui

/// Spec 0038 Part E (step 013) — the Materials window (UICOMP_XDUO_0009): the single-instance
/// window instantiating the step-012 `FacetedTreeControls` over the step-011 material facets,
/// beside the view panel and the Add / Edit / Remove / Categories… verbs rewired from the
/// retired Materials bay. Two layers, the repo precedent: pure tests for the MVU model, the
/// engine projection and the disarm discipline; headless proofs driving the REAL window (and
/// the REAL workbench strip button) by automation ids — the slice acceptance: the strip button
/// opens ONE window (a second click ACTIVATES it), applying facet constraints narrows the
/// corpus, the verbs operate over the shared app-scoped stores, and a category rename re-labels
/// the category facet. Step 016 adds the Select-state suite: the Select/Close pair, the
/// structural (non-removable) kind constraint banner, the targeted `onSelected` dispatch into
/// the sample editor's layer, close-on-table-selection-change, and the vanished-row no-op.
[<Collection("Library picker isolation")>]
module MaterialsWindowTests =

    module MW = OpticalConstructor.Ui.MaterialsWindowView
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
            // Spec 0046: scroll the box into view first (the facet rows now WRAP, so the tree can sit below
            // the visible area of its scroller, where a click at the box's centre lands nowhere).
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

    /// The label text INSIDE the clickable Border carrying `id` (a picker option box is a
    /// Border with a single TextBlock child).
    let private labelInside (window : Window) (id : string) : string =
        match window.GetVisualDescendants() |> Seq.tryPick (function :? Border as b when matchesId id b -> Some b | _ -> None) with
        | Some b ->
            match b.GetVisualDescendants() |> Seq.tryPick (function :? TextBlock as t -> Some t.Text | _ -> None) with
            | Some text -> text
            | None -> failwith $"%s{id} carries no text label"
        | None -> failwith $"%s{id} was not found in the visual tree"

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
    let private chosenFill = Color.FromRgb(150uy, 185uy, 235uy)

    /// The number of generated tree node rows (any control whose AutomationId carries the
    /// FacetTreeNode_ prefix) — gated mode must render ZERO.
    let private treeRowCount (window : Window) : int =
        window.GetVisualDescendants()
        |> Seq.filter (fun v ->
            match v with
            | :? Control as c ->
                let autoId = Avalonia.Automation.AutomationProperties.GetAutomationId(c)
                not (isNull autoId) && autoId.StartsWith("FacetTreeNode_")
            | _ -> false)
        |> Seq.length

    /// BuiltinCatalogue.sampleStore, BuiltinCatalogue.materialStore and BuiltinCatalogue.categoryStore
    /// stay isolated per test; removal consults the live sample and material references.
    let private freshStores () : MaterialProxy * SampleProxy * CategoryProxy =
        let samples = BuiltinCatalogue.sampleStore ()
        let materials = BuiltinCatalogue.materialStore (samplesReferencing samples)
        let categories = BuiltinCatalogue.categoryStore (materialsReferencingCategory materials)
        materials, samples, categories

    /// A recording stub context (the functional-proxy seam): the launchers append tags, so a
    /// verb's request is observable without opening a window.
    let private stubContext (materials : MaterialProxy) (categories : CategoryProxy) : ResizeArray<string> * MW.MaterialsWindowContext =
        let calls = ResizeArray<string>()
        let context : MW.MaterialsWindowContext =
            {
                materials = materials
                categories = categories
                catalogueImports = CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy
                treeAutoBuildThreshold = TreeAutoBuildThreshold.defaultValue
                openMaterialEditor =
                    fun intent ->
                        calls.Add(
                            match intent with
                            | MaterialEditorView.NewMaterial mintedId -> $"material-add:{mintedId.value}"
                            | MaterialEditorView.EditMaterial entry -> "material-edit:" + entry.name.value)
                openCategoryEditor = fun () -> calls.Add "categories-open"
                openImportDialog = fun _ -> calls.Add "import-open"
                requestClose = fun () -> calls.Add "close-requested"
            }
        calls, context

    let private freshModel () : ResizeArray<string> * MW.MaterialsWindowModel =
        let materials, _, categories = freshStores ()
        let calls, context = stubContext materials categories
        calls, MW.initMaterialsWindow context Browse

    let private freshDropFolder () : FolderName =
        let folder = FolderName (Path.Combine(System.AppContext.BaseDirectory, "MaterialsWindowTests", Guid.NewGuid().ToString("N")))
        Directory.CreateDirectory folder.value |> ignore
        folder

    let private writeDroppedMaterial () : FileName * MaterialEntry =
        let material =
            {
                BuiltinCatalogue.material MaterialIds.silicon with
                    id = newMaterialId ()
                    name = EntryName "Dropped Materials-window material"
            }
        let content = LibraryEntryContent.ofMaterialEntry VersionNumber.first UserProvenance material
        let envelope = content.``sealed`` |> Result.defaultWith (fun error -> failwith error.describe)
        let text = envelope.fileText |> Result.defaultWith (fun error -> failwith error.describe)
        let folder = freshDropFolder ()
        let leaf = FileName "dropped.material-json"
        let file = leaf.combine folder
        File.WriteAllText(file.value, text.value)
        file, material

    /// A recording Select-session context (spec 0038 step 016): `onSelected` tags the chosen
    /// material id, `onCancelled` tags the cancel — the window guarantees exactly one fires.
    let private selectContext (kind : CatalogueKind) (target : SelectionTarget) : ResizeArray<string> * SelectionContext<MaterialEntry> =
        let events = ResizeArray<string>()
        let context : SelectionContext<MaterialEntry> =
            {
                kindConstraint = KindConstraint kind
                currentEntryIdOpt = None
                target = target
                onSelected = fun entry -> events.Add ("selected:" + string entry.id.value)
                onCancelled = fun () -> events.Add "cancelled"
            }
        events, context

    /// Rename a built-in category through the proxy (built-ins ARE renamable — no origin guard
    /// on update), failing the test on a typed rejection.
    let private renameGlass (categories : CategoryProxy) (newName : string) : unit =
        match categories.updateCategory { id = CategoryIds.glass; name = CategoryName newName; visibility = SelectableOnCreate; origin = BuiltInCategory } with
        | Ok () -> ()
        | Error e -> Assert.Fail($"rename failed: %A{e}")

    let private filteredIds (m : MW.MaterialsWindowModel) : MaterialId list =
        m.filteredEntries |> List.map (fun e -> e.id)

    let private freshExportContext () : InMemoryCatalogueStores * MW.MaterialsWindowContext =
        let stores = CatalogueImportProxy.createInMemory ()
        let imports = CatalogueImportContext.create stores.proxy
        imports.importFrom (CatalogueProxy.createBuiltIn ())
        |> Result.bind (fun decisions -> imports.apply (decisions |> List.map _.byDefault))
        |> Result.defaultWith (fun error -> failwith error.describe)
        |> ignore
        let _, context = stubContext stores.materials stores.categories
        let configured =
            {
                context with
                    catalogueImports = imports
            }
        stores, configured

    type private MaterialsExportTestWindow(provider : IStorageProvider) =
        inherit HostWindow(Width = 1150.0, Height = 800.0)

        member _.exportStorageProvider : IStorageProvider = provider

    type private MaterialsExportTestAppBuilder() =
        static member BuildAvaloniaApp () : AppBuilder =
            let factory =
                {
                    new IStorageProviderFactory with
                        member _.CreateProvider topLevel =
                            match topLevel with
                            | :? MaterialsExportTestWindow as window -> window.exportStorageProvider
                            | _ -> null
                }
            TestAppBuilder.BuildAvaloniaApp().With<IStorageProviderFactory>(factory)

        static member run (action : unit -> unit) : unit =
            use session = HeadlessUnitTestSession.StartNew(typeof<MaterialsExportTestAppBuilder>)
            session.Dispatch(Action(action), System.Threading.CancellationToken.None).GetAwaiter().GetResult()

    let private exportPickerFolder (folder : FolderName) : IStorageFolder =
        LibraryPickerStorageTestProxy.create<IStorageFolder> (fun methodInfo _ ->
            match methodInfo.Name with
            | "get_Path" -> box (Uri folder.value)
            | "get_Name" -> box (Path.GetFileName folder.value)
            | "Dispose" -> null
            | _ -> invalidOp $"Unexpected export folder operation: {methodInfo.Name}")

    let private mountExportView (window : HostWindow) (model : MW.MaterialsWindowModel)
        (observe : MW.MaterialsWindowMsg -> MW.MaterialsWindowModel -> unit) : unit =
        Program.mkSimple
            (fun () -> model)
            (fun message current ->
                let updated = MW.updateMaterialsWindow message current
                observe message updated
                updated)
            MW.materialsWindowView
        |> Program.withHost window
        |> Program.run
        window.Show()
        Dispatcher.UIThread.RunJobs()

    // ============================ pure: ids contract ============================

    [<Fact>]
    let ``the MaterialsWindow UiIds are the stable intent-named ids`` () =
        Assert.Equal("MaterialsWindow", ConstructorUiId.materialsWindow.value)
        Assert.Equal("MaterialsFacetTreeHost", ConstructorUiId.materialsFacetTreeHost.value)
        Assert.Equal("MaterialsViewPanel", ConstructorUiId.materialsViewPanel.value)
        Assert.Equal("MaterialsViewPanelNkChart", ConstructorUiId.materialsViewPanelNkChart.value)
        Assert.Equal("MaterialsAddButton", ConstructorUiId.materialsAddButton.value)
        Assert.Equal("MaterialsEditButton", ConstructorUiId.materialsEditButton.value)
        Assert.Equal("MaterialsRemoveButton", ConstructorUiId.materialsRemoveButton.value)
        Assert.Equal("MaterialsCategoriesButton", ConstructorUiId.materialsCategoriesButton.value)
        Assert.Equal("MaterialsRemoveConfirmButton", ConstructorUiId.materialsRemoveConfirmButton.value)
        Assert.Equal("MaterialsRemoveCancelButton", ConstructorUiId.materialsRemoveCancelButton.value)
        Assert.Equal("MaterialsWindowMessage", ConstructorUiId.materialsWindowMessage.value)
        // The entry-leaf id derives from the tree-node code family, prefixed so it cannot collide.
        Assert.Equal(
            "FacetTreeNode_entry:" + string MaterialIds.glass152.value,
            (ConstructorUiId.materialsEntryNode MaterialIds.glass152).value)
        // The constructor-side entry point: the ribbon strip's right-aligned button.
        Assert.Equal("OpenMaterialsWindowButton", ConstructorUiId.openMaterialsWindowButton.value)

    [<Fact>]
    let ``the export and attribution UiIds of the Materials window are the stable intent-named ids`` () : unit =
        Assert.Equal("MaterialsExportButton", ConstructorUiId.materialsExportButton.value)
        Assert.Equal("MaterialsExportShownButton", ConstructorUiId.materialsExportShownButton.value)
        Assert.Equal("MaterialsExportStatusText", ConstructorUiId.materialsExportStatusText.value)
        Assert.Equal("MaterialsAttributionPanel", ConstructorUiId.materialsAttributionPanel.value)

    [<Fact>]
    let ``ExportSelectedMaterial writes the selected material's entry file and pins its version`` () : unit =
        MaterialsExportTestAppBuilder.run (fun () ->
            let stores, context = freshExportContext ()
            let labId = newMaterialId ()
            let otherId = newMaterialId ()
            let labName = EntryName "Lab glass"
            let otherName = EntryName "Lab quartz"
            let lab = BuiltinCatalogue.userMaterialContent labId labName
            let other = BuiltinCatalogue.userMaterialContent otherId otherName
            BuiltinCatalogue.putSealedEntries context.catalogueImports [ lab; other ]
            let held =
                stores.proxy.tryLatestEnvelope lab.id
                |> Result.defaultWith (fun error -> failwith error.describe)
                |> Option.defaultWith (fun () -> failwith "Lab glass must be held.")
            let folder = freshDropFolder ()
            let fileLeaf = FileName "Lab glass.material-json"
            let file = fileLeaf.combine folder
            let suggested = exportPickerFolder folder
            let pickedFile =
                LibraryPickerStorageTestProxy.create<IStorageFile> (fun methodInfo _ ->
                    match methodInfo.Name with
                    | "get_Path" -> box (Uri file.value)
                    | "Dispose" -> null
                    | _ -> invalidOp $"Unexpected export file operation: {methodInfo.Name}")
            let paths = ResizeArray<Uri>()
            let options = ResizeArray<FilePickerSaveOptions>()
            let mutable cancelPick = false
            let provider =
                LibraryPickerStorageTestProxy.create<IStorageProvider> (fun methodInfo arguments ->
                    match methodInfo.Name with
                    | "get_CanSave" -> box true
                    | "TryGetFolderFromPathAsync" ->
                        paths.Add(unbox<Uri> arguments.[0])
                        box (Task.FromResult<IStorageFolder> suggested)
                    | "SaveFilePickerAsync" ->
                        options.Add(unbox<FilePickerSaveOptions> arguments.[0])
                        box (Task.FromResult<IStorageFile>(if cancelPick then null else pickedFile))
                    | _ -> invalidOp $"Unexpected material save operation: {methodInfo.Name}")
            let configured =
                {
                    context with
                        catalogueImports =
                            {
                                context.catalogueImports with
                                    exportFolderOpt = Some folder
                            }
                }
            let initial = MW.initMaterialsWindow configured Browse
            Assert.Equal<ControlCaption option>(None, initial.entryExportStatus)
            Assert.Equal<LibraryExportError option>(None, initial.entryExportError)
            Assert.Same(initial, MW.updateMaterialsWindow (MW.ExportSelectedMaterial file) initial)
            Assert.False(File.Exists file.value)
            let selected =
                initial
                |> MW.updateMaterialsWindow (MW.CommitTextFilter (TextQuery "Lab"))
                |> MW.updateMaterialsWindow (MW.SelectEntry otherId)
            let mutable current = selected
            let exportedMessages = ResizeArray<MW.MaterialsWindowMsg>()
            let window = MaterialsExportTestWindow(provider)
            try
                mountExportView window selected (fun message updated ->
                    current <- updated
                    match message with
                    | MW.ExportSelectedMaterial _ ->
                        Assert.True(Dispatcher.UIThread.CheckAccess())
                        exportedMessages.Add message
                    | _ -> ())
                expandEntries window
                clickOn window (ConstructorUiId.materialsEntryNode labId).value
                clickOn window ConstructorUiId.materialsRemoveButton.value
                let before = current
                clickOn window ConstructorUiId.materialsExportButton.value
                Assert.Equal(MW.ExportSelectedMaterial file, Assert.Single exportedMessages)
                let save = Assert.Single options
                Assert.Equal("Export library entry", save.Title)
                Assert.Equal("Lab glass.material-json", save.SuggestedFileName)
                Assert.Equal("material-json", save.DefaultExtension)
                Assert.Equal(Nullable true, save.ShowOverwritePrompt)
                Assert.Same(suggested, save.SuggestedStartLocation)
                Assert.Equal("*.material-json", Assert.Single((Assert.Single save.FileTypeChoices).Patterns))
                Assert.Equal(folder.value, (Assert.Single paths).LocalPath.TrimEnd(Path.DirectorySeparatorChar))
                Assert.True(File.Exists file.value)
                let read = context.catalogueImports.readEntryFile MaterialEntryKind file |> Result.defaultWith (fun error -> failwith error.describe)
                Assert.Equal(held, read)
                let expected = ControlCaption $"Exported Lab glass version 1 to {file.value}; that version is now pinned."
                Assert.Equal<ControlCaption option>(Some expected, current.entryExportStatus)
                Assert.Equal<LibraryExportError option>(None, current.entryExportError)
                Assert.Equal(expected.value, textOf window ConstructorUiId.materialsExportStatusText.value)
                Assert.Equal(before.removeGate, current.removeGate)
                Assert.Equal(before.lifecycleGate, current.lifecycleGate)
                Assert.Equal(before.selectedId, current.selectedId)
                match current.mode with
                | Browse -> ()
                | Select _ -> Assert.Fail("Export must preserve Browse mode.")
                cancelPick <- true
                clickOn window ConstructorUiId.materialsExportButton.value
                Assert.Equal(2, options.Count)
                Assert.Single exportedMessages |> ignore
                Assert.Equal<ControlCaption option>(Some expected, current.entryExportStatus)
                let material = lab.toMaterialEntry |> Result.defaultWith (fun error -> failwith error.describe)
                let langasite = BuiltinCatalogue.material MaterialIds.langasite
                let edited =
                    {
                        material with
                            physics = langasite.physics
                    }
                Assert.Equal<Result<unit, MaterialError>>(Ok (), stores.materials.saveMaterial edited)
                let first = MaterialVersionId.firstOf labId
                let second =
                    {
                        first with
                            version = first.version.next
                    }
                Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some material), stores.materials.resolveVersion first)
                Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some edited), stores.materials.resolveVersion second)
            finally window.Close())

    [<Fact>]
    let ``ExportShownMaterials writes the shown materials, a category facet's included, with their user category`` () : unit =
        MaterialsExportTestAppBuilder.run (fun () ->
            let stores, context = freshExportContext ()
            let categoryId = CategoryId (Guid.NewGuid())
            let categoryName = CategoryName "Lab category"
            let category = BuiltinCatalogue.userCategoryContent categoryId categoryName
            let glassId = newMaterialId ()
            let quartzId = newMaterialId ()
            let otherId = newMaterialId ()
            let glassName = EntryName "Lab glass"
            let quartzName = EntryName "Lab quartz"
            let otherName = EntryName "Other glass"
            let glass =
                {
                    BuiltinCatalogue.userMaterialContent glassId glassName with
                        categoryOpt = Some categoryId
                }
            let quartz =
                {
                    BuiltinCatalogue.userMaterialContent quartzId quartzName with
                        categoryOpt = Some categoryId
                }
            let other = BuiltinCatalogue.userMaterialContent otherId otherName
            BuiltinCatalogue.putSealedEntries context.catalogueImports [ category; glass; quartz; other ]
            let start = freshDropFolder ()
            let targetFolder = FolderName (Path.Combine(start.value, "Lab catalogue"))
            let target = CatalogueExportTarget.ofFolder targetFolder
            let suggested = exportPickerFolder start
            let chosen = exportPickerFolder targetFolder
            let paths = ResizeArray<Uri>()
            let options = ResizeArray<FolderPickerOpenOptions>()
            let provider =
                LibraryPickerStorageTestProxy.create<IStorageProvider> (fun methodInfo arguments ->
                    match methodInfo.Name with
                    | "get_CanPickFolder" -> box true
                    | "TryGetFolderFromPathAsync" ->
                        paths.Add(unbox<Uri> arguments.[0])
                        box (Task.FromResult<IStorageFolder> suggested)
                    | "OpenFolderPickerAsync" ->
                        options.Add(unbox<FolderPickerOpenOptions> arguments.[0])
                        box (Task.FromResult<IReadOnlyList<IStorageFolder>> [| chosen |])
                    | _ -> invalidOp $"Unexpected catalogue picker operation: {methodInfo.Name}")
            let configured =
                {
                    context with
                        catalogueImports =
                            {
                                context.catalogueImports with
                                    exportFolderOpt = Some start
                            }
                }
            let initial = MW.initMaterialsWindow configured Browse
            let mutable current = initial
            let exportedMessages = ResizeArray<MW.MaterialsWindowMsg>()
            let window = MaterialsExportTestWindow(provider)
            try
                mountExportView window initial (fun message updated ->
                    current <- updated
                    match message with
                    | MW.ExportShownMaterials _ -> exportedMessages.Add message
                    | _ -> ())
                let categoryKey = categoryName.facetKey.uiKey
                clickOn window (ConstructorUiId.facetOfferedValue materialCategoryKey.uiKey categoryKey).value
                Assert.Equal<Set<MaterialId>>(Set.ofList [ glassId; quartzId ], current.filteredEntries |> List.map _.id |> Set.ofList)
                let before = current
                clickOn window ConstructorUiId.materialsExportShownButton.value
                Assert.Equal(MW.ExportShownMaterials target, Assert.Single exportedMessages)
                let picker = Assert.Single options
                Assert.Equal("Export to catalogue folder", picker.Title)
                Assert.False(picker.AllowMultiple)
                Assert.Same(suggested, picker.SuggestedStartLocation)
                Assert.Equal(start.value, (Assert.Single paths).LocalPath.TrimEnd(Path.DirectorySeparatorChar))
                let catalogue = (context.catalogueImports.openCatalogue targetFolder).readCatalogue () |> Result.defaultWith (fun error -> failwith error.describe)
                Assert.Equal<Set<LibraryEntryId>>(Set.ofList [ glass.id; quartz.id; category.id ], catalogue.envelopes |> List.map _.content.id |> Set.ofList)
                for envelope in catalogue.envelopes do
                    Assert.Equal<Result<LibraryEntryEnvelope option, CatalogueImportError>>(Ok (Some envelope), stores.proxy.tryLatestEnvelope envelope.content.id)
                let expected = ControlCaption $"Exported 3 entries to the catalogue folder {targetFolder.value}; their versions are now pinned."
                Assert.Equal<ControlCaption option>(Some expected, current.entryExportStatus)
                Assert.Equal<LibraryExportError option>(None, current.entryExportError)
                Assert.Equal(expected.value, textOf window ConstructorUiId.materialsExportStatusText.value)
                Assert.Equal<AppliedConstraint list>(before.appliedFacets, current.appliedFacets)
                Assert.Equal(before.selectedId, current.selectedId)
                Assert.Equal(before.removeGate, current.removeGate)
                Assert.Equal(before.lifecycleGate, current.lifecycleGate)
            finally window.Close())

    [<Fact>]
    let ``a refused Materials export shows its reason and writes nothing`` () : unit =
        HeadlessSession.run (fun () ->
            let _, context = freshExportContext ()
            let firstId = newMaterialId ()
            let secondId = newMaterialId ()
            let name = EntryName "Lab glass"
            let first = BuiltinCatalogue.userMaterialContent firstId name
            let second = BuiltinCatalogue.userMaterialContent secondId name
            BuiltinCatalogue.putSealedEntries context.catalogueImports [ first; second ]
            let folder = freshDropFolder ()
            let targetFolder = FolderName (Path.Combine(folder.value, "Refused catalogue"))
            let target = CatalogueExportTarget.ofFolder targetFolder
            let selected =
                MW.initMaterialsWindow context Browse
                |> MW.updateMaterialsWindow (MW.CommitTextFilter (TextQuery "Lab glass"))
                |> MW.updateMaterialsWindow (MW.SelectEntry firstId)
                |> MW.updateMaterialsWindow MW.RequestMarkInactive
            let before =
                {
                    selected with
                        entryExportStatus = Some (ControlCaption "Previous export")
                }
            Assert.Equal<Set<MaterialId>>(Set.ofList [ firstId; secondId ], before.filteredEntries |> List.map _.id |> Set.ofList)
            let refused = MW.updateMaterialsWindow (MW.ExportShownMaterials target) before
            let error =
                match refused.entryExportError with
                | Some (ExportCatalogueRefusedErr _ as error) -> error
                | _ -> failwith "Duplicate material filenames must refuse the catalogue export."
            Assert.Contains("Lab glass", error.describe)
            Assert.Equal<ControlCaption option>(None, refused.entryExportStatus)
            Assert.False(Directory.Exists targetFolder.value)
            Assert.Equal(before.removeGate, refused.removeGate)
            Assert.Equal(before.lifecycleGate, refused.lifecycleGate)
            Assert.Equal(before.selectedId, refused.selectedId)
            let window = HostWindow(Width = 1150.0, Height = 800.0)
            try
                mountExportView window refused (fun _ _ -> ())
                Assert.Equal(error.describe, textOf window ConstructorUiId.materialsExportStatusText.value)
                match tryFindControl window ConstructorUiId.materialsExportStatusText.value with
                | Some (:? TextBlock as status) ->
                    Assert.Equal(TextWrapping.Wrap, status.TextWrapping)
                    match status.Foreground with
                    | :? SolidColorBrush as color -> Assert.Equal(MW.messageColor, color.Color)
                    | _ -> Assert.Fail("The export refusal must use the message colour.")
                | _ -> Assert.Fail("The export refusal must be rendered.")
            finally window.Close()
            let fileLeaf = FileName "Lab glass.material-json"
            let file = fileLeaf.combine folder
            let recovered = MW.updateMaterialsWindow (MW.ExportSelectedMaterial file) refused
            Assert.True(File.Exists file.value)
            Assert.Equal<LibraryExportError option>(None, recovered.entryExportError)
            Assert.Equal<ControlCaption option>(Some (ControlCaption $"Exported Lab glass version 1 to {file.value}; that version is now pinned."), recovered.entryExportStatus)
            Assert.Equal(refused.lifecycleGate, recovered.lifecycleGate)
            Assert.Equal(refused.selectedId, recovered.selectedId)
            Assert.False(Directory.Exists targetFolder.value))

    [<Fact>]
    let ``a material the library holds is exportable, in Browse mode only`` () : unit =
        HeadlessSession.run (fun () ->
            let _, context = freshExportContext ()
            let labId = newMaterialId ()
            let labName = EntryName "Lab glass"
            let lab = BuiltinCatalogue.userMaterialContent labId labName
            BuiltinCatalogue.putSealedEntries context.catalogueImports [ lab ]
            let held =
                context.catalogueImports.proxy.tryLatestEnvelope lab.id
                |> Result.defaultWith (fun error -> failwith error.describe)
                |> Option.defaultWith (fun () -> failwith "Lab glass must be held.")
            let initial = MW.initMaterialsWindow context Browse
            let selected = MW.updateMaterialsWindow (MW.SelectEntry labId) initial
            let _, selection = selectContext Sample SampleSubstrateTarget
            let selecting = MW.initMaterialsWindow context (Select selection) |> MW.updateMaterialsWindow (MW.SelectEntry labId)
            let missingId = newMaterialId ()
            let missing = MW.updateMaterialsWindow (MW.SelectEntry missingId) initial
            let failingProxy =
                {
                    context.catalogueImports.proxy with
                        tryHeldEnvelope = fun _ _ -> Error (ImportStoreUnavailableErr "Read refused")
                }
            let failed =
                {
                    selected with
                        context =
                            {
                                context with
                                    catalogueImports =
                                        {
                                            context.catalogueImports with
                                                proxy = failingProxy
                                        }
                            }
                }
            Assert.Equal<LibraryEntryEnvelope option>(Some held, selected.exportableSelection)
            Assert.Equal<LibraryEntryEnvelope option>(None, initial.exportableSelection)
            Assert.Equal<LibraryEntryEnvelope option>(None, selecting.exportableSelection)
            Assert.Equal<LibraryEntryEnvelope option>(None, missing.exportableSelection)
            Assert.Equal<LibraryEntryEnvelope option>(None, failed.exportableSelection)
            let empty = MW.updateMaterialsWindow (MW.CommitTextFilter (TextQuery "no material has this name")) initial
            for model in [ initial; selected; selecting; missing; failed; empty ] do
                let window = HostWindow(Width = 1150.0, Height = 800.0)
                try
                    mountExportView window model (fun _ _ -> ())
                    match model.exportableSelection with
                    | Some _ ->
                        Assert.True(isPresent window ConstructorUiId.materialsExportButton.value)
                        Assert.Equal("Export…", textOf window ConstructorUiId.materialsExportButton.value)
                    | None -> Assert.False(isPresent window ConstructorUiId.materialsExportButton.value)
                    match model.mode, model.filteredEntries with
                    | Browse, _ :: _ ->
                        Assert.True(isPresent window ConstructorUiId.materialsExportShownButton.value)
                        Assert.Equal("Export shown to catalogue…", textOf window ConstructorUiId.materialsExportShownButton.value)
                    | _ -> Assert.False(isPresent window ConstructorUiId.materialsExportShownButton.value)
                finally window.Close())

    [<Fact>]
    let ``the Materials window shows a CC-BY material's citation and a CC0 material's licence name alone`` () : unit =
        HeadlessSession.run (fun () ->
            let _, context = freshExportContext ()
            let ptbId = newMaterialId ()
            let riiId = newMaterialId ()
            let labId = newMaterialId ()
            let ptbName = EntryName "PTB glass"
            let riiName = EntryName "RII glass"
            let labName = EntryName "Lab glass"
            let ccBy = EntryLicence "CC-BY-4.0"
            let cc0 = EntryLicence "CC0-1.0"
            let citation = EntryCitation "A. Author, Test optical constants (2026), doi:10.0000/test"
            let ptb = BuiltinCatalogue.licensedMaterialContent ptbId ptbName ccBy citation
            let rii = BuiltinCatalogue.licensedMaterialContent riiId riiName cc0 citation
            let lab = BuiltinCatalogue.userMaterialContent labId labName
            BuiltinCatalogue.putSealedEntries context.catalogueImports [ ptb; rii; lab ]
            let initial = MW.initMaterialsWindow context Browse
            let window = HostWindow(Width = 1150.0, Height = 800.0)
            try
                mountExportView window initial (fun _ _ -> ())
                commitFilter window "glass"
                expandEntries window
                clickOn window (ConstructorUiId.materialsEntryNode ptbId).value
                Assert.Equal("PTB glass: Licence CC-BY-4.0 — cite: A. Author, Test optical constants (2026), doi:10.0000/test",
                             textOf window ConstructorUiId.materialsAttributionPanel.value)
                match tryFindControl window ConstructorUiId.materialsAttributionPanel.value with
                | Some (:? StackPanel as panel) ->
                    Assert.Equal(Avalonia.Layout.Orientation.Vertical, panel.Orientation)
                    let line = Assert.IsType<TextBlock>(Assert.Single panel.Children)
                    Assert.Equal(TextWrapping.Wrap, line.TextWrapping)
                    Assert.Equal(380.0, line.MaxWidth)
                | _ -> Assert.Fail("Licensed materials must have an attribution panel.")
                clickOn window (ConstructorUiId.materialsEntryNode riiId).value
                let publicDomainText = textOf window ConstructorUiId.materialsAttributionPanel.value
                Assert.Equal("RII glass: Licence CC0-1.0", publicDomainText)
                Assert.DoesNotContain("cite:", publicDomainText)
                clickOn window (ConstructorUiId.materialsEntryNode labId).value
                Assert.False(isPresent window ConstructorUiId.materialsAttributionPanel.value)
            finally window.Close()
            let failedProxy =
                {
                    context.catalogueImports.proxy with
                        tryHeldEntry = fun _ -> Error (ImportStoreUnavailableErr "Attribution read refused")
                }
            let failedContext =
                {
                    context with
                        catalogueImports =
                            {
                                context.catalogueImports with
                                    proxy = failedProxy
                            }
                }
            let failed = MW.initMaterialsWindow failedContext Browse |> MW.updateMaterialsWindow (MW.SelectEntry ptbId)
            let failedWindow = HostWindow(Width = 1150.0, Height = 800.0)
            try
                mountExportView failedWindow failed (fun _ _ -> ())
                Assert.True(isPresent failedWindow ConstructorUiId.materialsViewPanel.value)
                Assert.False(isPresent failedWindow ConstructorUiId.materialsAttributionPanel.value)
            finally failedWindow.Close())

    // ============================ pure: projection ============================

    [<Fact>]
    let ``the initial projection lists every stored material as a selectable entry leaf with the live count`` () =
        let _, m = freshModel ()
        let state = MW.facetedState m
        Assert.Equal(12, state.resultCount.value)
        Assert.Equal(FacetedTreeControls.TreeMaterialized, state.materialization)
        Assert.Empty(state.breadcrumbs)
        Assert.Equal("", state.filterDraft.value)
        // The entries group leads the tree; its leaves are the whole corpus, entry-coded.
        let entries = List.head state.tree
        Assert.Equal("entries", entries.code.value)
        Assert.Equal(Some (FacetedTreeControls.FacetItemCount 12), entries.countOpt)
        Assert.Equal(12, List.length entries.children)
        for leaf in entries.children do
            Assert.StartsWith("entry:", leaf.code.value)
        // The two named representations are offered, category-first active by default.
        Assert.Equal<string list>(
            [ "by-category"; "by-physics" ],
            state.representations |> List.map (fun r -> r.code.value))
        Assert.Equal<ConstructorUiKey option>(Some (ConstructorUiKey "by-category"), state.activeRepresentationOpt)
        // Every material facet is discrete — no manual min–max box is ever offered.
        Assert.NotEmpty(state.offers)
        for group in state.offers do
            Assert.Equal(FacetedTreeControls.NoManualRange, group.manualRange)
        Assert.Contains(state.offers, fun (g : FacetedTreeControls.OfferGroup) -> g.code.value = materialCategoryKey.value)

    [<Fact>]
    let ``the committed text filter narrows the corpus and echoes as the box draft`` () =
        let _, m = freshModel ()
        let narrowed = MW.updateMaterialsWindow (MW.CommitTextFilter (TextQuery "glass")) m
        Assert.Equal(4, List.length (narrowed.filteredEntries))
        Assert.Contains(MaterialIds.glass152, filteredIds narrowed)
        Assert.DoesNotContain(MaterialIds.silicon, filteredIds narrowed)
        Assert.Equal("glass", (MW.facetedState narrowed).filterDraft.value)
        let restored = MW.updateMaterialsWindow (MW.CommitTextFilter (TextQuery "")) narrowed
        Assert.Equal(12, List.length (restored.filteredEntries))

    [<Fact>]
    let ``applying a facet constraint narrows the corpus and takes a removable breadcrumb chip with its after-count`` () =
        let _, m = freshModel ()
        let constrained = MW.updateMaterialsWindow (MW.ApplyFacetValue (materialCategoryKey, DiscreteKey "Crystal")) m
        Assert.Equal(4, List.length (constrained.filteredEntries))
        Assert.Contains(MaterialIds.langasite, filteredIds constrained)
        let chips = (MW.facetedState constrained).breadcrumbs
        Assert.Equal(1, List.length chips)
        Assert.Equal(materialCategoryKey.value, (List.head chips).code.value)
        Assert.Equal("Category: Crystal", (List.head chips).label.value)
        Assert.Equal(4, (List.head chips).afterCount.value)
        // Facet constraints AND the text filter compose: no glass is a crystal.
        let composed = MW.updateMaterialsWindow (MW.CommitTextFilter (TextQuery "glass")) constrained
        Assert.Empty(composed.filteredEntries)
        Assert.Equal(0, (MW.facetedState composed).resultCount.value)
        // Removing the chip restores (the text filter stays applied).
        let removed = MW.updateMaterialsWindow (MW.RemoveFacet materialCategoryKey) composed
        Assert.Equal(4, List.length (removed.filteredEntries))
        Assert.Empty((MW.facetedState removed).breadcrumbs)

    [<Fact>]
    let ``re-applying a constrained facet replaces its selection — one chip per facet`` () =
        let _, m = freshModel ()
        let replaced =
            m
            |> MW.updateMaterialsWindow (MW.ApplyFacetValue (materialCategoryKey, DiscreteKey "Crystal"))
            |> MW.updateMaterialsWindow (MW.ApplyFacetValue (materialCategoryKey, DiscreteKey "Glass"))
        let chips = (MW.facetedState replaced).breadcrumbs
        Assert.Equal(1, List.length chips)
        Assert.Equal("Category: Glass", (List.head chips).label.value)
        for entry in replaced.filteredEntries do
            Assert.Equal(CategoryIds.glass, entry.category)

    [<Fact>]
    let ``a category renamed through the proxy re-labels the category facet in the next projection`` () =
        let materials, _, categories = freshStores ()
        let _, context = stubContext materials categories
        let m = MW.initMaterialsWindow context Browse
        let valueKeysOf (state : FacetedTreeControls.FacetedTreeControlsState) : string list =
            state.offers
            |> List.find (fun g -> g.code.value = materialCategoryKey.value)
            |> fun g -> g.values |> List.map (fun v -> v.code.value)
        Assert.Contains("Glass", valueKeysOf (MW.facetedState m))
        // Rename through the SAME proxy the window re-queries: the very next projection
        // re-labels (the discrete value key doubles as the branch label — step 009).
        renameGlass categories "Glazing"
        let after = valueKeysOf (MW.facetedState m)
        Assert.Contains("Glazing", after)
        Assert.DoesNotContain("Glass", after)
        Assert.Equal(12, (MW.facetedState m).resultCount.value)

    [<Fact>]
    let ``facets vanish from the offers when constrained or inapplicable to the whole filtered population`` () =
        let _, m = freshModel ()
        let offerCodes (model : MW.MaterialsWindowModel) : string list =
            (MW.facetedState model).offers |> List.map (fun g -> g.code.value)
        // A constrained facet leaves the offers (its chip is the removal surface).
        let constrained = MW.updateMaterialsWindow (MW.ApplyFacetValue (materialCategoryKey, DiscreteKey "Crystal")) m
        Assert.DoesNotContain(materialCategoryKey.value, offerCodes constrained)
        // A facet inapplicable to every filtered item vanishes entirely: constant materials
        // carry no dispersive-eps segments, so the dispersion-model facet is gone — while
        // transparency (constant materials with value trees) stays offered.
        let constant = MW.updateMaterialsWindow (MW.ApplyFacetValue (materialDispersionKey, DiscreteKey "Constant")) m
        Assert.DoesNotContain(materialDispersionModelKey.value, offerCodes constant)
        Assert.Contains(materialTransparencyKey.value, offerCodes constant)

    // ============================ pure: verbs + confirm gate ============================

    [<Fact>]
    let ``Remove is confirm-gated: request arms with the id, cancel disarms, nothing is removed; no selection is inert`` () =
        let _, m = freshModel ()
        // No selection → the request is inert.
        Assert.Equal(MW.NoPendingRemove, (MW.updateMaterialsWindow MW.RequestRemoveSelected m).removeGate)
        let armed =
            m
            |> MW.updateMaterialsWindow (MW.SelectEntry MaterialIds.glass200)
            |> MW.updateMaterialsWindow MW.RequestRemoveSelected
        Assert.Equal(MW.PendingRemove MaterialIds.glass200, armed.removeGate)
        let cancelled = MW.updateMaterialsWindow MW.CancelRemove armed
        Assert.Equal(MW.NoPendingRemove, cancelled.removeGate)
        Assert.Equal(12, List.length (cancelled.filteredEntries))

    [<Fact>]
    let ``removing a REFERENCED material surfaces MaterialStillReferenced and leaves the store unchanged`` () =
        let materials, _, categories = freshStores ()
        let _, context = stubContext materials categories
        let refused =
            MW.initMaterialsWindow context Browse
            |> MW.updateMaterialsWindow (MW.SelectEntry MaterialIds.glass152)
            |> MW.updateMaterialsWindow MW.RequestRemoveSelected
            |> MW.updateMaterialsWindow MW.ConfirmRemove
        match refused.lastError with
        | Some (MaterialStillReferencedErr reason) ->
            Assert.Contains("still referenced", reason)
            // The block NAMES the referencing samples (never a cascade).
            Assert.Contains("Glass plate (n=1.52, 1 mm)", reason)
        | other -> Assert.Fail($"expected MaterialStillReferencedErr, got %A{other}")
        match materials.listMaterials ActiveOnly with
        | Ok entries -> Assert.Equal(12, List.length entries)
        | Error e -> Assert.Fail($"listMaterials failed: %A{e}")
        Assert.Contains(MaterialIds.glass152, filteredIds refused)
        Assert.Equal(Some MaterialIds.glass152, refused.selectedId)

    [<Fact>]
    let ``removing an UNREFERENCED material drops the entry from the projection in the same pass`` () =
        let materials, _, categories = freshStores ()
        let _, context = stubContext materials categories
        let removed =
            MW.initMaterialsWindow context Browse
            |> MW.updateMaterialsWindow (MW.SelectEntry MaterialIds.glass200)
            |> MW.updateMaterialsWindow MW.RequestRemoveSelected
            |> MW.updateMaterialsWindow MW.ConfirmRemove
        match removed.lastError with
        | None -> ()
        | Some e -> Assert.Fail($"expected no error, got %A{e}")
        Assert.DoesNotContain(MaterialIds.glass200, filteredIds removed)
        Assert.Equal<MaterialId option>(None, removed.selectedId)
        match materials.listMaterials ActiveOnly with
        | Ok entries -> Assert.Equal(11, List.length entries)
        | Error e -> Assert.Fail($"listMaterials failed: %A{e}")

    [<Fact>]
    let ``a query or selection change disarms a pending remove`` () =
        let armed () =
            let _, m = freshModel ()
            m |> MW.updateMaterialsWindow (MW.SelectEntry MaterialIds.glass152) |> MW.updateMaterialsWindow MW.RequestRemoveSelected
        Assert.Equal(MW.NoPendingRemove, (MW.updateMaterialsWindow (MW.CommitTextFilter (TextQuery "si")) (armed ())).removeGate)
        Assert.Equal(MW.NoPendingRemove, (MW.updateMaterialsWindow (MW.ApplyFacetValue (materialDispersionKey, DiscreteKey "Dispersive")) (armed ())).removeGate)
        Assert.Equal(MW.NoPendingRemove, (MW.updateMaterialsWindow (MW.RemoveFacet materialDispersionKey) (armed ())).removeGate)
        // A re-selection re-targets the verbs AND disarms.
        let retargeted = MW.updateMaterialsWindow (MW.SelectEntry MaterialIds.silicon) (armed ())
        Assert.Equal(Some MaterialIds.silicon, retargeted.selectedId)
        Assert.Equal(MW.NoPendingRemove, retargeted.removeGate)

    [<Fact>]
    let ``cancel leaves the inline refusal visible and the next query edit clears it`` () =
        let _, m = freshModel ()
        let refused =
            m
            |> MW.updateMaterialsWindow (MW.SelectEntry MaterialIds.glass152)
            |> MW.updateMaterialsWindow MW.RequestRemoveSelected
            |> MW.updateMaterialsWindow MW.ConfirmRemove
        match refused.lastError with
        | Some (MaterialStillReferencedErr _) -> ()
        | other -> Assert.Fail($"expected MaterialStillReferencedErr, got %A{other}")
        let cancelled = MW.updateMaterialsWindow MW.CancelRemove refused
        Assert.Equal(MW.NoPendingRemove, cancelled.removeGate)
        match cancelled.lastError with
        | Some _ -> ()
        | None -> Assert.Fail "the refusal must stay visible after a mere cancel"
        Assert.Equal(None, (MW.updateMaterialsWindow (MW.CommitTextFilter (TextQuery "si")) cancelled).lastError)

    [<Fact>]
    let ``Add mints a fresh upfront id per dispatch and reaches the launcher`` () =
        // Spec 0038 step 008: the id-mint stays off the save path — the Add verb mints the
        // entity's Guid AT the window-open dispatch, so every Add opens its own
        // registry-keyed editor (the recorded call carries the minted id).
        let calls, m = freshModel ()
        MW.updateMaterialsWindow MW.AddMaterial m |> ignore
        MW.updateMaterialsWindow MW.AddMaterial m |> ignore
        match calls |> Seq.filter (fun c -> c.StartsWith "material-add:") |> List.ofSeq with
        | [ a; b ] -> Assert.NotEqual<string>(a, b)
        | other -> Assert.Fail($"expected two material Adds, got %A{other}")

    [<Fact>]
    let ``Edit reaches the launcher only for an EDITABLE selection and Categories is a pure launch`` () =
        let calls, m = freshModel ()
        // Edit on an editable selection carries the resolved entry.
        m |> MW.updateMaterialsWindow (MW.SelectEntry MaterialIds.glass152) |> MW.updateMaterialsWindow MW.EditSelected |> ignore
        Assert.Equal<string list>([ "material-edit:Transparent glass (n = 1.52)" ], List.ofSeq calls)
        calls.Clear()
        // Silicon is view-only (its physics is not editable term data), so Edit is inert.
        m |> MW.updateMaterialsWindow (MW.SelectEntry MaterialIds.silicon) |> MW.updateMaterialsWindow MW.EditSelected |> ignore
        // No selection is inert too.
        MW.updateMaterialsWindow MW.EditSelected m |> ignore
        Assert.Empty(calls)
        // Categories… is a pure launch: the model is unchanged and the launcher records it.
        let after = MW.updateMaterialsWindow MW.OpenCategories m
        Assert.Equal<MW.MaterialsWindowModel>(m, after)
        Assert.Equal<string list>([ "categories-open" ], List.ofSeq calls)

    [<Fact>]
    let ``the view panel target follows the selection and the Edit affordance follows the step-013 complexity`` () =
        let _, m = freshModel ()
        Assert.Equal<MaterialEntry option>(None, m.selectedEntry)
        let glass = MW.updateMaterialsWindow (MW.SelectEntry MaterialIds.glass152) m
        match glass.selectedEntry with
        | Some entry -> Assert.Equal("Transparent glass (n = 1.52)", entry.name.value)
        | None -> Assert.Fail "the selected glass entry must resolve"
        match glass.editableSelection with
        | Some _ -> ()
        | None -> Assert.Fail "glass152 carries the edit model and must offer Edit"
        // Silicon is selectable (the panel shows it) but view-only: its physics is not editable term data.
        let silicon = MW.updateMaterialsWindow (MW.SelectEntry MaterialIds.silicon) m
        match silicon.selectedEntry, silicon.editableSelection with
        | Some _, None -> ()
        | other -> Assert.Fail($"expected a selectable but non-editable preset, got %A{other}")

    [<Fact>]
    let ``the tree reads alphabetically regardless of representation, while the picker still reshapes the offers`` () =
        // The faceted TREE is now sorted case-insensitively by display label at every level
        // (operator 010/Q1), OVERRIDING representation order — so choosing a representation no
        // longer reshapes the tree. The picker's effect survives in the OFFERS, which still
        // follow the chosen facet order.
        let _, m = freshModel ()
        let caseInsensitive (a : string) (b : string) : int =
            System.String.Compare(a, b, System.StringComparison.OrdinalIgnoreCase)
        let treeFacetLabels (model : MW.MaterialsWindowModel) : string list =
            (MW.facetedState model).tree |> List.tail |> List.map (fun n -> n.label.value)
        let firstOfferCode (model : MW.MaterialsWindowModel) : string =
            (MW.facetedState model).offers |> List.head |> fun g -> g.code.value
        // The tree's facet groups read alphabetically — NOT in the category-first representation
        // order (Category would otherwise lead, not Anisotropy).
        let byCategoryTree = treeFacetLabels m
        Assert.Equal<string list>(List.sortWith caseInsensitive byCategoryTree, byCategoryTree)
        Assert.Equal(materialCategoryKey.value, firstOfferCode m)
        // The physics reshuffle reorders the OFFERS (Anisotropy now leads) but leaves the tree
        // alphabetical and identical.
        let reshaped = MW.updateMaterialsWindow (MW.ChooseRepresentation (ConstructorUiKey "by-physics")) m
        Assert.Equal<ConstructorUiKey option>(Some (ConstructorUiKey "by-physics"), (MW.facetedState reshaped).activeRepresentationOpt)
        Assert.Equal(materialAnisotropyKey.value, firstOfferCode reshaped)
        Assert.Equal<string list>(byCategoryTree, treeFacetLabels reshaped)
        // Search order ≠ representation order: applying a constraint after the reshuffle keeps its
        // chip and count regardless of the active representation.
        let constrained = MW.updateMaterialsWindow (MW.ApplyFacetValue (materialCategoryKey, DiscreteKey "Crystal")) reshaped
        Assert.Equal(1, List.length (MW.facetedState constrained).breadcrumbs)
        Assert.Equal(4, (MW.facetedState constrained).resultCount.value)
        // An unknown code is inert.
        Assert.Equal<MW.MaterialsWindowModel>(constrained, MW.updateMaterialsWindow (MW.ChooseRepresentation (ConstructorUiKey "no-such")) constrained)

    [<Fact>]
    let ``the projected tree lists entry leaves and facet branches in case-insensitive alphabetical label order`` () =
        // Both the entry leaves (corpus order overridden) and every facet's branches read
        // alphabetically by label (operator 010/Q1) — the window-projection half of the acceptance.
        let _, m = freshModel ()
        let state = MW.facetedState m
        let caseInsensitive (a : string) (b : string) : int =
            System.String.Compare(a, b, System.StringComparison.OrdinalIgnoreCase)
        let isSorted (labels : string list) : bool = labels = List.sortWith caseInsensitive labels
        // BuiltinCatalogue.materials row order must not determine the tree's alphabetical order (010/Q1).
        let entries = List.head state.tree
        let leafLabels = entries.children |> List.map (fun n -> n.label.value)
        Assert.Equal(12, List.length leafLabels)
        Assert.True(isSorted leafLabels, $"entry leaves must be alphabetical: %A{leafLabels}")
        // Level 2/3 — every facet group's branches (each material facet is discrete, so its
        // branches come straight from the engine's now-alphabetical buildTree).
        for group in state.tree |> List.tail do
            let branchLabels = group.children |> List.map (fun n -> n.label.value)
            Assert.True(isSorted branchLabels, $"branches of %s{group.label.value} must be alphabetical: %A{branchLabels}")

    [<Fact>]
    let ``every facet group shows its total count and each branch expands to its member entry leaves`` () =
        // The operator gap (.manual/001-task): a facet group (e.g. Anisotropy) MUST show a total
        // count, and a branch (e.g. "Biaxial (3)") MUST expand to the 3 entries it contains — today
        // groups carried no count and branches were childless. Pure projection proof; a UI-less
        // assertion on `facetedState` would have caught both before the window ever rendered.
        let _, m = freshModel ()
        let state = MW.facetedState m
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
                    // Each branch-nested leaf is a SELECTABLE entry (its code resolves to a MaterialId).
                    match leaf.code.tryMaterialsEntryId with
                    | Some id -> distinct <- Set.add id distinct
                    | None -> Assert.Fail($"branch entry leaf %s{leaf.label.value} (%s{leaf.code.value}) must resolve to a material id")
            // 3) The group total is exactly the DISTINCT entries reachable by expanding its branches
            //    (a material a multi-valued facet lists under several branches counts once).
            Assert.Equal(Some (FacetedTreeControls.FacetItemCount (Set.count distinct)), group.countOpt)

    [<Fact>]
    let ``a single-valued facet group totals the whole population and its branch counts sum to it`` () =
        // Category is single-valued and total (every material carries exactly one), so its group
        // count is the whole corpus and its branch counts partition it — the operator's "the math
        // must add up" for a facet.
        let _, m = freshModel ()
        let state = MW.facetedState m
        let categoryGroup = state.tree |> List.find (fun n -> n.code.value = "facet:" + materialCategoryKey.value)
        Assert.Equal(Some state.resultCount, categoryGroup.countOpt)
        let branchSum = categoryGroup.children |> List.sumBy (fun b -> match b.countOpt with Some c -> c.value | None -> 0)
        Assert.Equal(state.resultCount.value, branchSum)

    [<Fact>]
    let ``a branch-nested entry leaf code selects its material, while bare branch and heading codes stay inert`` () =
        // The branch-nested leaf code `branch:<facet>:<value>:entry:<guid>` resolves to its id (so a
        // material selected THROUGH a facet branch works), yet the bare branch/heading codes remain
        // grouping-only — the selection contract after the fix. The handler carries the EXACT clicked
        // code (a `SelectEntryNode`), so the highlight can land on that node rather than the corpus copy.
        let leafCode = ConstructorUiKey ("branch:" + materialCategoryKey.value + ":Glass:" + MaterialIds.glass152.materialsEntryCode.value)
        Assert.Equal(Some MaterialIds.glass152, leafCode.tryMaterialsEntryId)
        Assert.Equal<MaterialId option>(None, (ConstructorUiKey ("facet:" + materialCategoryKey.value)).tryMaterialsEntryId)
        Assert.Equal<MaterialId option>(None, (ConstructorUiKey ("branch:" + materialCategoryKey.value + ":Glass")).tryMaterialsEntryId)
        let dispatched = ResizeArray<MW.MaterialsWindowMsg>()
        let handlers = MW.facetedHandlers dispatched.Add
        handlers.selectNode leafCode
        Assert.Equal<MW.MaterialsWindowMsg list>([ MW.SelectEntryNode (MaterialIds.glass152, leafCode) ], List.ofSeq dispatched)

    [<Fact>]
    let ``selecting a material THROUGH a facet branch highlights that branch row, not the corpus-group copy`` () =
        // The operator gap (.manual/002-followup): clicking an entry under a facet branch used to
        // highlight the matching row in the "Materials" corpus group instead of the clicked branch
        // row. The projected `selectedCode` must now be the EXACT clicked node code.
        let _, m = freshModel ()
        let leafCode = ConstructorUiKey ("branch:" + materialCategoryKey.value + ":Glass:" + MaterialIds.glass152.materialsEntryCode.value)
        let selected = MW.updateMaterialsWindow (MW.SelectEntryNode (MaterialIds.glass152, leafCode)) m
        let state = MW.facetedState selected
        // The highlight is on the clicked branch row — NOT the flat corpus-group `entry:<guid>` code.
        Assert.Equal<ConstructorUiKey option>(Some leafCode, state.selectedCodeOpt)
        Assert.NotEqual<ConstructorUiKey option>(Some (MaterialIds.glass152.materialsEntryCode), state.selectedCodeOpt)
        // The domain selection still resolves (the view panel / verbs target the material).
        Assert.Equal(Some MaterialIds.glass152, selected.selectedId)
        // The id-only path (programmatic / verb-driven) still highlights the corpus-group row.
        let viaId = MW.updateMaterialsWindow (MW.SelectEntry MaterialIds.glass152) m
        Assert.Equal<ConstructorUiKey option>(Some (MaterialIds.glass152.materialsEntryCode), (MW.facetedState viaId).selectedCodeOpt)

    [<Fact>]
    let ``the tree is collapsed by default and ToggleNode flips a node's expansion, re-projecting its children`` () =
        // Spec 0040 step 002: every top-level node opens CollapsedNode; the disclosure chevron's
        // ToggleNode records the code expanded (its children then render) and a second toggle collapses.
        let _, m = freshModel ()
        let entriesOf (model : MW.MaterialsWindowModel) : FacetedTreeControls.TreeNode = List.head (MW.facetedState model).tree
        // First open: EVERY top-level node (the entries group and every facet group) is CollapsedNode.
        let opened = MW.facetedState m
        for node in opened.tree do
            Assert.Equal(FacetedTreeControls.CollapsedNode, node.expansion)
        // The children are still PROJECTED (collapse is a render concern) — only the flag changes.
        Assert.Equal(12, List.length (entriesOf m).children)
        // Toggling "entries" records it expanded and the projected node flips to ExpandedNode.
        let expanded = MW.updateMaterialsWindow (MW.ToggleNode (ConstructorUiKey "entries")) m
        Assert.Equal(FacetedTreeControls.ExpandedNode, expanded.expandedNodes.nodeExpansion (ConstructorUiKey "entries"))
        Assert.Equal(FacetedTreeControls.ExpandedNode, (entriesOf expanded).expansion)
        Assert.Equal(12, List.length (entriesOf expanded).children)
        // A second toggle collapses it again (persisted, so it survives an unrelated re-projection).
        let collapsed = MW.updateMaterialsWindow (MW.ToggleNode (ConstructorUiKey "entries")) expanded
        Assert.Equal(FacetedTreeControls.CollapsedNode, collapsed.expandedNodes.nodeExpansion (ConstructorUiKey "entries"))
        Assert.Equal(FacetedTreeControls.CollapsedNode, (entriesOf collapsed).expansion)

    [<Fact>]
    let ``facetedState projects the selected entry's node code — empty when nothing is selected`` () =
        // Spec 0040 step 003: the control highlights the row whose code is `selectedCode`; the host
        // projects it from the Model's already-tracked selection (nothing selected → empty string).
        let _, m = freshModel ()
        Assert.Equal<ConstructorUiKey option>(None, (MW.facetedState m).selectedCodeOpt)
        let selected = MW.updateMaterialsWindow (MW.SelectEntry MaterialIds.glass152) m
        Assert.Equal<ConstructorUiKey option>(Some (MaterialIds.glass152.materialsEntryCode), (MW.facetedState selected).selectedCodeOpt)
        // A re-selection re-targets the highlight in the same projection.
        let retargeted = MW.updateMaterialsWindow (MW.SelectEntry MaterialIds.glass200) selected
        Assert.Equal<ConstructorUiKey option>(Some (MaterialIds.glass200.materialsEntryCode), (MW.facetedState retargeted).selectedCodeOpt)

    [<Fact>]
    let ``a result count above the threshold gates the tree and Show-Search materializes it`` () =
        let materials, _, categories = freshStores ()
        let _, context = stubContext materials categories
        let m = MW.initMaterialsWindow { context with treeAutoBuildThreshold = TreeAutoBuildThreshold 1 } Browse
        let gated = MW.facetedState m
        Assert.Equal(FacetedTreeControls.TreeGated, gated.materialization)
        // A gated pass projects NO tree at all — the whole point is skipping the heavy render —
        // while the live count and the offers stay up.
        Assert.Empty(gated.tree)
        Assert.Equal(12, gated.resultCount.value)
        Assert.NotEmpty(gated.offers)
        let shown = MW.facetedState (MW.updateMaterialsWindow MW.RequestTreeBuild m)
        Assert.Equal(FacetedTreeControls.TreeMaterialized, shown.materialization)
        Assert.NotEmpty(shown.tree)

    [<Fact>]
    let ``only an entry node code selects — branches and headings are grouping display`` () =
        Assert.Equal(Some MaterialIds.glass152, MaterialIds.glass152.materialsEntryCode.tryMaterialsEntryId)
        Assert.Equal<MaterialId option>(None, (ConstructorUiKey "branch:material-category:Crystal").tryMaterialsEntryId)
        Assert.Equal<MaterialId option>(None, (ConstructorUiKey "entries").tryMaterialsEntryId)
        Assert.Equal<MaterialId option>(None, (ConstructorUiKey "entry:not-a-guid").tryMaterialsEntryId)
        let dispatched = ResizeArray<MW.MaterialsWindowMsg>()
        let handlers = MW.facetedHandlers dispatched.Add
        handlers.selectNode (MaterialIds.glass152.materialsEntryCode)
        handlers.selectNode (ConstructorUiKey "branch:material-category:Crystal")
        handlers.selectNode (ConstructorUiKey "entries")
        // Only the entry-leaf click selects — carrying the EXACT clicked code (a corpus-group leaf
        // here) so the highlight lands on the clicked row; the bare branch/heading codes are inert.
        Assert.Equal<MW.MaterialsWindowMsg list>(
            [ MW.SelectEntryNode (MaterialIds.glass152, MaterialIds.glass152.materialsEntryCode) ],
            List.ofSeq dispatched)
        // The offer click lifts its tokens back to elevated engine values at the boundary.
        handlers.applyConstraint materialCategoryKey.uiKey (ConstructorUiKey "Crystal")
        Assert.Equal(MW.ApplyFacetValue (materialCategoryKey, DiscreteKey "Crystal"), dispatched.[1])

    // ============================ headless acceptance (ui-smoke) ============================

    /// Mount the REAL Main workbench MVU loop headless (the MainConstructorWindow shape with
    /// injectable stores), for the strip-button acceptance.
    let private mountMain (materials : MaterialProxy) (samples : SampleProxy) (categories : CategoryProxy) : HostWindow =
        let model0 = SceneTestFixture.mainSceneWith (BuiltinCatalogue.library ()) (Experiments.createInMemoryExperiments ()) materials samples categories
        let window = HostWindow(Width = 980.0, Height = 1050.0)
        Program.mkSimple (fun () -> model0) Scene.updateTableAndElementRotation Scene.mainView
        |> Program.withHost window
        |> Program.run
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    let private mountMaterialsWindow (materials : MaterialProxy) (categories : CategoryProxy) : MaterialsWindow =
        let window = MaterialsWindow(materials, categories, CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        // The tree opens collapsed (spec 0040 step 002); expand the entries group so the leaf-
        // clicking proofs below find their rows (expansion persists across the window's lifetime).
        expandEntries window
        window

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the strip button opens ONE Materials window and a second click ACTIVATES it`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshStores ()
            let window = mountMain materials samples categories
            Assert.True(isPresent window ConstructorUiId.openMaterialsWindowButton.value,
                        "the ribbon strip row must carry the right-aligned Materials… button")
            // Observe the windows the REAL defaults open (the WindowOpenedEvent seam the desktop
            // lifetime itself uses) — subscribed only after the Main window is shown.
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            clickOn window ConstructorUiId.openMaterialsWindowButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            let materialsWindow = opened.[0]
            Assert.True(matchesId ConstructorUiId.materialsWindow.value materialsWindow, "the opened window must be the Materials window")
            Assert.True(materialsWindow.IsVisible)
            // The single-instance acceptance: a second click ACTIVATES the live window — the
            // shared registry under MaterialsWindowKey creates nothing new.
            clickOn window ConstructorUiId.openMaterialsWindowButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            Assert.True(materialsWindow.IsVisible, "the activated window must still be the live one")
            // Close → the registry forgets the key, so the NEXT open creates afresh.
            materialsWindow.Close()
            Dispatcher.UIThread.RunJobs()
            clickOn window ConstructorUiId.openMaterialsWindowButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(2, opened.Count)
            opened.[1].Close()
            Dispatcher.UIThread.RunJobs()
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: applying a facet constraint narrows the corpus in the same render pass and the chip restores it`` () =
        HeadlessSession.run (fun () ->
            let materials, _, categories = freshStores ()
            let window = mountMaterialsWindow materials categories
            Assert.True(isPresent window ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value))
            Assert.True(isPresent window ((ConstructorUiId.materialsEntryNode MaterialIds.silicon).value))
            Assert.Equal("12 results", textOf window ConstructorUiId.facetResultCount.value)
            // Click the Dispersion facet's "Dispersive" offer: the corpus narrows to the two
            // wavelength-dependent presets IN THE SAME RENDER PASS.
            clickOn window (ConstructorUiId.facetOfferedValue (ConstructorUiKey materialDispersionKey.value) (ConstructorUiKey "Dispersive")).value
            Assert.True(isPresent window ((ConstructorUiId.materialsEntryNode MaterialIds.silicon).value),
                        "the matching entry must stay listed")
            Assert.False(isPresent window ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value),
                         "the non-matching entry must leave the tree in the same render pass")
            Assert.Equal("2 results", textOf window ConstructorUiId.facetResultCount.value)
            // The removable chip carries the after-count; clicking it restores the corpus.
            Assert.True(isPresent window (ConstructorUiId.facetBreadcrumbChip (ConstructorUiKey materialDispersionKey.value)).value)
            clickOn window (ConstructorUiId.facetBreadcrumbChip (ConstructorUiKey materialDispersionKey.value)).value
            Assert.True(isPresent window ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value))
            Assert.Equal("12 results", textOf window ConstructorUiId.facetResultCount.value)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the verbs operate over the shared app-scoped stores — Add persists through the real editor, a referenced remove is refused inline`` () =
        HeadlessSession.run (fun () ->
            // The SAME store composition the app scope performs; the window and the editors it
            // opens share these stores.
            let materials, _, categories = freshStores ()
            let window = mountMaterialsWindow materials categories
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            // Add → the REAL Material editor over the SHARED stores (through the real launcher
            // under the Add-minted MaterialEditorKey).
            clickOn window ConstructorUiId.materialsAddButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            let editor = opened.[0]
            Assert.True(matchesId ConstructorUiId.materialEditorWindow.value editor, "the opened window must be the Material editor")
            setText editor ConstructorUiId.materialNameBox.value "Faceted window material"
            clickOn editor ConstructorUiId.materialEditorSaveButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.False(editor.IsVisible, "Save must close the editor")
            // The save landed in the SHARED store…
            match materials.listMaterials ActiveOnly with
            | Ok entries -> Assert.Contains(entries, fun (e : MaterialEntry) -> e.name.value = "Faceted window material")
            | Error e -> Assert.Fail($"listMaterials failed: %A{e}")
            // …and the window's next dispatch-driven render re-queries it: committing the
            // matching filter narrows the tree to the just-saved entry.
            commitFilter window "Faceted window"
            Assert.Equal("1 results", textOf window ConstructorUiId.facetResultCount.value)
            // The referenced remove: narrow to BuiltinCatalogue.material glass, select its leaf, Remove →
            // Confirm → the typed refusal NAMES the referencing sample and the store keeps it.
            commitFilter window "1.52"
            clickOn window ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value)
            clickOn window ConstructorUiId.materialsRemoveButton.value
            clickOn window ConstructorUiId.materialsRemoveConfirmButton.value
            let message = textOf window ConstructorUiId.materialsWindowMessage.value
            Assert.Contains("still referenced", message)
            Assert.Contains("Glass plate", message)
            Assert.True(isPresent window ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value),
                        "the refused remove must leave the entry listed")
            match materials.listMaterials ActiveOnly with
            | Ok entries -> Assert.Equal(13, List.length entries)
            | Error e -> Assert.Fail($"listMaterials failed: %A{e}")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: a category renamed through the shared proxy re-labels the category facet and the create picker`` () =
        HeadlessSession.run (fun () ->
            let materials, _, categories = freshStores ()
            let window = mountMaterialsWindow materials categories
            // Before: the category facet offers "Glass" (the value key IS the label).
            Assert.True(isPresent window (ConstructorUiId.facetOfferedValue (ConstructorUiKey materialCategoryKey.value) (ConstructorUiKey "Glass")).value)
            // Rename through the shared proxy, then dispatch a MODEL-CHANGING re-render (a text
            // commit — an unchanged model is structurally equal and the Elmish host skips it).
            renameGlass categories "Glazing"
            commitFilter window "glass"
            Assert.True(isPresent window (ConstructorUiId.facetOfferedValue (ConstructorUiKey materialCategoryKey.value) (ConstructorUiKey "Glazing")).value,
                        "the category facet must re-label to the renamed catalogue name")
            Assert.False(isPresent window (ConstructorUiId.facetOfferedValue (ConstructorUiKey materialCategoryKey.value) (ConstructorUiKey "Glass")).value,
                         "the stale label must be gone")
            Assert.StartsWith("Glazing (", textOf window (ConstructorUiId.facetOfferedValue (ConstructorUiKey materialCategoryKey.value) (ConstructorUiKey "Glazing")).value)
            // The create picker re-labels too: the editor over the SAME proxy shows "Glazing"
            // for the same stable Guid id.
            let editor = MaterialEditorWindow(materials, MaterialEditorView.NewMaterial (newMaterialId ()), categories = categories)
            editor.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.Equal("Glazing", labelInside editor (ConstructorUiId.materialEditorCategoryOption CategoryIds.glass).value)
            editor.Close()
            Dispatcher.UIThread.RunJobs()
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless: Edit opens the Material editor on the selected entry and Categories opens the Category editor`` () =
        HeadlessSession.run (fun () ->
            let materials, _, categories = freshStores ()
            let window = mountMaterialsWindow materials categories
            commitFilter window "1.52"
            clickOn window ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value)
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            clickOn window ConstructorUiId.materialsEditButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            Assert.True(matchesId ConstructorUiId.materialEditorWindow.value opened.[0], "the opened window must be the Material editor")
            Assert.Contains("Transparent glass", opened.[0].Title)
            opened.[0].Close()
            Dispatcher.UIThread.RunJobs()
            clickOn window ConstructorUiId.materialsCategoriesButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(2, opened.Count)
            Assert.True(matchesId ConstructorUiId.categoryEditorWindow.value opened.[1], "the opened window must be the Category editor")
            opened.[1].Close()
            Dispatcher.UIThread.RunJobs()
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless: selecting an entry leaf shows the view panel with metadata and the embedded n-k chart`` () =
        HeadlessSession.run (fun () ->
            let materials, _, categories = freshStores ()
            let window = mountMaterialsWindow materials categories
            Assert.False(isPresent window ConstructorUiId.materialsViewPanel.value, "no selection → no view panel")
            commitFilter window "1.52"
            clickOn window ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value)
            Assert.True(isPresent window ConstructorUiId.materialsViewPanel.value, "the view panel must render for the selected entry")
            Assert.True(isPresent window ConstructorUiId.materialsViewPanelNkChart.value, "the view panel must embed the shared n/k chart host")
            Assert.Contains("Transparent glass", textOf window ConstructorUiId.materialsViewPanel.value)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless: a gated tree shows Show-Search with ZERO rows and the click materializes the entry leaves`` () =
        HeadlessSession.run (fun () ->
            let materials, _, categories = freshStores ()
            let _, context = stubContext materials categories
            // Mount the REAL MVU loop over a tiny threshold (the window minus its launcher
            // composition — the FacetedTreeControlsTests mounting precedent).
            let window = HostWindow(Width = 900.0, Height = 760.0)
            Program.mkSimple
                (fun () -> MW.initMaterialsWindow { context with treeAutoBuildThreshold = TreeAutoBuildThreshold 1 } Browse)
                MW.updateMaterialsWindow
                MW.materialsWindowView
            |> Program.withHost window
            |> Program.run
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(isPresent window ConstructorUiId.facetShowTreeButton.value, "the Show/Search button must gate the tree")
            Assert.Equal(0, treeRowCount window)
            Assert.Equal("12 results", textOf window ConstructorUiId.facetResultCount.value)
            clickOn window ConstructorUiId.facetShowTreeButton.value
            Assert.True(treeRowCount window > 0, "the explicit build must materialize the tree")
            // The materialized tree opens collapsed (spec 0040 step 002) — expand entries to reach
            // the leaves.
            expandEntries window
            Assert.True(isPresent window ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value))
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (002): the tree opens collapsed and the entries chevron toggles its leaves in the same render pass`` () =
        HeadlessSession.run (fun () ->
            let materials, _, categories = freshStores ()
            let _, context = stubContext materials categories
            // Mount the REAL MVU loop directly (NOT the auto-expanding helper) so the first render
            // is the collapsed default.
            let window = HostWindow(Width = 900.0, Height = 760.0)
            Program.mkSimple (fun () -> MW.initMaterialsWindow context Browse) MW.updateMaterialsWindow MW.materialsWindowView
            |> Program.withHost window
            |> Program.run
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // First open: the entries group renders a collapsed row WITH a chevron, but its leaves
            // do NOT render.
            Assert.True(isPresent window (ConstructorUiId.facetTreeNode (ConstructorUiKey "entries")).value, "the entries group row must render")
            Assert.True(isPresent window (ConstructorUiId.facetTreeChevron (ConstructorUiKey "entries")).value, "the entries group must carry a disclosure chevron")
            Assert.False(isPresent window ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value), "a collapsed tree must hide its entry leaves on first open")
            // Clicking the chevron expands the group — the leaves render in the same pass.
            expandEntries window
            Assert.True(isPresent window ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value), "the chevron click must reveal the entry leaves")
            // A second click collapses it again.
            expandEntries window
            Assert.False(isPresent window ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value), "a second chevron click must collapse the group")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (003): selecting an entry leaf highlights exactly that row with the chosen fill and a thicker border`` () =
        HeadlessSession.run (fun () ->
            let materials, _, categories = freshStores ()
            let window = mountMaterialsWindow materials categories
            // Narrow to the glass presets so both leaves render near the top and stay clickable.
            commitFilter window "glass"
            // Before any selection every leaf is idle (no row carries the chosen fill).
            Assert.Equal(Some idleFill, backgroundColorOf (labelBorderOf window ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value)))
            clickOn window ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value)
            let selected = labelBorderOf window ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value)
            let sibling = labelBorderOf window ((ConstructorUiId.materialsEntryNode MaterialIds.glass200).value)
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
        Assert.Equal("MaterialsSelectButton", ConstructorUiId.materialsSelectButton.value)
        Assert.Equal("MaterialsSelectCloseButton", ConstructorUiId.materialsSelectCloseButton.value)
        Assert.Equal("MaterialsSelectConstraint", ConstructorUiId.materialsSelectConstraint.value)

    /// A fresh SELECT-state model over recording stubs (targeting one sample-layer slot —
    /// the step-019 picking shape).
    let private freshSelectModel () : ResizeArray<string> * ResizeArray<string> * MW.MaterialsWindowModel =
        let materials, _, categories = freshStores ()
        let calls, context = stubContext materials categories
        let events, selectCtx = selectContext Sample (SampleLayerTarget (AtSingleLayer (FilmItemIndex 0)))
        events, calls, MW.initMaterialsWindow context (Select selectCtx)

    [<Fact>]
    let ``Select state keeps the WHOLE material corpus — the kind constraint is structural, no breadcrumb chip`` () =
        // Every material serves a sample-layer pick (materials carry no CatalogueKind), so the
        // pre-applied constraint narrows nothing here — it shows as the fixed banner only —
        // and it takes NO chip (nothing to remove).
        let _, _, m = freshSelectModel ()
        Assert.Equal(12, List.length (m.filteredEntries))
        let state = MW.facetedState m
        Assert.Equal(12, state.resultCount.value)
        Assert.Empty(state.breadcrumbs)

    [<Fact>]
    let ``ConfirmSelect returns the HIGHLIGHTED material through onSelected and closes — no highlight is inert`` () =
        let events, calls, m = freshSelectModel ()
        Assert.Equal<MW.MaterialsWindowModel>(m, MW.updateMaterialsWindow MW.ConfirmSelect m)
        Assert.Empty(events)
        let resolved =
            m
            |> MW.updateMaterialsWindow (MW.SelectEntry MaterialIds.glass152)
            |> MW.updateMaterialsWindow MW.ConfirmSelect
        Assert.Equal<string list>([ $"selected:{MaterialIds.glass152.value}" ], List.ofSeq events)
        Assert.Contains("close-requested", calls)
        Assert.Equal<LibraryWindowMode<MaterialEntry>>(Browse, resolved.mode)
        // A dismissal after the resolve is a no-op — onSelected and onCancelled never both fire.
        MW.updateMaterialsWindow MW.SelectDismissed resolved |> ignore
        Assert.Equal<string list>([ $"selected:{MaterialIds.glass152.value}" ], List.ofSeq events)
        // Browse-mode Confirm/Cancel are inert (the pair does not exist there).
        let _, browse = freshModel ()
        Assert.Equal<MW.MaterialsWindowModel>(browse, MW.updateMaterialsWindow MW.ConfirmSelect browse)
        Assert.Equal<MW.MaterialsWindowModel>(browse, MW.updateMaterialsWindow MW.CancelSelect browse)

    [<Fact>]
    let ``CancelSelect cancels once and a re-target supersedes the first session`` () =
        let events, calls, m = freshSelectModel ()
        let cancelled = MW.updateMaterialsWindow MW.CancelSelect m
        Assert.Equal<string list>([ "cancelled" ], List.ofSeq events)
        Assert.Contains("close-requested", calls)
        MW.updateMaterialsWindow MW.SelectDismissed cancelled |> ignore
        Assert.Equal<string list>([ "cancelled" ], List.ofSeq events)
        // Re-target: the superseded session cancels, the highlight clears, the new session
        // resolves through the NEW context (a second Choose closes the first, logically).
        let events1, _, m1 = freshSelectModel ()
        let highlighted = MW.updateMaterialsWindow (MW.SelectEntry MaterialIds.glass152) m1
        let events2, retargetCtx = selectContext Sample (SampleLayerTarget (AtSingleLayer (FilmItemIndex 1)))
        let retargeted = MW.updateMaterialsWindow (MW.RetargetSelect retargetCtx) highlighted
        Assert.Equal<string list>([ "cancelled" ], List.ofSeq events1)
        Assert.Empty(events2)
        Assert.Equal<MaterialId option>(None, retargeted.selectedId)
        retargeted
        |> MW.updateMaterialsWindow (MW.SelectEntry MaterialIds.glass200)
        |> MW.updateMaterialsWindow MW.ConfirmSelect
        |> ignore
        Assert.Equal<string list>([ $"selected:{MaterialIds.glass200.value}" ], List.ofSeq events2)

    // ============================ step 016 — Select mode (headless) ============================

    let private mountSelectMaterialsWindow (materials : MaterialProxy) (categories : CategoryProxy) (selectCtx : SelectionContext<MaterialEntry>) : MaterialsWindow =
        let window = MaterialsWindow(materials, categories, CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy, mode = Select selectCtx)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        // The tree opens collapsed (spec 0040 step 002); expand entries so the Select proofs reach
        // their leaves (expansion persists across the window's lifetime).
        expandEntries window
        window

    /// Mount the REAL Sample-editor MVU loop headless WITH a captured dispatch, so a Materials
    /// Select window's onSelected can dispatch the TARGETED `BindMaterialToLayer` into the live
    /// editor loop (the step-019 picking shape, driven end-to-end here).
    let private mountEditorWithDispatch (materials : MaterialProxy) (samples : SampleProxy) (intent : SampleEditorView.SampleEditorIntent) : HostWindow * (SampleEditorView.SampleEditorMsg -> unit) =
        let materialList =
            match materials.listMaterials ActiveOnly with
            | Ok entries -> entries
            | Error e -> failwith $"listMaterials failed: %A{e}"
        let context : SampleEditorView.SampleEditorContext =
            {
                materials = materials
                samples = samples
                openMaterialsSelect = fun _ _ -> ()
                requestClose = fun () -> ()
            }
        let window = HostWindow(Width = 1100.0, Height = 760.0)
        let mutable dispatchRef : SampleEditorView.SampleEditorMsg -> unit = ignore
        Program.mkProgram
            (fun () -> SampleEditorView.initSampleEditor context materialList intent, Cmd.ofEffect (fun d -> dispatchRef <- d))
            (fun msg m -> SampleEditorView.updateSampleEditor msg m, Cmd.none)
            SampleEditorView.sampleEditorView
        |> Program.withHost window
        |> Program.run
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window, (fun msg -> dispatchRef msg)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (016): the Materials Select state shows the Select-Close pair and the non-removable constraint — the ordinary window otherwise`` () =
        HeadlessSession.run (fun () ->
            let materials, _, categories = freshStores ()
            let _, selectCtx = selectContext Sample (SampleLayerTarget (AtSingleLayer (FilmItemIndex 0)))
            let window = mountSelectMaterialsWindow materials categories selectCtx
            Assert.True(isPresent window ConstructorUiId.materialsSelectButton.value, "the Select button must render")
            Assert.True(isPresent window ConstructorUiId.materialsSelectCloseButton.value, "the Close button must render")
            Assert.Equal("Select", textOf window ConstructorUiId.materialsSelectButton.value)
            Assert.Equal("Close", textOf window ConstructorUiId.materialsSelectCloseButton.value)
            let banner = textOf window ConstructorUiId.materialsSelectConstraint.value
            Assert.Contains("Sample", banner)
            Assert.Contains("fixed", banner)
            // No chip — the constraint is structural, and the whole material corpus stays.
            Assert.Equal("12 results", textOf window ConstructorUiId.facetResultCount.value)
            let chipCount =
                window.GetVisualDescendants()
                |> Seq.filter (fun v ->
                    match v with
                    | :? Control as c ->
                        let autoId = Avalonia.Automation.AutomationProperties.GetAutomationId(c)
                        not (isNull autoId) && autoId.StartsWith("FacetBreadcrumbChip_")
                    | _ -> false)
                |> Seq.length
            Assert.Equal(0, chipCount)
            // Everything else IS the ordinary window: Add / Categories… (add-on-the-fly) stay.
            Assert.True(isPresent window ConstructorUiId.materialsAddButton.value, "Add must survive Select state")
            Assert.True(isPresent window ConstructorUiId.materialsCategoriesButton.value, "Categories… must survive Select state")
            window.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (016): Select returns the material through the TARGETED dispatch into the sample editor's layer and closes`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshStores ()
            // BuiltinCatalogue.sample supplies one layer — the Select session targets ITS row 0.
            let editor, dispatch = mountEditorWithDispatch materials samples (SampleEditorView.EditSample (BuiltinCatalogue.sample SampleIds.glassFilm600Id))
            let cancels = ResizeArray<string>()
            let selectCtx : SelectionContext<MaterialEntry> =
                {
                    kindConstraint = KindConstraint Sample
                    currentEntryIdOpt = None
                    target = SampleLayerTarget (AtSingleLayer (FilmItemIndex 0))
                    onSelected = fun entry -> dispatch (SampleEditorView.BindMaterialToLayer (AtSingleLayer (FilmItemIndex 0), entry.id))
                    onCancelled = fun () -> cancels.Add "cancelled"
                }
            let selectWindow = mountSelectMaterialsWindow materials categories selectCtx
            commitFilter selectWindow "1.52"
            clickOn selectWindow ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value)
            clickOn selectWindow ConstructorUiId.materialsSelectButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.False(selectWindow.IsVisible, "Select must close the window after onSelected")
            Assert.Empty(cancels)
            // The TARGETED return re-materialed row 0 in the same pass.
            Assert.Contains("Transparent glass (n = 1.52)", textOf editor (ConstructorUiId.sampleLayerRow (ConstructorUiPosition 0)).value)
            Assert.Equal("", textOf editor ConstructorUiId.sampleEditorStatus.value)
            editor.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (016): a changed table selection cancels and CLOSES the open Select-state Materials window too`` () =
        HeadlessSession.run (fun () ->
            let materials, _, categories = freshStores ()
            let events, selectCtx = selectContext Sample (SampleLayerTarget (AtSingleLayer (FilmItemIndex 0)))
            let selectWindow = mountSelectMaterialsWindow materials categories selectCtx
            // The workbench staleness machinery is window-agnostic: the session handle closes
            // whichever Select-state window it points at.
            let sceneSamples = BuiltinCatalogue.sampleStore ()
            let sceneMaterials = BuiltinCatalogue.materialStore (samplesReferencing sceneSamples)
            let sceneCategories = BuiltinCatalogue.categoryStore (materialsReferencingCategory sceneMaterials)
            let model0 =
                SceneTestFixture.mainSceneWith (BuiltinCatalogue.library ()) (Experiments.createInMemoryExperiments ()) sceneMaterials sceneSamples sceneCategories
            let session : Scene.SelectSession =
                {
                    target = ElementId "det"
                    cancelAndClose = fun () -> selectWindow.Close()
                }
            let mainWindow = HostWindow(Width = 980.0, Height = 1050.0)
            Program.mkSimple
                (fun () -> { model0 with selection = Scene.ElementSelected (Scene.SceneElementIndex 1); activeSelect = Some session })
                Scene.updateTableAndElementRotation
                Scene.mainView
            |> Program.withHost mainWindow
            |> Program.run
            mainWindow.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(selectWindow.IsVisible)
            // A REAL canvas click on the empty table changes the selection: the staleness rule
            // closes the Materials Select window and its session cancels — exactly once.
            match tryFindControl mainWindow ConstructorUiId.tableElementCanvas.value with
            | Some canvas ->
                let p = canvas.TranslatePoint(Point(TableScene.center.sx, TableScene.center.sy), mainWindow)
                Assert.True(p.HasValue, "the canvas must have an on-screen position")
                mainWindow.MouseDown(p.Value, MouseButton.Left, RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                mainWindow.MouseUp(p.Value, MouseButton.Left, RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
            | None -> Assert.Fail "the table canvas was not found"
            Assert.False(selectWindow.IsVisible, "the changed table selection must close the Materials Select window")
            Assert.Equal<string list>([ "cancelled" ], List.ofSeq events)
            mainWindow.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (016): a vanished target layer makes the Select return a NO-OP plus the editor status line — never a throw`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshStores ()
            // A BLANK new sample: it has NO row 0, so the session's target does not exist —
            // the deleted-row race the targeted return must survive (step 019 wires the verb).
            let editor, dispatch = mountEditorWithDispatch materials samples (SampleEditorView.NewBlankSample (newSampleId ()))
            let selectCtx : SelectionContext<MaterialEntry> =
                {
                    kindConstraint = KindConstraint Sample
                    currentEntryIdOpt = None
                    target = SampleLayerTarget (AtSingleLayer (FilmItemIndex 0))
                    onSelected = fun entry -> dispatch (SampleEditorView.BindMaterialToLayer (AtSingleLayer (FilmItemIndex 0), entry.id))
                    onCancelled = fun () -> ()
                }
            let selectWindow = mountSelectMaterialsWindow materials categories selectCtx
            commitFilter selectWindow "1.52"
            clickOn selectWindow ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value)
            clickOn selectWindow ConstructorUiId.materialsSelectButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.False(selectWindow.IsVisible, "the Select window still closes after its return")
            // The editor no-opped and reports the vanished row on its status line.
            Assert.Contains("no longer in the stack", textOf editor ConstructorUiId.sampleEditorStatus.value)
            Assert.False(isPresent editor (ConstructorUiId.sampleLayerRow (ConstructorUiPosition 0)).value, "no layer row may appear from a vanished-target return")
            editor.Close()
            Dispatcher.UIThread.RunJobs())

    // ============================ step 023 — lifecycle (pure) ============================

    /// A material an editable template (glass152) supplies two DISTINCT versions of, under ONE id —
    /// the multi-version history the live store never grows until step 25, proven here through a
    /// stub proxy: `resolveVersion` serves v1 and v2, `tryGetMaterial` / `listMaterials` the latest.
    let private twoVersionMaterials () : MaterialId * MaterialEntry * MaterialEntry * MaterialProxy =
        let template = BuiltinCatalogue.material MaterialIds.glass152
        let id = newMaterialId ()
        let v1 = { template with id = id; name = EntryName "Versioned material v1" }
        let v2 = { template with id = id; name = EntryName "Versioned material v2" }
        let proxy : MaterialProxy =
            {
                listMaterials = fun _ -> Ok [ v2 ]
                searchMaterials = fun _ -> Ok [ v2 ]
                tryGetMaterial = fun mid -> Ok (if mid = id then Some v2 else None)
                resolveVersion =
                    fun mvid ->
                        if mvid.materialId = id then
                            match mvid.version.value with
                            | 1 -> Ok (Some v1)
                            | 2 -> Ok (Some v2)
                            | _ -> Ok None
                        else Ok None
                saveMaterial = fun _ -> Ok ()
                markMaterialInactive = fun _ -> Ok ()
                markMaterialActive = fun _ -> Ok ()
                supersedeMaterial = fun _ -> Ok ()
                pinMaterialVersion = fun _ -> Ok ()
                removeMaterial = fun _ -> Ok ()
            }
        id, v1, v2, proxy

    [<Fact>]
    let ``the step-023 lifecycle UiIds are the stable intent-named ids`` () =
        Assert.Equal("MaterialsShowInactiveToggle", ConstructorUiId.materialsShowInactiveToggle.value)
        Assert.Equal("MaterialsMarkInactiveButton", ConstructorUiId.materialsMarkInactiveButton.value)
        Assert.Equal("MaterialsMarkActiveButton", ConstructorUiId.materialsMarkActiveButton.value)
        Assert.Equal("MaterialsSupersedeButton", ConstructorUiId.materialsSupersedeButton.value)
        Assert.Equal("MaterialsLifecycleConfirmButton", ConstructorUiId.materialsLifecycleConfirmButton.value)
        Assert.Equal("MaterialsLifecycleCancelButton", ConstructorUiId.materialsLifecycleCancelButton.value)
        Assert.Equal("MaterialsVersionsPanel", ConstructorUiId.materialsVersionsPanel.value)
        Assert.Equal("MaterialsViewOnlyNote", ConstructorUiId.materialsViewOnlyNote.value)
        Assert.Equal("MaterialsVersionRow_2", (ConstructorUiId.materialsVersionRow (VersionNumber 2)).value)

    [<Fact>]
    let ``the show-inactive toggle scopes the corpus, badges retired entries, and keeps references resolving`` () =
        let materials, _, categories = freshStores ()
        let _, context = stubContext materials categories
        // Mark an UNREFERENCED material (glass200) inactive through the window's own verbs.
        let retired =
            MW.initMaterialsWindow context Browse
            |> MW.updateMaterialsWindow (MW.SelectEntry MaterialIds.glass200)
            |> MW.updateMaterialsWindow MW.RequestMarkInactive
            |> MW.updateMaterialsWindow MW.ConfirmLifecycle
        Assert.Equal(MW.NoPendingLifecycle, retired.lifecycleGate)
        // Default scope EXCLUDES it (pickers / facet counts), and the badge count is 1.
        Assert.DoesNotContain(MaterialIds.glass200, filteredIds retired)
        Assert.Equal(11, (MW.facetedState retired).resultCount.value)
        Assert.Equal(1, retired.inactiveCount.value)
        // The reference still resolves IGNORING lifecycle (the table keeps drawing it).
        match materials.resolveVersion (MaterialVersionId.firstOf MaterialIds.glass200) with
        | Ok (Some entry) -> Assert.Equal(MaterialIds.glass200, entry.id)
        | other -> Assert.Fail($"the retired material's version must still resolve, got %A{other}")
        // The toggle reveals it, badged, in the tree.
        let shown = MW.updateMaterialsWindow MW.ToggleShowInactive retired
        Assert.Equal(IncludeInactive, shown.showInactive)
        Assert.Contains(MaterialIds.glass200, filteredIds shown)
        Assert.Equal(12, (MW.facetedState shown).resultCount.value)
        let leaf =
            (MW.facetedState shown).tree
            |> List.head
            |> fun entries -> entries.children |> List.find (fun n -> n.code = MaterialIds.glass200.materialsEntryCode)
        Assert.Contains("inactive", leaf.label.value)
        // A second toggle hides them again.
        Assert.Equal(ActiveOnly, (MW.updateMaterialsWindow MW.ToggleShowInactive shown).showInactive)

    [<Fact>]
    let ``lifecycle verbs follow the selection's live-retired state, are confirm-gated, and a vanished id surfaces a typed refusal`` () =
        let materials, _, categories = freshStores ()
        let _, context = stubContext materials categories
        let m = MW.initMaterialsWindow context Browse
        // No selection → no lifecycle verbs.
        Assert.Empty(m.offeredLifecycleActions)
        // An ACTIVE selection offers Mark inactive + Supersede…
        let selected = MW.updateMaterialsWindow (MW.SelectEntry MaterialIds.glass200) m
        Assert.Equal<MW.LifecycleAction list>([ MW.MarkInactiveAction; MW.SupersedeAction ], selected.offeredLifecycleActions)
        // Confirm-gated: Request arms with the id, a query change disarms.
        let armed = MW.updateMaterialsWindow MW.RequestMarkInactive selected
        Assert.Equal(MW.PendingLifecycle (MaterialIds.glass200, MW.MarkInactiveAction), armed.lifecycleGate)
        Assert.Equal(MW.NoPendingLifecycle, (MW.updateMaterialsWindow (MW.CommitTextFilter (TextQuery "x")) armed).lifecycleGate)
        // Confirm retires it; the SAME selection now offers Mark active alone.
        let retired = MW.updateMaterialsWindow MW.ConfirmLifecycle armed
        Assert.Equal<MW.LifecycleAction list>([ MW.MarkActiveAction ], retired.offeredLifecycleActions)
        // Mark active revives it (back in the active listing).
        let revived = retired |> MW.updateMaterialsWindow MW.RequestMarkActive |> MW.updateMaterialsWindow MW.ConfirmLifecycle
        Assert.Contains(MaterialIds.glass200, filteredIds revived)
        Assert.Equal<MW.LifecycleAction list>([ MW.MarkInactiveAction; MW.SupersedeAction ], revived.offeredLifecycleActions)
        // A vanished id: arm, remove the material behind the window's back, then confirm → the
        // typed store refusal surfaces inline and nothing is silently swallowed.
        let armedAgain = revived |> MW.updateMaterialsWindow MW.RequestSupersede
        match materials.removeMaterial MaterialIds.glass200 with
        | Ok () -> ()
        | Error e -> Assert.Fail($"the out-of-band remove must succeed, got %A{e}")
        let refused = MW.updateMaterialsWindow MW.ConfirmLifecycle armedAgain
        match refused.lastError with
        | Some (UnknownMaterialIdErr reason) -> Assert.Contains("unknown material id", reason)
        | other -> Assert.Fail($"expected UnknownMaterialIdErr, got %A{other}")

    [<Fact>]
    let ``supersede retires the latest version exactly like mark-inactive`` () =
        let materials, _, categories = freshStores ()
        let _, context = stubContext materials categories
        let superseded =
            MW.initMaterialsWindow context Browse
            |> MW.updateMaterialsWindow (MW.SelectEntry MaterialIds.glass200)
            |> MW.updateMaterialsWindow MW.RequestSupersede
            |> MW.updateMaterialsWindow MW.ConfirmLifecycle
        Assert.DoesNotContain(MaterialIds.glass200, filteredIds superseded)
        // Still resolvable by version (superseded behaves as inactive — steps 021/022).
        match materials.resolveVersion (MaterialVersionId.firstOf MaterialIds.glass200) with
        | Ok (Some _) -> ()
        | other -> Assert.Fail($"a superseded version must still resolve, got %A{other}")

    [<Fact>]
    let ``marking a material inactive removes it from a Select-mode corpus while its reference still resolves`` () =
        let materials, _, categories = freshStores ()
        // Retire glass200 through a Browse window over the SHARED store…
        let _, browseCtx = stubContext materials categories
        MW.initMaterialsWindow browseCtx Browse
        |> MW.updateMaterialsWindow (MW.SelectEntry MaterialIds.glass200)
        |> MW.updateMaterialsWindow MW.RequestMarkInactive
        |> MW.updateMaterialsWindow MW.ConfirmLifecycle
        |> ignore
        // …a Select-state window over the SAME store no longer offers it (Select forces ActiveOnly).
        let _, selectCtx = stubContext materials categories
        let events, selection = selectContext Sample (SampleLayerTarget (AtSingleLayer (FilmItemIndex 0)))
        let selectModel = MW.initMaterialsWindow selectCtx (Select selection)
        ignore events
        Assert.DoesNotContain(MaterialIds.glass200, filteredIds selectModel)
        Assert.Equal(11, (MW.facetedState selectModel).resultCount.value)
        // The bound reference still resolves — a layer already pinning it keeps drawing.
        match materials.resolveVersion (MaterialVersionId.firstOf MaterialIds.glass200) with
        | Ok (Some _) -> ()
        | other -> Assert.Fail($"the bound reference must still resolve, got %A{other}")

    [<Fact>]
    let ``the view panel enumerates versions and an older version is view-only while Edit targets the latest`` () =
        let id, _, v2, materials = twoVersionMaterials ()
        let categories = BuiltinCatalogue.categoryStore (materialsReferencingCategory materials)
        let _, context = stubContext materials categories
        let selected = MW.initMaterialsWindow context Browse |> MW.updateMaterialsWindow (MW.SelectEntry id)
        // Both versions are enumerated (ascending), and the default view is the LATEST.
        Assert.Equal<int list>([ 1; 2 ], selected.selectedVersions |> List.map (fun (v, _) -> v.value))
        Assert.Equal<VersionNumber option>(None, selected.viewedVersion)
        // The Edit affordance targets the LATEST editable entry (v2).
        match selected.editableSelection with
        | Some entry -> Assert.Equal(v2.name, entry.name)
        | None -> Assert.Fail "the latest version must remain editable"
        // Viewing an OLDER version records it; the latest is not "older".
        let older = MW.updateMaterialsWindow (MW.ViewVersion (VersionNumber 1)) selected
        Assert.Equal<VersionNumber option>(Some (VersionNumber 1), older.viewedVersion)
        // A new selection resets the view back to the latest.
        Assert.Equal<VersionNumber option>(None, (MW.updateMaterialsWindow (MW.SelectEntry id) older).viewedVersion)

    // ============================ step 023 — lifecycle (headless) ============================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (023): marking a material inactive hides it, the toggle reveals it badged, and its reference still resolves`` () =
        HeadlessSession.run (fun () ->
            let materials, _, categories = freshStores ()
            let window = mountMaterialsWindow materials categories
            // Select the unreferenced glass200 and mark it inactive through the confirm gate.
            commitFilter window "2.00"
            clickOn window ((ConstructorUiId.materialsEntryNode MaterialIds.glass200).value)
            clickOn window ConstructorUiId.materialsMarkInactiveButton.value
            clickOn window ConstructorUiId.materialsLifecycleConfirmButton.value
            // Its row is gone from the default (ActiveOnly) tree…
            Assert.False(isPresent window ((ConstructorUiId.materialsEntryNode MaterialIds.glass200).value),
                         "the retired material must leave the default tree in the same render pass")
            // …but its version still resolves IGNORING lifecycle (the table keeps drawing it).
            match materials.resolveVersion (MaterialVersionId.firstOf MaterialIds.glass200) with
            | Ok (Some _) -> ()
            | other -> Assert.Fail($"the retired material's version must still resolve, got %A{other}")
            // The show-inactive toggle carries the count badge and reveals the entry, badged.
            Assert.Contains("(1)", textOf window ConstructorUiId.materialsShowInactiveToggle.value)
            clickOn window ConstructorUiId.materialsShowInactiveToggle.value
            Assert.True(isPresent window ((ConstructorUiId.materialsEntryNode MaterialIds.glass200).value),
                        "the toggle must reveal the retired entry in the tree")
            Assert.Contains("inactive", textOf window ((ConstructorUiId.materialsEntryNode MaterialIds.glass200).value))
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (023): an older material version opens VIEW-ONLY inline while the latest stays editable`` () =
        HeadlessSession.run (fun () ->
            let id, _, _, materials = twoVersionMaterials ()
            let categories = BuiltinCatalogue.categoryStore (materialsReferencingCategory materials)
            let _, context = stubContext materials categories
            // Mount the REAL MVU loop (the window minus its launcher composition — the gated-tree
            // mounting precedent) so the version-row clicks drive the pure update.
            let window = HostWindow(Width = 900.0, Height = 760.0)
            Program.mkSimple (fun () -> MW.initMaterialsWindow context Browse) MW.updateMaterialsWindow MW.materialsWindowView
            |> Program.withHost window
            |> Program.run
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // The tree opens collapsed (spec 0040 step 002) — expand entries to reach the leaf.
            expandEntries window
            clickOn window ((ConstructorUiId.materialsEntryNode id).value)
            // The version list renders both versions; the latest view carries NO view-only note.
            Assert.True(isPresent window ConstructorUiId.materialsVersionsPanel.value, "the view panel must list the entry's versions")
            Assert.True(isPresent window ((ConstructorUiId.materialsVersionRow (VersionNumber 1)).value))
            Assert.True(isPresent window ((ConstructorUiId.materialsVersionRow (VersionNumber 2)).value))
            Assert.False(isPresent window ConstructorUiId.materialsViewOnlyNote.value, "the latest version is editable — no view-only note")
            Assert.True(isPresent window ConstructorUiId.materialsEditButton.value, "the latest version keeps the Edit verb")
            // Clicking the OLDER version opens it view-only inline (no Save path).
            clickOn window ((ConstructorUiId.materialsVersionRow (VersionNumber 1)).value)
            Assert.True(isPresent window ConstructorUiId.materialsViewOnlyNote.value, "an older version must render the view-only note")
            Assert.Contains("view-only", textOf window ConstructorUiId.materialsViewOnlyNote.value)
            // The library still edits the LATEST — the Edit verb is unchanged by viewing an older version.
            Assert.True(isPresent window ConstructorUiId.materialsEditButton.value, "the Edit verb still targets the latest version")
            window.Close())

    [<Fact>]
    let ``dropping entry files on the Materials window decides them and opens the import dialog, applying nothing`` () : unit =
        let file, material = writeDroppedMaterial ()
        let materials, _, categories = freshStores ()
        let calls, context = stubContext materials categories
        context.catalogueImports.importFrom (CatalogueProxy.createBuiltIn ())
        |> Result.defaultWith (fun error -> failwith error.describe)
        |> List.map _.byDefault
        |> context.catalogueImports.apply
        |> Result.defaultWith (fun error -> failwith error.describe)
        |> ignore
        let opened = ResizeArray<ImportDecision list>()
        let context =
            {
                context with
                    openImportDialog = opened.Add
            }
        let initial = MW.initMaterialsWindow context Browse
        Assert.Equal<ControlCaption option>(None, initial.importStatus)
        Assert.Equal<CatalogueImportError option>(None, initial.importError)
        let before =
            {
                initial with
                    removeGate = MW.PendingRemove MaterialIds.glass200
                    importStatus = Some (ControlCaption "Previous import")
                    importError = Some (DroppedFileRefusedErr "Previous refusal")
            }
        Assert.Same(before, MW.updateMaterialsWindow (MW.DropMaterialEntryFiles []) before)
        Assert.Empty opened
        let dropped = MW.updateMaterialsWindow (MW.DropMaterialEntryFiles [ file ]) before
        let row = Assert.Single(Assert.Single opened)
        Assert.Equal(material.id.toLibraryEntryId, row.entryId)
        Assert.Equal(DroppedFileOrigin file.leafName, row.origin)
        match row.status with
        | EntryIdNotHeld incoming -> Assert.Equal(material.id.toLibraryEntryId, incoming.content.id)
        | _ -> Assert.Fail("A new material must be offered as an unheld entry.")
        Assert.Equal<ControlCaption option>(None, dropped.importStatus)
        Assert.Equal<CatalogueImportError option>(None, dropped.importError)
        Assert.Equal(before.removeGate, dropped.removeGate)
        Assert.Equal(before.lifecycleGate, dropped.lifecycleGate)
        match dropped.mode with
        | Browse -> ()
        | Select _ -> Assert.Fail("Dropping files must preserve Browse mode.")
        Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(Ok None, context.catalogueImports.proxy.tryHeldEntry material.id.toLibraryEntryId)
        Assert.Empty calls

    [<Fact>]
    let ``a refused drop on the Materials window shows its reason and opens no dialog`` () : unit =
        let folder = freshDropFolder ()
        let leaf = FileName "notes.txt"
        let file = leaf.combine folder
        File.WriteAllText(file.value, "Not a library entry")
        let calls, initial = freshModel ()
        let before =
            {
                initial with
                    lifecycleGate = MW.PendingLifecycle (MaterialIds.glass200, MW.MarkInactiveAction)
                    importStatus = Some (ControlCaption "Previous import")
            }
        let refused = MW.updateMaterialsWindow (MW.DropMaterialEntryFiles [ file ]) before
        let error =
            match refused.importError with
            | Some (DroppedFileRefusedErr reason as error) ->
                Assert.Contains("notes.txt", reason)
                error
            | _ -> failwith "The unsupported file must produce DroppedFileRefusedErr."
        Assert.Equal<ControlCaption option>(None, refused.importStatus)
        Assert.Equal(before.removeGate, refused.removeGate)
        Assert.Equal(before.lifecycleGate, refused.lifecycleGate)
        Assert.Empty calls
        HeadlessSession.run (fun () ->
            let window = HostWindow(Width = 1150.0, Height = 800.0)
            try
                Program.mkSimple (fun () -> refused) MW.updateMaterialsWindow MW.materialsWindowView
                |> Program.withHost window
                |> Program.run
                window.Show()
                Dispatcher.UIThread.RunJobs()
                Assert.Equal(error.caption.value, textOf window ConstructorUiId.materialsImportStatusText.value)
                match tryFindControl window ConstructorUiId.materialsImportStatusText.value with
                | Some (:? TextBlock as status) -> Assert.Equal(TextWrapping.Wrap, status.TextWrapping)
                | _ -> Assert.Fail("The refusal must render in a wrapping status TextBlock.")
            finally window.Close())

    [<Fact>]
    let ``an applied import of dropped files is the Materials window status`` () : unit =
        let calls, initial = freshModel ()
        let events, selection = selectContext Sample (SampleLayerTarget (AtSingleLayer (FilmItemIndex 0)))
        let before =
            {
                initial with
                    mode = Select selection
                    selectedId = Some MaterialIds.glass200
                    lifecycleGate = MW.PendingLifecycle (MaterialIds.glass200, MW.MarkInactiveAction)
                    importError = Some (DroppedFileRefusedErr "Previous refusal")
            }
        let summary = ControlCaption "Imported the dropped material."
        let applied = MW.updateMaterialsWindow (MW.MaterialsImportApplied summary) before
        Assert.Equal<ControlCaption option>(Some summary, applied.importStatus)
        Assert.Equal<CatalogueImportError option>(None, applied.importError)
        Assert.Equal(before.removeGate, applied.removeGate)
        Assert.Equal(before.lifecycleGate, applied.lifecycleGate)
        Assert.Equal(before.selectedId, applied.selectedId)
        match applied.mode with
        | Select actual -> Assert.Same(selection, actual)
        | Browse -> Assert.Fail("Import completion must preserve the Select session.")
        Assert.Empty calls
        Assert.Empty events

    [<Fact>]
    let ``the Materials window accepts dropped files`` () : unit =
        HeadlessSession.run (fun () ->
            let materials, _, categories = freshStores ()
            let window = mountMaterialsWindow materials categories
            try Assert.True(DragDrop.GetAllowDrop window)
            finally window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: a material file dropped on the Materials window opens the import dialog, and Apply adds it to the app library`` () : unit =
        HeadlessSession.run (fun () ->
            let owner = FolderName "MaterialsWindowTests"
            let context =
                AppContext.create (TestAppScope.fileLocations owner) WorkbenchSettings.defaults
                |> Result.defaultWith (fun error -> failwith error.describe)
            TestAppScope.settlePendingImports context
            let file, material = writeDroppedMaterial ()
            let window = MaterialsWindow(context.materials, context.categories, context.catalogueImports)
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
                commitFilter window material.name.value
                Assert.False(isPresent window ConstructorUiId.materialsImportStatusText.value)
                window.DropEntryFiles [ file ]
                Dispatcher.UIThread.RunJobs()
                let dialog = Assert.Single opened
                Assert.True(dialog.IsVisible)
                Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok None, context.materials.tryGetMaterial material.id)
                Assert.False(isPresent window ConstructorUiId.materialsImportStatusText.value)
                clickOn dialog ConstructorUiId.importApplyButton.value
                Assert.False(dialog.IsVisible)
                Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some material), context.materials.tryGetMaterial material.id)
                Assert.Equal("Imported the dropped entry files (decisions applied: 1).", textOf window ConstructorUiId.materialsImportStatusText.value)
                expandEntries window
                Assert.True(isPresent window (ConstructorUiId.materialsEntryNode material.id).value,
                            "Apply must refresh the Materials tree without reopening the window.")
            finally
                for dialog in opened do dialog.Close()
                window.Close()
                Dispatcher.UIThread.RunJobs())
