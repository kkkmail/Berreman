{
  "claims": [
    {
      "id": 1,
      "phrase": "The five domain proxies are created INSIDE `MainConstructorWindow` (`…App/Program.fs:129-147`), so a second window cannot share the stores",
      "spec_location": "Part C §C.0",
      "evidence": "Read OpticalConstructor.App/Program.fs:118-150 -> `type MainConstructorWindow` at :118; exactly five proxies constructed in its `do` block: library (:129), experiments (:130), samples (:139), materials (:140), categories (:147); comment at :133-134 says 'All FIVE proxies (library / experiments / materials / samples / categories) inject through initMainWith'. Numeric count re-derived: 5 == 5.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 2,
      "phrase": "no `appsettings.json` exists (user preferences live in `environment.json`, `…Ui/UserEnvironment.fs:181-185`)",
      "spec_location": "Part C §C.0",
      "evidence": "Glob **/appsettings.json over C:/GitHub/Berreman -> 0 files. Read OpticalConstructor.Ui/UserEnvironment.fs:181 -> `type EnvironmentSettings` (favorites/lastFolders/recentFiles at :183-185); Grep 'environment.json' -> :338 `Path.Combine(dir, \"environment.json\")` is the persisted file name.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 3,
      "phrase": "`Softellect.Sys` **10.1.301.54**, already referenced by `…Storage/OpticalConstructor.Storage.fsproj`",
      "spec_location": "Part C §C.0",
      "evidence": "Read OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj -> line 74 `<PackageReference Include=\"Softellect.Sys\" Version=\"10.1.301.54\" />`; obj/project.assets.json resolves `Softellect.Sys/10.1.301.54`.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 4,
      "phrase": "its `tryCreate` opens `appsettings.json`, its `SetOnMissing = true` writes missing keys' defaults back, and it exposes typed `get*OrDefault` accessors and `tryGetConnectionString`",
      "spec_location": "Part C §C.0",
      "evidence": "Verified against the Softellect source checkout C:/GitHub/Softellect/Sys/AppSettings.fs: `let SetOnMissing = true` :16, `type AppSettingsProvider` :411, typed accessors getString/Int/Decimal/Double/Guid/Bool/FolderName/FileNameOrDefault :412-419, `tryGetConnectionString` :439, `static member tryCreate` overloads :471-484 (the argless :484 opening the default `appSettingsFile`). Member names AppSettingsProvider/tryCreate/SetOnMissing/tryGetConnectionString/OrDefault also all present in the cached Softellect.Sys.dll binary.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 5,
      "phrase": "a host-layer `WindowRegistry` (mutable host state OUTSIDE the Elmish model, the registered-storage-provider pattern at `…App/Program.fs:96` in today's tree)",
      "spec_location": "Part C §C.0",
      "evidence": "Read OpticalConstructor.App/Program.fs:96 -> `this.Opened.Add(fun _ -> Shell.setStorageProvider this.StorageProvider)`; comment :93-95 says the IStorageProvider is 'held OUTSIDE the root model' — the cited registered-storage-provider pattern.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 6,
      "phrase": "moving the id-mint off the save path (`newMaterialId` at `…TestWindows/MaterialEditorView.fs:416`)",
      "spec_location": "Part C §C.0",
      "evidence": "Read OpticalConstructor.TestWindows/MaterialEditorView.fs:416 -> `| NewMaterial -> m.context.materials.addMaterial (entryUnder (newMaterialId ()))` — the id is minted inside the save handler, exactly as claimed.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 7,
      "phrase": "The seam generalizes the existing `EditorLaunchers` function-record (`…TestWindows/TableAndElementRotationView.fs:116-141`)",
      "spec_location": "Part C §C.0",
      "evidence": "Read OpticalConstructor.TestWindows/TableAndElementRotationView.fs:116-141 -> `[<ReferenceEquality>] type EditorLaunchers` at :116-131 (openMaterialEditor / openSampleEditor / openCategoryEditor) with `static member defaults` at :136-141. Range matches exactly.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 8,
      "phrase": "the dispersion-model facet is per axis/segment over the ten `DispersionModel` cases, `…Domain/DispersionModels.fs:168-178`",
      "spec_location": "Part D §D.0",
      "evidence": "Read OpticalConstructor.Domain/DispersionModels.fs:168-178 -> `type DispersionModel` with cases Sellmeier, Cauchy, Lorentz, Drude, TaucLorentz, GaussianOscillator, ForouhiBloomer, BrendelBormann, ConstantNK, SumOfTerms. Numeric count re-derived: 10 == 10; range matches exactly.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 9,
      "phrase": "Physics-derived offers reuse `availableGyrationClasses` (`…Domain/MaterialComplexityEditor.fs:373`), never re-derived",
      "spec_location": "Part D §D.0",
      "evidence": "Read OpticalConstructor.Domain/MaterialComplexityEditor.fs:373 -> `let availableGyrationClasses (anisotropy : Anisotropy) : GyrationClass<RhoValue> list =` at exactly the cited line.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 10,
      "phrase": "Material facets extract from `MaterialEntry.properties` (`…Domain/MaterialLibrary.fs:230-238`)",
      "spec_location": "Part D §D.0",
      "evidence": "Read OpticalConstructor.Domain/MaterialLibrary.fs:230-238 -> `type MaterialEntry` record spanning exactly :230-238 with `properties : OpticalPropertiesWithDisp` at :236.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 11,
      "phrase": "the domain-free rendering control (pure `State` + `Handlers`, the `…Controls/MaterialsControls.fs` shape)",
      "spec_location": "Part E §E.0",
      "evidence": "Grep OpticalConstructor.Controls/MaterialsControls.fs -> `type State` at :51 and `type Handlers` at :82 — the cited pure State + Handlers shape exists in the cited file.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 12,
      "phrase": "the embeddable dual-axis chart, `…Controls/EmbeddedChart.fs`",
      "spec_location": "Part E §E.0",
      "evidence": "Read OpticalConstructor.Controls/EmbeddedChart.fs:13-22 -> module EmbeddedChart, doc comment: 'the inline / embeddable form of the shared dual-axis ScottPlot chart' (n on the LEFT axis, k on the RIGHT).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 13,
      "phrase": "the existing verbs Add / Edit / Remove / Categories… rewired to the window",
      "spec_location": "Part E §E.0",
      "evidence": "Anchored via the adjacent materialsBay pointer. MaterialsControls.fs verb row: Add :251, Edit :245-246, Remove :254 (button ids Add/Edit/RemoveMaterialButton :111-115); the host-added 'Categories…' verb at TableAndElementRotationView.fs:2297 (`categoriesRow` :2293), composed into `materialsBay` at :2308-2309. All four verbs exist today.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 14,
      "phrase": "The Materials bay is REMOVED from the ribbon (`materialsBay` in `mainBays`, `…TestWindows/TableAndElementRotationView.fs:2339`)",
      "spec_location": "Part E §E.0",
      "evidence": "Read OpticalConstructor.TestWindows/TableAndElementRotationView.fs:2339 -> `{ name = BayNames.materials; content = materialsBay model dispatch; mode = Ribbon.FullSurface }` inside `mainBays` (:2329) — exactly the cited line; `materialsBay` defined at :2303.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 15,
      "phrase": "the seeded presets (`pol-lp` / `pol-cp-left` / `pol-cp-right` / `det-intensity` / `det-ellipsometer` / `src-600`, `…Domain/ElementId.fs:535-540`)",
      "spec_location": "Part F §F.0",
      "evidence": "Read OpticalConstructor.Domain/ElementId.fs:535-540 -> det-intensity :535, det-ellipsometer :536, pol-lp :537, pol-cp-left :538, pol-cp-right :539, src-600 :540 inside `seedEntries`. All six ids at exactly the cited range.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 16,
      "phrase": "Seeded SAMPLES remain `UserManaged` (they are editable examples today — recorded interpretation)",
      "spec_location": "Part F §F.0",
      "evidence": "Inferred assumption-shape phrase ('editable examples today' — not in the trigger list). Seeded samples are `SeedSamples.all` mapped into `seedEntries` (ElementId.fs:532-533); `SampleProxy` (:318-326) exposes updateSample/removeSample over any sample, and Grep 'EntryProtection|ProtectedBuiltIn' over the repo -> 0 hits, so no protection concept exists today — seeded samples are ordinary editable entries (the editor's `EditSample` intent edits an existing sample in place, TableAndElementRotationView.fs:123-126).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 17,
      "phrase": "`ComputedIdeal` synthesizes through the existing `Propagation.inputStokes` / `analyzerMueller` (`…Domain/Propagation.fs:54-89`) exactly as today",
      "spec_location": "Part F §F.0",
      "evidence": "Read OpticalConstructor.Domain/Propagation.fs:54-89 -> `let inputStokes (kind : PolarizerKind) (theta : Angle) : StokesVector` at :54 and `let analyzerMueller (kind : PolarizerKind) (theta : Angle) : MuellerMatrix` at :67, ending :89. Range matches exactly.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 18,
      "phrase": "the whole `LibraryEntry` corpus (`…Domain/ElementId.fs:188-192`, via `LibraryProxy` `:276-281` + `SampleProxy` for the sample verbs)",
      "spec_location": "Part F §F.0",
      "evidence": "Read OpticalConstructor.Domain/ElementId.fs -> `type LibraryEntry` DU (SampleItem/SourceItem/DetectorItem/PolarizerItem) at :188-192; `[<ReferenceEquality>] type LibraryProxy` at :276-281 (entriesForKind/libraryTrees/tryGetEntry); `type SampleProxy` at :318-326 with the sample verbs (listSamples/searchSamples/tryGetSample/addSample/updateSample/removeSample). All three pointers resolve.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 19,
      "phrase": "the Library bay (`samplesBay`, `…TestWindows/TableAndElementRotationView.fs:2340`) is removed the same way as Materials",
      "spec_location": "Part F §F.0",
      "evidence": "Read OpticalConstructor.TestWindows/TableAndElementRotationView.fs:2340 -> `{ name = BayNames.library; content = samplesBay model dispatch; mode = Ribbon.FullSurface }` inside `mainBays` — exactly the cited line; `samplesBay` defined at :2316.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 20,
      "phrase": "a changed table selection or a vanished target cancels/closes the open Select window (the pending-bind-clears precedent, `…TestWindows/TableAndElementRotationView.fs:941-944`)",
      "spec_location": "Part G §G.0",
      "evidence": "Read OpticalConstructor.TestWindows/TableAndElementRotationView.fs:941-944 -> comment 'a changed selection clears any pending (unconfirmed) Library bind' with `let pending = if selection = model.selection then model.pendingEntry else None` — the cited precedent at exactly the cited range.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 21,
      "phrase": "both paths converge on the SAME bind message that commits `placement.valueId` (`…TestWindows/TableAndElementRotationView.fs:768,780`)",
      "spec_location": "Part G §G.0",
      "evidence": "Read OpticalConstructor.TestWindows/TableAndElementRotationView.fs -> :768 (BindValueId) and :780 (ConfirmBindValueId) both execute `{ e with placement = { e.placement with valueId = Some entryId } }` — the valueId commit sits at exactly the two cited lines.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 22,
      "phrase": "every new table element except a sample is created pre-bound to its seeded default — light source → `src-600`, detector → `det-intensity`, and the polarizer palette exposes THREE add buttons LP / CPL / CPR pre-binding `pol-lp` / `pol-cp-left` / `pol-cp-right`",
      "spec_location": "Part G §G.0",
      "evidence": "The five named seeded defaults all exist as seeded Library entries: src-600 (ElementId.fs:540), det-intensity (:535), pol-lp (:537), pol-cp-left (:538), pol-cp-right (:539). The pre-binding behaviour itself is new; the assumption audited is that the referenced seed ids exist.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 23,
      "phrase": "with `CatalogueKind` UNCHANGED (`…Domain/Placement.fs:93-101`)",
      "spec_location": "Part G §G.0",
      "evidence": "Read OpticalConstructor.Domain/Placement.fs:93-101 -> `type CatalogueKind` with cases LightSource/LinearPolarizer/CircularPolarizer/Sample/Lens/FlatMirror/CurvedMirror/Detector spanning exactly :93-101.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 24,
      "phrase": "Samples stay unbound (the inverse hook)",
      "spec_location": "Part G §G.0",
      "evidence": "Inferred assumption-shape phrase ('stay' = unchanged-behaviour claim; anchored via the adjacent Placement.fs pointer). Today every new element's placement is created unbound: `valueId = None` in the placement construction default at Placement.fs:208, so samples staying unbound is the current mechanism.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 25,
      "phrase": "The silent ideal-analyzer fallback — the `Option.defaultValue Library.IdealLinear` tail of `runAnalyzerKind` (`…TestWindows/TableAndElementRotationView.fs:1407-1427`) — is retired",
      "spec_location": "Part G §G.0",
      "evidence": "Read OpticalConstructor.TestWindows/TableAndElementRotationView.fs:1407-1427 -> `let private runAnalyzerKind` at :1407 ending with `|> Option.defaultValue Library.IdealLinear` at :1427. Range matches exactly.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 26,
      "phrase": "The sample editor's `WrapPanel` material picker (`…TestWindows/SampleEditorView.fs:740-758`) is replaced by the Materials window in Select state",
      "spec_location": "Part G §G.0",
      "evidence": "Read OpticalConstructor.TestWindows/SampleEditorView.fs:740-758 -> `let private materialRow` at :740 containing `WrapPanel.create` (:748, id UiIds.materialPicker) listing material options, ending :758. Range matches exactly.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 27,
      "phrase": "the material list re-queries the proxy when a Select window returns and on window activation, replacing the load-once snapshot (`…TestWindows/SampleEditorWindow.fs:32-35`)",
      "spec_location": "Part G §G.0",
      "evidence": "Read OpticalConstructor.TestWindows/SampleEditorWindow.fs:32-35 -> `let entries = match materials.listMaterials () with | Ok list -> list | Error _ -> []` — the one-time snapshot resolved at window construction, at exactly the cited range (doc comment :12-13: 'material choices are resolved ONCE').",
      "verdict": "CONFIRMED"
    }
  ]
}
