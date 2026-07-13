{
  "claims": [
    {
      "id": 1,
      "phrase": "Full spec authored from the operator's preliminary spec `specs/0038/.manual/006-ui-standardization-preliminary-spec.md` (operator decisions Q1–Q24 of `003`, Q25–Q29 of `005`, and the three approved interpretation calls of its §17 are binding)",
      "spec_location": "header paragraph",
      "evidence": "[inferred existing-artefact claim; phrase not in the trigger list] Glob specs/0038/.manual/* -> 006-ui-standardization-preliminary-spec.md, 003-comments.txt, and 005-comments.txt all present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 2,
      "phrase": "Follow the existing elevated-id precedents `MaterialId` (`…Domain/MaterialLibrary.fs:27`)",
      "spec_location": "§0.1",
      "evidence": "Read OpticalConstructor.Domain/MaterialLibrary.fs:27 -> `type MaterialId =` (single-case DU over Guid with `.value` :30, `create` :31, `tryCreate` :33).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 3,
      "phrase": "and `SampleId` / the preset DUs (`…Domain/ElementId.fs:139-192`)",
      "spec_location": "§0.1",
      "evidence": "Read OpticalConstructor.Domain/ElementId.fs:139-192 -> `Sample` (:139, field `id : SampleId` at :141), `SourcePreset` :152, `DetectorKind` :161, `DetectorPreset` :165, `PolarizerKind` :174, `PolarizerPreset` :179, `LibraryEntry` :188 — the preset DUs are exactly inside the cited range. `type SampleId` itself is declared at :41 of the same file and is referenced inside the range.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 4,
      "phrase": "the Main-flow composition builds every store in memory at startup (`…App/Program.fs:129-147`, `createInMemory` proxies over the Domain seeds)",
      "spec_location": "§0.2",
      "evidence": "[inferred existing-wiring claim] Read OpticalConstructor.App/Program.fs:129-147 -> `Library.createInMemory ()` :129, `Experiments.createInMemory ()` :130, `SampleProxy.createInMemory ()` :139, `MaterialProxy.createInMemory (samplesReferencing samples)` :140, `CategoryProxy.createInMemory (...)` :147 — all five stores built in memory inside `MainConstructorWindow`.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 5,
      "phrase": "The only library-shaped serialization in the tree is metadata-only and unwired from that composition — the §I.8 share/export file format (`…Storage/Report.fs:17-24`, module `MaterialLibrary`, whose persisted `MaterialEntryDto` at `:36` carries display metadata only)",
      "spec_location": "§0.2",
      "evidence": "Read OpticalConstructor.Storage/Report.fs:17-24 -> module doc comment at :17-23, `module MaterialLibrary` at :24; `type private MaterialEntryDto` at :36 with fields id/name/category/description only (comment: 'the library file carries the schema's display metadata only'). Grep for OpticalConstructor.Storage under OpticalConstructor.App -> only a prose comment at Program.fs:128; App.fsproj carries no Storage ProjectReference — unwired from the Main-flow composition.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 6,
      "phrase": "and the import/export helpers (`…Storage/MaterialImport.fs` — `importRefractiveIndexInfo` `:407`, `importCsv` `:432`, `exportCsv` `:459`)",
      "spec_location": "§0.2",
      "evidence": "Grep OpticalConstructor.Storage/MaterialImport.fs -> `let importRefractiveIndexInfo` :407, `let importCsv` :432, `let exportCsv` :459 — all three at exactly the cited lines.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 7,
      "phrase": "The registered write-seam contracts `STORE_XDUO_0001 MaterialProxy` (`…Domain/MaterialLibrary.fs:506`) ... keep their names, kinds, and declaring project",
      "spec_location": "§0.2",
      "evidence": "Read/Grep OpticalConstructor.Domain/MaterialLibrary.fs -> `type MaterialProxy =` at :506 with contract comment 'spec 0033 steps 003/006, contract STORE_XDUO_0001' at :496; declared in OpticalConstructor.Domain as stated.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 8,
      "phrase": "and `STORE_XDUO_0002 SampleProxy` (`…Domain/ElementId.fs:318`)",
      "spec_location": "§0.2",
      "evidence": "Read/Grep OpticalConstructor.Domain/ElementId.fs -> `type SampleProxy =` at :318 with contract comment 'spec 0033 steps 004/005, contract STORE_XDUO_0002' at :309; declared in OpticalConstructor.Domain as stated.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 9,
      "phrase": "(the 0035 precedent of re-typing `MaterialEntry.category` through the same surface)",
      "spec_location": "§0.2",
      "evidence": "[class-anchored] Read OpticalConstructor.Domain/MaterialLibrary.fs:230-238 -> `type MaterialEntry` with `category : CategoryId` at :234; Storage/Report.fs:30-31 records 'the closed MaterialCategory union became a seeded catalogue keyed by CategoryId (spec 0035 step 001)' — the re-typing precedent exists as described.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 10,
      "phrase": "The one persisted user file this spec touches, `environment.json`, is loaded totally with defaults on shape mismatch (`…App/Program.fs:33-35`)",
      "spec_location": "§0.2",
      "evidence": "Read OpticalConstructor.App/Program.fs:33-35 -> doc comment: '`load` is total and falls back to the built-in `defaults` on a missing/invalid settings file'; `UserEnvironment.load (UserEnvironment.settingsPath ())` invoked at :37.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 11,
      "phrase": "the existing seam (`SampleProxy.createInMemory`, `…Domain/ElementId.fs:652`",
      "spec_location": "§0.3",
      "evidence": "Read OpticalConstructor.Domain/ElementId.fs:652 -> `static member createInMemory () : SampleProxy =` inside `type SampleProxy with` (:650); closes over a `ref Map` seeded from `seedEntries` per :639-654.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 12,
      "phrase": "`MaterialProxy.createInMemory`, `:722`)",
      "spec_location": "§0.3",
      "evidence": "Read OpticalConstructor.Domain/ElementId.fs:722 -> `static member createInMemory (samplesReferencing : MaterialId -> Sample list) : MaterialProxy =` — the bare `:722` resolves in the same file as the preceding pointer (the type augmentation lives in ElementId.fs per MaterialLibrary.fs:502).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 13,
      "phrase": "This spec writes NO catalogue, scene, or experiment files to disk and adds NO schema or import/export work; `.ocproj` stays unwired.",
      "spec_location": "§0.3",
      "evidence": "[inferred existing-state claim, phrase 'stays unwired'] Grep 'ocproj' under Berreman/OpticalConstructor -> hits only in OpticalConstructor.Storage (ProjectFile/Autosave), the retired Ui shell paths exercised by tests, and test projects; the Main-flow composition (App/Program.fs:118-150) builds only in-memory proxies and App.fsproj carries no Storage ProjectReference — `.ocproj` is not wired into the Main flow today.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 14,
      "phrase": "The only real IO permitted: (a) the existing `environment.json` persistence (`…Ui/UserEnvironment.fs`)",
      "spec_location": "§0.3",
      "evidence": "Glob -> OpticalConstructor.Ui/UserEnvironment.fs exists; App/Program.fs:37 loads it at startup (`UserEnvironment.load (UserEnvironment.settingsPath ())`).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 15,
      "phrase": "they use `AutomationProperties.AutomationId` (the documented pattern at `…Controls/MaterialsControls.fs:139-146`)",
      "spec_location": "§0.4",
      "evidence": "Read OpticalConstructor.Controls/MaterialsControls.fs:139-146 -> explanatory comment ('a freely-mutable attached property — unlike `Control.Name`', membership-change rationale) at :139-144 and `let private automationId (autoId : string) : IAttr<Border>` at :145-146 — exactly the cited range.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 16,
      "phrase": "The relocation and the new projects MUST NOT reintroduce `MSB3277` (the WebView2 chain was already resolved in 0035)",
      "spec_location": "§0.6",
      "evidence": "Read OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj:176-187 -> comment 'Spec 0035 §0.6 (slice 018): the `Microsoft.Web.WebView2` package reference is DROPPED to clear the MSB3277 WindowsBase conflict at the reference level (never suppressed)'; specs/0035/.spec-md:55-57 names the same chain. Resolution is recorded in the tree.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 17,
      "phrase": "`appsettings.json` and the three new project folders collide with nothing in the root `.gitignore` (checked: only build dirs, `*.binz`, `*.autosave`, and arc-runner logs are ignored)",
      "spec_location": "§0.7 (Committable files)",
      "evidence": "Read C:/GitHub/Berreman/.gitignore (348 lines) -> no pattern matches `appsettings.json` or an `OpticalConstructor.*` project-folder name; `*.binz` :336, `*.autosave` :340, arc-runner logs :346-347 present. The file is the full VisualStudio template (build results, caches, user-specific files, `*.log`, etc.), so the 'only …' enumeration is a coarse summary, but every ignored pattern is a build/tooling artifact and the no-collision assertion holds.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 18,
      "phrase": "`<STORE:SeedingProxy>` | proxy | OpticalConstructor.Seeding | seed-push seam over the existing Domain seeds",
      "spec_location": "Contract table",
      "evidence": "[class-anchored] Grep under OpticalConstructor.Domain -> `let seedEntries : LibraryEntry list` ElementId.fs:532, `let seedExperiments : Experiment list` ElementId.fs:1084, seeded built-in category catalogue MaterialLibrary.fs:114 and default seeded in-memory library MaterialLibrary.fs:463 — the Domain seeds exist.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 19,
      "phrase": "`LibraryControls` sets the write-once `Border.name` on generated, unkeyed rows (`leafRow`, `…Controls/LibraryControls.fs:112`",
      "spec_location": "§A.0",
      "evidence": "Read OpticalConstructor.Controls/LibraryControls.fs:105-126 -> `let private leafRow` at :105 builds `Border.create [ Border.name id; ... ]` with `Border.name id` at exactly :112; rows are generated per entry via `rowView` (:136-143) with no `View.withKey`.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 20,
      "phrase": "`actionButton`, `:148`)",
      "spec_location": "§A.0",
      "evidence": "Read OpticalConstructor.Controls/LibraryControls.fs:146-154 -> `let private actionButton` at :146 builds `Border.create [ Border.name id; ... ]` with `Border.name id` at exactly :148.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 21,
      "phrase": "The cure already exists in the same project — the freely-mutable `AutomationProperties.AutomationId` helper with the explanatory comment (`…Controls/MaterialsControls.fs:139-146`)",
      "spec_location": "§A.0",
      "evidence": "Read OpticalConstructor.Controls/MaterialsControls.fs:139-146 -> the `automationId` helper (AttrBuilder over `AutomationProperties.AutomationIdProperty`) with the membership-change explanatory comment, at exactly the cited range, in the same OpticalConstructor.Controls project as LibraryControls.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 22,
      "phrase": "and the same precedent noted in `…Controls/ExperimentControls.fs:261-266`",
      "spec_location": "§A.0",
      "evidence": "Read OpticalConstructor.Controls/ExperimentControls.fs:261-266 -> comment: reorderable lists 'carry an `AutomationProperties.AutomationId` (freely mutable ...) instead of `Border.name`', with the `automationId` helper immediately below at :268.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 23,
      "phrase": "Latent copies of the hazard: `…Controls/RotationControls.fs:119`",
      "spec_location": "§A.0",
      "evidence": "Read OpticalConstructor.Controls/RotationControls.fs:117-119 -> `clickBox` builds `Border.create [ Border.name id; ... ]` with `Border.name id` at exactly :119.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 24,
      "phrase": "`…Controls/RayPositionControls.fs:71`",
      "spec_location": "§A.0",
      "evidence": "Read OpticalConstructor.Controls/RayPositionControls.fs:69-71 -> `clickBox` builds `Border.create [ Border.name id; ... ]` with `Border.name id` at exactly :71.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 25,
      "phrase": "`…Controls/ElementPaletteControls.fs:55`",
      "spec_location": "§A.0",
      "evidence": "Read OpticalConstructor.Controls/ElementPaletteControls.fs:53-55 -> `clickBox` builds `Border.create [ Border.name id; ... ]` with `Border.name id` at exactly :55.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 26,
      "phrase": "and `…Controls/ExperimentControls.fs:240,581`",
      "spec_location": "§A.0",
      "evidence": "Grep `\\.name` in OpticalConstructor.Controls/ExperimentControls.fs -> `Border.name id` at exactly :240 and :581.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 27,
      "phrase": "The screen users actually see is `TableAndElementRotationView.mainView` in **`OpticalConstructor.TestWindows`** (`…TestWindows/TableAndElementRotationView.fs:2367`)",
      "spec_location": "§B.0",
      "evidence": "Read OpticalConstructor.TestWindows/TableAndElementRotationView.fs:2367 -> `let mainView (model : Model) (dispatch : Msg -> unit) : IView =` ('The Main screen view' doc at :2362-2366); the file lives in the OpticalConstructor.TestWindows project.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 28,
      "phrase": "hosted by `MainConstructorWindow` (`…App/Program.fs:118-150`)",
      "spec_location": "§B.0",
      "evidence": "Read OpticalConstructor.App/Program.fs:118-150 -> `type MainConstructorWindow() as this` at :118 mounts `TableAndElementRotationView.mainView` via `Program.mkSimple ... TableAndElementRotationView.mainView` at :148.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 29,
      "phrase": "The `OpticalConstructor.Ui` Elmish shell is dead code for the Main flow — the composition comment records that the old constructor shell \"is kept but no longer opened by the launcher's Main button\" (`…App/Program.fs:112-117`)",
      "spec_location": "§B.0",
      "evidence": "Read OpticalConstructor.App/Program.fs:112-117 -> doc comment ends '(The old constructor shell — `MainWindow` above — is kept but no longer opened by the launcher's Main button.)' — exact quoted text at the cited range; the launcher's Main button at :176 opens `MainConstructorWindow().Show()`, not the Elmish shell.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 30,
      "phrase": "the seven diagnostic scenes currently on the product launcher (`…App/Program.fs:177-224`)",
      "spec_location": "§B.0",
      "evidence": "[numeric-count claim] Read OpticalConstructor.App/Program.fs:177-224 -> exactly seven test-scene buttons: TableRotation :177, ElementRotation :184, TableAndElementRotation :191, ElementMovement :198, RendererTest :205, SnapToBeam :212, SnapToReflected :219. Live count 7 == cited count 7.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 31,
      "phrase": "after which `OpticalConstructor.App` drops its `OpticalConstructor.TestWindows` project reference entirely — test code leaves the product dependency graph",
      "spec_location": "§B.0",
      "evidence": "[inferred existing-wiring claim: the reference to be dropped must exist today] Grep OpticalConstructor.App/OpticalConstructor.App.fsproj -> `<ProjectReference Include=\"..\\OpticalConstructor.TestWindows\\OpticalConstructor.TestWindows.fsproj\" />` at :50.",
      "verdict": "CONFIRMED"
    }
  ]
}
