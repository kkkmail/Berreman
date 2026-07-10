{
  "claims": [
    {
      "id": 1,
      "phrase": "Full spec authored from the operator's preliminary spec `specs/0038/.manual/006-ui-standardization-preliminary-spec.md` (operator decisions Q1–Q24 of `003`, Q25–Q29 of `005`, and the three approved interpretation calls of its §17 are binding).",
      "spec_location": "intro ¶1",
      "evidence": "Inferred assumption-shape (cites existing operator artefacts; not in the trigger list). Glob specs/0038/.manual/ -> 006-ui-standardization-preliminary-spec.md, 003-comments.txt, 005-comments.txt all exist. Grep 003-comments.txt for Q1..Q24 -> 24 hits; Grep 005-comments.txt -> Q25–Q29 present (lines 4-10); Grep 006 -> '## 17. Decisions (from the operator, this cycle) + recorded interpretations' at line 561.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 2,
      "phrase": "The ordered work lives in `.spec-jsonl`",
      "spec_location": "intro ¶1",
      "evidence": "Inferred assumption-shape (points at an existing bundle artefact). Glob specs/0038/.spec-* -> .spec-jsonl exists beside .spec-md and .spec-bundle.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 3,
      "phrase": "Follow the existing elevated-id precedents `MaterialId` (`…Domain/MaterialLibrary.fs:27`)",
      "spec_location": "§0.1",
      "evidence": "Read OpticalConstructor.Domain/MaterialLibrary.fs:27 -> `type MaterialId = | MaterialId of Guid` with .value (:30), create (:31), tryCreate (:33) — the elevated single-case DU as claimed.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 4,
      "phrase": "and `SampleId` / the preset DUs (`…Domain/ElementId.fs:139-192`)",
      "spec_location": "§0.1",
      "evidence": "Read OpticalConstructor.Domain/ElementId.fs:139-192 -> Sample record (:139, field `id : SampleId` at :141), SourcePreset (:152), DetectorKind (:161), DetectorPreset (:165), PolarizerKind (:174), PolarizerPreset (:179), LibraryEntry (:188) — the preset DUs sit inside the cited range. Note: the `type SampleId` declaration itself (single-case Guid DU with .value/create) is at ElementId.fs:41, outside the cited range; the range covers its use plus the preset DUs.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 5,
      "phrase": "Nothing library-related is serialized yet, so re-typed structures replace old ones cleanly and every app start re-seeds in memory.",
      "spec_location": "§0.2",
      "evidence": "Inferred assumption-shape ('yet' = current-state claim); no <file>:<symbol> pointer. Grep contradicts the flat reading: OpticalConstructor.Storage/Report.fs:17-24 declares a 'Shareable material-library file format (§I.8 / R-4)' — module MaterialLibrary with a persisted-form MaterialEntryDto (:36) and schema-validated canonical-JSON export/import of `MaterialEntry list`; OpticalConstructor.Storage/MaterialImport.fs also imports/exports MaterialEntry (importRefractiveIndexInfo :407, importCsv :432, exportCsv :459). The narrower reading does hold: the live composition re-seeds in memory (OpticalConstructor.App/Program.fs:129-147, createInMemory over builtInEntries/seedEntries) and neither OpticalConstructor.App nor OpticalConstructor.TestWindows references any library file IO. Spec-writer should anchor/qualify the sentence (e.g. no persisted library STATE; the §I.8 share/export format in Storage/Report.fs is metadata-only and unwired from the Main flow).",
      "verdict": "REFUTED-NO-POINTER"
    },
    {
      "id": 6,
      "phrase": "The registered write-seam contracts `STORE_XDUO_0001 MaterialProxy` (`…Domain/MaterialLibrary.fs:506`) … keep their names, kinds, and declaring project",
      "spec_location": "§0.2",
      "evidence": "Read MaterialLibrary.fs:496-514 -> doc comment names contract STORE_XDUO_0001; `[<ReferenceEquality>] type MaterialProxy` at :505-506, declared in OpticalConstructor.Domain as a record of Result-returning functions.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 7,
      "phrase": "and `STORE_XDUO_0002 SampleProxy` (`…Domain/ElementId.fs:318`)",
      "spec_location": "§0.2",
      "evidence": "Read ElementId.fs:309-326 -> doc comment names contract STORE_XDUO_0002; `[<ReferenceEquality>] type SampleProxy` at :317-318, declared in OpticalConstructor.Domain.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 8,
      "phrase": "(the 0035 precedent of re-typing `MaterialEntry.category` through the same surface)",
      "spec_location": "§0.2",
      "evidence": "Class-anchored. Grep MaterialLibrary.fs -> :93-94 doc records that `MaterialEntry.category` and `MaterialQuery.category` are `CategoryId`-typed (spec 0035 step 001) — the re-typing precedent exists as described.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 9,
      "phrase": "the one persisted user file this spec touches, `environment.json`, is loaded totally with defaults on shape mismatch (`…App/Program.fs:33-35`)",
      "spec_location": "§0.2",
      "evidence": "Read App/Program.fs:33-35 -> comment: '`load` is total and falls back to the built-in `defaults` on a missing/invalid settings file'; :37 performs the load at startup.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 10,
      "phrase": "the existing seam (`SampleProxy.createInMemory`, `…Domain/ElementId.fs:652`; `MaterialProxy.createInMemory`, `:722`)",
      "spec_location": "§0.3",
      "evidence": "Read ElementId.fs:650-652 -> `type SampleProxy with static member createInMemory` at :652; ElementId.fs:720-722 -> `type MaterialProxy with static member createInMemory` at :722. Both close over private `ref` Maps and both proxy types carry [<ReferenceEquality>], matching the claim.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 11,
      "phrase": "`.ocproj` stays unwired",
      "spec_location": "§0.3",
      "evidence": "Inferred assumption-shape (current-state claim), class-anchored on the `.ocproj` token. Grep 'ProjectFile|ocproj' -> 0 hits in OpticalConstructor.App and OpticalConstructor.TestWindows (the live Main flow); the machinery exists in OpticalConstructor.Storage (ProjectFile.fs) and is consumed only by the retired Ui shell (Shell.fs, ConstructionPage.fs) and tests. Operator confirmation at specs/0038/.manual/005-comments.txt:4 (Q25: '.ocproj are not wired yet').",
      "verdict": "CONFIRMED"
    },
    {
      "id": 12,
      "phrase": "(a) the existing `environment.json` persistence (`…Ui/UserEnvironment.fs`)",
      "spec_location": "§0.3",
      "evidence": "Glob -> OpticalConstructor.Ui/UserEnvironment.fs exists (the only UserEnvironment.fs in the tree); App/Program.fs:37 loads it at startup.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 13,
      "phrase": "(c) the thin measured-data CSV file-read adapter behind `ExperimentDataProxy` (approved interpretation, preliminary §17.2)",
      "spec_location": "§0.3",
      "evidence": "Inferred assumption-shape (cites an existing operator-document section). Grep .manual/006-ui-standardization-preliminary-spec.md -> '## 17. Decisions (from the operator, this cycle) + recorded interpretations' at :561, and the document itself carries the same cross-reference 'parsers + `ExperimentDataProxy` (+ thin file adapter per §17.2)' at :624.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 14,
      "phrase": "the documented pattern at `…Controls/MaterialsControls.fs:139-146`",
      "spec_location": "§0.4",
      "evidence": "Read Controls/MaterialsControls.fs:139-146 -> explanatory comment (:139-144) plus `automationId` helper (:145-146) setting AutomationProperties.AutomationId through FuncUI's attr builder — exactly the documented pattern claimed.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 15,
      "phrase": "the WebView2 chain was already resolved in 0035",
      "spec_location": "§0.6",
      "evidence": "Grep *.fsproj for WebView2 -> only OpticalConstructor.Ui.fsproj:176-187, a comment recording 'Spec 0035 §0.6 (slice 018): the Microsoft.Web.WebView2 package reference is DROPPED to clear the MSB3277 WindowsBase conflict at the reference level (never suppressed)'. No project references the package today.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 16,
      "phrase": "`appsettings.json` and the three new project folders collide with nothing in the root `.gitignore` (checked: only build dirs, `*.binz`, `*.autosave`, and arc-runner logs are ignored)",
      "spec_location": "§0.7",
      "evidence": "Read /.gitignore -> no pattern matches 'appsettings.json' or an 'OpticalConstructor.*' folder name, so the no-collision core holds. The parenthetical inventory is an under-description: the file is the full VisualStudio template (also ignores *.log, *.user, /data, node_modules/, .idea/, x64/, etc. — lines 6-331) plus *.binz (:336), *.autosave (:340) and the arc-runner log rules (:346-347); none of the extra patterns collide with the files this spec commits either.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 17,
      "phrase": "seed-push seam over the existing Domain seeds (`<STORE:SeedingProxy>` row)",
      "spec_location": "Contract table",
      "evidence": "Plural class-anchored reference ('the existing Domain seeds'). Grep OpticalConstructor.Domain -> `let builtInEntries : MaterialEntry list` (MaterialLibrary.fs:363) and `let seedEntries : LibraryEntry list` (ElementId.fs:532); both feed the createInMemory stores at App/Program.fs:129-147. The Domain seeds exist.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 18,
      "phrase": "`LibraryControls` sets the write-once `Border.name` on generated, unkeyed rows (`leafRow`, `…Controls/LibraryControls.fs:112`",
      "spec_location": "§A.0",
      "evidence": "Read Controls/LibraryControls.fs:105-126 -> `leafRow` (:105) sets `Border.name id` at :112 on a generated row; no View.withKey in the builder.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 19,
      "phrase": "`actionButton`, `:148`)",
      "spec_location": "§A.0",
      "evidence": "Read Controls/LibraryControls.fs:146-158 -> `actionButton` (:146) sets `Border.name id` at :148.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 20,
      "phrase": "The cure already exists in the same project — the freely-mutable `AutomationProperties.AutomationId` helper with the explanatory comment (`…Controls/MaterialsControls.fs:139-146`)",
      "spec_location": "§A.0",
      "evidence": "Read Controls/MaterialsControls.fs:139-146 -> comment + `automationId` AttrBuilder helper present exactly there, in the same OpticalConstructor.Controls project as LibraryControls.fs.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 21,
      "phrase": "and the same precedent noted in `…Controls/ExperimentControls.fs:261-266`",
      "spec_location": "§A.0",
      "evidence": "Read Controls/ExperimentControls.fs:261-269 -> comment block (:261-267) documenting AutomationProperties.AutomationId for reorderable lists ('Avalonia forbids changing a styled control's Name'), with the `automationId` helper at :268.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 22,
      "phrase": "Latent copies of the hazard: `…Controls/RotationControls.fs:119`",
      "spec_location": "§A.0",
      "evidence": "Read Controls/RotationControls.fs:117-120 -> `clickBox` sets `Border.name id` at :119.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 23,
      "phrase": "`…Controls/RayPositionControls.fs:71`",
      "spec_location": "§A.0",
      "evidence": "Read Controls/RayPositionControls.fs:69-72 -> `clickBox` sets `Border.name id` at :71.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 24,
      "phrase": "`…Controls/ElementPaletteControls.fs:55`",
      "spec_location": "§A.0",
      "evidence": "Read Controls/ElementPaletteControls.fs:53-56 -> `clickBox` sets `Border.name id` at :55.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 25,
      "phrase": "and `…Controls/ExperimentControls.fs:240,581`",
      "spec_location": "§A.0",
      "evidence": "Read Controls/ExperimentControls.fs -> `Border.name id` at :240 (optionBoxV) and :581 (actionButton). (The :240 site's own comment (:232-237) explains it keeps a STABLE name and only flips visibility — whether it is truly hazardous is a semantic judgement outside this audit; the cited lines exist as claimed.)",
      "verdict": "CONFIRMED"
    },
    {
      "id": 26,
      "phrase": "The screen users actually see is `TableAndElementRotationView.mainView` in `OpticalConstructor.TestWindows` (`…TestWindows/TableAndElementRotationView.fs:2367`)",
      "spec_location": "§B.0",
      "evidence": "Read TestWindows/TableAndElementRotationView.fs:2367 -> `let mainView (model : Model) (dispatch : Msg -> unit) : IView`; App/Program.fs:148 mounts it and the launcher's Main button opens that window (:176).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 27,
      "phrase": "hosted by `MainConstructorWindow` (`…App/Program.fs:118-150`)",
      "spec_location": "§B.0",
      "evidence": "Read App/Program.fs:118-150 -> `type MainConstructorWindow` at :118; mounts TableAndElementRotationView.mainView via Program.mkSimple at :148-150.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 28,
      "phrase": "the composition comment records that the old constructor shell \"is kept but no longer opened by the launcher's Main button\" (`…App/Program.fs:112-117`)",
      "spec_location": "§B.0",
      "evidence": "Read App/Program.fs:112-117 -> the comment ends '(The old constructor shell — `MainWindow` above — is kept but no longer opened by the launcher's Main button.)' at :116-117 — the quoted text exists inside the cited range.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 29,
      "phrase": "The operator's decision (005/Q26–Q27): the workbench REAL-MOVES into `OpticalConstructor.Ui` … and the seven diagnostic scenes … move to a tiny new executable `OpticalConstructor.TestWindows.App`",
      "spec_location": "§B.0",
      "evidence": "Inferred assumption-shape (cites an existing operator artefact). Grep specs/0038/.manual/005-comments.txt -> 'Q26. Agree on the proposal.' (:7) and 'Q27. Agree on the proposal to give test windows their own tiny executable.' (:8).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 30,
      "phrase": "the seven diagnostic scenes currently on the product launcher (`…App/Program.fs:177-224`)",
      "spec_location": "§B.0",
      "evidence": "Numeric-count claim re-derived: Read App/Program.fs:177-224 -> exactly 7 test-scene buttons declared there (tableTest :177, elementTest :184, tableElementTest :191, elementMovementTest :198, rendererTest :205, snapTest :212, snapReflected :219). Live count == cited count (7).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 31,
      "phrase": "`UserEnvironment.fs` is live (`…App/Program.fs:37`) and stays",
      "spec_location": "§B.1",
      "evidence": "Read App/Program.fs:37 -> `let settings = UserEnvironment.load (UserEnvironment.settingsPath ())` — consumed at startup.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 32,
      "phrase": "The Fluent theme seam the `App` type uses (`AppShell.themeVariant`, `…App/Program.fs:244`) survives as a function",
      "spec_location": "§B.1",
      "evidence": "Read App/Program.fs:244 -> `this.RequestedThemeVariant <- AppShell.themeVariant Startup.settings.theme` inside `type App` (:239).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 33,
      "phrase": "the rest of the dead shell (the `MainWindow` host, `Shell.fs`, `Ribbon.fs`, `MaterialsView.fs`, `ConstructionPage.fs`, `ConstructorView.fs`, `LifecycleView.fs`, `Templates.fs`, `Help.fs`, and every Ui module plus Ui.Tests test left without a live consumer by those deletions) is removed, not paralleled",
      "spec_location": "§B.1",
      "evidence": "Glob OpticalConstructor.Ui/*.fs -> Shell.fs, Ribbon.fs, MaterialsView.fs, ConstructionPage.fs, ConstructorView.fs, LifecycleView.fs, Templates.fs, Help.fs all exist; the `MainWindow` HostWindow is at App/Program.fs:87-110. Every named artefact slated for removal resolves.",
      "verdict": "CONFIRMED"
    }
  ]
}
