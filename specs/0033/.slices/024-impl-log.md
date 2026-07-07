# Impl log — spec 0033, slice 024 (IMPLEMENT: Materials + Library workbench bays)

## Progress

- [x] Read task file, worker system prompt, project prompt, slice spec, gates snapshot
- [x] Context: prior SoWs (022/023), TableAndElementRotationView, MaterialsControls,
      SampleLibraryControls, LayerBandsControls, Ribbon, MaterialLibrary/ElementId proxies,
      editor windows, NkDispersionChart, App/Program.fs, Ui.Tests idioms
- [x] Impl plan written
- [x] TDD red: MainWorkbenchTests.fs + updated pins written FIRST; red build captured to
      `.artifacts/024-red-tdd.log` (FS0039/FS0003 naming the missing production symbols:
      `BayNames.library`, `EditorLaunchers`, `materialsState`, `materialCategoryOfCode`,
      4-argument `initMainWith`, …)
- [x] Production code (see Files modified)
- [x] Gates (advisory local runs) — all five green
- [x] State of the world

## Files modified

- `OpticalConstructor.TestWindows/OpticalConstructor.TestWindows.fsproj` — the four editor
  files (`SampleEditorView/Window.fs`, `MaterialEditorView/Window.fs`) moved BEFORE
  `TableAndElementRotationView.fs` so the workbench Add/Edit verbs construct the real editor
  windows without forward references (they depend only on Domain/Controls/NkDispersionChart —
  verified before the move).
- `OpticalConstructor.TestWindows/NkDispersionChart.fs` — gains `inlineCanvas (autoId) (chart)`:
  the inline dual-axis FuncUI canvas REAL-MOVED from `MaterialEditorView.previewCanvas`,
  parametrized by automation id (shared by the editor preview and the Materials View panel).
- `OpticalConstructor.TestWindows/MaterialEditorView.fs` — `previewCanvas` re-pointed to
  `NkDispersionChart.inlineCanvas UiIds.previewChart`; the private canvas body + constants
  removed. Its preview id and `previewRange` are unchanged (tests untouched).
- `OpticalConstructor.TestWindows/TableAndElementRotationView.fs` — the bulk:
  - `RemoveConfirm<'id>` (confirm-gated remove carrying the requested id) and
    `EditorLaunchers` (`[<ReferenceEquality>]` launcher seam; `defaults` opens the real
    step-022/023 editor windows) before `Model`;
  - Model + 13 fields: `materials`/`samples` write seams, `materialQuery`/`sampleQuery`,
    `selectedMaterial`/`selectedSample`, `materialRemoveConfirm`/`sampleRemoveConfirm`,
    `materialsError`/`samplesError`, `viewedMaterial`/`viewedSample`, `launchers`;
  - `BayNames.materials`/`BayNames.library` + `all` (workbenches after Selector; Details
    stays LAST — the 0027/026 pin);
  - `DefaultStores` nested module (scoped `open …Domain.Library` for the optional
    `MaterialProxy.createInMemory` extension); `initWith`/`initMainWith` take the two new
    proxies; `init`/`initMain` build default stores;
  - `Mat…` × 10 / `Smp…` × 10 Msg arms + update arms (side effects through the launcher seam,
    store writes through the proxies; query/selection changes disarm a stale confirm);
  - `sampleBandsState` extracted PUBLIC from `detailsState` (both use it);
  - the workbench section: `WorkbenchIds`, public facet code maps
    (`materialCategoryCode/OfCode`, `dispersionFilterCode/OfCode`, `substrateFacetCode/OfCode`),
    public `materialsState`/`samplesState` projections (re-query the proxy per render),
    private handlers (code→domain lifting at the control boundary), inline confirm rows,
    inline message rows, View panels, and the two bay content builders; `mainBays` + 2 bays.
- `OpticalConstructor.App/Program.fs` — `open OpticalConstructor.Domain.Library` + the two
  stores built in `MainConstructorWindow` and passed to the 4-argument `initMainWith`
  (mechanical; the final WIRE_UI step owns the composition acceptance).
- `OpticalConstructor.Ui.Tests/MainWorkbenchTests.fs` (NEW) — 12 pure + 5 headless tests.
- `OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` — compile entry.
- `OpticalConstructor.Ui.Tests/LibraryControlsTests.fs` — the step-014 "no bay labelled
  Library" pins updated: "Library" IS offered again, as the samples workbench (both the pure
  roster assert and the headless tab assert).
- `OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs` — the exact-roster pin extended to
  the nine-bay roster.

## Decisions

1. **Bay order**: `[ …; selector; materials; library; experiments; details ]` — the
   workbenches sit with the Selector (the binding / collection bays) and the Details bay stays
   LAST, preserving the 0027/026 `LayerBandsControlsTests` pin (first attempt appended the
   workbenches at the end and that pin caught it — order corrected instead of obsoleting a
   second pin).
2. **Window opening is an injected launcher seam** (`EditorLaunchers` on the Model,
   `defaults` = the real windows) rather than a module-level mutable hook: per-test
   substitution by record update, no shared mutable state, and the headless Add/Edit proof can
   still construct the REAL editor windows while recording them.
3. **`SmpMakeMultilayer` opens the step-022 editor on a NEW sample** (same as Add): the slice
   letter assigns only Add/Edit explicitly; the stack editor's MakeRepeatBlock + repeat-count
   steppers ARE the make-a-multilayer flow, so a separate creation surface would duplicate it.
   Recorded as the interpretation most consistent with the surrounding code.
4. **The Library-bay View panel reuses the Details bay's band construction verbatim**
   (`sampleBandsState`, extracted from `detailsState`): the slice says "as the Details bay
   already renders", so the ×N-collapsed representation of the expanded stack is kept (the
   Details bay never expands `Repeated` groups into physical layers either).
5. **The inline n/k canvas REAL-MOVED to `NkDispersionChart.inlineCanvas`** (parametrized
   automation id) instead of duplicating it in the workbench or making the workbench depend on
   an editor-view private: the chart module is the natural home and compiles before both hosts.
6. **Facet picks carry DOMAIN types in `Msg`** (`MaterialCategory option`,
   `DispersionFilter`, `SubstrateKind option`); the control's string codes are lifted at the
   handler boundary (`…OfCode`), so no bare code string enters the model.
7. **`searchMaterials`/`searchSamples` with the empty query serve the list-all case** —
   `listMaterials`/`listSamples` remain the store-level assertions' seam; the bay drives ONE
   query seam as the step-003/004 design intends.

## Testing state

All five gates pass in ADVISORY local runs (Invariant 6: the arc-runner gate engine re-runs
them authoritatively after exit):

- `build` — Release/x64, exit 0, 0 errors (91 warnings: the pre-existing
  MSB3277/NU190x/SYSLIB0051/FS3873 noise; 023 recorded 93).
- `unit-tests` — 119 passed, 5 skipped, 0 failed (= 119 baseline; no core file touched).
- `constructor-unit-tests` — 407 passed, 0 failed (= baseline; no Domain code touched).
- `ui-smoke` — 81 passed, 0 failed (76 baseline + 5 new headless proofs).
- `ui-tests` — 306 passed, 0 failed (294 baseline + 12 new pure tests).

## Artifacts

- `specs/0033/.artifacts/024-build.log`
- `specs/0033/.artifacts/024-unit-tests.log`
- `specs/0033/.artifacts/024-constructor-unit-tests.log`
- `specs/0033/.artifacts/024-ui-smoke.log`
- `specs/0033/.artifacts/024-ui-tests.log`
- `specs/0033/.artifacts/024-red-tdd.log` — the TDD red capture

## Gotchas

- `MaterialLibrary.MaterialId.tryCreate` is unreachable through the qualified path: in
  expression position `MaterialLibrary.MaterialId` names the union CASE, not the type (the
  documented type/case collision) — the row handler parses the Guid and constructs the case
  directly.
- `MaterialProxy.createInMemory` is an OPTIONAL type extension (declared in module `Library`,
  the type lives in `MaterialLibrary`), so it is visible only where
  `OpticalConstructor.Domain.Library` is OPEN — hence the scoped `DefaultStores` nested module
  in the view file and the plain open in `Program.fs`. `SampleProxy.createInMemory` is
  intrinsic (type + augmentation both in `Library`) and works fully qualified.
- Headless row clicks must land inside the list's 220 px scroll viewport: the tests narrow the
  search first so the target row is at the top (rows order by Guid, which is arbitrary).
- Two `LayerBandsControls` instances can now exist in the tree (Details bay + the Library
  bay's View panel) with the same control names; the panel renders only while `viewedSample`
  is set, and the new test scopes its band assertions to `SampleViewPanel` descendants.
- The task file's system-prompt path `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md`
  does not exist (the 015–023 drift); the real file is under
  `src\ai_strategy_generator\multistep\`.
- `.manifest.state.json` (modified, CRLF) and the untracked `.claude/` folder are the
  arc-runner's / harness's own files — left alone, as in slices 001–023.
