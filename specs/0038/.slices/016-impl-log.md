# Step 016 — impl log (IMPLEMENT, AC-G1)

## Progress

- [x] Read task file, worker system prompt (`implement_worker.system-md` — resolved under
  `src/ai_strategy_generator/multistep/`, the task file's path was stale again), shared base
  (`arc-runner.system-md`), project prompt, slice spec, spec `.spec-md`, steps 013/015/017/019
  for scope boundaries, and the current window/launcher/workbench/editor code.
- [x] Domain: new `WindowMode.fs` (KindConstraint / SelectionTarget / SelectionContext<'entry> /
  LibraryWindowMode<'entry>) + fsproj entry; `SampleStackEditor.isValidPosition` made public
  (the vanished-layer check reuses it).
- [x] WindowLauncher: `SelectOpen` now carries the baked `retargetWindow : Window -> unit`
  closure; a Select-state open of a LIVE key re-targets + activates and returns the
  already-declared `RetargetedWindow`; throws map to `WindowFactoryFailed (key, "retarget
  failed: …")`. Docs updated (header, outcome case, openOrActivate field, create).
- [x] LibraryWindowView: `mode` in the Model, `requestClose` in the context, the
  `ConfirmSelect / CancelSelect / RetargetSelect / SelectDismissed` messages, the
  corpus-seam constraint (`constrainedEntries` over `LibraryEntry.forKinds`), the Select/Close
  action row + fixed-constraint banner (`LibrarySelectButton` / `LibrarySelectCloseButton` /
  `LibrarySelectConstraint`), `selectedEntry` resolving through the constrained corpus.
- [x] LibraryWindow host: `?mode` ctor param, `mkSimple` → `mkProgram` with a `Cmd.ofEffect`
  dispatch capture, `Closed` hook dispatching `SelectDismissed`, public `Retarget` member
  (the launcher's re-target seam).
- [x] MaterialsWindowView + MaterialsWindow: the exact mirror over `MaterialEntry`
  (`MaterialsSelect*` ids); the material corpus satisfies the kind constraint structurally,
  so Select changes no projection — banner + pair + return path only.
- [x] Workbench (TableAndElementRotationView): `SelectSession` handle
  (`target : Library.ElementId`, `cancelAndClose`), Model fields `activeSelect` /
  `selectStatus`, staleness cancels on PointerUp selection change / AddElement /
  removeSelected, the TARGETED `BindValueIdTo (ElementId, entryId)` message (vanished element
  → no-op + status), the `WorkbenchSelectStatus` status line under the readout
  (AutomationId + keyed — never `Name`; Part A discipline).
- [x] SampleEditorView: the TARGETED `BindMaterialToLayer (LayerPosition, MaterialId)` message —
  live row → the selection-shaped transform-and-restore dance (the SetLayerOrientation
  precedent); vanished row → no-op + the existing `SampleEditorStatus` line.
- [x] Tests (see Testing state): LibraryWindowTests (5 pure + 4 headless), MaterialsWindowTests
  (4 pure + 4 headless), WindowLauncherTests (re-target acceptance + `SelectOpen` call-site
  updates), MainSceneMsgTests (4 pure scene staleness/bind tests), SampleEditorWindowTests
  (2 pure targeted-bind tests); existing helpers updated (contexts gained `requestClose`,
  `init` now takes the mode).
- [x] Diagnostic build + all four suites green.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/WindowMode.fs` (NEW)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/SampleStackEditor.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/WindowLauncher.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/LibraryWindowView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/LibraryWindow.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialsWindowView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialsWindow.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/SampleEditorView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LibraryWindowTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialsWindowTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/WindowLauncherTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MainSceneMsgTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SampleEditorWindowTests.fs`

## Testing state

Gates are executed by the arc-runner's deterministic gate engine after this worker exits
(IMPLEMENT Invariant 6 — the worker acts, it runs no checks). Roster: `build`, `unit-tests`,
`constructor-unit-tests`, `ui-smoke`, `ui-tests`.

Diagnostic verification (not gate authority):
- `dotnet build Berreman.slnx -c Release` — **0 errors**, no MSB3277, ZERO warnings from
  touched projects; only the step-001-catalogued pre-existing set in untouched files (FS3873
  Dispersion, FS1125 SeriesDataTests ×4, SYSLIB0051 vendored MathNet ×2, FS0044
  Controls/ChartWindow, NU1701 Wolfram.NETLink).
- `BerremanTests` **119 passed / 5 pre-existing skips** (== checkpoint 119).
- `OpticalConstructor.Tests` **571/571** (== checkpoint 571).
- ui-smoke **144/144** (checkpoint 135, +9 — with `--blame-hang-timeout 120s`, no hang).
- ui-tests **394/394** (checkpoint 379, +15).

## Artifacts

- `specs/0038/.artifacts/016-diag-constructor-unit-tests.log`
- `specs/0038/.artifacts/016-diag-ui-tests.log`
- `specs/0038/.artifacts/016-diag-ui-smoke.log`
- `specs/0038/.artifacts/016-diag-unit-tests.log`

## Gotchas

- The task file's system-prompt path `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md`
  does not exist; the real file is
  `C:\GitHub\AI-Strategy-Generator\src\ai_strategy_generator\multistep\implement_worker.system-md`
  (the step-007..015 gotcha recurred). The slice's
  `TestWindows/TableAndElementRotationView.fs:941-944` precedent line-ref is stale too — the
  pending-bind-clears code lives in `Ui/TableAndElementRotationView.fs`'s `PointerUp` arm
  after the Part B relocation; resolved by symbol.
- **`constraint` is an F# reserved word** — the spec'd `SelectionContext.constraint` field is
  `kindConstraint` (recorded in the type's doc; the alternative was ugly double-backticks in
  every consumer).
- **`SelectionContext<'entry>` is generic** — that is what makes ONE mode DU serve both windows
  ("the same window in code, never a copy"): the Library window instantiates `LibraryEntry`,
  the Materials window `MaterialEntry`. `[<ReferenceEquality>]` (function-valued callbacks)
  keeps every holding model's Elmish-required equality.
- **Materials window + kind constraint**: materials carry no `CatalogueKind`, and every
  material is sample-layer-eligible, so the pre-applied constraint narrows NOTHING there — it
  is satisfied structurally by the corpus and shows as the fixed banner only (non-removability
  is structural; the tests pin zero breadcrumb chips). The Library window applies it at the
  corpus seam through the existing `LibraryEntry.forKinds` (never re-derived).
- **onSelected/onCancelled exactly-once discipline**: `ConfirmSelect`/`CancelSelect` flip the
  mode to Browse IN THE SAME UPDATE that fires their callback and then request the close; the
  host's `Closed` hook dispatches `SelectDismissed`, which Elmish processes AFTER that commit
  (ring-buffer dispatch), so it finds Browse and no-ops. The title-bar X and a staleness
  `Close()` reach a still-pending session through the same hook — ONE cancel path.
- **Re-target fires the superseded session's `onCancelled`** ("a second Choose closes the
  first" — the logical session ends; the window instance stays) and clears the highlight; the
  user's browsing state (filter, chips, representation) is deliberately kept.
- **Close-on-selection-change for the MATERIALS window** is proven through the SAME workbench
  session machinery (the handle is window-agnostic — `cancelAndClose` closes whichever window
  the session points at). In product flow the Materials Select session will hang off the
  sample editor (step 019); the slice's staleness rule names only the TABLE selection, so no
  speculative editor-side selection-watcher was built.
- **Nothing in the workbench OPENS a Select window yet** — step 017's Choose… composes the
  `SelectOpen` launcher and populates `activeSelect`; step 016 lands the machinery
  (launcher re-target, session handle, targeted messages, staleness) and proves it headless
  with test-composed sessions, exactly as the slice orders the steps.
- **The vanished-target headless proof intentionally registers NO session** — the staleness
  rules normally close the window before the target can vanish; the targeted return's no-op +
  status line is the belt-and-braces path for the modeless race, so the test creates exactly
  that race.
- `EditorLaunchers` is UNCHANGED this step (no new fields) — but `LibraryWindowContext` /
  `MaterialsWindowContext` gained `requestClose`, and `LibraryWindowView.init` /
  `MaterialsWindowView.init` now take the mode; out-of-tree substitutes must follow.
- Step 002–015 carried-over gotchas remain valid (baselines from `.checkpoints-json`; the
  registry is app-global in tests — every opened window is closed; never pin a numeric
  assertion on a store thickness magnitude; the appsettings.json write-back into test output
  copies is expected).
