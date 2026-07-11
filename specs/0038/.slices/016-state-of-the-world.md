# Step 016 — state of the world

## Where we are

Step 016 opens spec 0038 Part G: *Selector = a catalogue window in a Select state*. Both
single-instance windows — Materials (step 013) and Library (step 015) — now carry ONE shared
Browse/Select mode DU from the new Domain `WindowMode` module: the Select case holds a
`SelectionContext` (the pre-applied, NON-REMOVABLE kind constraint, the typed target — table
element or sample-layer position — and the `onSelected` / `onCancelled` callbacks). The
step-008 `WindowLauncher` gained the Select-state re-target (a second Select open re-points
the LIVE single instance and returns the step-007-declared `RetargetedWindow`), and the
staleness rules landed in the receiving surfaces: the workbench cancels-and-closes the open
Select session on any table-selection change or the target's removal (the pending-bind-clears
precedent) and binds through the new TARGETED `BindValueIdTo`; the sample editor binds through
the new TARGETED `BindMaterialToLayer` — a vanished target is a no-op plus a status line on
both, never a throw. Next: step 017 wires the Selector-bay Choose…/quick-pick flow onto this
machinery; step 019 the sample-editor Choose material… verb.

## What's working

- Add Domain WindowMode (step 016): KindConstraint, SelectionTarget
  (TableElementTarget | SampleLayerTarget), SelectionContext with
  onSelected/onCancelled, LibraryWindowMode = Browse | Select — one generic
  DU shared by BOTH windows, never a copy.
- Give the Library AND Materials windows the Select state: the Select/Close
  pair (one row, positive/negative styling), the fixed no-chip constraint
  banner, the kind-constrained corpus (Library via forKinds; Materials
  structurally whole) — everything else stays the ordinary window.
- WindowLauncher: a Select-state open of a live key RE-TARGETS the single
  instance through the baked closure (RetargetedWindow); modal/modeless per
  the step-005 switch unchanged.
- Staleness + targeted returns: table-selection change / element add /
  removal cancels+closes the open Select window and fires onCancelled once;
  BindValueIdTo (workbench) and BindMaterialToLayer (sample editor) bind by
  id/position with the vanished-target no-op + status line.
- 24 new tests across five suites; diagnostics: build clean (no MSB3277),
  suites 571 / 119 / 144 (+9) / 394 (+15).

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this worker exits
  (IMPLEMENT Invariant 6 — the worker acts, it runs no checks). The roster for this step is
  `build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`, `ui-tests`.
- Diagnostic verification (not gate authority): `dotnet build Berreman.slnx -c Release`
  succeeded with 0 errors, **no MSB3277**, and zero warnings from the touched projects
  (Domain / Ui / Ui.Tests) — the only warnings are the step-001-catalogued pre-existing set in
  untouched files (FS3873 Dispersion, FS1125 SeriesDataTests ×4, SYSLIB0051 vendored
  MathNet ×2, FS0044 Controls/ChartWindow, NU1701 Wolfram.NETLink). Suites: ui-smoke
  **144/144** (checkpoint 135, +9: both windows' Select/Close pair + fixed-constraint banner
  with zero chips, the full Select→targeted-bind loops — Library→workbench readout,
  Materials→sample-editor layer row — the REAL canvas-click close-on-selection-change for
  BOTH windows, both vanished-target no-ops with status lines, and the launcher's
  second-Select-open re-target), ui-tests **394/394** (checkpoint 379, +15 pure: the mode's
  corpus constraint, Confirm/Cancel/Retarget/Dismiss exactly-once callback discipline for
  both windows, the workbench targeted bind + staleness cancels, the editor targeted bind +
  vanished-row no-op), OpticalConstructor.Tests **571/571** (== checkpoint), BerremanTests
  **119 passed / 5 pre-existing skips** (== checkpoint). Logs in
  `specs/0038/.artifacts/016-diag-*.log`.
- Nothing deferred.

## Architecture

- **One mode DU, generic over the entry** (`LibraryWindowMode<'entry>` /
  `SelectionContext<'entry>`): "the same window in code, never a copy" is realized by the
  type parameter — the Library window instantiates `LibraryEntry`, the Materials window
  `MaterialEntry`; `[<ReferenceEquality>]` on the context (function-valued callbacks) keeps
  every holding Elmish model's equality. The spec'd field name `constraint` is an F#
  reserved word, so the field is `kindConstraint`.
- **The constraint lives at the corpus seam, which IS its non-removability**: the Library
  window filters `liveEntries` through the existing `LibraryEntry.forKinds` (a compound
  polarizer serves both polarizer kinds — never re-derived), so no breadcrumb chip exists to
  remove and every count/offer/tree/selection already lives inside the constraint; the
  Materials corpus satisfies a sample-layer pick structurally (materials carry no
  CatalogueKind), so there Select changes only the surface (banner + pair) and the return
  path.
- **Exactly-once callback discipline via mode-flip-then-close**: Confirm/Cancel fire their
  callback, flip the mode to Browse in the SAME update, then request the close; the host's
  `Closed` hook dispatches `SelectDismissed` (Elmish ring-buffer processes it after the
  commit), which cancels only a STILL-pending session — so the title-bar X, a staleness
  `Close()`, and the two verbs share one path and `onSelected`/`onCancelled` can never both
  fire. The window hosts capture dispatch via `mkProgram` + `Cmd.ofEffect` (`Program.run` is
  synchronous in the ctor) and expose `Retarget` as the launcher's re-target seam.
- **Re-target = cancel the superseded session, keep the window**: the launcher's
  `SelectOpen` bakes `retargetWindow : Window -> unit` alongside the requesting window; on a
  live key it re-points the instance (the pure update fires the old context's onCancelled —
  "a second Choose closes the first", logically), clears the highlight, and keeps the user's
  browsing state.
- **The staleness owner is the requesting surface**: the workbench holds a
  `SelectSession` handle (`target` + `cancelAndClose`) OUTSIDE the window; selection change /
  add / removal invoke it, and closing the window is what fires onCancelled — one cancel
  path. The targeted messages (`BindValueIdTo`, `BindMaterialToLayer`) resolve their target
  by ID/position at RETURN time, so the modeless race (target vanishes, window still open)
  degrades to a no-op plus a status line (`WorkbenchSelectStatus` / the editor's existing
  status row) — never a throw. Step 017 populates the session from Choose…; nothing else
  opens Select windows yet.
- **`SampleStackEditor.isValidPosition` went public** as the vanished-row check — reused, not
  re-derived; the targeted material set is the same selection-shaped transform-and-restore
  dance as the per-layer orientation editor.

## Deferred

- The Selector-bay Choose… verb, the quick-pick threshold gating, and the two paths'
  convergence on the bind — step 017 (the machinery, targeted message and staleness rules are
  ready; `SelectSession.target` is there for its re-target-vs-fresh-open decision).
- The sample editor's Choose material… verb and the Select-return / activation re-query of
  its material list — step 019 (`BindMaterialToLayer` is its landing pad).
- Threading the app-configured `SelectWindowsModal` switch into product Select opens — the
  launcher honours it today; the composing sites land with steps 017/019 (step 47 owns the
  composition acceptance).
- Live category names inside the Library window's lifted material-category facet (carried
  from step 015).
- `SampleLibraryControls` retirement sweep; the pre-existing warnings in untouched files
  (Part N).

## Gotchas

- **The task file's system-prompt path was stale again** — the IMPLEMENT worker prompt lives
  under `src/ai_strategy_generator/multistep/implement_worker.system-md` in the tool repo
  (the step-007..015 gotcha recurred); the slice's
  `TestWindows/TableAndElementRotationView.fs:941-944` reference resolved by symbol to the
  relocated `Ui/TableAndElementRotationView.fs` `PointerUp` arm.
- **`LibraryWindowView.init` / `MaterialsWindowView.init` now take the mode** and both window
  CONTEXTS gained `requestClose` — an out-of-tree substitute must add the field and pass
  `Browse` (or a `Select context`). The window ctors gained optional `?mode` BEFORE the
  optional thresholds; positional callers are unaffected.
- **`WindowOpenMode.SelectOpen` changed shape** to
  `requestingWindow * retargetWindow : (Window -> unit)` — existing `SelectOpen requester`
  sites become `SelectOpen (requester, ignore)` when no re-target is composed.
- **Do not fire a Select window's callbacks from anywhere but its update** — the exactly-once
  discipline relies on the mode flip committing before the `Closed`-hook dismissal message is
  processed. If a future host needs an extra close path, dispatch `SelectDismissed`; never
  call `onCancelled` directly.
- **A test that opens a Select-state window must CLOSE it** (the step-008/013/015 app-global
  registry gotcha extends to Select opens — and an unclosed Select window would also leak a
  pending session whose callbacks outlive the test).
- **The workbench status line is AutomationId'd and keyed, never `Name`d** — it has variable
  membership (renders only while set), so `TextBlock.name` would re-trip the Part A
  write-once-Name hazard the repo already cured once.
- Step 002–015 carried-over gotchas remain valid (baselines come from `.checkpoints-json`,
  not this SoW's YAML; the FuncUI Elmish host skips a structurally-equal model; never pin a
  numeric assertion on a store thickness magnitude; the appsettings.json write-back into test
  output copies is expected).

## Changelog

- 2026-07-11 — Step 016 (IMPLEMENT, attempt 1): added Domain `WindowMode.fs` (KindConstraint /
  SelectionTarget / SelectionContext<'entry> / LibraryWindowMode<'entry>) and gave BOTH
  catalogue windows the Browse/Select mode — Select/Close pair with positive/negative
  styling, fixed no-chip kind-constraint banner, kind-constrained corpus (Library via
  forKinds), exactly-once onSelected/onCancelled; WindowLauncher SelectOpen now re-targets a
  live single instance (RetargetedWindow); workbench gained the SelectSession staleness
  handle (cancel+close on selection change / add / removal), the targeted `BindValueIdTo`
  and the `WorkbenchSelectStatus` line; sample editor gained the targeted
  `BindMaterialToLayer` over the now-public `isValidPosition`. 24 new tests. Build clean
  (no MSB3277); suites 571 / 119 / 144 / 394.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 571
  ui_smoke_tests: 144
  ui_tests: 394
```
