# Step 017 — state of the world

## Where we are

Step 017 lands spec 0038 Part G's Selector-bay rework on the step-016 Select-state machinery:
with an element selected, the bay's new **Choose…** verb opens the single-instance Library
window in Select state constrained to the element's kind — through the step-008
`WindowLauncher` (`SelectOpen`), honouring the step-005 modality switch, and re-targeting a
LIVE window on a second Choose… (the step-016 `Retarget` seam). The inline quick-pick strip
(today's kind-constrained rows + confirm panel via `LibraryControls`) renders ONLY while the
kind-constrained entry count sits BELOW the step-005 `QuickPickThreshold` (default 5); at or
above it the bay shows Choose… alone. BOTH paths converge on the SAME bind message: the
step-016 targeted `BindValueIdTo` is now the ONE arm that commits `placement.valueId` — the
strip's confirm and the legacy direct bind delegate to it. The workbench now populates the
step-016 `SelectSession` staleness handle from Choose… (reference-keyed bookkeeping via
`SelectSessionStarted`/`SelectSessionEnded`). Next in this part: step 019 gives the sample
editor its Choose material… verb over the same machinery.

## What's working

- Add the Selector bay's Choose… verb: opens/re-targets the kind-constrained
  Select-state Library window through WindowLauncher (new EditorLaunchers seam
  `openLibrarySelectWindow`), registers the SelectSession staleness handle.
- Gate the inline quick-pick strip behind the step-005 QuickPickThreshold:
  strip + Choose… below it, Choose… alone at/above (public `selectorOffer`).
- Converge BOTH bind paths on the ONE targeted `BindValueIdTo` commit arm
  (strip confirm and direct bind now delegate; bind cancels-and-closes a
  still-open Choose window).
- Stable ids `SelectorChooseButton` / `SelectorQuickPickStrip`; Model carries
  `quickPickThreshold` + `selectWindowModality` (defaults; step 47 threads
  appsettings).
- 8 new tests (4 pure + 4 headless incl. the identical-valueId acceptance and
  the re-target); build clean, suites 571 / 119 / 148 (+4) / 398 (+4).

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this worker exits
  (IMPLEMENT Invariant 6 — the worker acts, it runs no checks). The roster for this step is
  `build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`, `ui-tests`.
- Diagnostic verification (not gate authority): `dotnet build Berreman.slnx -c Release`
  succeeded with 0 errors, **no MSB3277**, and zero warnings from the touched projects
  (Ui / Ui.Tests) — the only warnings are the step-001-catalogued pre-existing set in
  untouched files (NU1701 Wolfram ×2, SYSLIB0051 vendored MathNet ×2, FS3873 Dispersion,
  FS1125 SeriesDataTests ×4, FS0044 Controls/ChartWindow). Suites: ui-smoke **148/148**
  (checkpoint 144, +4: over-threshold Choose…-alone with no strip, under-threshold strip +
  rows + Choose…, the identical-valueId acceptance through the REAL Choose…-opened Select
  window — banner, kind-constrained corpus, session handle held then cleared by the bind —
  and the second-Choose… re-target with the reference-keyed handle), ui-tests **398/398**
  (checkpoint 394, +4 pure: the strip-confirm/direct-bind ≡ targeted-bind model-equality
  convergence, the threshold gating incl. the at-threshold boundary, session start/end
  reference discipline, committed-bind cancels-and-closes incl. the vanished-target
  variant), OpticalConstructor.Tests **571/571** (== checkpoint), BerremanTests **119
  passed / 5 pre-existing skips** (== checkpoint). Logs in
  `specs/0038/.artifacts/017-diag-*.log`.
- Nothing deferred.

## Architecture

- **One commit site for `placement.valueId`**: `update` is `let rec` and the `BindValueId` /
  `ConfirmBindValueId` arms DELEGATE to `BindValueIdTo` — "both paths converge on the SAME
  bind message" is realized as one commit arm, provable as model equality. The strip's
  Confirm is a `LibraryControls` `unit -> unit` handler under a fixed `OnChangeOf` token
  (Controls is outside this step's touches), so a view closure carrying the pending id would
  go stale; the parameterless message + in-update resolution is the stale-safe shape.
- **Choose… composes in the VIEW**: the workbench runs `mkSimple`, so the render's `dispatch`
  is its only return path (the window hosts capture theirs with `Cmd.ofEffect`). The handler
  captures only render-stable state — the proxies, the launcher seam, the modality, and the
  element it re-keys its `OnChangeOf` subscription on — and resolves the modal owner from the
  pointer event's `TopLevel` at dispatch time.
- **Session bookkeeping is reference-keyed**: `SelectSessionStarted` stores the handle the
  seam returned; `onCancelled` dispatches `SelectSessionEnded` with its OWN session (a ref
  cell filled once the session exists), and the arm compares by reference — a session
  superseded by a re-target fires its cancel while the successor is stored, and the stale
  end is a no-op. A committed `BindValueIdTo` upgrades `activeSelect = None` to
  `cancelActiveSelect`: the strip can commit while the Choose… window is open (cancel AND
  close); on the window path the extra `Close()` is Avalonia's idempotent no-op.
- **The threshold gate counts what the strip would offer** (`allowedEntryIds` — the
  `entriesForKind` set the rows flatten from), strictly-below semantics; `SelectorOffer` is
  a named three-case DU (`QuickPickAndChoose | ChooseAlone | NoSelectorOffer`), and the
  no-selection bay keeps today's disabled `LibraryControls.empty` prompt (the
  WireUiComposition ids stay unconditional).
- **The seam returns the cancel lever, not the window**: `openLibrarySelectWindow` yields
  `(unit -> unit) option` — Created / Activated / Retargeted all reduce to "how to end the
  session" (`window.Close ()`), keeping the Elmish model window-free (the step-016
  `SelectSession` shape).

## Deferred

- The sample editor's Choose material… verb and its Select-return re-query — step 019
  (`BindMaterialToLayer` and the Materials window's Select state are ready).
- Threading the app-configured `QuickPickThreshold` / `SelectWindowsModal` into the product
  workbench Model (the fields default today; step 47 owns the composition acceptance).
- Live category names inside the Library window's lifted material-category facet (carried
  from step 015); `SampleLibraryControls` retirement sweep; the pre-existing warnings in
  untouched files (Part N).

## Gotchas

- **The task file's system-prompt path was stale again** — the IMPLEMENT worker prompt lives
  under `src/ai_strategy_generator/multistep/implement_worker.system-md` in the tool repo
  (the step-007..016 gotcha recurred); the slice's
  `TestWindows/TableAndElementRotationView.fs:768,780` reference resolved by symbol to the
  relocated `Ui/TableAndElementRotationView.fs` `BindValueId`/`ConfirmBindValueId` arms.
- **`ConfirmBindValueId` survives as a forwarder, not a commit** — it reduces to the
  identical `BindValueIdTo` in `update` (recorded interpretation of "the SAME bind
  message"): the strip's control-level Confirm cannot carry the ids without a stale-closure
  hazard under FuncUI's fixed-token re-subscription. Do NOT re-introduce a second
  `valueId`-writing arm.
- **`WorkbenchSettings.QuickPickThreshold.defaultValue` does not compile** — the single-case
  DU's case shadows the type under module-qualified long-ident resolution; open
  `OpticalConstructor.Domain.WorkbenchSettings` and use the unqualified type name (the
  `SelectWindowModality` long form works only because its cases are named differently).
- **`EditorLaunchers` gained a third field** (`openLibrarySelectWindow`) — an out-of-tree
  stub record must add it (the MainWorkbenchTests recording stub shows the shape).
- **Sample is an over-threshold kind now** (11 seeded entries ≥ default 5): a test that
  drives the INLINE strip with a Sample selected must widen `quickPickThreshold` in its
  fixture (`LibraryControlsTests.withWideQuickPick`) or pick an under-threshold kind
  (Detector: 2).
- **`open Avalonia.FuncUI` above `open Avalonia.FuncUI.Elmish` trips FS0893** in Ui.Tests —
  qualify `Avalonia.FuncUI.Component` instead of reordering the opens.
- **A test that opens a Select-state window must CLOSE it** (the app-global `WindowRegistry`
  gotcha, steps 008/013/015/016 — and the Choose… tests go through the REAL registry, so a
  leaked window would re-target into a later test).
- Step 002–016 carried-over gotchas remain valid (baselines come from `.checkpoints-json`,
  not this SoW's YAML; the FuncUI Elmish host skips a structurally-equal model; never pin a
  numeric assertion on a store thickness magnitude; the appsettings.json write-back into
  test output copies is expected; never fire a Select window's callbacks from anywhere but
  its update).

## Changelog

- 2026-07-11 — Step 017 (IMPLEMENT, attempt 1): Selector bay reworked onto the Select-state
  machinery — Choose… opens/re-targets the kind-constrained Select-state Library window
  through WindowLauncher (new `EditorLaunchers.openLibrarySelectWindow`, session handle via
  reference-keyed `SelectSessionStarted`/`SelectSessionEnded`); the inline quick-pick strip
  gated behind the step-005 QuickPickThreshold (public `selectorOffer`; stable ids
  `SelectorChooseButton`/`SelectorQuickPickStrip`); `BindValueIdTo` is the ONE commit arm of
  `placement.valueId` (strip confirm + direct bind delegate; a committed bind
  cancels-and-closes a still-open session). 8 new tests. Build clean (no MSB3277); suites
  571 / 119 / 148 / 398.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 571
  ui_smoke_tests: 148
  ui_tests: 398
```
