# Step 012 — impl log (ADD_COMPONENT UICOMP_XDUO_0008 FacetedTreeControls)

## Progress

- [x] Read the ADD_COMPONENT worker prompt, the project prompt, the slice spec,
  spec `.spec-md` (Parts D/E), and the step 009–011 states of the world.
- [x] `FacetedTreeControls.fs` (State + Handlers + UiIds + view) authored.
- [x] Controls fsproj compile entry added.
- [x] `FacetedTreeControlsTests.fs` (pure contract + headless proofs) authored.
- [x] Ui.Tests fsproj compile entry added.
- [x] Diagnostic build + suites run (NOT gate authority — Invariant 6).
- [x] State-of-the-world written.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/FacetedTreeControls.fs` — NEW:
  the domain-free control (State/Handlers/UiIds/view).
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/OpticalConstructor.Controls.fsproj`
  — compile entry after EmbeddedChart.fs.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/FacetedTreeControlsTests.fs` — NEW:
  2 pure contract facts (untagged → `ui-tests`) + 4 headless proofs (`ui-smoke`).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
  — compile entry after WindowLauncherTests.fs.

## Decisions

- **Handler argument shapes** (the slice names the handlers but not their args):
  `applyConstraint group value` (the same discrete key can appear under two
  facets, so the group is part of the identity — also encoded in the derived
  offered-value automation id), `removeConstraint chipCode`,
  `commitTextFilter boxText`, `chooseRepresentation code`, `requestBuild ()`,
  `selectNode nodeCode`, `applyManualRange group rawText`.
- **Manual range is ONE raw-text box per numeric offer group** committing on
  Enter/LostFocus; the HOST parses "min-max" and applies an ordinary constraint
  chip (spec §D.0). Two separate min/max boxes would need cross-box reads at
  commit time or per-keystroke draft state — both worse shapes.
- **`TreeNode.countOpt`**: branch rows read `label (count)` (expanded AND
  collapsed — "unexpanded branches carrying their counts"); heading/leaf rows
  the host projects without a count read the bare label. Optionality is in the
  `…Opt` name per CLAUDE.md.
- **No selected-node field in State** — the slice's State enumeration does not
  include one; `selectNode` dispatches the code and the host decides what
  selection means (it can re-project highlighting through labels later).
- **Gated mode generates ZERO node row views** (not hidden ones): the tree
  container itself is replaced by the Show/Search button; the two alternatives
  carry different `View.withKey` keys so the swap recreates cleanly.
- **Nothing in this control sets `StyledElement.Name`** — every id is an
  `AutomationProperties.AutomationId` and every generated element is keyed
  (`View.withKey`), including the singletons that swap membership (tree ↔
  button). Codes/labels are host-flattened raw strings and counts raw ints —
  the established Controls-project projection seam (MaterialsControls /
  CategoryControls precedent); modes are named two-case DUs
  (`TreeMaterialization`, `NodeExpansion`, `ManualRange`), never bools.
- **Test tagging follows the MaterialsControls/CategoryControls convention**:
  pure contract facts untagged (gate `ui-tests`), mounted structure proofs
  tagged `ui-smoke`.
- **No registry edit**: `.contract-ids/XDUO-json` and `specs/0038/.contracts-json`
  already carry UICOMP_XDUO_0008 (declared, step 12); the supervisor maintains
  both (step-007 precedent).

## Testing state

Gate authority is the arc-runner's deterministic gate engine (ADD_COMPONENT
Invariant 6 — the worker acts, it runs no checks). Diagnostic verification run
by this worker (advisory only):

- `dotnet build Berreman.slnx -c Release` — 0 errors, **no MSB3277**; the only
  warnings are the step-001-catalogued pre-existing set in untouched files
  (FS1125 SeriesDataTests ×4, FS3873 Dispersion, FS0044 ChartWindow,
  SYSLIB0051 vendored MathNet ×2, NU1701 Wolfram.NETLink ×2). Zero warnings
  from the new files.
- ui-smoke (`--filter Category=ui-smoke`): **128/128** (checkpoint 124; +4 —
  the four mounted FacetedTreeControls proofs).
- ui-tests (`--filter Category!=ui-smoke`): **363/363** (checkpoint 361; +2 —
  the two pure contract facts).
- BerremanTests: **119 passed / 5 pre-existing skips** (== checkpoint).
- OpticalConstructor.Tests: **559/559** (== checkpoint).

First ui-smoke run had 2 failures (Enter commit dispatched TWICE — FuncUI
subscribes `onKeyDown` on both the Tunnel and Bubble passes); fixed in the
control by `e.Handled <- true` before the Enter dispatch (`commitOnEnter`) and
re-run green.

## Artifacts

- `specs/0038/.artifacts/012-diag-build.log`
- `specs/0038/.artifacts/012-diag-ui-smoke.log`
- `specs/0038/.artifacts/012-diag-ui-tests.log`
- `specs/0038/.artifacts/012-diag-unit-tests.log`
- `specs/0038/.artifacts/012-diag-constructor-tests.log`

## Gotchas

- **The task file's system-prompt path was stale again** — the ADD_COMPONENT
  worker prompt lives under
  `src/ai_strategy_generator/multistep/add_component_worker.system-md` in the
  tool repo (the task file pointed at the repo root); located and read in full
  together with the shared base `arc-runner.system-md` (the step-007..011
  gotcha recurred).
- **FuncUI `onKeyDown` fires on BOTH the Tunnel and Bubble passes** (the same
  duplicate-pass behaviour the `clickBox` comment documents for pointer
  events): an Enter commit without `e.Handled <- true` dispatches twice. The
  older `RotationControls` / `ExperimentControls` Enter-commit sites never
  noticed because their commits are value-setting (idempotent); a
  dispatch-observing test sees the double. `commitOnEnter` handles the event
  before dispatching.
- **`window.KeyTextInput` + `Focus()` work headlessly** (Avalonia.Headless
  12.1.0) — the typing tests drive the real input pipeline; no per-keystroke
  dispatch is observable because the control subscribes NO text-change
  handler at all.
- The checkpoint baselines come from `.checkpoints-json`, not the SoW YAML;
  the window registry is app-global test state in Ui.Tests; the
  appsettings.json write-back into test output copies is expected (carried
  from steps 002–011).
