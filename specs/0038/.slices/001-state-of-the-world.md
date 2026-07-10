# Step 001 — state of the world

## Where we are

Step 001 opens spec 0038 (UI standardization) by retiring the write-once `StyledElement.Name` from
every generated or regenerable clickable Border in `OpticalConstructor.Controls` — the class of
control that FuncUI can recycle onto a different item's slot when a list's membership changes, which
Avalonia answers with the live Selector-bay crash "Cannot set Name : styled element already styled".
Every such box now carries the freely-mutable `AutomationProperties.AutomationId` (the documented
in-project `MaterialsControls.automationId` pattern), and every variable-membership generated list
item is keyed with `View.withKey` so a membership shift recreates a moved item instead of patching a
styled control in place. No behaviour change: labels, layout, handlers, and message flow are
untouched; headless tests that located these controls by `Name` switched to
`AutomationProperties.GetAutomationId`.

## What's working

- Convert LibraryControls.leafRow/actionButton, RotationControls.clickBox,
  RayPositionControls.clickBox, ElementPaletteControls.clickBox, and
  ExperimentControls.optionBoxV/actionButton/open-chart from Border.name to
  AutomationProperties.AutomationId (the MaterialsControls pattern).
- Key every variable-membership generated list item with View.withKey: Library rows/headers,
  palette buttons, experiment candidates/collection rows/result block, Materials and SampleLibrary
  rows/facets/verbs, Category rows.
- Rewire RotationControls' imperative key-highlight restyle to read GetAutomationId.
- Switch seven Ui.Tests files' Name lookups of converted controls to GetAutomationId.
- Add two headless recycling regressions: a leaf-row membership shift over a live Component, and
  the Selector bay driven through select, bind, and kind-change re-renders without a rename throw.

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this worker exits
  (IMPLEMENT Invariant 6 — the worker acts, it runs no checks). This session ran no gate commands
  as gates; the roster for this step is `build`, `unit-tests`, `constructor-unit-tests`,
  `ui-smoke`, `ui-tests`.
- Diagnostic verification (not gate authority): `dotnet build Berreman.slnx -c Release` succeeded
  with 0 errors and no new warning from any touched file; the full
  `OpticalConstructor.Ui.Tests` suite passed 437/437 (2 tests added, none removed, so neither
  `count_at_least` UI gate can regress from this slice); the solver and constructor test projects
  are untouched by this slice (changes are confined to `OpticalConstructor.Controls` +
  `OpticalConstructor.Ui.Tests`).
- The two NEW tests are `Trait("Category","ui-smoke")` render/interaction proofs in
  `LibraryControlsTests.fs`, matching that file's precedent: (1) a control-level rows
  [alpha;beta;gamma]→[beta;gamma]→back membership shift over a live re-rendering `Component` — the
  exact leaf-onto-leaf patch that used to throw; (2) the acceptance drive: Selector bay select
  (leaf click → pending), bind (Confirm → `valueId` committed), and two kind-change re-renders
  (polarizer added/selected → rows collapse; sample added/selected → rows rebuild), asserting the
  binding held and the rows stay locatable by AutomationId.

## Architecture

- **Ids-by-meaning survive control reuse.** Generated/regenerable clickables carry
  `AutomationProperties.AutomationId` (a styled attached property FuncUI may rewrite on reuse),
  never the write-once `StyledElement.Name`; each converting module hosts the same private
  `automationId` attr-builder helper (the established per-module pattern — no new shared module,
  matching MaterialsControls/SampleLibraryControls/CategoryControls/EmbeddedChart precedent).
- **Identity-keyed list items.** Every variable-membership generated item is keyed by its stable
  item identity (`View.withKey` → FuncUI `ViewKey`); FuncUI 2.0.0-preview1 diffs children
  positionally and RECREATES on key mismatch, so unchanged items still patch in place (no
  behaviour change) while cross-item reuse — the rename crash and the stale-visual-state class —
  is structurally eliminated.
- **Fixed-slot selectors stay fixed.** `ExperimentControls`' bounded variable selector keeps its
  all-present/visibility-toggled design (stable slots, coherent subscriptions); only its id kind
  changed. `Ribbon` (keyed pane, fixed tabs), `RendererControls` (static button), and
  `LayerBandsControls` (non-clickable, index-positional band names) keep `Name` — none is a
  generated/regenerable clickable, per the slice's enumeration.

## Deferred

- Nothing from this slice's scope. The remaining `Border.name` sites in Controls (Ribbon
  tabs/pane, Renderer swap button, LayerBands bands) are deliberate keeps, not deferrals — see
  Gotchas. The wider spec-0038 arc (shell retirement, workbench REAL-MOVE, etc.) belongs to later
  steps.

## Gotchas

- **The "Open chart window" Border was converted on a judgment call.** The slice's enumeration
  names optionBoxV/actionButton for ExperimentControls; the acceptance ("no generated or
  regenerable row/box … sets StyledElement.Name") also covers the regenerable open-chart box,
  whose named Border could cross-patch with the unnamed chart box when the Ψ/Δ readout toggles a
  slot shift. Converted and keyed; recorded in the impl-log.
- **`RotationControls`' key-press highlight locates buttons by AutomationId now** (`restyleBar` /
  `axisOfButtonId`). Anyone adding rotation buttons must give them the UiIds ids via the module's
  `automationId` helper or the highlight will skip them.
- **Mixed id kinds are intentional in Controls**: clickable generated boxes → AutomationId; text
  fields, readouts, and named container panels (list/tree/facet WrapPanels) → `Name` at fixed
  slots. Test helpers that collect id SETS therefore union `Control.Name` with
  `AutomationProperties.GetAutomationId`.
- **FuncUI key semantics**: `View.withKey` keys compare per-slot; a key change forces RECREATE
  (fresh subscriptions, fresh styling), which is exactly the desired semantics for
  variable-membership lists — do not key by list INDEX (a no-op) or by mutable display state.

## Changelog

- 2026-07-10 — Step 001 (IMPLEMENT): converted every generated/regenerable clickable Border in
  OpticalConstructor.Controls from Border.name to AutomationProperties.AutomationId
  (LibraryControls leafRow/actionButton, RotationControls clickBox + imperative restyle,
  RayPositionControls clickBox, ElementPaletteControls clickBox, ExperimentControls
  optionBoxV/actionButton/open-chart); keyed every variable-membership generated list item with
  View.withKey (Library, Palette, Experiments, Materials, SampleLibrary, Category); switched seven
  Ui.Tests files' Name lookups to GetAutomationId; added two headless recycling regressions
  (leaf-row membership shift; Selector bay select → bind → kind-change drive).

```yaml
gates:
  berreman_unit_tests: 0
  constructor_unit_tests: 0
  ui_smoke_tests: 0
  ui_tests: 0
```
