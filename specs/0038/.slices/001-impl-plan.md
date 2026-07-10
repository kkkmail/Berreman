# Step 001 — impl-plan

## Goal

Convert every generated or regenerable clickable Border in `OpticalConstructor.Controls`
from the write-once `StyledElement.Name` (`Border.name`) to the freely-mutable
`AutomationProperties.AutomationId`, following the in-project `MaterialsControls.automationId`
pattern (MaterialsControls.fs:139-146), and key every variable-membership generated list item
with `View.withKey`. NO behaviour change: labels, layout, handlers, and message flow stay
identical; headless tests that located these controls by `Name` switch to
`AutomationProperties.GetAutomationId`.

## Conversion set (clickable, generated or regenerable)

- `LibraryControls.leafRow` (Border.name at :112) and `LibraryControls.actionButton` (:148)
  — the live Selector-bay crash.
- `RotationControls.clickBox` (:119) — plus its imperative restyle path (`restyleBar` /
  `axisOfButtonName`) which READS `b.Name` to find the axis buttons; it must read the
  AutomationId instead (behaviour identical).
- `RayPositionControls.clickBox` (:71).
- `ElementPaletteControls.clickBox` (:55).
- `ExperimentControls.optionBoxV` (:240) and `ExperimentControls.actionButton` (:581)
  — the module already documents the AutomationId precedent (:261-266).
- `ExperimentControls` "Open chart" Border (:431) — a regenerable clickable Border inside the
  conditional result block; judged in-scope per the acceptance ("no generated or regenerable
  row/box sets StyledElement.Name") and it carries a latent rename/reset throw when the
  Ψ/Δ readout toggles and shifts the chart-block Borders one slot.

NOT converted (judged not generated/regenerable): `RendererControls.clickBox` (a single static
"Swap renderer" button), `LayerBandsControls.bandView` (not clickable; index-positional names
can never be renamed in place), `Ribbon.tab`/`pane` (fixed bay membership; the pane is already
the `View.withKey` precedent).

## View.withKey (variable-membership generated list items)

- `LibraryControls`: leaf rows + header rows (keys: entry id / derived header key).
- `ElementPaletteControls.clickBox`: keyed by its automation id (covers the add buttons and the
  trailing Remove-selected that shifts when addItems change).
- `ExperimentControls`: `idOptionBox` (candidates) keyed by automation id; collection rows keyed
  by experiment id; the conditional result-block items keyed so a Ψ/Δ toggle recreates instead of
  cross-patching.
- `MaterialsControls.clickBox` / `verbButton`: keyed by automation id (rows, facet options, and
  the verb row whose Edit member is removed for a view-only selection).
- `SampleLibraryControls.clickBox` / `verbButton`: keyed by automation id.
- `CategoryControls.rowView`: row keyed by category id.
- Skipped: `LayerBandsControls` bands (identity IS the position — an index key is a no-op).

## Test updates (Ui.Tests)

Switch strict `.Name =` lookups of converted controls to `AutomationProperties.GetAutomationId`:
`RotationControlsTests.borderNamed`, `RayPositionControlsTests` (names set),
`LibraryControlsTests.findConfirm`, `ExperimentControlsTests` (:421 addButton, :715 openChart),
`TableAndElementRotationTests` (visibleNames set, :434 r2Plus, :472 removeSelected),
`ElementRotationTests` (:223 r2Plus), `TableRotationTests` (:274 r2Plus).

New headless coverage in `LibraryControlsTests.fs`:
1. a control-level membership-shift re-render (rows [A;B;C] → [B;C] → [A;B;C] through a live
   `Component`) — the exact leaf-onto-leaf patch that threw "Cannot set Name : styled element
   already styled" before the sweep;
2. the acceptance drive: the Selector bay through select (click a leaf → pending), bind (click
   Confirm → valueId committed), and kind-change re-renders (add/select a polarizer → rows
   collapse; add/select a sample → rows rebuild) over a live re-rendering component, asserting
   the rows stay locatable by AutomationId and the binding took.

## Risks

- FuncUI 2.0.0-preview1 diffs children positionally and RECREATES on ViewKey mismatch — keys
  must be stable per item identity (entry/category/experiment ids) so unchanged items still
  patch in place.
- `RotationControls.restyleBar` must keep restyling the axis buttons live (key-press highlight
  test guards this) — reading `GetAutomationId` instead of `Name`.
- The `View` module name is ambiguous with `Avalonia.FuncUI.Types.View<'t>` in these files —
  fully qualify `Avalonia.FuncUI.DSL.View.withKey` (the Ribbon precedent).
- Gates: build + all four test suites run by the arc-runner's gate engine after exit
  (IMPLEMENT Invariant 6); local build/test runs are diagnostic only.
