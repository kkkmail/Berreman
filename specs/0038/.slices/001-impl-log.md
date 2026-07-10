# Step 001 — impl-log

## Progress

- [x] `LibraryControls.fs` — leafRow/actionButton → AutomationId; rows + headers keyed.
- [x] `RotationControls.fs` — clickBox → AutomationId; restyleBar/axisOfButtonId read AutomationId.
- [x] `RayPositionControls.fs` — clickBox → AutomationId.
- [x] `ElementPaletteControls.fs` — clickBox → AutomationId + keyed.
- [x] `ExperimentControls.fs` — optionBoxV/actionButton/openChart → AutomationId; candidates/collection/result-block keyed.
- [x] `MaterialsControls.fs` — clickBox/verbButton keyed.
- [x] `SampleLibraryControls.fs` — clickBox/verbButton keyed.
- [x] `CategoryControls.fs` — rows keyed.
- [x] Ui.Tests Name→AutomationId lookup switches (7 files).
- [x] New Selector-bay recycling tests (control-level shift + select/bind/kind-change drive).
- [x] Diagnostic local build + affected-test run (see Testing state).

## Files modified

`OpticalConstructor.Controls` (8 files):

- `LibraryControls.fs` — new private `automationId` helper (the `MaterialsControls` pattern);
  `leafRow` and `actionButton` drop `Border.name` for `automationId`; leaf rows are keyed by their
  entry-derived id and header rows by a `LibraryHeader_<depth>_<label>` key (`View.withKey`); the
  Confirm/Cancel action buttons are keyed by their stable ids.
- `RotationControls.fs` — new `automationId` helper; `clickBox` drops `Border.name`; the imperative
  key-highlight path (`axisOfButtonName` → renamed `axisOfButtonId`, and `restyleBar`) now reads
  `AutomationProperties.GetAutomationId` instead of `b.Name` (behaviour identical — the ui-smoke
  highlight test still drives it); the reset-swap comment updated to name AutomationIds. The three
  degree `TextBox`es keep `TextBox.name` (stable ids, fixed membership, not clickable Borders).
- `RayPositionControls.fs` — new `automationId` helper; `clickBox` drops `Border.name`. The metres
  `TextBox` and the readout `TextBlock` keep their `Name`s.
- `ElementPaletteControls.fs` — new `automationId` helper; `clickBox` drops `Border.name` and is
  keyed by its id (covers the generated add buttons AND the trailing Remove-selected that shifts
  when `addItems` change).
- `ExperimentControls.fs` — the `automationId` helper moved above its first use; `optionBoxV`,
  `actionButton`, and the "Open chart window" Border drop `Border.name`; `idOptionBox` (candidates)
  keyed by its automation id; collection rows keyed by experiment id; the conditional result-block
  items (Ψ/Δ readout, Open-chart button, chart box, description) keyed so a Ψ/Δ toggle recreates the
  shifted items instead of cross-patching them. The variable selector KEEPS its visibility-toggling
  fixed-slot design (no behaviour change) — only its id kind changed.
- `MaterialsControls.fs` — `clickBox` (rows + facet options) and `verbButton` (the verb row loses
  its Edit member for a view-only selection) keyed by their automation ids.
- `SampleLibraryControls.fs` — same two helpers keyed.
- `CategoryControls.fs` — each category row (`rowView`) keyed by its category id.

`OpticalConstructor.Ui.Tests` (7 files):

- `RotationControlsTests.fs` — `borderNamed` → `borderWithId` (locates by `GetAutomationId`).
- `RayPositionControlsTests.fs` — the mounted-ids assertion collects `Name` AND AutomationId.
- `LibraryControlsTests.fs` — `findConfirm` locates by AutomationId; NEW module-level `borderWithId`
  + `clickIn` helpers; NEW test `a leaf-row membership shift re-renders without a styled-element
  rename throw` (control-level [alpha;beta;gamma]→[beta;gamma]→back over a live `Component`); NEW
  test `the Selector bay survives select, bind, and kind-change re-renders` (live main view: click
  a leaf → pending, click Confirm → bound, `AddElement LinearPolarizer` → rows collapse,
  `AddElement Sample` → rows rebuild; asserts the binding held and rows stay locatable).
- `ExperimentControlsTests.fs` — the addButton and openChart lookups use the existing dual
  `matchesId` helper.
- `TableAndElementRotationTests.fs` — `visibleNames` collects both id kinds; the r2Plus and
  Remove-selected lookups use `GetAutomationId`.
- `ElementRotationTests.fs`, `TableRotationTests.fs` — the r2Plus lookups use `GetAutomationId`.

## Testing state

Gate execution belongs to the arc-runner's deterministic gate engine after this worker exits
(IMPLEMENT Invariant 6 — the worker acts, it runs no checks). Diagnostic verification only, not
gate authority:

- `dotnet build Berreman.slnx -c Release` — **Build succeeded, 0 errors.** No new warning from any
  touched file; the warnings in the log (`FS3873` Dispersion.fs, `FS1125` SeriesDataTests.fs,
  `FS0044` ChartWindow.fs:78, `NU1701` Wolfram.NETLink, `SYSLIB0051` vendored MathNet) all
  pre-exist this slice and are in files this slice did not touch.
- Affected Ui.Tests modules (`--filter FullyQualifiedName~…` over the 11 touched/adjacent test
  modules): **149/149 passed**, including the two NEW recycling tests.
- Full `OpticalConstructor.Ui.Tests` run (both categories): **437/437 passed** (2 added, none
  removed — no `count_at_least` regression possible from this slice).
- `git diff --numstat` equals `--numstat --ignore-cr-at-eol` — no CRLF churn (LF preserved).

## Artifacts

None required (no captured logs / screenshots this round; the diagnostic runs are summarised above).

## Gotchas

- **The conversion set is the slice's enumerated seven plus one judgment call.** The
  "Open chart window" Border (`ExperimentControls`, was `Border.name UiIds.openChart`) is a
  clickable, REGENERABLE box inside the conditional result block; when the Ψ/Δ readout toggles, the
  chart-block Borders shift one slot and FuncUI would patch a named Border into the unnamed chart
  box (or vice versa) — the same styled-rename/reset throw class. The acceptance says NO generated
  or regenerable row/box sets `StyledElement.Name`, so it was converted (and the result-block items
  keyed). Recorded here because the slice text's enumeration did not name it explicitly.
- **Deliberately NOT converted** (not generated/regenerable clickables): `RendererControls.clickBox`
  (a single static Swap button, stable label), `LayerBandsControls.bandView` (not clickable, and its
  per-INDEX names are positional — slot `i` is always `LayerBand_i`, so a membership change can
  never rename in place; an index key would be a no-op), and `Ribbon` tabs/pane (fixed bay
  membership; the pane is already the `View.withKey` precedent). `Border.name` remaining in
  Controls after the sweep: exactly these four sites.
- **`RotationControls`' live restyle reads ids, not Names.** The task-009 keyboard highlight finds
  the axis buttons imperatively; it now matches `AutomationProperties.GetAutomationId` — miss this
  and the highlight silently dies (the existing ui-smoke key-press test guards it, and passed).
- **FuncUI 2.0.0-preview1 `withKey` semantics**: children are diffed positionally and a `ViewKey`
  mismatch at a slot RECREATES the control (no reorder-matching). Keys are the item's stable
  identity (entry/material/sample/category/experiment id), so unchanged items still patch in place
  — no behaviour change; only cross-item reuse is eliminated.
- **`TextBox.name` / `TextBlock.name` / panel `Name`s stay.** The slice targets clickable Borders;
  text fields, readouts, and named container panels keep stable `Name`s at fixed slots (several
  tests locate them by `Name`, which is why the switched test helpers match Name OR AutomationId
  where they collect mixed sets).
- **No operator note was in flight** (the project prompt's Operator note section is empty).
