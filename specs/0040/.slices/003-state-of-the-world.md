# Step 003 — state of the world

## Where we are

Step 003 makes the **selected** faceted-tree node **visibly distinct** (operator). Building on
step 001's alphabetical tree and step 002's collapsible, collapsed-by-default expansion, the
domain-free `FacetedTreeControls` gains one pure `selectedCode : string` on its `State` (empty =
none, the `activeRepresentation` convention). `nodeRows` paints the row whose `code` equals
`state.selectedCode` with the existing `chosenBackground` (was `idleBackground`) plus a thicker
border — a non-hue, colourblind-safe cue — while every other row stays idle. Both host windows
project `selectedCode` from the Model's already-tracked selected node code
(`MaterialsWindowView` from `m.selectedId`, `LibraryWindowView` from `m.selectedEntryId`, each via
`entryNodeCode`). No handler signature changed: `selectNode` already existed, so selection stays a
host-owned concern and the control merely reads the projected code.

## What's working

- Add `selectedCode : string` to `FacetedTreeControls.State` (empty = none) and initialise it in `empty`.
- Paint the selected tree row in `nodeRows` with `chosenBackground` plus a thicker border (a non-hue,
  colourblind-safe cue), threading `selectedCode` through the recursion and `treeArea`.
- Project `selectedCode` in both windows from the Model's tracked selected id — headless-verified
  that exactly the clicked entry leaf renders chosen + thicker while a sibling stays idle.
- Add a control highlight proof plus, per window, a pure `selectedCode` projection proof and a
  headless highlight proof; no test removed.

## Tests

- Gate execution belongs to the arc-runner's deterministic gate engine after this worker exits
  (IMPLEMENT Invariant 6 — the worker acts, it runs no checks). This session ran no gate commands as
  gates; the roster for this step is `build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`,
  `ui-tests`.
- Diagnostic verification (NOT gate authority): `dotnet build Berreman.slnx -c Release` — 0 errors,
  no new warning from any touched file (the 4 warnings are pre-existing exempt NU1701 Wolfram.NETLink
  and SYSLIB0051 vendored MathNet); `OpticalConstructor.Ui.Tests` — 656/656 across both categories;
  `OpticalConstructor.Tests` — 675/675 (Domain untouched).
- Net Ui-test delta: +5 new tests (1 control highlight proof; per window a pure projection proof and
  a headless highlight proof), none removed — so no `count_at_least` gate can regress. The three
  headless proofs carry `Category=ui-smoke`; the two pure proofs fall under `ui-tests`, so both count
  gates rise.

## Architecture

- **The control renders; the host owns the selection.** The highlight lives in the domain-free
  control, but the selected-node CODE lives in each host Model (the same functional-proxy split as
  `selectNode`/`toggleNode`/`ExpandedNodes`). The control never remembers a selection — it highlights
  exactly the `selectedCode` the host projects, so behaviour stays testable without a window.
- **Non-hue cue = border thickness, not a label glyph.** A leading marker glyph would change the
  rendered label TEXT (which `textOf`/`labelInside` read, per the step-002 gotcha); a thicker border
  is achromatic, testable via `Border.BorderThickness.Top`, and leaves every existing text assertion
  valid. The slice permitted either.
- **Selection highlighting is display-only.** Projecting `selectedCode` disarms no gate and moves no
  state; `SelectEntry` already tracked the selection, so this round only reads that existing id — the
  highlight is a pure projection of Model state.

## Deferred

- Nothing from this slice's scope. Later slices of spec 0040 are separate rounds and out of scope here.

## Gotchas

- **A leading marker glyph was rejected on purpose.** Prepending a glyph to the label text would
  break the `textOf window (treeNode code)` / `labelInside` reads that assert exact row labels across
  the control and both window suites. The border-thickness cue keeps the label text untouched — read
  `Border.BorderThickness.Top` (a uniform `Border.borderThickness 3.0` sets `.Top = 3.0`) to verify it.
- **Adding a field to `State` breaks every FULL construction — there are exactly four:** `empty`,
  the control test's `knownState`, and both windows' `facetedState`. All four now set `selectedCode`;
  copy-and-update sites (`{ knownState with … }`) are unaffected. A future `State` field must update
  the same four.
- **The highlighted row's pointer handler re-subscribes on `(code, isSelected)`** (was `code`),
  matching the `clickBox` discipline; the handler payload is unchanged, so this is defensive against a
  reused restyled box keeping a stale handler, not a behaviour change.
- **Working-tree files are uniformly CRLF on this Windows checkout** (untouched files too — this is
  pre-existing, not introduced). `git diff --numstat` equals `git diff --numstat --ignore-cr-at-eol`
  on every touched file, so no CR-only churn was introduced; the Edit tool preserved each file's
  existing convention and commit-time `.gitattributes` normalises to LF.

## Changelog

- 2026-07-12 — Step 003 (IMPLEMENT): made the selected faceted-tree node visibly distinct — a
  `selectedCode : string` on `FacetedTreeControls.State`, `nodeRows` painting the matching row with
  `chosenBackground` plus a thicker (non-hue, colourblind-safe) border, both windows projecting
  `selectedCode` from the Model's tracked selected id, and a control proof plus per-window pure and
  headless highlight proofs (+5 Ui tests, none removed).

```yaml
gates:
  berreman_unit_tests: 0
  constructor_unit_tests: 0
  ui_smoke_tests: 0
  ui_tests: 0
```
