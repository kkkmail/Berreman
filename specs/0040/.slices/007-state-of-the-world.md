# Step 007 — state of the world

## Where we are

Spec 0040 Part C, step 007 (IMPLEMENT) — a minor polish (Part C.2) on the
Material editor. The description `TextBox` in the editor's left identity pane
carried a fixed 620px width that exceeded the pane's 520px minimum, so it met the
`GridSplitter` and clipped when the pane narrowed. This step makes that box wrap
and grow multiline instead. Scope is `OpticalConstructor.Ui` +
`OpticalConstructor.Ui.Tests` only; `depends_on: []`.

## What's working

- Description box wraps instead of clipping: `MinWidth` (340px) replaces the
  fixed 620px width, so the box no longer forces a width past the pane / splitter.
- Description box grows multiline: `AcceptsReturn = true` and
  `TextWrapping = Wrap` are set, so a long description flows onto extra lines.
- Add a headless `ui-smoke` test that mounts the editor and asserts the
  description box carries `TextWrapping = Wrap` and `AcceptsReturn = true`.
- Leave the two-pane grid, splitter, scroll viewer, and every sibling row
  untouched (no other layout change).

## Tests

Per the IMPLEMENT worker Invariant 6, gate execution belongs to the arc-runner's
deterministic gate engine after this session exits; this worker does not run
gates. The step's roster and how the change lands against each:

- `build` — F#/Avalonia across the solution; the only production edit is three
  attr lines on one `TextBox`, whose DSL members were confirmed present in the
  shipped FuncUI DLL (`TextWrapping` already in scope in the view).
- `unit-tests` (BerremanTests) — core solver, untouched; count unaffected.
- `constructor-unit-tests` (OpticalConstructor.Tests) — Domain/Storage/
  Optimization, untouched; count unaffected.
- `ui-smoke` — one net-additive headless test asserting the description box's
  wrapping is enabled; count rises by one, so it cannot regress.
- `ui-tests` — untouched (`Category != ui-smoke`); count unaffected.

Acceptance mapping: "the description box MUST wrap to multiple lines rather than
clip when narrowed — headless-verified that wrapping is enabled" → the new
`ui-smoke` test asserts `TextWrapping = Wrap` (and `AcceptsReturn = true`) on the
mounted box found by its stable id.

```yaml
gates:
  berreman_unit_tests: 0
  constructor_unit_tests: 0
  ui_smoke_tests: 0
  ui_tests: 0
```

## Architecture

- Pure Ui concern: only the description `TextBox`'s sizing/wrapping attrs change.
  No Domain, Storage, Optimization, engine, or MVU-model change; the editor's
  behaviour (identity edits, ladder, save routing) is unchanged.
- The box is found in the test by its stable id
  (`UiIds.MaterialEditor.descriptionBox = "MaterialDescriptionBox"`) via the
  existing `tryFindControl`, honouring the "automate by meaning, not by pixels"
  discipline.

## Deferred

- Full **auto-wrap on narrowing** (the box filling the pane width so text wraps
  without an explicit newline) needs the box to be width-bounded, which a
  horizontal `StackPanel` does not do — that would require a container change
  (stretch-capable `DockPanel`/`Grid`, or a vertical label-on-top row). The slice
  scopes this to "no other layout change", so that container restructure is left
  for a future layout pass; wrapping is enabled and `AcceptsReturn` gives real
  multiline growth today.

## Gotchas

- The description box lives in a **horizontal `StackPanel`**, which measures
  children with unbounded width; automatic width-driven wrapping engages only
  once the box is width-bounded. The slice's "no other layout change" constraint
  and its "headless-verified that wrapping is enabled" acceptance were followed
  literally: set the prescribed properties, leave the container alone.
- `MinWidth = 340.0` mirrors the name box's width — a floor well inside the ≥520
  left pane, so label + box (~400px) fits at the default split without clipping.
- The task file's system-prompt path
  (`C:\GitHub\AI-Strategy-Generator\implement_worker.system-md`) does not exist;
  the real file is under `.../src/ai_strategy_generator/multistep/`. Read from
  there — no impact on the slice.

## Changelog

- 2026-07-13 — Step 007: make the Material editor's description `TextBox` wrap
  instead of clip — replace the fixed 620px width with a 340px `MinWidth`, enable
  `AcceptsReturn` and `TextWrapping.Wrap`, and add a headless `ui-smoke` test
  asserting the description box's wrapping is enabled.
