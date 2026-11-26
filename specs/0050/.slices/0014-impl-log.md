# Step 0014 — IMPLEMENT — impl log

## Progress

- [x] Read the protocol (arc-runner base and the IMPLEMENT delta), the project prompt, the
      slice, its gates snapshot, step 13's state of the world and the `S12-SWEEP` oracle entry.
      The project prompt's Operator note section is empty.
- [x] `MaterialsWindowTests.fs`: rewrote the comments at `:459` and `:480`.
- [x] `NkDispersionChartTests.fs`: rewrote `siliconEntry`'s doc at `:27`. Renamed the fact at
      `:195` and rewrote its comment at `:198`.
- [x] `OpticalConstructor.Ui.Tests.fsproj`: the comment at `:120-124` now ends at "Save
      round-trips through MaterialProxy".
- [x] Read-only checks: the invariant grep, the old and new fact names, LF, and the four s02
      invariants.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialsWindowTests.fs`, 2 comment
  lines:
  - `:459` is now "Silicon is view-only (its complexity is not editable term data), so Edit is
    inert."
  - `:480` is now "Silicon is selectable (the panel shows it) but view-only: its complexity is
    not editable term data."
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/NkDispersionChartTests.fs`, 3 lines:
  - `:27` doc: "Silicon — a DISPERSIVE built-in: its dispersion is the published silicon law."
  - `:195` the fact is renamed `spec 0040 006: the gyration builder draws all six components when
    every component is requested`. This is the effects' `successor`, character for character.
  - `:198` comment: "The full set — what a complexity that names no gyration class draws."
  - The fact's body and both assertions are byte-identical.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`,
  1 line: `round-trips through MaterialProxy with complexity = Some). -->` became
  `round-trips through MaterialProxy). -->`. The comment was shortened, and no comment was
  added.

No product code changed. The test count is unchanged: the diff adds and removes no `[<Fact>]`.

## Decisions

- **The comments say why Silicon is view-only, as the slice words it.** Its complexity is not
  editable term data. That is the `MaterialComplexity.isEditable` predicate:
  - Silicon's axis is `PublishedLawAxis SiliconIndexLaw` (`MaterialLibrary.fs:407-412`);
  - `isEditable` maps that axis to `false` (`MaterialComplexityEditor.fs:866-880`).
- **The renamed fact's comment names what the full set means in the product.**
  `allGyrationComponents` is the set that `gyrationComponentsOfComplexity` returns for a
  complexity that names no gyration class (`NkDispersionChart.fs:144-158`). The comment no longer
  names a coded preset or a "fallback", and its line was rewritten in place, not added.
- **The assertion message at `MaterialsWindowTests.fs:484` ("expected a selectable but
  non-editable preset") is left alone.** It is an assertion argument, not a comment. It does not
  match the grep, and the slice limits the edit to the two comments.

## Testing state

**No gate was run.** Under Invariant 6 (`implement_worker.system-md`: "you act; you run no
checks") the arc-runner's gate engine is the only gate authority. This round made read-only
checks only:

- `git grep -n -E "complexity = (None|Some)" -- Berreman/` finds nothing (exit 1). That is S1.3's
  `s03-complexity-mandatory` grep, and the oracle's `S12-SWEEP` requirement.
- The old fact name (`for a coded preset with no stored class`) occurs nowhere outside `specs/`.
  The new name occurs once, at `NkDispersionChartTests.fs:195`.
- The four s02 invariants each find nothing: `elev-list-empty`, `no-vstest`, `no-kit-kit-json`
  and `no-eps-axis-evaluated`.
- LF holds:
  - `git diff --numstat` equals `--numstat --ignore-cr-at-eol` (2/2, 3/3, 1/1);
  - the three files contain no CR byte.
- The edited facts in `MaterialsWindowTests.fs` (`:452-484`) and the renamed NkDispersionChart
  fact carry no `ui-smoke` trait, so they are `ui-tests` facts. Both classes are in this step's
  ui-tests selection, and no ui-smoke fact changed.

What the gates should find:

- `build`: green. The changes are three comments, one doc line, one backtick-identifier rename
  using plain words, and one XML comment.
- `elevation`: no new finding. Only test-project files changed, and the test projects are
  outside the analyzers' scope.
- `ui-tests` and `ui-smoke`: the same counts as the tree before the step. The renamed fact keeps
  its assertions.

## Artifacts

- `.artifacts/0014-step-diff.txt`: the full `git diff -- Berreman/` of this step.

## Gotchas

- None from the operator. The project prompt's Operator note section is empty.
- **`ui_tests` has no carried baseline.** Its count depends on each step's class filter: step 13
  had five classes, and this step selects every compiled class. The engine's checkpoints record
  no `ui_tests` key, so the SoW's YAML carries only the keys that are comparable across steps.
