# Step 0014 — IMPLEMENT — state of the world

## Where we are

This is step 14 of spec 0050, the last step of section S1.2 ("A material's physics is its
complexity"), under obligation `S12-SWEEP`.

- Step 13 made `MaterialEntry.complexity` mandatory and derived `properties` from it.
- This step sweeps the three test-side texts that still described the old option. After it,
  `git grep -n -E "complexity = (None|Some)" -- Berreman/` finds nothing. That is the grep S1.3's
  invariant `s03-complexity-mandatory` runs after every step from S1.3's first step on.
- No product code changed.

## What's working

- Rewrite the two MaterialsWindowTests comments: Silicon is view-only because its complexity is not editable term data
- Document `siliconEntry` as the built-in whose dispersion is the published silicon law
- Rename the gyration-builder fact to "…draws all six components when every component is requested", assertions unchanged
- End the Ui.Tests project's MaterialEditorWindow comment at "Save round-trips through MaterialProxy"

## Tests

**No gate was run in this round.** Under Invariant 6 the gate engine is the only gate authority.
Read-only checks made:

- The `complexity = (None|Some)` grep over `Berreman/` finds nothing.
- The old fact name occurs nowhere outside `specs/`. The successor name occurs once
  (`NkDispersionChartTests.fs:195`) and matches the effects' `successor` exactly.
- The four s02 invariants find nothing.
- LF holds: numstat equals `--ignore-cr-at-eol`, and the files contain no CR.
- No `[<Fact>]` was added or removed, and no `ui-smoke` fact changed.

The gates should find:

- `build` green;
- `elevation` with no new finding (only test-project files changed);
- `ui-tests` passing over every compiled class, with the renamed fact;
- `ui-smoke` at 208.

```yaml
gates:
  ui_smoke_tests:         208
```

`ui_tests` is not carried. Its count depends on the step's class filter, so it is not
comparable with step 13's five-class run.

## Architecture

- No architectural change. The comments now state the mechanism that step 13 made the only one:
  - editability is `MaterialComplexity.isEditable`;
  - Silicon's `PublishedLawAxis` is not editable term data.
- The renamed fact describes `gyrationChart` given the full component list
  (`allGyrationComponents`). That list is what `gyrationComponentsOfComplexity` returns for a
  complexity that names no gyration class.

## Deferred

- Nothing. The slice's three items are all done, and the sweep grep finds nothing.

## Gotchas

- `MaterialsWindowTests.fs:484` keeps its assertion message "expected a selectable but
  non-editable preset". It is an assertion argument, not a comment, and the grep does not match
  it.
- From S1.3 on, any new text under `Berreman/` that says `complexity = None` or
  `complexity = Some`, comments included, fails the `s03-complexity-mandatory` invariant.

## Changelog

- 2026-11-05 — step 0014: swept the last `complexity = None/Some` mentions:
  - two MaterialsWindowTests comments;
  - the NkDispersionChartTests Silicon doc;
  - the Ui.Tests project comment.

  Renamed the gyration-builder "coded preset" fact to its successor. No product code changed.
