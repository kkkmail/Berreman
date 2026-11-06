# Step 0045 state of the world

## Where we are

Spec 0050 S1.7 now has catalogue-backed test doubles ready for the section's
re-point steps. S17-FIXTURE-DOUBLES is implemented in the shared fixture and its
named regression fact. Formal gate execution and acceptance remain with the
arc-runner.

## What's working

- Added fresh material, sample and category stores filled from Built-in files.
- Added a fresh library containing every sample followed by every preset.
- Added ordered source access and source lookup with missing-id diagnostics.
- Reported refused writes with the entry's name, id and diagnostic reason.
- Proved catalogue membership, source tuning and per-call sample-store isolation.

## Tests

Local Release/x64 solution build passed with zero warnings and zero errors,
including the fixture linked into OpticalConstructor.Ui.Tests. All 800
constructor tests, 208 UI smoke tests and 44 selected non-smoke UI tests passed
with no failures or skips. The constructor report contains the exact new fact,
`the fixture's store doubles hold the Built-in catalogue and are isolated per call`,
and both pre-existing fixture facts. The selected UI report contains exactly
AppContextTests (5) and LibraryWindowTests (39). The smoke count is unchanged.

Formal gates remain pending with the supervisor: build, elevation,
constructor-unit-tests, ui-smoke, ui-tests, invariant-s07-elev-list-empty,
invariant-s07-no-vstest, invariant-s07-no-kit-kit-json,
invariant-s07-no-eps-axis-evaluated, invariant-s07-complexity-mandatory,
invariant-s07-no-seeded-string-preset-ids and invariant-s07-no-seeding-seam.
No formal gate result is claimed.

The YAML preserves the latest available inherited checkpoint values (step 43:
799 constructor tests and 208 smoke tests). No checkpoint records ui_tests;
zero preserves the preceding handoff's absence marker. These are not this
round's observed counts; baseline updates belong to the supervisor.

```yaml
gates:
  constructor_unit_tests: 799
  ui_smoke_tests: 208
  ui_tests: 0
```

## Architecture

Each store helper creates the existing empty in-memory store and fills it
sequentially through its own write verb, reusing the fixture's catalogue decode
helpers. The sole cache remains the existing lazy catalogue load. The material
and category reference callbacks and VersionsInUse are passed through unchanged.
The library returns the empty store's proxy after inserting samples and presets;
its grouping trees remain empty by that store's contract. Source access filters
only SourceItem entries and retains row order. No product code, seed value,
durable format or migration changes apply.

## Deferred

No implementation requirement was deferred. Formal supervisor acceptance is
pending, with no waiver or carried failure.

## Gotchas

- Store listings use internal ordering, so id assertions sort both sides;
  insertion and source order remain the catalogue's.
- The required store errors have no Domain render member. Exhaustive matches
  preserve their diagnostic payloads without raw DU formatting.
- The initial local build exposed multiline nested OR-pattern syntax errors;
  ordinary outer match alternatives fixed them before the green rebuild.
- Logs and TRX reports are under specs/0050/.artifacts/0045-local-*; the initial
  failed build log is retained alongside the successful rebuild log.
- Local validation follows the coding-session instructions despite the worker
  role's act-only wording; formal gate collections remain empty.
- Only the two authorized F# files changed, with no whitespace errors or CRLF
  churn. The supervisor's existing .manifest.state.json edit was left intact.
- No version bump, git mutation, external-repository write or catalogue edit occurred.

## Changelog

- 2026-11-05: Added six Built-in fixture helpers and the catalogue/isolation fact; local build, 800 constructor tests, 208 smoke tests and 44 selected UI tests passed.
