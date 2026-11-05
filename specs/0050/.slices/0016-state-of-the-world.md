# Step 0016 — IMPLEMENT — state of the world

## Where we are

This is step 16 of spec 0050, in section S1.3 ("Every library entry is identified by a frozen
GUID"), under obligation `S13-CONSTRUCTOR-FIXTURES`.

- Step 15 put every seeded preset id in `Library.PresetIds`.
- This step switches the constructor suite to name those presets through the members. The ids
  still carry their step-15 text, so every fact keeps its meaning and its result.
- Step 20 mints the GUIDs, and it can now change `PresetIds` alone without touching these nine
  test classes.

## What's working

- Name every seeded preset in nine constructor test classes through `Library.PresetIds`
- Hold fixture ids as `ElementValueId` in `PlacedSampleEmissionTests`, `PolarizerBehaviorTests` and `SourceLibraryTests`, with no `.value` round trip
- Leave only non-preset `src-` ids in the suite: test-local user sources, the "src-t" fixture and the `SourceSpecId`s

## Tests

**No gate was run in this round.** The gate engine is the only gate authority. The local
construction checks agree with the step's acceptance:

| Check | Result | Note |
|---|---|---|
| build | exit 0, 0 errors, 0 warnings | Incremental build. |
| constructor-unit-tests | 736/736 | Equals the last checkpoint. |
| elevation | `ELEV-GATE: OK` | The test projects are out of the analyzers' scope. |
| five s03 invariants | find nothing | |

- No `Fact`, `Theory`, `Trait` or `InlineData` line was added or removed.

```yaml
gates:
  constructor_unit_tests: 736
```

## Architecture

- **No product code changed.** Only the nine test files in the slice's authorized effects were
  edited.
- **A test helper that looked a preset up by id text now takes an `ElementValueId`.** It
  compares ids directly: `p.id = id`, `e.entryId = id`, `s.id = id`.
- **Files that do not open a module name it qualified**:
  - `Library.PresetIds.*` in the two Scene test files;
  - `Placement.ElementValueId` in `PolarizerBehaviorTests`.

  This is the same form step 15 and `ExperimentRunTests` already use, so no new `open` was
  added.

## Deferred

- Nothing. Every item of the slice landed.

## Gotchas

- **`SourceLibraryTests.userSource` still takes a `string` id** and wraps it. The slice keeps the
  test-local user-source ids, and names no change to that helper.
- **`PolarizerBehaviorTests` does not open `Placement`.** A bare `ElementValueId` annotation
  there fails with FS0039.
- **The protected-ids fact still compares `string list`s** (the `.value` of each id, sorted), as
  the slice specifies. It will keep working after step 20 mints the GUIDs.

## Changelog

- 2026-11-05 — step 0016: switched nine constructor test classes to name the seeded presets
  through `Library.PresetIds`, with fixtures and helpers holding `ElementValueId`. The id text,
  every fact and the 736-test count are unchanged.
