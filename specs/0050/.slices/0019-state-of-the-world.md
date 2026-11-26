# Step 0019 — ADD_RED — state of the world

## Where we are

Spec 0050, section S1.3: "Every library entry is identified by a frozen GUID". Steps 15 to 18
gave every seeded preset one home, `Library.PresetIds`, and switched both test suites to it, so
the ids' text now lives in one place. This step declares RED_0004 (001 §B "Identity is GUID +
version + hash", `.spec-md` §3.3): every seeded Library entry id, and every source id
`newSourceId` mints, is the canonical string of a GUID. Today the samples already are; the
detector, polarizer and source presets carry text ids, and `newSourceId` prefixes `src-`. The
paired CLOSE_RED gives the presets their GUIDs and makes `newSourceId` mint a plain one.

## What's working

- Added the fact `every seeded Library entry id is the canonical string of a GUID, and no two are equal`
- Added the fact `newSourceId mints the canonical string of a fresh GUID`
- `Add source opens the editor on a freshly minted source id` now requires the minted id to be a canonical GUID string
- Observed RED_0004: exactly these three facts fail, every other fact of both classes passes

## Tests

The arc-runner's gate engine is the gate authority. These are local observations made to record
the red:

- `build`: exit 0, 0 errors; only the two pre-existing vendored MathNet SYSLIB0051 warnings.
- `constructor-unit-tests` (SourceLibraryTests): 8 facts, 2 failed, 6 succeeded. The failures
  are the two declared reds: 33 of 44 seeded ids (`det-intensity`, `pol-lp`, `src-600`, …) are
  not GUIDs, and `newSourceId` gave `src-<guid>`. Whole suite: 738, 2 failed, 736 succeeded.
- `ui-tests` (LightSourceEditorTests): 7 facts, 1 failed, 6 succeeded. The failure is the declared
  red: the minted id `src-<guid>` is not the string of a GUID. Whole suite: 560, 1 failed (that
  red), 559 succeeded.
- `ui-smoke`: Passed!, 208 of 208.
- `elevation`: `ELEV-GATE: OK` (test projects are outside the ELEV scope).
- The five s03 invariants each find nothing.

Details and the failing output are in `0019-impl-log.md` (Testing state).

```yaml
gates:
  constructor_unit_tests: 736
  ui_smoke_tests:         208
  ui_tests:                 6
```

## Architecture

- No production change, so no architectural decision.
- The facts fix the contract for the CLOSE_RED: an entry id is canonical when
  `Guid.TryParse` succeeds and the parsed GUID's `string` (lowercase, hyphenated, the form
  `SampleId.toElementValueId` writes) equals the id. A braced, upper-case or prefixed id fails.
- The facts name no new symbol, so they hold whatever literals the CLOSE_RED writes into
  `PresetIds`.

## Deferred

Nothing in this step's scope. The green belongs to the paired CLOSE_RED, as the spec orders it.

## Gotchas

- The canonical-GUID check exists once per test project (a private helper in
  `SourceLibraryTests`, inline in the UI fact): the authorized effects allow only the two `.fs`
  files, and the Domain has no such check.
- The slice's citation `ElementId.fs:594` for `newSourceId` has drifted to `:650`; the name finds
  it.
- Test-local user-source ids (`src-user-1`, `src-new-1`, `src-mine`, …) stay text: the stores
  take any id, and only seeded and minted ids are under the rule.
- The ui-tests selection here is one class (7 facts), so its count is not comparable with step
  18's five-class 36.

## Changelog

- 2026-11-05 — Step 0019: declared RED_0004. Seeded entry ids and `newSourceId`'s ids must be
  canonical GUID strings; today the presets' text ids and the `src-` prefix fail.
