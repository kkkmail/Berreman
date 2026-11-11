# Step 0299 — state of the world

## Where we are

Spec 0050 section S9.2 covers import profiles and the column-mapping wizard (001 §K).

- Step 298 added the closed sets a profile is made of: delimiter, header row, byte-order mark, scan column unit, scan
  reading and `DataColumnNumber`.
- This step adds the profile itself, a named mapping of a lab file's layout and columns, and stores the profiles in
  the kit file (001 §E, §K).
- Step 300 adds the four built-in profiles as code, and step 309 adds the wizard.

## What's working

- Add `ScanColumn`, `ReadingColumns` (`SignalColumns` | `MuellerElementColumns`), `ColumnRoles` and `ImportProfile`
  to `Kit.fs`, between `DataColumnNumber` and `NamingTemplate`.
- Add `ImportProfileError`: seven `…Err of reason` cases, each with its `describe` text.
- Add `ImportProfileName.availableAmong`, the rule for naming a new profile. It refuses a built-in's name
  (`BuiltInProfileNameErr`) and a held profile's name, compared ordinally (`DuplicateProfileNameErr`).
- `KitFile` gains `importProfiles` (empty in `create`) and `withImportProfile`, which appends a profile and never
  replaces one.
- `kit.schema.json` version 1 requires `importProfiles`, with exact keys at every level and the tag ranges and column
  numbers checked.
- constructor-unit-tests rises by 2, to 1498. Two `KitFileJsonTests` facts now also cover the stored profiles.

## Tests

These were development runs of each gate's own command over the final build. The arc-runner's gate engine runs the
gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors, no `error` in stdout | 16 s (first full build 74 s) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded | 198 s | 600 s |
| constructor-unit-tests | Passed!, 1498 of 1498, 0 failed, 0 skipped | 69 s | 180 s |
| invariant-s30-\* (16) | each `INVARIANT HOLDS` (`section_invariants --find-nothing`, exit 0) | < 1 s each | 300 s |

- **constructor-unit-tests: 1498**, which is the 1496 baseline plus the 2 new facts.
  - New and `Passed` in the TRX: `a profile name is refused when a built-in or a kit profile holds it, and a kit file
    appends a new profile` and `every import profile error renders its condition and its payload`.
  - Changed and `Passed` in the TRX: `a kit file round-trips through its canonical text` and `a schema-invalid or
    malformed kit file is refused before it binds`.
  - Every other kit-file fact passes unchanged.
- **Mutation check (red).**
  - Change: the duplicate-name check was disabled, and `importProfiles` was dropped from the schema's required keys.
  - Result: the new naming fact failed ("a kit file replaced the profile it holds"). The schema fact failed ("expected
    SchemaValidationErr: Missing field … importProfiles").
  - Restore: both files were restored byte for byte (SHA-256 checked), and the solution was rebuilt before the runs
    above.

```yaml
gates:
  constructor_unit_tests: 1498
```

## Architecture

- **The profile types live in `Kit.fs`, ahead of `KitFile`,** so the kit file holds them directly.
  `ImportProfileName.availableAmong` is an intrinsic extension declared after `ImportProfile`, because a profile holds
  its name.
- **A profile's disk form is the shared `CanonicalJson.options`; nothing new was registered:**
  - the five closed sets are their int tags (step 298);
  - `DataColumnNumber` unwraps to an int, and an absent column is `null`;
  - `ReadingColumns` is the serializer's adjacent-tag object `{ "Case", "Fields" }`, as `DataRowSelection` already is
    in `experiment.schema.json`, so the schema says only "an object".
- **The schema writes the column rule once.** A local `$defs` holds `columnNumber` (integer ≥ 1) and
  `optionalColumnNumber` (`null` or a `columnNumber`). The schema version stays 1, because no committed kit file
  exists (.spec-md §7). No up-converter, default-on-read, replace or delete verb was added.
- **Raw data is immutable (001 §K).** An experiment names its profile (step 291), so `withImportProfile` only appends,
  and a held name is refused.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **Shared record labels.** `reading` labels both `ScanColumn` and `ColumnRoles`, and `name` labels both
  `ImportProfile` and `KitFile`. Annotate a record literal's type so F# does not resolve its labels by declaration
  order.
- **F# chained indexers.** `a[1]["x"]` and `f()[0]` fail with FS0597. Bind the intermediate value first, as
  `KitFileJsonTests` does.
- **A wrong `reading` case fails at binding, not in the schema.** It is refused when the serializer binds it, as
  `MalformedJsonErr`, because the schema checks only that `reading` is an object.
- **The `Lab scan` fixture is shared.** It is public in `KitTests` (`KitTests.labScanProfile`) and reused by
  `KitFileJsonTests`, which compiles after it. Change it in one place.
- **Most `ImportProfileError` cases have no producer yet.** The slice defines all seven. Only `BuiltInProfileNameErr`
  and `DuplicateProfileNameErr` are produced so far, by `availableAmong`.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0299: import profiles go into the kit file (S9.2).
  - `Kit.fs` gains `ScanColumn`, `ReadingColumns`, `ColumnRoles`, `ImportProfileError`, `ImportProfile` and
    `ImportProfileName.availableAmong`.
  - `KitFile` gains `importProfiles` and `withImportProfile`, which appends a profile and refuses a built-in's or a
    held profile's name.
  - `kit.schema.json` requires the profiles.
  - constructor-unit-tests rises by two, to 1498.
