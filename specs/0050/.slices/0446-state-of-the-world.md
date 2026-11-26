# Where we are

Step 0446 is in S13.1 and carries obligation S131-SOURCES-LOCATION. Step 443 added the `MaterialSourceProxy`
seam, step 444 added the pulls, and step 445 added the refractiveindex.info readers. This step gives the online
sources' offline snapshots a data location of their own. Like every other 001 §G location, it is read from
`appsettings.json` and listed in the Data locations window. Step 447's real refractiveindex.info adapter stores
its snapshot under this folder.

# What's working

- Added `DataLocations.materialSourcesFolder`, which defaults beside the library database to
  `%LOCALAPPDATA%/Softellect/Berreman/OpticalConstructor/Sources`.
- Added `DataLocationKind.MaterialSourcesLocation`, listed last. The Data locations window shows it as "Online
  source snapshots" with Open folder.
- `AppConfig` reads `MaterialSourcesFolder` like the other folders: a missing or refused value has its default
  written back, and the value is expanded and rooted at the install folder.
- In the test run the folder is `TestRun/Sources`, and `TestAppScope` puts it under the owner's folder, so no test
  reaches `%LOCALAPPDATA%`.
- Widened the five named facts to the new location. No test was added, renamed or removed.

# Tests

Each gate ran with its own command and selection. The supervisor's gate engine re-runs the roster and is the
authority.

- **build:** exit 0, 83 s against 600 s.
  - Warnings: 12 × NU1902, which is exempt.
  - Also 4 × SYSLIB0051 from the vendored MathNet, the same as in step 0445's log. They do not come from this
    step.
- **constructor-unit-tests** (`all_discovered`): `Passed!`, 1783 of 1783, 106 s against 180 s. The count is
  unchanged from checkpoint 444.
- **ui-smoke** (whole): `Passed!`, 264 of 264, 226 s. The deadline is 600 s (`attempt_timeout_seconds`). The
  count equals the baseline.
- **ui-tests** over `AppConfigTests`, `AppContextTests` and `DataLocationsWindowTests`: `Passed!`, 25 of 25,
  37 s against 360 s.
- **The widened facts** each show `Passed` in their TRX:
  - `DataLocations.defaultsUnder holds the 001 §G locations as configured text`;
  - `every location is a data location kind, in the order of its field`;
  - `the rows list every location in order, the library by its file`;
  - `readDataLocations writes every missing location back as its default and answers it expanded`;
  - `the test run's appsettings.json keeps the library in memory and every folder under the test output`.
- **elevation** (ELEV004 excluded, all 13 projects): `ELEV-GATE: OK - 0 new finding(s)`, 216 s against 600 s.
- **The twenty-one s43 invariants:** every gate command answers `INVARIANT HOLDS`.
- No development run exceeded its effective deadline.

```yaml
gates:
  constructor_unit_tests: 1783
  ui_smoke_tests: 264
  ui_tests: 25
```

# Architecture

- The sources folder is one more 001 §G location, not a new knob (.spec-md §8). It follows exactly the path the
  other folders take:
  - a `DataLocations` field;
  - a `DataLocationKind` case in field order;
  - a `ConfigKey` literal;
  - a `readSetting` read;
  - a `resolveFolder` resolution;
  - a `folderRow` row.
- The default sits under `%LOCALAPPDATA%` beside the library database, never in Documents, because a snapshot is
  slow data that every kit shares.
- No stored kit or library format changed. The only new wire name is the `MaterialSourcesFolder` key in
  `appsettings.json`.

# Deferred

Nothing.

# Gotchas

- `AppConfigTests.assertSavedLocations` is shared with two facts the test census does not list. The
  `MaterialSourcesFolder` write-back is therefore checked inline in the authorized fact, and the helper is
  unchanged.
- The App's own `appsettings.json` is empty and is not in this step's `edits`. The key's default is written back
  on first read.
- Any new construction of a `DataLocations` record must set `materialSourcesFolder`. Test fixtures should put it
  under their test output, as `TestAppScope` and `DataLocationsTests` do (.spec-md §3.9).

# Changelog

- 2026-11-16, step 0446: added the `materialSourcesFolder` data location and its `MaterialSourcesLocation` kind,
  the `MaterialSourcesFolder` app setting and its Data locations row, put the folder under the test run's output,
  and widened the five location facts. Test counts are unchanged.
