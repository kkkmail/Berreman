# Step 0299 — impl log

## Progress

- [x] Read the task, the system and project prompts, the slice, AGENTS.md, step 298's state of the world, `Kit.fs`,
  `kit.schema.json`, `CanonicalJson.fs`, `KitTests.fs`, `KitFileJsonTests.fs` and manual 010 (0267/0299/0420: keep
  `KitFile.create`'s added defaults).
- [x] Domain: `ScanColumn`, `ReadingColumns`, `ColumnRoles`, `ImportProfileError`, `ImportProfile`,
  `ImportProfileName.availableAmong`, `KitFile.importProfiles` and `KitFile.withImportProfile`, in the slice's order
  between `DataColumnNumber` and `NamingTemplate`.
- [x] Schema: `importProfiles` is required under version 1, and `additionalProperties: false` holds at each level.
- [x] Tests: two new `KitTests` facts. The fixture and the two named `KitFileJsonTests` facts changed.
- [x] Development runs: build, constructor-unit-tests, elevation and the 16 s30 invariants, plus a mutation check.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`:
  - adds `ScanColumn`, `ReadingColumns`, `ColumnRoles`, `ImportProfileError` (with `describe`) and `ImportProfile`;
  - adds an intrinsic extension `type ImportProfileName with member this.availableAmong`;
  - `KitFile` gains `importProfiles` (`[]` in `create`) and `withImportProfile`.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/kit.schema.json`:
  - the version-1 body requires `importProfiles`;
  - a local `$defs` adds `columnNumber` (integer ≥ 1) and `optionalColumnNumber` (`null` or a `columnNumber`);
  - the description names S9.2.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitTests.fs`:
  - adds a public fixture `labScanProfile`;
  - adds the facts `a profile name is refused when a built-in or a kit profile holds it, and a kit file appends a new
    profile` and `every import profile error renders its condition and its payload`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitFileJsonTests.fs`:
  - adds a private fixture `polarimeterProfile`;
  - `kitFile` holds `[ KitTests.labScanProfile; polarimeterProfile ]`;
  - the round-trip fact also lists `importProfiles` among the keys, checks the profile names in order and checks that
    `Polarimeter`'s `delimiter` is `3`. Its empty-metadata variant also empties `importProfiles`;
  - the schema-invalid fact also removes `importProfiles` and sets `delimiter` to `0`. It also removes `roles` from
    the first profile.

## Decisions

- **`availableAmong` is an intrinsic extension** (`type ImportProfileName with`, same module and file). It needs
  `ImportProfile`, which is declared after `ImportProfileName` because a profile holds its name. This is the slice's
  item (6) as written.
- **Both refusals compare ordinally.** A built-in name uses `List.contains`, the union's structural equality over the
  ordinal string. A held name uses `profile.name = this`. Each reason is the name in quotes (`'Lab scan'`), so the
  error names it.
- **`availableAmong` does not check for blank names.** The slice's naming rule names only the two refusals.
  `ImportProfileName.tryCreate` (step 291) already refuses a blank text.
- **The append keeps order** (`this.importProfiles @ [ profile ]`). It never replaces or removes a profile, and no
  replace or delete verb was added.
- **`reading` is "an object" in the schema.** The shared `CanonicalJson.options` writes a multi-case union with fields
  as the serializer's adjacent-tag object `{ "Case": "SignalColumns", "Fields": [2, 3] }`. That is the form
  `DataRowSelection` already takes in `experiment.schema.json`. The shape is recorded in
  `.artifacts/0299-shape/lab-scan-profile.json`.
- **The column rule is written once.** The schema's local `$defs` (`columnNumber`, `optionalColumnNumber`) serve the
  scan column and the four `…Opt` keys. Local `$defs` references are already used by `material-metadata` and
  `optical-constructor-groups`.
- **The fixture is reused, not copied.** `labScanProfile` is public in `KitTests`, and `KitFileJsonTests`, which
  compiles after it, reads `KitTests.labScanProfile`. A second copy of the profile would be a duplicated fixture.
- The schema version stays 1. No committed `.kit-json` exists (`git ls-files` finds none), so old-shape kit files
  exist only under test output (.spec-md §7).

## Testing state

Every run below was a development run of the gate's own command. The gate engine runs the gates themselves after this
round.

- **build:** `dotnet build Berreman.slnx -c Release -nologo -v:m` exited 0 with 0 warnings and 0 errors. The final
  run took 16 s and the first full build 74 s. The deadline is 600 s.
  - The first two attempts failed on F# FS0597 (chained indexers `a[1]["x"]` and `f()[0]` in the test). Binding the
    intermediates fixed it.
- **constructor-unit-tests:** Passed!, 1498 of 1498, 0 failed, 0 skipped, in 69 s against a 180 s deadline.
  - That is the 1496 baseline plus the 2 new facts.
  - In the TRX, both new facts and both changed facts are `Passed`. The unchanged `a kit file of another schema
    version is UnsupportedSchemaVersionErr` also passes.
- **elevation:** run with `ASG_ARC_BASE_COMMIT` set to the `.arc-base-json` commit. It reported
  `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, with known findings 0 and ELEV004 excluded. It took 198 s
  against a 600 s deadline.
- **invariant-s30-\* (16):** each reported `INVARIANT HOLDS` with exit 0, taking under 1 s each.
- **Mutation check (red):**
  - Change: the duplicate check was disabled (`&& false`), and `importProfiles` was dropped from the schema's
    required keys.
  - Result: `a profile name is refused …` failed with "a kit file replaced the profile it holds". `a schema-invalid
    or malformed kit file …` failed with "expected SchemaValidationErr: Missing field for record type … KitFile:
    importProfiles".
  - Restore: both files were restored byte for byte (`sha256sum -c` OK). The solution was rebuilt and the suite
    re-run green before the results above.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0299-dev-build.log`: the final build.
- `0299-dev-constructor-unit-tests.log`: the final test run.
- `0299-dev-elevation.log`
- `0299-dev-invariants.log`
- `0299-mutation-build.log` and `0299-mutation-tests.log`: the red run.
- `0299-shape/kit-shape.fsx` and `0299-shape/lab-scan-profile.json`: the on-disk shape of `Lab scan`.
- `0299-shape/pre-mutation.sha256`

## Gotchas

- **Shared record labels.** `reading` labels both `ScanColumn` and `ColumnRoles`, and `name` labels both
  `ImportProfile` and `KitFile` (and other records). Annotate a record literal's type (`let profile : ImportProfile =
  …`) so F# resolves the labels from the annotation, not from declaration order.
- **F# chained indexers** (`profiles[1]["delimiter"]`, `node["x"].AsArray()[0]`) fail with FS0597. Bind the
  intermediate value first.
- **A wrong `reading` case fails at binding, not in the schema.** It is refused when the serializer binds it, as
  `MalformedJsonErr`, because the schema says only "an object" (by the slice's wording, as for `DataRowSelection`).
- **Five of the seven `ImportProfileError` cases have no producer yet:** `BlankProfileNameErr`,
  `SignalColumnCountErr`, `MixedReadingColumnsErr`, `RepeatedRoleErr` and `MuellerElementCountErr`. The slice defines
  them, with their texts, and asks this step for no function that produces them. Only `availableAmong` produces errors
  here: `BuiltInProfileNameErr` and `DuplicateProfileNameErr`.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.
