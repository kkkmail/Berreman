# Step 0286 — impl log

## Progress

- [x] Experiments.fs: moved `ExperimentError`, added `InvalidExperimentNameErr` and `describe`
- [x] Experiments.fs: added `ExperimentName` and `ExperimentId.defaultName`
- [x] NamingTemplates.fs: added `NameText.toExperimentName` and `NamingTemplate.experimentNameAmong`
- [x] Tests: three new facts, written
- [x] Build, constructor-unit-tests and the 16 invariants (development runs)
- [x] elevation (development run)
- [x] Mutation check, then the file restored byte for byte and re-verified

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Experiments.fs`
  - `ExperimentError` moved from the end of the module to directly after `ExperimentNotes` (step 285's last type).
    Both of its cases are kept. It gains `InvalidExperimentNameErr of reason : string` and `describe`.
  - New `ExperimentName`: `value`, `tryCreate`, `fileStem` and `nextFreeAmong`.
  - New intrinsic extension `type ExperimentId with`: `defaultName` (`E<n>`), plus `fileStem`, moved here (see
    Decisions).
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/NamingTemplates.fs`
  - Adds `open OpticalConstructor.Domain.Experiments`.
  - Adds `type NameText with member this.toExperimentName` and
    `type NamingTemplate with member this.experimentNameAmong`, both after step 271's `setupNameAmong`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentProxyTests.fs`: two new facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/NamingTemplateTests.fs`: one new fact, and
  `open OpticalConstructor.Domain.Experiments`.

## Decisions

- **`ExperimentId.fileStem` is derived from `defaultName`.**
  - The brief says `defaultName` is "the stem step 146 gives it today, so every such experiment keeps its file name".
  - With a second `$"E{this.value}"` in `defaultName`, that text would be written twice: a re-transcribed constant,
    which AGENTS.md's "Reuse, never copy" forbids.
  - So `fileStem` (unchanged doc and value: `FileStem.ofText "E<n>"` = `E<n>`) moved out of the `ExperimentId` body
    into the intrinsic extension, after `defaultName`, as `this.defaultName.fileStem`.
  - This adds no member and changes no signature or compiled shape: an intrinsic extension compiles into the type.
    Step 290 deletes `ExperimentId.fileStem` anyway.
- **`nextFreeAmong` passes the taken names' texts to `FileStem.firstFreeText`.**
  - This matches `SetupName.nextFreeAmong` and `ItemLabel.nextFreeAmong` exactly.
  - `firstFreeText` compares the stems of the texts it is given (`FileStem.ofText`), and its doc says why: the taken
    are texts, not stems, because ELEV010 refuses a `string` beside a `FileStem`.
  - So this is the brief's "over the taken names' `fileStem`s": `ofText` is idempotent on a stem.
- **`experimentNameAmong` follows `setupNameOf` / `setupNameAmong`.**
  - It calls `nameReadingOf slots items noteOpt`, then `writeName`, then `toExperimentName`, then `nextFreeAmong`.
  - Any `Error` (a required placeholder empty, or a blank written name) answers `unnamed`.
  - It adds no second naming rule.
- **`open OpticalConstructor.Domain.Experiments` in `NamingTemplates.fs`.** `Experiments.fs` compiles before
  `NamingTemplates.fs`. No record label or name in the file changed its resolution, and the build is green.

## Testing state

These are development runs on the final tree, using the snapshot's commands.

- **build:** exit 0, 0 errors, in 74 s. The only warnings were the two vendored MathNet `SYSLIB0051`, which are not
  ours. Log: `.artifacts/0286-dev-build.log`.
- **constructor-unit-tests:** Passed!, 1477 of 1477 (1474 at the last checkpoint, +3), in 69 s against a 180 s
  deadline. The TRX shows all three new facts `Passed`. Log: `.artifacts/0286-dev-constructor-unit-tests.log`.
- **invariant-s29-\*** (16): every decoded grep finds nothing. Log: `.artifacts/0286-dev-invariants.log`.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, in 206 s against a 600 s
  deadline. Without a `-Base` it analyzed all 12 projects, a superset of the touched ones and their dependents. Log:
  `.artifacts/0286-dev-elevation.log`.
- **Mutation check:**
  - Changed `ExperimentName.tryCreate`'s `IsNullOrWhiteSpace` to `IsNullOrEmpty`.
  - Both name facts went red, each naming the bug: `'   ' was kept as '   '`, and `a blank text was kept as '  '`.
    Log: `.artifacts/0286-mutation-run.log`.
  - The file was restored from a copy. Its SHA-256 matches the pre-mutation hash (`8456ffec…877ca`).
- **Final tree:**
  - The full solution build is green: incremental, 0 warnings, 0 errors (`.artifacts/0286-dev-build-final.log`).
  - constructor-unit-tests: Passed!, 1477 of 1477, in 68 s.
  - Elevation ran on byte-identical sources, before the mutation.

## Artifacts

- `specs/0050/.artifacts/0286-dev-build.log`
- `specs/0050/.artifacts/0286-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0286-dev-invariants.log`
- `specs/0050/.artifacts/0286-dev-elevation.log`
- `specs/0050/.artifacts/0286-mutation-run.log`
- `specs/0050/.artifacts/0286-dev-build-final.log`

## Gotchas

- No code matched over `ExperimentError` before this step. Only `ExperimentProxyTests` names the type, and only in
  `Assert.Equal` type arguments. So "a match over the error anywhere gains the case" had no site to change.
- In Git Bash, `grep -c $'\r'` reports every line, so it is not a CR count. Count CR bytes with Python instead. All
  four files are LF.
