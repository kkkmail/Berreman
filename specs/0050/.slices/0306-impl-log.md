# Step 0306 — impl log

## Progress

- [x] Read the protocol, the project prompt, the slice, AGENTS.md, the touched files, their callers and the copy
  tests.
- [x] Tests first (red). The test project failed to compile, and every error was a missing production symbol:
  `ImportProfileName.nextFreeAmong`, and `ExperimentCopyReport.profiles` / `.kitFile`
  (`.artifacts/0306-red-build.log`).
- [x] Production code: `Kit.fs`, `KitContentFiles.fs`, `TableAndElementRotationView.fs`.
- [x] First green: the solution builds with 0 warnings. KitTests + ExperimentCopyTests + KitWindowModelTests passed
  81 of 81.
- [x] Mutation check, three rounds, each defect caught by its own fact (`.artifacts/0306-mutation/round-*.txt`):
  - A: the window ignores `report.kitFile`, and the copy always writes the kit file. The window fact, the new copy
    fact (its "equal profile writes nothing" half) and the full-report literal fact failed.
  - B: on a clash the source profile is added unrenamed. The new copy fact failed.
  - C: the kit file is saved after the experiments. The new copy fact failed with its order message.
  - Both files were restored and their SHA-256 verified against `sha256-before.txt`.
- [x] Final rebuild and development runs of every gate in `0306.gates`. All passed (see Testing state).

## Files modified

- **`OpticalConstructor.Domain/Kit.fs`.** At its end, the intrinsic extension `type ImportProfileName with member
  this.nextFreeAmong (taken : ImportProfileName list) : ImportProfileName` returns the name of
  `FileStem.firstFreeText this.value` over the taken names' texts. That is the same body shape as
  `ItemLabel` / `SetupName` / `ExperimentName.nextFreeAmong`, and `firstFreeText` stems both sides by `FileStem.ofText`,
  ignoring case.
- **`OpticalConstructor.Storage/KitContentFiles.fs`.**
  - `open OpticalConstructor.Domain.ImportProfiles` (for `ChosenImportProfile`).
  - `ExperimentCopyReport` gains `profiles : ImportProfile list` and `kitFile : KitFile` as its last fields, each with
    a one-line S9.2 doc.
  - The private `ExperimentCopyPlan` gains `profiles`. `planItems` starts it at `[]`.
  - New local `placeProfiles` (pure). It takes the kept experiments' `dataOpt` profile names, distinct and in
    first-use order, and keeps those that `ChosenImportProfile.tryOfName source.kitFile.importProfiles` answers as
    `KitProfileChoice`. Each is then placed against `this.kitFile.importProfiles @ <added before>`:
    - held under its name with equal content: reused;
    - held with other content: added as `{ profile with name = profile.name.nextFreeAmong <held names> }`, and the
      experiments mapped by it follow the new name;
    - otherwise: added as it is.
  - New local `saveProfiles`. With nothing to add it answers `this.kitFile` and writes nothing. Otherwise it answers
    `this.saveKitFile { this.kitFile with importProfiles = this.kitFile.importProfiles @ added }`, its `KitError`
    mapped to `KitLibraryKitErr`, and returns the saved kit file.
  - `writeAll` runs, in order: snapshots → data files → `saveProfiles` → items → setups → experiments. The report
    carries `profiles = plan.profiles` and `kitFile` = the saved one, else `this.kitFile`.
  - The pipeline runs `placeProfiles` over the renumbered plan, after every read and before `writeAll`. The
    read-then-write order and every existing error are unchanged.
  - The member doc names the profiles and cites S9.2.
- **`OpticalConstructor.Ui/TableAndElementRotationView.fs`.** `CopyChosenExperiments` sets the window content through
  `withKitContent { model.kit with kitFile = report.kitFile } …`. The comment cites S9.2, and the status texts are
  unchanged.
- **`OpticalConstructor.Tests/KitTests.fs`.** New fact `a profile name takes the first free suffix among the taken
  ones`: over `[]` the name is itself, over `[ Lab scan ]` it is `Lab scan (2)`, and over `[ Lab scan; LAB SCAN (2) ]`
  it is `Lab scan (3)`.
- **`OpticalConstructor.Tests/ExperimentCopyTests.fs`.**
  - Helpers:
    - `withProfiles`: saves the kit file with profiles appended and answers the saved context.
    - `watchingWrites`: records each kit-file write as `None` and each content-file write as `Some kind`.
  - The one full-report literal (`a copy brings exactly the chosen experiments, …`) gains `profiles = []` and
    `kitFile = target.context.kitFile`. That is the shape change; the fact's assertions are otherwise unchanged.
  - New fact `a copy brings the kit profiles its experiments use, reusing an equal one and renaming a different one`.
    - The source maps E1 by `Lab scan` and E3 by `Intensity series`. Its kit file is saved holding `Lab scan`, and it
      is reopened read-only.
    - Copying [E1; E3] into a target holding another `Lab scan` (σ in column 5):
      - `readKitFile` holds `[ its own; Lab scan (2) ]`, and `report.kitFile` equals it;
      - `report.profiles` is exactly `[ Lab scan (2) ]`;
      - E1 is mapped by `Lab scan (2)` and E3 by `Intensity series`, in both the report and the kit;
      - the kit file is written exactly once, before any setup or experiment.
    - Copying E1 into a target holding an equal `Lab scan` adds no profile and writes no kit file. `report.kitFile` and
      `readKitFile` are the held kit file, and E1 keeps `Lab scan`.
- **`OpticalConstructor.Tests/KitWindowModelTests.fs`.**
  - `copySourceUnder` becomes `copySourceMappedUnder (profileOpt : ImportProfile option)`:
    - with a profile, it saves the source kit file holding it (through `saveKitFile`, before the source is closed)
      and maps E2 by it;
    - `copySourceUnder` is that function with `None`, so every other fact's source is unchanged.
  - The changed fact uses `Some KitTests.labScanProfile`. It replaces `Assert.Equal<KitContext>(model.kit, copied.kit)`
    with these asserts:
    - the model's kit file had no profile before the copy;
    - `copied.kit.kitFile.importProfiles` and the target's `readKitFile ()` hold `[ Lab scan ]`;
    - `copied.kit` equals `{ model.kit with kitFile = <read back> }`.

  Its other assertions are unchanged.

## Testing state

These are development runs of each gate's own command over the final build, after the mutation files were restored.
The arc-runner's gate engine runs the gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0; `0 Warning(s)`, `0 Error(s)`; no lowercase `error` in stdout | 21 s (incremental; first full build 56 s) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded | 205 s | 600 s |
| constructor-unit-tests | Passed!, 1520 of 1520, 0 failed, 0 skipped | 73 s | 180 s |
| ui-smoke | Passed!, 251 of 251, 0 failed, 0 skipped | 228 s | 600 s |
| ui-tests (KitWindowTests) | Passed!, 16 of 16, 0 failed, 0 skipped | 24 s | 360 s |
| invariant-s30-\* (16) | each exit 0, `INVARIANT HOLDS` | under 1 s each | 300 s |

- **constructor-unit-tests: 1520**, the 1518 baseline plus 2. In the TRX, the two new identities and the changed one
  are `Passed` under their exact names.
- **ui-tests: 16** over the selected class `KitWindowTests`. No file in `OpticalConstructor.Ui.Tests` changed, so the
  class's count is unchanged.
- **ui-smoke: 251**, unchanged.

## Artifacts

All of these are in `specs/0050/.artifacts/`:

- `0306-red-build.log`
- `0306-build.log`
- `0306-elevation.log`
- `0306-constructor-unit-tests.log` / `.trx`
- `0306-ui-smoke.log` / `.trx`
- `0306-ui-tests.log` / `.trx`
- `0306-invariants.log`
- `0306-mutation/` (`round-a.txt`, `round-b.txt`, `round-c.txt`, `sha256-before.txt`)

## Gotchas

- **Interpretation: "held" includes the profiles the copy added before.** The slice says one "the target holds under
  that name" is reused or renamed, and any other is added as it is. Read literally, a renamed profile could take a
  name a later copied profile carries, and the kit file would hold two profiles of one name. Example: the target holds
  `Lab scan`, and the source holds `Lab scan` and `Lab scan (2)`.
  - So a later profile is checked against the target's profiles and the ones added before it. It is then reused or
    renamed by the same rule.
  - Whenever no added profile shares a later one's name, this is exactly the slice's rule, and so in every fact.
  - The renamed name's `nextFreeAmong` already runs over "the target's names and the ones added before it", as the
    slice says.
- **Interpretation: "a test-local signal profile `Lab scan`" reuses `KitTests.labScanProfile`.** It is the exact
  profile the slice describes (comma, header, no BOM, scan 1 in degrees as a physical value, `SignalColumns (2,
  Some 3)`, key 4, no timestamp, capture index or observable). `KitTests.fs` compiles before both `ExperimentCopyTests.fs`
  and `KitWindowModelTests.fs`, and `KitFileJsonTests` / `ImportProfileTests` already reuse it. AGENTS.md's "reuse,
  never copy" decides against a fourth copy.
- **The full-report literal in `a copy brings exactly the chosen experiments, …` had to name the two new fields.**
  `kitFile = target.context.kitFile` holds because nothing is added. This is the contract's `change_shape`, not a
  change of that fact's expectation.
- **Profile names compare ordinally, file stems ignore case.** `Lab scan` beside a held `LAB SCAN` is not a clash (as
  `availableAmong` allows it), but a renamed name skips any stem the kit holds in any case.
- **A built-in name, or a name the source kit lacks, is never copied.** `ChosenImportProfile.tryOfName` resolves a
  built-in first, so a source kit profile can never shadow one.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.
