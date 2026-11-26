# Step 0289 — impl log

## Progress

- [x] Domain: `VaryDialReading` (Experiments.fs), and its arms in MeasuredData.fs and OutOfBandDiagnostic.fs
- [x] Domain: `SetupSlot.scanVariablesWith` (Setups.fs), over the shared `SetupSlot.tryFindDialMounted`
- [x] Controls: `VariableChoice.VaryDialReading`, `allVariables`
- [x] Ui: the mirror maps, `variablesOfElement`, `clampSweep`, the choose arms, `chartForRun`
- [x] Tests: SetupTests, ExperimentControlsTests; FsxAcceptanceTests' match (see Gotchas)
- [x] Development runs (all green on the final tree)

## Files modified

### Product

- `OpticalConstructor.Domain/Experiments.fs`: `VariableParameter.VaryDialReading`, after `VaryR2`.
  - It has code `dial`, label `Dial reading` and unit `°`.
  - `VariableRange.forVariable VaryDialReading` is 0…360° over 19 points.
  - `allowedFor` is unchanged.
- `OpticalConstructor.Domain/MeasuredData.fs`: `validateAgainstExperiment` puts `Some VaryDialReading` in
  `VaryR1`'s arm (no wavelength check).
- `OpticalConstructor.Domain/OutOfBandDiagnostic.fs`: `requestedWavelengthsFor` puts `Some VaryDialReading` in
  `VaryR1`'s fixed-wavelength arm.
- `OpticalConstructor.Domain/Setups.fs`:
  - New: `SetupSlot.tryFindDialMounted items` is the item of `items` the slot holds (`content.itemOpt`) when it is
    `DialMounted`, with its sign.
  - The private `tryDialReadingIn` (step 258) now reads through it, and `drawnWith` / `recordedWith` pass the slot.
    Their behaviour is unchanged.
  - New, after step 277's members: `SetupSlot.scanVariablesWith items` is `allowedFor` of the slot's kind, with
    `VaryR1` → `VaryDialReading` when `tryFindDialMounted` finds an item.
- `OpticalConstructor.Controls/ExperimentControls.fs`: `VariableChoice.VaryDialReading`, with `uiKey` `dial` and
  caption `Dial reading`. `allVariables` lists it last, so the bay draws `ExperimentVariable_dial`.
- `OpticalConstructor.Ui/TableAndElementRotationView.fs`:
  - Both mirror maps gain the case.
  - `variablesOfElement` reads `e.scanVariablesWith model.kitItems` for every non-source kind. It serves both
    `ExpChooseElement` and the bay's `variableChoices`.
  - The `ranged` arms of `ExpChooseElement` / `ExpChooseVariable` name the case, and so does `clampSweep`, whose
    `| _ -> range` wildcard is replaced by explicit arms.
  - `chartForRun`:
    - `variedRotationAt` turns each swept reading x to `sign.physicalAngleOf item.dialZeroAngle (Angle.degree x)`
      for the varied slot's dial-mounted item.
    - Both rotation sweeps (an item step and a preset polarizer step) use it for `VaryR1 | VaryDialReading`.
    - The chart has `xLabel` `Dial reading (°)`, the vary text `the dial reading over <lo>…<hi>°`, the title
      `<quantity> vs dial reading` and `AngularXAxis`.
    - The source text and the no-polarizer status treat it as `VaryR1`.
  - The section and `ExpChooseElement` comments name `SetupSlot.scanVariablesWith`.

### Tests

- `OpticalConstructor.Tests/SetupTests.fs`: new fact
  `a dial-mounted item's slot scans its dial reading where a fixed one scans R1`.
- `OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs`: new fact
  `a dial scan draws the run at the physical angle of each reading`. It is a ui-tests fact, with no smoke trait.
- `OpticalConstructor.Ui.Tests/FsxAcceptanceTests.fs`: one match arm (see Gotchas).
- ElementMenuTests, InverseConstructorTests, SolverHandoffWindowTests and WorkbenchInfoAndCaptureTests compile
  as they are, with no change.

## Decisions

- **One dial lookup, `SetupSlot.tryFindDialMounted`.** `scanVariablesWith` (Domain) and `chartForRun` (Ui) both
  need "the dial-mounted item this slot holds". The step-258 `tryDialReadingIn` already did that lookup privately,
  so the lookup is now a public member, and `tryDialReadingIn` keeps only its `DialTurn` reading match. This is a
  reuse refactor inside an authorized file: the lookup lives once (AGENTS.md "Reuse, never copy").
- **A slot with no dial-mounted item that is given `VaryDialReading` sweeps its R1.**
  - This is never offered by the bay. It can arise only from a stored experiment whose item was later re-mounted.
  - It follows `SetupSlot.turnAngle`: a slot without a dial reads its R1, and a fixed item's zero is `Angle.zero`.
  - The chart still says "dial reading". An empty chart was the alternative; it would hide a legitimate run.
- **The no-polarizer status covers the dial reading too.** The slice says to sweep `VaryDialReading` "as it sweeps
  `VaryR1`", and turning a dial is a rotation, so with no polarizer on the path the typed no-analyzer status
  applies to both.
- **The UI fact's first polarizer stands at R1 30°.** Behind a polarizer at 0°, Malus' law is even in the analyzer
  angle, so `−(x − 10°)` and `x − 10°` would read alike. At 30° the point-for-point comparison also checks the
  dial's sign.
- **The UI fact's "fixed analyzer" is a fixed-mounted item, `LPf`, over the same LP preset.** The two runs differ
  only in mounting, and both go through the item step. The reference range is set through the bay's own messages
  from `DialNegative.physicalAngleOf 10° 0°` to `… 360°` (10° to −350°), in 19 points. Each reference x is checked
  against the reading's physical angle (1e-9°), and each y against the dial run's (1e-12).
- **The bay boxes are checked in a rendered headless window.** It checks that `ExperimentVariable_dial` is
  effectively visible and `ExperimentVariable_r1` is not, as well as the state's `variableChoices`.

## Testing state

Every run below is on the final tree, run one after another.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors | 15 s (incremental) | 600 s |
| constructor-unit-tests | Passed!, 1484 of 1484 | 69 s | 180 s |
| ui-smoke | Passed!, 249 of 249 | 208 s | 600 s |
| ui-tests (8 selected classes, the gate's exact command) | Passed!, 103 of 103 | 109 s | 360 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)` | 201 s | 600 s |

- **constructor-unit-tests:** 1483 at step 0288, plus 1. The new SetupTests fact is `Passed` in the TRX.
- **ui-smoke:** 249, unchanged from step 0288. No smoke fact was added.
- **ui-tests:** all eight selected classes ran.
  - Per class: ElementMenuTests 15, ExperimentControlsTests 36, FsxAcceptanceTests 14, InverseConstructorTests 14,
    MainWorkbenchTests 5, OutOfBandBadgeTests 2, SolverHandoffWindowTests 9 and WorkbenchInfoAndCaptureTests 8.
  - ExperimentControlsTests rose by the one new fact, which is `Passed` in the TRX, so the selection was 102 before.
- **invariant-s29-\* (16):** every decoded `git grep` finds nothing.
- **Elevation:** the standalone driver had no comparison base, so it analyzed all 12 product projects (a superset of
  the gate's arc-origin scope).
- **Red evidence.** Two separate mutations, each rebuilt; every other fact stayed green.
  - **Mutation A:** `scanVariablesWith` keeps `VaryR1`. Exactly the two new facts turn red, both with
    `Expected: [VaryDialReading] / Actual: [VaryR1]`. SetupTests ran 17 with 1 failed; ExperimentControlsTests (no
    smoke) ran 36 with 1 failed.
  - **Mutation B:** `chartForRun` sweeps the raw reading as R1. Only the new UI fact turns red: `reading 0: the dial
    scan read 0.75, the fixed analyzer at 10° 0.8830222215594892` (36 run, 1 failed).
  - **Restore:** both files were restored, their SHA-256 hashes checked OK, and the tree rebuilt. Every
    Domain / Ui DLL copy postdates its restored source.
- **Line endings:** every changed file is LF in the index and the working tree (`git ls-files --eol`), and
  `git diff --numstat` equals the `--ignore-cr-at-eol` diff.
- No run exceeded its effective deadline.

## Artifacts

- `specs/0050/.artifacts/0289-build-dev-1.log`: the first build. Product green; Ui.Tests red on FS0025 in
  FsxAcceptanceTests.fs:197.
- `specs/0050/.artifacts/0289-build-dev-2.log`: green, 0 warnings, 0 errors.
- `specs/0050/.artifacts/0289-build-final.log`: the final build, 0 warnings, 0 errors.
- Development suite logs:
  - `0289-constructor-unit-tests-dev-1.log`
  - `0289-ui-tests-new-fact.log` (the new fact and step 258's dial fact, 2 of 2)
  - `0289-ui-tests-dev-1.log`
- Final logs:
  - `0289-constructor-unit-tests-final.log`
  - `0289-ui-smoke-final.log`
  - `0289-ui-tests-final.log`
  - `0289-elevation-final.log`
- Mutation evidence:
  - `0289-mutation-hashes.txt`
  - `0289-mutation-a-build.log` and `0289-mutation-a-run.log`
  - `0289-mutation-b-build.log` and `0289-mutation-b-run.log`

## Gotchas

- **FsxAcceptanceTests needed a one-arm edit.** The slice says it "compiles as it is". It does not:
  - its `match run.variable with` over `ExperimentControls.VariableChoice` raised FS0025, which the build treats as
    an error;
  - the file is in the step's authorized edits, so `VaryDialReading` joins the existing `VaryR1 | VaryR2` arm, which
    varies the sample;
  - no assertion changed, and no script varies a dial reading.
- **`VaryDialReading` exists in both Domain and Controls.** This is the existing mirror pattern of `VaryR1`. Controls
  references no project and Domain does not reference Controls, so ELEV016 sees no collision; the elevation run
  confirms it with 0 new findings.
- **Counting `\r` inside a quoted command substitution in Bash miscounts.** `"$(grep -c $'\r' f)"` reported every
  line. Count carriage returns byte-wise (`tr -cd '\r' < f | wc -c`), or read `git ls-files --eol`.
