# Step 0289 — state of the world

## Where we are

Spec 0050 section S9.1 covers Experiments v2: scans, roles, sessions and attachments. Step 288 landed Experiment v2
(a scan of axes and blocks). This step adds the dial reading as a scan variable. A slot holding a dial-mounted item
is scanned by its raw dial reading in R1's place (001 §I "Experiments record raw dial readings"; 001 §K "dial
0…360 step 20"):
- the domain offers it through `SetupSlot.scanVariablesWith`;
- the bay draws its `dial` box;
- `chartForRun` evaluates each reading at the item's physical angle.

## What's working

- Add `VariableParameter.VaryDialReading` (code `dial`, label `Dial reading`, unit `°`). Its default range is 0…360°
  in steps of 20 (19 points). Every match over the variable names it, with no wildcard.
- Add `SetupSlot.scanVariablesWith items`: the slot kind's `allowedFor`, with `VaryR1` replaced by `VaryDialReading`
  when the slot holds a dial-mounted item. It is built on a new shared lookup, `SetupSlot.tryFindDialMounted`.
- Mirror it in the bay as `VariableChoice.VaryDialReading` (`dial`, `Dial reading`), listed last.
  `ExpChooseElement` and the bay's choices read `scanVariablesWith model.kitItems`.
- `chartForRun` sweeps a dial reading as it sweeps R1, each reading x at
  `sign.physicalAngleOf item.dialZeroAngle (Angle.degree x)`. Its labels are `Dial reading (°)` and
  `<quantity> vs dial reading`, on an angular axis.
- Two new facts:
  - SetupTests: `a dial-mounted item's slot scans its dial reading where a fixed one scans R1`;
  - ExperimentControlsTests: `a dial scan draws the run at the physical angle of each reading`.

## Tests

These are development runs on the final tree, using the snapshot's commands and selectors, run one after another.
The arc-runner's gate engine runs the gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors (final incremental build) | 15 s | 600 s |
| constructor-unit-tests | Passed!, 1484 of 1484, 0 failed, 0 skipped | 69 s | 180 s |
| ui-smoke | Passed!, 249 of 249, 0 failed, 0 skipped | 208 s | 600 s |
| ui-tests (8 selected classes, the gate's exact command) | Passed!, 103 of 103, 0 failed, 0 skipped | 109 s | 360 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded | 201 s | 600 s |

- **constructor-unit-tests:** 1484 is step 0288's 1483 plus the one new fact, which is `Passed` in the TRX.
- **ui-smoke:** 249, unchanged. This step adds no smoke fact.
- **ui-tests:** every selected class ran, and the new fact is `Passed` in the TRX.
  - Per class: ElementMenu 15, ExperimentControls 36, FsxAcceptance 14, InverseConstructor 14, MainWorkbench 5,
    OutOfBandBadge 2, SolverHandoffWindow 9 and WorkbenchInfoAndCapture 8.
  - Step 0288's run of the same eight classes counted 102 (ExperimentControls 35), so the count rose by 1.
  - Step 0288 selected ten classes (132, including KitWindow and ScrollExtent), so 103 is not comparable to that
    total.
- **invariant-s29-\*** (16): every decoded `git grep` finds nothing.
- **Mutation check (red).** Two separate mutations, each built and run; every other fact stayed green.
  - **Mutation A:** `scanVariablesWith` keeps `VaryR1`. Exactly the two new facts turn red, both with
    `Expected: [VaryDialReading] / Actual: [VaryR1]`.
  - **Mutation B:** `chartForRun` sweeps the raw reading as R1. Only the new UI fact turns red: `reading 0: the dial
    scan read 0.75, the fixed analyzer at 10° 0.8830222215594892`.
  - **Restore:** both files were restored byte for byte (SHA-256 checked) and rebuilt, and every suite above
    re-ran green.

```yaml
gates:
  constructor_unit_tests: 1484
  ui_smoke_tests: 249
  ui_tests: 103
```

## Architecture

- **One dial lookup.** `SetupSlot.tryFindDialMounted items` is "the item of `items` this slot holds, when it is
  dial-mounted, with its sign". `scanVariablesWith` (Domain) and `chartForRun` (Ui) both read it. Step 258's private
  `tryDialReadingIn` now reads it too, keeping only its `DialTurn` reading match, so the lookup lives once.
  `drawnWith` / `recordedWith` are unchanged in behaviour.
- **`allowedFor` is unchanged.** A kind does not know its item's mounting, so the slot (`scanVariablesWith`) decides.
  `variablesOfElement` keeps its light-source rule (a tunable source only) and reads `scanVariablesWith` for every
  other kind.
- **`chartForRun` resolves the varied slot's rotation once.** `variedRotationAt` is a dial-mounted item's reading at
  its physical angle, else the swept value itself. Both rotation sweeps (an item step and a preset polarizer step)
  use it for `VaryR1 | VaryDialReading`. The no-polarizer status, the source text and the angular axis treat the
  dial reading as R1.
- **A `VaryDialReading` on a slot that holds no dial-mounted item sweeps its R1.**
  - The bay never offers that; only a stored experiment whose item was re-mounted could carry it.
  - It follows `SetupSlot.turnAngle`: a slot without a dial reads its R1, and a fixed item's zero is `Angle.zero`.
- **`clampSweep`'s `| _ -> range` wildcard is replaced by explicit arms**, so the new case cannot hide behind it.

## Deferred

Nothing. Every requirement of the step landed in this round.

## Gotchas

- **FsxAcceptanceTests needed a one-arm edit, though the slice says it "compiles as it is".**
  - Its `match run.variable with` over `ExperimentControls.VariableChoice` raised FS0025, which the build treats as
    an error.
  - The file is in the step's authorized edits, so `VaryDialReading` joins the existing `VaryR1 | VaryR2` arm, which
    varies the sample. No assertion changed.
  - ElementMenuTests, InverseConstructorTests, SolverHandoffWindowTests and WorkbenchInfoAndCaptureTests compiled
    unchanged.
- **Behind a polarizer at 0°, Malus' law is even in the analyzer angle**, so a dial sign error would not show. The
  UI fact turns its first polarizer to R1 30°, and its point-for-point comparison then checks both the dial's sign
  and its zero.
- **`VaryDialReading` is declared in both Domain and Controls**, as `VaryR1` already is. Neither project references
  the other, so ELEV016 sees no collision; the elevation run shows 0 new findings.

## Changelog

- 2026-11-11 — Step 0289: added the dial-reading scan variable.
  - Domain: `VaryDialReading` (0…360°, 19 points) and `SetupSlot.scanVariablesWith` over the shared
    `tryFindDialMounted`.
  - Controls: the bay's `dial` box.
  - Ui: `chartForRun` sweeps each reading at its physical angle.
  - Tests: two new facts. Constructor tests 1484, smoke 249, selected ui-tests 103.
