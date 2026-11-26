# Step 0238 — impl log (IMPLEMENT: the constructor's flow strip as model state)

## Progress

- [x] Read the protocol (arc-runner + IMPLEMENT worker), the project prompt, the slice and its `.gates` snapshot.
      The operator note is empty.
- [x] Added `ConstructorFlowStep`, the `flowStep` field, `ShowFlowStep` and the arms in `TableAndElementRotationView.fs`.
- [x] Added the three declared `KitWindowModelTests` facts at the end of the module.
- [x] Development runs:
  - build;
  - the whole constructor-unit-tests suite;
  - ui-smoke;
  - ui-tests over `KitWindowTests`;
  - elevation from the arc origin;
  - the fourteen s24 invariant greps;
  - a four-mutation check, followed by a rebuild and a re-run of every suite.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs` (+37 / −1):
  - `type ConstructorFlowStep`, declared before the model, with:
    - the cases `ItemsFlowStep | SetupsFlowStep | ExperimentsFlowStep`;
    - `static member all : ConstructorFlowStep list`;
    - `member this.caption : ControlCaption`.
  - The record field `flowStep : ConstructorFlowStep`, right after `kitItems`. `initWith` sets it to `SetupsFlowStep`.
  - `| ShowFlowStep of ConstructorFlowStep` in `TableAndElementRotationMsg`, right after `SelectBay`.
  - In `updateTableAndElementRotation`, the `SelectBay` arm now also sets `flowStep`. Three `ShowFlowStep` arms were
    added.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs` (+87): the three declared facts.

No other file changed. Both files are LF (`git diff --numstat` equals `--ignore-cr-at-eol`).

## Decisions

- **Arms as the slice gives them.**
  - `ShowFlowStep ItemsFlowStep` sets `flowStep` alone.
  - `ShowFlowStep SetupsFlowStep` sets `flowStep`. If `ribbon = BayNames.experiments`, it moves `ribbon` to
    `BayNames.add`; any other bay stays.
  - `ShowFlowStep ExperimentsFlowStep` sets `flowStep` and `ribbon = BayNames.experiments`.
  - `SelectBay name` sets `ribbon` as before. It also sets `ExperimentsFlowStep` for `BayNames.experiments` and
    `SetupsFlowStep` for any other bay.
- **No helper function.** The bay test is an inline `if` in each arm. A free `BayName -> ConstructorFlowStep` helper
  would raise the ELEV013 question (a function of one of our types belongs on the type). The slice also says no
  other message or field changes.
- **No `value` tag on `ConstructorFlowStep`.** It is window state and never stored (the slice says so), like
  `ConstructorMode`. ELEV017 constrains a `value` member only where one exists.
- **The forward and inverse constructors show the same strip.** Both build their model through `initWith`, and both
  run the one update function, so neither needs code of its own. Fact 1 also checks that `initInverseWithKit` opens
  on `SetupsFlowStep` with `BayNames.add`.
- **Every check stays inside the three declared identities.** No other identity was added.
  - Fact 2 compares whole models after each press, so it also proves that nothing else changes.
  - Fact 3 adds a committed experiment first, so the claim that "the experiments are unchanged" is not checked
    over an empty collection. It also checks `SelectBay` from the Items step, as well as from Experiments.
- **Placement.** The facts are at the end of the `KitWindowModelTests` module, after the step 223 copy facts.

## Testing state

No gate verdict is claimed here (Invariant 6). These are development runs over the final tree. The logs are in
`specs/0050/.artifacts/`.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`):
  - first run: exit 0 in 70 s. Its only 2 warnings were the known SYSLIB0051 in the vendored MathNet
    `Exceptions.cs` (manual 013). Log: `0238-build.log`.
  - after the mutation check, rebuild from the restored source: exit 0, `0 Warning(s)`, `0 Error(s)`.
    Log: `0238-build-final.log`.
- **constructor-unit-tests**: the gate's command over every test discovered.
  - Result: `Passed!`, succeeded 1366, failed 0, skipped 0, in 1 min 04 s (estimate 180 s).
  - That is checkpoint 236's 1363 plus the 3 declared facts. The TRX shows all three ids as `Passed`.
  - Log: `0238-constructor-unit-tests.log`.
- **ui-smoke**: the gate's command.
  - Result: `Passed!`, succeeded 242, failed 0, skipped 0, in 3 min 18 s.
  - The attempt timeout is 600 s, and 242 equals checkpoint 232's 242.
  - Log: `0238-ui-smoke.log`.
- **ui-tests**: the gate's command with the step's selector, `OpticalConstructor.Ui.Tests.KitWindowTests`.
  - Result: `Passed!`, succeeded 13, failed 0, skipped 0, in 19.5 s. Log: `0238-ui-tests.log`.
- **elevation** (`ASG_ARC_BASE_COMMIT=f395597…`, ELEV004 excluded):
  - Result: exit 0, `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 203s`. Known findings: 0.
  - The run widened to all 12 projects because the solution changed since the arc origin.
  - The analyzed source is byte-identical to the final tree: `cmp` with the saved original after the mutation
    restore.
  - Log: `0238-elevation.log`.
- **The fourteen s24 invariant greps:** none finds anything. Log: `0238-invariants.log`.
- **The facts bite.** `0238-mutations/run-mutations.py` applied each mutation, built the test project and ran
  `KitWindowModelTests`:

  | Mutation | Fact that went red |
  |---|---|
  | M1: `initWith` opens on `ItemsFlowStep` | fact 1 (`…a window opens on Setups`) |
  | M2: `SelectBay` leaves `flowStep` unchanged | fact 3 (`choosing a ribbon bay…`) |
  | M3: `ShowFlowStep SetupsFlowStep` keeps the Experiments bay | fact 2 (`showing a flow step…`) |
  | M4: `ShowFlowStep ExperimentsFlowStep` does not open the bay | fact 2 (`showing a flow step…`) |

  The script then restored the source, and `cmp` with the saved original reports it identical. The solution was then
  rebuilt and every suite re-run, as recorded above. Summary: `0238-mutations/summary.txt`.

No run came near a deadline. No test was removed or renamed, and no assertion was weakened.

## Artifacts

- `specs/0050/.artifacts/0238-build.log`, `0238-build-final.log`
- `specs/0050/.artifacts/0238-constructor-unit-tests.log`
- `specs/0050/.artifacts/0238-ui-smoke.log`
- `specs/0050/.artifacts/0238-ui-tests.log`
- `specs/0050/.artifacts/0238-elevation.log`
- `specs/0050/.artifacts/0238-invariants.log`
- `specs/0050/.artifacts/0238-mutations/`: the driver, the saved original, the per-mutation build and test logs,
  and `summary.txt`.

## Gotchas

- **`SelectBay` now writes two fields.** A test that sets `ribbon` by record update, rather than through
  `SelectBay`, leaves `flowStep` at its old value. Several Ui.Tests do this, for example `ScrollExtentTests`,
  `ElementMenuTests` and `SourceAndDetectorMoveTests`. That is harmless today, because no view reads `flowStep` yet.
  When the strip is drawn, such a model can show Setups over the Experiments bay.
- **Items keeps the ribbon.** `ShowFlowStep ItemsFlowStep` leaves `ribbon` as it was, so a model on Items can still
  hold `BayNames.experiments`. Pressing Setups from there opens `BayNames.add`, which fact 2 checks.
- **The first build showed 2 SYSLIB0051 warnings** from the vendored MathNet sources (manual 013). They are not ours,
  and the incremental rebuild reported `0 Warning(s)`.
