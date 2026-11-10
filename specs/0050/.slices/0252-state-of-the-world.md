# Step 0252 — state of the world

## Where we are

Spec 0050, section S8.1 ("The run law walks every object; the source is real"), step 252 (IMPLEMENT).

- Steps 244–251 built the item-model evaluator: `ParameterValues`, `emissionWith`, `muellerAt` and `boundWith`.
- This step makes the table's run use that evaluator.
  - `ExperimentRun` folds every step of the light path through its Mueller matrix. It starts from a real source
    emission and has no first-polarizer arm.
  - `chartForRun` builds its steps through `ItemEvaluation`.
- S8.2's slots and S11.2's detector forward model come later.
- **The step is not commit-ready.** Its ui-tests gate selects `InverseConstructorTests`, which carries a red
  inherited from step 0227 that only an out-of-scope fixture repair can clear (see Tests).

## What's working

- `ExperimentRun` folds every `ElementStep` / `ObjectStep` through its Mueller matrix in light order from
  `SourceEmission.emittedInto`
- The default source (unpolarized, unit scale after the first polarizer) reproduces every old reading exactly
- `chartForRun` evaluates polarizers and samples through `ItemEvaluation`, every sample on the detector's path for
  its own branch
- Retired `Propagation.behaviorInputStokes` and `runInputStokes`
- `SampleMaterialErr` keeps its failure chart; any other evaluation error shows "The run cannot be evaluated"
- `ExperimentRunTests` gains four facts; two `PolarizerBehaviorTests` facts are deleted; a new UI fact checks two
  samples solved in series

## Tests

No gate verdict is claimed here (Invariant 6). The worker's development runs over the final tree are below, with
logs under `specs/0050/.artifacts/0252-*`.

- **build:** exit 0, `0 Warning(s)`, `0 Error(s)` (`0252-build-final.log`).
- **constructor-unit-tests,** the gate's command: `Passed!`, 1401 succeeded, 0 failed, 0 skipped
  (`0252-constructor-unit-tests-final.log`, 68 s against the 180 s estimate).
  - That is checkpoint 250's 1399, plus the four new facts, minus the two deleted `behaviorInputStokes` facts (+2).
  - The renamed `the default source is unpolarized light at unit scale, referenced after the first polarizer` and
    the four new facts are `Passed` in the TRX.
- **ui-smoke,** the gate's command: `Passed!`, 245 of 245, 0 failed (`0252-ui-smoke.log`). It took 214 s against the
  600 s `attempt_timeout_seconds`. A re-run over the final rebuild passed 245 of 245 in 199 s
  (`0252-ui-smoke-final.log`).
- **ui-tests over the six selected classes,** the gate's command and selector (`0252-ui-tests-final.log`):
  - **84 of 85 passed, 1 failed.**
  - The new `two bound samples in series are both solved on the detector's path` is `Passed`.
  - **The failure:** `InverseConstructorTests.recommitting a copied experiment preserves its assigned physical ids`
    (`InverseConstructorTests.fs:509`). Its kit status is "the kit has no item of that id: 3c9e1f47-…".
  - **It is inherited and deterministic.** It also fails run alone (`0252-ui-recommit-alone.log`).
    - The fixture assigns three hard-coded `ItemId`s that it never writes into the kit.
    - Since step 0227, `recordExperiment` refuses a missing piece before writing.
    - Step 0242 reproduced the same red on an unmodified `git archive HEAD` copy (`0242-head-check-test.log`).
    - This step's diff does not reach the copy or record path.
  - **The conflicting obligation:** the slice's acceptance says "ui-tests MUST pass over the selected classes", and
    its selection includes `InverseConstructorTests`.
  - **The missing authority:** the repair is in `InverseConstructorTests.fs` (write the assigned pieces into the kit
    before recommitting), or on the product's record path. Neither is in this step's authorized `edits`, and the
    slice declares no test effect on that fact.
  - **Where it is scheduled:** step 0261 is the step authorized to rework the fact.
  - An operator decision is needed: authorize the fixture repair here, or exempt this one inherited fact at 0252.
- **elevation** from the arc origin `f395597`: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 193 s against the
  600 s estimate (`0252-elevation.log`). Known findings: 0.
- **The fourteen s25 invariants,** by their greps: all find nothing (`0252-invariants.log`).
- **Red, measured by mutation** (`0252-red-mutation-tests.log`).
  - **The mutations:**
    - the intensity reference forced to 1;
    - the host skipping every sample but the varied one.
  - **What went red:**
    - every new `ExperimentRunTests` fact, the renamed one and `a polarizer AFTER the sample analyzes` (6 of 9);
    - the new UI fact.
  - **Restored:** both sources match their SHA-256 (`0252-mutation-sources.sha256`), the solution was rebuilt, and
    the final runs above were repeated.
- No development run came near a deadline. No assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1401
  ui_smoke_tests: 245
  ui_tests: 84
```

## Architecture

- **`RunStep` keeps only what the law needs:** a Mueller matrix, and whether the step is an object.
  - `ElementStep` is any non-object (today a polarizer; S8.2 adds prep and analyzer items).
  - `ObjectStep` is a solved object.
  - A run holds any number of objects.
- **The intensity reference belongs to the emission and the path:**
  - `SourceEmission.emittedInto` scales `stokes` by r.
  - r = 1 / S₀ of unpolarized light through the path's leading element; r = 1 for a path that opens on an object or
    is empty.
  - An ideal polarizer passes exactly ½, so r = 2 and the old unit-intensity rule is reproduced bit for bit.
  - A constant-compound first polarizer now also passes unit intensity, where `behaviorInputStokes` left it
    un-normalized.
- **The host binds once and evaluates per point:**
  - A sample is `BoundOpticalElement.ofStackSample` over its own thickness defaults, bound once per path step, so its
    materials resolve once per run as before.
  - Swept steps answer `Result`.
  - The first evaluation error, in branch, step and point order, decides the failure chart.
- **Every chart reads the default source.** A source preset carries no parameter values; S8.2's source slot passes
  an item's own values through `emissionWith`.

## Deferred

Nothing in this step's scope is deferred. The one open item is the inherited `InverseConstructorTests` red. It
belongs to step 0261's scope and needs an operator decision at this step (see Tests).

## Gotchas

- **The ui-tests gate cannot pass at this step as authorized.** `InverseConstructorTests.recommitting a copied
  experiment preserves its assigned physical ids` has been red since step 0227. Selected ui-tests counts are not
  checkpointed, so 84 is this run's measurement.
- **`WaveLength` and `IncidenceAngle` are `Berreman.Fields` types,** not `Berreman.Geometry` types.
- **The UI file now opens `ItemModels` and `ItemEvaluation`,** which their extension members need. They are opened
  just after `OpticalConstructor.Domain`, so every later open keeps its precedence. The orientation records are
  annotated `ElementOrientation`.
- **`ItemEvaluation.sequenced` is private.** `chartForRun` therefore collects `Result`s with a local `inOrder`
  (`tryPick` / `choose`), since `ItemEvaluation.fs` is outside this step's edits.

## Changelog

- 2026-11-10 — Step 0252:
  - The run law folds every element and object through its Mueller matrix from a real source emission.
  - `chartForRun` builds steps through `ItemEvaluation`.
  - `behaviorInputStokes` and `runInputStokes` retired.
  - Constructor tests 1399 → 1401; one new UI fact.
  - Blocked on the inherited `InverseConstructorTests` red.
