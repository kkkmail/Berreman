# Step 0483 — impl log

## Progress

- Read the task, both system prompts, the project prompt, rulings R0001–R0005, the slice and its gates snapshot.
  - None of the rulings touches this step's files.
  - Their preserved behaviours (series dispatch, analyzing compounds, the element-identity provider, the concurrent
    residual, the single `rhoT` read) live elsewhere and stay untouched.
- `MeasurementPlanner.fs`:
  - (1) `PlannerError` gained `InstrumentEvaluationErr of reason : string`, described as
    `an optic of the kit cannot be evaluated: <reason>`.
  - (2)–(4) After step 482's `type PlannerInputs with` block:
    - `InstrumentChoice { polarizer; compensator; angles; condition }`, with the planner's constant
      `static member compensatorAngleGrid` (0°, 15°, …, 165°);
    - `InstrumentFinding = ConditionedInstrumentFinding of InstrumentChoice | CompensatorMissingFinding of
      PlannerOptic`, with `text`;
    - `type PlannerInputs with member this.instrumentFindingsWith items choiceOf materials`.
- `PlannerInputsTests.fs`: the three facts the census names, plus their helpers:
  - `noMaterials`, an empty in-memory store;
  - `instrumentFindingsOver`, `findingsValue`, `evaluatedMuellerOf` and `conditionOfRows`.
- Consequential one-line edits: the exhaustive `PlannerError` or-patterns in `PlannerInputsTests.fs`
  (`unresolvedReasonOf`) and `PlannerReportTests.fs` gained `InstrumentEvaluationErr _` on their existing line.
- Build 1 was green and every run below passed first time.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs` (+152 / −1)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerInputsTests.fs` (+175 / −2)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerReportTests.fs` (+1 / −1, consequential, see Decisions)

## Decisions

- **The physics, exactly as the slice gives it, through the one item evaluator.**
  - Each optic is bound by `ItemModelChoice.boundWith materials (ParameterValues.ofParameters item.parameters)`.
  - Each optic is turned by `BoundOpticalElement.muellerAt (ElementOrientation.transmittingAt θ)` at the first source's
    `waveLength`.
  - `s(θ) = M_C(θ) · (M_P(0) · Propagation.unpolarizedStokes)`, and `a(θ)` is row 0 of `M_P(0) · M_C(θ)`, read through
    `MuellerMatrix.element`.
  - Each instrument row is `MuellerReconstruction.designRow s a`, each generator state with each analyzer row: 16 × 16.
  - Both conditions are `ResidualJacobian.conditionNumber` (`MuellerInverse.fs`), the MathNet SVD ratio.
  - Nothing is cached or stored, and no option is added.
- **The choice of angles.**
  - The 495 four-subsets are enumerated by a local recursive `subsetsOf`, which yields the grid's lexicographic order.
  - `List.minBy` keeps the first of equal keys, so a tie keeps the earliest set.
  - The analyzer turns to the same four angles.
- **Only paired optics are evaluated.** A polarizer with no compensator in the lab gets `CompensatorMissingFinding`
  without its matrix being read, because that finding needs no physics. The slice's refusal clause therefore applies
  to the optics a pairing evaluates. With several polarizers and compensators, the first refusal in polarizer-then-
  compensator order answers, through `ItemEvaluation.sequenced`.
- **Refusal reasons** are `<label>: <cause>`:
  - `the kit holds no such item` when the optic's item is missing from `items`;
  - `its model is not in the library` when `choiceOf` answers `None`;
  - the `ItemEvaluationError`'s own `describe` otherwise.
- **Compound optics.** `CompoundOptic` is neither a polarizer nor a compensator, so a circular polarizer is never
  paired. A compound holding no polarizer is a `CompensatorOptic` (step 471's `compoundRoleOf`) and is paired as one.
- **ELEV-clean by construction.**
  - The `deg` measure is written qualified (`15.0<Berreman.Constants.deg>`), so `MeasurementPlanner.fs` gains no `open`.
  - The text destructures `ConditionNumber` rather than reading `.value`.
  - Each design row is read by pattern (`let (MuellerReconstruction.DesignRow design) = …`).
  - The two `ResidualJacobian`s are born at their `let` (`[| … |] |> ResidualJacobian`), as `FitQuality.fs` builds
    its own.
  - The angle text reuses the module's private `Angle.plannerDegreesText` (one decimal, invariant culture). The
    condition is `F2` under `CultureInfo.InvariantCulture`.
- **Consequential edit outside `effects.edits`: `PlannerReportTests.fs`.**
  - Its `a target the crystal does not have refuses the report, naming it` matches `PlannerError` exhaustively, and
    the test project compiles with `--warnaserror+:25`. A new case is therefore a build error there.
  - The or-pattern gained `InstrumentEvaluationErr _` on its existing line. No assertion or identity changed.
  - This is the precedent of steps 470, 471 and 477, which recorded the same edit for their new cases.
- **Test strengthening within the three authorized identities** (no identity added, renamed or removed):
  - fact 1 checks:
    - the grid itself, and that the angles are four, distinct, from the grid and in its order;
    - that the chosen set is the earliest of the least (the tie rule);
    - the Kronecker identity: the instrument condition equals generator × analyzer condition within 1e-9 relative,
      each computed in the test from the slice's definitions;
    - the full sentence;
    - polarizer order (LP1 then LP2, LP2's choice equal to LP1's but for the polarizer);
  - fact 2 checks that CPL1 stays unpaired beside RET, and gives no finding without a source;
  - fact 3 checks the missing-item and evaluator-refusal causes (RET without its `fastAxisOffset`) and the
    `describe` prefix.

## Testing state

- Build: `dotnet build Berreman.slnx -c Release -nologo -v:m` (from `Berreman/`), the gate's own command. Green, 84 s,
  0 errors. Log: `.artifacts/0483-build-01.log`.
  - Its warnings are the exempt NU190x advisories plus four `SYSLIB0051` warnings from the vendored C#
    `MathNetNumerics/Numerics/Exceptions.cs`. Those appear on every build that recompiles MathNet (0478, 0479, 0480,
    0481 and 0482-build-01 logs all carry them) and are untouched by this step.
- `PlannerInputsTests` and `PlannerReportTests` filtered: 26 of 26. `PlannerInputsTests` alone: 14 of 14, the three new
  ids exactly as the census names them. Logs: `.artifacts/0483-planner-tests-01.log` and
  `.artifacts/0483-planner-inputs-tests.log`, with TRX in `.artifacts/0483-trx/`.
- `constructor-unit-tests`, the gate's own command over the whole suite (its selection is `all_discovered`):
  - Passed!, succeeded 1880, failed 0, skipped 0, in 116 s against a 180 s deadline.
  - Checkpoint 482 was 1877, so the count rises by exactly 3.
  - Log: `.artifacts/0483-constructor-unit-tests.log`.
- The 21 `invariant-s46-*` must-find-nothing greps, decoded from `.slices/0483.gates`: every one finds nothing.
  Commands: `.artifacts/0483-invariant-commands.txt`.
- Elevation: `Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004`, the gate's own command, from the repo root. With
  no base it analyses all 13 projects.
  - `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 241s`, known findings 0, against a 600 s deadline.
  - Domain: 0 findings (170 s). Ui: 0 findings.
  - Log: `.artifacts/0483-elevation.log`. It ran concurrently with the constructor suite, after the build.
- No run exceeded its effective gate deadline.

## Artifacts

- `specs/0050/.artifacts/0483-build-01.log`
- `specs/0050/.artifacts/0483-planner-tests-01.log`, `0483-planner-inputs-tests.log`, `0483-trx/`
- `specs/0050/.artifacts/0483-constructor-unit-tests.log`
- `specs/0050/.artifacts/0483-invariant-commands.txt`
- `specs/0050/.artifacts/0483-elevation.log`

## Gotchas

- Adding a `PlannerError` case breaks every exhaustive or-pattern over it in the test project (FS0025 is an error
  there). Today there are two: `PlannerInputsTests.unresolvedReasonOf` and `PlannerReportTests`' unknown-target fact.
  Keep each or-pattern on one line: wrapped inside `Error (…)` it fails with FS0583 (step 477).
- Mirror-image angle sets (θ ↔ 180° − θ about a polarizer at 0°) have mathematically equal generator conditions.
  The tie rule keeps the earliest set only because the test and the planner compute each condition identically (same
  rows, same order, same SVD). A test comparing against a differently ordered matrix should allow a rounding margin.
- A recursive search of `Berreman/OpticalConstructor` for catalogue JSON (`grep -r`, Glob) times out on this machine;
  search a specific folder.
