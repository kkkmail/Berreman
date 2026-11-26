# Step 0372 — state of the world

## Where we are

Spec 0050 section S11.2 (the forward model of an experiment and the study residual). Step 371 opened
`OpticalConstructor.Domain.StudyForward` with what a detector records and each measured point placed on its scan. This
step adds the parameter vector a fit searches, laid out by the generated axes: one value per owner of each unknown
(S11.2–S11.3; 001 §L, §N), with the context a study's forward model reads. Later S11.2 steps assign a vector to an
experiment's items and materials and build the predictions and the residual on this layout.

## What's working

- Add `StudyForwardContext`: the kit, library, material and data stores, `choiceOf` and the solver parameters
- Add `StudyRunUse` and `ExperimentRole.studyUse` (residual rows, a Dark run's dark level, or unread)
- Add `Study.fittedRunsIn`: the enabled experiments whose role gives residual rows, in study order
- Add `StudyParameterVector` (a numeric carrier) and `StudyParameterLayout` with `startVector`
- Add `Study.parameterLayoutWith`: one entry per owner of each unknown, owned only through enabled fitted runs that use it
- Add two `StudyForwardTests` facts (constructor suite 1623 → 1625)

## Tests

```yaml
gates:
  constructor_unit_tests: 1625
```

- build: exit 0, no warning from our code (only NU1902 advisories), 24 s incremental and 46 s for the first full
  build (deadline 600 s).
- constructor-unit-tests: Passed!, 1625 succeeded (1623 at checkpoint 371, +2), 0 failed, 0 skipped, 72 s (deadline
  180 s); both new facts are `Passed` in the TRX.
- elevation from arc base `f395597c`: OK, 0 new findings in 13 projects, 224 s (deadline 600 s).
- The twenty s37 invariants: each `section_invariants --find-nothing` exits 0.
- Nothing deferred. All durations are within their deadlines.

## Architecture

- The layout is pure: `parameterLayoutWith` reads only `context.kit` and `context.choiceOf`. The library, material and
  data stores and the solver parameters sit in the context for the later forward-model steps, as the how-to sets it.
- An unknown's users are the fitted runs whose resolved setup slots hold its item, or, for a material component, an
  item whose stack-sample choice is made of the opened version (`Sample.openedMaterialsIn`). A setup that does not
  resolve holds nothing. Owners still come from `StudyUnknown.owners` (step 362), so item and material scopes keep
  their one owner.
- `sessions` is `sessionsIn` over every study experiment, so a `SessionOwner` always names a session of the layout.
- A vector place is a `StudyVectorIndex` (0-based) because ELEV010 refuses an `int` beside `ParameterValue`, the same
  reason `ParameterNameIndex` exists. This deviates from the how-to's `valueAt (index : int)` and is recorded in the
  impl-log.

## Deferred

Nothing.

## Gotchas

- `valueAt` takes a `StudyVectorIndex`: a later step's `vector.valueAt k` is `vector.valueAt (StudyVectorIndex k)`.
  An index past the end throws, so check the vector length first (the later step's `VectorLengthErr`).
- An unknown of an item that only a disabled or unread run uses still has its one item-scoped entry:
  `unknownsWith (kit.itemsUsedBy this)` covers every study experiment, and item and material scopes ignore users.
- A material component is only `MaterialScope`, so its user rule never changes the layout today. The tests show the
  same rule on item unknowns: a setup without CPL1, and a setup the kit lacks.

## Changelog

- 2026-11-13 — Step 0372: the study parameter-vector layout in `StudyForward.fs` (`StudyForwardContext`,
  `StudyRunUse` / `ExperimentRole.studyUse`, `Study.fittedRunsIn`, `StudyVectorEntry`, `StudyVectorIndex`,
  `StudyParameterVector`, `StudyParameterLayout`, `Study.parameterLayoutWith`) plus two `StudyForwardTests` facts
  (constructor suite 1623 → 1625).
