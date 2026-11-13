# Step 0372 — impl log

## Progress

- Read the task, both system prompts, the project prompt, the slice and its gates snapshot. No operator note or
  ruling is attached.
- Read the seams the step builds on: `Studies.fs` (`Study`, `enabledExperiments`, `withExperimentEnablement`,
  `sessionsIn`, `unknownsWith`, `StudyUnknown.owners`, `UnknownOwner`, `Sample.openedMaterialsIn`, `StudyContext`,
  `itemsUsedBy`), `Experiments.fs` (`ExperimentRole`), `Kit.fs` (`ItemModelChoice`, `ItemDraft`,
  `UnknownParameterData`), `Setups.fs` (`Setup.create`, `Experiment.slotsIn`, `SlotContent.itemOpt`),
  `ExperimentDataProxy.fs`, `ElementId.fs` (`LibraryProxy`), `MaterialLibrary.fs` (`MaterialProxy`), the step-371
  `StudyForward.fs` and the test fixtures (`StudyTests`, `StudyMaterialTests`, `SetupTests`, `PrefitCheckTests`,
  `BuiltinCatalogue`, `CrystalMaterialFixtures`). Confirmed none of the new type, case or member names exists.
- Read STL's ELEV001 / ELEV005 / ELEV010 analyzers (read-only). ELEV010 checks public members and cannot be silenced:
  an `int` beside an elevated type is a finding, so the prescribed `valueAt (index : int)` would fail (see Gotchas).
- `StudyForward.fs`, after `measuredRowsFor`, in the how-to's order: `StudyForwardContext` (`[<ReferenceEquality>]`),
  `StudyRunUse` (`ResidualRunUse` / `DarkLevelRunUse` / `UnreadRunUse`), `ExperimentRole.studyUse` (every role, no
  wildcard), `Study.fittedRunsIn`, `StudyVectorEntry`, `StudyVectorIndex` (the deviation), `StudyParameterVector`
  (`NumericCarrier`, `.value`, `ofParameterValues`, `valueAt`), `StudyParameterLayout` (`startVector`) and
  `Study.parameterLayoutWith`. New opens: `Berreman.Solvers`, `MaterialLibrary`, `ItemModels`, `Setups`, `Studies`,
  `ExperimentData`; `Library.LibraryProxy` is written qualified, as `Setups.fs` writes it.
- `StudyForwardTests.fs`: test-local fixtures and the two census facts, appended after the step-371 facts. CPL1, the
  Phone and QZ are `ItemDraft`s over the shipped item models (`BuiltinCatalogue.itemModel`, step 210); QZ is a
  `StackSampleChoice` of a test-local 1 mm plate whose substrate is a test-local uniaxial crystal version
  (`CrystalMaterialFixtures.uniaxialParameters`); the study opens that version and frees `n_o`. Setups come from
  `Setup.create` over slots built as `SetupTests` builds them. The forward context uses the Built-in library and
  materials, a data proxy that answers every read with an error, and `SolverParameters.defaultValue`.
- Built, ran the whole constructor suite, the twenty s37 invariants and elevation from the arc base (below).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/StudyForward.fs` (+130 lines).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyForwardTests.fs` (+320 / -1: new opens, the module doc
  line, fixtures, two facts).

No project file changed (both files were already compiled). All LF (`git diff --numstat` equals
`--ignore-cr-at-eol`, no CR in either file). `specs/0050/.manifest.state.json` was already modified at round start
(supervisor state) and is untouched by this round.

## Testing state

All run locally after the final edit (Release x64):

| Gate | Result | Duration | Deadline |
|---|---|---|---|
| build | exit 0, `0 Error(s)`; the only warnings are NU1902 advisories (exempt) | 24 s (incremental; 46 s for the first full build) | 600 s |
| constructor-unit-tests | `Test run summary: Passed!`, 1625 succeeded, 0 failed, 0 skipped (checkpoint 371: 1623; +2); both new facts `Passed` in the TRX | 72 s | 180 s |
| elevation (arc base `f395597c`) | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` (all 13: the slnx changed since the arc base), exit 0; Domain `findings=0` | 224 s | 600 s |
| twenty s37 invariants | each `section_invariants --find-nothing` exit 0 | < 5 s each | 300 s |

Every requirement of the slice landed this round; nothing is deferred.

## Artifacts

- `specs/0050/.artifacts/0372-build.log`
- `specs/0050/.artifacts/0372-constructor-unit-tests.log`
- `specs/0050/.artifacts/0372-elevation.log`
- `specs/0050/.artifacts/0372-invariants.log`

## Gotchas

- **Deviation (recorded when made): `valueAt` takes a `StudyVectorIndex`, not an `int`.** The how-to writes
  `member this.valueAt (index : int) : ParameterValue`. ELEV010 (NoPrimitiveBesideElevated) checks public members,
  is not silenced by any boundary mark, and reports an `int` beside `ParameterValue`; the tree's precedent is
  `ItemModels.ParameterNameIndex`, "elevated because ELEV010 refuses an `int` beside `ParameterName`". So the place is
  elevated as `StudyVectorIndex of int` (counted from 0, `.value`), declared just before `StudyParameterVector`. The
  later step that reads `vector.valueAt k` writes `vector.valueAt (StudyVectorIndex k)`. It is one type beyond the
  eight contract symbols, as step 371's `MeasuredAbscissa.toSweptValue` was.
- `valueAt` indexes the array: an index past the end throws. Its doc states the precondition (the caller has checked
  one value per entry, which the later step's `VectorLengthErr` does).
- Users: an item parameter's users are the fitted runs whose resolved slots hold its item id; a material parameter's
  are the fitted runs whose slots hold a kit item whose `choiceOf` is a `StackSampleChoice` whose sample's
  `openedMaterialsIn this` holds that version. A run whose setup does not resolve holds nothing. An item- or
  material-scoped unknown has its one owner whatever its users (`owners`, step 362), so an unknown of an item used
  only by a disabled or unread run still has its one entry: `unknownsWith (kit.itemsUsedBy this)` is over every study
  experiment, as the how-to says.
- `layout.sessions` is `sessionsIn` over all the study's experiments, the disabled ones included, so a session owner
  always names a session of the list.
- The material-user rule is only observable through an owner that depends on users, and a material component takes
  only `MaterialScope` (`withParameterScope`). The test therefore asserts it as the how-to does (n_o keeps its one
  `MaterialOwner` when E3's setup holds no QZ) and adds observable cases for the same rule on item unknowns: E3 over a
  setup without CPL1 owns no per-experiment retardance value, and E3 over a setup the kit lacks uses nothing (the
  Phone's gain loses `Session 2`, which E3 alone forms).
- The test fixtures re-state two small `SetupTests` shapes (`emptySlot`, a `holding` slot) as the how-to asks ("slots
  built as `SetupTests` builds them"); those helpers are private to `SetupTests.fs`, which is outside this step's
  authorized edits. The unknowns are written as records, not through a copy of `StudyTests`' private `unknownFrom`.
- Start values are compared exactly (`Assert.Equal<float[]>`): the layout passes each start through unchanged, so no
  tolerance applies.
