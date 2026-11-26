# Step 0372 — impl plan

## Approach

Append the study's parameter-vector layout to `OpticalConstructor.Domain/StudyForward.fs`, after `measuredRowsFor`
(step 371), in the order the how-to lists it:

1. `StudyForwardContext` (`[<ReferenceEquality>]`): the kit (`StudyContext`), `Library.LibraryProxy`, `MaterialProxy`,
   `ExperimentDataProxy`, `choiceOf : Item -> ItemModelChoice option`, `SolverParameters`.
2. `StudyRunUse` = `ResidualRunUse` | `DarkLevelRunUse` | `UnreadRunUse`, and `ExperimentRole.studyUse` matching every
   role (no wildcard).
3. `Study.fittedRunsIn`: `enabledExperiments` resolved in `experiments`, kept when the role's use is `ResidualRunUse`.
4. `StudyVectorEntry = { unknown : StudyUnknown; owner : UnknownOwner }`.
5. `StudyParameterVector of float[]`, a `NumericCarrier`, with `.value`, `ofParameterValues` and `valueAt`.
6. `StudyParameterLayout = { entries; sessions }` with `startVector`.
7. `Study.parameterLayoutWith`: sessions from `sessionsIn`; per unknown of `unknownsWith (kit.itemsUsedBy this)`, one
   entry per owner of `unknown.owners sessions users`, users the fitted runs whose resolved slots hold the item (item
   parameter) or an item whose `choiceOf` is a `StackSampleChoice` whose sample's `openedMaterialsIn this` holds the
   version (material parameter). An unresolved setup holds nothing.

Two facts in `StudyForwardTests` over test-local CPL1 / Phone / QZ items made by `ItemDraft` from the shipped item
models (`BuiltinCatalogue.itemModel`), a test-local uniaxial crystal version opened by the study with `n_o` freed,
setups by `Setup.create` over `SetupTests`-style slots, and experiments E1, E2 (Science, Session 1), D1 (Diagnostic),
E3 (Science).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/StudyForward.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyForwardTests.fs`

No project file changes (both files are already compiled).

## Risks

- **ELEV010**: `member this.valueAt (index : int) : ParameterValue` mixes a bare `int` with an elevated type; ELEV010
  checks members and cannot be silenced (precedent: `ItemModels.ParameterNameIndex`, "elevated because ELEV010
  refuses an `int` beside `ParameterName`"). Plan: elevate the index as a single-case `StudyVectorIndex` and record the
  deviation.
- Record-label inference (`entries`, `sessions`, `unknown`, `owner`): annotate constructions.
- Opening `Studies` / `Setups` / `ExperimentData` / `Berreman.Solvers` in `StudyForward.fs` must not shadow names the
  step-371 code already uses (names are unique across our assemblies, so only third-party shadowing could bite).
