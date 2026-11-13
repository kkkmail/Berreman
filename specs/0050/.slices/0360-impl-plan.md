# Step 0360 — impl plan

## Approach

This step lands AddExperimentsToStudy's pure half (spec 0050 S11.1; 001 §P, §L) on `Study` in
`OpticalConstructor.Domain/Studies.fs`:

- `member this.withExperiments (names : ExperimentName list) (kitExperiments : Experiment list) : Result<Study, StudyError>`.
- **Refusal.** The first of `names` that no kit experiment has is refused as `UnknownStudyExperimentErr`, naming it, and
  nothing changes.
- **Otherwise** it answers the study with its own experiments in place, followed by each given name it does not hold
  yet, in the given order and each once. Every other field is kept, since scopes, links and gain settings name
  parameters and sessions, not the experiment list.
- **No removal member.** Disabling an experiment is step 365's.

## Reuse, not copy

`Study.create` already holds the "first experiment the kit lacks" check and its `UnknownStudyExperimentErr` text. That
check moves into one private static member on `Study`, `unknownExperimentAmong`, which both `create` and
`withExperiments` call. `create`'s local `inKit` is deleted, and its behaviour and message are unchanged.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Studies.fs`: the shared private check, `create` calling it,
  and the new `withExperiments`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyTests.fs`: two facts.
  - `adding experiments appends the new ones in order and keeps those the study holds`: a study of [E2], with
    non-default other fields, given [E3; E2; E1; E3], answers [E2; E3; E1] with its other fields equal.
  - `adding an experiment the kit lacks changes nothing`: given [E1; E9], it answers `UnknownStudyExperimentErr`
    naming E9.

## Risks

- **ELEV.** No primitive appears in the new signatures; the private member returns `StudyError option`.
- **Count.** The two facts are new, so constructor-unit-tests rises by exactly 2.
