# Step 0363 — impl plan

SetParameterScope's pure half (001 §P `SetParameterScope`; 001 §L "Scope is the sharing rule"; spec 0050 S11.1).

## Approach

- `Studies.fs`, `StudyError`: add `ScopeNotAllowedErr of reason : string`, rendered
  `the parameter cannot take that scope: <reason>`. No other code matches `StudyError` exhaustively (grep), so the
  new case breaks no match.
- `Studies.fs`, the `type Study with` block holding `scopeOf`: add
  `member this.withParameterScope (scoped : ScopedParameter) (used : Item list) : Result<Study, StudyError>`.
  Checks, in the order the step lists them, each refusing with nothing changed:
  1. the parameter is the study's: an item parameter needs an item of `used` with that id that holds the name
     (`Item.tryFindParameter`); a material component needs an opened copy of that version holding the name. Else
     `UnknownStudyParameterErr` whose reason starts with the reference's `describe`.
  2. the scope fits the parameter's kind: an item parameter takes `ItemScope`, `SessionBlockScope`,
     `ExperimentScope`; a material component `MaterialScope` only. Else `ScopeNotAllowedErr` naming the reference's
     `describe` and the scope's `text`.
  3. the parameter is no link's follower or leader. Else `ScopeNotAllowedErr` naming the link (follower = leader).
  Otherwise `scopes` with the entry replacing that parameter's earlier one in place, or appended.
- Tests in `StudyTests` (two new facts over step 362's `cpl1`, `cpl2`, `phone`, `unknownsStudy`), and the
  `StudySessionTests` error-render fact widened by the new case.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Studies.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudySessionTests.fs`

## Risks

- ELEV: no new free function, no new type; local closures follow `ruleWith`'s precedent; the new DU case ends in
  `Err` and its name is unique among ours and STL's (grep).
- Count: constructor-unit-tests rises by exactly 2 (two new facts; the third is a changed expectation).
