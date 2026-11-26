# Step 0364 — impl plan

LinkParameters' pure half (spec 0050 S11.1; 001 §L, §P).

## Approach

- `Studies.fs`:
  - `StudyError` gains `InvalidParameterLinkErr of reason : string`, rendered `the link is not valid: <reason>`.
  - Step 363's parameter-existence check moves out of `withParameterScope` into one private member,
    `Study.missingParameterOpt`, so `withParameterScope` and `withLink` share the one rule (reuse, never copy).
  - `ParameterLink.describe` renders `<follower> = <leader>`. Step 363's `ScopeNotAllowedErr` text and every new
    `InvalidParameterLinkErr` text read it.
  - `Study.withLink (link) (used) : Result<Study, StudyError>`, in the `type Study with` block that holds `scopeOf`,
    checks in the step's order:
    1. the follower, then the leader, through `missingParameterOpt` (`UnknownStudyParameterErr`);
    2. a self link;
    3. a follower already in a link, as its follower or its leader;
    4. a leader that follows a link (no chain);
    5. `scopeOf` differing.
    Each answers `InvalidParameterLinkErr "<follower> = <leader>: <rule>"`. Otherwise the link is appended.
- `StudyTests`: the two facts the step names, over step 362's CPL1 / CPL2 / Phone items.
- `StudySessionTests`: the error-render fact gains the `InvalidParameterLinkErr` row.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Studies.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudySessionTests.fs`

## Risks

- ELEV: the new private member and `describe` must stay clean. They take and return only our types and strings on
  members, which members may speak.
- Behaviour of `withParameterScope` must not change. The extraction keeps its texts, and its link text reads
  `ParameterLink.describe`, the same string.
