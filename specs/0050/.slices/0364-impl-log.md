# Step 0364 — impl log

## Progress

- Read the protocol, the project prompt, AGENTS.md and slice 0364. Read step 0363's impl-log, state of the world and
  post-mortem. The post-mortem was mechanical, after a 403 from the provider, and holds nothing for this step.
- Read `Studies.fs`, `StudyTests.fs` and `StudySessionTests.fs`. Grep: nothing outside `describe` and the tests
  matches `StudyError` exhaustively, and `withLink` is a new name here.
- `Studies.fs`:
  - `StudyError` gains `InvalidParameterLinkErr of reason : string`, rendered `the link is not valid: <reason>`.
  - `ParameterLink.describe` renders `<follower describe> = <leader describe>`. Step 0363's `ScopeNotAllowedErr`
    text for a linked parameter now reads it, and produces the same string.
  - Step 0363's parameter check moved, unchanged, from a local of `withParameterScope` into
    `member private this.missingParameterOpt (reference) (used) : StudyError option`. Both
    `withParameterScope` and `withLink` call it, so the rule has one body.
  - Added `Study.withLink (link : ParameterLink) (used : Item list) : Result<Study, StudyError>`, after
    `withParameterScope` in the `type Study with` block that holds `scopeOf`. It checks in the step's order:
    1. The follower, then the leader, through `missingParameterOpt`. A missing one answers
       `UnknownStudyParameterErr` naming it.
    2. A follower equal to its leader answers `InvalidParameterLinkErr "<link>: a parameter cannot follow itself"`.
    3. A follower already in a link answers `InvalidParameterLinkErr`. As that link's follower, the text is
       `"<link>: the follower already follows in <held>, and a follower takes one leader"`. As its leader, it is
       `"<link>: the follower leads <held>, and a leader follows no one"`.
    4. A leader that follows a link answers `InvalidParameterLinkErr`
       `"<link>: the leader follows in <held>, and a link does not chain"`.
    5. Differing `scopeOf` answers `InvalidParameterLinkErr`
       `"<link>: the follower is per <scope>, the leader per <scope>, and a link equates one value per owner"`.

    Otherwise it answers `{ this with links = this.links @ [ link ] }`.
- `StudyTests` (module doc names LinkParameters too), with a `linkOf` helper:
  - `linking CPL2's retardance to CPL1's makes it follow and leaves it out of the unknowns`:
    - the linked study equals `unknownsStudy` with `links = [ link ]`;
    - `unknownsWith` keeps CPL1's retardance with its own data and drops CPL2's, and is otherwise the unlinked list;
    - a second link (CPL1's `c2.fastAxisOffset` following CPL2's) is appended after the first.
  - `a link to itself, a chain, a second link of one follower, a missing parameter or differing scopes is refused`:
    - CPL1's retardance linked to itself;
    - with the first link held: the leader made a follower, the chain through a follower and the second link of one
      follower;
    - CPL1's `gain`, a name CPL1 lacks, as the follower and as the leader;
    - CPL2's `c2.fastAxisOffset` (Item) following Phone's `gain` (Session).

    Each answers its error, with the reason's prefix and rule text asserted, and the study keeps only its first link.
- `StudySessionTests`' `every study error renders its condition and its payload` gains the `InvalidParameterLinkErr`
  row (index 13).
- Built, ran constructor-unit-tests, elevation and the 20 invariants over the final tree (see Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Studies.fs`: `StudyError.InvalidParameterLinkErr`,
  `ParameterLink.describe`, `Study.missingParameterOpt` (private, moved out of `withParameterScope`) and
  `Study.withLink`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyTests.fs`: the two new facts, a `linkOf` helper and the
  module doc.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudySessionTests.fs`: the new case in the error-render fact.

## Testing state

These are development runs of each gate's own command from `0364.gates`, over the final tree.

- **build:** exit 0, `0 Error(s)`, no lowercase `error`, in 76 s (deadline 600 s).
  - The only warnings are the exempt NU1902, plus the vendored MathNet SYSLIB0051 pair that has been there since
    step 0001.
- **constructor-unit-tests:** `Passed!`, 1601 of 1601 (1599 + 2), in 73 s (deadline 180 s).
  - In the TRX, both new `StudyTests` ids and the widened `StudySessionTests` fact are `Passed`.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 233s`, with 1518 ELEV004 findings suppressed,
  as at step 0363 (deadline 600 s).
- **invariant-s36-\* (20):** I decoded each `--find-nothing` grep from `0364.gates` and ran it over the final tree.
  All 20 find nothing.

## Artifacts

- `specs/0050/.artifacts/0364-dev-build.log`
- `specs/0050/.artifacts/0364-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0364-dev-elevation.log`

## Gotchas

- **Rule 3 covers both of the step's "follower" cases.** A follower that already follows a link and one that already
  leads a link are the same refusal, as the step names it. The text says which, so the "leader made a follower" and
  the "second link of one follower" cases read differently.
- **No parameter can both lead and follow.** A leader that follows is refused, and so is a follower that leads. The
  first held link that names the follower therefore tells which role it has.
- **A known parameter can be linked.** The step refuses only the five conditions. The success fact's second link has a
  known follower, CPL1's `c2.fastAxisOffset`. `unknownsWith` already leaves out a follower, whatever its state.
- **Scopes are compared through `scopeOf`,** so the default scopes and explicit ones both count. Once linked, neither
  end can take a new scope (step 0363's refusal), so the two scopes stay equal.
- **`ParameterLink.describe` is the one rendering of a link.** It is not named in the step's contracts. Without it,
  step 0363's `"<f> = <l>"` text would be written again four times in `withLink`. It is an intrinsic member and yields
  the same string, so step 0363's assertions on that text hold unchanged.
- **The "study unchanged" assertion holds by construction.** A refusal is an `Error`, and the record is immutable.
  The fact still asserts that `linked` holds only its first link after every refusal.
- **`specs/0050/.manifest.state.json` was already modified** when the round started. It is the supervisor's, and this
  round did not touch it.
