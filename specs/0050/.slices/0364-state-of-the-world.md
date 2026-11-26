# Step 0364 — state of the world

## Where we are

Spec 0050, section S11.1 (studies).

- Steps 0358–0363 gave the study:
  - its shape and its file;
  - AddExperimentsToStudy and OpenMaterial's working copy;
  - its unknowns with their scopes and owners;
  - SetParameterScope.
- This step lands LinkParameters' pure half (001 §L "Explicit links ("CPL2.δ = CPL1.δ") are constraints"; 001 §P
  `LinkParameters`). A link's follower takes its leader's value, so `unknownsWith` leaves it out (step 0362).

## What's working

- Added `Study.withLink`, LinkParameters' pure half, which appends a link between two parameters of one scope.
- It refuses a parameter the study lacks with `UnknownStudyParameterErr`. That rule is now one private member that
  `withParameterScope` shares.
- It refuses, with `InvalidParameterLinkErr`, each of these:
  - a self link;
  - a follower already in a link;
  - a leader that follows a link (no chain);
  - two parameters of differing scopes.
- `StudyError` gains `InvalidParameterLinkErr`, rendered `the link is not valid: <reason>`. `ParameterLink.describe`
  renders a link as `<follower> = <leader>`.

## Tests

These are development runs of each gate's own command from `0364.gates`, over the final tree:

- **build:** exit 0, `0 Error(s)`, in 76 s (deadline 600 s). The only warnings are the exempt NU1902 and the vendored
  MathNet SYSLIB0051 pair.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 233s`, with 1518 ELEV004 findings suppressed,
  as at step 0363 (deadline 600 s).
- **constructor-unit-tests:** `Passed!`, 1601 of 1601 (1599 + 2), in 73 s (deadline 180 s).
  - Passed: `linking CPL2's retardance to CPL1's makes it follow and leaves it out of the unknowns`.
  - Passed: `a link to itself, a chain, a second link of one follower, a missing parameter or differing scopes is
    refused`.
  - Passed: the widened `every study error renders its condition and its payload`.
- **invariant-s36-\* (20):** each decoded `--find-nothing` grep finds nothing over the final tree.

No run went past its deadline.

```yaml
gates:
  constructor_unit_tests: 1601
```

## Architecture

- **`withLink` sits in the `type Study with` block that holds `scopeOf` and `withParameterScope`.** It reads
  `scopeOf` for both ends, so default scopes and explicit ones both count.
- **One parameter-existence rule.** Step 0363's local check is now the private member `Study.missingParameterOpt`,
  moved unchanged. `withParameterScope` and `withLink` both call it, and its refusal texts are unchanged.
- **`ParameterLink.describe` is the one rendering of a link.** Every `InvalidParameterLinkErr` reason opens with
  `<link>: ` and ends with the broken rule. Step 0363's linked-parameter scope refusal reads the same member, and its
  text is unchanged.
- **The checks run in the step's order:** the follower's existence, then the leader's, then the self link, the
  follower already linked, the leader that follows, and differing scopes. No unlink member was added.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **A follower that follows a link and one that leads a link are one refusal,** as the step names it. The text says
  which, so a leader made a follower and a second link of one follower read differently.
- **No parameter can both lead and follow,** so the first held link that names the follower tells its role.
- **A known parameter can be linked.** Only the five conditions are refused. `unknownsWith` leaves out a follower
  whatever its state.
- **`specs/0050/.manifest.state.json` was already modified** when the round started. It is the supervisor's, and this
  round did not touch it.

## Changelog

- 2026-11-13: step 0364 added LinkParameters' pure half:
  - `Study.withLink`;
  - `StudyError.InvalidParameterLinkErr`;
  - `ParameterLink.describe`;
  - the shared private `Study.missingParameterOpt`;
  - two `StudyTests` facts, and the new case in `StudySessionTests`' error-render fact.
