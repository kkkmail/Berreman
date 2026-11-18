# Step 0478 — impl log

## Progress

- Read the arc-runner and IMPLEMENT system prompts, the project prompt, AGENTS.md, the slice and its gates.
- Read the five operator rulings R0001–R0005 (`.manual/030`, `031`, `033`, `035`, `036`). None names this step. For
  0478 they bind only preservation:
  - the dispatch rule;
  - the analyzer law;
  - element identity;
  - the single Jacobian and report;
  - the concurrent residual;
  - the single `rhoT` read;
  - the server collector.

  This step edits none of that code.
- Checked name uniqueness (ELEV014/016/019) over every `.fs` under `Berreman/`. None of `PlanCriterion`,
  `PlanOption`, `RankedOption`, `DeterminantPlanCriterion`, `TargetTracePlanCriterion` or `rankWith` existed.
- (1) In `PlannerInformation.fs`, after step 477's declarations (last, after the `ReducedChiSquared` extension), added:
  - `PlanCriterion` with its `text`;
  - `PlanOption<'Candidate>`;
  - `RankedOption<'Candidate>`;
  - the intrinsic extension `type PlannerInformation with static member rankWith`.

  Each doc is one to three lines and cites S14.2.
- `rankWith` works in rounds, through a local `rankFrom` over a local `bestFrom`:
  - Each round walks the options left in their given order. Each option is joined after the baseline and the options
    already ranked, and `PlannerInformation.over` is taken of the whole.
  - An option replaces the held best only when it is strictly `better`, so a tie keeps the earlier option.
  - The chosen option leaves the list by position (`List.removeAt`), because `'Candidate` carries no equality
    constraint.
  - The first `over` refusal returns at once, so nothing is ranked in part.
  - With no options, the answer is `Ok []`.
- `better` is a local closure, never a member or private function. A signature mixing `PlannerInformation` with
  `bool` would be ELEV010 / ELEV010P, which step 477 measured on members.
  - Rank comes first.
  - At equal rank, `DeterminantPlanCriterion` takes the larger log determinant.
  - At equal rank, `TargetTracePlanCriterion` takes the smaller trace when both traces exist (full rank), else the
    larger log determinant.
  - The match is exhaustive, with no wildcard on the criterion.
- (2) `PlannerInformationTests.fs` gained three facts, named exactly as the census names them. They run over literal
  rows at noise 1 and coordinates a and b.
  - Each `ResidualJacobian` is wrapped at its literal (elevated at origin). The helper `atUnitNoise` takes a
    `ResidualJacobian`.
  - Each ranking is checked in both given orders, so the order comes from the criterion, not from the list.
- The tests' red: the facts were written with the production code, so before it they fail to compile (`rankWith`
  and the types are undefined). This is the protocol's "NameError counts as red".
- Build 1 is green. `PlannerInformationTests` alone: 7 of 7. Elevation: OK. The whole constructor suite: 1873 of 1873.
  All 21 s46 invariants find nothing.
- Started the gates' selected long runs in the background, after elevation was clean (0477 gotcha: a rebuild during
  a run fails on locked DLLs).
- Reworded `rankWith`'s doc comment (comment only) while they ran. Rebuilt after they finished.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/PlannerInformation.fs` (+87):
  - `PlanCriterion`;
  - `PlanOption`;
  - `RankedOption`;
  - `PlannerInformation.rankWith`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerInformationTests.fs` (+91): three facts and their
  private helpers (`atUnitNoise`, `bothCriteria`, `rankedOver`, `candidatesOf`, `traceOf`).

Neither project file changed: both files were already in their compile lists at step 477. Line endings are LF: 0 CR
bytes in both files, and `git diff --numstat` is the same with and without `--ignore-cr-at-eol`.

## Testing state

Development runs only; the arc-runner's gate engine is the authority (Invariant 6).

- **build:** exit 0 in 84 s (full compile).
  - The warnings are 12 `NU1902` (exempt) and 4 `SYSLIB0051` in the vendored `MathNetNumerics/Numerics/Exceptions.cs`.
    That file is untouched; its warnings are in every arc build log from `0001-build-tests-project.log` on, and it
    is C# in vendored third-party code, not ours.
  - There is no FS and no MSB warning.
- **elevation** (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`, no base, so all 13 projects):
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 226s` (deadline 600 s).
  - The known findings are 0, and the suppressed count is 1770, unchanged from 0477.
- **constructor-unit-tests** (whole suite, the gate's `all_discovered`): `Passed!`, 1873 of 1873 in 112 s
  (deadline 180 s).
  - The count rises by 3 over 0477's 1870.
  - The three new identities are `Passed` in the TRX.
- **invariant-s46-\*** (21): each decoded `git grep` finds nothing.
- **unit-tests**, selected `BerremanTests.MuellerInverseTests` and `BerremanTests.BiaxialInverseTests`, with the
  gate's own command and filter: `Passed!`, 31 of 31 in 1103 s (deadline 7200 s).
- **kit-acceptance**, selected `OpticalConstructor.Ui.Tests.KitAcceptanceTests` under `Category=kit-acceptance`:
  `Passed!`, 14 succeeded and 1 skipped (total 15) in 1420 s (deadline 3600 s).
  - The skipped fact is AT-2's Explicit five-seed ensemble; the gate's skip policy is `allow`.
  - AT-2's grid-generator fact, which includes the fit through the shell, took 791 s, inside R0005's 1800 s bound.
- **Final build** after the comment-only doc edit: exit 0 (incremental, 11 s), with only the 12 exempt `NU1902`
  warnings. `PlannerInformationTests` on that build: 7 of 7.
  - Elevation was not re-run. The edit changes a `///` doc comment only, and the analyzers read code.

The two long runs ran at the same time, as at 0477. No run exceeded its effective deadline.

## Artifacts

- `specs/0050/.artifacts/0478-dev-build.log`
- `specs/0050/.artifacts/0478-dev-planner-class.log`
- `specs/0050/.artifacts/0478-dev-elevation.log`
- `specs/0050/.artifacts/0478-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0478-dev-unit-tests.log`
- `specs/0050/.artifacts/0478-dev-kit-acceptance.log`
- `specs/0050/.artifacts/0478-dev-build-final.log`
- `specs/0050/.artifacts/0478-dev-planner-class-final.log`

## Gotchas

- **No bool-returning member.** `PlanCriterion` could have carried a `prefers (a : PlannerInformation) (b :
  PlannerInformation) : bool` member, but ELEV010 checks members (0477). The comparison is a closure local to
  `rankWith`. Local functions are not analyzed, as `over`'s `refusalOf` shows.
- **`rows` is two records' label.** `PlannerInformation.rows : ResidualJacobian` and `PlanOption.rows : CandidateRows`
  share it. Every access in the new code is through an annotated value (`planOption`, `best`), and each test option
  is annotated `PlanOption<string>`.
- **An unvalidated baseline with no options.** With no options there is no `over` call, so a noiseless baseline
  answers `Ok []`. The slice defines the refusal as "the first `over` error", and validating the baseline up front
  would be an addition it does not ask for.
- **Ties are exact.** The comparison is strict and has no tolerance (the slice adds no threshold). The tie fact
  therefore uses identical rows, whose joined informations are bit-identical.
- **`'Candidate` in the tests is a `string` label.** It is the caller's payload (the Plan view's candidate at step 491).
  The test projects are outside the ELEV scope, and the label wraps no domain quantity.
