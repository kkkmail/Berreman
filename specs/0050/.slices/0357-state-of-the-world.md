# Step 0357 — state of the world

## Where we are

This is spec 0050 S11.1 (studies: unknowns, scopes, links, sessions and opened materials), its second step on
sessions. Step 0356 made a session's gain model data. Step 0357 makes the sessions themselves data: a study's
experiments fall into session blocks, and each session's dark comes from the Dark runs (001 §L; the S11.1 brief;
step 285's hand-set `SessionName`). Reading the dark level from those runs' data is the forward model's job (S11.2).
Later S11.1 steps add the study's unknowns, scopes, links and opened materials to `Studies.fs`.

## What's working

- Added `Studies.StudySession = { name : SessionName; experiments : ExperimentName list }`, one session block.
- `StudySession.splitAll` gathers each hand-set session and splits the other runs at each Diagnostic run into
  `Session <k>` blocks. `k` is the least name no hand-set session or earlier block holds. No session is empty, and
  sessions are ordered by their first experiment.
- `StudySession.darkRunsIn` answers the session's own Dark runs in its order, or else every Dark run of the study.
- Added two `StudySessionTests` facts over test-local experiments built like `ExperimentProxyTests`' E1.

## Tests

These are development runs of each gate's own command from `0357.gates`, over the final tree. The gate engine
runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; warnings only NU1902 (exempt) and the vendored MathNet C# `SYSLIB0051` | 76 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; known findings 0; suppressed 1510 (ELEV004 excluded, unchanged) | 223 s | 600 s |
| constructor-unit-tests | Passed!, 1574 of 1574, 0 skipped; step 0356 measured 1572, so +2. Both new facts Passed under their authorized ids | 70 s | 180 s |
| invariant-s36-\* (20) | each snapshot command reports `INVARIANT HOLDS` | under 1 s each | 300 s |

- No run went past its effective deadline.
- The elevation run had no comparison base in this shell, so it analysed all 13 projects. That is wider than the
  gate's arc-origin scope.

```yaml
gates:
  constructor_unit_tests: 1574
```

## Architecture

- **`StudySession` follows `SessionGainModel.ruleWith`** in `Studies.fs`. It adds no other type, member or option.
  `splitAll` is a static member and `darkRunsIn` an instance member, so no free function is added.
- **`splitAll` is one fold over the experiments as given.** Every hand-set name is collected first, so an automatic
  block takes its `Session <k>` name the moment its first run opens it.
  - A hand-set run (a hand-set Diagnostic run included) joins its own session and leaves the open block alone.
  - A Diagnostic run with no hand-set session closes the block.
  - Any other run joins the open block, or opens a new one.
  - Appending each new session at its first run gives the first-experiment order. A block only opens on a run that
    joins it, so no session is empty.
- **A session name is free by `SessionName` equality.** It does not use the case-insensitive file-stem rule of
  `SetupName.firstFree`, because a session is not a file.

## Deferred

Nothing. Every item of the step's brief landed this round.

## Gotchas

- `StudySession`'s labels `name` / `experiments` are also labels of `Experiment`, `SetupReference` and
  `ExperimentCollection`. Annotate a `StudySession` record literal (`splitAll` does; the tests use `sessionOf`).
- The test experiments share `ExperimentId 1`, because the brief has them differ only in name, role and session.
  Sessions never read the id.

## Changelog

- 2026-11-13 — step 0357: added `StudySession` with `splitAll` (hand-set sessions plus `Session <k>` blocks split at
  Diagnostic runs, in first-experiment order) and `darkRunsIn` (own Dark runs, else every Dark run). Added two
  `StudySessionTests` facts; constructor-unit-tests 1572 → 1574.
