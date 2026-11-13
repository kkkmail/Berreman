# Step 0363 — state of the world

## Where we are

Spec 0050, section S11.1 (studies).

- Steps 0358–0362 gave the study its shape, its file, AddExperimentsToStudy, OpenMaterial's working copy, and its
  unknowns with their scopes and owners.
- This step lands SetParameterScope's pure half (001 §P; 001 §L "Scope is the sharing rule").
- Step 0364 brings LinkParameters, whose links always join parameters of one scope.

## What's working

- Added `Study.withParameterScope`, SetParameterScope's pure half. It replaces a parameter's scope entry in place, or
  appends one.
- It refuses a parameter the study does not use, whether an item outside `used`, a name the item lacks, an unopened
  material version or a component the copy lacks. That refusal is `UnknownStudyParameterErr`.
- It refuses a scope the parameter's kind does not take, with `ScopeNotAllowedErr`. An item parameter takes Item,
  Session or Experiment, and a material component takes Material only.
- It refuses a parameter that is a link's follower or leader, with `ScopeNotAllowedErr` naming the link.
- `StudyError` gains `ScopeNotAllowedErr`, rendered `the parameter cannot take that scope: <reason>`.

## Tests

These are development runs of each gate's own command from `0363.gates`, over the final tree:

- **build:** exit 0, `0 Error(s)`, in 75 s (deadline 600 s). The only warnings are the exempt NU1902 and the vendored
  MathNet SYSLIB0051 pair.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 220s`, with 1518 ELEV004 findings suppressed, as at step 0362 (deadline 600 s).
- **constructor-unit-tests:** `Passed!`, 1599 of 1599 (1597 + 2), in 72 s (deadline 180 s).
  - Passed: `setting a parameter's scope replaces its entry in place or adds it, and the default applies until then`.
  - Passed: `a scope is refused for a parameter the study does not use, for a linked parameter, and MaterialScope fits
    a material component only`.
  - Passed: the widened `every study error renders its condition and its payload`.
- **invariant-s36-\* (20):** each decoded `--find-nothing` grep finds nothing over the final tree.

No run went past its deadline.

```yaml
gates:
  constructor_unit_tests: 1599
```

## Architecture

- **`withParameterScope` sits in the `type Study with` block that holds `scopeOf`.** That block is an intrinsic
  augmentation in the same module, so `Item` (from `Kit`) and `ScopedParameter` are both in view. No type, free
  function or other member was added.
- **The allowed scopes per kind are a local list.** An item takes `[ ItemScope; SessionBlockScope; ExperimentScope ]`
  and a material `[ MaterialScope ]`. The refusal renders them through `ParameterScope.text`, never `%A`.
- **The replacement uses `List.tryFindIndex` and `List.updateAt`,** so the entry keeps its position and the list keeps
  its length.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **Refusals run in the step's order:** the parameter, then the kind, then the link. A linked material component
  under `ItemScope` answers the kind refusal.
- **Both ends of a link are refused, matched on the whole `StudyParameterRef`,** as `unknownsWith`'s follower filter
  is.
- **A known parameter can take a scope.** Only a parameter the study lacks is refused. The scope applies once the
  parameter is freed.
- **`specs/0050/.manifest.state.json` was already modified** when the round started. It is the supervisor's, and this
  round did not touch it.

## Changelog

- 2026-11-13: step 0363 added SetParameterScope's pure half:
  - `Study.withParameterScope`;
  - `StudyError.ScopeNotAllowedErr`;
  - two `StudyTests` facts, and the new case in `StudySessionTests`' error-render fact.
