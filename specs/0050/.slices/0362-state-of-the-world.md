# Step 0362 — state of the world

## Where we are

Spec 0050, section S11.1 (studies).

- Steps 0358–0361 gave the study its shape, its file, AddExperimentsToStudy and OpenMaterial's working copy.
- This step names the study's unknowns (001 §L, "the unknowns (owner + scope)").
  - Each unknown is an item parameter or an opened-material component whose state is `UnknownParameter`, with the
    scope it is shared by.
  - Each scope gives the unknown its owners. The fitter holds one value per owner (S11.2–S11.3).
- The forward model that picks each unknown's users (S11.2) and the fitter's vector (S11.3) are later steps.

## What's working

- Added `UnknownOwner`, what holds one value of an unknown: an item, a material version, a session block or an
  experiment. It is derived, so it has no tag.
- Added `StudyUnknown`, a parameter with its scope and its start, bounds and prior.
- Added `Study.scopeOf`: the study's own scope for a parameter, else the default.
  - A material component is `MaterialScope`.
  - A detector's `gain` is `SessionBlockScope`.
  - Any other item parameter is `ItemScope`.
- Added `Study.unknownsWith`: the used items' unknown parameters, then the opened materials', each with its scope. A
  link's follower is left out.
- Added `StudyUnknown.owners`: the one item or material, one session per session holding a user, or one experiment
  per user.
- Added two `StudyTests` facts over CPL1, CPL2, a Phone and an opened absorbing crystal.

## Tests

These are development runs of each gate's own command from `0362.gates`, over the final tree:

- **build:**
  - exit 0, `0 Error(s)`, no lowercase `error` in stdout;
  - the final incremental build shows only NU1902 warnings, which are exempt. It took 16 s against a 600 s deadline.
  - The first full rebuild (76 s) also showed the vendored MathNet SYSLIB0051 pair, which predates this arc.
- **elevation:**
  - `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0;
  - suppressed 1518, all ELEV004, which is excluded. That is the same as step 0361: the new code reads no `.value`.
  - 220 s against a 600 s deadline;
  - with no `ASG_ARC_BASE_COMMIT` in this shell it analysed all 13 projects, wider than the gate's arc-origin scope.
- **constructor-unit-tests:**
  - `Test run summary: Passed!`, 1597 of 1597, 0 skipped. That is 1595 + 2, the rise the acceptance asks for.
  - The TRX shows both new `StudyTests` ids `Passed`.
  - 69 s against a 180 s deadline.
- **invariant-s36-\* (20):** every snapshot command answers `INVARIANT HOLDS`. Both changed files are tracked, so
  `git grep` sees them.

No run went past its effective deadline.

```yaml
gates:
  constructor_unit_tests: 1597
```

## Architecture

- **Two extensions after step 361's `Sample.openedMaterialsIn`.** `type Study with` holds `scopeOf` and
  `unknownsWith`, and `type StudyUnknown with` holds `owners`. Both types are declared just before.
  - `Item` (from `Kit`) is visible here.
  - Step 361 left `Study`'s intrinsic members unchanged.
- **Order is data.** `unknownsWith` keeps the order of `used` and the order of each item's parameters (its model's
  axes, then its dial). Then come the study's opened materials and each one's components (the crystal record's
  generated axes).
- **The gain default reads `ParameterName.gain`**, the name `IntensityDetectorModel` publishes (step 208). It is not a
  second literal.
- **Owners read the parameter's case under item or material scope.** An `ItemParameterRef` gives `ItemOwner` and a
  `MaterialParameterRef` gives `MaterialOwner`, so the match is total for every scope a user can set.
- **The caller supplies `used`, `sessions` and `users`.**
  - `used` is `Experiment.itemsUsedBy` (step 261).
  - `sessions` is `Study.sessionsIn`.
  - `users` comes from the forward model (S11.2).
  - The domain stays pure and does no lookup of its own.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **`ExperimentScope` owners take `users` once each** (`List.distinct`), in order. One value per owner means a
  repeated user must not open a second value. The fact asserts `[E3; E1; E3]` → E3, E1.
- **A follower is matched on its whole `StudyParameterRef`**: case, id and name. CPL2's `c2.retardance` following
  CPL1's leaves CPL1's own unknown in.
- **Record literals of `{ parameter; scope }` are annotated `ScopedParameter`** in the tests. `StudyUnknown`,
  declared later, shares both field names.
- **The vendored MathNet SYSLIB0051 pair appears only on a full rebuild.** It has been on the tree since step 0001.
  It is not ours, and an incremental build shows only NU1902.

## Changelog

- 2026-11-13: step 0362 added the study's unknowns:
  - `UnknownOwner` and `StudyUnknown`;
  - `Study.scopeOf` and `Study.unknownsWith`;
  - `StudyUnknown.owners`.

  `StudyTests` gained two facts (+2), and constructor-unit-tests went from 1595 to 1597.
