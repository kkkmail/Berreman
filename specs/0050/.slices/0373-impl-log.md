# Step 0373 — impl log

## Progress

- Read the task, both system prompts, the project prompt, the slice and its gates snapshot. No operator note or
  ruling is attached.
- Read the seams the step builds on: step 372's `StudyForward.fs` and its test fixture, `ItemEvaluation.ParameterValues`
  (`ofParameters`, `withValue`, `valueOf`), `Studies.fs` (`OpenedMaterial.currentCrystal`, `CrystalParameters.parameterAxes`,
  `ParameterAxis.parameterValueOf` / `writeValue`, `ParameterLink`, `Study.withLink`, `StudySession`, `UnknownOwner`),
  `MaterialLibrary.MaterialProxy` / `MaterialEntry` / `CrystalParameterPhysics`, `MaterialStore.fs` (a newly saved
  material is v1), `Propagation.resolveSampleMaterials` (a stack sample resolves each layer through `resolveVersion`)
  and STL's ELEV001 analyzer (read-only: it flags only `Ctor other.value` re-wraps, so `StudyVectorIndex k` is clean).
  Confirmed none of the new names exists.
- `StudyForward.fs`: `StudyForwardError.VectorLengthErr` (rendered `the parameter vector does not fit the study:
  <reason>`); after `parameterLayoutWith`, `ExperimentAssignment` (`valuesOf`, `crystalOf`, `materialProxyOver`) and
  `StudyParameterLayout.assignmentFor`. New opens: `CrystalPhysics`, `ItemEvaluation`. Domain compiled clean.
- `StudyForwardTests.fs`: `open ItemEvaluation` and `open Berreman.MaterialProperties` (for `RefractionIndex`); step
  372's `freeing` generalised to `stating` (any parameter state) so CPL2 can be known at 1.2; `forwardFor` now calls a
  new `forwardOver items` (same context, items passed); CPL2's draft joins `choiceOf`; `VectorLengthErr` in the
  error-render fact; four new facts appended with their fixtures (`qzLayout`, `qzVector`, `crystalAtVector`,
  `assignmentOf`, `openedOf`, `ownValuesOf`, a twin crystal version).
- First full build failed on the missing `Berreman.MaterialProperties` open in the test (FS0039 `RefractionIndex`);
  fixed. Then built, forced a Domain recompile inside a full build to see its warnings, ran the whole constructor suite,
  elevation from the arc base and the twenty s37 invariants (below).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/StudyForward.fs` (+125).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyForwardTests.fs` (+200 / -13).

No project file changed. Both files are LF (`git diff --numstat` equals `--ignore-cr-at-eol`; no CR byte in either).
`specs/0050/.manifest.state.json` was already modified at round start (supervisor state) and is untouched by this round.

## Testing state

All run locally after the final edit (Release x64):

| Gate | Result | Duration | Deadline |
|---|---|---|---|
| build | exit 0, `0 Error(s)`; the only warnings are 12 NU1902 advisory lines (exempt); a forced Domain recompile shows no FS warning | 24 s (17 s for the forced Domain recompile; 46 s for the first, failed, full build) | 600 s |
| constructor-unit-tests | `Test run summary: Passed!`, 1629 succeeded, 0 failed, 0 skipped (checkpoint 372: 1625; +4); the four new facts and the widened error-render fact are `Passed` in the TRX under exactly the authorized ids | 71 s | 180 s |
| elevation (arc base `f395597c`) | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, exit 0; Domain `findings=0` | 227 s | 600 s |
| twenty s37 invariants | each `section_invariants --find-nothing` exits 0 | a few seconds each | 300 s |

The four new facts reference symbols this step creates, so before the production code they could not compile (their
red); they were written after the Domain code and were not separately observed failing.

Every requirement of the slice landed this round; nothing is deferred.

## Artifacts

- `specs/0050/.artifacts/0373-build.log`
- `specs/0050/.artifacts/0373-constructor-unit-tests.log`
- `specs/0050/.artifacts/0373-elevation.log`
- `specs/0050/.artifacts/0373-invariants.log`

## Gotchas

- **A link whose leader or follower the assignment does not hold changes nothing.** The how-to sets a follower to
  "its leader's value in this assignment" and names `VectorLengthErr` as the only new error. A leader item missing from
  `items` (or a component no generated axis names) has no value in the assignment, so the follower keeps its own;
  `withLink` already refuses a link over a parameter the study's used items or opened copies lack. A caller should
  pass every item the study uses (e.g. `kit.itemsUsedBy study` or the kit's items), not only one experiment's, so a
  follower in the experiment always finds its leader.
- `itemValues` holds exactly the items passed; `valuesOf` falls back to the item's own values for any other item, and
  `crystalOf` to the copy's `currentCrystal` for a version the assignment lacks.
- Owners: `ItemOwner` / `MaterialOwner` hold every experiment, a `SessionOwner` only an experiment its session of
  `layout.sessions` lists (a Diagnostic run is in none, so it keeps the item's own gain), an `ExperimentOwner` only its
  own run. An experiment is in at most one session, so at most one entry per parameter applies.
- `materialProxyOver` replaces only `resolveVersion`, and only for an assigned version; the store's answer keeps every
  other field (id, name, category, description). `None` from the store stays `None`.
- `assignmentFor` writes a component through `writtenComponent`, a three-line local (find the generated axis by name,
  `writeValue`) also used for a material follower; `OpenedMaterial.currentCrystal` (Studies.fs, outside this step's
  edits) folds the same lookup over its own parameters.
- Test fixture: step 372's `freeing` became `stating name state draft` (two call sites updated) and `forwardFor` is
  now `forwardOver [ cpl1; qz; phone ]`, so the link fact can add CPL2 to the kit without a second context builder.
  Beyond the how-to's assertions, the facts also cover the per-experiment owner (E2's own retardance), D1 in no
  session, an item the assignment was not given, a material-to-material link (a twin version of the probe crystal),
  and `tryGetMaterial` passing through `materialProxyOver`.
- The working-copy fact resolves QZ's plate's own pinned substrate version through `resolveVersion`. It compares
  `MaterialEntry` values, not the resolved engine properties (`Propagation.ResolvedSample` holds dispersion
  functions, which have no value equality).
