# Step 0413 state of the world

## Where we are

Spec 0050 S11.5 now exposes PublishCalibration and PromoteToLibrary as constructor-window messages over the existing publishing domain members. The two command arms and all four authorized facts are implemented. Focused development validation passed; the frozen acceptance gates remain for the supervisor.

## What's working

- Added guarded PublishCalibration and PromoteToLibrary messages resolving the named run and its own study.
- Published linked item calibrations through recordItem, retaining successful revisions when a later write fails.
- Promoted fitted materials and black boxes through existing library operations, with fit provenance and kit snapshots.
- Reported missing dependencies, domain refusals and storage errors through kitStatus while preserving unrelated model state.
- Added the four authorized facts covering revision history, retry/refusal, promotion and read-only protection.

## Tests

Focused development command: `dotnet test -c Release --filter-class OpticalConstructor.Tests.StudyWindowModelTests --no-ansi`, with TRX and captured output under the arc artifacts directory. This rebuilt the test project and its references in Release/x64 and passed all **12 cases**, with **0 failures and 0 skips**. Final total wall time was **24.218 s**, with MTP test duration **6.688 s**. The final development build emitted no warnings.

An initial development build stopped after 76.802 s on a fixture's nonexistent MaterialError.describe and reserved local identifier; both were corrected in source and recompiled. The first successful run took 36.049 s and passed the same 12 cases. Its final rerun verifies the subsequent product cleanup. The initial build also reported two existing SYSLIB0051 warnings in vendored MathNet Exceptions.cs; no vendored source was changed. Evidence: `.artifacts/0413-development-tests.log`, `0413-development-tests-02.log`, `0413-development-tests-03.log` and `0413-study-window-tests/study-window.trx`.

These are focused development results, not frozen gate verdicts. Pending supervisor gates are build (600 s), elevation (600 s), complete constructor-unit-tests (180 s), complete ui-smoke (600 s), ui-tests selecting OpticalConstructor.Ui.Tests.KitWindowTests (360 s), and the twenty invariant-s40 gates (300 s each). No gate was executed or green-lit here, and no gate duration is claimed. Development runs stayed within the corresponding build and constructor-test allowances. The gate roster, selections, acceptance oracle, test census, count floors and deadlines were preserved.

Exactly four facts were added in StudyWindowModelTests. Coverage includes CPL1/CPL2 revision 2, exact calibrated value/sigma/run, old value history, unchanged other fields/parameters, persisted item equality, ordered partial writes and retry, repeated publication, absent sigma/run/study, named-study selection, material v2 provenance/snapshot/fitted n1, black-box v1 provenance/snapshot/fitted m00, exact stored item-model version resolution, missing/error resolution, promotion refusal, and a real read-only opening that reaches neither resolver and changes no files or held material entry.

Historical floors are copied from supervisor checkpoint 411 (1,748 constructor and 258 smoke cases) and the prior handoff for the same selected KitWindowTests class (16 cases). Four additions imply 1,752 constructor cases, which is an expectation, not a measured full-suite count. No checkpoint or baseline file was modified.

```yaml
gates:
  constructor_unit_tests: 1748
  ui_smoke_tests: 258
  ui_tests: 16
```

## Architecture

The private withPublishingRun helper applies guardKitWrite before looking up a run and the study it names. Publishing calls Study.calibratedItemsWith, then reuses writtenInOrder, KitLibraryContext.recordItem and kitItemsWith. A failed write returns the model containing earlier successes. No independent loop, transaction, rollback or undo mechanism was introduced.

Material promotion resolves the requested MaterialVersionId from the window's material store and calls KitLibraryContext.promoteMaterial. Black-box promotion resolves the held item through tryItemModelChoice, including its stored ItemModelVersion through ItemModelProxy, then calls promoteBlackBox. Only kitStatus changes in the returned window model; domain members own library versions, provenance and snapshots.

Tests reuse the existing real-folder withWindow fixture and shipped model entries. The material fixture uses one database for materials and catalogue imports, writes the fit run before reload, and opens the prepared folder read-only for refusal coverage. No serialized shape, stored format, migration, additional option or public message beyond the two requested was added.

## Deferred

No slice implementation is deferred. Supervisor acceptance verification is pending. Wiring the Publish step's buttons remains step 0418's explicitly separate scope.

## Gotchas

- specs/0050/.manifest.state.json was already modified on entry and was preserved.
- Only the two authorized product/test files and the three required worker documents were edited; runtime evidence is under the supplied artifacts folder.
- Promotion stores must share a database. Independent in-memory stores cannot establish that a material saved through the window is available to catalogue promotion.
- Publishing does not require the run's study to be selected, and a second item-write failure must not discard the first recorded revision from the model.
- Product/test numstats match with and without CR ignoring; diff whitespace checks are clean. Worker-authored text uses LF.
- No version bump, notebook edit, external repository mutation, analyzer suppression, gate/census/checkpoint edit or git mutation occurred.
- commit_ready is false because the supervisor's executable acceptance evidence is pending, not because requested implementation is missing.

## Changelog

- 2026-11-14: Added guarded calibration publication and material/black-box promotion, four authorized regression facts and focused Release/x64 evidence with 12 passing cases; prepared the supervisor handoff.
