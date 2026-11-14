# Step 0410 state of the world

## Where we are

Spec 0050 S11.5 now carries studies and fit runs from kit opening into the constructor window, reloads them with the kit, and handles the specified study commands through the existing domain and storage boundaries. Attempt 03 adds the missing Storage module import that exposes CatalogueImportContext.create to the new test fixture. The prior indentation repair remains in place. Supervisor verification of the repaired source remains pending.

## What's working

- Added ordered study and fit-run loading with failed-open cleanup and preserved overlay rebinding.
- Added transient study selection and New study form state, kit-backed context projections, and per-study fit-run selection.
- Routed study commands through the read-only guard, pure study update and recordStudy persistence, preserving state on refusal.
- Added the five authorized regression facts for opening, reloading, study creation, refusal and fit-run selection.
- Corrected six nested record-update layouts in the study handlers and their test fixture, preserving behavior and assertions.
- Brought the existing catalogue-import factory extension into scope in StudyWindowModelTests.

## Tests

The attempt-03 task supplies the preceding supervisor build failure: exit 1, one FS0039 at StudyWindowModelTests.fs:179 because CatalogueImportContext.create was not in scope, elapsed 38.86 seconds against the 600-second build deadline. That is historical supervisor evidence, not a worker-run check or a result for this repair. The fixture now opens OpticalConstructor.Storage.CatalogueImportStore, where the existing static extension is declared. The six reported warnings are exempt third-party NU1902 advisories. No deadline was exceeded in the supplied build result.

No build, tests, analyzer or invariant commands were run by the worker. The task's IMPLEMENT worker protocol, `implement_worker.system-md` Invariant 6, assigns executable checks and gate verdicts to the supervisor after worker exit. Source review is not a passing gate result or verified commit readiness. No repaired-source duration or deadline overrun is claimed.

The pending frozen roster is build (600 s), elevation (600 s), the complete constructor-unit-tests suite (180 s), the complete ui-smoke suite (600 s), ui-tests selecting OpticalConstructor.Ui.Tests.KitWindowTests (360 s), and all twenty invariant-s40 gates (300 s each).

Exactly five facts were added, with the prescribed identities:

- KitOpeningTests: opening a kit reads its studies and their fit runs, and a study file that does not read refuses the open.
- StudyWindowModelTests: a loaded kit's studies and fit runs are the window's, and Reload reads a study another writer wrote.
- StudyWindowModelTests: CreateStudy from the window records the study, selects it and clears the New study form.
- StudyWindowModelTests: a refused study message is told and records nothing, and a read-only kit takes none.
- StudyWindowModelTests: selecting a study shows its own fit runs, oldest first.

Within those facts, assertions cover read-write/read-only opening, study and fit-run parse failures releasing the lock, rebinding preservation, external study and fit-run changes, failed reloads retaining the offer and model, retained selections and fallback after removal, no-write form edits, stored study creation and replacement, scopes and links, persistence refusal, all three material-resolution outcomes, and run ordering and unknown-study selection.

Historical baselines are carried unchanged: checkpoint 408 records 1,739 constructor cases; checkpoint 392 records 258 smoke cases; the 0393 handoff records 16 cases for this same selected KitWindowTests class. Five additions imply an expected constructor total of 1,744, not a measured result. No census, checkpoint, gate contract, count floor or deadline was edited.

```yaml
gates:
  constructor_unit_tests: 1739
  ui_smoke_tests: 258
  ui_tests: 16
```

## Architecture

The internal KitProxy.readStudiesWithFitRuns extension in KitOpening.fs is the one shared read pipeline for opening and reloading. It calls readStudies and each study's readFitRuns, using ItemEvaluation.sequenced to preserve ordering and the first error. Existing openedOver cleanup closes a failed kit. OpenedKit.withRebindings retains the new fields through its existing record update.

The existing withKitContent function owns loaded studies and runs, selection retention and fallback for both loadKit and ReloadKitChanges. CopyChosenExperiments passes the window's held studies and runs. Existing KitFileKind change detection already covers both file kinds.

StudySurfaceState is window-only state. StudyContext reuses the window's kit content and materials.resolveVersion, mapping absent versions and material-store failures to MaterialUnavailableErr. StudyCommand applies StudyMsg only after guardKitWrite and records success through KitLibraryContext.recordStudy; it updates the window only after successful persistence. The other three messages write no files.

No durable format or schema changes, migration, new proxy fields or additional settings were introduced. The new fixture uses one BuiltinCatalogue.freshStores set for its stores and catalogue import context, shipped ItemModelVersions, the specified unknown parameters, and real kit files under the test output directory. Fit-run data reuses FitRunFileFixtures.

## Deferred

No slice implementation is deferred. Executable verification belongs to the supervisor under the worker protocol. The visual study surfaces are outside this slice's stated scope.

## Gotchas

- specs/0050/.manifest.state.json was modified before this work; the supervisor's edit was preserved.
- KitLibraryKitErr contributes its own description to a failed recordStudy status; the persistence-refusal test expects that wrapped error.
- Fit-run selection retention follows the specified kit-membership rule on reload. SelectStudy explicitly chooses the selected study's latest run, or none.
- Source diff numstats match with and without CR ignoring for the edited tracked files. Files written by this worker use LF.
- No versions, notebooks, analyzer settings, external repositories or Git history were changed. No runtime artifacts were produced.
- commit_ready is false because gate evidence is pending, not because requested implementation was omitted.
- F# strict indentation requires these multiline nested record fields to start below `model with`; keeping `with field =` inline left the nested record offside.
- CatalogueImportContext.create is declared as an extension in OpticalConstructor.Storage.CatalogueImportStore; opening the Domain module alone does not expose it.

## Changelog

- 2026-11-14: Implemented kit/window study and fit-run state, semantic study commands and the five authorized tests; left executable verification to the supervisor.
- 2026-11-14 (attempt 02): Repaired nested record-update indentation in the study message arms and two fixture updates; preserved the five test identities and all assertions, with supervisor verification pending.
- 2026-11-14 (attempt 03): Added the missing CatalogueImportStore import to the study-window fixture for the existing create extension; preserved implementation and assertions, with supervisor verification pending.
