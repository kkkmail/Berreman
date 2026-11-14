# Implementation log — 0410

## Progress

### Attempt 03 — repair complete, supervisor verification pending

- Read the task, worker and project prompts, slice and gate snapshot, applicable operator rulings, naming conventions and preceding handoff.
- The supplied supervisor build failed with one FS0039 at StudyWindowModelTests.fs:179 after 38.86 seconds, below its 600-second deadline. The diagnostic says CatalogueImportContext has no create member; the other reported warnings are exempt third-party NU1902 advisories.
- Located the existing create extension in OpticalConstructor.Storage.CatalogueImportStore. StudyWindowModelTests opens the Domain module but omits the Storage extension module; the corresponding KitWindowModelTests fixture opens both. Add only the missing module import, preserving one shared BuiltinCatalogue store set and all five authorized facts and assertions.
- Added open OpticalConstructor.Storage.CatalogueImportStore to StudyWindowModelTests.fs. The fixture now brings the existing static extension into scope; no factory, proxy, test identity, assertion or product behavior changed in this attempt.
- Reviewed the existing kit-opening and window-state implementation against the slice. The ordered reader, reload preservation, read-only guard, study application and persistence remain intact. No executable checks were run; supervisor verification is pending.

### Attempt 02 — repair complete, supervisor verification pending

- Read the retry task and worker protocol. The supervisor reported build exit 1 after 56.78 seconds, with 15 F# syntax/indentation errors in TableAndElementRotationView.fs; this is supplied evidence, not a worker-run check.
- Located nested record updates whose field expression starts after `model with` on the same line while its record body is indented too little under F# strict indentation. The new test fixture contains the same layout in two places.
- Moved the four nested studySurface bindings in SelectStudy, SetNewStudyName and ToggleNewStudyExperiment onto their own indented lines, and repaired the matching materials/proxy fixture updates in StudyWindowModelTests.fs. No behavior or assertion changed.
- Reviewed the authorized source diff, compile ordering, shared study/run reader, storage ordering, StudyMsg application and material-error mapping. The five authorized test identities and all assertions remain intact; no further implementation changes were needed.
- Updated the plan and state-of-the-world with this repair and the supplied failure evidence. No executable build, test, analyzer or invariant check was run by the worker; all repaired-source gate results remain pending.

### Attempt 01 — implementation

- Read AGENTS.md, worker and project prompts, slice effects and gate snapshot, naming conventions, and applicable operator guidance.
- Located the kit-opening failure cleanup, shared window content loader, read-only guard, study messages and existing test fixture precedents.
- Added exactly the five authorized facts, including malformed study/fit-run opens and reloads, selection retention and fallback, no-write form edits, study creation and edits, domain/persistence/read-only refusal, and per-study run ordering.
- The new fixture uses shipped ItemModelVersions and one fresh catalogue store set; all files stay under the test output directory. Shared FitRunFileFixtures supplies serialized fit runs.
- Extended OpenedKit and its existing failure-cleanup path. Added the internal KitProxy.readStudiesWithFitRuns member in KitOpening.fs so both opening and reloading call the same ordered reader using ItemEvaluation.sequenced.
- Extended shared withKitContent, loadKit, ReloadKitChanges and CopyChosenExperiments. Added StudySurfaceState, model projections and the four specified messages; writes use guardKitWrite, StudyMsg.applyTo and KitLibraryContext.recordStudy.
- Reviewed the source against the existing error types and proxy signatures. Corrected the persistence-failure assertion to include KitLibraryKitErr's wrapper description. Extended the authorized facts with newly written fit-run reload coverage and all three material resolution outcomes.
- Source diffs show matching ordinary and ignore-CR numstats for the edited tracked files. No executable build, test, analyzer or invariant checks have run.
- Implementation and the required state-of-the-world handoff are complete. No gate result or verified readiness is claimed.

## Files modified

- 0410-impl-plan.md and this log: implementation approach and durable progress.
- KitOpeningTests.fs: the authorized opening fact.
- StudyWindowModelTests.fs and OpticalConstructor.Tests.fsproj: four authorized window facts and final compile entry.
- KitOpening.fs: ordered study/run reads, carried content and failure cleanup.
- TableAndElementRotationView.fs: transient study state, content loading, context projections and semantic commands.
- 0410-state-of-the-world.md: complete implementation and pending-verification handoff, with historical baselines attributed to checkpoint 408, checkpoint 392 and the 0393 handoff.

## Testing state

The supplied attempt-03 task reports the supervisor's preceding build failed (exit 1, one FS0039 at StudyWindowModelTests.fs:179, 38.86 seconds against a 600-second deadline). The missing Storage module import now brings CatalogueImportContext.create into scope. The attempt-02 syntax repair remains in place. No passing build is claimed.

No executable checks were run by the worker. The IMPLEMENT worker protocol's Invariant 6 assigns gate execution to the supervisor. The authorized test delta remains five facts, with no retirement or rename. Checkpoint 408 records 1,739 constructor cases; 1,744 is the expected count after this slice, not a measured result. Build, elevation, the full constructor and smoke suites, selected KitWindowTests and all frozen invariant gates await supervisor execution over the repaired source.

## Artifacts

No runtime artifacts generated.

## Gotchas

- specs/0050/.manifest.state.json was already modified at entry; preserve the supervisor's edit.
- Gate counts and results must come from supervisor measurement, never an inferred pass.
- The only extra implementation helper is an internal KitProxy extension shared by open and reload; it adds no proxy field, message, setting or serialized shape. This avoids duplicating the study/run read pipeline within the two authorized UI files.
- No scope or migration deviation: study surfaces themselves remain outside this slice as specified; every requested window-state and message behavior is implemented.
- CatalogueImportContext.create is a Storage module extension, not an intrinsic member of the Domain type. Callers must open OpticalConstructor.Storage.CatalogueImportStore.
