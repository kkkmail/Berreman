# Step 0411 implementation log

## Progress

- Read the task, both worker protocols, the project prompt, AGENTS.md, naming conventions, slice, immutable S115-PARAMETER-EDITOR oracle, gate selection and standing manuals 013/015.
- Inspected the existing draft, Items editor, study-window fixture, StudyParameterRef and OpenedMaterial lookup surfaces. No extra domain abstraction or storage change is needed.
- Before test edits: retain the existing Items test's assertions with the new reference, and add exactly the authorized study-window fact using existing fixtures and real material save/OpenMaterial paths.
- Updated the authorized Items fact to assert the full StudyParameterRef and reopen drafts through it, preserving its parsing, prior, calibration and cancellation assertions.
- Added the one authorized study-window fact covering unknown item state, saved/opened crystal n1, read-only opening, missing owners/names/versions, absent or different selected studies, retained drafts on failed lookup and unchanged model/files outside the editor field. Reused CrystalMaterialFixtures and the existing withWindow setup.
- Before production edits: replace the draft key, add the unguarded lookup message, and extract the existing view without changing its visual contract.
- ParameterEditDraft now carries StudyParameterRef; ofParameter and tryState preserve the original state rules and name diagnostics. EditItemParameter builds the selected item's reference. EditStudyParameter resolves kitItems or selectedStudyOpt.materials by exact version and name, and changes only parameterEditOpt on success.
- Moved the existing editor body into public parameterEditorView, taking setWith and returning IView. Items renders it only for an ItemParameterRef owned by the selected item. Kept all automation ids, labels, colours, field rules, keys and action layout, and deleted the old private list-returning editor.
- Reviewed the four-file source diff and all ParameterEditDraft.ofParameter call sites. Corrected the new test's next-version fixture to use MaterialVersionId's record with VersionNumber.next; MaterialVersionId itself has no next member. Diff numstats are identical with and without CR ignoring.
- Completed the slice handoff at the exact required state-of-the-world path. Carried historical baselines from checkpoint 409 and the step 0263 selected ItemsPanelTests result, without changing supervisor metadata or claiming a measured result.

### Attempt 02 recovery

- Read the retry task and supervisor diagnostic: build failed with FS0039 at ItemsPanelModelTests.fs(397,73), where the existing SetParameterState fact still reads ParameterEditDraft.name.
- Reviewed the retained implementation against S115-PARAMETER-EDITOR. The shared draft, unguarded item/material lookup, moved editor and declared study fact are already present.
- Before repair: change only the obsolete assertion accessor to parameter.name in the authorized ItemsPanelModelTests.fs. This preserves its existing expected ParameterName, behavior, identity and all other assertions; no new test effect or compatibility member is needed.
- Repaired the assertion to read `(openDraft editing).parameter.name`. The retry changes no production behavior or test identity; it adapts the existing assertion to the already-authorized draft shape.
- Reviewed draft declarations and consumers across OpticalConstructor, including the shared editor and draft construction calls. The repaired assertion now uses the same StudyParameterRef.name accessor as the production editor. ItemsPanelModelTests.fs diff numstats remain identical with and without CR ignoring (16 additions, 6 deletions including attempt 01 changes).
- Updated the required state-of-the-world handoff with the supplied failure and pending supervisor verification. This retry edited only ItemsPanelModelTests.fs and the three worker documents; retained source changes and supervisor metadata were preserved.

### Attempt 03 recovery

- Read the retry task, worker protocols, slice and immutable S115-PARAMETER-EDITOR obligation. The task reports green review/build and one failing constructor case, with 1,744 successes of 1,745 and no skips in 159.917 seconds against the 180-second deadline.
- Inspected the existing constructor-unit-tests.trx: StudyWindowModelTests.fs:464 expected no kitStatus, but OpenMaterial returned "Not saved to the kit — neither the library nor the kit holds the reference: 41100000-0000-4000-8000-000000000001 v1".
- Traced the failure through StudyCommand's recordStudy and KitLibrary's snapshot lookup. CatalogueImportProxy.createInMemory owns an envelope map separate from its MaterialProxy; directly saving a user material updates the latter only. The parameter editor has not yet been exercised when the assertion fails.
- Before repair: reuse StudyMessageTests' shared-database arrangement inside the one authorized study editor fact. Point the window's materials and catalogueImports at the same BuiltinCatalogue.freshLibraryDatabase, preserving the real save/OpenMaterial path, every assertion and the other facts' fixture. No production change or new test identity is needed.
- Repaired StudyWindowModelTests.fs by opening MaterialDatabaseStore and supplying both proxies from one disposable, catalogue-populated database inside the declared fact. The original saveMaterial, StudyCommand OpenMaterial, file/state comparisons, read-only, missing-reference and draft assertions are unchanged. No executable checks were run.
- Reviewed the final source diff and database-proxy call path. StudyWindowModelTests.fs has identical numstats with and without CR ignoring (123 additions, no deletions, including the retained earlier-attempt fact). Updated the three required worker documents with the diagnosis, measured supervisor failure and pending verification. Existing production changes, ItemsPanelModelTests changes and supervisor metadata were preserved.

## Files modified

- 0411-impl-plan.md and this implementation log: worker handoff documents.
- ItemsPanelModelTests.fs and StudyWindowModelTests.fs: the authorized changed fact and new fact.
- TableAndElementRotationView.fs: shared draft key and semantic study editor opening.
- ItemsPanelView.fs: one shared editor view and the Items owner match.
- 0411-state-of-the-world.md: completed mandatory handoff, architecture, test selection and historical baselines.

## Testing state

No executable checks run. The IMPLEMENT worker's Invariant 6 assigns builds, tests, analyzers and invariants to the supervisor after exit. Source inspection is not gate evidence. Frozen roster: build, elevation, complete constructor-unit-tests, complete ui-smoke, ui-tests selecting ItemsPanelTests, and twenty s40 invariants.

Attempt 02 received a supervisor build failure: exit code 1, FS0039 at ItemsPanelModelTests.fs(397,73), elapsed 81.35 seconds against the 600-second deadline. The obsolete member access has been repaired in source. No post-repair build or gate verdict exists in this worker session; the remaining gates await the supervisor's normal execution.

Attempt 03's task reports green review/build followed by constructor-unit-tests exit code 2: 1,744 successes, one failure, no skips, 1,745 total, duration 159.917 seconds against the 180-second deadline. The supplied run was within its allowance. The existing TRX identifies the absent material snapshot at OpenMaterial, before the material editor assertions. This retry repairs the fixture's shared persistence context; no post-repair gate verdict is claimed. The historical YAML baselines and all supervisor metadata remain unchanged.

The existing Items fact's identity and coverage are retained, and exactly the declared StudyWindowModelTests fact was added. Constructor count is expected to rise from checkpoint 409's 1,744 to 1,745; smoke retains its historical 258 and the selected ItemsPanelTests its 5 non-smoke cases. These are inherited evidence/expectations, not worker-run outcomes. No duration or deadline overrun is claimed. Implementation is complete; commit_ready remains false pending supervisor verification.

## Artifacts

No runtime artifacts produced. Any persistent run artifacts belong only under specs/0050/.artifacts/.

## Gotchas

- specs/0050/.manifest.state.json was already modified on entry and is preserved.
- specs/0050/.expected-red-observed-json was also modified on attempt 03 entry and is preserved.
- The in-memory catalogue's envelope map does not observe a direct MaterialProxy save; the declared fact now follows StudyMessageTests by sharing one database between the two proxies.
- No user prompt is needed; this is the authorized 0411 implementation only.
- The worker protocol overrides the legacy local-gate procedure; do not report unchecked code as a passing gate or verified commit readiness.
