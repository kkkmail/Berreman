# Step 0411 state of the world

## Where we are

Spec 0050 S11.5 now shares the Items parameter editor with study parameter editing. The draft identifies an item parameter or material component through StudyParameterRef, and EditStudyParameter opens it from the window's held content. Attempt 02 repaired a stale draft-name accessor. Attempt 03 repairs the declared study editor fact's material/catalogue fixture after the supervisor's single constructor failure. Implementation is complete; post-repair supervisor verification is pending.

## What's working

- Keyed ParameterEditDraft by StudyParameterRef while preserving its state parsing, prior and calibration behavior.
- Added unguarded EditStudyParameter lookup over kit items and the selected study's opened material versions, preserving the model when a reference is absent.
- Extracted one public parameterEditorView with an injected Set message, preserving the Items panel's controls, styling and ownership rule.
- Updated the authorized Items assertion and added the specified study editor regression fact, reusing the window and crystal fixtures.
- Repaired the existing SetParameterState assertion to read the name through the draft's parameter reference, preserving its expected value and test identity.
- Connected the declared study editor fact's material save and catalogue snapshot lookup to one database, preserving every assertion and the real OpenMaterial command.

## Tests

No build, tests, analyzer or invariant commands were run by the worker. The task's implement_worker.system-md Invariant 6 assigns executable checks and gate verdicts to the supervisor after worker exit. Source review is not a passing gate result or verified commit readiness.

Attempt 02 repaired the supplied FS0039 build failure at ItemsPanelModelTests.fs(397,73) by reading `(openDraft editing).parameter.name`. Attempt 03's task reports that review and build gates subsequently passed. These are supplied supervisor findings, not worker-run results.

The current retry supplies a failed constructor-unit-tests run: exit code 2, 1,745 total, 1,744 succeeded, one failed, no skips, duration 159.917 seconds against the 180-second deadline. The failed fact is the declared study editor fact. Its existing TRX shows OpenMaterial failed at StudyWindowModelTests.fs:464 because catalogue snapshot lookup could not find the newly saved crystal's version. The fixture now uses one BuiltinCatalogue.freshLibraryDatabase for both MaterialProxy.createInDatabase and CatalogueImportProxy.createInDatabase, following StudyMessageTests. The supplied run did not exceed its deadline. No post-repair build or test result exists; readiness for supervisor verification is distinct from a passing gate verdict.

The frozen roster remains build (600 s), elevation (600 s), all constructor-unit-tests (180 s), all ui-smoke (600 s), ui-tests selecting OpticalConstructor.Ui.Tests.ItemsPanelTests (360 s), and the twenty invariant-s40 gates (300 s each). No gate contract, census, checkpoint, count floor or deadline was changed.

Exactly one fact was added: StudyWindowModelTests.the study's parameter editor opens on an item parameter or an opened material component, and a parameter it lacks opens nothing. Its assertions cover CPL1's unknown start 1.5 within [1, 2], the n1 of an absorbing crystal saved through the window's materials and opened through StudyCommand, no file writes or unrelated state changes from editor opening, read-only draft opening, missing item/material/name/version, selected-study isolation, and preservation of an existing draft when lookup fails. Attempt 03 changes only this fact's setup and the required extension-module import; every assertion and identity is retained. The existing ItemsPanelModelTests parameter-editor fact retains its assertions using the full reference, and ItemsPanelTests remains unchanged.

Historical baselines below are carried from checkpoint 409 (constructor 1,744, smoke 258) and the selected ItemsPanelTests result recorded by step 0263 (5 non-smoke cases). The one authorized addition implies 1,745 constructor cases; that is an expectation, not a measured result.

```yaml
gates:
  constructor_unit_tests: 1744
  ui_smoke_tests: 258
  ui_tests: 5
```

## Architecture

StudyParameterRef is the existing shared owner/name identity. ParameterEditDraft.ofParameter reads the supplied current Parameter and keeps every prior field and parsing rule. The study message resolves item parameters independently of item selection and resolves material components only from selectedStudyOpt, matching the full MaterialVersionId. Opening is a transient model update with no write guard or storage call.

parameterEditorView is the moved editor body, not a second implementation. Its setWith function chooses the caller's semantic Set message. Items calls it only when the draft holds an ItemParameterRef belonging to its selected item, retaining SetParameterState. The study surface caller belongs to step 0414's explicit scope. No durable format, schema, proxy or migration changed.

The retry reuses the existing database-backed material/catalogue proxies only inside the declared fact. The shared withWindow fixture remains unchanged. Direct writes to the separate in-memory material store do not populate CatalogueImportProxy's envelope map, whereas both database-backed proxies use the same entry-version rows. The disposable in-memory database remains alive for the whole test callback and is closed afterward.

## Deferred

No slice implementation is deferred. Executable verification belongs to the supervisor under the worker protocol.

## Gotchas

- specs/0050/.manifest.state.json was modified before this work and was preserved.
- specs/0050/.expected-red-observed-json was also modified before attempt 03 and was preserved.
- MaterialVersionId has no next member; the test's unopened-version reference updates its version with VersionNumber.next.
- The shared editor retains its existing view keys and automation ids, as required; callers retain responsibility for their enclosing owner context.
- Only the four authorized source files and this slice's worker documents were edited. Source diff numstats match with and without CR ignoring.
- Attempt 02 changes only the stale accessor in ItemsPanelModelTests.fs plus the three worker documents; the existing assertion and every test identity remain intact.
- Attempt 03 changes only StudyWindowModelTests.fs plus the three worker documents. Do not replace the real save/OpenMaterial assertions with injected opened-material state; the shared database supplies the actual snapshot path required by the slice.
- No version bump, external-repository modification, notebook edit, analyzer suppression or Git mutation was performed. No runtime artifacts were produced.
- commit_ready is false solely because supervisor gate evidence is pending.

## Changelog

- 2026-11-14: Shared the parameter editor through StudyParameterRef and parameterEditorView, added study parameter opening and the authorized regression coverage; handed executable verification to the supervisor.
- 2026-11-14 (attempt 02): Repaired the supervisor-reported FS0039 by reading the existing SetParameterState assertion's name through StudyParameterRef; preserved acceptance and handed post-repair gate execution to the supervisor.
- 2026-11-14 (attempt 03): Repaired the study editor fact's fixture with shared database-backed material and catalogue proxies after the supervisor's missing-snapshot failure; preserved all assertions and handed verification to the supervisor.
