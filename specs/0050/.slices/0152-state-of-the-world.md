# Where we are

Spec 0050 step 0152 implements S43-CONSTRUCTOR-KIT: constructor experiments now persist individually through the working kit. The named collection persistence seam is retired. Manual/010's fallible scratch-construction contract is applied through the App, diagnostic windows and test callers. Attempt 02 adds explicit generic types to the three xUnit assertion calls identified by the supervisor's failed build. The implementation awaits the arc-runner's unchanged gate sequence; this worker has not executed or certified gates.

# What's working

- Store committed experiments, edits and attachments in the constructor's kit, and delete their files on removal.
- Preserve live experiments and attachment statuses on refused writes, with a visible KitStatus explanation.
- Load experiments from OpenedKit and revalidate unchanged, changed, missing and unreadable attachments.
- Remove named collection persistence, messages, controls and app-scope experiment stores; retain the solver hand-off snapshot.
- Propagate scratch-kit initialization errors through window factories and show them before opening a constructor.
- Replace retired coverage with authorized folder-kit facts, share successful scene construction and disambiguate the three reported xUnit calls without changing assertions.

# Tests

This worker performed no build, test, analyzer, census or invariant execution, per IMPLEMENT invariant 6. The attempt-02 task supplies the supervisor's previous build failure: three FS0041 errors in InverseConstructorTests.fs at lines 377, 390 and 444. Explicit string, experiment-list and status-map type arguments now disambiguate those assertion calls. No subsequent build result is available. The supplied build also reports two vendored MathNet SYSLIB0051 warnings; this retry does not claim to clear them.

The supervisor must run build, elevation, constructor-unit-tests, ui-smoke, selected ui-tests and all eleven s17 invariants from 0152.gates. No result below is a new measurement.

Checkpoint 150 supplies 1,155 constructor cases and 215 smoke cases. Manual/006 corrects the retirement of five constructor methods to six executed cases, making the expected constructor count 1,149, subject to supervisor measurement. The UI effects remain a net decrease of one; selected UI results are not a whole-suite baseline. The inherited counts are:

```yaml
gates:
  constructor_unit_tests: 1155
  ui_smoke_tests: 215
```

The two new facts cover experiment-file creation, attachment replacement, committed edits, deletion, read-only failures preserving state and the already-missing deletion case. The three renamed facts preserve attachment/reopen and validation assertions. The five obsolete collection-proxy methods and the three specifically authorized UI methods are removed; every other test identity and assertion is retained.

# Architecture

The model owns KitContext and reads measured data through its proxy. recordExperiment snapshots library references and writes the existing canonical experiment file before the model accepts the new collection. ExpRemove uses the existing kit deletion seam, accepting MissingKitFileErr as an already-completed deletion. loadKit reuses the existing validation function with the opened kit's measured-data proxy.

initWith receives an initialized context. Scratch entry points return Result; private window constructors accept a successful model, and public tryCreate factories propagate KitError. App and diagnostic launchers report initialization failures before displaying a constructor. A single SceneTestFixture source, file-linked into both test projects, unwraps success only in tests. TestAppScope supplies fresh output-local folder kits.

No durable schema or serializer changes, conversion, fallback or migration were introduced. CollectionName and ExperimentCollectionSnapshot remain solely for the solver hand-off until its scheduled retirement. Manual/010 authorizes the necessary caller edits outside the original slice's narrow file list.

# Deferred

No implementation requirement from 0152 is deferred. Gate execution is the supervisor's next action. Empty gate results and commit_ready false indicate pending verification, not an unresolved implementation question.

# Gotchas

- The read-only recordExperiment diagnostic includes KitLibraryKitErr's description around KitReadOnlyErr; assertions retain the underlying reason.
- Folder-kit reopen fixtures close the prior context before reacquiring the lock. Attach validation mocks are applied to OpenedKit.experimentData so loadKit inspects the requested seam.
- The existing AppContextTests settings assertion replaces the retired root-store composition fact; no replacement identity was invented.
- The shared scratch initializers intentionally changed signature under manual/010. App/TestWindows callers report errors; test callers use the shared success fixture.
- No gate roster, baseline/checkpoint, census/oracle, analyzer exemption, version ring or ASG/STL file was modified. The pre-existing supervisor-owned .manifest.state.json change was left untouched.
- Source and worker output edits use LF. No Git mutation was performed.
- xUnit assertions over strings, F# lists and maps can match both scalar and sequence overloads; these three calls now specify the intended type explicitly.

# Changelog

- 2026-11-08: Moved constructor experiments into kits, retired collection persistence, propagated scratch initialization errors and prepared authorized persistence coverage and supervisor handoff.
- 2026-11-08 (attempt 02): Repaired the three supervisor-reported FS0041 assertion calls with explicit generic types; preserved acceptance and inherited counts, with gates pending supervisor execution.
