# Where we are

Step 0429 implements S12.1's AT-1 committed-kit read path after 0428's exact-file
write-path comparison. The empty-library fixture and the one authorized
acceptance fact are implemented. Supervisor verification remains pending.

# What's working

- Added an empty-library AppContext with a fresh empty shipped-catalogue folder.
- Added read-only AT-1 coverage through the start-screen message and real kit proxy.
- Asserted nine snapshot-resolved items, complete kit content and all attachment groups.
- Reused the reference instrument and object assertions for the direct staged refit.
- Asserted unchanged files, bytes and directories after opening, fitting and closing.

# Tests

No build, test, builder invocation or gate was run. IMPLEMENT Invariant 6 assigns
gate execution and verdicts to the supervisor after worker exit. The structured
exit therefore reports no gate results and commit_ready=false; this records
pending verification, not an unresolved implementation or authority issue.

The supervisor must run the unchanged 0429 roster: build; elevation from the arc
origin; all ui-smoke cases; ui-tests selecting StudyViewTests and DataStepTests;
kit-acceptance selecting KitAcceptanceTests; and all twenty-one s41 invariants.
Exactly one declared fact was added, with no existing fact edited or removed.
The expected kit-acceptance increase is one case, from four to five; this is an
expectation for supervisor measurement, not a measured result. No development
run occurred, so there is no runtime or deadline-exceedance result to report.

The following baseline block is inherited unchanged from the 0428 handoff. The
supervisor's checkpoints and case evidence remain authoritative; no count floor
or checkpoint was authored or changed here.

```yaml
gates:
  constructor_unit_tests: 1761
  ui_smoke_tests: 264
  ui_tests: 9
  kit_acceptance_tests: 0
```

# Architecture

`TestAppScope.emptyLibraryAppContext` reuses `testLocations owner`, including its
SQLite :memory: connection, and overrides only the shipped-catalogue location.
AppContext's normal startup attempts the empty catalogue and logs its refusal.
No import is settled and no library data is seeded.

The new fact sends OpenKitReadOnly through KitStartView and composes
`inverseModelOver`. The existing KitOpening/KitContext path applies the real
folder proxy's readOnlyView. It asserts 53 experiments, 42 setups, nine items,
QZ-LR and its two stored fit runs; the window receives those same values.
Database stores stay empty while every item resolves its stored model version
and binding through the kit snapshot overlay. Each attachment reports its
recorded hash unchanged and maps through Phone ROI CSV to its own lab group.

The direct StudyFitParam uses the window's studyForwardContext and kitFitRuns,
the same MathNet SVD and seed-1 deviates as StudyFitEnvironment.create, and a
clock fixed at 2026-05-31T12:03:00Z. ObjectStage holds calibration fixed and runs
under UnwatchedIterations. It must return LinearStudyEngine and satisfy the
existing reference matrices and product-check ratio assertion at dataMatrixTol.
It invokes no fit-run persistence path.

The initial snapshot includes every relative file and its bytes, without an .oc
exclusion, plus every directory. Comparisons after open/model composition, after
fit and in the close finalizer detect lock/cache files and empty cache folders
as well as changed metadata, snapshots or lab bytes. The finalizer runs even if
an earlier assertion fails.

# Deferred

No implementation requirement is deferred. Supervisor review, gate execution
and measured checkpoint publication are the remaining lifecycle work.

# Gotchas

- Preserve R0001's independent-series dispatch, R0002's analyzer traversal and
  R0003's element-identity provider; this slice changes none of their code.
- The empty catalogue's startup refusal is intentional fixture behavior.
- The committed kit is opened in place. The test creates no output under data/.
- Only the two authorized UI test files and canonical 0429 handoff files changed.
  The existing facts, product code, durable formats, reference kit, version
  files, gates, census and external repositories were not edited.
- `.manifest.state.json` was already dirty on entry and remains supervisor-owned.
- Git diff inventories with and without CR-at-EOL ignoring agree for both source
  files; the edits contain no line-ending churn. No Git mutation was performed.

# Changelog

- 2026-11-15: Implemented the empty-library read-only AT-1 reconstruction fact and saved the supervisor handoff.
