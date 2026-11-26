# Where we are

Spec 0050 step 0151 retires the in-memory scene seam after the kit-file scene
implementation and kit opening from earlier S4.3 steps. S43-NO-SCENE-SEAM is
implemented within its authorized effects. The arc-runner must now execute the
unchanged gate snapshot; this IMPLEMENT worker has not run or certified gates.

# What's working

- Removed SceneProxy, its in-memory factory and AppContext.scenes composition.
- Preserved scene data types, schema version, file stem and library references.
- Moved four SceneName/SnapMode methods unchanged into SceneRoundTripTests,
  retaining both blank-name theory rows; deleted the two proxy-only facts.
- Removed the scene assertion from the existing UI composition fact while
  retaining its identity, other assertions and smoke coverage.
- Updated the specified scene documentation and existing project comments.

# Tests

No build, test, analyzer, invariant or census execution was performed, as
required by IMPLEMENT Invariant 6. Pending supervisor gates: build, elevation,
constructor-unit-tests, ui-smoke, selected ui-tests and all eleven s17
invariants in 0151.gates. No pass or measured post-change count is claimed.

Manual 006 preserves the blank-name theory's two rows. The four migrated
methods represent five executed cases; only the two proxy-specific facts are
retired. Checkpoint 149 records 1157 constructor cases and 215 smoke cases.
The expected constructor result is therefore 1155, subject to supervisor
measurement and census verification. Selected UI results are not a whole-suite
baseline. These are inherited counts, not newly authored floors:

```yaml
gates:
  constructor_unit_tests: 1157
  ui_smoke_tests: 215
```

# Architecture

Scene.fs retains pure scene data and the existing named, versioned .setup-json
envelope. KitProxy remains the existing persistence owner; no replacement
store, fallback, conversion or migration is introduced. AppContext no longer
holds a scene store. OpenedKit.scenes is an independent list of scene names
and remains unchanged. Existing kit-file round-trip tests retain all assertions.

# Deferred

No implementation requirement from 0151 is deferred. Verification belongs to
the supervisor after worker exit. Empty gate results and commit_ready false
mean pending supervisor verification, not an implementation or authority
blocker. No baseline or checkpoint has been published by this worker.

# Gotchas

- The UI composition fact deliberately retains its historical scene-seam name:
  the authorized effect changes its expectation, not its identity.
- SceneRoundTripTests already opens OpticalConstructor.Domain.Scene, so the
  migrated SnapMode cases need no additional open.
- ExperimentDataProxy.fs already contains no SceneProxy or SceneProxyTests
  reference and required no edit.
- Historical comments in ExperimentCollectionStore.fs and
  TableAndElementRotationView.fs fall outside this slice's edit authority.
  The next step's broader retirement owns that scope; it is not unfinished
  0151 work.
- Product diffs have matching numstats with and without ignoring CR at line
  ends. The existing supervisor-owned .manifest.state.json change was untouched.
- No version bump, Git mutation, ASG/STL change, gate/census/oracle edit or
  analyzer exemption was made.

# Changelog

- 2026-11-08: Retired the scene proxy and composition, preserved scene data,
  migrated validation/wire-form coverage, and prepared the supervisor handoff.
