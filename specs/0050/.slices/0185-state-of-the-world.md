# Step 0185 — state of the world

# Where we are

Step 0185 implements S5.3 read-only kit behaviour in the Optical Constructor:
a kit opened read-only refuses every write message and withholds the controls
that send them. The implementation and manual 014's authorized test repairs are
committed (`6607a49`, `b6668ae`). This attempt is the operator-requested normal
`--executability-profile 1` resume (attempt 01, relaunched). It changes no code
and hands the committed slice to normal supervisor review and the frozen gates,
which publish measured checkpoint 184 before 0186.

# What's working

- Add `TableAndElementRotationModel.kitWrites`: kit access projected once to `ControlOffered` / `ControlWithheld`.
- Guard `ExpCommit`, `ExpRemove`, `AttachDataFileTo` and `SceneSave` so a read-only kit answers
  `The kit is open read-only: nothing is saved.` before any proxy call and writes nothing.
- Disable and dim (never hide) Add experiment, Remove, Attach data file and Save scene when writes are withheld;
  add `ExperimentControlsState.removalAvailability`.
- Add two `KitWindowModelTests` facts and one `KitWindowTests` fact; keep the inverse and scene-save
  storage-refusal fixtures at read-write access (manual 014).
- Check visible-but-disabled Save and no status after a click in the launcher read-only acceptance fact (manual 014).

# Tests

This resume ran no gate (Invariant 6): the supervisor reruns all 19 frozen gates
after exit. The committed tree is the one the operator measured green through
the gate evaluator (manual 014, `.artifacts/0185-operator-repair/`):

| Test gate | Cases | Method identities | Failed | Skipped |
|---|---:|---:|---:|---:|
| constructor-unit-tests | 1197 | 1172 | 0 | 0 |
| ui-smoke | 227 | 227 | 0 | 0 |
| ui-tests (4 selected classes) | 47 | 47 | 0 | 0 |

Build: zero owned-code warnings (two existing SYSLIB0051 in vendored MathNet).
Elevation: 12 of 12 projects from immutable origin
`f395597c071910aaa215518626563f6c784bfb62`, zero findings (ELEV004 excluded).
All fourteen s21 invariants passed. Constructor cases rise by exactly the two
declared facts (1195 → 1197); smoke is unchanged; the selected UI classes
include the one declared new fact.

The YAML keeps checkpoint 183's inherited floors, as the operator directs, until
the supervisor measures checkpoint 184. `ui_tests` has no inherited floor.

```yaml
gates:
  constructor_unit_tests: 1195
  ui_smoke_tests: 227
```

# Architecture

- `kitWrites` is the single place the kit's access decides a control or a write
  message. `guardKitWrite` is one private function that the four write arms call;
  the step 152/153 bodies run unchanged inside it under `ControlOffered`.
- Two refusal layers, kept separately tested: the model guard refuses on access
  before storage; the storage proxy (`readOnlyView`) refuses inside storage. A
  storage-refusal fixture keeps read-write model access and swaps only the proxy.
- One button body draws the workbench buttons: `availableWorkbenchButton`;
  `workbenchButton` is it with `ControlOffered`. `rowActionButton` and
  `attachDataFileButton` take a `ControlAvailability` the same way `actionButton` does.

# Deferred

Nothing. Every requirement of the slice is in the committed tree; this resume
adds no work and leaves none for later.

# Gotchas

- A proxy-refusal test must keep `ReadWriteKitAccess`: with `ReadOnlyKitAccess`
  the model guard answers first, so the storage error text never appears.
- Manual 014 preserves the model-guard / storage-refusal / UI-control split at
  0187, 0188 and 0260 (the `SceneSaveLoadTests` → `SetupSaveLoadTests` rename).
- A disabled control ignores its click; the model guard still refuses a write
  message that arrives directly.
- `attachDataFileButton` keys its press handler on `(experiment uiKey, availability)`,
  so a change of access re-subscribes it.
- The operator note matches the code: committed product and test files are as
  manual 014 describes, LF only, and the working tree has no product changes.

# Changelog

- 2026-11-08, attempt 01: added the four-message guard, withheld controls and the
  three declared tests; escalated because the scene fixture was outside effects.
- 2026-11-08, operator recovery (manual 014): removed the one conflicting scene
  fixture line, adapted the launcher acceptance fact, passed all 19 gates; committed.
- 2026-11-08, normal profile-1 resume: verified the committed tree against the slice
  and manual 014, changed no code, restored the required SoW sections.
