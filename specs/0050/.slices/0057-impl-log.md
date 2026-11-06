# Step 0057 implementation log

## Operator implementation, 2026-11-06

Implemented before accepting 0056 under the operator ruling in
`specs/0050/.manual/005-step-0056-parity-retirement.md`.

- Deleted BuiltInCatalogueParityTests.fs and its five facts.
- Added BuiltInCatalogueIntegrityTests.fs with the exact step-57 fact identity.
  It reads the shipped catalogue through CatalogueProxy.createBuiltIn,
  checks manifest identity/version, each row's recorded and recomputed hash,
  id uniqueness across kinds, and all six specified kind counts.
- Replaced the project compile entry in place. No product code changed.

13:14:49 UTC: all ten unchanged step-57 gates passed through the evaluator.
Constructor tests: 801 passed, zero failed/skipped; exact census matched with
the five deletions and one creation declared at step 57. The parity file and
all references to it under the constructor test folder are absent.
The eleven step-56 gates also passed. Evidence and warning details are in
the ruling and `.artifacts/0056-operator-repair/`.

The published contracts, operator test effects and supervisor records remain
unchanged. No checkpoint was written and 0057 has not been accepted. After the
operator commits and 0056 is accepted, the normal 0057 cycle must verify this
implementation and record its measured constructor count before 0058.

## Normal worker cycle, 2026-11-06

### Progress

- Before implementation review: read the worker protocols, project prompt,
  slice, operator ruling and existing handoffs. The source replacement is
  already present. Review its coverage against S17-INTEGRITY and its read path
  before deciding whether any source edit is required.
- Created `0057-impl-plan.md`. This cycle must preserve the existing test
  effects and allow the normal supervisor cycle to measure the constructor
  count at checkpoint index 56.
- After implementation review: the committed replacement already fulfills
  S17-INTEGRITY. The exact single fact reads through `CatalogueProxy.createBuiltIn`,
  asserts the manifest id/name/version, finds exactly one envelope by each
  row's id and version, compares the recorded and recomputed hashes, requires
  distinct envelope ids, and checks every `LibraryEntryKind.all` count:
  12 materials, 5 categories, 11 samples, 28 sources, 3 polarizers, 2 detectors.
- The reader delegates to the existing schema/version/hash parser and
  `CatalogueData.validate`, and rejects missing or unlisted entry files.
  The test therefore exercises the shipped files without a seed fixture or
  a second parser. Its two-line module documentation cites S1.7 and the ruling.
- The test project already compiles the integrity file in the former parity
  slot. No source edit is necessary. The earlier implementation is committed
  as `1294d50`; the current worker dispatch follows the recorded 0056
  accept-current-slice override (`f059772`).
- Before the handoff update: preserve the historical verification and replace
  the stale waiting-for-0056 narrative with this normal-cycle state, including
  all seven required state-of-the-world headings.
- After the handoff update: wrote `0057-state-of-the-world.md`. No implementation
  requirement remains unaddressed; gate execution and the measured checkpoint
  remain the supervisor's next actions.

### Files modified

- `0057-impl-plan.md`: normal-cycle approach and risks.
- `0057-impl-log.md`: this normal-cycle record; historical evidence retained.
- `0057-state-of-the-world.md`: normal-cycle handoff and required structure.

No F# source, project file, test effect, gate descriptor, acceptance oracle,
version, or supervisor record was changed by this worker.

### Testing state

No gates or tests have been run by this worker. The IMPLEMENT protocol's
Invariant 6 assigns them to the supervisor after worker exit. The operator's
earlier 801-pass measurement above is historical evidence, not this cycle's
result. The current roster remains build, elevation, constructor-unit-tests
and the seven s07 invariants.

The existing `.checkpoints-json` retains `constructor_unit_tests: 805` at
index 52. The state-of-the-world YAML copies that inherited baseline; it does
not invent a new measurement or change a checkpoint. The five-to-one test
replacement has the declared net delta of -4. The supervisor must measure
801 constructor passes with the exact census and write checkpoint index 56
before step 0058. No current-cycle pass is claimed.

### Artifacts

No new run artifacts. Existing operator evidence remains under
`.artifacts/0056-operator-repair/`.

### Gotchas

The initial working tree already contains changes to `.manifest.overrides.jsonl`
and `.manifest.state.json`; these belong to the supervisor and are untouched.
Do not recreate the deleted parity class or replay its five deletions and the
integrity fact's creation in the published test effects.

The source work was completed early under the operator's explicit amendment,
so this worker cycle intentionally changes only its three handoff files.
Historical verification remains attributed to the operator; it is not a reason
to skip the normal step-0057 gate/checkpoint cycle. No scope deviation or
unfinished implementation remains.
