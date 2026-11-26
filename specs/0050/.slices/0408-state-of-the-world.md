# Step 0408 state of the world

## Where we are

Spec 0050 S11.5 now persists step 0407's pure material and black-box promotion
values through the existing library import seam, then pins and snapshots the
promoted version into the kit. The two authorized source files and three worker
documents are complete. Supervisor verification remains pending.
Attempt 02 repairs the test compilation error and reserved-identifier warnings
reported by the supervisor; the promotion implementation is unchanged.

## What's working

- Added material promotion that mints the library entry's next version under
  fit provenance and snapshots it without altering the study's opened version.
- Added black-box promotion under the item's GUID, starting at version one and
  minting another version for subsequent promotions.
- Added read-only refusal before IO and typed import, entry and snapshot errors.
- Added the three authorized persistence facts and extended error rendering,
  covering retained versions, provenance, payloads, pins and snapshot bytes.
- Repaired the material-save fixture's invalid error-renderer call and renamed
  the reserved local identifier without changing test identities or coverage.

## Tests

No build, test, analyzer, invariant or other gate was run by this worker.
`implement_worker.system-md` Invariant 6 assigns gate execution and verdicts to
the supervisor after worker exit. No passing result, measured duration, deadline
compliance or verified readiness is claimed.

The supervisor's prior build failed (exit code 1, elapsed 78.79 s within the
600 s deadline), as supplied in `.artifacts/0408-attempt-02-task.txt`. Attempt 02
replaces the nonexistent `MaterialError.describe` call with the typed
`Result<unit, MaterialError>` success assertion used by `StudyMessageTests` and
renames the `component` binding and its uses to `muellerComponent`. This
addresses FS0039 and all three reported FS0046 locations in source; a fresh
build and the remaining gates are pending. The supplied log also reports
NU1902 package advisories and two SYSLIB0051 warnings in vendored MathNet,
outside the slice's authorized edits.

The pending roster is `build` (600 s), `elevation` (600 s),
`constructor-unit-tests` (180 s), and all twenty `invariant-s40-*` gates
(300 s each), as frozen in `0408.gates`. Constructor selection remains the
whole discovered suite. No descriptor, allowance, count floor, oracle or census
was changed.

Exactly three facts were added under the names authorized by the slice. Each
uses a fresh disposable library database, an initialized in-memory kit opened
through `KitContext.openKit`, an injected stepping clock and the existing pure
promotion fixtures. The material is saved at version one and snapshotted and
pinned by `recordStudy` before promotion. The facts assert:

- Material version two resolves to the fitted crystal, version one retains its
  saved entry and hash, fit provenance names the kit/study/run, both snapshots
  and pins exist, and the stored study and earlier file bytes are unchanged.
- Black-box versions one and two retain the item's identity and custom-Mueller
  behavior, carry their respective run provenance, remain pinned and snapshotted,
  and leave the earlier version and files unchanged while latest advances.
- Read-only promotion returns the exact `promoteToLibrary` refusal for both
  methods, preserves held entries, paths and file bytes, and does not call the
  guarded library reads, seal, write, pin or snapshot functions.

The existing error-rendering fact now covers all fourteen cases, retaining its
earlier assertions. The YAML preserves the preceding measured constructor count
from checkpoint `406` in `.checkpoints-json`; three added facts imply an expected
1738, which is not a measured result or replacement baseline.

```yaml
gates:
  constructor_unit_tests: 1735
```

## Architecture

- The only new public behavior is the prescribed `KitLibraryContext` extension
  pair and three `PublishError` cases. No serialized format or schema changes.
- Both members refuse read-only access first, then call the existing pure
  promotion member. One private function shares the persistence sequence:
  `tryHeldEntry`, next-version selection, content construction, `sealEntry`,
  `putEntryVersion` with `MoveLatestVersion`, then `snapshotReferences` of the
  explicit version. This adds no library or kit IO verb.
- Entry content carries `run.provenanceIn` for the actual kit identity. Material
  content preserves `CrystalMaterialPayload`; the black box uses the existing
  polarizer-preset constructor and shipped resolved black-box model.
- The study, samples and earlier library versions are not rewritten or deleted.
  No rollback, retry, migration or compatibility path was added.

## Deferred

No slice implementation is deferred. The supervisor owns the pending build,
test, analyzer and invariant verification under the IMPLEMENT worker protocol.

## Gotchas

- `LibraryEntryContent.ofMaterialEntry` is total and returns content directly;
  `ofPolarizerPreset` returns `Result`. The shared sequence lifts material
  content into `Ok` and maps actual preset-construction errors to
  `PromotionEntryErr`, preserving both existing APIs.
- The library write precedes snapshotting, so the explicit read-only guard is
  essential. A later snapshot failure is returned as `PromotionKitErr` after
  the new library version has been written, as required by the specified order.
- The in-memory kit keeps snapshots as envelopes; the byte-preservation fixture
  serializes those through the existing entry-file serializer and reads other
  file bytes through the kit proxy, excluding only the known fixture folders.
- The inherited `specs/0050/.manifest.state.json` modification belongs to the
  supervisor and remains untouched. No version, analyzer configuration,
  notebook, external repository or git history was changed.
- `commit_ready: false` denotes pending supervisor gate evidence, not an
  intentionally partial implementation.

## Changelog

- 2026-11-14: Implemented step 0408's library version minting, fit provenance,
  kit snapshots and pins, read-only refusal, typed errors and three persistence
  regression facts.
- 2026-11-14 (attempt 02): Repaired the supervisor-reported fixture compilation
  error and reserved-name warnings; preserved all acceptance assertions and
  left gate verification to the supervisor.
