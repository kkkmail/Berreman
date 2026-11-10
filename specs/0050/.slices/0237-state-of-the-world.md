# Step 0237 — state of the world

## Where we are

Spec 0050, section S7.2 ("The Items panel, the flow strip and item mapping on copy"), step 0237 (IMPLEMENT),
obligation `S72-RELABEL-FILE`.

- Step 217 gave an item its `<label>.item-json` file.
- Step 218 read and wrote a kit's items; until now no member renamed an item's file.
- Step 219 added `KitLibraryContext.recordItem`.
- Step 236 added the pure `Item.relabelledAmong`.
- This step persists a relabel. The item moves to the file of its new label, and a failed relabel leaves it under
  its old label.
- The Items panel's label edit records through this member when it is wired.

## What's working

- Added `KitLibraryContext.recordRelabelledItem`. It records the relabelled item and then deletes its old label's
  file. If that delete fails, it removes the new file, so a kit never holds two files of one `ItemId`.
- A change of case alone (`cpl1` → `CPL1`) deletes the one file before rewriting it. If that record fails, the
  previous item is written back.
- A read-only kit refuses with `KitReadOnlyErr` and writes nothing. A failed undo is logged with `Logger.logError`.
- Added three `KitLibraryTests` facts (constructor-unit-tests 1360 → 1363).

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster: build, elevation,
constructor-unit-tests over everything discovered, and the fourteen s24 invariants.

The worker's development runs over the final tree are in the impl-log's `Testing state`, with logs under
`specs/0050/.artifacts/0237-*`:

- **build:** exit 0, `0 Warning(s)`, `0 Error(s)`. The run was a clean rebuild from the final source.
- **constructor-unit-tests:** `Passed!`, 1363 succeeded, 0 failed, 0 skipped, in 65 s against a 180 s estimate. That
  is checkpoint 236's 1360 plus the 3 declared facts:
  - `a relabelled item moves to the file of its new label`;
  - `a relabel that changes only the case rewrites the one file`;
  - `a relabel whose old file cannot be deleted leaves the item under its old label`.
- **Mutation check:** four mutations of the member were each caught by a declared fact:
  - no old-file delete;
  - the case change recorded before its delete;
  - no undo delete;
  - no restore.

  The source was then restored byte for byte, rebuilt and re-run.
- **elevation** from the arc origin `f395597`: exit 0, `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, in
  193 s. Known findings: 0.
- **The fourteen s24 invariant greps:** none finds anything.

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1363
```

## Architecture

- **One recorder, two orders.** `recordRelabelledItem` always writes through `recordItem`, so a relabelled item's
  model and binding are snapshotted and pinned exactly as for a new one. Only the order differs:
  - different stems, ignoring case: write the new file, then delete the old one;
  - equal stems: delete first, because the kit root is one Windows folder, where a rewrite keeps the held file's
    case.
- **Undo, never two files.** Each branch undoes what it wrote before it answers. The different-stem branch deletes
  the new file, and the equal-stem branch writes `previous` back. The first error is answered, and a failed undo is
  logged. That keeps `readItems`' repeated-id refusal (step 218) from ever firing over a failed relabel.
- **No separate access check.** The read-only refusal comes from the first effect: `snapshotReferences` or the
  read-only view's `deleteKindFile`.
- No other member is added. `relabelledAmong` (step 236) still owns the label rule, and this member only persists
  its answer.

## Deferred

Nothing in this step's scope is deferred. Wiring the Items panel's label edit to this member belongs to a later
S7.2 step's own slice; it is not left over from this step.

## Gotchas

- **The in-memory folder, like Windows, keeps a held file's case on rewrite.** Recording `CPL1` over `cpl1.item-json`
  writes into `cpl1.item-json`. That is why the case-only branch deletes first. If it recorded first, the delete
  would leave no item file at all, as mutation M2 shows.
- **The caller sees only the first error.** A failure of the undo itself (the restore write, or the delete of the
  new file) shows only in the log, through `Logger.logError` with the error's `describe`.
- **Exactly equal stems take the delete-first branch too.** That includes two labels `FileStem.ofText` maps to one
  stem, such as `A/B` and `A_B`.

## Changelog

- 2026-11-09 — Step 0237:
  - added `KitLibraryContext.recordRelabelledItem`, which moves a relabelled item to its new label's file, rewrites a
    change of case as one file, and undoes a failed relabel;
  - added three `KitLibraryTests` facts (constructor-unit-tests 1360 → 1363).
