# Step 0224 — state of the world

## Where we are

Spec 0050, section S6 ("Send and receive kits; copy experiments between kits"), step 0224 (IMPLEMENT). This is the
first copy operation of 001 §H "Between kits".

- `KitContext.copyExperimentsFrom` copies chosen experiments from another kit. With each it brings the data files
  it references and the source's snapshots of its library closure that the target lacks.
- Step 225 puts it behind the kit window's semantic messages.
- Steps 227-229 extend it with item transfer and explicit mapping before S6's closing composition.

## What's working

- Added `ExperimentCopyReport` and `KitContext.copyExperimentsFrom` (Storage). The copy reads everything before it
  writes anything, and only reads the source.
- Copied experiments are renumbered on an id clash (`Experiment.renumberedFor`).
- Data files keep their data folder and take the next free suffix on a name clash, and their hashes are
  re-recorded. A file shared by several copied experiments is copied once.
- The closure is resolved from the source's own snapshots. Versions the kit already holds are kept, not
  rewritten.
- A read-only target, a missing source experiment and a missing source data file each refuse the copy with nothing
  written.
- Added `ExperimentCopyTests`, the eight declared facts; constructor-unit-tests went from 1323 to 1331.

## Tests

These are development runs over the final source, recorded in `0224-impl-log.md`. The gate engine gives the
verdict (Invariant 6).

- **build:** exit 0, 0 warnings, 0 errors on the final rebuild.
- **constructor-unit-tests** (all discovered): **1331** passed, 0 failed, 0 skipped.
  - That is 1323 + 8, the eight declared identities.
  - Each is `Passed` in the TRX.
- **elevation** (from the arc origin `f395597`, ELEV004 excluded):
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 201 s against its 600 s estimate;
  - suppressed findings went from 1311 to 1312: one ELEV004 `.value` naming the missing experiment's file in the
    error reason.
- **The fourteen `invariant-s22-*` gates:** each holds; their greps find nothing, also with `--untracked`.
- **The facts bite.** Eight mutants were each caught by the fact meant to guard them, then restored byte for byte:
  - held snapshots rewritten;
  - a shared file copied twice;
  - an attachment not replaced;
  - no renumbering;
  - snapshots written before the data are read;
  - the data folder dropped;
  - no read-only guard;
  - a missing id ignored.
- No deadline was exceeded. This step's roster is build, elevation and constructor-unit-tests; `unit-tests`,
  `ui-smoke` and `ui-tests` are not in it, and no solver or UI code changed.

```yaml
gates:
  constructor_unit_tests: 1331
```

## Architecture

- **The copy is a member on the target's `KitContext`,** beside the typed experiment reads and writes it uses
  (steps 146-149). It is a read phase followed by a write phase:
  - **Reads:** source experiments; source snapshots and their closure; the target's held snapshots; source data
    files; the target's experiment ids.
  - **Writes:** snapshots, then data files, then experiments, so a written experiment never references a file the
    kit lacks.
  - Every `KitError` becomes `KitLibraryKitErr`. The first failure stops the copy and keeps what it wrote, as
    `recordExperiment` does.
- **No library pinning.** A copied version arrives as the source's snapshot, as a received kit's does. The kit's
  import dialog resolves it at the next open (step 141).
- **Data files go through `copyInFile` directly** (step 133's suffix rule), never through `attachLabFile`. Its
  in-kit check could mistake a source path for the target's own: every in-memory kit shares one absolute folder name.

## Deferred

Nothing in this step's scope is deferred. Steps 225 (the kit window's copy messages) and 227-229 (items and
explicit mapping) are their own steps, named by this step's brief. Nothing is left over from this one.

## Gotchas

- `CatalogueImportContext.create` lives in `OpticalConstructor.Storage.CatalogueImportStore`. A test that records
  through the library opens it.
- An in-memory read-write open adds `.oc` and `.oc/lock` to `paths ()`. The brief's exact-path list for the first
  fact leaves them out, and also leaves out the `E1.experiment-json` the copy writes. The fact asserts the full,
  real list.
- The latest-snapshot resolution repeats a private pattern in `KitLibrary.fs` (`readSnapshot`). That file was
  outside this step's authorized edits, so the resolution stays local to the member.

## Changelog

- 2026-11-09 — Step 0224: added `ExperimentCopyReport` and `KitContext.copyExperimentsFrom`. The copy brings chosen
  experiments from another kit with their data files and missing snapshots, renumbered and suffixed on clashes,
  reading everything before writing. Added `ExperimentCopyTests` (eight facts); constructor-unit-tests went from
  1323 to 1331.
