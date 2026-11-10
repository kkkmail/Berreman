# Step 0237 — impl log (IMPLEMENT: `KitLibraryContext.recordRelabelledItem`)

## Progress

- [x] Read the protocol (arc-runner + IMPLEMENT worker), the project prompt, the slice, its `.gates` snapshot,
      manual 013 (arc-origin elevation) and the step 0219 / 0236 hand-offs. The operator note is empty.
- [x] Added `KitLibraryContext.recordRelabelledItem` to step 219's `type KitLibraryContext with` extension in
      `KitContentFiles.fs`, right after `recordItem`. Added `open Softellect.Sys.Logging` for `Logger.logError`.
- [x] Added the three declared `KitLibraryTests` facts and three private helpers beside them.
- [x] Development runs: build, the whole constructor-unit-tests suite, a four-mutation check, a clean rebuild and
      re-run, elevation from the arc origin, and the fourteen s24 invariant greps.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitContentFiles.fs` (+33):
  - `open Softellect.Sys.Logging`;
  - `member this.recordRelabelledItem (previous : Item) (relabelled : Item) : Result<unit, KitLibraryError>`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitLibraryTests.fs` (+105):
  - private helpers `relabelledItem`, `itemFilePaths` and `assertReadOnlyRelabelRefused`;
  - the facts `a relabelled item moves to the file of its new label`, `a relabel that changes only the case rewrites
    the one file` and `a relabel whose old file cannot be deleted leaves the item under its old label`.

No other file changed. Both files are LF (`git diff --numstat` equals `--ignore-cr-at-eol`).

## Decisions

- **Different stems, ignoring case.** The member calls `this.recordItem relabelled` (snapshot, pin, write), then
  `proxy.deleteKindFile ItemKitFile oldStem`. If that delete fails, it deletes `newStem` through the same verb. A
  failure of that undo is logged with `Logger.logError` and its `describe`. The member answers the first error as
  `KitLibraryKitErr`. A failed `recordItem` is answered as it is: it wrote no item file, and the snapshots it took
  stay, as `recordItem`'s own contract says.
- **Equal stems, ignoring case.** The member deletes `oldStem` first and then calls `recordItem relabelled`. If the
  record fails, it writes `previous` back through `proxy.writeItem` and logs that write's own failure. It answers
  the record's error.
  - Exactly equal stems take this branch too. That includes two labels that `FileStem.ofText` maps to one stem.
  - In this branch the delete-and-rewrite is the only correct shape. A rewrite keeps the held file's case, both on
    Windows and in the in-memory folder (`writeKindFile` reuses `tryFindTextPath`'s held path).
- **Read-only kit.** The member needs no access check of its own:
  - with different stems, the first effect is `snapshotReferences`, which answers `KitReadOnlyErr "snapshotReferences"`;
  - with equal stems, it is the read-only view's `deleteKindFile`, which answers `KitReadOnlyErr "deleteKindFile"`.
  Nothing is written in either case.
- **The undo logging stays inline in each branch.** The member has no local helper with a `string` parameter, so
  it cannot raise an ELEV003P or ELEV010 question.
- **Test coverage beyond the three named checks stays inside the three declared identities.** No new identity was
  added.
  - Facts 1 and 2 also check the read-only refusal through a read-only reopening of the same folder.
  - Fact 2 also checks the restore of `previous` when the record fails after the delete. It does this through a
    context whose `listSnapshots` fails.

## Testing state

No gate verdict is claimed here (Invariant 6). These are development runs over the final tree. The logs are in
`specs/0050/.artifacts/`.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`):
  - first run: exit 0 in 71 s. Its only warnings were the known SYSLIB0051 in vendored MathNet (manual 013).
  - after the mutation check, clean rebuild from the restored source: exit 0, `0 Warning(s)`, `0 Error(s)`, 8 s.
  - log: `0237-build.log`.
- **constructor-unit-tests** (the gate's command, every test discovered): `Test run summary: Passed!`, total 1363,
  succeeded 1363, failed 0, skipped 0, in 1 min 05 s, against a 180 s estimate. That is checkpoint 236's 1360 plus
  the 3 declared facts. The TRX holds the three new ids, each `Passed`. Log: `0237-constructor-unit-tests.log`.
- **The facts bite.** `0237-mutations/run-mutations.py` applied each mutation to `recordRelabelledItem`, built the
  test project and ran `KitLibraryTests`:

  | Mutation | Facts that went red |
  |---|---|
  | M1: no delete of the old file | facts 1 and 3 |
  | M2: a change of case recorded before the delete (taking the other branch) | fact 2 |
  | M3: no undo delete of the new file | fact 3 |
  | M4: no restore of `previous` | fact 2 |

  The script then restored the source; `cmp` with the saved original is identical. The solution was rebuilt and the
  suite re-run on that source, as recorded above. Summary: `0237-mutations/summary.txt`.
- **elevation** (`ASG_ARC_BASE_COMMIT=f395597…`, ELEV004 excluded): exit 0, `ELEV-GATE: OK - 0 new finding(s) in
  12 project(s), 193s`. Known findings: 0. The run widened to all 12 projects because the solution changed since
  the arc origin. Log: `0237-elevation.log`.
- **The fourteen s24 invariant greps:** none finds anything. Log: `0237-invariants.log`, which is empty.

No run came near a deadline. No test was removed or renamed, and no assertion was weakened.

## Artifacts

- `specs/0050/.artifacts/0237-build.log`
- `specs/0050/.artifacts/0237-constructor-unit-tests.log`
- `specs/0050/.artifacts/0237-elevation.log`
- `specs/0050/.artifacts/0237-invariants.log`
- `specs/0050/.artifacts/0237-mutations/`: the driver, the saved original, the per-mutation build and test logs,
  and `summary.txt`.

## Gotchas

- **The in-memory folder keeps a held file's case on rewrite.** Recording `CPL1` over an existing `cpl1.item-json`
  writes into `cpl1.item-json`. So the case-only branch must delete before it records. Fact 2 asserts the exact
  resulting name. M2 shows that recording first leaves no item file at all.
- **The refusing proxy in fact 3 refuses only the stem `CPL1`** (`stem = cpl1Item.label.fileStem`). The undo delete
  of `CPL 1` therefore reaches the in-memory folder and succeeds. That is how the fact proves that no second file of
  one id is left, which `readItems` would refuse.
- **An undo failure is logged, never answered.** The caller sees the first error. A failure of the restore or the
  undo delete is visible only in the log.
