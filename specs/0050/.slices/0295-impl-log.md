# Step 0295 — impl log

## Progress

1. Read the task, the system and project prompts, the slice, AGENTS.md and the 0294 records, then wrote the impl plan.
2. **Domain** (`ExperimentDataLoad.fs`): added `ExperimentDataBadge` after `loadAndValidate`.
   - Cases: `NoDataBadge`, `MappedDataBadge of problemOpt`, `ValidatedDataBadge of points`,
     `DataChangedBadge of attachment * currentOpt` and `OutOfRangeBadge of problem`.
   - `text` and `static member ofCheck` (the one rule, in the slice's order).
   - Added `open OpticalConstructor.Domain.LibraryEntryFiles` for `ContentHash`.
3. **Captions** (`ConstructorUiIdOfDomain.fs`): added `type ExperimentDataLoad.ExperimentDataBadge with member this.caption`,
   with the seven captions the slice lists.
4. **View** (`TableAndElementRotationView.fs`):
   - Deleted `ExperimentFileStatus` and `ExperimentDataError.fileStatus`.
   - `experimentDataStatus` is now `Map<ExperimentId, ExperimentDataBadge>`.
   - `validateExperimentAttachment` became `checkExperimentData model experiment`. It reads the attachment status,
     validates only while the file is unchanged, and answers `ExperimentDataBadge.ofCheck`. `withKitContent` and the
     attach arm both call it.
   - The attach arm's `Error` now leaves the experiment and the badges alone and sets `kitStatus` to
     `Not attached — <describe>`.
   - Added `model.dataBadgeOf`. `dataFileRow` shows its caption, and the dead `isVisible (statusText <> "")` went,
     because every badge has a caption.
5. **Tests**:
   - `ExperimentDataProxyTests` gains `the data badge follows the attachment, its file's hash and its validation`.
   - `InverseConstructorTests`: four facts changed, one renamed, and three moved to the badge type mechanically (see
     Decisions).
6. **First full build**: exit 0, 0 errors. The only warnings are the two known vendored-MathNet `SYSLIB0051` warnings
   (`specs/0048/.manual/001`, `specs/0050/.manual/004`). That build recompiled MathNet; later builds are incremental.
7. **`KitError.caption` probe**: removed the extension and rebuilt the whole solution, products and tests. There were no
   errors, so it had no caller left. Its doc named the retired attachment status ("as the measured-data attachment
   status, spec 0050 S4.2"), so it stays deleted as superseded code (AGENTS.md: superseded code is deleted in the
   change that replaces it).
8. **Rebuild**: touched every edited file and rebuilt. exit 0, 0 warnings, 0 errors.
9. **Development runs** with the gates' own commands: constructor-unit-tests, the selected ui-tests, ui-smoke,
   elevation and the 16 invariants. All green (see Testing state).
10. **Mutation check, red then green.** Both product files were saved first (SHA-256 in `0295-mutation/`).
    - Mutant A moved `Ok series` ahead of the changed and missing statuses in `ofCheck`. Only the new badge fact
      failed: `Expected: DataChangedBadge …`.
    - Mutant B left `kitStatus` as `None` on a refused attach. Only the renamed fact failed.
    - Both files were restored byte for byte (the hashes match) and rebuilt.
    - constructor-unit-tests (1495) and the selected ui-tests (89) were rerun green on the restored binaries.
11. Reworded the `ofCheck` doc. This was a comment-only change, and the final build has exit 0 with 0 warnings.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ExperimentDataLoad.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentDataProxyTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/InverseConstructorTests.fs`

`ScrollExtentTests.fs` is in the authorized edits, but the slice says it "passes as written". It was not touched, and it
passes (14 of 14).

## Decisions

- **One private check for both callers.**
  - `checkExperimentData` reads the attachment status, then validates only while the file is unchanged, then answers
    `ofCheck`.
  - The kit-open recheck (`withKitContent`) and the attach arm both call it, so the attach arm now reads the status too.
  - A freshly attached file is unchanged. Every mock and the real store answer `AttachedDataUnchanged` there, so no
    badge an attach produced before changes.
- **A setup revision the kit lacks** used to give `AttachedFileValidationError`, shown as `Validation error — <describe>`.
  - The badge needs an `ExperimentDataError`, so the `SetupError` maps to `DataUnitsMismatchErr` of its `describe`.
  - That makes it an `OutOfRangeBadge`, so its caption is unchanged.
  - Units is chosen because the revision holds the detector that fixes the file's expected shape, and that detector is
    unknown.
- **A `KitError` from the status read** becomes `MalformedDataFileErr e.describe`, as `ExperimentDataStore` maps one
  (step 135). The badge is `MappedDataBadge (Some _)` and the caption is `Parse error — <describe>`, as before.
- **The data-file row always shows its badge.** `No data` now shows for an experiment without a file, which the slice's
  caption list requires (`dataFileRow` shows it). The visibility condition was dead, so it went.
- **Three facts outside the change_expectation list** named the retired type, so they were moved to the badge type
  mechanically. Their identities and every assertion are kept:
  - `the detector kind selects the ellipsometric load path`: `DataFileValidated 2` became `ValidatedDataBadge 2`.
  - `opening a kit re-validates an attachment whose data is unchanged`: the type arguments became the badge, and
    `AttachedFileParseError e.caption` became `MappedDataBadge (Some e)`. The out-of-range match is now on
    `OutOfRangeBadge` with the same `Contains` assertion.
  - `a kit write that fails keeps the experiment and shows why`: only the map's type argument changed.
- **The four changed facts** assert the badge cases where they asserted the status cases, and now also pin the captions:
  - `Loaded — 3 point(s), schema-valid`;
  - `Parse error — …` and `Validation error — …`;
  - the changed and missing captions, as before, plus `Parse error — <describe>` for the unreadable kit.
- **The renamed fact** keeps every old check in substance:
  - `dataOpt` is `None` and the collection is unchanged;
  - the reason is now asserted through `kitStatus = Some "Not attached — <describe>"`, where it used to be asserted
    through the badge;
  - it adds `NoDataBadge`, an unchanged status map, and a replacement that keeps the earlier `ValidatedDataBadge 2`.

## Testing state

These are development runs with the gates' own commands and this step's selectors. No run went over its effective
deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors (final incremental build; the first full build showed only the two known MathNet SYSLIB0051) | 14 s (77 s full) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, suppressed 1389 (as at 0294) | 207 s | 600 s |
| constructor-unit-tests | Passed!, 1495 of 1495 (1494 + 1) | 69 s | 180 s |
| ui-tests (5 selected classes) | Passed!, 89 of 89 | 104 s | 360 s |
| ui-smoke | Passed!, 249 of 249 | 212 s | 600 s |
| invariant-s29-\* (16) | each decoded `git grep` finds nothing | < 1 s | 300 s |

The ui-tests per class: ExperimentControls 39, FsxAcceptance 14, InverseConstructor 14, ScrollExtent 14 and
WorkbenchInfoAndCapture 8. InverseConstructorTests still declares 15 facts, as at HEAD. The 15th is a ui-smoke fact.

`commit_ready: true`. Every requirement of the slice landed in this round.

## Artifacts

- `specs/0050/.artifacts/0295-build.log`
- `specs/0050/.artifacts/0295-elevation.log`
- `specs/0050/.artifacts/0295-constructor-unit-tests.log`
- `specs/0050/.artifacts/0295-ui-tests.log`
- `specs/0050/.artifacts/0295-ui-smoke.log`
- `specs/0050/.artifacts/0295-mutation/`: the pre-mutation copies of both product files, the SHA-256 before and after,
  and the two red runs (`constructor-red.log`, `ui-red.log`)

## Attempt 02 (retry)

12. **Retry hint read.** It asks to delete the now-unused `KitRelativePath.toControlCaption` extension and, optionally,
    to pin the refused attach's row caption.
13. **`KitRelativePath.toControlCaption` deleted** (`ConstructorUiIdOfDomain.fs`, with its doc line). Before the edit,
    a grep over `Berreman/` found only the definition. Its one caller was the retired
    `DataFileMissingFromKit data.attachment.path.toControlCaption` (HEAD `TableAndElementRotationView.fs:1969`), and
    the missing-file caption now reads `attachment.path.value` inside `ExperimentDataBadge.caption`. After the edit,
    `git grep toControlCaption` outside `specs/` finds nothing. It is superseded code, like `KitError.caption`.
14. **The renamed fact** `a picked file the kit cannot attach records no attachment and tells the kit status why` also
    asserts `(failed.dataBadgeOf failedExperiment).caption.value = "No data"`, because the row now always shows its
    badge. No test identity was added.
15. **Rebuilt and reran everything** with the gates' own commands and this step's selectors, over the two edits above.

### Attempt 02 testing state

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors | 37 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, suppressed 1389, ELEV004 excluded | 207 s | 600 s |
| constructor-unit-tests | Passed!, 1495 of 1495, 0 failed, 0 skipped | 73 s | 180 s |
| ui-tests (5 selected classes) | Passed!, 89 of 89, 0 failed, 0 skipped | 103 s | 360 s |
| ui-smoke | Passed!, 249 of 249, 0 failed, 0 skipped | 208 s | 600 s |
| invariant-s29-\* (16) | each decoded `git grep` finds nothing | < 1 s | 300 s |

- In `ui-tests.trx`, `a picked file the kit cannot attach records no attachment and tells the kit status why` is
  `Passed`.
- No run went over its effective deadline.
- `git ls-files --eol` and a carriage-return count show the edited files are LF.
- The counts are unchanged from attempt 01 (1495 / 89 / 249). The extension had no test, and the added assertion
  joins an existing fact.
- The attempt 01 logs in `specs/0050/.artifacts/0295-{build,elevation,constructor-unit-tests,ui-tests,ui-smoke}.log`
  were overwritten by these runs. The `0295-mutation/` folder is attempt 01's and was not rerun: neither edit touches
  `ofCheck` or the attach arm.

`commit_ready: true`.
