# Step 0295 — state of the world

## Where we are

Spec 0050 section S9.1 covers Experiments v2: scans, roles, sessions and attachments.
- Step 294 made an experiment's role, session and notes editable by message.

This step gives each experiment's data status one pure Domain badge (001 §K: no data / mapped / validated (points) /
data changed / out of range; 001 §E: a changed or missing data file is flagged). The window's `ExperimentFileStatus` is
deleted, and the view projects the badge. 001's excluded-points badge arrives with S9.3's `ExcludePoint`, by the spec.

## What's working

- Added `ExperimentDataBadge` to Domain: `NoDataBadge`, `MappedDataBadge`, `ValidatedDataBadge`, `DataChangedBadge` and
  `OutOfRangeBadge`, with `text`. `ExperimentDataBadge.ofCheck` is the one rule over the mapped attachment, the file's
  status and the validation.
- Retired the view's `ExperimentFileStatus`. `experimentDataStatus` holds badges, computed by one private check on kit
  open and on attach. `model.dataBadgeOf` answers any experiment's badge.
- Added `ExperimentDataBadge.caption`, which keeps every caption the window showed and adds `No data` and
  `Mapped — not validated`. The data-file row always shows it.
- Deleted the superseded `KitError.caption` and `KitRelativePath.toControlCaption` extensions, whose only callers were
  the retired status's cases.
- An attach the kit refuses keeps the experiment's data and badge and says `Not attached — <reason>` in the kit status.
- Added a new Domain fact for the badge rule and texts. Four inverse facts now assert badges, and the attach-refusal fact
  was renamed to its new behaviour.

## Tests

These are attempt 02's development runs with the gates' own commands and this step's selectors, run after the retry's
two edits. The arc-runner's gate engine runs the gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors | 37 s (attempt 02) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded | 207 s | 600 s |
| constructor-unit-tests | Passed!, 1495 of 1495, 0 failed, 0 skipped | 73 s | 180 s |
| ui-smoke | Passed!, 249 of 249, 0 failed, 0 skipped | 208 s | 600 s |
| ui-tests (5 selected classes, the gate's exact command) | Passed!, 89 of 89, 0 failed, 0 skipped | 103 s | 360 s |
| invariant-s29-\* (16) | every decoded `git grep` finds nothing | < 1 s each | 300 s |

- **constructor-unit-tests: 1495**, which is 1494 plus `the data badge follows the attachment, its file's hash and its
  validation`.
- **ui-tests: 89** over this step's five classes: ExperimentControls 39, FsxAcceptance 14, InverseConstructor 14,
  ScrollExtent 14 and WorkbenchInfoAndCapture 8.
  - The renamed `a picked file the kit cannot attach records no attachment and tells the kit status why` passes. It
    now also pins the row's `No data` caption.
  - The four changed facts pass.
  - InverseConstructorTests still declares 15 facts, as at HEAD. One of them is ui-smoke, so the count is unchanged.
- **ui-smoke: 249**, unchanged.
- **Mutation check (red, attempt 01).** Both files were restored byte for byte (SHA-256 checked) and rebuilt, and the suites were
  rerun green.
  - Validating ahead of the changed or missing status failed only the new badge fact.
  - A refused attach with no kit status failed only the renamed fact.

```yaml
gates:
  constructor_unit_tests: 1495
  ui_smoke_tests: 249
  ui_tests: 89
```

## Architecture

- **The badge is Domain behaviour; the view projects it.**
  - `ExperimentDataLoad.ExperimentDataBadge.ofCheck` decides the badge from data alone: no data first, then a changed or
    missing file, then the validation's outcome.
  - The caption is a Ui extension in `ConstructorUiIdOfDomain.fs`, because Controls' `ControlCaption` is not visible to
    Domain.
- **One private check, `checkExperimentData`, serves the kit open and the attach.**
  - It reads the attachment's status, validates only while the file is unchanged, and maps a kit read failure to
    `MalformedDataFileErr`, as `ExperimentDataStore` does.
  - A setup revision the kit lacks maps to `DataUnitsMismatchErr`, so that it stays `Validation error — …`.
- **`experimentDataStatus` keeps a badge only for experiments with a mapped attachment**, as before.
  `dataBadgeOf` falls back to `ofCheck experiment.dataOpt None None`.
- **`KitError.caption` and `KitRelativePath.toControlCaption` are deleted.**
  - Their only callers were the retired status's cases (`AttachedFileParseError`, and `DataFileMissingFromKit
    data.attachment.path.toControlCaption`).
  - The missing-file caption now reads the path inside `ExperimentDataBadge.caption`.
  - After the deletion, `git grep toControlCaption` outside `specs/` finds nothing, and the build is clean.

## Deferred

Nothing. Every requirement of the step landed in this round. The excluded-points badge needs S9.3's exclusions, so it
arrives with them, by the slice.

## Gotchas

- **The attach arm now reads the attachment status as well**, because both callers share one check. That costs one hash
  of the just-attached file.
  - A mock proxy whose `attachmentStatus` throws, or answers changed or missing, now affects the attach badge.
  - Every current mock answers `AttachedDataUnchanged`.
- **The data-file row's status line is always visible now.** It shows `No data` for an experiment without a file.
  ScrollExtentTests and ui-smoke pass with it.
- **Three InverseConstructorTests facts** outside the change_expectation list named the retired type, and they changed
  type only:
  - `the detector kind selects the ellipsometric load path`;
  - `opening a kit re-validates an attachment whose data is unchanged`;
  - `a kit write that fails keeps the experiment and shows why`.
  Their identities and assertions are kept.
- **A full rebuild that recompiles the vendored MathNet** shows its two known `SYSLIB0051` warnings
  (`specs/0050/.manual/004`). An incremental build shows none.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0295: an experiment's data status is one Domain badge.
  - Added `ExperimentDataBadge` with `ofCheck` and `text`, and the Ui `caption`.
  - Retired `ExperimentFileStatus` and `KitError.caption`.
  - Added `model.dataBadgeOf`.
  - A refused attach is told in the kit status.
  - One new Domain fact; four inverse facts changed and one renamed.
- 2026-11-11 — Step 0295, attempt 02: deleted the unused `KitRelativePath.toControlCaption` extension. The renamed
  attach-refusal fact also pins the `No data` caption. Every gate was rerun green.
