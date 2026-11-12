# Step 0319 — impl-log

## Progress

- [x] Model: `DataSplitState` is declared before the model. `dataSplitOpt` was added beside the Data step's fields, and
  `initWith` sets it to `None`.
- [x] Messages: `SplitByColumn`, `ToggleSplitOffer` and `CancelDataSplit` sit after `SaveImportProfile`, each with a
  one-line doc.
- [x] Handlers:
  - `SplitByColumn` runs inside `guardKitWrite`.
  - `ToggleSplitOffer` acts on offered groups only.
  - `CancelDataSplit` sets `dataSplitOpt` to `None`.
- [x] Tests:
  - New `DataSplitModelTests` with three facts.
  - `KitWindowModelTests`' read-only fact also sends `SplitByColumn` and asserts that `dataSplitOpt` is unchanged.
- [x] Development runs: build, constructor-unit-tests, ui-smoke, ui-tests (selected), elevation and the 16 invariants,
  all green over the final tree.
- [x] Mutation check: two defects, each caught by its fact. The file was restored byte-identical.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
  - Opens `OpticalConstructor.Domain.DataSplits`, for `splitKeyOpt`, `splitGroups`, `DataSplit.ofGroups` and the match
    cases.
  - Opens `OpticalConstructor.Storage.LabFileImport`, for `ChosenImportProfile.mapText`.
  - Adds the `DataSplitState` record, the model field and its `initWith` value, the three messages and their handlers.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DataSplitModelTests.fs`: new, `module DataSplitModelTests`, with
  three facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: one `<Compile>` line after
  `DataSplitTests.fs`, with no comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`: the read-only fact sends
  `SplitByColumn` and asserts that `dataSplitOpt` is unchanged for every refused message.

## Decisions

- **The table is `model.recordedSlots`.** That member is exactly the slice's
  `model.elements |> List.map (fun s -> s.recordedWith model.kitItems)`, so it is reused, not re-written.
- **Error rendering.** A read error is a `KitError` and renders with `describe`. A `mapText` or `splitGroups` error is
  an `ExperimentDataError` and renders with its Ui `caption` (`ConstructorUiIdOfDomain.fs`). This is the slice's
  "caption or describe".
- **The status leaf is `file.leafName`**, STL's `FileName.leafName` (`Path.GetFileName`).
- **`ToggleSplitOffer` and `CancelDataSplit` are not behind the read-only guard.** The slice says they "write nothing".
  A read-only kit also never holds a split, because `SplitByColumn` is refused there.
- **A new split replaces the held one** and starts with `creating = []`. A refusal changes only `kitStatus`.
- **Fixture.** The items and the template table are test-local copies of `SetupModelTests`' private fixture, using new
  GUIDs (`31900000-…`). `SetupModelTests.fs` is outside this step's edits, so its fixture cannot be shared. Step 318's
  `DataSplitTests` did the same. LPd's dial zero is 10°, as `SetupModelTests.stocked` sets it.
- **File locations.** The kit is `<BaseDirectory>/DataSplitModelTests/<guid>/kit`. The Phone ROI file is
  `<guid>/phone-roi.csv`, outside the kit. The `<guid>` folder is deleted at the end.

## Testing state

These were development runs of each gate's own command over the final tree. The gate engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors; no lowercase `error` in stdout | 38 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known 0; ELEV004 excluded (suppressed 1434, as at 318) | 216 s | 600 s |
| constructor-unit-tests | Passed!, 1545 of 1545 | 70 s | 180 s |
| ui-smoke | Passed!, 253 of 253 | 222 s (run beside elevation) | 600 s |
| ui-tests (InverseConstructorTests, KitWindowTests) | Passed!, 30 of 30 | 29 s | 360 s |
| invariant-s31-\* (16) | each decoded `git grep` finds nothing, untracked files included | < 1 s each | 300 s |

- **constructor-unit-tests: 1545.** This is checkpoint 1542 + 3. The TRX shows the three new `DataSplitModelTests`
  facts and the changed `a kit opened read-only refuses every write message and writes nothing` as `Passed`.
- **ui-smoke: 253.** This equals checkpoint 253.
- **ui-tests: 30** over the two selected classes. No UI test was added or changed, so the count is unchanged.
- **Mutations** (`.artifacts/0319-dev-mutation.log`):
  - `ToggleSplitOffer` was made to tick any group's key. `a split offer is ticked and unticked, and cancelling the split
    forgets it` failed.
  - `SplitByColumn`'s `guardKitWrite` was replaced by a pass-through. The read-only fact failed.

  The view file was restored from its backup, `cmp`-identical, and the final tree was rebuilt and re-tested.
- No run exceeded its effective deadline.

## Artifacts

- `specs/0050/.artifacts/0319-dev-build.log`
- `specs/0050/.artifacts/0319-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0319-dev-trx/constructor-unit-tests.trx`
- `specs/0050/.artifacts/0319-dev-ui-smoke.log`
- `specs/0050/.artifacts/0319-dev-ui-tests.log`
- `specs/0050/.artifacts/0319-dev-elevation.log`
- `specs/0050/.artifacts/0319-dev-invariants.log`
- `specs/0050/.artifacts/0319-dev-mutation.log`

## Gotchas

- **The slice's "five groups in file order" means four.** The file has five rows. Two of them (capture indices 0 and 1)
  belong to `LP1-(QZ#45)-LPd`, so there are four distinct keys. The listed groups and the required status
  (`1 matched, 2 offered, 1 unmatched`) both describe four groups, so the fact asserts four.
- **xUnit v3 filters of different kinds are AND-ed.** `--filter-class X` together with `--filter-method Y` ran zero
  tests, so the classes were run one at a time.
- **Opening `DataSplits` in the view does not disturb its record inference.** Its labels (`key`, `points`, `setup`,
  `table`, `items`, …) are shared with other records, but the build is clean. Every new construction is still annotated.
- **On this host, a Git Bash `grep -c` for a carriage return miscounts.** Count the CR bytes with Python, or use
  `git ls-files --eol`. Every touched file is LF only.
