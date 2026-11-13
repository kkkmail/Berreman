# Step 0369 — impl log

## Progress

- [x] Domain `PrefitChecks.fs` + fsproj entry (after `ExperimentMatrices.fs`, no comment)
- [x] `SolverHandoffView.fs` calls `prefitCheckWith`; the private slot-state rule is deleted
- [x] `PrefitCheckTests.fs` (5 facts) + fsproj entry (appended last, no comment)
- [x] Build, constructor tests, SolverHandoffWindowTests, ui-smoke, elevation, the twenty s37 invariants

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/PrefitChecks.fs` (new): `module PrefitChecks` —
  `SlotSpecification` (`SpecifiedSlot` | `UnspecifiedSlot`), `SetupSlot.specification`, `PrefitError`
  (`UnspecifiedSlotsErr` / `MissingDataErr` of a reason, `UnreadableDataErr` / `InconsistentDataErr` of an
  `ExperimentDataError`) with `describe`, and `Experiment.prefitCheckWith library dataProxy setups :
  Result<MappedPoints, PrefitError>`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`: one `<Compile>` line.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/SolverHandoffView.fs`: `validateExperiment` is
  `prefitCheckWith` mapped onto `ExperimentHandoffStatus`; the private `HandoffSlotState` union and the
  `SetupSlot.handoffState` member are deleted; `handoffSetupCaption` tags a slot through a private `handoffTag` that reads
  `specification` (an unspecified `Sample` slot still reads "the unknown (sample)").
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PrefitCheckTests.fs` (new): `module PrefitCheckTests`, the five
  facts the slice names.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: one `<Compile>` line, last.
- `SolverHandoffWindowTests.fs`: authorized but not edited — it runs unchanged, every fact and fixture as it was.

## Decisions

- **Reasons moved verbatim.** `prefitCheckWith`'s reasons are exactly the texts `validateExperiment` gave
  (`its setup is not in the collection: <SetupError.describe>`, `every element except the sample must be specified;
  still unbound: <roles>`, `no measured-data file is attached to this experiment`), so every caption the hand-off screen
  shows is unchanged. The slice's "a `SetupError` answering `UnspecifiedSlotsErr` of its `describe`" is read as the
  reason *carrying* the `describe` with the view's prefix: dropping the prefix would change a caption, which the
  acceptance forbids.
- **ELEV012-safe filter.** The unspecified slots are gathered with `List.choose` answering the role's `text`, never a
  `bool` over the two-case `SlotSpecification`.
- **No free reason helper.** `PrefitError.describe` reads an `ExperimentDataError`'s reason through or-patterns, so no
  free function of one of our types appears (ELEV013P), and `MeasuredData.fs` (outside the step's edits) is untouched.
- **Test fixtures.** Slots as `SetupTests` builds them (source bound to `PresetIds.heNe633`, LP1 on
  `PolarizerPresetChoice` of `polarizerLp`, an empty sample, PD1 on `intensityDetectorModelId`), setups by
  `Setup.create`, the experiment as `StudySessionTests`' run with a `oneAxis VaryR1` scan of the polarizer over 0…90° in
  4 points, items through `ItemDraft.toItem`, and a test-local `ExperimentDataProxy` whose `readMapped` answers by path
  and whose other fields answer `KitIoErr`.

## Testing state

All run locally after the final edit (Release x64):

| Gate | Result | Duration | Deadline |
|---|---|---|---|
| build | exit 0, no warning from our code (only vendored MathNet `SYSLIB0051` and NuGet advisories, pre-existing) | 79 s full / 24 s incremental | 600 s |
| constructor-unit-tests | Passed!, 1619 succeeded (checkpoint 367: 1614; +5 `PrefitCheckTests`) | 71 s | 180 s |
| ui-tests (`SolverHandoffWindowTests`) | Passed!, 9 succeeded (the class's two ui-smoke facts run in ui-smoke) | 19 s | 360 s |
| ui-smoke | Passed!, 258 succeeded | 229 s | 600 s |
| elevation (arc base `f395597c`) | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` | 218 s | 600 s |
| twenty s37 invariants | each decoded `git grep` found nothing (tracked and untracked) | < 5 s | 300 s |

## Artifacts

- `specs/0050/.artifacts/0369-build.log`
- `specs/0050/.artifacts/0369-constructor-unit-tests.log`
- `specs/0050/.artifacts/0369-ui-tests.log`
- `specs/0050/.artifacts/0369-ui-smoke.log`
- `specs/0050/.artifacts/0369-elevation.log`

## Gotchas

- The slice names the retired rule `SolverHandoffView.unspecifiedElements`; in the tree it was the private
  `HandoffSlotState` union and `SetupSlot.handoffState` member (step 261). Those are what was deleted; the screen's tag
  now reads `SetupSlot.specification`.
- An `{ points = … }` record literal infers `EllipsometricSeries` (the last type with that label) — the test annotates
  `IntensitySeries` / `IntensityPoint`.
