# Step 0310 — impl log

## Progress

- Read the protocol, the project prompt, AGENTS.md, the slice and step 0309's state of the world.
- Wrote the impl plan.
- View (`TableAndElementRotationView.fs`):
  - `KitWindowShell.openImportWizard`; `inert` holds `ignore`.
  - `AttachDataFileTo` became `AttachDataFile (id, file, profile)`. The arm records the message's profile. The
    detector-slot lookup it used only for the built-in profile is gone; everything else is unchanged.
  - Added `OpenImportWizard` and `SaveImportProfile` arms behind `guardKitWrite`.
  - Added `ImportWizardApplied.messages`.
  - The Attach data file… button now sends `OpenImportWizard`.
- `InverseConstructorTests`:
  - Fifteen dispatches now send `AttachDataFile … intensitySeries`, and the ellipsometric fixture sends
    `ellipsometricSeries`.
  - The ellipsometric fact is renamed. It also asserts the recorded profile, an extension with no new identity.
- `KitWindowModelTests`:
  - The `wizardShell` recorder, the `withWizardKit` fixture, and `phoneRoiBytes` / `phoneRoiByColumns`.
  - Three new facts.
  - The read-only fact also sends the three new messages and asserts an empty recorder.
- Build, constructor-unit-tests, selected ui-tests, ui-smoke, elevation and the s30 invariants were run as development
  checks (see Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/InverseConstructorTests.fs`

## Decisions

- **`Lab scan` is `KitTests.labScanProfile`.** That public test fixture is exactly the slice's `Lab scan`: comma, a
  header row, no mark, scan column 1 in degrees as a physical value, `SignalColumns (2, Some 3)` and key column 4.
  AGENTS.md's "reuse, never copy" rules out re-transcribing it. `Phone ROI by columns` has no public twin (the
  `ImportWizardTests` and `LabFileImportTests` copies are private), so it is test-local.
- **`withWizardKit` builds the scene with `initInverse`.** It uses fresh Built-in stores, then `loadKit`s a read-write
  folder kit and commits `E1` through the window (`committedPolarizerExperiment`, the `filledExperiments` recipe). The
  fixture asserts that `E1` is a one-axis R1 scan over `VariableRange.forVariable VaryR1` (0…360°).
- **Extensions with no new identity:**
  - Fact 1 also refuses a built-in name (the acceptance's "a held or built-in name"). It checks that the window's own
    write is not offered for reload, and that each refusal changes only `kitStatus`.
  - Fact 2 also checks that the request's `readPickedFile` reads the picked bytes, and that an unknown experiment
    opens nothing.
  - Fact 3 also checks `messages` without a saved profile.
- **`OpenImportWizard` reads the slots the way `AttachDataFileTo` did**, with `slotsIn … |> Result.defaultValue []`. An
  unresolvable revision falls back to the detector default `Intensity`, as `detectorKindOf` does.

## Testing state

Development runs of each gate's own command over the final tree:

- build: exit 0, 0 errors. The only warnings are the two vendored MathNet SYSLIB0051 warnings that the operator
  recorded as pre-existing (0185 operator note). There is no `error` in stdout. 78 s.
- constructor-unit-tests: Passed!, 1525 of 1525 (1522 + 3). 69 s.
- ui-tests (DataFilePickerFolderTests, InverseConstructorTests, KitWindowTests): Passed!, 37 of 37. 29 s.
- ui-smoke: Passed!, 251 of 251. 219 s.
- elevation: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 suppressed 1426 (as at
  step 309). 213 s.
- invariant-s30-\* (16): each exit 0, `INVARIANT HOLDS`.
- No run exceeded its effective deadline.
- `git grep -n -F "AttachDataFileTo" -- Berreman/` finds nothing.

## Artifacts

- `specs/0050/.artifacts/0310-build.log`
- `specs/0050/.artifacts/0310-constructor-unit-tests.log`
- `specs/0050/.artifacts/0310-ui-tests.log`
- `specs/0050/.artifacts/0310-ui-smoke.log`
- `specs/0050/.artifacts/0310-elevation.log`
- `specs/0050/.artifacts/0310-invariants.log`
