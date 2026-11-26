# Step 0305 — state of the world

## Where we are

Spec 0050 section S9.2 covers import profiles and the column-mapping wizard (001 §K).

- Steps 298 to 304 added the profiles, `MappedPoints`, one read of a lab file through any profile
  (`ChosenImportProfile.mapText`) and one validation of mapped points (`MappedPoints.validatedFor`).
- Until now the data seam still read only the two X,Y files, through `tryLoadIntensity` and `tryLoadEllipsometric`.
- This step deletes that strict seam (001 §R) and replaces it with attach-with-profile:
  - the proxy reads any attachment through its profile, a built-in or the kit file's, and keeps the rows of its row
    selection (001 §K, one file → many experiments);
  - it reads a picked file for the wizard (step 308).

## What's working

- Add `MappedPoints.selectedBy`: every row, or one key's capture rows, observations or mapped rows. A series or
  unkeyed rows refuse a key, and an empty selection is `EmptyDataFileErr` (S9.2).
- `ExperimentDataProxy` is now `{ readPickedFile; attachDataFile; attachmentStatus; readMapped }`.
  `tryLoadIntensity` and `tryLoadEllipsometric` are deleted.
- `ExperimentDataStore.createFileBacked` reads through the kit file's profiles and keeps the selected rows; a picked
  file is read whole without touching the kit.
- `loadAndValidate` is `readMapped` bound to `validatedFor`; step 291's name switch and key refusal are gone.
- Add four constructor facts (1514 → 1518). Rename four, and move the UI mocks to the new fields.

## Tests

These were development runs of each gate's own command over the final build. The arc-runner's gate engine runs the
gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0; `0 Warning(s)`, `0 Error(s)`; no lowercase `error` in stdout | 57 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded | 218 s | 600 s |
| constructor-unit-tests | Passed!, 1518 of 1518, 0 failed, 0 skipped | 73 s | 180 s |
| ui-smoke | Passed!, 251 of 251, 0 failed, 0 skipped | 224 s | 600 s |
| ui-tests (InverseConstructorTests, ScrollExtentTests, SolverHandoffWindowTests) | Passed!, 37 of 37, 0 failed, 0 skipped | 30 s | 360 s |
| invariant-s30-\* (16) | each `INVARIANT HOLDS` (`section_invariants --find-nothing`, exit 0) | under 1 s each | 300 s |

- **constructor-unit-tests: 1518**, the 1514 baseline plus 4. Every identity the slice names is `Passed` in the TRX
  under its exact name.
  - New:
    - `ImportProfileTests.a row selection keeps every row or the rows of one key`
    - `ExperimentDataStoreTests.createFileBacked reads a data file through the kit's own profile and keeps the rows of
      its key`
    - `… refuses a profile the kit lacks and a key an X,Y file cannot select`
    - `… reads a picked file whole without attaching it`
  - Renamed:
    - `ExperimentDataProxyTests.a mock ExperimentDataProxy reads canned mapped points and picked files through its
      exact signature`
    - `… an unknown path yields a typed ExperimentDataError, never a throw`
    - `… loading data validates the points the attachment's profile maps`
    - `ExperimentDataStoreTests.createFileBacked maps a missing file to a typed error through either X,Y profile,
      never a throw`
  - The three changed-expectation store facts pass under their unchanged names.
- **ui-tests: 37**, over the three selected classes: Inverse 14, ScrollExtent 14, SolverHandoff 9.
  - No fact was added or removed in any of them, so each class's count is unchanged.
  - Inverse + SolverHandoff = 23, as at step 304. ScrollExtentTests joins the selection this step.
- **Acceptance grep:** `git grep -n -E "tryLoadIntensity|tryLoadEllipsometric" -- Berreman/` finds nothing.
- **Red first:** the test project failed to compile on the missing `readMapped` / `readPickedFile` / `selectedBy`.
- **Mutation check:** four defects went into one build, and each failed its own fact:
  - unfiltered keyed captures;
  - the store dropping `selectedBy`;
  - `readPickedFile` dropping bytes;
  - `loadAndValidate` skipping validation.

  The files were restored and their SHA-256 verified before the final rebuild.

```yaml
gates:
  constructor_unit_tests: 1518
  ui_smoke_tests: 251
  ui_tests: 37
```

## Architecture

- **One read of an attachment: `readMapped`.** The store resolves the profile name (`ChosenImportProfile.tryOfName`
  over the kit file's profiles: a built-in first, then the kit's). It then reads the data file through the kit, maps
  it (`ChosenImportProfile.mapText`) and keeps the experiment's rows (`MappedPoints.selectedBy`).
  - `loadAndValidate` no longer knows any profile. It reads, then validates (`validatedFor`).
- **Row selection is a fact of the points.** `selectedBy` lives on `MappedPoints` beside `validatedFor`, and every
  shape answers it:
  - a capture's experiment label, an observation's key and a mapped row's key column select;
  - an X,Y series and a mapping without a key column refuse a key.
- **The refusal texts moved with their rules.**
  - "not available" now comes from the store.
  - "has no key column" comes from `selectedBy`, without the path prefix it carried in `loadAndValidate`.
  - Both stay on the four-case `ExperimentDataError` channel, and a `KitError` of either read is one
    `MalformedDataFileErr` naming the path and the profile.
- **The picked file has one read.** `FileName.pickedLabFile`, a private extension member in `ExperimentDataStore`,
  serves both `readPickedFile` (the wizard's preview, step 308) and `attachDataFile`. It is a member because ELEV013P
  refuses a private one-parameter function of one of our unions.
- **Compile order.** `ExperimentDataStore.fs` now compiles directly after `LabFileImport.fs`, which it reads through.
  Nothing between its old and new places names it.
- **The UI is unchanged.** `AppContext`, `SolverHandoffView`, `SolverHandoffWindow` and `TableAndElementRotationView`
  hold the proxy, build none, and compile unchanged.
- **Recorded deviation:** `OpticalConstructor.Tests.fsproj` is outside the effects list. Its stale comment blocks
  above `ExperimentDataProxyTests.fs` and `ExperimentDataStoreTests.fs` were deleted, because the first names the
  deleted fields and the acceptance grep must find nothing. The project is in `touches`, and nothing else in the file
  changed.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **`readMapped` reads the kit file first.** An uninitialised folder answers `MalformedDataFileErr` wrapping
  `NotAKitFolderErr`. Store tests must `initialiseKit` their folders; the app's kits always are.
- **The single-series UI mocks answer `readMapped` for any attachment and any profile.** The old mocks also refused
  the other X,Y profile. No fact relied on that, but a new fact that attaches an ellipsometric file under
  `mockIntensity` would now read intensity points.
- **ELEV013P is live.** A private module function whose one parameter is one of our unions (including Softellect's
  `FileName`) is a finding. Use a (private) extension member.
- **`KitTests.labScanProfile` compiles after `ExperimentDataStoreTests.fs`.** The store tests hold a test-local copy
  of `Lab scan`, as `ImportProfileTests` holds its rows.
- **The TRX writes apostrophes literally in `testName`.** Grep with `.` for `'`.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0305: the strict X,Y data seam is replaced by attach-with-profile (S9.2).
  - `MappedPoints.selectedBy` selects an experiment's rows.
  - `ExperimentDataProxy` reads attachments through `readMapped` and picked files through `readPickedFile`; the two
    X,Y loads are deleted.
  - `createFileBacked` reads through the kit file's profiles.
  - `loadAndValidate` reads, then validates.
  - constructor-unit-tests rises by four, to 1518.
