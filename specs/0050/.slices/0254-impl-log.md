# Step 0254 — impl log

## Progress

- Read the task, both system prompts, the project prompt, the slice, `.manual/016`, the 0254 gate snapshot
  and step 0253's slice and state of the world.
- Red first: added the new `ElementMenuTests` fact, built (exit 0), ran it alone against the old host. It failed on
  the offer, naming the bug (`.artifacts/0254-dev-red-new-fact.log`):
  `Expected: [CaptureTransmitted, CaptureReflected, CaptureBoth]` / `Actual: [CaptureTransmitted]`. Its setup
  assertions (the detector on the second plate's reflected ray; light leaving plate 1 by T, plate 2 by R) passed.
- Domain: `detectedBranches` takes `objects` and answers the distinct branches of `detectorCaptures`, in order. It
  moved below `detectorCaptures` (F# declaration order).
- Host: `measuredElementOf` deleted; `objectIds` and `detectorIds` added; `captureDetectors` and `availableCaptures`
  re-pointed; the no-sample T filter deleted.
- Tests: the two `BeamLayoutTests` facts pass `[ Library.ElementId "s" ]`, assertions unchanged.
- AGENTS.md: one clause added to the "An experiment runs on the light's real path" gotcha.
- First build after the change: FS0491 on `member private` (see Decisions); `internal` fixed it. Build: exit 0,
  0 warnings, 0 errors.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/BeamLayout.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/BeamLayoutTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ElementMenuTests.fs`
- `AGENTS.md`

## Decisions

- **`member internal`, not `member private`, for `objectIds`.** The slice names `member private model.objectIds`,
  and `availableCaptures` is a module-level `let` that must read it. F# refuses a private member of an intrinsic
  augmentation outside its type (FS0491). `internal` is this file's established accessibility for helper members
  that module functions read (`withViewRotationsReset`, `withSelectedRemoved`, `wavelengthRange`).
- **`member internal model.detectorIds` beside it.** `captureDetectors` and `availableCaptures` both need "the
  scene's `Detector` elements". One member serves both, so the list is not copied (AGENTS.md: "Reuse, never
  copy").
- `captureDetectors` maps each capture's detector to its `SceneElementIndex` through the existing
  `model.indexedElements`.
- `ElementMenuTests` opens `Berreman.Constants` and `Berreman.Fields` (for `nm`, `deg`, `WaveLength`,
  `IncidenceAngle`), placed before the `OpticalConstructor` opens as in `ExperimentControlsTests`.

## Testing state

Development runs over the final tree, each the gate's own command. No gate verdict is claimed (Invariant 6):
the arc-runner runs the frozen roster after exit.

- **build:** exit 0, 0 warnings, 0 errors.
- **constructor-unit-tests** (whole): `Passed!`, 1403 succeeded, 0 failed, 0 skipped, 68 s (deadline 180 s).
  That equals checkpoint 253's 1403: two facts changed their argument, none added.
- **ui-smoke** (whole): `Passed!`, 245 succeeded, 0 failed, 0 skipped, 202 s (deadline 600 s).
- **ui-tests** (the six selected classes): `Passed!`, 86 succeeded, 0 failed, 0 skipped, 106 s (deadline
  360 s). That is 85 plus the new fact. The TRX records
  `ElementMenuTests.a detector on the second sample's reflected ray offers R, decided by the last object on its
  path` as Passed.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 195s`, known findings 0 (deadline 600 s).
- **The fourteen s25 invariants**, by their greps: all find nothing. `git grep -n -F "measuredElementOf" --
  Berreman/` finds nothing (S8.2's invariant).
- **Extra check (not a gate):** the whole non-smoke, non-kit-acceptance UI suite passed 675 tests (0252 measured
  674, plus the new fact), 0 failed, 0 skipped, 200 s. The new no-object behaviour broke no unselected class.
- No development run exceeded its deadline.

## Artifacts

- `specs/0050/.artifacts/0254-dev-build-red.log`: the build with the new fact, before the fix.
- `specs/0050/.artifacts/0254-dev-red-new-fact.log`: the new fact, red against the old host.
- `specs/0050/.artifacts/0254-dev-build.log`: the final build.
- `specs/0050/.artifacts/0254-dev-constructor-unit-tests.log`, `0254-dev-ui-smoke.log`, `0254-dev-ui-tests.log`,
  `0254-dev-elevation.log`: the gate-command runs.
- `specs/0050/.artifacts/0254-dev-whole-ui.log` and `0254-dev-whole-ui/whole-ui.trx`: the whole UI run.

## Gotchas

- The host's capture changes only where a captured path holds no bound sample. Before, the path had to hold the
  first bound sample, else nothing was captured. Now such a detector captures T when the varied element transmits
  to it (step 253's no-object rule). Every capture whose path holds the one bound sample is unchanged, as the slice
  requires.
- A first build after MathNet's obj is stale prints its two pre-existing `SYSLIB0051` warnings (MathNet C#,
  vendored). The next build is clean.
