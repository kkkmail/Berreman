# Implementation log — 0419

## Progress

- Read the task, worker base and WIRE_UI delta, project prompt, repository and
  naming instructions, applicable manuals 010/013/015, slice and gate snapshot, and
  immutable S115-WIRE acceptance obligation.
- Inspected the shared window mount, real fit host, study views and existing
  KitWindowTests/TestAppScope helpers. Both windows use the same mount; its shell
  currently retains the inert study-fit host.
- Before implementation: add only the authorized smoke identity using the real
  shown window, then wire the existing host and add the specified documentation.
- Added the authorized smoke fact before changing production composition. It
  creates Empty through the shown Inverse window, reads the kit's study files,
  runs the Fit action and pumps dispatcher jobs for at most ten seconds until
  error text appears. It checks the no-stage-run explanation and study name,
  visible error, retained study file and absence of fit-run files.
- Before production wiring: connect both fit callbacks through hostDispatch,
  extend both window contracts and insert the one six-line AGENTS gotcha.
- Connected `studyFits` to `StudyFitJobHost.onUiThread` in the shared mount.
  Progress and completion wrap their existing fit messages in `StudyFit` and
  use the same captured dispatch as the window's other asynchronous work.
- Added one short contract line on each constructor window and exactly one
  six-line gotcha after the import-profile bullet. Other shell seams and
  existing test consumers/assertions remain unchanged.
- Reviewed the source diff against S115-WIRE. Ordinary and ignore-CR-at-EOL
  numstats agree: AGENTS +6, Program +7, KitWindowTests +43, with no deletions.
  This was source inspection only, not a gate result.
- Wrote the state-of-the-world at the exact assigned path. Implementation is
  complete; runtime acceptance remains the supervisor's responsibility.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`: real fit host
  composition and concise contracts on both constructor windows.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`:
  the one authorized real-window study-fit smoke fact.
- `AGENTS.md`: the required six-line study-flow gotcha only.
- Worker documents: the task's exact 0419 impl-plan, impl-log and state-of-world paths.

## Testing state

No checks or gates run. WIRE_UI Invariant 6 assigns execution and verdicts to the
supervisor after worker exit. Expected census effect: one new ui-smoke identity;
no constructor or selected non-smoke test identity changes. Verification pending.

The supervisor must run the unchanged 0419 roster: build and arc-origin elevation
(600 seconds each), constructor-unit-tests (180 seconds), ui-smoke (600 seconds),
selected ui-tests (360 seconds), and all twenty s40 invariants (300 seconds each).
The selected non-smoke classes are AppContextTests, KitWindowTests, LauncherTests
and TableAndElementRotationTests. No development-run duration or deadline overrun
exists to report. `commit_ready=false` reflects pending verification only.

## Architecture

Reuse `StudyFitJobHost.onUiThread` and the mount's captured dispatch. No new job
runner, storage path, UI seam, schema or migration.

## Gotchas

- Preserve the pre-existing `specs/0050/.manifest.state.json` change.
- No gate/census/checkpoint changes, version bump, Git mutation or external-repo edits.
- No scope deviation is needed. Existing test helpers already resolve stable IDs,
  bring controls into view and press their centres.
- `FitStageErr.describe` wraps `NoStageRunErr.describe` with a prefix, and the
  latter appends its diagnostic payload. The test asserts the specified phrase
  and Empty's name without changing either existing renderer.
- The other authorized consumer files need no edits because no signature changed.

## Artifacts

No runtime artifacts produced. Any persistent runtime output belongs only under
`C:/GitHub/Berreman/specs/0050/.artifacts`.
