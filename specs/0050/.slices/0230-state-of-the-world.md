# Step 0230 — state of the world

## Where we are

Spec 0050, section S6 ("Send and receive kits; copy experiments between kits"), step 0230 (IMPLEMENT), obligation
`S6-ZIP-MESSAGES`.

- Steps 197–200 built the archive seam (`receiveKit`), the job host and the job state.
- This step makes Open kit from zip… a set of semantic messages of the start screen. A zip is extracted into an
  empty or missing folder through the context's job host, then opened exactly as Open kit opens a folder.
- Step 231 adds the button and its progress row, and step 232 the real zip dialog.

## What's working

- Added Open kit from zip's start-screen messages: `OpenKitZip`, `OpenKitZipProgressed`, `OpenKitZipEnded` and
  `CancelOpenKitZip`.
- One extraction runs through the context's job host, and a second waits. A finished extraction opens its folder
  through Open kit's own path, read-write, bumping the history.
- Added `KitZipOpening`. The start screen's context gained `zipPicker`, `archive` and `archiveJobs`, and its model
  `zipJob` and `zipOpeningOpt`.
- The start window composes the real archive and a UI-thread job host over its captured dispatch, with
  `KitZipPicker.none` until step 232.
- Added five `KitStartTests` facts: extract-and-open, a non-empty folder refused, a cancel that leaves no folder, a
  zip without a kit, and progress shown while a second open waits.

## Tests

Development runs over the final source, recorded in `0230-impl-log.md`. The gate engine gives the verdict
(Invariant 6).

- **build:** exit 0, 0 errors, and no lowercase `error` in the log.
- **constructor-unit-tests** (all discovered): **1353** passed, 0 failed, 0 skipped.
  - That is 1348 + the five new facts, each `Passed` in the TRX.
- **ui-tests** over `KitStartWindowTests`: **6** passed, 0 failed, 0 skipped.
- **ui-smoke** (all discovered): **239** passed, 0 failed, 0 skipped.
  - It took 195 s against the 600 s `attempt_timeout_seconds`; no deadline was exceeded.
- **elevation** (ELEV004 excluded, all 12 projects): `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 202 s.
  - 0 known findings, 1317 suppressed findings.
- **The fourteen `invariant-s22-*` gates:** each holds.
- **The facts bite.** Seven mutants of `updateKitStart` were each caught, and the source was restored byte for byte
  and rebuilt:
  - the running guard dropped;
  - a finished extraction opening nothing;
  - Cancel cancelling nothing;
  - progress ignored;
  - the end keeping the job;
  - an error untold;
  - the opening unrecorded.
- Logs are in `.artifacts/0230-dev-*.log` and `.artifacts/0230-mutants/`.

```yaml
gates:
  constructor_unit_tests: 1353
  ui_smoke_tests: 239
  ui_tests: 6
```

## Architecture

- **The update reaches IO only through the context.**
  - `OpenKitZip` hands `archive.receiveKit opening.zip opening.target` to `archiveJobs.startArchiveJob`. Progress and
    the end come back as messages, so the window and a test drive the same update.
  - A test runs the job at once (`KitArchiveJobHost.immediate`) or keeps it (a test-local deferred host).
- **One open path.** OpenKit's body is now the private `kitStartOpenExisting`, which a finished extraction also calls.
  The extracted kit gets the same activation check, the same `openExistingKitAt` and the same reading of every answer
  (`kitStartOpened`): an offer, a status, or `showKit` with a history bump.
- **The archive decides what is refused.** A non-empty folder (`KitZipTargetNotEmptyErr`) and a zip without
  `.kit-json` (`KitZipHoldsNoKitErr`) are step 197–198 errors, told as `The zip could not be opened — <describe>`.
  The screen adds no retry, overwrite or merge.

## Deferred

Nothing in this step's scope is deferred. The start screen's button and progress row are step 231's, and the real
zip dialog is step 232's.

## Gotchas

- **The window's job-host callbacks must stay lambdas.** `OpenKitZipProgressed >> dispatch` would bind the
  mutable `dispatch` field's initial `ignore` when the context is built.
- **`KitArchiveJobState.cancel` acts only while the job runs.** A Cancel sent after the end message is a no-op.
- **A finished end without `zipOpeningOpt` opens nothing.** It can only be a stale message.
- **`KitUnzipJob` shares the `zip` / `target` labels.** Annotate `KitZipOpening` record expressions.
- **The real extraction creates its target before the first file.** A cancel removes the outermost folder it
  created, so the target's missing parents go too.
- **Under Git Bash, `grep -c $'\r'` miscounts CRs.** Count bytes instead: the four edited files are pure LF.

## Changelog

- 2026-11-09 — Step 0230:
  - added Open kit from zip's start-screen messages, model fields and context seams, with the window composing the
    real archive and UI-thread job host;
  - made OpenKit's open path one shared private function;
  - added five `KitStartTests` facts; constructor-unit-tests 1348 → 1353.
