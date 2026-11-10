# Step 0230 — impl log

## Progress

- Read the protocol, the project prompt, AGENTS.md, the slice, its gates and manuals 013 and 015. The operator-note
  section is empty.
- Surveyed the code this step builds on:
  - steps 197–200: `KitArchive`, `KitArchiveStore`, `KitArchiveJob`;
  - the kit window's Send kit messages, the pattern followed here;
  - steps 231–232, out of scope: the button, the progress row and the real dialog.
- Implemented the context fields, `KitZipOpening`, the model fields, the four messages and their update.
- Composed the window's seams and updated both test contexts.
- Added the five `KitStartTests` facts.
- Built, ran the gates' commands, ran seven mutants, then rebuilt and re-ran over the restored final source.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitStartView.fs`
  - `KitStartContext` gained `zipPicker : KitZipPicker`, `archive : KitArchiveProxy` and
    `archiveJobs : KitArchiveJobHost`, each with a one-line doc.
  - `KitZipOpening = { zip : FileName; target : KitFolder }` is declared before the model.
  - `KitStartModel` gained `zipJob : KitArchiveJobState` and `zipOpeningOpt : KitZipOpening option`.
    `initKitStart` sets `KitArchiveJobState.idle` and `None`.
  - `KitStartMsg` gained `OpenKitZip of KitZipOpening`, `OpenKitZipProgressed of JobRunner.JobProgress`,
    `OpenKitZipEnded of Result<KitArchiveOutcome, KitArchiveError>` and `CancelOpenKitZip`.
  - `updateKitStart` handles the four messages as the brief says.
  - A new private `kitStartOpenExisting` is OpenKit's one path; `OpenKit` and a finished extraction both call it.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitStartWindow.fs`: the composed context gained:
  - `zipPicker = KitZipPicker.none`;
  - `archive = KitArchiveProxy.create ()`;
  - `archiveJobs = KitArchiveJobHost.onUiThread (fun progress -> dispatch (OpenKitZipProgressed progress)) (fun result -> dispatch (OpenKitZipEnded result))`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitStartTests.fs`
  - `contextOver` holds `KitZipPicker.none`, `KitArchiveProxy.createInMemory (InMemoryArchiveDisk.create ())` and
    `KitArchiveJobHost.inert`.
  - New helpers:
    - `sentExperiment`;
    - `KitStartSentZip` and `sentKitZipUnder`: `openKitAt`, then `writeExperiment` of E1, close, then
      `KitArchiveProxy.create ().sendKit` into `<fact>/exports/Optics QZ.zip`;
    - `receivedKitIn` (`<fact>/received/k`);
    - `extractingContext` (the real archive with `KitArchiveJobHost.immediate` over recorders);
    - `deferredHost` and `passedThrough`.
  - Five new facts, exactly the five ids of the census.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitStartWindowTests.fs`: `contextOver` gained the same
  three seams and their opens. No fact changed.

## Decisions

- **OpenKit's path is shared, not copied.**
  - The brief says a finished extraction "answers exactly as `OpenKit opening.target` does": `openExistingKitAt`,
    then step 177's `kitStartOpened`.
  - `OpenKit`'s body (`kitStartChosen folder (fun () -> openExistingKitAt …)`) was moved into one private
    `kitStartOpenExisting`, and both `OpenKit` and `OpenKitZipEnded (Ok (KitArchiveFinished _))` call it.
  - So the extracted folder also gets OpenKit's window-activation check, which is inert for a folder that was just
    extracted. `OpenKit`'s behaviour is unchanged, and its existing facts still pass.
- **A finished end with no opening answers the idle model and opens nothing.** That is
  `OpenKitZipEnded (Ok (KitArchiveFinished _))` with `zipOpeningOpt = None`. The brief opens a folder only "with
  `Some opening`", and no status is invented for what could only be a stale message.
- **The window's callbacks are lambdas, not `>>`.** `OpenKitZipProgressed >> dispatch` would capture the field's
  initial `ignore` when the context is built, before the init Cmd sets `dispatch`. The comment there says so in one
  line.
- **The waiting fact also shows the progress.** Its name says it shows its progress, so:
  - it runs the kept job over a recording observer;
  - it passes each `OpenKitZipProgressed` through the update and checks the `n / total` text;
  - it then ends the job, and the FIRST opening's folder is the kit opened, while the second's was never made.
- **The facts use only the census's five ids.** No test identity was added or renamed.

## Testing state

Development runs. The gate engine gives the verdict (Invariant 6). The final runs are over the restored final source,
rebuilt after the mutants.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`):
  - exit 0, 0 errors, and no lowercase `error` in either log;
  - the first full build had two SYSLIB0051 warnings in vendored MathNet's `Exceptions.cs`, which manual 013 records
    as existing; the final incremental build had 0 warnings.
- **constructor-unit-tests** (all discovered, the gate command with TRX):
  - **1353** passed, 0 failed, 0 skipped: 1348 + 5, against the 1348 floor of checkpoint 228;
  - all five new facts are `Passed` in the TRX, and every one of the 1353 results is `Passed`.
- **ui-tests** over `KitStartWindowTests`: **6** passed, 0 failed, 0 skipped.
- **ui-smoke** (all discovered): **239** passed, 0 failed, 0 skipped.
  - It took 211 s alongside the elevation run and 195 s alone (final), against the 600 s `attempt_timeout_seconds`.
  - No deadline was exceeded.
- **elevation** (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`, standalone, so all 12 projects):
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 202 s;
  - 0 known findings;
  - 1317 suppressed findings (the excluded ELEV004 hits, the same as step 229).
- **The fourteen `invariant-s22-*` greps:** each finds nothing.
- **The facts bite.** Seven mutants of `updateKitStart` were each caught, and the source was restored byte for byte
  (SHA-256 `233f1e4b…` before and after):

  | Mutant | Caught by |
  |---|---|
  | m1, no running guard | the waiting fact |
  | m2, a finished extraction opens nothing | the extract fact and the waiting fact |
  | m3, Cancel cancels nothing | the cancel fact |
  | m4, progress ignored | the waiting fact |
  | m5, the end keeps the job | all five facts |
  | m6, an error is untold | the not-empty fact and the no-kit fact |
  | m7, the opening is not recorded | the extract fact and the waiting fact |

## Artifacts

All under `specs/0050/.artifacts/`:

- `0230-dev-build.log`, `0230-dev-build-final.log`
- `0230-dev-kitstarttests.log`
- `0230-dev-constructor-unit-tests.log`, `0230-dev-constructor-unit-tests-final.log`
- `0230-dev-ui-tests.log`, `0230-dev-ui-tests-final.log`
- `0230-dev-ui-smoke.log`, `0230-dev-ui-smoke-final.log`
- `0230-dev-elevation.log`
- `0230-mutants/`:
  - `run_mutants.py`, `KitStartView.fs.orig`;
  - per-mutant test logs;
  - `summary-run1.txt` (m2, m4, m6, m7; the three multi-line patterns were first written CRLF and skipped);
  - `summary-run2.txt` (m1, m3, m5 with LF patterns).

## Gotchas

- **The sources are LF.** `grep -c $'\r'` under Git Bash miscounts (it reported every line). A byte count in Python
  shows 0 CRLF in all four edited files. `git diff --numstat` equals `--ignore-cr-at-eol`, so there is no
  line-ending churn.
- **`KitUnzipJob` also has `zip` and `target` labels**, so every `KitZipOpening` record expression in the tests
  carries its type annotation.
- **The real `extractKitZip` creates the target before its first file and removes the outermost folder it made on a
  cancel.** So the cancel fact also checks that `<fact>/received` is gone.
- **`KitArchiveJobState.cancel` cancels only while the job's status is `Running`.** The cancel fact therefore sends
  `CancelOpenKitZip` to the started model, never to the ended one.
