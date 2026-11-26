# Step 0232 — impl log

## Progress

- **Reading.** I read:
  - the system prompts (base and `WIRE_UI`), the project prompt and AGENTS.md;
  - the slice and its gates snapshot;
  - manual 015 (the ui-smoke 600 s deadline);
  - slices 0230 and 0231, with 0231's state of the world.

  No operator note is in flight. Checkpoint 230, the floor for this step, is constructor 1353 and ui-smoke 240.
- **Seams read.** `KitStartWindow.fs` (step 230 composes `zipPicker = KitZipPicker.none`) and
  `KitZipPicker.ofWindow` (`KitArchiveJob.fs`, step 200). I also read the `LauncherWindow` in `Program.fs`, which
  holds `locations : DataLocations` (`exportsFolder`, step 81), and step 230's zip fixture in `KitStartTests`.
- **KitStartWindow.**
  - The constructor is now `KitStartWindow(context, mode, pickerFor, zipPickerFor : TopLevel -> KitZipPicker,
    showConstructor)`, with the new argument after `pickerFor`, mirroring it.
  - The composed context's `zipPicker` is `zipPickerFor this`, in place of `KitZipPicker.none`.
  - The type doc gains one line: its folder and zip dialogs are `pickerFor` and `zipPickerFor` over this window
    (spec 0050 S6).
- **LauncherWindow (Program.fs).**
  - One local `zipPickerFor (owner : TopLevel) : KitZipPicker = KitZipPicker.ofWindow owner locations.exportsFolder`,
    beside `launchers`. Both start screens (Main, Inverse) are built with it, so the dialog starts where Send kit…
    writes.
  - The launcher's own signature is unchanged. Its doc gains one sentence citing spec 0050 S6.
  - `Program.fs` opens `OpticalConstructor.Domain.KitArchive` (the type) and `OpticalConstructor.Ui.KitArchiveJob`
    (the `ofWindow` extension).
- **Other constructions.** Each passes a zip picker that answers nothing, with its assertions unchanged:
  - `ScrollExtentTests`' `the kit start screen leaves everything reachable`;
  - `WireUiFinalCompositionTests`' launcher fact.

  Both pass `fun _ -> KitArchive.KitZipPicker.none`, qualified as `ScrollExtentTests` already qualifies `KitArchive.*`.
  The two `KitStartWindowTests` window facts pass `fun _ -> KitZipPicker.none`.
- **The new fact.** `KitStartWindowTests`, trait `ui-smoke`:
  `acceptance: Open kit from zip extracts a sent kit and opens it in the constructor of the screen's mode`.
  - **New helpers:**
    - `sentExperiment` (`E1`);
    - a private `SentKitZip` record (`sentKitFile`, `sentZip`);
    - `sentKitZipUnder root imports`. It makes `Optics QZ` under the fact's root through `openKitAt`, writes `E1` and
      closes the kit, failing on either error. It then sends the kit through `KitArchiveProxy.create ().sendKit` into
      `<root>/exports/Optics QZ.zip`.
  - **The window** is built over `windowScope ()`: `TestAppScope.freshAppContext (FolderName "KitStartWindowTests")`
    with a fresh in-memory history, in `ForwardConstructor`.
    - Its picker answers `<root>/received/k`, which does not exist.
    - Its zip picker answers the sent zip.
    - Its `showConstructor` records the mode in `modes` and the kit in `withKitStartRoot`'s `shown`, so the fixture
      closes the kit before it deletes the root.
  - **The run.** The fact presses `openKitZipButton` at its centre, which starts the real `onUiThread` extraction on a
    worker. It then runs `Dispatcher.UIThread.RunJobs ()` until `shown` holds a kit or a `Stopwatch` passes ten
    seconds. There is no sleep.
  - **Asserts:**
    - `modes` is `[ ForwardConstructor ]`;
    - one kit, at the received folder, read-write;
    - its kit file equals the sent one, and its experiments are `[ E1 ]`;
    - every entry of the zip (read with `ZipFile.OpenRead`, `.kit-json` among them) exists in the received folder,
      a folder entry as a folder;
    - the screen closed once and is not visible;
    - the history's entries' folders are exactly `[ received ]`.
  - Opens added: `System.IO.Compression`, `OpticalConstructor.Domain` (for `Experiments.*`),
    `OpticalConstructor.Storage.KitArchiveStore` (`KitArchiveProxy.create`) and
    `OpticalConstructor.Storage.KitContentFiles` (`writeExperiment`). The first build failed with FS0039 on
    `writeExperiment` until the last open was added.
- **LauncherTests:** no edit was needed. The launcher's signature is unchanged, its start screens are now built with
  the real dialog, and no fact presses it. Every LauncherTests fact passes with its assertions unchanged.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitStartWindow.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitStartWindowTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/WireUiFinalCompositionTests.fs`

`LauncherTests.fs` is in the step's effects but unchanged (see above).

## Testing state

These are development runs over the final source. The gate engine gives the verdict (Invariant 6).

- **build:** `dotnet build Berreman.slnx -c Release -nologo -v:m` exit 0, 0 errors.
  - The first full build printed only the two vendored `SYSLIB0051` warnings
    (`MathNetNumerics/Numerics/Exceptions.cs`), as steps 0230 and 0231 recorded. None comes from our code.
  - The final build: **0 warnings, 0 errors** (`0232-dev-build-final.log`).
- **ui-tests** over `KitStartWindowTests`, `LauncherTests` and `ScrollExtentTests`: **28** passed, 0 failed,
  0 skipped: KitStartWindowTests 8, LauncherTests 8 and ScrollExtentTests 12.
  `the kit start screen leaves everything reachable` and both start-window facts are `Passed` in the TRX.
- **ui-smoke** (all discovered): **241** passed, 0 failed, 0 skipped. That is checkpoint 230's 240 plus the new fact,
  which is `Passed` in the TRX.
  - It took 199 s against the 600 s `attempt_timeout_seconds`; no deadline was exceeded.
  - The new fact alone took 7 s.
- **constructor-unit-tests** (all discovered): **1353** passed, 0 failed, 0 skipped. That is unchanged, as this step
  adds none.
- **elevation:** run with `ASG_ARC_BASE_COMMIT=f395597…` (`.arc-base-json`), ELEV004 excluded:
  `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 202s`, with 0 known findings and 1317 suppressed findings.
- **The fourteen `invariant-s22-*` greps:** each finds nothing.
- **LF:** the five edited files and this step's markdown hold no CR byte and no BOM (counted in bytes).
  `git ls-files --eol` shows `w/lf`, and `git diff --numstat` equals `--ignore-cr-at-eol`.
- **The fact bites.** Mutant `no-zip-dialog`: `KitStartWindow` composes `KitZipPicker.none` again.
  - After a rebuild, the new fact failed after its ten-second bound: `Assert.Equal() Failure: Collections differ`.
    The recorder held no mode.
  - The source was then restored byte for byte (sha256 check `OK`) and the solution rebuilt. The restored
    `Softellect.Berreman.OpticalConstructor.Ui.dll` is identical (sha256 `6a760031…`) in the Ui, App and Ui.Tests
    outputs. Every run above is over the restored build.

## Artifacts

- `specs/0050/.artifacts/0232-dev-build.log`, `0232-dev-build-final.log`
- `specs/0050/.artifacts/0232-dev-new-fact.log`
- `specs/0050/.artifacts/0232-dev-ui-tests.log`, `ui-tests-0232-dev.trx`
- `specs/0050/.artifacts/0232-dev-ui-smoke.log`, `ui-smoke-0232-dev.trx`
- `specs/0050/.artifacts/0232-dev-constructor-unit-tests.log`, `constructor-unit-tests-0232-dev.trx`
- `specs/0050/.artifacts/0232-dev-elevation.log`
- `specs/0050/.artifacts/0232-mutants/` (the mutant's build and test logs, and the original's sha256)

## Gotchas

- **`writeExperiment` is an extension in `OpticalConstructor.Storage.KitContentFiles`.** A test that writes an
  experiment through `kit.proxy` must open that module.
- **The sent-zip fixture is a local helper, not a reuse.** `KitStartTests.sentKitZipUnder` (step 230) is private to
  `OpticalConstructor.Tests`, and making it public would edit a file outside this step's effects. This file already
  keeps its own copy of `KitStartTests`' history fixture for the same reason (its step 176 comment). The local helper
  is shorter: it returns only the kit file and the zip this fact needs.
- **A zip holds folder entries** (`KitTreeFolder` → `"<path>/"`). "Every sent file" is checked entry by entry, a
  folder entry as a folder.
- **The wait is bounded, not slept.** A missing kit fails after ten seconds on the mode assertion, so a broken wiring
  costs ten seconds of smoke time, never a hang.
