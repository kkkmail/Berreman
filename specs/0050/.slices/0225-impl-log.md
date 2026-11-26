# Step 0225 — impl log

## Progress

- [x] Read:
  - the protocol and the project prompt;
  - the slice and its gates snapshot;
  - AGENTS.md;
  - the step 224 SoW;
  - manuals 013 and 015.
- [x] View, `OpticalConstructor.Ui/TableAndElementRotationView.fs`:
  - `KitWindowShell.kitPicker`;
  - `KitCopySource`, with its `toggled` and `release` members;
  - `kitCopyOpt`;
  - the four messages and their arms;
  - the `closeCopySource ()` model member.
- [x] Tests: five `KitWindowModelTests` facts, all green (KitWindowModelTests 22 → 27).
- [x] Mutation checks: six mutants, each caught by its fact, then the source restored.
- [x] Final build, constructor-unit-tests, ui-smoke, ui-tests (KitWindowTests), elevation and the fourteen invariants
  (development runs; results below).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`:
  - **`open OpticalConstructor.Storage.KitFolderStore`**, for `KitProxy.create`.
  - **`KitWindowShell.kitPicker : KitFolderPicker`.** `inert` answers `None` at once.
  - **`KitCopySource`,** declared before the model:
    - `toggled`: chooses or unchooses, keeping the source's order. An id the source does not hold is never chosen.
    - `release ()`: closes the source and logs a failed close.
  - **`kitCopyOpt : KitCopySource option`** after `kitSend`; `initWith` sets `None`.
  - **`closeCopySource ()`,** a model member placed after `withKitCheckFailure`.
  - **`OpenCopySource`, `ToggleCopiedExperiment`, `CopyChosenExperiments` and `CloseCopySource`,** declared after
    `CancelSendKit`, with their arms after its arm.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`:
  - the `CopySourceKit` fixture and `copySourceUnder`;
  - the `darkLine`, `appCacheOf` and `experimentFilesIn` helpers;
  - the five declared facts.
- `KitWindowTests.fs` is authorized but unchanged. Every shell there is built `KitWindowShell.inert with …`, so the
  new field needs no edit, and this step declares no UI test.

## Decisions

- **Two private helpers are members instead of free functions.**
  - The source close is `KitCopySource.release ()`, and the model's close-and-forget is `model.closeCopySource ()`.
    A free function whose single parameter is one of our types belongs on the type (ELEV013).
  - The readExperiments-failure path calls `release ()` on the context it just opened, as the brief says ("an
    `Error e` closes what was opened").
- **The source's `.oc` is removed in the fixture.** The fixture makes the source with `openKitAt` read-write, as the
  brief asks. The read-write lock leaves an `.oc` folder behind after close: the lock file is deleted, the folder is
  not. To assert "the source folder holds no `.oc`", the fixture removes that app cache after closing the source.
  The fact then proves the read-only open recreates none, and that every byte is kept.
- **The content update.** The success arm calls `withKitContent model.kit experiments model.kitItems
  model.savedScenes model`. That is the private content function `loadKit` and Reload share (step 187), and it
  receives `model.kitItems` as the brief requires (step 223 → 227). The source is then closed and the status set:
  `withKitContent` clears `kitStatus`.
- **The guard comes first.** A read-only kit answers the read-only text for `OpenCopySource` and
  `CopyChosenExperiments` even with no source open: the guard runs first, as the brief orders.
- **The copy status's count** is the report's experiments, the ones actually written.

## Testing state

All runs are development runs over the final source; the gate engine gives the verdict.

- **build:** `dotnet build Berreman.slnx -c Release` (final, after the mutants were restored and the comment
  reworded): 0 warnings, 0 errors, no lowercase `error` in the log.
- **constructor-unit-tests:** 1336 passed, 0 failed, 0 skipped (1331 → 1336, +5). Each of the five new identities
  is `Passed` in the TRX.
- **ui-smoke:** 237 passed, 0 failed, 0 skipped, in 192 s against the 600 s deadline.
- **ui-tests** over `OpticalConstructor.Ui.Tests.KitWindowTests`: 8 passed, 0 failed, 0 skipped.
- **elevation:**
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 183 s;
  - 1312 suppressed findings, 0 known findings;
  - arc base `f395597`.
  - The run came before the one-line comment rewording at the copy arm, which renamed no code.
- **The fourteen `invariant-s22-*` greps:** every one holds, tracked and `--untracked`.
- **Line endings:** LF only in every file I touched. `git diff --numstat` matches `--ignore-cr-at-eol`.
- **Not mine:** `specs/0050/.manifest.state.json` was dirty before the round. It is the supervisor's, untouched.
- **Mutants.** Each failed exactly the fact meant to guard it (26 / 27 passed):
  - `[]` passed instead of `model.kitItems` → `copying the chosen experiments …`;
  - the self check made without `canonical` → `a kit cannot copy from itself …`;
  - `CopyChosenExperiments` without the guard → `a kit opened read-only neither …`;
  - toggling that only adds → `toggling an experiment twice …`;
  - the source opened read-write → `choosing a kit to copy from …`, which sees `.oc`;
  - the source left open after the copy → `copying the chosen experiments …`.

## Artifacts

All are in `specs/0050/.artifacts/`:

- `0225-dev-build.log` — the final solution build;
- `0225-dev-constructor-unit-tests.log`;
- `0225-dev-ui-smoke.log`;
- `0225-dev-ui-tests.log`;
- `0225-dev-elevation.log`;
- `0225-dev-invariants.log`.

## Gotchas

- A folder kit's read-write lock leaves an empty `.oc` behind after close. A fact that asserts "no `.oc`" must start
  from a folder without one.
- `Program.fs` composes the real shell as `KitWindowShell.inert with …`, so the app's window keeps the inert kit
  picker until a later step wires one. The send's `zipPicker` is in the same position today. App is outside this
  step's authorized edits, and step 226 adds the verb that uses the picker.
