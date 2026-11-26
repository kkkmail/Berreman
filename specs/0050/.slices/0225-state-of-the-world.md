# Step 0225 — state of the world

## Where we are

Spec 0050, section S6 ("Send and receive kits; copy experiments between kits"), step 0225 (IMPLEMENT).

- This step puts step 224's `KitContext.copyExperimentsFrom` behind semantic messages of the kit window: Copy
  experiments from another kit…, model and update only.
- Step 226 adds the Kit-menu verb and the panel that sends these messages.
- Steps 227–229 extend the copy with item transfer and explicit mapping before S6's closing composition.

## What's working

- Added `KitWindowShell.kitPicker`, the folder dialog that chooses the kit to copy from. The inert shell answers
  `None`.
- Added `KitCopySource` (source opened read-only, its experiments, the chosen ids in source order) and
  `TableAndElementRotationModel.kitCopyOpt`.
- Added the four messages, `OpenCopySource`, `ToggleCopiedExperiment`, `CopyChosenExperiments` and
  `CloseCopySource`.
- A read-only kit is refused before any open or write, and a kit cannot copy from itself. A folder that is no kit
  is told in the kit status.
- A copy shows the copied experiments through the shared kit-content update, keeping the loaded items and scenes.
  It closes the source and tells `Copied <n> experiment(s) from <kit>.`
- Added five `KitWindowModelTests` facts; constructor-unit-tests went from 1331 to 1336.

## Tests

These are development runs over the final source, recorded in `0225-impl-log.md`. The gate engine gives the
verdict (Invariant 6).

- **build:** `dotnet build Berreman.slnx -c Release` exits 0 with 0 warnings and 0 errors, and the log contains no
  lowercase `error`.
- **constructor-unit-tests** (all discovered): **1336** passed, 0 failed, 0 skipped.
  - That is 1331 + 5, the five declared identities.
  - Each is `Passed` in the TRX.
- **ui-smoke** (all discovered): **237** passed, 0 failed, 0 skipped. It took 192 s against the 600 s
  `attempt_timeout_seconds`; no deadline was exceeded.
- **ui-tests** over `OpticalConstructor.Ui.Tests.KitWindowTests`: **8** passed, 0 failed, 0 skipped.
- **elevation** (from the arc origin `f395597`, ELEV004 excluded):
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 183 s against its 600 s estimate;
  - 1312 suppressed findings, unchanged from step 224, and 0 known findings.
  - The run was made before a one-line comment rewording. That edit changes no code, and every other gate was
    re-run after it.
- **The fourteen `invariant-s22-*` gates:** each holds; their greps find nothing, also with `--untracked`.
- **The facts bite.** Six mutants were each caught by the fact meant to guard them, then the source was restored:
  - `[]` passed for the items;
  - the self check made without `canonical`;
  - no guard on Copy;
  - a toggle that only adds;
  - the source opened read-write;
  - the source left open after the copy.

```yaml
gates:
  constructor_unit_tests: 1336
  ui_smoke_tests: 237
  ui_tests: 8
```

## Architecture

- **The copy lives in the pure update.** It is a model field and four messages, so the panel (step 226) and a test
  send the same messages.
  - The source is a `KitContext` opened read-only through `KitProxy.create`. A read-only open takes no lock and
    writes nothing, not even `.oc`.
  - The model holds the source until Copy or Cancel. Another `OpenCopySource` closes it first.
- **The write rules.**
  - `OpenCopySource` and `CopyChosenExperiments` run behind the existing private `guardKitWrite` (step 185).
  - Toggling and Cancel write nothing.
  - The copy's files are written through the window's own proxy, which stamps them. `changedKitFiles` therefore
    offers none of them; the second fact checks this.
- **The success path reuses the content update.** It goes through `withKitContent`, the content function shared by
  `loadKit` and Reload (step 187), over `model.kit`, the re-read experiments, `model.kitItems` (step 223) and
  `model.savedScenes`.
- **The closes are members, not free functions** (ELEV013). `KitCopySource.release ()` closes the source and logs a
  failed close. `model.closeCopySource ()` closes and forgets it.

## Deferred

Nothing in this step's scope is deferred. The verb and panel (step 226) and the item transfer and explicit mapping
(steps 227–229) are their own steps, named by this step's brief.

## Gotchas

- A folder kit's read-write lock leaves an empty `.oc` folder after close. The test fixture removes it from the
  source, so the first fact can show that a read-only open creates none.
- `withKitContent` clears `kitStatus` and `kitChanges`. Set the copy's status after it, never before.
- `Program.fs` builds the app's shell as `KitWindowShell.inert with …`, so the real window keeps the inert kit
  picker until a later step wires one, as with `zipPicker` today. `OpticalConstructor.App` was not in this step's
  authorized edits.
- `KitWindowTests.fs` was authorized but needed no change: every shell there is a record update of
  `KitWindowShell.inert`.

## Changelog

- 2026-11-09 — Step 0225: Copy experiments from another kit… as kit-window messages:
  - added `KitWindowShell.kitPicker`, `KitCopySource`, `kitCopyOpt` and `OpenCopySource` /
    `ToggleCopiedExperiment` / `CopyChosenExperiments` / `CloseCopySource`, guarded for read-only kits and the
    kit's own folder;
  - added five `KitWindowModelTests` facts; constructor-unit-tests went from 1331 to 1336.
