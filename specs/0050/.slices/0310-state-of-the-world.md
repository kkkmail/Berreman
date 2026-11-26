# Step 0310 — state of the world

## Where we are

Spec 0050 section S9.2 covers import profiles and the column-mapping wizard (001 §K).

- Step 308 gave the wizard its pure MVU, and step 309 gave it its view.
- This step has the kit window attach a file through the wizard, by semantic messages:
  - Attach data file… opens the wizard through the shell;
  - the wizard's Attach comes back as the window's own messages: save the new profile, then attach by it.
- Next:
  - step 311 adds `ImportWizardWindow` and sets the App's shell to open it (`KitWindowShell.openImportWizard`);
  - step 321 makes the wizard's target read attach or split.

## What's working

- The kit window's Attach data file… now opens the column-mapping wizard (spec 0050 S9.2). It sends
  `OpenImportWizard`, which hands the shell the picked file, the window's `readPickedFile`, the kit's profiles and the
  detector's built-in default.
- `SaveImportProfile` writes the wizard's new mapping into `.kit-json`. It refuses a held or built-in name, and a
  refused name writes nothing.
- `AttachDataFile (id, file, profile)` replaces `AttachDataFileTo`: the file is mapped by the profile the wizard chose.
- `ImportWizardApplied.messages` turns the wizard's Attach into the window's messages, saving a new profile before
  the file is mapped by it.
- A read-only kit refuses all three messages and opens nothing.
- Add three `KitWindowModelTests` facts, and extend the read-only fact. `InverseConstructorTests` dispatches
  `AttachDataFile`, and its ellipsometric fact is renamed.

## Tests

These were development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the gates
themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, `0 Error(s)`; no `error` in stdout; the only warnings are the two pre-existing vendored MathNet SYSLIB0051 (operator note at 0185) | 78 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded (suppressed 1426, unchanged from step 309) | 213 s | 600 s |
| constructor-unit-tests | Passed!, 1525 of 1525, 0 failed, 0 skipped | 69 s | 180 s |
| ui-smoke | Passed!, 251 of 251, 0 failed, 0 skipped | 219 s | 600 s |
| ui-tests (selected: `DataFilePickerFolderTests`, `InverseConstructorTests`, `KitWindowTests`) | Passed!, 37 of 37, 0 failed, 0 skipped | 29 s | 360 s |
| invariant-s30-\* (16) | each exit 0, `INVARIANT HOLDS` | under 1 s each | 300 s |

- **constructor-unit-tests: 1525**, which is 1522 + 3, as the slice says. All four slice identities are `Passed` in the
  TRX:
  - `Save import profile writes the wizard's new mapping into the kit file and refuses a name the kit holds`
  - `opening the import wizard hands it the picked file, the kit's profiles and the detector's default`
  - `the wizard's Attach saves its new mapping first and attaches the file by it`
  - the changed `a kit opened read-only refuses every write message and writes nothing`
- **ui-tests: 37** over the three selected classes, with the count unchanged (one rename, no new fact). These are
  `Passed`:
  - the renamed `the profile the wizard chose selects the ellipsometric read`;
  - the four changed attach facts.
- **ui-smoke: 251**, equal to step 309's checkpoint. The smoke fact in `InverseConstructorTests` dispatches
  `AttachDataFile` now and passes.
- **Elevation** analyzed all 12 product projects (a standalone run with no comparison base).
- No run went over its effective deadline.
- `git grep -n -F "AttachDataFileTo" -- Berreman/` finds nothing (exit 1).
- **All three edited files are LF only**, with 0 CR bytes, and `git diff --numstat` equals the `--ignore-cr-at-eol`
  count.
- **Red by construction.** `AttachDataFile`, `OpenImportWizard`, `SaveImportProfile`, `messages` and
  `openImportWizard` are new, so every new or changed fact fails to compile against step 309's tree.

```yaml
gates:
  constructor_unit_tests: 1525
  ui_smoke_tests: 251
  ui_tests: 37
```

## Architecture

- **The wizard's window is reached through the shell**, the functional-proxy seam the Kit menu already uses (explorer,
  zip and kit pickers). The update hands over a pure request and answers the model unchanged, so it stays testable
  without a window: a test's shell records the request.
- **The wizard's Attach returns as data.** `ImportWizardApplied.messages` is a list of the window's own messages, so
  the button path and the tests drive the same `update`. The save comes first, so `readMapped` finds the new profile
  in the kit file when the attach validates.
- **`SaveImportProfile` has `SaveNamingTemplate`'s shape:**
  - the name rule is `KitFile.withImportProfile` (step 299);
  - the write is `KitContext.saveKitFile`, whose recorded stamps keep the window's own write from being offered for
    reload (step 130);
  - its statuses are `Not saved — …` and `Not saved to the kit — …`.
- **`AttachDataFile` no longer looks up the experiment's slots**, which it used only to choose the built-in profile.
  That lookup moved to `OpenImportWizard`, where it picks the wizard's default.
- No stored format, schema, version or package changed.

## Deferred

Nothing. Every requirement of the slice landed this round. These are later steps' slices, not deferrals:

- the wizard's window and the App shell's real `openImportWizard` (step 311);
- the split target (step 321).

## Gotchas

- **In the running app, Attach data file… opens nothing until step 311.** The App's shell is
  `{ KitWindowShell.inert with … }` (`OpticalConstructor.App/Program.fs`), so its `openImportWizard` is `ignore` until
  step 311 sets it. The slice keeps every shell built as an update of `inert` unchanged.
- **The `Lab scan` fixture is `KitTests.labScanProfile`, reused rather than copied** (AGENTS.md "reuse, never copy").
  That public fixture is exactly the slice's `Lab scan`. `Phone ROI by columns` and its bytes are test-local, because
  the existing copies in `ImportWizardTests` and `LabFileImportTests` are private.
- **`withWizardKit` builds its scene with `initInverse`** over fresh Built-in stores, then `loadKit`s a read-write folder
  kit. It commits `E1` through the window, the `filledExperiments` recipe. It asserts `E1` is one R1 axis over
  `VariableRange.forVariable VaryR1` (0…360°), so the phone file's 0° and 45° validate.
- **Extensions with no new identity:**
  - fact 1 also refuses a built-in name, checks that the own write is not offered for reload, and checks that each
    refusal changes only `kitStatus`;
  - fact 2 also checks that the request reads the picked bytes and that an unknown experiment opens nothing;
  - fact 3 also checks `messages` with no saved profile;
  - the renamed ellipsometric fact also asserts the recorded profile.
- **The build's two warnings are not this step's.** They are vendored MathNet SYSLIB0051 (`MathNetNumerics/Numerics/
  Exceptions.cs`), which the operator recorded as pre-existing at step 0185.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0310: the kit window attaches through the column-mapping wizard (S9.2).
  - `KitWindowShell.openImportWizard`.
  - `OpenImportWizard`, `SaveImportProfile` and `AttachDataFile`, which replaces `AttachDataFileTo`.
  - `ImportWizardApplied.messages`.
  - Three new `KitWindowModelTests` facts, and the read-only fact extended.
  - `InverseConstructorTests` moved to `AttachDataFile`, and its ellipsometric fact renamed.
