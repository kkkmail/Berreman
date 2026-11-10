# Step 0260 — impl log

## Progress

- [x] Read the protocol, the project prompt, the slice, `.manual/006` (0260 case accounting), `.manual/014` (keep the
  storage-refusal distinction when the scene test class becomes `SetupSaveLoadTests`) and `.manual/016` (the
  `InverseConstructorTests` copied-item fixture, untouched here).
- [x] Domain: `Setups.fs` (`libraryReferences`, `SetupFile`, `setupFile`), `Scene.fs` reduced to `SnapMode`, fsproj comment.
- [x] Storage: schema, `validateSetupFile`, `KitContentFiles` setup reads, writes and record.
- [x] Ui: `KitOpening.OpenedKit.setups`, table model fields, messages, arms and view; ids. Product projects build;
  only the test projects fail, on the retired symbols.
- [x] Tests: two class renames, the migrations, and the new `SetupModelTests`.
- [x] Development runs of build, elevation, constructor-unit-tests, ui-smoke, the selected ui-tests, the fifteen
  invariants and the acceptance grep.

## Files modified

**Domain**
- `OpticalConstructor.Domain/Setups.fs`
  - `Setup.libraryReferences` is step 147's rule, moved: the current slots and then each history record's, in slot
    order, each once, as `EntryLatestReference`.
  - `heldItems` and `libraryReferences` share the new private `slotsOfEveryRevision`, so the revision walk is written
    once.
  - `SetupFile` (`schemaVersion`, `setup`), its `currentSchemaVersion` = 1, and `type Setup with member
    this.setupFile` (one direction only, ELEV006).
- `OpticalConstructor.Domain/Scene.fs` — `SceneStoreError`, `SceneName`, `SceneElement`, `SceneSnapshot` and
  `SceneFile` are deleted. `SnapMode` stays, under a module doc saying a setup does not carry it.
- `OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
  - The comment above `Scene.fs` is cut to `Spec 0038: snap mode, pure data.`
  - The BeamLayout comment loses "and before Scene.fs (which persists BeamHost)", which is no longer true; that is a
    cut only (see Decisions).

**Storage**
- `OpticalConstructor.Storage/schema/setup.schema.json` describes the setup file: an unconditional positive
  `schemaVersion`; under `if 1`, exactly `schemaVersion` + `setup`; `setup` holds exactly `name` (`\S`), `revision` (≥ 1),
  `slots` (array) and `history` (items exactly `revision` ≥ 1 + `slots` array).
- `OpticalConstructor.Storage/SchemaValidation.fs` — `validateSceneFile` is renamed `validateSetupFile`, still behind
  one `lazy`.
- `OpticalConstructor.Storage/KitContentFiles.fs`
  - `SetupFile.fileText` and `KitFileText.parseSetupFile` run over the step-146 stack.
  - `KitProxy.writeSetup` goes through `writeRenderedKindFile`, so a rendering refusal is `KitFileRejectedErr` naming
    the file.
  - `KitProxy.readSetups` goes through `readEveryKindFile`. A file that does not read, or whose setup has another
    stem (ignoring case), is `KitFileRejectedErr` naming it and refuses the whole read; the answer is in ordinal
    name order.
  - `KitLibraryContext.recordSetup` is `snapshotReferences setup.libraryReferences`, then `writeSetup`.
  - `writeScene`, `readScene`, `readSceneNames` and `recordScene` are deleted.

**Ui and Controls**
- `OpticalConstructor.Ui/KitOpening.fs` — `OpenedKit.setups : Setup list` (doc line) replaces `scenes`, and
  `openedOver` reads `readSetups ()`.
- `OpticalConstructor.Ui/TableAndElementRotationView.fs`
  - Model fields: `setupName`, `kitSetups` and `activeSetupOpt` (one doc line each). `initWith` sets `ControlCaption
    ""`, `[]` and `None`.
  - `withKitContent` takes the setups and keeps `activeSetupOpt` as the read setup of the same name.
  - `loadKit` passes `opened.setups`; `ReloadKitChanges` reads `readSetups ()`; `CopyChosenExperiments` passes
    `model.kitSetups`.
  - Messages: `SetSetupName`, `CreateSetup` and `ActivateSetup of SetupName` replace `SceneSetName`, `SceneSave` and
    `SceneLoad`.
  - The `CreateSetup` and `ActivateSetup` arms follow the brief (4)(c).
  - `captureScene` and `restoreScene` are deleted, and so is the now-unused `open OpticalConstructor.Domain.Scene`.
  - The view's setup row (heading `Setup:`) holds the `Setup name` field, `Create setup` (withheld read-only) and the
    `Saved setups:` WrapPanel.
- `OpticalConstructor.Controls/UiIds.fs` — `setupNameField`, `createSetupButton` and `savedSetups` replace the
  three scene ids.
- `OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs` — `savedSetup (name : Kit.SetupName)` (`SavedSetup_<name>`)
  and `type Kit.SetupName with member this.caption` replace the scene versions.

**Tests**
- `OpticalConstructor.Tests/SceneRoundTripTests.fs` is deleted and replaced by `SetupRoundTripTests.fs` (`module
  SetupRoundTripTests`; `<Compile>` entry and its comment renamed, the comment no longer).
  - `KitContentRoundTripFixture` stays at the top of the file.
  - Four facts are renamed per the census and `SnapMode` is unchanged.
  - The three `SceneName.tryCreate` methods are deleted (four cases: the two-row theory and two facts).
- `OpticalConstructor.Tests/SetupModelTests.fs` is new, appended last in the compile list with no comment. It has
  three facts.
- `OpticalConstructor.Tests/KitContentFileJsonTests.fs` — the scene fixture becomes setup `Bench: A` (r2, an LP item
  on `DialTurn (−20°)`, r1 empty). The five facts are renamed or changed per the census; this file now opens
  `Berreman.Geometry`.
- `OpticalConstructor.Tests/KitLibraryTests.fs`
  - `recordingScene` becomes `recordingSetup` (plus the `recordedSlot` helper), and the census fact is renamed.
  - The two un-renamed facts that used `recordScene` / `readSceneNames` (`a reference the library and the kit
    cannot resolve records nothing`, `a read-only kit records nothing`) now use `recordSetup` / `readSetups`. Their
    identities and assertions are kept.
- `OpticalConstructor.Tests/KitOpeningTests.fs` — the census fact is renamed and `opened.scenes` becomes
  `opened.setups`.
- `OpticalConstructor.Tests/ExperimentCollectionRoundTripTests.fs` — the census fact is renamed and uses
  `writeSetup` / `readSetups`.
- `OpticalConstructor.Tests/KitWindowModelTests.fs`
  - The read-only fact sends `SetSetupName` / `CreateSetup` and compares `kitSetups`.
  - The reload fact has the other writer write a setup, and the reload holds it.
  - The copy fact's `savedScenes` assertion compares `kitSetups` instead.
- `OpticalConstructor.Ui.Tests/SceneSaveLoadTests.fs` is deleted and replaced by `SetupSaveLoadTests.fs` (`module
  SetupSaveLoadTests`), with its `<Compile>` entry following it. The four census facts are renamed.
- `OpticalConstructor.Ui.Tests/KitWindowTests.fs` — `writeControlIds` uses `createSetupButton` (and its doc says
  Create setup).
- `OpticalConstructor.Ui.Tests/LauncherTests.fs` is outside the edit list; see Gotchas.

## Decisions

- **The kit status line stays the kit bar's.** Brief (4)(d) lists "then the `kitStatus` line" in the setup row.
  Since S5.3 the kit bar (`KitWindowView`) renders `ConstructorUiId.kitStatus` whichever bay is open; the
  collection builder's doc says one control carries it, and `KitWindowTests` asserts `Assert.Single` over that id.
  A second line would break that, so the row has no status line of its own. The kit bar's line shows
  `Created setup …` / `Not created — …` / `Load failed — …`.
- **`activeSetupOpt` across a reload/copy** is the read setup with the active one's name, else `None`, per brief
  (4)(a).
- **`ActivateSetup`'s refusal reason** is `UnknownSetupErr $"'{name.value}'"`, which reads `the kit has no such
  setup: 'nowhere'`.
- **Storage refusal in `SetupSaveLoadTests`** (`.manual/014`): the `protectedModel` keeps
  `proxy = ordered.kit.proxy.readOnlyView` with read-write access. `CreateSetup` refuses a held name before storage,
  so the fixture types a fresh name (`Protected bench`) to reach the refusing proxy. The `Not saved to the kit — … /
  read-only` assertions and the unchanged-model and unchanged-disk assertions are kept. The model-access refusal stays
  covered by `KitWindowModelTests` and `KitWindowTests`.
- **The repeat save in that fact**: the old fact re-saved under the same name, which overwrote. Creating a held name
  is now refused (`Not created`) and the single file stays, so the fact asserts that.
- **The fsproj BeamLayout comment** named Scene.fs as the persister of `BeamHost`, which is now false. Only that
  clause is cut; nothing is added.
- **No `--untracked` grep surprise**: the acceptance grep and the fifteen invariants find nothing tracked or
  untracked.

## Testing state

Development runs over the final tree, each with the gate's own command. No gate verdict is claimed; the supervisor
runs the gates.

| Run | Result | Wall | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors (every changed project recompiled warning-free) | 4–21 s incremental | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0 | 199 s | 600 s |
| constructor-unit-tests | `Passed!` 1420 / 0 failed / 0 skipped; all 18 census identities `Passed` in the TRX | 67 s | 180 s |
| ui-smoke | `Passed!` 245 / 0 / 0; `the setup row renders …` and the launcher acceptance fact `Passed` | 220 s (beside elevation) | 600 s |
| ui-tests (47 selected classes) | `Passed!` 643 / 0 / 0; the four census facts `Passed` | 185 s | 360 s |
| fifteen s26 invariants | each finds nothing (tracked and untracked) | — | — |
| acceptance grep `SceneSnapshot\|SceneName\|SceneFile\|recordScene\|readSceneNames` | finds nothing | — | — |

Constructor count: 1421 → 1420. Per `.manual/006` (step 0260), three SceneName methods leave with four cases (the
two-row blank theory plus two facts) and three `SetupModelTests` facts arrive, so the count falls by one by design.
Smoke is unchanged at 245. The ui-tests selection is 47 classes this step (it was 16 at 0259), so 643 is not
comparable with 0259's 324.

No run exceeded its deadline.

## Artifacts

`specs/0050/.artifacts/` holds `0260-dev-build.log`, `0260-dev-elevation.log`,
`0260-dev-constructor-unit-tests.log`, `0260-dev-ui-smoke.log`, `0260-dev-ui-tests.log` and
`0260-dev-invariants.log`. The TRX reports are under each test project's `TestResults/`.

## Gotchas

- **`LauncherTests.fs` changed outside the edit list.** The fact `acceptance: a kit open elsewhere offers Open
  read-only and Cancel in one row, and Open read-only opens it without writing` named
  `ConstructorUiId.sceneNameField` / `saveSceneButton`. This step must retire both ids, and the build cannot pass
  while they are referenced.
  - Only the two ids changed (`setupNameField` / `createSetupButton`), plus the local name, the typed text and the
    assertion message wording that named Save scene.
  - Every assertion `.manual/014` set is kept: visible but disabled, `KitStatus` absent before and after the click,
    files unchanged. No identity or count changed.
- **The shell ate a backslash.** A quoted bash heredoc wrote the schema's `"\\S"` as `"\S"`, which is invalid JSON
  (every setup parse failed with "'S' is an invalid escapable character"). It was fixed with the Edit tool, and the
  diff was checked for any other lost backslash; none was found.
- **The Berreman.Geometry `Angle`** is shadowed by its case under `Berreman.Geometry.Angle.degree`; open the module
  and write `Angle.degree`.
- `Setups` is not opened in test files that build `Experiment` records (whose `setup` label `SetupFile` shares);
  they qualify `Setups.` instead.
