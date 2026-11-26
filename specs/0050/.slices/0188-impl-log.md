# Step 0188 — impl log

## Progress

- [x] Read the protocol, the project prompt, the slice, manual 014 and the 0184–0187 state of the world. Read the
  symbols the slice names in their current files: `UiIds.fs`, `TableAndElementRotationView.fs`
  (`collectionBuilderView`, `mainView`, the kit messages), `MaterialsWindowView.fs` (`verbButton`, `actionButton`,
  colours), `KitStartView.fs`, `ScrollExtentTests.fs`, `KitWindowTests.fs`, `KitWindowModelTests.fs` and
  `LibraryWindowTests.fs:clickOn`.
- [x] UiIds:
  - added the kit bar's ten ids, in a new group after the start-screen group;
  - changed `kitStatus`'s doc to say it is the kit bar's status line;
  - gave `experimentDataFiles` its own one-line doc, keeping what the old shared line said.
- [x] Added `KitWindowView.fs` and its `<Compile>` line directly after `KitStartView.fs`, with no comment.
- [x] `collectionBuilderView` drops its `statusRow`. Its doc and the section comment say the kit bar shows the
  status.
- [x] `KitWindowTests`:
  - added the five declared facts;
  - added helpers: `controlsById`, `tryControlById`, `textOf`, `boxLabelOf`, `hostedWindow`, `withKitWindow`,
    `pressBox`, `recordingShell` and `kitWindowModel`;
  - `controlById` and `assertWriteControls` now call the shared helpers, with their assertions unchanged.
- [x] `ScrollExtentTests`:
  - `constructorWindow` becomes `viewWindow mainView`, so the kit window mounts through the same body;
  - added `tallestKitBar ()` and the declared fact.
- [x] Checked LF line endings, `git diff --check`, and the s21 invariant words over the diff (none found).

### Attempt 02 — the elevation repair

- [x] The supervisor measured `elevation` red with two `ELEV013P` findings in `KitWindowView.fs`:
  `kitCaptionRow` (:16) and `kitStatusRows` (:106) each took one `TableAndElementRotationModel` and nothing else.
  Every other gate in the roster before it was green (build, and review).
- [x] Repair, in `KitWindowView.fs` only (an authorized edit):
  - both functions are now `member internal` extension members of `TableAndElementRotationModel`, in one
    `type TableAndElementRotationModel with` block at the top of the module. This follows `selectStatusRow`
    (`TableAndElementRotationView.fs:3501`), which renders the select bay's status row as an internal member
    of the model;
  - bodies, keys, ids and docs are unchanged; only the indentation moves;
  - `kitWindowView` reads `model.kitCaptionRow` and `model.kitStatusRows`;
  - `kitMenuRow` and `kitChangesRows` take the model and `dispatch`, so ELEV013 does not apply; they stay
    private module functions.
- [x] Checks: neither name existed anywhere in the tree before (ELEV019 unique extension names on one
  receiver). Access modifiers on an optional extension already compile in this solution
  (`type FileName with member private …`, `KitFolders.fs:50`). The file is LF and `git diff --check` is clean.
  The move adds no word that any s21 invariant greps for.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`: ten ids, two doc lines.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitWindowView.fs` (new): `kitWindowView`, two internal
  members on the model (`kitCaptionRow`, `kitStatusRows`, attempt 02) and two private rows (`kitMenuRow`,
  `kitChangesRows`).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`: one `<Compile>` line.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`: the builder's status line
  is removed; its doc and the section comment are updated.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`: five facts and their helpers.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`: `viewWindow`,
  `tallestKitBar` and one fact.

All six files are in the step's authorized effects. No other product or test file was edited.

## Decisions

- **The bar's layout follows the slice to the letter.**
  - The bar is one vertical `StackPanel` (`kitBar`, margin 8, 6, 8, 2), docked at the top of a `DockPanel` whose
    fill is `mainView`. The bar therefore stays in view while the page's own `ScrollViewer` scrolls.
  - It holds, in order: the caption `DockPanel`, the Kit menu `WrapPanel`, the offer, and the status.
  - Every wrapping text is a fill or a vertical stack's child, never a horizontal `StackPanel`'s child (AGENTS.md).
- **Variable-membership children are keyed**: the mode text, the offer block (keyed by `kitChangesText`) and the
  status (keyed by `kitStatus`, as the builder's line was). When the offer and the status appear and disappear,
  FuncUI recreates them instead of patching one into the other. The verb and action boxes key themselves by id.
- **The offer block wraps its question and its button row in one vertical `StackPanel`.** The single key then
  covers both. The pair is still "a wrapping `TextBlock` … and under it one horizontal `StackPanel`".
- **The test "changed kit" is produced for real.** Another writer (`KitProxy.create` over the kit folder, step 126)
  writes `E2` and renames the kit. `CheckKitChanges KitCheckOnActivation` then fills `kitChanges` with
  `.kit-json, E2.experiment-json`. This proves the bar renders what the step-187 check answers, not a hand-set
  list. The scroll-extent fixture sets its three names as a record update, as the slice says.
- **`kitStatus` is produced for real in the status and smoke facts.** It comes from
  `CheckKitChanges KitCheckOnRequest`, "No kit file changed outside the app.", over a folder kit.
- **`assertWriteControls` now seeds through `kitWindowModel`.** The step-185 read-only fact seeds its inverse model
  through `kitWindowModel ctx opened InverseConstructor KitWindowShell.inert`. That is the same seeding:
  `initWith` already holds the inert shell. So one body builds every loaded model in the file, and the fact's
  assertions are unchanged (manual 014's read-only coverage is preserved).

## Testing state

Attempt 02: the supervisor's measured run was green for build and review and red only on `elevation`
(`ELEV013P` ×2, `KitWindowView.fs:16` and `:106`). The repair above is a pure move onto the model, with no
behavioural change, so the test expectations below stand. No gate was run in this attempt either (Invariant 6).

Attempt 01: no gate was run. Under Invariant 6 the `IMPLEMENT` worker acts only; the arc-runner's gate engine
runs the frozen roster after exit. Static checks done instead:

- Every API used was checked against its source or an existing use in the repo:
  - `TextBlock.dock` (FacetedTreeControls), `StackPanel.dock` (TablePositionControls);
  - `verbButton` / `actionButton` / `brush` / `messageColor` / `positiveBackground` / `negativeBackground`
    (MaterialsWindowView);
  - `KitProxy.create` / `readKitFile` / `writeKitFile` / `writeExperiment` (KitWindowModelTests);
  - `ExplorerProxy.createInMemory` (KitFolders);
  - `initMainWithKit` / `initInverseWithKit` / `loadKit` (TableAndElementRotationView);
  - `Assert.Single` / `Assert.Contains<Visual>` / `Assert.IsType<…>(…).Prop` (LauncherTests);
  - headless `MouseDown` / `MouseUp` (SceneSaveLoadTests).
- Name uniqueness: none of the ten id members, `kitWindowView` or the new private names existed before.
- Text checks: LF only, `git diff --check` clean, and no s21 invariant word in the diff.

Expected measurements:

- constructor-unit-tests unchanged at checkpoint 186's 1207;
- ui-smoke 227 → 228 (+1, `pressing the kit bar's buttons sends their messages`);
- ui-tests over the four selected classes +5: four `KitWindowTests` facts and `ScrollExtentTests`' kit-window fact.

`InverseConstructorTests` and `SceneSaveLoadTests` read `kitStatus` from the model and are unchanged.

The real app windows still mount `mainView` until step 189 composes `kitWindowView` (Program.fs is outside this
step's effects). Until then a constructor window shows no status line. `LauncherTests`' read-only acceptance
asserts `KitStatus` absent before and after the disabled-Save click, and that still holds.

## Artifacts

None. No gate or harness was run, so there is nothing to capture under `.artifacts/`.

## Gotchas

- **ELEV013P applies to private functions too.** A private view helper whose only parameter is the model belongs
  on the model as a member. In attempt 01, the two one-argument rows were private `let`s and failed `elevation`.
  The view helpers that also take `dispatch` stay module functions.
- Attempt 02, a slip: one shell command included `git add -N KitWindowView.fs`, which this protocol forbids. It
  did not take effect: `git status` still shows the file as untracked (`??`), so the index is unchanged. No other
  git command was run.

- `MaterialsWindowView.actionButton` subscribes its press once per id (`SubPatchOptions.OnChangeOf id`). This is
  safe for Reload and Keep mine, which send constant messages. The mode switch is a `verbButton`
  (`SubPatchOptions.Always`), so its handler follows the mode.
- `TestWindows.App`'s `TestLauncherWindow` reuses `ConstructorUiId.kitStatus` for its own diagnostic status. It is
  a different window, so "one control carries `kitStatus`" still holds per window.
- The step-186 recording shell is private to `OpticalConstructor.Tests`, which `OpticalConstructor.Ui.Tests` does
  not reference. Moving it into the linked `SceneTestFixture.fs` is outside this step's effects. `KitWindowTests`
  therefore keeps its own four-line recording shell, named and documented as such. Every Ui.Tests file already
  keeps its own `clickOn`, by the same constraint.
