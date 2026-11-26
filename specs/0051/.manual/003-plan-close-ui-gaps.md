# 003 — Plan: close the UI gaps found by 0050 `.manual/038` and 0051 `.manual/002`

2026-11-26. Requested by the operator: "we clearly must close the gaps identified in … 038 … and … 002 …, starting
with the missed requirements that all controls must have IDs so that to be drivable by automation. Write a plan of how
to close all the found gaps". This is a plan for discussion, not a spec and not a ruling. No product file was changed.

- **Inputs.** The two surveys:
  - `specs/0050/.manual/038-ui-behaviour-survey.md` (038). Its items are written `038-1` … `038-10`, plus 038's other
    sections.
  - `specs/0051/.manual/002-ui-behaviour-survey-real-desktop.md` (002). Its new findings are written `002-1` …
    `002-12`, plus its other sections.
- **Shape.** Six phases, in the order to implement them. Phase 1 (automation contract) comes first, as the operator
  asked. Every later phase builds on its shared controls and test driver.
- **Spec readiness.** Each phase is written so it can become a section of spec 0051 under the ASG constitution: wiring,
  minimum scope, negative scope, reuse, verification surface, and migration intent.
- **Traceability.** The table at the end maps every finding to the step that closes it.
- **Operator decisions.** The choices only the operator can make are collected under "Decisions for the operator".
  Each has a recommended default, because the arc-runner cannot ask.

## Phase 1 — Every control has an id that UI Automation can find, and can be driven by keyboard

Closes: 002-1, the "(colour only)" rows of 038 and 002, and 038's "Explorer and dialogs not checkable" gap.

### 1.1 What went wrong

AGENTS.md already requires `AutomationProperties.AutomationId` on every interactive control, and the code sets one
(`ConstructorUiId.automationAttribute`, `OpticalConstructor.Controls/UiIds.fs:33`). The ids are on the wrong controls.

- **Most "buttons" are pointer-only `Border`s.** A `Border` or a panel is not exposed to UI Automation, so the id on it
  is invisible. The builders that make them:
  - shared: `MaterialsWindowView.actionButton`, `verbButton`, `verbButtonOn`;
  - in `OpticalConstructor.Controls`: `LibraryControls.actionButton`, `ExperimentControls.actionButton` and
    `rowActionButton`, `CategoryControls.button`, `MaterialsControls.verbButton`, `SampleLibraryControls.verbButton`,
    and `RotationControls.clickBox` with its `stepButton`. `RayPositionControls.stepButton` and the
    `ElementPaletteControls` box are made the same way;
  - private copies in `CategoryEditorView`, `LibraryWindowView` (with `libraryVerbButtonWithBackground`),
    `LightSourceEditorView`, `MaterialEditorView` and `SampleEditorView`;
  - `TableAndElementRotationView.workbenchButton` and `KitWindowView.switchButton`;
  - the flow strip, ribbon tabs, slot chips, item rows, data rows and facet rows.
- **The count.** There are about 60 `onPointerPressed` press sites in 27 files, and 118 ids placed on a `Border` or a
  panel.
- **Measured on the real desktop (002).**
  - The kit window exposes 0 UIA buttons, and `SendKitButton` and `SaveSetupButton` are absent from the UIA tree.
  - Tab reaches only two text boxes and the slot chips.
- **Selected state is shown only by colour.** A green chip says "chosen" to the eye only. No control state carries it,
  so neither automation nor a screen reader can read it.
- **Ids are also set through `Name`.** The ribbon tabs and `RotationControls`' fields set the id as `Name`, not as
  `AutomationProperties.AutomationId`. UIA reads `Name` as a fallback, but the rule says AutomationId.
- **The duplication.** The same `actionButton` exists in six files, against AGENTS.md "Reuse, never copy".

### 1.2 The contract (what "drivable" means)

- **The definition.** An *interactive control* is anything the user presses, types into, picks, toggles or drags. Each
  one must meet all of these:
  1. **A real control.** It is a control that Avalonia exposes to UI Automation as a control element: a `Button`,
     `ToggleButton`, `CheckBox`, `RadioButton`, `TextBox`, `ComboBox`, a `ListBox` and its items, or a `Slider`. It is
     never a `Border` or a panel with a pointer handler.
  2. **An id and a name.** It carries `AutomationProperties.AutomationId` from `ConstructorUiId`, and
     `AutomationProperties.Name` set to its caption.
  3. **Keyboard reachable.** It can be reached by Tab when enabled, and activated by Enter or Space.
  4. **The right pattern.** It exposes the UIA pattern of its kind: Invoke, Toggle, Value, SelectionItem or
     ExpandCollapse.
  5. **State is not colour-only.** A choice shown by colour is a `ToggleButton` (or a selected list item) whose
     IsChecked or IsSelected says the same thing. This closes the "(colour only)" rows.
  6. **Unique ids.** Ids are unique within a window.
- **Text a test reads.** Status lines, readouts and badges carry an id too. A `TextBlock` is already exposed, so these
  already work.
- **The table canvas.** It stays one named control: AGENTS.md's "relative coordinates inside a named canvas". The drawn
  elements get a semantic path through the element list of step 3.6, not a custom automation peer each.

### 1.3 One shared set of command controls (reuse, not copy)

- **One new module.** A new module in `OpticalConstructor.Controls` holds the only builders of pressable and choosable
  controls. The names below are placeholders; the final names must be unique (ELEV014/015/016).
  - `commandButton (id) (caption) (emphasis) (availability) (onClick)` builds a `Button` with its id and Name, keyed by
    the id.
  - `choiceToggle (id) (caption) (chosen) (availability) (onChoose)` builds a `ToggleButton`. It is used for:
    - the Fit mode, calibration, Plan criteria and study chips;
    - the new-study experiment chips;
    - the slot-chip choices and the saved-setup chips;
    - the import dialog's per-row actions, and the wizard's Layout and Profile chips.
  - `CommandEmphasis`, a DU with two-word cases (neutral, positive, negative, armed), replaces the loose colour
    arguments.
- **The look stays.** The colours ride on each button's own resources (the Fluent theme's `ButtonBackground*`,
  `ButtonBorderBrush*` and `ButtonForeground*` keys), so pointer-over does not repaint them and no window needs a new
  style. Padding and margins are kept, so layouts do not move. This matters for the rotation test windows, whose
  headless tests click fixed coordinates (AGENTS.md).
- **Two handlers that read the pointer event today.**
  - `verbButtonOn` uses the press to find its owner window. It reads `TopLevel.GetTopLevel` of the clicked button
    instead.
  - `RotationControls`' modifier-sized step reads the modifiers of the press that caused the click, through a
    tunnelling `PointerPressed` on the same button. A keyboard click steps by the base step.
- **FuncUI subscriptions.** The builder re-subscribes when its caption or action changes. This is the reset button's
  "Reset → Yes / No" case (`RotationControls.clickBox` comment).
- **The migration is a move.** Every builder listed in 1.1 is deleted in the change that moves its call sites, in both
  `OpticalConstructor.Ui` and `OpticalConstructor.TestWindows`.
- **`Name` ids become AutomationIds.** The ribbon tabs and the rotation fields move to `automationAttribute`.
- **`automationAttribute` also sets the Name.** It takes the caption, so no call site sets the two separately.

### 1.4 The gate: `AutomationContractTests`

- **Home.** A new test class in `OpticalConstructor.Ui.Tests`, in the `ui-smoke` trait so the existing gate picks it up.
- **The window catalogue.** It walks every window the app can show, in each state that matters: kit window bays and
  flow steps, open panels, the start screen, the import dialog and wizard, Library, Materials, the editors, Data
  locations, the chart window and Material sources.
  - That list already exists as the fixtures of `ScrollExtentTests` ("add a new window to it").
  - It moves into one shared test module, a window catalogue. `ScrollExtentTests` and the new class both read it.
    This is a move, not a copy.
- **Each window state must satisfy:**
  1. **No ids on unexposed controls.** No `Border` or panel carries an AutomationId. This is the old pattern, and every
     former box carried an id, so this check alone finds all of them.
  2. **Every input control is exposed and named.** Every input control's automation peer is a control element with a
     non-empty AutomationId and Name, and is reachable from the window's root peer through `GetChildren`. That is the
     tree Windows UI Automation walks.
  3. **Keyboard reachable.** Every enabled input control is a tab stop.
  4. **Unique ids** within the window.
  5. **Chosen state is readable.** Every `choiceToggle` reports its chosen state through its toggle pattern.
- **It starts red.** It fails until Phase 1 is done, and lists every offender by window and id. Record it in the spec's
  expected-red file, as the arc does for planned-red tests.

### 1.5 One test driver instead of about 40 private click helpers

- **The problem today.** About 40 test files define their own `clickOn`, `pressAt`, `pressBox`, `clickBorder` and so
  on, and 34 of them assert `IsType<Border>`.
- **The fix: one `UiDriver` test module.** It sits beside the window catalogue and drives by the AGENTS.md priority
  order:
  - `invoke`, `toggle`, `setText` and `select` act by `ConstructorUiId` through the control's automation peer
    patterns. That is the "accessibility pattern" level.
  - `pressAtCentre` stays for tests that check layout, hit-testing or rendering. It does a pointer press at the
    control's centre after `BringIntoView`.
- **The move.** Every private helper is deleted, and its tests call `UiDriver`. `FsxAcceptanceTests` and the rotation
  test windows' fixed-coordinate tests move too.

### 1.6 The real-desktop layer: `OpticalConstructor.Desktop.Tests`

- **What it is.** A new test project (layer 3 of AGENTS.md's three test layers). It uses FlaUI (UIA3), the trait
  `desktop`, x64, Release and Microsoft Testing Platform like the others.
- **The sandbox.** It launches the built App with an `appsettings.json` whose locations are all under its own output
  directory, as `TestAppScope` does. It copies `data/optics-mueller` there and opens the copy read-write. The committed
  kits stay untouched.
- **What it checks.**
  - It walks Launcher → start screen → kit window → every bay and flow step.
  - Every id the window catalogue expects is found by UIA and can be invoked.
  - It drives the native dialogs through their Win32 controls, as 002 did, and the Explorer buttons.
- **The ids are never retyped.** It references `ConstructorUiId` directly, with no id strings copied into scripts.
- **It never runs in the arc.** It skips with `Assert.Skip` when there is no interactive desktop. The arc and its gates
  run in session 0, so its result never blocks them. The operator runs it, in a session where the app runs as the
  desktop's own user, so the Explorer windows can finally be checked.
- **Packages.** `FlaUI.Core` and `FlaUI.UIA3` are pinned once in the root `Directory.Build.targets`.

### 1.7 AGENTS.md

One line is added under "Stable IDs": an id counts only on a control UI Automation exposes; never put a press handler on
a `Border` or a panel; build every pressable control with the shared command controls; `AutomationContractTests` is
the law.

### 1.8 Verification, migration and negative scope for Phase 1

- **Verification.**
  - `AutomationContractTests` is green.
  - Every test class that moved to `UiDriver` is green, run filtered to the touched classes.
  - The `elevation` gate is clean, and rendered frames look the same in the `ui-smoke` frames.
  - The desktop suite is green when the operator runs it.
- **Migration intent.** None needed. Ids are runtime only, and every existing `ConstructorUiId` value is kept.
- **Not in Phase 1:**
  - custom automation peers for the drawn table elements (the element list of 3.6 is the semantic path);
  - any visual redesign;
  - localized Names (the Russian UI is in 0051 `001` §8);
  - a WinForms port.

## Phase 2 — Nothing changes the kit unless the user meant it

Closes: 038-4, 038-5, 002-6, the rewrite on an unchanged Fit mode, and 002's Remove without a status.

- **2.1 Loading a setup and saving it unchanged must change nothing.**
  - `SaveSetup` already compares the table with its setup (`TableAndElementRotationView.fs:3947`,
    `model.tableWrittenOver active = active`). The comparison never holds just after a load, because
    `ActivateSetup` → `slot.drawnWith` → `recordedSlots` is not an identity.
  - Make it one. The proof is a Domain property test (FsCheck through `Check.One`) over every setup of the two reference
    kits: activate, then write back, gives the same setup.
  - Then `SaveSetup`, `RecordExperiment` and `ExpCommit` stop minting revisions for an unchanged table.
- **2.2 The kit writes nothing when nothing changed.** At the `KitProxy` seam, an atomic write whose canonical JSON
  equals the file on disk writes nothing. The caller is told the file is unchanged. This one rule covers Update
  experiment on an untouched row, a re-pressed Fit mode and every other no-op write.
- **2.3 Remove asks first.** Remove confirms inline, as Rotate → Reset does ("Remove experiment 'X' and its file?
  Yes / No"), and then reports "Removed experiment 'X'.". Undo stays in 0051 `001` §7.
- **2.4 Re-attaching keeps what the experiment had.**
  - The wizard opens on the experiment's current profile and row selection. Today it opens on Intensity series and
    all rows.
  - It offers the key-column row choice whenever the profile has a key column, the way Split already groups.
  - `AttachDataFile` carries the chosen selection instead of "every row the run's" (`TableAndElementRotationView.fs`,
    message `AttachDataFile`).
- **2.5 The fixed / as priors choice is saved in the study** (decision D4). The study file gains the field.
- **Verification.**
  - The Domain round-trip property and KitProxy no-op-write tests run in `constructor-unit-tests`.
  - The UI tests are SetupSaveLoadTests, DataStepTests, ImportWizardTests and the Fit view's tests, run filtered.
  - The acceptance tests confirm the committed reference kits are unchanged by these steps.
- **Migration intent.**
  - 2.1–2.4 change no stored shape.
  - 2.5 changes `*.study-json`, so its `schemaVersion` is bumped. A study file of the old version is
    `UnsupportedSchemaVersionErr`, as AGENTS.md's "no legacy, no migration" rule says.
  - Both reference kits hold a study (`QZ-LR.study-json`, `AbsorbingTriclinic.study-json`), so AT-1 and AT-2 rebuild
    them, and the changes are re-committed in the same step.

## Phase 3 — The table is the context, and the UI shows it

Closes: 038-1, 038-2, 038-3, 038-6, 038-8, 038-10, 002-7, 002-8 (measured points), 002-9, and 038's palette and
Record-experiment notes.

- **3.1 A kit opens on a setup, not on an empty table** (decision D2).
  - The per-user kit history (`KitHistory.fs`, `KitHistoryStore.fs`) records the setup last active in each kit. The kit
    window opens on it.
  - Else it opens on the setup of the most recently written experiment.
  - A kit with no setup opens on the default table, with a hint that names where setups are made.
- **3.2 The Setups step owns setups.** The Setup section moves from the Experiments bay to the top of the Setups step:
  the name field, Create setup, Save setup and Saved setups. This closes 038-6. It also closes the "Setup section
  4,000 px down" part of 002-5. The active saved setup is a chosen `choiceToggle`, so it is highlighted and UIA reads
  it.
- **3.3 Choosing an experiment row loads its setup revision.**
  - `ExpEdit` also draws the experiment's own setup revision on the table: the exact revision, not the latest of that
    name. The status says "revision r of n" when it is not the latest.
  - This removes the misleading "no detector captures …" message of 038-3.
- **3.4 View ↗ charts the experiment's own setup, in both constructors.**
  - `chartForExperiment` (`TableAndElementRotationView.fs:5676`) runs the scan over the slots of the experiment's
    setup revision, not over `model.elements`.
  - The Inverse gate `experimentChartVisible` keeps guarding only the live draft preview. This closes 002-7 and the
    wrong-setup chart of 038-2.
  - The chart window draws the experiment's kept points (`MappedPoints.keptBy`) on the right axis, in their own units,
    beside the model on the left. The window already supports per-series axes.
  - When no chart can be computed, the kit status says why, instead of nothing happening.
- **3.5 One name for each element.**
  - The Experiments bay's element choices, the scan descriptions and the selection readout use the slot bar's naming:
    role, number and kit item ("Analyzer 1: LP2"). Today they say "Linear polarizer #6" or the library binding.
  - Experiment files store the old label (`"label": "Linear polarizer #6"`), so this touches a stored shape
    (decision D3).
- **3.6 Preconditions are visible, and a list selects elements.**
  - A control that needs a precondition stays visible but disabled, with a one-line reason: Create study, Generate,
    Add experiment, Add variable, Add block.
  - Element choices list only elements that have something to vary.
  - The Select bay gains an element list, a `ListBox` with one id per element. It selects without the canvas, and it is
    automation's semantic path to the table (1.2).
- **3.7 The table drawing and its readout.**
  - The zoom fits the table to its canvas on open and on resize.
  - Elements are drawn with their kit item's label.
  - The readout names the kit item.
  - Angles are shown normalised to [0°, 360°), so 0° is never shown as "360°".
  - The element readout's "zoom 5.0×" is `e.zoom`, the element's own drawing scale, not the view's. It is labelled as
    such or dropped.
- **3.8 A palette element added in a kit window is an empty slot** (decision D7). Its chip opens at once. It is not
  silently bound to the "Ideal linear polarizer" preset.
- **3.9 Record experiment asks for its note.** It fills the template's `{note}` inline, so repeated records get
  meaningful names instead of " (2)", " (3)".
- **Verification.**
  - Domain tests cover the setup-revision run and the angle normalisation.
  - UI tests are filtered to the touched classes: ExperimentControlsTests, SetupSaveLoadTests, KitWindowTests,
    SlotBarTests, EmbeddedChartTests and the start-screen tests.
  - `FsxAcceptanceTests` must stay green, because the chart path changes.
- **Migration intent.**
  - The kit-history file gains a field. It is per-user preferences, not kit data. An old file is the typed
    schema-version error, and the start screen offers to start a new history. Nothing reads the old shape.
  - D3, if the stored label is dropped: `*.experiment-json`'s `schemaVersion` is bumped, and AT-1 and AT-2 rebuild both
    reference kits, re-committed in the same step.

## Phase 4 — The Data and Inverse steps can be read

Closes: 002-4, 002-5 (except the Setup section, which is 3.2), 002-10, the Review panel placement, and 038's Publish
calibration message.

- **4.1 The Data files rows name their experiment.** Each row leads with the experiment's name and badge.
- **4.2 The Review panel opens where it was asked for.**
  - It opens directly under the row whose Review was pressed, and is brought into view.
  - Its scatter's x axis is the scanned variable (the dial reading), not the point number.
  - The Reason box clears after an exclusion, so the next Exclude does not silently reuse the old reason.
- **4.3 No page is wider than its window.**
  - The new-study experiment chips wrap, and so does the "Experiments: …" line. The wrapping container is vertical
    (AGENTS.md: never a WrapPanel in a horizontal StackPanel).
  - The experiment matrix scrolls sideways inside its own scroller.
  - The ribbon's `Library…` and `Materials…` wrap at the 820 px default width instead of leaving the window.
  - `ScrollExtentTests` gains a width check: at 1000 × 540, no page is wider than its window outside a named inner
    scroller.
- **4.4 Copy experiments: the item mapping is visible and named.**
  - The item rows move to their own section, outside the 160 px experiment list.
  - Each entry is named by its library name and version, not its GUID.
  - The kit's library overlay is refreshed after the copy, so the copied item resolves at once instead of reading
    "(model not in the library)".
- **4.5 Publish calibration gives one message.** It shows one outcome, not "Published …" and "already published" at
  once.
- **Verification.** DataStepTests, StudyUnknownsView's tests, KitWindowTests (copy) and ScrollExtentTests, run filtered.
- **Migration intent.** None: no stored shape changes.

## Phase 5 — Messages, dialogs and import questions

Closes: 002-2, 002-3, 002-8 (exports), 002-11, 002-12, 038-7, 038-9, and the wording notes of 002.

- **5.1 Status messages say whether they report success or failure, and they stay.**
  - `kitStatus` becomes a two-case DU, done or refused. It is drawn neutral for done and firebrick (`#B22222`) for
    refused. Today all 63 status sites are drawn red.
  - An action that produces no message no longer clears the last one. The 12 `kitStatus = None` sites are reviewed.
  - Remove and Reload report what they did.
- **5.2 Each dialog starts in a known folder, with a fitting filter.**
  - `AppContext.create` creates the Kits, Exports and Catalogues folders on start (decision D5).
  - Each picker purpose starts in its folder, or in its last-used folder from `environment.json`'s `lastFolders`. That
    mechanism exists for "measured-data" and is extended to every purpose.
  - Register catalogue folder… and Import glass catalogue… stop opening on Desktop.
  - Choose photo… filters images. Attach data file… filters CSV and text, plus All.
  - The unpack dialog of Open kit from zip… says what it is for: "Choose an empty folder to unpack the kit into".
- **5.3 Import questions only when there is something to decide** (decision D6).
  - A row whose only action is "Reuse the held entry" is applied without asking, and summarised in the status.
  - On first start, the shipped catalogue's new entries are imported without a question. Only changed rows wait for a
    decision, as spec 0050 S2 intends.
  - Reopening a kit asks nothing when nothing changed.
  - While decisions are pending, Materials and Library say so, instead of showing 0 entries.
- **5.4 Wording.**
  - The glass import is headed by its file: "Import 3 glasses from test-glasses.agf". It is not "dropped entry files".
  - Export my library… says plainly that the library holds no entries of its own.
  - The split panel's "— →" becomes one arrow.
- **5.5 The item photo is shown as a thumbnail** in the item panel.
- **5.6 The start screen's history** shows the folder when two kits share a name, and local times instead of UTC.
- **5.7 The app is a windowed program.**
  - `OpticalConstructor.App.fsproj` becomes `WinExe`, and the log goes to its file. No console window opens behind
    the Launcher.
  - `UserEnvironment.settingsPath` takes `DataLocations.preferencesFolder`, so `environment.json` lives where the Data
    locations window says.
- **5.8 Chart exports use a save dialog.**
  - Export PNG and Export CSV open a save dialog in Exports and report the file written. The fixed
    `%TEMP%\experiment-chart.*` files go (`ChartWindow.fs:529`, `:532`).
  - SVG, PDF and Excel export stay in 0051 `001` §5.
- **Verification.** KitWindowTests, ImportDialogTests, LauncherTests, KitStartWindowTests, DataLocationsWindowTests and
  the chart window's tests (ChartStyleTests, EmbeddedChartTests), run filtered. The desktop suite covers the dialogs' start folders and the Explorer windows.
- **Migration intent.** `environment.json` moves folder. The old file under `%APPDATA%` is simply no longer read, and
  the app starts with defaults. No other stored shape changes.

## Phase 6 — Re-survey on the real desktop

- **What it is.** Run `OpticalConstructor.Desktop.Tests`, then repeat the 038 and 002 survey on the real desktop. Its
  report is the next `NNN` in `specs/0051/.manual/`.
- **Done means:** every row of the traceability table below is closed, or explained there.

## Traceability: every finding → the step that closes it

| Finding | Step |
|---|---|
| 038-1 kit opens on an empty table | 3.1 |
| 038-2 View ignores the experiment's setup; no measured points | 3.4 |
| 038-3 row click does not load its setup | 3.3 |
| 038-4 Remove without confirmation | 2.3 |
| 038-5 revisions written when nothing changed | 2.1, 2.2 |
| 038-6 Setups step has no setup list | 3.2 |
| 038-7 status messages vanish | 5.1 |
| 038-8 element labels differ from kit names | 3.5, 3.7 |
| 038-9 one-answer import questions; 0 entries shown until applied | 5.3 |
| 038-10 hidden preconditions; no list selects an element | 3.6 |
| 038 Review / Exclude "not confirmed" | confirmed working in 002; placement in 4.2 |
| 038 Plan cannot run on black-box kits | not a UI gap: the Planner's targets are 0051 `001` §2 |
| 038 "(colour only)" controls | 1.2 item 5 (`choiceToggle`) |
| 038 Light source / Detector offered with nothing to vary | 3.6 |
| 038 Publish calibration contradicts itself | 4.5 |
| 038 palette polarizer bound to a library preset | 3.8 |
| 038 Record experiment piles up numbered names | 3.9 |
| 038 dialogs and Explorer not checkable headless | 1.6 |
| 002-1 buttons unreachable by keyboard and UIA | Phase 1 |
| 002-2 every status message red | 5.1 |
| 002-3 dialog start folders and filters | 5.2 |
| 002-4 Data rows unnamed; Review panel far away | 4.1, 4.2 |
| 002-5 pages too wide or tall; active setup not highlighted | 4.3, 3.2 |
| 002-6 re-attach widens the experiment; wrong profile pre-selected | 2.4 |
| 002-7 View ↗ never charts in Inverse | 3.4 |
| 002-8 chart without measured points; silent temp-file exports | 3.4, 5.8 |
| 002-9 small table, readout mismatches, "360°" | 3.7 |
| 002-10 Copy: hidden item mapping, GUIDs, stale "(model not in the library)" | 4.4 |
| 002-11 repeated import questions, duplicate history names, UTC times | 5.3, 5.6 |
| 002-12 console window; `environment.json` ignores the Preferences folder | 5.7 |
| 002 "as priors" not saved | 2.5 |
| 002 Fit mode re-press rewrites the study | 2.2 |
| 002 Remove reports nothing; Reload reports nothing | 2.3, 5.1 |
| 002 Exclusion reason persists | 4.2 |
| 002 photo never displayed | 5.5 |
| 002 wording: "dropped entry files", unpack title, Export my library, "— →" | 5.2, 5.4 |
| 002 Send kit progress too fast to see | not a gap |

## Decisions for the operator

Each has a recommended default, which the arc applies unless the operator rules otherwise.

- **D1. The real-desktop layer.**
  - Recommended: a FlaUI test project, as in 1.6. Its ids come from `ConstructorUiId`, so no id string is retyped. It
    skips in session 0.
  - Alternative: a committed PowerShell driver like 002's. It needs no new packages, but it retypes the ids.
- **D2. Where a kit opens.**
  - Recommended: the setup last active on this machine, as in 3.1.
  - Alternative: a setup chooser shown on open.
- **D3. The stored axis label in `*.experiment-json`.**
  - Recommended: derive the label from the setup slot when drawing, and drop the stored field. This bumps the
    `schemaVersion`, and AT-1 and AT-2 rebuild the reference kits.
  - Alternative: keep the field stored and change only what is shown. That leaves a stale "Linear polarizer #6" in
    every file.
- **D4. Fixed / as priors.**
  - Recommended: save it in the study, as in 2.5.
  - Alternative: keep it window-only, but say so beside the chips.
- **D5. Missing data folders.**
  - Recommended: create Kits, Exports and Catalogues on start.
  - Alternative: a picker falls back to the nearest existing parent folder. That keeps spec 0050 S6's "no folder is
    made".
- **D6. Import questions.**
  - Recommended: apply one-option rows silently, and import the shipped catalogue's new entries on first start without
    a question, as in 5.3.
  - Alternative: keep asking, with every row already applied.
- **D7. A palette element in a kit window.**
  - Recommended: an empty slot whose chip opens, as in 3.8.
  - Alternative: bind the first kit item that fits the slot.

## Not in this plan

These are named so they are not added on the way:
- undo and redo (0051 `001` §7);
- the Russian UI (`001` §8);
- the Planner for non-gyrotropic kits (`001` §2);
- SVG, PDF and Excel export (`001` §5);
- a report generator;
- a WinForms port;
- a visual redesign or theme change beyond the fixes above;
- custom automation peers for drawn table elements;
- new configuration knobs for any of the defaults above.

## Gates and order

- **Phase 1 first.** It touches every window and about 40 test files. Every later phase writes its controls with the
  shared builders and its tests with `UiDriver`.
- **Phase 2 next.** It stops unintended kit writes before more UI is built on top of them.
- **The rest.** Phases 3–5 follow in order. Phase 6 closes.
- **The gates.**
  - Each phase runs the `build` and `elevation` gates, `constructor-unit-tests` for Domain changes, `ui-smoke` (which
    includes `AutomationContractTests` from Phase 1), and `ui-tests` filtered to the touched classes.
  - The reference-kit acceptance tests run wherever a stored shape changes (2.5, and D3 if accepted).
  - `BerremanTests` is untouched: no solver code changes.
