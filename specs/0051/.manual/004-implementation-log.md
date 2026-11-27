# 004 — Implementation log: closing the UI gaps (plan `003`)

2026-11-26. The operator accepted plan `003-plan-close-ui-gaps.md` with every recommended decision (D1–D7) and asked to
"implement it in full and keep an implementation log as you go", then re-drive the app through UI automation. This log
is written as the work happens: each entry says what changed, where, how it was checked, and any deviation from `003`
with its reason (ASG constitution, Principle 9).

Decisions in force: D1 FlaUI desktop test project · D2 open on the last active setup · D3 drop the stored axis label ·
D4 save fixed / as priors in the study · D5 create Kits, Exports, Catalogues on start · D6 no one-option import
questions · D7 a palette element in a kit window is an empty slot.

## Phase 1 — automation contract

### 1.a The shared command controls

- New `OpticalConstructor.Controls/CommandControls.fs`, compiled right after `UiIds.fs`. `ControlAvailability` and
  `ControlCaption` moved there from `RotationControls.fs`, because the rotation bar now uses the shared builders.
- `CommandEmphasis` is the one palette: neutral, positive, negative, chosen, armed, pending. It replaces about 17 copies
  of the same `idleBackground` / `idleBorder` constants.
- `CommandButton` (a `Button`) and `ChoiceToggle` (a `ToggleButton`). `ChoiceToggle` overrides `Toggle` with a no-op, so
  only the model sets its checked state.
  - Their colours are written into the button's own resources under the Fluent theme's keys, read from the Avalonia 12
    theme dll: `ButtonBackground*`, `ButtonBorderBrush*`, `ButtonForeground*`, and the `ToggleButton*` keys including
    the checked and indeterminate states. Pointer-over and press keep the fill.
  - `CommandButton.pressModifiers` replaces the `PointerPressedEventArgs.KeyModifiers` the rotation and position bars
    read for their step size.
- The builders are `CommandControls.commandButton`, `commandButtonOn` (handed the pressed button, for its window or a
  sibling text box) and `choiceToggle`. Each sets the AutomationId, `AutomationProperties.Name` (the caption) and
  wrapping caption text, keyed by the id, with `SubPatchOptions.Always`.
- **Deviation from `003` 1.3.** `automationAttribute` keeps its signature (about 400 call sites, most on text). The
  builders set the Name themselves.
- **Deviation from `003` 1.4 rule 1.** An id is allowed on a panel, border or canvas, provided the container is exposed
  to UI Automation. `ConstructorUiId` gained `groupAttributes`, `surfaceAttributes` and `figureAttributes`. They set
  `AutomationProperties.AccessibilityView = Control` and `ControlTypeOverride` (Group / Pane / Image), which Avalonia 12
  supports. The reason: about 70 containers carry ids that tests scope their searches by, and exposing them keeps that
  ability for UI Automation too.
- **Converted in `Controls`:**
  - the rotation, ray-position and table-position bars (`stepButton` reads `pressModifiers`);
  - the renderer bay, the element palette and the ribbon (tabs are `ChoiceToggle`s, the pane is a group);
  - the library, materials, sample-library and category controls (facet options and rows are choices, verbs are
    commands);
  - the experiment bay: options, candidates, roles, action and row buttons, and the experiment row (a choice, amber
    while loaded, double-click opens View);
  - the faceted tree: representation options and tree rows are choices; chips, offered values, Show/Search and the
    chevrons are commands;
  - layer bands (figures).
- Every `TextBox.name` / `TextBlock.name` / `Slider.name` id now uses `automationAttribute`.

### 1.b The `Ui` project and the test windows

- **Removed copies.** Every press-handling `Border` in `OpticalConstructor.Ui` is gone, and so are its private button
  builders: the `MaterialsWindowView` verb, action and choice copies, `LibraryWindowView`'s two, and the four editors'
  (Sample, Material, Light source, Category). All callers use the `CommandControls` presets: `verbCommand`,
  `verbCommandOn`, `actionCommand`, `choiceCommand`. The duplicated colour constants went with them.
- **Choices.** Twelve "chosen = positive fill" sites are now `choiceCommand`, a `ToggleButton` whose checked state is
  the choice:
  - the flow strip, item rows, the copy item mapping, online source choice, Plan material and criteria, the setup grid,
    Results tabs, study chips, new-study experiments, split offers, the matrix review entry, and the slot-chip choices;
  - the Fit mode and calibration choices, fit-run rows, the import dialog's actions and the wizard's layout and profile
    chips.
- **Richer content.** The sample editor's layer row (with its inline orientation boxes), the group expander and the start
  screen's history rows use the new `commandButtonShowing` / `choiceToggleShowing`. These show a view and are named by a
  caption. A history row's Name says its folder, so two kits of one name differ for UI Automation. The history star is
  a choice.
- **Press handlers.** Handlers that read the press now take the pressed `CommandButton`: owner-window lookups, the Data
  step's typed reason and correction note, Create setup, Choose…, the menu's Choose…, the photo, attach and split
  pickers.
- **Exposed containers and figures.**
  - 71 containers and canvases with ids are exposed through `groupAttributes` / `surfaceAttributes`, including the test
    windows' canvases.
  - Drawn shapes with ids (layer bands, the inline chart polyline, review dots, element hover rings) use
    `figureAttributes`.
- **Named inputs.** 37 inputs without an automation Name get `inputAttributes`: the id plus a Name spelled from the id
  (`ConstructorUiId.spokenName`). The chart window's combo boxes and text boxes get the same through
  `AutomationProperties.SetName`. The φ/θ/ψ boxes of a layer's orientation editor had no id at all; they now carry
  `sampleLayerOrientationAngle`.
- **Deviation from `003` 1.2 item 2.** Most of these Names are spelled from the id, not taken from a visible label. The
  reason: about 40 generic field helpers (numeric boxes, per-column choosers, scope combos) have no caption in scope.
  Threading one through each would touch every caller, for a Name a screen reader already reads sensibly
  ("Naming template field").
- **FuncUI attribute order.** A test showed FuncUI keeps the first attribute of a property. `CommandControls.merged`
  therefore drops a default (padding, border thickness) that `extras` sets, by `IAttr.UniqueName`.

### 1.c Tests

- **`UiDriver`** (new, `OpticalConstructor.Ui.Tests/UiDriver.fs`):
  - finds by the id UI Automation reports (AutomationId, else Name);
  - reads captions, texts and the effective fill of a `CommandButton` / `ChoiceToggle`;
  - presses at the centre with a release, or invokes through the automation peer (`invoke`);
  - `UiDriver.ById` offers the same over `ConstructorUiId`.
- **Helpers removed.** A scripted pass deleted the private `matchesId`, `clickOn`, `isPresent`, `tryFindControl`,
  `textOf`, `controlOf`, `controlById`, `tryControlById`, `pressAt`, `pressBox`, `pressControl`, `boxLabelOf`, `fillOf`
  and `assertAbsent` copies from 30 test files. It also moved `SlotBarTests`' internal helpers, used about 200 times
  across files, onto `UiDriver.ById`.
- **Tests updated.** `Border`-typed lookups, `Name`-based lookups and `Background` / `Child` reads in the remaining tests
  now use `UiDriver.hasId` / `idOf` / `fillOfControl` / `textOfControl`.
- **The contract checker.** `AutomationContract.violations` (new) enforces the contract:
  1. an id on a control UI Automation does not expose;
  2. an input with no id or no Name;
  3. an enabled input the keyboard cannot reach;
  4. an input not reachable from the window's automation root;
  5. a choice without a toggle pattern;
  6. a duplicated id.
- **Where it runs.** `ScrollExtentTests.assertReachable` applies the checker to every window it builds.
  `AutomationContractTests` (new, `ui-smoke`) applies it to the Main window in every bay, the kit window, Library and
  Materials. It also proves that the checker refuses an id on a bare `Border`.
- **Deviation from `003` 1.4.** The window catalogue stays in `ScrollExtentTests` (its builders are now `internal`)
  instead of moving to a new module. Both laws read the same builders, which is what the plan asked for, without moving
  700 lines of fixtures.
- **First full run.** After the conversion, the first full run of `OpticalConstructor.Ui.Tests` showed 225 failures, all
  from the old `Border`-based helpers. After the driver: 69 failures, about 20 of them real contract breaches the
  checker found in the product: unnamed inputs, shape ids, and the orientation boxes without an id.

### 1.d The real-desktop suite

- **The project.** `OpticalConstructor.Desktop.Tests` (new; FlaUI.Core and FlaUI.UIA3 5.0.0 and System.Management
  10.0.15, pinned centrally; in `Berreman.slnx`).
- **The sandbox.**
  - `DesktopSession` copies the built App once per test process and writes its `appsettings.json` to a sandbox under
    the test output.
  - It copies `data/optics-mueller`, found through `BerremanTestSupport.CommittedKit`, which also gives the repository
    root.
  - It answers native dialogs through their Win32 name box and default button.
- **What it checks:**
  - the Launcher's buttons are found and invoked by id;
  - a kit opens through the real folder dialog, and every flow step (Main, then Inverse) and ribbon tab is invoked by id,
    its toggle state read back;
  - Show in Explorer opens an Explorer window on the kit folder. This skips when the desktop's shell belongs to another
    account, as in `002`.
- **When it runs.** Every fact skips in session 0, so it never runs in the arc's gates.

## Phase 2 — nothing changes the kit unless the user meant it

- **2.1, the cause.** Saving an unchanged setup wrote a new revision because the table holds each dial-mounted slot
  drawn as a physical angle under the item's current zero. The stored slot was written under the zero of its day (LP2
  was calibrated since), so `tableWrittenOver active` never equalled `active`.
- **2.1, the fix.** `tableWrittenOver` now compares the table with the active setup's slots drawn and recorded alike,
  and writes nothing when they match. This also covers Record experiment and Update experiment, which go through the
  same member.
- **2.2.** The kit folder's `writeTextBytes` leaves a file alone when it already holds the bytes to write. This covers a
  re-pressed Fit mode, an untouched Update experiment and every other no-op write.
- **Deviation from `003` 2.2.** Callers are not told "unchanged". Save setup and Save template already say so
  themselves; the others need no message.
- **2.5, the data.** `CalibrationUse` moved from `FitRuns.fs` into `Studies.fs`. `StudyFitSettings` gained
  `calibrationUse` (initially held fixed). A new `SetCalibrationUse` study command records it, and fit requests read the
  study's own setting.
- **2.5, the format.** The study file's `schemaVersion` is now 2; `study.schema.json` validates version 2 and requires
  the field. Per AGENTS.md's no-migration rule, a version-1 study file is the typed version error.
- **2.5, the window.** The window-only `studySurface.calibrationUse` and the `SetStudyCalibrationUse` message are gone.
- **2.3.** Remove only arms its question (`ExpRemove`, unknown ids stay inert). The question takes its own line under the
  row: "Remove this experiment and its file?" with `Yes, remove` and `No` (`experimentRemovePrompt`,
  `experimentRemoveConfirmButton`, `experimentRemoveCancelButton`). The Yes (`ExpConfirmRemove`) deletes the file behind
  the read-only guard and reports "Removed experiment 'X' and its file."; the No is `ExpCancelRemove`.
  - **Deviation from `003` 2.3.** The question is on a line of its own, not inside the row. Inline, it made the row wider
    than a 1000-px window, and the No could not be reached.
- **2.4.**
  - **The request.** A re-attach opens the wizard on the experiment's own profile and rows (`defaultProfile`,
    `defaultRows`), with a kit profile's own layout.
  - **The rows.** When the chosen mapping has a key column, the wizard offers "Every row" and one choice per key, with
    its point count (`importWizardAllRowsChoice`, `importWizardRowsChoice`); a key the mapping cannot read falls back to
    every row.
  - **The message.** `AttachDataFile` now carries a `DataFileAttachParam` (experiment, file, profile, rows), because
    four fields would exceed ELEV011's limit of three; the attachment keeps the chosen rows.
- **Tests (Phase 2).**
  - `KitProxyFolderTests`: a write of the bytes a file already holds leaves it and its write time untouched.
  - `SetupSaveLoadTests`: an FsCheck property over the two committed kits' setups. Activating any sequence of them, then
    writing the table back, gives the active setup.
  - `ImportWizardTests`: a re-attach opens on the experiment's profile and rows, the rows offered follow the profile,
    and the row choices render.
  - `KitWindowModelTests`: a re-attach request carries the experiment's profile and rows.
  - `ExperimentControlsTests`: Remove asks inline; No keeps the experiment, Yes removes it.
  - `StudyWindowModelTests`: the calibration choice is written into the study file and nothing else.
- **Deviation from `003` 2.1 verification.** The round-trip property runs in `OpticalConstructor.Ui.Tests`, not
  `constructor-unit-tests`. That project is where the committed-kit fixture (`TestAppScope.openCommittedReadOnly`) lives,
  so it now references FsCheck.
- **Found on the way.** The Plan step's Run plan cast its view to a `Border` and failed to render; it is now an action
  with an availability (`CommandControls.actionCommandWhen`). The table canvas was found by `Name` in `SceneInput` and the
  page-scroll lookup, which broke clicks and the wheel once the canvas carried only an AutomationId; both now use
  `ConstructorUiId.isCarriedBy`.

## Phase 3 — the table is the context

- **3.1 (D2).**
  - **The history.** Each kit-history entry records its kit's last active setup (`lastSetupOpt`; `withLastSetup`,
    `tryLastSetup`, kept by `bump`); `kit-history.json` is version 2.
  - **The memory.** The window holds a `KitSetupMemory` (`KitHistoryProxy.setupMemoryOf`), and the App's update
    remembers each change of the active setup (`rememberSetupSince`).
  - **Opening.** A kit window opens on `openedOnSetup`: the remembered setup while the kit holds it, else its last
    experiment's, else the default table with "This kit has no setup yet: …".
  - **Deviation.** The old history file is not offered for replacement on the start screen. The store already logs the
    version error and starts an empty history, which the next open rewrites.
- **3.2.** The Setup section (name field, Create setup, Save setup, Saved setups) is the top of the Setups step, above
  the ribbon. It left the Experiments bay. Each saved setup is a choice, chosen while active.
- **3.3.**
  - **ExpEdit.** It draws the experiment's own setup revision and says "Loaded E1 on its setup 'Bench', revision 1 of 2."
    when it is not the latest.
  - **ExpCommit.** When the table is still that revision (`editedOnItsRevisionOpt`), the experiment keeps its revision
    and no setup is written.
- **3.4.**
  - **The chart.** `chartForExperiment` runs over the experiment's own revision in both constructors. The Inverse gate
    keeps guarding only the draft preview.
  - **The measured points.** View ↗ adds them as one series on the right axis (`measuredSeriesOf`, a `ChartWindow`
    built with an initial style).
  - **The x axis.** The Review scatter and that series plot against the scanned value (`MappedPoints.chartPoints`), not
    the point number (also 4.2's x axis).
  - **No chart.** A View that opens nothing says why in the kit status.
- **3.5 (D3).**
  - **The stored label.** `ScanAxis.label`, the draft's `elementLabel` and `chooseElement`'s label are gone; the
    experiment file is version 2.
  - **The names.** A slot is named by `RoledSlot.slotName` ("Analyzer 1: LP2"), the chip's name without its angle.
    The bay's choices, scan lines, chart series, readout and descriptions all read it through
    `SetupSlot.elementLabelIn`; descriptions take the experiment's own revision (`experimentDescription`,
    `KitCopySource.describe`).
  - **Validation messages.** These now name a slot by its id ("slot 'sample' Rotation R1"): the Domain check has no
    items at hand.
- **3.6.**
  - **Reasons.** Add experiment, Add variable, Add block, Generate and Create study stay in place, withheld, with one
    reason line (`CommandControls.withheldReason`; `experimentAddReason`, `setupGridGenerateReason`,
    `createStudyReason`).
  - **Choices.** The bay offers only elements that have something to vary.
  - **The element list.** The Select bay opens with a `ListBox` of the table's elements, one named item each
    (`tableElementList`, `tableElementListItem`), which selects through `SelectTableElement`.
- **3.7.**
  - **The zoom.** The table opens fitted to its canvas (`OpticalTable.fittedView`).
  - **The canvas.** Elements are drawn with their kit item's label (`Drawable.caption`).
  - **The readout.** It names the slot, shows turns in [0°, 360°) (`Angle.turnCaption`) and labels the element's own
    scale "drawn 5.0× size".
  - **Deviation.** "On resize" needs nothing: the canvas has a fixed 820 × 560 size inside a scrolling page, so it does
    not change with the window.
- **3.8 (D7).** In a kit window a palette element is an empty slot whose chip opens at once.
- **3.9.** A note field beside Record experiment fills the template's `{note}` (`SetRecordNote`; `experimentTargets`
  takes the note). It clears after each record.
- **Tests (Phase 3).** `SetupModelTests` covers:
  - the opening setup, and its memory written only on a change;
  - an experiment's own revision drawn, and an unchanged Update writing nothing;
  - the record note;
  - the turn caption;
  - the empty palette slot.

  `KitHistoryTests` covers the remembered setup. The existing tests follow the new shapes: labels from slots, the
  versions moved to 2 (history, experiment, study), and preset binding made explicit where a kit window now adds an
  empty slot (`SceneTestFixture.withPresetElement`, the user's Choose…).
- **Found on the way.**
  - **The fitted zoom.** Applied to every scene, it broke the fixed-coordinate click tests, so it belongs to a kit
    window's open (`loadKit`).
  - **The workbench.** The Setup section's slot is always present, empty outside the Setups step, so Setups ↔
    Experiments still patches the workbench in place.
  - **The element list.** It is a tab stop itself (`ListBox.focusable`), as the contract asks.

## Phase 4 — the Data and Inverse steps can be read

- **4.1.** Each data-file row leads with "E1 [Science] — <badge>" (`dataFileHeading`); the file name beside the verbs
  wraps.
- **4.2.**
  - **Placement.** Review opens directly under its row and is scrolled into view once laid out (a dispatcher post after
    attach).
  - **The axis.** The scatter plots against the scanned value (see 3.4).
  - **The reason.** A recorded exclusion clears its reason.
- **4.3.**
  - **The pages.** Unknowns, Data, Items, Plan, Fit, Results and Publish lay out at the window's width in a
    `MinWidthPanel` (520 px minimum), so chips, the experiments line and text wrap.
  - **The inner scrollers.** The Unknowns parameter grid, the experiment matrix and the wizard's preview
    (`importWizardPreviewScroller`) scroll sideways in their own scrollers.
  - **The ribbon.** Library… and Materials… sit on the wrapping tab strip (`Ribbon.ribbonViewWith`).
  - **The width law.** `ScrollExtentTests.assertReachable` gains it. At 1000 × 540, no page scroller is wider than its
    view; a control's own template scroller and a named `…Scroller` are not pages. A failure names the widest controls.
  - **Deviation.** Items, Plan, Fit, Results and Publish were not on `003`'s list, but the law found them wider than
    their window, so they took the same treatment.
- **4.4.**
  - **Layout.** The item mapping is a section of its own beside the experiment list, each in a 160-px scroller with
    its heading inside, so the kit bar grows no taller.
  - **Names.** Each source item names its model "<name> v<n>" through the window's item models overlaid with the
    source kit's snapshots (`KitCopySource.sourceItemModels`, `modelCaptionOf`).
  - **After the copy.** The window's stores take the kit's overlay anew, so a copied item resolves at once.
- **4.5.** A run already published says "Published: every item this fit run calibrates holds its values." instead of
  the red refusal beside "Published …".

## Phase 5 — messages, dialogs and import questions

- **5.1.**
  - **The type.** `kitStatus` is a `KitStatus option`, `KitStatusDone` or `KitStatusRefused`, drawn in the text colour
    or firebrick. The 80 sites are classified by what they report.
  - **Keeping the last message.** The silent `kitStatus = None` successes are gone, so a step that says nothing keeps
    the last message; the initial value and the content load keep theirs.
  - **New messages.** Reload names the files it read again (and says nothing when nothing was offered). Add item says
    what it added. Remove says what it removed (2.3).
  - **Tests.** They read the text through `kitStatusCaption`; checks that meant "nothing went wrong" read
    `kitRefusalOpt`.
- **5.2.**
  - **D5.** `AppContext.create` and `recreateLibrary` make the Kits, Exports and Catalogues folders.
  - **The kit window's file dialogs.** Data file, split and photo remember their folder per purpose (`measured-data`,
    `item-photo`) in the environment file. Without one, a data file starts in the kit's `data/` and a photo in the kit
    folder. They filter "Lab data (*.csv, *.txt, *.tsv)" plus All, or images.
  - **The catalogue dialogs.** Register catalogue folder… and Import glass catalogue… start in the Catalogues location.
  - **The folder question.** It titles each kit-folder dialog (`KitFolderQuestion`); the unpack target asks "Choose an
    empty folder to unpack the kit into".
  - **Deviation.** The Library and Materials dialogs start in Catalogues every time but remember no last folder: those
    windows have no environment seam, and spec 0050 S3.3 makes Catalogues their folder.
- **5.3 (D6).**
  - **The rule.** A start-up row with no alternative settles itself, and so does a new entry of the shipped
    catalogue's first import (`ImportDecision.questionOn`). A catalogue whose rows all settle themselves is applied on
    start (`PendingCatalogueImport.unaskedChoicesOpt`), and the Launcher says how many entries it imported.
  - **What still asks.** A reopened kit whose rows are all reuses opens without the dialog. The dialog lists only the
    rows that ask, with a count of the rest. The Library window says how many entries wait.
  - **Deviation.** The Materials window has no pending list to read, so it says nothing. With the first import applied
    on start it is no longer empty for that reason.
- **5.4.**
  - **The import heading.** A dropped file's import is headed by it: "Import 3 glasses from test-glasses.agf.", or
    "… entries from …".
  - **Export my library….** With no entries of its own, it says so and writes nothing.
  - **The split row.** It reads "<key>, <n> point(s) → E1".
- **5.5.** The Items panel shows the photo itself as a thumbnail (160 × 120 at most, `itemPhotoImage`).
  - **Deviation.** The view decodes the photo file itself, at its edge. A file that does not decode shows its path
    alone. The reason: no proxy reads kit images, and the Items step is the only reader. A cost: the decode runs on
    every render of the selected item.
- **5.6.**
  - **The times.** The history shows local times.
  - **Deviation.** Every row already shows its folder under the kit's name, so two kits of one name differ already.
- **5.7.**
  - **The program.** The App is a `WinExe`.
  - **The log.** It goes to `optical-constructor.log` in the Preferences location, redirected as soon as the locations
    are read.
  - **The environment file.** It is `DataLocations.environmentFile` in the Preferences location; the `%APPDATA%` path
    is no longer read.
  - **Deviation.** The log lives in the Preferences location: the plan adds no new location, and the Data locations
    window already shows that one.
- **5.8.**
  - **The dialog.** Export PNG… and Export CSV… each open a save dialog in Exports (the shell's `exportsFolderOpt`) and
    say what they wrote, or why not, beside the buttons.
  - **The fixed files.** The `%TEMP%\experiment-chart.*` files are gone.

## Closing the suites

### The layout law at 800 × 600 (`003` 4.3)

`ScrollExtentTests` found rows that a kit window beside its flow strip cuts off. Each one now fits the page:
- **They wrap.** Fit's three runs, Plan's run row, and the Experiments bay's action row, each experiment row, and its
  Remove question.
- **The verb docks beside wrapped text.** A Plan candidate row docks Add to kit on the right, beside a caption that
  wraps.
  - At 1000 px, Add to kit used to sit past the page's right edge, so `PlanViewTests`' press never reached it.
- **They scroll in their own scroller.** The Items parameter grid, the Results tables, and the Main table's
  fixed-size canvas scroll sideways, never the page. The table page's minimum width is now 520 px, not the canvas
  width.

### Tests brought to the new behaviour

- **D6.**
  - **Start-up catalogues.** The Launcher test now proves that a fresh library and an all-reuse catalogue settle on
    start: the settled notice shows and no dialog opens. Only the mixed catalogue asks, listing the rows that ask and
    counting the rest.
  - **Reuse-only kits.** A kit whose snapshots are all reuses opens without a question.
  - **Recreate library.** A recreated library holds the shipped catalogue.
  - **The composed app.** On a new library it opens no import dialog.
  - **`AppContextTests`.** Each first start now asserts that the shipped catalogue is settled, held and logged once,
    with nothing pending (`assertShippedFirstStart`), where it used to settle a pending catalogue by hand.
- **5.4 wording.** "Import 3 entries from 3 files.", the split row "K1, 2 point(s) → E1", and Export my library's
  status, not a refusal.
- **Layout assertions.** They expect the WrapPanel and DockPanel rows. The Results test counts a table's own grids, not
  its scroller's template grid. The reopened-kit check reads the saved setups on the Setups step (3.2).
- **Pressing settles first.** `UiDriver.settlesOn` renders until a press's centre hit-tests to the control. Every
  `pressAtCentre` uses it, and so does `StudyViewTests`, whose own copy of the loop is deleted. This fixed the kit
  window's chip-swap test, whose press landed on a frame that trailed the scroll.

### Tests the plan's rows lacked

Three closed rows had no test of their own. They have one now:
- **3.6.** `ExperimentControlsTests`: no element but the tunable source is offered.
- **4.5.** `StudyViewTests`: after Publish calibration, the preview says "Published: every item this fit run calibrates
  holds its values.".
- **5.5.** `ItemsPanelTests`: the photo shows as a thumbnail, and a missing photo file shows its path alone.
  - Headless drawing stubs every bitmap, so a missing file made an image too. The view now checks the file exists
    before decoding it, which holds on every platform.

### The reference kits (D3, D4, D7)

- **Rebuilt.** AT-1 and AT-2 rebuilt both kits, and the builds were mirrored into `data/` byte for byte, `.oc/` aside:
  96 files changed in `optics-mueller` and 11 in `absorbing-biaxial-noisy`.
- **What changed.**
  - Experiment files are schema 2, with no stored axis label: the stale "Linear polarizer #6" is gone.
  - Studies are schema 2 and carry `calibrationUse`.
  - Each kit lost one library snapshot, the palette preset that palette adds used to bind: "Ideal linear polarizer"
    and "Intensity detector". With D7 nothing references them.
- **AT-2's expected snapshots** no longer list the detector preset.

### Found by the desktop survey

- **Copy experiments… named preset and sample items by GUID** ("Laser — 239b0cfb-… v1"). `003` 4.4 had fixed only the
  bare-model case.
  - The fix: `ItemModelInstance.itemModel` answers the model version in every case, and `KitCopySource.modelCaptionOf`
    names it by its name and version through the source kit's snapshots.
  - `KitWindowTests`' copy-panel expectation, which had pinned the GUID text, now expects the name and version.
- **A quiet start wrote no log file.** The file was created only by the first line logged. The App now logs its start
  line, with its version and the Preferences folder.
- **A crash would leave no trace.** A windowed program has no console, so the App now also logs an unhandled
  exception before it stops (`AppDomain.UnhandledException`). It came up while the photo fact was diagnosed; the app
  had not crashed.

### The real-desktop suite (`003` 1.6, Phase 6)

- **Shared helpers.** `DesktopSession` now holds `openKit`, `shownText` and `trimmed`, moved there from
  `DesktopTests`. `openKitSeeing` also answers the import dialogs it applied.
- **Typing into file dialogs.** `typeIntoNativeDialog` types the path. A file dialog's name box is a combo box, which
  never raises its edit-change notice for WM_SETTEXT, so a save dialog kept its suggested name.
- **Test fixes.**
  - The shell-owner query selects `Win32_Process`'s key `Handle`, without which `GetOwner` cannot run.
  - The wait for a closed kit window matched the Launcher's own title, "Optical Constructor — Launcher".
  - The element list's items are found as list items.
  - The chart window is found by its export button, since it is titled by its chart.
- **One desktop at a time.** Every fact is in one xUnit collection (`DesktopSession.RealDesktop`).
  - With two classes, xUnit ran them side by side. Two apps then shared one desktop's focus and the app copy's
    `appsettings.json`.
  - An assembly-level `CollectionBehavior(DisableTestParallelization = true)` reached the assembly but did not stop
    it, so it was removed.
  - `Dispose` waits for the app to exit, and the settings write retries while a scanner holds the fresh file.
- **A stale window after a dialog.** A window's UIA element can be disconnected once a file dialog closes
  (RPC_E_DISCONNECTED), with the app still running. A fact that goes on after a dialog finds its window again.
- **New: `DesktopSurvey.fs`.** Six facts re-drive the 038 and 002 survey by id, each naming the findings it closes.
  The report is `005-ui-survey-after-the-fixes.md`.
- **AGENTS.md** names the suite in Build & test and in the project layout.

## After the survey: the chart's Polar view (operator report, 2026-11-27)

The report: open `optics-mueller`, Experiments, any experiment, View. The chart shows two lines, "Intensity" (blue) and
"Measured" (orange). Polar view keeps only the measured line, and back in XY the intensity lies at 0.

### What the two lines are

- **Intensity** is the model's prediction. The experiment's scan is run on its own setup revision
  (`chartForExperiment` → `chartForRun`), and each point is the S₀ the detector reads, relative to a unit source.
  The scale is therefore 0 to 1, on the left axis.
- **Measured — <experiment>** is the experiment's attached lab data, the kept points with exclusions removed
  (`measuredSeriesOpt`). It is in the lab file's own units, counts for `optics-mueller`, on the right axis (`003` 3.4).

### Why the Polar view lost the intensity

A polar grid has one radius, and `ChartStyle.polarRadius` took the largest |y| of every visible series. That is the
measured counts, in the thousands, so the intensity curve was drawn within a thousandth of the radius: a dot at the
centre.

### Why XY then drew the intensity at 0

ScottPlot 5.1.59's polar axis changes the plot beyond its own plottable, and `Plot.Clear` undoes none of it:

- Its first render adds a `SquareZoomOut` axis rule, which keeps the bottom and left axes at equal units per pixel.
  Back in XY, a 0–180° bottom axis held the left axis at about ±72, so a 0–0.45 intensity was a flat line at 0. The
  right axis is outside the rule, so the measured line looked right.
- `Add.PolarAxis` hides every axis, the title and the grid. The window showed only the bottom, left and right axes
  again, so the top frame edge and the title stayed hidden, and the grid stayed off with "Major grid" ticked.

The regression test of spec 030 missed this because Avalonia.Headless never rasterizes ScottPlot. The polar axis was
never rendered, so the rule was never added, and the test compared the limits the code had set, not those a render
draws.

### The fix

- **`ChartStyle.polarChartOf`** replaces `polarRadius`. It answers the polar plot's radius and its series:
  - the radius is the left axis's largest visible |y|;
  - every right-axis series is scaled onto that radius, as XY gives each side its own scale, and its legend reads
    "(scaled to the left axis)";
  - with nothing visible on the left, the right axis keeps its own units.
- **`ChartWindow`**:
  - back in XY, `restoreCartesianFrame` clears the axis rules (the window adds none of its own) and shows every axis;
  - the title is shown in both views;
  - the grid follows its two toggles in XY and stays off in polar (`setGridLines`);
  - a style edit in the polar view rebuilds it (`restyle`), since the radius and the scaling follow which series
    show and on which side.

### Tests

- `ChartStyleTests`: the radius test reads `polarChartOf`, and a new test checks that a right-axis series is scaled
  onto the left axis and keeps its units when the left side is hidden.
- `ExperimentControlsTests`:
  - the round-trip test now opens the View chart's shape (intensity left, counts right) and renders each view. It
    checks that the limits come back to nine decimals, no rule is left, and every axis, the title and the grid show;
  - a new test checks that both polar series reach the same radius, and that moving the measured series to the left
    axis redraws it in its own units;
  - with the fix taken out, both tests fail.
- `DesktopSurvey`, a seventh fact on the real app: View on `optics-mueller`'s first experiment, then Polar view and
  back. Both series' colours must be drawn in the polar view, and the XY plot afterwards must equal the first one
  pixel for pixel.

### Found while proving it

- **The chart's plot was not in the UI Automation tree.** The window is plain Avalonia, so the plot had only an
  Avalonia `Name`, and UI Automation did not expose it. `ConstructorUiId.exposeAsFigure` is `figureAttributes` for a
  control built outside FuncUI; the chart window's plot now carries it.
- **Measured points were drawn in capture order.** `lp_lp.csv` captured `LP-(AIR#0)-LP-2` at dial
  60, 40, 20, 0, 340, 320 … 80, 60, so XY drew a straight line from 0° to 340° across the chart. This was not part of
  the reported bug, so it went to the operator as a question, with two remedies: dots without a line, or the points
  sorted by x. The operator ruled three times, the last ruling standing; see "The measured line within one turn" below.

### Results

- Solution build: 0 errors; the only warnings are the exempt NuGet advisories (NU1701, NU190x).
- ELEV gate: OK, 0 findings in 13 projects, run after the last product edit.
- `OpticalConstructor.Ui.Tests`: `ChartStyleTests`, `ExperimentControlsTests`, `ScrollExtentTests`,
  `AutomationContractTests` and `NkDispersionChartTests`, 125 of 125 passed.
- Real desktop: the two View facts, 2 of 2 passed, the new one again after the final build.
  - Without the scaling, the polar view drew 20 px of the modelled series (its legend swatch) beside 1,810 px of the
    measured one.
  - Without clearing the rule, 50,611 of 422,688 plot pixels differed from the first XY view.
  - With the fix, the polar view draws both series, and the XY view after the round trip equals the first.

## The measured line within one turn (operator rulings, 2026-11-27)

The operator ruled three times on how the measured line is drawn; each ruling replaced the one before.

1. Sort the measured points by x. Built, then replaced: a run read at 0, 20 … 340, 0 ends at 360, not back at 0.
2. Unwrap the readings point by point, so a run may take several turns. Built (`ChartSeries.unwrappedAngles`), then
   replaced. The lab's runs turn the dial down, from 60 through 0 round to 60, so the unwrapped run covered 60–420°
   beside the model's 0–360°.
3. The ruling that stands:
   - every angular chart's XY view spans 0–360°;
   - the measured line may go round 0 or 360, but it is drawn within 0–360°: unwrapped, brought back into one turn
     (mod 360), then sorted by x;
   - the polar view draws the data as read.

How it is built:

- **Mod 360 alone.** Unwrapping adds whole turns, and mod 360 takes them away again, so unwrapping and then taking
  mod 360 gives back each reading mod 360. The XY view therefore applies mod 360 and the sort directly, and
  `unwrappedAngles` is deleted.
- **`ChartSeries.inOneTurn`** moves an x outside 0–360° into [0°, 360°) by whole turns, then sorts the points by x.
  The sort is stable, so two readings at one angle keep their capture order. An x inside the range, 360 included,
  stays where it is, so the model's last point stays at the right edge; a strict mod would move it to 0.
- **`ExperimentChart.xyViewSeries`** is every series of an angular chart in one turn, and any other chart's series as
  they are. `ChartPlot.buildCartesianScatters` draws it, so the chart window and the embedded host share it.
- **`ChartStyle.chartBounds`** is the bounds the XY view fits. An angular chart's x is 0–360° without padding,
  whatever its data covers. `defaultChartStyleState` and Auto (`ChartPlot.dataBoundsVisible`) both read it, and a
  pinned range still wins.
- **The chart data stays as read.** `measuredSeriesOpt` returns the kept points in capture order. The polar view
  draws them so: the lab's run is a closed loop.
- **Which charts count as angular.** Every scan variable but the wavelength is an angle (`AngularXAxis`), so
  incidence (R2) sweeps of 0–90° are drawn on 0–360° too, as the ruling's "all" says.
- **What the lab's runs look like.** A reading of 0 is drawn at 0, so a run's closing reading lands beside its first
  (60 twice for the LP runs). The measured line covers 0–340°, and nothing is measured between 340° and 360°.

### Tests

- **`ChartStyleTests`**: the unwrap tests are replaced by four tests.
  - The lab's run sorts to 0 … 340 within one turn; the two 60° readings keep their capture order, and the chart
    itself keeps the run as read.
  - 380, −20, 720, 360 and 0 come to 20, 340, 0, 360 and 0.
  - A non-angular chart is drawn as it is.
  - An angular chart's x range is 0–360°, in the default style and in `chartBounds`; any other chart fits its data.
- **`ExperimentControlsTests`**:
  - The View chart fixture now has a model over a full turn and a measured run in capture order, from 90 through 0 and
    round to 90.
  - A new test renders the window. In XY the bottom axis is 0–360° and the measured line is sorted; in polar the
    measured points lie at the captured angles in capture order.
- **`InverseConstructorTests`**: the measured series is the readings as read, in capture order.
- **Checked without the fix:**
  - with `buildCartesianScatters` drawing `chart.series`, the new window test fails;
  - with `chartBounds` fitting the data, the range test fails.

### Results

- Solution build: 0 errors; the only warnings are the exempt NuGet advisories (NU1902).
- ELEV gate: OK, 0 findings in 13 projects, run after the product edits.
- `OpticalConstructor.Ui.Tests`, all passed:
  - `ChartStyleTests` 24 of 24, `ExperimentControlsTests` 63 of 63 and `InverseConstructorTests` 16 of 16;
  - `DataStepTests` 13 of 13, `EmbeddedChartTests` 6 of 6, `NkDispersionChartTests` 14 of 14 and
    `ScrollExtentTests` 24 of 24.
- Real desktop: the two View facts, 2 of 2 passed. On `LP-(AIR#0)-LP-2`:
  - the XY view spans 0–360°, with the model across it and the measured line from 0° to 340° in dial order;
  - the polar view draws the run as captured, a closed loop.
