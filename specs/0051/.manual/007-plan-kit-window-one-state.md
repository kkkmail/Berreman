# 007 — Plan: the kit window, one page and one state at a time (the operator's review `006`)

2026-11-27. The operator reviewed the app after `005` in fifteen comments (`006-task.txt`), ending "Write a plan how to
implement all that into the next NNN MD … Make no other changes." This is that plan. It is a plan for discussion, not a
spec and not a ruling. No product file was changed.

- **Input.** `006-task.txt`. Its comments are written `006-1` … `006-15`. Where one comment makes several demands, they
  are lettered (`006-1e`) as the traceability table at the end lists them.
- **Shape.** Ten phases, in the order to implement them. Phase 1 builds the page rules every later page follows, as
  `003` Phase 1 built the command controls.
- **Answers first.** `006-5`, `006-6`, `006-7`, `006-8`, `006-9` and `006-14` ask what something is or how to do it.
  "What the pages are for, today" answers them now, from the code. Phase 9 puts the answers into the app.
- **Decisions.** The choices only the operator can make are under "Decisions for the operator", each with a recommended
  default that the arc applies unless the operator rules otherwise.
- **Spec readiness.** As in `003`: each phase names its wiring, minimum scope, negative scope, reuse, verification
  surface and migration intent.

## What the pages are for, today

- **The experiment matrix (Data, `006-5b`).**
  - `ExperimentMatrices.ExperimentMatrix.ofExperiments` places each experiment by the slots of its setup revision, in
    light order. A row is the items of the Prep slots and each object at its angle. A column is the analyzer items.
  - `LP1 · AIR@0` is the row "Prep: LP1; object: AIR at 0°". The column heads are the analyzers. A cell lists the
    experiments measured there.
  - Nothing on the page says this, and the row key is printed as one string. That is why it reads as noise.
- **Unknowns (`006-6b`).**
  - A *study* is one fit problem: a named set of the kit's experiments fitted together (`Studies.fs`).
  - Its unknowns are the parameters marked Unknown on the items those experiments use, plus the parameters of the
    materials the study opens. Each unknown has a scope (one value per item, material version, session or experiment)
    and may be linked to another (`CPL2.δ = CPL1.δ`).
  - The chips under "New study" are *experiments*, not setups. In `optics-mueller` the naming template names each
    experiment after its setup, so the two look alike.
  - The page shows the chosen study and the new-study form at once: the mixed state `006-6c` objects to.
- **Plan (`006-7`).**
  - It runs the Measurement Planner (`MeasurementPlanner.fs`, spec 0050 S14). For the chosen study it says which
    unknowns the kit's experiments can recover (rank, confounded combinations, each target's σ). It ranks the
    experiments the kit could add to recover more, and Add to kit creates one.
  - It needs a Mueller polarimeter (or a polarizer–sample–analyzer arrangement read as one) and a crystal material opened
    in the study. `optics-mueller` has neither, so its Plan page can only say "no candidate".
- **Fit (`006-8`).**
  - It runs the chosen study. "Before the fit" checks that each experiment's data reads.
  - The mode is joint (every unknown at once) or staged. Stage 1 calibrates the instruments on the reference runs, such
    as AIR. Stage 2 fits the objects with that calibration held fixed or used as priors.
  - Each run is written as an append-only `<study>@<stamp>.fitrun-json`, and the "Fit runs" list is those files. A run
    is created by pressing Run. Results and Publish read the run chosen in that list.
- **Results.** The chosen run's values, σ, correlations and diagnostics, in six tabs.
- **Publish.** Writes the run's calibrated item parameters as each item's next revision, and promotes a fitted material
  to a new library version.
- **An inverse problem today (`006-9`)**, in the order of the flow strip:
  1. Items: add the instruments and samples, and mark the parameters to fit as Unknown.
  2. Setups: make one setup per arrangement measured.
  3. Experiments: make one experiment per measured run, over its setup.
  4. Data: attach each run's lab file through an import profile, or split one file by its key column.
  5. Unknowns: create a study of the experiments to fit; set scopes, links and sessions.
  6. Plan (optional): see what the kit can recover.
  7. Fit: run the study.
  8. Results: read the fitted values.
  9. Publish: write the calibration back into the items.
- **A direct problem today (`006-14`).**
  1. Items: add a source, a polarizer, a sample and detectors.
  2. Setups: build the table and create a setup.
  3. Experiments: choose what to vary and press Add.
  4. Press View ↗ for the chart.

  The steps exist. Nothing in the app says them, and the pages mix them with the inverse ones.

## Phase 1 — The rules every page follows

Closes: `006-1e` to `006-1l`, `006-11`, `006-12`, and the status part of `006-5a` and `006-6a`.

### 1.1 What is wrong now

- **Everything at once.** Every page shows its list, its details and its creation form together.
  - Items: the add form above the list, the selected item below it.
  - Setups: a name field, Create setup, Save setup, the saved-setup chips and the table.
  - Unknowns: the study chips, the New study form and the chosen study.
- **Details far from their entry.** Details open below the whole list: Items' `selectedItemRows`, the Data review
  under every Data files row, and the Unknowns parameter editor at the page's end.
- **Edits write at once.** Nothing is saved or cancelled:
  - an item's label is written on losing focus (`RelabelKitItem`), and so are its notes;
  - a parameter's Set writes at once;
  - the study's Scope and Link combo boxes are always live (`SetParameterScope`, `LinkParameters`);
  - an exclusion writes at once.
- **No unselect.** A chosen entry stays chosen when pressed again (`SelectKitItem`, `ActivateSetup`, `SelectStudy`).
- **One status for every page.** `kitStatus` is one line for the whole window, shown in the kit bar and again by
  `studyBarView`. So "Loaded LP-(AIR#0)-LP-2 on its setup 'LP-(AIR#0)-LP'." from the Experiments page is still shown
  on Data and Unknowns. This is half of `006-5a` and `006-6a`; 7.1 is the other half.
- **Nothing can be copied.** Text is `TextBlock`, which cannot be selected. Grids (the Data matrix, the Unknowns grid,
  Plan's bounds, Results) have no gridlines and no copy.
- **No tooltips or help.** No button has a tooltip (`ToolTip` is used only by two table badges), and no page says what
  it is for.

### 1.2 One page, one state

- **The three states.** A list page is in exactly one of them:
  - **Browsing:** the list, with at most one entry chosen, its details open directly under its row;
  - **Editing one entry:** its editor in place of its details, under its row. No other entry can be chosen, and New is
    not offered;
  - **Creating a new entry:** only the page header and the creation form, directly under New. The list is hidden.
- **The transitions.**
  - Pressing an unchosen entry chooses it: it turns green and its details open. Pressing the chosen entry unchooses it:
    it turns grey and its details close. This is the `ChoiceToggle`'s own toggle, so UIA reads it (`003` 1.2 item 5).
  - Edit, on the chosen entry's line, enters Editing.
  - **Save is withheld until the draft differs from the entry** (`draft <> original`, the comparison `003` 2.1 made
    exact for setups). Save writes once, then returns to Browsing with the entry chosen.
  - Cancel with an unchanged draft returns at once. With a changed draft it asks first, in place: "Discard your changes
    to X? Yes / No", as Remove asks (`003` 2.3).
  - **Leaving counts as Cancel.** Leaving the page, choosing another entry, switching Main / Inverse or closing the
    window with a changed draft asks the same question first (D11).
  - New enters Creating. Create writes, then returns to Browsing with the new entry chosen. Cancel works as above.
- **Where it lives.**
  - **The state type.** One generic, pure state type in `OpticalConstructor.Controls`, with a mode of
    `BrowsingEntries | EditingEntry | CreatingEntry`. The window model holds one state per page.
  - **The view builder.** One builder draws:
    - the rows;
    - the chosen entry's details slot;
    - the Edit / Save / Cancel line;
    - the discard question.

    Each page supplies its rows, its details view, its editor view and its draft comparison.
  - **The messages.** They are generic, each carrying its page: toggle entry, begin edit, save edit, cancel edit, confirm
    discard, keep editing, begin create, commit create. A test then drives every page the same way.
  - **Names.** All of these are placeholders. The final names must be unique (ELEV014–016).
- **Drafts, not writes.**
  - Every editor edits a draft held in the model, and nothing is written until Save.
  - One Save is one write: one revision of an item, a setup or a study, through the KitProxy no-op rule (`003` 2.2).
- **Reuse.** The existing editors become the editor views:
  - `parameterEditorView`, the `ExperimentControls` editor, the workbench table;
  - the 2.3 inline question.

  Their direct writes are replaced, not kept beside the new path.

### 1.3 Dynamic buttons: context menu and rename (`006-12`)

- **The definition.** A *dynamic button* is an entry toggle whose caption is built from kit data:
  - an item's `label — model — revision`;
  - the name of a setup, an experiment, a study or a fit run.
- **One builder, one menu.** One builder makes them, with a `ContextMenu` of `Copy name` and, where the entry can be
  renamed, `Rename…`.
  - Shift+F10 and the menu key open the menu.
  - Each item sends a semantic message.
- **Copy name.** It writes the caption through a new `ClipboardProxy` seam of the shell. Under the AGENTS proxy pattern,
  a headless test reads what was copied, and the App's proxy calls `TopLevel.Clipboard`.
- **Rename is Editing with only the name field,** shown in place of the caption. Save is withheld until the name is
  new, valid and free. Edit offers the same field. What each rename does:
  - **An item:** `RelabelKitItem`, as today, which renames its file.
  - **A setup:** new. It writes `<new>.setup-json`, re-points every experiment that references any revision of it,
    updates `setupMemory`, then removes the old file.
  - **An experiment:** new. It renames its file and re-points every study that lists it.
  - **A study:** only while it has no fit run, because fit runs are append-only files named by their study. Otherwise
    the menu item is withheld with that reason (D13).
  - **A fit run:** Copy name only.
- **A rename that touches several files** writes every new file before deleting any old one. An interrupted rename then
  leaves a kit that still opens, which Reload changes sees.

### 1.4 Copyable text and tables (`006-1e`, `006-1f`)

- **Copyable text.**
  - Every text that shows kit or model data is a `SelectableTextBlock`, which can be selected and copied with Ctrl+C,
    made by one builder. Static labels such as "Naming template:" may stay `TextBlock`.
  - The rule the gate checks: a text block that carries a `ConstructorUiId` is selectable. `SelectableTextBlock`
    derives from `TextBlock`, so UIA should still see it as text; Phase 1's desktop facts confirm that.
- **Tables.**
  - One grid builder draws a table with gridlines and a header row. Its `Copy as CSV` button and menu item copy the
    table: RFC 4180, invariant culture, header first.
  - Used by:
    - the Data matrix;
    - the Unknowns grid and its sessions table;
    - Plan's bounds;
    - every Results tab, the correlation matrix as a square table;
    - an item's parameter grid.
  - The chart window's `ExperimentChart.toCsv` and this builder share one CSV cell writer, moved, not copied.

### 1.5 Tooltips and Options (`006-11`)

- **Every static command has a tooltip.** A static command is one with a fixed `ConstructorUiId` and a caption that
  never changes.
  - The tooltip is the caption as its title, then one or two sentences on what the command does and what it changes:
    the MS Word ScreenTip shape.
  - The same text goes into `AutomationProperties.HelpText`, which UIA and screen readers read.
- **The text cannot be left out.** It is a required argument of the static builders, so a static command cannot be
  built without it.
  - A withheld command's tooltip adds why it is withheld, from the reason its page already has (`withheldReason`).
  - A dynamic button's tooltip is its full caption, since captions are clipped at 300 px.
- **The delay.** One application style sets `ToolTip.ShowDelay` on every control, 1000 ms by default.
  - It is a new elevated `Preferences` field in `environment.json`, in milliseconds from 0 to 5000.
  - The style is rewritten when the option changes, so a change applies at once.
- **Options.** The Launcher's footer gains `Options…` beside Data locations. It opens a single-instance Options window,
  which holds only the tooltip delay for now and follows 1.2: Save withheld until changed, Cancel asks.
- **`environment.json` stays schema-validated.** Its schema gains the field. An older file fails validation and `load`
  falls back to the defaults, `UserEnvironment`'s existing no-migration rule.

### 1.6 Help on every page (`006-1l`)

- **Where.** Every page of the kit window, and every window, has a `?` Help toggle at the right of its header.
- **What it says.** It opens a short panel at the top of the page:
  - what the page is for;
  - what must exist before the page can be used;
  - what to do next.
- **One module.** The texts live in one module, one entry per page, so a test can check that every page has one.
- **The guides.** `Guide…`, on the Launcher and on the Kit page, opens the two step-by-step guides (Phase 9).

### 1.7 A status belongs to its page

- `KitStatus` carries the page that raised it. A page shows only its own status, in its header, and keeps it until its
  next action. `038-7` stays closed: leaving a page and returning still shows its status.
- The kit bar's copy and `studyBarView`'s copy go. The Kit page shows the kit-level messages (Send kit, Reload, Copy).
- The reload offer (files changed outside the app) stays a banner on every page, because it concerns whatever is shown.

### 1.8 The gate

- **`AutomationContractTests` gains four rules,** over every window and every kit page in each of its states:
  - every command with a fixed id has a non-empty tooltip, equal to its HelpText;
  - every text block with an id is selectable;
  - every entry toggle has a context menu whose first item is Copy name;
  - every page has a Help toggle with text.
- **The state machine is tested without a window.**
  - From every state, every message leads to exactly one state.
  - Save with an unchanged draft is withheld.
  - Cancel, a page change and an entry change each ask when the draft has changed.
  - FsCheck over random message sequences, through `Check.One` in a fact.
- **`ScrollExtentTests` covers each page in each of its three states,** at 800 × 600 and 1000 × 540.
- **The real desktop.** A fact hovers a command and reads its tooltip after the delay, and right-click → Copy name puts
  the caption on the clipboard.
- **Migration intent.**
  - `environment.json` gains one field.
  - Writing once per Save changes how many revisions a build mints, so AT-1 and AT-2 rebuild the reference kits and
    the change is re-committed (Phase 3 rebuilds them anyway).
  - Tests of retired behaviour are rewritten to the new behaviour, not deleted, so the `count_at_least` baselines hold.

## Phase 2 — The kit window's frame

Closes: `006-1a` to `006-1d`.

- **2.1 One row of page buttons.** The flow strip leaves the left side and becomes the window's top row, left to right:
  - Main: Items · Setups · Experiments · Inverse · Kit
  - Inverse: Items · Setups · Experiments · Main · Data · Unknowns · Plan · Fit · Results · Kit

  Details:
  - **The mode switch.** `Inverse` / `Main` is `SwitchConstructorMode`, captioned by the constructor it opens; "Switch
    to" and the "Main constructor" text go. The window title, which names the kit and its folder, also names the mode.
  - **Wrapping.** The row wraps at the window's width: a `WrapPanel` docked at the top, never inside a horizontal stack
    (AGENTS.md).
  - **The step type.** `ConstructorFlowStep` gains a step for the Kit page and loses `PublishFlowStep`, whose content
    moves to the Kit page.
- **2.2 The Kit page (D1, D2).** It shows when Kit is pressed, under the row like every page:
  - the kit's name, folder, created and last saved, from `.optics-kit` (Phase 3);
  - Send kit…, Show in Explorer, Copy experiments from another kit…, Reload changes, Close kit;
  - the naming template, read-only with Edit in place, under 1.2's rules;
  - Publish: the kit's fit runs as entries. Choosing one opens its preview, and Publish calibration / Promote, under it
    (today's `StudyPublishView` content, same messages);
  - Guide… (Phase 9).

  The kit bar above the pages goes. No other page shows any of it, except the reload banner and a running Send's
  progress line.
- **2.3 The table's buttons only where the table is.**
  - The ribbon (Add, Rotate, Move, Render, Details) shows only on the Setups page, while a setup is edited or created
    (Phase 5).
  - The Experiments bay leaves the ribbon (`BayNames.allBayNames`): `006-1d`.
  - Library… and Materials… leave the ribbon's row. The Items page is the only place a kit chooses from the library
    (`006-2`), and the Launcher still opens both windows.
  - The Select bay leaves kit windows (D9). A slot holds a kit item, put there by its slot chip; binding a library entry
    is the Items page's job. The rotation test windows keep their own bars.
- **Verification.**
  - Headless: `KitWindowTests`, `MainWorkbenchTests`, `RibbonPaneTests`, `AutomationContractTests` and
    `ScrollExtentTests`.
  - Real desktop: the desktop facts press every page button of both constructors by id.
- **Migration intent.** None stored; ids of retired controls are deleted with them.

## Phase 3 — The kit is a file: `.optics-kit`, and the last folder

Closes: `006-15`, `006-10`.

- **3.1 What exists.**
  - A kit folder is already marked by a dotted file: `.kit-json` (`KitFile.fileName`, an empty stem plus `.kit-json`).
  - It holds `created` and `modified`, beside the kit's id, name, author, notes, send excludes, naming template and
    import profiles.
  - `modified` is stamped only when that file itself is saved (`KitContext.saveKitFile`), never when an item or an
    experiment is written.
- **3.2 The change (D3, recommended).**
  - The kit file *is* the marker. `.kit-json` becomes `.optics-kit`.
  - `modified` becomes `lastSaved`, stamped by every kit write that changes a file. The stamp is made at the KitProxy
    seam, so a no-op write stamps nothing (`003` 2.2). `created` stays.
  - One file then marks the kit and carries both stamps, and nothing is copied into a second file.
  - The alternative: a separate `.optics-kit` holding only `created` and `lastSaved`, the two fields moved out of
    `.kit-json`. Every kit is then marked by two files, and every write touches both.
- **3.3 Opening a kit is opening its file.**
  - Open kit… on the start screen, Copy experiments from another kit… and Locate… become file dialogs, filtered to
    "Optics kit (*.optics-kit)". The kit is the file's folder.
  - New kit… stays a folder dialog, since a kit is made in a folder, and writes `.optics-kit`.
  - A folder without `.optics-kit` is not a kit: a typed error whose message names the file.
  - The zip check in `KitArchive` reads `.optics-kit` at the zip's root.
- **3.4 The last folder (`006-10`).**
  - Every kit opened or created, by any route, records the folder *above* the kit. The routes are the dialogs, the
    history, Locate and unpacking a zip.
  - It is recorded in `environment.json`'s `lastFolders`, under a new `PickerPurpose` for kits. That map already
    remembers the photo picker's folder.
  - Every kit dialog starts there; the Kits folder is the default until one is recorded. Today `Program.fs` always
    builds `KitFolderPicker.ofWindow` on `kitsFolder` (lines 125 and 451).
- **3.5 Every kit in `data/` has the file.**
  - AT-1 and AT-2 write `.optics-kit`. The committed kits are rebuilt and re-committed in the same step.
  - AT-3 and AT-4 (Phase 9) are born with it.
  - `.gitattributes`' default rule (`* text=auto eol=lf`) already stores `.optics-kit` as LF text.
- **Verification.**
  - Storage and Domain tests of `KitFolderStore`, `KitFolders` and `KitArchive`.
  - `KitStartWindowTests`: open by file, the start folder.
  - `KitAcceptanceTests`' file-by-file facts.
  - A desktop fact: Open kit… starts in the remembered folder and opens the picked `.optics-kit`.
- **Migration intent.**
  - `.kit-json` is gone, and the kit file's `schemaVersion` goes to 2.
  - A folder holding only `.kit-json` is "not a kit", with that reason; there is no reader for the old name (AGENTS.md:
    no legacy). Its schema file is renamed with it.
  - Kit-history entries name folders and are unchanged.
  - AGENTS.md's kit-folder gotcha changes (Phase 10).

## Phase 4 — Items

Closes: `006-2`.

- **4.1 The list.** A vertical list of entry toggles (`label — model — revision`), under 1.2: press to choose and open
  its details under it, press again to close.
- **4.2 Details under the item.**
  - Read-only: model, dial, notes, the photo with its thumbnail, the parameter grid (copyable, 1.4) and the revisions.
  - Edit on its line opens the label, the notes, the photo (Choose / Remove) and the parameters. In Edit, pressing a
    parameter row opens `parameterEditorView` directly under that row.
  - Save writes the item once, as its next revision. Today each Set writes a revision of its own.
- **4.3 New item… (Creating) opens the Library in its own window.**
  - It opens the Library window in its Select mode: the spec 0038 Choose… flow, `openLibrarySelectWindow`, in its own
    window, fitted to its screen by `WindowFit.keepOnScreen`.
  - A new select target, "a new kit item", offers everything today's add form offers: item models, light sources,
    polarizers and samples.
  - The Library window does not list item models today (`LibraryWindowView` has none). It gains them as a kind in its
    faceted tree.
  - The pick returns to the Items page's creation form: the chosen model with Choose another…, then Label, Dial and
    Create / Cancel.
  - The 220-px offer list (`addItemFormRows`) is deleted.
  - Materials are chosen where a sample is made, in the Library window's sample editor, which Select mode already
    reaches. The Items page needs no Materials button of its own.
- **Verification.**
  - Headless: `ItemsPanelTests`; `LibraryWindowTests` for the new target and the item-model kind;
    `AutomationContractTests`.
  - Real desktop: New item… opens the Library window at its screen's size, and its choice lands in the form.
- **Migration intent.** None stored. Fewer revisions are minted (Phase 1).

## Phase 5 — Setups

Closes: `006-3`.

- **5.1 The list.** One entry toggle per setup, captioned by its name.
  - **The line.** A chosen setup's row shows its name, then each slot in light order as "role: item", from the existing
    `SetupSlot.elementLabelIn`: "Source: Laser · Prep 1: CPL1 · Obj 1: LR @ 45° · Analyzer 1: CPL2 ·
    Detector 1: Phone". Edit and Copy… sit on the same line.
  - **The table under it, read-only.** Elements can be selected, and the readout and the Details panel show the
    selected one. The table can be panned and zoomed, but nothing is moved, rotated, added or removed.
- **5.2 The selected setup.** The chosen setup is the kit window's *selected setup*: the one the Experiments page
  creates experiments over (`006-3b`).
  - Choosing it again unselects it.
  - The kit still opens on the setup last selected (`003` D2).
- **5.3 Edit.**
  - The table under the row becomes editable, with the ribbon (Add, Rotate, Move, Render, Details). The slot chips sit
    above it, each putting a kit item, or nothing, into its slot (`AssignSlot`).
  - Save is withheld until the table differs from the setup (`model.tableWrittenOver setup <> setup`). It writes the
    next revision when an experiment references the current one, and rewrites it otherwise; `Lifecycle.decideVersioning`
    is unchanged.
  - Cancel asks when the table has changed.
  - `SwapSlot` retires (D9). It jumps to the held setup of that content, or makes a new one, which contradicts editing
    in place.
- **5.4 Copy….**
  - A name field opens under the line, prefilled `<base> # <N>`. `<base>` is the name without a trailing
    ` # <digits>`, and `<N>` is the first number from 1 that no setup of that base has:
    - `CPL-(LR#45)-CPL` → `CPL-(LR#45)-CPL # 1`, then `# 2`;
    - copying `X # 1` gives the next free `X # <N>`.
  - Create copies the setup's slots under that name, as revision 1, and chooses it.
  - This is a new `SetupName` member. The shared `(n)` sequence for name collisions (`FileStem.firstFreeText`) is
    unchanged (D15).
- **5.5 Rename.** As in 1.3.
- **5.6 New setup (Creating).**
  - A name, then an editable table that starts with the light source alone (D14), with the ribbon and the chips. Create
    writes revision 1.
  - It replaces Create setup, Save setup, the name field and the "Active setup:" line (`setupSectionView`).
- **5.7 Off this page (`006-3a`).** Record experiment, its note field and Setup grid… leave the Setups page.
  - The grid moves to the Experiments page (6.5, D5).
  - Record experiment retires: creating an experiment over the selected setup is New experiment.
- **Verification.**
  - Headless: `SetupSaveLoadTests`, `SlotBarTests`, `SetupGridPanelTests`, `MainWorkbenchTests` and
    `ScrollExtentTests`.
  - Real desktop: choose a setup, press Edit, change a slot. Cancel asks, and Save writes exactly one revision.
- **Migration intent.** None stored.

## Phase 6 — Experiments

Closes: `006-4`, `006-13`.

- **6.1 No table and no ribbon on this page (`006-4a`).**
  - The page header names the selected setup: "New experiments use setup CPL-(LR#45)-CPL, revision 2".
  - **No setup selected (`006-4b`).** New experiment is not shown, and the page says "Choose a setup in Setups first:
    press Setups, then the setup."
- **6.2 The list: every experiment of the kit** as an entry toggle (name, role, data badge).
  - **Details under a chosen experiment (`006-4c`):**
    - its setup: name, revision and the 5.1 line, with no drawn table (D16);
    - its scan: each block's axes, ranges and capture (`ScanDescription`);
    - the source wavelength, role, session, notes and data badge;
    - View chart ↗, Export data, Edit and Remove.
  - Choosing an experiment no longer loads its setup on a table, since there is none on this page. `003` 3.3's row
    press is superseded.
- **6.3 Edit (`006-4e`).**
  - The editor opens in place under the row: today's `ExperimentControls` steps 1–4, Add variable, Add block and the
    source wavelength.
  - It works over the slots of the experiment's own setup revision, not the table: its candidates and preview chart read
    `exp.slotsIn model.kitSetups`, as `chartForExperiment` already does.
  - Role, session and notes are edited here too; today they are written as typed.
  - Correct setup (re-point to another revision, with an audit note) moves here from the Data page.
  - Save writes the experiment file once.
- **6.4 New experiment (Creating, a setup selected).**
  - The same editor, over the selected setup, with its preview; then Create.
  - **The same-setup warning (`006-4d`).** When an experiment already uses that setup (same name, any revision, D8),
    Create first asks in place: "E3 and E7 already use setup X. Create another? OK · Cancel · Show".
    - OK creates it.
    - Cancel returns to the form.
    - Show discards the new experiment and opens E3, the first of them, in the list (D12).
- **6.5 New experiments from a setup grid… (Creating).** Today's grid panel (`SetupGridView`, `GenerateSetupGrid`), moved
  from Setups, which creates setups and experiments together (D5).
- **6.6 View ↗ (`006-13`).**
  - **When it is offered.** Only when the experiment has a chart. Otherwise it is withheld, with a tooltip giving the
    reason `chartForRun` already gives. The same holds on the Data page.
  - **Knowing without running.** Evaluating every run at each render to find this out is too slow. So `chartForRun`
    splits into a pure *plan* (what would be computed, or why nothing) and the evaluation. Availability reads the plan,
    cached per experiment and kit revision.
  - **Why `absorbing-biaxial-noisy` has no chart.**
    - Its detectors are items of the "Mueller polarimeter" item model.
    - `detectorKindAt` reads a detector's kind only from a bound library *detector entry*, and otherwise falls back to
      Intensity (`TableAndElementRotationView.fs:2183`).
    - So every run is charted as unpolarized intensity with no polarizer on the path, which `chartForRun` refuses as "No
      analyzer present".
    - This is read from the code. The phase's first step confirms it in a test over the committed kit.
  - **Then (D6, recommended).** A Mueller polarimeter reads its Mueller matrix:
    - the chart shows its elements normalized by m₁₁, model and measured;
    - the chart window's series toggles hide elements;
    - the measured series are read through the Mueller-matrix observations profile.

    The alternative: View withheld, with "a Mueller polarimeter's run has no chart yet". Either way the silent Intensity
    fallback goes: an unrecognized detector is a typed reason, not intensity.
- **Verification.**
  - Headless: `ExperimentControlsTests`, `InverseConstructorTests`, `ChartStyleTests` and `MainWorkbenchTests`.
  - A new test over AT-2's committed kit: every View is offered and opens a chart (D6), or is withheld with its reason.
  - Real desktop: the same-setup warning's three answers.
- **Migration intent.** None stored. The experiment file keeps its shape; the correction moves only in the UI.

## Phase 7 — Data

Closes: `006-5`.

- **7.1 Nothing about setups (`006-5a`).**
  - The status line is the page's own (1.7).
  - The review's "Re-point to <setup>" list leaves; correcting a setup is an experiment's Edit (6.3).
- **7.2 The matrix as a table (`006-5b`).** One 1.4 grid with gridlines and a header row. Its columns:
  - one per Prep slot ("Prep 1");
  - one per object slot ("Obj 1", the item at its angle: "AIR @ 0°");
  - one per analyzer combination, headed by its analyzer items ("Analyzer 1: LP2").

  A note at the top-left corner says: "rows: what prepares the light and what it crosses; columns: what analyses it".
  Each cell holds the experiments measured there, as entry toggles. Copy as CSV, and every label is copyable
  (`006-5f`).
- **7.3 The matrix is the list (D4).**
  - Pressing an experiment chooses it (green) and opens its data panel immediately under it, in a full-width row
    inserted under its matrix row. Pressing it again closes it (`006-5c`, `006-5d`).
  - The separate "Data files" list goes; it opened every experiment below the matrix.
  - Experiments the matrix cannot place, because their setup is missing, get the same rows under the matrix.
  - **The panel:**
    - the attached file, its profile and rows, and the badge's detail;
    - Attach data file… or Re-attach, which opens the wizard;
    - View chart ↗;
    - the points chart, with one row per point.
  - Exclude and Include need Edit (Phase 1). Save writes the exclusions once.
- **7.4 Experiments with no data (`006-5e`).** Their toggles have their own unchosen fill, a new amber
  `CommandEmphasis`, and their caption begins "⚠ ", so the state is not shown by colour alone (`003` 1.2 item 5).
- **7.5 Split a file by key…** is the page's Creating state; the held split's panel is its form.
- **Verification.**
  - Headless: `DataStepTests`, `ImportWizardTests` and `ScrollExtentTests`.
  - A test that the committed `optics-mueller` kit's matrix CSV has its header and one line per matrix row.
- **Migration intent.** None stored.

## Phase 8 — Unknowns, Plan, Fit, Results

Closes: `006-6`, `006-7` and `006-8` (with 1.6's help), and the Publish part of `006-1c`.

- **8.1 The study is the context.**
  - The study chosen on Unknowns is the context of Plan, Fit and Results, as the selected setup is the Experiments
    page's.
  - Each of those pages is headed "Study: QZ-LR". With none chosen it says "Choose a study in Unknowns first." and
    shows nothing else.
  - `studyBarView` (the study chips, and the status on every study page) goes.
- **8.2 Unknowns.**
  - **The list.** One entry toggle per study. Under a chosen study, read-only:
    - its experiments, as dynamic buttons;
    - the parameter grid: owner, parameter, state, value, bounds, σ, scope and link;
    - the sessions and gains table.
  - **Edit.**
    - Add experiment, Remove experiment and Open material: today's offers.
    - The grid's rows become pressable. Pressing a parameter opens its editor directly under that row: state, value,
      bounds, σ, scope and link.
    - Scope and link are text in the grid and editable only in that editor (`006-6d`, `006-6e`). Today they are combo
      boxes, live in every row.
    - Save writes the study, and each item whose parameters changed, once each.
  - **New study (Creating, `006-6c`).**
    - The name, then the experiments to fit, headed "Experiments to fit:", each with its role and data badge so it does
      not read as a setup; then Create.
    - The form never shows beside a chosen study.
- **8.3 Plan (`006-7`).**
  - The help says what "What the pages are for" says.
  - Run plan is the page's Creating action, with its progress and Cancel.
  - The report: findings, rank, the bounds table and the instrument states.
  - The ranked candidates are entries; each opens its gain and Add to kit under it.
  - When the kit cannot be planned (no Mueller polarimeter, no crystal material), the page says that one reason and shows
    no controls.
- **8.4 Fit (`006-8`).**
  - **The list.** The study's fit runs as entries, newest first. Each opens its summary under it (stage, engine, starts,
    residual) and Open results.
  - **New fit run… (Creating):**
    - the pre-fit checks;
    - the mode: joint, stage 1 or stage 2, and for stage 2, held fixed or as priors;
    - Run, with its progress in place and Cancel.

    The finished run is written and chosen.
  - The mode and the calibration choice are written into the study when the run starts (`003` D4 keeps them there), not
    on each press.
- **8.5 Results.** The six tabs of the run chosen in Fit. Every table is a 1.4 grid with Copy as CSV (`006-1f`).
- **8.6 Publish** is on the Kit page (2.2), with the same messages: `PublishCalibration`, `PromoteToLibrary`.
- **Verification.**
  - Headless: `StudyViewTests`, `PlanViewTests` and `InverseConstructorTests`.
  - AT-1 and AT-2's study and fit facts, which drive the same messages.
- **Migration intent.** None stored. The study file keeps the fields `003` 2.5 gave it.

## Phase 9 — Two direct kits, and the guides

Closes: `006-14`, `006-9`.

- **9.1 Two kits in `data/`.**
  - **How they are made.** As AT-1 and AT-2 are: built through the app's messages by AT-3 and AT-4 in
    `KitAcceptanceTests`, committed as their output, compared file by file, and never hand-edited.
  - **Direct problems only.** Both have no data files and no study.
  - **`data/multilayer-thin-film/`** (`MultilayerThinFilm_02.fsx`: `plot f fn i` and `plot f fn w`).
    - Items:
      - the "Xenon lamp + monochromator, 190–1000 nm" source, which is tunable;
      - a linear polarizer at the script's s;
      - the stack: 100 quarter-wave pairs at 600 nm, of n = 1.75 and n = 1.50, on n = 1.50 glass;
      - Detector R and Detector T.
    - One setup, and two experiments that capture R and T:
      - incidence (R2) 0–89° at 600 nm, set by `sourceWaveLengthOpt`;
      - wavelength 500–700 nm at normal incidence.
  - **`data/langasite-on-silicon/`** (`LangasiteOnSilicon.fsx`'s first two charts: `plot film fn w` and
    `plot substrate fn w`).
    - The same source, polarizer and detectors.
    - Two samples: the built-in "Langasite film on silicon (10 µm, dispersive)", and a 10 µm langasite *plate* on silicon
      (`langasiteSubstrateOnSilicon`).
    - Two setups, one experiment each: wavelength 250–600 nm at normal incidence, R and T.
  - **The two new samples** (the 100-pair stack and the langasite plate) ship as catalogue entries, with frozen GUIDs in
    `SampleIds` (D7). A user then also finds them in the Library.
  - **Point counts.** 1 nm and 1° steps (201, 90 and 351 points), against the scripts' 2000 (D10).
  - **Checked against the scripts.** Each experiment's chart is compared with its script's curve using
    `FsxAcceptanceTests`' comparison, moved to a shared test module, not copied.
- **9.2 The guides.**
  - Two step-by-step pages, "Build a direct problem" and "Build an inverse problem", opened by Guide… on the Launcher
    and on the Kit page.
  - Each step names its page and its button by caption, and says what the step writes into the kit.
  - The direct guide walks `multilayer-thin-film`; the inverse one walks `optics-mueller`. Outlines below.
  - A test reads every caption the guides name and checks that a control with that caption exists on the named page. A
    renamed button then fails a test instead of misleading the user.
- **9.3 Help.** The help text of every page (1.6) is written in the same pass, so it uses the final captions.
- **Verification.**
  - `KitAcceptanceTests`: the two new kits, file by file, and their curves.
  - `FsxAcceptanceTests`, unchanged.
  - `CatalogueTests`: the two new built-ins.
  - `LauncherTests`: Guide… and Options….
- **Migration intent.**
  - The shipped catalogue gains two entries. The start-up import applies them as `003` D6 rules: new entries without a
    question.
  - `data/` gains two kits.

### Outline: build a direct problem (`multilayer-thin-film`)

1. Launcher → Main → New kit…: choose an empty folder. The kit's `.optics-kit` is written.
2. Items → New item…: in the Library window, choose "Xenon lamp + monochromator, 190–1000 nm", label it, then Create.
   Repeat for a linear polarizer, the sample "Quarter-wave glass n = 1.75 / 1.50 multilayer (100 pairs)" and two
   detectors.
3. Setups → New setup: name it. Put each item in its slot with the chips:
   - the source;
   - the polarizer;
   - the sample;
   - one detector on the reflected ray and one on the transmitted ray.

   Then Create.
4. Experiments, with that setup chosen → New experiment: vary the sample's incidence (R2) from 0 to 89°, capture both,
   90 points, at 600 nm. Create. Repeat, varying the wavelength from 500 to 700 nm.
5. View ↗ on each experiment shows R and T. The chart window's Export CSV… saves the numbers.

### Outline: build an inverse problem (`optics-mueller`)

1. Open the kit in Inverse.
2. Items: the instruments and samples. Edit each and mark the parameters to fit as Unknown.
3. Setups: one per arrangement measured.
4. Experiments: one per measured run, over its setup.
5. Data: attach each lab file through its import profile, or use Split a file by key… for a file that holds many runs.
   Review the points, and exclude bad ones in Edit.
6. Unknowns → New study: choose the experiments to fit. Then Edit: scopes, links, sessions.
7. Plan, only in a kit with a Mueller polarimeter: what the kit can recover, and the experiments it proposes.
8. Fit → New fit run…: stage 1 on the reference (AIR) runs, then stage 2 on the samples; or joint.
9. Results: the fitted values, σ and correlations, each table with Copy as CSV.
10. Kit → Publish: write the calibration into the items, and promote a fitted material to the library.

## Phase 10 — AGENTS.md, and the re-survey

- **AGENTS.md.** These gotchas change:
  - the kit folder: `.optics-kit`, and opening a kit by its file;
  - "Main ribbon" and "The kit window's left flow strip": the top row, the Kit page, and where the ribbon shows;
  - "The slot bar is the quick swap": `SwapSlot` and Record experiment retire, Setup grid… moves;
  - the reference kits: four, AT-1 to AT-4;
  - a new gotcha for the page rules (one state, Edit / Save / Cancel, details in place), since every new page must
    follow them;
  - "`AutomationContractTests` is the law" gains the tooltip, copyable-text and help rules.
- **The re-survey.**
  - Re-drive `006`'s fifteen comments on the real desktop, with `OpticalConstructor.Desktop.Tests` extended by a fact
    per comment.
  - Report it as the next `NNN`, as `005` did.

## Traceability: every demand → the step that meets it

| Demand | Step |
|---|---|
| 006-1a page buttons as the top row; Inverse adds Data … Results | 2.1 |
| 006-1b no "Switch to"; Inverse / Main button | 2.1 |
| 006-1c kit operations and Publish behind the last button | 2.2, 8.6 |
| 006-1d no Experiments tab; table buttons only where the table is | 2.3, 5.3 |
| 006-1e dynamic labels selectable and copyable | 1.4 |
| 006-1f matrices copyable as CSV | 1.4, 7.2, 8.5 |
| 006-1g one state: view / edit or create | 1.2 |
| 006-1h edits in place, under the thing edited | 1.2 |
| 006-1i Edit required before any change | 1.2 |
| 006-1j Save withheld until something changed | 1.2 |
| 006-1k Cancel asks when something changed | 1.2 (and leaving, D11) |
| 006-1l help on every page | 1.6, 9.3 |
| 006-2 Items: details under the item, second click closes, Library in its own window, Edit required | 4.1–4.3 |
| 006-3a Setups: no Record experiment, no Setup grid | 5.7 |
| 006-3b the selected setup is what experiments use | 5.2, 6.1 |
| 006-3c Copy with `# <N>` | 5.4 |
| 006-3d Rename | 1.3, 5.5 |
| 006-3e select and unselect | 1.2, 5.1 |
| 006-3f the line, Edit on it, read-only table under it | 5.1 |
| 006-3g Edit, Save withheld until changed, Cancel asks | 5.3 |
| 006-4a Experiments: no table buttons, no table | 6.1 |
| 006-4b no setup selected: nothing to add, and the page says why | 6.1 |
| 006-4c details under the experiment | 6.2 |
| 006-4d same-setup warning: OK / Cancel / Show | 6.4 |
| 006-4e Edit required | 6.3 |
| 006-5a Data: no setup shown | 1.7, 7.1 |
| 006-5b the matrix explained, a table with gridlines | 7.2; "What the pages are for" |
| 006-5c data hidden; a press opens it under the experiment | 7.3 |
| 006-5d second press closes it | 7.3 |
| 006-5e no data: own colour and "⚠ " | 7.4 |
| 006-5f matrix labels copyable | 1.4, 7.2 |
| 006-6a Unknowns: no setup shown | 1.7, 8.1 |
| 006-6b what the page does | 1.6; "What the pages are for" |
| 006-6c new study or a chosen study, never both | 8.2 |
| 006-6d parameter edited under the parameter | 8.2 |
| 006-6e Scope and Link editable only while editing that parameter | 8.2 |
| 006-7 what Plan does | 8.3, 1.6; "What the pages are for" |
| 006-8 what Fit does, and how its runs are made | 8.4, 1.6; "What the pages are for" |
| 006-9 a step-by-step inverse guide | 9.2 |
| 006-10 remember the folder above the last kit | 3.4 |
| 006-11 tooltips with a configurable delay (1 s); Options on the Launcher | 1.5 |
| 006-12 dynamic buttons: context menu with Copy name; Rename by Edit and by the menu | 1.3 |
| 006-13 View withheld when there is no chart (`absorbing-biaxial-noisy`) | 6.6 |
| 006-14 two direct kits from the scripts, and how to build one | 9.1, 9.2 |
| 006-15 `.optics-kit` in every kit, with created and last saved; open the file | 3.2, 3.3, 3.5 |

## Decisions for the operator

Each has a recommended default, which the arc applies unless the operator rules otherwise.

- **D1. The last button's caption.** Recommended: **Kit**. Alternatives: **File** (Word's word for the same thing),
  **Kit operations**.
- **D2. What Kit opens.** Recommended: a page of its own, under the row like every page. Alternative: a panel that drops
  down over the current page.
- **D3. `.optics-kit`.**
  - Recommended: it is `.kit-json` renamed, `modified` becoming `lastSaved` (3.2). One file marks the kit.
  - Alternative: a second file holding only the two stamps, moved out of `.kit-json`.
- **D4. The Data page's list.** Recommended: the matrix is the list, and a chosen experiment's panel opens in a
  full-width row under its matrix row. Alternative: the matrix, then a separate list whose rows open in place.
- **D5. Setup grid….** Recommended: it moves to the Experiments page as "New experiments from a setup grid…".
  Alternative: it leaves the UI, kept only as the message AT-2 builds through.
- **D6. A Mueller polarimeter's View.** Recommended: chart the normalized Mueller elements, model and measured.
  Alternative: View withheld, with the reason.
- **D7. The new kits' two samples.** Recommended: shipped catalogue entries. Alternative: made in each kit's build
  through the Library window, so they exist only in those kits' snapshots.
- **D8. "The same setup" in the warning.** Recommended: the same setup name, any revision. Alternative: the same
  revision only.
- **D9. `SwapSlot` and the Select bay in kit windows.** Recommended: both retire; a slot chip assigns a kit item in
  Edit. Alternative: keep both inside Edit.
- **D10. Point counts of the new kits.** Recommended: 1 nm and 1° steps. Alternative: the scripts' 2000 points.
- **D11. Leaving with unsaved changes.** Recommended: leaving the page, choosing another entry, switching mode or
  closing the window asks, like Cancel. Alternative: only Cancel asks, and leaving discards silently.
- **D12. Show in the same-setup warning.** Recommended: it discards the new experiment and opens the existing one.
  Alternative: it keeps the draft to return to.
- **D13. Renaming a study.** Recommended: only while it has no fit run. Alternative: a rename also rewrites its fit-run
  files, which are append-only today.
- **D14. A new setup's table.** Recommended: it starts with the light source alone. Alternative: it starts as a copy of
  the selected setup (Copy… already does that).
- **D15. The copy suffix.** Recommended: ` # <N>` for copies, as asked, with the `(n)` collision sequence unchanged.
  Alternative: one sequence for both.
- **D16. An experiment's details.** Recommended: its setup as the slot line, with no drawn table, since `006-4` says
  "no table … at all". Alternative: a read-only table under the experiment.

## Not in this plan

These are named so they are not added on the way:

- undo and redo (0051 `001` §7);
- the Russian UI (`001` §8), including localized help and tooltips;
- the Planner for non-gyrotropic kits (`001` §2);
- opening a `.optics-kit` by double-clicking it in Explorer, which needs a file association and an installer;
- options beyond the tooltip delay;
- saving tables as CSV files: Copy as CSV puts them on the clipboard;
- context-menu items beyond Copy name and Rename;
- removing a setup (not asked);
- a WinForms port;
- a visual redesign or theme change beyond these steps.

## Gates and order

- **The order.**
  - **Phase 1 first.** Every later page is built with its state machine, entry toggles, copyable text, grids, tooltips
    and help.
  - **Phase 2 next.** The frame every page sits in.
  - **Phase 3.** A stored shape that AT-1 and AT-2 must rebuild, done before the pages change the builds again.
  - **Phases 4–8,** in the flow's order.
  - **Phase 9 after the pages.** Its kits are built through the final messages, and its guides name the final captions.
  - **Phase 10 closes.**
- **The gates.**
  - **Every phase** runs the `build` and `elevation` gates, `ui-smoke` (which includes `AutomationContractTests`) and
    `ui-tests` filtered to the touched classes.
  - **`constructor-unit-tests`** runs for Domain and Storage changes: 3, 5.4, 6.6 and the renames of 1.3.
  - **The reference-kit acceptance tests** run wherever a stored shape or a build changes: Phases 1, 3, 4, 5 and 9.
    Their output is re-committed in the same step, as AGENTS.md requires.
  - **`fsx-acceptance`** runs when 9.1 moves its comparison helpers.
  - **`BerremanTests` is untouched:** no solver code changes.
