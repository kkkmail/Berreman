# 038 — What the Optical Constructor UI actually does: a button-by-button survey

2026-11-19. Requested by the operator: "drive the app, identify what every button actually does … I need to get an
understanding what works and what does not on the UI". This is a report, not a ruling. No product file was changed.

## How this was done

- **There is no WinForms app.** The repository has no `UseWindowsForms` project and no `System.Windows.Forms`
  reference. Every window is Avalonia FuncUI, so this survey drives Avalonia.
- **The real windows, driven headlessly.** The agent's shell runs in Windows session 0, which has no desktop, so the
  real app could not be driven by UI Automation. Instead, a scratch console driver ran the real composition-root
  windows (`LauncherWindow`, `KitStartWindow`, `MainConstructorWindow`, `InverseConstructorWindow`, `MaterialsWindow`,
  `LibraryWindow`, `DataLocationsWindow`) under Avalonia.Headless. It referenced the built `OpticalConstructor.Ui.Tests`
  assembly; the driver lived in the agent's scratchpad and was not committed.
  - The library was fresh, with the shipped `Catalogue/` imported and every default import decision accepted.
  - The kit was a fresh copy of `data/optics-mueller` in the scratchpad, opened read-write. The committed kit was
    never opened, and `git status` stayed clean.
- **One press, one fresh window.** For each state, the driver listed every visible control that has an automation id.
  It then pressed each one, at its centre with a real mouse down and up, in a fresh window over a fresh kit copy. It
  recorded what changed: texts, controls appearing or disappearing, enabled state, windows opened, and kit files added,
  changed or deleted. Multi-step flows were scripted the same way.
- **What a headless run cannot show.** Operating-system dialogs (file and folder pickers, Explorer) do not exist
  headless, so buttons that start with one show "no visible change". Each of those was checked in code instead, and
  is marked **(dialog)** below. Colour-only feedback, such as a toggle that only changes its background, is not
  detected; those are marked **(colour only)**.
- **Where a row says "no visible change" without (dialog) or (colour only), nothing on screen changed.** That is a
  finding in itself.

## The ten things that make the UI hard to understand

1. **Opening a kit shows an empty table, not one of its setups.** `optics-mueller` holds 42 setups, but Main and
   Inverse both open on a default table: a source chip and a detector chip, both unassigned (`Source: — ▾`,
   `Detector 1: — ▾`). The only notice is "The table is not a setup of the kit yet.", and only in the Experiments bay.
   Nothing on the table side says that the setups are under **Experiments bay → Saved setups**.

2. **View ↗ ignores the experiment's own setup.** This is why the operator's View showed nothing.
   - Code: `viewExperimentHook` → `chartForExperiment` → `chartForRun model exp.scan` runs the scan over the elements
     **currently on the table** (`model.elements`), not over the setup revision the experiment references. When the
     result has no series, the hook silently does nothing (`if not (List.isEmpty chart.series)`).
   - Driven:
     - On a freshly opened kit, View on row 1 (`LP-(AIR#0)-LP-2`) opened no window, and so did View after clicking
       the row.
     - After loading the row's own setup (`SavedSetup_LP-(AIR#0)-LP`), View opened "Intensity vs dial reading".
     - **With that same AIR setup still on the table, View on row 3 (`LP-(LR#0)-LP-2`, which needs the LR setup) also
       opened a chart.** That chart is computed for the wrong setup, and nothing says so.
   - The chart is the forward model of the scan (`chartForRun` evaluates `runEmission`). The experiment's measured
     points are not drawn in it.

3. **Clicking an experiment row does not load its setup either.** A row click (`ExperimentEdit_<n>`) puts the bay into
   edit mode ("Update experiment", Role, Session, Notes) but leaves the table as it was. On the default table the bay
   then says "no detector captures this element's light — put a detector on its transmitted beam…", which is
   misleading: the experiment's setup does have a detector.

4. **Remove deletes the experiment file at once, with no confirmation.** Pressing `Remove` on a row deleted
   `LP-(AIR#0)-LP-2.experiment-json` from the kit folder immediately. There is no undo and no "are you sure". By
   contrast, Rotate → Reset asks "Reset the selection's rotations? Yes / No".

5. **Saving or committing writes new revisions even when nothing changed.**
   - `Save setup` with no edit wrote `LP-(AIR#0)-LP.setup-json` as revision 2.
   - `Add experiment` and `Record experiment` each bumped the active setup to the next revision as well as writing
     the experiment.
   - `Update experiment` on an untouched row rewrote its `.experiment-json` and bumped the setup again.

   A user who only looks around and presses buttons can accumulate setup revisions.

6. **"Setups" in the flow strip has no setup list.** The Setups step is the table with the Add bay (the element
   palette). Load, Create and Save setup, and the saved-setups list, all live in the **Experiments** bay (code:
   `ShowFlowStep SetupsFlowStep` only switches the ribbon away from Experiments). So the place called "Setups" cannot
   load a setup, and the place called "Experiments" is where setups are managed.

7. **Status messages vanish on the next action.** "Loaded setup 'LP-(AIR#0)-LP'." is the only confirmation that a
   setup loaded. Any later press replaces it, including opening Setup grid, Record experiment, or Create setup. Only
   the Experiments bay keeps a durable "Active setup: …, revision n" line.

8. **Element labels do not match the kit's names.** With `LP-(AIR#0)-LP` loaded:
   - The slot bar uses kit item names: `Prep 1: LP1`, `Obj 1: AIR @ 0°`, `Analyzer 1: LP2`, `Detector 1: Phone`.
   - The Experiments bay's "Element to vary" list uses kind-and-position labels for the same elements:
     `Light source #1`, `Sample #2` (that is AIR), `Detector #3`, `Linear polarizer #4`, `Sample #5`,
     `Linear polarizer #6` (that is LP2, the analyzer).

   Every experiment row says "Linear polarizer #6: vary Dial reading". The user has to work out that #6 means LP2.

9. **Opening a kit asks import questions that have only one answer.** Open kit… and New kit… on the start screen
   first show "Import decisions: 8 library entries need a decision". Every row's only option is "Reuse the held
   entry", and the kit window does not open until Apply. The Launcher on a first start likewise shows "Library entries
   waiting for an import decision: 72". Until those are applied, Materials shows 0 materials and Library shows 0
   entries, with no hint why.

10. **Many controls appear only after a precondition, with no hint.**
    - Create study appears only once a valid name is typed.
    - Generate in Setup grid appears only once a cell is ticked.
    - Add experiment, Add variable and Add block stay disabled until an element is chosen as "Element to vary". The
      default table offers only `Light source #1` and `Detector #2`, which have nothing to vary.
    - Rotate, Move and Select do nothing until an element is selected, and the only way to select is to click the
      canvas. No list selects an element.

## What works (verified by driving)

- **Kit bar.**
  - Reload changes ("No kit file changed outside the app.").
  - Close kit: closes the window and releases `.oc/lock`.
  - Switch to Inverse / Switch to Main: the flow strip gains or loses Data, Unknowns, Plan, Fit, Results and Publish.
  - Save template: "Saved the naming template." and writes `.kit-json`, or "…is unchanged." when unchanged.
  - Materials… and Library… open their windows over the kit-overlaid library.
- **Slot bar quick swap works well.** In `LP-(AIR#0)-LP`, choosing `LR` in `Obj 1` switched the table to the held setup
  `LP-(LR#0)-LP`, and choosing `QZ` switched it to `LP-(QZ#0)-LP`.
- **Setup grid → Generate.** Ticking `Obj 1: LR` and `Obj 1: QZ`, then Generate, wrote
  `LP-(LR#0)-LP (2).setup-json` and `LP-(QZ#0)-LP (2).setup-json` ("Generated 2 setups (2 new) and 0 experiments").
  The new setups were named " (2)" because setups of those names already exist.
- **Create setup.**
  - With a name, it writes `<name>.setup-json` and makes it active.
  - A second press says "Not created — the kit already has a setup of that name".
  - With a blank name it says "Not created — the setup name is not valid: '': a setup name must not be blank".
- **Experiments bay draft.**
  - Choosing `Linear polarizer #6` offers Dial reading, Capture and Range.
  - Export data: "Exported 1 series".
  - Add experiment and Record experiment write `<setup name>.experiment-json` (then " (2)"), role Science, badge
    "no data".
  - New clears the draft.
- **Items step.**
  - It lists the kit's nine items.
  - A row opens that item's parameters, revisions and provenance.
  - Add item from library model… lists the library models.
- **Inverse, Data step.**
  - The experiment matrix lists every experiment with its badge: "validated (19 points)", "excluded points (1
    excluded, 18 validated)".
  - A matrix entry shows its points and the setup-correction choices.
- **Inverse, Fit step.**
  - "Before the fit" lists each experiment as ready, with its point count.
  - Run joint and Run stage 2 start a background fit with a progress bar and Cancel.
  - Run stage 1 refuses with "'QZ-LR' holds no unknown that Stage 1 (calibration) fits".
- **Inverse, Results step.** The Values, Correlation, Residuals, Realizability, Product check and Profile likelihood
  tabs all render the stored run QZ-LR@20260531T120200Z. Profile likelihood says "Nothing to show for this fit run".
- **Inverse, Publish step.**
  - Promote writes `library/<id>@v1.polarizer-json` ("Promoted to the library as … v1").
  - Publish calibration writes `LR.item-json` and `QZ.item-json`. It then shows **both** "Published the calibration
    of LR, QZ…" and "the calibration is already published: every item … already holds its values" at the same time,
    which contradicts itself.
- **Inverse, Unknowns step.**
  - Typing a study name reveals Create study, which writes `S2.study-json` and then offers "Add <experiment>" for
    every other experiment.
  - A parameter row opens its Known/Unknown, Start, Lower, Upper and Prior σ editor.
- **Launcher.** Main and Inverse open "Open a kit — Main/Inverse". Materials, Library, Data locations and Review
  imports… each open their window.
- **Library window.**
  - Add sample, Make multilayer and Add source open their editors.
  - Back up library now writes `library-<stamp>.db`.
  - Restore library… lists the backups and asks to confirm.
  - Show inactive toggles, and the facet filters narrow the list.
- **Materials window.** Add opens the Material Editor, Categories… the Category Editor, and Online sources… the
  sources window (refractiveindex.info, PTB OCDB, CXRO Henke; "No snapshot yet").
- **Rotate bay.**
  - With the table selected, R1/R2/R3 ± step the table by 15°.
  - With an element selected, they rotate the element.
  - Lock Rn disables that axis's field and buttons.
  - Reset and Reset All ask Yes/No.
- **Move bay** (element selected). − and + move the element 0.05 m along the beam. Reset position moves it to 0 m.
  Moving past a neighbour reorders the slots: Analyzer 1 and Analyzer 2 swapped.
- **Select bay.** Choose… opens the Library in pick mode, fixed to the element's kind. Choosing an entry offers
  Confirm and Cancel.
- **Render bay.** Swap renderer toggles Wireframe and "Shapes + codes".

## What does nothing, or not visibly, in a running app

- **View ↗** with no setup on the table: nothing (item 2).
- **Data step → Review** (`DataReview_<n>`) and **Exclude point** (`DataExcludePoint_<n>`) showed no change in the
  driven window, with or without a reason typed. The code path exists (`ReviewExperimentData`, `ExcludePoint`, which
  needs a reason, writes the experiment file and never the lab file), and `DataStepTests` exercises it. Treat this as
  **not confirmed from the UI**; it may be a layout or scroll issue, or a defect in how the driver typed the reason.
- **Plan step: nothing to run for this kit.** It reads "The study opens no crystal material: open one in Unknowns."
  An "Open <material>" offer exists only for stack samples. `optics-mueller`'s objects are black-box Mueller matrices,
  so Plan can never run on this kit and there is no Run button.
- **Plan criteria** (D-optimal / A-optimal), **Fit mode Joint**, **Calibration fixed / as priors**, the **study
  choice** chip, and **New study experiment** toggles: **(colour only)**. Fit mode Joint did write `QZ-LR.study-json`
  when pressed.
- **Rotate about the middle of the incident face** (check box): no visible change until an element is rotated.
- **Experiment candidates `Light source #1` and `Detector #<n>`**: no change. They have nothing to vary, but they are
  offered as choices anyway.

## Buttons that open an operating-system dialog (checked in code, marked (dialog))

These worked only as far as the dialog in a headless run; in the real app each opens a dialog first.

| Where | Button | What it does after the dialog |
|---|---|---|
| Kit bar | Send kit… | Zip save dialog in the exports folder, then a background zip with progress and Cancel |
| Kit bar | Show in Explorer | Opens the kit folder in Explorer (`ExplorerProxy.system`) |
| Kit bar | Copy experiments from another kit… | Folder picker, then a list of the other kit's experiments and items to copy |
| Start screen | New kit… / Open kit… | Folder picker, then the import-decisions dialog (item 9), then the constructor window |
| Start screen | Open kit from zip… | Zip open dialog in the exports folder |
| Data step | Attach data file… | File picker, then the column-mapping import wizard; the wizard's Attach saves the profile and attaches |
| Data step | Split a file by key… | File picker, then the wizard on Phone ROI CSV with key-column grouping |
| Items | Choose photo… | Image picker for the item's photo |
| Library | Register catalogue folder…, Export shown to catalogue…, Export my library… | Folder pickers |
| Materials | Export shown to catalogue…, Import glass catalogue… | Folder / file pickers |
| Data locations | Open folder (×8) | Opens each folder in Explorer |

## Surface map (where each thing lives)

- **Launcher.** Main, Inverse, Materials, Library, Review imports… (only while decisions are pending), and the footer
  link Data locations.
- **Start screen.** New kit…, Open kit…, Open kit from zip…, the history filter, and a star per history row.
- **Kit window, top bar.** Send kit…, Show in Explorer, Copy experiments from another kit…, Reload changes, Close kit,
  Switch to Inverse/Main, the Naming template field and Save template.
- **Flow strip.**
  - Main: Items, Setups, Experiments.
  - Inverse: those three, then Data, Unknowns, Plan, Fit, Results and Publish.
- **Below the flow strip.** Materials…, Library…
- **Ribbon (Setups and Experiments steps).**
  - **Add:** `+ Linear polarizer`, `+ Circular polarizer (L/R)`, `+ Sample`, `+ Lens`, `+ Flat/Curved mirror`,
    `+ Detector`, Remove selected.
  - **Rotate:** R1/R2/R3 ±, the fields, Lock Rn, Reset, Reset All, and the incident-face check box.
  - **Move:** −, +, the field, Reset position.
  - **Render:** Swap renderer and six sliders.
  - **Select:** Choose…
  - **Details:** read-only.
  - **Experiments:** element candidates, variable, capture, range, wavelength, Add experiment / Update experiment,
    New, Add variable, Add block, Export data, the experiment rows (click to edit, View ↗, Remove), and the Setup
    section (Setup name, Create setup, Save setup, Saved setups).
- **Under the table.** Slot chips: one per slot, each listing the kit items that fit it. Record experiment.
  Setup grid…
- **Item behaviour that surprises.**
  - Adding a polarizer from the palette onto a loaded setup binds it to the library entry "Ideal linear polarizer",
    not to a kit item. Its slot chip reads `Analyzer 2: —` until a kit item is chosen.
  - Every Record experiment names the experiment after the active setup ("LP-(AIR#0)-LP", then
    "LP-(AIR#0)-LP (2)"), so repeated records pile up numbered copies.

## Suggested reading order for the operator

1. Items 1–3: the table, View and row selection are one problem. The table is the only context anything runs in, and
   nothing loads it for you.
2. Item 4 (Remove) and item 5 (silent revisions) are the two that change kit files without the user meaning to.
3. Item 6 (Setups vs Experiments) and item 8 (element labels) are naming problems that make the rest hard to learn.
