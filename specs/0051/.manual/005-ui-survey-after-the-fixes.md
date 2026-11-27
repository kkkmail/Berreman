# 005 — The Optical Constructor UI after plan `003`: the 038 and 002 survey, re-driven on the real desktop

2026-11-27. The operator asked to implement plan `003-plan-close-ui-gaps.md` in full, keeping a log (`004`), "then
re-drive the app via UI automation to prove that everything works as expected". This is that re-drive, `003` Phase 6.
It is a report, not a ruling. Nothing is committed: the operator commits.

## How this was done

- **A real desktop.** The agent ran in Windows session 1, the console session at 1920 × 1080. It drove the real
  `Softellect.Berreman.OpticalConstructor.App.exe`, the Release build of the working tree over `ef4b4338`, version
  10.1.402.7 (no bump was requested).
- **A sandbox per fact.** Each fact gets its own copy of the build output, with an `appsettings.json` that puts every
  data location under the test's output folder. Each also gets a fresh library, and kits copied from `data/`, which
  AT-1 and AT-2 rebuilt in this change. The committed kits were never opened.
- **The driver is committed.** `OpticalConstructor.Desktop.Tests` is FlaUI over UIA3.
  - `DesktopTests` (7 facts) is `003` 1.6's suite. `DesktopSurvey.fs` (6 facts) re-drives the 038 and 002 survey, each
    fact naming the findings it closes. All 13 run one at a time, in one xUnit collection, since they share the
    desktop.
  - Every control is found by its `ConstructorUiId`, so no id string is retyped. It is pressed through its own UIA
    pattern (Invoke, Toggle, SelectionItem).
  - A native dialog is answered through its own controls: a folder by its name box, a file name typed into the box.
  - Colour is measured from the screen's pixels.
  - Every fact skips in session 0. Run it with `--filter-trait "Category=desktop"`.
- **The second layer is the headless suites** (`003`, "Three test layers"): `OpticalConstructor.Tests` and
  `OpticalConstructor.Ui.Tests`, including `AutomationContractTests` and `ScrollExtentTests`.

## Results

| Suite | Result |
|---|---|
| `dotnet build Berreman.slnx -c Release` | succeeded; no warning but the exempt NU1701 / NU1902 advisories |
| ELEV gate (`-ExcludeRules ELEV004`) | OK: 0 findings in 13 projects |
| `OpticalConstructor.Tests` | 1914 of 1914 passed |
| `OpticalConstructor.Ui.Tests`, whole suite | 1043 of 1044 passed, 0 failed. 1 skipped: AT-2's five-seed ensemble, an `Explicit` fact run on demand only (spec 0050 S12.2) |
| `OpticalConstructor.Desktop.Tests`, real desktop | 12 of 13 passed. 1 skipped: Show in Explorer, for the account reason below |

## 038's and 002's findings, closed

"Shown by" names the real-desktop fact that drives the behaviour, then the headless tests where that layer is the
proof. Facts beginning "survey:" are `DesktopSurvey.fs`; the others are `DesktopTests.fs`.

| Finding | Closed by (`003`) | Shown by, on the real desktop |
|---|---|---|
| 038-1 kit opens on an empty table | 3.1 (D2) | "survey: the first start asks nothing…": the kit opens on one of its 42 setups. "a kit reopens on the setup it last had": choosing `CPL-(LR#0)-CPL`, closing and reopening shows "Active setup: CPL-(LR#0)-CPL" |
| 038-2 View ignores the experiment's setup; no measured points | 3.4 | "survey: in Inverse each Data row…": View ↗ on E1 opens its chart, and the exported CSV's header has the column "Measured — LP-(AIR#0)-LP-2" beside the model's |
| 038-3 row click does not load its setup | 3.3 | "survey: the first start…": pressing E1's row says "Loaded LP-(AIR#0)-LP-2 on its setup 'LP-(AIR#0)-LP'.", and the Setups step reads "Active setup: LP-(AIR#0)-LP" |
| 038-4 Remove without confirmation | 2.3 | "Remove asks first…": Remove shows "Remove this experiment and its file?", and No keeps the row and every `.experiment-json` |
| 038-5 revisions written when nothing changed | 2.1, 2.2 | "survey: browsing a kit writes nothing…" visits all nine Inverse steps, loads E1, presses Update experiment over its unchanged setup, and re-presses the chosen fit mode. The SHA-256 of every kit file outside `.oc/` is unchanged |
| 038-6 Setups step has no setup list | 3.2 | "survey: the first start…": the Setups step lists 42 saved setups, exactly one of them on: the active one |
| 038-7 status messages vanish | 5.1 | "survey: the first start…": the Loaded status is still shown after moving to the Setups step and back |
| 038-8 element labels differ from kit names | 3.5, 3.7 | "survey: the first start…": every item of the Select bay's list reads role and kit item ("Source: Laser", …). No text in the kit window names a slot the old way ("Linear polarizer #6"). "Remove asks first…": the readout names the slot chosen in the list |
| 038-9 one-answer import questions; 0 entries shown until applied | 5.3 (D6) | "survey: the first start…": the Launcher says "Imported N catalogue entries on start; none needed a decision." with nothing pending, and opening `optics-mueller` asks nothing (002 counted 8 questions). Headless: `ImportDialogTests`, `LauncherTests` (a dialog lists only the rows that ask, and counts the rest) |
| 038-10 hidden preconditions; no list selects an element | 3.6 | "Remove asks first…": choosing the last item of the Select bay's list selects that slot. "survey: the first start…": after New, Add says why it is withheld (`ExperimentAddReason`) |
| 038 Review / Exclude "not confirmed" | 4.2 | "survey: in Inverse…": Review opens its panel right under its row, not 5,300 px down. Headless: `DataStepTests` |
| 038 Plan cannot run on black-box kits | not a UI gap | 0051 `001` §2 |
| 038 "(colour only)" controls | 1.2 | Every choice is a `ToggleButton`; the surveys read their Toggle state (saved setups, fit mode) |
| 038 Light source / Detector offered with nothing to vary | 3.6 | Headless: `ExperimentControlsTests`, "the Experiments bay lists candidates and a click picks the element" (only the tunable source is offered; no other element is) |
| 038 Publish calibration contradicts itself | 4.5 | Headless: `StudyViewTests`, "pressing Publish calibration…" (the published run's preview says "Published: every item this fit run calibrates holds its values.") |
| 038 palette polarizer bound to a library preset | 3.8 (D7) | Headless: `SetupModelTests` (an empty slot, chip open). The rebuilt kits no longer snapshot the palette presets |
| 038 Record experiment piles up numbered names | 3.9 | Headless: `SetupModelTests` (the record note) |
| 038 dialogs and Explorer not checkable headless | 1.6 | The dialogs: "each native dialog starts in its own folder…" and the survey's Copy and zip facts. Explorer: see the end of this section |
| 002-1 buttons unreachable by keyboard and UIA | Phase 1 | "a kit opens through the real folder dialog and every step and bay is driven by id": the kit bar's buttons are found by id and offer Invoke, Toggle or Value, and every flow step of both constructors and every ribbon tab is pressed through UIA and reads On. Every other fact drives its controls by id the same way. Headless: `AutomationContractTests`, over every window |
| 002-2 every status message red | 5.1 | "survey: the first start…": on screen, the Loaded status's glyphs are grey, not red (red over green and blue below 40; refused is about 144) |
| 002-3 dialog start folders and filters | 5.2 (D5) | Folders and filters below |
| 002-4 Data rows unnamed; Review panel far away | 4.1, 4.2 | "survey: in Inverse…": E1's row is headed "LP-(AIR#0)-LP-2 [role] — badge", and Review opens under it |
| 002-5 pages too wide or tall; active setup not highlighted | 4.3, 3.2 | Headless: `ScrollExtentTests` at 800 × 600 and 1000 × 540, every kit-window step included. The active setup is the one saved setup toggled on |
| 002-6 re-attach widens the experiment; wrong profile pre-selected | 2.4 | Headless: `ImportWizardTests`, `KitWindowModelTests` (the wizard keeps the experiment's rows and profile) |
| 002-7 View ↗ never charts in Inverse | 3.4 | "survey: in Inverse…": View ↗ in Inverse opens the chart window |
| 002-8 chart without measured points; silent temp-file exports | 3.4, 5.8 | Main ("Remove asks first…") and Inverse: Export CSV… opens a save dialog in Exports, writes the typed path, and says "Saved <path>." |
| 002-9 small table, readout mismatches, "360°" | 3.7 | "Remove asks first…": the readout names the slot chosen in the list. Headless: `SetupModelTests`, "a turn is shown in whole degrees within 0 to 360" |
| 002-10 Copy: hidden item mapping, GUIDs, stale "(model not in the library)" | 4.4 | "survey: Copy experiments…": the mapping has its own list. Every row names its item by label and model, never by GUID or "(model not in …)". Cancel copies nothing. **This survey found the GUID still shown for preset and sample items; fixed below** |
| 002-11 repeated import questions, duplicate history names, UTC times | 5.3, 5.6 | Opening asks nothing (038-9 above). Local times: headless `KitStartWindowTests`. The duplicate names were already told apart by each row's folder line (`004` 5.6, deviation) |
| 002-12 console window; `environment.json` ignores the Preferences folder | 5.7 | "the app is a windowed program…": the exe's PE subsystem is 2 (GUI), the log file appears in the Preferences folder, and Exports and Catalogues exist after the first start. The photo fact's remembered picker folder went to `environment.json` in its sandbox's Preferences folder, and this account's `%APPDATA%\Softellect\Berreman\OpticalConstructor` does not exist |
| 002 "as priors" not saved | 2.5 (D4) | "survey: browsing a kit writes nothing…": As priors writes `"calibrationUse": 2` into `QZ-LR.study-json` and nothing else. After closing and reopening the kit, the Fit step shows As priors on |
| 002 Fit mode re-press rewrites the study | 2.2 | The same fact: re-pressing the chosen mode leaves every file's hash unchanged |
| 002 Remove reports nothing; Reload reports nothing | 2.3, 5.1 | Headless: `ExperimentControlsTests` and `InverseConstructorTests` ("Removed experiment 'X' and its file."), `KitWindowModelTests` ("Reloaded … from the kit.") |
| 002 Exclusion reason persists | 4.2 | Headless: `DataStepTests` |
| 002 photo never displayed | 5.5 | "survey: Choose photo… shows the photo itself beside its path": choosing a PNG for CPL1 copies it to `data/mount.png`, shows "Photo: data/mount.png", and draws the photo at most 160 px wide. Headless: `ItemsPanelTests` (a missing photo file shows its path alone) |
| 002 wording: "dropped entry files", unpack title, Export my library, "— →" | 5.2, 5.4 | "survey: Send kit…": the unpack dialog is titled "Choose an empty folder to unpack the kit into". The rest is headless: `ImportDialogTests`, `LibraryWindowTests`, `DataStepTests` |
| 002 Send kit progress too fast to see | not a gap | — |

**The dialogs, on the real desktop:**

| Button | Dialog | Start folder | Fact |
|---|---|---|---|
| Start screen → New kit… | "Choose a kit folder" | (title checked) | each native dialog… |
| Library → Register catalogue folder… | folder dialog | Catalogues | each native dialog… |
| Data step → Attach data file… | file dialog with a "Lab data" filter | the kit's `data/` | each native dialog… |
| Kit bar → Copy experiments from another kit… | "Choose a kit folder" | Kits | survey: Copy… |
| Kit bar → Send kit… | save dialog | Exports, which exists from the first start | survey: Send kit… |
| Start screen → Open kit from zip… | file dialog, then "Choose an empty folder to unpack the kit into" | Exports | survey: Send kit… |
| Chart → Export CSV… | save dialog | Exports | Remove asks first…; survey: in Inverse… |
| Items → Choose photo… | file dialog | — (the typed path is taken) | survey: Choose photo… |

**Explorer windows are still not shown.** The agent's processes run under a second Windows account inside the operator's
session, so an Explorer it starts never shows a window (002). "Show in Explorer opens the kit folder in an Explorer
window" checks that the desktop's shell belongs to the running account, and skips with that reason when it does not.
Run on the account that owns the desktop, it presses Show in Explorer and waits for an Explorer window on the kit
folder. That run has not been made.

## Found by this survey, and fixed

- **Copy experiments… still showed preset and sample items by GUID.**
  - The panel read "Laser — 239b0cfb-9938-5f3b-a767-c4252ea671b6 v1". `003` 4.4 had named only an item that
    instantiates a bare model; a preset or sample item fell through to the reference's GUID.
  - Fixed: `ItemModelInstance.itemModel` answers the item model in every case. `KitCopySource.modelCaptionOf` names it
    by name and version through the source kit's snapshots.
- **A quiet start left no log file.** The file was created by the first line logged, and a start without errors logged
  none. The App now logs its start, with its version and the Preferences folder. It also logs an unhandled exception
  before it stops, since a windowed program has no console to show one.
- **A missing photo file still drew an "image" under headless drawing**, which stubs every bitmap. The view now checks
  the file exists before decoding it.
- **The driver, not the app.** These failed in the driver, not in the app, and none of them changed the product:
  - A save dialog keeps its suggested name when the name box is set by WM_SETTEXT, so the driver types instead.
  - The chart window is titled by its chart, so the driver finds it by its own export button.
  - Two desktop facts must never run at once: xUnit ran the two classes side by side until they shared one collection.
  - A window's UIA element can be disconnected once a file dialog closes, so the driver finds the window again.
