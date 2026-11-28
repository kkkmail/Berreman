# 009 — The Optical Constructor after plan `007`: `006`'s fifteen comments, re-driven on the real desktop

2026-11-28. The operator ruled on plan `007` and asked for the whole plan, then: "Make sure to drive through UI once you
are done with the development." This is that drive, `007` Phase 10's re-survey. It is a report, not a ruling. The
implementation log is `008`. Nothing is committed: the operator commits.

## How this was done

- **A real desktop.** The agent ran in Windows session 1, the console session at 1920 × 1080. It drove the real
  `Softellect.Berreman.OpticalConstructor.App.exe`: the Release build of the working tree over `6d2653e6`, version
  10.1.402.7 (no bump was requested).
- **A sandbox per fact**, as in `005`: the app's own copy with an `appsettings.json` that puts every data location
  under the test's output folder, a fresh library, and the kits copied from `data/`, which were never opened in place.
- **The driver is committed** (`OpticalConstructor.Desktop.Tests`, FlaUI over UIA3). There are 29 facts, run one at a
  time in one xUnit collection:
  - `DesktopTests`: 7 facts, `003` 1.6's suite.
  - `DesktopSurvey.fs`: 7 facts, `005`'s survey of 038 and 002, brought to the pages.
  - `DesktopPagesSurvey.fs`: 15 facts, one per comment of `006`, named by their comments.
  Every control is found by its `ConstructorUiId` and pressed through its own UIA pattern. A native dialog is answered
  through its own controls. Colour is read from the screen's pixels, and the clipboard through the Win32 clipboard.
  Every fact skips in session 0. Run it with `--filter-trait "Category=desktop"`.
- **The second layer is the headless suites**: `OpticalConstructor.Tests`, and `OpticalConstructor.Ui.Tests` with
  `AutomationContractTests` and `ScrollExtentTests`.

## Results

Every figure is of the working tree as the operator will find it: after this survey's fixes and the ELEV gate's.

| Suite | Result |
|---|---|
| `dotnet build Berreman.slnx -c Release` | succeeded: 0 errors, and no warning but the exempt NU1701 / NU1902 advisories |
| ELEV gate (`-ExcludeRules ELEV004`) | OK: 0 findings in 13 projects, on its third run (`008`, "The ELEV gate") |
| `OpticalConstructor.Tests` | 1911 of 1911 passed |
| `OpticalConstructor.Ui.Tests`, whole suite | 1028 of 1029 passed, 0 failed. 1 skipped: AT-2's five-seed ensemble, an `Explicit` fact run on demand only (spec 0050 S12.2) |
| `OpticalConstructor.Ui.Tests`, `ui-smoke` | 273 of 273 passed |
| AT-1 to AT-4: the built kits equal the committed ones, file by file; AT-3 and AT-4 also open read-only | 8 of 8 passed |
| `BerremanTests.MuellerReconstructionTests`, the one solver class over the changed `CommittedKits` | 25 of 25 passed |
| `OpticalConstructor.Desktop.Tests`, real desktop | 28 of 29 passed, 0 failed. 1 skipped: Show in Explorer, for the account reason below |

`BerremanTests` as a whole (about 52 minutes) was not run: no solver project changed, and `BerremanTestSupport` only
gained the two new kits' cases.

## `006`'s comments, met

"Shown by" names the real-desktop fact. Facts beginning "006-" are `DesktopPagesSurvey.fs`; "survey:" facts are
`DesktopSurvey.fs`; the rest are `DesktopTests.fs`. Where the headless layer is the proof, it is named.

| Comment | Met by (`007`) | Shown by, on the real desktop |
|---|---|---|
| 006-1a the pages as the top row; Inverse adds Data … Results | 2.1 | "006-1: the page row leads the kit window…": Items, Setups, Experiments, Main, Data, Unknowns, Plan, Fit, Results, Kit, in that order, in one row |
| 006-1b no "Switch to" | 2.1 | the same fact: the switch reads "Main" in Inverse, and no text in the window starts "Switch to" |
| 006-1c the kit's operations and Publish behind the last button | 2.2, 8.6 | the same fact: Send kit…, Show in Explorer, Copy experiments…, Reload changes, Close kit and Guide… show on no other page, and on Kit, with the kit's two fit runs under Publish |
| 006-1d no Experiments tab; the table's tabs only where the table is | 2.3, 5.3 | the same fact: no page shows the Add tab. A setup's Edit shows every tab, and none is named Experiments |
| 006-1e dynamic labels selectable and copyable | 1.4 | "006-12, 006-1: a dynamic button's menu copies its name…". Headless: `AutomationContractTests` (every text carrying an id is a `SelectableTextBlock`, on every page of both constructors) |
| 006-1f tables copyable as CSV | 1.4, 7.2, 8.5 | "006-12, 006-1…": the Data matrix's Copy as CSV puts "Prep 1,…" on the clipboard, and Results' fitted values put "Parameter,Start,Fitted,σ" |
| 006-1g one state at a time | 1.2 | "006-6: … New study is the page alone": while a study is made, the studies are hidden |
| 006-1h edits in place, under the thing edited | 1.2 | "006-1, 006-2: an item opens under its row…": the details and the edit open between the item's row and the next one. "006-6": the parameter's scope opens under its row, above the next |
| 006-1i Edit before any change | 1.2 | "006-1, 006-2": no label field until Edit |
| 006-1j Save withheld until something changed | 1.2 | "006-1, 006-2" and "006-3": Save is disabled until the label is typed into, and on an unchanged setup Edit |
| 006-1k Cancel asks when something changed | 1.2 (and leaving, D11) | "006-1, 006-2": Cancel asks "Discard your changes…", No keeps the edit, Yes drops it, and every kit file's hash is unchanged. "006-3": an unchanged Cancel asks nothing |
| 006-1l help on every page | 1.6, 9.3 | "006-1": every page of the Inverse constructor heads with its help, opened and read |
| 006-2 Items: details under the item, a second press closes them, the library in its own window, Edit first | 4.1–4.3 | "006-1, 006-2" (the details, the second press, Edit) and "006-2: New item… opens the library chooser in its own window, as large as its screen allows" (1600 × 1200, shrunk to the working area) |
| 006-3 Setups: no Record experiment, no setup grid; Copy with ` # <N>`; Rename; choose and unchoose; the line with Edit on it and the read-only table under it; Save and Cancel | 5.1–5.7, 1.3 | "006-3": no Record experiment or setup-grid button. Edit and Copy… sit on the setup's line, and the read-only table (no tabs) under it. A second press unchooses it. Copy… prefills `<name> # 1`. Rename: "006-12" renames in place by the menu; headless `SetupModelTests` re-points the experiments |
| 006-4 Experiments: no table or tabs; nothing to add without a setup, and why; details under the experiment; OK / Cancel / Show; Edit first | 6.1–6.4 | "006-4": no table and no tab. The details open between the experiment and the next one. A second experiment over the chosen setup asks "… already use …" with OK, Cancel and Show, and Cancel creates nothing. With the setup unchosen, the page says "Choose a setup in Setups first…" and offers no New experiment |
| 006-5 Data: no setup shown; a gridded matrix; data hidden until a press, under the entry; a second press closes it; no-data entries apart, with ⚠; labels copyable | 7.1–7.4, D4 | "006-5": no "Active setup" or "Loaded setup" text. The matrix heads "Prep 1" and "Analyzer …". E1's data opens under its entry and closes on a second press. In `multilayer-thin-film` every entry begins "⚠ " and is filled apart from a data entry, measured on screen. The matrix copies as CSV |
| 006-6 Unknowns: no setup shown; new study or a chosen one, never both; a parameter edited under it; Scope and Link only while editing it | 8.1, 8.2 | "006-6": no setup text. No scope choice shows while browsing. In Edit, a pressed unknown shows its scope under it, alone. New study hides the studies |
| 006-7 what Plan does | 8.3, 1.6 | "006-7, 006-8": Plan's help opens on "The Measurement Planner says…" |
| 006-8 what Fit does, and how its runs are made | 8.4, 1.6 | "006-7, 006-8": Fit's help names New fit run…. The runs are listed newest first, and New fit run… offers the stages and the calibration choices. "survey: browsing a kit writes nothing…": the form's choices write nothing; a started run records As priors in the study, read back after the kit is reopened |
| 006-9 a step-by-step inverse guide | 9.2 | "006-9": Guide… on the Kit page opens the inverse guide, numbered from "1. ", naming Data, Unknowns, Fit and Results. The Launcher's Guide… opens it too |
| 006-10 the folder above the last kit | 3.4 | "006-10": after a kit under `elsewhere/` is opened, Open kit… starts in `elsewhere`, and again after the app restarts |
| 006-11 tooltips after a configurable delay, 1 s by default; Options on the Launcher | 1.5 | "006-11": Options shows the delay 1000. The kit's commands carry their help as UIA help text. Hovering New item… shows its help as a tooltip, no sooner than 0.7 s |
| 006-12 a dynamic button's menu: Copy name and Rename | 1.3 | "006-12, 006-1": right-clicking an item offers Copy name and Rename…. Copy name puts the label on the clipboard, and Rename… opens the name field in place, holding the label |
| 006-13 View withheld when there is no chart (`absorbing-biaxial-noisy`) | 6.6, D6 | "006-13": each of the kit's five experiments offers View chart ↗, and it opens the Mueller-element chart; no "No chart for …" |
| 006-14 two direct kits from the scripts, and how to build one | 9.1, 9.2 | "006-14": `multilayer-thin-film` and `langasite-on-silicon` open. Each of their four experiments charts R and T, and the exported CSV's header names "Intensity (R)" and "Intensity (T)". The direct guide names Items, Setups and Experiments. Headless: AT-3 and AT-4 compare every point with the scripts (`008`, Phase 9) |
| 006-15 `.optics-kit` in every kit, with created and last saved; open the file | 3.2, 3.3, D3 | "006-15": every kit in `data/` holds `.optics-kit` with `created` and `modified`. Open kit… is a file dialog titled "Open a kit: choose its .optics-kit file", filtered to "Optics kit (*.optics-kit)", and typing the file opens the kit. Every other fact opens its kit that way |

**Show in Explorer is still skipped**, for `005`'s reason: the agent's processes run under a second Windows account
inside the operator's session, so an Explorer they start never shows a window. Run on the account that owns the
desktop, the fact presses Show in Explorer and waits for an Explorer window on the kit folder.

## Found by this survey, and fixed

- **Edit and Copy… were not on the setup's line.** The view put the slot line, then the revision, then Edit and Copy…
  on a row of their own. `007` 5.1 and `006-3` put them on the line. They now flow on the line itself, and move to the
  next row only when the line is wider than the page. `SetupSaveLoadTests` checks it headless.
- **The guide's steps were not exposed to UI Automation.** Their panel carried its id through the plain
  `automationAttribute`, which a `StackPanel` does not expose. It now uses `groupAttributes`.
- **Three windows were outside the scroll law.** The Options, Guide and library chooser windows, all new in `007`, were
  not in `ScrollExtentTests`, which `AGENTS.md` requires of a new window. That law also holds the automation contract,
  so the guide's panel had gone unnoticed. They are in it now. The law then found that the chooser's offers ran
  2386 px wide in a 1000 × 540 window. Their captions name binding, model, version and GUIDs, as spec 0050 S7.2 has
  them, so the captions now wrap, and the list no longer scrolls sideways.
- **The driver, not the app.** These failed in the driver and changed nothing in the product:
  - Opening a kit is now a file dialog for its `.optics-kit`, so the driver types the file.
  - An owned window (Options, Guide, the chooser) is its owner's child in UI Automation, so the driver looks under
    each top-level window too.
  - A window's element can be disconnected once a file dialog closes, so the driver finds the window again.

`008` records what the headless tests found before this survey: Edit on data-less data, Apply split's mode, Copy's
withheld Create, and AT-4's langasite gyration.
