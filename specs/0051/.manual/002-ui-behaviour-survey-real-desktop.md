# 002 — The Optical Constructor UI on a real desktop: closing the gaps of 0050 `.manual/038`

2026-11-26. Requested by the operator: "Close the UI gaps that Windows session 0 does not allow to test, then write an
updated report". This updates `specs/0050/.manual/038-ui-behaviour-survey.md` (038). It is a report, not a ruling. No
product file was changed.

## How this was done

- **A real desktop.** The agent ran in Windows session 1 (the console session, 1920 × 1080), not session 0, and drove the
  real `Softellect.Berreman.OpticalConstructor.App.exe`, not the composition-root windows under Avalonia.Headless.
- **The same product code as 038.** The build is the Release build of `ac97eff1` (Launcher footer: Version 10.1.402.7).
  Nothing under `Berreman/OpticalConstructor/` changed between 038 (`28a9e7a9`) and `ac97eff1`, so 038's findings apply
  to this build unchanged.
- **A sandbox, as in 038.**
  - The build output was copied into the agent's scratchpad. Its `appsettings.json` put every data location there:
    the library database, backups, preferences, kits, exports, catalogues and online-source snapshots.
  - The library was fresh, with the shipped `Catalogue/` imported (72 decisions, all accepted).
  - The kits were copies of `data/optics-mueller` and `data/absorbing-biaxial-noisy`. The committed kits were never
    opened, and `git status` stayed clean.
- **The driver.**
  - Windows UI Automation (UIA) from PowerShell found each control.
  - Every press was a real mouse down and up at the control's centre. Before each press, the driver checked by hit-test
    that the point belonged to that control, not to something covering it or scrolled over it.
  - Text was typed as real keystrokes. Native dialogs were filled through their own name box and default button.
  - Colours were measured from screen pixels.
  - Screenshots and the driver stayed in the agent's scratchpad and are not committed.
- **What still could not be checked: Explorer windows.** The agent's processes run under a second Windows account inside
  the operator's session. `explorer.exe` started by that account exits without a window, even for a world-readable folder
  such as `specs/0051`. So Show in Explorer and the eight Open folder buttons were checked only this far: the app starts
  `explorer.exe` on the folder (process seen), and a missing folder is reported. The window itself needs a session where
  the app runs as the desktop's own user.

## 038's gaps, closed

### Buttons that open an operating-system dialog

All of these were pressed for real, and each dialog was answered. "Start folder" is where the dialog opened.

| Where | Button | Dialog title, start folder, filter | What followed |
|---|---|---|---|
| Kit bar | Send kit… | "Send kit"; Exports **only if that folder exists**, else Desktop; name `optics-mueller`, Zip archive | Wrote the zip (141 files, 196,682 bytes): "The kit was sent: 141 file(s) zipped." It took under a second, so the progress bar and Cancel were never visible |
| Kit bar | Show in Explorer | — | Starts `explorer.exe` on the kit folder; the window is not observable here (see above) |
| Kit bar | Copy experiments from another kit… | "Choose a kit folder", in Kits | The other kit's experiments as check boxes; Copy appears on the first tick. Copying E2 wrote its experiment, setup, 3 items, data file and 6 library snapshots: "Copied 1 experiment(s) and added 3 item(s) from absorbing-biaxial-noisy." |
| Start screen | New kit… | "Choose a kit folder", in Kits | On an **empty** folder: no import decisions; writes `.kit-json`, `data/`, `library/`, `.oc/lock` and opens the kit window |
| Start screen | Open kit… | "Choose a kit folder", in Kits | Import decisions (8, item 9 of 038), Apply, then the kit window, 820 × 770 |
| Start screen | Open kit from zip… | "Open kit from zip", in Exports, Zip archive | A **second** dialog, again titled "Choose a kit folder", asks where to unpack; nothing says so. Then the same 8 decisions and the kit window over the unpacked copy (142 files) |
| Data step | Attach data file… | "Attach measured-data file"; the first time, **the last folder any dialog of the app used**; filter All | The column-mapping wizard; Attach rewrote the experiment (see finding 6) |
| Data step | Split a file by key… | "Split a lab file by key", in the kit's `data/` | The wizard on Phone ROI CSV; Split shows an in-page panel of groups ("9 matched, 0 offered, 0 unmatched"); Apply split re-attached the file to the 9 experiments |
| Items | Choose photo… | "Choose item photo", Desktop, filter All | Copies the file to `data/photo.png`, rewrites the item, shows "Photo: data/photo.png" and Remove photo. **The photo is never displayed**, only its path |
| Library | Register catalogue folder… | "Register catalogue folder", **Desktop** | 44 decisions, all "Reuse the held entry"; then the folder is listed under Catalogue folders |
| Library | Export shown to catalogue… | "Export to catalogue folder", in Catalogues | "Exported 44 entries to the catalogue folder …; their versions are now pinned." (45 files with `.catalogue-json`) |
| Library | Export my library… | "Export my library to a catalogue folder", in Catalogues | With no user-made entries: "there is nothing to export: catalogue folder '…'" |
| Materials | Export shown to catalogue… | "Export to catalogue folder", in Catalogues | "Exported 15 entries …" |
| Materials | Import glass catalogue… | "Import glass catalogue", **Desktop**, filter "Zemax glass catalogue" | Decisions headed "Import 3 dropped entry files" (nothing was dropped); Apply took Glass (4) to Glass (7) |
| Data locations | Open folder (× 8) | — | An existing folder starts `explorer.exe`. A missing one says "The folder could not be opened — the folder does not exist: …" |

**Corrections to 038:**
- New kit… does not show import decisions on an empty folder.
- Send kit… opens in the exports folder only when that folder already exists. Nothing in the app creates it
  (`KitZipPicker.ofWindow`: "no folder is made").

### Controls marked (colour only), now measured

The selected chip is green `#BAE0BA` and the others are grey `#E8E8E8`, everywhere.

| Control | Anything besides the colour? | Written to the kit? |
|---|---|---|
| Fit → Fit mode Joint / Staged | No; Run joint, Run stage 1 and Run stage 2 all stay | Yes: `QZ-LR.study-json` `mode`. Pressing the mode already chosen rewrites the file with identical content |
| Fit → Stage 2 holds the calibration: fixed / as priors | No | No. The study file has no field for it, so the choice lives only in the open window |
| Plan → D-optimal / A-optimal | No (and this kit has no Run, 038) | No |
| Unknowns / Plan / Fit → study chip `QZ-LR` | No: the kit has one study, already chosen | No |
| Unknowns → New-study experiment chips | No, until a study name is typed (038 item 10) | No |

### Data step: Review and Exclude point work

038 left these "not confirmed from the UI". They work.

- **Review opens its panel at the bottom of the Data page.**
  - The panel appears below Split a file by key…, about 5,300 px under the first row's Review in a maximized window.
    The page does not scroll to it, so nothing visible changes where the user pressed. That is what 038's driver saw.
  - The panel shows "Points of LP-(AIR#0)-LP-2 — validated (19 points)", a Reason box, a small scatter of reading
    against point **number** (not dial reading), and one Exclude per point.
- **Exclude point.**
  - With the typed reason "glare on the sensor", it said "Excluded point 1 of LP-(AIR#0)-LP-2: glare on the sensor."
  - It rewrote `LP-(AIR#0)-LP-2.experiment-json` only. The lab file's hash was unchanged.
  - The matrix badge, the row status ("18 point(s) kept, 1 excluded") and the panel ("excluded — glare on the sensor")
    all updated.
- **Empty reason.** An empty reason is refused: "Not excluded — an exclusion needs a reason." The kit status line stays
  on screen above the page, so this message is visible even 5,000 px down.
- **The reason persists.** The Reason box keeps the last reason after an exclusion, so the next Exclude reuses it
  without asking.

### Rotate about the middle of the incident face

- With an element selected (a real click on it in the table), R1 ± turns it about the beam axis. The option makes no
  difference there, as expected: that axis passes through the face's middle.
- With R2 = 45°, turning the option on shifts the drawn polarizer by about 4 px at once, on the element already tilted.
  The effect is real but barely visible at the default zoom.
- Reset asks inline: "Reset the selection's rotations? Yes / No". Yes clears all three angles.

## New findings that only a real desktop shows

1. **Most buttons cannot be reached by keyboard or found by UI Automation.**
   - The cause: `MaterialsWindowView.actionButton` and `verbButton` draw a `Border` with a pointer-press handler, not a
     `Button`. Every button in these windows is made this way: the kit window (kit bar, flow strip, ribbon, bays,
     experiment rows, View ↗, Remove, slot chips), the start screen, the import dialog, the wizard, Data locations,
     Library and Materials.
   - The Border is focusable by neither Tab nor UIA. The ids set on it are not in the UIA tree: `SendKitButton` and
     `SaveSetupButton` are absent, while `NamingTemplateField` and `KitStatus` are present.
   - The kit window exposes **0** UIA buttons. Of the windows checked, only the Launcher (5) and the chart window have
     real ones.
   - Tab in the kit window cycles through just the Naming template field, the Setup name field and the slot chips.
   - So a keyboard-only or screen-reader user cannot operate the kit window. The external UI-Automation layer that
     AGENTS.md plans (FlaUI / Appium) cannot find these controls by id either, only by their caption text.
2. **Every kit status message is drawn in firebrick red, `#B22222`.** `kitStatusRows` uses
   `MaterialsWindowView.messageColor`, "the library windows' refusal colour". So "Loaded setup …", "The kit was sent …"
   and "Copied 1 experiment(s) …" look like errors.
3. **Dialogs start in inconsistent folders, and some filters are too broad.**
   - Exports folder, only if it exists: Send kit… and Open kit from zip….
   - Catalogues: both Export… buttons.
   - The kit's `data/`: Split a file by key….
   - Desktop: Register catalogue folder…, Import glass catalogue… and Choose photo….
   - The last folder any dialog of the app used: the first Attach data file…. Only after that does `environment.json`
     remember a "measured-data" folder.
   - Choose photo… and Attach data file… filter on "All".
4. **The Data files list never names its experiment.**
   - Each row shows the scan ("Linear polarizer #6: vary Dial reading over 0…360 ° (19 pts), capture Transmitted
     (T)"), the file and the status, but not the experiment's name.
   - In `optics-mueller` the first eleven rows read identically ("file: data/raw/final/lp_lp.csv").
   - With the Review panel at the bottom, the page is about 6,600 px tall even maximized.
5. **Several pages are far wider or taller than the window.**
   - The Unknowns step's new-study experiment chips sit in one row that never wraps: 57 chips reach x ≈ 8,250. The whole
     page scrolls sideways about four screens, and the "Experiments: …" line does not wrap either.
   - At the default 820 × 770 kit window:
     - the Data step's experiment matrix is cut off partway through its second column, and its third is out of view;
     - `Materials…` sits past the right edge; both are reached only by scrolling sideways.
   - In the Experiments bay the Setup section (Create setup, Save setup, Saved setups) comes after every experiment row.
     With 57 experiments that is about 4,000 px down. The loaded saved setup is not highlighted in that list.
6. **Re-attaching a file through the wizard widens a keyed experiment to the whole file.**
   - `OtherOpticAxisCut` (copied from `absorbing-biaxial-noisy`) held `rows: KeyedDataRows ["OtherOpticAxisCut"]`,
     which is 4 points.
   - Attach data file… on the same file, with the Mueller-matrix observations profile, wrote `rows: AllDataRows`.
     The experiment now holds 44 points from all four samples.
   - The wizard offered no row or key choice, and nothing on screen warned.
   - Separately, the wizard pre-selects the Intensity series profile, not the profile the experiment already uses.
7. **View ↗ never opens a chart in the Inverse constructor for this kit.**
   - `chartForExperiment` returns an empty chart unless a Sample on the table is bound to a library entry
     (`experimentChartVisible`). `optics-mueller`'s objects are kit items over a black-box Mueller model.
   - So in Inverse, View ↗ did nothing even with the row's own setup loaded, and nothing says why.
   - In Main, the same press opened the chart window, as in 038 item 2.
8. **The chart window: forward model only, and silent exports.**
   - It renders well (ScottPlot) with a live crosshair readout ("x=300, y=0.6638").
   - It draws only the forward model, scaled 0–1. The experiment's measured points (readings in the thousands) are
     absent (038 item 2).
   - Export PNG and Export CSV open no dialog and show no message. They overwrite the fixed files
     `%TEMP%\experiment-chart.png` and `%TEMP%\experiment-chart.csv`.
9. **The table is small, and its readout disagrees with the drawing.**
   - In a maximized window the table is drawn about 400 × 240 px at zoom 1.00×. Its elements are unlabelled thin boxes,
     and empty slots are dashed.
   - A real click on an element selects it: "Selected: Element 4 (Linear polarizer) … zoom 5.0× bound: Ideal linear
     polarizer". The drawing does not change scale despite "zoom 5.0×".
   - The readout names the library binding, not the kit item LP1 the slot bar shows (038 item 8).
   - After R1 + three times and R1 − three times, the readout says R1 **360°**, not 0°.
   - The wheel over a fully visible table zooms it (1.00× → 1.10× → 1.21×) and leaves the page still, as designed.
10. **Copy experiments from another kit: the item mapping is hidden.**
    - The item rows ("add each to this kit, or use an item it already has") share the 160 px list with the experiment
      toggles. They appear only after scrolling that small list past every experiment, and nothing says to scroll.
    - Each row names its library entry by GUID ("OtherOpticAxisCut — 4d13ecb1-2006-576f-99e7-e8bc266e8624 v1").
    - Right after Copy, the Items step listed the copied item as "(model not in the library)". After closing and
      reopening the kit it resolved to "Absorbing triclinic plate 100 um v1 (Sample by stack v1)".
11. **Opening a kit asks the same questions every time, and some labels are confusing.**
    - Reopening `optics-mueller` asked the same 8 one-option "Reuse the held entry" decisions again (038 item 9).
    - The start screen's history shows the original kit and its zip copy under the same name, `optics-mueller`. They
      differ only by the path line, which is easy to misclick.
    - History times are in UTC ("Last opened 2026-11-26 18:05 UTC" at 13:05 local).
12. **Two smaller real-process facts.**
    - The App is a console program (`OutputType Exe`): a console window opens behind the Launcher and shows the log.
    - `environment.json` (picker folders, theme, units) is always written to
      `%APPDATA%\Softellect\Berreman\OpticalConstructor`, whatever `PreferencesFolder` says. The Data locations window
      lists only the configured Preferences folder.

## What works on the real desktop

These were confirmed beyond what 038 drove.

- Every native dialog above opens over its window, takes a path and returns it to the app.
- **Outside changes are offered on activation.**
  - An experiment file was edited on disk while the kit window was in the background. Activating the window offered
    "Changed outside the app: BullshitCheck-2.experiment-json. Reload them, or keep your version?" with Reload and
    Keep mine.
  - Reload took the change without writing anything. It shows no confirmation message.
- **Slot bar by keyboard (`SlotBarView`).**
  - With Obj 1 open, typing **l** picked LR and loaded the held setup ("Loaded setup 'LP-(LR#0)-LP'.").
  - Tab moves a visible focus ring from chip to chip. UIA does not report that ring as focus.
- **Window sizes.** Every window opened inside the 1920 × 1080 working area (Launcher 380 × 300, start screen
  720 × 560, kit window 820 × 770, Library and Materials 1150 × 800, import dialog 900 × 640, wizard 1000 × 700, chart
  980 × 680).

## 038's findings, re-checked on the real app

- **Item 1 (empty table on open):** confirmed. The kit window opens on `Source: — ▾`, `Detector 1: — ▾`.
- **Item 2 (View):** confirmed in Main: nothing on the default table; a chart with the row's setup loaded. In Inverse it
  is worse: nothing even with the right setup (new finding 7).
- **Item 4 (Remove):** confirmed. Remove on the last row deleted `OtherOpticAxisCut.experiment-json` at once, with no
  confirmation and **no status message**.
- **Item 5 (silent revisions):** confirmed. Save setup with no edit wrote "Saved setup 'LP-(LR#0)-LP', revision 2." Fit
  mode re-pressed rewrites the study file too.
- **Item 8 (labels):** confirmed on screen. The Experiments bay offers `Light source #1`, `Sample #2`, `Detector #3`,
  `Linear polarizer #4`, `Sample #5`, `Linear polarizer #6`, while the slot bar shows Laser, LP1, AIR @ 0°, LP2, Phone.
- **Item 9 (import decisions):** confirmed, and repeated on every reopen (new finding 11).

## Suggested reading order for the operator

1. **New finding 1 (keyboard and UI Automation).** It decides whether the planned external UI tests are possible at all,
   and it touches every window.
2. **New finding 6 (re-attach widens the experiment) and 038 items 4–5 (Remove, silent revisions).** These change kit
   files without the user meaning to.
3. **New findings 4, 5 and 7 (unnamed data rows, very long and wide pages, View in Inverse).** Together with 038 items
   1–3, they are why the Inverse flow is hard to follow.
4. **New findings 2, 3 and 8 (red success messages, dialog start folders, silent chart exports).** These are small,
   local fixes.
