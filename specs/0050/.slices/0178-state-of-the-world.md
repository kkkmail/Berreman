# Where we are

Step 0178 is an `IMPLEMENT` step in spec 0050 S5.2, the kit start screen and the Launcher (obligation S52-VIEW). It
adds the start screen's view over the model, messages and update of steps 176–177 (001 §F 'Start-screen actions',
'History'; 001 §O 'Automation'). The view is New kit…, Open kit…, the status, the offer with its opposing pair on one
row, the filter box and the history list, every control under a stable `ConstructorUiId`.

Step 179 adds the window that hosts the view. This is attempt 2. Attempt 1's test file did not compile: it lacked two
opens. A local analyzer run then found one ELEV013P in the view, which this round also fixes. Gate verification
belongs to the arc-runner.

# What's working

- Added the kit start screen's 13 `ConstructorUiId`s: nine fixed ids, plus row, star, Locate… and Remove from
  history over a row's position.
- Added `KitHistoryEntry.lastOpenedCaption` (`Last opened yyyy-MM-dd HH:mm UTC`) and `KitFolder.notAKitCaption`.
- Added `kitStartView`, a pure projection that scrolls over a 520-wide `MinWidthPanel`:
  - New kit… / Open kit… pick a folder through `picker.pickThen`;
  - the status and the offer, with its positive / negative pair on one row;
  - a filter box that re-lists on each change;
  - one row per kit with its name, folder, last open and star; a missing folder's row is greyed and inert, with
    Locate… and Remove from history.
- Added `KitStartWindowTests`: five ui-tests facts (ids, rows, missing rows, offers, filter) and two ui-smoke facts
  (star / remove / open a row; New kit and Open kit).

# Tests

No gate was run or reported: invariant 6 leaves gates to the arc-runner. It must run:
- build;
- elevation;
- constructor-unit-tests, all discovered;
- ui-smoke, all discovered;
- ui-tests, `OpticalConstructor.Ui.Tests.KitStartWindowTests` selected;
- the fourteen s20 section invariants.

Attempt 1 failed the build, so this round checked its repair locally. These are worker checks, not gate results; the
logs are under `.artifacts/0178-attempt-02-*.log`:
- the solution builds with 0 errors and 0 warnings;
- `KitStartWindowTests` passes 7 of 7;
- the ELEV analyzers (ELEV004 excluded) report 0 new findings over Controls, Ui, App, TestWindows and TestWindows.App;
- the ui-smoke suite passes 226 of 226 (224 + 2);
- `OpticalConstructor.Tests` passes 1,186 of 1,186.

The seven new facts are `OpticalConstructor.Ui.Tests.KitStartWindowTests.`:
- `the kit start screen UiIds are the stable intent-named ids` (ui-tests);
- `the history rows show name, folder and last open in display order, each with its star` (ui-tests);
- `a missing folder's row is greyed with Locate and Remove from history, and a present row offers neither`
  (ui-tests);
- `each offer shows its text with its two buttons on one row, Cancel to the right` (ui-tests);
- `typing into the filter box narrows the list` (ui-tests);
- `pressing a row's star, its Remove from history and a present row sends StarKit, ForgetKit and OpenKit`
  (ui-smoke);
- `New kit and Open kit open the folder the picker answers` (ui-smoke).

The YAML carries the counts measured locally this round.

```yaml
gates:
  constructor_unit_tests: 1186
  ui_smoke_tests: 226
```

# Architecture

- **The view is a pure projection.** `kitStartView` reads only the model; every press dispatches a `KitStartMsg` or
  calls the context's picker, whose answer is dispatched. The buttons and the tests send the same messages.
- **Shared look.** The verbs are `MaterialsWindowView.verbButton`, the offer pair is `MaterialsWindowView.actionButton`
  with `positiveBackground` / `negativeBackground`, and the status uses `messageColor`. The scrolling shell is
  `twoPane`'s ScrollViewer-over-MinWidthPanel shape.
- **Ids are AutomationIds over positions.**
  - A row's four ids are positional (`ConstructorUiPosition`, 0 first). The row Border is keyed by its id, as
    `verbButton` keys its box.
  - The offer block is keyed by its offer and folder, because `actionButton` subscribes its press only once per id.
- **Presence lives in the handler.** A row's Border always handles its press and matches the row's presence inside the
  handler. Rows keyed by position can switch between present and missing kits without relying on FuncUI to drop a
  subscription.

# Deferred

Nothing. Every requirement of the slice landed in this round.

# Gotchas

- **Avalonia 12.1.4 / FuncUI 2.0.0 make `TextBox.Watermark` obsolete** (FS0044). The filter uses
  `TextBox.placeHolderText`, and the test reads `PlaceholderText` (`.artifacts/0178-textbox-dsl-probe.log`).
- **`actionButton` keeps its first press handler per id** (`SubPatchOptions.OnChangeOf id`). Key an ancestor when an
  id's payload can change, as the offer block does.
- **A null text is ignored.** The filter box drops a null `Text` before it reaches `entriesMatching`, which trims.
- **`AppContext` is ambiguous in the Ui tests.** `open OpticalConstructor.Ui` brings in the Ui `AppContext`; use
  `System.AppContext.BaseDirectory`.
- **ELEV013P binds private functions.** A private helper whose only parameter is one of our types (here
  `KitStartModel`) is refused. Fold it into its caller as a local value, or make it a member.
- **Open the declaring modules in tests.** `KitId` is in `Domain.LibraryEntryFiles`, and `CatalogueImportContext.create`
  is an extension in `Storage.CatalogueImportStore`.
- **Restated fixture values.** The test restates `KitStartTests`' five entries and step 173's holder, both private to
  `OpticalConstructor.Tests`, which the Ui test project does not reference.

# Changelog

- 2026-11-08 — Step 0178, attempt 2: opened the two modules the test file needed (the build's three FS0039), and moved
  the status rows into `kitStartView` (ELEV013P). Checked locally: build, the class, ELEV, ui-smoke and constructor.
- 2026-11-08 — Step 0178: added the kit start screen view (`kitStartView`), its 13 ids and two captions, and
  `KitStartWindowTests` (5 ui-tests and 2 ui-smoke facts).
