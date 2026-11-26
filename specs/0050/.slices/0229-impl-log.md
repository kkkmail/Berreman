# Step 0229 — impl log

## Progress

- [x] UiIds: `kitCopyItemList`
- [x] ConstructorUiIdOfDomain: `kitCopyItemChoice`
- [x] KitWindowView: the mapping table inside the copy panel's scroller
- [x] KitWindowTests: three census facts
- [x] ScrollExtentTests: `tallestKitBar` with forty chosen experiments and twenty item rows
- [x] Build (second attempt: 0 warnings, 0 errors), KitWindowTests 19/19, ScrollExtentTests 14/14
- [x] Mutants (`.artifacts/0229-mutants/`): eight of eight caught; sources restored byte for byte
- [x] Final development runs: build, constructor-unit-tests, ui-smoke, ui-tests (selected), elevation, invariants

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`: `kitCopyItemList` (`"KitCopyItemList"`), with a
  one-line doc, after step 226's copy ids in the kit-bar group.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`:
  `kitCopyItemChoice (sourceItem : Kit.ItemId) (choice : Kit.ItemCopyChoice)`, beside `kitCopyExperimentToggle`.
  - `AddCopiedItem` gives `KitCopyItemChoice_<source GUID>_Add`.
  - `MapOntoKitItem id` gives `KitCopyItemChoice_<source GUID>_<kit item GUID>`.
  - It has a one-line doc citing S7.2.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitWindowView.fs`:
  - `open OpticalConstructor.Domain.Kit`;
  - a new private `kitCopyItemRows model copySource dispatch`, with a two-line doc citing S7.2:
    - nothing while `itemRows` is empty;
    - otherwise the wrapping intro line, then a vertical `StackPanel` carrying `kitCopyItemList`, keyed by the id, of
      one block per row;
    - a block is the wrapping line `<label> — <entryReferenceOpt's describe>`, then a horizontal `WrapPanel` of
      `Add` and `Use <label>` for each `model.kitItems` item;
    - each choice carries `kitCopyItemChoice` and sends `ChooseCopiedItem (source id, choice)`;
    - the row's choice is `MaterialsWindowView.actionButton … positiveBackground`, the others `verbButton`;
  - `kitCopyRows`: the scroller's `StackPanel` now holds the toggles, then `kitCopyItemRows`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`:
  - `open OpticalConstructor.Domain.LibraryEntryFiles`, for the `ItemModelVersionId.entryReference` extension;
  - fixtures, all test-local and private:
    - `laser600Choice`, `plateChoice` (step 210's shipped models);
    - `pieceOf`;
    - `scaledLaser600`, which sets `intensityScale`;
    - the source's `copiedLaser600` (0.75) and `spareLaser600` (0.5), and `sourceQz`;
    - the window's `ownLaser600` (default parameters, same label and preset);
    - the bench's `benchLaser600` (0.25, a third id);
    - `itemE1` and `itemE2` through `assignedExperiment`;
    - `itemSourceKit`: `recordItem` ×3, then `recordExperiment` ×2, then close;
    - `openedHolding`, `itemFilesIn`, `ReopenedKitCopy` / `reopenedCopy`, `fillOf`, `choiceBoxesIn`,
      `mappingIntrosIn`;
  - three facts, exactly the census identities (see Testing state).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`: `tallestKitBar` now has:
  - all forty experiments chosen;
  - twenty source items (`Laser600 piece n`), each with a row, alternating `AddCopiedItem` and `MapOntoKitItem`;
  - `kitItems` of three bench lasers, so each row offers `Add` and three `Use` choices.

  Its doc says so. The assertion is unchanged.

## Decisions

- **"Its doc says so in one to three lines citing spec 0050 S7.2" is read as the table's own doc.**
  - The table is its own private function, `kitCopyItemRows`.
  - Its two-line doc says the table shares the toggles' bounded scroller, so the bar grows no taller and no control
    leaves the screen.
  - Step 226's three-line `kitCopyRows` doc is unchanged.
- **The block's line falls back to the label alone when `entryReferenceOpt` is `None`.** Every
  `ItemModelInstance` case answers `Some` today, but the member is an option, and a match must cover `None`.
- **A choice is chosen when it equals the row's choice.**
  - A `MapOntoKitItem` naming an id the window's `kitItems` lacks marks no box.
  - The copy's own `itemsToAdd` refuses that row with a typed error (step 227). The view adds no second check.
- **"Both source pieces sharing that model" follows step 227's census wording.**
  - Step 227 has two source pieces, A and B, of one preset. Only B is bound, and the target T has a third id and
    different parameters.
  - Here the source holds the copied Laser600 (B, `intensityScale` 0.75, assigned by E1) and `Laser600 spare` (A,
    0.5, assigned by nothing).
  - T is `Laser600 bench` (0.25, a third id). The bench also holds `ownLaser600`, the same preset and label under a
    fourth id, so `Use Laser600 bench` is a real choice against the default.
  - After the Use branch's copy and reopen, `itemsUsedBy` resolves E1 to exactly T's id and parameters. Neither A nor
    B is among the bench's items or files, and T's file bytes are unchanged.
- **The Add branch asserts:**
  - the reopened descriptor's `itemIdOpt` is the source's id;
  - the resolved piece is the source's Laser600, relabelled `Laser600 (2)`, with its parameters;
  - the kit's ids are its own Laser600 and the added one, with the spare absent.
- **The smoke fact also checks the marks.** Before the press, the default `Use Laser600` is filled. After it, the
  pressed choice is filled and the default is not. Choosing writes no item file.
- **The list fact drives the real messages.** It opens the real source through `OpenCopySource`, then sends
  `ToggleCopiedExperiment` E1 and E2. It also renders the source with nothing chosen, to show no table, intro or
  choice.
- **`ScrollExtentTests.tallestKitBar` gives the window's kit three items.**
  - The brief names forty chosen experiments and twenty item rows.
  - With an empty `kitItems`, each row would offer only `Add`, which is not the bar at its tallest.
  - `kitItems` is read by no other view (grep), so only the table grows.
- **`KitWindowTests.fs` opens `OpticalConstructor.Domain.LibraryEntryFiles`.** `ItemModelVersionId.entryReference`
  is a type extension there, and the list fact derives the expected line from the shipped model's version, not from
  the item.

## Testing state

Development runs, over the final source after the mutants restored it. The gate engine gives the verdict
(Invariant 6).

- **build** (first attempt): exit 1, 4 errors in `KitWindowTests.fs`, because `entryReference` needed the
  `LibraryEntryFiles` open.
  - It also printed the two long-standing vendored-MathNet `SYSLIB0051` warnings, because MathNet recompiled.
  - The log was overwritten by the second attempt.
- **build** (second attempt, after the open): exit 0, **0 warnings, 0 errors**. Log: `.artifacts/0229-dev-build.log`.
- **KitWindowTests** (all facts, smoke included): 19 passed (16 + 3). Log: `.artifacts/0229-dev-kitwindow-tests.log`.
- **ScrollExtentTests:** 14 passed. Log: `.artifacts/0229-dev-scroll-extent-tests.log`.
- **Mutants** (`.artifacts/0229-mutants/run_mutants.py`, `summary.txt`):
  - Each was applied alone, then the UI test project was rebuilt.
  - Each ran against KitWindowTests and the kit-window scroll fact. Every one was caught:
    - m1, the table never shown: the list fact and the smoke fact;
    - m2, the table outside the bounded scroller: the list fact and `the kit window: the kit bar and the page stay
      reachable`;
    - m3, the choice never marked: the list fact and the smoke fact;
    - m4, every choice sends Add: the smoke fact (its Use branch);
    - m5, no Use choices: the list fact and the smoke fact;
    - m6, the intro shown with no rows: the list fact;
    - m7, a mapped id repeating the source GUID: the ids fact and the smoke fact;
    - m8, the line showing the binding reference instead of the model's: the list fact.
  - `restored: True`: both sources are byte for byte as before. `git diff --numstat` is unchanged.
- **build** (final, over the restored source): exit 0, **0 warnings, 0 errors**, and no lowercase `error` in the log.
  Log: `.artifacts/0229-dev-build-final.log`.
- **constructor-unit-tests** (all discovered, the gate's command): **1348** passed, 0 failed, 0 skipped, in 65 s
  (unchanged). Log: `.artifacts/0229-dev-constructor-unit-tests.log`.
- **ui-tests** (the gate's command over `KitWindowTests` and `ScrollExtentTests`): **25** passed, 0 failed, 0 skipped,
  in 22 s.
  - That is 23 + the two non-smoke census facts.
  - The TRX shows both new identities and the changed `the kit window: …` fact as `Passed`.
  - Log: `.artifacts/0229-dev-ui-tests.log`.
- **ui-smoke** (all discovered, the gate's command): **239** passed, 0 failed, 0 skipped.
  - That is 238 + `pressing a mapping choice and Copy adds or maps the item as chosen`, which is `Passed` in the TRX.
  - It took 196 s against the 600 s `attempt_timeout_seconds`.
  - Log: `.artifacts/0229-dev-ui-smoke.log`.
- **elevation**, with `ASG_ARC_BASE_COMMIT=f395597c…` (the `.arc-base-json` origin) and ELEV004 excluded:
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 195 s;
  - 0 known findings;
  - 1317 suppressed findings, against 1315 at step 228. `OpticalConstructor.Ui` went from 482 to 484; these are
    excluded-rule ELEV004 `.value` hits, measured and not gated;
  - log: `.artifacts/0229-dev-elevation.log`.
- **The fourteen `invariant-s22-*` gates**, through `ai_strategy_generator.step_compiler.section_invariants`: each
  holds (exit 0, found nothing). Log: `.artifacts/0229-dev-invariants.log`.
- **Line endings:** `git diff --numstat` equals `--ignore-cr-at-eol` for all five source files.
- **Baselines:** `constructor_unit_tests` 1348, `ui_smoke_tests` 239, `ui_tests` 25.

## Artifacts

- `.artifacts/0229-dev-build.log`, `.artifacts/0229-dev-build-final.log`
- `.artifacts/0229-dev-kitwindow-tests.log`, `.artifacts/0229-dev-scroll-extent-tests.log`
- `.artifacts/0229-mutants/run_mutants.py`, `.artifacts/0229-mutants/summary.txt`,
  `.artifacts/0229-mutants/m{1..8}-*-{build,tests}.log`
- `.artifacts/0229-dev-constructor-unit-tests.log`
- `.artifacts/0229-dev-ui-tests.log`
- `.artifacts/0229-dev-ui-smoke.log`
- `.artifacts/0229-dev-elevation.log`
- `.artifacts/0229-dev-invariants.log`

## Gotchas

- **A choice box switches helpers in place.** The same keyed `Border` is a `verbButton` while unchosen and an
  `actionButton` once chosen. Its message, `ChooseCopiedItem (source id, choice)`, is fixed by its id, so
  `actionButton`'s `OnChangeOf id` press closure is safe here, unlike for a model-dependent action (step 226's
  gotcha).
- **A `Use <label>` box does not wrap,** because the shared box helpers do not wrap their text. A label wider than the
  copy panel would be clipped sideways inside the vertical-only scroller. Lab labels are short (`LP1`, `QZ`), and the
  scroll fixture's labels fit at 800 × 600.
- **`kitItems` is the window's kit as last read or written.** The copy reads the kit's items afresh and keeps the
  chosen rows (step 228's gotcha), so a stale `Use` choice is refused by the copy with a typed error rather than
  drawn wrong.
- **`ItemModelVersionId.entryReference` is an extension in `LibraryEntryFiles`.** A file that uses it must open that
  module.
