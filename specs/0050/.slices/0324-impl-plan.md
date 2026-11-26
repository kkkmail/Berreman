# Step 0324 — impl plan

## Approach

The Data step (step 323) gains the held split's panel under its Split verb, over the split model of steps 319–321
(`DataSplitState`, `ToggleSplitOffer`, `CancelDataSplit`, `MatchByTemplate`). Nothing in the update changes: the panel
only sends the three existing messages.

1. `OpticalConstructor.Controls/UiIds.fs`: three fixed ids in the Data step group — `dataSplitKeyColumn`,
   `dataSplitApplyButton`, `dataSplitCancelButton`.
2. `OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`: `Kit.DataRowKey.uiKey`, `ConstructorUiId.dataSplitGroupRow` /
   `dataSplitOfferToggle` over a `DataRowKey`, and `DataSplits.DataSplitError.caption` (its `describe`), appended as a
   spec 0050 S9.3 group after the S9.2 wizard extensions.
3. `OpticalConstructor.Ui/DataStepView.fs`: a private `splitGroupRow` (a `DockPanel`, the `<key> — <n> point(s) — `
   label docked left, then `→ <name>`, the offer toggle, or the wrapping `unmatched — <reason>` in the message colour)
   and a private `dataSplitPanel` (a bordered vertical panel: the wrapping key-column line, one row per group, then the
   Apply split / Cancel row). `dataStepView` appends the panel under the Split verb while `dataSplitOpt` is `Some`.
   - The offer toggle is `MaterialsWindowView.actionButton` on the positive fill while ticked, `verbButton` otherwise
     (the `itemsPanelChoice` / import wizard `choiceBox` precedent).
   - Apply split is `actionButton` on the positive fill inside a `Border` that is disabled and dimmed while the kit's
     writes are withheld (`actionButton` has no availability parameter, and `MaterialsWindowView.fs` is outside the
     effects); Cancel is `actionButton` on the negative fill. The action box's right margin is the gap.
4. Tests: new `OpticalConstructor.Ui.Tests/DataStepTests.fs`, `module DataStepTests`, appended at the end of the compile
   list (after `KitWindowTests.fs` and the files that follow it), so it reuses `SlotBarTests`' internal window helpers
   (`controlById`, `tryControlById`, `boxLabelOf`, `topLeftOf`, `pressBox`, `latestOf`) instead of copying them.
   - Fixture: a fresh folder kit under the class's `TestAppScope` owner, holding `Dark` and `E1` written through the
     kit proxy, closed and reopened read-write, loaded into `initInverseWithKit` as `KitWindowTests.kitWindowModel`
     builds it; a GUID folder under the test output holds `phone.csv`.
   - Test-local split over `phone.csv` by `experiment` through `Phone ROI CSV`: `K1` (2) → E1, `K2` (1) offered `Bench B`
     / `K2`, `K3` (1) unmatched with `SplitKeyNameErr`.
   - Four facts as the slice names them; the smoke fact splits a real file through `SplitByColumn` and presses Apply.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/DataStepView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/DataStepTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`

## Risks

- The kit refuses to record an experiment whose setup it lacks (`recordExperiment` → `itemsUsedBy`), so the smoke
  fixture writes E1's `Dark` setup too, or Apply would stop at E1.
- A kit's experiments are read when it opens (step 323's gotcha): E1 is written, the kit closed and reopened.
- A wrapping text in an unbounded-width scroll viewer wraps only at its `maxWidth`; the row is a `DockPanel` (AGENTS.md).
- ELEV: every new private signature takes elevated types only; no boxing into `obj`; DU rendering through members.
