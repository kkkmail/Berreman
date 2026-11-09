# Step 0226 — impl plan

Spec 0050 S6, step 0226 (IMPLEMENT): the Kit menu's `Copy experiments from another kit…` verb and the copy panel
that sends step 225's messages (`OpenCopySource`, `ToggleCopiedExperiment`, `CopyChosenExperiments`,
`CloseCopySource`).

## Approach

1. `OpticalConstructor.Controls/UiIds.fs`, kit-bar group: five fixed ids with one-line docs —
   `kitCopyExperimentsButton`, `kitCopySourceText`, `kitCopyExperimentList`, `kitCopyChosenButton`,
   `kitCopyCancelButton`.
2. `OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`, beside `attachDataFile`:
   `kitCopyExperimentToggle (experimentId : ExperimentId)` = `KitCopyExperimentToggle_<n>`.
3. `OpticalConstructor.Ui/KitWindowView.fs`:
   - the verb after Show in Explorer, present for `kitWrites = ControlOffered` with no open source; its press runs
     `model.kitShell.kitPicker.pickThen (OpenCopySource >> dispatch)`;
   - `kitCopyRows`, placed after the send progress row: a wrapping source text, a bounded `ScrollViewer`
     (`MaxHeight` 160, vertical `Auto`) over one `CheckBox` per source experiment, and a horizontal row with Copy
     (positive, only with a non-empty choice) and Cancel (negative). The Copy box's right margin is the gap.
4. Tests in `KitWindowTests`:
   - the UiIds fact;
   - the panel layout fact over a source opened through the messages (E1, E2, with E2 chosen);
   - the read-only fact;
   - the ui-smoke press flow through a picker answering a source kit that holds E2.
   `pressAtCentre` is widened from `Border` to `Control` so it can also press a `CheckBox`.
5. `ScrollExtentTests.tallestKitBar` adds a running, progressed send and an open copy source of forty experiments
   (two chosen), over a separate in-memory scratch kit with a long name.

## Risks

- A bar taller than the 1000 × 540 client would push controls out of reach, because the bar sits outside the page's
  scroll. The list's `MaxHeight` bounds it. The scroll test measures it.
- The CheckBox's own toggle and the model's `isChecked` must agree. `onClick` fires only on user activation, as in
  the existing check boxes, so there is no feedback loop.
- `actionButton` subscribes with `OnChangeOf id`. The Copy and Cancel closures are constant, so a stale closure is
  harmless.
