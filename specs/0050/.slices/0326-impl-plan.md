# Step 0326 — impl plan

## Approach

The Data step's review panel (step 325) gains a closing `Setup:` block. It corrects the reviewed experiment's setup after
the fact, with a mandatory audit note, and lists the audit trail (spec 0050 S9.3; 001 §K, §P `CorrectSetup`). The model
side already exists from step 316: `SetCorrectionNote`, `CorrectSetup`, `dataDraft.correctionNote`,
`Experiment.corrections` and `SetupCorrection`. This step is view, ids and tests only.

- **`DataStepView.fs`.** After the point rows, the review panel shows:
  - the line `Setup: <experiment.setup.describe>`;
  - an `Audit note:` field (`dataCorrectionNoteField`). It shows `model.dataDraft.correctionNote` and commits
    `SetCorrectionNote` on Enter and on losing focus.
  - a `WrapPanel` with one `Re-point to <reference.describe>` verb (`dataCorrectSetupChoice name`) for each kit setup,
    in order, whose reference is not the experiment's own. Each verb sends `CorrectSetup (id, reference, note)` and is
    disabled while `kitWrites` is withheld.
  - when the experiment has corrections, a vertical `dataCorrectionTrail` with one wrapping line per correction, oldest
    first: `<previous> → <corrected>: <note>`.
- **Reuse instead of copy.**
  - The existing `Reason:` field and the new `Audit note:` field share one private labelled-field builder.
  - `typedReason` becomes a field-id-parameterised `typedText`, used by both Exclude and Re-point. A verb box takes no
    focus, so pressing it does not commit the field: the step-325 / S4.3 precedent.
  - The Apply-split availability wrapper becomes a private helper shared with the re-point verbs.
- **The re-point verb is `verbButtonOn`** (its handler re-patched on every render) inside the availability wrapper.
  `availableWorkbenchButton` re-patches only when the id changes, so a setup re-recorded at a new revision under the
  same name would otherwise keep sending the stale revision.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/DataStepView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`: `dataCorrectionNoteField`, `dataCorrectionTrail`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`: `dataCorrectSetupChoice`, beside
  `savedSetup`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/DataStepTests.fs`:
  - the ids fact pins the three new ids;
  - new: `the correction panel offers every other setup of the kit and lists the audit trail`;
  - new: `pressing a setup sends CorrectSetup with the typed audit note`.

## Risks

- Two text fields now live in the review panel, so the live-text lookup must select its field by id.
- A WrapPanel needs a bounded width to wrap. The panel sits in a ScrollViewer with horizontal scrolling, so the
  WrapPanel takes the panel's 360 max width, like the panel's texts.
- ELEV:
  - no single-parameter free function of one of our types (ELEV013);
  - no call-site re-wrap of a bare local (ELEV001);
  - a unique public name for the new extension member.
