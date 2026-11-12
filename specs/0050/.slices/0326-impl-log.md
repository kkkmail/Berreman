# Step 0326 — impl log

## Progress

- [x] Read the protocol, the project prompt, the slice and the surrounding code (steps 316, 323–325).
- [x] UiIds: `dataCorrectionNoteField`, `dataCorrectionTrail`.
- [x] ConstructorUiIdOfDomain: `dataCorrectSetupChoice`, beside `savedSetup`.
- [x] DataStepView: the review panel's closing `Setup:` block.
- [x] DataStepTests: the ids fact extended and two new facts. The trail fact also covers a twice-corrected experiment.
- [x] Development build: exit 0, 0 errors (`.artifacts/0326-dev-build.log`).
- [x] DataStepTests: 8 of 8 (`.artifacts/0326-dev-ui-tests.log`).
- [x] 16 s31 invariants hold (`.artifacts/0326-dev-invariants.log`).
- [x] Mutation check (`.artifacts/0326-dev-mutation.log`): 13 of 13 caught, each source restored byte-identical.
- [x] Final development runs over the final tree, after the mutations: all green.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`: two ids at the end of the Data step group.
  - `dataCorrectionNoteField` (`DataCorrectionNoteField`).
  - `dataCorrectionTrail` (`DataCorrectionTrail`).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`: on `ConstructorUiId`, right after
  `savedSetup`, `dataCorrectSetupChoice (name : Kit.SetupName)` (`DataCorrectSetup_<name>`).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/DataStepView.fs`:
  - new `setupCorrectionBlock`, appended to the review panel after the point rows. In order it shows:
    - the line `Setup: <experiment.setup.describe>`;
    - the `Audit note:` field;
    - a `WrapPanel` of `Re-point to <reference.describe>` verbs, one per kit setup in order whose reference is not the
      experiment's own, all inside an availability box keyed to `kitWrites`;
    - when the experiment has corrections, the `dataCorrectionTrail` stack: one wrapping line per correction, oldest
      first, reading `<previous> → <corrected>: <note>`.
  - Each verb sends `SetCorrectionNote note`, then `CorrectSetup (experiment.id, setup.reference, note)`. The note is the
    field's live text.
  - `typedReason` became `typedText`, which looks up a field by its id. Exclude and Re-point both use it.
  - The `Reason:` field and the new `Audit note:` field are both built by the private `reviewField`. Its text-box
    handlers re-patch on every render (`SubPatchOptions.Always`), as `ItemsPanelView` / `SetupGridView` do.
  - Apply split's inline availability wrapper became the private `availabilityBox`, shared with the re-point verbs.
  - The `dataStepView` and `dataReviewPanel` docs now name the setup correction.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/DataStepTests.fs`:
  - the ids fact pins `DataCorrectionNoteField`, `DataCorrectionTrail` and `DataCorrectSetup_Bench B`;
  - new fixtures `setup1`, `lp1Correction` and `withCorrectionModel`;
  - two new facts;
  - the module doc names the correction.

## Decisions

- **Re-point commits the note field's live text** (the S4.3 precedent, as step 325's Exclude).
  - A verb box takes no focus, so pressing Re-point does not blur the note field. A note typed without Enter would
    otherwise still be the draft's old text, and `CorrectSetup` would be refused for want of a note.
  - Re-point therefore reads the field's live text (falling back to `model.dataDraft.correctionNote`). It sends
    `SetCorrectionNote` with it, then `CorrectSetup`.
  - Both messages already exist, so no message is added. When the field was committed, the note sent is exactly
    `model.dataDraft.correctionNote`, as the how-to states.
- **The re-point verb is `MaterialsWindowView.verbButtonOn` inside an availability box**, not
  `availableWorkbenchButton`.
  - `availableWorkbenchButton` re-patches its press only when its id changes, and the id carries only the setup's name.
    A setup re-recorded at a new revision would otherwise keep re-pointing to the stale revision, which `CorrectSetup`
    then refuses as the same setup.
  - `verbButtonOn` re-patches on every render. Its box is disabled and dimmed while the kit takes no writes; a disabled
    parent takes no pointer press, and `CorrectSetup` guards the kit's writes itself.
- **One availability box for the whole WrapPanel**, rather than one per verb. Every verb shares the one availability.
- **The note field stays enabled over a read-only kit.** `SetCorrectionNote` writes nothing, as with the `Reason:` field.
- **The WrapPanel takes the panel's 360 max width.** The Data step sits in a horizontally scrolling `ScrollViewer`, so an
  unbounded WrapPanel would never wrap (AGENTS.md).
- **The block is a list of the panel's children, with no wrapping container.** No other control is added, as the how-to
  requires.
- **The trail fact also covers two corrections** (an extension within a created fact). With one correction a reversed
  trail could not be seen, so a third window shows E1 corrected again to Bench B: two lines oldest first, with Setup 1
  now offered.

## Testing state

Development runs of each gate's own command over the final tree (`.artifacts/0326-dev-gates-summary.log`):

- build: exit 0, 0 warnings, 0 errors, 35 s.
- elevation: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 206 s.
- constructor-unit-tests: 1552 of 1552, 74 s.
- ui-smoke: 256 of 256, 222 s.
- ui-tests (`DataStepTests`): 8 of 8, 16 s. That is the 6 from before plus the 2 created facts; the changed ids fact
  passes.
- invariant-s31-\*: all 16 hold.

No run exceeded its effective deadline.

Mutations (13 of 13 caught, each restored byte-identical):

| Mutation | Caught by |
|---|---|
| M1 draft note sent | `pressing a setup…` |
| M2 own setup offered | `the correction panel…` |
| M3 note read from the reason field | `pressing a setup…` |
| M4 trail newest first | `the correction panel…` (twice-corrected window) |
| M5 choices offered over a read-only kit | `pressing a setup…` |
| M6 note field commits as the reason | `pressing a setup…` |
| M7 setup line without its revision | `the correction panel…` |
| M8 empty trail shown | `the correction panel…` |
| M9 own setup sent | `pressing a setup…` |
| M10 choices reversed | `the correction panel…` |
| M11 note dropped from the trail line | `the correction panel…` |
| M12 reason field commits as the note | step 325's `pressing Exclude…` |
| M13 Apply split box ignores availability | step 324's split-panel fact |

## Artifacts

- `.artifacts/0326-dev-gates-summary.log`
- `.artifacts/0326-dev-build.log`, `0326-dev-elevation.log`, `0326-dev-constructor-unit-tests.log`,
  `0326-dev-ui-smoke.log`, `0326-dev-ui-tests.log`, `0326-dev-invariants.log`, `0326-dev-mutation.log`.
- `.artifacts/0326-dev-trx/`: `ui-tests.trx`, `ui-smoke.trx`, `constructor-unit-tests.trx`.
- `.artifacts/0326-mutation-tools/mutate.py`: the mutation driver, adapted from step 324's.

## Gotchas

- `.manifest.state.json` was already modified when the round started. It is the supervisor's file and was not touched.
- The vendored MathNet's SYSLIB0051 warnings appear only when MathNet recompiles. They are pre-existing, as in step
  325's Ui build, and the final build reported 0 warnings.
