# Step 0365 — impl plan

## Approach

Step 0365 adds a study's settings as pure updates on `Study` (spec 0050 S11.1; 001 §L "Weights and ablation"; 001
§E notes). All the code goes into `Studies.fs`, with the tests in `StudyTests` and `StudySessionTests`.

1. `ExperimentEnablement = EnabledInStudy | DisabledInStudy` goes after `StudyFitSettings`. It is a two-case
   union in place of a flag. It is not stored, so it has no `value` tag: the study keeps its `disabled` list.
2. `StudyError` gains three cases, each rendered as the step words it:
   - `UnknownStudySessionErr`: `the study has no such session: <reason>`;
   - `GainAnchorOutsideSessionErr`: `the AIR run is not in that session: <reason>`;
   - `UnknownStudyItemErr`: `the study uses no such item: <reason>`.
3. New members on `Study`, after `gainModelOf`. Each refusal changes nothing.
   - `withSessionGainModel session model kitExperiments` reads `sessionsIn kitExperiments` (step 358):
     - a session it lacks is `UnknownStudySessionErr`;
     - the first anchor (in list order), or the opening then the closing repeat, outside that session is
       `GainAnchorOutsideSessionErr`;
     - otherwise the `SessionGainSetting` is replaced in place or appended.
   - `withFitMode` sets the mode in place.
   - `withExperimentWeight` and `withExperimentEnablement` answer `UnknownStudyExperimentErr` for a name the
     study does not hold:
     - a weight is replaced in place or appended;
     - `DisabledInStudy` adds the name to `disabled` once;
     - `EnabledInStudy` removes it.
   - `withDetectorWeight detector weight used` answers `UnknownStudyItemErr` for an item outside `used`;
     otherwise the weight is replaced in place or appended.
   - `withNotes` sets the notes.
   - `enabledExperiments` is the study's experiments without the disabled ones, in order.
   - `residualWeightOf experiment detector` is the experiment's weight times the detector's, each
     `ResidualWeight.unit` when unset.
4. Tests:
   - three new `StudyTests` facts, over a kit of E1, E2, D1 (Diagnostic) and E3 and step 362's items (CPL1, CPL2,
     Phone);
   - the three new rows in `StudySessionTests`' error-render fact.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Studies.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudySessionTests.fs`

## Risks

- **ELEV:** every new member sits on `Study` and takes our types, so ELEV003/013 do not apply.
  - `ExperimentEnablement` carries no `value`, so ELEV017 does not apply.
  - Its case names are new in the tree (ELEV016). A grep found none.
  - `withNotes` also exists on `Experiment` and `Item`. Those are other receivers, and all three are intrinsic
    members, so ELEV019 is not involved.
- **The `UnknownStudyExperimentErr` render stays `the kit has no such experiment: <reason>`.** Later steps (374+)
  quote that text. The reason names the study for the settings refusals.
- **`ResidualWeight` multiplication** goes through `.value` inside its own type's member (`ResidualWeight.times`
  would be new surface). I keep it inside `residualWeightOf`, which is `Study`'s member.
