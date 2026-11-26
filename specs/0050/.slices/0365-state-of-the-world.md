# Step 0365 — state of the world

## Where we are

Spec 0050, section S11.1 (studies).

- Steps 0358–0364 gave the study:
  - its shape and its file;
  - AddExperimentsToStudy and OpenMaterial's working copy;
  - its unknowns with their scopes and owners;
  - SetParameterScope and LinkParameters.
- This step lands the study's settings as pure updates (S11.1 brief "Per session a gain model …", "Fit settings
  (joint / staged, S11.3), weights per experiment or per detector, enable / disable per experiment, and notes"; 001
  §L "Weights and ablation"; 001 §E notes):
  - per-session gain models, anchored within their session;
  - the fit mode;
  - experiment and detector weights;
  - enablement;
  - notes.
- It also adds the two reads that S11.2 and S11.3 consume, `enabledExperiments` and `residualWeightOf`.

## What's working

- Added `Study.withSessionGainModel`, which sets a session's gain model only for a session the study's experiments
  form, with every AIR anchor or repeat inside it.
- Added `withFitMode`, `withExperimentWeight`, `withExperimentEnablement` (over the new two-case
  `ExperimentEnablement`), `withDetectorWeight` and `withNotes`. Each sets its value in place or appends it, and
  refuses an unknown experiment or item without changing anything.
- Added `Study.enabledExperiments` (the study's runs without the disabled ones) and `Study.residualWeightOf` (the
  experiment's weight times the detector's, one when unset).
- `StudyError` gains `UnknownStudySessionErr`, `GainAnchorOutsideSessionErr` and `UnknownStudyItemErr`.
- Replace-or-append is now one private `Study.replacedOrAppended`, shared with `withParameterScope`.

## Tests

These are development runs of each gate's own command from `0365.gates`, over the final tree:

- **build:** exit 0, `0 Error(s)`, no lowercase `error`. A full build took 75 s and the final incremental one 25 s
  (deadline 600 s). The only warnings are the exempt NU1902 and the vendored MathNet SYSLIB0051 pair.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 314s` over the final tree, with 1520 ELEV004
  findings suppressed and 0 known findings (deadline 600 s). The 314 s run shared the machine with the test run.
  - An earlier run, 244 s, was also OK.
  - It started before a local rename (`settings` to `gains` in `withSessionGainModel`), so it was run again.
  - ELEV004 went from 1518 to 1520. ELEV004 is measured, not gated: these are `.value` reads in the new refusal
    texts and in `residualWeightOf`.
- **constructor-unit-tests:** `Passed!`, 1604 of 1604 (1601 + 3), in 69 s and then 103 s (deadline 180 s). The
  103 s run shared the machine with elevation.
  - Passed: `a session's gain model is set for a session of the study, anchored within it`.
  - Passed: `fit mode, weights, enablement and notes are set in place, and an unset weight reads one`.
  - Passed: `a setting naming an experiment, item or session the study lacks is refused`.
  - Passed: the widened `every study error renders its condition and its payload`.
- **invariant-s36-\* (20):** each decoded `--find-nothing` grep finds nothing over the final tree.

No run went past its deadline.

```yaml
gates:
  constructor_unit_tests: 1604
```

## Architecture

- **The settings members sit on `Study` itself, after `gainModelOf`.** `withSessionGainModel` reads `sessionsIn`
  (step 0358), so a session is what the study's experiments form in study order.
  - The runs of the model are checked in order: the anchors, or the opening and then the closing repeat.
  - The first one outside the session is the refusal.
- **One replace-or-append rule.** `static member private Study.replacedOrAppended (keyOf) (entry) (entries)` puts
  an entry in place of the one with its key, else appends it. It serves the scopes (step 0363's inline body, moved
  unchanged) and the session-gain, experiment-weight and detector-weight lists. The key function avoids a `bool`
  predicate in the signature.
- **One study-experiment refusal.** `member private Study.unheldExperimentOpt` answers
  `UnknownStudyExperimentErr "'<name>' is not among the study's <n> experiments"`. Both the experiment weight and
  the enablement use it.
- **`ExperimentEnablement` is not stored,** so it has no `value` tag or `tryOfWire`. The study keeps `disabled`, and
  a run is listed there once.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **`UnknownStudyExperimentErr` keeps its render, `the kit has no such experiment: <reason>`.** Later steps quote it.
  For the settings refusals, the reason names the study's experiments.
- **`withSessionGainModel` accepts `AnchoredSessionGain []`.** The step names only the two refusals.
  `SessionGainModel.ruleWith` refuses an empty anchor list when the rule is derived.
- **A Diagnostic run the study holds is in no session,** so it cannot be a session's anchor.
- **`residualWeightOf` builds the product through the `ResidualWeight` case**, as `ruleWith` builds an `AirGain`.
  Only absurd weights (around 1e200 each) could overflow past the type's finite rule.
- **A disabled run keeps its weight entry.** Enablement and weights are independent.
- **`specs/0050/.manifest.state.json` was already modified** when the round started. It is the supervisor's, and this
  round did not touch it.

## Changelog

- 2026-11-13: step 0365 added a study's settings as pure updates:
  - `withSessionGainModel`, `withFitMode`, `withExperimentWeight`, `withExperimentEnablement`,
    `withDetectorWeight` and `withNotes`;
  - `enabledExperiments` and `residualWeightOf`;
  - `ExperimentEnablement`;
  - three `StudyError` cases;
  - the shared private `replacedOrAppended` and `unheldExperimentOpt`;
  - three `StudyTests` facts, and three new rows in `StudySessionTests`' error-render fact.
