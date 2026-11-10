# Step 0254 — impl plan

The host stops guessing "the first bound sample" and asks the step-253 Domain rule
(`BeamLayout.detectorCaptures`) which branch each detector captures, so any number of samples in
series each decide the capture of the paths they end (spec 0050 S8.1).

## Changes

1. `OpticalConstructor.Domain/BeamLayout.fs`: `detectedBranches` takes `objects : Library.ElementId list`
   in place of `measured` and answers the distinct branches of `detectorCaptures`, in their order. It
   moves below `detectorCaptures` (F# declaration order); its doc cites S8.1.
2. `OpticalConstructor.Ui/TableAndElementRotationView.fs`:
   - delete `measuredElementOf`;
   - add `member private model.objectIds` (the elements bound to a library sample, in scene order), and
     beside it `member private model.detectorIds` (the scene's Detector elements) so the two callers
     share one detector list instead of copying it;
   - `captureDetectors varied` maps each `detectorCaptures` entry to its branch and the detector's
     `SceneElementIndex`;
   - `availableCaptures` is `capturesFor (detectedBranches … model.objectIds)`; its no-sample T filter goes
     (the rule's, step 253);
   - both docs: a detector captures the branch by which its light leaves the last object on its path.
3. `AGENTS.md`: one clause in the "An experiment runs on the light's real path" gotcha.
4. Tests: the two `BeamLayoutTests` facts pass `[ Library.ElementId "s" ]`; a new `ElementMenuTests` fact
   proves the two-sample R offer and the R series values against `sampleMuellerSpun`.

## Risks

- `member private` on an intrinsic augmentation may be inaccessible from the module-level
  `availableCaptures`; build will tell. If so, `internal` is the nearest accessibility (recorded).
- A detector whose path holds no bound sample now captures T when the varied element transmits to it
  (the step-253 rule) instead of nothing; every existing single-sample capture holds that sample on every
  captured path, so the selected UI classes must stay green — run them whole.
