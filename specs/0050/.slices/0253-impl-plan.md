# Step 0253 — impl plan

## Approach

One pure Domain rule, added to `OpticalConstructor.Domain/BeamLayout.fs` right after `detectedBranches`:

- `DetectorCapture = { detector : Library.ElementId; branch : BeamTree.BeamBranch }` — one detector an
  experiment captures and the branch it captures.
- `detectorCaptures scene detectors varied objects : DetectorCapture list` — walks `scene.elements` (scene
  order) and keeps every LIT detector of `detectors` whose path holds `varied` (the filter `detectedBranches`
  applies today). Such a detector captures the branch of the LAST step of its path whose element is one of
  `objects`; with no object on its path it captures `TransmittedBeam` when its path leaves `varied` by the
  transmitted branch, and nothing otherwise (the no-sample pass-through rule of
  `TableAndElementRotationView.fs:availableCaptures`).

`detectedBranches` and every UI caller stay unchanged (step 254 re-points them). No other type or function.

## Tests (`BeamLayoutTests`, two new facts over `el`, `tilted`, `transmitting`, `onReflectedOf`, `layout`)

1. `each detector captures the branch its light leaves the last object by` — src, `s1` (R2 30°, both
   branches), `s2` behind it on the main beam (R2 30°, both), `dT` behind `s2`, `dR2` on `s2`'s reflected ray,
   `dR1` on `s1`'s reflected ray. Varied `src`, objects [`s1`; `s2`] → `dT` T, `dR2` R, `dR1` R in scene order;
   objects [`s1`] → `dR2` T.
2. `a detector whose path holds no object captures only light the varied element transmits` — src, a flat
   mirror `m` (R2 45°), detector `d` behind it, an unlit detector behind `d`. No objects: varied `src` → `d` T;
   varied `m` → nothing; the unlit detector never appears.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/BeamLayout.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/BeamLayoutTests.fs`

## Risks

- A new record with fields `detector` / `branch` can capture `x.branch` / `x.detector` lookups on values whose
  type is not yet inferred in files that `open BeamLayout`; the build shows any such site.
- ELEV: the record and function carry only elevated types; names `DetectorCapture` / `detectorCaptures` are
  unused anywhere in `Berreman/`.
