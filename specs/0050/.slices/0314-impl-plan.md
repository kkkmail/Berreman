# Step 0314 — impl plan

An excluded point stays in the data but leaves what is validated, and the data badge says how many are excluded
(spec 0050 S9.3; 001 §K).

## Approach

1. `OpticalConstructor.Domain/ImportProfiles.fs`: after `selectedBy`, add `MappedPoints.keptBy (exclusions)`.
   - Number the points 1, 2, … in order (`MappedPointNumber`) and drop each one an exclusion names.
   - Keep the case. A series keeps its kind through `{ series with points = … }`. An exclusion that names no
     point changes nothing.
2. `OpticalConstructor.Domain/ExperimentDataLoad.fs`:
   - `loadAndValidate` becomes `readMapped`, then `keptBy data.exclusions`, then `validatedFor`.
   - `ExperimentDataBadge` gains `ExcludedPointsBadge` after `ValidatedDataBadge`, with text
     `excluded points (<k> excluded, <n> validated)`.
   - `ofCheck` answers it for `Ok points` when the attachment's exclusions are not empty. Every other branch, and
     their order, stays as it is.
3. `OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`: the caption is `Loaded — <n> point(s) kept, <k> excluded`.
4. The callers (`TableAndElementRotationView.fs`, `SolverHandoffView.fs`) are unchanged.

## Tests

- `ImportProfileTests`:
  - Lift step 300's test-local fixtures (the two-point series, the three Phone ROI rows, the three observations and
    the three mapped rows) out of `mapped points count what they hold` into private module values. This is a move,
    and no assertion changes.
  - Add `kept points leave out the excluded ones and keep the rest in order`.
- `ExperimentDataProxyTests`:
  - Extend the badge fact.
  - Add `loading data validates the kept points, so an excluded point outside the range does not refuse the data`.

## Risks

- **ELEV011 (repeated tuple element type).** The slice's literal case `ExcludedPointsBadge of kept :
  MeasuredPointCount * excluded : MeasuredPointCount` repeats a type. The pinned analyzer
  (`Softellect.Analyzers 10.1.402.136`) reports exactly that shape on a union case. The analyzers win (AGENTS.md),
  so the two counts go in a record `KeptPointsInfo = { kept; excluded }`, the analyzer's own suggested fix. Field
  names and types stay the slice's.
- The new facts must be found by `constructor-unit-tests`, which runs every discovered test.
