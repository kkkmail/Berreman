# Step 0304 — impl plan

## Approach

One rule validates mapped points against an experiment's detector and scan (spec 0050 S9.2; 001 §K status badges).

1. **Compile order.** In `OpticalConstructor.Domain.fsproj`, move the `ExperimentDataProxy.fs` and
   `ExperimentDataLoad.fs` `<Compile>` lines, in that order and with no comment, to directly after
   `ImportProfiles.fs`. Their old comment blocks go with them. Nothing compiled between the two places
   (`MuellerReconstruction.fs`, `MuellerInverse.fs`, `ItemEvaluation.fs`) names either module: only two comments
   mention the proxy.
2. **`ImportProfiles.fs`, after `MappedPoints`:**
   - `ScanQuantity` (element, label, variable, ranges) and its `admits`;
   - `ExperimentScan.quantities`, the distinct `(elementId, variable)` pairs of `axes` in order of first appearance;
   - `MappedPoints.validatedFor detector scan`.

   `validatedFor` is the one validation:
   - `SeriesPoints` is step 288's rule, moved out of `loadAndValidate` with its texts.
   - `CapturePoints` checks the detector and the scan's quantity count, then each `description` against the one
     quantity.
   - `ObservationPoints` pass as read.
   - `ColumnPoints` checks rows: signal vs ellipsometer, cell count vs quantities, then per cell the unit kind, raw vs
     physical, and the range.
3. **`ExperimentDataLoad.fs`.** `loadAndValidate` keeps step 291's reader choice and refusals, wraps the read series
   in `SeriesPoints` and answers its `validatedFor`; its own scan match is deleted. `ExperimentDataBadge.ofCheck` takes
   `Result<MappedPoints, ExperimentDataError> option`, and `ValidatedDataBadge` holds the points' `pointCount`.
4. **UI.**
   - `SolverHandoffView.validateExperiment` answers `HandoffReady points.pointCount`.
   - `TableAndElementRotationView.checkExperimentData` already passes the outcome as it comes. Only its doc changes,
     to name the mapped points.
5. **Tests.**
   - Two new facts in `ImportProfileTests`.
   - The three `ExperimentDataProxyTests` facts change their expected type to `MappedPoints`, with every assertion
     kept.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ImportProfiles.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ExperimentDataLoad.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/SolverHandoffView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ImportProfileTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentDataProxyTests.fs`

## Risks

- **Name clashes from the new opens in `ImportProfiles.fs`** (`Library`, `Experiments`). `Intensity` is a
  `DetectorKind` case. The build will show any clash.
- **ELEV rules on the new members.** Locals stay lambdas inside the member, so nothing new is a module-level function
  of one domain type (ELEV013).
- **Step 287's plate scan is private to `ExperimentProxyTests`**, which this step may not edit. The new fact builds
  the same three axes test-locally over the `sample` id.
