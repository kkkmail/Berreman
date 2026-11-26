# Step 0371 — impl plan

## Approach

Add `OpticalConstructor.Domain/StudyForward.fs` (`module StudyForward`), compiled after `PrefitChecks.fs`, holding in
order: `StudyForwardError` (first case `UnpairedPointsErr`, `describe`), `RecordedReading` (`RecordedSignal` /
`RecordedPsiDelta` / `RecordedMueller`), `MeasuredRow` (`SettingsRow` / `ObservationRow`) and the extension
`MappedPoints.measuredRowsFor (scan : ExperimentScan)`.

`measuredRowsFor` reads `scan.quantities` once and maps each point, in order, to a `Result<MeasuredRow, _>`, then folds
them with the existing ordered collector `ItemEvaluation.sequenced` (the first error wins, as the 0252 ruling has it):

- intensity series point: the one quantity at its abscissa (none under a dark line), no wavelength, `RecordedSignal`;
- ellipsometric point: the scan's first `VaryR2` quantity at its angle of incidence when both exist, else none; `Some`
  of its wavelength; `RecordedPsiDelta` of its Ψ and Δ; never refused;
- capture row: the one quantity at its description in degrees (none under a dark line), `RecordedSignal` of `avgTotal`;
- column row: cell i the i-th quantity at `displayValue`; `RecordedSignal` of a signal (σ dropped) or `RecordedMueller`;
- observation: `ObservationRow` of it.

A series point or capture row under two or more quantities, or a column row whose cell count differs from the quantity
count, is `UnpairedPointsErr` naming the 1-based point number and both counts.

The abscissa → scan value conversion is a named member on the source type (`MeasuredAbscissa.toSweptValue`, ELEV001),
declared in `StudyForward.fs` because that is the only Domain file this step may edit.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/StudyForward.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` (one `Compile` line)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyForwardTests.fs` (new, three facts)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (one `Compile` line, last)

## Risks

- ELEV001 on `SweptValue x.value`: handled by the member above.
- Float round trip of degrees through `Angle` (20°, 60°): the test checks exactness and otherwise builds the expected
  value through the same conversion.
- Record-label inference for `PsiDelta { psi; delta }`: annotated.
