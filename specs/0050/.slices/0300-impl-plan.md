# Step 0300 — implementation plan

## Approach

Add `OpticalConstructor.Domain/ImportProfiles.fs` (`module ImportProfiles`, namespace `OpticalConstructor.Domain`),
compiled directly after `ItemEvaluation.fs`, the last Domain file. It holds, in the slice's order:

1. `type Observable with` (`value`, `tryOfWire`, `tryOfCell`) and `type SampleCut with` (`value`, `tilt`,
   `tryOfWire`): int disk tags for the two closed sets of a configuration column. `MuellerInverse.fs` is not edited.
2. `BuiltInImportProfile` (four cases, `name`, `all`, `tryOfName`), named and never stored, so no int tag.
3. `ChosenImportProfile` (`BuiltInProfileChoice` | `KitProfileChoice`, `name`, `tryOfName`): a built-in first, then
   the first kit profile of that name.
4. The points: `MeasuredSigma`, `RowTimestamp`, `ScanCell` (+ `displayValue`), `MappedReading`, `MappedRow`,
   `KeyedObservation`, `MappedPoints` (+ `pointCount`).
5. The wizard's column choices: `MuellerElementIndex`, `ColumnRoleChoice` (+ `text`), `ColumnChoice` and
   `type ColumnRoles with static member tryOfChoices`, the one rule from choices to roles.

Tests: new `OpticalConstructor.Tests/ImportProfileTests.fs` (`module ImportProfileTests`), appended at the end of the
test compile list, with the five facts the slice names. The `Lab scan` profile is `KitTests.labScanProfile`, reused
(AGENTS.md: reuse, never copy; step 299's gotcha keeps that fixture in one place).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ImportProfiles.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` (one `<Compile>` line)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ImportProfileTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (one `<Compile>` line)

## Risks

- **ELEV010** holds members to "no primitive beside an elevated type" unless the member is an elevation-pattern name
  (`tryOfWire` is; `tryOfCell` is not). `Observable.tryOfCell (cell : string) : Observable option` may be reported.
  Measure it with the elevation gate's own command and resolve inside the slice's scope if it is.
- **ELEV001**: `SweptValue x.value` across types is reported, and `UnitMagnitude.toSweptValue` would be the reverse of
  the existing `SweptValue.toUnitMagnitude` (ELEV006). `displayValue` binds the magnitude by pattern and builds the
  `SweptValue` at its origin.
- **Name resolution**: FSharp.Core has an `Observable` module; the extension and the tests may need the type
  qualified.
- Unique names (ELEV014/016/019): every new type and case name was checked against Berreman and STL.
