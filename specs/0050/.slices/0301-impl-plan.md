# Step 0301 — implementation plan

## Approach

Add the Mueller-matrix observation file (spec 0050 S9.2; 001 §K, §Q AT-2) as one new Storage module,
`OpticalConstructor.Storage/LabFileImport.fs` (`module LabFileImport`), compiled directly after `MuellerDataStore.fs`.

- `KeyedObservation.observationFileText` (static extension member) writes the header and one row per observation:
  key, cut tag and tilt (rad), thickness (m, `Thickness.metersOpt`), incidence and azimuth (rad), observable tag,
  wavelength (nm, the `Nm` case's own number), the sixteen elements of `measured.normalized` in row order, and
  `measured.element 0 0`. Doubles are `ToString("R", InvariantCulture)`; the int tags are invariant integers. The first
  observation it cannot write (a bad key, `Thickness.Infinity`, a `Mkm` wavelength, an un-normalizable matrix) is a
  `MalformedDataFileErr` naming its 1-based position and key, and no text is returned.
- `KitFileText.parseMuellerObservations` (instance extension member) reads it: split on `\n`, one trailing `\r` trimmed,
  blank lines skipped; a wrong first line is `MalformedDataFileErr` naming it; nothing after it is `EmptyDataFileErr`;
  each row must hold 25 cells, its numbers read invariant-culture, cut / observable through the step-300 `tryOfWire`s;
  the configuration is built from radians (not `IncidenceAngle.create`) and the matrix as throughput × normalized, m₀₀
  the throughput (the `misreadAbsolute` arithmetic).
- The header's column names live once, in a private `string list` (the element names come from
  `MuellerElementIndex.text`, step 300, so `m00 … m33` is not re-transcribed). Everything else is local to the two
  members, so no private module-level function carries a primitive (ELEV003P / ELEV010P).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/LabFileImport.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj` (one `<Compile>` line)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LabFileImportTests.fs` (new, the three facts)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (one `<Compile>` line, last)

## Risks

- Exact round trip of `t · nₖ` through `/ t` and `* t`: checked numerically up front for t = 0.8125 and 0.03 (all 15
  elements exact).
- ELEV rules on the new module (private surface enforced): keep primitives in member bodies only; run the elevation
  analyzers over the change.
- Record-label resolution (`key`, `measured`, `configuration`): annotate literals.
