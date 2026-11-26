# Step 0003 — IMPLEMENT — impl plan

Move `AbscissaKind` from the Domain into the core, so the transcendental laws of steps 5 and 7
can carry their abscissa as data (.spec-md §3.1). No number moves and no behaviour changes.

## Approach

1. `Berreman/Berreman/Dispersion.fs`, directly after `ComplexDispersionFormula`:
   - the record `ReciprocalAbscissa` (`[<Softellect.Sys.NumericCarrier("…")>]`), with
     `wavelengthScale : float<meter>` and `numerator : float`, declared first because the union
     names it;
   - the public union `AbscissaKind` with `LinearInX of wavelengthScale : float<meter>` and
     `ReciprocalInX of ReciprocalAbscissa`, carrying the doc comment that sat on the Domain's
     private type (its `Dispersion.fs:238` line reference becomes the name
     `DispersionFormula.wavelengthScale`, since the comment now lives in that file).
2. `OpticalConstructor.Domain/DispersionModels.fs`:
   - delete the private `AbscissaKind`;
   - in its place, `type AbscissaKind with static member ofUnit (u : UnitOfMeasure) : AbscissaKind`,
     the same body, except the reciprocal case is built as
     `ReciprocalInX { wavelengthScale = nanometer; numerator = (fromMeters u nanometer).value }`;
   - the five lowering sites (ConstantNK, Cauchy, Sellmeier, Lorentz, Drude) match
     `ReciprocalInX r` and read `r.wavelengthScale` / `r.numerator` where they read
     `(scale, numerator)` / `numerator.value`.
3. No test changes: the AC-B5 grid facts in `DispersionModelsTests.fs` are the proof and stay
   unchanged.

## Files

- `Berreman/Berreman/Dispersion.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs`

## Risks

- A fourth record with a `wavelengthScale` label (after `DispersionFormula` and
  `ComplexDispersionFormula`) could change F#'s record-label inference for an unannotated
  `x.wavelengthScale` or `{ terms = …; wavelengthScale = … }` anywhere downstream. The build shows
  it. If it bites, annotate the site rather than renaming the field the slice names.
- ELEV: the new names must be unique across every `Softellect.*` assembly (census: no match in
  STL); `numerator : float` is a bare primitive, which the carrier mark admits as the slice
  directs.
- `Dispersion.fs` carries a UTF-8 BOM and LF endings; keep both.
