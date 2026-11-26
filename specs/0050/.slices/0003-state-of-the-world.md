# Step 0003 — IMPLEMENT — state of the world

## Where we are

Step 3 of spec 0050, section S1.1 ("Every dispersion axis is data: tabulated and transcendental
cases"). Step 2 made a tabulated table an axis case of the core. This step moves `AbscissaKind`,
the classification of a model's abscissa as linear or reciprocal in the reduced wavelength, from a
private Domain type into the core. The transcendental laws of steps 5 and 7 can then carry their
abscissa as data inside the axis tree (.spec-md §3.1). Nothing else moves: no number changes, and
the Domain's lowering behaves exactly as before.

## What's working

- Moved `AbscissaKind` into the core `Dispersion.fs` as a public union (`LinearInX` / `ReciprocalInX`)
- Added the carrier record `ReciprocalAbscissa` (`wavelengthScale`, `numerator`) as the reciprocal case's payload
- Deleted the Domain's private `AbscissaKind`; `ofUnit` is now a static extension member of the core type
- The five reciprocal lowering sites read `wavelengthScale` / `numerator` from the record; the AC-B5 grid facts pass unchanged

## Tests

Observed locally as development checks. The arc-runner's gate engine runs and records the gates.

- `build`: exit 0, 0 errors. The only warnings are the two vendored MathNet SYSLIB0051 warnings
  that were already on the start tree.
- `constructor-unit-tests` (whole suite): 732 / 732, including the 29 `DispersionModelsTests`
  facts (AC-B5 among them), none of them edited.
- `unit-tests` (`EpsWithDispValueTests` and `DispersionFormulaTests`): 21 / 21.
- `ui-smoke`: 208 / 208.
- `ui-tests` (`MaterialEditorWindowTests` and `NkDispersionChartTests`): 41 / 41.
- `elevation`: `ELEV-GATE: OK`, 0 new findings in 11 projects (the two changed projects plus
  their dependents).
- The three s01 invariants each find nothing.

```yaml
gates:
  constructor_unit_tests: 732
  berreman_unit_tests: 21
  ui_smoke_tests: 208
  ui_tests: 41
```

## Architecture

- `Berreman.Dispersion.ReciprocalAbscissa` is a `[<NumericCarrier>]` record:
  - `wavelengthScale : float<meter>` gives the metres per unit of x;
  - `numerator : float` is the k in abscissa = k / x.
  It is declared after `ComplexDispersionFormula` and before the union that names it.
- `Berreman.Dispersion.AbscissaKind` has two cases:
  - `LinearInX of wavelengthScale : float<meter>`;
  - `ReciprocalInX of ReciprocalAbscissa`.
  The case data was a tuple `float<meter> * UnitMagnitude`. It is now a record, because the core
  cannot see `Units.UnitMagnitude`.
- `AbscissaKind.ofUnit (u : UnitOfMeasure)` stays in the Domain as a type extension, because it
  reads the `Units` seam (`toMeters` / `fromMeters`) and the core cannot reference Domain. Its
  `.value` read is the single place where the seam's `UnitMagnitude` becomes the carrier's number.

## Deferred

- Nothing in this step's scope. The transcendental data cases that will carry an `AbscissaKind`
  (steps 5 and 7), the removal of `EpsAxisEvaluated` and the custom equality (step 9) belong to
  later steps of the arc, as the spec orders them.

## Gotchas

- The slice names the match variable `r` (`r.wavelengthScale`, `r.numerator`). The code uses
  `reciprocal`, because the Lorentz branch's `oscillator` already has a parameter `r` (the
  resonance). Inside that lambda, `r` would have shadowed the record. The reads are the ones the
  slice names.
- `ReciprocalAbscissa` is the third record with a `wavelengthScale` label, after
  `DispersionFormula` and `ComplexDispersionFormula`. F# resolves an unannotated
  `x.wavelengthScale` to the most recently declared record with that label. Every current site is
  typed by its context, and the build is clean. A new unannotated site after this declaration
  needs a type annotation.
- The moved doc comment pointed at `Dispersion.fs:238`. Now that it lives in that file, it names
  `DispersionFormula.wavelengthScale` instead of a line number.

## Changelog

- 2026-11-05 — step 0003: `AbscissaKind` and its `ReciprocalAbscissa` payload moved into the core.
  The Domain's private type was deleted, and `ofUnit` became an extension member. No number moved.
