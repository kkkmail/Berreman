# Step 0245 — IMPLEMENT — impl plan

Obligation `S81-PARAMETER-VALUES`: the home of the one item-model evaluator, and the values it reads.

## Approach

1. New `OpticalConstructor.Domain/ItemEvaluation.fs`, `module ItemEvaluation` in `namespace OpticalConstructor.Domain`,
   opening `ItemModels` and `Kit`, compiled directly after `MuellerInverse.fs` (one `<Compile>` line, no comment).
2. `ItemEvaluationError`: `MissingParameterValueErr of reason : string` and `ParameterOutOfRangeErr of reason : string`,
   with `describe` rendering `the values hold no number for the parameter: <reason>` and
   `the parameter value is outside its range: <reason>`.
3. `ParameterValues of Map<ParameterName, ParameterValue>` with exactly the brief's members:
   `ofParameters` (known → value, unknown → start, calibrated → value), `defaultsOf` (each axis's default),
   `withValue` (replace or add), `valueOf` (value or `MissingParameterValueErr` naming the parameter) and `checkedFor`
   (first failure in axis order; missing → `MissingParameterValueErr`; NaN / infinite, a fraction outside [0, 1], a
   scale or thickness below 0 → `ParameterOutOfRangeErr` naming the parameter, value and range; angle, phase,
   Mueller element and reading take any finite number; values the axes do not name are kept and ignored).
4. Tests: new `OpticalConstructor.Tests/ItemEvaluationTests.fs`, `module ItemEvaluationTests`, appended last in the
   test project's compile list, with exactly the four named facts.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemEvaluation.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemEvaluationTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- ELEV: every public signature speaks our types only (`describe : string` is the sanctioned render member); the number
  rendering for a reason stays a local closure inside `checkedFor`, as `Parameter.validate` does in `Kit.fs`.
- Name uniqueness (ELEV014/015/016): `ParameterValues`, `ItemEvaluationError`, the two case names and the module name
  are unused across Berreman and STL (grepped).
- The brief says no other member is added, so `ParameterValues` gets no `.value` accessor; its map is read by pattern
  inside its own members.
