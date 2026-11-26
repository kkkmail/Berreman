# Step 0214 — impl plan

## Approach

The step appends a parameter and its state to the end of the `Kit` module, after `ItemModelInstance` (step 213),
in the brief's order:

1. `ParameterInterval`;
2. `ParameterSigma`;
3. `ParameterPrior`;
4. `UnknownParameterData`;
5. `FitRunName`;
6. `CalibratedParameterData`;
7. `ParameterState`, with the cases `KnownParameter`, `UnknownParameter` and `CalibratedParameter`;
8. `Parameter`, with `knownAtDefault` and `validate`.

All eight are pure Domain data. The name reuses `ItemModels.ParameterName` and every number reuses
`ParameterValue`. Same-typed pairs sit in records, never in case tuples (ELEV011).

`Parameter.validate` collects the state's broken rules in a fixed order and answers the first one as
`InvalidParameterStateErr "<name>: <rule>"`:

- finiteness comes first, so a NaN never reports as a misplaced bound;
- then the interval's order and the start's position, ends included;
- then a positive sigma.

Numbers render invariantly (`"R"`, `CultureInfo.InvariantCulture`). No range is checked by kind: that is S8.1's,
by the model.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`: append the eight types.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitItemTests.fs`: add the two declared facts. The file is
  already in the test project, so the fsproj is not touched.

## Risks

- **Record-label inference.** `name`, `sigma` and `value` are shared by several records. The tests annotate
  every record value with its type.
- **ELEV rules.** These cover unique names (checked: no `Parameter*`, `FitRunName` or `*Parameter` case exists in
  this repo or in STL), primitives only inside single-case DUs, and no tuple of repeated types.
