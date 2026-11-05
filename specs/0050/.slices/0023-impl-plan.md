# Step 0023 — CLOSE_RED — impl plan

## Goal

Close RED_0005 (declared by step 22): `CanonicalJson.options` cannot carry a
`System.Numerics.Complex` (STJ writes its four properties and reads back zero) nor a MathNet
`Matrix<double>` (reading it throws `NotSupportedException`, the type is abstract). The three
`CanonicalJsonTests` facts are red for exactly those causes.

## Approach

One file, `Berreman/OpticalConstructor/OpticalConstructor.Storage/CanonicalJson.fs` (the step's
only authorized edit):

1. `ComplexConverter` — a private `JsonConverter<Complex>` beside `ComplexMatrixConverter`. Writes
   `[re, im]`; reads the pair back through `JsonDocument.ParseValue`, as `ComplexMatrixConverter`
   reads its cells. A value that is not a two-number array raises `JsonException`.
2. `RealMatrixConverter` — a private `JsonConverter<Matrix<double>>`. Writes an array of rows of
   numbers; reads them back through `Matrix<double>.Build.DenseOfArray`. A value that is not a
   rectangular array of number arrays raises `JsonException`.
3. Register both in `options` before the `JsonFSharpConverter` (beside `ComplexMatrixConverter`),
   and name them in the options' doc comment. No other option changes.

The shape checks live in the converter members (members are exempt from the primitive-signature
rules), so no free/private function with a primitive signature is added (ELEV003P).

## Risks

- `Complex` is reached everywhere FSharp.SystemTextJson unwraps a single-case union over it
  (`ComplexRefractionIndex`, ...). The project, groups and environment formats gain the `[re, im]`
  form for a complex scalar; no schema constrains it (the project schema's `$defs` hold no complex
  object), so their facts stay green. Check by running the whole constructor suite.
- ELEV: private classes with overrides only; matches the existing converters, which are clean.

## Verification (local observation; the gate engine is the authority)

build; constructor-unit-tests whole suite (742 expected, 0 failed); elevation over Storage and its
dependents; the six s04 invariant greps.
