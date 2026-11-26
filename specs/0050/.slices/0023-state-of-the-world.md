# Step 0023 — CLOSE_RED — state of the world

## Where we are

Spec 0050, section S1.4: "Library entry files: envelope, provenance, canonical JSON, content hash,
schemas". Step 21 moved the app's one serializer options value into `CanonicalJson.options`. Step 22
declared RED_0005 (obligation S14-NUMBERS): entry files hold two kinds of number those options could
not carry, a `System.Numerics.Complex` and a MathNet `Matrix<double>`. This step closes RED_0005. It
adds the two converters to `CanonicalJson.options`, and the three `CanonicalJsonTests` facts go green.

## What's working

- Added `ComplexConverter` to `CanonicalJson.options`: a `Complex` is written and read as `[re, im]`
- Added `RealMatrixConverter`: a `Matrix<double>` is written as its rows of numbers and read back through `Matrix<double>.Build.DenseOfArray`
- A malformed complex or real matrix (not a pair, ragged or null rows, a non-number) raises `JsonException`, which callers map to `MalformedJsonErr`
- `ComplexMatrixConverter` writes and reads its cells through `ComplexConverter`, so `[re, im]` has one writer and one reader; its wire form is unchanged
- RED_0005 is closed: the complex, real-matrix and absorbing built-in material facts pass, and so do all 742 constructor tests

## Tests

The arc-runner's gate engine is the gate authority. These are local observations:

- `build`: exit 0, `0 Error(s)`. The two warnings are `SYSLIB0051` from the vendored
  `MathNetNumerics/Numerics` (C#), which recompiled in this run. Earlier steps' build logs show the
  same warning, and no warning comes from our code (`.artifacts/0023-build.log`).
- `constructor-unit-tests`, whole suite: 742 total, 742 succeeded, 0 failed, 0 skipped. In the TRX,
  the three RED_0005 facts are `Passed` (`.artifacts/0023-constructor-unit-tests-whole.log`).
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 5 project(s)`, over Storage and its dependents
  App, TestWindows.App, TestWindows and Ui. Known findings: 0 (`.artifacts/0023-elevation-local.log`).
- The six s04 invariants: each grep finds nothing.
- Malformed-input probe: outside the product tree, it fed bad shapes through
  `CanonicalJson.options` (`.artifacts/0023-malformed-probe.log`).

```yaml
gates:
  constructor_unit_tests: 742
```

## Architecture

- All three number converters read their numbers through System.Text.Json itself. They call
  `JsonSerializer.Deserialize<float[]>` / `<float[][]>`, or a cell's `Deserialize<Complex>`, over the
  same options, so STJ parses doubles one way everywhere. A converter adds only the shape check that
  STJ cannot make: exactly two numbers, rectangular rows.
- FSharp.SystemTextJson unwraps single-case unions down to these BCL and MathNet types:
  `ComplexRefractionIndex` reaches `Complex`, and `MuellerMatrix` reaches `Matrix<double>` through
  `RealMatrix4x4` and `RealMatrix`. Registering a converter on the type therefore covers every
  domain wrapper over it.
- Every store reads `CanonicalJson.options`, so the project, groups and environment formats now write
  a complex scalar as `[re, im]` too. No schema constrains it: the project schema's `$defs` hold no
  complex object. No committed JSON held the old four-property form (`git grep`).

## Deferred

Nothing. RED_0005 is closed in full. No other option changed.

## Gotchas

- STJ reads an out-of-range number such as `1e400` as infinity, both here and for every `float`
  field in the app. The probe shows `[1e400, 0]` reading as `<∞; 0>`. This is STJ's default and was
  not introduced here.
- A converter for a struct (`Complex`) is called for a JSON `null`, unlike one for a reference type.
  The `[| re; im |]` pattern rejects the resulting null array, which raises `JsonException`.
- `ComplexMatrixConverter` still sizes its array from the first row. A ragged complex matrix reads
  as it did before: a longer row throws (still mapped to `MalformedJsonErr`), and a shorter one is
  zero-filled. Its row handling is outside this step; only its cells changed.

## Changelog

- 2026-11-05 — Step 0023 (CLOSE_RED, RED_0005): `CanonicalJson.options` gains `ComplexConverter`
  (`[re, im]`) and `RealMatrixConverter` (rows, `DenseOfArray`), and `ComplexMatrixConverter`'s cells
  go through `ComplexConverter`. The three `CanonicalJsonTests` facts pass; the constructor suite is
  742/742.
