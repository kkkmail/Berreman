# Step 0022 — ADD_RED — state of the world

## Where we are

Spec 0050, section S1.4: "Library entry files: envelope, provenance, canonical JSON, content hash,
schemas". Step 21 moved the app's one serializer options value into `CanonicalJson.options`. This
step declares RED_0005 (obligation S14-NUMBERS): entry files hold two kinds of number the shared
options cannot carry today.

- A `System.Numerics.Complex`: STJ writes `Real` / `Imaginary` / `Magnitude` / `Phase` and reads
  back zero.
- A MathNet `Matrix<double>`: reading it throws `NotSupportedException`, because it is an abstract
  type; only `Matrix<Complex>` has a converter.

001 §B says an entry that cannot serialise is a test failure. Step 23 (CLOSE_RED) adds
`ComplexConverter` and `RealMatrixConverter` to `CanonicalJson.options` and turns these facts green.

## What's working

- Added `CanonicalJsonTests`, three facts over `CanonicalJson.options`, red for the reasons the slice names
- Pinned the complex wire form: `Complex(1.5, -0.25)` must round-trip equal as a two-number array `[re, im]`
- Pinned the real-matrix wire form: the 30° ideal-linear Mueller matrix must round-trip bit-identical as four rows of four numbers
- Pinned an absorbing built-in: the EUV molybdenum entry's complexity must round-trip equal
- Registered the class in `OpticalConstructor.Tests.fsproj` after `PlacedSampleEmissionTests.fs`; no production file changed

## Tests

The arc-runner's gate engine is the gate authority. These are local observations made to record
the red:

- `build`: exit 0, `0 Warning(s)`, `0 Error(s)`.
- `constructor-unit-tests`, filtered to CanonicalJsonTests: 3 facts, all 3 fail. They are the
  declared reds and the class's only facts:
  - `a complex number survives the shared options as its real and imaginary parts`:
    expected `<1.5; -0.25>`, got `<0; 0>`;
  - `a real matrix survives the shared options as its rows`: `NotSupportedException`, because
    `Matrix<double>` is abstract;
  - `an absorbing built-in material survives the shared options`: the Mo index read back as
    `<0; 0>`.
- `constructor-unit-tests`, whole suite: 742 total, 3 failed (the reds), 739 succeeded. This equals
  step 21's baseline.
- `elevation`: not run. The change is test-only, and the test projects are outside the ELEV scope.
- The six s04 invariants: their patterns are absent from both changed files.
- A probe outside the product tree checked step 23's converter shape against the same three
  conditions, and all three hold (`.artifacts/0022-green-probe.log`).

The failing output in full is in `0022-impl-log.md` (Testing state).

```yaml
gates:
  constructor_unit_tests: 739
```

## Architecture

- The facts reach the options through the domain types an entry file holds, not through bare
  MathNet values:
  - `MuellerMatrix`, which FSharp.SystemTextJson unwraps through `RealMatrix4x4` and `RealMatrix`
    to `Matrix<double>`;
  - `MaterialComplexity`, whose `IsotropicAbsorbing` case carries a `ComplexRefractionIndex`,
    unwrapped to `Complex`.

  A converter registered for `Complex` or for `Matrix<double>` on `CanonicalJson.options` therefore
  reaches both.
- The real-matrix fact compares every element through `BitConverter.DoubleToInt64Bits`, both after
  reading back and against the JSON cell. That pins row-major order and exact doubles, not merely a
  tolerance.

## Deferred

Nothing in this step's scope. The converters themselves are step 23, the paired CLOSE_RED.

## Gotchas

- Each fact round-trips before it inspects the JSON shape. The red is the named cause, not a shape
  assertion; once step 23 lands, the shape checks pin the wire form.
- Today `Matrix<double>` serializes without throwing. Only the read fails.
- F# parses `Assert.True(x = y, msg)` with a plain identifier `x` as a named argument (FS0691).
  Bind the comparison first; `assertBitIdentical` does this.

## Changelog

- 2026-11-05 — Step 0022 (ADD_RED, RED_0005): added `CanonicalJsonTests` with three facts. A complex
  number, a real matrix and an absorbing built-in material each fail to survive
  `CanonicalJson.options`. All three are red for the named reasons; 739 other tests are unchanged.
