# Step 0022 — ADD_RED — impl plan

## Approach

Red first (RED_0005, obligation S14-NUMBERS): three facts over `CanonicalJson.options` (step 21) that
pin the two number shapes entry files hold and the shared options cannot carry today. Test sources
only; no production file changes.

- New `Berreman/OpticalConstructor/OpticalConstructor.Tests/CanonicalJsonTests.fs`:
  `module CanonicalJsonTests` in `namespace OpticalConstructor.Tests`.
- Appended to `OpticalConstructor.Tests.fsproj`'s compile list right after
  `PlacedSampleEmissionTests.fs`, with no new comment.

## The three facts

1. `a complex number survives the shared options as its real and imaginary parts`:
   - `Complex(1.5, -0.25)` is serialized and read back, and the result equals the original;
   - its JSON is a two-element number array `[re, im]`.
   - Red today: System.Text.Json writes `Real` / `Imaginary` / `Magnitude` / `Phase` and reads back zero.
2. `a real matrix survives the shared options as its rows`:
   - `analyzerMueller IdealLinear` at 30° (built as `PolarizerBehaviorTests` builds `lp0`) is
     serialized and read back;
   - every element is bit-identical, compared through `BitConverter.DoubleToInt64Bits` via the
     `MuellerMatrix.element` read seam;
   - its JSON is four arrays of four numbers, row-major.
   - Red today: reading `Matrix<double>` (abstract) throws `NotSupportedException`.
3. `an absorbing built-in material survives the shared options`: the `complexity` of the
   `builtInEntries` entry `MaterialIds.euvMolybdenum` (an `IsotropicAbsorbing` complex index) is
   serialized and read back, and the result equals the original. Red today for the reason of (1).

No fact catches an exception: a throw is the failure. Each fact does the round-trip before it checks
the JSON shape, so the red is the named reason (the zero read-back, the `NotSupportedException`).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CanonicalJsonTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (one Compile line)

## Risks

- The overloads of `JsonSerializer.Serialize` / `Deserialize` and `Assert.Equal`: annotate the types
  explicitly.
- Write side of `Matrix<double>`: STJ may write it as an object or throw. Either way the fact is red.
  The recorded output says which.
