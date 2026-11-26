# Step 0022 — ADD_RED — impl log

## Progress

- [x] Read the inputs:
  - the slice and its `.gates` snapshot;
  - step 21's SoW;
  - `CanonicalJson.fs`, `PolarizerBehaviorTests.fs` and `MaterialLibrary.fs`;
  - step 23, the paired CLOSE_RED, so the facts pin exactly what its converters deliver.
- [x] Authored `CanonicalJsonTests.fs` and registered it in the fsproj, right after
  `PlacedSampleEmissionTests.fs`, with no comment.
- [x] Fixed the first build's FS0691: F# read `expected = …` inside `Assert.True(…)` as a named
  argument. The bit comparison moved into the private helper `assertBitIdentical`.
- [x] Built (exit 0, 0 warnings, 0 errors) and observed all three facts red, each for the reason the
  slice names.
- [x] Probed, outside the product tree, that step 23's converter shape turns all three green. See
  Artifacts.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CanonicalJsonTests.fs` (new):
  - `module CanonicalJsonTests` in `namespace OpticalConstructor.Tests`;
  - three facts over `CanonicalJson.options`, plus private helpers: `toJson` / `ofJson`,
    `arrayItems`, `numbersOf` and `assertBitIdentical`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`:
  - one `<Compile Include="CanonicalJsonTests.fs" />` line after `PlacedSampleEmissionTests.fs`.

No production file changed.

## Testing state

These are local observations made to record the red. The arc-runner's gate engine is the gate
authority.

- `build`: `dotnet build Berreman.slnx -c Release -nologo -v:m` exited 0 with
  `0 Warning(s)`, `0 Error(s)`.
- `constructor-unit-tests` (CanonicalJsonTests, the step's filter): `total: 3`, `failed: 3`,
  `succeeded: 0`. The facts are the class's only facts, and each fails for its declared reason.
  Actual output:

  ```
  failed OpticalConstructor.Tests.CanonicalJsonTests.a complex number survives the shared options as its real and imaginary parts (254ms)
    Assert.Equal() Failure: Values differ
    Expected: <1.5; -0.25>
    Actual:   <0; 0>
      at ...CanonicalJsonTests.fs:41
  failed OpticalConstructor.Tests.CanonicalJsonTests.a real matrix survives the shared options as its rows (86ms)
    Xunit.MicrosoftTestingPlatform.XunitException: System.NotSupportedException : Deserialization of interface or abstract types is not supported. Type 'MathNet.Numerics.LinearAlgebra.Matrix`1[System.Double]'. Path: $ | LineNumber: 0 | BytePositionInLine: 1.
      at System.Text.Json.Serialization.JsonUnwrappedUnionConverter`2.Read(...)   (MuellerMatrix -> RealMatrix4x4 -> RealMatrix -> Matrix<double>)
      at ...CanonicalJsonTests.fs:49
  failed OpticalConstructor.Tests.CanonicalJsonTests.an absorbing built-in material survives the shared options (97ms)
    Assert.Equal() Failure: Values differ
    Expected: { eps = EpsWithoutDispValue (IsotropicAbsorbing (ComplexRefractionIndex <0.99; 0.045>)) ... }
    Actual:   { eps = EpsWithoutDispValue (IsotropicAbsorbing (ComplexRefractionIndex <0; 0>)) ... }
      at ...CanonicalJsonTests.fs:63
  Test run summary: Failed!
    total: 3
    failed: 3
    succeeded: 0
  ```

- `constructor-unit-tests`, whole suite: `total: 742`, `failed: 3`, `succeeded: 739`.
  - The three failures are exactly the declared reds.
  - 739 equals step 21's baseline, so nothing regressed.
- `elevation`: not run. This step touches only the test project, which is outside the ELEV scope
  (AGENTS.md), and the ADD_RED role runs no gate.
- The six s04 invariants: not run as gates. A grep of the two changed files for every invariant
  pattern finds nothing:
  - `ELEV0`;
  - the VSTest packages;
  - `kit.kit-json`;
  - `EpsAxisEvaluated`;
  - `complexity = (None|Some)`;
  - the seeded string preset ids.
- Line endings: both files are LF. `git diff --numstat` equals `--ignore-cr-at-eol`.

## Artifacts

In `specs/0050/.artifacts/`:

- `0022-build.log`: the green build.
- `0022-constructor-unit-tests-red.log`: the class run, three reds with full stacks.
- `0022-constructor-unit-tests-whole.log`: the whole suite, 742 / 3 failed / 739 succeeded.
- `0022-green-probe.log`: the feasibility probe of step 23's shape.
  - The probe was an fsx over the built assemblies. It copied `CanonicalJson.options` and inserted
    a `[re, im]` Complex converter and a rows `Matrix<double>` converter. The log embeds the script,
    and the script itself was deleted after the run.
  - Output:
    - the complex JSON is `[1.5, -0.25]` and equal after reading back;
    - the Mueller JSON is four rows of four numbers, bit-identical after reading back;
    - the Mo complexity is equal after reading back.

## Gotchas

- **Round-trip before shape.** Each fact reads back before it inspects the JSON, so its red is the
  named cause (the zero read-back, the `NotSupportedException`) and not a shape assertion. Once
  step 23 lands, the shape checks pin the wire form:
  - `[re, im]` for a complex number;
  - row-major rows of numbers for a matrix, with each JSON cell bit-identical to its element.
- **The Mueller matrix is serialized as `MuellerMatrix`, not as a bare `Matrix<double>`.**
  FSharp.SystemTextJson unwraps `MuellerMatrix -> RealMatrix4x4 -> RealMatrix` down to
  `Matrix<double>`. The stack trace shows this as three `JsonUnwrappedUnionConverter` frames. The
  fact therefore exercises the shape an entry file actually holds (a `ConstantMueller` component's
  `matrix`), and a `JsonConverter<Matrix<double>>` registered on the options reaches it.
- **The angle is built as the slice says.** `Angle.degree 30.0<deg>` is the same construction
  `PolarizerBehaviorTests`' private `deg` helper performs. That helper is private, so it is not
  copied; the literal is written directly.
- **The `Matrix<double>` write side does not throw today.** Only the read throws, as the slice
  says.
- **FS0691.** In F#, `Assert.True(x = y, msg)` with a plain identifier `x` parses as a named
  argument. Bind the comparison first.
