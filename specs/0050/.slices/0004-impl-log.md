# Step 0004 — ADD_RED — impl log

## Progress

- Read the protocol (`arc-runner.system-md`, `add_red_worker.system-md`), the project prompt,
  `AGENTS.md`, the slice, its `.gates` snapshot, the SoWs of steps 1 to 3, and step 5's brief.
  Reading step 5 confirmed that the fact compares only through `=` / `hash`. That fits the
  `TaucLorentzAxis` / `GaussianOscillatorAxis` law cases step 5 adds.
- Confirmed the red reason in the code:
  - `toEpsAxis` lowers `TaucLorentz` / `GaussianOscillator` to `EpsAxisEvaluated this.evaluate`
    (`OpticalConstructor.Domain/DispersionModels.fs:766-769`).
  - `evaluate` (`:571-574`) answers a new `fun w -> …` on every call.
  - `EpsAxisDispersion.Equals` compares `EpsAxisEvaluated` with `PhysicalEquality`, and
    `GetHashCode` uses `PhysicalHash` (`Berreman/Berreman/Dispersion.fs`).
- Hoisted the AC-B5 coefficient sets so they are reused, not re-transcribed (AGENTS.md "Reuse,
  never copy"):
  - Added `taucLorentzCoefficients ()` and `gaussianOscillatorCoefficients ()`, private builders
    with the exact literals of `AC-B5 transcendental models lower to the evaluated case and match
    evaluate`. Each call builds a fresh record.
  - The AC-B5 fact now reads `TaucLorentz (taucLorentzCoefficients ())` and
    `GaussianOscillator (gaussianOscillatorCoefficients ())`. Its comment, its match on
    `EpsAxisEvaluated _` and its assertions are unchanged.
- Added `assertLowersToEqualData (label) (build) (shifted)`, a private helper that checks, in this
  order:
  1. `model.toEpsAxis` lowered twice: the axes are equal and their `hash` values are equal. This
     is the first assertion, and on the start tree it fails.
  2. Two separately built models (`build ()` called twice, each with a fresh coefficient record):
     the axes are equal and their hashes are equal.
  3. `toEpsValue` of one model taken twice is equal, and `toEpsValue` of the two equal models is
     equal.
  4. `shifted.toEpsAxis` is not equal to the model's axis.
- Added the fact `equal Tauc-Lorentz and Gaussian-oscillator models lower to equal axis data`
  right after the AC-B5 transcendental fact. It runs the helper twice:
  - Tauc-Lorentz: the shifted model has `bandGap` 2.2 → 2.4.
  - Gaussian oscillator: the shifted model has `energy` 2.5 → 2.7.
- No production file was touched.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DispersionModelsTests.fs` (+53 / −21).
  The file is LF: `git diff --numstat` equals `git diff --ignore-cr-at-eol --numstat`.

## Testing state

These runs are local observations made to record the red, as the ADD_RED obligation requires.
The arc-runner's gate engine runs and records the gates.

- `dotnet build Berreman.slnx -c Release -nologo -v:m` exited 0 with 0 errors. The only
  warnings are the two vendored `MathNetNumerics/Numerics/Exceptions.cs` SYSLIB0051 warnings,
  which were already on the start tree. After review I added an explicit `: unit` return type to
  `assertLowersToEqualData`. I then rebuilt (0 warnings, 0 errors, because the vendored projects
  were not recompiled) and re-ran the class, which gave the same red at the same lines. Log:
  `.artifacts/0004-build.log`, from the rebuild.
- The gate's selected run, `dotnet test --no-build -c Release -p:Platform=x64 --no-ansi
  --filter-method "OpticalConstructor.Tests.DispersionModelsTests.*" --filter-method
  "OpticalConstructor.Tests.DispersionModelsTests"`, exited 2 with `total: 30, failed: 1,
  succeeded: 29`. Log: `.artifacts/0004-red-dispersionmodelstests.log`. The actual failure:

  ```
  failed OpticalConstructor.Tests.DispersionModelsTests.equal Tauc-Lorentz and Gaussian-oscillator models lower to equal axis data (5ms)
    Tauc-Lorentz: one model lowered twice gave unequal axes (EpsAxisEvaluated <fun:get_evaluate@573> vs EpsAxisEvaluated <fun:get_evaluate@573>)
      at OpticalConstructor.Tests.DispersionModelsTests.assertLowersToEqualData(...) in ...\DispersionModelsTests.fs:350
      at OpticalConstructor.Tests.DispersionModelsTests.equal Tauc-Lorentz and Gaussian-oscillator models lower to equal axis data() in ...\DispersionModelsTests.fs:365
  ```

  This matches the acceptance. Only the new fact fails, and it fails on the first equality of
  two lowered axes, two `EpsAxisEvaluated` closures built by the same `get_evaluate`. The 29
  facts the class held before all pass, including the AC-B5 transcendental fact over the hoisted
  builders.
- A whole-suite run of `constructor-unit-tests` gave `total: 733, failed: 1, succeeded: 732`.
  The one failure is the declared red, and the 732 passing facts equal the prior baseline. Log:
  `.artifacts/0004-constructor-unit-tests-whole.log`.
- `elevation` (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`) reported
  `ELEV-GATE: OK - no in-scope project was touched`, because the test project is outside the ELEV
  scope. Log: `.artifacts/0004-elevation.log`.
- Each of the three s01 invariants (`s01-elev-list-empty`, `s01-no-vstest`,
  `s01-no-kit-kit-json`) ran its `git grep` and found nothing (exit 1).

## Artifacts

All under `specs/0050/.artifacts/`:

- `0004-build.log`
- `0004-red-dispersionmodelstests.log`
- `0004-constructor-unit-tests-whole.log`
- `0004-elevation.log`

## Gotchas

- I edited the existing AC-B5 transcendental fact, though only to call the shared builders. The
  slice says the new fact "uses" AC-B5's coefficient sets, and AGENTS.md calls a re-transcribed
  constant a defect, so I hoisted the sets rather than copying them. The fact's assertions, its
  `EpsAxisEvaluated _` match and its comment are exactly as before. Step 5 still edits only those
  (its brief cites `:300-337`, `:302-305` and `:378-379`). Those lines have drifted by the hoist
  and the insertion, and the names find them.
- The builders are `unit -> record` functions, not values, so that "two separately built equal
  models" are two allocations down to the coefficient record. A shared record value would let a
  reference comparison of the coefficients pass by accident.
- Each comparison inside `Assert.True` / `Assert.False` is parenthesised, so that F# cannot read
  `x = y` as a named method argument.
