# Step 0340 — impl log

## Progress

- [x] Added `misreadAbsolute` (after `misread`) and `noisyObserveAbsoluteWith` (after `noisyObserveWith`) to
  `BenchNoise.fs`, both answering `Result`s.
- [x] Deleted both declarations from `InverseFitHarness.fs`.
- [x] `AbsorbingBiaxialNoisyTests.recoverFromNoisy` pipes `noisyObserveAbsoluteWith … |> expectOk`.
- [x] Development runs: build, elevation, constructor-unit-tests, unit-tests (`BerremanTests.AbsorbingBiaxialNoisyTests`)
  and the 19 invariants are all green.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/BenchNoise.fs`:
  - `misreadAbsolute : UniformDeviateProvider -> NoiseParam -> MuellerMatrix -> Result<MuellerMatrix, MuellerFitError>`,
    after `misread`. The body is the harness body: the throughput draw first, then the fifteen element draws row by
    row in `misread`'s order, `m₀₀` the reported throughput. The harness's `failwith` on a normalization error is now
    `Error (MuellerNormalizationErr e)`. Its doc is three lines and keeps the throughput-first hazard line, citing
    specs/0044/.manual/019.
  - `noisyObserveAbsoluteWith … : Result<MuellerObservation list, MuellerFitError>`, after `noisyObserveWith`. The
    harness's `failwith` on a forward-model error is now `ObservationForwardErr`, as in `noisyObserveWith`.
  - A private `noisyObserveThrough`, the shared walk, takes the reading rule. It holds `noisyObserveWith`'s former
    body with `misread draws noise` replaced by `readWith draws noise`. `noisyObserveWith` calls it with `misread`,
    and `noisyObserveAbsoluteWith` calls it with `misreadAbsolute`.
- `Berreman/BerremanTests/InverseFitHarness.fs`: deleted `misreadAbsolute` and `noisyObserveAbsoluteWith` with their
  docs (40 lines). Nothing else changed.
- `Berreman/BerremanTests/AbsorbingBiaxialNoisyTests.fs`: `recoverFromNoisy` appends `|> expectOk`. The file already
  opens `OpticalConstructor.Optimization.BenchNoise` (step 339), so the call now resolves to the product.

## Decisions

- **One walk, not two copies.** The step describes `noisyObserveAbsoluteWith` as "the same walk as
  `noisyObserveWith` with `misreadAbsolute`". Copying the fold would leave two identical function bodies one
  argument apart, which AGENTS.md calls a defect ("Reuse, never copy … a duplicated function body … is a defect").
  - The walk is therefore a private `noisyObserveThrough` that takes the reading rule, and both public functions are
    one-line calls to it.
  - `noisyObserveWith`'s behaviour, signature and draw order are unchanged. Its doc is now two lines; the walk's
    description moved to `noisyObserveThrough`.
  - Everything stays in `BenchNoise.fs`, an authorized edit, and adds no public name.
- **Bit-identity.** Per configuration the stream is: two draws in `disturb` (incidence, then azimuth), then one
  throughput draw, then fifteen element draws row by row, skipping `m₀₀`. The harness's `List.map` and the product's
  in-order `Result` fold visit the configurations in the same order, and with no error every configuration is
  evaluated. The arithmetic is the harness's, character for character.
- **Error mapping.** Both harness `failwith`s are now typed errors, the same ones the transparent path already uses:
  `MuellerNormalizationErr` and `ObservationForwardErr`. No new error case was needed.

## Testing state

Logs are under `specs/0050/.artifacts/0340-dev-*.log`. Each holds the final run.

- **build:** exit 0, 0 errors, in 80 s. The 2 warnings are the vendored MathNet SYSLIB0051 in `Exceptions.cs`,
  which steps 0337 and 0339 also recorded. Our code has none.
- **elevation (no base, 12 of 12 projects):** `ELEV-GATE: OK - 0 new finding(s)`, known findings 0, in 219 s.
  - Optimization has 0 findings; its suppressed count rose from 11 to 15. The four new ones are the ELEV004 `.value`
    reads in `misreadAbsolute`, which the gate command excludes.
- **constructor-unit-tests (whole):** `Passed!`, 1553 of 1553, in 72 s. The count is unchanged from step 339.
- **unit-tests (`BerremanTests.AbsorbingBiaxialNoisyTests`):** `Passed!`, 1 of 1, in 1492 s (24 min 50 s), against a
  7200 s deadline.
  - The run is slower than the step's 946 s estimate because elevation and the constructor suite ran concurrently
    for its first five minutes or so.
  - The change-expectation fact's TRX outcome is `Passed`. The class declares 1 fact, the same as HEAD.
- **invariants:** all 19 `git grep`s found nothing.
- **Acceptance greps:**
  - `git grep -n -F "let noisyObserveAbsoluteWith" -- Berreman/BerremanTests/` finds nothing.
  - `misreadAbsolute` and `noisyObserveAbsoluteWith` are each declared once, in `BenchNoise.fs`.
- **LF:** the three edited files have 0 CR bytes; `git ls-files --eol` shows `i/lf w/lf`.

## Artifacts

- `specs/0050/.artifacts/0340-dev-build.log`
- `specs/0050/.artifacts/0340-dev-elevation.log`
- `specs/0050/.artifacts/0340-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0340-dev-invariants.log`
- `specs/0050/.artifacts/0340-dev-unit-tests.log`

## Gotchas

- **`LabFileImportTests.fs:29` names `misreadAbsolute` in a doc comment.** It describes the matrix shape, which is
  unchanged. The name now resolves to `BenchNoise.misreadAbsolute`. The file is outside this step's effects, so the
  comment is unchanged.
- **The class doc of `AbsorbingBiaxialNoisyTests` still tells the history of the third budget.** The step authorizes
  only the `recoverFromNoisy` pipe in that file, so the narrative is unchanged.
- **Git Bash's `grep -c $'\r'` reported every line as a CR.** The byte count in Python is 0. Check line endings with
  `git ls-files --eol` or a byte count.
