# Step 0340 — state of the world

## Where we are

Spec 0050 section S10.1 moves the bench noise model and the fit harness into Optimization.

- Steps 335–338 moved the fit machinery into `OpticalConstructor.Optimization/MuellerFitting.fs`.
- Step 339 moved the noise model's data and its TRANSPARENT path into `OpticalConstructor.Optimization.BenchNoise`.
- This step moves the ABSORBING path (`misreadAbsolute`, `noisyObserveAbsoluteWith`) under obligation
  S101-ABSOLUTE-NOISE-MOVE. With it, `BerremanTests` declares no noise function, and
  `git grep -n -F "let noisyObserveAbsoluteWith" -- Berreman/BerremanTests/` finds nothing (001 §T "From section 10").

## What's working

- Moved `misreadAbsolute` and `noisyObserveAbsoluteWith` from `InverseFitHarness` into
  `OpticalConstructor.Optimization.BenchNoise`; each is now declared once.
- Both answer `Result`s: the harness's two `failwith`s are now `MuellerNormalizationErr` and `ObservationForwardErr`.
- `noisyObserveWith` and `noisyObserveAbsoluteWith` share one private walk, `noisyObserveThrough`, which takes the
  reading rule; the fold is no longer written twice.
- `AbsorbingBiaxialNoisyTests` draws its observations through the product and unwraps them with `expectOk`; its
  numbers and count are unchanged.

## Tests

These are development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the
gates themselves. Logs are under `specs/0050/.artifacts/0340-dev-*.log`.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; 2 warnings, both vendored MathNet SYSLIB0051 | 80 s | 600 s |
| elevation (no base, 12 of 12 projects) | `ELEV-GATE: OK - 0 new finding(s)`, known findings 0 | 219 s | 600 s |
| constructor-unit-tests (whole) | Passed!, 1553 of 1553, 0 failed, 0 skipped | 72 s | 180 s |
| unit-tests (`BerremanTests.AbsorbingBiaxialNoisyTests`) | Passed!, 1 of 1, 0 failed, 0 skipped | 1492 s (24 min 50 s) | 7200 s |
| 19 × invariant-s33-* | every `git grep` found nothing | < 1 s each | 300 s |

- No run exceeded its deadline.
  - The unit-tests run took longer than the step's 946 s estimate because elevation and the constructor suite ran
    concurrently for its first five minutes or so. 1492 s is still well inside the 7200 s deadline.
- **The change-expectation fact passed:** `an ensemble of noisy experiments recovers the absorption far better than
  the optical activity`. Its TRX outcome is `Passed`, in 24 min 49 s.
  - The fact asserts bias, scatter and ordering bands over the ensemble. The draws are taken in the harness's order
    with the harness's arithmetic, so its numbers are unchanged.
- **Counts:**
  - `AbsorbingBiaxialNoisyTests` declares 1 fact, the same as HEAD.
  - The constructor suite is at 1553, the same as step 339.
  - No test was added, removed or renamed.
- **The build's two warnings** are SYSLIB0051 from the vendored `MathNetNumerics/Numerics/Exceptions.cs`. Steps 0337
  and 0339 recorded the same pair. Our code has no warnings.
- **Acceptance greps:**
  - `git grep -n -F "let noisyObserveAbsoluteWith" -- Berreman/BerremanTests/` finds nothing.
  - `misreadAbsolute` and `noisyObserveAbsoluteWith` are each declared once, in `BenchNoise.fs`, and both answer
    `Result`s.
- **LF:** the three edited files have 0 CR bytes.

```yaml
gates:
  berreman_unit_tests: 1
  constructor_unit_tests: 1553
```

## Architecture

- **The bench noise model lives only in `BenchNoise`.** The module now holds both reading rules (`misread`,
  `misreadAbsolute`) and both observers (`noisyObserveWith`, `noisyObserveAbsoluteWith`).
- **One walk, two reading rules.** The private `noisyObserveThrough` holds the ordered `Result` fold. For each
  configuration it disturbs, evaluates, reads and files the reading against the nominal configuration, and the first
  error stops it. The public observers differ only in the reading rule they pass. This follows AGENTS.md's "a
  duplicated function body … is a defect"; the step's "the same walk as `noisyObserveWith`" is implemented as that
  same walk, not as a copy of it.
- **The seeded stream is unchanged.** Per configuration: two draws in `disturb`, then one throughput draw, then the
  fifteen element draws row by row. The arithmetic is the harness's, character for character.

## Deferred

Nothing. Every requirement of the step landed in this round.

## Gotchas

- **`LabFileImportTests.fs:29` names `misreadAbsolute` in a doc comment** describing the matrix shape. The name now
  resolves to `BenchNoise.misreadAbsolute`. The file is outside this step's effects, so the comment is unchanged.
- **`noisyObserveWith`'s doc changed with its body.** The walk's description moved to `noisyObserveThrough`, and the
  public doc says what the function returns. Signature, behaviour and draws are unchanged.
- **Optimization's suppressed ELEV count rose from 11 to 15.** The four new ones are ELEV004 `.value` reads in
  `misreadAbsolute`, which the gate command excludes.
- **Git Bash's `grep -c $'\r'` reports every line as a CR.** Check line endings with `git ls-files --eol` or a byte
  count.

## Changelog

- 2026-11-12 — Step 0340: moved the bench noise model's absorbing path (`misreadAbsolute`,
  `noisyObserveAbsoluteWith`) from `InverseFitHarness` into `OpticalConstructor.Optimization.BenchNoise`.
  - Both now answer `Result`.
  - Both observers share the private walk `noisyObserveThrough`.
  - `AbsorbingBiaxialNoisyTests` unwraps the observations with `expectOk`.
  - The numbers are bit-identical.
