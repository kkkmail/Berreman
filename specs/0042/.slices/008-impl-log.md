# Impl-log — spec 0042, slice 008 (IMPLEMENT — Stage-1 AIR calibration)

## Progress

- [x] Read system/project/slice prompts and reference (`matrix_step1_air_fit.py`
      at `C:\GitHub\optics-mueller\analysis\scripts\`).
- [x] Confirmed ground-truth constants by running the reference pipeline against
      the OPM final CSVs (see Artifacts): z_LP 155.4401, z_CPL 96.9961, δ_an
      83.6680, θ_src 39.3110, δ_src 82.5534, dark_mean 869.666.
- [x] Added `CosineFit` / `SourceCplModel` + `freeCosineFit` /
      `retardanceFromVisibility` / `sourceCplFromLpFrame` (+ private `wrapDeg180`)
      to `OpticalConstructor.Domain.MuellerReconstruction`.
- [x] Added the always-run synthetic fact + the data-dependent cross-check fact
      (xunit.v3-native `Assert.Skip`) to `BerremanTests`.
- [x] Verified LF line endings on both edited `.fs` files (0 CR bytes).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`
  — appended the slice-008 Stage-1 section (2 types, 3 public functions, 1 private
  helper). Additive; no existing symbol changed.
- `Berreman/BerremanTests/MuellerReconstructionTests.fs` — `open System.IO`; five
  `let` helpers (`degree` alias, `tryFindOpmFinalDir`, `reduceAirTraces`,
  `lpFrameCoeffs`, `unwrapFit`) ahead of the members; two new `[<Fact>]` members.

## Decisions

- **`freeCosineFit` takes the proxy and returns `Result`.** The slice names
  `(anglesDeg) (norm) : CosineFit` *and* "through the MuellerSolverProxy"; the
  proxy is `Result`-returning and CLAUDE.md forbids throwing across a public
  boundary, so the faithful shape is
  `MuellerSolverProxy -> float[] -> float[] -> Result<CosineFit, SolverError>`.
- **`[<Fact>] + Assert.Skip` instead of `[<SkippableFact>]`** — the referenced
  `Xunit.SkippableFact 1.5.61` is an xunit **v2** package; this project runs
  xunit.v3. See Gotchas.
- **`sourceCplFromLpFrame` stays total** (no throw on the `sin 2θ ≈ 0`
  singularity the reference raises on) — the data never approaches it and
  CLAUDE.md forbids the throw.

## Testing state

- Per IMPLEMENT **Invariant 6** (the worker acts; it runs no checks), this worker
  did NOT execute the build/unit-tests/constructor-unit-tests gates — the
  arc-runner's deterministic gate engine runs them after exit. Correctness was
  established by static review + the reference-pipeline ground truth (Artifacts),
  the `Assert.Skip`/`SkipException` symbols confirmed present in
  `xunit.v3.assert.dll`, and a parse-cleanliness check of all four OPM CSVs (no
  malformed required cell → `parseMuellerCsv` loads every row).
- `commit_ready: true` — every slice requirement lands this round.

## Artifacts

- `.artifacts/008-stage1-groundtruth.py` / `.log` — reference reproduction of the
  five §2.2 constants (pins the 1° test tolerance bands).
