# 008 — Calculations & analysis layer (Part F) — impl-log

## Progress

- [x] Read system prompt, project prompt, slice spec, engine seams.
- [x] Edit `FieldFunctions.fs:93` `em.muellerMatrix` (F.3).
- [x] Create `Analytics/AnalysisFunctions.fs` (F.1/F.3/F.4/F.5).
- [x] Create `Analytics/FieldProfile.fs` (F.6/F.7).
- [x] Create `Analytics/Colorimetry.fs` (F.8).
- [x] Register new files in `Analytics.fsproj`.
- [x] Create `BerremanTests/MuellerMatrixTests.fs`.
- [x] Create `BerremanTests/AnalysisFunctionsTests.fs`.
- [x] Register test files + Analytics ref + Softellect.Sys in `BerremanTests.fsproj`.
- [x] `build` gate green.
- [x] `unit-tests` gate green (76 passed, baseline 70).
- [x] `constructor-unit-tests` gate green (107 passed, baseline 107).
- [x] **Cycle 2 (cleanup):** stripped the incidental CRLF EOL-normalization churn from
  `FieldFunctions.fs` and both `.fsproj` files; the diff now collapses to only the real
  edits (verified `git diff HEAD` == `git diff HEAD --ignore-space-at-eol`).
- [x] **Cycle 2 (cleanup):** `MuellerMatrixTests` now reuses `MatrixComparison.allowedDiff`
  instead of a fresh `1.0e-10` literal; re-ran build + unit-tests green.

## Files modified

New:
- `Berreman/Analytics/AnalysisFunctions.fs` — `absorptance`, `stokesSystem`,
  `degreeOfPolarization`, `psiDelta`, `ncs`.
- `Berreman/Analytics/FieldProfile.fs` — `fieldDepthProfile`, `layerAbsorptance`,
  `totalAbsorbedPower`.
- `Berreman/Analytics/Colorimetry.fs` — `Illuminant`, `spectrumToXyz`, `xyzToLab`,
  `xyzToSrgb` + embedded CIE 1931 2° CMF and D65/A illuminant tables (10 nm grid).
- `Berreman/BerremanTests/MuellerMatrixTests.fs` — AC-F3 engine-edit guard.
- `Berreman/BerremanTests/AnalysisFunctionsTests.fs` — AC-F1/F4/F6/F9/F2 smokes.

Edited:
- `Berreman/Berreman/FieldFunctions.fs` — implemented the commented-out
  `member em.muellerMatrix` (line 93) as `MuellerMatrix.fromEmFields em em`.
- `Berreman/Analytics/Analytics.fsproj` — registered the three new files after
  `Charting.fs` (AnalysisFunctions → FieldProfile → Colorimetry).
- `Berreman/BerremanTests/BerremanTests.fsproj` — registered the two new test files,
  added a `ProjectReference` to `Analytics` and a `PackageReference` to `Softellect.Sys`
  (10.0.101.41, matching `OpticalConstructor.Storage`) needed by the AC-F2 `.binz` test.

## Decisions

- **`em.muellerMatrix` (F.3).** Implemented as a pure delegation
  `MuellerMatrix.fromEmFields em em` — the field's own s/p-resolved amplitudes
  (`amplitudeS`/`amplitudeP`) supplied as both arguments. No 4×4 algebra re-derived
  (that lives in `MuellerMatrix.create`). The mandatory `MuellerMatrixTests` smoke asserts
  the member equals `MuellerMatrix.fromEmFields em em` element-wise within
  `MatrixComparison.allowedDiff` (the project's shared tolerance home, per the project
  prompt's Code-style note — cycle 2 dropped the earlier fresh `1.0e-10` literal). The
  delegation diff is identically zero, so the looser shared tolerance still guards.
- **`psiDelta`/`ncs` ratio (F.5).** ρ is defined as
  `reflected.amplitudeP / reflected.amplitudeS` (r_p/r_s of the single computed
  `Solution`'s reflected field) — matching the **consumer** definition in slice 010 §G.6
  ("ρ = r_p / r_s … exposed on `EmFieldSystem.reflected`") and slice 011's AC-F5 invariant
  (`ncs` satisfies N²+C²+S²=1, which holds by construction). The slice-008 AC-F5 prose
  said "r_pp/r_ss from `solutionP()`/`solutionS()`", but that requires two solves and is
  incompatible with the spec's own `psiDelta (s : Solution)` signature; the downstream
  §G.6 single-reflected-field definition is the one §F.5 must NOT be forked from, so I
  used it. tan Ψ = |ρ|, Δ = arg ρ.
- **`totalAbsorbedPower` (F.7).** Anchored to the §F.1 `absorptance` (1−R−T) of the full
  solution so it equals the system-level A within solver tolerance (AC-F7) and introduces
  no second, divergent absorption definition. The per-layer Poynting-flux breakdown is
  reported separately by `layerAbsorptance`, which honours the carried-forward
  `EmField.propagate` limitation (`BerremanMatrix.fs:221`) rather than being summed into a
  total that would diverge from A.
- **AC-F2 `.binz` round-trip.** Performed via the in-memory `Softellect.Sys.Core.serialize`/
  `deserialize` with `BinaryZippedFormat` (the exact named seam, returns/consumes a
  `byte[]`). No file is written to disk, so the rule "never under `%TEMP%`" is satisfied
  trivially and no machine-specific absolute artifacts path is baked into a committed test
  (which lives on through slice 011+). Fidelity is asserted on the float keys and the R
  channel (`Solution.func R`) rather than reference equality of the `Solution` class
  graph; JSON-exclusion is asserted by checking the payload begins with the gzip magic
  (0x1f 0x8b), never the canonical JSON `'{'`.
- **CIE tables.** Embedded the CIE 1931 2° CMFs and the D65/A relative SPDs at a 10 nm grid
  (380–780 nm). `spectrumToXyz` normalises Y by Σ(I·ȳ) so a perfect reflector yields the
  illuminant white point (Y = 1); `xyzToSrgb` uses the standard D65 sRGB matrix + gamma,
  so flat R = 1 under D65 → near-white (slice-011 AC-F8 sanity check).

## Testing state

All three gates pass locally (commit-ready):
- `build`: `dotnet build Berreman.slnx -c Release` — succeeded, 0 errors.
- `unit-tests` (`BerremanTests`): 76 passed, 5 skipped, 0 failed (baseline 70 → 76).
- `constructor-unit-tests` (`OpticalConstructor.Tests`): 107 passed, 0 failed (baseline 107).

New tests (6): `MuellerMatrixTests` (1), `AnalysisFunctionsTests` (AC-F1/F4/F6/F9/F2 = 5).
The FULL AC-F3/F5/F7/F8 equivalence suite remains deferred to slice 011's
`OptimizationTests.fs` per the parent's routing.

## Artifacts

No persistent run artifacts produced (the AC-F2 `.binz` round-trip is in-memory; no file
written). `C:\GitHub\Berreman\specs\0022\.artifacts` left unused this round.
