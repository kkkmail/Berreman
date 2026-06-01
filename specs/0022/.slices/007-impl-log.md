# 007 — Impl log: Light sources & illumination (Part E)

## Progress

- [x] SourceSpec.fs (Domain) — types + pure toIncidentLight/sample/expand/angularSpectrum
- [x] SourceCombination.fs (Domain) — weighted incoherent StokesVector reducer
- [x] Project.fs aggregate + `sources` field; compile-order reorder (SourceSpec/SourceCombination before Project)
- [x] SpectralImport.fs (Storage) — FSharp.Data CSV → (WaveLength*float) list in meters
- [x] SourceEditorView.fs (Ui seam) — editor update + axis helpers + palette entry
- [x] PolarizationPicker.fs (Ui seam) — presets + bidirectional write-back + live Stokes + Poincaré marker
- [x] SourceProjectionTests.fs / SourceExpansionTests.fs
- [x] Gates: build / unit-tests / constructor-unit-tests — all PASS

## Files modified

New:
- `OpticalConstructor/OpticalConstructor.Domain/SourceSpec.fs`
- `OpticalConstructor/OpticalConstructor.Domain/SourceCombination.fs`
- `OpticalConstructor/OpticalConstructor.Storage/SpectralImport.fs`
- `OpticalConstructor/OpticalConstructor.Ui/Sources/SourceEditorView.fs`
- `OpticalConstructor/OpticalConstructor.Ui/Sources/PolarizationPicker.fs`
- `OpticalConstructor/OpticalConstructor.Tests/SourceProjectionTests.fs`
- `OpticalConstructor/OpticalConstructor.Tests/SourceExpansionTests.fs`

Edited:
- `OpticalConstructor.Domain.fsproj` — compile `SourceSpec.fs`+`SourceCombination.fs` after BeamTree but before `Project.fs`.
- `OpticalConstructor.Domain/Project.fs` — `OpticalConstructorProject` gains `sources : SourceSpec.SourceSpec list`.
- `OpticalConstructor.Storage.fsproj` — register `SpectralImport.fs`.
- `OpticalConstructor.Ui.fsproj` — register `Sources/SourceEditorView.fs`, `Sources/PolarizationPicker.fs`.
- `OpticalConstructor.Tests.fsproj` — register the two new test files.
- `ProjectJsonRoundtripTests.fs` (×2), `RoundTripTests.fs`, `StackEditTests.fs` — the four
  `OpticalConstructorProject` literals gain `sources = []`.

## Testing state

All three gates PASS in the local run:

- `build` — `Build succeeded.` / `0 Error(s)` (no lowercase `error` token). Log `.artifacts/007-build.log`.
- `unit-tests` — Passed 70 / Skipped 5 / Total 75; `BerremanTests` untouched, baseline
  `berreman_unit_tests = 70` held. Log `.artifacts/007-unit-tests.log`.
- `constructor-unit-tests` — Passed 106 / Failed 0 / Total 106 (up from the slice-006
  baseline of 77; +29: 12 projection + 13 expansion + 4 round-trip sites recompiled).
  Log `.artifacts/007-constructor-unit-tests.log`.

Two failures surfaced mid-round and were fixed before exit:
1. `SpectralProfile.sample` originally used `Analytics.getWaveLengthValue`, which
   re-wraps the meter magnitude as a nm scalar and so mis-scales `.value` by
   `nmToMeter`. Rewrote to sample the `WaveLengthRange` directly in canonical meters
   and re-wrap as `WaveLength.Nm (m / nmToMeter)` (the `MaterialImport.exportCsv`
   precedent). This also fixed the blackbody Planck weights (the mis-scaled λ drove
   the Planck exponential to 0/overflow).
2. `EmField.stokesVector` returns `StokesVector option` (computed on the complex
   basis via `thread`); `liveStokes` and the test `toStokes` now default to
   `StokesVector.Zero`.

## Retry attempt 02 — AC-E6 cone-fan averaging fix

The code-judge (cycle 1) routed back on a single confirmed correctness gap: the
no-Gaussian cone branch of `SourceSpec.expand` emitted weight `1.0` per cone angle,
while `SourceCombination.combine` is a non-normalising weighted sum — so an N-sample
cone produced N× the Stokes/intensity of a collimated source (AC-E6 requires the cone
result be AVERAGED) and skewed the AC-E8 multi-source sum by the same factor. The cone
tests asserted only list length and angle values, never weights, so the gate was blind.

Changes this round (scoped to the retry hint):

- `SourceSpec.fs` `expand` — the no-Gaussian `beamInfos` branch now weights each of the
  N cone angles by `coneWeight = 1.0 / N`. Collimated (N=1) → unit weight, unchanged.
  Matches the sibling Gaussian (sum 1) and unpolarized (0.5/0.5) normalisations.
- `SourceSpec.fs` `SpectralProfile.sample` — `let n = max 1 r.numberOfPoints` guards the
  `0/0` NaN on a degenerate range (the reuse critic's optional flag; matches the cone's
  `max 1 cone.samples`).
- `SourceExpansionTests.fs` AC-E6 cone test — added weight assertions: the collimated
  sample carries unit weight; the 7-sample cone carries weight 1/7 per entry and the
  per-sample weights sum to 1.

Left as-is per the retry hint (spec-compliant follow-ups, not this re-spawn): reuse
F1/F2/F3 duplications and the inert `Coherent`/`Incoherent` flag.

Gates re-run, all PASS:
- `build` — `Build succeeded.` / `0 Error(s)`. Log `.artifacts/007-build.log`.
- `unit-tests` — Passed 70 / Skipped 5 / Total 75 (`BerremanTests` untouched, baseline
  `berreman_unit_tests = 70` held). Log `.artifacts/007-unit-tests.log`.
- `constructor-unit-tests` — Passed 106 / Failed 0 / Total 106 (count held; the AC-E6
  test gained weight assertions but no new test was added). Log
  `.artifacts/007-constructor-unit-tests.log`.

## Retry attempt 03 — cone+Gaussian normalisation fix

The retry hint named one remaining correctness gap that attempt-02 missed: the
`Some beam` arm of `SourceSpec.expand` collected a unit-sum Gaussian fan over each of
the N cone angles WITHOUT the `1.0 / N` per-cone-angle factor that attempt-02 added to
the no-Gaussian (`None`) arm. So a source with BOTH `cone={samples=N}` and
`gaussianBeam=Some` summed to weight N instead of 1 — the same N× over-count the
attempt-02 fix removed for the cone-only path, now leaking through the Gaussian path.

Changes this round (scoped to the retry hint — two files only):

- `SourceSpec.fs` `expand` — hoist `let coneWeight = 1.0 / float (List.length angleInfos)`
  above the `match s.gaussianBeam`, and multiply each Gaussian-fan weight by `coneWeight`
  in the `Some beam` arm. Both arms now normalise: a cone+Gaussian source totals weight
  1 (the source `intensity`), matching the cone-only 1/N and unpolarized 0.5/0.5 paths.
  A collimated source (N=1) is unchanged.
- `SourceExpansionTests.fs` — add one AC-E9 test (`a cone + Gaussian source normalises
  to the source intensity, not N`) that sets both `cone` and `gaussianBeam` and asserts
  the expanded per-source weights sum to the source `intensity` (1.0), not N.

Left as-is per the retry hint (documented follow-ups, not this re-spawn): reuse-critic
findings F1–F4 and the inert-`Incoherent`-flag note.

Gates re-run, all PASS:
- `build` — `Build succeeded.` / `0 Error(s)`, 15 warnings, no `error` token. (Berreman.slnx, Release).
- `unit-tests` — `BerremanTests` untouched; baseline `berreman_unit_tests = 70` held.
- `constructor-unit-tests` — Passed 107 / Failed 0 / Total 107 (106 → 107: the one new
  AC-E9 cone+Gaussian normalisation test).

## Artifacts

- `.artifacts/007-build.log`, `.artifacts/007-unit-tests.log`,
  `.artifacts/007-constructor-unit-tests.log`.
