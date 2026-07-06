# Code judge -- 007.slice-md cycle 1

## Inputs read

- Slice spec: C:\GitHub\Berreman\specs\0033\.slices\007.slice-md
- State-of-the-world: C:\GitHub\Berreman\specs\0033\.slices\007-state-of-the-world.md
- Impl-log: C:\GitHub\Berreman\specs\0033\.slices\007-impl-log.md
- Gate results: build pass / unit-tests pass / constructor-unit-tests pass / ui-smoke pass / ui-tests pass
- Critic critiques: (none this cycle)

## Rationale

All five gates pass and no critic ran this cycle, so the verdict turns on two
things I verified directly against the working tree: that the diff delivers
every element the slice spec names, and that the new public surface is
exercised by tests in the diff.

The slice spec requires four formula blocks appended to
`Berreman/Berreman/Dispersion.fs` with the engine unions untouched. The diff is
93 insertions / 0 deletions in that file (plus two `open`s at the top —
`System.Numerics` for `Complex`, `Constants` for the `meter` measure), exactly
as the impl-log declares: `WaveLengthInterval` carries two elevated
`WaveLength` endpoints with no validation (per §0.4); `DispersionTerm` has the
pinned `lambda : double / coefficients : double array / power : int /
multiplier : double` shape and evaluates `multiplier * (Σ coefficients.[k] *
(x − lambda)^k) ^ power` — I checked the `Array.foldBack (fun c acc -> acc * d + c)`
fold and it is a correct Horner evaluation of Σ c_k·d^k, with `pown` giving the
negative-power Laurent/inverse shapes; `DispersionFormula.evaluate` reduces the
canonical wavelength via `w.value / (wavelengthScale * 1.0<meter>)`
(`WaveLength.value` at `Fields.fs:284` returns metres, so `wavelengthScale` is
metres-per-coefficient-unit as specified) and sums the terms; the
`ComplexDispersionTerm` / `ComplexDispersionFormula` mirrors are field-for-field
with a private `complexPown` (empty-range fold correctly yields `Complex.One`
for n = 0, inversion on negative n keeps poles as poles). Everything sits after
the pre-existing engine unions, which the diff does not touch.

The acceptance criterion — `DispersionFormula.evaluate` reproduces
hand-computed polynomial and inverse-polynomial values within the project
tolerance in BerremanTests — is met directly. The new
`DispersionFormulaTests.fs` (5 facts, unit-tests 84 → 89 matching the SoW's
gate YAML) pins: a Cauchy-shaped Laurent `A + B/x² + C/x⁴` at 500 nm against
µm coefficients (also pinning the reduction direction, x = 0.5); a
Sellmeier-shaped inverse via the exact partial fraction
`(1 + B) + B·C·(x² − C)^(−1)` compared against the direct rational form (I
verified the algebra — the decomposition is exact, so the shared tolerance is
not absorbing an approximation); a shifted-centre squared term pinning the
`(x − lambda)` centring and a positive outer power; the complex Lorentz pole
`1 + S/(x₀² − x² − iγx)` (coefficient vector `[x₀²; −iγ; −1]` is the correct
polynomial form); and the interval's elevated endpoints. All comparisons use
`MatrixComparison.allowedDiff` — the `MuellerMatrixTests.fs:24` /
`OptimizationTests.fs:260` precedent — with no new epsilon logic, exactly as
the slice's testing plan demands. Every piece of new public surface
(`DispersionFormula.evaluate`, `ComplexDispersionFormula.evaluate`,
`WaveLengthInterval`; the term-level `evaluate`s are exercised through the
formulas) is covered by a test in the diff.

The SoW and impl-log line up with the diff in every claim I spot-checked: the
93/0 insertion count, the one-line fsproj `<Compile>` entry, the five facts,
the `List.fold … Complex.Zero` in place of `sumBy`, and the `.manifest.state.json`
modification being the arc-runner's own file. The recorded interpretation on
elevation — the slice explicitly pins raw-`double` term fields as dimensionless
model coefficients whose unit is defined by `wavelengthScale`, with `WaveLength`
staying elevated at the API — is defensible and documented in the impl-log's
Decisions/Gotchas as the project prompt requires. Style conforms to CLAUDE.md
(braces on own lines, `name : Type` spacing, camelCase fields, immutable
evaluation).

Minor advisory notes, none blocking: `DispersionTerm.evaluate` takes the raw
reduced `double` — acceptable as an intra-module seam since only the owning
formula owns the unit, per the slice's own design; and the untracked `.claude/`
folder in `git status` is ambient harness configuration, not part of the
worker's diff or claims. Nothing here approaches route-back grounds.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "All five gates pass, no critic findings this cycle. Verified against the working tree: the diff adds exactly the four formula blocks the slice spec names (WaveLengthInterval, DispersionTerm, DispersionFormula, and the Complex mirrors) as a pure 93/0 addition to Dispersion.fs with the engine unions untouched; the Horner fold, pown/complexPown power handling, and metres-per-coefficient-unit reduction are mathematically correct. The acceptance criterion is met: 5 new BerremanTests facts (84 -> 89) pin Cauchy-Laurent, Sellmeier partial-fraction (exact algebra), shifted-centre power, the complex Lorentz pole, and the interval endpoints against hand-computed values using MatrixComparison.allowedDiff with no new epsilon logic. SoW and impl-log match the diff in every spot-checked claim; all new public surface is exercised by tests in the diff.", "retry_hint": ""}
```
