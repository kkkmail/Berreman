# Code judge -- 025.slice-md cycle 1

## Inputs read

- Slice spec: C:\GitHub\Berreman\specs\0033\.slices\025.slice-md
- State-of-the-world: C:\GitHub\Berreman\specs\0033\.slices\025-state-of-the-world.md
- Impl-log: C:\GitHub\Berreman\specs\0033\.slices\025-impl-log.md
- Gate results: build pass / unit-tests pass / constructor-unit-tests pass / ui-smoke pass / ui-tests pass
- Critic critiques: (none supplied this cycle)

## Rationale

All five gates pass, and the task file supplies no critic critiques for this
cycle, so the verdict turns on whether the diff meets the slice-spec contract
and whether the worker's SoW/impl-log line up with what is actually on disk. I
verified both directly against `git diff HEAD`.

The slice's binding requirements are all met in the diff. (1) `ImportError`
gains the NEW typed case `UnsupportedFormula of formulaNumber : int * reason :
string` (MaterialImport.fs), exactly the "NEW typed unsupported-formula case on
ImportError" the spec demands, and it is gated in `importRefractiveIndexInfo`
BEFORE coefficient parsing, so a formula-8/9 page without coefficients still
returns the typed error rather than degrading to `MalformedYaml`. (2) Formulas
2-7 are each parsed and lowered: 2 rides the catalogue `Sellmeier`
`DispersionModel` via `toEpsAxis` (the spec's "through a DispersionModel"
ramp), while 3-7 build `EpsAxisDispersion` term data directly (`ComplexEps`
for the eps-shaped 3/4, `RealNK` with a zero k formula for the n-shaped 5/6/7
-- the spec's "or SumOfTerms" ramp). (3) `entryOfAxis` carries the axis in an
`EpsWithDispValue`-backed `MaterialComplexity` with `complexity = Some ...`
and `properties = complexity.toProperties`, making formula imports editable as
required. (4) The micrometre convention flows only through the `Units` seam:
`micrometerScale` is `toMeters Micrometer 1.0` and `rangeInterval` uses
`toWaveLength Micrometer`; the only new numeric literal is Herzberger's
published 0.028 um^2 pole, a formula constant, not a conversion factor.

The testing requirement is satisfied to the letter. Nine new fixture files
(`formula{1..9}.yml`) are registered as `Content` in the test fsproj, and
MaterialImportTests.fs adds nine facts: one per supported formula (1-7)
pinning the imported entry's evaluated index through the engine `getEps` path
against the published formula computed inline at two sample wavelengths
(tolerance 1e-9 via the existing `closeC` helper), each also asserting the
editable `EpsWithDispValue` complexity shape; one fact pinning the parsed
`wavelength_range` on the formula-2 segment; and one fact asserting formulas 8
AND 9 return `Error (UnsupportedFormula (8|9, _))`. That covers both
acceptance criteria verbatim, and the constructor-unit-tests count moved 407
-> 416, matching the claimed +9 with no regression elsewhere (unit-tests 119,
ui-smoke 81, ui-tests 306, all at baseline -- consistent with a diff that
touches no core or UI code). All new public surface is exercised by tests in
the diff.

The one judgment call -- unifying formula 1 onto the same data route instead
of keeping its closure-backed path -- is documented in the impl-log's
Decisions and is the defensible reading: the old closure carried a "view-only
until Part G lowers them to data" comment, the slice's stated purpose is "so
imports are editable", and a dedicated formula-1 fact pins that the evaluated
index is preserved. The tabulated/CSV paths are untouched, as the spec's
scope implies. The SoW and impl-log match the diff file-for-file and
claim-for-claim, including the honest disclosure that attempt 01's console
was killed after the round completed but before the exit JSON; the
re-dispatched session verified rather than re-implemented, and the gate
results in the task file are the supervisor's authoritative reruns anyway.

Minor notes, none verdict-affecting: the worker flags the unreachable
`NotAFiniteTermSum -> MalformedYaml` branch in `sellmeierAxis` (kept for match
totality -- fine), and `rangeInterval` also accepts a bare `range:` key
(harmless leniency at the IO boundary). Nothing here warrants a re-spawn.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "All five gates pass and no critic critiques were supplied this cycle. Verified against git diff HEAD: the slice contract is fully met -- formulas 2-7 parsed and lowered (1/2 via the catalogue Sellmeier DispersionModel toEpsAxis, 3-7 as direct EpsAxisDispersion term data: ComplexEps for 3/4, RealNK with zero k for 5/6/7) into an EpsWithDispValue-backed MaterialComplexity so imports are editable; the NEW typed UnsupportedFormula (formulaNumber, reason) ImportError case covers formulas 8/9 and is gated before coefficient parsing; micrometre wavelengths flow only through the Units seam (toMeters/toWaveLength Micrometer). Both acceptance criteria are pinned by tests: one fixture-driven fact per formula 1-7 asserting the evaluated index against hand-computed published-formula values at two wavelengths (1e-9 tolerance), plus a fact asserting formulas 8 and 9 return the typed error; constructor-unit-tests 407 -> 416 (+9), all other suites at baseline. SoW and impl-log match the diff exactly; the formula-1 unification onto the data route is a documented, test-pinned judgment call consistent with the slice's editability goal.", "retry_hint": ""}
```
