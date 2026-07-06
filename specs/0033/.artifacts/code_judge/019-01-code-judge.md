# Code judge -- 019.slice-md cycle 1

## Inputs read

- Slice spec: C:\GitHub\Berreman\specs\0033\.slices\019.slice-md
- State-of-the-world: C:\GitHub\Berreman\specs\0033\.slices\019-state-of-the-world.md
- Impl-log: C:\GitHub\Berreman\specs\0033\.slices\019-impl-log.md
- Gate results: build pass / unit-tests pass / constructor-unit-tests pass / ui-smoke pass / ui-tests pass
- Critic critiques: (none this cycle)

## Rationale

All five roster gates pass and no critic filed a critique this cycle, so the
verdict turns on whether the diff meets the slice contract and whether the
worker's SoW/impl-log line up with it. I verified both directly against the
working tree.

**The REAL-MOVE is genuine.** `axisLabel` / `spectralRange` / `axisTicks` now
live in `OpticalConstructor.Domain.SpectralAxis` (new `SpectralAxis.fs`, compile
item placed right after `Units.fs`), the three definitions are deleted from
`Ui/MaterialPreview.fs` (no re-export shim, no duplication), and every caller is
re-pointed: `MaterialsView.dispersionPreview` now calls
`SpectralAxis.spectralRange`, and `MaterialPreviewTests.fs` gains the one-line
`open OpticalConstructor.Domain.SpectralAxis` (slice-017 precedent; test bodies
untouched, constructor-unit-tests stay at the 373 baseline). The moved code is
verbatim except for a doc-comment addition — a pure refactor of already-tested
code, so the existing AC-D7 cases keep covering it.

**The builder meets the acceptance, verified by headless model tests.**
`OpticalConstructor.TestWindows.NkDispersionChart.nkDispersionChart` is pure
(`OpticalPropertiesWithDisp -> UnitOfMeasure -> Range<WaveLength> ->
ExperimentChart`), TestWindows gains the required Analytics ProjectReference,
and the x-axis comes from the moved `SpectralAxis` helpers. The left/right
assignment ships as the paired `nkDispersionStyle` seed
(`defaultState |> setSeriesAxisSide kSeriesIndex RightAxis`) rather than inside
`ExperimentChart` itself — the only shape 018's architecture permits ("the side
is style, not data"; the slice depends on 18 and names the shared
`ExperimentChart` as the codomain), and exactly the host pattern 018's SoW
prescribed. The new `NkDispersionChartTests.fs` (+4 tests, ui-tests 259 → 263)
asserts the acceptance directly: two series n/k on one shared grid spanning the
requested 400…800 nm range, n → `LeftAxis` and k → `RightAxis` through the
seed, silicon yields a curve, vacuum yields flat n=1/k=0 through the SAME
builder, and the AC-D7 eV/nm rescale leaves the y data identical. Every piece
of new public surface (`nkDispersionChart`, `nkDispersionStyle`, the two series
indices) is exercised in the diff.

**The single §7 deviation is real, empirically pinned, and precedented.** The
slice named `calculateN11Re`/`calculateXi11Im` as the series source, but I
confirmed the defect the worker cites: `Analytics.getWaveLengthValue`
(Variables.fs:125-127) feeds the raw meter magnitude of the grid point into
`WaveLength.create`, which re-wraps it in the range's native unit
(Fields.fs `Nm _ -> fun e -> WaveLength.Nm (e * 1.0<nm>)`) — so every
`calculate*` builder evaluates dispersion at λ×10⁻⁹ and draws dispersive
entries flat, contradicting this step's own acceptance ("Dispersive entries
yield curves"). Two prior slices already documented the workaround in-repo
(`SourceSpec.SpectralProfile.sample`'s comment names `getWaveLengthValue`'s
mis-scale explicitly; `MaterialImport.exportCsv` at MaterialImport.fs:185-190
does the identical meter-grid + `Complex.Sqrt (getEps w)[0,0]` extraction the
new builder reuses). The worker's first advisory ui-tests run failing with
"silicon n span = 0" corroborates it empirically. Acceptance over named
mechanism is the right call here, the choice is recorded in the impl-log
Gotchas, the module header, and the SoW's Architecture §7-deviation entry, and
no dispersion formula was re-derived. The residual defect under the Ui's
`plot*Series` consumers is correctly scoped out (outside `touches`) and
recorded in Deferred as its own future slice.

SoW and impl-log line up with the diff in every particular I checked (file
list, test counts, fsproj edits, the out-of-touches one-line test open, which
the worker flagged rather than hid). Minor advisory note for the trail, not a
blocker: the axis-side acceptance is carried by the style seed rather than the
chart record — a later WIRE step should keep the pair together, as the Deferred
section already plans.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "All five gates pass and no critic critiques were filed. Verified against the working tree: the REAL-MOVE of axisLabel/spectralRange/axisTicks into OpticalConstructor.Domain.SpectralAxis is genuine (definitions deleted from MaterialPreview, callers re-pointed, no duplication); TestWindows gains the Analytics reference and a pure nkDispersionChart builder returning the shared ExperimentChart with n left / k right via the paired nkDispersionStyle seed on the 018 setSeriesAxisSide seam; 4 new headless model tests (ui-tests 259 -> 263) assert the acceptance shape (n->LeftAxis, k->RightAxis, requested 400-800 nm span), silicon curve, vacuum flat line, and the AC-D7 rescale, so all new public surface is exercised. The one deviation from the slice's named data builders (calculateN11Re/calculateXi11Im) is justified: getWaveLengthValue re-wraps the meter magnitude as a native-unit scalar so dispersion evaluates at lambda x 1e-9 and draws dispersive entries flat, contradicting the step's own acceptance; the builder follows the documented in-repo workaround (SourceSpec.SpectralProfile.sample, MaterialImport.exportCsv) via the engine getEps seam, and the choice is recorded in the impl-log, module header, and SoW.", "retry_hint": ""}
```
