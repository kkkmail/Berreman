# Architecture critique -- 012.slice-md cycle 3

## Summary

Attempt 03 closes cycle 2's one load-bearing finding cleanly: the eV/cm⁻¹
range-override inversion is fixed by `normalizePair` (`ChartSettings.fs:130-133`),
applied in *both* projections before the converted pair reaches the renderer
(`ChartSettings.fs:149-151,184-191`), and the previously-dead inverse-unit path now
has a dedicated AC-H5 test asserting ascending `Left < Right`
(`ChartSettingsTests.fs:97-124`). The `SeriesData` choose-on-`Some` duplication was
also folded into one `projectPoints` helper (`SeriesData.fs:43-46`). The slice
remains well-layered and reuse is genuine. No correctness defect survives; the
remaining items are advisories the code judge can weigh against the green gates.

## Separation of concerns

The double trace-visibility mechanism flagged in cycles 1–2 still stands.
`Plot1DView.render` skips hidden traces at add-time (`Plot1DView.fs:31-32`) *and*
`applyToScottPlot` re-flips `Scatter.IsVisible` by legend-name match
(`ChartSettings.fs:166-173`). On the view path the second pass is dead (the hidden
scatter was never added), so the omission rule is expressed twice and the two
predicates could diverge on a legend-name collision. Harmless today, advisory only —
either let the view add every trace and rely solely on the projection, or note that
the projection loop exists only for the externally-populated `ChartSettingsTests`
path.

## Consistency

The Plotly half still does not honour the §H.1 "sole projection" invariant the way
the ScottPlot half does, and this did not move in attempt 03. `applyToPlotly`
applies axis ranges, `AxisType`, and title but silently drops `showLegend`/`showGrid`
(`ChartSettings.fs:179-198`, acknowledged in `Deferred`); and the surface/Poincaré
views thread no settings at all — `Plot3DView.surfaceChart`/`render`
(`Plot3DView.fs:20,29`) and `PolarizationPlots.poincareSphere`
(`PolarizationPlots.fs:57`) accept no `ChartSettings` and never call `applyToPlotly`.
So AC-H4's "the change MUST appear via the H.1 projection" is demonstrated only on
the ScottPlot path and the isolated `applyToPlotly` unit test. Given live WebView2
wiring is explicitly deferred, this is acceptable for *this* slice, but the wiring
slice must thread settings through the Plotly views or the §H.1 invariant won't hold
on the Plotly half.

## Spec fit

Coverage is faithful: all eleven requirements land in the named files, the seven
modules are registered in build order, the `Analytics` ProjectReference is the
correct layering edge, and overlay/surface/dispersion all reuse the engine bodies
(`plotComparison` layout and `fullName + " (" + i + ")"`, `SeriesData.fs:60-64`;
the `kk:20180922` swap + `Contours.initXyz(Show = true)`, `Plot3DView.fs:22`; the
exact `calculateN11Re`…`calculateRho33Im` builders, `SeriesData.fs:103-111`). No
`Chart.show`, no OpenTK, no clone.

One fresh note for the judge: **the §F.8 `{ x; y; rgb }` contract Part H is told to
consume does not exist in Part F.** R-10 and the non-requirements say Part H
"consumes the resulting `{ x : float; y : float; rgb : byte*byte*byte }`" owned by
§F.8 and must NOT implement "the XYZ→xy and XYZ→sRGB numbers." But
`Analytics/Colorimetry.fs` exposes `spectrumToXyz`/`xyzToLab`/`xyzToSrgb` returning
`float*float*float` (sRGB in [0,1]) — there is **no** XYZ→xy chromaticity function
and **no** byte-triple producer anywhere. `CieView.ChromaticityPoint`
(`CieView.fs:15-20`) therefore *defines* that shape locally rather than consuming it.
The view staying a pure consumer is the right call; but the adapter that builds a
`ChromaticityPoint` from §F.8's outputs is unbuilt, and the `X/(X+Y+Z)` xy
projection the spec assigned to §F.8 has no home. Not a Part-H defect, but the slice
leaves the CIE wiring contract only half-specified.

## Evolvability

`SeriesData.fieldDepthSeries` still hardcodes the legend name `"|E|^2"`
(`SeriesData.fs:93`), though R-9 admits |E|², Poynting, *or* absorption profiles.
Once §F.6 grows the other variants every field-depth trace is mislabeled. A
`name : string` parameter — exactly as the private `dispersion` helper just below
already takes — future-proofs it at near-zero cost. Advisory; the wiring slice can
absorb it.

The CIE gap above is the one place a future slice is mildly cornered: the Part I/J
wiring will have to compute xy from XYZ and byte-scale the [0,1] sRGB itself, while
*not* re-implementing the gamma/matrix §F.8 owns. Recording in the impl-log that
`CieView` deliberately defines (not imports) the `{x;y;rgb}` shape would save that
slice a re-litigation.

## Risks

- **Sweep-range / null-limit interaction on the view path stays untested.**
  `Plot1DView.render` sets the sweep range only when *both* overrides are `None`
  (`Plot1DView.fs:46-48`), then `applyToScottPlot` immediately calls
  `SetLimits(toNullable …)` (`ChartSettings.fs:151`). Correct behaviour relies on
  ScottPlot treating a `Nullable()` bound as "keep current"; if it instead resets to
  auto-scale, the just-set sweep range is wiped. The new attempt-03 test exercises
  the *override* path (both bounds `Some`), not this sweep-default-survives path, so
  the keep-current assumption is still unverified. One assertion on `plot.Axes`
  limits after `renderSweep` with no override would lock the load-bearing default
  path down. Low severity.
- **`seriesComparison` / `fieldDepthSeries` have no direct test.** SeriesDataTests
  cover `series1D` and `surfaceFromData`; the §H.4 comparison `(index)` naming and
  the §H.9 `Units.fromMeters` depth conversion are exercised only indirectly. This
  matches the stated testing plan (AC-H2/H8 at the adapter boundary), so it is an
  observation, not a gate miss — but both contain logic that could regress silently.
- **Poincaré "sphere" is an O(n²) `Point3D` cloud, not a `Chart.Surface`**
  (`PolarizationPlots.fs:57-72`). Cosmetic; a large caller `n` would be costly. Low.

## Bottom line

I lean ship. The cycle-2 correctness defect (eV/cm⁻¹ bound inversion) is genuinely
fixed, the fix is structurally sound and now test-covered, and the reuse folding is a
real simplification. The remaining findings are all advisory: the deferred Plotly
settings threading and the §F.8 `{x;y;rgb}` contract gap are documented and fall
inside the §0#6 minimum-implementation rule, and neither corners the codebase so long
as the CIE-wiring expectation is carried forward to the next slice; the dead second
visibility pass, the hardcoded `"|E|^2"` label, and the two untested paths are
low-severity polish. I have no gate authority — my read is that a hold is
unnecessary; ship with the CIE-contract and sweep-default-path notes carried forward.
