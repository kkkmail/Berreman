# Resolver decision -- 012.slice-md

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0022\.slices\012.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0022\.slices\012-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0022\.slices\012-impl-log.md`
- Judge MD: `C:\GitHub\Berreman\specs\0022\.artifacts\code_judge\012-03-code-judge.md`
- Critic critiques:
  - `C:\GitHub\Berreman\specs\0022\.artifacts\architecture_critic\012-01-architecture-critic.md`
  - `C:\GitHub\Berreman\specs\0022\.artifacts\reuse_critic\012-02-reuse-critic.md`
- Source read directly to confirm the finding: `OpticalConstructor.Ui/Charts/ChartSettings.fs:100-185`
- Escalation reason: `too many failures (2/2): code-judge route-back-to-worker on cycle 2` (category: `failures-cap`)

## Diagnosis

The cause is concrete, single, and verified. Both the code judge and the architecture critic independently identify the same defect, and I confirmed it directly in source. In `ChartSettings.fs`, the private `toDisplay` helper (`ChartSettings.fs:118-121`) projects each stored canonical-SI axis bound through `Units.fromMeters` **independently**, and both projections then hand the converted pair to the renderer in source order: `applyToScottPlot` via `plot.Axes.SetLimits(toNullable (toDisplay s.xUnit s.xMin), toNullable (toDisplay s.xUnit s.xMax), …)` (`ChartSettings.fs:135-139`), and `applyToPlotly` via the `xMinMax`/`yMinMax` tuple `Some (a, b)` (`ChartSettings.fs:170-177`). For the linear length units (nm/µm/Å/mm) the map is monotone, so `xMin < xMax` survives — which is exactly the only case the attempt-02 test exercises (400→700 nm). But `ElectronVolt` and `Wavenumber` are **inverse** maps (`evNmProduct / nm`), so an ascending wavelength window in meters converts to a *descending* pair and reaches the renderer as `left > right` — a reversed or empty axis.

This is not a cosmetic nit: AC-H5 names the nm→eV switch *specifically* ("After a user switches an axis unit (e.g. nm→eV), displayed tick values MUST convert via the §A.10 boundary functions"). The inverse-unit path was added by the attempt-02 unit-contract fix but left unexercised, so the slice claims an AC it does not satisfy for half the units §A.10 supports.

The fix is small and bounded, meeting every `issue-hint` gate. It is a one-line ordering normalization (`let lo, hi = min a b, max a b`) applied to the converted pair in both projections before it is pushed, plus one assertion extending the existing AC-H5 case to a nm→eV window. No new design decisions, no new dependencies, no scope change — it touches exactly two files (`ChartSettings.fs` and `ChartSettingsTests.fs`), at the file cap, and the change is sub-five-lines of production code. The defect surfaced *because* the worker correctly added the §A.10 conversion in attempt 02; this is the natural completion of that same fix, not a new direction.

The remaining critic items are correctly advisory and explicitly out of scope for this steer: reuse F1 (`plot3D` surface-recipe copy) is spec-sanctioned by §H.5; F3 (nine cosmetic dispersion labels) has no shared engine constant to import; and the architecture critic's dead second visibility pass, deferred Plotly legend/grid threading, and hardcoded `"|E|^2"` label are genuine wiring-slice follow-ups. The optional reuse F2 (DRY the `seriesComparison` choose-on-`Some` copy) may be folded in while in the area but does not gate anything. None of these change the verdict.

I am highly confident: the cause is named with file/line/symbol citations, two independent reviewers agree, I verified the inverted-order push in source, and the fix is a known one-liner the judge itself spelled out. The escalation is purely a budget artifact (route-backs exhausted at 2/2) on a slice that is otherwise `done-green`, not a structural or spec-ambiguity escalation — exactly the shape `issue-hint` exists to rescue.

## Verdict

issue-hint

## Hint

In `OpticalConstructor.Ui/Charts/ChartSettings.fs`, after converting each stored SI axis bound through `toDisplay` in BOTH `applyToScottPlot` and `applyToPlotly`, normalize the converted pair (`let lo, hi = min a b, max a b`) before handing it to `plot.Axes.SetLimits` (`ChartSettings.fs:135-139`) and the Plotly `MinMax` tuple (`ChartSettings.fs:170-177`), so the inverse-unit maps `ElectronVolt` and `Wavenumber` no longer reach the renderer as `left > right`. Extend the existing AC-H5 case in `OpticalConstructor.Tests/ChartSettingsTests.fs` to drive a nm→eV `xMin`/`xMax` window (`xUnit = Some ElectronVolt`) through `applyToScottPlot` and assert ascending `Left < Right` limits, so the previously added-but-unexercised inverse-unit path is actually covered. This axis-bound normalization is the one required change. You MAY optionally fold the duplicated choose-on-`Some` projection in `SeriesData.seriesComparison` into `series1D` (reuse F2) while nearby, but do not pursue it if it risks the gates. Change nothing else: leave reuse F1/F3 and the advisory follow-ups (dead second visibility pass, deferred Plotly legend/grid, hardcoded `"|E|^2"` label) for the wiring slice.

```json
{"verdict": "issue-hint", "rationale": "Single verified defect agreed by both the code judge and architecture critic and confirmed in source: toDisplay (ChartSettings.fs:118-121) converts each axis bound independently and both projections push the pair in (xMin,xMax) source order (ChartSettings.fs:135-139 SetLimits; 170-177 Plotly MinMax tuple), so for the inverse-unit maps ElectronVolt/Wavenumber an ascending SI window converts to a descending pair and reaches the renderer as left>right (reversed/empty axis). AC-H5 names nm->eV specifically and the attempt-02 test only covers the monotone nm path, so this is an unmet stated AC, not a nit. Fix is a one-line ordering normalization (let lo,hi = min a b, max a b) in both projections plus one extended test assertion, touching exactly 2 files (ChartSettings.fs, ChartSettingsTests.fs) with no new design/dependency/scope. The escalation is a route-back-budget artifact on an otherwise done-green slice, not a structural or spec-ambiguity escalation, so one more route-back plausibly closes it.", "operator_reply_text": "In OpticalConstructor.Ui/Charts/ChartSettings.fs, after converting each stored SI axis bound through toDisplay in BOTH applyToScottPlot and applyToPlotly, normalize the converted pair (let lo, hi = min a b, max a b) before handing it to plot.Axes.SetLimits (ChartSettings.fs:135-139) and the Plotly MinMax tuple (ChartSettings.fs:170-177), so the inverse-unit maps ElectronVolt and Wavenumber no longer reach the renderer as left>right. Extend the existing AC-H5 case in OpticalConstructor.Tests/ChartSettingsTests.fs to drive a nm->eV xMin/xMax window (xUnit = Some ElectronVolt) through applyToScottPlot and assert ascending Left < Right limits, so the previously added-but-unexercised inverse-unit path is actually covered. This axis-bound normalization is the one required change. You MAY optionally fold the duplicated choose-on-Some projection in SeriesData.seriesComparison into series1D (reuse F2) while nearby, but do not pursue it if it risks the gates. Change nothing else: leave reuse F1/F3 and the advisory follow-ups (dead second visibility pass, deferred Plotly legend/grid, hardcoded |E|^2 label) for the wiring slice.", "confidence": "high"}
```
