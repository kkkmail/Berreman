# Code judge — 015.slice-md cycle 1

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0035\.slices\015.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0035\.slices\015-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0035\.slices\015-impl-log.md`
- Gate results: build pass / unit-tests pass / constructor-unit-tests pass / ui-smoke pass / ui-tests pass
- Critic critiques: (none — no project critics declared; none emitted this cycle)

## Rationale

Every gate is green and the diff is a faithful, minimal realization of the
slice contract. The engine gains the required third case
`EpsAxisDispersion.EpsAxisEvaluated of (WaveLength -> ComplexRefractionIndex)`
whose `complexIndex` applies the closure (`Dispersion.fs:318,324`), and because
a function-typed case removes structural equality type-wide, the type is
correctly marked `[<CustomEquality; NoComparison>]` with structural comparison
for `RealNK`/`ComplexEps` and by-reference comparison
(`LanguagePrimitives.PhysicalEquality`/`PhysicalHash`) for the evaluated case
(`Dispersion.fs:326-340`) — exactly as the slice prescribes to keep the
`DispersionModel`/`EpsWithDispValue`/`MaterialComplexity` derived equality the
round-trip tests rely on.

The lowering is now total. `toEpsAxis` returns a bare `EpsAxisDispersion` and
lowers all ten model cases, the four transcendental ones to
`EpsAxisEvaluated (evaluate model)` (`DispersionModels.fs:604,670`) — the same
closure route the engine's own `EpsWithDisp` case takes. The dead rejection
path is genuinely gone: `EpsAxisLoweringError`/`NotAFiniteTermSum` are deleted,
`toEpsValue`/`toOpticalProperties` collapse to a single route, and in the editor
`SegmentNotLowerable`, `lowerAxis`, and the `traverse` plumbing are removed so
`dispersiveEps`/`toComplexity` derive a dispersive segment for a transcendental
model. `toComplexity` keeps its `Result` shape (always `Ok`) mirroring the
documented `ofComplexity` precedent — a defensible, low-churn choice. A grep of
the whole tree confirms the three removed names survive only in doc comments
describing their removal, never in live code. The compile-driven consumers
outside the `touches` list (`Storage.sellmeierAxis`,
`TestWindows.editErrorReason`) were updated because the `build` gate compiles
the whole `.slnx`; this is the mechanical consequence of the mandated removals,
correctly recorded under Gotchas, not scope creep.

Test coverage meets the `done-green` bar for every piece of new public surface.
The four transcendental models each get a grid-match assertion that the lowered
`axis.complexIndex w` equals `evaluate model` within tolerance —
`Assert.Equal((f w).value, (axis.complexIndex w).value)` across `visibleGrid`
in both the TaucLorentz/Gaussian test (`DispersionModelsTests.fs:330-336`) and
the ForouhiBloomer/BrendelBormann test (`:543-549`). `toEpsValue`'s new total
outcome is exercised (`:378-380`), and the mandatory derive-and-evaluate test is
present: `MaterialComplexityTests.fs` "step 015: a transcendental dispersive
segment derives and evaluates without rejection" builds a ForouhiBloomer
dispersive segment through `toComplexity`, asserts the evaluated case, and
checks the derived eps reproduces `evaluate` across the grid — no
`SegmentNotLowerable`. The Ui.Tests migration converts the old
"surfaces the typed reason" pure and ui-smoke tests into "derives a dispersive
eps" tests. No tests were removed, so the `count_at_least` gates cannot regress,
consistent with all gates passing.

The SoW and impl-log line up with the diff on every point. One immaterial
note: the slice text attributed the `modelParameters` evaluated arm to
`MaterialComplexityEditor.fs`, but `modelParameters` lives in
`DispersionModels.fs`, where the `| EpsAxisEvaluated _ -> []` arm was correctly
added (`:369`); the build's exhaustiveness-as-error enforcement confirms every
match on the extended union is total. That defensive "no editable term scalars"
arm has no dedicated unit test, but it is a pure structural branch with no
externally-observable behavior of its own and is not called out by the slice's
testing plan — a note, not a defect. Nothing here rises to an unmet slice
requirement, a layering violation, or a coverage gap that would justify a
re-spawn.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "All five gates pass and the diff faithfully implements every slice requirement: EpsAxisDispersion gains the EpsAxisEvaluated closure case with correct [<CustomEquality; NoComparison>] (structural for RealNK/ComplexEps, by-reference for the evaluated case); toEpsAxis/toEpsValue/toOpticalProperties are total and lower the four transcendental models via evaluate; EpsAxisLoweringError, NotAFiniteTermSum, and SegmentNotLowerable are fully removed (grep confirms only doc-comment mentions remain) and the editor's lowerAxis/traverse plumbing is gone so toComplexity derives a dispersive segment. New public surface is covered: each of the four models has a grid assertion that complexIndex equals evaluate within tolerance, plus the required MaterialComplexity derive-and-evaluate test and the migrated Ui.Tests. SoW and impl-log match the diff; the only nit is that modelParameters (with its defensive EpsAxisEvaluated -> [] arm) lives in DispersionModels.fs rather than the file the slice text named, which is immaterial given the compiler's exhaustiveness-as-error enforcement.", "retry_hint": ""}
```
