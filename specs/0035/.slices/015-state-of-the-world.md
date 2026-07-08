# Step 015 — state of the world

## Where we are

Step 015 of spec 0035 makes the four transcendental dispersion models
(Tauc–Lorentz, Gaussian oscillator, Forouhi–Bloomer, Brendel–Bormann) first-class
inside a dispersive segment. Before this slice they were an honest negative:
`DispersionModels.toEpsAxis` returned `Error (NotAFiniteTermSum _)` for them, a
dispersive segment carrying one failed `MaterialComplexityEditor.toComplexity` with
`SegmentNotLowerable`, and the editor could not derive/chart/save it. This slice adds
an **evaluated-directly** third case `EpsAxisEvaluated` to `EpsAxisDispersion` (the
engine's own `WaveLength -> ComplexRefractionIndex` shape), lowers the four models to
it, and deletes the whole rejection path — `toEpsAxis` is now TOTAL. It touches the
engine (`Berreman/Dispersion.fs`), the Domain (`DispersionModels.fs`,
`MaterialComplexityEditor.fs`), and — because the `build` gate compiles the whole
solution — the compile-driven consumers in `OpticalConstructor.Storage`,
`OpticalConstructor.TestWindows`, and the three test projects.

## What's working

- Add `EpsAxisDispersion.EpsAxisEvaluated`, an evaluated-directly closure case whose
  `complexIndex` applies it, with custom equality (structural for `RealNK`/`ComplexEps`,
  by-reference for the evaluated case) so `DispersionModel`/`EpsWithDispValue`/`MaterialComplexity`
  keep the equality the round-trip tests use.
- Make `toEpsAxis` total: the four transcendental models lower to `EpsAxisEvaluated (evaluate model)`.
- Remove `EpsAxisLoweringError` / `NotAFiniteTermSum` and `SegmentNotLowerable`; `toEpsValue`,
  `toOpticalProperties`, and the editor's `dispersiveEps` become total-lowering, and
  `toComplexity` derives a dispersive segment for a transcendental model.
- Give `modelParameters` an evaluated-case arm returning no editable term scalars.
- Update the compile-driven consumers (Storage `sellmeierAxis`, TestWindows `editErrorReason`)
  and migrate/rewrite the tests to the total outcome; add a `toComplexity` derive-and-evaluate test.

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this worker
  exits (IMPLEMENT Invariant 6 — the worker acts, it runs no checks). This session ran
  no gate commands.
- Step 015 roster: `build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`,
  `ui-tests`. `build` covers the engine case + custom equality and the total
  `toEpsAxis` / `toEpsValue` / `toComplexity` surface across every project, including
  the Storage / TestWindows consumers of the removed error cases.
- `constructor-unit-tests`: the four transcendental grid-match tests (each model's
  lowered `complexIndex` equals `evaluate` across the sampled grid) and the new
  `MaterialComplexityTests` test (a transcendental dispersive segment derives through
  `toComplexity` and evaluates without `SegmentNotLowerable`).
- `ui-tests` / `ui-smoke`: the editor-driven pick of ForouhiBloomer / BrendelBormann
  now derives a dispersive eps (the evaluated segment) instead of surfacing a rejection.
- `unit-tests` (Berreman): `EpsWithDispValueTests` only constructs `RealNK`/`ComplexEps`
  (no exhaustive match), so it is behaviourally untouched by the new case.
- No tests were removed (rewrites are in place; one new test added), so no
  `count_at_least` regression.

## Architecture

- **The transcendental route is the evaluated segment, not a special error.** A
  transcendental model lowers to `EpsAxisEvaluated (evaluate model)` — the SAME
  closure the engine's `EpsWithDisp` case applies — so it rides the exact same
  serializable segment tree the finite-term models do. `toOpticalProperties` therefore
  has one route, and a dispersive segment carrying a transcendental model derives,
  charts, and evaluates identically to `evaluate`.
- **Custom equality preserves the derived-value contract.** A function-typed union
  case removes F# structural equality type-wide, so `EpsAxisDispersion` is
  `[<CustomEquality; NoComparison>]`: `RealNK`/`ComplexEps` compare structurally
  (so the finite-term round-trips stay value-identical) and the evaluated case
  compares by closure reference. `DispersionModel`/`EpsWithDispValue`/`MaterialComplexity`
  auto-derive equality that delegates to this, keeping `Assert.Equal(model, back)` and
  `Assert.Equal(axis, toEpsAxis model)` green.
- **`toComplexity` keeps a `Result` return that always succeeds.** Every derivation
  error is gone, so `toComplexity` can no longer fail; keeping the `Result` shape
  (mirroring `ofComplexity`'s documented "kept for the consumer contract") avoids
  churning the view/test call sites.

## Deferred

- **Serialising the evaluated case.** `EpsAxisEvaluated` carries a closure and cannot
  be JSON-round-tripped as-is, so persisting a transcendental `MaterialComplexity`
  through the Storage DTO path is a later-slice concern. No gated test exercises it
  (built-ins are finite-term; the DTO path is untouched this round).

## Gotchas

- **Blast radius beyond the `touches` list.** `touches` is
  `[Berreman, BerremanTests, OpticalConstructor.Domain, OpticalConstructor.Tests]`, but
  removing `NotAFiniteTermSum` / `SegmentNotLowerable` and totalising `toEpsAxis` breaks
  compilation in `OpticalConstructor.Storage` (`sellmeierAxis`),
  `OpticalConstructor.TestWindows` (`editErrorReason`) and `OpticalConstructor.Ui.Tests`
  (three tests). The `build` gate compiles the whole `.slnx`, so those were updated —
  the mechanical consequence of the mandated removals, not new scope.
- **`complexitySummary`'s `[%08x…]` digest is reference-based for a transcendental
  derived complexity** (the evaluated case hashes by closure identity), so it varies
  per derivation. It is display-only and never feeds back into the model, so it does
  not spin a render loop; the ui-smoke test asserts on "dispersive" / "not derivable".

## Changelog

- 2026-07-08 — Step 015 (IMPLEMENT): added `EpsAxisDispersion.EpsAxisEvaluated`
  (evaluated-directly closure case) with custom equality; made `toEpsAxis` /
  `toEpsValue` / `toOpticalProperties` and the editor's `dispersiveEps` / `toComplexity`
  total by lowering the four transcendental models to the evaluated case; deleted
  `EpsAxisLoweringError` / `NotAFiniteTermSum` / `SegmentNotLowerable`; added the
  `modelParameters` evaluated arm; updated the Storage / TestWindows consumers and
  migrated/rewrote the tests to the total outcome, adding a `toComplexity`
  derive-and-evaluate test for a transcendental dispersive segment.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 447
  ui_smoke_tests: 98
  ui_tests: 329
```
