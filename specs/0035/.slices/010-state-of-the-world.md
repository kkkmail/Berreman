# State of the world — slice 010 (IMPLEMENT — dispersive gyration ρ and Polder μ editing)

## Where we are

Slice 010 is the Part C IMPLEMENT step: it lifts the func-valued gyration ρ and
Polder μ cases into the pure `MaterialComplexityEditor` edit model so a dispersive
active/magnetic entry is first-class and editable, not view-only. It extends the
optically-active and magnetic rungs to a Constant vs Dispersive sub-branch (the eps
ladder pattern), derives `RhoWithDispValue` / `MuWithDispValue` through `toComplexity`,
seeds them back through `ofComplexity`, and DELETES the `UnsupportedComplexity`
view-only fallback (§0.2). It touches only `OpticalConstructor.Domain` and
`OpticalConstructor.Tests` (plus one forced 1-line edit in `OpticalConstructor.TestWindows`
to drop the deleted error case from a match) — matching the slice `touches`. The engine
builders `RhoWithDispValue.toRhoWithDisp` (Active.fs) and `MuWithDispValue.toMuWithDisp`
(Dispersion.fs) are UNCHANGED.

## What's working

- Extend the activity and magnetic rungs to a Constant vs Dispersive sub-branch: a new
  `ComponentDispersion` DU plus per-component `DispersionFormula` facets
  (`gyrationDispersion`, `polderDispersion`) stored INDEPENDENTLY of the constant facets.
- Derive a dispersive gyration through `toComplexity` into `RhoWithDispValue` whose
  per-wavelength `toRhoWithDisp` assembly equals the engine class builders.
- Derive a dispersive Polder μ through `toComplexity` into `MuWithDispValue` whose
  per-wavelength `toMuWithDisp` assembly equals the engine Polder tensor.
- Seed a dispersive ρ / μ back through `ofComplexity`, round-tripping value-identically
  by sampled tensor value.
- Restore the constant losslessly when the Dispersive sub-toggle is unchecked, and the
  edited formulas when it is re-checked.
- Delete the `UnsupportedComplexity` view-only case; dispersive entries now open editable.

## Tests

Per the IMPLEMENT worker role (Invariant 6 — the worker acts and runs no checks), the
`build` / `unit-tests` / `constructor-unit-tests` / `ui-smoke` / `ui-tests` gates are
executed by the arc-runner's deterministic gate engine AFTER this worker exits; they were
NOT run here as authoritative gates. The counts below were observed locally as due
diligence (advisory only).

- NEW in `MaterialComplexityTests` (`constructor-unit-tests`), 4 tests: `toComplexity` of
  an on-Dispersive gyration builds `RhoWithDispValue` matching the engine `toRhoWithDisp`
  over a wavelength grid; the Polder analogue against `toMuWithDisp`; `ofComplexity` then
  `toComplexity` round-trips a dispersive ρ and μ by sampled tensor; unchecking the new
  Dispersive sub-toggle restores the constant ρ / μ losslessly. `constructor_unit_tests`
  440 → 444.
- `MaterialEditorWindowTests` (`ui-tests`) unchanged and green (31): the constant built-in
  round-trip and the eps lossless-uncheck tests still pass; the `UnsupportedComplexity`
  deletion required no test edit there. `ui_tests` 327 (held).
- `ui-smoke` 93 (held); `berreman_unit_tests` 119 (held — no core file changed).
- Build: 0 errors, no warning from our code in the three changed files.

## Architecture

- **Parallel dispersive facets, not a combined per-component edit record.** The CONSTANT
  fields (`gyration : GyrationClass<RhoValue>`, `polder : PolderValue<MuValue>`) are left
  exactly as they were, so the `MaterialEditorView` constant panels and every existing
  test compile and pass verbatim. The DISPERSIVE facets
  (`gyrationDispersion : GyrationClass<DispersionFormula>`,
  `polderDispersion : PolderValue<DispersionFormula>`) live alongside, feeding the engine's
  `RhoWithDispValue` / `MuWithDispValue` DIRECTLY (those cases carry `GyrationClass<DispersionFormula>`
  / `PolderValue<DispersionFormula>` — zero lossy conversion, exact round-trip).
- **A component's dispersion IS a `DispersionFormula`** (spec §C.0), the raw
  `SumOfTerms (RealNK …)` payload the 0033 `modelParameters` surface edits — the honest,
  total representation of a real-scalar dispersion, where a general
  `DispersionModel -> DispersionFormula` would be impossible for the transcendental cases.
- **Lossless by construction, class-synced.** Each rung stores its constant and dispersive
  facets independently; `toComplexity` reads one under its sub-toggle. `syncGyrationDispersion`
  keeps `gyrationDispersion` on the same symmetry class as `gyration` at the 3 mutation
  points, so the single class picker (driven by `gyration`) stays valid for both facets.
- **Generic gyration helpers.** `gyrationComponents` / `setGyrationComponent` are now
  `GyrationClass<'g>` so the constant (`RhoValue`) and dispersive (`DispersionFormula`)
  facets share one implementation.

## Deferred

- **The dispersive editing UI** (the sub-toggle controls, the per-component raw
  `SumOfTerms` / `modelParameters` boxes, their stable ids and headless proofs) is a
  future Ui slice — `OpticalConstructor.Ui` / `.TestWindows` are outside this slice's
  `touches`. The Domain model, messages, and derivation are complete and unit-proven so
  that wiring is a thin projection. Nothing from THIS slice's own scope is deferred.

## Gotchas

- **`UnsupportedComplexity` is gone (§0.2).** Any future match on
  `MaterialComplexityEditError` sees only `SegmentNotLowerable` / `NoSuchSegment` /
  `LastSegmentNotRemovable`. The one consumer (`MaterialEditorView.editErrorReason`) was
  updated in place; a dispersive ρ / μ entry now opens EDITABLE, not view-only.
- **Dispersive μ is always the full Polder tensor** — the engine's `MuWithDispValue` has
  no scalar dispersive case; `muKind` gates only the constant magnetic branch. The
  magnetization axis is one physical choice, so `ChooseGyrationAxis` sets it on both facets.
- **Class-sync invariant** between `gyration` and `gyrationDispersion` is load-bearing:
  a bug that let them diverge would make `toComplexity` build a dispersive tensor of a
  class the picker does not show. It is localised to `syncGyrationDispersion` and pinned by
  the round-trip test.
- **Future Ui note:** the dispersive component number boxes must commit on `LostFocus`
  (not auto-commit) to avoid the FuncUI render-loop hang, matching the existing coefficient
  boxes.
- **MSB3277 (WindowsBase) is pre-existing** and not owned by this slice (no project
  reference changed).

## Changelog

- 2026-07-08 — Part C: extended the optically-active and magnetic rungs of
  `MaterialComplexityEditor` to a Constant vs Dispersive sub-branch. Added the
  `ComponentDispersion` DU and the independent dispersive facets `activityDispersion` /
  `gyrationDispersion` / `magneticDispersion` / `polderDispersion` (+ their `defaultState`
  seeds, the class-sync helper, and 6 messages); generalised `gyrationComponents` /
  `setGyrationComponent` to `GyrationClass<'g>`. `toComplexity` now builds
  `RhoWithDispValue` / `MuWithDispValue` under the sub-toggle and `ofComplexity` seeds
  from a dispersive ρ / μ verbatim; the `UnsupportedComplexity` view-only case is DELETED
  (one forced edit to `MaterialEditorView.editErrorReason`). Added 4 Part-C tests to
  `MaterialComplexityTests`.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 444
  ui_smoke_tests: 93
  ui_tests: 327
```
