# Step 006 — state of the world

## Where we are

Spec 0040 Part C, step 006 (IMPLEMENT). The Material editor's preview pane draws
three tabs — n/k always, plus Gyration when optically active and μ (Polder) when
magnetic. Step 006 restricts the Gyration and μ charts to the components that
are physically applicable to the entry (its symmetry class / μ kind), instead of
always drawing the full six gyration components and four μ components. Scope is
`OpticalConstructor.Ui` + `OpticalConstructor.Ui.Tests` only — the
class → component tables already live in Domain (`MaterialComplexityEditor`) and
are unchanged.

## What's working

- Restrict the gyration preview to the components the entry's symmetry class
  admits: `gyrationChart` takes a `GyrationComponent list` and emits one series
  per component (named by `gyrationComponentLabel`, read as Im[ρᵢⱼ] at each
  component's (i,j) slot).
- Restrict the μ preview by `MuKind`: a scalar μ draws one component, a
  gyromagnetic tensor the μ₁₁/μ₂₂/μ₃₃ diagonal plus the gyration magnitude.
- Thread the selector at the preview call site: an editable entry reads its own
  ladder (`m.editor.gyration` / `m.editor.muKind`); a view-only entry reads its
  stored `complexity` value tree; a coded preset with no tree keeps all
  components.
- Carry a view-only entry's stored `complexity` on the editor model
  (`Model.viewOnlyComplexity`) so the preview can classify it as data.
- Side the μ gyration series to the right axis by name, so a scalar-μ chart's
  single series stays on the left.

## Tests

Per the IMPLEMENT worker Invariant 6, gate execution belongs to the arc-runner's
deterministic gate engine after this session exits; this worker does not run
gates. The step's roster and how the change lands against each:

- `build` — F# across the solution; all `gyrationChart` / `muChart` call sites
  (one in `MaterialEditorView`, three in tests) updated to the new signatures.
- `unit-tests` (BerremanTests) — core solver, untouched; count unaffected.
- `constructor-unit-tests` (OpticalConstructor.Tests) — Domain/Storage/
  Optimization, untouched; count unaffected.
- `ui-smoke` — the "one frame per preview tab" render updated to the new
  signatures (2-series gyration, 4-series μ); count unchanged.
- `ui-tests` — `NkDispersionChartTests` now assert the restricted series
  (uniaxial-active g₁₁/g₃₃, all-six fallback, gyromagnetic four + right-axis g,
  scalar-μ one left-axis series) plus a direct `muKindOfComplexity` classifier
  test over derived scalar / gyromagnetic / dispersive-Polder trees (all three
  substantive branches); `MaterialEditorWindowTests` add four threading tests.
  Net-additive, so the count does not regress.

Acceptance mapping: uniaxial-active gyration → g₁₁/g₃₃
(`NkDispersionChartTests` + view-only Langasite in `MaterialEditorWindowTests`);
scalar-μ → single μ component (`NkDispersionChartTests`); coded preset with no
stored tree → all components (`MaterialEditorWindowTests` synthetic
complexity=None entry + `allGyrationComponents` chart test).

```yaml
gates:
  berreman_unit_tests: 0
  constructor_unit_tests: 0
  ui_smoke_tests: 0
  ui_tests: 0
```

## Architecture

- The gyration/μ restriction is a Ui concern: the class → component mapping is
  the existing Domain `gyrationComponents` / `gyrationComponentLabel`; the Ui
  only selects which selector to thread and reads those tables. No Domain, no
  engine, no tensor math changed.
- `gyrationChart` takes a `GyrationComponent list` (the resolved components)
  rather than a `GyrationClass<'g>`: the class is generic in its payload and the
  two view-only sources instantiate `'g` differently, so a component list is the
  single boundary representation. The reduction happens in the threading helper.
- View-only classification mirrors `NkDispersionChart.anisotropyOfEntry`: read
  the class / kind off the stored `complexity` value tree as DATA, never by
  evaluating the assembled engine tensor.

## Deferred

- No built-in entry is magnetic, so `muKindOfComplexity` (the view-only μ-kind
  classifier `previewMuKind` threads) is covered by a direct unit test over
  derived scalar / gyromagnetic / dispersive-Polder value trees — all three
  substantive branches — rather than by a shipping view-only magnetic entry. A
  seeded magnetic built-in would additionally exercise the `previewMuKind`
  view-only arm end-to-end (the μ analog of the Langasite gyration test); that
  end-to-end path is what remains deferred, not the classifier itself.
- The "coded preset with no stored tree" (`complexity = None`) is defensive:
  every seeded built-in carries `complexity = Some` since step 004, so the
  fallback is tested via a synthetic entry, not a shipping one.

## Gotchas

- `muStyle` previously hard-coded flipping series index 3 to the right axis. A
  scalar-μ chart has only one series (index 0), so `muStyle` now finds the
  gyration series by its shared name (`muGyrationSeriesName = "g"`) and flips
  only that — a harmless no-op for a scalar chart. (`setSeriesAxisSide` on an
  out-of-range index is already a no-op, but name-matching states the intent.)
- A view-only entry's stored `complexity` was previously discarded at `init`
  (only `presetProperties` was kept). It is now retained on
  `Model.viewOnlyComplexity` so the preview can restrict by class/kind; the
  `EditMaterial` init arm was split (`Some` vs `None`) to capture it. The
  reason strings are unchanged.
- Langasite is the load-bearing real-entry acceptance case: it opens view-only
  (its ε is an opaque `EpsAxisEvaluated` closure) yet its stored tree carries a
  `UniaxialActive` gyration class, so its gyration preview restricts to g₁₁/g₃₃
  from data — the value the window test asserts.

## Changelog

- 2026-07-13 — Step 006: restrict the Material editor's gyration/μ preview charts
  to the components applicable to the entry's symmetry class / μ kind; thread the
  selector from the editable ladder or the view-only stored complexity tree;
  carry the view-only complexity on the model; add restricted chart-builder and
  threading tests.
- 2026-07-13 — Step 006 (attempt 02): add a direct `muKindOfComplexity` classifier
  test in `NkDispersionChartTests` covering all three substantive branches
  (scalar / gyromagnetic / dispersive-Polder stored trees), closing the gap where
  the new public classifier had a caller (`previewMuKind`) but no test reached it;
  correct the Deferred note that claimed a `muKindOfComplexity unit path` which
  did not yet exist.
