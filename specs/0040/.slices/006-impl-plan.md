# Step 006 — impl-plan

## Goal

Restrict the Material editor's gyration and Polder-μ preview charts (Optical
Constructor, spec 0040 Part C) to the components that are physically applicable
to the entry, instead of always drawing the full six gyration components / four
μ components. Currently `gyrationChart` always emits g₁₁…g₂₃ and `muChart`
always emits μ₁₁/μ₂₂/μ₃₃ + g regardless of the entry's symmetry class / μ kind.

## Approach

`touches: [OpticalConstructor.Ui, OpticalConstructor.Ui.Tests]` — no Domain
changes. The gyration-class → component mapping already lives in Domain
(`MaterialComplexityEditor.gyrationComponents` / `gyrationComponentLabel` /
`GyrationComponent`), so the Ui only threads the right selector down and reads
those tables.

### `OpticalConstructor.Ui/NkDispersionChart.fs`

- `gyrationChart` takes a `GyrationComponent list` (the components to draw) and
  emits ONE series per component, named by `gyrationComponentLabel`, read as
  g₍ᵢⱼ₎ = Im[ρᵢⱼ] at the (i,j) slot each `GyrationComponent` denotes (new
  private `gyrationSlot`). Rationale for a component *list* rather than a raw
  `GyrationClass<'g>` parameter: the class is generic in its component payload
  and the two view-only sources (`RhoWithoutDispValue` carries `RhoValue`,
  `RhoWithDispValue` carries `DispersionFormula`) instantiate `'g` differently,
  so no single `GyrationClass<'g>` value crosses the boundary — reducing the
  class to `gyrationComponents cls |> List.map fst` is the uniform
  representation. `allGyrationComponents` is the six-slot fallback a coded
  preset keeps.
- Add `gyrationComponentsOfComplexity` / `muKindOfComplexity` — read the class /
  μ kind off a stored `complexity` value tree (mirroring `anisotropyOfEntry`;
  data, no physics re-derived).
- `muChart` takes a `MuKind`: `ScalarMuKind` → one scalar μ series;
  `GyromagneticMuKind` → μ₁₁/μ₂₂/μ₃₃ + gyration magnitude (the current
  rendering, and the coded-preset fallback).
- `muStyle` flips the gyration-magnitude series (found by name) to the right
  only when present, so a scalar-μ chart keeps its single series on the left.

### `OpticalConstructor.Ui/MaterialEditorView.fs`

- Add `Model.viewOnlyComplexity : MaterialComplexity option` — the stored value
  tree of a view-only entry (None for a coded preset with no tree); populated in
  `init`.
- Add public `previewGyrationComponents` / `previewMuKind` threading helpers:
  `EditableMaterial` reads `m.editor.gyration` / `m.editor.muKind`;
  `ViewOnlyMaterial` reads the stored `complexity` tree; a coded preset with no
  tree keeps the all-components rendering.
- Wire the two into the `previewPane` gyration/μ tab builders.

### Tests

- `NkDispersionChartTests` — chart-builder level: uniaxial-active → g₁₁/g₃₃;
  all-components fallback → six; gyromagnetic → four + right-axis g; scalar-μ →
  one series on the left.
- `MaterialEditorWindowTests` — threading level: editable uniaxial-active →
  `[G11;G33]`, editable scalar/gyromagnetic → the right `MuKind`, view-only
  Langasite (uniaxial-active stored tree) → `[G11;G33]`, a synthetic
  complexity=None preset → all components / gyromagnetic. Update the
  "one frame per preview tab" ui-smoke render to the new signatures.

## Risks

- Adding a `Model` field: only two full record literals exist (both in `init`);
  all other constructions are `{ … with … }` copies and tests build via `init`.
- `muStyle` was hard-coded to flip index 3; a scalar-μ chart has no such index —
  fixed by matching the gyration series by name (harmless no-op when absent;
  `setSeriesAxisSide` on an out-of-range index is already a no-op too).
