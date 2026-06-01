# Architecture critique -- 004.slice-md cycle 2

## Summary

Clean. The attempt-02 re-spawn closed both items the cycle-1 judge
routed back on, and verification against `git diff HEAD` plus the new
source confirms the fixes landed without collateral. Layering remains
correct, the engine closure shape is reused rather than forked, and the
two deeper persistence concerns are correctly carried forward as
hand-off risks rather than half-fixed in scope. I see no new
architectural defect introduced by the route-back edits; the only
residual notes are two minor, pre-existing nits already flagged in
cycle 1 and explicitly outside the retry hint.

## Consistency

The route-back's two required changes are both verified present and
correct:

1. **Duplicate `"description"` key folded.** The schema `dispersionModel`
`$def` (`schema diff`, `optical-constructor-project.schema.json`) now
carries a single `"description"` member that merges both prior strings
("Tagged analytic dispersion model…" plus the "coefficient scalars are
admitted as additional properties" note). JSON last-wins can no longer
drop the authored model description on load. The cycle-1 defect is
closed.

2. **`isotropicProperties` lifted to the lower layer.** The
`Mu.vacuum.dispersive` / `Rho.vacuum.dispersive` convention now appears
in exactly one place (`DispersionModels.fs:219-220`) — a grep across the
whole `OpticalConstructor` tree returns only that helper. The three
former inline literals in `DispersionModels.fs`
(`toOpticalProperties`/`toAnisotropicOpticalProperties`) and the
private copy that lived in `MaterialImport.fs` are gone; `MaterialImport.fs:86,132`
now call the Domain helper through `open OpticalConstructor.Domain.DispersionModels`.
Reuse-critic F1 is resolved and the vacuum-μ/ρ convention is localised
to a single site in the correct (lowest) layer.

The one remaining consistency nit is reuse-critic F3, untouched (it was
labelled optional): `baseIndex` builds Lorentz/Drude complex terms with
raw `Complex(s, 0.0)` / `Complex(r*r - x*x, -d*x)` (`DispersionModels.fs:168-172`)
while the rest of the module uses the engine's `createComplex` helper.
Low value; not worth a third spawn.

## Separation of concerns

`exportCsv` (`MaterialImport.fs:171-186`) still recovers n as
`Complex.Sqrt (entry.properties.epsWithDisp.getEps w).[0,0]` — it reads
back the `ε₁₁` tensor component and roots it rather than sampling the
entry's n,k. For the isotropic tabulated/constant entries this slice
produces that is exact, but it silently narrows an anisotropic entry to
its x-principal index and assumes the n≥0 branch. This was raised in
cycle 1, was not in the judge's retry hint, and remains un-noted in the
function docstring (lines 167-170 mention "n is recovered as √(ε₁₁)"
but not the anisotropic narrowing). Still only a one-line-comment-worth
of clarity, not a defect — the export contract this slice owns is
isotropic-by-construction.

## Spec fit

R-1..R-8 are delivered and unit-tested; the SoW's "Files modified" list
matches the diff (Domain `DispersionModels.fs`/`MaterialLibrary.fs`,
Storage `MaterialImport.fs` + `.fsproj`, Ui `MaterialPreview.fs` +
`.fsproj`, schema, and the four test artefacts). AC-D7 is exercised at
the `plotInput` boundary (`MaterialPreviewTests.fs:19-24` asserts the nm
and eV toggles hand the engine the identical `Range<WaveLength>`); AC-D8
covers both the `None`-byte-identical path and the `Some` first-order
correction with the operating `T` proven absent from the serialized form
(`DispersionModelsTests.fs:96-110`); the `resolveMaterial` seam has both
known-id and `UnknownMaterialId` cases. The AC-D4 "Sellmeier-matches-Silicon"
contradiction remains correctly surfaced in the SoW Gotchas and covered
two ways — unchanged from cycle 1, and still the most defensible reading
under the no-ask rule.

## Evolvability

The two structural concerns I raised in cycle 1 — (a) the schema
`dispersionModel` `$def` documents an internally `kind`-tagged shape the
shared `ProjectJson.options` serializer does not emit (it produces the
adjacent-tag `Case`/`Fields` encoding), and (b) `MaterialEntry` stores
an opaque `OpticalPropertiesWithDisp` closure, not the originating
`DispersionModel`, so coefficients cannot round-trip back out — both
still hold in the code. That is the correct outcome: the judge directed
them to be deferred, not fixed, because the aggregate carries no
`materials` field yet and nothing serializes through this `$def` in this
slice. They are now recorded explicitly under SoW **Deferred** as
"HAND-OFF RISK" entries naming the materials-wiring slice (Part A/I) as
owner. The hand-off is honest and the cornering is documented; I have no
further ask here. I note only that the JSON round-trip test
(`DispersionModelsTests.fs:96-110`) proves `model = back` through the
real serializer but never asserts the emitted JSON matches the `$def`,
so the tag-shape divergence stays invisible at the gate until the wiring
slice asserts schema-conformance — which is exactly where it belongs.

## Risks

The hand-rolled meter-walking sample loop still appears twice —
`MaterialPreview.axisTicks` (`MaterialPreview.fs:40-44`) and `exportCsv`
(`MaterialImport.fs:177-185`) — both sidestepping the engine's
`getWaveLengthValue` mis-scaling quirk (SoW Gotcha) in two different
projects. If the engine quirk is ever fixed, both copies must change in
lockstep. Minor duplication across a layer boundary, not a defect; a
shared sampler would localise the workaround, but it is below the bar
for another spawn.

## Bottom line

This is the slice I recommended shipping in cycle 1, now with the two
route-back items resolved: the duplicate schema key is folded and the
`isotropicProperties` reuse is lifted to Domain, both verified against
the diff and a tree-wide grep. The catalogue, `evaluate`/`toOpticalProperties`,
the anisotropic three-index path, the thermo-optic correction, and the
`resolveMaterial` seam are correct, reuse-faithful, and well-tested; the
deferred persistence concerns are properly carried forward rather than
patched. The residual notes (exportCsv isotropic-narrowing comment,
`createComplex` consistency, duplicated sampling loop) are all minor and
none was in scope for this re-spawn. My read for the judge: pass. The
verdict is yours.
