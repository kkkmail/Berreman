# Architecture critique -- 007.slice-md cycle 3

## Summary

Clean. The high-signal correctness defect cycle 2 held for — the
cone+Gaussian branch summing to weight `N` instead of `1` — is now
genuinely fixed: `SourceSpec.expand` hoists `coneWeight = 1/N` and
multiplies the Gaussian-fan weights by it, so both the `Some beam` and
`None` branches normalise, and a new AC-E9 test asserts a cone+Gaussian
source totals the source `intensity`, not `N`. With that closed, the
"expand-on-the-boundary, average on output" contract holds across every
fan. The remaining items are forward-looking seams, all spec-deferred; the
most important is that spectral expansion lives *outside* `expand`, so the
multi-source `combineSources` path silently ignores every spectral profile
except `LaserLine`.

## Spec fit

Coverage of R-1..R-10 maps cleanly onto AC-E1..E10 and the tests assert the
load-bearing properties directly (cone weights sum to 1, cone+Gaussian
totals intensity, no field-level summation, `Coherence.Unpolarized` as a
flag not a sentinel). One genuine seam: `SourceSpec.expand`
(`SourceSpec.fs:281-309`) folds in cone (§E.6), Gaussian (§E.9), the
unpolarized s/p split (§E.7) and the per-source intensity weight
(§E.2/§E.8) — but for `spectralProfile` it handles only `LaserLine`,
falling through to `baseInfo` for `Flat`, `Blackbody`, `Illuminant`, and
`ImportedSpectrum`. This is defensible: the R-8 signature
`SourceSpec -> (IncidentLightInfo * float) list` carries no wavelength
range, so range-based sampling *cannot* live in `expand` and correctly
belongs in `SpectralProfile.sample`. The under-delivery is spec-sanctioned,
but its consequence deserves a plain statement (see Evolvability).

## Evolvability

The expand-on-the-boundary contract is, in practice, split across two
entry points with two weighting conventions: `SpectralProfile.sample range`
for the wavelength axis, and `SourceSpec.expand` for everything else.
`combineSources` (`SourceCombination.fs:38-44`) — the multi-source path
this slice owns and tests — calls only `expand`. So a project holding a
`Blackbody` or `D65` source with no cone/Gaussian combines as a
*monochromatic* source at full intensity; its spectral weighting is dropped
on this path. Nothing is wrong for 007 (AC-E5 is met by `sample`, AC-E8 by
the combine mechanics), but Part F is now cornered into stitching the two
halves — threading the range through `sample`, then re-feeding each sampled
wavelength back through the cone/Gaussian/unpol fan, re-deriving the
weight multiplication `expand` already encodes. When Part F lands, consider
hoisting a single `expandOverRange : RangedVariable -> SourceSpec ->
(IncidentLightInfo * float) list` that composes `sample` with the `expand`
fan, so the two weighting conventions are reconciled in one place. No
change is needed in this slice.

The cycle-2 reuse note still stands for the reuse critic: `sample`
hand-rolls the wavelength linspace (`SourceSpec.fs:~190-200`) where the
engine exposes `RangedVariable.value i` (`Variables.fs:68`) as a
canonical-meter sampler over the same grid. That is the reuse critic's call
and an architecture divergence risk, not a defect.

## Risks

- **No round-trip test for a populated `sources` list.** The aggregate
  gained `sources : SourceSpec list` (`Project.fs:30`), but every literal
  touched sets `sources = []` and `ProjectJsonRoundtripTests` exercises
  only the empty case. The serializer is reflection-based
  FSharp.SystemTextJson, so a populated list will likely *emit*, but
  `SourceSpec` is a deep payload — `IncidentLightInfo`, several DUs,
  `double<K>`, `Angle` — and `SchemaValidation` runs on load against a
  `sourceSpec` `$def` the SoW itself calls "permissive". Nothing currently
  proves a non-empty `sources` survives serialize → schema-validate →
  deserialize. Deferring the `$def` to slice-003 is fine, but a future
  slice that populates `sources` may find the schema or a DU
  representation rejects it. One round-trip test with a real `SourceSpec`
  would have de-risked this; absent it, flag it.

- **`Incoherent` flag still inert, and AC-E7's test exercises the engine,
  not the flag.** As cycle 2 noted, `expand` treats `Coherent` and
  `Incoherent` as identical pass-throughs, and the AC-E7 test asserts a
  `Plate` substrate yields a `Multiple` solution — which holds regardless
  of `coherence`. R-7 scopes this to "ONLY the source-level flag" with
  wiring owned downstream, so carrying an unused flag is in-scope; the
  residual concern is purely that the test gives false confidence the flag
  *does* something. Unchanged from cycle 2; low.

- **Exact float-zero branch in `toIncidentLight`.** `SourceSpec.fs:257`
  selects `create` vs `createInclined` on `incidenceAngle.value = 0.0`
  (exact equality). Correct for current editor inputs, but a near-zero
  angle from a range endpoint or cone re-projection would route through
  `createInclined`; an `abs < eps` guard (the `MatrixComparison.fs` float
  precedent) is more in keeping with the codebase. Low — the two paths
  converge after the `refractionIndex` record-copy.

- **`angularSpectrum` magic constants and Gaussian-replaces-angle.**
  `n = 64` / `windowFactor = 8.0` are unlabelled knobs; by construction
  `sigma` is waist-independent so the 1/waist scaling is correct and
  tested, but the absolute calibration is approximate (SoW acknowledges).
  Separately, when both `cone` and `gaussianBeam` are set the Gaussian fan
  *overwrites* the incidence angle with magnitudes-from-normal, discarding
  the oblique base and the cone — in-scope, but a hidden composition rule a
  future steered-beam slice must unwind. Both low; a named `let` and a
  one-line comment would de-risk.

## Bottom line

I would ship this. The cone+Gaussian weight defect that justified cycle
2's hold is now fixed *and* guarded by a dedicated AC-E9 test, closing the
last correctness gap in the slice's own load-bearing contract. Reuse and
layering remain exemplary — `Units.toWaveLength`, `createGaussian` +
`FourierTransform.fft`, `EmField.stokesVector`, `StokesVector.(+)`/`Zero`,
and the substrate `Multiple` path are all reused rather than reinvented,
and the Avalonia-free seams match the slice-005/006 precedent. The two
substantive remaining items — the split spectral/expand seam and the
untested populated-`sources` round-trip — are both deferred to Part F and
slice-003, so they are forward-looking risks, not defects in what 007 was
asked to deliver. None rises to a re-spawn in my read; I have no gate
authority — the judge decides, and may weigh the round-trip gap more
heavily if storage fidelity is a hard line for this arc.
