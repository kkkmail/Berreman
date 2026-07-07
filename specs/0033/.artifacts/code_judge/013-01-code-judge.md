# Code judge -- 013.slice-md cycle 1

## Inputs read

- Slice spec: C:\GitHub\Berreman\specs\0033\.slices\013.slice-md
- State-of-the-world: C:\GitHub\Berreman\specs\0033\.slices\013-state-of-the-world.md
- Impl-log: C:\GitHub\Berreman\specs\0033\.slices\013-impl-log.md
- Gate results: build pass / unit-tests pass / constructor-unit-tests pass / ui-smoke pass / ui-tests pass
- Critic critiques: (none this cycle)

## Rationale

All five gates in the roster pass, and no critic critique was produced this
cycle, so the decision rests on whether the diff meets the slice-spec
requirements and whether the new public surface is tested. I verified the
worker's claims directly against `git diff HEAD` and the new test file rather
than taking the SoW at its word.

The slice's core deliverables are all present and match the spec letter:
`MaterialComplexity` in `OpticalConstructor.Domain/MaterialLibrary.fs` has
`eps : EpsWithDispValue` always present with `magnetic : MuWithDispValue option`
and `active : RhoWithDispValue option`; the pure `toProperties` member composes
`OpticalPropertiesWithDisp` through `eps.toEpsWithDisp` and defaults absent
options to the vacuum μ/ρ via `DispersionModels.isotropicProperties` (the
documented single vacuum-convention site from slice 011 — no new vacuum
literals), assembling present options through the engine's `toMuWithDisp` /
`toRhoWithDisp`. `MaterialEntry` gained `complexity : MaterialComplexity option`,
and the nine expressible built-ins are re-expressed with
`properties = complexity.toProperties` bound literally at the seed — the sync
invariant holds by construction. Silicon, langasite, and the vacuum spacer keep
`None`, as the spec's enumeration requires.

The one deviation from the slice letter — encoding the uniaxial crystal and the
active crystal's eps as `BiaxialTransparent` per-axis values instead of
`UniaxialTransparent` — is verified correct, not a shortcut. The engine maps
`UniaxialTransparent (nO, nE)` to `Eps.fromRefractionIndex (nO, nE, nO)`
(equal first/third diagonal entries), while `Eps.uniaxialCrystal` is
`(1.5, 1.65, 1.65)` (unique axis x) and `planarCrystal` is diag(n₁₁², n₁₁², n₃₃²)
(unique axis z); no (o, e, o) assignment reproduces either tensor, and an axis
permutation would change off-normal physics and break `PropagationTests`' exact
seed pin. The binding acceptance ("complexity.toProperties MUST reproduce the
original preset's OpticalProperties at reference wavelengths within tolerance")
outranks the case name in the how-to text. The decision is recorded in the
impl-log (Decision 1), in Gotchas, and at the code site — exactly the project
prompt's "pick the most consistent interpretation and record it" convention.

Test coverage satisfies the `done-green` criterion. The new public surface —
`toProperties`, `MaterialEntry.complexity`, and the nine re-expressed seeds —
is exercised by four new AC-B7 tests in `MaterialComplexityTests.fs` (registered
in the fsproj): vacuum μ/ρ defaults asserted both by value (Mu.vacuum /
Rho.vacuum at 600 nm) and by shape (non-dispersive short-circuit cases);
Polder-μ and PlanarActive-ρ assembly through the engine paths with negative
controls against the vacuum defaults; all nine re-expressed entries pinned
against the original `Standard.fs`/`Active.fs` preset expressions (restated
verbatim, not read back from the entries) at visible/EUV reference wavelength
grids, asserting both `toProperties` and the seeded `properties`; and the
None/Some partition (three `None` entries named, exactly nine `Some`).
Constructor tests went 369 → 373, matching the SoW's ledger, and the acceptance
maps one-to-one onto the third and first tests.

The declared-`touches` ripple ([Domain, Tests] plus `complexity = None` at four
record literals in `Storage/MaterialImport.fs`, `Storage/Report.fs`, and
`Tests/DispersionModelsTests.fs`) is the minimal compile-restoring change a
required record field forces, is honestly disclosed in the impl-log, and each
site carries an accurate comment (`None` = view-only until Part G / §D.9). The
known storage seam (the JSON DTO not carrying `complexity`) is pre-existing,
consistent with slices 011/012, and explicitly deferred by constraint 0.3. SoW,
impl-log, and diff agree everywhere I checked; nothing blocks `done-green`.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "All five gates pass and no critic raised findings. Verified against git diff HEAD: MaterialComplexity (eps always present, magnetic/active optional) with pure toProperties defaulting absent options to vacuum mu/rho through the single DispersionModels.isotropicProperties site; MaterialEntry.complexity added with the nine expressible built-ins re-expressed as complexities bound via properties = complexity.toProperties, silicon/langasite/vacuum staying None. The BiaxialTransparent encoding of the uniaxial/active crystals deviates from the slice letter's UniaxialTransparent but is forced by the binding acceptance: the engine maps UniaxialTransparent to an (o, e, o) diagonal which cannot reproduce diag(1.5^2, 1.65^2, 1.65^2) or diag(n11^2, n11^2, n33^2); the deviation is verified in Dispersion.fs and recorded per the project convention. New public surface is covered by four AC-B7 tests (vacuum defaults by value and shape, Polder/PlanarActive assembly, all nine seeds pinned against verbatim preset expressions at reference wavelengths, None/Some partition); constructor tests 369 -> 373. The complexity = None ripple into Storage is the minimal compile-restoring change and is honestly disclosed.", "retry_hint": ""}
```
