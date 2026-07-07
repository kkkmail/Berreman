# Impl plan — spec 0033, slice 013 (AC-B7: MaterialComplexity / MaterialEntry.complexity)

## Approach

Add the edit-model option tree `MaterialComplexity` to
`OpticalConstructor.Domain/MaterialLibrary.fs` (right before `MaterialEntry`,
which gains `complexity : MaterialComplexity option`):

- `eps : EpsWithDispValue` (ALWAYS present), `magnetic : MuWithDispValue option`,
  `active : RhoWithDispValue option`.
- `toProperties : OpticalPropertiesWithDisp` routes eps through
  `EpsWithDispValue.toEpsWithDisp`, defaults absent options through the EXISTING
  single vacuum-μ/ρ wrapper (`DispersionModels.isotropicProperties`), and
  assembles present options through `toMuWithDisp` / `toRhoWithDisp` (the
  `Active.fs` type extension — already `open`ed by this file).

Re-express nine built-ins as complexities and set
`properties = complexity.toProperties` at seed time so the source-of-truth
invariant holds by construction. Constructions reuse the preset constants and
the preset arithmetic paths so the seeded tensors stay VALUE-IDENTICAL to the
presets (PropagationTests pins seeded systems with exact
`Assert.Equal<OpticalSystem>` — tolerance zero):

- four glasses → `IsotropicTransparent RefractionIndex.transparentGlass{,150,175,200}`
  (same single-arg `Eps.fromRefractionIndex` route as the presets);
- euv-molybdenum / euv-silicon → `IsotropicAbsorbing` over
  `createComplex (1 − Eps.euv…Delta) Eps.euv…Beta` (the preset expression);
- uniaxial crystal → `BiaxialTransparent (1.5, 1.65, 1.65)`,
  biaxial crystal → `BiaxialTransparent (1.5, 1.65, 1.75)` — see Risks;
- active crystal → `BiaxialTransparent (n11, n11, n33)` with n11/n33 derived via
  `EpsValue.fromRefractionIndex → .refractionIndex` (the exact sqrt-roundtrip the
  `planarCrystal` preset performs) plus
  `active = Some (RhoWithoutDispValue { gyration = PlanarActive (RhoValue 1.5e-6); hand = RightHanded })`.

`silicon` / `langasite` keep `None` (dispersion coded in
`OpticalProperties/Dispersive.fs:98-99`); `vacuum` also keeps `None` per the
slice's explicit enumeration (it is the structural spacer — view-only).

## Files to modify

- `…Domain/MaterialLibrary.fs` — new type + field + re-expressed seeds (in `touches`).
- `…Tests/MaterialComplexityTests.fs` (new) + fsproj registration — AC-B7 tests (in `touches`).
- Mechanical ripple (required field breaks every record literal; the build gate
  covers the whole solution): `complexity = None` at
  `…Storage/MaterialImport.fs:83,129`, `…Storage/Report.fs:94` (imports/DTO
  rebuilds are closure-backed → not yet expressible as data; Part G's lowering
  is a later slice), and `…Tests/DispersionModelsTests.fs:147`.

## Risks

- **`UniaxialTransparent` cannot reproduce two presets.** The core maps it to the
  (ordinary, extraordinary, ordinary) diagonal (`Dispersion.fs:297`), but
  `Eps.uniaxialCrystal` is diag(1.5², 1.65², 1.65²) (unique axis x) and the
  active crystal's `planarCrystal` is diag(n₁₁², n₁₁², n₃₃²) (unique axis z).
  The acceptance MUST ("reproduce the original preset's OpticalProperties") wins
  over the slice letter's case naming → `BiaxialTransparent` per-axis encodings
  for those two. Recorded in the impl-log.
- **Exact-equality seed tests** (`PropagationTests`): mitigated by identical
  arithmetic paths (above), and the AC-B7 tests re-pin every re-expressed entry
  against the original preset expressions.

## Test plan (red first)

New `MaterialComplexityTests.fs`: (1) None options default to vacuum μ/ρ
(values + `MuWithoutDisp`/`RhoWithoutDisp` short-circuit shape); (2) Some
options assemble through `toMuWithDisp`/`toRhoWithDisp` (Polder + PlanarActive
pinned against the engine builders); (3) every re-expressed built-in's
`complexity.toProperties` reproduces the original preset's eps/μ/ρ at reference
wavelengths (400/600/800 nm; 10/13.5 nm for EUV) and equals the entry's own
`properties`; (4) silicon/langasite/vacuum carry `None`, the nine re-expressed
entries carry `Some`. Red = FS0039/FS0764 on the missing type/field.
