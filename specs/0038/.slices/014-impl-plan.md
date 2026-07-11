# Step 014 — impl-plan (IMPLEMENT: protected entries + polarizer behaviour data)

## Approach

Spec 0038 Part F.0 first half, Domain-only: the "ideal element" notion
dissolves into ordinary protected library entries, and polarizer physics
becomes data.

1. **`EntryProtection = ProtectedBuiltIn | UserManaged`** (ElementId.fs, before
   the preset records). Protection is DATA on each preset record
   (`SourcePreset` / `DetectorPreset` / `PolarizerPreset` gain
   `protection : EntryProtection`) — not derived from the entry case — so a
   future user-created preset is `UserManaged` without a type change. Samples
   carry NO field: every sample (seeded or user-created) is `UserManaged` by
   the spec ("seeded SAMPLES remain UserManaged — recorded interpretation"),
   so `LibraryEntry.protection` maps `SampleItem _ -> UserManaged` and reads
   the field off the other three cases. Seeds: the six presets (src-600,
   det-intensity, det-ellipsometer, pol-lp, pol-cp-left, pol-cp-right) become
   `ProtectedBuiltIn`.

2. **`PolarizerBehavior` / `MuellerComponent` / `PolarizerCategory`**
   (ElementId.fs, before `PolarizerPreset`):
   `MuellerComponent = { matrix : MuellerMatrix; offset : Angle }` (stored at
   reference orientation), `PolarizerBehavior = ComputedIdeal of PolarizerKind
   | ConstantMueller of MuellerComponent list` (ordered),
   `PolarizerCategory = LpCategory | CpCategory | LpCpCategory | CpLpCategory
   | CustomMueller` with a `.label`. `PolarizerPreset` drops `kind` and
   carries `behavior + category + protection`; `forKinds` maps LpCategory →
   LinearPolarizer, CpCategory → CircularPolarizer, the compound/custom
   categories → both roles; `fullDescription` matches behavior (the three
   ideal strings unchanged; ConstantMueller gets category-labelled prose).
   The three seeded ideals become `ComputedIdeal` entries (Lp/Cp/Cp).

3. **Evaluation lives ONLY in the Stokes/Mueller pipeline** (Propagation.fs):
   `muellerElement` (read seam mirroring `muellerOfRows`/`stokesComponents`),
   `rotationMueller` (the standard Stokes rotation R(θ)),
   `rotateMueller theta m = R(−θ)·M·R(θ)`, `componentMueller` (a component
   rotated by its offset), `compoundMueller` (the ORDERED product — list in
   light-traversal order, first component applied first, i.e. Mₙ·…·M₁),
   `behaviorMueller` (ComputedIdeal → the EXISTING `analyzerMueller`;
   ConstantMueller → whole-compound rotation by the element's θ — provably
   equal to rotating each component by θ+offset), and `behaviorInputStokes`
   (ComputedIdeal → the EXISTING `inputStokes`; ConstantMueller → the
   compound applied to unpolarized light, un-normalized). Nothing enters the
   Berreman stack.

4. **The polarizer facet** (LibraryFacets.fs): `polarizerCategoryFacetKey` +
   `polarizerCategoryKey : PolarizerCategory -> DiscreteKey` + a def that
   applies only to `PolarizerItem`, appended to `libraryFacets` right after
   the kind facet (step-011's SoW already deferred this here as "the
   polarizer-category facet for non-sample entries — Part F").

5. **UI compile-fix only** (TableAndElementRotationView.fs, 4 sites):
   `runInputStokes` goes through `behaviorInputStokes` (identical result for
   the ideal seeds); `runAnalyzerKind` / `runAnalyzerOpt` extract the kind
   from `ComputedIdeal` (a ConstantMueller-bound polarizer — unreachable
   today, no editor and no seed — is skipped exactly like a non-polarizer).
   The `IdealLinear` fallback retirement is Part G (step later), untouched.

6. **Tests** (OpticalConstructor.Tests, new `PolarizerBehaviorTests.fs` +
   LibraryProxyTests migration): ComputedIdeal ≡ analyzerMueller element-wise
   across all three kinds × sampled orientations; rotateMueller of an LP at 0°
   ≡ analyzerMueller at θ; the two-component ConstantMueller ordered product
   (incl. order-matters and crossed-compound physics checks);
   behaviorMueller whole-compound rotation parity; every seeded preset
   ProtectedBuiltIn / every seeded sample UserManaged; the polarizer facet
   applies only to polarizers and classifies the seeded 1×Lp + 2×Cp.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs` — types, preset reshape, seeds, members.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Propagation.fs` — rotation/compound/behavior evaluation.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryFacets.fs` — polarizer-category facet.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs` — 4 read sites re-pointed.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PolarizerBehaviorTests.fs` — NEW.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryProxyTests.fs` — preset construction/assertion migration.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — register the new file.

## Risks

- **Rotation-convention sign**: R(θ) rows must be [0, c, s, 0] / [0, −s, c, 0]
  so that R(−θ)·LP₀·R(θ) reproduces `analyzerMueller IdealLinear θ` exactly —
  pinned by a dedicated test before anything builds on it.
- **Order convention** of the compound ("ordered product") is ambiguous in the
  slice; choosing light-traversal order (first listed = first hit), recorded
  in the impl-log Gotchas and pinned by tests.
- **Record-shape break ripple**: PolarizerPreset loses `kind`; the compiler
  finds every site (Ui ×4, LibraryProxyTests ×9 constructions). Storage never
  serializes presets (checked — only catalogue-kind enums in the JSON schemas).
- ui-smoke / ui-tests baselines must not regress: the UI change is a pure
  re-expression for all reachable states.
