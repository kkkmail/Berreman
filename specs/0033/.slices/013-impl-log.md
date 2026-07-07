# Impl log — spec 0033, slice 013 (AC-B7: MaterialComplexity / MaterialEntry.complexity)

## Progress

- [x] Read task file, worker system prompt (implement_worker + arc-runner base),
      project prompt, slice spec, steering .spec-md step 13, slice-012
      state-of-the-world, Dispersion.fs, MaterialLibrary.fs, Standard.fs,
      Active.fs, Dispersive.fs, DispersionModels.fs, existing tests
      (DispersionModelsTests, MaterialProxyTests, PropagationTests).
- [x] Red: 4 AC-B7 tests added (`MaterialComplexityTests.fs` + fsproj
      registration); build fails FS0039 on the missing `magnetic`/`active`
      labels, `toProperties`, and `MaterialEntry.complexity`
      (`013-red-tdd.log`).
- [x] Implementation: `MaterialComplexity` + `toProperties`,
      `MaterialEntry.complexity`, nine re-expressed seeds
      (`properties = complexity.toProperties`), ripple `complexity = None`
      at the four non-Domain construction sites.
- [x] Green: constructor tests 369 → 373 on the first run, no test edits
      after red.
- [x] Full advisory gate runs — all five pass (see Testing state).
- [x] State-of-the-world written.

## Decisions

1. **`UniaxialTransparent` cannot reproduce two of the named presets — the
   acceptance wins.** The slice letter says "uniaxial crystals as
   UniaxialTransparent … active-crystal as UniaxialTransparent plus a constant
   PlanarActive gyration", but the engine maps `UniaxialTransparent` to the
   (ordinary, extraordinary, ordinary) diagonal (`Dispersion.fs:297`, the
   slice-008 langasite precedent), while `Eps.uniaxialCrystal` is
   diag(1.5², 1.65², 1.65²) (unique axis x) and the active crystal's
   `planarCrystal` is diag(n₁₁², n₁₁², n₃₃²) (unique axis z). No
   (o, e, o)-shaped value equals either tensor, and the binding acceptance
   ("complexity.toProperties MUST reproduce the original preset's
   OpticalProperties … within tolerance") plus the exact-equality seed pin in
   `PropagationTests` rule out an axis permutation (it would change off-normal
   physics). Both entries are therefore encoded as `BiaxialTransparent`
   per-axis values that reproduce the presets exactly; the deviation is
   documented at the bindings.
2. **Seeds hold the invariant by construction.** Each re-expressed entry sets
   `properties = complexity.toProperties` (module-private complexity bindings),
   so "properties is the complexity's composition" is literal code, not a
   convention. Constructions reuse the preset constants
   (`RefractionIndex.transparentGlass…`, `Eps.euv…Delta/Beta`) and the preset
   arithmetic paths — the active crystal derives n₁₁/n₃₃ through the same
   `EpsValue.fromRefractionIndex → .refractionIndex` sqrt round-trip
   `planarCrystal` performs — keeping every seeded tensor VALUE-IDENTICAL to
   the original preset, which the exact `Assert.Equal<OpticalSystem>` seed test
   in `PropagationTests` requires (it passed unmodified).
3. **`toProperties` routes through the single vacuum-μ/ρ site.**
   `DispersionModels.isotropicProperties` is documented as the one site for the
   vacuum convention (slice 011); `toProperties` builds its defaults there and
   overrides only the aspects present, assembling them through the engine's
   `toMuWithDisp` / `toRhoWithDisp` (the latter the `Active.fs` type
   extension already opened by `MaterialLibrary.fs`). No new vacuum literals.
4. **Vacuum keeps `None`.** The slice's re-expression list is an explicit
   enumeration (four glasses, uniaxial/biaxial, the two EUV entries,
   active-crystal — nine entries) that does not include the vacuum spacer,
   although n = 1 is trivially expressible. Followed the enumeration: vacuum
   stays preset-backed/view-only, which also protects the structural
   multilayer seeds' spacer semantics from editing. Recorded under Gotchas.
5. **Ripple sites get `complexity = None`.** A required record field breaks
   every `MaterialEntry` literal in the solution; the four non-Domain sites
   (`Storage/MaterialImport.fs` ×2, `Storage/Report.fs` dtoToEntry,
   `Tests/DispersionModelsTests.fs` round-trip entry) take `complexity = None`
   — imports and metadata-only DTO rebuilds are closure-backed until Part G /
   §D.9 lower them to data, so `None` (view-only) is the honest value, not a
   placeholder.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs`
  — `MaterialComplexity` (eps ALWAYS present; `magnetic : MuWithDispValue
  option`; `active : RhoWithDispValue option`) with pure
  `toProperties : OpticalPropertiesWithDisp`; `MaterialEntry.complexity :
  MaterialComplexity option` (+ doc); `constantComplexity` helper and the nine
  module-private complexity bindings; the nine entries re-pointed to
  `complexity.toProperties` / `Some complexity`; silicon, langasite, vacuum
  get `complexity = None`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialComplexityTests.fs`
  (new) — the 4 AC-B7 tests; registered in
  `OpticalConstructor.Tests.fsproj` after `SampleProxyTests.fs`.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/MaterialImport.fs`,
  `…Storage/Report.fs`,
  `…Tests/DispersionModelsTests.fs` — mechanical `complexity = None` at the
  existing `MaterialEntry` literals (compile ripple; see Decision 5 and
  Gotchas on `touches`).

## Testing state

TDD: red first (FS0039 naming the missing production symbols,
`013-red-tdd.log`), then the implementation, then green on the first run —
the tests were not modified after red. All five gates in the slice roster
pass in the worker's local ADVISORY runs (the arc-runner gate engine re-runs
them authoritatively after exit):

- `build` — `dotnet build Berreman.slnx -c Release` exit 0, 0 errors.
- `unit-tests` — BerremanTests 119 passed, 5 skipped (pre-existing), 0 failed
  (= 119 baseline; no core file touched this slice).
- `constructor-unit-tests` — 373 passed, 0 failed (+4 over the 369 baseline).
- `ui-smoke` — 54 passed (= baseline).
- `ui-tests` — 249 passed (= baseline).

No CRLF churn: `git diff --numstat` equals `--ignore-cr-at-eol`, and the new
test file is pure LF.

## Artifacts

- `specs/0033/.artifacts/013-red-tdd.log` — red build (FS0039).
- `specs/0033/.artifacts/013-build.log` — solution build.
- `specs/0033/.artifacts/013-unit-tests.log` — BerremanTests run.
- `specs/0033/.artifacts/013-constructor-unit-tests.log` — constructor tests.
- `specs/0033/.artifacts/013-ui-smoke.log` — ui-smoke run.
- `specs/0033/.artifacts/013-ui-tests.log` — ui view tests.

## Gotchas

- **The slice letter's `UniaxialTransparent` for the uniaxial/active crystals
  is unimplementable against the acceptance** (Decision 1): the engine's
  (o, e, o) mapping cannot produce diag(1.5², 1.65², 1.65²) or
  diag(n₁₁², n₁₁², n₃₃²). `BiaxialTransparent` per-axis encodings reproduce
  the tensors exactly; if a future editor wants the "uniaxial" label for
  these entries it needs either a rotation or new engine cases — out of this
  slice's `touches`.
- **`touches` ripple**: the slice declares [OpticalConstructor.Domain,
  OpticalConstructor.Tests], but adding a required field to `MaterialEntry`
  breaks the record literals in `OpticalConstructor.Storage`
  (`MaterialImport.fs`, `Report.fs`). The three-line mechanical
  `complexity = None` additions there are the minimal compile-restoring
  change, not scope expansion.
- **The Storage JSON schema / library-file DTO does not carry `complexity`**
  — same pre-existing seam as slices 011/012 recorded for the new value
  types; an exported-then-imported library entry comes back `complexity =
  None` (view-only). Serialization breadth of the `…Value` trees is the
  spec's storage seam, deferred (constraint 0.3 adds no schema work outside
  Part G).
- **Do not "simplify" the active-crystal binding to
  `RefractionIndex 2.315` directly**: the `EpsValue.fromRefractionIndex →
  .refractionIndex` round-trip reproduces `planarCrystal`'s sqrt(n²)
  arithmetic so the seeded tensor stays value-identical to the preset under
  `PropagationTests`' exact `Assert.Equal<OpticalSystem>`; skipping it risks
  a 1-ulp mismatch.
- `.manifest.state.json` shows as modified in `git status` — the arc-runner's
  own file (same as slices 001–012), left alone.
