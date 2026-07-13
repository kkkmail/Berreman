# Step 014 — impl-log

## Progress

- [x] Read protocol / project prompt / slice spec; surveyed the migration surface.
- [x] ElementId.fs — EntryProtection, PolarizerBehavior/MuellerComponent/PolarizerCategory, preset reshape, seeds.
- [x] Propagation.fs — muellerElement read seam, rotationMueller/rotateMueller/componentMueller/compoundMueller/behaviorMueller/behaviorInputStokes.
- [x] LibraryFacets.fs — polarizerCategoryFacetKey + polarizerCategoryKey + polarizerCategoryDef appended after the kind facet.
- [x] TableAndElementRotationView.fs — runInputStokes via behaviorInputStokes; idealKindOf helper; runAnalyzerKind/runAnalyzerOpt via ComputedIdeal extraction.
- [x] Tests — PolarizerBehaviorTests.fs (new, registered in fsproj) + LibraryProxyTests migration (9 construction sites, 2 kind assertions).
- [x] Diagnostic build + suites; SoW.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs` — EntryProtection DU; protection fields on SourcePreset/DetectorPreset; PolarizerKind doc updated; MuellerComponent/PolarizerBehavior/PolarizerCategory (with `.label`); PolarizerPreset reshaped to behavior+category+protection; LibraryEntry.protection member; fullDescription/forKinds migrated; the six seeded presets ProtectedBuiltIn, the ideals ComputedIdeal Lp/Cp/Cp.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Propagation.fs` — muellerElement (read seam); the Part-F evaluation block after identityMueller.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryFacets.fs` — the polarizer-category facet.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs` — the four `p.kind` read sites re-pointed through behavior.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PolarizerBehaviorTests.fs` — NEW (11 facts).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryProxyTests.fs` — preset constructions/assertions migrated.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — new test file registered.

## Decisions

- **Protection is a field on the preset records, derived only for samples**: a future user-created
  preset must be `UserManaged` without a type change, so `SourcePreset`/`DetectorPreset`/
  `PolarizerPreset` carry `protection : EntryProtection`; `Sample` carries no field (spec F.0 pins
  every sample as UserManaged) and `LibraryEntry.protection` maps `SampleItem _ -> UserManaged`.
- **Compound order = light-traversal order**: the slice's "ordered product" is ambiguous; the list
  is read as the order light hits the components (first listed = first surface), so the matrix is
  Mₙ·…·M₁ and `compoundMueller components * sv` applies the first component first. Pinned by tests.
- **behaviorMueller rotates the WHOLE compound** by the element's θ (R(−θ)·Π·R(θ)) — algebraically
  identical to rotating each component by (θ + offset) since R(a)·R(b) = R(a+b); a test pins the
  identity.
- **behaviorInputStokes for ConstantMueller is un-normalized**: the compound applied to unpolarized
  natural light — there is no analytic kind to normalize by, so the compound's own throughput
  attenuates S0 (an ideal-LP single-component compound passes ½). `ComputedIdeal` keeps the
  existing unit-intensity `inputStokes` exactly.
- **forKinds by category**: LpCategory → LinearPolarizer, CpCategory → CircularPolarizer; the
  compound/custom categories serve BOTH roles (the old "never both" comment applied to the ideal
  presets only; a custom Mueller polarizer is not intrinsically linear or circular).
- **UI keeps kind-typed runners**: the sweep builders take `PolarizerKind`, so `runAnalyzerKind` /
  `runAnalyzerOpt` extract the kind from `ComputedIdeal` and skip a `ConstantMueller`-bound
  polarizer like a non-polarizer — unreachable today (no seed, no editor), and Part G reworks this
  resolution (retiring the IdealLinear fallback) anyway. `runInputStokes` goes through
  `behaviorInputStokes`, which IS today's `inputStokes` for every reachable entry.
- **Facet placement**: `polarizerCategoryDef` sits right after the kind facet in `libraryFacets`
  (the non-sample facet; step-011's SoW deferred exactly this to Part F). Facet label vocabulary
  comes from `PolarizerCategory.label` (the DU owns its human-readable names).

## Testing state

Diagnostic verification (not gate authority — the arc-runner's gate engine runs the roster after
this worker exits):

- `dotnet build Berreman.slnx -c Release -nologo -v:m` — **0 errors**, no MSB3277; the only 10
  warnings are the step-001-catalogued pre-existing set in untouched files (NU1701 Wolfram.NETLink,
  FS0044 ChartWindow, SYSLIB0051 vendored MathNet ×2, FS3873 Dispersion, FS1125 SeriesDataTests ×4).
  Zero warnings from touched files.
- OpticalConstructor.Tests: **571/571** (checkpoint 559, +12 — the whole PolarizerBehaviorTests
  suite).
- BerremanTests: **119 passed / 5 pre-existing skips** (== checkpoint).
- ui-smoke: **131/131** (== checkpoint).
- ui-tests: **367/367** (== checkpoint).
- Line endings verified: `git diff --numstat` identical with `--ignore-cr-at-eol`; the new test
  file carries zero CR bytes.

## Artifacts

- `specs/0038/.artifacts/014-diag-build.log`
- `specs/0038/.artifacts/014-diag-constructor-tests.log`
- `specs/0038/.artifacts/014-diag-unit-tests.log`
- `specs/0038/.artifacts/014-diag-ui-smoke.log`
- `specs/0038/.artifacts/014-diag-ui-tests.log`

## Gotchas

- **The task file's system-prompt path was stale again** — `implement_worker.system-md` lives under
  `src/ai_strategy_generator/multistep/` in the tool repo, not at the repo root the task file
  points to; located and read in full together with the shared base `arc-runner.system-md` (the
  step-007..013 gotcha recurred).
- **"Ordered product" needed an interpretation**: the slice does not say whether the component list
  is matrix-notation order or light-traversal order. Chose light-traversal (first listed = first
  surface light hits ⇒ product Mₙ·…·M₁), consistent with how `propagate` documents its pipeline
  (sample then analyzer, written right-to-left in matrix order) and with the LpCp/CpLp category
  names reading in traversal order. Pinned by the ordered-product and order-matters tests.
- **`Angle -theta.value` parses as subtraction** in F# (`Angle - theta.value`), not application —
  `rotateMueller` needs `Angle (- theta.value)`.
- **The rotation sign convention is load-bearing**: R(θ) must carry `+s` in row 1 and `−s` in row 2
  for R(−θ)·LP₀·R(θ) to equal `analyzerMueller IdealLinear θ` (the parity acceptance); the
  dedicated `rotateMueller` test pins it before anything builds on it.
- **`PolarizerPreset` lost `kind`** — any out-of-tree construction site must switch to
  `behavior`/`category`/`protection`; the compiler finds every site (this round: 4 Ui reads, 9 test
  constructions). Storage never serializes presets (only catalogue-kind enums appear in the JSON
  schemas), so no schema change.
- Step 002–013 carried-over gotchas remain valid (baselines come from `.checkpoints-json`, not the
  SoW YAML; the window registry is app-global test state in Ui.Tests; the appsettings.json
  write-back into test output copies is expected).
