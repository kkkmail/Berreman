# Step 014 — state of the world

## Where we are

Step 014 opens spec 0038 Part F in the Domain: the notion "ideal element"
dissolves — the six seeded presets (src-600, det-intensity, det-ellipsometer,
pol-lp, pol-cp-left, pol-cp-right) are now ordinary PROTECTED library entries
(`EntryProtection = ProtectedBuiltIn | UserManaged`, exposed by every
`LibraryEntry`; seeded and user-created samples stay `UserManaged`), and
polarizer physics is DATA (`PolarizerBehavior = ComputedIdeal of PolarizerKind
| ConstantMueller of MuellerComponent list`, with `PolarizerCategory` as the
polarizer facet value). The three seeded ideals are `ComputedIdeal` entries
that still synthesize through the existing `Propagation.inputStokes` /
`analyzerMueller` exactly as before; `ConstantMueller` compounds evaluate only
in the Stokes/Mueller pipeline (never the Berreman stack) and their dedicated
editor stays a non-breaking future addition. Next in Part F: the Library
window over the whole entry corpus (the step-012 tree control instantiated a
second time, the step-013 Materials-window precedent).

## What's working

- Elevate EntryProtection (ProtectedBuiltIn | UserManaged) as data on the
  Source/Detector/Polarizer preset records, expose LibraryEntry.protection
  (samples always UserManaged), and seed the six presets ProtectedBuiltIn.
- Reshape PolarizerPreset to behavior + category + protection: MuellerComponent
  { matrix; offset } stored at reference orientation, PolarizerBehavior
  (ComputedIdeal | ConstantMueller), PolarizerCategory (Lp / Cp / LpCp / CpLp /
  CustomMueller) with its own labels; the three seeded ideals are ComputedIdeal
  entries with Lp/Cp categories.
- Add the pipeline evaluation in Propagation: rotationMueller / rotateMueller
  (R(−θ)·M·R(θ)) / componentMueller / compoundMueller (ordered product in
  light-traversal order) / behaviorMueller / behaviorInputStokes, plus the
  muellerElement read seam; ComputedIdeal routes through the EXISTING
  analyzerMueller / inputStokes unchanged.
- Add the polarizer-category facet to the library facet catalogue (applies only
  to polarizer entries, vanishes over polarizer-free populations) and re-point
  the four Ui read sites through the behavior seam (identical behaviour for
  every reachable state).
- 12 new pure facts in PolarizerBehaviorTests: ComputedIdeal parity with the
  analyzer synthesis across orientations, the pinned rotation convention, the
  ordered rotated-product compound (incl. order-matters, crossed extinction,
  Malus through a compound), whole-compound rotation ≡ offset shift, protection
  over every seed, and the facet classification.
- Suites 571 (+12) / 119 / 131 / 367; build clean, no MSB3277, zero warnings
  from touched files.

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this
  worker exits (IMPLEMENT Invariant 6 — the worker acts, it runs no checks).
  The roster for this step is `build`, `unit-tests`, `constructor-unit-tests`,
  `ui-smoke`, `ui-tests`.
- Diagnostic verification (not gate authority): `dotnet build Berreman.slnx -c
  Release` succeeded with 0 errors, **no MSB3277**, and zero warnings from the
  touched projects (Domain / Ui / Tests) — the only warnings are the
  step-001-catalogued pre-existing set in untouched files (FS1125
  SeriesDataTests ×4, FS3873 Dispersion, FS0044 ChartWindow, SYSLIB0051
  vendored MathNet ×2, NU1701 Wolfram.NETLink). Suites:
  OpticalConstructor.Tests **571/571** (checkpoint 559, +12: the whole
  PolarizerBehaviorTests suite — ComputedIdeal ≡ analyzerMueller /
  inputStokes across three kinds × 13 orientations, the R(−θ)·M·R(θ) sign
  convention pinned, the two-component ordered product with order-matters and
  crossed-extinction physics checks, whole-compound rotation ≡ per-component
  offset shift, ProtectedBuiltIn on exactly the six named presets with every
  seeded sample UserManaged, and the polarizer-category facet), BerremanTests
  **119 passed / 5 pre-existing skips** (== checkpoint), ui-smoke **131/131**
  (== checkpoint), ui-tests **367/367** (== checkpoint). Logs in
  `specs/0038/.artifacts/014-diag-*.log`.
- Nothing deferred.

## Architecture

- **Protection is data on the preset records, derived only for samples**:
  `SourcePreset` / `DetectorPreset` / `PolarizerPreset` carry
  `protection : EntryProtection` so a future user-created preset is
  `UserManaged` without a type change; `Sample` carries no field — spec F.0
  pins every sample as UserManaged, so `LibraryEntry.protection` maps the case.
  Enforcement (no delete/inactivate/supersede on protected entries) is the
  Library window's and Part H's job; this step makes the state DATA.
- **Behaviour is data in the Library domain; evaluation lives only in the
  pipeline**: `ElementId.fs` holds the DUs (pure data, engine `MuellerMatrix` /
  `Angle` payloads); `Propagation.fs` owns rotation and the compound product —
  a ConstantMueller entry can never reach `sampleToSystem` / the Berreman
  stack by construction.
- **The compound is the ORDERED product in light-traversal order** (first
  listed component = first surface hit ⇒ matrix Mₙ·…·M₁); run-time orientation
  rotates the WHOLE compound (R(−θ)·Π·R(θ)), provably identical to shifting
  every component's offset by θ (R(a)·R(b) = R(a+b)) — one rotation, not one
  per component.
- **ComputedIdeal delegates, never duplicates**: `behaviorMueller` /
  `behaviorInputStokes` route the ideal cases to the EXISTING `analyzerMueller`
  / `inputStokes` — the parity acceptance is a delegation test, and the Ui's
  `runInputStokes` now goes through the behaviour seam with identical results.
- **forKinds reads the category**: Lp → LinearPolarizer, Cp →
  CircularPolarizer, the compound/custom categories serve both roles (the old
  "never both" held only for the three ideals).
- **The polarizer facet is one ordinary AttributeDef** (`polarizerCategoryDef`,
  right after the kind facet): applicable only to polarizer entries, so it
  vanishes entirely over polarizer-free populations — no engine change.

## Deferred

- The Library window over the whole entry corpus (`<UICOMP:LibraryWindow>`),
  the Library-bay removal, and protection ENFORCEMENT in verbs — later Part F
  steps; Browse/Select modes — Part G.
- The ConstantMueller entry editor — explicitly OUT OF SCOPE (operator, 001);
  the DU makes it a non-breaking future addition.
- Retiring the `IdealLinear` analyzer fallback and behaviour-typed sweep
  runners (`runAnalyzerKind` / `runAnalyzerOpt` still extract kinds from
  `ComputedIdeal`; a ConstantMueller-bound polarizer — unreachable today — is
  skipped like a non-polarizer) — Part G's pre-binding rework.
- `ProtectedBuiltIn` × lifecycle interaction (cannot inactivate/supersede) —
  Part H.
- The pre-existing warnings in untouched files remain for spec 0038 Part N's
  sweep (carried from steps 002–013).

## Gotchas

- **The task file's system-prompt path was stale again** — the IMPLEMENT
  worker prompt lives under
  `src/ai_strategy_generator/multistep/implement_worker.system-md` in the tool
  repo; located and read in full (the step-007..013 gotcha recurred).
- **"Ordered product" was ambiguous** — chose light-traversal order (first
  listed = first hit ⇒ Mₙ·…·M₁), consistent with the pipeline's own
  documentation style and the LpCp/CpLp category names; pinned by the
  ordered-product and order-matters tests. Recorded interpretation.
- **The Stokes rotation sign convention is load-bearing**: R(θ) rows must be
  [0, c, s, 0] / [0, −s, c, 0] for R(−θ)·LP₀·R(θ) to reproduce
  `analyzerMueller IdealLinear θ`; a dedicated test pins it. (`Angle
  -theta.value` parses as subtraction — `Angle (- theta.value)`.)
- **`PolarizerPreset` changed shape** (kind → behavior/category/protection) and
  the Source/Detector presets gained `protection`: out-of-tree construction
  sites must be updated (the compiler finds them; Storage never serializes
  presets, so no schema change).
- **`behaviorInputStokes` for ConstantMueller is un-normalized** — the compound
  applied to unpolarized light, so its own throughput attenuates S0 (a
  single-LP compound passes ½, where the ideal-LP synthesis emits unit
  intensity by convention). Recorded interpretation, pinned by test.
- Step 002–013 carried-over gotchas remain valid (baselines come from
  `.checkpoints-json`, not the SoW YAML; the window registry is app-global
  test state in Ui.Tests; the appsettings.json write-back into test output
  copies is expected).

## Changelog

- 2026-07-11 — Step 014 (IMPLEMENT, attempt 1): elevated the seeded presets to
  ordinary protected entries (`EntryProtection` data on the preset records +
  `LibraryEntry.protection`, samples always UserManaged) and polarizer
  behaviour to data (`MuellerComponent` / `PolarizerBehavior` /
  `PolarizerCategory`; `PolarizerPreset` reshaped; the three ideals
  `ComputedIdeal` Lp/Cp/Cp); added the pipeline evaluation
  (`rotationMueller` / `rotateMueller` / `componentMueller` /
  `compoundMueller` / `behaviorMueller` / `behaviorInputStokes` +
  `muellerElement`), the polarizer-category facet, the four Ui read-site
  re-points, and 12 pure facts in PolarizerBehaviorTests. Build clean (no
  MSB3277); suites 571 / 119 / 131 / 367.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 571
  ui_smoke_tests: 131
  ui_tests: 367
```
