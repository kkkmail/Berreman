# Step 0207 — state of the world (IMPLEMENT: the parameter axes the existing models publish)

## Where we are

Spec 0050, section S6 (send and receive kits; copy experiments between kits), closing on its S7 prerequisite
(001 §H, §I, §S6): the shared item contracts land before S7 so that copying an experiment can map its items.

- Step 205 moved `ParameterName` and `DialSign` into `ItemModels.fs`.
- Step 206 added the vocabulary (`ParameterValue`, `ParameterKind`, `ItemParameterAxis`) and the detector's noise
  axes.
- This step makes the existing models publish their parameter axes (001 §I: generated, not hand-written): the
  source preset, the ideal polarizer presets and the stack `Sample`.

## What's working

- Add `Thickness.parameterValueOpt`: a finite thickness in metres, `Infinity` → `None`.
- Add `SourcePreset.parameterAxes`: azimuth and ellipticity (angles, 0), degree of polarization (fraction, 0) and
  intensity scale (scale, 1), i.e. unpolarized light at unit scale.
- Add `PolarizerPreset.parameterAxes` (none) and `Sample.parameterAxes`: one thickness per finite layer, in
  stack order (`film<k>.thickness`, `film<k>.layer<j>.thickness`, `substrate.thickness`).
- Add four `ItemModelsTests` facts (constructor unit tests 1266 → 1270).

## Tests

These are development runs, recorded in `0207-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 warnings, 0 errors.
- **constructor-unit-tests:** 1270 passed, 0 failed, 0 skipped (1266 → 1270, +4). The four new identities and the
  two existing `ItemModelsTests` facts are `Passed` in the TRX.
- **elevation** (from the arc origin): `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects (ELEV004 excluded).
- **The fourteen `invariant-s22-*` greps:** each finds nothing.

```yaml
gates:
  constructor_unit_tests: 1270
```

## Architecture

- **A model's parameters are a member on the model** (spec 0050 S7.1, 001 §I). They are optional type extensions
  in `ItemModels`, which now opens `Berreman.Media`, `MaterialLibrary` and `Library`; both modules compile before it.
- **Thickness axes have one conversion,** `Thickness.parameterValueOpt`. A sample's axis list is that conversion
  over its layers in stack order. A `Repeated` cell's layer is one axis, shared by every period. A semi-infinite
  layer publishes nothing.
- **Materials publish nothing at the sample level.** A study opens a material version and owns its unknowns
  (001 §I, S11.1).
- **λ and the tunable limits stay the source preset's own fields.** The source's axes are only its output
  polarization and its intensity scale.

## Deferred

Nothing in this step's scope.

- The item's parameter states created from these axes, and the fitter's `ParameterAxis` list generated from them,
  are S7 steps by the spec's plan.

## Gotchas

- **The extensions are optional:** `parameterAxes` and `parameterValueOpt` are visible only where
  `OpticalConstructor.Domain.ItemModels` is opened.
- **Axis names are identities.** Item parameter states and fit reports will key on `film2.layer1.thickness` and
  the like; changing the naming scheme changes them.
- **Compare catalogue thicknesses with a tolerance:** the 600 nm film is stored as `6.000000000000001E-07`.
- **The two noise facts share step 206's three test-local values,** hoisted to private module bindings.
- **A partial build recompiles the vendored `MathNetNumerics/Numerics`** and prints its two `SYSLIB0051`
  warnings, as at step 206. The red test-project build here did so; the solution build after it printed none.

## Changelog

- 2026-11-09 — step 0207: added `Thickness.parameterValueOpt`, `SourcePreset.parameterAxes`,
  `PolarizerPreset.parameterAxes` and `Sample.parameterAxes` to `ItemModels.fs`, plus four new facts.
