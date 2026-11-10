# Step 0266 — state of the world

## Where we are

Spec 0050, section S8.2 (setups as slots), step 0266 (IMPLEMENT).

- Step 0252 made `chartForRun` fold every bound polarizer and sample on a detector's path, starting from the default
  source emission.
- Step 0265 made the run read the slots' items, leaving the source slot's item aside.
- This step makes the source slot's item the run's source. It emits its own Stokes state times its intensity scale,
  through `emissionWith` over its own parameters (001 §N "The source is real").

## What's working

- Added `model.runEmission`: the first light-source slot's item emitting through `emissionWith` over its own
  parameters, else step 252's default light; its error is the failure chart's text
- `chartForRun` reads `runEmission`; a source item that cannot emit, or that the kit or stores lack, shows
  `The run cannot be evaluated` with the reason
- Kept the run's wavelength as the bound preset's (`boundSource`), which `withItem` binds for a source item
- Added two `ExperimentControlsTests` facts: a HeNe source item's own polarization through a swept polarizer, and an
  out-of-range `degreeOfPolarization` naming itself on the failure chart

## Tests

No gate verdict is claimed (Invariant 6); the supervisor runs the frozen roster. The development runs below used each
gate's own command over the final tree, with logs in `specs/0050/.artifacts/0266-dev-*.log`.

- **build:** exit 0, 0 warnings, 0 errors, 36 s (deadline 600 s). The round's first build recompiled the vendored
  MathNet and showed its two existing `SYSLIB0051` warnings (`MathNetNumerics/Numerics/Exceptions.cs`, untouched), as
  at step 265. Every later build showed none.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, 190 s (deadline 600 s).
- **constructor-unit-tests:** `Passed!`, 1429 succeeded, 0 failed, 0 skipped, 70 s (deadline 180 s). Unchanged.
- **ui-smoke:** `Passed!`, 245 succeeded, 0 failed, 0 skipped, 199 s (deadline 600 s). Unchanged.
- **ui-tests (six selected classes):** `Passed!`, 91 succeeded, 0 failed, 0 skipped, 108 s (deadline 360 s).
  - The count is 89 at HEAD, so it rises by 2.
  - Both new identities are `Passed` in `TestResults/ui-tests.trx` under the census's exact names.
- **Red, measured against HEAD's view file** (`0266-red-check-tests.log`):
  - with the new tests as written, the build fails with FS0039 (`runEmission` does not exist);
  - with the tests' `runEmission` lines removed, both facts fail on behaviour. The polarization fact read 1 at
    x = 0 where 1.5 was expected (the default source), and the out-of-range fact still plotted a series;
  - both files were restored and checked against their SHA-256.
- **The fifteen s26 invariants** each find nothing (`0266-dev-invariants.log`).
- `git diff --numstat` equals `--ignore-cr-at-eol` for both edited files: no CRLF churn.
- No run exceeded its deadline.
- **Attempt 02** (the code judge's doc-only route-back; logs `0266-dev-*-attempt-02.log`). Every gate command was
  re-run over the final tree:
  - build: exit 0, 0 warnings, 0 errors, 16 s (600 s), with `OpticalConstructor.Ui` recompiled;
  - elevation: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, 193 s (600 s);
  - constructor-unit-tests: 1429 succeeded, 71 s (180 s);
  - ui-smoke: 245 succeeded, 209 s (600 s);
  - ui-tests: 91 succeeded, 105 s (360 s), both new identities `Passed` in the TRX;
  - the fifteen s26 invariants each find nothing;
  - no CRLF churn, and no run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1429
  ui_smoke_tests: 245
  ui_tests: 91
```

## Architecture

- **`runEmission` lives on the model extension that holds `tryItemModelChoice` and `resolveSlotItem`.** It answers
  `Result<ExperimentRun.SourceEmission, ControlCaption>`:
  - for the first light-source slot holding an item, `resolveSlotItem` then
    `choice.emissionWith (ParameterValues.ofParameters item.parameters)`. An `ItemEvaluationError` or `SetupError`
    answers its `describe`;
  - with no such slot, `SourceEmission.ofValues (ParameterValues.defaultsOf SourcePreset.lightSourceAxes)`, exactly
    the emission `chartForRun` built before.
- **`chartForRun` matches `model.runEmission, slotItems`.** The source leads the light, so its error is told first.
  `Ok emission, Ok items` runs step 265's pipeline unchanged, over that emission.
- **The source item is never a step.** Step 265's `slotItems` excludes the source slot, so the item only emits. Its
  slot's preset binding still sets λ through `boundSource` and `operatingWaveLength`.
- **With no source item, every chart is step 252's.** `FsxAcceptanceTests`, the chart classes and export pass
  unchanged.
- **Both docs state the λ rule in at most three lines, citing spec 0050 S8.2.** The `chartForRun` doc says the run's
  λ stays the bound preset's (`boundSource`, which `withItem` binds for a source item), because `chartForRun` reads λ
  through `operatingWaveLength`. The `runEmission` doc says the same.

## Deferred

Nothing in this step's scope.

## Gotchas

- **`initMain`'s source slot `src` is unbound.** A run without a source item reads 600 nm. Assigning a source item
  binds the slot to the item's preset (`withItem`, step 258), and from then on the run reads that preset's λ.
- **`SetParameterState` accepts an out-of-range fraction.** `Parameter.validate` checks only finiteness, so a
  `degreeOfPolarization` of 1.5 is stored. The run refuses it through `checkedFor` (`ParameterOutOfRangeErr`,
  step 245).
- **An angle parameter is in radians.** Set `azimuth` as `ParameterValue (Angle.degree d).value`, which is the
  tests' `knownDegrees`.
- **A record copy needs parentheses** before a member access: `({ held with kitItems = [] }).experimentResult`.
- **The intensity reference.** A path that opens on an ideal polarizer scales the emission by r = 2, so an unpolarized
  unit source reads S₀ = 1 through it, and a fully polarized one at ψ reads 1 + cos 2(x − ψ).

## Changelog

- 2026-11-10: Step 0266. The source slot's item is the run's source: `model.runEmission` emits through `emissionWith`
  over the item's own parameters, else the default light, and `chartForRun` reads it, showing
  `The run cannot be evaluated` with the reason when the item cannot emit. Two `ExperimentControlsTests` facts were
  added. Constructor 1429, smoke 245, selected UI 91 (+2).
- 2026-11-10: Step 0266, attempt 02 (code-judge route-back). Rewrote the `chartForRun` doc from four lines to three.
  It now names `runEmission` and states that λ stays the bound preset's (`boundSource`), citing spec 0050 S8.2. No
  code or test changed. Every gate command was re-run green: 1429 / 245 / 91.
