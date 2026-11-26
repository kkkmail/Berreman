# Step 0208 — impl log (IMPLEMENT: the parameterised item models as data)

## Progress

- [x] Read the protocol (`arc-runner.system-md`, `implement_worker.system-md`), the project prompt, AGENTS.md,
  slice 0208 and its `.gates`, step 0207's SoW / impl-log, and the 0209 / 0210 / 0213 / 0216 briefs that consume
  these forms.
- [x] Wrote the five facts in `ItemModelsTests.fs` first; red build (below).
- [x] Added the forms, the typed binding and their members to `ItemModels.fs`.
- [x] Development runs: build, constructor unit tests, elevation, the fourteen s22 invariant greps.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemModels.fs`
  - opens `System` (`Math.PI`), `Berreman.Geometry` (`Angle`) and `Berreman.Fields` (`WaveLength`); the module doc
    now also names the item models as data;
  - after step 207's extensions, in this order, each doc citing spec 0050 S7.1:
    - `PolarizerLeakage` = `ExtinctionRatioLeakage | DiattenuationLeakage`;
    - `RetardanceDispersion` = `ConstantRetardance | InverseWaveLengthRetardance of WaveLength`;
    - `BlackBoxNormalisation` = `SixteenElementMueller | FifteenElementsAndScale`;
    - `OpticalForm` = `RealPolarizerModel | RetarderPlateModel | BlackBoxMuellerModel | CompoundModel of
      CompoundComponent list`, `and CompoundComponent = { form : OpticalForm; offset : Angle }`, with
      `member this.parameterAxes`;
    - `ItemModelBindingKind` = `SourcePresetBinding | PolarizerPresetBinding | StackSampleBinding`;
    - `ItemModelBinding` = `SourcePresetBound of SourcePreset | PolarizerPresetBound of PolarizerPreset |
      StackSampleBound of Sample`, with `bindingKind` and `parameterAxes` (which CALLS step 207's
      `SourcePreset.parameterAxes`, `PolarizerPreset.parameterAxes` and `Sample.parameterAxes`);
    - `ItemModelForm` = the brief's eight cases, with `bindingKindOpt`, `parameterAxes` and
      `tryBoundParameterAxes (binding : ItemModelBinding)`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemModelsTests.fs`
  - opens `System`, `Berreman.Geometry`, `Berreman.Fields`;
  - four private fixtures (`quarterWave`, `realLpByExtinction`, `constantRetarder`, `circularPolarizer` — the 001 §I
    LP at 0° then a constant retarder at 45°);
  - the five authorized facts, verbatim names:
    `a real linear polarizer publishes its leakage and its axis offset`,
    `a retarder plate publishes its retardance at a quarter wave and its fast-axis offset`,
    `a compound publishes its components' parameters under their light-order prefixes`,
    `a black-box Mueller matrix publishes sixteen elements, or a scale and fifteen, at the identity`,
    `an empty slot publishes nothing and each detector its gain, dark offset and noise as its form says`.

## Decisions

- **The axes, as tabulated.** Real LP: `extinctionRatio` (fraction, 0) or `diattenuation` (fraction, 1), then
  `axisOffset` (angle, 0). Retarder, under either dispersion: `retardance` (phase, π / 2), `fastAxisOffset`
  (angle, 0). Black box: `m00` … `m33` row-major (Mueller element, 1 on the diagonal, 0 elsewhere), or `scale`
  (scale, 1) followed by the fifteen without `m00`. Compound: component k's axes renamed `c<k>.<name>`, kind and
  default kept, recursively (`c2.c1.extinctionRatio`). Detectors: the intensity detector `gain` (scale, 1),
  `darkOffset` (reading, 0), then its noise list unchanged; the polarimeter and the ellipsometer the noise list.
  `OpticalElementModel f` is `f`'s axes; `EmptySlotModel` publishes nothing.
- **The typed binding (the brief's "MUST declare the corresponding typed binding kind and derive their bound
  parameter axes by calling … step 207's members").** The binding forms are nullary, as the brief lists them,
  because they are serialized library payloads (step 210) and cannot hold the preset. So:
  - `ItemModelForm.bindingKindOpt` declares the kind each form requires (`None` for a standalone form);
  - `ItemModelBinding` is the resolved preset or sample, and its `parameterAxes` calls step 207's members (no
    copy);
  - `ItemModelForm.tryBoundParameterAxes` answers the form's own axes followed by the binding's when the binding
    is of the form's kind, and `None` for a mismatched binding or a standalone form.
  - The binding forms' unbound `parameterAxes` is `[]` (no binding-dependent axis).
  - Step 216's `ItemModelChoice` resolves the choice and can use `bindingKindOpt` for its form-matches-binding
    check and `tryBoundParameterAxes` for its axes.
  - These two types and two members are the smallest typed shape that meets the brief. They are not in the
    contract list, and no other symbol was added.
- **Coverage of the binding forms extends the authorized fifth fact** (the "publishes nothing" fact). No new test
  identity is added. The fact asserts:
  - each binding form publishes `[]` unbound and declares its kind;
  - a standalone form declares none;
  - each binding form over a binding of its kind answers exactly that entry's step-207 axes (heNe633, the Built-in
    LP, the 1 mm glass plate);
  - a mismatched binding and a standalone form answer `None`.
- **Fact 5 builds its noise list with step 206's `detectorNoiseAxes`** over the module's three test-local
  `ParameterValue`s (`noiseRotation`, `noiseElement`, `noiseThroughput`). Every detector's axes end with that
  list unchanged, asserted by `=` against it.
- **π / 2 is written `ParameterValue (Math.PI / 2.0)`** at its origin, the record field. `Angle.halfPi` is an
  angle and a retardance is a phase, so no cross-type conversion was introduced. `ItemModels.fs` opens `System`
  for `Math`; step 209's brief opens it for `Guid` anyway.
- **The Mueller names come from a comprehension inside the member** (`for row in 0 .. 3 do for column in 0 .. 3`).
  There is no `(row, column)` tuple (ELEV011) and no local function over `int` (ELEV003P). The scale case
  replaces the row-major head `m00` with `scale` (`:: List.tail elements`).
- **The compound prefix uses `axis.name.value`** in the interpolation, the `ParameterName` accessor, rather
  than destructuring the single-case union outside it (AGENTS.md).

## Testing state

These are development runs only. The gate engine gives the verdict (Invariant 6).

- **Red:** `dotnet build OpticalConstructor.Tests.fsproj -c Release`, run before the domain change, fails with
  FS0039 (`'OpticalForm'`, `'RealPolarizerModel'`, `'CompoundModel'`, `'OpticalElementModel'`,
  `'BlackBoxMuellerModel'` … not defined). Log: `.artifacts/0208-red-build.log`.
- **build:** `dotnet build Berreman.slnx -c Release -nologo -v:m` → exit 0, 0 warnings, 0 errors (52 s).
- **constructor-unit-tests:** 1275 passed, 0 failed, 0 skipped (1270 → 1275, +5). All eleven `ItemModelsTests`
  identities are `Passed` in the TRX, the five new ones included.
- **The fourteen `invariant-s22-*` greps:** each finds nothing.
- **elevation** (run from the arc origin, `ASG_ARC_BASE_COMMIT=f395597c…` per `.arc-base-json`): `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, exit 0, 201 s, ELEV004 excluded, known findings 0. The arc's `Berreman.slnx` change widens the scope to all 12 projects.

## Artifacts

- `specs/0050/.artifacts/0208-red-build.log`
- `specs/0050/.artifacts/0208-build.log`
- `specs/0050/.artifacts/0208-constructor-unit-tests.log`
- `specs/0050/.artifacts/0208-dev-elevation.log`

## Gotchas

- **The brief names `Berreman.Fields` as the file's new open, but `Angle` (the compound offset) lives in
  `Berreman.Geometry`.** The file opens both, in the same order as `ElementId.fs`. `Library` is opened after
  them, so its names win any clash.
- **`ParameterKind`'s doc lists the axis kinds; this step uses `ReadingParameter` for the first time**, for the
  dark offset.
- **The unbound axes of a binding form are `[]` by design.** Use `tryBoundParameterAxes` with the resolved
  binding to get the axes an item instantiates. Reading `parameterAxes` alone silently drops the source, polarizer
  or sample axes.
- **Axis names are identities.** The compound prefix (`c<k>.`) is 1-based in light order. Reordering a
  compound's components renames its parameters.
