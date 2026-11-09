# Step 0206 — impl-log (IMPLEMENT: the detector noise axes in `ItemModels`)

## Progress

- [x] Red first.
  - The new fact in `OpticalConstructor.Tests/ItemModelsTests.fs` was written before the production types.
  - The solution build failed only in `OpticalConstructor.Tests`: `FS0039` on `ParameterValue` (18–20),
    `ItemParameterAxis` (22,20), `ParameterKind` (27, 34–36) and each of the seven cases (31), plus the `FS0072`
    inference errors they cause. No other error. Log: `.artifacts/0206-dev-red-build.log`.
- [x] `OpticalConstructor.Domain/ItemModels.fs`, after `ParameterName` and `DialSign` (step 205):
  - `ParameterValue of double` with `.value : double`, one parameter's number in its kind's unit;
  - `ParameterKind`, seven cases in the slice's order (`AngleParameter`, `PhaseParameter`, `FractionParameter`,
    `ScaleParameter`, `MuellerElementParameter`, `ThicknessParameter`, `ReadingParameter`), each with a one-line
    doc giving its unit and examples; `.value : int` = 1…7 in declaration order;
    `static member tryOfWire (tag : int) : ParameterKind option` (0 and any other tag → `None`); and a separate
    `.text : string` render member (`"angle"`, `"phase"`, `"fraction"`, `"scale"`, `"Mueller element"`,
    `"thickness"`, `"reading"`);
  - `ItemParameterAxis = { name : ParameterName; kind : ParameterKind; defaultValue : ParameterValue }` with
    `static member detectorNoiseAxes (rotation : ParameterValue) (element : ParameterValue)
    (throughput : ParameterValue) : ItemParameterAxis list`, returning `rotationUncertainty` (`AngleParameter`),
    `elementUncertainty` and `throughputUncertainty` (`FractionParameter`) with the supplied values, in that order.
- [x] Test `OpticalConstructor.Tests.ItemModelsTests.the detector noise axes keep their declared quantities`:
  - test-local `ParameterValue 0.001` (radians), `0.02`, `0.03`;
  - asserts the axes' names, kinds and exact defaults, in order;
  - asserts tags 1…7 in declaration order, every tag round-trips through `tryOfWire`, and 0 and 8 read as `None`;
  - asserts the seven `.text` renderings (the text is not the tag).
- [x] Development runs (below).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemModels.fs`: `ParameterValue`, `ParameterKind`,
  `ItemParameterAxis` (+78 lines, nothing else changed).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemModelsTests.fs`: the new fact; the module's doc line now
  says it covers the item-model vocabulary, not only the step-205 move.

No project file changed: both files are already compiled (step 205).

## Decisions

- **`.text` is the separate rendering the slice requires** ("its text rendering MUST remain separate from its
  tag"; ELEV017's honest render member). The fact asserts it, so the member is not untested surface.
- **No `ParameterKind.all`.** The fact lists the seven kinds itself (as `LibraryEntryFilesTests` does for its kinds
  outside `all`'s own assertion); the slice's contracts name only `value` and `tryOfWire` on the type.
- **The axis names are the camelCase identifiers the slice gives** (`rotationUncertainty`, `elementUncertainty`,
  `throughputUncertainty`), wrapped where they are born (the literal inside the record).
- **No detector model type is added.** The slice's effects authorize only the vocabulary in `ItemModels.fs`; the
  item models that carry the axis list are S7 steps. The type's doc states that contract.
- **Domain names no bench-noise type**, in code or comments: `git grep --untracked -E
  "AngleUncertainty|ElementUncertainty|ThroughputUncertainty|NoiseParam" -- …/OpticalConstructor.Domain/` finds
  nothing. Their sole declarations stay in `BerremanTests/InverseFitHarness.fs` (123, 141, 159, 172). No harness
  constant (`decentLab`'s 0.2°, 0.005, 1 %) is copied: the test uses its own values.
- **Exact float comparison in the fact** (`Assert.Equal<float list>`): the member must preserve the supplied values,
  so equality is exact by contract; no tolerance helper applies.

## Testing state

Development runs only; the gate engine gives the verdict (Invariant 6).

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`): exit 0, 0 warnings, 0 errors.
  Log: `.artifacts/0206-dev-build.log`.
- **constructor-unit-tests** (the gate's command, whole suite): `Test run summary: Passed!`, total 1266, succeeded
  1266, failed 0, skipped 0 (1265 → 1266, +1). In the TRX,
  `OpticalConstructor.Tests.ItemModelsTests.the detector noise axes keep their declared quantities` — Passed.
  Log: `.artifacts/0206-dev-constructor-unit-tests.log`.
- **elevation**: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, exit 0, 187 s, ELEV004 excluded, known
  findings 0. Run with `ASG_ARC_BASE_COMMIT=f395597c…` (`.arc-base-json`); the arc's `Berreman.slnx` change widens
  the scope to all 12 projects. Log: `.artifacts/0206-dev-elevation.log`.
- **The fourteen `invariant-s22-*` greps**: each finds nothing (0 lines each).
- **Declarations**: `type (ParameterValue|ParameterKind|ItemParameterAxis) ` is found only in `ItemModels.fs` (29, 36,
  86); none of the ten new names (three types, seven cases) exists elsewhere in Berreman or in STL's `.fs` sources.
- **Line endings**: both touched files are LF (`git diff --numstat` equals `--ignore-cr-at-eol`; no CR in either).

## Artifacts

- `specs/0050/.artifacts/0206-dev-red-build.log`
- `specs/0050/.artifacts/0206-dev-build.log`
- `specs/0050/.artifacts/0206-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0206-dev-elevation.log`

## Gotchas

- `specs/0050/.manifest.state.json` was already modified at the start of the round (supervisor state); it was not
  touched.
- The red solution build recompiled the vendored `MathNetNumerics/Numerics` and printed its two `SYSLIB0051`
  warnings (`Exceptions.cs` 24, 77). The green solution build after it printed none (0 warnings).
