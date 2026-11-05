# Step 0023 — CLOSE_RED — impl log

## Progress

- 2026-11-05: read the protocol, the project prompt, the slice, the gate snapshot and step 22's
  SoW and probe log. Callers already map any converter exception to `MalformedJsonErr`
  (`ProjectJson.fs`, `GroupsLibrary.fs`, `Report.fs`, `UserEnvironment.fs` catch `e`). No committed
  JSON (`data/`, fixtures, schemas) holds the old four-property `Complex` form (`git grep`).
- Edited `CanonicalJson.fs`: added `ComplexConverter` (`[re, im]`) and `RealMatrixConverter`
  (rows of numbers, read back through `Matrix<double>.Build.DenseOfArray`), registered both before the
  `JsonFSharpConverter`, and named them in the options' doc. `ComplexMatrixConverter` now writes and
  reads each cell through the shared options, so through `ComplexConverter` (see Decisions).
- Build: exit 0, 0 errors. Constructor suite whole: 742/742 passed, the three RED_0005 facts among
  them. Malformed-input probe: every bad shape raises `JsonException`. The six s04 invariants find
  nothing.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/CanonicalJson.fs`: `ComplexConverter`,
  `RealMatrixConverter`, their registration and the options' doc; `ComplexMatrixConverter`'s cell
  write and read go through `ComplexConverter`. This is the step's only authorized edit, and no other
  product or test file changed.

## Decisions

- **`ComplexMatrixConverter`'s cells go through `ComplexConverter`.** Before this step it wrote and
  read `[re, im]` inline. With `ComplexConverter` added, that form would have had two writers and two
  readers. AGENTS.md "Reuse, never copy" calls a second parser for one format a defect, and says the
  change that adds code at its new home deletes it at the old one. The edit is four lines in the
  authorized file. The `Matrix<Complex>` wire form is byte-identical (`[[[1, 2], [3, -4]]]` in the
  probe), and the project round-trip and schema facts stay green.
- **Both reads parse their numbers through System.Text.Json** (`Deserialize<float[]>`,
  `Deserialize<float[][]>` over the same options), not a hand-walked `JsonDocument`. A non-array or
  non-number therefore already raises `JsonException` with its JSON path. The converter raises
  `JsonException` for the shapes STJ accepts but the type does not: a pair that is not two numbers,
  ragged or null rows. Callers map any of these to `MalformedJsonErr` (they catch `e`).
- **The shape checks stay inside the override members.** Members are exempt from ELEV003/003P
  (`NoPrimitivesOutsideBoundary.fs`: `m.IsMember` / `IsOverrideOrExplicitInterfaceImplementation`).
  No free or private function with a `float` signature was added.
- **Registration order.** `ComplexConverter`, `ComplexMatrixConverter` and `RealMatrixConverter` come
  first, then the four union/boolean converters, then `JsonFSharpConverter` last. Each converter
  claims one distinct type, so only "before `JsonFSharpConverter`" matters.

## Testing state

These are local observations; the arc-runner's gate engine is the authority.

- `build` (`dotnet build Berreman.slnx -c Release -nologo -v:m`): exit 0, `0 Error(s)`. The two
  warnings are `SYSLIB0051` in the vendored `MathNetNumerics/Numerics/Exceptions.cs` (C#, third-party
  source), which recompiled in this run. Steps 0002, 0003 and 0006 logged the same warning. No
  warning comes from our code.
- `constructor-unit-tests`, whole suite (the gate's own command, no selector): `Test run summary:
  Passed!`, total 742, failed 0, succeeded 742, skipped 0. The TRX marks all three RED_0005 facts
  `Passed`:
  - `a complex number survives the shared options as its real and imaginary parts`;
  - `a real matrix survives the shared options as its rows`;
  - `an absorbing built-in material survives the shared options`.

  Step 22 measured 739 succeeded with these 3 red, so the suite is 739 + 3, and no fact was edited.
- `elevation` (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`): `ELEV-GATE: OK - 0 new finding(s) in
  5 project(s)`. It ran over Storage plus its dependents App, TestWindows.App, TestWindows and Ui.
  Known findings: 0.
- The six s04 invariants (`ELEV0` in the known-findings list, VSTest packages, `kit.kit-json`,
  `EpsAxisEvaluated`, `complexity = (None|Some)`, the seeded string preset ids): each `git grep`
  finds nothing.
- Malformed-input probe through `CanonicalJson.options`. Each of these raises `JsonException`:
  - for `Complex`: `null`, one number, three numbers, an object, a string cell;
  - for `Matrix<double>`: ragged rows, a null row, a null first row, a string cell, a bare number.

  `[]` reads as a 0x0 matrix and `[[1,2,3],[4,5,6]]` as 2x3. `Matrix<Complex>` writes
  `[[[1, 2], [3, -4]]]`, unchanged.

## Artifacts

- `.artifacts/0023-build.log`: the build.
- `.artifacts/0023-constructor-unit-tests-whole.log`: the whole constructor suite.
- `.artifacts/0023-elevation-local.log`: the ELEV gate over the five projects.
- `.artifacts/0023-malformed-probe.log`: the probe script, inlined, and its output. The script was
  deleted after the run and is not product code.

## Gotchas

- STJ reads an out-of-range number such as `1e400` as infinity. That is its default for every
  `float` in the app, not something this step introduced.
- A `JsonConverter` for a struct (`Complex`) is handed a JSON `null`, while one for a reference type
  is not. `Deserialize<float[]>` returns null there, and the `[| re; im |]` pattern rejects it.
- `ComplexMatrixConverter` still sizes its array from the first row. A ragged complex matrix behaves
  as before: a longer row throws (mapped to `MalformedJsonErr`), and a shorter one is zero-filled.
  Only its cells are in this step's scope.
- The working tree also shows `specs/0050/.manifest.state.json` modified. That is the supervisor's
  file, and this worker did not touch it.
