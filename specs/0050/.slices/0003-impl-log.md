# Step 0003 — IMPLEMENT — impl log

## Progress

- Read the protocol (arc-runner.system-md, implement_worker.system-md), the project prompt, the
  slice, its `.gates` snapshot, `AGENTS.md`, step 2's SoW / impl-log, `.spec-md` §3,
  `Dispersion.fs:200-330` and `DispersionModels.fs:560-780`.
- Name census: `AbscissaKind`, `ReciprocalAbscissa`, `LinearInX`, `ReciprocalInX` match nothing
  in any `*.fs` under `C:\GitHub\Softellect`; inside `Berreman/` only the Domain's private type
  used them, and it is deleted in this step. So ELEV014/016 hold.
- Edited `Dispersion.fs`: after `ComplexDispersionFormula`, the carrier record
  `ReciprocalAbscissa` (`wavelengthScale : float<meter>`, `numerator : float`), then the public
  union `AbscissaKind` (`LinearInX of wavelengthScale : float<meter>` /
  `ReciprocalInX of ReciprocalAbscissa`). The record comes first because the union names it. The
  Domain type's doc comment moved with it. Its `Dispersion.fs:238` line reference is now the name
  `DispersionFormula.wavelengthScale`, because the comment sits in that file, and a line closes
  it citing spec 0050 §3.1.
- Edited `DispersionModels.fs`: deleted the private `AbscissaKind`. `ofUnit` is now
  `type AbscissaKind with static member ofUnit (u : UnitOfMeasure) : AbscissaKind` at the same
  place, before its first use, with its doc comment and body unchanged except
  `ReciprocalInX { wavelengthScale = nanometer; numerator = (fromMeters u nanometer).value }`.
  The five lowering sites (ConstantNK, Cauchy, Sellmeier, Lorentz, Drude) match
  `ReciprocalInX reciprocal`. They read `reciprocal.wavelengthScale` where they read `scale` and
  `reciprocal.numerator` where they read `numerator.value`.

## Files modified

- `Berreman/Berreman/Dispersion.fs` (+19: the two new types; the UTF-8 BOM is kept)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs` (+16 / −22)

No test file is touched. All LF: `git diff --numstat` equals `git diff --ignore-cr-at-eol --numstat`.

## Testing state

Observed locally as development checks. The arc-runner's gate engine runs and records the gates.

- `dotnet build Berreman.slnx -c Release -nologo -v:m`: exit 0, 0 errors. Every project rebuilt,
  the core and the Domain included. The only warnings are the vendored
  `MathNetNumerics/Numerics/Exceptions.cs` SYSLIB0051 pair already on the start tree.
- `constructor-unit-tests`, whole suite: `Test run summary: Passed!`, 732 / 732. The 29
  `DispersionModelsTests` facts (the AC-B5 lowering grid among them) all pass with no edit.
- `unit-tests`, filtered to `BerremanTests.EpsWithDispValueTests` and
  `BerremanTests.DispersionFormulaTests`: 21 / 21.
- `ui-smoke` (`Category=ui-smoke`): 208 / 208.
- `ui-tests`, filtered to `MaterialEditorWindowTests` and `NkDispersionChartTests`: 41 / 41.
- `elevation` (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`): `ELEV-GATE: OK - 0 new finding(s)
  in 11 project(s)`, known findings 0.
- The invariants `s01-elev-list-empty`, `s01-no-vstest` and `s01-no-kit-kit-json`: each
  `git grep` finds nothing.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0003-build.log`
- `0003-constructor-unit-tests.log`
- `0003-unit-tests.log`
- `0003-ui-smoke.log`
- `0003-ui-tests.log`
- `0003-elevation.log`

## Gotchas

- The slice names the match variable `r`. It is `reciprocal` in the code, because the Lorentz
  branch's `oscillator` already takes a parameter `r` (the resonance), which would shadow it
  inside the lambda. The reads are exactly the ones the slice names: `.wavelengthScale` and
  `.numerator`.
- `ofUnit` is public, as the slice's signature has it. Before, it was effectively private,
  because its type was private.
- `ReciprocalAbscissa` is the third record with a `wavelengthScale` label. Every existing
  construction and read is typed by its context, so nothing downstream changed inference. The
  build proves it.
