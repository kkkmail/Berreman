# Step 0345 — impl log

## Progress

- Read the task, both system prompts, the project prompt, AGENTS.md and the slice. No operator note, no
  `## Operator rulings for this step` section; none of the standing rulings in the project prompt names step 0345
  (0013's arc-origin elevation scope applies as always).
- Moved `Berreman/BerremanTests/BiaxialSample.fs` to `Berreman/BerremanTestSupport/BiaxialSample.fs` (plain `mv`,
  see Gotchas). Namespace `BerremanTests` → `BerremanTestSupport`. Opens and every declaration and body unchanged:
  `referenceWaveLength`, `index1..3`, `g11..g12`, `triclinic`, `buildTriclinic` (still
  `p.opticalProperties`, the `TriclinicParameters` member over the step-333 kernel), `solverParameters`,
  `scalingAround`, `opticAxisFromLargest`, `opticAxisWaveNormal`, `epsDiagonal`, `rhoImaginary`,
  `recoveredQuantities`. `diff` against `HEAD:Berreman/BerremanTests/BiaxialSample.fs` shows exactly four hunks:
  the namespace line, "every biaxial suite in this assembly shares" → "every biaxial suite shares", the one
  appended S10.2 sentence ending the first doc paragraph ("Spec 0050 S10.2 (001 §Q AT-2): the sample lives in
  `BerremanTestSupport`, so one compiled copy serves `BerremanTests` and `OpticalConstructor.Ui.Tests`."), and
  "in the TEST assembly" → "in test code". No other doc line changed and no line was reflowed.
- `BerremanTestSupport.fsproj`: `<Compile Include="BiaxialSample.fs" />` after `CommittedKits.fs`; project
  reference to `..\OpticalConstructor\OpticalConstructor.Optimization\OpticalConstructor.Optimization.fsproj`
  after the Domain one (Optimization references only Domain, so there is no cycle). No comment added.
- `BerremanTests.fsproj`: deleted `<Compile Include="BiaxialSample.fs" />` and the comment above it, and the
  comment above `<Compile Include="InverseFitHarness.fs" />`; that line keeps its place (after
  `MuellerReconstructionTests.fs`, before `MuellerInverseTests.fs`).
- `open BerremanTests.BiaxialSample` → `open BerremanTestSupport.BiaxialSample`, trailing comments kept, in
  `BiaxialInverseTests.fs`, `FeasibleBiaxialInverseTests.fs`, `AbsorbingBiaxialInverseTests.fs`,
  `AbsorbingBiaxialNoisyTests.fs`, `AbsorbingBiaxialSample.fs`. Nothing else in them changed (one line each).
- No qualified `BiaxialSample.x` use exists in code (only in doc comments), so the opens are the whole consumer
  change. No test added or removed.

## Files modified

- `Berreman/BerremanTestSupport/BiaxialSample.fs` (moved from `Berreman/BerremanTests/BiaxialSample.fs`)
- `Berreman/BerremanTests/BiaxialSample.fs` (deleted by the move)
- `Berreman/BerremanTestSupport/BerremanTestSupport.fsproj`
- `Berreman/BerremanTests/BerremanTests.fsproj`
- `Berreman/BerremanTests/BiaxialInverseTests.fs`
- `Berreman/BerremanTests/FeasibleBiaxialInverseTests.fs`
- `Berreman/BerremanTests/AbsorbingBiaxialInverseTests.fs`
- `Berreman/BerremanTests/AbsorbingBiaxialNoisyTests.fs`
- `Berreman/BerremanTests/AbsorbingBiaxialSample.fs`

All within the step's authorized `edits`. `git diff --numstat` equals `--ignore-cr-at-eol`, and the new file and
both project files hold no CR (LF only).

## Testing state

- build: `dotnet build Berreman.slnx -c Release -nologo -v:m` from `Berreman/` — exit 0, `0 Error(s)`, 77 s
  (deadline 600 s). `6 Warning(s)`, the same set as step 0344's build: NU1902 (exempt advisory) on
  BerremanTestSupport and BerremanTests, and SYSLIB0051 from the vendored `MathNetNumerics/Numerics/Exceptions.cs`
  (pre-existing; this step does not touch MathNet). `BerremanTestSupport -> ...\bin\x64\Release\net10.0\BerremanTestSupport.dll`.
- s34 invariants: all 20 `git grep` commands (decoded from the snapshot) find nothing, run both as written and
  with `--untracked` so the not-yet-tracked moved file is searched too.
- Acceptance structure: `module BiaxialSample` is declared once, in `Berreman/BerremanTestSupport/BiaxialSample.fs`;
  `Berreman/BerremanTests/BiaxialSample.fs` is absent and not compiled; only `BerremanTests.fsproj` references
  `BerremanTestSupport` (no product project does); `buildTriclinic` is `p.opticalProperties`
  (`OpticalConstructor.Domain/MuellerInverse.fs`, `TriclinicParameters.opticalProperties`).
- elevation: `Run-ElevationGate.ps1 -ExcludeRules ELEV004` (standalone, so all 13 in-scope projects) —
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0, exit 0, 217 s (deadline 600 s);
  `BerremanTestSupport.fsproj findings=0`.
- unit-tests (selected `BerremanTests.BiaxialInverseTests`, `BerremanTests.FeasibleBiaxialInverseTests`, the
  gate's own command and selector): `Test run summary: Passed!`, total 16, succeeded 16, failed 0, skipped 0;
  TRX: 11 `BiaxialInverseTests` + 5 `FeasibleBiaxialInverseTests`, all `Passed`. 1575 s wall clock (deadline
  7200 s); it ran concurrently with the elevation run for its first ~3.5 min. No assertion or tolerance changed,
  so the passing facts are the unchanged numbers.

## Artifacts

- `specs/0050/.artifacts/0345-build.log`
- `specs/0050/.artifacts/0345-invariants.log`
- `specs/0050/.artifacts/0345-elevation.log`
- `specs/0050/.artifacts/0345-unit-tests.log`

## Gotchas

- The brief says `git mv`; the worker protocol forbids every git-mutation command, so the file was moved with a
  plain `mv` (as step 0344 did). The working tree is identical; git detects the rename at commit time.
- The brief's line numbers (`BiaxialSample.fs:13`, `:23`) are two lines off the file as committed (15, 25); the
  quoted text located each edit unambiguously.
- The mandated doc sentence says one compiled copy serves `OpticalConstructor.Ui.Tests`; that project does not
  reference `BerremanTestSupport` yet and this step's `edits` do not include it, so the sentence was written as
  the brief words it.
