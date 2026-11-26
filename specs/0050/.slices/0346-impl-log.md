# Step 0346 — impl log

## Progress

- Read the task, both system prompts, the project prompt, AGENTS.md and the slice. No operator note, no
  `## Operator rulings for this step` section; none of the standing rulings in the project prompt names step 0346
  (0013's arc-origin elevation scope applies as always).
- Moved `Berreman/BerremanTests/AbsorbingBiaxialSample.fs` to `Berreman/BerremanTestSupport/AbsorbingBiaxialSample.fs`
  (plain `mv`, see Gotchas). `diff` against `HEAD:Berreman/BerremanTests/AbsorbingBiaxialSample.fs` shows exactly three
  hunks: `namespace BerremanTests` → `namespace BerremanTestSupport`; `open BerremanTests.InverseFitHarness` deleted
  (the harness exports `expectOk`, `describeErrors`, `describeColumnNorms`, `assertMuellerEqual` and `ensembleSeeds`,
  none of which the file reads); and two doc lines appended to the first doc paragraph, after "…and the measurement
  set.": "Spec 0050 S10.2 (001 §Q AT-2): the sample and its design live in `BerremanTestSupport`, so one compiled /
  copy serves `BerremanTests` and `OpticalConstructor.Ui.Tests`." Every other doc line and every declaration and body
  is unchanged: `epsIm11..epsIm12`, `absorbing`, `buildAbsorbing` (still `p.opticalProperties`, the
  `AbsorbingTriclinicParameters` member over the step-334 kernel), `absorbingScalingAround`, `plate`, `waveLength`,
  `opticAxisCut`, `otherOpticAxisCut`, `configuration` (`float<deg>` into `configurationOf`), `a1`, `a2`, `a3`,
  `fullConfigurations` (`a1 @ a2 @ a3`, 8 + 12 + 24 = 44 in the same order), `perturbedStart`, `searchBox`
  (`SearchBox`, `MuellerFitting`'s). `open BerremanTestSupport.BiaxialSample` kept.
- `BerremanTestSupport.fsproj`: `<Compile Include="AbsorbingBiaxialSample.fs" />` after `BiaxialSample.fs`. No
  reference and no comment added.
- `BerremanTests.fsproj`: `<Compile Include="AbsorbingBiaxialSample.fs" />` deleted; its compile list now holds no
  ground-truth file.
- `open BerremanTests.AbsorbingBiaxialSample` → `open BerremanTestSupport.AbsorbingBiaxialSample` in
  `AbsorbingBiaxialInverseTests.fs` and `AbsorbingBiaxialNoisyTests.fs`, trailing comments kept (the comment column
  held by taking six spaces out of the padding). Nothing else in them changed (one line each). No qualified
  `AbsorbingBiaxialSample.x` use exists anywhere under `Berreman/`. No test added or removed.

## Files modified

- `Berreman/BerremanTestSupport/AbsorbingBiaxialSample.fs` (moved from `Berreman/BerremanTests/AbsorbingBiaxialSample.fs`)
- `Berreman/BerremanTests/AbsorbingBiaxialSample.fs` (deleted by the move)
- `Berreman/BerremanTestSupport/BerremanTestSupport.fsproj`
- `Berreman/BerremanTests/BerremanTests.fsproj`
- `Berreman/BerremanTests/AbsorbingBiaxialInverseTests.fs`
- `Berreman/BerremanTests/AbsorbingBiaxialNoisyTests.fs`

All within the step's authorized `edits`. `git diff --numstat` equals `--ignore-cr-at-eol`, and the moved file, both
project files and both suites hold no CR (LF only).

## Testing state

- build: `dotnet build Berreman.slnx -c Release -nologo -v:m` from `Berreman/` — exit 0, `0 Error(s)`, 78 s
  (deadline 600 s). `6 Warning(s)`, the same set as steps 0344 and 0345: NU1902 (exempt advisory) on
  BerremanTestSupport and BerremanTests, and SYSLIB0051 from the vendored `MathNetNumerics/Numerics/Exceptions.cs`
  (pre-existing; this step does not touch MathNet).
- s34 invariants: all 20 `git grep` commands (decoded from the snapshot) find nothing, run both as written and with
  `--untracked` so the not-yet-tracked moved file is searched too.
- Acceptance structure: `module AbsorbingBiaxialSample` is declared once, in
  `Berreman/BerremanTestSupport/AbsorbingBiaxialSample.fs` (namespace `BerremanTestSupport`), with no `open` of a
  `BerremanTests` module; `Berreman/BerremanTests/` holds neither `BiaxialSample.fs` nor `AbsorbingBiaxialSample.fs`
  and compiles neither; both absorbing suites open `BerremanTestSupport.AbsorbingBiaxialSample`;
  `buildAbsorbing` is `p.opticalProperties`, i.e. `AbsorbingTriclinicParameters.opticalProperties`
  (`OpticalConstructor.Domain/MuellerInverse.fs:435`); only `BerremanTests.fsproj` references `BerremanTestSupport`
  and no `.fs` outside `BerremanTests` / `BerremanTestSupport` names it.
- elevation: `Run-ElevationGate.ps1 -ExcludeRules ELEV004` (standalone, so all 13 in-scope projects) —
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0, exit 0, 223 s (deadline 600 s);
  `BerremanTestSupport.fsproj findings=0`, every project `findings=0`.
- unit-tests (selected `BerremanTests.AbsorbingBiaxialInverseTests`, the gate's own command and selector):
  `Test run summary: Passed!`, total 7, succeeded 7, failed 0, skipped 0; the TRX lists the seven
  `change_expectation` facts of the step's effects, all `Passed`. 193 s wall clock (deadline 7200 s); it ran
  concurrently with the elevation run. No assertion or tolerance changed, so the passing facts are the unchanged
  numbers. `AbsorbingBiaxialNoisyTests` (946 s) is not selected; it compiles against the moved module.

## Artifacts

- `specs/0050/.artifacts/0346-build.log`
- `specs/0050/.artifacts/0346-invariants.log`
- `specs/0050/.artifacts/0346-elevation.log`
- `specs/0050/.artifacts/0346-unit-tests.log`

## Gotchas

- The brief says `git mv`; the worker protocol forbids every git-mutation command, so the file was moved with a
  plain `mv` (as steps 0344 and 0345 did). The working tree is identical; git sees a deletion plus an untracked file
  until commit, where the rename is detected.
- The S10.2 doc sentence was added as two new doc lines after the paragraph's last line rather than appended to it,
  so no existing doc line was reflowed ("Every other doc line keeps its text").
- The mandated sentence names `OpticalConstructor.Ui.Tests` as a consumer; that project does not reference
  `BerremanTestSupport` yet and is outside this step's edits (the same wording step 0345 used).
- The new `open` is six characters longer; six spaces came out of its padding so the trailing comment keeps its
  column.
