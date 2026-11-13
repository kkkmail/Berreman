# Step 0346 — impl plan

## Approach

A pure MOVE of the absorbing ground truth and its measurement design into `BerremanTestSupport`, beside its
transparent twin `BiaxialSample` (moved in step 0345), per 001 §Q AT-2 and spec 0050 S10.2.

1. Move `Berreman/BerremanTests/AbsorbingBiaxialSample.fs` to `Berreman/BerremanTestSupport/AbsorbingBiaxialSample.fs`
   with a plain `mv` (the worker protocol forbids git-mutation commands, so not `git mv`; git detects the rename at
   commit time, as in steps 0344 and 0345).
2. In the moved file: namespace `BerremanTests` → `BerremanTestSupport`; delete `open BerremanTests.InverseFitHarness`
   (nothing in the file reads the harness: it exports `expectOk`, `describeErrors`, `describeColumnNorms`,
   `assertMuellerEqual`, `ensembleSeeds`, none used here); keep `open BerremanTestSupport.BiaxialSample`; append one
   S10.2 sentence to the first doc paragraph. Every declaration and body unchanged (`buildAbsorbing` stays
   `p.opticalProperties`, the `AbsorbingTriclinicParameters` member over the step-334 kernel).
3. `BerremanTestSupport.fsproj`: `<Compile Include="AbsorbingBiaxialSample.fs" />` after `BiaxialSample.fs`. No
   reference, no comment (Domain and Optimization are already referenced).
4. `BerremanTests.fsproj`: delete `<Compile Include="AbsorbingBiaxialSample.fs" />`.
5. `AbsorbingBiaxialInverseTests.fs`, `AbsorbingBiaxialNoisyTests.fs`: `open BerremanTests.AbsorbingBiaxialSample` →
   `open BerremanTestSupport.AbsorbingBiaxialSample`, trailing comment kept.

## Files

All within the step's authorized `edits`: the moved source (both paths), the two project files, the two absorbing suites.

## Risks

- A symbol in the sample that actually resolves through a `BerremanTests` module would fail to compile in
  `BerremanTestSupport` (which references no test project). Checked by reading the harness's exports; the build confirms.
- Compile order: `AbsorbingBiaxialSample.fs` must follow `BiaxialSample.fs` (it opens it).
- LF line endings preserved; no CRLF churn.
