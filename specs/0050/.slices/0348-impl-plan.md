# Step 0348 — impl plan

## Approach

001 §Q AT-1 / §S10 / the S10.3 brief: the reference pipeline's transcribed expectations get one home in test
support. This is a MOVE: a new `BerremanTestSupport.MuellerReference` module holds every expected literal of the
Stage-1 and section 7.2 facts, plus the two acceptance bands and the excluded point, and
`MuellerReconstructionTests` reads them as `MuellerReference.<name>`, deleting its own copies in the same step.

1. `Berreman/BerremanTestSupport/MuellerReference.fs` (new): `module MuellerReference` in
   `namespace BerremanTestSupport`, shaped like `BiaxialSample.fs`. In order:
   - (a) `stage1Tol` (1.0e-12) and `dataMatrixTol` (1.0e-9), their docs and the spec 0044 §8.5 / §9 comment block;
   - (b) the six Stage-1 constants, each born elevated (`DarkMean`, `Angle.degree`, `Retardance.degree`);
   - (c) `qzMatrix`, `lrMatrix`, `combinedMatrix` (`MuellerMatrix`), the three cascade metrics
     (`MatrixDeviation`) and `cascadeLargestDeltaPositions` (`Set<MuellerRowIndex * MuellerColumnIndex>`);
   - (d) `excludedPoints : ExcludedPoint list`.
2. `BerremanTestSupport.fsproj`: one `<Compile Include="MuellerReference.fs" />` after `AbsorbingBiaxialSample.fs`.
3. `MuellerReconstructionTests.fs`: delete `stage1Tol`, `dataMatrixTol` and their comment block; the Stage-1 fact,
   the `buildDesign` fact and the section 7.2 fact read the module; the class doc's §8.5 bullet names the module.

## Files

- `Berreman/BerremanTestSupport/MuellerReference.fs` (new)
- `Berreman/BerremanTestSupport/BerremanTestSupport.fsproj`
- `Berreman/BerremanTests/MuellerReconstructionTests.fs`

## Risks

- Bit-identity: each degree literal must come back unchanged through `Angle.degree … |> .degrees` and
  `Retardance.degree … |> .degrees`. Checked with `dotnet fsi` (`.artifacts/0348-roundtrip-check.log`): all five
  are exact. The matrices and metrics are the same double literals.
- Name clashes: `MuellerReference` exists nowhere else in `Berreman/`; the Stage-1 fact's local `darkMean` is
  distinct from the qualified `MuellerReference.darkMean`.
- The largest-delta set now holds `(MuellerRowIndex, MuellerColumnIndex)` tuples; single-case DUs over `int` have
  structural comparison, so `Set` equality is the same check.
- CRLF churn: checked with `git diff --numstat` vs `--ignore-cr-at-eol`.
