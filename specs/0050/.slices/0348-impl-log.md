# Step 0348 — impl log

## Progress

- Read the task, both system prompts, the project prompt, AGENTS.md and the slice. There is no operator note and no
  `## Operator rulings for this step` section; ruling 013 (arc-origin elevation scope) applies as always.
- Verified every Stage-1 and section 7.2 literal against `reference_step1_summary.json` and
  `reference_step2_linear_summary.json` (same doubles), and the excluded point against `matrix_glue.py:22`.
- Verified the five degree literals round-trip bit for bit through `* (π/180) / (π/180)`, which is what
  `Angle.degree` / `Retardance.degree` followed by `.degrees` compute (`.artifacts/0348-roundtrip-check.log`).
- Created `Berreman/BerremanTestSupport/MuellerReference.fs`: `module MuellerReference` in
  `namespace BerremanTestSupport`, opening `Berreman.Constants`, `Berreman.Geometry`, `Berreman.Fields` and
  `OpticalConstructor.Domain.MuellerReconstruction` only. In the brief's order:
  - (a) the spec 0044 §8.5 / §9 comment block verbatim, `stage1Tol` (1.0e-12) and `dataMatrixTol` (1.0e-9) with
    their docs;
  - (b) `darkMean : DarkMean`, `lpAnalyzerZero`, `cplAnalyzerZero`, `cplSourceAxisOffset : Angle`,
    `cplAnalyzerRetardance`, `cplSourceRetardance : Retardance`, each with a one-line doc naming its summary key;
  - (c) `qzMatrix`, `lrMatrix`, `combinedMatrix : MuellerMatrix` (the fact's literals, unchanged text),
    `cascadeFrobenius`, `cascadeMeanAbs`, `cascadeMaxAbs : MatrixDeviation`, and
    `cascadeLargestDeltaPositions : Set<MuellerRowIndex * MuellerColumnIndex>` = {(3, 3); (3, 1)} with the spec 0044
    note cut to two lines;
  - (d) `excludedPoints : ExcludedPoint list`, the one `LP-(LR#90)-CPL-2` / 140° / capture 17 point.
- `BerremanTestSupport.fsproj`: one `<Compile Include="MuellerReference.fs" />` after `AbsorbingBiaxialSample.fs`;
  no comment, no reference.
- `MuellerReconstructionTests.fs`:
  - deleted the comment block, `stage1Tol` and `dataMatrixTol`;
  - class doc's §8.5 bullet now says the expectations live in `BerremanTestSupport.MuellerReference` (S10.3);
  - Stage-1 fact compares against `MuellerReference.darkMean.value` and the `.degrees` of the five angle /
    retardance values, same order, at `MuellerReference.stage1Tol`; its provenance comment is three lines naming
    `MuellerReference`;
  - `buildDesign` fact: local `excluded` deleted, reads `MuellerReference.excludedPoints`;
  - section 7.2 fact: `expectedQz` / `expectedLr` / `expectedComb`, the local `excluded` (and its comment) and the
    three metric literals deleted; reads the module's values, compares at `MuellerReference.dataMatrixTol`, and
    enumerates the largest-delta positions as `(MuellerRowIndex i, MuellerColumnIndex j)` against
    `MuellerReference.cascadeLargestDeltaPositions`. Its comments naming the summary say the values are
    `MuellerReference`'s; the spec 0044 note moved to the module with the positions.
- After the move, `git grep` for every moved literal (`869.666` aside, see Gotchas) finds it only in
  `MuellerReference.fs`.

## Files modified

- `Berreman/BerremanTestSupport/MuellerReference.fs` (new)
- `Berreman/BerremanTestSupport/BerremanTestSupport.fsproj` (+1 line)
- `Berreman/BerremanTests/MuellerReconstructionTests.fs`

All three are LF-only (no CR); `git diff --numstat` equals `--ignore-cr-at-eol`.

## Testing state

These are development runs of each gate's own command from `0348.gates`. The final runs are over the final tree,
after two comment-only reflows that followed the first runs.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`): exit 0, `0 Error(s)`.
  - The first full build took 76 s; the final incremental rebuild took 14 s (deadline 600 s).
  - The only warnings are NU1902 (an exempt advisory, on `BerremanTestSupport`, `BerremanTests` and
    `OpticalConstructor.Ui.Tests`) and the vendored MathNet SYSLIB0051. Both were present at step 0347.
  - Both `BerremanTestSupport` and `BerremanTests` recompiled.
- **elevation** (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`, standalone, so all 13 projects):
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 0 known findings, exit 0.
  - The first run took 224 s and the final run 221 s (deadline 600 s).
  - The suppressed count is 1502, unchanged from step 0347.
  - `BerremanTestSupport` reports `findings=0`.
- **unit-tests** (the snapshot's `step_select` `BerremanTests.MuellerReconstructionTests`, with the gate's own
  selector): `Test run summary: Passed!`, 25 of 25, 0 failed, 0 skipped, 3 s (deadline 7200 s).
  - The TRX shows the Stage-1, `buildDesign` and section 7.2 facts `Passed`.
  - The count equals the class's last measured selection (step 0331: 25).
  - The `[<Fact>]` markers are unchanged by the diff (26 marker strings including one in a comment, i.e. 25 facts).
- **s35 invariants**: I decoded all 20 `git grep` commands from the snapshot and ran each one as written and again
  with `--untracked`, because `MuellerReference.fs` is new. None found anything (20 of 20 OK).
- **Acceptance structure**: a `git grep --untracked` for every moved literal finds it only in `MuellerReference.fs`.
  The two exceptions are the out-of-scope inputs listed under Gotchas.
- No run went past its effective deadline.

## Artifacts

- `specs/0050/.artifacts/0348-roundtrip-check.fsx`, `0348-roundtrip-check.log`
- `specs/0050/.artifacts/0348-build.log`
- `specs/0050/.artifacts/0348-elevation.log`
- `specs/0050/.artifacts/0348-unit-tests.log`
- `specs/0050/.artifacts/0348-invariants.log`

## Gotchas

- **The bands stay `float`.** `stage1Tol` bounds a dark-count deviation and degree deviations in one assertion, so
  no single elevated type fits. The brief moves the bands "with their values", and the facts compare them against
  raw deviations, the way `MatrixComparison.allowedDiff` is used.
- **Two same-valued literals remain outside the three changed facts:**
  - The Stage-3 signal-reduction fact uses `DarkMean 869.666` as a synthetic input (always-run). Its test id is not
    among this step's authorized effects, so it is unchanged.
  - The `buildDesign` fact's raw-row fixture keeps an `LP-(LR#90)-CPL-2` / capture 17 / 140° `MuellerRawRow`. That
    row is the input the excluded point must drop, not a copy of the `ExcludedPoint`.
- **Interpretation: the spec 0044 note on the largest-delta positions moved.** The brief attaches it to
  `cascadeLargestDeltaPositions` ("with the fact's spec 0044 note on them cut to one to three lines"). I deleted it
  from the fact so the decision is documented once (AGENTS.md comments rule). The fact keeps its spec 0042 paragraph.
- **Interpretation: "no other comment changes" applies to the class doc.** The brief itself changes the Stage-1 and
  section 7.2 comments. I also rewrote one clause of the `buildDesign` fact's comment ("supplied here as
  BerremanTests data"), because after the move it was false. The comment above the 7.2 fact's deleted local
  `excluded` went with the binding.
- **The module docs avoid citing `Berreman/Data/MuellerMatrix/matrix_glue.py`.** The s35 invariant
  `t9-no-data-mueller-matrix` greps `Data/MuellerMatrix` under `Berreman/`, so the doc cites `matrix_glue.py:22`
  (the copy in `data/optics-mueller/data/reference/`).
- **`BerremanTestSupport` appears in elevation's scope list but is not analyzed for findings.** Its assembly is not
  `Softellect.Berreman.*` (`.elevation-analyzers.json` `inScopePatterns`), so it reports `findings=0` and nothing
  suppressed, as step 0344 records.
