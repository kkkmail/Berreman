# Step 0353 — impl log

## Progress

- [x] Read the protocol, the project prompt, the slice, `0353.gates`, AGENTS.md, `MuellerInverse.fs` and every file
      that names one of the three records.
- [x] `CrystalPhysics.fs` created by copying `MuellerInverse.fs`'s exact line ranges; fsproj line added.
- [x] `MuellerInverse.fs`: records removed, `axes` extensions added, open and doc sentence added.
- [x] Consumers open `CrystalPhysics` (8 listed files + the unlisted `ItemEvaluationTests.fs`).
- [x] New `MaterialComplexityTests` fact.
- [x] Development runs of build, elevation, constructor-unit-tests, unit-tests (the three selected classes) and the
      20 s36 invariants.

## Files modified

- **`Berreman/OpticalConstructor/OpticalConstructor.Domain/CrystalPhysics.fs`** (new): `module CrystalPhysics` in
  `namespace OpticalConstructor.Domain`.
  - The module doc is two lines: the crystal-parameter records a material version can hold as its physics, each with
    its engine builder (spec 0050 S11.1, .spec-md §3.16).
  - It opens `System.Numerics` (the absorbing builder's `Complex`), `Berreman.MaterialProperties` and
    `OpticalProperties.Active`.
  - It declares `UniaxialParameters`, `TriclinicParameters` and `AbsorbingTriclinicParameters`, in that order. Each
    keeps its doc, its fields and its `member this.opticalProperties : OpticalProperties` exactly as steps 332–334
    left them. The text was copied by line range from the pre-move `MuellerInverse.fs`.
- **`Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`**: one line,
  `<Compile Include="CrystalPhysics.fs" />`, directly after `DispersionModels.fs` and before `MaterialLibrary.fs`.
  No comment.
- **`Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerInverse.fs`**:
  - `open OpticalConstructor.Domain.CrystalPhysics` comes after `open OpticalProperties.Active`.
  - The module doc's builder sentence now reads "… as `opticalProperties` (spec 0050 §3.16) and is declared in
    `CrystalPhysics` (spec 0050 S11.1), and no named material lives here."
  - The three record declarations and their builders are deleted.
  - Directly after `ParameterAxis` come `type UniaxialParameters with`, `type TriclinicParameters with` and
    `type AbsorbingTriclinicParameters with`. Each holds `static member axes`, with the same doc and body as before.
  - `MuellerObservation` is unchanged. It now follows the three extensions, because the two records that were
    around it moved out.
  - No other line changed. A line-set comparison of the diff against `CrystalPhysics.fs` confirms the move.
- **One `open OpticalConstructor.Domain.CrystalPhysics` line** goes directly before each file's
  `open OpticalConstructor.Domain.MuellerInverse`. Nothing else changes in these files:
  - `BerremanTests/`: `MuellerInverseTests.fs`, `LangasiteInverseTests.fs`, `BiaxialInverseTests.fs`,
    `AbsorbingBiaxialInverseTests.fs`, `AbsorbingBiaxialNoisyTests.fs` and `FeasibleBiaxialInverseTests.fs`;
  - `BerremanTestSupport/`: `BiaxialSample.fs` and `AbsorbingBiaxialSample.fs`;
  - `OpticalConstructor.Tests/ItemEvaluationTests.fs`, which is outside the edit list (see Decisions).
- **`Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialComplexityTests.fs`**:
  - opens `OpticalConstructor.Domain.CrystalPhysics`, before `MaterialLibrary`;
  - new fact `each crystal record's builder composes its permittivity from its own fields`. It builds three
    test-local records as annotated literals. Every comparison goes through the file's existing `closeC` at 1e-15,
    and each expected n² is computed from the record's own field:
    - the uniaxial `opticalProperties.eps` is diag(n_o², n_o², n_e²), all nine entries checked;
    - the triclinic one is diag(n₁², n₂², n₃²), all nine entries checked;
    - the absorbing one has ε₁₁ = n₁² + i·ε″₁₁ and ε₁₂ = i·ε″₁₂. Every ε″ component is distinct, so a swapped index
      would fail.
- **Not written:** `Berreman/BerremanTests/BiaxialSample.fs` and `AbsorbingBiaxialSample.fs`. They are census paths
  for files that steps 345–346 moved to `BerremanTestSupport/`, and they do not exist (the brief says nothing is
  written there).

## Decisions

- **`ItemEvaluationTests.fs` is edited although the effects list omits it.**
  - It names `UniaxialParameters` (lines 129, 139, 140, 721) and opens only `MuellerInverse`, so the build fails
    without the open.
  - The brief's rule (3), "the files that name a record open `OpticalConstructor.Domain.CrystalPhysics` where the
    compiler asks and change nothing else", covers it; its file list missed it. The same one-line open is the whole
    change.
  - Precedent: steps 0259, 0291 and 0313 edited unlisted fixtures that the build needed under their slice's own rule.
  - No test identity changes.
- **`CrystalPhysics.fs` does not open `Berreman.Fields`.** The brief lists "the engine's `Fields`, `MaterialProperties`
  and `OpticalProperties.Active`" as what the moved bodies read. In fact they read only:
  - `RefractionIndex`, `EpsValue`, `RhoValue`, `Eps`, `Mu`, `Rho` and `OpticalProperties`, all from
    `Berreman.MaterialProperties`;
  - `OpticalProperties.type_3_4_6_Crystal` and `Rho.type_1_Crystal`, from `OpticalProperties.Active`;
  - `Complex`, from `System.Numerics`.

  The rule is "opens what the moved bodies read", so `System.Numerics` is opened and `Fields` is not. The build
  confirms the set.
- **The consumers open `CrystalPhysics` directly before `MuellerInverse`.** The records used to be in
  `MuellerInverse`, so putting the new open at the same place keeps record-label resolution where it was. Only
  `MaterialComplexityEditor` shares labels with the records (`index1..3`, in another state record), and none of
  these files opens it.
- **In `MaterialComplexityTests` the open comes before `MaterialLibrary` / `MaterialComplexityEditor`.** Those later
  opens keep their labels for any unannotated literal. The engine's gyration records (`Berreman.Dispersion`,
  opened earlier) share `g11`/`g33`, but every existing literal of theirs in the file is typed by its union case. The
  new fact annotates its three literals. The build confirms nothing changed resolution.
- **The `axes` extensions sit in `MuellerInverse`, on the records.** They are optional extensions in the module
  that declares `ParameterAxis`, so `UniaxialParameters.axes` resolves wherever `MuellerInverse` is open, which is
  every caller. `axes` sits on three different receivers, and ELEV019 found nothing.
- **The uniaxial builder squares n_o twice.** It goes through `EpsValue.fromRefractionIndex` (n²), and then the
  engine's `Eps.planarCrystal` takes `sqrt` and squares again. The fact's 1e-15 band on values near 2.4 covers that
  path, and it passed.

## Testing state

These are development runs of each gate's own command from `0353.gates`, over the final tree. The arc-runner's gate
engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0; the word `error` appears 0 times; warnings only NU1902 (exempt) and the vendored MathNet SYSLIB0051 | 76 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; known findings 0; suppressed 1509 (ELEV004 excluded), unchanged from step 0352 | 210 s | 600 s |
| constructor-unit-tests | Passed!, 1559 of 1559, 0 failed (checkpoint 351's 1558 + the new fact, Passed in the TRX) | 70 s | 180 s |
| unit-tests (`AbsorbingBiaxialInverseTests`, `FeasibleBiaxialInverseTests`, `LangasiteInverseTests`) | Passed!, 18 of 18, 0 failed (7 + 5 + 6 in the TRX, the facts the three files declare; count unchanged) | 272 s | 7200 s |
| invariant-s36-\* (20) | each decoded `git grep` found nothing (exit 1, empty), both as written and with `--untracked` | under 1 s each | 300 s |

- `git grep --untracked -n -E "type (UniaxialParameters|TriclinicParameters|AbsorbingTriclinicParameters) =" -- Berreman/`
  finds only `CrystalPhysics.fs:15`, `:48` and `:110`. Without `--untracked` it finds nothing until the new file is
  committed.
- `git grep -n -E "type (…) with" -- Berreman/` finds the three `axes` extensions in `MuellerInverse.fs`, at lines
  171, 195 and 240.
- LF only: `git diff --numstat` equals `--ignore-cr-at-eol`, no edited or new file contains a CR, and
  `CrystalPhysics.fs` has no BOM.
- `specs/0050/.manifest.state.json` was already modified when this step started. It belongs to the supervisor, not
  this step.

## Artifacts

- `.artifacts/0353-build.log`
- `.artifacts/0353-elevation.log`
- `.artifacts/0353-constructor-unit-tests.log`
- `.artifacts/0353-unit-tests.log`
- `.artifacts/0353-invariants.log`
- `.artifacts/0353-MuellerInverse.before.fs.txt`, the pre-move `MuellerInverse.fs` the line ranges were copied from
