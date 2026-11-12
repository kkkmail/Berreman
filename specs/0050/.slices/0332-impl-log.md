# 0332 — impl log

## Progress

- [x] MuellerInverse.fs: `UniaxialParameters.opticalProperties` + `open OpticalProperties.Active` + module doc
- [x] MuellerInverseTests.fs: `buildUniaxialGyrotropic` retired, `forward` and the `orientForCut` fact read the member
- [x] LangasiteInverseTests.fs: `buildLangasite` retired, `forward` reads the member
- [x] BiaxialInverseTests.fs: the fact-local `buildUniaxial` retired, `uniaxialForward` reads the member
- [x] Development runs of build, elevation, constructor-unit-tests, unit-tests (selected) and the 19 s33 invariants

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerInverse.fs`
  - `open OpticalProperties.Active` (Domain reaches `OpticalProperties` through its `Analytics` reference, as
    `MaterialLibrary.fs` does).
  - `UniaxialParameters.opticalProperties : OpticalProperties`, after `axes`. Its body is the retired builders'
    expression, `OpticalProperties.type_3_4_6_Crystal (EpsValue.fromRefractionIndex this.ordinaryIndex)
    (EpsValue.fromRefractionIndex this.extraordinaryIndex) this.g11 this.g33`. Its doc is two lines citing spec 0050 §3.16.
  - Module doc lines 16-20: the "Domain does not reference `OpticalProperties`" sentence now reads: the forward proxy takes
    a caller-supplied `'Parameters -> OpticalProperties` builder, each parameter record carries its crystal class's
    builder as `opticalProperties` (spec 0050 §3.16), and no named material lives here. No other line changed.
- `Berreman/BerremanTests/MuellerInverseTests.fs`: `buildUniaxialGyrotropic` and its doc deleted;
  `forward = createBerremanForward (fun (p : UniaxialParameters) -> p.opticalProperties) solverParameters`;
  the `orientForCut carries the optic axis …` fact's `props = quartz.opticalProperties`.
- `Berreman/BerremanTests/LangasiteInverseTests.fs`: `buildLangasite` and its doc deleted; `forward` as above.
- `Berreman/BerremanTests/BiaxialInverseTests.fs`: the local `buildUniaxial` deleted from `the triclinic parameter set is
  a strict generalization of the uniaxial one`; `uniaxialForward` as above.

## Decisions

- **The dead `open OpticalProperties.Active` went with the builders.** Each of the three test files opened it only for
  `OpticalProperties.type_3_4_6_Crystal` inside the deleted builder (checked: no other `Eps.` / `Rho.` /
  `OpticalProperties.` static or `toRhoWithDisp` use remains in them). The open is part of the code the move retires
  (AGENTS.md "superseded code is deleted in the change that replaces it"). No assertion, test or name changes.
- **The lambdas are written exactly as the step's `how_to` gives them**, `(fun (p : UniaxialParameters) -> p.opticalProperties)`.
  The annotation is needed because the member is resolved on `p` before `createBerremanForward`'s `'Parameters`
  is fixed.
- **Bit-identity.** The member evaluates the same expression with the same arguments in the same order as the three builders
  did, so every number the facts compute is unchanged.
- **The module doc says "each parameter record"** as the step prescribes, though only `UniaxialParameters` carries the
  member after this step. Steps 0333 (`TriclinicParameters`) and 0334 (`AbsorbingTriclinicParameters`) add the other two
  (`.spec-jsonl`), so the sentence is the section's end state.

## Testing state

These are development runs of each gate's own command from `0332.gates`, over the final tree.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors | 79 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded | 213 s | 600 s |
| constructor-unit-tests (all) | Passed!, 1552 of 1552 succeeded, 0 failed, 0 skipped | 75 s | 180 s |
| unit-tests (`BerremanTests.LangasiteInverseTests`) | Passed!, 6 of 6 succeeded, 0 failed, 0 skipped | 8 s | 7200 s |
| invariant-s33-\* (19) | each `INVARIANT HOLDS`, exit 0 | under 1 s each | 300 s |

- **Build warnings.** Only the two known `SYSLIB0051` warnings from the vendored MathNet `Exceptions.cs`.
- **Counts.** constructor-unit-tests 1552, equal to checkpoint 330's `constructor_unit_tests`. `LangasiteInverseTests`
  declares 6 facts before and after the step (`[<Fact` count, HEAD and working tree), and all 6 ran.
- **The two changed facts outside the selected class** were also run, by `--filter-method`: `orientForCut carries the optic
  axis from the surface normal into the surface plane` (MuellerInverseTests) and `the triclinic parameter set is a strict
  generalization of the uniaxial one` (BiaxialInverseTests). Both passed (2 of 2), at their unchanged tolerances.
- **Elevation** ran with no comparison base, over all 12 projects. It reports 1451 suppressed (ELEV004, measured
  but not gated); step 330 recorded 1453, and step 331 retired the archive seam in between. This diff adds and
  removes no `.value`.
- No development run exceeded its effective deadline.
- **Leftovers.** `grep -rn "buildUniaxialGyrotropic|buildLangasite|buildUniaxial\b" Berreman --include=*.fs` finds nothing.
- **LF.** Every edited source file has zero CR bytes, and `git diff --numstat` equals `--ignore-cr-at-eol`.

## Artifacts

- `specs/0050/.artifacts/0332-dev-build.log`
- `specs/0050/.artifacts/0332-dev-elevation.log`
- `specs/0050/.artifacts/0332-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0332-dev-unit-tests.log`
- `specs/0050/.artifacts/0332-dev-extra-facts.log` (and `0332-dev-extra-results/`)
- `specs/0050/.artifacts/0332-dev-invariants.log`

## Gotchas

- `OpticalProperties` is both a namespace (the `OpticalProperties` project) and the engine record type
  (`Berreman.MaterialProperties.OpticalProperties`). After `open OpticalProperties.Active`,
  `OpticalProperties.type_3_4_6_Crystal` resolves to the static extension on the record, the same shape
  `MaterialLibrary.fs` and the tests already used.
