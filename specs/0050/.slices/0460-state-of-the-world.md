# Step 0460 — state of the world

## Where we are

Spec 0050, section S13.2 (PTB OCDB, the Henke compound calculator and glass catalogues). Steps 0453–0459 gave tabulated
tables their source's per-sample σ and wired the online sources. This step lets a study open such a table.

A single isotropic tabulated segment now opens as a study-local working copy, as a crystal does. Each row's n and k are
known components, `row<i>.n` and `row<i>.k`. Freeing one takes the source σ as its prior. The forward model solves a
stack of the opened table with the working table, and promotion writes the fitted rows without the source's
uncertainty.

Every reader of `Study.materials` and their `parameters` reads table components unchanged. That covers unknowns, the
layout, the unknowns table, the fit run's parameters, promotion subjects and the S11.3 prior residual rows. The Planner
(S14) reads the same copies.

## What's working

- Open a single isotropic tabulated segment in a study with one known component per row's n and k (`row<i>.n`,
  `row<i>.k`); any other complexity tree is still refused.
- Give a freed table component its source σ as its prior; an explicit prior, a zero σ and a known or calibrated state
  are kept as given.
- Hold the working copy's physics as `OpenedMaterialPhysics` (crystal or table) in the study, the forward assignment and
  promotion; every crystal number is unchanged.
- Solve a stack sample made of an opened table with the study's working table; links between table rows follow their
  leader.
- Promote a fitted table as its opened version's entry holding the fitted rows without the source's uncertainty.
- Regenerate AT-2's committed `AbsorbingTriclinic.study-json`: the `crystal` key is now `physics`, holding
  `OpenedCrystalPhysics` of the same crystal, copied byte for byte from the build.

## Tests

These are development checks. The gate engine runs the roster after this session and is the authority. Every figure
below is from the final source and the final build. The one exception is the study-file regeneration run in the second
bullet.

- **build**: exit 0 in 22 s (incremental; 600 s deadline). 0 errors. The only warnings are 12 × NU1902 (exempt). No FS
  or MSB warning.
- **AT-2 study-file regeneration** (development step, not a gate):
  - The fact `the built absorbing-biaxial-noisy kit equals the committed kit file by file`, filtered alone, failed as
    expected in 802 s, on `AbsorbingTriclinic.study-json` only.
  - `diff -rq` of the whole built kit against `data/absorbing-biaxial-noisy/` (`.oc/` aside) named that one file.
  - Its diff is exactly the `crystal` → `physics` / `OpenedCrystalPhysics` move.
  - The built file was copied into `data/`. `cmp` confirms it is identical, and it is LF.
  - This run was on the build before a two-line, behaviour-neutral pattern edit (see Gotchas). The gate run below is on
    the final build.
- **constructor-unit-tests** (`all_discovered`): `Passed!`, 1828 of 1828, in 105 s (180 s deadline).
  - That is 1823 + 5, the brief's "rising by 5".
  - All five new facts passed.
  - So did the unchanged `the generated axes of an opened absorbing triclinic material equal
    AbsorbingTriclinicParameters.axes` and `a material whose physics is a complexity tree, or one opened twice, is
    refused`.
- **ui-smoke** (`all_discovered`): `Passed!`, 267 of 267, in 219 s (600 s deadline).
- **ui-tests** over `OpticalConstructor.Ui.Tests.StudyViewTests`: `Passed!`, 8 of 8, in 19 s (360 s deadline). The
  class is untouched, so its count is unchanged.
- **kit-acceptance** over `OpticalConstructor.Ui.Tests.KitAcceptanceTests`: `Passed!`, 14 succeeded and 1 skipped, in 1441 s (3600 s deadline).
  - The skip is AT-2's five-seed ensemble, xUnit v3 Explicit; the gate's skip policy is `allow`.
  - Both `… kit equals the committed kit file by file` facts passed, so AT-2's committed study file equals the built one.
  - The committed-kit read-only facts parse the regenerated file.
- **elevation**, all 13 projects (standalone, no base): `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 258s`
  (600 s deadline). 0 known findings. Suppressed ELEV004 is 1757, as at step 0459.
- **The 21 s44 invariants**: each decoded `git grep` finds nothing, both as written and with `--untracked`.
- **LF**: `git diff --numstat` equals `--ignore-cr-at-eol` over every changed file.

```yaml
gates:
  constructor_unit_tests: 1828
  ui_smoke_tests: 267
  ui_tests: 8
  kit_acceptance_tests: 14
```

## Architecture

- **`OpenedMaterialPhysics`** (`Studies.fs`, directly before `OpenedMaterial`) is the working copy's physics. It has
  two cases, `OpenedCrystalPhysics of CrystalParameters` and `OpenedTablePhysics of TabulatedNkTable`, and is stored in
  the serializer's case form.
  - It exposes `componentNames`, `componentValue`, `withComponent`, `componentSigmaOpt`, `withoutSourceUncertainty`,
    `crystalOpt` and `materialPhysics`.
  - A table's components come from one private row list (`TableRowComponent`: name, value, written table, positive
    source σ). Each table write produces new columns.
- **`OpenedMaterial.physics`** replaces `crystal`, and `currentPhysics` replaces `currentCrystal`.
  - `openFrom` accepts `ComplexityPhysics` of exactly one isotropic `TabulatedNK` segment with no magnetic or active
    part.
  - In `withParameterState`, an unknown with no prior takes `{ centre = opened value; sigma = source σ }` when the
    component has a positive σ.
- **`ExperimentAssignment.physics`** replaces `crystals`, and `physicsOf` replaces `crystalOf`.
  - `materialProxyOver` answers each assigned version's `materialPhysics`.
  - `observedCrystalOpt` reads `crystalOpt`, so a table plate's observation is `ObservationPlateErr`.
  - The residual is untouched and stays concurrent (R0004).
- **`ParameterName.tableRowIndex` / `tableRowExtinction`** take `ParameterNameIndex`, as their positional siblings do,
  because ELEV010 refuses an `int` there (step 0244).
- **The study file keeps schema version 1.** `study.schema.json` leaves the inner shapes to the serializer. Only AT-2's
  committed study opens a material, and it was regenerated. `data/optics-mueller/QZ-LR.study-json` opens none and keeps
  its bytes.

## Deferred

Nothing. Every part of the brief landed in this step.

## Gotchas

- **`tableRowIndex` / `tableRowExtinction` take `ParameterNameIndex`, not the brief's `int`.** This is a recorded
  deviation; ELEV010 refuses an `int` beside `ParameterName`.
- **Two compile-only edits outside the effects list.** `KitLibraryTests.fs` holds the shared `StudyFileFixtures`
  `OpenedMaterial` record, and `StudyEngineTests.fs` has two `ExperimentAssignment` records. Brief (7)'s field renames
  require them. Four lines change, and no assertion or test identity.
- **The test-local table material lives in `TabulatedStudyFixtures`** at the top of `StudyMaterialTests.fs`, the first
  of the three fact files to compile. `StudyForwardTests` and `StudyPublishingTests` open it.
- **A source σ of 0 is no prior**, because a prior's σ must be above zero. The prior's centre is the opened version's
  value, not the unknown's start.
- **AT-2's study file must be regenerated from a build, never hand-edited.** The canonical writer sorts keys, so
  `physics` lands after `parameters`, and the extra case level re-indents the crystal.
- **The two table-column writers bind the number by pattern** (`fun (ParameterValue value) -> …`). An earlier draft
  read `value.value`, which added 2 suppressed ELEV004 findings. The final source keeps the count at 1757.

## Changelog

- 2026-11-17 — step 0460: studies open single-segment tabulated materials (`OpenedMaterialPhysics`, `row<i>.n/k`
  components, source-σ priors); the forward assignment and promotion carry the working physics; AT-2's committed study
  file regenerated.
