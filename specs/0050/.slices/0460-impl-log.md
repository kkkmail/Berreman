# Step 0460 — impl log

## Progress

- Read the task, both system prompts, the project prompt, rulings R0001–R0005 and the slice. Of the rulings, only
  R0004 names this step: at 0460, `StudyForward.fs` keeps the residual concurrent. `Study.residualWith` is not
  touched.
- **Domain.**
  - `ItemModels.fs`: `ParameterName.tableRowIndex` / `tableRowExtinction` (`row<i>.n` / `row<i>.k`).
  - `Studies.fs`:
    - `OpenedMaterialPhysics`, with its seven members, over a private `TableRowComponent` row list;
    - `OpenedMaterial.physics`;
    - `openFrom` for one isotropic tabulated segment;
    - the source-σ prior in `withParameterState`;
    - `currentPhysics`.
  - `StudyForward.fs`: `ExperimentAssignment.physics`, `physicsOf`, `materialProxyOver` over `materialPhysics`,
    `assignmentFor` through `withComponent` / `componentValue`, and `observedCrystalIn` through `crystalOpt`.
  - `StudyPublishing.fs`: `promotedMaterialWith` over `currentPhysics.withoutSourceUncertainty.materialPhysics`.
  - The Domain built clean on the first try.
- **Tests.**
  - Every `OpenedMaterial` record and `crystal` read moved to `physics`, and every `ExperimentAssignment` record moved
    to `physics`, with assertions unchanged. The expected value is wrapped in `OpenedCrystalPhysics` where a whole
    physics is compared.
  - The five new facts landed.
  - A `TabulatedStudyFixtures` module at the top of `StudyMaterialTests.fs` holds the test-local table material. It is
    the first of the three files to compile, as `StudyFileFixtures` does in `KitLibraryTests.fs`.
  - `StudyPublishingTests`' `materialRun` snapshot lambda became `materialSnapshotOf`, which the new promotion fact
    shares instead of copying it.
- Full build green; constructor-unit-tests green at 1828 (1823 + 5).

## Files modified

Authorized by the step's effects:

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemModels.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Studies.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/StudyForward.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/StudyPublishing.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyMaterialTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyForwardTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyPublishingTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`
- `data/absorbing-biaxial-noisy/AbsorbingTriclinic.study-json`, copied byte for byte from AT-2's build.

Outside the effects list, compile-only (see Gotchas, deviation 2):

- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitLibraryTests.fs`: `StudyFileFixtures.openedCrystalMaterial`'s
  record and one `opened.crystal` read.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs`: two `ExperimentAssignment` record
  expressions, `crystals = Map.empty` → `physics = Map.empty`.

Six authorized files needed no edit. They reach `OpenedMaterial` only through `openFrom` or the shared fixtures, and none
reads `crystal`:
- `StudyTests.fs`
- `KitStudyFileTests.fs`
- `KitContentFileJsonTests.fs`
- `StudyWindowModelTests.fs`
- `StudyMessageTests.fs`
- `StudyViewTests.fs`

- AT-2's study file was regenerated through the fact `the built absorbing-biaxial-noisy kit equals the committed kit
  file by file`, filtered alone.
  - It failed, as expected, in 802 s, on `AbsorbingTriclinic.study-json` only.
  - `diff -rq` of the whole built kit against `data/absorbing-biaxial-noisy/` (`.oc/` aside) named only that file. Its
    diff is the `crystal` → `physics` (`OpenedCrystalPhysics`) move and nothing else.
  - The built file was copied into `data/`, and `cmp` confirms it is identical (LF).
- Elevation over the first build was OK, but suppressed ELEV004 rose 1757 → 1759, from the two `value.value` reads in
  the table-column writers.
  - Both now bind by pattern (`fun (ParameterValue value) -> …`).
  - A kit-acceptance run already under way on the old binaries was stopped, its two test hosts ended, and the solution
    rebuilt.
  - Every check below ran on that final build.

## Testing state

These are development checks only. The gate engine runs the roster after this session and is the authority. Every
figure is from the final source and final build.

- **build**: exit 0, 22 s incremental (600 s deadline), 0 errors. Warnings: 12 × NU1902 (exempt) only. Log:
  `.artifacts/0460-dev-build.log`.
- **constructor-unit-tests**, as the gate selects it (`all_discovered`): `Passed!`, 1828 of 1828, in 105 s (180 s
  deadline).
  - 1823 at step 0459 + 5 new facts.
  - The five new facts and the two named unchanged facts are `Passed` in the TRX.
  - Log: `.artifacts/0460-dev-constructor-unit-tests.log`.
- **ui-tests**, the gate's own command over `OpticalConstructor.Ui.Tests.StudyViewTests`: `Passed!`, 8 of 8, in 19 s
  (360 s deadline). The class is untouched, so the count is unchanged. Log: `.artifacts/0460-dev-ui-tests.log`.
- **ui-smoke**, as the gate selects it (`all_discovered`): `Passed!`, 267 of 267, in 219 s (600 s deadline). Log:
  `.artifacts/0460-dev-ui-smoke.log`.
- **kit-acceptance** over `OpticalConstructor.Ui.Tests.KitAcceptanceTests`: `Passed!`, 14 succeeded and 1 skipped, in 1441 s (3600 s deadline).
  - The skip is AT-2's five-seed ensemble, xUnit v3 Explicit; the gate's skip policy is `allow`.
  - Both `… kit equals the committed kit file by file` facts passed, so AT-2's committed study file equals the built one.
  - The committed-kit read-only facts parse the regenerated file.
  - Log: `.artifacts/0460-dev-kit-acceptance.log`.
- **elevation**, all 13 projects (standalone, no base): `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 258s`
  (600 s deadline). 0 known findings; suppressed ELEV004 is 1757, as at step 0459. It ran concurrently with
  kit-acceptance. Log: `.artifacts/0460-dev-elevation.log`.
- **The 21 s44 invariants**: each decoded `git grep` finds nothing (exit 1), both as written and with `--untracked`.
  Log: `.artifacts/0460-dev-invariants.log`.
- **LF**: `git diff --numstat` equals `--ignore-cr-at-eol` over every changed file.

## Artifacts

- `.artifacts/0460-dev-build.log`
- `.artifacts/0460-dev-at2-file-by-file.log`: the regeneration run, expected red on the study file.
- `.artifacts/0460-dev-constructor-unit-tests.log`
- `.artifacts/0460-dev-ui-tests.log`
- `.artifacts/0460-dev-ui-smoke.log`
- `.artifacts/0460-dev-kit-acceptance.log`
- `.artifacts/0460-dev-elevation.log`
- `.artifacts/0460-dev-invariants.log`

## Gotchas

- **Deviation 1: `tableRowIndex` / `tableRowExtinction` take `ParameterNameIndex`, not `int`.**
  - The brief writes `(row : int)`.
  - Step 0244 measured ELEV010 refusing an `int` beside `ParameterName` in exactly these static members
    (`0244-impl-log.md`). That step introduced `ParameterNameIndex` for every positional name member.
  - Elevation is in this step's roster, so the two members take `ParameterNameIndex` as their siblings do.
  - Names and numbering are as briefed: `row<i>.n`, `row<i>.k`, 1-based.
- **Deviation 2: two test files outside the effects list.**
  - `KitLibraryTests.fs` holds `StudyFileFixtures`, the one shared `OpenedMaterial` record expression.
  - `StudyEngineTests.fs` holds two `ExperimentAssignment` record expressions.
  - Neither compiles after the brief's own field renames (`OpenedMaterial.physics`, `ExperimentAssignment.physics`).
    Brief (7) asks for this edit: "every `OpenedMaterial` record expression writes `physics = OpenedCrystalPhysics …`".
  - The edits are mechanical (4 lines). No assertion or number changes, and no test identity is added, renamed or
    removed.
- **A table's components are one private row list** (`TableRowComponent`: name, value, written table, source σ). This
  keeps `componentNames`, `componentValue`, `withComponent` and `componentSigmaOpt` from walking the columns four ways.
  Record labels are distinct (`rowName`, `rowValue`, …) so they never shadow `Parameter`'s `name` in `Studies.fs`.
- **σ = 0 is no prior.** `ParameterPrior` validation needs a σ above zero, so `componentSigmaOpt` answers only a positive
  source σ. This is why freeing `row1.k` (σk 0) stores no prior.
- **The prior's centre is the opened version's value**, `this.physics.componentValue`. It is not the unknown's start, so
  freeing `row2.n` from any start centres the prior at 1.6, as the brief says.
- **`currentPhysics` regenerates the crystal's axes after each write.** `assignmentFor`'s `writtenComponent` already did
  this. The lifted axes consult their `own` record only for a crystal of another case, so every crystal number is
  unchanged. The whole constructor suite, the AT-2 study facts and the committed fit run confirm it.
- **`materialPhysics` restates the one-segment shape `TabulatedNkTable.isotropicEntry` builds in Storage**
  (`MaterialImport.fs`, step 454). The brief asks for that shape here. The Domain cannot reference Storage, and the
  Storage file is outside this step's effects, so the two sites stand side by side. The one-isotropic-segment
  construction already appears at two other sites (`MaterialImport.fs:527`, `DispersionModels.fs:782`).
- **No separate red run.** Each new fact was written against symbols that did not exist yet (`OpenedTablePhysics`,
  `physicsOf`, `tableRowIndex`, …), so before the production code it was a compile failure, which the protocol counts
  as red.
