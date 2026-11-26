# Step 0006 — ADD_RED — impl log

## Progress

- Read the protocol (`arc-runner.system-md`, `add_red_worker.system-md`), the project prompt,
  `AGENTS.md`, the slice, its `.gates` snapshot, the outputs of steps 4 and 5, and step 7's brief
  (the CLOSE_RED that clears RED_0003).
- Confirmed the red reason on the tree step 5 left:
  - `toEpsAxis` lowers `ForouhiBloomer` / `BrendelBormann` to `EpsAxisEvaluated this.evaluate`
    (`OpticalConstructor.Domain/DispersionModels.fs:787-788`).
  - `evaluate` (`:607-610`) answers a new `fun w -> …` on every call.
  - `EpsAxisDispersion.Equals` compares `EpsAxisEvaluated` with `PhysicalEquality` (`:549`), and
    `GetHashCode` uses `PhysicalHash` (`:560`), both in `Berreman/Berreman/Dispersion.fs`.
  - The slice's citations `:403-430` and `Dispersion.fs:386` have drifted; the names locate the
    code.
- Hoisted the AC-B6 coefficient sets into builders, as step 4 did for AC-B5, so the fact reuses
  them rather than copying them (AGENTS.md "Reuse, never copy"):
  - Added `forouhiBloomerASiCoefficients ()` and `brendelBormannGoldCoefficients ()`, private
    builders with the exact literals. Each call builds a fresh record.
  - The source doc comments moved with the literals onto the builders.
  - `forouhiBloomerASi` and `brendelBormannGold` keep their names and are now
    `ForouhiBloomer (forouhiBloomerASiCoefficients ())` and
    `BrendelBormann (brendelBormannGoldCoefficients ())`. The AC-B6 facts that read them, and
    step 7's citations by name, are unchanged.
- Added the fact `equal Forouhi-Bloomer and Brendel-Bormann models lower to equal axis data`
  after `AC-B6 ForouhiBloomer and BrendelBormann lower to the evaluated case and match evaluate`.
  It calls step 4's `assertLowersToEqualData` (not copied) once per model. That helper checks,
  in this order:
  1. One model lowered twice: the axes are equal and their hashes are equal. On this tree this
     assertion fails first.
  2. Two separately built models: the axes are equal and their hashes are equal.
  3. `toEpsValue` taken twice from one model is equal, and `toEpsValue` of the two equal models
     is equal.
  4. A shifted copy lowers to an unequal axis.
- The shifted copies:
  - Forouhi-Bloomer: `bandGap` 0.906 → 1.0.
  - Brendel-Bormann: the first `broadening` 0.742 → 0.8, through
    `List.updateAt 0 0.8 gold.broadening`, so the other four broadenings are not re-transcribed.
- No production file was touched.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DispersionModelsTests.fs` (+42 / −26).
  The file is LF: `git diff --numstat` equals `git diff --ignore-cr-at-eol --numstat`.

## Testing state

These runs are local observations made to record the red, as the ADD_RED obligation requires.
The arc-runner's gate engine runs and records the gates.

- `dotnet build Berreman.slnx -c Release -nologo -v:m` exited 0 with 0 errors. The only
  warnings are the two vendored `MathNetNumerics/Numerics/Exceptions.cs` SYSLIB0051 warnings,
  which were already on the start tree (step 4 recorded the same two). Log:
  `.artifacts/0006-build.log`.
- I ran the gate's selection, `dotnet test --no-build -c Release -p:Platform=x64 --no-ansi
  --filter-method "OpticalConstructor.Tests.DispersionModelsTests.*" --filter-method
  "OpticalConstructor.Tests.DispersionModelsTests"`. It exited 2 with `total: 31, failed: 1,
  succeeded: 30`. Log: `.artifacts/0006-red-dispersionmodelstests.log`. The actual failure:

  ```
  failed OpticalConstructor.Tests.DispersionModelsTests.equal Forouhi-Bloomer and Brendel-Bormann models lower to equal axis data (1ms)
    Forouhi-Bloomer: one model lowered twice gave unequal axes (EpsAxisEvaluated <fun:get_evaluate@610> vs EpsAxisEvaluated <fun:get_evaluate@610>)
      at OpticalConstructor.Tests.DispersionModelsTests.assertLowersToEqualData(...) in ...\DispersionModelsTests.fs:351
      at OpticalConstructor.Tests.DispersionModelsTests.equal Forouhi-Bloomer and Brendel-Bormann models lower to equal axis data() in ...\DispersionModelsTests.fs:596
  ```

  This matches the acceptance:
  - Only the new fact fails.
  - It fails on the first equality of two lowered axes: two `EpsAxisEvaluated` closures built by
    the same `get_evaluate`.
  - The 30 facts the class held before all pass, including the AC-B6 facts that now read the
    hoisted builders.
- A whole-suite run of `constructor-unit-tests` gave `total: 734, failed: 1, succeeded: 733`. The
  one failure is the declared red. The 733 passing facts equal step 5's baseline
  (`constructor_unit_tests: 733`). Log: `.artifacts/0006-constructor-unit-tests-whole.log`.
- `elevation` (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`) exited 0 and reported
  `ELEV-GATE: OK - no in-scope project was touched`, because the test project is outside the ELEV
  scope. Log: `.artifacts/0006-elevation.log`.
- The three s01 invariants each ran their `git grep` and found nothing (exit 1):
  `s01-elev-list-empty`, `s01-no-vstest`, `s01-no-kit-kit-json`.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0006-build.log`
- `0006-red-dispersionmodelstests.log`
- `0006-constructor-unit-tests-whole.log`
- `0006-elevation.log`

## Gotchas

- I read "Lowering `forouhiBloomerASi` and `brendelBormannGold` twice each, and lowering a
  separately built equal copy of each" as being about the coefficient SETS. Step 7's brief calls
  them "the `forouhiBloomerASi` and `brendelBormannGold` sets". The helper therefore builds both
  the model and its twin from the same builder. Each is a model equal to the named value, built
  in its own allocation down to the coefficient record, so the twin cannot pass by sharing a
  reference.
- The two named values stay as values built from the builders. Step 7 cites the AC-B6 lowering
  fact (`:534-552`, now `:570-588`) and the sets (`:403-430`, now `:432-465`) by line. Those lines
  have drifted, and the names find them.
- The Brendel-Bormann shift uses `List.updateAt` on a fresh coefficient record, so only the
  first broadening differs and nothing is re-transcribed.
