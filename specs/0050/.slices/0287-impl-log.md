# Step 0287 — impl log

## Progress

- Read the system prompts, the project prompt, the slice, manual 013 and step 286's state of the world.
- Read `Experiments.fs`, `ExperimentProxyTests.fs` and `chartForRun`'s grid (`TableAndElementRotationView.fs:4173-4179`).
- Read steps 288 and 293 in `.spec-jsonl`. Step 288 makes `description` read `<axis describe>, capture <label>`.
  Step 293 points `chartForRun`'s x values at `sweptValues`.
- **Red first.** I wrote the three facts and built the test project. The compile was red on exactly the missing
  production symbols: `The type 'ScanAxis' is not defined`, `ScanBlock` is not defined, and `VariableRange` does not
  define `sweptValues`.
- **Production code** in `Experiments.fs`:
  - `VariableRange.sweptValues`.
  - `ScanAxis` (with `describe`), `ScanSetting`, `ScanPoint`, `ScanBlock` (with `points`) and `ExperimentScan` (with
    `points`, `axes`, `oneAxis` and `darkLine`). They sit after `ElementLabel`, in the brief's order.
  - `Experiment.description` renders its varied text through `ScanAxis.describe`. The text is unchanged.
- The first build failed with `FS0010` because F# lambdas take no return-type annotation. I dropped it; the
  `settings` label is unique to `ScanPoint`. The rebuild was green.
- **Gates** (development runs): build, constructor-unit-tests, elevation and the 16 invariants. See Testing state.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Experiments.fs`:
  - `VariableRange.sweptValues`.
  - The five scan types.
  - `Experiment.description` now reuses `ScanAxis.describe`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentProxyTests.fs`:
  - A new section, "spec 0050 S9.1: an experiment's scan".
  - Test-local `sample` / `analyzer` element ids.
  - The plate's axes (`plateIncidence`, `plateAzimuthPair`, `plateAzimuths`) and blocks (`incidenceByAzimuthBoth`,
    `azimuthsInT`).
  - The three new facts.

## Testing state

- **build:** exit 0, 0 warnings and 0 errors, in 54 s against a 600 s deadline.
- **constructor-unit-tests:** Passed!, 1480 of 1480, 0 failed and 0 skipped, in 71 s against a 180 s deadline. That
  is 1477 plus 3. The TRX shows each new fact `Passed`.
- **invariant-s29-\*** (16): every decoded grep finds nothing.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0 and ELEV004 excluded, in
  199 s against a 600 s deadline. With no comparison base it analyzes all 12 projects.
- **Mutation check:**
  - I nested `ScanBlock.points` the other way round, so the last axis was outermost.
  - `a scan block's points are the product of its axes, the first outermost, each read on the block's capture` went
    red at its order assertion (line 219, `Collections differ`). The other 33 `ExperimentProxyTests` facts stayed green.
  - The file was restored byte for byte (SHA-256 `OK`) and the solution rebuilt with 0 warnings.
  - constructor-unit-tests passed again, 1480 of 1480.
- No development run went over its effective deadline.

## Artifacts

- `specs/0050/.artifacts/0287-dev-build.log`
- `specs/0050/.artifacts/0287-dev-constructor-unit-tests.log`, `0287-dev-constructor-unit-tests.trx`
- `specs/0050/.artifacts/0287-dev-elevation.log`
- `specs/0050/.artifacts/0287-mutation-tests.log`, `0287-mutation-sha.txt` (the mutation check)

## Gotchas

- **Record-label inference.** The new records reuse the labels `elementId`, `label`, `variable`, `range` and
  `sourceWaveLengthOpt`. They are declared before `VariedElement`, `Experiment` and `ExperimentDraft`, so an
  unannotated lookup of those labels still resolves to the older types, as it did before. The whole solution built
  without a change elsewhere.
- **`chartForRun` still computes its own `xs`.** That is in `OpticalConstructor.Ui`, outside this step's `touches`
  and effects. Step 293 replaces it with `sweptValues` (".spec-jsonl" 293: "the x values are the axis's
  `sweptValues`"). `sweptValues` keeps the same arithmetic order, so the numbers are bit-identical.
- **`description` reuses `describe`** (AGENTS.md: reuse, never copy), so the varied text's format exists once. Its
  output is byte-identical, and this is the form step 288 gives it. No member was added.
