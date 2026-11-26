# Step 0392 — impl log

## Progress

- [x] Read the protocol, project prompt, slice and gates. There are no operator rulings and no operator note this step.
- [x] Shared helpers, so nothing is copied:
  - `revisedWith`, factored out of `publishing`;
  - `stageOneAirRows` and `stageOneValuesOf`, factored out of step 389's fact;
  - `prefitStaged` takes the `CalibrationUse`.
- [x] Hand-off fixture and the two facts. Both pass on the first run, and 3 facts take 3.4 s.
- [x] Fixture hardened: QZ's rows are observed with the instruments *known* at stage one's values, so the data never
  shares the calibrated-value read that stage 2 is checked on. Mutation M2 relies on this.
- [x] Mutation checks M1–M4b (below). Each source was restored byte for byte, checked with `cmp`.
- [x] Gate runs on the final tree: build, constructor-unit-tests (whole), elevation, and the twenty s38 invariants, all
  green (Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs`, the one authorized edit.

## Decisions

- **QZ's Science rows are the run law's at the values the reference stage one reads**, not step 390's rows.
  - Step 390's rows were made at the stage-1 truth, with CPL1's retarder offset +1°. Through LP2's negative dial, stage
    one reads that offset as −1°, as step 389's fact records.
  - After a faithful hand-off, stage 2 over those rows misses M_QZ by 5.7e-4 (measured, M1). The brief's 1e-9 is only
    reachable over rows consistent with the published values.
  - The fixture therefore reads the AIR rows with the reference `calibrateFromAirRows` (S10.3's stage one, as step
    389's fact does). It sets the three instruments known at those constants and observes QZ's sixteen free-session
    Science runs (`freeFamilyRuns "Q"`) through `observedOver`.
- **One study:** S1–S3, then K1, then Q1–Q16. Its kit holds `stageOneInstruments` with the five parameters unknown,
  plus the plate and QZ all free. Its setups are the three AIR benches and QZ's sixteen family benches. The slots
  reference items by id only, so step 390's benches serve the unknown items.
- **The publish** (`publishedBy`): every kit item owning s1 entries is `revisedWith` (`Item.publishRevision`) those
  entries as `CalibratedParameter { value = fitted; sigma = ParameterSigma 1.0e-6; fitRun = s1.name }`. It is
  recorded into the forward context's `kit.items`. s2's `StudyFitParam` also holds `fitRuns = [ s1 ]`, as the kit
  holds it once step 393 writes it.
- **Every fit goes through `runFit` with `createMathNetSvd ()`.** The priors fact stages through `stageWith`, by way
  of the widened `prefitStaged`, and reads `engineWith` and `parameterLayoutWith`, as the brief names them. It runs
  no nonlinear fit: the brief asks for the engine's choice and the layout only.
- `assertLoneFitRun` (step 391) checks s2's entries (QZ's sixteen, row-major, from the identity, within 1e-9), its one
  complete rank-16 object solve and its one member.

## Testing state

Local gate runs on the final tree (2026-11-14). Every run finished within its effective deadline:

- `build` (`dotnet build Berreman.slnx -c Release -nologo -v:m`): exit 0, 0 errors, 40 s (deadline 600 s). The log holds
  no `error`. Its warnings are only NU1902 and the vendored MathNet `SYSLIB0051` (Gotchas). Log:
  `0392-build-local.log`.
- `constructor-unit-tests`, whole suite: `Test run summary: Passed!`, 1691/1691, 116 s (deadline 180 s).
  - The count is 1689 → 1691, +2.
  - The TRX `TestResults/constructor-unit-tests.trx` shows both new ids `Passed`, and step 389's refactored fact
    `Passed`.
  - Log: `0392-constructor-unit-tests.log`.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 217s` (deadline 600 s). Log: `0392-elevation.log`.
- The twenty `invariant-s38-*` must-find-nothing greps, decoded from the snapshot and run with `git grep`: all twenty
  find nothing.
- LF: 0 CR bytes in every file written, and `git diff --numstat` equals `--ignore-cr-at-eol`.

Mutation checks; each fails exactly the fact it targets, and the other two pass:

- **M1 (fixture):** observe QZ's rows with step 390's `calibratedLp2/Cpl2/Cpl1` (the truth). The hand-off fact fails:
  M_QZ is missed by 5.7e-4.
- **M2 (product, `ParameterState.currentValue`):** a calibration read 1e-4 off. The hand-off fact fails: M_QZ is
  missed by 6.5e-5.
- **M3 (product, `stageWith`):** `CalibrationAsPrior` frees nothing. The priors fact fails: the engine is
  `LinearStudyEngine`.
- **M4a (product):** a box of 40 sigma. The priors fact fails on the bounds.
- **M4b (product):** a prior sigma doubled. The priors fact fails on the prior.

Logs are `specs/0050/.artifacts/0392-mut*-test.log`.

## Artifacts

- `specs/0050/.artifacts/0392-build.log`: the first solution build.
- `specs/0050/.artifacts/0392-dev-*.log`, `0392-mut*-*.log`: the development and mutation runs.
- `specs/0050/.artifacts/0392-scripts/`: the spliced section, the run script, and the pre-mutation copies.

## Gotchas

- **Heredocs.** A long Bash heredoc holding apostrophes fails in this harness, as step 390 recorded. The new section
  was written with the Write tool to `.artifacts/0392-scripts/hand-off-section.fs` and appended with Python, LF kept.
- **`SYSLIB0051` in the vendored MathNet.** Any build that recompiles `MathNetNumerics/Numerics` shows two
  `SYSLIB0051` lines, each printed twice. They come from vendored code, not ours, and step 0185's operator note
  already names them as existing. Earlier incremental logs show none only because MathNet was not recompiled.
