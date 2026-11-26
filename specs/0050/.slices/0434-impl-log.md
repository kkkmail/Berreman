# Step 0434 — implementation log

## Progress

- Read the task, both system prompts, the project prompt, rulings R0001–R0003 (`.manual/030`, `031`, `033`), the slice
  and its gate snapshot, and step 0433's impl-log and state of the world.
- Read the APIs the step drives:
  - `BenchNoise.noisyObserveAbsoluteWith` and `misreadAbsolute`, and `MuellerInverse.createBerremanForward`;
  - `AbsorbingBiaxialSample` (`noisySeeds` = seeds 1–5, `fullConfigurations`) and `AbsorbingBiaxialNoisyTests`' draw;
  - `LabFileImport` (`observationFileText`, `parseMuellerObservations`);
  - `DataSplits` (`splitKeyOpt`, `splitGroups`, `DataSplit.ofGroups`);
  - the `SplitByColumn` and `MatchByTemplate` handlers and `splitWritesOver` in `TableAndElementRotationView.fs`;
  - `ExperimentDataStore.createFileBacked` and `KitProxy.attachLabFile` (in-place attach).
- Probed the readback arithmetic. The parser rebuilds each element as throughput × normalized, so exact readback needs
  `T·((T·x)/T)` = `T·x`. `.artifacts/0434-probe/roundtrip.fsx` found no failure in 10⁶ random pairs.
- Wrote the harness functions, the build continuation, the assertions and the fact, all in `KitAcceptanceTests.fs`.
- Development build and runs; see Testing state.
- Shortened two assertion docs to one line each, as the how-to asks of every private function. Rebuilt and re-ran
  the kit-acceptance class.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`, the step's only authorized edit
  (+135 / −1, LF only).

## Decisions

- **How-to (1), the harness data.** Each private function has a one-line doc citing spec 0050 S12.2.
  - `noisyObservationsOf seed : MuellerObservation list` is `noisyObserveAbsoluteWith` over
    `MuellerInverse.createBerremanForward (fun (p : CrystalPhysics.AbsorbingTriclinicParameters) -> p.opticalProperties)
    BiaxialSample.solverParameters`, with `AbsorbingBiaxialSample.absorbing`, `NoiseParam.decentLab`, the seed and
    `AbsorbingBiaxialSample.fullConfigurations`. An `Error` fails the fact, naming the seed and the error (`%A`:
    `MuellerFitError` has no render member).
  - `plateLabelOf cut : ItemLabel` reads step 433's `absorbingPlateCuts` from cut to plate.
  - `plateKeyedObservationsOf seed` keys each observation `DataRowKey` of its plate's label, in harness order. It is
    split out of `observationFileOf` so that the build and the fact key observations through one body (AGENTS.md
    "Reuse, never copy").
  - `observationFileOf seed : KitFileText` is `KeyedObservation.observationFileText` of those keyed observations. An
    `Error` fails the fact.
- **How-to (2), the build.** `buildAbsorbingKit` goes on after `buildAbsorbingGrids` with `attachNoisyObservations`:
  1. For each seed of `AbsorbingBiaxialSample.noisySeeds`, in order, it writes `data/noisy-observations-seed-<n>.csv`
     (`noisyObservationsFile`) with the text of `observationFileOf seed`: UTF-8 without a byte-order mark, so the bytes
     are the text's own, LF included.
  2. It sends `SplitByColumn (<seed 1's absolute file>, ImportProfileName.muellerObservations)`. It asserts the key
     column `sample` and five groups in file order, `OpticAxisCut, OtherOpticAxisCut, ZCut, XCut, YCut`, each
     `ExperimentForGroup` of the experiment of that name.
  3. It sends `MatchByTemplate`. It asserts the split is cleared, the experiment names and setups are unchanged, and
     every experiment holds data through the `Mueller-matrix observations` profile.

  The attached seed is `attachedSeed` (the head of `noisySeeds`, seed 1). The other four files stay unattached in
  `data/`, for steps 437 and 438.
- **How-to (3), the fact.** `AT-2 writes each seed's noisy observations through the Mueller-matrix observation profile
  and attaches seed 1's to its plates` runs two checks.
  - `assertNoisyObservationFiles`:
    - `data/` holds exactly the five file names;
    - each file's bytes equal the UTF-8 bytes of `observationFileOf seed`, regenerated in the fact;
    - each file, decoded through the kit's one decode (`FileByteContent.kitFileText`), reads back through
      `parseMuellerObservations` to `plateKeyedObservationsOf seed`. Every key, every configuration and every
      `measured.element i j` must be exactly equal (`assertSameObservations`).
  - `assertNoisyObservationAttachments`, over a second `KitProxy.create`:
    - the stored experiments equal the window's model;
    - each experiment's `dataOpt` is exactly `{ attachment = { path = data/noisy-observations-seed-1.csv; contentHash =
      SHA-256 of the regenerated seed-1 text }; profile = Mueller-matrix observations; rows = KeyedDataRows (DataRowKey
      <name>); exclusions = [] }`;
    - `built.model.experimentData.readMapped` reads each to `ObservationPoints` of seed 1's observations with that key,
      in file order, compared element by element.

  Both the path and the seed are written as the oracle (`NoiseSeed 1`, the literal path), not derived from
  `attachedSeed`, so the fact checks independently that the build attached seed 1.
- **Element-wise comparison.** Matrices are compared through `MuellerMatrix.element`, which `Propagation.fs` documents
  as the seam tests compare matrices through. This does not rely on `RealMatrix4x4`'s equality.
- **The step-432 and step-433 facts are unchanged.** The split writes only experiment files: no snapshot, setup, scan,
  role or session changes. Both facts pass over the extended build.
- **R0001–R0003.** The step adds no table slot, so the element-id provider still mints exactly the one reflected-detector
  id (R0003). No study runs at this step (R0001, R0002).

## Testing state

These are development runs, evidence only and not gate verdicts; the supervisor runs the gates.
- `dotnet build OpticalConstructor.Ui.Tests.fsproj -c Release`: exit 0, 0 errors.
  - The first build rebuilt the vendored MathNet. It shows that project's existing `SYSLIB0051` warnings, which are in
    the 0405 and 0406 local build logs too.
  - The final build after the doc edit shows only the exempt NU1902 advisory.
  - Logs: `.artifacts/0434-attempt-01-dev-build.log`, `.artifacts/0434-attempt-01-dev-build-final.log`.
- The kit-acceptance command over `OpticalConstructor.Ui.Tests.KitAcceptanceTests`: `Passed!`, total 9, succeeded 9
  (8 before, +1), in 84 s against a 3600 s deadline.
  - The TRX lists the new test as `OpticalConstructor.Ui.Tests.KitAcceptanceTests.AT-2 writes each seed's noisy
    observations through the Mueller-matrix observation profile and attaches seed 1's to its plates`.
  - Logs: `.artifacts/0434-attempt-01-dev-kit-acceptance.log`, `…-final.log`.
  - TRX: `.artifacts/0434-attempt-01-dev-results/kit-acceptance-dev.trx`, `kit-acceptance-dev-final.trx`.
- ui-smoke command (`Category=ui-smoke`, every discovered test): `Passed!`, 264 of 264, in 238 s against 600 s. Log:
  `.artifacts/0434-attempt-01-dev-ui-smoke.log`.
- ui-tests command selecting `StudyViewTests` and `DataStepTests`: `Passed!`, 18 of 18, in 28 s against 360 s. Log:
  `.artifacts/0434-attempt-01-dev-ui-tests.log`.
- Elevation was not run. The only edited file is in a test project, which is outside the ELEV scope.
- The twenty-one s42 invariants: each decoded `git grep` command finds nothing.
- Line endings: `git diff --numstat` equals `--ignore-cr-at-eol` (135 / 1); the file has no CR and no BOM.
- The built kit was inspected. `data/` holds the five files, about 18 KB each. Seed 1's rows are keyed `OpticAxisCut`
  ×4, `OtherOpticAxisCut` ×4, `OpticAxisCut` ×12, then `ZCut`, `XCut` and `YCut` ×8 each. `ZCut.experiment-json`
  records `data/noisy-observations-seed-1.csv`.

No deadline was exceeded, and no requirement is blocked.

## Artifacts

- `.artifacts/0434-probe/roundtrip.fsx`: the floating-point readback probe.
- `.artifacts/0434-attempt-01-dev-build.log`, `.artifacts/0434-attempt-01-dev-build-final.log`: development builds.
- `.artifacts/0434-attempt-01-dev-kit-acceptance.log`, `.artifacts/0434-attempt-01-dev-kit-acceptance-final.log`,
  `.artifacts/0434-attempt-01-dev-ui-smoke.log`, `.artifacts/0434-attempt-01-dev-ui-tests.log`: development runs.
- `.artifacts/0434-attempt-01-dev-results/`: their TRX reports.

## Gotchas

- The observation file stores the normalized elements and the throughput. The parser rebuilds each element as
  throughput × normalized, so exact readback rests on `T·((T·x)/T)` round-tripping. It does for every element of all
  five seeds, and the probe found no counter-example. If a later change writes absolute elements or another number
  format, the exact-equality assertions will name the element.
- In file order, the `OpticAxisCut` rows are not contiguous: A1's four, `OtherOpticAxisCut`'s four, then A2's twelve.
  `KeyedDataRows` keeps file order, so `OpticAxisCut`'s 16 points read A1's four first, then A2's twelve. That is the
  order of its scan's configurations in step 433.
- `MatchByTemplate` needs no draft capture here: every group matches an existing experiment, so `creating` is empty.
