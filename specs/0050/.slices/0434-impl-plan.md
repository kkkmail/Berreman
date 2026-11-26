# Step 0434 — implementation plan

AT-2's third part: the noisy observations become kit data files (spec 0050 S12.2; 001 §Q AT-2). All edits go into
`Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`, the step's only authorized file.

## Approach

1. **Harness data.** Three private functions, each with a one-line doc citing spec 0050 S12.2:
   - `noisyObservationsOf seed`: `noisyObserveAbsoluteWith` of the Berreman forward
     (`createBerremanForward (fun p -> p.opticalProperties) BiaxialSample.solverParameters`),
     `AbsorbingBiaxialSample.absorbing`, `NoiseParam.decentLab`, the seed and `fullConfigurations`. An `Error` fails
     the fact, naming the seed and the error;
   - `plateLabelOf cut`: `absorbingPlateCuts` (step 433) read from cut to plate label;
   - `observationFileOf seed`: `KeyedObservation.observationFileText` of those observations in harness order, each
     keyed `DataRowKey` of its plate's label.
2. **The build.** `buildAbsorbingKit` goes on after `buildAbsorbingGrids` with `attachNoisyObservations`. It:
   - writes `data/noisy-observations-seed-<n>.csv` for each of `AbsorbingBiaxialSample.noisySeeds`, in order, as the
     text's own bytes (UTF-8, no BOM, LF);
   - sends `SplitByColumn (seed 1's absolute file, ImportProfileName.muellerObservations)`, then asserts the `sample`
     key and five groups in file order, each `ExperimentForGroup` of the experiment its key names;
   - sends `MatchByTemplate`, then asserts the split is cleared, no experiment or setup is added, and every
     experiment holds seed 1's attachment.
3. **The fact** `AT-2 writes each seed's noisy observations through the Mueller-matrix observation profile and attaches
   seed 1's to its plates`:
   - `data/` holds exactly the five files, each byte-identical to `observationFileOf` regenerated in the fact;
   - each file reads back through `parseMuellerObservations` to its seed's observations keyed by plate, comparing
     every key, configuration and `measured.element i j` exactly;
   - a second `KitProxy.create` reads each experiment's `dataOpt`: the attachment path and SHA-256, the profile,
     `KeyedDataRows (DataRowKey <name>)` and no exclusion;
   - `built.model.experimentData.readMapped` gives `ObservationPoints` of the plate's seed-1 observations in file
     order, compared element by element.

## Risks

- **Exact readback.** The parser rebuilds every element as throughput × normalized, so readback is exact only if
  `T·((T·x)/T)` round-trips. A probe of 10⁶ random pairs in `.artifacts/0434-probe/` found no failure. The fact
  checks it anyway.
- **Effects on the step-432 and step-433 facts.** The split writes only the experiment files, so `library/`,
  setups, scans, roles and sessions are unchanged.
- **The census id holds no `@`**, so FS1104 does not apply and no `[<CompiledName>]` is needed.
