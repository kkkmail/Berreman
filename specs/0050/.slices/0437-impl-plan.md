# Step 0437 — impl plan

AT-2's five-seed ensemble, on demand (spec 0050 S12.2; 001 §Q AT-2 "4. Full five-seed ensemble"). One file is
edited: `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`. Rulings R0001–R0005 bind
(no production edit here; their repairs are preserved by not touching product code).

## Approach

1. **Clock.** `testClockAt` returns the clock and its setter instead of the bare ref cell; `BuiltAcceptanceKit`
   carries the setter (`setTime`), filled by both builds (AT-1 passes it to `finish` as before, AT-2 moves it to
   `absorbingFitTime` through it). After its build, the ensemble moves the same clock to 12:0k for the k-th seed.
2. **Re-attachment, reused.** `attachNoisyObservations`'s split-and-match body becomes
   `attachSeedObservations (folder) (seed) (model)`; the build calls it for seed 1 after writing the five files, and
   the ensemble calls it for seeds 2–5 (the same `SplitByColumn` / `MatchByTemplate` messages). The helper also checks
   each experiment now holds that seed's file and its own key's rows.
3. **Recovered crystal, reused.** `assertAbsorbingRecovery`'s `promotedMaterialWith` read moves to
   `recoveredCrystalOf (study) (run)`; the recovery fact and the ensemble both use it.
4. **Fit times.** `ensembleFitTimes : (NoiseSeed * ClockTime) list`, the k-th seed of `noisySeeds` at
   2026-06-01T12:0k:00Z (seed 1's is `absorbingFitTime`).
5. **The fact** `AT-2's five-seed ensemble through the study engine keeps every reported quantity's scatter within the
   noisy suite's bands`, `[<Fact(Explicit = true)>]`, trait `kit-acceptance`, one `///` line citing S12.2:
   `buildAbsorbingKit (FolderName "AT-2-ensemble")`; for each later seed, move the clock, re-attach, check the study
   reads that seed's observations, `runStudyFitThroughShell` of `absorbingFitRequest`, expect the committed run name
   `fitRunNameAt`; read the five stored runs (names in seed order, equal to the window's), recover the five crystals,
   `statisticsFor` over `absorbingReportedQuantities`, every scatter above zero, `absorbingBandBreaches` over
   `rowFor`'s scatter empty (an `Error` failing the fact). The kit is closed in a `finally`.

## Risks

- The fact costs a build plus five ~811 s fits (~70 min); the gate only discovers it (Explicit → skipped, skips
  allowed). A development run on demand is the only evidence that the study engine's five fits keep the bands.
- The new attachment assertion also runs in AT-2's shared build; it restates what `assertNoisyObservationAttachments`
  already proves for seed 1, so it cannot newly fail there.
- No test identity besides the census's one is added; `KitAcceptanceTests`'s gate count stays 11 succeeded.
