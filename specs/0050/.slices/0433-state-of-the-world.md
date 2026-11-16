# Where we are

Step 0433 lands the second part of AT-2, the absorbing-biaxial-noisy acceptance kit (spec 0050 S12.2; 001 §Q AT-2,
§J). After step 0432's library drop, items and table, the build now runs the grid generator three times. That records
the five setups `OpticAxisCut`, `OtherOpticAxisCut`, `ZCut`, `XCut` and `YCut`, and one experiment per plate. Read in
kit order, the experiments' multi-variable scans reproduce `a1 @ a2 @ a3` with each plate's configurations together.
Steps 0434–0439 extend the same build with the data, the study and the fit. Attempt 02 repairs the new fact's
compiled name, which the supervisor's kit-acceptance run observed truncated to `…a1 @2132`.

# What's working

- AT-2's build records its five setups and five experiments through three grids, each over a fresh draft and grid panel.
- Added `configurationsOf`, which reads an experiment's scan as `AbsorbingBiaxialSample.configuration`s point by point.
- Added the fact checking the setups, the experiments, the plate and laser, and all 44 configurations against `a1`–`a3`.
- The step-432 fact keeps its assertions; its exact `library/` set adds the Intensity-detector seed the setups snapshot.
- The new fact compiles under its exact census id through `[<CompiledName>]`, clearing FS1104 and the `@<line>` mangling.

# Tests

**Attempt 02.** The supervisor's attempt-01 gates passed build, but kit-acceptance failed on identity. It passed 8 of 8,
yet observed `…reproduces a1 @2132` instead of the census id. The cause was F#'s handling of `@` in identifiers (FS1104;
see Gotchas). After the repair, these development runs were made (evidence only, not gate verdicts):
- the UI test project builds with 0 errors. Its only warnings are the exempt NU1902 advisory; FS1104 is gone;
- reflection shows the IL method under the exact census id;
- kit-acceptance's command over `OpticalConstructor.Ui.Tests.KitAcceptanceTests` reports `Passed!`, 8 of 8 succeeded,
  in 66 s against its 3600 s deadline. Its TRX `testName` is the authorized id
  `OpticalConstructor.Ui.Tests.KitAcceptanceTests.AT-2's grid generator records one experiment per plate whose
  multi-variable scan reproduces a1 @ a2 @ a3 configuration by configuration`.

No deadline was exceeded. The supervisor's gates stay authoritative.

**Attempt 01.** No gate was run in that round. The IMPLEMENT role's Invariant 6 leaves gate execution and verdicts
to the supervisor after the worker exits. As in steps 0428–0432, each attempt's final JSON reports no gate results, and
`commit_ready=false` only because no gate was run by the worker. No requirement or authority is blocked.

The 0433 roster is unchanged:
- build;
- elevation (the edited file is in a test project, outside the ELEV scope);
- kit-acceptance, selecting `OpticalConstructor.Ui.Tests.KitAcceptanceTests`;
- ui-smoke over every discovered smoke test;
- ui-tests, selecting `StudyViewTests` and `DataStepTests` (both untouched);
- the twenty-one s42 invariants. The added lines match none of their patterns.

The new fact `AT-2's grid generator records one experiment per plate whose multi-variable scan reproduces a1 @ a2 @ a3
configuration by configuration` raises the kit-acceptance count by one.

`KitAcceptanceTests` declares 8 facts with `Category=kit-acceptance`, up from 7, and attempt 02's development run
measured 8 succeeded. The supervisor's checkpoints stay authoritative.

```yaml
gates:
  kit_acceptance_tests: 8
```

# Architecture

Everything is in `KitAcceptanceTests.fs`.

- **The grids.** `buildAbsorbingGrids` runs three `generatePlateGrid` passes. Each one sends `ExpNew`, the draft's axis
  messages, `OpenSetupGrid`, one `ToggleSetupGridItem` per plate, `GenerateSetupGrid` of `model.setupGrid` and
  `CloseSetupGrid`. It also asserts that the grid added exactly its plates' setups and experiments. The axis messages
  are composed from `plateAxis`, `azimuthsInT` (A1's four azimuths in T) and `incidencesByAzimuthInTAndR` (an
  incidence axis by azimuths 0 and 90°, read in T then R).
- **The cuts.** `absorbingPlateCuts` is the test's table from each plate item to its `SampleCut`.
- **The reading.** `configurationsOf` maps each scan point of the plate in Object 1 to its configurations: R2 is the
  incidence (0 when the point sets none), R1 the azimuth, and the capture gives T, R or both.
- **Shared slot checks.** The table and every setup are checked by `assertAbsorbingSlots`, so the step-432 table
  assertion and the new setup assertions share one body.

# Deferred

Nothing is deferred. Steps 0434–0439 add AT-2's later parts, as the spec plans.

# Gotchas

- An F# identifier must never contain `@`. The compiler warns FS1104 and compiles it as `<prefix>@<line>`, even under
  `[<CompiledName>]`. A test id containing `@` keeps an `@`-free identifier and puts the id in `[<CompiledName("…")>]`.
- Setups read back in ordinal name order (`…, XCut, YCut, ZCut`). Experiments read back in id order (`…, ZCut, XCut,
  YCut`), which is the order the configurations are compared in.
- `GenerateSetupGrid` and `ExpAddScanBlock` take the block's capture from `draftCapture`. A draft that does not end on
  the plate with its measurement chosen records setups but no experiment.
- `library/` now holds `ffa07a4d…@v1.detector-json`, the reflected detector's leftover `AddElement Detector` seed. The
  step-432 fact's exact snapshot set includes it (0433-impl-log.md Decisions).

# Changelog

- 2026-11-15 — Step 0433: AT-2's second part. Three grids record five setups and one multi-variable experiment per plate.
  Added `configurationsOf` and the fact comparing all 44 configurations with `a1`–`a3`. The step-432 snapshot set adds
  the detector seed, and the table and setups share one slot check.
- 2026-11-15 — Step 0433 attempt 02: the new fact's id rides on `[<CompiledName>]`, because F# compiles an `@` identifier
  as `…a1 @2132` and warns FS1104. kit-acceptance now observes the authorized id.
