# Step 0433 — implementation plan

AT-2's second part (spec 0050 S12.2; 001 §Q AT-2, §J): the build records, through three grids, the five setups and one
experiment per plate whose scans read in kit order `(a1 |> List.take 4) @ a2 @ (a1 |> List.skip 4) @ a3`.

## Approach

All edits are in `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`, the step's only
authorized file.

1. **Data.** Name the five plate items (`otherOpticAxisCutPlate`, `zCutPlate`, `xCutPlate`, `yCutPlate` beside
   `opticAxisCutPlate`), keep `absorbingItemSpecs` in the same order, and add the test's cut table
   `absorbingPlateCuts : (AcceptanceItem * SampleCut) list`.
2. **Build.** `buildAbsorbingKit` continues after `buildAbsorbingTable` with `buildAbsorbingGrids`: three grids, each
   `ExpNew`, the draft's axis messages (`ExpChooseElement`, `ExpChooseVariable`, `ExpSetRangeMin`, `ExpSetRangeMax`,
   `ExpSetRangePoints`, `ExpChooseMeasurement`, `ExpAddScanBlock` / `ExpAddScanAxis`), `OpenSetupGrid`,
   `ToggleSetupGridItem` per plate, `GenerateSetupGrid` of `model.setupGrid` (`gridOf`), `CloseSetupGrid`. Each grid
   checks that it appended exactly its plates' setups and experiments, named by the `{obj1}` template.
3. **Reading.** `configurationsOf (setups : Setup list) (experiment : Experiment) : MeasurementConfiguration list`, as the
   how-to defines it: each scan point in order, the plate's R2 setting the incidence (0 when none), its R1 the azimuth, the
   cut from Object 1's plate by `absorbingPlateCuts`, each capture's branches T then R.
4. **Fact.** `AT-2's grid generator records one experiment per plate whose multi-variable scan reproduces a1 @ a2 @ a3
   configuration by configuration`: the five setups (slots shared with the table assertion through one helper), the five
   experiments (setup revision 1, `ScienceRun`, no session, block counts 2/1/1/1/1), the 16/4/8/8/8 configuration counts
   and the configuration-by-configuration comparison, and the plate thickness / Laser wavelength read from the kit's
   `library/` snapshots.
5. **Step-432 fact.** It keeps every assertion. Its exact snapshot set grows by the Intensity-detector preset
   (`PresetIds.detectorIntensity`), which the setups now snapshot: the reflected detector slot keeps `AddElement
   Detector`'s seed (`SetupSlot.withItem` keeps a standalone model's `valueId`), and `Setup.libraryReferences` names it
   (0432-impl-log.md Gotchas). The table assertion's slot checks move into the shared helper unchanged.

## Risks

- The grid's `GenerateSetupGrid` reads the capture from `draftCapture` (the draft's element and measurement), so each
  draft must end on the plate with the measurement chosen.
- Configurations compare by structural equality of floats; `VariableRange.sweptValues` gives the design's angles exactly
  (0, 22.5, 45, 67.5; 20, 40, 60; 0, 90; 0, 45).
- No production file may change, so the leftover detector seed is asserted, not repaired.

## Attempt 02

The supervisor's kit-acceptance run observed `…reproduces a1 @2132`. F# compiles an `@`-bearing identifier as
`<prefix>@<line>` and warns FS1104. The fix: the fact's identifier drops the `@` (`a1, a2 and a3`), and
`[<CompiledName("…a1 @ a2 @ a3 configuration by configuration")>]` carries the census id into the IL method name, so
xUnit, the TRX and `--filter-method` all see the authorized id. No other line changes.
