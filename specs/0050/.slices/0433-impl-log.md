# Step 0433 — implementation log

## Progress

- Read the task, both system prompts, the project prompt, rulings R0001–R0003, the slice and the gate snapshot.
- Read step 0432's slice, impl-log and state of the world.
- Read the APIs the grids drive: `Experiments.fs` (draft, `addScanAxis` / `addScanBlock`, `ScanBlock.points`,
  `VariableRange.sweptValues`, `commitOverSetups`), `SetupGrids.fs` (`gridOver`, `generate`), `Setups.fs`
  (`withItem`, `libraryReferences`, `slotsAt`, `inLightOrder`), and the `ExpChooseElement` … `ExpAddScanBlock`,
  `OpenSetupGrid`, `ToggleSetupGridItem`, `GenerateSetupGrid` and `CloseSetupGrid` handlers and `draftCapture` /
  `experimentTargetsOver` in `TableAndElementRotationView.fs`. Also `KitContentFiles.fs` (`recordSetup`, reader order)
  and `AbsorbingBiaxialSample.fs` (`configuration`, `a1`–`a3`).
- Added the plate data, the three grids, `configurationsOf`, the assertions and the fact, all in `KitAcceptanceTests.fs`.
- Reviewed every identifier statically against its definition: union-case resolution (`VaryR1` / `VaryR2` /
  `CaptureBoth` resolve to `Experiments`, opened after `OpticalConstructor.Controls`; `ExperimentControls` is not
  `AutoOpen`; `ZCut` / `XCut` / `YCut` exist only in `MuellerInverse`), member hosts (`toSample` / `toSourcePreset` on
  `LibraryEntryContent`), and equality on `MeasurementConfiguration`. Reviewed the diff and line endings:
  `git diff --numstat` equals `--ignore-cr-at-eol` (235 / 18).

- **Attempt 02 (retry: kit-acceptance identity mismatch).** The supervisor's kit-acceptance run passed 8 of 8 but
  observed `…reproduces a1 @2132` where the census expects `…reproduces a1 @ a2 @ a3 configuration by configuration`.
  Diagnosis, measured:
  - reflection over the built DLL showed the IL method itself was `…reproduces a1 @2132`;
  - the gate's build log carries `KitAcceptanceTests.fs(2132,9): warning FS1104: Identifiers containing '@' are reserved
    for use in F# code generation`. F# compiles such an identifier as its prefix before the first `@`, then `@<line>`;
  - a `dotnet fsi` probe showed the same mangling for a `[<CompiledName>]` on an `@`-bearing identifier, and NO mangling
    and no warning for a `[<CompiledName("…@…")>]` on an identifier without `@`.
- Repair: the fact's F# identifier reads `…reproduces a1, a2 and a3 configuration by configuration`, and
  `[<CompiledName("AT-2's grid generator … a1 @ a2 @ a3 configuration by configuration")>]` carries the census id, with
  a two-line comment giving the reason. The body and every assertion are unchanged.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`, the step's only authorized edit.

## Decisions

- **The build (how-to (1)).** `buildAbsorbingKit` now calls `buildAbsorbingGrids` after the table. That runs three
  `generatePlateGrid` passes. Each one sends:
  1. `ExpNew`, then the draft's messages;
  2. `OpenSetupGrid` and one `ToggleSetupGridItem (sample, plate)` per plate;
  3. `GenerateSetupGrid (gridOf model)` (`model.setupGrid`), then `CloseSetupGrid`.

  The draft is built from three message lists:
  - `plateAxis` sends one axis: `ExpChooseElement sample`, `ExpChooseVariable`, `ExpSetRangeMin`, `ExpSetRangeMax`,
    `ExpSetRangePoints`.
  - `azimuthsInT` is R1 over 0…67.5° in 4 points, then `ExpChooseMeasurement CaptureT`.
  - `incidencesByAzimuthInTAndR low high points` is R2 over that range, `ExpAddScanAxis`, R1 over 0…90° in 2 points,
    then `ExpChooseMeasurement CaptureBoth`.

  The three grids:
  - (a) `azimuthsInT`, `ExpAddScanBlock`, then the incidences 20…60° in 3, over `OpticAxisCut`;
  - (b) `azimuthsInT` over `OtherOpticAxisCut`;
  - (c) the incidences 0…45° in 2, over `ZCut`, `XCut` and `YCut`, in that order.

  After each grid, the build asserts that it added exactly its plates' setups and appended exactly one experiment per
  plate, named by the `{obj1}` template. The kit status is quoted in the message. The capture of each block is the one
  `draftCapture` gives, because `ExpAddScanBlock` and `GenerateSetupGrid` read the capture there.
- **Data.** The five plate items are now named values (`otherOpticAxisCutPlate`, `zCutPlate`, `xCutPlate`,
  `yCutPlate`); `absorbingItemSpecs` keeps its order and ids. `absorbingPlateCuts` is the test's cut table:
  - `OpticAxisCut` → `AbsorbingBiaxialSample.opticAxisCut`;
  - `OtherOpticAxisCut` → `otherOpticAxisCut`;
  - `ZCut`, `XCut`, `YCut` → the cuts of the same name.
- **The reading (how-to (2)).** `configurationsOf setups experiment` takes the setup's slots at the experiment's
  revision and the plate in Object 1 (`SetupSlot.inLightOrder`, `ObjectSlotRole 1`). It maps that plate to its cut
  through `absorbingPlateCuts`. Then it reads each scan point in order:
  - the incidence is the plate slot's R2 setting, or 0 when the point sets none;
  - the azimuth is its R1 setting (a point without one fails, naming the experiment);
  - the capture gives T, R, or T then R.

  Each pair becomes `AbsorbingBiaxialSample.configuration cut incidence azimuth observable`.
- **The fact (how-to (3)).** A second `KitProxy.create` of the built folder reads:
  - **the five setups**, in `readSetups`' ordinal name order, so `XCut`, `YCut` and `ZCut` come after
    `OtherOpticAxisCut`. Each is at revision 1 with no history, and each holds the table's slots through the shared
    `assertAbsorbingSlots`;
  - **the five experiments**, in id order 1–5. Each is over its setup at revision 1, a `ScienceRun` with no session.
    `OpticAxisCut` has two blocks, every other one block;
  - **the configurations.** Counts per experiment are 16, 4, 8, 8 and 8. Joined, they are compared configuration by
    configuration with `(a1 |> List.take 4) @ a2 @ (a1 |> List.skip 4) @ a3`. A mismatch names the experiment's file,
    the index within it, the position of 44 and both configurations (`%A`). Two guards check that this expected order
    holds `fullConfigurations` exactly;
  - **the plate and the Laser**, read from the kit's `library/` snapshots. The sample's substrate thickness must equal
    `AbsorbingBiaxialSample.plate`, and the heNe633 preset's wavelength must equal `AbsorbingBiaxialSample.waveLength`.
- **The step-432 fact keeps every assertion, with one deviation (recorded per AGENTS.md "the working code is
  what is").** `assertAbsorbingLibrary`'s exact snapshot set grows by the Intensity-detector preset
  `PresetIds.detectorIntensity` (`ffa07a4d…@v1.detector-json`). The reason:
  - the reflected detector slot keeps `AddElement Detector`'s seed: `SetupSlot.withItem` leaves a standalone model's
    `valueId`, and the Mueller polarimeter is a standalone model;
  - `Setup.libraryReferences` names every slot `valueId`, and `KitLibraryContext.recordSetup` snapshots each one;
  - so the first setup recorded writes that preset, as 0432-impl-log.md's Gotchas predicted.

  The set is still exact, at version 1, and the snapshot is still byte-compared with its `Catalogue/Builtin/` file.
  Keeping "seven" would need a production change outside this step's effects. The table assertion's slot checks moved
  unchanged into `assertAbsorbingSlots`, which the setups reuse (AGENTS.md "Reuse, never copy"). Its slot-list
  message now reads "slots" rather than "table slots". `presetEntryIdOf` replaces 432's inline heNe633 lookup and
  serves both preset ids.
- **R0003.** The grids add no element, so the element-id provider still mints exactly one id. Any extra request would
  fail the build.

- **Attempt 02: the census id rides on `[<CompiledName>]`, not on xUnit's `DisplayName`.** The compiled name puts the
  id into the IL method name. From there xUnit's default display name, the TRX `testName` and `TestMethod name`,
  `--filter-method` (the gate's per-test retry selector) and ASG's FCS inventory (`dotnet_semantics.fsx` reads
  `CompiledName`) all carry the same string. A `DisplayName` would have left the method name and the retry filter on a
  different string. Renaming the identity is outside this step's authority, and so is suppressing FS1104 with `#nowarn`,
  which AGENTS.md's zero-warnings rule forbids.

## Testing state

Attempt 01 ran no build, test, analyzer or invariant gate: the IMPLEMENT role's Invariant 6 leaves gate
execution and verdicts to the supervisor. The code was checked statically against the APIs it calls (see Progress). No
requirement is blocked.

Static count: `KitAcceptanceTests` declares 8 facts with `Category=kit-acceptance` (7 before, +1). The new fact's id is
`OpticalConstructor.Ui.Tests.KitAcceptanceTests.AT-2's grid generator records one experiment per plate whose
multi-variable scan reproduces a1 @ a2 @ a3 configuration by configuration`. `StudyViewTests`, `DataStepTests` and the
ui-smoke facts are untouched. The edited file is in a test project, outside the ELEV scope. The added lines match no
s42 invariant pattern (checked with `git diff -U0`).

**Attempt 02 development runs** (development evidence, not gate verdicts; the supervisor runs the gates):
- `dotnet build OpticalConstructor.Ui.Tests.fsproj -c Release`: exit 0, 0 errors. The only warnings are the exempt
  NU1902 advisory (`Microsoft.AspNetCore.WebUtilities`); FS1104 is gone. Log:
  `.artifacts/0433-attempt-02-dev-build.log`.
- Reflection: the IL method is `AT-2's grid generator records one experiment per plate whose multi-variable scan
  reproduces a1 @ a2 @ a3 configuration by configuration`.
- kit-acceptance's command over `OpticalConstructor.Ui.Tests.KitAcceptanceTests`, with the TRX routed to the artifacts
  folder: `Passed!`, total 8, succeeded 8, in 66 s against a 3600 s deadline. The TRX `testName` is
  `OpticalConstructor.Ui.Tests.KitAcceptanceTests.AT-2's grid generator records one experiment per plate whose
  multi-variable scan reproduces a1 @ a2 @ a3 configuration by configuration`. Log:
  `.artifacts/0433-attempt-02-dev-kit-acceptance.log`; TRX: `.artifacts/0433-attempt-02-dev-results/kit-acceptance-dev.trx`.
- Line endings: `git diff --numstat` equals `--ignore-cr-at-eol` (238 / 18), no CR in the file.

## Artifacts

- `.artifacts/0433-attempt-02-dev-build.log` — attempt 02's development build of the UI test project.
- `.artifacts/0433-attempt-02-dev-kit-acceptance.log` and `.artifacts/0433-attempt-02-dev-results/kit-acceptance-dev.trx` —
  attempt 02's development run of the kit-acceptance class.

## Gotchas

- `readSetups` returns setups in ordinal name order and `readExperiments` in id order, so the five setups read as
  `OpticAxisCut, OtherOpticAxisCut, XCut, YCut, ZCut`, while the experiments (and the configurations joined "in kit
  order") follow `OpticAxisCut, OtherOpticAxisCut, ZCut, XCut, YCut`.
- `GenerateSetupGrid` and `ExpAddScanBlock` both take the capture from `draftCapture` (the draft's element and chosen
  measurement), not from the draft alone. A draft must end on the plate with its measurement chosen, or the grid
  records setups with no experiment. `generatePlateGrid`'s appended-experiments check catches that.
- The kit's `library/` now also holds `ffa07a4d-1fd4-4e71-83e9-d15f5dfa25b6@v1.detector-json` (the reflected detector's
  leftover seed). Later AT-2 steps that record more setups over the same table add no further snapshot.
- Each AT-2 fact asserts only what its part writes and later parts keep. This fact does not assert the experiments'
  data, notes or corrections, which steps 0434+ attach.
- **An F# identifier must never contain `@`** (FS1104). The compiler compiles it as `<prefix>@<line>`, even under
  `[<CompiledName>]`, and the warning breaks the zero-warnings rule. A test id that needs `@` keeps an `@`-free
  identifier and states the id in `[<CompiledName("…")>]`, as this fact does.
