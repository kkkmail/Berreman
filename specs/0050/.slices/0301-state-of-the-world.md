# Step 0301 — state of the world

## Where we are

Spec 0050 section S9.2 covers import profiles and the column-mapping wizard (001 §K).

- Steps 298 to 300 added the closed sets, the kit profiles and the built-in profiles, with the observable's and the
  cut's disk tags and `KeyedObservation`.
- This step adds the Mueller-matrix observation file, the shape the `Mueller-matrix observations` profile reads. The
  app writes it, and AT-2 (001 §Q) will regenerate its kit files through it and compare them byte for byte. It lives
  in Storage as `LabFileImport.fs`.
- Nothing calls it yet. Step 302 reads lab files through the profiles.

## What's working

- Add `LabFileImport.fs` to Storage: the Mueller-matrix observation file (spec 0050 S9.2).
- `KeyedObservation.observationFileText` writes the header and one row per observation: key, configuration, the
  normalized m00…m33 and the absolute throughput, every float in round-trip `R` text, LF line ends, no BOM. It refuses
  a bad key, an infinite thickness, a micrometre wavelength or an un-normalizable matrix, naming the position and key.
- `KitFileText.parseMuellerObservations` reads the file back element for element. The matrix is the throughput times
  each normalized element, and the incidence is kept bit for bit. It refuses a wrong header, a header with no rows and
  the first bad row, naming the line and cell.
- `LabFileImportTests` gains three facts. constructor-unit-tests rises by 3, to 1506.

## Tests

These were development runs of each gate's own command over the final build. The arc-runner's gate engine runs the
gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors, no `error` in stdout | 8 s incremental (first full build 78 s) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded | 205 s | 600 s |
| constructor-unit-tests | Passed!, 1506 of 1506, 0 failed, 0 skipped | 72 s | 180 s |
| ui-tests (FsxAcceptanceTests) | Passed!, 14 of 14, 0 failed, 0 skipped | 79 s | 360 s |
| invariant-s30-\* (16) | each `INVARIANT HOLDS` (`section_invariants --find-nothing`, exit 0) | < 1 s each | 300 s |

- **constructor-unit-tests: 1506**, the 1503 baseline plus the 3 new facts. Each new identity is `Passed` in the TRX,
  exactly as the slice names it:
  - `the observation file writes each observation's configuration, normalized elements and throughput in one row`
  - `an observation file reads back to the same observations, element for element, and writes back to the same text`
  - `an observation file of another shape, or one the profile cannot write, is refused`
- **ui-tests** ran over `OpticalConstructor.Ui.Tests.FsxAcceptanceTests` only, as the step selects. All 14 passed;
  that class was not edited.
- **Mutation check (red).** One defect per fact failed all three facts, each naming its defect:
  - absolute elements written instead of normalized ones;
  - elements transposed on read;
  - the key check cut down to the empty key.

  The file was restored byte for byte (SHA-256 checked) and the solution rebuilt before the runs above.

```yaml
gates:
  constructor_unit_tests: 1506
  ui_tests: 14
```

## Architecture

- **One Storage module, two extension members.** Following the slice, the file's writer and reader are its only
  surface.
  - The writer is a static member on `KeyedObservation`, the Domain record step 300 made for this row.
  - The reader is an instance member on `KitFileText`, the kit's text type.
  - Every other piece is local to the two member bodies. This keeps every private module-level signature free of
    primitives (ELEV003P / ELEV010P).
- **The header is written once.** The private `observationFileColumns` lists the nine configuration and throughput
  names, and takes `m00 … m33` from `MuellerElementIndex.text` (step 300). The writer joins it; the reader compares the
  first line with it and names a refused cell from it.
- **First-error collection reuses `Softellect.Sys.Rop.unzip`.** It is generic and already referenced, so no third
  copy of `ItemEvaluation.sequenced`'s body was written. This follows the 0252 ruling against duplicated local
  collectors.
- **The arithmetic is `misreadAbsolute`'s.**
  - Written elements are `measured.normalized`. The read matrix is `throughput × normalized`, and its m₀₀ is the
    throughput itself.
  - The incidence is rebuilt as `IncidenceAngle (Angle radians)`, never through `IncidenceAngle.create`.
  - The thickness is written through the existing `Thickness.metersOpt`.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **The tags are read as integers** (`Int32.TryParse`, invariant), and every other number by `Double.TryParse`
  (`NumberStyles.Float`, invariant). `Int32.ToString("R")` is not a valid format, so the tags are written as invariant
  integers, and "every number in `R` text" applies to the floats.
- **The reader trims only one trailing `\r` per line.** It deliberately does not use `DataFileText.nonEmptyRows`, which
  trims all whitespace and would change a key with leading or trailing spaces.
- **The reader checks the 25-cell count before reading any cell.** Its cell readers are functions; an eagerly bound
  read of cell 7 would throw on a short row.
- **The reader is lenient where the slice is silent.** It does not refuse an empty key, a normalized m₀₀ other than 1
  or a non-positive throughput, and m₀₀ is always the throughput. The writer never produces any of these.
- **`Observable` is also an FSharp.Core module.** Storage calls `MuellerInverse.Observable.tryOfWire`, which needs
  `open OpticalConstructor.Domain`.
- **Storage compiles with `--warnaserror+:25`.** Removing a case from the writer's match is a build error, not a
  warning.
- **A cold full build reports two `SYSLIB0051` warnings** in the vendored, untouched
  `MathNetNumerics/Numerics/Exceptions.cs`. The incremental build after this step's change reports 0 warnings.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0301: the Mueller-matrix observation file (S9.2).
  - New `LabFileImport.fs` adds `KeyedObservation.observationFileText` and `KitFileText.parseMuellerObservations`.
  - New `LabFileImportTests` adds three facts; constructor-unit-tests rises by three, to 1506.
