# Step 0301 — implementation log

## Progress

- [x] Read the protocol, the project prompt, AGENTS.md, the slice and step 300's log and state of the world.
- [x] Checked the round trip numerically (t · nₖ through `/ t`, `R` text and `* t`) for t = 0.8125 and 0.03: all 15
  off-diagonal-of-m₀₀ elements come back exact.
- [x] `LabFileImport.fs` written and wired into Storage (directly after `MuellerDataStore.fs`).
- [x] `LabFileImportTests.fs` written and wired into the test project (end of the compile list).
- [x] Mutation (red) check: one defect per fact, all three red, file restored byte for byte.
- [x] Development runs: build, constructor-unit-tests, ui-tests (FsxAcceptanceTests), the sixteen s30 invariants,
  elevation.
- [x] State of the world written.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/LabFileImport.fs` (new): `module LabFileImport`.
  - `observationFileColumns` (private `string list`): the header's 25 column names, the one place they are written.
  - `type KeyedObservation with static member observationFileText`: the writer.
  - `type KitFileText with member this.parseMuellerObservations`: the reader.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`: one `<Compile>` line after
  `MuellerDataStore.fs`, no comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LabFileImportTests.fs` (new): `module LabFileImportTests`, the
  three facts the slice names.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: one `<Compile>` line at the
  end, no comment.

## Decisions

- **The header's element names come from `MuellerElementIndex.text`** (step 300), not a second `m00 … m33` spelling.
  The other nine names are written once, in the private `observationFileColumns`; the writer joins it for the header
  row, and the reader joins it to compare the first line and indexes it to name a refused cell. The test holds the
  whole header as a literal oracle, so a change to either list is caught.
- **No new Result collector.** Both members stop at the first refusal through `Softellect.Sys.Rop.unzip` (STL,
  already referenced by Storage): the successes in order, or the first failure. `ItemEvaluation.sequenced` is fixed to
  `ItemEvaluationError`, and the 0252 ruling (`.manual/016`) forbids a duplicated local collector.
- **The two tags are read by `Int32.TryParse`** (`NumberStyles.Integer`, invariant), every other number by
  `Double.TryParse` (`NumberStyles.Float`, invariant) as the slice says. A tag is an integer disk value (`tryOfWire`
  takes an `int`), and the writer writes it as an invariant integer, because `Int32.ToString("R")` is not a valid
  format. "Every number is `ToString("R")`" is therefore applied to every `float`.
- **The thickness is written through the existing `Thickness.metersOpt`** (`PropagationReadouts`), so `Infinity` is
  its `None` and no unit arithmetic is repeated.
- **The reader does not reuse `DataFileText.nonEmptyRows`.** That helper trims every line's whitespace. The slice
  trims only a trailing `\r`, so a key with leading or trailing spaces reads back as written.
- **The reader checks the cell count first**, then reads left to right (key; cut tag and tilt; metres, incidence and
  azimuth; observable tag; wavelength, the sixteen elements and the throughput). The first failing cell is named by
  line, 1-based cell number and column name.
- **The reader adds no rule the slice does not state.** It does not refuse an empty key, a non-unit normalized m₀₀ or
  a non-positive throughput. m₀₀ is the throughput itself, as the slice says.
- **Refusal order in the writer:** key, thickness, wavelength, then normalization. The slice does not fix an order;
  each refusal names the 1-based position and the key, and no text is returned.
- **Exact float comparison in the round-trip fact.** The slice requires bit-for-bit identity ("equals … exactly"), so
  `Assert.Equal<float>` is used rather than the tolerance helpers in `BerremanTests/MatrixComparison.fs`.

## Testing state

These were development runs of each gate's own command over the final build. The arc-runner's gate engine runs the
gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors, no `error` in stdout | 8 s incremental (first full build 78 s) | 600 s |
| constructor-unit-tests | Passed!, 1506 of 1506, 0 failed, 0 skipped | 72 s | 180 s |
| ui-tests (FsxAcceptanceTests) | Passed!, 14 of 14, 0 failed, 0 skipped | 79 s | 360 s |
| invariant-s30-\* (16) | each `INVARIANT HOLDS` (`section_invariants --find-nothing`, exit 0) | < 1 s each | 300 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded | 205 s | 600 s |

- **Elevation** ran with no comparison base, so it covered all 12 projects. Storage reports 0 findings. Its
  suppressed count (ELEV004, which the gate excludes and only measures, per R7) went from 157 at step 300 to 161. The
  four new `.value` reads are on the file's serialization line, which is where AGENTS.md allows `.value`.
- No run went over its effective deadline.

- **constructor-unit-tests: 1506**, the 1503 baseline (step 300) plus the 3 new facts. The TRX shows each new identity
  `Passed`, named exactly as the slice names it.
- **The first full build** recompiled the vendored MathNet C# project, which reports two `SYSLIB0051` warnings in
  `MathNetNumerics/Numerics/Exceptions.cs`. That code is vendored and untouched by this step; the final incremental
  build reports 0 warnings.
- **Mutation check (red).** One defect per fact, all in a single build:
  - the writer wrote the absolute elements instead of the normalized ones;
  - the reader transposed the elements;
  - the writer's key check was cut down to the empty key.

  All three facts failed, each naming its defect: `Expected: "1" Actual: "0.8125"`,
  `Expected: -0.0955… Actual: 0.1941…`, and `expected MalformedDataFileErr naming 'observation 2 ('a,b')', got Ok`
  (log `.artifacts/0301-mutation-red.log`). The first attempt to drop the `Mkm` case instead did not compile
  (`--warnaserror+:25`, an incomplete match), so the key check was used. The file was restored from a copy; SHA-256
  `5492acac…a479a5` matches before and after. The solution was rebuilt before the runs above.
- The new files are LF with no byte-order mark. `git diff --numstat` matches `--ignore-cr-at-eol`.
- The s30 invariants use `git grep`, which does not see untracked files, so the two new files were also grepped
  directly for every pattern: no match.

## Artifacts

- `specs/0050/.artifacts/0301-build-dev.log`: the final build.
- `specs/0050/.artifacts/0301-mutation-red.log`: the mutation run, all three facts red.
- `specs/0050/.artifacts/0301-mutation-sha-before.txt`: the SHA-256 of `LabFileImport.fs` before the mutation.
- `specs/0050/.artifacts/0301-constructor-unit-tests-dev.log`: the final suite run, 1506 passed.
- `specs/0050/.artifacts/0301-ui-tests-dev.log`: the FsxAcceptanceTests run, 14 passed.
- `specs/0050/.artifacts/0301-invariants-dev.log`: the sixteen invariant runs.
- `specs/0050/.artifacts/0301-elevation-dev.log`: the elevation run.

## Gotchas

- **`Observable` is also an FSharp.Core module.** Storage calls the step-300 extension as
  `MuellerInverse.Observable.tryOfWire`, which needs `open OpticalConstructor.Domain`.
- **Dropping a case of the writer's match does not compile.** Storage builds with `--warnaserror+:25`, so an
  incomplete match is an error, not a warning.
- **The reader's lets must not index a short row.** Every cell reader is a function, called only after the 25-cell
  check. An eagerly bound `tagAt 7` would throw on a short row.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.
