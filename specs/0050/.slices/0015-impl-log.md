# Step 0015 — IMPLEMENT — impl log

## Progress

- [x] Read the protocol (arc-runner base and the IMPLEMENT delta), the project prompt, the
      slice, its gates snapshot, step 14's state of the world and the `S13-WELL-KNOWN-IDS`
      oracle entry. The project prompt's Operator note section is empty.
- [x] Domain: `SampleIds` and `PresetIds` in `ElementId.fs`. The seeds read them. The
      replacement ran as a script (`.artifacts/0015-scripts/seed-ids.py`) that asserts each
      literal's count, so no literal was missed or doubled.
- [x] Domain: `detectorKindOf` doc in `ExperimentDataLoad.fs`.
- [x] Ui: delete `SeedEntryIds`. `defaultSeedEntry` and `paletteButtons` read `PresetIds`.
- [x] Tests: MainSceneMsgTests, TableAndElementRotationTests, ExperimentControlsTests and
      FsxAcceptanceTests, each by a count-asserting script under `.artifacts/0015-scripts/`.
- [x] Local compile and local test runs. These are construction checks, not gate runs (see
      Testing state).
- [x] Read-only checks: greps, LF, the five s03 invariants.

## Files modified

Exactly the seven files in the slice's authorized effects.

- `OpticalConstructor.Domain/ElementId.fs`:
  - **`module SampleIds`**, directly after `newSampleId`: eleven `SampleId` values,
    `glassPlate1mmId` … `langasiteSiliconId`. Each Guid literal moved, character for character,
    out of its `SeedSamples` record, and each record now reads `id = SampleIds.<name>Id`.
  - **`module PresetIds`**, directly after it: 33 `ElementValueId` values, each written
    `ElementValueId "<its current id>"`. Blank lines group them as the seeds do: the reference
    source, the lasers, the lithography sources, the tunables, then the detectors and polarizers.
  - Both are plain modules shaped like `MaterialLibrary.MaterialIds`: no
    `[<RequireQualifiedAccess>]`, no `[<Literal>]`, every member annotated with its type.
  - `SeedSources` passes the members to `source` / `tunable`. The five `seedEntries`
    detector and polarizer records and the five `seedTrees` leaves read them.
  - Docs:
    - `PresetIds` and `SampleIds`: two lines each, citing spec 0050 §3.3.
    - `SampleId`: the fixed literals are in `SampleIds`.
    - `SeedSamples`: each id is its literal in `SampleIds`. The "a new seeded sample adds a
      new literal here" clause went, because it is no longer true.
    - `sampleLeaf`: its doc also said the literal "lives only in `SeedSamples`", and now says
      `SampleIds`.
- `OpticalConstructor.Domain/ExperimentDataLoad.fs`: `detectorKindOf`'s doc names
  `PresetIds.detectorIntensity` / `PresetIds.detectorEllipsometer`. The paragraph was reflowed
  and its wording is otherwise unchanged.
- `OpticalConstructor.Ui/TableAndElementRotationView.fs`:
  - `module SeedEntryIds` and its doc are deleted.
  - `defaultSeedEntry` returns `Some Library.PresetIds.source600` / `detectorIntensity` /
    `polarizerLp` / `polarizerCpLeft`. Its doc's `pol-cp-right` reads
    `PresetIds.polarizerCpRight`.
  - `paletteButtons`' CPL and CPR prebinds are `Some Library.PresetIds.polarizerCpLeft` and
    `Some Library.PresetIds.polarizerCpRight`.
- `OpticalConstructor.Ui.Tests/MainSceneMsgTests.fs`:
  - The theory `AddElement lands PRE-BOUND to the kind's seeded default entry` has the rows
    `"S"`, `"D"`, `"LP"` and `"CP"`, and takes `(code : string)`.
  - A private `expectedSeedOf (code : string) : ElementValueId` maps each code to its
    `PresetIds` member. It is declared beside `kindOfCode`, and like it, fails on an unknown
    code.
  - The theory's name, its four rows, its three assertions and its failure message are
    unchanged. The message renders `%s{seedId.value}`, which is the same text as before.
  - The CPR fact (`:147-151`), the palette fact (`:162-164`) and the seven detector binds
    (`:490-507`, `:598-604`) read the members.
- `OpticalConstructor.Ui.Tests/TableAndElementRotationTests.fs:532`: `Some
  Library.PresetIds.polarizerLp` / `polarizerCpLeft` / `polarizerCpRight`.
- `OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs`:
  - `bind` takes `entryId : ElementValueId`.
  - `glassFilm200Id`, `glassFilm600Id`, `glassPlate1mmId` and `euvMirrorId` hold
    `(Library.SampleItem …).entryId` itself, and their doc says the literal lives in
    `Library.SampleIds`.
  - `:631` passes `glassFilm200Id`.
  - The binds read `Library.PresetIds.*`: `withTunableSource` binds `xenonMonochromator`,
    `:109` binds `heNe633` and `:280` binds `detectorEllipsometer`.
  - `:308` and `:347` read the members. The comments at `:307`, `:308` and `:318` name
    `PresetIds.polarizerLp`, `polarizerCpRight` and `polarizerCpLeft`. The two trailing
    comments at `:307-308` were re-aligned.
- `OpticalConstructor.Ui.Tests/FsxAcceptanceTests.fs`:
  - `TableRun.sourceEntry : ElementValueId`.
  - `tableRun`'s `source` parameter follows.
  - `userSource` takes and returns the `ElementValueId` it saves. Its "src-test-10nm" is
    elevated at the literal, at the one call site.
  - `runOnTable` binds `run.sourceEntry` and `PresetIds.detectorIntensity` directly.
  - `glassOnTable` and the two 600 nm runs pass `PresetIds.source600`. `xenon` is
    `PresetIds.xenonMonochromator`, and `euvAgainstWaveLength` passes
    `PresetIds.euvBeamline`.
  - The sample entries were changed as well (see Decisions):
    - `sample` returns `(SampleItem s).entryId`;
    - `TableRun.sampleEntry`, `tableRun`'s `sampleEntry` parameter, `filmsSample`,
      `quarterWaveStack`, `glassPairsStack` and `euvStack` carry `ElementValueId`;
    - `runOnTable` binds `run.sampleEntry` directly.

## Decisions

- **The FsxAcceptanceTests sample entries hold `ElementValueId` too.** Item (4)'s rule is: "A
  fixture holds a preset or sample id as an `ElementValueId` … so no fixture re-wraps `.value`".
  Before this step, `sample` returned `(SampleItem s).entryId.value`, and `runOnTable` wrapped it
  back as `ElementValueId run.sampleEntry`, which is exactly that round trip. Item (d) names only
  the source side, but the rule covers the sample side of the same file, so both were changed. No
  fact's body or assertion changed.
- **`Library.PresetIds.*` where the test file does not open `Library`.**
  `TableAndElementRotationTests` and `ExperimentControlsTests` have no `open
  OpticalConstructor.Domain.Library`, and `ExperimentControlsTests` already writes
  `Library.SeedSamples` / `Library.SampleItem`. The two files therefore name
  `Library.PresetIds.<member>`, the same form the slice gives for the Ui view. They name the same
  members the slice lists. Adding an `open` would have put every `Library` name in scope
  over the files' existing opens.
- **`userSource` takes the `ElementValueId`.** Item (4) says "a helper that wrapped a `string`
  id takes the `ElementValueId`", and item (d) says it "returns the `ElementValueId` it saves".
  Both hold now. The test-local id "src-test-10nm" is unchanged and is elevated at its literal.
- **The pre-binding theory's failure message** renders the id through `.value` (`%s{seedId.value}`).
  That keeps the message text identical, and `.value` is used only to render to the test output.
- **`expectedSeedOf` fails on an unknown code** with `failwith`, the way `kindOfCode` beside it
  already does. A row the map does not know is a defect in the test.

## Testing state

**No gate was run.** Under Invariant 6 the gate engine is the only gate authority. To be sure
the construction is real code, the round compiled the tree and ran the affected suites locally.
These are observations, not gate results:

- **Compile.** `dotnet build Berreman.slnx -c Release -nologo -v:m` exited 0 with 0 errors. The
  log contains no lowercase "error". The only warnings are the two vendored MathNet SYSLIB0051
  ones that were already on the start tree. Log: `.artifacts/0015-build.log`.
- **The four selected ui-tests classes**, under the gate's trait filters: 147/147 passed.
  Log: `.artifacts/0015-ui-tests-local.log`.
- **ui-smoke, whole suite**: 208/208 passed. That equals the step 14 baseline, and it
  includes the edited `ExperimentControlsTests` smoke fact. Log:
  `.artifacts/0015-ui-smoke-local.log`.
- **constructor-unit-tests, whole suite**: 736/736 passed. That equals the last checkpoint
  (`constructor_unit_tests: 736`). Log: `.artifacts/0015-constructor-unit-tests-local.log`.
- **ELEV analyzers** (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`): `ELEV-GATE: OK - 0 new
  finding(s) in 8 project(s)`, with 766 suppressed findings, the same number as step 13's run.
  Log: `.artifacts/0015-elevation-local.log`.

Read-only checks:

- The five s03 invariants each find nothing (exit 1): `elev-list-empty`, `no-vstest`,
  `no-kit-kit-json`, `no-eps-axis-evaluated` and `complexity-mandatory`.
- `SeedEntryIds` occurs nowhere outside `specs/`.
- **Each identity is written once.**
  - Product code holds each of the 33 preset id literals exactly once, in `PresetIds`.
  - Each of the eleven sample Guids occurs exactly once, in `SampleIds`.
- The four test files contain no seeded preset id text. The only id text left is the
  test-local "src-test-10nm".
- No fixture in the four files wraps an id it unwrapped: no `ElementValueId <identifier>`
  wrap and no `entryId.value` remains.
- **The test counts cannot have moved.** The test-file diff adds and removes no `[<Fact>]`,
  `[<Theory>]` or `[<Trait>]`. The theory's four `InlineData` rows were rewritten in place,
  four for four.
- **LF holds.** `git diff --numstat` equals `--numstat --ignore-cr-at-eol` for all seven
  files, and none of them contains a CR byte.

## Artifacts

- `.artifacts/0015-step-diff.txt`: the full `git diff -- Berreman/` of this step.
- `.artifacts/0015-scripts/`: the count-asserting replacement scripts (`seed-ids.py`,
  `main-scene-tests.py`, `experiment-controls-tests.py`, `fsx-acceptance-tests.py`).
- `.artifacts/0015-build.log`, `0015-ui-tests-local.log`, `0015-ui-smoke-local.log`,
  `0015-constructor-unit-tests-local.log` and `0015-elevation-local.log`: the local
  observations.

## Gotchas

- None from the operator. The project prompt's Operator note section is empty.
- **Two comments outside this step's authorized effects still say the sample literal lives in
  `SeedSamples`:**
  - `OpticalConstructor.Tests/PropagationTests.fs:205`;
  - `OpticalConstructor.Ui.Tests/LayerBandsControlsTests.fs:27`.

  The slice does not authorize those files, so they were left alone. Both comments describe
  facts that read `SeedSamples` values, which S1.7 deletes, so they change with those facts.
- **The xUnit display names of the four theory rows change**, from `(code: "S", seedId:
  "src-600")` to `(code: "S")`. The fact name and the row count are the same, so a filter by
  class or method still selects all four.
- **`ui_tests` has no carried baseline.** Its count depends on the step's class filter: 147
  here over four classes. The SoW's YAML carries only the whole-suite keys.
