# Step 0018 — IMPLEMENT — impl log

## Progress

- [x] Read the protocol (the arc-runner base and the IMPLEMENT delta), the project prompt, the
      slice, its gates snapshot, and step 17's impl log and state of the world. The project
      prompt's Operator note section is empty.
- [x] Ran the slice's grep before editing. It found the preset ids at every site the slice
      names, and no preset id text in a comment. The "comments included" clause therefore had
      nothing to act on.
- [x] Switched the seven files with a count-asserting script,
      `.artifacts/0018-scripts/preset-ids.py`. This follows step 17's precedent: every
      replacement must match its exact expected count, and no file is written unless every
      replacement in every file matched. All of them matched on the first run.
- [x] Compiled and ran the affected suites locally (see Testing state). The first compile was
      clean.

## Files modified

The slice's authorized effects list seven files. These seven are the only product files touched
(56 lines in, 55 out).

- **`ElementRendererBindingTests.fs`** (+11 / −10).
  - Adds `open OpticalConstructor.Domain.Library` so `PresetIds` is in scope.
  - `drawableOf` takes `valueId : ElementValueId option` and binds it as given (the
    `Option.map ElementValueId` is gone).
  - `stateOf` takes `ElementValueId option`. Its test-local ids are wrapped where they are
    written: `Some (ElementValueId "ignored")`, `"lib:sample"`, `"lib:lp"` and `"lib:src"`.
  - The two render proofs (`:105`, `:124`) wrap their `"lib:lp"` / `"lib:sample"` the same way.
  - The HideNormals fact draws `Some PresetIds.polarizerLp`.
- **`InverseConstructorTests.fs:155`**: `BindValueIdTo (detector.id, PresetIds.detectorEllipsometer)`.
- **`LightSourceEditorTests.fs`**: `:173` is `LW.SelectLibraryEntry PresetIds.euv13p5`, and
  `:210` is `Scene.BindValueIdTo (sourceId, PresetIds.arf193)`.
- **`SolverHandoffWindowTests.fs`**.
  - `baseExperiment` takes `srcBind` and `detBind` as `Placement.ElementValueId option`.
  - `bindOne` takes `Placement.ElementValueId option` and binds the id it receives.
  - The 14 calls pass `Some PresetIds.source600` with `Some PresetIds.detectorIntensity`
    (13 calls), or with `Some PresetIds.detectorEllipsometer` (`:182`).
- **`WireUi0040CompositionTests.fs:253-261`** uses `ConstructorUiId.libraryEntryNode` over
  `PresetIds.source600` (1), `PresetIds.polarizerLp` (3) and `PresetIds.polarizerCpLeft` (1).
- **`WireUiCompositionTests.fs:197`**: `ConstructorUiId.libraryEntryNode PresetIds.source600`.
- **`WorkbenchInfoAndCaptureTests.fs`**.
  - `bind` and `filmOnR2` take an `ElementValueId`.
  - `glassFilm200Id` and `glassPlate1mmId` hold the sample's `entryId` (an `ElementValueId`,
    no `.value`).
  - The binds and lookups read `PresetIds.heNe633`, `arf193`, `krf248`, `xenonMonochromator`,
    `source600`, `detectorEllipsometer` and `detectorIntensity`.
  - `:125` is `Some PresetIds.heNe633.libraryEntryCode`.
  - The test-local "src-tune" (`:289-292`) stays.

## Decisions

- **`ElementRendererBindingTests.fs` gains `open OpticalConstructor.Domain.Library`.**
  - The slice writes the member as `Some PresetIds.polarizerLp`, and the file opened only
    `OpticalConstructor.Domain` and `…Placement`.
  - The other six files already open `Library`.
  - The open shadows nothing; the compile is clean.
- **`SolverHandoffWindowTests.fs` writes `Placement.ElementValueId` in its annotations.** The
  file does not open `Placement` and already qualified the type that way. The slice's
  `ElementValueId option` is the same type.
- **The ids in `ElementRendererBindingTests` are not presets.** `"lib:sample"`, `"lib:lp"`,
  `"lib:src"` and `"ignored"` are test-local ids. Following the slice, they are wrapped where
  they are written (AGENTS.md "Elevate at origin"), not replaced by members.
- **`WireUi0040CompositionTests` and `WireUiCompositionTests` are in no ui-tests selection.**
  Every fact in both files carries `ui-smoke`. The ui-smoke gate runs them, as the slice's
  Acceptance states.

## Testing state

**No gate was run.** Under Invariant 6, the gate engine is the only gate authority. The round
compiled the tree and ran the affected suites locally to be sure the construction is real code.
These are observations, not gate results.

- **Compile.** `dotnet build Berreman.slnx -c Release -nologo -v:m` (from `Berreman/`) exited 0
  with `Build succeeded`, 0 errors and no lowercase `error` in the log.
  - Its only warnings are the two pre-existing SYSLIB0051 warnings from the vendored
    `MathNetNumerics/Numerics/Exceptions.cs`. This step touches no MathNet code.
  - Log: `.artifacts/0018-build.log`.
- **ui-tests, selected over the five classes.** This used the gate's command and selector:
  ElementRendererBindingTests, InverseConstructorTests, LightSourceEditorTests,
  SolverHandoffWindowTests and WorkbenchInfoAndCaptureTests.
  - Result: `Test run summary: Passed!`, 36 total, 36 succeeded, 0 failed, 0 skipped.
  - The static count matches: 3 + 10 + 7 + 8 + 8 non-smoke facts.
  - Log: `.artifacts/0018-ui-tests-local.log`.
- **ui-smoke, every discovered fact.** Result: `Test run summary: Passed!`, 208 total,
  208 succeeded, 0 failed, 0 skipped.
  - This equals the last checkpoint (`ui_smoke_tests: 208`).
  - The TRX shows every smoke fact of the seven files Passed: ElementRendererBindingTests 2,
    InverseConstructorTests 1, LightSourceEditorTests 2, SolverHandoffWindowTests 2,
    WireUi0040CompositionTests 4, WireUiCompositionTests 3, WorkbenchInfoAndCaptureTests 3.
  - Log: `.artifacts/0018-ui-smoke-local.log`.
- **ELEV analyzers** (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`): `ELEV-GATE: OK - no
  in-scope project was touched`. The test project is outside the analyzers' scope, so its files
  are noted and not analyzed. Log: `.artifacts/0018-elevation-local.log`.

Read-only checks:

- **The slice's grep lists only test-local ids.**
  `git grep -n -E "(src|det|pol)-[a-z0-9]" -- Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests`
  returns exactly the ids the slice names:
  - `LightSourceEditorTests.fs`: "src-new-1", "src-x", "src-y", "src-u", "src-mine",
    "src-typed";
  - `ScrollExtentTests.fs:149`: "src-x";
  - `FsxAcceptanceTests.fs:546`: "src-test-10nm";
  - `WorkbenchInfoAndCaptureTests.fs:289` and `:292`: "src-tune".
- **The five s03 invariants each find nothing** (exit 1): `elev-list-empty`, `no-vstest`,
  `no-kit-kit-json`, `no-eps-axis-evaluated` and `complexity-mandatory`.
- **The test count cannot have moved.**
  - The diff adds and removes no `[<Fact>]`, `[<Theory>]`, `[<Trait>]` or `InlineData` line.
  - Each of the seven files has the same `[<Fact>]` count as at `HEAD`: 5, 11, 9, 10, 4, 3
    and 11.
- **LF holds.** `git diff --numstat` equals `--numstat --ignore-cr-at-eol` for all seven files,
  and none contains a CR byte.

## Artifacts

- `.artifacts/0018-scripts/preset-ids.py`: the count-asserting replacement script.
- `.artifacts/0018-step-diff.txt`: the step's diff over the seven files.
- `.artifacts/0018-build.log`: the local compile.
- `.artifacts/0018-ui-tests-local.log`: the local selected ui-tests run.
- `.artifacts/0018-ui-smoke-local.log`: the local ui-smoke run.
- `.artifacts/0018-elevation-local.log`: the local ELEV analyzer run.
