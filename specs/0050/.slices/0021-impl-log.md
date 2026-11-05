# Step 0021 — IMPLEMENT — impl log

## Progress

- [x] Read the protocol (the arc-runner base and the IMPLEMENT delta), the project prompt, the
      slice, its gates snapshot, and step 20's state of the world. The project prompt's Operator
      note section is empty.
- [x] Census before editing: `git grep -n -F "ProjectJson.options" -- Berreman/` found 25 lines
      (`.artifacts/0021-census-before.txt`). They are exactly the slice's files, plus the existing
      comment at `OpticalConstructor.Storage.fsproj:26`.
- [x] Created `CanonicalJson.fs` by copying `ProjectJson.fs:24-145` at HEAD.
      A `diff` against HEAD shows the only difference is the `options` doc comment.
- [x] Deleted the moved code and the three opens it alone used from `ProjectJson.fs`.
      `parseProject` and `serializeProject` now read `CanonicalJson.options`.
- [x] Re-pointed the readers and swept the comments with a count-asserting script,
      `.artifacts/0021-scripts/repoint.py`. Every replacement matched its exact expected count on
      the first run.
- [x] First local compile: clean. The constructor, ui-smoke and selected ui-tests suites were
      green.
- [x] First local ELEV run: **1 new finding**, `ELEV013 ProjectJson.fs:50:9` on `serializeProject`
      (`.artifacts/0021-elevation-local-run1.log`). See Decisions.
- [x] Repaired ELEV013. `serializeProject` became an extension member, and its callers moved to
      the member form (`.artifacts/0021-scripts/serialize-member.py`, count-asserting, matched
      on the first run).
- [x] Re-compiled and re-ran the suites and ELEV over the final tree: all green (see Testing
      state).

## Files modified

There are 13 edited files and 1 new file: 57 insertions and 177 deletions in the edits, and
`CanonicalJson.fs` has 132 lines. The full diff is in `.artifacts/0021-step-diff.txt`.

In the slice's authorized effects:

- **`OpticalConstructor.Storage/CanonicalJson.fs` (new).** `module CanonicalJson` holds `options`
  and the five private converters, moved verbatim.
- **`OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`.**
  - Adds `<Compile Include="CanonicalJson.fs" />` between `SchemaValidation.fs` and
    `ProjectJson.fs`.
  - The existing GroupsLibrary comment (`:26-27`) names `CanonicalJson`.
- **`OpticalConstructor.Storage/ProjectJson.fs`.**
  - The converters and `options` are gone, and so are the opens `System.Numerics`,
    `System.Text.Json.Serialization` and `MathNet.Numerics.LinearAlgebra`.
  - `parseProject` reads `CanonicalJson.options`.
  - `serializeProject` is now `type OpticalConstructorProject with member this.serializeProject`
    (ELEV013, see Decisions). Its body is unchanged except that it reads `this` and
    `CanonicalJson.options`.
- **`OpticalConstructor.Storage/GroupsLibrary.fs`.** The 3 reads (`:82`, `:90`, `:93`) and 2 docs
  (`:17`, `:86`) name `CanonicalJson.options`.
- **`OpticalConstructor.Storage/RecentFiles.fs`.** The 2 reads (`:55`, `:70`) and 1 doc (`:12`).
- **`OpticalConstructor.Storage/Report.fs`.**
  - The 3 reads in `MaterialLibrary` and its 2 docs.
  - `appendRevision` calls `project.serializeProject`, and the file gains
    `open OpticalConstructor.Storage.ProjectJson`.
- **`OpticalConstructor.Ui/UserEnvironment.fs`.** The 3 reads (`:367`, `:377`, `:380`) and 4 comments
  (`:13`, `:141`, `:250`, `:373`).
- **`OpticalConstructor.Ui/Localization.fs:39`.** Comment.
- **`OpticalConstructor.Domain/Groups.fs`.** Comments `:16`, `:23` and `:123`.
- **`OpticalConstructor.Domain/Placement.fs:279`.** Comment.
- **`OpticalConstructor.Tests/DispersionModelsTests.fs`.** The 2 reads in `AC-D8 …`.

Outside the listed files, as forced consumer repairs (see Decisions):

- **`OpticalConstructor.Storage/ProjectFile.fs`.** `saveProject` calls `project.serializeProject`,
  and its doc names `OpticalConstructorProject.serializeProject`.
- **`OpticalConstructor.Tests/PlacementTests.fs`.** Adds
  `open OpticalConstructor.Storage.ProjectJson`, and 1 call site is `project.serializeProject`.
- **`OpticalConstructor.Tests/ProjectJsonRoundtripTests.fs`.** Adds the same open, and 7 call sites
  use the member form.

## Decisions

- **Deviation: `serializeProject` becomes an extension member, and three files outside the listed
  effects change.**
  - The move itself raised the finding. ELEV013 (STL `Analyzers/NoSingleParameterFunction.fs`)
    exempts a one-domain-parameter free function whose body reads a module-level value of its
    own enclosing module.
  - `ProjectJson.serializeProject (project : OpticalConstructorProject)` read
    `ProjectJson.options` and so was exempt. Once the slice moves `options` to `CanonicalJson`,
    the exemption no longer holds, and the elevation gate fails.
  - AGENTS.md forbids silencing a finding, and a module value whose only job is to keep the
    exemption would be exactly that. The repair is therefore the one the analyzer names:
    `member this.serializeProject` on `OpticalConstructorProject`, declared as a type extension
    in `ProjectJson`.
  - Its callers change only in call form (`project.serializeProject`):
    - `Report.fs` (an authorized edit, which gains `open OpticalConstructor.Storage.ProjectJson`);
    - `ProjectFile.fs` (already opens `ProjectJson`);
    - `PlacementTests.fs` and `ProjectJsonRoundtripTests.fs` (each gains the same `open`).
  - `ProjectFile.fs`, `PlacementTests.fs` and `ProjectJsonRoundtripTests.fs` are outside the
    slice's file list. They lie inside the step's `touches` projects (`OpticalConstructor.Storage`,
    `OpticalConstructor.Tests`).
  - This is a consumer repair the authorized move forces, like step 13's `ExportImportTests.fs`
    precedent.
  - No fact's assertion changes, so the round-trip facts are unchanged in substance.
- **The fsproj's existing GroupsLibrary comment is re-pointed, not swept beyond.**
  `OpticalConstructor.Storage.fsproj:26-27` named `ProjectJson.options`, and the acceptance grep
  must find nothing. It now names `CanonicalJson.options` and says the store compiles after
  `CanonicalJson`. No comment is added to the fsproj.
- **Doc comments.**
  - The new module's doc says it is the stack every JSON store shares.
  - The `options` doc keeps its old text and adds "the one options value every JSON store of the
    app reads (spec 0050)", as the slice asks.
  - The converters' docs move verbatim.

## Testing state

**No gate was run.** Under Invariant 6, the gate engine is the only gate authority. The round
compiled the tree and ran the affected suites locally. These are observations over the FINAL tree,
not gate results.

- **Compile.** `dotnet build Berreman.slnx -c Release -nologo -v:m` (from `Berreman/`) exited 0,
  with 0 errors and 0 warnings.
  - The round's first build showed only the two pre-existing vendored MathNet SYSLIB0051
    warnings. MathNet was up to date for the second build.
  - Log: `.artifacts/0021-build.log`.
- **constructor-unit-tests, whole suite.** `Test run summary: Passed!`, 739 total, 739 succeeded,
  0 failed, 0 skipped. That equals the step-20 checkpoint (739).
  - The TRX shows every named round-trip class Passed, each with the same fact count as at HEAD:
    ProjectJsonRoundtripTests 7, RoundTripTests 3, GroupsRoundTripTests 9,
    EnvironmentRoundTripTests 33, ExportImportTests 7, DispersionModelsTests 31 and
    PlacementTests 10.
  - Log: `.artifacts/0021-constructor-unit-tests-local.log`.
- **ui-smoke, every discovered fact.** `Test run summary: Passed!`, 208 of 208. Log:
  `.artifacts/0021-ui-smoke-local.log`.
- **ui-tests, the 3 selected classes** (the gate's command and selector). `Passed!`, 26 of 26:
  AppShellSeamTests 3, DataFilePickerFolderTests 7 and LocalizationTests 16. Log:
  `.artifacts/0021-ui-tests-local.log`.
- **ELEV analyzers** (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`).
  - Final run: `ELEV-GATE: OK - 0 new finding(s) in 8 project(s)`. The 3 changed projects
    expanded with their dependents; Storage, Domain and Ui all have 0 findings. Log:
    `.artifacts/0021-elevation-local.log`.
  - The first run, before the member repair: 1 new ELEV013 on `ProjectJson.fs:50`. Log:
    `.artifacts/0021-elevation-local-run1.log`.

Read-only checks:

- **Acceptance grep.** `git grep -n -F "ProjectJson.options" -- Berreman/` finds nothing (exit 1),
  and neither does a plain `grep` over the untracked `CanonicalJson.fs`.
- **One options value.** `JsonSerializerOptions` is constructed only in `CanonicalJson.fs:119`.
- **The six s04 invariants each find nothing** (exit 1): elev-list-empty, no-vstest,
  no-kit-kit-json, no-eps-axis-evaluated, complexity-mandatory and no-seeded-string-preset-ids.
- **Verbatim move.** `diff` of `ProjectJson.fs:24-145` at HEAD against the converter and options
  body in `CanonicalJson.fs` differs only in the `options` doc comment.
- **LF holds.** `git diff --numstat` equals `--numstat --ignore-cr-at-eol`, and no changed or new
  file contains a CR byte.
- **No fact moved.** The test diff changes only call forms and adds opens. No `[<Fact>]`, assertion
  or name changed.

## Artifacts

- `.artifacts/0021-census-before.txt`: the `ProjectJson.options` census before the move (25 lines).
- `.artifacts/0021-scripts/repoint.py`: the count-asserting reader and comment re-point script.
- `.artifacts/0021-scripts/serialize-member.py`: the count-asserting ELEV013 caller repair.
- `.artifacts/0021-step-diff.txt`: the step's diff, with the new file appended.
- `.artifacts/0021-build.log`, `0021-constructor-unit-tests-local.log`,
  `0021-ui-smoke-local.log`, `0021-ui-tests-local.log`, `0021-elevation-local.log` and
  `0021-elevation-local-run1.log`: the local runs.
