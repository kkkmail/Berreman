# Step 0017 — IMPLEMENT — impl log

## Progress

- [x] Read the protocol (the arc-runner base and the IMPLEMENT delta), the project prompt, the
      slice, its gates snapshot, step 16's impl log and the oracle requirement for this step
      (`.acceptance-oracle-json:858`). The project prompt's Operator note section is empty.
- [x] Counted every preset literal in `LibraryWindowTests.fs` before editing:

  | id | `(ElementValueId "…")` | other `ElementValueId "…"` | bare text |
  |---|---|---|---|
  | `src-600` | 37 | 2 | 4 |
  | `pol-lp` | 7 | 0 | 2 |
  | `pol-cp-left` | 2 | 0 | 1 |
  | `pol-cp-right` | 0 | 0 | 1 |
  | `det-intensity` | 8 | 0 | 5 |
  | `det-ellipsometer` | 1 | 0 | 2 |

  "Other" means `ElementValueId "…"` without its own parentheses: the two tuple elements of
  `SelectLibraryEntryNode (…, code)`. "Bare text" means the id string outside an
  `ElementValueId`.
- [x] Switched the file with a count-asserting script, `.artifacts/0017-scripts/preset-ids.py`.
  Every pattern matched exactly its expected count. The script then confirmed that the slice's
  grep finds no line before it wrote the file.
- [x] Compiled and ran the affected suites locally (see Testing state). The first compile was
  clean.

## Files modified

`Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LibraryWindowTests.fs` (67 lines
changed, 67 in and 67 out) is the only file in the slice's authorized effects, and the only
product file touched.

- **(1) Ids as values.** Every `ElementValueId "src-600"` / `"pol-lp"` / `"pol-cp-left"` /
  `"det-intensity"` / `"det-ellipsometer"` is now `PresetIds.source600` / `polarizerLp` /
  `polarizerCpLeft` / `detectorIntensity` / `detectorEllipsometer`. Where the literal had its own
  parentheses, they went with it. The members now appear inside:
  - `ConstructorUiId.libraryEntryNode`;
  - `.libraryEntryCode` (for example `PresetIds.source600.libraryEntryCode.tryLibraryEntryId`);
  - `.libraryBranchLeaf`;
  - `SelectLibraryEntry`, and `SelectLibraryEntryNode (PresetIds.source600, leafCode)`;
  - `Some PresetIds.<m>`.
- **(2) Text compared with `filteredEntryIds` or the `onSelected` recorder.**
  - `:437` and `:623` use `PresetIds.source600.value`.
  - `:447` uses `PresetIds.polarizerLp.value`.
  - `:1080` uses `[ PresetIds.polarizerLp.value ]`.
  - `:1102` and `:1106` use `"selected:" + PresetIds.detectorIntensity.value`.
  - `:1151` uses `"selected:" + PresetIds.detectorEllipsometer.value`.
  - `:1073` is `List.sort [ PresetIds.polarizerCpLeft.value; PresetIds.polarizerCpRight.value ]`.
  - `:1144` is `List.sort [ PresetIds.detectorEllipsometer.value; PresetIds.detectorIntensity.value ]`.
- **(3) The id-format facts.**
  - `:251` expects `"FacetTreeNode_entry:" + PresetIds.source600.value`.
  - `:370` expects `"branch:" + entryKindFacetKey.value + ":Source:entry:" + PresetIds.source600.value`.
- **(4) The Select-bay rows.** `:1372` and `:1383` use
  `ConstructorUiId.libraryEntry PresetIds.detectorIntensity.uiKey`.

## Decisions

- **No `open` was added.** The file already opens `OpticalConstructor.Domain.Library`, which
  brings `PresetIds` into scope. It also opens `OpticalConstructor.Ui`, whose
  `[<AutoOpen>] ConstructorUiIdOfDomain` provides `ElementValueId.uiKey`. `:1361` already wrote
  `glassFilm600EntryId.uiKey` the same way. Only one `PresetIds` module exists among the opened
  namespaces (`ElementId.fs:66`).
- **Parenthesised literals lose their parentheses.** `Some (ElementValueId "src-600").libraryEntryCode`
  became `Some PresetIds.source600.libraryEntryCode`. A long identifier is one atom, so this
  still parses as `Some (….libraryEntryCode)`, and the compile confirms it.
- **The `// pending` comment at `:1383` keeps its three-space gap.** It was not aligned with the
  next line's comment before the edit either, so the line was not re-padded.
- **No comment held preset id text.** Before the edit, the slice's grep matched only code lines,
  so the "comments included" clause had nothing to act on.

## Testing state

**No gate was run.** Under Invariant 6 the gate engine is the only gate authority. To be sure
the construction is real code, the round compiled the tree and ran the affected suites locally.
These are observations, not gate results.

- **Compile.** `dotnet build Berreman.slnx -c Release -nologo -v:m` (from `Berreman/`) exited 0,
  with `Build succeeded` and 0 errors. The log has no lowercase `error`. Its only warnings are
  the two SYSLIB0051 warnings from the vendored `MathNetNumerics/Numerics/Exceptions.cs`. They
  were there before this step, which touches no MathNet code. Log: `.artifacts/0017-build.log`.
- **ui-tests, selected `OpticalConstructor.Ui.Tests.LibraryWindowTests`** (the gate's command and
  selector): `Test run summary: Passed!`, 39 total, 39 succeeded, 0 failed, 0 skipped. The file
  declares 61 `[<Fact>]`s: 22 carry `ui-smoke`, which `--filter-not-trait` excludes, leaving 39.
  Log: `.artifacts/0017-ui-tests-local.log`.
- **ui-smoke, every discovered fact**: `Test run summary: Passed!`, 208 total, 208 succeeded,
  0 failed, 0 skipped. That equals the last checkpoint (`ui_smoke_tests: 208`). The 208 include
  `:1372` and `:1383`, the two Select-bay facts. Log: `.artifacts/0017-ui-smoke-local.log`.
- **ELEV analyzers** (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`): `ELEV-GATE: OK - no
  in-scope project was touched`. The test projects are outside the analyzers' scope, so the file
  is noted and not analyzed. Log: `.artifacts/0017-elevation-local.log`.

Read-only checks:

- **The slice's grep finds nothing** (exit 1):
  `git grep -n -E "(src|det|pol)-[a-z0-9]" -- Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LibraryWindowTests.fs`.
- **The five s03 invariants each find nothing** (exit 1): `elev-list-empty`, `no-vstest`,
  `no-kit-kit-json`, `no-eps-axis-evaluated` and `complexity-mandatory`.
- **The test count cannot have moved.** The diff adds and removes no `[<Fact>]`, `[<Theory>]`,
  `[<Trait>]` or `InlineData` line. The file had 61 `[<Fact>]`s before the edit (at `HEAD~2`)
  and has 61 after.
- **LF holds.** `git diff --numstat` equals `--numstat --ignore-cr-at-eol` (67 / 67), and the
  file contains no CR byte.

## Artifacts

- `.artifacts/0017-scripts/preset-ids.py`: the count-asserting replacement script.
- `.artifacts/0017-build.log`: the local compile.
- `.artifacts/0017-ui-tests-local.log`: the local selected ui-tests run.
- `.artifacts/0017-ui-smoke-local.log`: the local ui-smoke run.
- `.artifacts/0017-elevation-local.log`: the local ELEV analyzer run.
