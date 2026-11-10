# Step 0244 — impl log

## Progress

1. Read the protocol, the IMPLEMENT delta, the project prompt, the slice, its gate snapshot, AGENTS.md and the 0242 /
   0243 outputs (conventions, baseline `constructor_unit_tests: 1377`). Surveyed `ItemModels.fs`, `Kit.fs`
   (`ItemDial`), `ItemModelsTests.fs`, and every `ParameterName "<text>"` in the tree.
2. **Red first.** Added `every published parameter name is one ParameterName member` to `ItemModelsTests` as the brief
   words it; the test project build failed with `FS0039` for every new member (`0244-red-build.log`).
3. `ParameterName` members (one-line docs citing S8.1) and the publishers, as the brief words them, positions as
   `int`. Solution build: exit 0, 0 warnings. constructor-unit-tests: 1378 / 1378, the new fact `Passed`.
4. **Elevation from `f395597`: red, 4 × ELEV010** on `muellerElement`, `filmThickness`, `repeatedLayerThickness` and
   `inComponent`: "mixes bare primitive(s) int with elevated types in one signature".
   - **Cause:** the brief keeps the positions `int` on the strength of AGENTS.md's "members may speak their type's
     own primitives". `ParameterName`'s own primitive is `string`, not `int`.
   - **The analyzer has no own-primitive exemption anyway.** STL's `Analyzers/NoPrimitiveBesideElevated.fs` (read
     only) sanctions only the elevation-pattern members (`value`, `tryCreate`, `create`, the `ofDb` / `ofWire` /
     `toDb` / `toWire` pair), case constructors, overrides and loggers. It says no boundary mark exempts it either.
     That is why `ExperimentId.create (i : int)` and every `tryOfWire (tag : int)` pass, and these members do not.
   - **Fix:** see Decisions — one elevated type, `ParameterNameIndex`.
5. `ParameterNameIndex` in `ItemModels.fs`, before `ParameterName`. The four positional members take it and
   destructure it, so no `.value` is read. The publishers bind each index at origin. The test binds `one`, `two` and
   `three`.
6. Final runs over the final tree, solution rebuilt: see Testing state.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemModels.fs`:
  - `ParameterNameIndex`;
  - the `ParameterName` members;
  - `SourcePreset.lightSourceAxes`;
  - the publishers name the members.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`: `ItemDial.parameterAxes` names
  `ParameterName.dialZero`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemModelsTests.fs`: the fact `every published parameter
  name is one ParameterName member`.

All three are the slice's authorized `edits`. No other file changes. LF line endings: `git diff --numstat` equals
`--ignore-cr-at-eol`.

## Decisions

- **Deviation: `ParameterNameIndex`, the one type this step adds** (brief: "no other member, type or option is
  added"; "the positions stay `int`").
  - **Why:**
    - The acceptance requires elevation to pass, and ELEV010 refuses an `int` beside `ParameterName` in a member
      (progress 4).
    - AGENTS.md (operator, spec 0048 `.manual/002` R6): "Where any document … disagrees with the analyzers, the
      analyzers win". The brief's premise is that very disagreement.
    - A boundary mark cannot help (the rule ignores it), and editing the analyzers or the rules is forbidden.
  - **Why not an existing type:**
    - `MuellerRowIndex` / `MuellerColumnIndex` (`MuellerReconstruction.fs`) and `FilmItemIndex` /
      `CellLayerIndex` (`SampleStackEditor.fs`) compile after `ItemModels.fs`. Moving them means editing files
      outside the step's authorized `edits`. The last two are also 0-based where the names are 1-based.
    - `GroupMemberIndex` means something else.
  - **Why one type:** it is the smallest deviation that satisfies the rule. It stands exactly where the brief put
    `int`, with the same parameter names and numbering: a Mueller row or column 0 to 3, and a 1-based film,
    cell-layer or component position.
  - **Unchanged:** every member name, every text, order, kind and default. The ten contract symbols are created as
    listed.
  - **Elevated at origin:**
    - The black box maps `[ 0 .. 3 ]` to indices once. Its diagonal test compares indices.
    - Each `List.mapi` position is bound as a `ParameterNameIndex` in its own `let` before it is passed.
    - The test binds `one`, `two` and `three` the same way.
- **Doc comments.** Each new member has a one-line doc citing spec 0050 S8.1. Two-line docs go to
  `repeatedLayerThickness`, `inComponent`, `ParameterNameIndex` and `lightSourceAxes`. The `ParameterName` doc gains
  its S8.1 clause beside the S7.1 one. `SourcePreset.parameterAxes` keeps its S7.1 citation and adds S8.1.
- **Published names that stay outside `ParameterName`.** `MuellerInverse` (`n_o`, `g11`, …) and the Optimization
  project's `DesignParameters` (`film[k].thickness`, `substrate.wedgeAngle`) are not item-model publishers. They are
  outside the brief and outside this step's edits, so they are left as found.
- **Tests that still spell the text.** `KitItemTests`, `KitItemFileTests`, `ExperimentCopyTests` and the Ui tests
  spell names as text (`ParameterName "dialZero"`). They are outside the edits, and their facts are to run unchanged.
  The existing `ItemModelsTests` facts (steps 205–208) also keep their literal texts. That is deliberate: they now
  check that the members still produce the published text.

## Testing state

Development runs over the final tree. No gate verdict is claimed (Invariant 6).

- build (`dotnet build Berreman.slnx -c Release`): exit 0, `0 Warning(s)`, `0 Error(s)`.
- constructor-unit-tests (whole suite, as the gate's `all_discovered`): `Passed!`, 1378 / 1378, 69 s (estimate
  180 s). That is checkpoint 243's 1377 plus 1. `every published parameter name is one ParameterName member` is
  `Passed` in the TRX, and all 12 `ItemModelsTests` facts pass.
- elevation from the arc origin `f395597`: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 198 s. Known
  findings: 0. The first run, before `ParameterNameIndex`, was red with 4 × ELEV010 (progress 4).
- The fourteen s25 invariant greps, untracked files included: none finds anything.
- Red first: the new fact failed to compile, `FS0039` for every new member, before the production code
  (`0244-red-build.log`).

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

## Artifacts

- `specs/0050/.artifacts/0244-red-build.log` — the red compile, before the members.
- `specs/0050/.artifacts/0244-build.log` — the final solution build.
- `specs/0050/.artifacts/0244-constructor-unit-tests.log` — the final whole-suite run.
- `specs/0050/.artifacts/0244-elevation.log` — the final elevation run (green).
- `specs/0050/.artifacts/0244-invariants.log` — the fourteen s25 greps.

## Gotchas

- **The member exemption is not the analyzer's.** AGENTS.md's "members may speak their type's own primitives" does
  not cover ELEV010. That rule sanctions only `value` / `tryCreate` / `create` / the `ofDb`–`ofWire` pair, case
  constructors, overrides and loggers. Any public member that takes a primitive beside one of our types is a
  finding, whatever primitive the type wraps.
- **Later steps call these members with a `ParameterNameIndex`.** For example, the spec's step that adds
  `Sample.withThicknessValues` reads `ParameterName.filmThickness k` and `repeatedLayerThickness k j`. Its `k` and
  `j` are `ParameterNameIndex` values, bound at origin as `Sample.parameterAxes` binds them.
