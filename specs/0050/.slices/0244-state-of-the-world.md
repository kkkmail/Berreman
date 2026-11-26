# Step 0244 — state of the world

## Where we are

Spec 0050, section S8.1 ("The run law walks every object; the source is real"), step 0244 (IMPLEMENT), obligation
`S81-PARAMETER-NAMES`.

- Steps 205–208 and 215 made the item models publish their parameter axes, but each publisher spelled its names as
  text.
- This step writes every published name once, as a member of `ParameterName`. The section's coming item-model
  evaluator can then read a value under the very name a model publishes.
- No name's text, order, kind or default changes.

## What's working

- Write every published parameter name once, as a `ParameterName` member (`azimuth` … `substrateThickness`,
  `muellerElement`, `filmThickness`, `repeatedLayerThickness`, `inComponent`).
- Add `SourcePreset.lightSourceAxes`, the four light-source axes; `SourcePreset.parameterAxes` answers it.
- Make every publisher name the members, `ItemDial.parameterAxes` in `Kit.fs` included.
- Add `ParameterNameIndex` for the positions in a name, so ELEV010 passes (a recorded deviation).
- Add the fact `every published parameter name is one ParameterName member`.

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster:

- build and elevation;
- constructor-unit-tests over everything discovered;
- the fourteen s25 invariants.

The worker's development runs over the final tree, with logs under `specs/0050/.artifacts/0244-*`:

- **build:** exit 0, `0 Warning(s)`, `0 Error(s)`.
- **constructor-unit-tests:** `Passed!`, 1378 succeeded, in 69 s (estimate 180 s).
  - That is checkpoint 243's 1377 plus `every published parameter name is one ParameterName member`, `Passed` in
    the TRX.
  - Every earlier `ItemModelsTests` fact, those of steps 207 and 208 included, passes unchanged.
- **elevation** from the arc origin `f395597`: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, in 198 s.
  Known findings: 0.
- **The fourteen s25 invariant greps,** untracked files included: none finds anything.
- **Red before green:** the new fact failed to compile before the production code (`FS0039` for every new member).

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1378
```

## Architecture

- **One home per published name.** Every name an item model publishes is a static member of `ParameterName`, or a
  positional member over `ParameterNameIndex`. Each publisher names the member, so the text exists once.
- **A compound prefixes through `inComponent`,** so a compound inside a compound prefixes twice (`c2.c1.…`), as
  before.
- **The light source's axes are a static list,** `SourcePreset.lightSourceAxes`. A run with no bound source can read
  the same default set without a preset instance.
- **Deviation from the brief: `ParameterNameIndex`.** The brief kept the positions `int` under AGENTS.md's member
  exemption, and said no other type is added.
  - ELEV010 has no such exemption. STL's analyzer sanctions only `value`, `tryCreate`, `create`, the
    `ofDb`/`ofWire` pair, case constructors, overrides and loggers.
  - The first elevation run was red, with 4 × ELEV010.
  - AGENTS.md (spec 0048 `.manual/002` R6) rules that the analyzers win over any document, and the acceptance
    requires elevation to pass.
  - So one single-case DU over `int` stands exactly where the brief put `int`, with the same numbering: Mueller rows
    and columns 0 to 3, positions 1-based.
  - The existing index types (`MuellerRowIndex`, `MuellerColumnIndex`, `FilmItemIndex`, `CellLayerIndex`) compile
    after `ItemModels.fs`, in files outside this step's edits.
  - The impl-log's Decisions section records the full reasoning.

## Deferred

Nothing in this step's scope is deferred.

## Gotchas

- **Positional members take a `ParameterNameIndex`, not an `int`.** A later step that reads `ParameterName.filmThickness k` or
  `repeatedLayerThickness k j` (the spec's `Sample.withThicknessValues` step) binds `k` and `j` as `ParameterNameIndex`
  at origin, as `Sample.parameterAxes` does.
- **AGENTS.md's "members may speak their type's own primitives" does not exempt a member from ELEV010.** A public
  member taking a primitive beside one of our types is a finding unless it is one of the elevation-pattern names.
- **Some names stay outside `ParameterName`.** `MuellerInverse`'s fitted names (`n_o`, `g11`, …) and Optimization's
  `DesignParameters` names (`film[k].thickness`) are not item-model publishers. They are outside this step and
  unchanged.
- **Older facts still spell the text.** Tests outside this step's edits (`KitItemTests`, `KitItemFileTests`, the Ui
  tests) and the earlier `ItemModelsTests` facts still write names as text. That is deliberate: they pin the text
  the members must produce.

## Changelog

- 2026-11-10 — Step 0244:
  - every published parameter name is one `ParameterName` member;
  - `SourcePreset.lightSourceAxes` added;
  - the publishers name the members;
  - `ParameterNameIndex` elevates the positions for ELEV010;
  - one constructor-unit-tests fact added.
