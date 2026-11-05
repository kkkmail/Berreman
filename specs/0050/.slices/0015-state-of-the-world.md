# Step 0015 — IMPLEMENT — state of the world

## Where we are

This is step 15 of spec 0050, in section S1.3 ("Every library entry is identified by a frozen
GUID"), under obligation `S13-WELL-KNOWN-IDS`.

- Every built-in identity is now named in one place in Domain:
  - `MaterialIds` and `CategoryIds`, which already existed;
  - the new `Library.SampleIds` and `Library.PresetIds`.
- The Ui's `SeedEntryIds` is gone.
- The preset ids keep their old text ("src-600", "det-intensity", …) until step 20 mints their
  GUIDs. Every value and every fact is therefore unchanged in this step.

## What's working

- Add `Library.SampleIds` (eleven seeded sample ids) and `Library.PresetIds` (33 seeded source, detector and polarizer ids) in Domain
- Seed samples, sources, entries and grouping trees from the id modules, so each built-in id is written once
- Delete the Ui's `SeedEntryIds`; the default prebinds and the CPL/CPR palette buttons read `PresetIds`
- Name presets through `PresetIds` in four Ui test classes, with fixtures holding `ElementValueId` (no `.value` re-wrap)

## Tests

**No gate was run in this round.** The gate engine is the only gate authority. The local
construction checks agree with the step's acceptance:

| Check | Result | Note |
|---|---|---|
| build | exit 0, 0 errors | Only the two vendored MathNet SYSLIB0051 warnings already on the tree. |
| ui-tests | 147/147 | The four selected classes. |
| ui-smoke | 208/208 | Equals step 14. |
| constructor-unit-tests | 736/736 | Equals the last checkpoint. |
| elevation | `ELEV-GATE: OK` | 0 new findings in 8 projects. |
| five s03 invariants | find nothing | |

- Each of the 33 preset literals and the eleven sample Guids occurs exactly once in product code.
- No `Fact`, `Theory` or `Trait` was added or removed.
- The pre-binding theory keeps its name and its four rows.

```yaml
gates:
  constructor_unit_tests: 736
  ui_smoke_tests:         208
```

`ui_tests` is not carried. Its count depends on the step's class filter.

## Architecture

- **Well-known ids sit in `Library`, directly after `newSampleId`.**
  - `SampleIds` needs `SampleId` (declared just above it), and `ElementId.fs` compiles after
    `MaterialLibrary.fs`, so the two modules live here rather than beside `MaterialIds`.
  - Both are plain modules of typed values, shaped like `MaterialIds`: no
    `RequireQualifiedAccess`, no `Literal`.
- **The id text is an implementation detail.** Product code and the four test classes now name a
  preset by its `PresetIds` member, so step 20 can change the text to GUIDs in `PresetIds` alone.
- **A member cannot ride `InlineData`.** The pre-binding theory carries the kind code, and a
  private `expectedSeedOf` maps each code to its member.

## Deferred

- Nothing. Every item of the slice landed.
- Step 20 mints the preset GUIDs, as the spec plans; that was never part of this step.

## Gotchas

- `PropagationTests.fs:205` and `LayerBandsControlsTests.fs:27` still say the sample literal
  lives in `SeedSamples`. Both files are outside this step's authorized effects, and both
  comments go with S1.7's deletion of `SeedSamples`.
- **The other test files still write the preset id text** (`"src-600"`, `"pol-lp"`, …):
  `LibraryWindowTests`, `SourceLibraryTests` and others outside this step's four classes. They
  pass while the text is unchanged. When step 20 mints the GUIDs, every one of them must read
  `PresetIds` instead.
- The four theory rows' xUnit display names lost their `seedId` argument. Class and method
  filters still select all four.

## Changelog

- 2026-11-05 — step 0015: moved every built-in sample and preset identity into
  `Library.SampleIds` / `Library.PresetIds`, with the seeds reading the members. Deleted the
  Ui's `SeedEntryIds`, and switched four Ui test classes to `PresetIds` with `ElementValueId`
  fixtures. The id text and every fact are unchanged.
