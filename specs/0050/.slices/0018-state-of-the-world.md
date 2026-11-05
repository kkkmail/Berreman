# Step 0018 — state of the world

## Where we are

Spec 0050, section S1.3: "Every library entry is identified by a frozen GUID".

- **Step 15** gave every seeded preset one home, `Library.PresetIds` (`ElementId.fs:66`). It kept
  the ids' old text.
- **Step 16** switched the constructor suite to those members.
- **Step 17** switched `LibraryWindowTests`.
- **This step** switches the rest of the UI suite that names a seeded preset:
  ElementRendererBindingTests, InverseConstructorTests, LightSourceEditorTests,
  SolverHandoffWindowTests, WireUi0040CompositionTests, WireUiCompositionTests and
  WorkbenchInfoAndCaptureTests.

The UI test project now spells only test-local ids. A later step that gives the presets their
frozen GUIDs leaves every UI fact unchanged.

## What's working

- Seven UI test files name every seeded preset through `Library.PresetIds`.
- Fixture helpers take an `ElementValueId` instead of wrapping a string: `drawableOf`,
  `stateOf`, `baseExperiment`, `bindOne`, `bind` and `filmOnR2`.
- The sample fixtures hold `entryId` itself, not its `.value`.
- Library tree-node ids in both composition tests are built from `PresetIds` members.
- Every fact keeps its assertion. Selected ui-tests (36) and ui-smoke (208) keep their counts.

## Tests

No gate was run: under Invariant 6 the gate engine is the only gate authority. The round ran
local construction checks, which are observations, not gate results:

- **Build**: exit 0, 0 errors. The only warnings are the two pre-existing MathNet SYSLIB0051.
- **Selected ui-tests** (ElementRendererBindingTests, InverseConstructorTests,
  LightSourceEditorTests, SolverHandoffWindowTests, WorkbenchInfoAndCaptureTests): Passed!,
  36 of 36.
- **ui-smoke**: Passed!, 208 of 208, equal to the checkpoint. It includes all 7 composition-test
  facts.
- **ELEV**: `ELEV-GATE: OK`. The test project is out of scope.
- **The five s03 invariants**: each finds nothing.
- **The slice's grep**: lists only the test-local ids the slice names.

Details are in `0018-impl-log.md` (Testing state).

```yaml
gates:
  ui_smoke_tests:         208
  ui_tests:                36
```

## Architecture

- **No new shape.** The tests read the `PresetIds` members as the domain already exposes them.
- **A fixture holds an id as an `ElementValueId` from its origin** (AGENTS.md "Elevate at
  origin").
  - A helper no longer wraps a `string` at its call into `BindValueIdTo` or
    `ElementPlacement.valueId`.
  - Test-local ids are wrapped once, where they are written.

## Deferred

Nothing. Every clause of the slice, (1) to (6), landed this round.

## Gotchas

- **`ElementRendererBindingTests.fs` now opens `OpticalConstructor.Domain.Library`** so it can
  write `PresetIds.polarizerLp` unqualified. The other six files already opened it.
- **Every fact in the two composition files carries `ui-smoke`.** These are
  `WireUi0040CompositionTests.fs` and `WireUiCompositionTests.fs`. The ui-tests selection does
  not run them; the ui-smoke gate does.
- **`SolverHandoffWindowTests.fs` does not open `Placement`.** Its new annotations are written
  `Placement.ElementValueId option`, as the file already wrote the type.
- **What the slice's grep still finds is all test-local.** These are user sources and editor
  fixtures, not seeded presets: "src-new-1" / "src-x" / "src-y" / "src-u" / "src-mine" /
  "src-typed", "src-x", "src-test-10nm" and "src-tune". They do not need to change when the
  presets take GUIDs.

## Changelog

- 2026-11-05 — Step 0018: seven UI test files switched from preset id literals to
  `Library.PresetIds` (56 in, 55 out). Fixture helpers now take `ElementValueId`. Counts are
  unchanged.
