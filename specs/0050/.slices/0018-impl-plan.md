# Step 0018 — IMPLEMENT — impl plan

## Approach

This step switches seven UI test files from seeded preset id literals to their `Library.PresetIds`
members (`.spec-md` §3.3). Step 15 kept the ids' old text, so every fact keeps its result. Per
AGENTS.md "Elevate at origin", a fixture holds an id as an `ElementValueId`. So a helper that
wrapped a `string` id now takes the `ElementValueId`, and its callers pass the member.

1. **`ElementRendererBindingTests.fs`.**
   - `drawableOf` takes `valueId : ElementValueId option` and binds it directly.
   - `stateOf` follows and wraps its test-local ids where it writes them, for example
     `Some (ElementValueId "lib:sample")`.
   - The two render-proof calls (`:104`, `:123`) wrap theirs the same way.
   - The HideNormals fact draws `Some PresetIds.polarizerLp`. This needs
     `open OpticalConstructor.Domain.Library`, which every other file of the seven already has.
2. **`InverseConstructorTests.fs:155`** uses `PresetIds.detectorEllipsometer`.
3. **`LightSourceEditorTests.fs`** uses `PresetIds.euv13p5` at `:173` and `PresetIds.arf193` at
   `:210`. The user sources keep their test-local ids.
4. **`SolverHandoffWindowTests.fs`.**
   - `baseExperiment` takes `srcBind` and `detBind` as `Placement.ElementValueId option`.
   - `bindOne` binds the id it receives.
   - Every call passes `Some PresetIds.source600` with `Some PresetIds.detectorIntensity`, or
     with `Some PresetIds.detectorEllipsometer` at `:182`.
5. **`WireUi0040CompositionTests.fs:253-261` and `WireUiCompositionTests.fs:197`** use
   `ConstructorUiId.libraryEntryNode PresetIds.<m>`.
6. **`WorkbenchInfoAndCaptureTests.fs`.**
   - `bind` and `filmOnR2` take an `ElementValueId`.
   - `glassFilm200Id` and `glassPlate1mmId` hold the sample's `entryId`, without `.value`.
   - Every preset bind and lookup reads its `PresetIds` member.
   - "src-tune" stays.

The edits are made with a count-asserting script under `.artifacts/0018-scripts/`, the step-17
precedent. Each replacement must match its expected count before the file is written.

## Files

The seven files in the slice's authorized effects, and nothing else.

## Risks

- **Opening `Library` in `ElementRendererBindingTests.fs` could shadow a name.** The unique-name
  rules (ELEV014/015/016) make that unlikely, and the compile settles it.
- **`SolverHandoffWindowTests.fs` does not open `Placement`.** Its annotations use
  `Placement.ElementValueId`, as its existing code does.
- **No test may be added, removed or renamed.** The diff must not touch a
  `[<Fact>]` / `[<Trait>]` line.
