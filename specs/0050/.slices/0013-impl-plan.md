# Step 0013 — IMPLEMENT — impl plan

## Approach

`MaterialEntry.complexity` becomes mandatory, and `properties` stops being a field: it becomes
the member `this.complexity.toProperties` (001 §B, .spec-md §0.1 and §3.2). Steps 10 and 11
already made every construction set `properties = complexity.toProperties` with a present
complexity, so the change is mechanical. It lands in one step so the build stays green.

1. **Record (`MaterialLibrary.fs`).** Delete the `properties` field and make the field
   `complexity : MaterialComplexity`. Add `member this.properties`. Rewrite the doc so the
   complexity is the entry's physics and `properties` its engine composition. In
   `builtInEntries`, drop every `properties = …` line and every `Some`. The record then has
   structural equality.
2. **Constructions.** Drop `properties = …` and `Some` everywhere a `MaterialEntry` is built:
   - Storage: `entryFromTabulated`, `entryOfAxis` and `dtoToEntry`;
   - Ui: `MaterialEditorView.entryUnder`;
   - the test fixtures the slice names;
   - the copy-and-updates in `MaterialProxyTests` / `SampleProxyTests`, which keep only
     `complexity = …`.
3. **Readers of the option.**
   - `MaterialStore.payloadOf` becomes `VersionPayload<MaterialComplexity, MaterialMetadata>`.
   - The `LibraryFacets` private members and the anisotropy facet read the value.
   - `OutOfBandDiagnostic.ofEntry` reads the value.
   - `NkDispersionChart.previewAnisotropy` is deleted. Its two callers in
     `MaterialsWindowView` read `complexity.anisotropy`.
   - `editableSelection` and the view panel's caption read `isEditable` directly. The caption
     becomes ` (view-only)`.
4. **Editor (`MaterialEditorView.fs`).**
   - The view-only case becomes `ViewOnlyMaterial of reason : ControlCaption * shown : MaterialComplexity`.
   - `presetProperties` and `viewOnlyComplexity` are deleted, and the three preview members
     read `shown`.
   - `init` has two paths. An editable complexity seeds the editor through `ofComplexity`, and
     its `Error` opens view-only with the complexity. Any other entry opens view-only with the
     caption the slice gives.
   - The binders take `(reason, _)`.
   - The docs are rewritten, including the window's doc.
5. **Tests.**
   - Delete the two facts that lose their subject.
   - Rename the three Save / view-only facts.
   - Make every option match read the value, and rewrite every `complexity = None/Some`
     comment and failure message in the edited files.

## Files

- Domain: `MaterialLibrary.fs`, `MaterialStore.fs`, `LibraryFacets.fs`, `OutOfBandDiagnostic.fs`,
  `MaterialComplexityEditor.fs` (the `isEditable` doc).
- Storage: `MaterialImport.fs`, `Report.fs`.
- Ui: `NkDispersionChart.fs`, `MaterialsWindowView.fs`, `MaterialEditorView.fs`,
  `MaterialEditorWindow.fs`.
- Tests: `DispersionModelsTests.fs`, `LibraryFacetsTests.fs`, `MaterialComplexityTests.fs`,
  `MaterialImportTests.fs`, `MaterialProxyTests.fs`, `OutOfBandDiagnosticTests.fs`,
  `SampleProxyTests.fs`.
- Ui.Tests: `FsxAcceptanceTests.fs`, `MaterialEditorWindowTests.fs`, `OutOfBandBadgeTests.fs`.

## Risks

- **A consumer outside the listed edits.** `OpticalConstructor.Tests/ExportImportTests.fs:159`
  asserts `Assert.Equal(Some vacuumPlaceholderComplexity, e.complexity)`. That stops compiling
  once the field is not an option. The only fix is to drop the `Some`, a fixture repair forced
  by the authorized contract change. It is recorded as a deviation.
- **`properties` is recomputed on every read.** `toProperties` builds closures and is cheap.
  Step 11 already moved the facts that compared `properties` by reference to value comparison.
- **`MaterialEditorModel` stays `[<ReferenceEquality>]`**, so the Elmish re-render behaviour
  does not change. Only its doc loses the preset-properties reason.

## Attempt 2 (retry after `constructor-unit-tests: count 736 < baseline 737`)

1. Diagnose the measured finding: find which facts left the suite, and why the gate's floor did
   not follow the step's authorized deletion.
2. Repair it if a repair exists within the declared effects. Otherwise report the authority
   conflict, naming the obligation and the authority that is missing. Do not pad the suite, and
   do not undo the authorized deletion.

No product or test edits are planned. The tree the gate measured is the tree the slice asks for.
