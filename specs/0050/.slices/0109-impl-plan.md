# Slice 0109 implementation plan

Thread the existing CatalogueImportContext through MaterialsWindowContext,
MaterialsWindow, SampleEditorWindow, the Library sample-editor launch,
EditorLaunchers.openMaterialsWindow and the application launcher. Preserve the
same app-owned context through Browse and Select paths. This slice changes
dependency wiring only; file-drop behavior remains outside its scope.

Update the authorized UI test constructions with the existing in-memory import
proxy, or ctx.catalogueImports in final composition tests. Adjust the recording
launcher arity without changing any assertions, test identities or test count.
WorkbenchInfoAndCaptureTests inherits defaults and needs no edit.

Keep edits within the six product files and required test callers listed in the
slice. The principal risk is missing a constructor or launcher call after making
the dependency mandatory. Add the required namespace imports and concise seam
documentation. Do not change durable formats, packages, versions or gates.

The session-level verification requirement takes precedence over the IMPLEMENT
worker's act-only wording. Run the resolved build, elevation, smoke, selected UI
and eleven invariant checks; formal acceptance and checkpoints remain owned by
the supervisor. Record measurements in the specified log and state-of-world
files, with captured output under the supplied artifacts folder.
