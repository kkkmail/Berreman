# Slice 0111 implementation plan

Wire the Library window to the existing CatalogueImportContext,
ImportDialogWindow and EntryFileDrop surfaces. Add the injected dialog launcher,
semantic drop/review/completion messages, pending-registration removal and the
conditional Review imports verb. Preserve Browse/Select state and confirmation
gates; import preview must not write to the library.

Modify only the six source files named by the slice: LibraryWindowView.fs,
LibraryWindow.fs, UiIds.fs, LibraryWindowTests.fs, LightSourceEditorTests.fs and
WorkbenchInfoAndCaptureTests.fs. Add exactly the five declared UI facts and one
smoke acceptance, reusing existing catalogue fixtures, serializers and headless
control helpers. Update all LibraryWindowContext constructions.

Risks are premature application, removing an unrelated pending registration,
losing confirmation/session state and failing to refresh the live tree. Tests
will exercise those seams and the real dialog Apply button. Run the resolved
build, elevation, smoke, selected UI and eleven invariant checks as required by
the higher-priority session verification instructions. Keep formal acceptance,
checkpoint updates and git mutations with the supervisor. Record results and
artifacts in the prescribed log and state-of-the-world paths; no version or
durable-format change is involved.
