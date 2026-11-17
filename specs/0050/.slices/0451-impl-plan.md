# Step 0451 — implementation plan

Step 0451 hosts step 450's Online sources surface (`MaterialSourcesView`) in its own single-instance window, spec 0050
S13.1, obligation S131-SOURCES-WINDOW.

## Approach

1. **`OpticalConstructor.Controls/UiIds.fs`.** Add `materialSourcesWindow` (`"MaterialSourcesWindow"`) to step 450's
   Online sources group, with a one-line doc.
2. **`OpticalConstructor.Ui/WindowLauncher.fs`.** `WindowKey` gains `MaterialSourcesWindowKey`, with a one-line doc.
   The module doc's single-instance list names it too.
3. **New `OpticalConstructor.Ui/MaterialSourcesWindow.fs`.** It compiles directly after `MaterialSourcesView.fs`, with
   one `<Compile>` line and no comment.
   - The window inherits `HostWindow`, is titled `Online sources`, carries its id as `Name` and `AutomationId`, opens
     at 720 × 640, and calls `WindowFit.keepOnScreen`.
   - It captures dispatch from the init command, as `MaterialsWindow` and `KitStartWindow` do.
   - It runs `Program.mkProgram` over `context.initMaterialSources`, `updateMaterialSources` and `materialSourcesView`.
     The jobs host is `SourceSnapshotJobHost.onUiThread`.
   - Its `Closed` hook dispatches `CancelSourceSnapshot`.
4. **`WireUiFinalCompositionTests.rootWindowFactory`.** It answers `MaterialSourcesWindowKey` with
   `MaterialSourcesWindow(ctx.catalogueImports, ignore)`. Facts, keys and assertions are unchanged.
5. **Tests.**
   - `ScrollExtentTests` gains `the Online sources window leaves everything reachable`.
   - New `MaterialSourcesWindowTests` is appended last in the test project's compile list. It has three facts without a
     trait and two `ui-smoke` facts, exactly the identities the census names.

## Risks

- **The jobs callbacks must read `dispatch` when called.** `SourceSnapshotProgressed >> dispatch`, as the how_to
  writes it, would bind the value `ignore` that `dispatch` holds before the loop starts. The window uses lambdas that
  read the field when called, as `KitStartWindow` does. The meaning is the same.
- **The pull runs over a bare in-memory import context,** with the Built-in library not imported. This is the context
  the how_to names. If its proxy refused the pull, the fixture would have to import first; the run checks this.
- **ELEV uniqueness of the new names.** Neither `materialSourcesWindow` nor `MaterialSourcesWindowKey` exists yet.
