# Step 007 — impl-plan

## Anchor / scope

`IMPLEMENT`. Touches `OpticalConstructor.Ui` and `OpticalConstructor.Ui.Tests`
only. `depends_on: []`.

Part C.2 minor: make the material-editor **description** `TextBox` wrap to
multiple lines instead of clipping when the left pane narrows.

## Approach

The description box lives in `MaterialEditorView.fs`'s `descriptionRow` (a
horizontal `StackPanel`: `labelBlock "Description:"` + a `TextBox`). Today the
box carries a **fixed** `TextBox.width 620.0`. The left identity/ladder pane has
`Border.minWidth 520.0`, so a 620-wide box exceeds the pane, meets the
`GridSplitter`, and clips.

The fix, exactly as the slice prescribes:

1. Replace the fixed `TextBox.width 620.0` with `TextBox.minWidth 340.0` (the
   same floor the name box uses) so the box no longer forces a width past the
   pane / splitter.
2. Set `TextBox.acceptsReturn true` — a long description grows downward into
   extra lines.
3. Set `TextBox.textWrapping TextWrapping.Wrap` — wrapping is enabled (the
   acceptance's verified condition). `TextWrapping` is already in scope
   (`open Avalonia.Media` at the top of the view).

No other layout change: the two-pane grid, the splitter, the scroll viewer, and
every sibling row are untouched.

## Test

Add one headless assertion in `MaterialEditorWindowTests.fs` that mounts the
editor over a fresh `NewMaterial` and asserts the description box (found by its
stable id `UiIds.MaterialEditor.descriptionBox`) is a `TextBox` whose
`TextWrapping = Wrap` and `AcceptsReturn = true`. Marked
`[<Trait("Category", "ui-smoke")>]` to match the sibling headless window proofs,
so it runs in the `ui-smoke` gate. `Avalonia.Media` is not `open`ed in the test
file, so the enum is referenced fully qualified as
`Avalonia.Media.TextWrapping.Wrap`.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialEditorView.fs` —
  `descriptionRow`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs`
  — one new headless test.

## Risks

- **Horizontal `StackPanel` measures children with unbounded width**, so
  automatic wrapping only kicks in once the box is width-bounded; the slice caps
  scope to "no other layout change", and the acceptance is verified by the
  wrapping property being *enabled* (headless), plus `AcceptsReturn` gives real
  multiline growth on explicit newlines. Recorded in the impl-log Gotchas.
- FuncUI DSL member availability — confirmed `TextBox.minWidth`,
  `TextBox.acceptsReturn`, `TextBox.textWrapping` all exist in
  `Avalonia.FuncUI 2.0.0-preview1`'s DSL by inspecting the shipped DLL.
