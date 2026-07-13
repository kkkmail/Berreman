# Step 002 — impl-plan

## Goal

Make the faceted tree **collapsible** with **persisted, collapsed-by-default** expansion
(operator). A disclosure chevron on every parent node toggles that node; the expanded-code set
lives in each host window's Model as an elevated named type, projected into `TreeNode.expansion`.

## Files to modify

1. `OpticalConstructor.Controls/UiIds.fs` — add `FacetedTree.treeNodeChevron` (a distinct
   `FacetTreeChevron_` prefix so a chevron is never counted as a `FacetTreeNode_` label row).
2. `OpticalConstructor.Controls/FacetedTreeControls.fs` — add `toggleNode : string -> unit` to
   `Handlers` beside `selectNode`; render a per-parent chevron in `nodeRows` (a leaf with empty
   `children` shows none) whose pointer-press dispatches `handlers.toggleNode node.code`, while
   the row label keeps dispatching `selectNode`. Chevron + label + row each keyed with
   `View.withKey`; chevron carries the `treeNodeChevron` AutomationId, label keeps `treeNode`.
3. `OpticalConstructor.Ui/MaterialsWindowView.fs` — elevated `type ExpandedNodes = ExpandedNodes
   of Set<string>` (`.value`, `.isExpanded`, `.toggle`, `.empty`); `expandedNodes` on the Model;
   `ToggleNode of string` Msg + update arm that flips a code; project each node's `expansion` via
   `expansionOf code` (ExpandedNode iff the code is in the set) — replacing every hardcoded
   `ExpandedNode`; wire `toggleNode` in `facetedHandlers`.
4. `OpticalConstructor.Ui/LibraryWindowView.fs` — the same shape (numeric-bucket branches too).
5. `OpticalConstructor.Ui.Tests/FacetedTreeControlsTests.fs` — add `toggleNode` to the recorder
   stub; add a chevron structure/dispatch test; assert the new UiId.
6. `OpticalConstructor.Ui.Tests/MaterialsWindowTests.fs` + `LibraryWindowTests.fs` — the tree is
   now collapsed on first open, so entry leaves and facet branches no longer render until their
   parent is expanded. Add `expandEntries`/`expandNode` helpers, auto-expand entries in the shared
   mount helpers, and add explicit expands to the inline-mount/launcher-opened tests. Add a new
   collapsed-by-default + chevron-toggle acceptance test (pure + headless) to BOTH windows.

## Risks

- **Collapsed-by-default hides browsed entry leaves.** Every headless test that finds/clicks an
  entry leaf (or facet branch) breaks unless its parent is expanded first. Mitigated by auto-
  expanding `entries` in the shared mount helpers (expansion PERSISTS in the Model across
  re-renders) and explicit expands where a window is opened inline or via the real launcher.
- **`textOf` on a tree node** must still read the label, not the chevron: the chevron is a
  SIBLING of the label box (not its descendant), so `textOf (treeNode code)` is unchanged.
- **Adding a Handlers field** breaks every `Handlers` construction — the two `facetedHandlers`
  and the test `recorder` — all updated.
