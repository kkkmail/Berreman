# Step 012 — impl plan (ADD_COMPONENT UICOMP_XDUO_0008 FacetedTreeControls)

## Approach

Author the domain-free faceted-tree rendering control in
`OpticalConstructor.Controls/FacetedTreeControls.fs`, following the
`MaterialsControls` State+Handlers shape exactly: a pure control-local `State`
record the host projects Domain state into (host-flattened string codes/labels
and int counts — the established Controls-project seam; named two-case DUs
where the slice needs a mode, never a naked bool), a `Handlers` record of
unit-returning stub-replaceable functions, one `UiIds` module of intent-named
`[<Literal>]` constants plus derived per-item id functions, and a pure
`view : State -> Handlers -> IView`. The control renders exactly what it is
given — it never references `OpticalConstructor.Domain`, a proxy, or the facet
engine.

State (the slice's enumeration): the built tree (`TreeNode` — recursive, with
`countOpt` so branch rows read `label (count)` while heading/leaf rows may
carry no count, and a `NodeExpansion` DU so unexpanded branches still carry
their counts), the applied-constraint breadcrumbs (`BreadcrumbChip` with label
+ after-count), the offered values grouped per facet (`OfferGroup` /
`OfferedValue` with count-previews, plus a `ManualRange` DU marking numeric
groups that take a raw min–max text entry), the named representations plus the
active one, the text-filter draft, the live result count, and a
`TreeMaterialization` mode (`TreeMaterialized | TreeGated`).

Handlers: `applyConstraint (group) (value)`, `removeConstraint (chip)`,
`commitTextFilter (text)`, `chooseRepresentation (code)`, `requestBuild ()`,
`selectNode (code)`, `applyManualRange (group) (rawText)` — all codes are
host-supplied stable tokens; the host parses the manual-range raw text
(domain-free: the control never interprets values).

Render, all inside one ScrollViewer: filter row (TextBox committing on
Enter/LostFocus ONLY — no text-change subscription at all, the
RotationControls/ExperimentControls `commit e.Source` idiom), representation
picker (clickable option boxes, active highlighted), breadcrumb strip of
removable chips labelled `label (afterCount) ×` (chip click dispatches
`removeConstraint`), the live result count, the offer groups (value boxes
labelled `label (preview)` dispatching `applyConstraint`; numeric groups add
the raw min–max box), and the tree area: when `TreeGated`, ONE Show/Search
button (dispatches `requestBuild`) and ZERO generated node rows; when
materialized, depth-indented recursive rows labelled `label (count)`
dispatching `selectNode`, collapsed nodes rendering no children. Every
generated element carries `AutomationProperties.AutomationId` +
`View.withKey`; NOTHING in this control sets `StyledElement.Name` (the
gated↔materialized swap and every dynamic list are membership changes — the
Part A hygiene rule).

Headless tests in `OpticalConstructor.Ui.Tests/FacetedTreeControlsTests.fs`,
following the MaterialsControls/CategoryControls test convention: pure
contract facts untagged (gate `ui-tests`) — empty state + the stable UiIds and
derived ids; mounted structure proofs tagged `ui-smoke` — (1) mount over a
known State, locate every declared automation id, chip click dispatches
`removeConstraint`, offered-value click dispatches `applyConstraint`,
representation/node clicks dispatch their handlers, collapsed children absent,
`label (count)` texts pinned; (2) gated mode: the Show/Search button present,
click dispatches `requestBuild`, ZERO tree node rows; (3) the filter box:
typed text (headless `KeyTextInput`) dispatches NOTHING per keystroke, Enter
commits exactly once with the typed text, LostFocus commits; (4) the manual
range box commits its raw text on Enter, never per keystroke.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/FacetedTreeControls.fs` — NEW.
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/OpticalConstructor.Controls.fsproj` — add the compile entry.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/FacetedTreeControlsTests.fs` — NEW.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` — add the compile entry.

No registry edit: `.contract-ids/XDUO-json` and `specs/0038/.contracts-json`
already carry `UICOMP_XDUO_0008 FacetedTreeControls` (declared, step 12) — the
supervisor maintains both (the step-007 precedent).

## Risks

- Headless typing: `window.KeyTextInput` must exist in Avalonia.Headless
  12.1.0 (it does upstream since 11.1); fallback is raising
  `InputElement.TextInputEvent` on the focused box directly.
- Enter-commit relies on single-line TextBox not swallowing Enter — the
  established production pattern (RotationControls / ExperimentControls), so
  low risk.
- Focus shifts fire LostFocus commits — tests are sequenced so every count
  assertion happens at a known focus state.
- `View.withKey` keys must be unique among siblings — node codes are
  documented as host-supplied unique tokens (e.g. path-shaped).
