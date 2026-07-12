# 0038-009 — Preliminary spec: faceted-tree, material-editor chart & sample-geometry / R–T tweaks

**Status: preliminary — feature list / mini-spec, input to the next spec-writer →
arc-runner cycle.** Every type named below is a *target shape*, not final. Assembled
from the operator observations in `007-improvements.txt` (inconveniences in the BMN
0038 implementation, not spec/impl gaps) and the code analysis recorded in
`008-tree-chart-geometry-improvements-impl-log.md`. Grounds every change in the
*existing* code and concepts — nothing here is a new invention where an established
type already exists.

**Already implemented in the working tree (staged, uncommitted) — the cycle should
fold these in, not re-derive them:**
- **Material-editor n/k chart by anisotropy** (`NkDispersionChart.fs` `nkAxisSpec` /
  `nkDispersionChart` / `nkDispersionStyle` take `Anisotropy`; isotropic → `n`,`k`;
  uniaxial → `n_o`,`n_e`,… (extraordinary at diagonal index 1, per
  `Dispersion.fs:297`); biaxial → `n₁…k₃`). Editor + Materials view-panel call sites
  updated; isotropic/uniaxial tests added. This is **Part C.1**, done.
- **Removed the Domain `SubstrateKind.Wedge`** display facet (leaving `ThinFilm |
  Plate`); the engine `Berreman.Media.Substrate.Wedge` is untouched. This is **Part
  D.1**, done.

Everything else below is to implement.

---

## 0. Binding constraints (every part)

0.1 **Reuse, don't invent.** Where a concept already exists, extend it — do NOT add a
parallel type. In particular R/T is the existing `Emission` DU (§D), the tree is the
existing `FacetedTreeControls.TreeNode` recursive model (§A), the facets are the
existing `LibraryFacets` catalogue (§B), the anisotropy/gyration classes are the
existing `MaterialComplexityEditor` seams (§C).

0.2 **CLAUDE.md discipline.** Elevate every primitive (single-case DUs with `.value`);
no naked `bool` (named two/three-case DUs); no `enum`; explicit concrete signatures on
public functions; interpolated strings never `sprintf`; four-space indent; space
before annotation colons; camelCase record fields; LF endings; **zero warnings from
our code** (only `NU1701`/`NU1901`–`NU1904` exempt); build green is non-negotiable.

0.3 **UI testable without a window.** Behaviour lives in pure Domain edit models +
`State`/`Handlers` records, unit-tested in Domain and structure/interaction-tested in
`Avalonia.Headless` (`ui-smoke`). New/changed interactive controls carry stable
intent-named `AutomationProperties.AutomationId` from the one `UiIds` module; generated
list rows never set `StyledElement.Name` and are keyed with `View.withKey`.

0.4 **No physics re-derivation without an explicit decision.** Where a facet or chart
must classify a coded engine preset (`complexity = None`, physics is a closure) or a
dispersive material, the classification approach is a decision recorded in §Open
questions — not guessed.

---

## Part A — The faceted tree: a real collapsible tree, visible selection, collapsed by default, sorted

**Observation.** *"rendered as a list of buttons that occupy the whole width … must be
a normal tree with collapsible / expandable branches. Selected node/leaf must be
distinctly visible. … some nodes are NOT expandable … all top nodes must be collapsed
by default … sorted alphabetically."*

**Current state (grounded).** The control `FacetedTreeControls`
(`OpticalConstructor.Controls/FacetedTreeControls.fs`) already carries a recursive
`TreeNode { code; label; countOpt; expansion; children }` (`:102-109`) with
`NodeExpansion = ExpandedNode | CollapsedNode` (`:46-48`), but `nodeRows` (`:351-368`)
flattens it into a vertical `StackPanel` of full-width `Border` rows indented by
`depth*16` — no `TreeView`, no disclosure affordance, and every row hardcodes
`idleBackground` (`:356`) so the selected row is indistinguishable. Expansion is honored
by `nodeRows` but nothing is ever `CollapsedNode`: both host projections hardcode
`ExpandedNode` (`MaterialsWindowView.fs:728,736,748,756`;
`LibraryWindowView.fs:853,861,878,888,895`). `Handlers` has `selectNode` but no
`toggleNode` (`:149-167`). Selection lives only in the host Model and surfaces only in
the right-hand metadata panel. Entry leaves are corpus-ordered (unsorted);
`buildTree` already sorts branches (`Facets.fs:248`, ordinal by value).

**Target.**
A.1 **Collapsible rendering.** Render the recursive `TreeNode` as a genuine tree:
a per-row disclosure affordance (chevron) on nodes with children that toggles
expand/collapse, children indented under their parent. A node with children must be
expandable; a leaf is not. (May be a real Avalonia `TreeView` or the existing keyed
`Border` rows plus an explicit expander column — either is acceptable if it reads as a
tree and stays headless-testable.)

A.2 **Persisted expand/collapse state.** Add `Handlers.toggleNode : string -> unit` and
hold expansion in each window's Model (e.g. a `Set<nodeCode>` DU of expanded codes)
with a `Msg` + `update` arm, threaded into `facetedState`; rows are already keyed by
`code` (`:364`), so re-projection with a flipped `expansion` recreates cleanly.

A.3 **Collapsed by default.** Project all top-level nodes as `CollapsedNode`.

A.4 **Visible selection.** Carry the selected node's code into the control
(`State.selectedCode` or `TreeNode.selected`) and paint the selected row with the
existing `chosenBackground` (`:174`, the `clickBox` chosen precedent) — a distinct,
colorblind-safe tint/border, not hue alone.

A.5 **Alphabetical sort at every level.** Sort entry leaves by display name in both
projections (`MaterialsWindowView.fs:730-738`, `LibraryWindowView.fs:855-863`); ensure
branches sort case-insensitively by *display label* (`Facets.buildTree`/`:248`). The
**facet-node (top-level group) order** is today the deliberate *representation* order —
see §Open question Q1.

A.6 **"Repeated leaves / non-expandable nodes."** With A.1–A.3 the flat-button artifact
(everything shown at once, nothing expandable) disappears. Confirm no genuine duplicate
leaf remains once collapsible nesting is in place; if the multiple top facet groups
were what the operator read as "repetitions," A.3 (collapsed) resolves the visual.

A.7 **Tests.** Update the headless tests that assume entry leaves are first-and-expanded
so a row is reachable without scrolling (`MaterialsWindowView.fs:716-718` comment); add
tests for toggle, collapsed-default, selection tint, and sort order.

---

## Part B — Facet counts must map to the whole selected domain

**Observation.** *"each top node MUST map to the whole selected domain … Biaxial (3)
and Isotropic (6) … 3 + 6 != 12 … Transparency shows Absorbing (2) and Transparent (7)
!= 12 … likely due to dispersive NOT categorized as absorbing."*

**Current cause (grounded).** In `LibraryFacets.fs` these facets are `appliesTo`-gated
so items fall out and branch counts under-sum:
- **Anisotropy** (`materialAnisotropyDef`, `:217-232`) applies only when
  `complexity = Some` — coded presets (silicon, langasite, vacuum; `complexity = None`)
  are excluded.
- **Transparency** (`materialTransparencyDef`, `:252-267`) applies only to *constant*
  materials — every dispersive material is excluded (the 003/Q9 "constant only"
  decision, now revised).

**Target.** Make the anisotropy and transparency facets **total** over the material
corpus so each facet's branch counts sum to the population. Reuse the existing readers
(`anisotropyOf`, `:132-143`; `transparencyOf`, `:147-150`) for materials that carry a
value tree; the classification of the remaining materials (coded presets for
anisotropy; dispersive materials for transparency) is §Open question Q2. Whichever
route is chosen, add a test asserting Σ(branch counts) = population size for both
facets over the seeded corpus.

---

## Part C — Material-editor chart applicability

**C.1 n/k chart by anisotropy — DONE (staged).** See the header note. Draws only the
axes the anisotropy distinguishes, named to match.

**C.2 Gyration & μ charts (the same bug).** *"The same BUG is applicable when optically
active and/or magnetic is selected. Only what is applicable … must be available AND the
names must match."* `gyrationChart` (`NkDispersionChart.fs:94-114`) still emits all six
tensor components and `muChart` (`:125-146`) all three diagonals + gyration, regardless
of class. Restrict them to the *independent* components of the current gyration/μ class,
named to match, reusing the editor's `availableGyrationClasses` / `GyrationClass` /
`MuKind` seams (`MaterialComplexityEditor.fs`) — never re-derived. (E.g. a cubic-active
medium exposes only `g₁₁`; a uniaxial-active one `g₁₁`,`g₃₃`.) For a VIEW-ONLY preset,
classify as the n/k path does (from the stored value tree, else the existing
`hasGyration`/`hasMagnetic` assembled-tensor classifiers, `:155-165`).

**C.3 Description text box width (minor; operator: "very minor … if not, leave as
is").** The material-editor description `TextBox` has a fixed width and can meet the
splitter when dragged left. If cheap: make it wrap (`AcceptsReturn` /
`TextWrapping = Wrap`, a `MinWidth` inside the left pane) so it grows multiline rather
than clipping. Lowest priority; skip if it fights the layout.

---

## Part D — Sample geometry & R/T support

**D.1 Wedge removed — DONE (staged).** See the header note. Geometry is now
`ThinFilm | Plate`.

**D.2 Geometry semantics (operator).**
- **Thin film** = 0+ layers on a **semi-infinite** substrate → supports **R only** (T
  is meaningless). This is `structure.substrate = None`, light meets a semi-infinite
  lower half-space (`Propagation.sampleToSystem`, `:249-256`).
- **Plate** = 0+ layers on a **thick plate** substrate → the substrate **must be
  specified**, and since we compute the Mueller matrix the lower medium is **always
  vacuum**. Supports **R and/or T**.
A `Plate` with no substrate is invalid — extend `validateSample`
(`ElementId.fs:799-808`) to require `structure.substrate = Some _` for `Plate`.

**D.3 R/T support — reuse the existing `Emission` DU (NOT a new concept).** The
established type is `Emission = EmitReflectedOnly | EmitTransmittedOnly | EmitBoth`
(`Placement.fs:108-138`, spec 0027/0028), whose smart setters `withReflected` /
`withTransmitted` (`:126-138`) already implement the operator's exact clearing rule
("turning the second one off automatically turns the other back on"), and
`MeasurementMode.ofEmission` (`Experiments.fs:106-110`) derives the capture; the beam
wiring already consumes it (`RayModel.emittedGroups`). Give the Library `Sample` a
supported-emission (reusing `Emission`), **geometry-constrained**: `ThinFilm` forces
`EmitReflectedOnly` (R only; the single allowed branch cannot be cleared); `Plate`
allows any of the three (clearing one forces the other on, via the existing setters).
The sample editor exposes R / T checkboxes driven by `Emission.withReflected` /
`withTransmitted`. This supported-emission then constrains the placed element's
`Emission` / the experiment's `MeasurementMode` that "wire up the beams on the optical
table." Touch points: `Sample` (`ElementId.fs:175-185`) + `SampleEditSnapshot` /
`toSample` / `init` + editor `Model`/`Msg` (`SampleEditorView.fs`), and the default at
`MeasurementMode.ofEmission` / the Emission of a placed sample.

**D.4 Substrate "Set from chosen" is always disabled.** *"It is unclear how to set a
substrate plate as `Set from chosen` is always disabled."* Cause: the button's enabled
condition is `hasChosenMaterial m` (`SampleEditorView.fs:963`, `:923-926`), and
`m.chosenMaterial` is only ever set by picking a material for an *existing film-layer
row* (`BindMaterialToLayer`, `:492`); a `Plate` with `films = []` has no rows → no
pick path → the button is dead. Fix per spec 0038 Part G: the substrate "Set…" opens
the **Materials window in Select state** targeted at the substrate slot (the
`WindowLauncher` / Select flow), then applies `SampleStackMsg.SetSubstrate`
(`SampleStackEditor.fs:367-368`). The lower half-space stays vacuum for a Plate (D.2).

---

## Open questions / decisions to lock in the spec cycle

**Q1 (A.5 facet-node order).** Sort the top-level facet groups alphabetically too, or
keep the deliberate *representation* order (the "search order ≠ representation order"
invariant)? The operator said "top-level tree nodes … sorted alphabetically", which
conflicts with representation order — confirm which wins for the facet groups (leaves
and branches are alphabetical either way).

**Q2 (B facet totality).** How are the currently-excluded materials classified so the
counts sum? Options: (a) classify from the assembled engine tensor at a classify
wavelength (mirrors the existing `hasGyration`/`hasMagnetic` view-only classifiers —
physics-derived but display-only); (b) an explicit residual branch (e.g. "Coded /
unspecified") so counts sum without re-deriving physics. For transparency specifically,
is a dispersive material "Absorbing" iff k>0 over its range (a physics evaluation), or
does every dispersive material go to "Absorbing" as the operator's phrasing suggests?

**Q3 (D.3 default emission).** For a `Plate` sample, what is the default supported
emission — `EmitBoth`, or match the current `defaultEmission` for a placed sample? And
does the sample-level supported-emission override, or merely bound, the placed
element's `Emission`?

---

## Suggested phasing (small slices, each independently green)

1. **Facet totality (Part B)** — pure Domain (`LibraryFacets`), once Q2 is decided;
   count-sum tests. Contained, no UI.
2. **Tree sorting (A.5)** — the two projection sorts + branch label sort; test.
3. **Tree collapse/expand + collapsed-default (A.1–A.3, A.6)** — control state + handler
   + rendering; update headless tests.
4. **Tree selection highlight (A.4)** — control selected-code + tint; test.
5. **Gyration/μ chart applicability (C.2)** — `NkDispersionChart` + editor; tests.
6. **Sample geometry validation (D.2)** — `Plate` requires substrate; `validateSample`.
7. **Substrate "Set from chosen" via Select window (D.4)** — Materials-window Select
   flow for the substrate slot.
8. **Sample R/T support via `Emission` (D.3)** — Domain field + editor checkboxes +
   `MeasurementMode`/beam constraint; tests.
9. **Description box wrap (C.3)** — minor, optional.

## References

- The 0038 full spec (`.spec-md`) and preliminary (`006`) for the faceted-tree,
  material-editor, and sample-geometry design already in place.
- Spec 0027 (`specs/0027/.manual/024-materials-and-experiments-spec.md`) — the `Emission`
  / `MeasurementMode` R/T model this reuses.
- `008-tree-chart-geometry-improvements-impl-log.md` — the code analysis and the two
  already-staged fixes (C.1, D.1).
