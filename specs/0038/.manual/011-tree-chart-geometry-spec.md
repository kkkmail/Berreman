# 0038-011 — Spec: faceted-tree, material-editor chart & sample-geometry / R–T tweaks

**Status: authoritative spec — no open questions. Implement exactly what is written.**
Input to the next spec-writer → arc-runner cycle. Sources: the operator observations in
`007-improvements.txt` and the answers in `010-comments.txt` (which are binding), grounded
in the existing code and reusing existing concepts throughout.

---

## 0. Binding constraints (every part)

0.1 **Reuse, don't invent.** Extend the existing types — never add a parallel concept.
R/T is the existing `Emission` DU (§D); the tree is the existing
`FacetedTreeControls.TreeNode` recursive model (§A); the facets are the existing
`LibraryFacets` catalogue (§B); anisotropy / gyration / μ classes are the existing
`MaterialComplexityEditor` seams (§C).

0.2 **CLAUDE.md discipline.** Elevate every primitive (single-case DUs with `.value`); no
naked `bool` (named DUs); no `enum`; explicit concrete signatures on public functions;
interpolated strings never `sprintf`; four-space indent; a space before every annotation
colon; camelCase record fields; LF endings; **zero warnings from our code** (only
`NU1701` / `NU1901`–`NU1904` exempt); a green build is non-negotiable.

0.3 **UI testable without a window.** Behaviour lives in pure Domain edit models plus
`State` / `Handlers` records, unit-tested in Domain and structure/interaction-tested in
`Avalonia.Headless` (`ui-smoke`). New/changed interactive controls carry stable
intent-named `AutomationProperties.AutomationId` from the one `UiIds` module; generated
list rows never set `StyledElement.Name` and are keyed with `View.withKey`.

---

## Part A — The faceted tree: collapsible, selection-visible, collapsed by default, fully sorted

**A.0 Problem.** The tree renders as a flat list of full-width buttons: nothing is
expandable, the selected node is indistinguishable, everything is shown at once, and the
ordering is not alphabetical.

**A.1 Current shape (grounded).** `FacetedTreeControls`
(`OpticalConstructor.Controls/FacetedTreeControls.fs`) already carries a recursive
`TreeNode { code; label; countOpt; expansion; children }` (`:102-109`) with
`NodeExpansion = ExpandedNode | CollapsedNode` (`:46-48`), but `nodeRows` (`:351-368`)
flattens it into a vertical `StackPanel` of full-width `Border` rows indented by
`depth*16`, every row hardcoding `idleBackground` (`:356`). Both host projections hardcode
`ExpandedNode` (`MaterialsWindowView.fs:728,736,748,756`;
`LibraryWindowView.fs:853,861,878,888,895`). `Handlers` has `selectNode` but no
`toggleNode` (`:149-167`). `buildTree` sorts branches ordinally (`Facets.fs:248`); entry
leaves are corpus-ordered; facet groups are representation-ordered.

**A.2 Requirements.**
1. **Collapsible tree.** Render the recursive `TreeNode` as a genuine tree with a
   per-node disclosure affordance (chevron) that toggles expand/collapse; children indent
   under their parent. A node with children is expandable; a leaf is not.
2. **Persisted expand/collapse.** Add `Handlers.toggleNode : string -> unit`, hold
   expansion in each window's Model (an elevated `Set<nodeCode>`-shaped state, not a naked
   set of raw strings in the record surface) with a `Msg` + `update` arm, threaded into
   `facetedState`. Rows are already keyed by `code` (`:364`), so re-projection with a
   flipped `expansion` recreates cleanly.
3. **Collapsed by default.** Every top-level node projects as `CollapsedNode`.
4. **Visible selection.** Carry the selected node's code into the control
   (`State.selectedCode`) and paint the selected row with the existing `chosenBackground`
   (`:174`, the `clickBox` precedent) as a distinct border/tint — pattern or luminance,
   colorblind-safe, not hue alone.
5. **Alphabetical order at EVERY level (operator, 010/Q1).** Sort, case-insensitively by
   display label: the **top-level facet groups** (overriding representation order — the
   operator requires the groups themselves alphabetized), every branch, and every entry
   leaf. Apply in `buildTree`/the facet-node assembly and in both host projections'
   entry-leaf lists (`MaterialsWindowView.fs:730-738`, `LibraryWindowView.fs:855-863`).

**A.3 Acceptance.** Top nodes render collapsed; clicking a disclosure expands/collapses
and the state survives re-render; the selected node is visibly distinct; every level —
facet groups, branches, leaves — is alphabetical. Headless tests that relied on
entry-leaves-first-and-expanded (`MaterialsWindowView.fs:716-718` comment) are updated.

---

## Part B — Every facet's branch counts sum to the whole population

**B.0 Problem.** Under a representation the facet branches do not sum to the population
(e.g. Anisotropy shows Biaxial + Isotropic but omits the coded presets; Transparency shows
Absorbing + Transparent but omits every dispersive material), so the "math" does not add up.

**B.1 Principle (operator, 010/Q2).** Any element either has an attribute or it does not.
Every **single-valued** material facet partitions the whole population that possesses the
attribute — each such element contributes to exactly one branch — so its branch counts sum
to that population's size, and all such facets in a window share that same total. In the
Materials window every material possesses Category, Anisotropy, Dispersion, and
Transparency, so all four sum to the full material count.

**B.2 Requirements (`LibraryFacets.fs`).**
1. **Anisotropy is total.** Classify every material into exactly one of Isotropic /
   Uniaxial / Biaxial, coded engine presets (`complexity = None`) included. For a material
   with a stored eps value tree, read the class via `anisotropyOf` (`:132-143`); for a
   coded preset, classify the assembled engine eps tensor at a fixed classify wavelength —
   all three principal permittivities coincide → Isotropic; exactly two coincide →
   Uniaxial; otherwise Biaxial — the same view-only classification pattern the chart's
   `hasGyration` / `hasMagnetic` use. Remove the `complexity = Some` `appliesTo` gate.
2. **Transparency is total.** Classify every material into exactly one of Transparent /
   Absorbing: a **constant** material by its permittivity (no absorption → Transparent,
   else Absorbing) via `transparencyOf` (`:147-150`); **every dispersive material →
   Absorbing** (operator, 010/Q2), coded presets included (the constant-vs-dispersive
   class comes from `materialDispersion`, which already applies to every entry). Remove
   the "constant materials only" `appliesTo` gate (`:252-267`).
3. **Multi-valued facets are the sole exception.** The per-axis/segment Dispersion-model
   facet, the Film-material facet, and the Film-thickness facet are genuinely multi-valued
   (an item carries several values by design), so their branch counts need not sum — they
   stay multi-valued and are explicitly outside B.1.
4. When a single-valued material facet is lifted onto samples (the Library window), the
   same single-value classification applies per constituent per the existing lifting
   semantics.

**B.3 Acceptance.** A test asserts that over the seeded material corpus the Category,
Anisotropy, Dispersion, and Transparency facets each sum to the material count; a
dispersive material appears under Absorbing; a coded preset appears under its anisotropy.

---

## Part C — Material-editor chart applicability

**C.1 Gyration & μ charts draw only the applicable components.** *"The same bug is
applicable when optically active and/or magnetic is selected. Only what is applicable to
the current choice must be available on the chart AND the names must match what's
applicable."* Today `gyrationChart` (`NkDispersionChart.fs:94-114`) emits all six tensor
components and `muChart` (`:125-146`) all three diagonals + gyration, regardless of class.

*Requirement.* Restrict each chart to the *independent* components of the current class,
named to match, reusing the editor seams (`availableGyrationClasses` / `GyrationClass` /
`MuKind`, `MaterialComplexityEditor.fs`) — never re-derived. For example a cubic-active
medium exposes only `g₁₁`; a uniaxial-active one `g₁₁` and `g₃₃`; a scalar-μ medium a
single `μ`. For a VIEW-ONLY preset with no ladder to read, classify from the stored value
tree, else the assembled-tensor classifiers (`hasGyration` / `hasMagnetic`, `:155-165`).
*Acceptance:* a uniaxial-active entry's gyration chart carries only its independent
components with matching names; a magnetic entry's μ chart carries only what its `MuKind`
exposes.

**C.2 Description text box width (minor; operator: "very minor … if not, leave as is").**
The material-editor description `TextBox` has a fixed width and can meet the splitter when
it is dragged left. If it can be done cheaply, make it wrap (`AcceptsReturn` /
`TextWrapping = Wrap`, a `MinWidth` inside the left pane) so it grows multiline rather than
clipping; if it fights the layout, leave it as is. Lowest priority.

---

## Part D — Sample geometry & R/T support

**D.1 Geometry semantics (operator).** The sample geometry is `ThinFilm | Plate`:
- **Thin film** — 0+ layers on a **semi-infinite** substrate; supports **reflectance
  only** (transmittance is meaningless). Modelled as `structure.substrate = None`; light
  meets a semi-infinite lower half-space (`Propagation.sampleToSystem`, `:249-256`).
- **Plate** — 0+ layers on a **thick plate** substrate; the substrate **must be
  specified**, and because the Mueller matrix is computed the lower medium is **always
  vacuum**. Supports **reflectance and/or transmittance**.
A `Plate` with no substrate is invalid — extend `validateSample` (`ElementId.fs:799-808`)
to require `structure.substrate = Some _` when the geometry is `Plate`.

**D.2 R/T support reuses the existing `Emission` DU.** R/T is the established
`Emission = EmitReflectedOnly | EmitTransmittedOnly | EmitBoth` (`Placement.fs:108-138`),
whose smart setters `withReflected` / `withTransmitted` (`:126-138`) already enforce
"never neither — clearing one forces the other on", and `MeasurementMode.ofEmission`
(`Experiments.fs:106-110`) derives the capture; the beam wiring consumes it
(`RayModel.emittedGroups`). Give the Library `Sample` a supported-emission field reusing
`Emission`, geometry-constrained (operator, 010/Q3):
- **Thin film** — forced `EmitReflectedOnly`; the single allowed branch cannot be cleared.
- **Plate** — **default `EmitBoth`**; the user may constrain it to R-only or T-only
  (clearing one leaves the other, via the existing setters).
The sample editor exposes R / T checkboxes driven by `Emission.withReflected` /
`withTransmitted` (so the clearing and never-neither rules fall out of the existing DU).
The supported-emission then **bounds** the placed element's `Emission` and the experiment's
`MeasurementMode`: when the sample supports both, the experiment chooses which to capture —
R, T, or both — defaulting from the supported-emission (operator, 010/Q3). Touch points:
`Sample` (`ElementId.fs:175-185`) + `SampleEditSnapshot` / `toSample` / `init` + editor
`Model` / `Msg` (`SampleEditorView.fs`), and `MeasurementMode.ofEmission` / the placed
sample's default `Emission`.

**D.3 Substrate "Set from chosen" must work for a Plate.** Today the button is enabled only
by `hasChosenMaterial m` (`SampleEditorView.fs:963`, `:923-926`), and `m.chosenMaterial` is
set only by picking a material for an existing film-layer row (`BindMaterialToLayer`,
`:492`) — so a `Plate` with no films has no way to set its substrate. Per spec 0038 Part G,
material picking goes through the Materials window in Select state: the substrate "Set…"
opens the Materials window in Select mode targeted at the substrate slot (the
`WindowLauncher` / Select flow), then applies `SampleStackMsg.SetSubstrate`
(`SampleStackEditor.fs:367-368`). The lower half-space stays vacuum (D.1).

**D.4 Acceptance.** A Plate cannot be saved without a substrate; its substrate is settable
from the Materials Select window; a Thin film is R-only and its R branch cannot be cleared;
a Plate defaults to both and is constrainable to R-only or T-only; the experiment's capture
is bounded by and defaults from the sample's supported emission.

---

## Suggested phasing (small slices, each independently green)

1. **Facet totality (Part B)** — pure Domain (`LibraryFacets`); count-sum tests.
2. **Tree sorting at every level, incl. facet groups (A.2.5)** — projections + branch
   sort; tests.
3. **Tree collapse/expand + collapsed-default (A.2.1–A.2.3)** — control state + handler +
   rendering; update headless tests.
4. **Tree selection highlight (A.2.4)** — control selected-code + tint; tests.
5. **Gyration/μ chart applicability (C.1)** — `NkDispersionChart` + editor; tests.
6. **Sample geometry validation (D.1)** — Plate requires substrate; `validateSample`.
7. **Substrate "Set from chosen" via the Materials Select window (D.3)**.
8. **Sample R/T support via `Emission` (D.2)** — Domain field + editor R/T checkboxes +
   `MeasurementMode` bound/default; tests.
9. **Description-box wrap (C.2)** — minor, optional.

## References

- The 0038 full spec (`.spec-md`) and preliminary (`006`) for the faceted-tree,
  material-editor, and sample-geometry design in place.
- Spec 0027 (`specs/0027/.manual/024-materials-and-experiments-spec.md`) — the `Emission` /
  `MeasurementMode` R/T model reused in Part D.
