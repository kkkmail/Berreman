# 002 — Comments and questions on the BMN UI standardization ideas (reply to 001)

Discussion only — no code was changed. I read the relevant code (file:line references
below are to the current `0038` branch) and researched the open issues (faceted
navigation practice, reference-data versioning, ellipsometric file formats, the
Avalonia crash). Questions are numbered **Q1…Q24** inline and collected at the end.

---

## 0. One architectural fact to settle before "standardizing controls"

The screen the user actually sees is *not* the `OpticalConstructor.Ui` shell. The
Launcher's **Main** button opens `MainConstructorWindow`, which mounts
`TableAndElementRotationView.mainView` from **`OpticalConstructor.TestWindows`**
(`OpticalConstructor.App\Program.fs:148`). The older `MainWindow` hosting
`Shell.view` is no longer opened (`Program.fs:116-117`), so `Shell.fs`, `Ribbon.fs`
(Ui project), `MaterialsView.fs`, `ConstructorView.fs` are effectively dead code for
the Main flow. The real workbench is one 2 400-line file
(`TestWindows\TableAndElementRotationView.fs`) plus the shared controls in
`OpticalConstructor.Controls`.

Since 0038 is explicitly about standardizing UI pieces, I suggest the spec also
covers the *home* of those pieces: promote the working views out of a project named
`TestWindows` into a properly named project (or fold them into `.Ui`), and retire or
re-purpose the dead shell. Otherwise we standardize controls that live in a project
whose name says they are throwaway.

**Q1.** Should 0038 include this relocation/renaming (my recommendation: yes, as a
mechanical first slice), or is the project layout out of scope for now?

---

## 1. Materials and Library out of the ribbon — where should they live?

Today Materials and Library are `FullSurface` ribbon bays (`TableAndElementRotationView.fs:2339-2340`)
— they hijack the whole area under the tab strip, which is exactly why they feel like
they don't belong: they can't be seen next to the table, and the full-surface slot
needed a special keyed border to avoid a FuncUI crash (`:2391-2398`).

Alternatives considered:

- **(a) Launcher-only entry (your inclination).** Clean, but during real work you
  constantly need the library *mid-flow* (Selector's "Choose…" opens it anyway, and
  the sample editor will open the materials window to pick a layer material). If the
  only way to browse is to go back to the launcher, that's a workflow regression.
- **(b) Launcher entry AND openable from the constructor.** The windows must be
  reachable from the constructor flow regardless (Selector, sample editor), so also
  giving them a small direct affordance (toolbar button or menu) costs nothing.
- **(c) Keep them as ribbon bays.** Rejected — the current pain.
- **(d) Menu bar on the main window only.** Works, but hides them from someone who
  just launched the app and wants to curate the library without opening a table.
- **(e) IDE-style dockable side panels.** Nice long-term, heavy in FuncUI; defer.

**Recommendation: (b).** Launcher gets `Main / Inverse / Materials / Library`
buttons (your proposed order), and the same windows are openable from the
constructor. Both routes open the *same* window instance.

Two consequences that are architectural, not cosmetic:

1. **Proxy scope must move up.** Today `Library / SampleProxy / MaterialProxy /
   CategoryProxy / Experiments` are created *inside* `MainConstructorWindow`
   (`Program.fs:129-148`). If the Launcher opens a Materials window, the proxies
   must be created once at app scope and injected into every window, or a
   launcher-opened Materials window and a constructor would see two different
   in-memory databases. This lift is needed anyway for the seeding/EFC plan (§13).
2. **Window instance policy.** Everything today is non-modal `.Show()` with no
   single-instance guard — every click makes a brand-new window. For Library and
   Materials I propose: single instance, second open request activates/focuses the
   existing window (re-constraining it if opened in Select state, see §2).

**Q2.** Agree with (b), or do you want launcher-only (a) strictly?
**Q3.** Single-instance for Materials/Library windows (activate on re-open) — agree?

---

## 2. Library ≡ Selector unification — one hidden ambiguity to resolve first

Fully agree with the principle: *a Selector is the Library window in a selection
state, the same window in code, not a copy.* But "Library" currently means two
different things in the code, and the unification has to pick:

- The **Library bay** (`SampleLibraryControls`) shows **samples only**
  (`SampleProxy.searchSamples`, `TableAndElementRotationView.fs:2088-2101`).
- The **Selector's corpus** is `LibraryEntry` = samples **and** sources, detectors,
  polarizers (`ElementId.fs:188-193`), filtered per element kind via
  `LibraryEntry.forKinds` (`ElementId.fs:234-242`).

A selector must be able to select a detector or a source, so the unified Library
window must cover *all element presets*, not just samples. Proposal:

- **Library window** = all `LibraryEntry` kinds; the default tree groups by kind
  (Sources / Polarizers / Detectors / Samples), and the sample subtree carries the
  sample facets of §5. Opening it from a Selector constrains it to the element's
  kind (the constraint is just a pre-applied facet the user cannot remove in Select
  state).
- **Materials window** = materials + categories (materials are not table elements;
  they stay their own window, selected from the sample editor).

State is a mode DU on the window's context, not a copy of the view:

```fsharp
type LibraryWindowMode =
    | Browse
    | Select of SelectionContext   // fixed kind constraint + where the result goes

type SelectionContext =
    { constraint : KindConstraint
      onSelected : LibraryEntryId -> unit   // dispatched back to the requester
      onCancelled : unit -> unit }
```

Select mode adds exactly two buttons — **Select** (returns the highlighted entry and
closes) and **Close** (cancels) — everything else (add, edit, filter, categories) is
the ordinary library because it *is* the ordinary library. That directly satisfies
"the user can add an element on the fly".

Because windows are non-modal, three lifecycle rules need agreeing:

- If the user changes the table selection while a Select-state library is open, the
  pending selection context is stale. Today the analogous stale pending bind is
  cleared on selection change (`TableAndElementRotationView.fs:941-944`). I propose
  the same: **close (cancel) the Select-state window** when its requesting target
  changes or disappears.
- A second "Choose…" while one Select window is open closes/cancels the first
  (single-instance, §1).
- On select, the result is dispatched as a message carrying the *target* (element id
  or sample-layer position), so if the target vanished meanwhile it's a no-op with a
  status line, never a crash.

**Q4.** Confirm the corpus split: Library window = all element presets (kind as top
facet), Materials window separate. Or did you intend "Library" to stay samples-only
with detectors/sources handled some other way?
**Q5.** Select-state windows: modal would sidestep all the staleness rules — you
specified non-modal for the sample editor's material picker; do you want non-modal
everywhere (with the staleness rules above), or non-modal for Browse but modal for
Select?

---

## 3. New Selector flow

Agreed. Restated against the code: today's flow (click Selector bay → click table
element → kind-filtered rows appear in the bay → click row → confirm/cancel panel,
`LibraryControls.fs:105-197`, `TableAndElementRotationView.fs:1298-1338`) becomes:
click Selector → click element → **Choose…** button → Library window opens in
Select state constrained to the element kind.

One design question: the current inline row list is genuinely convenient when there
are 2–3 choices (detectors: exactly 2). Options: (i) drop it entirely — one path,
maximal standardization; (ii) keep a small "quick pick" strip of the N most recent /
few available entries next to the Choose button, both routes converging on the same
bind message. I lean (i) for v1 — one code path, and the Library window with a
pre-applied constraint on a 2-element corpus is instant anyway — with (ii) as a
later ergonomic addition if it hurts.

**Q6.** Drop the inline quick-pick entirely (i), or keep a reduced strip (ii)?

Related: when nothing is bound, the run currently synthesizes ideal defaults — the
analyzer silently defaults to `IdealLinear` (`TableAndElementRotationView.fs:1418-1427`)
and input to unpolarized Stokes. Once ideal elements are proper library entries
(§4), should an unbound element still get a silent ideal fallback, or should new
elements be **created pre-bound** to the ideal library entry (explicit, visible in
the UI, and consistent with §10's set/not-set display)? I recommend pre-binding on
add: `AddElement LinearPolarizer` binds `pol-lp` immediately; the user re-binds via
Selector if they want something else.

**Q7.** Pre-bind new elements to their ideal library entry on add — agree?

---

## 4. Ideal LP/CP (and LPCP/CPLP…) as library entries

Current facts: the ideal presets **are already library entries** — `pol-lp`,
`pol-cp-left`, `pol-cp-right` are seeded (`ElementId.fs:537-539`) with
`PolarizerKind = IdealLinear | IdealCircularLeft | IdealCircularRight`
(`:174-177`). What is *computed on the fly* is the physics: the constant Mueller
matrices come from `Propagation.analyzerMueller` / `inputStokes`
(`Propagation.fs:54-89`) as functions of `(kind, θ)`. So "add them to the library
instead of wherever we add them now" is mostly about (a) making the *behavior* a
stored, extensible description rather than a hardcoded 3-case function, and (b) new
compound categories. Sketch:

```fsharp
type PolarizerBehavior =
    | ComputedIdeal of PolarizerKind          // physics synthesized as today
    | ConstantMueller of MuellerMatrix        // stored 4x4, defined at theta = 0

type PolarizerCategory =                       // the facet for the tree (§5)
    | LpCategory | CpCategory | LpCpCategory | CpLpCategory | CustomMueller
```

Two technical notes supporting this:

- **Rotation still works for stored matrices.** A constant entry is stored at its
  reference orientation and rotated at run time, `M(θ) = R(−θ) · M · R(θ)` with the
  standard Mueller rotation matrix — so `VaryR1` experiments keep working for any
  constant element, including compounds. Compounds (LPCP = CP·LP as matrix product)
  can be stored either as the product (opaque) or as an ordered list of components
  each with its own offset angle; the latter stays editable and self-describing. I
  recommend the component-list form with the product computed on demand.
- **Scope check:** constant-Mueller elements live only in the Stokes/Mueller
  pipeline (which is how non-sample elements are applied today, `Propagation.fs`).
  They cannot participate in field-level/coherent calculations — fine for polarizers
  and detectors, but worth stating in the spec so nobody later tries to put a
  constant-Mueller "sample" into the Berreman stack.

Acknowledged that the dedicated edit entry point for these special elements is *not
now*; the DU above just leaves the door open (a new case and an editor later are
non-breaking additions).

**Q8.** Store compounds as an ordered component list (recommended) or as the
premultiplied matrix?

---

## 5. The filterable reconfigurable tree — this is faceted navigation

What you describe is precisely **faceted navigation/search** (the e-commerce /
library-science pattern), and the good news is the industry practice matches your
requirements almost one for one:

- counts on every branch/value; never show zero-count branches (some UIs gray-out
  instead of hide to keep layout stable — your "never show" is fine and simpler);
- **AND across attributes, OR within an attribute** (your multiselect of film
  materials);
- **count preview** when choosing a constraint: each offered value shows the count
  of what the result *would be* if applied — this is exactly your "500 → 100 → 5
  must all be visible while building the search";
- an **applied-constraints breadcrumb** where each constraint shows its
  after-count and is individually removable, in any order.

### Data model

The existing filtering is ad-hoc (`MaterialQuery` with text/category/dispersion,
`MaterialLibrary.fs:481-494`; `SampleQuery` with text/substrate,
`ElementId.fs:287-298`; hand-built `LibraryTree` nodes, `:551-606`). There is no
generic attribute abstraction yet — this is net-new, and F# DUs fit it well:

```fsharp
type AttributeKey = AttributeKey of string            // elevated, per house style

type AttributeValue =
    | DiscreteValue of DiscreteKey                    // one of a known finite set
    | NumericValue of float                           // bucketed dynamically (thickness, nm)

type AttributeKind =
    | DiscreteAttribute of DiscreteKey list           // fixed or corpus-derived value set
    | NumericAttribute of RangeStrategy               // dynamic bucket construction

type AttributeDef<'item> =
    { key : AttributeKey
      name : string
      kind : AttributeKind
      appliesTo : 'item -> Applicability              // dependent facets, see below
      extract : 'item -> AttributeValue list }        // LIST — multi-valued, see below
```

Three subtleties that the spec should nail down because they shape everything:

1. **Dependent facets.** Handedness and symmetry class exist only if optically
   active; dispersion model only if dispersive; gyromagnetic refinement only if
   magnetic. That's the `appliesTo` predicate: an attribute inapplicable under the
   current constraints simply doesn't appear as a branch/offer. Note the code
   already encodes one such dependency — eps anisotropy constrains offered gyration
   classes (`availableGyrationClasses`, `MaterialComplexityEditor.fs:373-385`) — the
   facet layer should reuse those functions, not re-derive the physics.
2. **Multi-valued attributes.** A dispersive biaxial material has one model *per
   axis per segment* (`EditSegment { model1; model2; model3 }`,
   `MaterialComplexityEditor.fs:100-106`), so "dispersion model" is a set, not a
   scalar; likewise a sample's "film material" is a set. Hence `extract` returns a
   list, and branch counts will not sum to the total — expected and correct in
   faceted UIs, but worth a line in the spec so nobody "fixes" it.
3. **Extractors must run on the resolved engine value, not on `complexity`.**
   `MaterialEntry.complexity` is `None` for the coded presets (silicon, langasite,
   vacuum — `MaterialLibrary.fs:221-238`); the truth for all entries is
   `properties : OpticalPropertiesWithDisp`, from which anisotropy, transparency,
   activity, magnetics are all pattern-matchable (`Dispersion.fs:285-291, 383-408,
   528-564, 652-669`).

Mapping your material attribute list to code, with two flags:

| Facet | Source | Note |
|---|---|---|
| Category | `MaterialEntry.category : CategoryId` | categories are data now (`MaterialCategory`), so the value set is corpus-derived |
| Anisotropy | `ConstantEpsValue` / `EpsDispersiveValue` cases | Isotropic / Uniaxial / Biaxial |
| Constant vs dispersive | `EpsWithDispValue` case | |
| Transparent vs absorbing | `ConstantEpsValue` case | **only explicit for constant** materials; for dispersive ones k comes out of the model. Your text scopes the refinement to "constant" — matches the code. If you also want it for dispersive, it must be *derived* (k ≈ 0 over the visible range?) — needs a definition. **Q9** |
| Dispersion model | `DispersionModel` (10 cases) | multi-valued (per axis / per segment) |
| Optically active + symmetry class + handedness | `GyrationClass`, `Handedness` | dependent facet |
| Magnetic: scalar vs gyromagnetic | `ConstantMuValue` / `MuWithDispValue` | dependent facet |

For the sample (Library) facets:

- *Primary material (substrate)* — ambiguity: `SampleStructure.substrate :
  SampleLayer option` (the thick plate) vs `SubstrateKind = ThinFilm | Plate |
  Wedge` (geometry, `ElementId.fs:53-56`) vs `lower : MaterialId option` (exit
  half-space). I assume you mean the `structure.substrate` layer's material.
  **Q10.**
- *Thin films / no thin films* — I propose: derived from `structure.films`
  non-empty, **independent** of `SubstrateKind` (a Plate can carry films in the
  model). **Q11** — confirm.
- *Film material(s)* — corpus-derived value set ("only what is actually used"),
  sorted by name, multiselect: exactly the `extract`-list + OR-within-facet
  semantics above.
- *Film thickness ranges* — see below. Open sub-question: for `Repeated` period
  groups, is the faceted thickness the per-layer thickness, the period thickness,
  or total stack thickness? I'd offer per-layer for v1 (matches "10–20 nm layer").
  **Q12.**
- *"Same as for material"* — I read this as: material facets apply to a sample
  through its constituent materials (a sample is "optically active" if any referenced
  material is). Scope question: any layer's material, substrate's material only, or
  user-pickable scope? Recommend "any constituent material" for v1. **Q13.**

### Dynamic numeric ranges

For thickness I recommend **log-decade "nice" buckets** (1–2–5 per decade: 1–2 nm,
2–5 nm, … 10–20 nm, 20–50 nm …), clipped to the currently-constrained population,
splitting only buckets that are populated, plus a manual min/max entry for the
power user. Quantile buckets adapt better statistically but produce unmemorable
boundaries (13.7–41.2 nm); physics users think in decades. The buckets recompute
against the constrained set, which gives exactly your 500 → 100 → "sensible ranges
within those 100" behavior.

### Ordering, representations, and the tree

Two distinct things are in play, and I suggest the spec separates them:

- **Representation** = the facet *order* used to build the tree (category →
  anisotropy → … vs optically-active-first). A representation is just a permutation
  (or subset) of `AttributeKey`s — a small, nameable, persistable value. "Changing
  the representation changes the order of branches" falls out for free.
- **Breadcrumb of applied constraints** = the *history* of the user's search, in
  application order, each with its count. Your "tree must be branched in the order
  of selections" reads like you want the tree re-rooted to follow the breadcrumb.
  That is one option (representation = breadcrumb order + natural order for the
  remaining attributes); the other is a fixed representation with the breadcrumb
  shown separately above the tree. I mildly prefer the second (stable tree, no
  reshuffle on every click), but this is taste. **Q14.**

### Threshold, performance, and FuncUI cautions

- The "auto-rebuild under N=100" gate: counting is always cheap (a fold over a few
  hundred items), so counts stay live; only *tree materialization* is gated behind
  the Search/Show button. The threshold: note there is currently **no
  appsettings.json anywhere** — configuration is the per-user schema-validated
  `%AppData%…\environment.json` (`UserEnvironment.fs:181-205, 333-351`), and there
  is an exact precedent for a 100-default (`Preferences.sweepPoints = 100`,
  `UserEnvironment.fs:104, 230`). Recommendation: put the threshold in
  `Preferences` now; introduce `appsettings.json` only with the EFC work (§13),
  for infrastructure (connection strings), not UI preferences. **Q15.**
- Corpus size (tens–hundreds, maybe low thousands) means naive pure-F# fold
  filtering and counting is fine — no indices, fully unit-testable, matches the
  proxy style. No premature optimization needed.
- Two FuncUI-specific cautions from this codebase's own history: (a) every
  generated tree row must use `AutomationProperties.AutomationId` (never
  `Border.name`) and ideally `View.withKey` per item — that exact mistake is
  today's Selector crash (§12); (b) the filter text box should commit on
  Enter/LostFocus or debounce, *not* per keystroke, because each commit rebuilds a
  potentially large tree (the same class of issue that previously hung ui-smoke
  with auto-committing number boxes).

The unified control then is `FacetedTree<'item>` (generic, pure state + pure
builders, Avalonia-free core, per your standardization goal), instantiated twice:
materials (over `MaterialProxy`) and library (over the merged element-preset
corpus), each supplying its own `AttributeDef` list. Edit forms stay as they are —
agreed, they are genuinely different.

---

## 6. Material edit form — chart squeeze and "what is being drawn"

Confirmed root causes:

- **Layout**: the form is a `DockPanel`; the progressive ladder is a `StackPanel`
  docked Top with **no ScrollViewer**, and the n/k chart is the center fill
  (`MaterialEditorView.fs:1067-1132`). Every unlocked panel (segments, gyration,
  μ) grows the top dock and squeezes the chart toward zero height.
- **Single curve**: `NkDispersionChart` samples **only ε₁₁** and plots n, k of
  `√ε₁₁` (`NkDispersionChart.fs:40-64`). For a biaxial material the ε₂₂/ε₃₃ axes
  are simply never shown; gyration and μ dispersion are never shown.

Proposed solution (concrete):

1. **Two-column layout with a splitter.** Ladder in the left column inside a
   `ScrollViewer`; chart fills the right column at full form height, with a
   sensible `MinWidth`/`MinHeight`. The window is 1150×980 — wide enough, and
   charts want width more than the ladder does. (Alternative: keep vertical
   stacking but put the ladder in a scroll region and give the chart a fixed
   minimum height with a `GridSplitter`; acceptable, but the chart then competes
   with the ladder forever. I prefer the two-column form.) **Q16.**
2. **One chart area, multiple tabs, all relevant curves with a legend.**
   - Tab "n, k" — all principal axes: n_x, n_y, n_z solid; k_x, k_y, k_z on the
     right axis, per-series legend toggles. Uniaxial shows two (o/e), isotropic
     one — the tab renders what the anisotropy provides. This is what
     refractiveindex.info and the commercial ellipsometry packages do; six curves
     with distinguishable styles is normal.
   - Tab "Gyration" — g components vs λ (only present when optically active;
     dispersive activity is your "it may also have dispersion" case).
   - Tab "μ" — Polder components vs λ (only when magnetic).
   Tabs appear/disappear with the ladder toggles, so "what is being drawn" is
   always answered by the visible tab + legend, and each tab has honest vertical
   space.
3. **Unsaved-edits confirmation.** There is none today — Cancel calls
   `requestClose()` directly (`MaterialEditorView.fs:423-424`). The editor state is
   plain records/DUs, so dirty = structural inequality between the state captured
   at load and the current state — no flags to maintain. The confirm must hook
   **window close** (`OnClosing` on the `HostWindow`), not just the Cancel button,
   or the X button remains a silent data loss. Same treatment for the sample
   editor (`SampleEditorView.fs:549-550` has the identical pattern, and its header
   comment even says "Cancel discards").

---

## 7. Sample edit form — material choice via the standardized window

Confirmed: materials are picked from a `WrapPanel` of clickable name-boxes
(`SampleEditorView.fs:740-758`) — the "buttons" pattern you want gone. Replacing it
with the **Materials window in Select state** (same mode DU as §2) makes the sample
editor and the table Selector use one mechanism, which is the whole point.

Details worth fixing in the same stroke:

- The selection context must carry the **target layer position**
  (`SampleStackEditor.LayerPosition`), and the returned message is
  `SetLayerMaterial (position, materialId)` — if the row was deleted while the
  picker was open, it's a no-op with a status note (same staleness rule as §2).
- The sample editor loads the material list **once at construction**
  (`SampleEditorWindow.fs:32-35`). With add-material-on-the-fly, that snapshot goes
  stale the moment the user adds a material from the picker. Minimal fix that
  avoids a pub/sub system: re-query the proxy whenever a Select-state window
  returns, and on window activation. A real change-notification seam
  (subscription field on the proxies) can come with storage. **Q17** — is
  re-query-on-return acceptable for now (my recommendation), or do you want live
  cross-window notifications in this spec?
- Dirty confirmation: same as §6.

---

## 8. Delete / inactive / supersede — research and recommendation

Current state: no lifecycle at all — the only guards are referential
(`MaterialStillReferenced` blocks removal, `ElementId.fs:751-766`;
`BuiltInNotRemovable` for categories). Nothing is versioned, nothing is persisted
between runs yet, which makes **now** exactly the right time to decide this (§13's
schema depends on it).

The two candidate mechanisms, against industry practice:

- **(A) Copy-on-use (snapshot).** The experiment embeds a frozen copy of every
  property it used. This is the "invoice copies the price" pattern; in lab-data
  terms, full denormalized provenance. Pros: an experiment is self-contained and
  archival-safe; no version resolution anywhere. Cons: a sample references
  materials **by id** (`SampleLayer.materialId`, `ElementId.fs:80-85`), so a true
  snapshot must deep-copy the whole sample→materials closure per experiment;
  cross-experiment queries ("everything measured on quartz") degrade into
  value-matching; and edits after copy silently diverge from the library with no
  traceable lineage.
- **(B) Immutable-once-used + supersede (versioning).** This is Slowly Changing
  Dimensions Type 2 / temporal reference data: a used entity's physics becomes
  immutable; "edit" creates a new version that supersedes the old under the same
  name; old references keep resolving to the exact version they used. This is also
  how LIMS/scientific-provenance systems treat reference data, because
  reproducibility demands that the parameters behind a stored measurement be
  recoverable forever.

**Recommendation: (B) as the primary mechanism, with a cheap dose of (A) at the
export boundary.** Concretely:

- Identity becomes two-level: `MaterialId` (stable name-carrying identity — your
  quartz stays "quartz") + `MaterialVersionId` (what samples/experiments actually
  reference). Same for samples.
- "Used" ⇒ physics immutable. The Edit button on a used version becomes "New
  version…" (pre-filled). Metadata (description, category assignment) stays
  editable without versioning — only physics edits version. **Q18** — agree with
  that metadata/physics split?
- **Inactive** is an independent orthogonal flag (your first paragraph): hides the
  entry from *new-use* pickers; resolution of old references ignores it. Superseded
  versions behave like inactive automatically (only the latest active version is
  offered for new use, with a "show obsolete/superseded" toggle in the tree).
  Faceted counts (§5) count offer-able entries by default.
- The dose of (A): since everything serializes to JSON anyway, a project/report
  **export** embeds the resolved versions it references (belt-and-braces archival,
  and it solves the "send a project to a colleague without the database" problem).
  No snapshotting inside the live model.

Definition needed for "used": referenced by a sample? bound on a table? referenced
by a persisted experiment? Recommendation: *used = referenced by any persisted
experiment or saved project*; mere library-internal references (sample→material)
keep today's delete-block but do not freeze physics. **Q19.**

**Q20.** Version granularity: every save of physics = new version silently, or an
explicit "publish/supersede" action with drafts editable in place until published?
(I lean explicit supersede — matches your description and avoids version spam.)

---

## 9. Experiments must hold the full setup

Confirmed gap, and one more surprise: **two experiment models coexist** —

- `Experiments.Experiment` holds only the *varied element's* `ElementId` + label +
  variable + measurement + range (`ElementId.fs:909-917`); the setup "is the scene
  itself" by design (`:781-795`);
- `Groups.ExperimentCollection` (persisted in the groups JSON) is an older
  toggle-list aggregate (`Groups.fs:114-129`) — and its name collides with the
  "experiment collection" term you now want for the inverse problem.

Agree the experiment must capture the full element chain. Given §8's decision, I
propose the experiment references the setup as **an ordered list of element
descriptors** (kind, placement/orientation, bound entry *version* reference), i.e.
reference-based with versioned targets — not a value-copy of the scene — plus your
calibration semantics falling out naturally:

- sample element optional (E2 — no sample), light source optional (E3 — dark line);
- an experiment with a rotate captures which element rotates, as today;
- detector kind is part of the chain, which also determines the expected data-file
  shape (§11).

The naming/consolidation of `Groups.ExperimentCollection` vs the new
experiment-collection-set concept should be part of the spec (rename the old one or
absorb it).

**Q21.** Setup capture: ordered element-descriptor list referencing versioned
library entries (recommended), or full value-embedding of the scene per experiment?

---

## 10. Set / not-set on the table — proposal

Confirmed: the renderer receives only `placement/centre/zoom/opticalSign`
(`ElementRenderer.fs:26-32`) — bound state isn't even available to it; the only
visual state is selected vs not (`:71, 218-219`); "bound/unbound" appears only in
the text readout (`TableAndElementRotationView.fs:1096-1103`).

Proposal (primary): **unbound elements render with a dashed outline and ghosted
(desaturated / reduced-opacity) fill; bound elements solid.** Rationale: it is a
*pattern* difference, not a hue difference (colorblind-safe, survives theming), it
matches the CAD/EDA convention for placeholder geometry, and it works for every
element shape the renderer draws. Secondary options: a small badge glyph near the
element (extra clutter, but screenshots read well), or label-text color (too weak
alone). Tooltip/readout stays.

Model it as a proper DU on the renderer input, per house style — and note it is
three-valued, not a bool: `Bound | Unbound | NotBindable` (lenses/mirrors have no
library presets today — `forKinds` never yields them — and they should render
normally, not as ghosts forever).

If §3's pre-binding recommendation (Q7) is accepted, "unbound" becomes rare and
loud — which is exactly what you want for the inverse flow where the sample is
deliberately unbound.

**Q22.** Does "set" mean strictly "valueId bound", or must the element also be
fully parameterized (e.g. a source preset carries its wavelength — is a bound
source with a default wavelength "set")? I assume bound ⇒ set, since presets are
complete.

---

## 11. Inverse problem

The overall shape — same constructor form in a special state, sample unbound or
hint-bound, experiments fed with measured data files, then a solver — fits the
existing architecture well (`Optimization` project with Alglib already exists for
fitting). Comments by piece:

- **Launcher**: `Main / Inverse / Materials / Library` — fine (§1).
- **Hint semantics**: "no hint → no experiment chart; hint → chart as now" is
  clean and cheap to implement, since chart drawing already requires a resolvable
  sample.
- **Mueller extraction**: "always attempt to find the Mueller matrix for a given
  (λ, θ)" — one caution from polarimetry practice: a full 4×4 Mueller matrix needs
  ≥16 independent polarization-state-generator/analyzer configurations; with only
  rotating ideal LPs you recover a subspace, and the reconstruction's conditioning
  depends on the angle sets chosen. For v1 a plain least-squares per (λ, θ) over
  whatever the collection provides is right; the spec should just note that the
  solver reports rank/conditioning so an under-determined collection produces a
  diagnostic, not silent garbage. The second stage (fitting material parameters
  across many (λ, θ) Mueller matrices — your "experiment collection set") is then a
  standard parametric fit through the existing merit-function machinery.
- **Intensity CSV**: your spec (first row = labels, ignored; then `X,Y` rows) is
  fine. I recommend: strict comma + invariant culture, typed parse errors
  (`Result`, per house style), and a validation step that checks the file's X range
  against the experiment's varied parameter and range — the X column *means*
  whatever `VariableParameter` says (nm for `VaryWaveLength`, degrees for
  `VaryR1/R2`), and mismatches should be loud. Also: raw intensities need the
  calibration experiments to be useful — the correction pipeline (e.g.
  `(E_sample − E_dark) / (E_reference − E_dark)`) should be an explicit, named
  computation on the experiment collection, since you've now made calibration
  experiments first-class. **Q23** — should v1 apply that normalization
  automatically when the collection contains reference/dark experiments, or leave
  data raw and let the solver's model include the imperfect elements (your E2
  captures non-ideal LPs, which suggests the *model-includes-imperfections* route)?
- **Ellipsometric data** (researched): there is no single industry standard file,
  but the de-facto shape across vendors (Woollam CompleteEASE/WVASE, Horiba
  DeltaPsi2, Sentech, Accurion) is ASCII with header lines then rows of
  `wavelength (nm or eV), Ψ (deg), Δ (deg)` at a stated angle of incidence;
  multi-angle data either repeats blocks per AOI or adds an AOI column. Two other
  encodings appear in the wild: N/C/S (`N = cos 2Ψ, C = sin 2Ψ cos Δ,
  S = sin 2Ψ sin Δ`) and, from Mueller-matrix ellipsometers, normalized
  `m_ij/m_11` elements. Recommendation for v1: CSV `wavelength_nm, psi_deg,
  delta_deg` with optional `aoi_deg` column (one file per experiment, AOI from the
  experiment if the column is absent); design the loader proxy so NCS and Mueller
  imports are additional parsers later, not schema changes.
- **Loader proxy**: exactly the house pattern —
  `ExperimentDataProxy = { tryLoadIntensity : DataFilePath -> Result<IntensitySeries, ExperimentDataError>; tryLoadEllipsometric : … }`,
  in-memory/mock implementation for tests, real file IO at the boundary. Unit tests
  with mock proxies then cover parsing, validation, and the correction pipeline
  with zero file IO.
- **Last folder**: note that `EnvironmentSettings.lastFolders` already exists,
  round-trips, and is schema-validated — but is **inert**: pickers never set
  `SuggestedStartLocation` and nothing updates it (`Shell.fs:403-450`,
  `UserEnvironment.fs:184`). So this is wiring, not new design; "unless the user
  cancels" = update only on a confirmed selection, which falls out naturally.

---

## 12. Ellipsometer detector and the Selector crash

Two separate items here:

- **The ellipsometer already exists** as a seeded detector preset
  (`det-ellipsometer`, `ElementId.fs:535-536`, `DetectorKind = Intensity |
  Ellipsometer`), and the Ψ/Δ physics is implemented (`PsiDelta`,
  `psiDeltaOfStokes`, wavelength/R2 Ψ-Δ sweeps — `Propagation.fs:304-394`). What is
  broken is only *selecting* it, because of the crash below. After §2/§3, detector
  choice is the same Choose… → Library-in-Select-state flow as everything else.
- **The crash is diagnosed** and is not detector-specific — the detector rows are
  just the easiest way to trigger it. `LibraryControls` sets `Border.name id` on
  every generated row (`LibraryControls.fs:112`, also `:148` for confirm/cancel),
  in an **unkeyed** `List.map` (`:220`). Avalonia's `StyledElement.Name` is
  write-once after styling; when the row set changes (different element kind
  selected) or a row is re-patched in place (`RowPlain → RowPending` highlight flip
  on click), the FuncUI virtual-DOM patcher *reuses* the `Border` instance and
  tries to assign a new `Name` → the `InvalidOperationException` in your trace.
  The codebase already contains the fix pattern, with an explanatory comment, in
  `MaterialsControls.fs:139-146` and `ExperimentControls.fs:264`: use
  `AutomationProperties.AutomationId` (freely mutable attached property) for
  dynamic rows, never `Border.name`; plus `View.withKey` per row for stable
  identity. `LibraryControls` is the un-migrated outlier — and the same latent bug
  sits in `RotationControls.fs:119`, `RayPositionControls.fs:71`,
  `ElementPaletteControls.fs:55`, `ExperimentControls.fs:240, 581`, which should
  all be swept in the same fix. Whatever happens to the Selector redesign, this
  sweep is a small, independent, immediately shippable slice.

---

## 13. Storage / EFC / seeding

Facts first: there is **no EF Core, no DbContext, no SQLite/MSSQL code anywhere**
in the repo (only transitive DLLs in `bin/`); persistence today is `.ocproj` JSON
(beam tree, systems, sources, placements, table — **not** materials, samples,
categories, or experiments; `Project.fs:26-48`) plus the per-user groups and
environment JSONs. Materials/samples/experiments live in seeded in-memory proxies
and reset every run. The seed data already sits close to its source with fixed
literal Guids (`MaterialIds` `MaterialLibrary.fs:51-63`, `builtInEntries`
`:363-461`, `SeedSamples` `ElementId.fs:381-525`) — so your "information must be
kept close to the source" requirement is *already satisfied*; the seeding project
should **pull** these values from Domain and push them through proxies, not re-declare
them.

The plan (separate C# EFC project on the STL model, MSSQL/SQLite switch, F# seeding
project, proxy-swap later) is sound and fits the existing architecture unusually
well — the composition root already wires five `createInMemory` proxies explicitly
(`Program.fs:129-148`), so "seeding without a database" is literally: the seeding
logic takes a `SeedingProxy` (record of `save…` functions), and today's
implementation of that proxy is backed by the in-memory stores. Later the EFC
implementation is dropped in and nothing above the proxy changes. Comments:

- **Decide §8 (versioning) before the EFC schema.** Version tables vs single-row
  entities is the biggest schema fork; retrofitting SCD2 onto a live schema is the
  classic expensive mistake.
- **Seeded Guids become foreign keys** the moment a database exists — worth an
  explicit spec line that shipped seed Guids are frozen forever.
- **Configuration split**: `appsettings.json` (new, EFC convention) for
  infrastructure — provider choice + connection strings; `environment.json` stays
  the home of user preferences (§5's threshold). Don't merge the two.
- **What stays file-based**: I'd keep the scene/project as `.ocproj` and put the
  library + experiments in the DB, with experiments referencing library version
  ids. Consequence to note now, solve later: a `.ocproj` then depends on DB
  content, so "send a project to a colleague" needs the §8 export-with-embedded-
  versions bundle. **Q24** — agree with that DB/file split?

---

## 14. Smaller observations picked up along the way

- **Two recent-files stores** exist in parallel: `EnvironmentSettings.recentFiles`
  and `RecentFiles.fs` (`%AppData%…\recent.json`) — consolidation candidate.
- **UiIds are scattered** as per-control modules (a dozen `module UiIds`
  instances) while CLAUDE.md prescribes one central constants module —
  consolidation candidate for the standardization spec.
- **`lastFolders` is persisted but inert** (§11) — cheap win.
- **Window policy** is uniformly non-modal `.Show()` with unlimited instances —
  the single-instance decisions in §1/§2 should be stated as a general policy in
  the spec, not per-window ad-hockery.

---

## 15. Consolidated questions

| # | Topic | Question (short form) |
|---|---|---|
| Q1 | Layout | Include the TestWindows→product-project relocation in 0038? |
| Q2 | Placement | Launcher + constructor affordance (b), or launcher-only (a)? |
| Q3 | Windows | Single-instance Materials/Library windows, activate on re-open? |
| Q4 | Library corpus | Library window = all element presets (kind facet), Materials separate — confirm? |
| Q5 | Modality | Non-modal Select state with staleness rules, or modal Select? |
| Q6 | Selector | Drop the inline quick-pick entirely, or keep a small strip? |
| Q7 | Binding | Pre-bind new elements to their ideal library entry on add? |
| Q8 | Compounds | LPCP/CPLP stored as component list (recommended) or premultiplied matrix? |
| Q9 | Facets | Transparent/absorbing for *dispersive* materials: omit (as your text implies) or derive from k? |
| Q10 | Facets | "Primary material (substrate)" = `structure.substrate` layer's material? |
| Q11 | Facets | "Has thin films" = `films` non-empty, independent of `SubstrateKind`? |
| Q12 | Facets | Thickness facet for `Repeated` groups: per-layer (recommended), period, or total? |
| Q13 | Facets | Material facets on samples apply to *any* constituent material? |
| Q14 | Tree | Tree re-roots to follow selection order, or fixed representation + breadcrumb (my lean)? |
| Q15 | Settings | Tree threshold in `Preferences`/environment.json now; appsettings.json only with EFC? |
| Q16 | Material editor | Two-column layout with splitter (my lean) vs vertical with scroll + min-height chart? |
| Q17 | Refresh | Re-query proxies when a Select window returns (v1), live notifications later? |
| Q18 | Lifecycle | Metadata edits don't version; physics edits do — agree? |
| Q19 | Lifecycle | "Used" = referenced by persisted experiment/saved project — agree? |
| Q20 | Lifecycle | Explicit "supersede/publish" action (my lean) vs auto-version on save? |
| Q21 | Experiments | Setup captured as ordered element descriptors referencing versioned entries? |
| Q22 | Table display | "Set" = bound to a library entry (presets being complete) — confirm? |
| Q23 | Inverse data | Auto-apply dark/reference normalization, or model-includes-imperfections (raw data)? |
| Q24 | Storage | Library + experiments in DB; scene stays `.ocproj`; export bundles versions — agree? |
