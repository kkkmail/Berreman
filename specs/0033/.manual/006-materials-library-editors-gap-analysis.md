# 0033-006 — Gap analysis: Materials / Library workbenches and the two editor windows

Task: `specs/0033/.manual/005-gap-analysis.txt`. Spec 0033 is implemented and all five
gates (build / unit-tests / constructor-unit-tests / ui-smoke / ui-tests) are green, yet
the delivered surfaces diverge from the operator's intent. This document identifies each
gap and traces it to its source(s): the preliminary spec
(`specs/0027/.manual/035-materials-samples-library-preliminary-spec.md`, below
**`the-spec`**), the 0033 full spec (`specs/0033/.spec-md` + `.spec-jsonl`, below
**`0033`**), the implementation, or the procedures/verifiers around them.

Style-preference items (sticky buttons vs checkboxes vs radio buttons, colors, wrap
layout) are **not** counted as gaps, per the task statement. Where a hint turned out to
be about widget *style* rather than *structure*, the structural part is analysed and the
style part is noted as out of scope.

---

## Summary table

| # | Gap | Primary source | Secondary source(s) |
|---|-----|----------------|---------------------|
| G1 | Materials / Library bays still show the optical table | **spec gap** (`the-spec` §2 + `0033` Part C model the workbenches as ribbon bays of the Main screen) | — |
| G2 | Materials / Library are not the last two ribbon tabs | **spec gap** (no order mandated) | implementation choice + a stale 0027/026 "Details stays LAST" pin |
| G3 | Window titles say "Material editor" / "Sample editor", not "… Editor" | **implementation gap** (UI copy) | spec prose uses sentence case throughout |
| G4 | Category list is hardcoded; categories cannot be added/edited/removed | **spec gap** (`the-spec` §4 fixes the closed DU) | new requirement — no spec ever asked for category maintenance |
| G5 | `Vacuum` exists as a category | **implementation gap** (0033 step 001 worker invention) | spec-model gap: 0033 forces a vacuum *material entry* with no category that fits |
| G6 | Ladder does not model `the-spec` §6.1's DU tree (Constant-vs-Dispersive is not the branch; dead Absorbing toggle under Dispersive) | **spec gap** (`0033` slice 23 frames absorbing/dispersive as sibling toggles) | `the-spec` §6 framing contradicts its own §6.1 tree; implementation violates the spec's own remove-don't-grey rule |
| G7 | No controls to enter parameters for ANY dispersion model | **spec gap** (`0033` slice 23 mandates only "bounds + a model picker") | `the-spec` §9 summary omitted it too; verifier gap (acceptance never exercises coefficients) |
| G8 | No per-axis dispersion editing for uniaxial/biaxial dispersive media | **spec gap** (slice 23 silent) | consequence of G7 |
| G9 | No gyration-component entry; gyration class count ≠ spec (PlanarActive/mm2 unreachable) | **spec gap** (`the-spec` §6.2 never assigns mm2 to an anisotropy; `0033` narrowed "orthorhombic" to "222") | implementation faithful to the narrowed text; contradicts the repo's own seed |
| G10 | Dispersive ρ (gyration) and dispersive Polder μ are view-only | **spec gap** (`0033` slice 23 covers constant ρ/μ only) | recorded honestly in code as `UnsupportedComplexity` |
| G11 | "Add segment" renders full-width and appends a default segment instead of committing the edited one | layout: **implementation gap**; semantics: **spec gap** (no interaction model specified) | semantics presuppose G7's fix |
| G12 | Sample editor cannot edit/show `SampleStructure.substrate` / `lower` | **spec gap** (slices 21/22 enumerate film ops only) | seeds carry these fields since slice 001 |
| G13 | AddLayer bypasses the pure Domain edit model | **spec-internal inconsistency** (slice 22 mandates `AddLayerButton`; slice 21's message DU has no Add arm) | procedure gap: `touches` fence blocked the Domain fix; judge accepted the deviation |
| G14 | Sample editor minor gaps (QWOT readout in metres, no inline thickness edit, unsearchable one-shot material picker, "Make multilayer" ≡ "Add", empty sample saves) | mixed (see below) | — |

---

## G1 — Materials and Library must NOT show the optical table

**Observed.** The Main screen is one fixed layout: the ribbon control bar docked on top,
the optical-table canvas filling the rest — for *every* bay
(`TableAndElementRotationView.fs:2301-2317`, `mainView`). Selecting Materials or Library
swaps only the strip inside the control bar; the table (and its pointer/wheel gestures)
stays live underneath. The workbench lists therefore render in a cramped horizontal
strip above an irrelevant table.

**Wanted.** Materials and Library are full workbench surfaces; the optical table is not
shown while they are active.

**Trace.** Spec gap, inherited from `the-spec`. §2 of `the-spec` explicitly models both
workbenches as `Ribbon.Bay` rows of the Main screen ("each of these is one `BayNames`
constant + one `mainBays` row and **no** change to `Ribbon.view`"), and `0033` Part C +
slice 24 transcribe that verbatim. Nothing anywhere says "hide the table for workbench
bays" — the operator's mental model (a bay can own the whole screen) was never captured.
The implementation is faithful to the letter. Fixing this needs a small spec addition:
a per-bay "content mode" (table vs full-surface) on the ribbon host, so a bay can
declare that it replaces the canvas area.

## G2 — Materials and Library must be the last two ribbons

**Observed.** Bay order is Rotation, Move, Add, Render, Selector, **Materials**,
**Library**, Experiments, Details (`mainBays`,
`TableAndElementRotationView.fs:2268-2277`; `BayNames.all` `:237`).

**Wanted.** …, Experiments, Details, **Materials**, **Library** (the last two).

**Trace.** Spec gap first: slice 24 says only "add `BayNames.materials` … and
`BayNames.library` … to the BayNames module and mainBays" — no position. The worker then
made a documented choice (`:235-236`): "The workbenches sit with the Selector (the
binding / collection bays); **Details stays LAST (the 0027/026 pin)**." So a secondary
source is a *procedure* issue: a pin recorded in an earlier spec (0027/026: Details is
the last bay) survived into 0033 unexamined and actively steered the placement. The
operator's directive supersedes that pin; whichever spec fixes this should explicitly
retire it.

## G3 — "Material Editor", not "Material editor"

**Observed.** `MaterialEditorWindow.fs:21-24` sets the title to
`"Material editor — <name>"` / `"Material editor — new material"`;
`SampleEditorWindow.fs:21-24` likewise `"Sample editor — …"`. (The flow itself —
Materials → Add → the Material Editor window opens — is correct, as the task confirms.)

**Wanted.** Title-case for the user-facing window titles: "Material Editor", "Sample
Editor".

**Trace.** Implementation gap, with a spec-wording contribution: both `the-spec` (§9
"Material editor window") and `0033` (Part F, slice 23 "the Material editor window")
consistently use sentence case in running prose, and the worker copied the prose casing
into the user-visible title. The automation ids (`MaterialEditorWindow`) were mandated
and are correct. One-line fixes in the two window constructors.

## G4 — Categories are hardcoded; no way to add / edit / remove them

**Observed.** `MaterialCategory` is a closed compile-time DU
(`MaterialLibrary.fs:67-72`). The editor's category picker hardcodes the five cases
(`MaterialEditorView.fs:518`), the Materials-bay facet hardcodes the same list
(`TableAndElementRotationView.fs:1913-1914`), and there is no category-maintenance
surface anywhere.

**Trace.** Spec gap originating in `the-spec`: §4 fixes
"`MaterialCategory = Glass | Metal | Semiconductor | Crystal` (`:21`) is the category
facet" — a pre-existing closed type from spec 0027, reused as-is; `0033` inherited it.
No spec has ever asked for user-manageable categories, so this is also a **new
requirement**: if categories become user data, they need the same treatment as
materials/samples — an elevated `CategoryId`, a category catalogue + proxy verbs, and a
referential-integrity rule for removing a category that entries still use (the
`MaterialStillReferenced` precedent). Note the repo's "no enum → DU" discipline is not
the obstacle — the DU would simply move from *enumerating the categories* to *naming the
category id*, with categories themselves becoming entries in a store.

## G5 — Vacuum is NOT a category

**Observed.** `Vacuum` is a fifth `MaterialCategory` case, annotated "(spec 0033 step
001) categorises the vacuum spacer entry" (`MaterialLibrary.fs:65-66`). It is offered as
a creatable category in the Material Editor (`MaterialEditorView.fs:518`) and as a
search facet in the Materials bay (`TableAndElementRotationView.fs:1913-1934`) — i.e.
the user can file arbitrary new materials under "Vacuum".

**Trace.** `.spec-jsonl` contains **no occurrence of "Vacuum"** — the spec never asked
for the category. What slice 001 *did* mandate is structural seeds whose quarter-wave
stack alternates "glass quarter-wave; **vacuum** quarter-wave" layers and a
`lower : material-id option where None = vacuum`. Since layers reference materials by
id, a vacuum *material entry* had to exist — and the closed category DU (G4) offered no
honest value for it, so the step-001 worker invented a `Vacuum` case. So: an
implementation gap (the invented category) forced by a spec-model gap (a mandated
vacuum entry with nowhere to file it). The operator's ruling — there is exactly one
vacuum, it is not a category — suggests the fix direction: keep the single seeded,
view-only vacuum entry (it already has a fixed id, `Ids.vacuum`, and
`complexity = None`), remove the `Vacuum` DU case, and give the entry a neutral
categorisation or a dedicated "built-in/singleton" marker rather than a user-selectable
category. That is a small spec decision, not a mechanical fix.

## G6 — The ladder does not model `the-spec` §6.1's DU tree

**Observed.** The editor asks Anisotropy first, then shows one flat toggle row —
Absorbing, Dispersive, Optically active, Magnetic (`MaterialEditorView.fs:546-569`).
"Absorbing" and "Dispersive" are presented as independent siblings. When Dispersive is
ON, the Absorbing toggle **stays visible and does nothing**: the `transparency` facet is
read only in the non-dispersive derivation branch (`MaterialComplexityEditor.fs:434-441`
via `toComplexity` `:511-515`), so clicking it changes no derived model, no summary, no
preview.

**Wanted (per the DU tree, `the-spec` §6.1/037).** The *first* question is the
`EpsWithDispValue` branch: **Constant** (non-dispersive) OR **Dispersive** — two
mutually exclusive choices. Under Constant: transparent/absorbing (plus the optional
active/magnetic aspects). Under Dispersive: absorption lives inside the formulas, so
only active/magnetic remain. (Whether the choices render as sticky buttons, radio
buttons or checkboxes is style, not a gap.)

**Terminology.** For the "Simple (Fixed???)" question: the established term in optics /
thin-film-software is **"constant optical constants"** — ellipsometry tools model a
non-dispersive material as "constant n & k" (see the
[J.A. Woollam optical-constants tutorial](https://www.jawoollam.com/resources/ellipsometry-tutorial/optical-constants));
the physics adjective is **"non-dispersive"**. The repo already uses both: the
dispersion facet label "Non-dispersive" and the `ConstantNK` "Constant n + ik" model.
Recommended pair of labels: **Constant** / **Dispersive** (not "Simple", not "Fixed").

**Trace.** Spec gap in `0033`: slice 23 itself frames the ladder as "the absorbing
toggle (transparent vs absorbing constant cases), the dispersive toggle
(ConstantEpsValue vs segment list)" — sibling toggles, faithfully rendered. A
contributing source is `the-spec` §6's own framing ("every richer feature is unlocked by
a checkbox/toggle"), which its later §6.1 DU tree (rev. 036/037) superseded without the
§6 prose being rewritten — the 0033 spec-writer transcribed the stale framing. There is
also an implementation-side violation: leaving the dead Absorbing toggle visible under
Dispersive contradicts the spec's *own* progressive-disclosure rule ("REMOVED, not
greyed" — `0033` Part F, `the-spec` §13), which the worker applied to the activity
toggle but not here.

## G7 — No controls to input parameters for ANY dispersion model

**Observed.** In the dispersive branch, a segment's editor is: the two
`wavelengthInterval` bounds plus a model-kind picker
(`MaterialEditorView.fs:602-645`). That is all. Every model kind carries a fixed,
hardcoded default coefficient set — BK7 Sellmeier, Rakić-1998 gold Brendel–Bormann,
Horiba a-Si Forouhi–Bloomer, etc. (`defaultModelChoices`,
`MaterialComplexityEditor.fs:179-208`) — with **no way to edit a single coefficient**.
Even `ConstantNK` (the default segment model) exposes no n/k entry in the dispersive
branch, and `SumOfTerms` (the "raw escape hatch") has no term editor. The only editable
numbers in the entire dispersive branch are the segment's interval bounds. In effect,
a user can build exactly one dispersive material: BK7-by-default under various model
names. (The magnetic Polder panel — μ diagonal/parallel/g and the axis — is the one
place with real parameter entry, as the task notes; the constant-index branch also has
its n/k boxes.)

**Trace.** Spec gap in `0033`: slice 23 mandates "Per-segment dispersion editor:
wavelengthInterval bounds plus a DispersionModel picker …" — the coefficient entry was
never written down, and the slice's acceptance ("choosing biaxial exposes three
principal-index fields, … unchecking a toggle restores the default losslessly, Save
round-trips") never touches a coefficient. The omission has a lineage: `the-spec` §9
summarises the material editor as "the per-segment dispersion editor (`DispersionModel`
picker + `WaveLengthInterval`s + the raw `SumOfTerms` escape hatch)" — also silent on
coefficient entry — even though §1's stated goal is "make dispersive materials **fully
serializable and editable**". So `the-spec` is internally inconsistent (goal vs §9
summary), `0033` codified the summary, the worker implemented the letter, and the
verifiers (gates + per-slice code judge, which judge against the slice text) had nothing
to object to. **The data model is NOT the gap** — `DispersionModel`'s coefficient
records, `DispersionFormula`, and the `…Value` trees carry everything and round-trip;
only the edit surface can't reach them. The fix is UI + edit-model messages
(per-model coefficient fields, list-valued for Sellmeier/Lorentz oscillators), no engine
work.

## G8 — No per-axis dispersion for uniaxial / biaxial dispersive media

**Observed.** `EditSegment` holds `model1/model2/model3` (one per principal axis), but
the UI renders **one** picker per segment and `ChooseSegmentModel` writes the same model
into all three slots (`MaterialComplexityEditor.fs:403-411`). Through the UI, a uniaxial
dispersive material therefore always has ordinary ≡ extraordinary (and biaxial x ≡ y ≡
z) — physically the whole point of a uniaxial dispersive medium is that they differ. The
per-axis slots exist today only so an existing per-axis `MaterialComplexity` seeds and
round-trips losslessly via `SumOfTerms`.

**Trace.** Spec gap: `the-spec` §6.1's segment records (`UniaxialEpsSegment` with
separate `ordinary`/`extraordinary`, `BiaxialEpsSegment` with x/y/z) demand per-axis
entry, but slice 23 never mentions per-axis model assignment in the editor. Largely a
consequence of G7 (with no coefficient entry, per-axis pickers alone would be
near-useless); fixing G7 should include the per-axis dimension.

## G9 — Gyration: no component entry, and the class count does not match the spec

**Observed — components.** The gyration panel offers a class picker and a handedness
switch only (`MaterialEditorView.fs:649-677`). The component magnitudes (g₁₁, g₃₃, g₁₂,
…) are pinned to the seeded default `1.5e-6` (`defaultGyrationComponent`,
`MaterialComplexityEditor.fs:163`, used for every class in `availableGyrationClasses`
`:256-267`) — no entry boxes, so e.g. quartz's g₁₁ ≈ +5.9×10⁻⁵ / g₃₃ ≈ −10.1×10⁻⁵ (the
worked example in `the-spec` §6.2/§13) cannot be entered at all.

**Observed — class count.** The engine DU has **seven** classes
(`Berreman/Dispersion.fs:501-508`: Cubic, Uniaxial, **Planar**, Orthorhombic222,
Monoclinic2, MonoclinicM, Triclinic1). The picker offers **six** across all anisotropy
choices — isotropic → {Cubic}, uniaxial → {Uniaxial}, biaxial → {222, Mono2, MonoM,
Tri1} (`availableGyrationClasses`, `MaterialComplexityEditor.fs:256-267`).
`PlanarActive` (mm2 / 4̄2m-type, single g₁₂) is **unreachable from any anisotropy**.
This is not hypothetical: the repo's own seeded *active-crystal* built-in is a uniaxial
eps **with `PlanarActive` gyration** (`MaterialLibrary.fs:232-238` — matching the
engine's own `OpticalProperties.planarCrystal`, which pairs the (n₁₁,n₁₁,n₃₃) eps with
the planar ρ, `OpticalProperties/Active.fs:106-111`). Open that entry in the editor and
the class picker shows only "Uniaxial 3 / 4 / 6" with nothing highlighted — the entry's
actual class is not on offer — and changing anisotropy or re-toggling activity silently
snaps it to `UniaxialActive` (`snapGyration`, `MaterialComplexityEditor.fs:355-360`).

**Trace.** Dual spec gap. (1) `the-spec` §6.2 declares `PlanarActive` in the DU but its
picker-constraint list — "isotropic ⇒ {23,432}; uniaxial ⇒ {3,32,4,422,6,622}; biaxial ⇒
triclinic/monoclinic/orthorhombic" — never assigns mm2/PlanarActive to any anisotropy;
(2) `0033` slice 23 then narrowed "orthorhombic" to "biaxial offers
222/monoclinic/triclinic", cementing the omission. The implementation follows the 0033
letter exactly — and thereby contradicts the seed that slice 001 of the same spec
created. The component-entry omission has the same lineage as G7 (slice 23 mandates "a
handedness switch" and nothing about component boxes). Fix: add PlanarActive to the
offered classes (per the repo's own physics, it belongs at least with uniaxial — the
seeded active crystal — and with biaxial mm2), and add per-component numeric entry.

## G10 — Dispersive gyration and dispersive Polder μ are view-only

**Observed.** An entry whose ρ or μ is formula-valued (`RhoWithDispValue …` /
`MuWithDispValue …`) opens view-only with the typed `UnsupportedComplexity` reason
(`MaterialComplexityEditor.fs:594-603`); the editor writes constant ρ/μ only.

**Trace.** Spec narrowing in `0033`: `the-spec` §6.2/§6.3 make `'g = DispersionFormula`
a first-class case of both `GyrotropicValue<'g>` and `PolderValue<'g>`, but slice 23's
editor mandate covers the constant flavors only. The limitation is honestly recorded in
code and the seeding degrades gracefully (view-only, not corrupting), so this is a
deliberate scope cut — but it is a gap vs `the-spec` and needs either a follow-up slice
or an explicit permanent-scope decision.

## G11 — "Add segment": placement and semantics

**Observed.** The button is the last child of the segments panel's *vertical*
StackPanel with no horizontal alignment set, so it stretches to the panel's full width
(`segmentsPanel`, `MaterialEditorView.fs:637-645`; `verbButton` `:417-431` sets only
`VerticalAlignment`). Semantically it **appends a fresh default segment** (`AddSegment →
segments @ [defaultSegment]`, `MaterialComplexityEditor.fs:387`) — the user cannot
"configure parameters, then add THAT segment", which is the operator's required
interaction model.

**Trace.** Layout: implementation gap — Avalonia's default stretch in a vertical
StackPanel was not constrained (`HorizontalAlignment.Left` is one attribute). Position:
the button *is* already last in the panel. Semantics: spec gap — slice 23 names
`AddSegmentButton` in the UiIds list but gives no interaction model, so the worker chose
append-then-edit; the operator wants edit-then-commit. Note the wanted model only makes
sense once G7 exists (today there are no parameters to configure before adding), so this
should be fixed together with G7 as one "segment editing" slice.

## G12 — Sample editor: substrate and lower half-space are invisible and uneditable

**Observed.** `SampleStructure` has carried `substrate : SampleLayer option` and
`lower : <material-id> option` since slice 001, and seeds use them (langasite-on-silicon
= `films [langasite]` with `lower = silicon`). But `SampleStackEditor` has **no message
touching either field** (grep over `SampleStackEditor.fs`: zero matches for
substrate/lower; the message DU `:63-82` is films-only) and the window renders films
only. Consequences: a plate-on-substrate sample can never be *created* in the UI; when
an existing one is edited, its substrate/lower are silently preserved but never shown —
the user cannot even see they exist. The "Geometry" row edits only the `SubstrateKind`
facet (ThinFilm/Plate/Wedge), which is a different thing.

**Trace.** Spec gap: slices 21 and 22 enumerate the stack-table verbs exhaustively
(select/bulk/period ops) and never mention the substrate/lower fields that slice 001 of
the same spec made first-class. `the-spec` §5 ("set thickness/geometry … for a stack use
the period builder") is too coarse to have caught it. Implementation faithful.

## G13 — AddLayer bypasses the pure edit model (spec-internal inconsistency)

**Observed.** Slice 22 mandates an `AddLayerButton`, but slice 21's `SampleStackMsg` has
no Add arm — so the view performs the structural edit inline
(`SampleEditorView.fs:391-403`, with the worker's own comment: "The step-21 message DU
has no Add arm (and Domain is outside this slice's touches), so appending a layer is a
view-level structural edit"). This violates binding constraint `0033` §0.4 ("New editor
state MUST be expressible and testable without a window") for exactly one verb.

**Trace.** Spec-internal inconsistency (slice 21 vs slice 22), compounded by two
procedure effects: the slice's `touches` fence made the correct fix (one more Domain
message arm) out-of-bounds for the step-22 worker, and the per-slice code judge —
judging against the slice letter — accepted the workaround. Mechanical fix: add
`AddLayer of SampleLayer` (or materialId + defaults) to `SampleStackMsg` and re-route
the button. (The restated-not-moved `imaginaryIndexGainWarning` rule in
`MaterialComplexityEditor.fs:329-339` is the same `touches`-fence pattern — duplication
where the spec said "move with the editors, never duplicated" — worth cleaning in the
same pass.)

## G14 — Sample editor: smaller gaps found in the sweep

1. **QWOT readout prints metres.** The derived t = λ/(4n) renders as e.g.
   `9.375e-08 m` (`SampleEditorView.fs:866-869`) beside nm-labelled entries everywhere
   else. Implementation gap (display unit); one formatting change.
2. **Thickness is not editable on a row.** A layer's thickness is a read-only cell; the
   only path is select-row → type in the toolbar → "Set thickness", while orientation
   *is* editable inline per row. The asymmetry is jarring but arguably a
   like/do-not-like item — flagged, not counted.
3. **Material picker: flat, unsearchable, resolved once.** The picker is a wrap panel of
   *every* material, snapshotted from `listMaterials` when the window opens
   (`SampleEditorView.fs:6-7, 615-631`). No search/filter, and materials added while the
   window is open never appear. Fine for nine seeds; unusable at catalogue scale. Spec
   gap (slice 22 says only "material choice by MaterialId from
   `MaterialProxy.listMaterials`").
4. **"Make multilayer" ≡ "Add".** Both verbs open the same blank editor
   (`TableAndElementRotationView.fs:853-857`); the multilayer entry point mandated by
   slice 16 (`MakeMultilayerButton`) was given no distinct behaviour by any slice (e.g.
   opening pre-seeded with a 2-layer period ready to fold). Spec gap.
5. **A completely empty sample saves.** The store validates only a non-blank name
   (`ElementId.fs:596-599`); zero films + no substrate persists fine. Mild spec gap
   (slices 4/5 specified only the name rule).

---

## Cross-cutting: how the gaps got past the pipeline

- **The condensation step is the main leak.** `the-spec`'s *data model* (§6.1/6.2/6.3
  DU trees) was transcribed into `0033` Part B faithfully — the engine `…Value` types
  are complete and correct. The *editor* mandates (slices 22/23), however, were written
  as short feature bullets ("bounds plus a picker", "a handedness switch"), and every
  editor gap above (G6-G12) lives exactly in the distance between the rich data model
  and the thin UI mandate. The data layer can express what the UI cannot edit.
- **Verifiers verify the letter.** The gates run the slice-mandated headless tests, and
  `count_at_least` protects test *count*, not intent coverage; the per-slice code judge
  reviews against the slice text. A picker-only segment editor, a components-less
  gyration panel, and a table-under-every-bay layout all satisfy their acceptance
  sentences, so everything stayed green. Gaps of the "spec never said it" kind are
  structurally invisible to this pipeline — they surface only in operator review (this
  document), which argues for an intent-level review pass against the preliminary spec,
  not just the slice text, before a spec is closed.
- **Old pins persist unexamined.** The 0027/026 "Details stays LAST" pin steered G2; the
  slice `touches` fences produced G13 and the duplicated gain-warning rule. Both are
  procedure-level sources, worth an explicit "retire/re-confirm standing pins" step when
  a new spec touches the same surface.

## Not gaps (checked and confirmed fine, or explicitly style-only)

- Materials → Add opens the Material Editor window — flow correct (title casing aside,
  G3).
- Engine-preset entries (silicon, langasite, the vacuum entry) open view-only with the
  Edit affordance removed, not greyed — matches `0033` Part B/F exactly.
- Remove is confirm-gated inline; `MaterialStillReferenced` hard-blocks and names the
  referencing samples — matches Q3.
- Sticky-button vs checkbox/radio rendering of the toggles and pickers — style
  preference, out of scope per the task statement (the *structure* issues are G6).
- Save/Cancel: one row, distinct positive/negative styling — matches §0.6.

## Suggested disposition (for operator review)

| Bucket | Gaps | Route |
|---|---|---|
| Mechanical fixes, no spec needed | G3, G11-layout, G14.1 | direct small slice / manual fix list |
| Needs small spec text + implementation | G1, G2 (retire the 0027 pin), G13, G14.4, G14.5 | follow-up spec, one slice each |
| Needs real spec work (new UI + edit-model messages) | G6, G7, G8, G9, G11-semantics, G12, G14.3 | a "Material/Sample editor completion" spec — the largest chunk; G7/G8/G9/G11 are one coherent segment-editor slice |
| Needs a design decision first | G4 (categories as data?), G5 (vacuum as singleton), G10 (dispersive ρ/μ scope) | operator decision, then spec |

Source for the terminology check in G6:
[J.A. Woollam — Ellipsometry tutorial: Optical Constants](https://www.jawoollam.com/resources/ellipsometry-tutorial/optical-constants).
