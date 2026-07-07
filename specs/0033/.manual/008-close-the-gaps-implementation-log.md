# 0033-008 — Implementation log: closing the Materials/Library editor gaps

Task: `specs/0033/.manual/007-close-the-gaps-task.txt`. Closes the gaps identified in
`006-materials-library-editors-gap-analysis.md`, with special attention to **G9** (the
anisotropy → gyration-class mapping). All work keeps the five gates green.

---

## 1. Gap-percentage analysis (spec vs non-spec)

The gap analysis (006) enumerated **14 top-level gaps (G1–G14)**. Classified by *primary*
source:

| Source | Gaps | Count | Share |
|---|---|---|---|
| **Spec gap** (spec omitted / mis-modelled the requirement) | G1, G2, G4, G6, G7, G8, G9, G10, G12 | 9 | 64% |
| **Spec-internal inconsistency** (spec contradicted itself) | G13 | 1 | 7% |
| **Implementation gap** (spec was right; code diverged) | G3, G5 | 2 | 14% |
| **Mixed** (spec + implementation) | G11 (semantics=spec, layout=impl), G14 (sub-items split) | 2 | 14% |

**Spec-rooted gaps are 10 of 14 = ~71%** (the 9 spec gaps plus the spec-internal
inconsistency G13, which is a spec-side defect). Implementation-only gaps are **2 of 14 =
~14%** (G3 title casing, G5 the invented `Vacuum` category). The two **mixed** gaps are
themselves majority-spec (G11's interaction model and G14's three real sub-gaps —
searchable picker, distinct make-multilayer, empty-sample validation — are all spec
omissions; only G11's button width and G14.1's unit are implementation). Counting those
spec halves in, the spec is the dominant source of **~80–85%** of the gap surface.

**Headline: roughly 70% of the gaps (10/14) are spec gaps; ~14% are pure implementation
gaps; the rest are mixed but majority-spec.** The condensation of the preliminary spec's
rich data model into thin per-slice editor mandates is where most of the loss happened —
the engine/data layer was faithful, the editor mandates were under-specified.

---

## 2. G9 — double-checked mapping (the priority)

**The correct mapping.** The optical-activity gyration tensor gᵢⱼ is a symmetric second-rank
*axial* tensor; its independent components are fixed by the crystal point group, and the
optical anisotropy the editor tracks (isotropic / uniaxial / biaxial) is a read-off of the
crystal *system*. The physically-correct constraint is therefore:

- **Isotropic ⇐ cubic** (23, 432) → `CubicActive`, gyration `diag(g, g, g)`.
- **Uniaxial ⇐ tetragonal / trigonal / hexagonal** (3, 32, 4, 422, 6, 622) → `UniaxialActive`,
  `diag(g₁₁, g₁₁, g₃₃)`.
- **Biaxial ⇐ orthorhombic / monoclinic / triclinic**:
  - 222 (orthorhombic) → `Orthorhombic222`
  - **mm2 (orthorhombic) → `PlanarActive`** ← the previously-unreachable class
  - 2 (monoclinic) → `Monoclinic2`
  - m (monoclinic) → `MonoclinicM`
  - 1 (triclinic) → `Triclinic1`

**The bug.** `PlanarActive` (mm2) was offered by *no* anisotropy, so it was unreachable —
even though the seeded "Active (gyrotropic) crystal" built-in uses it. That built-in stores
its eps as `BiaxialTransparent (n₁₁, n₁₁, n₃₃)`, so `ofComplexity` classifies it as
**biaxial**; mm2 is orthorhombic → optically biaxial → it belongs on the biaxial list. Adding
it there is both crystallographically correct *and* makes that built-in round-trip through
the editor (previously it opened with nothing highlighted and snapped to another class on any
interaction). This also confirms the earlier 006 note (which speculated PlanarActive might
belong under uniaxial): it is a **biaxial-only** class.

**Documentation + references added to the code** (`MaterialComplexityEditor.availableGyrationClasses`):
the full system→anisotropy→class table as a doc comment, with citations —
- J. F. Nye, *Physical Properties of Crystals* (Oxford, 1985), Ch. XIV;
- "Optical activity of time-invariant crystals", arXiv:2501.03684 (<https://arxiv.org/abs/2501.03684>);
- International Tables for Crystallography, Vol. A, §3.2;
- Quartz gyration (class 32), *Appl. Opt.* 48(28), 5307 (2009)
  (<https://opg.optica.org/ao/abstract.cfm?uri=ao-48-28-5307>).

**Component entry (the other half of G9).** The panel previously showed only a class picker
and a handedness switch; every component magnitude was pinned to the seed default `1.5e-6`,
so quartz's measured g₁₁/g₃₃ could not be entered. Added a `GyrationComponent` key DU,
`gyrationComponents` (lists exactly the symmetry-allowed components of the current class),
`setGyrationComponent` (updates one component; a component the class forbids is a no-op), and
a `SetGyrationComponent` message — wired to per-component numeric boxes in the gyration panel.

---

## 3. What was closed (implemented, gates green)

| Gap | Change | Files |
|---|---|---|
| **G9** | mm2/`PlanarActive` → biaxial (correct mapping + references); per-component `g₍ᵢⱼ₎` entry boxes | `MaterialComplexityEditor.fs`, `MaterialEditorView.fs`, test updated + 2 new tests |
| **G7** | Coefficient entry for *every* analytic model via a data-driven `modelParameters` descriptor + a `SetSegmentModel` message that stores edited coefficients verbatim; per-model boxes rendered under each segment's kind picker | `DispersionModels.fs`, `MaterialComplexityEditor.fs`, `MaterialEditorView.fs`, 2 new tests |
| **G6** | Absorbing toggle is REMOVED (not greyed) in the dispersive branch; the dispersion choice leads the toggle row ("off = constant n, k") so Constant-vs-Dispersive reads as the primary branch | `MaterialEditorView.fs` |
| **G3** | Window titles → "Material Editor" / "Sample Editor" | `MaterialEditorWindow.fs`, `SampleEditorWindow.fs` |
| **G11** (layout) | Add-segment verb left-aligned (no longer spans the panel width) | `MaterialEditorView.fs` (`verbButton`) |
| **G12** | Substrate plate and lower half-space are now shown and editable (Set-from-chosen / Clear); new `SetSubstrate` / `SetLower` Domain messages | `SampleStackEditor.fs`, `SampleEditorView.fs` |
| **G13** | Add-layer routes through a new pure Domain `AddLayer` message instead of a view-level structural edit (restores window-free testability, binding constraint §0.4) | `SampleStackEditor.fs`, `SampleEditorView.fs` |
| **G14.1** | QWOT derived thickness renders in nm (not raw metres) | `SampleEditorView.fs` |

Every new domain behaviour is a pure `Result`-returning transform, testable without a window
(CLAUDE.md rule). Ids are elevated: `GyrationComponent` is a DU (not a string), the
coefficient descriptor `ModelParameter` is a transient record of functions (never stored),
and the new messages carry typed payloads.

---

## 4. What remains (deferred, with reasons)

These need a design decision or a larger slice than a bug-fix pass, so they are **not** closed
here — closing them by guessing would either break the acceptance contract or pre-empt the
operator's call (the operator explicitly said he would review 006 and re-look at the screens).

- **G2 — Materials & Library as the last two ribbon bays.** *Implemented, then reverted.*
  Reordering the bays (and updating the two order-pinning tests) built and passed the pure
  tests, but broke two `MainWorkbenchTests` ui-smoke tests: with the Library bay moved to the
  end, its **sample rows are no longer effectively visible** (`SampleRow_… was not found`), so
  clicking a sample row fails. Bisected to the reorder itself (restoring the original order made
  the test pass). The `Ribbon` renders *all* bays' panes in one vertical stack and toggles
  `IsVisible`; the breakage is tied to the pane list order, so this is a real ribbon-hosting
  interaction (it would affect the actual app, not just the harness), not a pure-cosmetic move.
  Making it order-independent is a `OpticalConstructor.Controls/Ribbon.fs` change (host only the
  active pane, or key panes stably) — deferred so the reorder does not ship a regression. The
  original order stands.
- **G1 — hide the optical table for the workbench bays.** Requires a ribbon-host "content
  mode" (table area vs full-surface) so a bay can replace the canvas. Architecturally
  significant (touches `mainView` and the pointer/wheel gesture wiring); a design decision on
  how the shell hosts full-surface bays. (Related to G2 — both are ribbon-hosting changes.)
- **G4 — categories as user data.** A new subsystem: an elevated `CategoryId`, a category
  catalogue + proxy verbs (add/edit/remove), and a referential-integrity rule for removing a
  category still in use (the `MaterialStillReferenced` precedent). No spec has ever scoped
  this; it is a genuine new requirement.
- **G5 — `Vacuum` is not a category.** The single seeded vacuum *entry* must stay (structural
  seeds reference it by id), but the `Vacuum` DU case should go. How to file the singleton
  (a neutral categorisation vs a "built-in/singleton" marker) is the operator's call, and the
  change ripples through the closed `MaterialCategory` DU + its seeds/tests.
- **G8 — per-axis dispersion.** `SetSegmentModel` currently writes the same model to all three
  principal axes (the isotropic case is fully closed). Distinct ordinary/extraordinary (or
  x/y/z) dispersion needs per-axis pickers + coefficient boxes; the data model and per-axis
  storage (`model1/2/3`) already support it, only the per-axis UI is missing. Best done as a
  follow-up that also settles the layout.
- **G10 — dispersive ρ / μ editing.** The edit model covers constant gyration/Polder only;
  a formula-valued ρ/μ opens view-only (`UnsupportedComplexity`). Adding dispersive-tensor
  editing is a sizeable feature (per-component `DispersionFormula` entry) — a scope decision.
- **G14.3 — searchable material picker (sample editor).** The picker is a flat, one-shot
  snapshot of every material. Fine at nine seeds; a search/refresh is a small enhancement,
  deferred with the other sample-editor polish.
- **G14.4 — make-multilayer distinct from Add.** Both open a blank editor. Making
  "Make multilayer" open pre-seeded with a foldable period needs a launcher-signature change
  (a seed structure for a NEW sample) that ripples into the composition root; deferred as
  minor.
- **G14.5 — reject an empty sample on save.** Attempted as an editor guard, then **reverted**:
  two existing tests deliberately save an empty (name-only) sample and rely on the store's
  name-only validation, so requiring content is a store-contract change the operator should
  decide, not a safe mechanical fix.
- **G14.2 — inline per-row thickness edit.** A like/do-not-like item (the analysis flagged it
  as not-a-gap), so not counted.
- **G6 (presentation).** The concrete dead-toggle bug is fixed and the branch now reads
  Constant-vs-Dispersive; rendering it as two mutually-exclusive radio buttons (rather than a
  labelled toggle) is a *style* choice, and the existing acceptance tests pin the toggle's
  click-twice-restores semantics, so a radio rewrite is deferred to avoid rewriting the
  acceptance.

---

## 5. A render-loop bug found (and fixed) during verification

The first G7 implementation committed each coefficient box on `TextChanged`. In the dispersive
branch that **hung** the `MaterialEditorWindowTests.the segment editor … non-lowerable pick`
ui-smoke test: FuncUI's programmatic re-render echoes each box's text back through
`TextChanged`, which re-dispatched a whole-model rebuild → new model → re-render → echo → an
infinite loop (a `--blame-hang-timeout` dump pinned the exact test). `%g`-vs-round-trip
formatting and a structural short-circuit both failed to break it. The fix: the coefficient and
gyration-component boxes (`coeffNumberBox`) commit on **`LostFocus`**, which never fires during
render, so the echo cannot form; values are shown as the shortest round-trippable string so no
precision is lost. The plain `numberBox` (index / μ boxes) is unchanged. A view-level
`EditorMsg` short-circuit (return the same model when the edit is structurally a no-op) was kept
as defence in depth. This is why the gate is validated per-class below — a hang is invisible to
a summary line.

## 6. Verification

All five gates pass (run from `Berreman/`):

- **build** — `dotnet build Berreman.slnx -c Release` — **succeeded** (no errors; the core
  `--warnaserror+:25` projects compile clean).
- **unit-tests** (`BerremanTests`) — **119 passed, 5 skipped**.
- **constructor-unit-tests** (`OpticalConstructor.Tests`) — **416 passed**.
- **ui-tests** (`OpticalConstructor.Ui.Tests`, `Category!=ui-smoke`) — **310 passed**,
  including the 4 new pure tests for G9 (component set / mm2 round-trip) and G7 (coefficient
  set / `modelParameters`).
- **ui-smoke** (`Category=ui-smoke`) — **83 passed** (every editor / panel view renders a
  frame; the material & sample editor, workbench, composition and controls smoke classes were
  additionally validated in isolation with hang detection).

Test changes: the G9 biaxial-list test updated to include `Planar`; the QWOT test updated to
expect nanometres (G14.1); four new pure tests added. No test count regressed. The G2 test
edits were fully reverted along with the G2 source change.

---

## 6. Notes for the operator

- G9's mapping is now crystallographically defensible and documented in the code; the seeded
  active-crystal built-in opens and round-trips instead of snapping.
- G7 gives every analytic model editable coefficients; the transcendental models
  (Tauc–Lorentz / Gaussian / Forouhi–Bloomer / Brendel–Bormann) still cannot *lower* into a
  dispersive segment (that is a pre-existing engine limitation — they carry no finite term
  data — surfaced as the typed `NotAFiniteTermSum` reason), but their coefficients are now
  editable.
- The deferred items in §4 (G1/G4/G5/G10 especially) are the ones you said you would review;
  each is flagged with the specific decision it waits on.
