# 0033-010 — Remaining gaps & additions (preliminary spec)

**Status: preliminary — feature list / mini-spec, no code yet.** Closes the gaps left open after
`006-materials-library-editors-gap-analysis.md` / `008-close-the-gaps-implementation-log.md` and the
`009` follow-up. This document is the input to the next spec-writer → arc-runner cycle; every type
named below is a *target shape*, not final. It changes no files.

The gaps this covers were deferred for two reasons (per the 008 §4 triage): some needed an operator
decision before work could start (categories, vacuum), and some are cross-cutting enough to want a
proper multi-slice cycle rather than an ad-hoc edit (ribbon hosting, dispersive tensor editing). The
decisions are now made (§10); the work below is ready to spec into slices.

---

## 1. Goal

Close the remaining Materials / Library editor gaps and land the decided additions:

- **Categories become an open, editable set** (Guid-keyed, renamable) instead of a closed
  compile-time union — with add / rename / remove and referential integrity (§4).
- **Vacuum becomes a special built-in category**, hidden from the new-material create picker (§4).
- **Workbench bays own the whole surface** (no optical table) and sit **last** in the ribbon,
  through an order-independent ribbon-hosting model (§5).
- **Dispersive gyration ρ and Polder μ become editable** (no longer view-only), reusing the §009
  formula / coefficient surface (§6).
- **Sample-editor polish**: a searchable material picker, a distinct *Make multilayer* entry point,
  and empty-sample validation (§7).
- **Material-editor polish**: present the Constant-vs-Dispersive choice as a mutually-exclusive
  control (§8).

Everything stays against **mocked, in-memory IO proxies** (the 0033 seam). **No legacy, no fallback,
no migration** — schemas are fluid and nothing is persisted, so every app start begins from the
seeded in-memory state (§3).

---

## 2. What is already closed (context, do NOT redo)

From the 007/008 cycle and the 009 follow-up, these are done and green (build + 4 test gates):

- **G3** editor window titles ("Material Editor" / "Sample Editor").
- **G6 (core)** the dead Absorbing toggle is removed under Dispersive; **G7** every dispersion model
  has editable coefficients; **G8** uniaxial / biaxial dispersive media edit one formula **per
  principal axis**; **G9** the gyration class↔anisotropy mapping is corrected (mm2 / `PlanarActive`
  under biaxial) with per-component entry; **009 units** every dimensioned coefficient shows its unit
  and the length-based defaults read in nm; **009 SumOfTerms** the raw escape hatch is fully editable.
- **G11** add-segment button width; **G12** substrate / lower half-space editing; **G13** the pure
  `AddLayer` Domain message; **G14.1** QWOT readout in nm.
- Repo-wide: **`sprintf` → interpolated strings**, and the CLAUDE.md F# conventions.

Still open — the subject of this spec: **G1, G2, G4, G5, G10, G14.3, G14.4, G14.5**, plus the
**G6 presentation** polish.

---

## 3. Binding constraints (every part)

3.1 **Elevate every primitive** (CLAUDE.md). New ids are single-case DUs with `.value` + `create`;
enumerated conditions are two-case DUs (no naked `bool`); every `*Error` case carries `reason :
string` or a typed payload; public functions have explicit concrete signatures.

3.2 **No legacy, no fallback, no migration.** Categories change from the closed multi-case union to an
open Guid-keyed set with **no compatibility shim** — nothing is serialized, so there is nothing to
migrate. The catalogue is re-seeded in memory on every start.

3.3 **All catalogue IO stays mocked and in-memory.** A new `CategoryProxy` is a `[<ReferenceEquality>]`
record of camelCase `Result`-returning functions built by `createInMemory` over a private mutable
`Map`, exactly like `MaterialProxy` / `SampleProxy` (`…Domain/MaterialLibrary.fs`, `…/ElementId.fs`).
No disk, no JSON, no schema work.

3.4 **UI is testable without a window** (WinForms-portability rule). All new editor state is a pure
edit model + a `Handlers` record; behaviour is unit-tested in Domain, structure/interaction in the
`Avalonia.Headless` gates. Stable intent-named `[<Literal>]` automation ids for every new control.

3.5 **Interpolated strings, never `sprintf`** (the new CLAUDE.md rule).

---

## 4. Categories — an open, editable, Guid-keyed set (G4 + G5)

Today `MaterialCategory = Glass | Metal | Semiconductor | Crystal | Vacuum` is a closed union
(`…Domain/MaterialLibrary.fs:67`), hardcoded in the editor picker (`MaterialEditorView.categoryRow`)
and the bay facet (`…TestWindows/TableAndElementRotationView.fs`), and `MaterialEntry.category` is that
union. Replace it with **data**.

### 4.1 The elevated id + the category record

The standard single-case-DU wrapper is the id; the category itself is a small record in a runtime
catalogue (the "open set"):

```fsharp
type CategoryId =                                         // the standard elevated-primitive wrapper
    | CategoryId of System.Guid
    member this.value = let (CategoryId g) = this in g
    static member create () : CategoryId = CategoryId (System.Guid.NewGuid())

/// Whether a category is offered in the NEW-material create picker (never a naked bool).
type CategoryVisibility = SelectableOnCreate | HiddenOnCreate     // Vacuum = HiddenOnCreate

/// Whether a category is a protected seed or user-managed (never a naked bool).
type CategoryOrigin = BuiltInCategory | UserCategory             // built-ins are not removable

/// The category as DATA: a stable Guid PK + a renamable / translatable display name.
type MaterialCategory =                                          // REPLACES the closed union
    {
        id : CategoryId
        name : string                                           // display label; mutable, translatable
        visibility : CategoryVisibility
        origin : CategoryOrigin
    }
```

`MaterialEntry.category : MaterialCategory` becomes **`category : CategoryId`** — the entry references
the PK; the name is resolved through the catalogue (so a rename is one edit, not a sweep). Likewise
`MaterialQuery.category : MaterialCategory option` becomes **`category : CategoryId option`**.

### 4.2 The seeded catalogue

Seed the current set with **fixed literal Guids** (deterministic tests): `Glass`, `Metal`,
`Semiconductor`, `Crystal` as `BuiltInCategory` / `SelectableOnCreate`; **`Vacuum` as
`BuiltInCategory` / `HiddenOnCreate`** (G5 — a real category the single seeded vacuum material
references, but never chosen for a new material). User categories added at runtime are `UserCategory` /
`SelectableOnCreate`.

### 4.3 The mutating category proxy (write-seam)

```fsharp
type CategoryError =
    | UnknownCategoryId    of reason : string
    | DuplicateCategoryId  of reason : string
    | CategoryStillReferenced of reason : string     // a Material references it (Q3-style hard block)
    | BuiltInNotRemovable  of reason : string        // origin = BuiltInCategory
    | InvalidCategory      of reason : string         // blank name

[<ReferenceEquality>]
type CategoryProxy =
    {
        listCategories   : unit -> Result<MaterialCategory list, CategoryError>
        addCategory      : MaterialCategory -> Result<unit, CategoryError>      // rejects duplicate id
        updateCategory   : MaterialCategory -> Result<unit, CategoryError>      // rename; rejects unknown id
        removeCategory   : CategoryId -> Result<unit, CategoryError>            // HARD-BLOCKS if referenced or built-in
    }
```

`removeCategory` hard-blocks (typed `CategoryStillReferenced`) while any `MaterialEntry.category` names
it — mirroring `MaterialProxy.removeMaterial`'s `MaterialStillReferenced` (Q3) — and refuses a built-in
(`BuiltInNotRemovable`); no cascade, no silent delete. Reference lookup is a `materialsReferencingCategory`
seam over the live materials store, exactly as `samplesReferencing` feeds material removal today.

### 4.4 The category-management surface (the "edit categories" ability)

A lightweight **Category editor** reached from the Materials bay (a `Categories…` verb beside
Add / Edit / Remove), following the existing editor-window pattern (`MaterialEditorWindow` /
`SampleEditorWindow` in `…TestWindows`, over a pure Domain edit model). It lists categories with:

- **Add** — mints `CategoryId.create ()`, appends a `UserCategory` / `SelectableOnCreate` row with an
  inline name box; Save calls `addCategory`.
- **Rename** — inline edit of `name`; Save calls `updateCategory` (built-ins ARE renamable — the Guid is
  the identity, so even a built-in's display label can change / translate; only removal is blocked).
- **Remove** — confirm-gated inline; surfaces `CategoryStillReferenced` (naming the referencing
  materials, offering to jump to them) or `BuiltInNotRemovable`; never cascades.

Domain-free control (`CategoryControls`, the `LibraryControls` shape) + a pure `CategoryEditor` edit
model, unit-tested without a window.

### 4.5 Where categories are consumed

- **Material editor create picker** (`MaterialEditorView.categoryRow`) offers only
  `visibility = SelectableOnCreate` categories (Vacuum removed — not greyed). The picker reads the
  catalogue, not a hardcoded list.
- **Materials bay category facet** (`…TableAndElementRotationView` `materialCategories` / `…Code`) is
  driven by the catalogue (`listCategories`); the facet option's stable code is the `CategoryId` Guid
  string, its label the category `name`. An entry filed under a `HiddenOnCreate` category is still
  filterable (the facet may list all catalogue categories that any entry uses).
- **View / metadata** resolves the category name through the catalogue by id.

**Translation note.** The mutable `name` field is what enables translation (rename = translate); a
per-locale name table wired through the existing `Localization` module is a *future* addition, not this
spec — the data model just stops blocking it.

---

## 5. Ribbon hosting — full-surface workbench bays (G1) + last-two ordering (G2)

Two ribbon-hosting changes that must land together (the second regressed last time precisely because
the first was missing).

### 5.1 Root cause to fix first (G2's earlier regression)

`Ribbon.view` (`…Controls/Ribbon.fs`) renders **every** bay's pane in one vertical `StackPanel` with
`IsVisible` toggles. Moving the Library bay to the end broke its sample-row layout in the headless
harness ("SampleRow… not found"), because pane layout is coupled to list position. **Fix the ribbon to
host only the ACTIVE pane's content** in a single stable content slot (keyed so FuncUI never recycles a
styled/named control across bays — the exact hazard the current all-panes design was avoiding). Once
pane hosting is order-independent, reordering is safe.

### 5.2 Full-surface bays (G1)

A bay declares its content mode — a two-case DU, never a bool:

```fsharp
type BayContent =
    | TableBay                       // the canvas + pan / zoom / rotate gestures (Rotation, Move, …)
    | FullSurfaceBay of content : IView   // replaces the canvas area entirely (Materials, Library)
```

`Ribbon.Bay` carries the mode (or the Main host maps bay name → mode). `mainView`
(`…TableAndElementRotationView.fs`) renders the table canvas + pointer/wheel handlers **only** for a
`TableBay`; a `FullSurfaceBay` fills the whole area below the ribbon strip with the bay's own content,
and the table gestures are not wired. WinForms-portable (the mode is data; each stack renders it its own
way).

### 5.3 Ordering (G2)

With 5.1 done, make **Materials and Library the last two** bays in `mainBays` / `BayNames.all`; retire
the 0027/026 "Details stays LAST" pin. The `ui-smoke` MainWorkbench tests must drive the reordered,
full-surface bays green.

---

## 6. Dispersive gyration ρ and Polder μ editing (G10)

Today `MaterialComplexityEditor.ofComplexity` returns `UnsupportedComplexity` for a formula-valued
`RhoWithDispValue` / `MuWithDispValue`, so such an entry opens **view-only**; the ladder edits only the
**constant** gyration (`GyrationClass<RhoValue>`) and Polder (`PolderValue<MuValue>`). Extend both rungs
to a **Constant vs Dispersive** sub-branch, mirroring the eps ladder.

- The activity rung gains a **Dispersive** sub-toggle (per the eps Constant/Dispersive pattern). When
  on, each symmetry-allowed gyration component (§G9's `gyrationComponents`) is a **`DispersionFormula`**
  edited through the **same `DispersionModels.modelParameters` + SumOfTerms coefficient surface** built
  in 009 (with units). `toComplexity` then builds `RhoWithDispValue` instead of `RhoWithoutDispValue`.
- The magnetic rung likewise: the Polder components (`muDiagonal` / `muParallel` / `gyration`) become
  `DispersionFormula`s under a Dispersive sub-toggle, building `MuWithDispValue`.
- The edit state generalises: store gyration / Polder as the constant values **plus** a dispersive
  representation (or a `'g`-generic facet the derivation reads per sub-toggle) so unchecking Dispersive
  restores the constant losslessly — the ladder's existing lossless discipline.
- `ofComplexity` seeds from a dispersive ρ / μ (the `UnsupportedComplexity` view-only fallback is
  deleted — no fallback, §3.2). The engine `toRhoWithDisp` / `toMuWithDisp` (`OpticalProperties/Active.fs`)
  are unchanged; only the editor lifts the func-value data.

Pure derivation + seed round-trip is unit-tested in Domain; the headless gate proves the sub-toggle
exposes the per-component formula boxes.

---

## 7. Sample-editor polish (G14.3 / G14.4 / G14.5)

- **G14.3 — searchable material picker.** The picker in `SampleEditorView.materialRow` is a flat,
  one-shot snapshot of `listMaterials`. Add a search box + a category facet (mirroring the Materials
  bay), filtering the resolved list; the picker reads the material set through the proxy so it reflects
  the current catalogue. Keeps the by-`MaterialId` selection contract.
- **G14.4 — distinct *Make multilayer*.** `SmpAdd` and `SmpMakeMultilayer` both open a blank editor
  today (`…TableAndElementRotationView`). Give `SmpMakeMultilayer` a launcher path that opens a **NEW**
  sample pre-seeded with a foldable starter period (a 2-layer cell ready for the K-stepper), distinct
  from the blank `Add`. Extend the editor's `init` with a "new-with-seed `SampleStructure`" path (not a
  `Some existing`, which means update-in-place) so Save still mints a fresh `SampleId`.
- **G14.5 — empty-sample validation.** A sample with **no films AND no substrate** is empty; refuse the
  save with a typed `InvalidSample` (a structure-content rule added to the store's `validateSample`,
  which today checks the name only). The two existing tests that deliberately save an empty name-only
  sample are updated to add content (they were pinning the name-only contract, which this decision
  supersedes).

---

## 8. Material-editor polish (G6 presentation)

The Constant-vs-Dispersive branch is currently a `Dispersive` sticky toggle ("off = constant n, k").
Present it as **two mutually-exclusive options** (Constant / Dispersive) so the primary branch reads as
a choice, per the operator's `the-spec` §6.1 intent. The `absorbingToggle` / `dispersiveToggle`
acceptance tests that pin the click-twice-restores toggle semantics are updated to the two-option model.
Cosmetic; behaviour (the derived model) is unchanged.

---

## 9. What this touches (when built — not in this document)

- **`…Domain/MaterialLibrary.fs`** — `CategoryId` (Guid), the `MaterialCategory` record + `CategoryVisibility`
  / `CategoryOrigin`, the seeded catalogue with fixed Guids, `CategoryProxy` + `CategoryError` +
  `createInMemory` + `materialsReferencingCategory`; re-point `MaterialEntry.category` and
  `MaterialQuery.category` to `CategoryId`; every seed / filter / lookup site.
- **`…Domain/CategoryEditor.fs`** (NEW) — the pure category edit model (add / rename / remove).
- **`…Domain/MaterialComplexityEditor.fs`** — the gyration / Polder Constant↔Dispersive sub-branches;
  `toComplexity` builds the WithDisp values; `ofComplexity` drops the `UnsupportedComplexity` fallback.
- **`…Controls/Ribbon.fs`** — single active-pane hosting; the `BayContent` mode.
- **`…Controls/CategoryControls.fs`** (NEW) — the domain-free category-manager control.
- **`…TestWindows/TableAndElementRotationView.fs`** — `mainView` full-surface vs table bay; bay
  reorder; the catalogue-driven category facet; the `Categories…` launcher; the distinct
  make-multilayer path.
- **`…TestWindows/CategoryEditorWindow.fs`** (NEW) — the category editor window.
- **`…TestWindows/MaterialEditorView.fs`** — catalogue-driven create picker; the two-option
  Constant/Dispersive control; the dispersive ρ/μ panels.
- **`…TestWindows/SampleEditorView.fs`** — searchable material picker; empty-sample guard; the seeded
  make-multilayer init path.
- **`…App/Program.fs`** — build the mock `CategoryProxy` at the composition root.
- **Tests** — category proxy add / rename / remove-hard-block round-trips; create-picker excludes
  `HiddenOnCreate`; ribbon full-surface + reorder headless proofs; dispersive ρ/μ derive + seed
  round-trip; searchable sample picker; make-multilayer seed; empty-sample rejection.

---

## 10. Decisions (from the operator, this cycle)

- **Q — categories:** editable, **open set** keyed by **Guid (PK) + renamable name** — the standard
  single-case-DU wrapper (`CategoryId`), NOT a closed multi-case union. Editing (add / rename / remove)
  is **in scope** for this spec. §4.
- **Q — vacuum:** a **special built-in category, HIDDEN from the new-material create picker**; the
  single seeded vacuum material references it. §4.2 / §4.5.
- **Q — persistence:** nothing is serialized (schemas fluid); therefore **no legacy, no fallback, no
  migration** — the DU→open-set change is clean. §3.2.
- **Q — built-ins:** renamable (Guid is identity) but **not removable**; user categories are removable
  (subject to the reference hard-block). §4.3.
- **Q — dispersive ρ/μ:** **in scope** — edit through the 009 formula/coefficient surface; the
  view-only `UnsupportedComplexity` path is removed. §6.
- **Q — empty sample:** **invalid** — refuse the save; supersede the name-only store contract. §7.

---

## 11. Proposed phasing (small slices, each independently green)

1. **Categories as data** — `CategoryId` + the record + `CategoryVisibility` / `CategoryOrigin`; seeded
   catalogue (fixed Guids; Vacuum `HiddenOnCreate`); re-point `MaterialEntry.category` /
   `MaterialQuery.category` to `CategoryId`; update every seed / filter / lookup / test. No UI. Pure
   round-trip + reference-block tests.
2. **CategoryProxy + reference integrity** — the mutating write-seam; `removeCategory` hard-block
   (`CategoryStillReferenced` / `BuiltInNotRemovable`); `materialsReferencingCategory`.
3. **Category manager UI** — `CategoryControls` + `CategoryEditor` edit model + `CategoryEditorWindow`,
   launched from the Materials bay; catalogue-driven create picker + bay facet (create picker excludes
   `HiddenOnCreate`). Headless add / rename / remove-block.
4. **Ribbon hosting** — single active-pane content slot (order-independent); `BayContent`
   full-surface mode; Materials / Library hide the table and move LAST. Headless MainWorkbench green.
5. **Dispersive ρ / μ** — the gyration / Polder Constant↔Dispersive sub-branches over the 009 formula
   surface; `ofComplexity` seeds dispersive (drop `UnsupportedComplexity`); derive + round-trip tests.
6. **Sample-editor polish** — searchable material picker; distinct seeded make-multilayer; empty-sample
   validation (update the two name-only tests).
7. **Material-editor polish** — the two-option Constant / Dispersive control (update the toggle tests).

---

## 12. References (web)

**Categories as data / referential integrity / translatable display names**
- Surrogate (Guid) primary key vs natural key — <https://en.wikipedia.org/wiki/Surrogate_key>
- Referential integrity & restrict-on-delete (no cascade) — <https://en.wikipedia.org/wiki/Referential_integrity>
- Separating a stable id from a translatable display label (i18n of user data) — W3C Internationalization —
  <https://www.w3.org/International/questions/qa-international-multilingual>

**Ribbon / tab hosting UX (single active content pane; tab ordering)**
- NN/g — Tabs, Used Right — <https://www.nngroup.com/articles/tabs-used-right/>
- Avalonia `TabControl` / `ContentControl` (single realized content region) —
  <https://docs.avaloniaui.net/docs/reference/controls/tabcontrol>
- Microsoft Fluent — ribbon / command surfaces — <https://learn.microsoft.com/en-us/windows/apps/design/controls/>

**Progressive disclosure (the Constant/Dispersive branch; dispersive ρ/μ sub-branch)**
- NN/g — Progressive Disclosure — <https://www.nngroup.com/articles/progressive-disclosure/>
- Microsoft Learn — Progressive-disclosure controls (remove, don't disable) —
  <https://learn.microsoft.com/en-us/windows/win32/uxguide/ctrl-progressive-disclosure-controls>

**Dispersive gyration / magneto-optics (G10 — reused from 0027-035 §13)**
- Optical activity tensor forms by point group (arXiv:2501.03684) — <https://arxiv.org/abs/2501.03684>
- Quartz gyration dispersion (class 32) — *Appl. Opt.* 48(28), 5307 (2009) —
  <https://opg.optica.org/ao/abstract.cfm?uri=ao-48-28-5307>
- Polder (gyromagnetic μ) tensor — <https://en.wikipedia.org/wiki/Polder_tensor>
