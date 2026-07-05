# 0027-033 — Materials & Samples: the "Materials" ribbon tab, a progressive material editor, and a dual-axis dispersion chart (preliminary spec)

**Status: preliminary — feature list / mini-spec, no code yet.** Follows the shape of `024` (this bay is the successor to `024`'s **Library** bay). §11 records decisions and open questions; §12 the phasing. Grounded on real engine/constructor/chart types — every type named below already exists at the cited path unless flagged **NEW**. No files are changed by this document.

---

## 1. Goal

Turn the read-only **Library** bay of `024`/`026` into a full **Materials** workbench: the user *maintains* (add / remove / edit / view / search) both **Materials** (infinite/bulk optical media) and **Samples** (cut-out plates and multilayer stacks), through two dedicated editor windows, still against **mocked IO proxies** — but proxies that now **write** into an internal list so every verb is unit-testable. It also gives dispersive materials a proper **n / k vs wavelength** chart with the same formatting UX as the experiment chart, by extracting the shared chart pieces into the already-shared `OpticalConstructor.Controls` project.

The two nouns of `024` §2a are unchanged and remain the spine:

- **Material** — infinite/bulk medium (glass, quartz, silicon, langasite …): the existing `MaterialLibrary.MaterialEntry` (`OpticalConstructor.Domain/MaterialLibrary.fs:31`), which pairs display metadata with the engine's `OpticalPropertiesWithDisp` (`Berreman/Dispersion.fs`).
- **Sample** — a cut-out plate / stack of one-or-more materials at a thickness/geometry: the existing `Library.Sample` (`OpticalConstructor.Domain/ElementId.fs:43`), which maps to engine `Layer` / `OpticalSystem` (`Berreman/Media.fs:24,94`).

**Scope note — homogeneous media only.** Every material edited here is a **single, spatially-homogeneous** bulk medium: the `eps`/`mu`/`rho` edit-model of §6 is strictly **per-layer-uniform**. Spatially-inhomogeneous / graded-index media (a continuous n(z) profile within one layer) are **out of scope**; a stack is composed only of homogeneous `Layer`s (§7), never a graded slab.

---

## 2. The **Materials** ribbon bay (supersedes **Library**)

The Main screen is a generic `Ribbon` of `Ribbon.Bay` records (`OpticalConstructor.Controls/Ribbon.fs`; `Bay = { name : string; content : IView }`), so adding/renaming a bay is one constant + one `mainBays` entry and **no** change to `Ribbon.view`.

- Rename the `BayNames.library = "Library"` binding (`OpticalConstructor.TestWindows/TableAndElementRotationView.fs:150`, in the `BayNames` module at `:144`) → **`BayNames.materials = "Materials"`** (keep it in `BayNames.all`, `:156`); the current `mainBays` row (`mainBays` opens at `:1577`, the Library row at `:1582`) `LibraryControls.view (libraryState model) (libraryHandlers dispatch)` becomes `MaterialsControls.view …`.
- The bay keeps the `024`/`026` **confirm-gated bind** flow for *choosing* an entry for the selected table element (`LibraryControls`'s `selectEntry`/`confirmEntry`/`cancelEntry`, `pendingDescription` readout) — a Sample must still be *in the store* to be bound to a `valueId`. On top of that it adds the **maintain** verbs and the two editor-window launchers.
- **`MaterialsControls`** (**NEW**, copy the domain-free `LibraryControls.fs` shape: pure `Row`/`State`, a `Handlers` record of `unit`-returning fns, `[<RequireQualifiedAccess>] module UiIds` of stable `[<Literal>]` automation ids). Two tabs/lists inside the bay — **Materials** and **Samples** — each with a search box and `Add… / Edit… / Remove / View`. The host (`TableAndElementRotationView`) flattens the proxy results into `Row`s (as `libraryState`/`flattenNode` already do) and maps `Handlers` back to `Msg`s, opening the relevant editor window.

**Automation contract:** `MaterialsControls.UiIds` gets stable, intent-named ids (`AddMaterialButton`, `EditMaterialButton`, `RemoveMaterialButton`, `MaterialSearchBox`, `AddSampleButton`, `MakeMultilayerButton`, …) so the headless `ui-smoke` gate drives the bay by meaning, not layout.

---

## 3. IO proxies — the **locked mock write-seam** (mutating, unit-testable)

`024` §3 proposed a read-only `MaterialProxy = { listMaterials; tryGetMaterial }` but it was never implemented; today bulk materials live only as the static value `MaterialLibrary.standard = { entries = builtInEntries }` (`MaterialLibrary.fs:140`), and the shipped `Library.LibraryProxy` (`ElementId.fs:178`) exposes **only reads** (`entriesForKind` / `libraryTrees` / `tryGetEntry`). Maintaining materials/samples needs a **write** seam.

Follow the established functional-proxy convention exactly (a `[<ReferenceEquality>]` record of camelCase `Result`-returning functions, built by `createInMemory`, that a test replaces with a stub of the same shape). The mock is **locked** (in-process) and **stateful**: `createInMemory` closes over a private mutable cell (`ref (… list)`), and `add/remove/update` mutate it — mutation confined to the proxy's IO boundary, the one place CLAUDE.md permits it, so add-then-list / edit-then-get / remove-then-search are fully unit-testable without disk.

### 3.1 The id-elevation seam (decided)

`MaterialEntry.id` (`MaterialLibrary.fs:33`), `Sample.id` and `Sample.materialId` (`ElementId.fs:45,47`) are all **raw `string`** today. There are two ways to reconcile that with "elevate every primitive," and this spec **decides** between them so the proxy signatures and the stored records agree:

- **Now (mock phase):** `MaterialId` / `SampleId` are **elevated lookup-key wrappers** — the *external contract* of the proxy surface and the editor windows. The stored records keep their **raw-string** `id` / `materialId` fields; the proxy **wraps at its boundary** (returns `MaterialId` by lifting the stored string; `tryGetMaterial (MaterialId s)` matches on `s`). The raw string lives only at the proxy/storage seam — exactly where CLAUDE.md permits a primitive — never in a new public signature.
- **Later (Storage):** elevating the record fields themselves (`MaterialEntry.id : MaterialId`, `Sample.id : SampleId`, `Sample.materialId : MaterialId`) is a **breaking type + JSON-schema change** requiring a serialization codec that maps each DU to/from its wire string. It is deferred to when the disk-backed `create` lands and is tracked as **Q8**.

This keeps the mock honest: the elevated key is the contract, the raw field is the (soon-to-be-migrated) storage value, and the two never disagree because the proxy is the sole wrap/unwrap point.

```fsharp
/// Elevated lookup keys (the proxy/editor contract; the stored `MaterialEntry.id`/`Sample.id`
/// remain raw `string` in the mock — see §3.1, Q8). Mirror Library.ElementId (ElementId.fs:22).
type MaterialId =                                    // NEW
    | MaterialId of string
    member this.value = let (MaterialId s) = this in s
    static member tryCreate : string -> Result<MaterialId, MaterialError>

type SampleId =                                      // NEW
    | SampleId of string
    member this.value = let (SampleId s) = this in s
    static member tryCreate : string -> Result<SampleId, MaterialError>

/// Elevated search query — not a bare string. The dispersion facet is a DU, not a naked bool
/// (no-naked-bool rule; resolves the draft's Q4).
type DispersionFilter =                              // NEW
    | AnyDispersion
    | OnlyDispersive
    | OnlyNonDispersive

type MaterialQuery =                                 // NEW
    { text : string; category : MaterialCategory option; dispersion : DispersionFilter }

/// Labelled error. Relabelling MaterialLibrary.MaterialError's `UnknownMaterialId of string`
/// (MaterialLibrary.fs:42-43) to `of reason : string` is SAFE: F# positional case construction is
/// unaffected by adding a field label, so the existing `resolveMaterial` call site
/// `Error (UnknownMaterialId id)` (MaterialLibrary.fs:72) keeps compiling unchanged.
type MaterialError =                                 // extend MaterialLibrary.fs:42
    | UnknownMaterialId of reason : string
    | DuplicateMaterialId of reason : string
    | InvalidMaterial     of reason : string
    | MaterialUnavailable of reason : string

[<ReferenceEquality>]
type MaterialProxy =                                 // NEW — the write-seam MaterialsControls calls
    {
        listMaterials   : unit          -> Result<MaterialEntry list, MaterialError>
        searchMaterials : MaterialQuery -> Result<MaterialEntry list, MaterialError>   // reuse byCategory/byNameContains (MaterialLibrary.fs:53,57)
        tryGetMaterial  : MaterialId    -> Result<MaterialEntry option, MaterialError>
        addMaterial     : MaterialEntry -> Result<MaterialId, MaterialError>            // rejects duplicate id
        updateMaterial  : MaterialEntry -> Result<unit, MaterialError>                  // rejects unknown id
        removeMaterial  : MaterialId    -> Result<unit, MaterialError>                  // rejects id still referenced by a Sample (see Q3)
    }

[<ReferenceEquality>]
type SampleProxy =                                   // NEW — same shape over Library.Sample (ElementId.fs:43)
    {
        listSamples   : unit       -> Result<Sample list, SampleError>
        searchSamples : SampleQuery -> Result<Sample list, SampleError>
        tryGetSample  : SampleId    -> Result<Sample option, SampleError>
        addSample     : Sample      -> Result<SampleId, SampleError>
        updateSample  : Sample      -> Result<unit, SampleError>
        removeSample  : SampleId    -> Result<unit, SampleError>
    }
```

- `createInMemory ()` seeds the cell from `MaterialLibrary.builtInEntries` (`MaterialLibrary.fs:79`) / `Library.seedEntries` (`ElementId.fs:189`) and returns closures that read/mutate it — deterministic, no IO.
- **Later** (`OpticalConstructor.Storage`): a real disk-backed `create` (JSON via the existing `MaterialImport.fs` seam, `OpticalConstructor.Storage/MaterialImport.fs:26`); the bay and editors are unchanged.
- The read-only `LibraryProxy` (element binding) and the new `MaterialProxy`/`SampleProxy` (maintenance) coexist: binding still reads through `LibraryProxy`; the Materials bay maintains through the write proxies. A `MaterialsContext` bundles the proxies + current selection so the views stay pure.

---

## 4. Maintain **Materials** — add / remove / edit / view / search

- **List / search** — `searchMaterials (MaterialQuery …)` over the seeded `MaterialEntry` list, reusing the existing pure `MaterialLibrary.byCategory`/`byNameContains` linear filters (`MaterialLibrary.fs:53,57`); `MaterialCategory = Glass | Metal | Semiconductor | Crystal` (`MaterialLibrary.fs:21`) is the category facet, `DispersionFilter` the dispersion facet.
- **View** — read-only render of an entry's metadata (id/name/category/description) plus the **dual-axis n/k dispersion chart** (§8); dispersive entries show the curve, non-dispersive ones show flat lines.
- **Add / Edit** — open the **Material editor window** (§9) on a blank or loaded `MaterialComplexity` (§6); Save calls `addMaterial` / `updateMaterial`.
- **Remove** — `removeMaterial (MaterialId …)`; refuses (typed `MaterialError`) if a Sample still references the id (referential integrity across the two stores — Q3).

---

## 5. Maintain **Samples** — add / remove / edit / view / search

- **List / search** — `searchSamples` over `Library.Sample` (`ElementId.fs:43`: `{ id; name; materialId; thickness : Thickness; substrate : SubstrateKind; description }`), where `SubstrateKind = ThinFilm | Plate | Wedge` (`ElementId.fs:35`) is the geometry facet mapped by `Propagation.sampleToSystem` (`OpticalConstructor.Domain/Propagation.fs:139`) to films-vs-substrate placement.
- **View** — metadata + the resolved geometry; for a multilayer, the repeating-unit prose already carried in `Sample.description` (`ElementId.fs:52`) plus the layer-band view (`LayerBandsControls`, already used by the Details bay).
- **Add / Edit** — open the **Sample editor window** (§9): pick material(s) by `MaterialId`, set thickness/geometry, and — for a stack — use the **multilayer period builder** (§7). Save calls `addSample` / `updateSample`.
- **Note — the single-material `Library.Sample` cannot express an arbitrary stack today.** `sampleToSystem` recognises a multilayer only by matching a hard-coded `sample.id` (`"sample-multilayer-qw"`, `"sample-euv-mosi"`, `Propagation.fs:140-141`) and buries the repeat counts (20, 100) as literals. §7 introduces the editable structure this needs.

---

## 6. **Material complexity** — the progressive-unlocking model

The simplest material is **transparent, isotropic, non-dispersive** (one real index n, `k = 0`, no anisotropy, no activity, no magnetism) — an idealisation that is convenient at a fixed wavelength. Every richer feature is **unlocked** by a checkbox/toggle (progressive disclosure — NN/g and Microsoft guidance in §13), and each unlock lifts **exactly one field** of the engine record `OpticalPropertiesWithDisp = { epsWithDisp; muWithDisp; rhoWithDisp }` (`Berreman/Dispersion.fs`) off its vacuum/scalar default. Unchecking restores the default losslessly. This is *net-new UI + a small domain edit-model over already-existing physics types* — not a solver change. Consistent with §1's scope note, the whole edit-model describes **one homogeneous medium**; there is no per-layer graded profile.

The edit model is one elevated record; a pure `toProperties : MaterialComplexity -> OpticalPropertiesWithDisp` maps it to the engine (unit-tested in `OpticalConstructor.Tests`, no window):

```fsharp
type MaterialComplexity =                 // NEW (OpticalConstructor.Domain)
    {
        eps       : EpsComplexity         // anisotropy × dispersion × absorption  → epsWithDisp
        activity  : ActivityState         // optical activity (gyration)           → rhoWithDisp
        magnetism : MagneticState         // magnetic permeability                 → muWithDisp
    }
```

### 6.1 Absorbing & Dispersive → `epsWithDisp` (reuse `DispersionModels`)

The index of each principal axis is one elevated `IndexSpec`. **Absorbing** simply unlocks the imaginary part `k`; **Dispersive** swaps the flat index for one-or-more range-scoped formulas.

```fsharp
type WaveLengthInterval =                 // NEW — a CLOSED validity band. Distinct from Analytics.Variables.Range<WaveLength>
    { lower : WaveLength; upper : WaveLength }   // (Range carries numberOfPoints = a SAMPLING grid; validity is not that)

type DispersionSegment =                  // NEW — RII stores several range-scoped records per material (see §13)
    { validity : WaveLengthInterval
      model    : DispersionModels.DispersionModel }   // DispersionModels.fs:120

type IndexSpec =                          // NEW
    | ConstantIndex   of DispersionModels.ConstantNKCoefficients   // ConstantNKCoefficients {n; k; …}  — k=0 ⇒ transparent
    | DispersiveIndex of DispersionSegment list                    // ordered segments; evaluation selects the covering one

type EpsComplexity =                      // NEW — this DU IS the anisotropy encoding (no parallel toggle type)
    | Isotropic    of IndexSpec
    | UniaxialEps  of ordinary : IndexSpec * extraordinary : IndexSpec
    | BiaxialEps   of x : IndexSpec * y : IndexSpec * z : IndexSpec
```

**The anisotropy toggle is editor state, not a second domain type.** The draft's `AnisotropyKind = Uniaxial | Biaxial` was a redundant parallel encoding of what `EpsComplexity` already says. Drop it from the domain model. The isotropic / uniaxial / biaxial selector is a **3-way editor-MVU toggle** (`AnisotropySelection = SelectIsotropic | SelectUniaxial | SelectBiaxial`) that simply **chooses which `EpsComplexity` case `toProperties` constructs** — it lives in the editor's message DU, never in `MaterialComplexity`, and it is also what constrains the gyration-class picker in §6.2 (keyed off the selected `EpsComplexity` case shape).

Mapping to the engine reuses the shipped composers, but **not** all "verbatim" — a multi-segment axis has no single-`DispersionModel` representation:
- `ConstantIndex` → `DispersionModels.ConstantNK` (`DispersionModels.fs:127`), emitted as `EpsWithoutDisp` (zero closure overhead) by `toOpticalProperties` (`:228-232`).
- `DispersiveIndex segments` → a `WaveLength -> ComplexRefractionIndex` closure that **selects the segment whose `validity` covers λ**, then calls `DispersionModels.evaluate` (`:206`) on that segment's `DispersionModel`; wrap via `toOpticalProperties`. Define explicit behaviour for gaps/overlap/out-of-range: **clamp to the nearest segment** (mirroring `MaterialImport.buildTabulatedClosure`'s total, end-clamped closure, `MaterialImport.fs:57`) and/or surface a typed validation note; the closure stays total over the chart range (Q5).
- **Anisotropic, single `DispersionModel` per axis** (every axis is a `ConstantIndex` or a *single-segment* `DispersiveIndex`): build a `DispersionModels.AnisotropicModel` (`Uniaxial of ordinary*extraordinary | Biaxial of x*y*z`, `:242-244`) and delegate to `toAnisotropicOpticalProperties` (`:259`).
- **Anisotropic, multi-segment on any axis:** `AnisotropicModel` carries only **one** `DispersionModel` per axis, so it **cannot** express a per-axis `DispersionSegment` **list** — `toAnisotropicOpticalProperties` is therefore **not** reusable here. Instead `toProperties` builds a **per-axis segment-selecting closure** `WaveLength -> ComplexRefractionIndex` (pick the covering segment, then `evaluate`, `:206`) for each principal axis and combines them into a single `EpsWithDisp` closure through the public `uniaxialEps` (`:249`) / `biaxialEps` (`:254`) constructors. Only the single-`DispersionModel`-per-axis case delegates to `toAnisotropicOpticalProperties`.

The formula picker is driven off the `DispersionModel` DU (`Sellmeier | Cauchy | Lorentz | Drude | TaucLorentz | GaussianOscillator | ConstantNK`, `DispersionModels.fs:120-127`), each option showing only its own coefficient record + `wavelengthUnit`. Editor default = **Sellmeier** for transparent glass/crystal, **Cauchy** for visible dielectric films, the oscillator/Drude family for absorbing/metal bands, **Constant** and **Tabulated** as universal fallbacks (§13). Two families the task's domain wants are **not yet in `DispersionModels.fs`** and are flagged **NEW DU cases**: **Forouhi–Bloomer** (amorphous-dielectric/semiconductor UV edge; primary reference Forouhi & Bloomer 1986/1988, §13) and **Brendel–Bormann** (metal interband, Rakić 1998, §13).

### 6.2 Optically active → `rhoWithDisp` (gyration, symmetry-driven)

Optical activity is a rank-2 **axial** gyration tensor `g_ij`; only its **symmetric** part rotates polarization, it is **zero for all centrosymmetric classes**, and of the 15 optically-active point groups the editor never needs more than **6 independent components — usually 1 or 2** (research §13, arXiv 2501.03684, Nye). **Never expose a free 3×3.** Drive it from a symmetry-class DU whose *each case carries exactly its allowed symmetric components*, so "only a handful are non-zero" is a compile-time guarantee, not UI validation:

```fsharp
type GyrationComponent = GyrationComponent of double         // NEW — elevate g_ij (maps to RhoValue, MaterialProperties.fs:127)
type Handedness = LeftHanded | RightHanded                    // enantiomorph = one overall sign flip (g → −g)

type GyrationClass =                                          // NEW — offer ONLY the 15 rotation-producing classes
    | CubicActive        of g   : GyrationComponent                                   // 23, 432  (isotropic g)
    | UniaxialActive     of g11 : GyrationComponent * g33 : GyrationComponent         // 3, 32, 4, 422, 6, 622 — diag(g11,g11,g33)
    | TetragonalS4       of g11 : GyrationComponent * g12 : GyrationComponent         // -4  (traceless g11=−g22)
    | TetragonalD2d      of g11 : GyrationComponent                                   // -42m (traceless g11=−g22)
    | Orthorhombic222    of g11 : GyrationComponent * g22 : GyrationComponent * g33 : GyrationComponent
    | OrthorhombicMm2    of g12 : GyrationComponent                                   // off-diagonal only
    | Monoclinic2        of g11 : GyrationComponent * g22 : GyrationComponent * g33 : GyrationComponent * g13 : GyrationComponent
    | MonoclinicM        of g12 : GyrationComponent * g23 : GyrationComponent
    | Triclinic1         of g11:GyrationComponent * g22:GyrationComponent * g33:GyrationComponent * g23:GyrationComponent * g13:GyrationComponent * g12:GyrationComponent

type ActivityState =
    | NotActive
    | OpticallyActive of class' : GyrationClass * hand : Handedness
```

Mapping to the solver reuses the **existing** crystal-class → `Rho` constructors in `OpticalProperties/Active.fs`, which already build the imaginary gyrotropic matrices via `Rho.fromIm` (`MaterialProperties.fs:139`):
- `CubicActive` → `Rho.cubicCrystal` (`Active.fs:30`, `diag(g,g,g)`).
- `UniaxialActive` → **`Rho.type_3_4_6_Crystal` only** (`Active.fs:46`, which takes exactly `(g11, g33)` and builds `diag(g11, g11, g33)`). This is the correct **sole** target for all six uniaxial enantiomorphic classes (3, 32, 4, 422, 6, 622): their symmetric gyration is diagonal `diag(g11,g11,g33)`, which `type_3_4_6_Crystal` is. **`Rho.type_32_42_62_Crystal` (`Active.fs:54`) is NOT a `UniaxialActive` target** — it requires three arguments `(g11, g12, g33)` and injects an **antisymmetric** off-diagonal `±g12` that a two-component `UniaxialActive` has no value to supply and that is not a symmetric-gyration component. (Reserve `type_32_42_62_Crystal` for a future distinct class case that genuinely owns a g12; the six uniaxial classes do not.)
- `OrthorhombicMm2` (planar, off-diagonal only) → `Rho.planarCrystal` (`Active.fs:38`, single `g12`).
- The **biaxial 222 / monoclinic / triclinic / traceless tetragonal (-4, -42m)** classes have **no** constructor yet — **extend `Active.fs`** with `Rho.fromIm` builders for them (a small, well-bounded addition).
- **Symmetric-vs-antisymmetric reconciliation.** The crystallographic gyration tensor is symmetric, but some current `Rho` helpers carry an *antisymmetric* off-diagonal (`Rho.planarCrystal`, `Rho.type_32_42_62_Crystal`). The editor exposes only the **physicist-facing symmetric components**, and the domain layer owns the mapping into `Rho` — which is why `UniaxialActive` maps to the purely-diagonal `type_3_4_6_Crystal` and never to the antisymmetric `type_32_42_62_Crystal`. Keep the class list + component forms in one constants/table module so the picker and the mapping share a single source of truth.
- The **anisotropy** choice (the selected `EpsComplexity` case, §6.1) constrains the class picker: `Isotropic ⇒ {23,432}`; `UniaxialEps ⇒ {3,32,4,422,6,622}`; `BiaxialEps ⇒ triclinic/monoclinic/orthorhombic`. For a centrosymmetric / non-rotating class the toggle is **removed** (not greyed — §13 "remove don't disable"). Quartz (class 32) is the worked example: `g = diag(g11, g11, g33)`, with the measured components `g11 ≈ +5.9×10⁻⁵`, `g33 ≈ −10.1×10⁻⁵` at 24 °C (Appl. Opt. 48, 5307, 2009 — §13), the overall sign flipping with handedness.

### 6.3 Magnetic → `muWithDisp`

For natural media at optical frequencies `mu = 1` is physically correct (Landau–Lifshitz, §13), so the default is scalar `Mu.vacuum = identity` (`MaterialProperties.fs:114`) and most entries never open the tensor. **Magnetic** unlocks a small **gyromagnetic (Polder) tensor**: a diagonal `mu`, a field-parallel `mu_par` (defaults to 1, the saturated Polder limit), a single gyration `g`, and an axis (default z = Faraday geometry). Model it as a typed named-condition (no naked bool), mirroring the gyrotropic `Rho` form:

```fsharp
type GyrationAxis = AlongX | AlongY | AlongZ                 // default AlongZ (Faraday); transverse = Voigt
type GyrotropicMu =                                          // NEW
    { muDiagonal : MuValue                                   // reuse MuValue (MaterialProperties.fs:101)
      muParallel : MuValue
      gyration   : GyrationComponent
      axis       : GyrationAxis }
type MagneticState = NonMagnetic | Magnetic of GyrotropicMu  // third case (full anisotropic μ) is a non-breaking addition
```

Mapping: build `[[mu, +ig, 0]; [−ig, mu, 0]; [0, 0, mu_par]]` (permuted by axis) through **`Mu.create`** (`MaterialProperties.fs:112`, complex — the off-diagonals are imaginary), lifted via `Mu.dispersive` into `muWithDisp`. **No solver work:** `BerremanMatrix.fs` already reads off-diagonal `mu` entries. The only new code is a `Mu.gyrotropic` helper in the `OpticalProperties` layer, mirroring `Rho.planarCrystal`.

---

## 7. Multilayer sample builder — the repeat-**K** period group

The user must **never** hand-enter 2·N interleaved layers. Instead: select **two-or-more** layers and **repeat them K times** (an editable K), i.e. a first-class **period group**. The pure primitive already exists and is tested: `RepeatBuilder.expand (cell : Layer list) (count : int) = List.replicate count cell |> List.concat` (`OpticalConstructor.Ui/RepeatBuilder.fs:24`), guarded by `Validation.validateRepeatCount` (count ≥ 1, `Validation.fs:56`). What is missing is the **interactive** wiring — `expand`'s count/cell are compile-time literals today and no `StackMsg` carries a repeat op.

- **Period super-row.** Present a period as one collapsible super-row in the stack table (rotating-triangle expander — §13 Microsoft guidance) with an **inline K stepper** on the header and the unit-cell `Layer`s nested beneath. Editing any cell row updates every period; changing K adds/removes whole periods.
- **Domain wiring.** Add a repeat operation to the pure stack editor: extend `StackEditor.StackMsg` (`OpticalConstructor.Ui/StackEditor.fs`) with e.g. `MakeRepeatBlock of indices : int list * count : int`, whose `applyStackMsg` arm gathers the selected contiguous cell from `OpticalSystem.films`, calls `Validation.validateRepeatCount`, then `RepeatBuilder.expand cell count`, splicing the flat result back into `films`. Keep this **distinct** from the existing organisational `StackEditor.groupLayers` (`:69`), whose own comment states it is *not* the repeat builder — two separate toolbar actions with separate AutomationIds (`MakeRepeatBlockButton` vs `GroupSelectionButton`) so users/automation never conflate them.
- **Persistence caveat.** By the `RepeatBuilder.fs` header's design the project persists only the **flat** `films` list — the unfactored period is **not** recoverable after expansion, so K is not re-editable post-save (Q2). Two ways out, to decide: (a) accept flat-only for the mock and re-derive K on demand; (b) introduce a first-class `PeriodGroup` structure carried on the Sample so K stays live. This spec proposes **(b)** as a small NEW domain type (`{ cell : Layer list; count : int }`) surfaced only in the editor, expanded to `films` at `sampleToSystem` time — matching how FilmStar/RP Coating keep the period a live parameter (§13).
- **Optional QWOT entry.** For quarter-wave/DBR stacks let the user enter *optical* thickness (QWOT at a design λ) with the physical `Thickness` (`Media.fs:12`) derived read-only via `t = λ/(4n)` — a display/derivation seam like `MaterialPreview.fs`'s nm/µm/eV toggle, leaving the stored value canonical-SI metres. (Templates already ship one periodic example: `Templates.dbr` expands `Templates.dbrCell` (`Templates.fs:103`) `Templates.dbrPeriods = 8` times (`:106`).)

---

## 8. Dispersive-material view — a **two-Y-axis** (n left / k right) chart, extracted into `OpticalConstructor.Controls`

The dispersive material view is a wavelength chart with **two Y axes**: **n on the left**, **k on the right**. The two curves already exist as pure builders — `SeriesData.plotN11Series` (n = Re[√ε₁₁], wraps `Variables.calculateN11Re`) and `SeriesData.plotXi11Series` (k = Im[√ε₁₁], wraps `calculateXi11Im`) at `OpticalConstructor.Ui/Charts/SeriesData.fs:103-104` (the `Re[e11]`/`Im[e11]` legend strings are a misnomer — because the transform is `SquareRoot`, `MaterialProperties.fs:22-24`, the pair *is* the complex refractive index n+ik). The X axis reuses `MaterialPreview.spectralRange`/`axisLabel`/`axisTicks` (`MaterialPreview.fs:31,18,40` — nm/µm/Å/eV/cm⁻¹, display-only). **The current preview (`MaterialsView.dispersionPreview`, `MaterialsView.fs:236`) plots only n via Plotly in a WebView2 — so this is net-new wiring.**

The task requires this chart to have **the same functionality as the experiment chart page**, so **extract the shared pieces into `OpticalConstructor.Controls`** — the project that already hosts `Ribbon`, `LibraryControls`, `LayerBandsControls` and that **both** `OpticalConstructor.TestWindows` (`…TestWindows.fsproj:81`) and `OpticalConstructor.Ui` (`…Ui.fsproj:184`) reference. Today those pieces live in the harness app `OpticalConstructor.TestWindows`, not the shipping `OpticalConstructor.Ui`:

- **Pure, renderer-neutral model** — `ExperimentChart` / `ChartFont` / `ChartStyle` (`OpticalConstructor.TestWindows/ExperimentChart.fs`): `ChartSeries`, `toCsv` (`:47`), the element-picker DU `ChartElement` (`:152`), per-axis `AxisStyle` (`:199`) + `NumberFormat` (`:160`) + `formatValue` (`:184`), `LegendStyle` (`:222`) / `LegendPlacement` (`:208`), `SeriesStyle` (`:229`), `ChartStyleState` (`:241`) + `dataBounds`/`defaultState` (`:261,281`).
- **ScottPlot IO seam** — `ChartWindow` (`OpticalConstructor.TestWindows/ChartWindow.fs`): the pop-out element picker + contextual properties panel + crosshair + grid toggles + PNG/CSV export, with `applyAxisLimits`/`applyAxisFormat`/`applyAxisFonts`/`setCartesianAxesVisible`/`rebuildPlot` (`:133,121,104,151,178`) and centralized `ChartWindowIds`.

**Extraction is a real MOVE + project reference, NOT file-linking.** File-linking (`<Compile Include=… Link=… />`) compiles the source into *each* assembly separately, minting **distinct type identities** (`ChartStyleState`, `ChartElement`, …) that the two projects cannot exchange — the opposite of a shared control. Instead:

1. **Move the pure model** (`ExperimentChart` / `ChartFont` / `ChartStyle`) into `OpticalConstructor.Controls`. It is domain-neutral (no Berreman types) and needs **no new package** — Controls already references Avalonia + FuncUI (`…Controls.fsproj:51-52`). Both hosts get the *same* types by identity through their existing Controls project reference.
2. **Move `ChartWindow`** (the ScottPlot seam) into Controls too, adding a **`ScottPlot.Avalonia` 5.1.59** package reference to `OpticalConstructor.Controls` (that version already ships in Ui and TestWindows, `…Ui.fsproj:173` / `…TestWindows.fsproj:75`).
3. **Reconcile the Ui-native `Charts/ChartSettings.fs`** (`type ChartSettings` `:50`, with its own `applyToScottPlot` `:138` / `applyToPlotly` `:179`, also single-Y) **into the one shared Controls model** — re-point Ui at the moved model rather than growing a third settings type.

**The specific change to support a second Y axis** (ScottPlot 5.1.59 supports it natively via `plot.Axes.Right`, no new package; `plot.Axes.Right` is currently **never used anywhere in the repo**):

1. `ChartElement` (`ExperimentChart.fs:152`) gains a right-axis case — `YAxisLeft | YAxisRight` (or `YAxis of AxisSide`); `elements`/`elementLabel` (`:299,303`) enumerate/label it.
2. `ChartStyleState` (`:241`) holds a **second** `AxisStyle` (`yAxisRight` alongside `yAxis`); the axis mutators keyed by `isX : bool` (`mapAxis`, `:315`) become tri-state (X / Y-left / Y-right). `AxisStyle` itself is already per-axis and reused unchanged.
3. `SeriesStyle` (`:229`) gains an **axis-assignment** field (`axis : AxisSide`) — n→left, k→right.
4. `ChartWindow.rebuildPlot` (`:178`) sets each `scatter.Axes.YAxis` to `plot.Axes.Left` or `plot.Axes.Right` per series; `applyAxisLimits`/`applyAxisFormat`/`applyAxisFonts`/`setCartesianAxesVisible` (`:133,121,104,151`), currently hard-wired to `plot.Axes.Left/Bottom`, are generalized to also drive `plot.Axes.Right` (make it visible, set `Label.Text = "k"`, point its `TickGenerator.LabelFormatter` and grid at it).
5. `dataBounds`/`defaultState` (`:261,281`) compute **independent** per-axis Y bounds (n→left, k→right) instead of one shared Y range.
6. The crosshair/`GetNearest` readout resolves *which* axis the nearest series uses; the polar toggle stays hidden (`angular = false`, `ExperimentChart.fs:29`) — a wavelength sweep is non-angular.

`ExperimentChart.toCsv` (`:47`) exports the n/k table unchanged.

---

## 9. Two editor windows — Material editor vs Sample editor

Two separate windows, because the shared surface is real but the domains diverge.

**Shared (extract once, reference from both):**
- Window scaffold: name / id / description fields, `Save` (→ `add`/`update` on the proxy) / `Cancel`, inline validation via `OpticalConstructor.Ui/Validation.fs` (`validateThickness` `:46`, `validateWavelengthRange` `:64`, `validateRepeatCount` `:56`, and the physical-sanity `imaginaryIndexGainWarning` `:92`).
- The **dual-axis n/k dispersion chart** (§8), now the extracted `OpticalConstructor.Controls` control, embedded as a live preview in both windows.
- The **progressive-disclosure** widget conventions + a single central `[<Literal>]` **UiIds** module (§13; stable AutomationIds, "remove don't disable", persist expand state).
- The pure edit-model discipline: an Avalonia-free message DU mirroring `StackEditor.StackMsg`, unit-tested in `OpticalConstructor.Tests` without a window.

**Material editor (distinct):** the whole `MaterialComplexity` model (§6) — the anisotropy / absorbing / dispersive / optically-active / magnetic unlock ladder, the per-segment dispersion editor (`DispersionModel` picker + `WaveLengthInterval`s), the symmetry-class gyration panel, and the Polder-μ panel. It edits **tensors**; it never sets thickness/geometry.

**Sample editor (distinct):** material selection by `MaterialId`, geometry (`SubstrateKind = ThinFilm | Plate | Wedge`), `Thickness` (+ optional QWOT mode), and the **multilayer period builder** (§7). It edits **structure**; it never edits tensors — it *composes* materials (resolved through `MaterialProxy` / `resolveMaterial`, `MaterialLibrary.fs:69`) into `Layer`/`OpticalSystem`.

---

## 10. What this touches (when built — not in this task)

- **`OpticalConstructor.Domain`** — `MaterialLibrary.fs`: `MaterialProxy` (mutating), `MaterialId` (lookup-key wrapper, §3.1), labelled `MaterialError`, `DispersionFilter`/`MaterialQuery`. `ElementId.fs` (`Library`): `SampleProxy`, `SampleId`, `SampleError`, optional `PeriodGroup`. `DispersionModels.fs`: `WaveLengthInterval` + `DispersionSegment`, and NEW `ForouhiBloomer` / `BrendelBormann` cases. NEW `MaterialComplexity` / `EpsComplexity` / `IndexSpec` / `ActivityState` / `GyrationClass` / `MagneticState` + `toProperties` (per-axis segment-selecting closure for multi-segment anisotropy; `AnisotropicModel`/`toAnisotropicOpticalProperties` only for single-`DispersionModel`-per-axis). `Propagation.sampleToSystem`: consume `PeriodGroup` instead of literal-id branching.
- **`Berreman/OpticalProperties/Active.fs`** — extend the crystal-class → `Rho` constructors (biaxial/monoclinic/triclinic/traceless tetragonal); add `Mu.gyrotropic`. (No change to the `UniaxialActive` mapping target — it reuses the existing `Rho.type_3_4_6_Crystal`.)
- **`OpticalConstructor.Controls`** — NEW `MaterialsControls` (domain-free `State`+`Handlers`+`UiIds`), superseding `LibraryControls` in the bay; **receives the moved `ExperimentChart`/`ChartFont`/`ChartStyle`/`ChartWindow`** and a new `ScottPlot.Avalonia` 5.1.59 package reference.
- **`OpticalConstructor.Ui`** — the two editor windows; embed the extracted dual-axis chart control; reconcile `Charts/ChartSettings.fs` against the moved model; a repeat op on `StackEditor.StackMsg`; wire `RepeatBuilder`/`validateRepeatCount` to the period super-row.
- **`OpticalConstructor.TestWindows`** — rename `BayNames.library`→`materials`, swap the `mainBays` row; drop the local `ExperimentChart`/`ChartWindow` compile items now moved to Controls.
- **`OpticalConstructor.App`** — build the mock `MaterialProxy`/`SampleProxy` at the composition root; inject them.
- **Tests** — proxy add/remove/edit/search round-trips (stateful mock); `toProperties` per complexity facet (esp. gyration-class → `Rho`, multi-segment anisotropy → per-axis closure, Polder-μ → `Mu`); segment selection/clamping; `MakeRepeatBlock` expansion; headless `ui-smoke`: "unlock Anisotropic ⇒ Biaxial fields render", "n on left / k on right", "repeat 2 layers ×K ⇒ 2K films".

---

## 11. Decisions & open questions

**Decided (consistent with `024`):** ribbon tab = **Materials** (supersedes Library); mocks only, but now **stateful/mutating** so maintenance is testable; `MaterialId`/`SampleId` are **elevated lookup-key wrappers** over the still-raw stored `id` fields for the mock (§3.1); the search dispersion facet is a `DispersionFilter` DU, not a bool (resolves old Q4); reuse `MaterialEntry`/`Sample`/`DispersionModel`/`AnisotropicModel`/`Active.fs`, extend rather than fork; anisotropy is encoded by `EpsComplexity` alone (no parallel `AnisotropyKind` — the toggle is editor state selecting the case); gyration driven by a symmetry-class DU (compile-time "handful of components"), `UniaxialActive` → the diagonal `Rho.type_3_4_6_Crystal` only; μ defaults to scalar 1, unlocks a small Polder tensor; media are homogeneous only (no graded index); the dispersion chart is the experiment-chart model/window **moved into `OpticalConstructor.Controls`** with a second Y axis.

**Open:**
- **Q1 — bay rename vs coexist:** rename `BayNames.library`→`materials` outright, or keep a thin Library bay for element-binding and add Materials for maintenance? (Proposed: rename; Materials owns both binding and maintenance.)
- **Q2 — period persistence:** flat-only `films` (K not re-editable, per `RepeatBuilder.fs` design) vs a first-class `PeriodGroup` keeping K live (proposed).
- **Q3 — referential integrity:** should `removeMaterial` hard-block when a Sample references it, cascade, or warn only?
- **Q5 — segment gaps/overlap/out-of-range:** clamp-to-nearest (proposed, total closure) and/or a typed validation note; overlap resolution order.
- **Q6 — Forouhi–Bloomer / Brendel–Bormann:** in scope now, or defer (they are net-new `DispersionModel` cases + coefficient records)?
- **Q7 — RII import breadth:** extend `MaterialImport.importRefractiveIndexInfo` (`MaterialImport.fs:109`) beyond formula 1 + tabulated to the other common formula numbers now, or later.
- **Q8 — id field elevation:** when Storage lands, elevate `MaterialEntry.id`/`Sample.id`/`Sample.materialId` from `string` to `MaterialId`/`SampleId` (a breaking type + JSON-schema change with a wire-string codec), or keep the lookup-key-wrapper split of §3.1 permanently?

---

## 12. Proposed phasing (small slices, each independently green)

1. **Mutating proxies** — `MaterialProxy`/`SampleProxy` (`createInMemory` over a mutable cell) + `MaterialId`/`SampleId` lookup-key wrappers + `DispersionFilter`/`MaterialQuery` + labelled errors; pure add/remove/update/search/list tests. No UI.
2. **Materials bay** — rename to `Materials`; `MaterialsControls` (search + list + verbs) wired to the proxies; headless "search filters, remove removes".
3. **Sample editor (simple)** — single-material Sample add/edit (material pick + `SubstrateKind` + `Thickness`); Save round-trips through `SampleProxy`.
4. **Multilayer period builder** — `MakeRepeatBlock` on `StackEditor.StackMsg` + `RepeatBuilder.expand` + `validateRepeatCount`; period super-row with K stepper; "2 layers ×K ⇒ 2K films".
5. **Material editor — complexity ladder** — `MaterialComplexity` + `toProperties`, unlock ladder (absorbing/dispersive/anisotropic) over `DispersionModels`; per-segment dispersion + `WaveLengthInterval`; single-segment → `toAnisotropicOpticalProperties`, multi-segment → per-axis segment-selecting closure via `uniaxialEps`/`biaxialEps`.
6. **Optically active + magnetic** — `GyrationClass`/`ActivityState` → `Active.fs` `Rho` (`UniaxialActive` → `type_3_4_6_Crystal`; new builders for biaxial/monoclinic/triclinic/traceless tetragonal); `MagneticState` → `Mu.gyrotropic`; symmetry-constrained picker keyed off the `EpsComplexity` case.
7. **Extract the shared chart control** — **move** `ExperimentChart`/`ChartFont`/`ChartStyle`/`ChartWindow` into `OpticalConstructor.Controls` (+ add `ScottPlot.Avalonia` there); reconcile with `Charts/ChartSettings.fs`; both charts still green (no behaviour change).
8. **Dual-axis dispersion chart** — add the second Y axis (§8 steps 1–6); n-left/k-right preview embedded in the Material editor.
9. **Later (`OpticalConstructor.Storage`)** — real disk-backed `create`; broaden RII import; Forouhi–Bloomer / Brendel–Bormann if deferred; decide Q8 (field id elevation).

---

## 13. References (web)

**Optical-material databases & dispersion formulas**
- Refractiveindex.info database of optical constants (Nature Scientific Data) — <https://www.nature.com/articles/s41597-023-02898-2>
- RefractiveIndex.INFO — About (CC0 license, the 9 formula types, data access) — <https://refractiveindex.info/about>
- refractiveindex — Python package documenting the 9 RII formula types — <https://pypi.org/project/refractiveindex/>
- Filmetrics (KLA) Refractive Index Database — <https://www.kla.com/products/instruments/refractive-index-database>
- Luxpop refractive-index file directory — <http://www.luxpop.com/>
- FreeSnell: Refractive Index Spectra (n,k tables) — <https://people.csail.mit.edu/jaffer/FreeSnell/nk.html>
- J.A. Woollam — Optical Constants and dispersion model families — <https://www.jawoollam.com/resources/ellipsometry-tutorial/optical-constants>
- Sellmeier equation (Wikipedia) — <https://en.wikipedia.org/wiki/Sellmeier_equation> · RP Photonics — <https://www.rp-photonics.com/sellmeier_formula.html>
- Cauchy's equation (Wikipedia) — <https://en.wikipedia.org/wiki/Cauchy's_equation>
- Tauc–Lorentz model (Wikipedia) — <https://en.wikipedia.org/wiki/Tauc%E2%80%93Lorentz_model>
- **Forouhi–Bloomer model** (the NEW `ForouhiBloomer` dispersion case, §6.1) — A. R. Forouhi & I. Bloomer, "Optical dispersion relations for amorphous semiconductors and amorphous dielectrics," *Phys. Rev. B* **34**, 7018 (1986) — <https://link.aps.org/doi/10.1103/PhysRevB.34.7018> · crystalline follow-up, *Phys. Rev. B* **38**, 1865 (1988) · overview (Wikipedia) — <https://en.wikipedia.org/wiki/Forouhi%E2%80%93Bloomer_model>
- Rakić et al. 1998, Optical properties of metallic films (Lorentz–Drude & Brendel–Bormann) — <https://pubmed.ncbi.nlm.nih.gov/18286006/> · Au Rakic-LD/BB fits (RII) — <https://refractiveindex.info/?shelf=main&book=Au&page=Rakic-LD>

**Gyration tensor / optical activity by symmetry class**
- The role of Berry curvature derivatives in the optical activity of time-invariant crystals (arXiv:2501.03684) — <https://arxiv.org/abs/2501.03684> · HTML forms by point-group family — <https://arxiv.org/html/2501.03684v2>
- **Quartz gyration tensor (class 32; the §6.2 worked-example g11 ≈ +5.9×10⁻⁵, g33 ≈ −10.1×10⁻⁵ at 24 °C)** — primary measurement: "Determination of the components of the gyration tensor of quartz by oblique-incidence transmission two-modulator generalized ellipsometry," *Appl. Opt.* **48**(28), 5307 (2009) — <https://opg.optica.org/ao/abstract.cfm?uri=ao-48-28-5307> · corroborating HAUP measurement: "High-accuracy universal polarimeter measurement of optical activity and birefringence of α-quartz…," *J. Opt. Soc. Am. B* **15**(3), 1147 (1998) — <https://opg.optica.org/josab/abstract.cfm?uri=josab-15-3-1147>
- J. F. Nye, *Physical Properties of Crystals* (canonical gyration/optical-activity tensor forms) — <https://books.google.com/books/about/Physical_Properties_of_Crystals.html?id=ugwql-uVB44C>
- Ramachandran, Theory of Optical Activity of Crystals — I — <https://www.ias.ac.in/public/Volumes/seca/033/04/0217-0227.pdf>
- International Tables for Crystallography §3.2 (point groups; enantiomorphic/non-centrosymmetric enumeration) — <https://onlinelibrary.wiley.com/iucr/itc/Ac/ch3o2v0001/sec3o2o2o1/>
- Enantiomorphic Point Groups (UCL PDNN — the 11 chiral groups) — <http://pd.chem.ucl.ac.uk/pdnn/symm2/enantio1.htm>

**Magnetic permeability μ / magneto-optics**
- Polder tensor (Wikipedia — the gyromagnetic μ form, saturated limit) — <https://en.wikipedia.org/wiki/Polder_tensor>
- Metamaterials and the Landau–Lifshitz μ=1 argument (PNAS) — <https://www.pnas.org/doi/full/10.1073/pnas.0808478106>
- Observation of optical gyromagnetic properties in a magneto-plasmonic metamaterial (Nature Communications) — <https://www.nature.com/articles/s41467-022-29452-9>
- Nanofabricated media with negative permeability at visible frequencies (arXiv physics/0504178) — <https://arxiv.org/pdf/physics/0504178>

**Multilayer/coating & progressive-disclosure UX**
- FilmStar DESIGN — Layers / Groups / Stack editors, repeated layer groups — <https://www.ftgsoftware.com/design.htm>
- OpTaliX — Coating Formula Editor, quarter-wave shorthand `(HL)^K` — <https://www.optenso.com/optix/ex_coat.html>
- RP Coating — parameterized multilayer definition — <https://www.rp-photonics.com/rp_coating.html> · Bragg mirrors / DBR / QWOT — <https://www.rp-photonics.com/bragg_mirrors.html>
- JML Optical — Quarter Wave Optical Thickness (n·d = λ/4) — <https://www.jmloptical.com/technical-resources/glossary/quarter-wave-optical-thickness/>
- Nielsen Norman Group — Progressive Disclosure — <https://www.nngroup.com/articles/progressive-disclosure/>
- Microsoft Learn — Progressive Disclosure Controls (chevrons/triangles, More/Fewer, remove-don't-disable, persist state) — <https://learn.microsoft.com/en-us/windows/win32/uxguide/ctrl-progressive-disclosure-controls>
