# 0027-035 — Materials, Samples & the serializable dispersion model (preliminary spec, rev. 3)

**Status: preliminary — feature list / mini-spec, no code yet.** Revises `033`→`034`, then refines the
dispersion model per `036`. Key changes vs `033`: (1) **ids are `Guid`, elevated now** — no wrap/unwrap, no
deferral (§3.1); (2) a **serializable "func-value" dispersion model added to the engine `Dispersion.fs`**
with `toEpsWithDisp`/`toMuWithDisp`/`toRhoWithDisp` (§6.1); (3) **naming**: **Materials** is materials only,
**Library** is samples, and the old element-binding "Library" bay is renamed **Selector** (§2, Q1);
(4) layered samples stay **fully editable with multi-select bulk layer ops** (§7, Q2).

**`036` dispersion refinements (this rev.):** every dispersion expression is **elevated per tensor**
(`EpsDispersionExpression` / `MuDispersionExpression` / `RhoDispersionExpression` — never mixed), while a
genuinely-generic thing stays generic (a `WaveLengthInterval` is *just* an interval); **real models stay
real** with **separate n and k formulas** (complex is used *only* where a model is inherently complex, e.g.
Lorentz/Drude ε); the wavelength band is **one per segment, shared across the principal axes** (fixing the
per-axis-interval error), giving **three segment flavors** (isotropic / uniaxial / biaxial) elevated into a
collection; `validity` is renamed **`wavelengthInterval`**; and the medium is a **DU option tree** — eps is
always present as a two-case `EpsDispersion` (non-dispersive | dispersive), while magnetic (`MuDispersion`)
and optical-activity (`RhoDispersion`) are **optional**, each itself a two-case DU. §6 carries the full model.

§11 records the now-**decided** answers; §12 the phasing. Every type named below already exists at the cited
path unless flagged **NEW** / **MOVE** / **extend**. No files are changed by this document.

---

## 1. Goal

Give the app a proper **material & sample workbench**, still against **mocked IO proxies** — but proxies that
now **write** into an internal keyed store so every verb (add / remove / edit / view / search) is
unit-testable — and make dispersive materials **fully serializable and editable** by adding a serializable
dispersion representation to the engine.

Three distinct nouns, three distinct surfaces (Q1):

- **Material** — an infinite/bulk optical medium (glass, quartz, silicon, vacuum, langasite …): the existing
  `MaterialLibrary.MaterialEntry` (`OpticalConstructor.Domain/MaterialLibrary.fs:31`), pairing display
  metadata with the engine's `OpticalPropertiesWithDisp` (`Berreman/Dispersion.fs:53`). Maintained in the
  **Materials** bay/editor. *Materials only — never samples.*
- **Sample** — a cut-out plate / multilayer stack composed of materials at thicknesses/geometry: the existing
  `Library.Sample` (`OpticalConstructor.Domain/ElementId.fs:43`), mapping to engine `Layer`/`OpticalSystem`
  (`Berreman/Media.fs`). Maintained in the **Library** bay/editor (the word *Library* now means *the library
  of samples*).
- **Assigning** a catalogue entry to a table element (source / detector / polarizer / sample) is a separate
  concern — the current confirm-gated bind bay, **renamed Selector** (Q1); it *picks*, it does not *maintain*.

Consistent with the engine, every material edited here is a **single spatially-homogeneous** medium; a stack
is composed only of homogeneous `Layer`s (§7), never a graded slab.

---

## 2. Ribbon changes — **Selector** (rename) + **Materials** + **Library** (Q1)

The Main screen is a generic `Ribbon` of `Ribbon.Bay` records (`OpticalConstructor.Controls/Ribbon.fs`;
`Bay = { name : string; content : IView }`), so each of these is one `BayNames` constant + one `mainBays`
row and **no** change to `Ribbon.view`.

- **Rename** the current picker. `BayNames.library = "Library"`
  (`OpticalConstructor.TestWindows/TableAndElementRotationView.fs:150`, module `BayNames` at `:144`) →
  **`BayNames.selector = "Selector"`**. Its role (constrain by the selected element's kind, confirm-gated
  bind to `valueId` via `LibraryControls` / the read-only `LibraryProxy`, `ElementId.fs:178`) is unchanged —
  only the label frees up the word *Library*. (`LibraryControls`/`LibraryProxy` may keep their names or be
  renamed `SelectorControls`/`SelectorProxy` for clarity — cosmetic, not required.)
- **New `BayNames.materials = "Materials"`** — the **materials** workbench (§4): a searchable materials list
  with `Add… / Edit… / Remove / View`, launching the **Material editor window** (§9). `MaterialsControls`
  (**NEW**, domain-free, copying the `LibraryControls.fs` shape: pure `Row`/`State`, a `Handlers` record of
  `unit`-returning fns, `[<RequireQualifiedAccess>] module UiIds` of stable `[<Literal>]` ids).
- **New `BayNames.library = "Library"`** — the **samples** workbench (§5): a searchable samples list with the
  same verbs plus the **multilayer builder** (§7), launching the **Sample editor window** (§9).
  `SampleLibraryControls` (**NEW**, same domain-free shape).

The host (`TableAndElementRotationView`) flattens each proxy's results into `Row`s (as `libraryState` /
`flattenNode` already do) and maps `Handlers` back to `Msg`s, opening the relevant editor window.

**Automation contract:** each control's `UiIds` gets stable, intent-named ids (`AddMaterialButton`,
`EditMaterialButton`, `RemoveMaterialButton`, `MaterialSearchBox`, `AddSampleButton`,
`MakeMultilayerButton`, …) so the headless `ui-smoke` gate drives by meaning, not layout.

---

## 3. IO proxies — the mutating mock write-seam (unit-testable)

`024` §3 proposed only read functions; today bulk materials live as the static `MaterialLibrary.standard`
(`MaterialLibrary.fs:140`) and `Library.LibraryProxy` (`ElementId.fs:178`) is read-only. Maintenance needs a
**write** seam.

Follow the functional-proxy convention exactly (a `[<ReferenceEquality>]` record of camelCase
`Result`-returning functions, built by `createInMemory`, that a test replaces with a stub of the same shape).
The mock is **stateful**: `createInMemory` closes over a private mutable store and `add/remove/update`
mutate it — mutation confined to the proxy's IO boundary (the one place CLAUDE.md permits it), so
add-then-list / edit-then-get / remove-then-search round-trip in a unit test with no disk.

### 3.1 Ids are `Guid`, elevated now (Q8)

**Decided.** All ids are `Guid` internally, elevated into single-case DUs **now**, and those DUs are the
record fields themselves — there is no raw `string`, no wrapping/unwrapping, and no elevation/de-elevation
logic in the mock. An elevated id **is** a first-class key in the F# `Map` the mock stores (single-case DUs
have structural equality/comparison), which is precisely why this is clean.

```fsharp
type MaterialId =                                       // NEW — replaces MaterialEntry.id : string
    | MaterialId of System.Guid
    member this.value = let (MaterialId g) = this in g
    static member create () : MaterialId = MaterialId (System.Guid.NewGuid())

type SampleId =                                         // NEW — replaces Sample.id : string
    | SampleId of System.Guid
    member this.value = let (SampleId g) = this in g
    static member create () : SampleId = SampleId (System.Guid.NewGuid())
```

Elevate the fields directly (an in-scope change, done now — no legacy, no fallback, no migration):
`MaterialEntry.id : MaterialId` (`MaterialLibrary.fs:33`), `Sample.id : SampleId` and
`Sample.materialId : MaterialId` (`ElementId.fs:45,47`). Every construction/lookup site updates in the same
change: `MaterialLibrary.builtInEntries` seeds, `Library.seedEntries`, `resolveMaterial` (`MaterialLibrary.fs:69`,
now a `MaterialId`-keyed `Map` lookup), and — importantly — `Propagation.sampleToSystem`
(`OpticalConstructor.Domain/Propagation.fs:139`), whose current **string-id branching** (`"sample-multilayer-qw"`,
`"sample-euv-mosi"`, `:140-141`) becomes impossible with opaque `Guid`s and is **replaced by the structural
`PeriodGroup`/layer data** of §7 (so the multilayer shape is read off the sample, not matched by a magic
string). The human-facing label lives in `MaterialEntry.name` / `Sample.name`; the id is an opaque key.

Search is elevated too (no naked bool/string):

```fsharp
type DispersionFilter = AnyDispersion | OnlyDispersive | OnlyNonDispersive          // NEW
type MaterialQuery = { text : string; category : MaterialCategory option; dispersion : DispersionFilter }  // NEW
type SampleQuery   = { text : string; substrate : SubstrateKind option }                                   // NEW
```

Errors carry a `reason` (relabelling `MaterialLibrary.MaterialError`'s `UnknownMaterialId of string`,
`:42`, to `of reason : string` is source-compatible — F# positional case construction ignores the label, so
`Error (UnknownMaterialId g)` keeps compiling):

```fsharp
type MaterialError = UnknownMaterialId of reason:string | DuplicateMaterialId of reason:string
                   | MaterialStillReferenced of reason:string | InvalidMaterial of reason:string
type SampleError   = UnknownSampleId of reason:string | DuplicateSampleId of reason:string | InvalidSample of reason:string

[<ReferenceEquality>]
type MaterialProxy =                                    // NEW — the write-seam MaterialsControls calls
    {
        listMaterials   : unit          -> Result<MaterialEntry list, MaterialError>
        searchMaterials : MaterialQuery -> Result<MaterialEntry list, MaterialError>   // reuse byCategory/byNameContains (MaterialLibrary.fs:53,57)
        tryGetMaterial  : MaterialId    -> Result<MaterialEntry option, MaterialError>
        addMaterial     : MaterialEntry -> Result<unit, MaterialError>                 // rejects a duplicate id
        updateMaterial  : MaterialEntry -> Result<unit, MaterialError>                 // rejects an unknown id
        removeMaterial  : MaterialId    -> Result<unit, MaterialError>                 // HARD-BLOCKS if a Sample references it (Q3)
    }

[<ReferenceEquality>]
type SampleProxy =                                      // NEW — same shape over Library.Sample
    {
        listSamples   : unit        -> Result<Sample list, SampleError>
        searchSamples : SampleQuery  -> Result<Sample list, SampleError>
        tryGetSample  : SampleId     -> Result<Sample option, SampleError>
        addSample     : Sample       -> Result<unit, SampleError>
        updateSample  : Sample       -> Result<unit, SampleError>
        removeSample  : SampleId     -> Result<unit, SampleError>
    }
```

- The editor mints the id (`MaterialId.create ()`) when the user starts a **new** material/sample; `add`
  stores the entry under its own id and **hard-blocks a duplicate id** (Q3 applies to removal;
  duplicate-add is a `DuplicateMaterialId`). Because the entry already carries its id, `add` needs no
  minting and stays deterministic under test (a test constructs a fixed `Guid`).
- `createInMemory ()` seeds the store from `MaterialLibrary.builtInEntries` (`:79`) / `Library.seedEntries`
  (`ElementId.fs:189`) and returns closures over a `ref (Map<MaterialId, MaterialEntry>)` — no IO,
  deterministic.
- The read-only picker `LibraryProxy` (element binding) and the mutating `MaterialProxy`/`SampleProxy`
  (maintenance) coexist. A `MaterialsContext` bundles the proxies + current selection so views stay pure.

---

## 4. Maintain **Materials** — add / remove / edit / view / search

- **List / search** — `searchMaterials (MaterialQuery …)` over the seeded `MaterialEntry` list, reusing the
  pure `MaterialLibrary.byCategory`/`byNameContains` filters (`:53,57`); `MaterialCategory =
  Glass | Metal | Semiconductor | Crystal` (`:21`) is the category facet, `DispersionFilter` the dispersion
  facet.
- **View** — read-only render of metadata (id/name/category/description) plus the **dual-axis n/k dispersion
  chart** (§8); dispersive entries show curves, non-dispersive ones flat lines.
- **Add / Edit** — open the **Material editor window** (§9) on a blank or loaded `MaterialComplexity` (§6);
  Save calls `addMaterial` / `updateMaterial`.
- **Remove** — `removeMaterial (MaterialId …)`; **hard-blocks** (typed `MaterialStillReferenced`) when any
  Sample references the id (Q3). The bay surfaces the block as a message and offers to jump to the
  referencing sample(s); it never cascades or silently deletes.

---

## 5. Maintain **Samples** (the **Library** bay) — add / remove / edit / view / search

- **List / search** — `searchSamples` over `Library.Sample` (`ElementId.fs:43`:
  `{ id : SampleId; name; materialId : MaterialId; thickness : Thickness; substrate : SubstrateKind;
  description; … }`), where `SubstrateKind = ThinFilm | Plate | Wedge` (`:35`) is the geometry facet
  `Propagation.sampleToSystem` maps to films-vs-substrate placement.
- **View** — metadata + resolved geometry; for a multilayer, the layer-band view (`LayerBandsControls`,
  already used by the Details bay) over the expanded stack.
- **Add / Edit** — open the **Sample editor window** (§9): pick material(s) by `MaterialId`, set
  thickness/geometry, and — for a stack — use the **multilayer period builder** and the **multi-select bulk
  layer editor** (§7). Save calls `addSample` / `updateSample`.
- **Note — `Library.Sample` today cannot express an arbitrary stack.** It carries a single `materialId`;
  §7 introduces the editable multi-layer structure a real sample library needs (and which replaces the
  string-id multilayer branching now that ids are `Guid`, §3.1).

---

## 6. **Material complexity** — the progressive-unlocking model

The simplest material is **transparent, isotropic, non-dispersive** (one real index n, `k = 0`, no
anisotropy, no activity, no magnetism). Every richer feature is **unlocked** by a checkbox/toggle
(progressive disclosure — §13), and each unlock lifts **exactly one field** of the engine record
`OpticalPropertiesWithDisp = { epsWithDisp; muWithDisp; rhoWithDisp }` (`Berreman/Dispersion.fs:53`) off its
vacuum/scalar default; unchecking restores the default losslessly. This is *net-new UI + a small domain
edit-model over already-existing physics types* — not a solver change.

The edit model is a **DU option tree** (036): a medium **always** has an `eps` (which is itself a two-case
`EpsDispersion` — non-dispersive or dispersive), and **may** additionally be magnetic and/or optically
active — each an *optional* property that, when present, is again a two-case DU:

```fsharp
type MaterialComplexity =                 // NEW (OpticalConstructor.Domain) — the option tree
    {
        eps      : EpsDispersion          // ALWAYS present; two-case (non-dispersive | dispersive) → epsWithDisp (§6.1)
        magnetic : MuDispersion option    // OPTIONAL; when Some, two-case                          → muWithDisp  (§6.3)
        active   : RhoDispersion option   // OPTIONAL; when Some, two-case                          → rhoWithDisp (§6.2)
    }
    member toProperties : OpticalPropertiesWithDisp
    // = { epsWithDisp = eps.toEpsWithDisp
    //     muWithDisp  = match magnetic with Some m -> m.toMuWithDisp  | None -> Mu.vacuum.dispersive
    //     rhoWithDisp = match active   with Some a -> a.toRhoWithDisp | None -> Rho.vacuum.dispersive }
```

`toProperties` is a pure map (unit-tested in `OpticalConstructor.Tests`, no window) that calls the engine's
`to…WithDisp` members (§6.1) and supplies the `Mu.vacuum` / `Rho.vacuum` defaults for absent properties. The
**isotropic / uniaxial / biaxial** choice is *inside* `EpsDispersion` (§6.1); the editor's 3-way toggle
chooses which case is built and constrains the gyration-class picker (§6.2). `EpsDispersion`, `MuDispersion`
and `RhoDispersion` are **distinct elevated types** — the compiler makes it impossible to pass an eps
dispersion where a μ or ρ one is expected.

### 6.1 Serializable dispersion — elevated, real-first, in the engine `Dispersion.fs` (Q6, 036)

`EpsWithDisp`/`MuWithDisp`/`RhoWithDisp` (`Berreman/Dispersion.fs:9,24,38`) each wrap a raw
`WaveLength -> Eps/Mu/Rho` **closure** — a function value that **cannot be serialized**. We do **not** change
those types. We **add, in the same file, elevated serializable representations that PRODUCE them**, plus
`toEpsWithDisp`/`toMuWithDisp`/`toRhoWithDisp`. Q6: these sit strictly **before** the WithDisp types and merely
expose the `WaveLength -> Eps/Mu/Rho` function — no solver change.

**Generic building blocks — a formula is just a formula, an interval just an interval (036).** Across the
standard catalogues an optical constant is a **sum of polynomial and inverse-polynomial terms in the
wavelength** (§13: the Schott/Laurent glass form `n² = Σ Aᵢλ^{2i}`; the refractiveindex.info "formula 3/4"
sum of `cᵢλ^{pᵢ}/(λ^{qᵢ} − rᵢ^{sᵢ})` terms). A single term is `multiplier · (Σ_k cₖ·(λ − λᵢ)^k)^power`. These
are **tensor-agnostic** and therefore stay generic (like a `WaveLengthInterval` — *just* an interval — they
are not "eps" or "mu" anything); they are only ever held *inside* an elevated Eps/Mu/Rho expression below.

```fsharp
type WaveLengthInterval = { lower : WaveLength; upper : WaveLength }         // GENERIC — an interval is just an interval

/// One REAL term:  multiplier · ( Σ_k coefficients[k]·(λ − lambda)^k ) ^ power.   (real coefficients)
/// Example  −2.5·(λ − 0.0012345)^(−2)  =  { lambda = 0.0012345; coefficients = [|0;1|]; power = −2; multiplier = −2.5 }.
type DispersionTerm = { lambda : double; coefficients : double[]; power : int; multiplier : double }

/// A REAL func-value: an ordered sum of real terms + the abscissa scale (metres per coefficient-unit, so the
/// engine needs no Units DU). GENERIC — a real function of λ; never held bare, always inside an elevated type.
type DispersionFormula =
    { terms : DispersionTerm list; wavelengthScale : double }
    member this.evaluate : WaveLength -> double     // Σ terms at x = w.value / wavelengthScale
```

**Real-first n / k; complex only where needed (036).** Some models are *explicitly real* with **separate
formulas for n and k** (Cauchy, tabulated, transparent Sellmeier where k ≡ 0) — forcing them through complex
is needless complexity. Others (Lorentz, Drude) are *inherently complex* in ε. So one principal axis is a
small DU that keeps the real path real and reaches for complex only when a model demands it:

```fsharp
type EpsAxisDispersion =
    | RealNK     of n : DispersionFormula * k : DispersionFormula    // real n(λ), k(λ) — k = the zero formula for transparent
    | ComplexEps of ComplexDispersionFormula                         // ε(λ) directly, only where complex is genuine (Lorentz/Drude); n + ik = √ε
    member this.complexIndex : WaveLength -> ComplexRefractionIndex
```

`ComplexDispersionFormula` / `ComplexDispersionTerm` mirror the real pair with `System.Numerics.Complex`
coefficients and are used **only** by `ComplexEps`. `RealNK.complexIndex` → `n + i·k` (two real evaluations);
`ComplexEps.complexIndex` → `Complex.Sqrt ε`. Both yield the engine `ComplexRefractionIndex`
(`MaterialProperties.fs:43`).

**Three segment flavors, a band shared across the axes, elevated into a collection (036).** A medium's
anisotropy is uniform, and a wavelength band applies to **all** its principal axes at once — so the band is
**one per segment, shared across the axes** (fixing rev.2's per-axis-interval error) and is renamed the
self-describing **`wavelengthInterval`** (the old `validity` said *nothing about what* was valid). That gives
exactly three segment shapes, which anisotropy elevates into a **collection** (Q5 — the segment **higher in
the list wins** on overlap; no clamps, no validation):

```fsharp
type IsotropicEpsSegment = { wavelengthInterval : WaveLengthInterval; eps : EpsAxisDispersion }                                        // NEW
type UniaxialEpsSegment  = { wavelengthInterval : WaveLengthInterval; ordinary : EpsAxisDispersion; extraordinary : EpsAxisDispersion }// NEW
type BiaxialEpsSegment   = { wavelengthInterval : WaveLengthInterval; x : EpsAxisDispersion; y : EpsAxisDispersion; z : EpsAxisDispersion } // NEW

/// The eps dispersion, elevated: anisotropy flavor × a HOMOGENEOUS ordered collection of its segments.
type EpsDispersionExpression =                                       // NEW — globally-unique cases
    | IsotropicEps of IsotropicEpsSegment list
    | UniaxialEps  of UniaxialEpsSegment list
    | BiaxialEps   of BiaxialEpsSegment list
```

**The two-case, per-tensor elevated properties (036).** eps is always present; μ and ρ are optional (§6).
Each is a **distinct** two-case DU, so the compiler can never let an eps dispersion stand in for a μ or ρ one:

```fsharp
type EpsDispersion =                                                 // NEW — "eps: dispersive or non-dispersive"
    | EpsNonDispersive of Eps                                        // constant tensor (Eps already encodes anisotropy)
    | EpsDispersive    of EpsDispersionExpression
    member this.toEpsWithDisp : EpsWithDisp
```

`toEpsWithDisp`: `EpsNonDispersive e` → `EpsWithoutDisp e` (zero closure overhead); `EpsDispersive x` builds
the `WaveLength -> Eps` closure — per medium selecting the covering segment (top-of-list, else the topmost,
extrapolating, Q5), evaluating each `EpsAxisDispersion` to n+ik, and calling the engine index constructors:
isotropic → `Eps.fromComplexRefractionIndex n`; uniaxial → `(n_o, n_e, n_o)`; biaxial → the three-index
constructor (`MaterialProperties.fs:87`). **Absorbing** is just a non-zero k; **Dispersive** is just more than
one constant term.

**μ and ρ get the identical elevated treatment** so all three WithDisp types become serializable — this is the
whole of "we address EpsWithDisp, MuWithDisp, RhoWithDisp":

```fsharp
type MuDispersion  = MuNonDispersive of Mu | MuDispersive of MuDispersionExpression       // NEW; member toMuWithDisp  (§6.3)
type RhoDispersion = RhoNonDispersive of Rho | RhoDispersive of RhoDispersionExpression    // NEW; member toRhoWithDisp (§6.2)
```

`MuDispersionExpression` (the Polder-tensor entries, §6.3) and `RhoDispersionExpression` (the gyration-class
components, §6.2) hold **real** `DispersionFormula`s — the magnitudes are real and the engine places them into
the *imaginary* off-diagonals at assembly through `Mu.create` / `Rho.fromIm` (`MaterialProperties.fs:112,139`),
so no complex appears in the stored gyration/Polder data either. Constant tensors emit the `…WithoutDisp` case.

**The named formulas are a convenience catalogue that lowers to this (Q6 — "all known dispersion models").**
Keep the editor-facing `DispersionModels.DispersionModel` DU (`DispersionModels.fs:120`:
`Sellmeier | Cauchy | Lorentz | Drude | TaucLorentz | GaussianOscillator | ConstantNK`), **add**
`ForouhiBloomer` / `BrendelBormann` (§13) and a raw `SumOfTerms` escape hatch, and give each case a
`toEpsAxis : DispersionModel -> EpsAxisDispersion` that **compiles** the analytic form into the elevated axis
expression (`x` = abscissa in the model's unit):

- **Constant `n + ik`**, **Cauchy `n = A + B/λ² + C/λ⁴`** (`:51`), **transparent Sellmeier
  `n² = 1 + Σ Bᵢλ²/(λ²−λᵢ²)`** (`:42`) → **`RealNK`** (real n; k the zero formula, or a second real Cauchy for
  an absorbing dielectric). Cauchy is a Laurent polynomial (real terms `A·λ⁰`, `B·λ^(−2)`, `C·λ^(−4)`);
  Sellmeier uses `λ²/(λ²−λᵢ²) = 1 + λᵢ²/(λ²−λᵢ²)` so each oscillator is a real constant `Bᵢ` **plus** a real
  inverse term `{ coefficients = [| −λᵢ²; 0; 1 |]; power = −1; multiplier = Bᵢλᵢ² }` — all real, the n formula
  evaluating `√(1 + Σ)`.
- **Lorentz `ε = ε∞ + Σ sⱼ/(rⱼ²−E²−i·dⱼ·E)`** (`:63`), **Drude `ε = ε∞ − ωp²/(E²+i·γE)`** (`:74`) →
  **`ComplexEps`** (complex is genuinely needed — the `−i·dⱼE` / `i·γE` damping): a complex inverse term per
  oscillator, e.g. Lorentz `{ coefficients = [| rⱼ²; −i·dⱼ; −1 |]; power = −1; multiplier = sⱼ }` plus the
  `ε∞` constant; then `n + ik = √ε`.
- **Tauc–Lorentz** (`:85`), **Gaussian** (`:96`), **Forouhi–Bloomer**, **Brendel–Bormann** — piecewise /
  transcendental (band-gap step, `exp`, √-argument, Voigt). Not a finite term sum; they stay **named cases
  evaluated directly** (their coefficient record is already serializable) and `toEpsAxis` wraps
  `DispersionModels.evaluate` (`:206`) as a `ComplexEps`. The term array is the serializable *lingua franca*
  for the rational family; the transcendental ones serialize as their record. Both routes end at
  `WaveLength -> ComplexRefractionIndex`, consumed by `EpsDispersion.toEpsWithDisp`.

`DispersionModels.AnisotropicModel` (`Uniaxial`/`Biaxial`, `:242`) is **removed** — superseded by
`EpsDispersionExpression`; `toAnisotropicOpticalProperties`/`toOpticalProperties` re-point at
`EpsDispersion.toEpsWithDisp`.

### 6.2 Optically active → `rhoWithDisp` (gyration, symmetry-driven)

Optical activity is a rank-2 **axial** gyration tensor `g_ij`; only its **symmetric** part rotates
polarization, it is **zero for every centrosymmetric class**, and of the optically-active point groups the
editor never needs more than **6 components — usually 1 or 2** (§13). **Never expose a free 3×3.** Drive it
from a symmetry-class DU whose *each case carries exactly its allowed components*, so "only a handful are
non-zero" is a compile-time guarantee. Each surviving component is a **real** `DispersionFormula` (§6.1 — the
gyration magnitude is real; the engine places it into the imaginary off-diagonals at assembly) so gyration can
itself disperse and serialize; a non-dispersive component is a single constant term. Whether ρ is present at
all is the `active : RhoDispersion option` of §6; whether present ρ disperses is the two-case `RhoDispersion`:

```fsharp
type Handedness = LeftHanded | RightHanded                       // enantiomorph = one overall sign flip (g → −g)

type GyrationClass =                                             // NEW — offer ONLY the rotation-producing classes
    | CubicActive        of g   : DispersionFormula                                    // 23, 432  → diag(g,g,g)
    | UniaxialActive     of g11 : DispersionFormula * g33 : DispersionFormula          // 3,32,4,422,6,622 → diag(g11,g11,g33)
    | PlanarActive       of g12 : DispersionFormula                                    // mm2 (off-diagonal only)
    | Orthorhombic222    of g11 : DispersionFormula * g22 : DispersionFormula * g33 : DispersionFormula
    | Monoclinic2        of g11 : DispersionFormula * g22 : DispersionFormula * g33 : DispersionFormula * g13 : DispersionFormula
    | MonoclinicM        of g12 : DispersionFormula * g23 : DispersionFormula
    | Triclinic1         of g11:DispersionFormula * g22:DispersionFormula * g33:DispersionFormula * g23:DispersionFormula * g13:DispersionFormula * g12:DispersionFormula

/// The elevated ρ dispersion expression — a gyration class + the enantiomorph sign. Distinct from Eps/Mu.
type RhoDispersionExpression = { gyration : GyrationClass; hand : Handedness }    // NEW

type RhoDispersion =                                            // NEW — the rhoWithDisp source (§6.1); two-case
    | RhoNonDispersive of Rho                                   // a constant gyrotropic tensor
    | RhoDispersive    of RhoDispersionExpression
    member this.toRhoWithDisp : RhoWithDisp
```

`toRhoWithDisp` builds the `WaveLength -> Rho` closure (`RhoNonDispersive` short-circuits to `RhoWithoutDisp`),
evaluating each component's `DispersionFormula` and assembling the **imaginary** gyrotropic matrix through the
existing `Rho.fromIm` (`MaterialProperties.fs:139`),
reusing the crystal-class `Rho` constructors in `OpticalProperties/Active.fs`:
- `CubicActive` → `Rho.cubicCrystal` (`Active.fs:30`, `diag(g,g,g)`).
- `UniaxialActive` → the **diagonal** `Rho.type_3_4_6_Crystal` (`Active.fs:46`, `(g11,g33) → diag(g11,g11,g33)`)
  — the correct sole target for all six uniaxial enantiomorphic classes. (`Rho.type_32_42_62_Crystal`,
  `Active.fs:54`, is **not** a `UniaxialActive` target: it needs a third `g12` and injects an antisymmetric
  off-diagonal a symmetric two-component uniaxial has no value to supply.)
- `PlanarActive` → `Rho.planarCrystal` (`Active.fs:38`, single `g12`).
- **222 / monoclinic / triclinic** have no constructor yet — **extend `Active.fs`** with `Rho.fromIm`
  builders (small, well-bounded).
- The anisotropy choice (the `EpsDispersionExpression` case / non-dispersive `Eps` shape, §6.1) constrains the
  picker: `IsotropicEps ⇒ {23,432}`; `UniaxialEps ⇒ {3,32,4,422,6,622}`;
  `BiaxialEps ⇒ triclinic/monoclinic/orthorhombic`. For a
  centrosymmetric / non-rotating class the toggle is **removed** (not greyed — §13). Quartz (class 32) is the
  worked example: `g = diag(g11,g11,g33)`, measured `g11 ≈ +5.9×10⁻⁵`, `g33 ≈ −10.1×10⁻⁵` at 24 °C (§13),
  sign flipping with handedness.

### 6.3 Magnetic → `muWithDisp`

For natural media at optical frequencies `μ = 1` is correct (§13), so `magnetic = None` (§6) for almost every
entry (`toProperties` then supplies `Mu.vacuum`). Whether μ is present is the `magnetic : MuDispersion option`
of §6; whether present μ disperses is the two-case `MuDispersion`. Its dispersive expression is a small
**gyromagnetic (Polder) tensor** whose entries are **real** `DispersionFormula`s (§6.1):

```fsharp
type GyrationAxis = AlongX | AlongY | AlongZ                    // default AlongZ (Faraday); transverse = Voigt
/// The elevated μ dispersion expression — a Polder tensor with real entries. Distinct from Eps/Rho.
type MuDispersionExpression = { muDiagonal : DispersionFormula; muParallel : DispersionFormula     // NEW
                                gyration : DispersionFormula; axis : GyrationAxis }
type MuDispersion =                                            // NEW — the muWithDisp source (§6.1); two-case
    | MuNonDispersive of Mu                                    // a constant (scalar or Polder) tensor
    | MuDispersive    of MuDispersionExpression
    member this.toMuWithDisp : MuWithDisp
```

`toMuWithDisp` assembles `[[μ, +ig, 0]; [−ig, μ, 0]; [0, 0, μ_par]]` (permuted by axis, off-diagonals
imaginary) through `Mu.create` (`MaterialProperties.fs:112`), evaluating each real entry at λ
(`MuNonDispersive` short-circuits to `MuWithoutDisp`). **No solver work** — `BerremanMatrix.fs` already reads
off-diagonal `μ`. The only new engine code is the `toMuWithDisp` assembly (mirroring `Rho.planarCrystal`).

---

## 7. Sample structure — multilayer period groups + multi-select bulk editing (Q2)

A sample is a **fully editable stack**, not a frozen artifact: once created, nothing prevents further edits,
and bulk edits over many layers must be convenient (Q2).

**Never hand-enter 2·N interleaved layers.** Select **two-or-more** layers and **repeat them K times** (an
editable K) — a first-class **period group**. The pure primitive exists and is tested:
`RepeatBuilder.expand (cell : Layer list) (count : int) = List.replicate count cell |> List.concat`
(`OpticalConstructor.Ui/RepeatBuilder.fs:24`), guarded by `Validation.validateRepeatCount` (count ≥ 1,
`Validation.fs:56`). Missing is the interactive wiring.

- **First-class `PeriodGroup`.** Carry the stack as an editable structure on the Sample (a NEW small domain
  type `{ cell : Layer list; count : int }`, plus plain single layers), expanded to `films` only at
  `sampleToSystem` time (matching FilmStar/RP-Coating keeping the period a live parameter, §13). K stays
  re-editable after save — and, since ids are now `Guid` (§3.1), this structural data is what
  `sampleToSystem` reads **instead of** the removed string-id matching.
- **Period super-row.** A period renders as one collapsible super-row (rotating-triangle expander, §13) with
  an inline **K stepper** and the unit-cell `Layer`s nested beneath; editing any cell row updates every
  period, changing K adds/removes whole periods.
- **Multi-select bulk layer ops (Q2).** The stack table supports **multi-selection** with convenience
  selectors — notably **"select all layers of material *m*"** (by `MaterialId`) — and **bulk actions** over
  the selection: **set thickness/height**, **change material** (re-point the selected layers' `materialId`),
  **remove**, **group into a period**, **move up/down**. These are pure `StackEditor.StackMsg`
  (`OpticalConstructor.Ui/StackEditor.fs`) additions — e.g. `SelectByMaterial of MaterialId`,
  `SetThicknessOfSelected of Thickness`, `SetMaterialOfSelected of MaterialId`, `RemoveSelected`,
  `MakeRepeatBlock of indices:int list * count:int` — whose `applyStackMsg` arms fold over the selected
  indices; each is unit-tested without a window. Keep `MakeRepeatBlock` **distinct** from the existing
  organisational `StackEditor.groupLayers` (`:69`) — separate toolbar actions, separate AutomationIds
  (`MakeRepeatBlockButton`, `SelectByMaterialButton`, `SetLayerHeightButton`, …).
- **Optional QWOT entry.** For quarter-wave/DBR stacks let the user enter *optical* thickness (QWOT at a
  design λ) with the physical `Thickness` (`Media.fs`) derived read-only via `t = λ/(4n)` — a display seam
  like `MaterialPreview.fs`'s unit toggle, storing canonical-SI metres. (`Templates.dbr` already expands
  `Templates.dbrCell` `Templates.dbrPeriods = 8` times, `Templates.fs:103,106`.)

---

## 8. Dispersive-material view — a **two-Y-axis** (n left / k right) chart, extracted into `OpticalConstructor.Controls`

Unchanged from `033` §8 (still valid). Summary: the dispersive view is a wavelength chart with **n on the
left Y axis, k on the right**. The two curves already exist as pure builders — `SeriesData.plotN11Series`
(n) and `SeriesData.plotXi11Series` (k) at `OpticalConstructor.Ui/Charts/SeriesData.fs:103-104` (the pair
*is* n+ik because the transform is `SquareRoot`, `MaterialProperties.fs:22`); the X axis reuses
`MaterialPreview.spectralRange`/`axisLabel`/`axisTicks` (`MaterialPreview.fs`). The task wants the same
functionality as the experiment chart, so **MOVE** the shared pieces into `OpticalConstructor.Controls`
(referenced by both `TestWindows` and `Ui`):

1. **Move** the pure model `ExperimentChart` / `ChartFont` / `ChartStyle`
   (`OpticalConstructor.TestWindows/ExperimentChart.fs`) into Controls (domain-neutral, no new package).
2. **Move** `ChartWindow` (the ScottPlot seam, `…/ChartWindow.fs`) into Controls, adding a
   `ScottPlot.Avalonia` 5.1.59 package reference there.
3. **Reconcile** the Ui-native `Charts/ChartSettings.fs` into the one shared model (re-point Ui, don't grow
   a third settings type). File-linking is **not** acceptable — it mints distinct type identities per
   assembly; this must be a real move + project reference.

**Second Y axis** (ScottPlot 5.1.59 native `plot.Axes.Right`, currently unused in the repo): `ChartElement`
gains `YAxisLeft | YAxisRight`; `ChartStyleState` holds a second `AxisStyle` (`yAxisRight`) and its mutators
go tri-state (X / Y-left / Y-right); `SeriesStyle` gains an `axis : AxisSide` assignment (n→left, k→right);
`ChartWindow.rebuildPlot` sets each `scatter.Axes.YAxis` to Left/Right and `applyAxisLimits/Format/Fonts` /
`setCartesianAxesVisible` also drive `plot.Axes.Right`; `dataBounds/defaultState` compute independent per-axis
Y bounds; the polar toggle stays hidden (`angular = false`). `ExperimentChart.toCsv` exports the n/k table
unchanged.

---

## 9. Two editor windows — Material editor vs Sample editor

Two windows, because the shared surface is real but the domains diverge.

**Shared (extract once, reference from both):** the window scaffold (name/id/description, Save→`add`/`update`,
Cancel), inline validation (`OpticalConstructor.Ui/Validation.fs` — `validateThickness`,
`validateWavelengthRange`, `validateRepeatCount`, `imaginaryIndexGainWarning`), the **dual-axis n/k chart**
(§8) as a live preview, the progressive-disclosure widget conventions + a single central `[<Literal>]`
**UiIds** module (§13), and the pure edit-model discipline (an Avalonia-free message DU mirroring
`StackEditor.StackMsg`, unit-tested without a window).

**Material editor (from the Materials bay):** the whole `MaterialComplexity` model (§6) — the
anisotropic / absorbing / dispersive / optically-active / magnetic unlock ladder, the per-segment dispersion
editor (`DispersionModel` picker + `WaveLengthInterval`s + the raw `SumOfTerms` escape hatch), the
symmetry-class gyration panel, the Polder-μ panel. It edits **tensors**; it never sets thickness/geometry.

**Sample editor (from the Library bay):** material selection by `MaterialId`, geometry (`SubstrateKind`),
`Thickness` (+ optional QWOT), the **period builder**, and the **multi-select bulk layer editor** (§7). It
edits **structure**; it never edits tensors — it *composes* materials (resolved through `MaterialProxy`) into
`Layer`/`OpticalSystem`.

---

## 10. What this touches (when built — not in this task)

- **`Berreman/Berreman/Dispersion.fs`** — NEW serializable, **generic** `WaveLengthInterval` /
  `DispersionTerm` / `DispersionFormula` (real) + `ComplexDispersionFormula` (complex, only where needed);
  the **elevated** per-tensor `EpsAxisDispersion`, the three segment flavors + the anisotropy collection
  `EpsDispersionExpression` (IsotropicEps/UniaxialEps/BiaxialEps), and the two-case `EpsDispersion` /
  `MuDispersion` / `RhoDispersion` (each with `MuDispersionExpression` / `RhoDispersionExpression`) carrying
  `toEpsWithDisp` / `toMuWithDisp` / `toRhoWithDisp`. (The `…WithDisp`/`…WithoutDisp` union types are unchanged.)
- **`Berreman/OpticalProperties/Active.fs`** — extend the crystal-class → `Rho` constructors (222 /
  monoclinic / triclinic); add the Polder-μ assembly. (`UniaxialActive` reuses the existing diagonal
  `Rho.type_3_4_6_Crystal`.)
- **`OpticalConstructor.Domain/DispersionModels.fs`** — keep the editor-facing `DispersionModel` catalogue;
  add `ForouhiBloomer` / `BrendelBormann` cases + a `SumOfTerms` escape hatch, and a `toEpsAxis` lowering
  each analytic case to the elevated `EpsAxisDispersion` (`RealNK` for real models, `ComplexEps` for
  Lorentz/Drude); **remove** `AnisotropicModel` (superseded by `EpsDispersionExpression`) and re-point
  `toOpticalProperties`/`toAnisotropicOpticalProperties` at `EpsDispersion.toEpsWithDisp`.
- **`OpticalConstructor.Domain`** — `MaterialLibrary.fs`: `MaterialId` (Guid), elevate `MaterialEntry.id`,
  `MaterialProxy` (mutating), labelled `MaterialError`, `MaterialQuery`/`DispersionFilter`. `ElementId.fs`
  (`Library`): `SampleId` (Guid), elevate `Sample.id`/`Sample.materialId`, `SampleProxy`, `SampleError`,
  `SampleQuery`, the `PeriodGroup` stack structure. NEW `MaterialComplexity` + `toProperties`.
  `Propagation.sampleToSystem`: read `PeriodGroup` structure (drop string-id branching).
- **`OpticalConstructor.Controls`** — NEW `MaterialsControls` + `SampleLibraryControls` (domain-free); the
  renamed Selector; **receives the moved `ExperimentChart`/`ChartFont`/`ChartStyle`/`ChartWindow`** + a new
  `ScottPlot.Avalonia` package reference.
- **`OpticalConstructor.Ui`** — the two editor windows; embed the extracted dual-axis chart; reconcile
  `Charts/ChartSettings.fs`; the `StackEditor.StackMsg` bulk/period ops; wire `RepeatBuilder` /
  `validateRepeatCount`. **`OpticalConstructor.Storage/MaterialImport.fs`** — extend
  `importRefractiveIndexInfo` (`:109`) beyond formula 1 + tabulated to the other RII formula numbers, each
  lowered to an `EpsAxisDispersion` via a `DispersionModel` (Q7).
- **`OpticalConstructor.TestWindows`** — rename `BayNames.library`→`selector`; add `materials`/`library` bay
  rows; drop the moved chart compile items.
- **`OpticalConstructor.App`** — build the mock `MaterialProxy`/`SampleProxy` at the composition root.
- **Tests** — proxy add/remove/edit/search round-trips (stateful mock; `Guid` keys); `removeMaterial` hard-block;
  `EpsAxisDispersion.complexIndex` per formula (Sellmeier/Cauchy `RealNK`, Lorentz/Drude `ComplexEps` lowerings
  equal the analytic value); `EpsDispersion.toEpsWithDisp` per anisotropy; segment top-of-list selection;
  `RhoDispersion`/`MuDispersion` → `Rho`/`Mu`; `MakeRepeatBlock`/`SelectByMaterial`/bulk ops; headless
  `ui-smoke` (unlock Anisotropic ⇒ Biaxial fields; n-left/k-right; repeat 2 layers ×K ⇒ 2K films;
  select-by-material bulk set-thickness).

---

## 11. Decisions (from `034`)

- **Q1 — naming:** **Materials** = optical materials only; **Library** = samples; the old element-binding
  "Library" bay is **renamed Selector** (a picker, not a maintainer). §2.
- **Q2 — samples stay fully editable:** a saved stack is editable, with **multi-select convenience** ("select
  all layers of material *m*") and **bulk actions** (set height, change material, remove, group, move). §7.
- **Q3 — referential integrity:** `removeMaterial` **hard-blocks** when a Sample references the material
  (typed `MaterialStillReferenced`); no cascade, no silent delete. §3.1 / §4.
- **Q4 —** does not exist.
- **Q5 — dispersion segments:** an ordered collection of same-flavor segments (each carrying one shared
  `wavelengthInterval`, §6.1); **the segment higher in the list wins** on overlap; **no clamps, no
  validation** — the segments are what they are. Out-of-all-ranges evaluates the topmost segment (the formula
  extrapolates). §6.1.
- **Q6 — all known dispersion models:** in scope now (incl. Forouhi–Bloomer & Brendel–Bormann). The models
  sit **before** `EpsWithDisp`/`MuWithDisp`/`RhoWithDisp` and merely expose the `WaveLength -> Eps/Mu/Rho`
  functions; the rational family lowers to the serializable (real `RealNK` or complex `ComplexEps`)
  `EpsAxisDispersion`, the transcendental ones serialize as their coefficient records. §6.1.
- **Q7 — import:** **extend `MaterialImport`** (`importRefractiveIndexInfo`, `MaterialImport.fs:109`) to the
  other RII formula numbers, each lowered to a `DispersionModel` → `EpsAxisDispersion`. §10.
- **Q8 — ids:** **elevate now** — `MaterialId`/`SampleId` are `Guid`-backed single-case DUs and are the record
  fields themselves; used directly as `Map` keys in the mock. No raw string, no wrap/unwrap in the proxy, no
  legacy/fallback/migration. §3.1.

---

## 12. Proposed phasing (small slices, each independently green)

1. **Elevate ids + mutating proxies** — `MaterialId`/`SampleId` (`Guid`) as the record fields; `MaterialProxy`/
   `SampleProxy` (`createInMemory` over a `Map`) + `MaterialQuery`/`SampleQuery`/`DispersionFilter` + labelled
   errors + `removeMaterial` hard-block; update all `MaterialEntry`/`Sample` construction/lookup sites and
   drop `sampleToSystem` string-id branching. Pure round-trip tests. No UI.
2. **Serializable dispersion in the engine** — generic `WaveLengthInterval`/`DispersionTerm`/
   `DispersionFormula` (+ complex variant); elevated `EpsAxisDispersion`, the three segment flavors,
   `EpsDispersionExpression`, and the two-case `EpsDispersion` + `toEpsWithDisp`; `toEpsAxis` lowerings for
   Sellmeier/Cauchy (`RealNK`) and Lorentz/Drude (`ComplexEps`) with equality tests vs the analytic values;
   segment top-of-list selection. No UI.
3. **Bays** — rename to Selector; add `MaterialsControls` (Materials) + `SampleLibraryControls` (Library)
   wired to the proxies; headless "search filters, remove hard-blocks when referenced".
4. **Sample editor + multilayer/bulk ops** — single-material add/edit; `PeriodGroup`; `MakeRepeatBlock` +
   `SelectByMaterial` + bulk set-thickness/change-material/remove on `StackEditor.StackMsg`; "2 layers ×K ⇒
   2K films", "select material *m* ⇒ set height".
5. **Material editor — complexity ladder** — `MaterialComplexity` (the option tree) + `toProperties`;
   absorbing/dispersive/anisotropic unlock over `EpsDispersion` + the segment/`DispersionModel` picker
   (+ `SumOfTerms`).
6. **Optically active + magnetic** — `GyrationClass`/`RhoDispersion` → `Active.fs` `Rho` (new 222/mono/tri
   builders; `UniaxialActive` → `type_3_4_6_Crystal`); `MuDispersion` → Polder-μ; symmetry-constrained picker.
7. **Extract the shared chart control** — MOVE `ExperimentChart`/`ChartFont`/`ChartStyle`/`ChartWindow` into
   `OpticalConstructor.Controls` (+ ScottPlot there); reconcile `Charts/ChartSettings.fs`; both charts green.
8. **Dual-axis dispersion chart** — add the second Y axis (§8); n-left/k-right preview in the Material editor.
9. **Forouhi–Bloomer / Brendel–Bormann + RII import breadth** — the transcendental `DispersionModel` cases;
   extend `MaterialImport` to the remaining RII formula numbers (Q7).

---

## 13. References (web)

**Optical-material databases & dispersion formulas (incl. the general term / Laurent-polynomial precedent)**
- Refractiveindex.info database of optical constants (Scientific Data) — the formula catalogue —
  <https://www.nature.com/articles/s41597-023-02898-2>
- RefractiveIndex.INFO — About (CC0 license, the formula types, data access) — <https://refractiveindex.info/about>
  · database repository — <https://github.com/polyanskiy/refractiveindex.info-database>
- refractiveindex — Python package documenting the RII formula types (incl. the general "formula 4" sum of
  `cᵢλ^{pᵢ}/(λ^{qᵢ} − rᵢ^{sᵢ})` terms and "formula 3" Laurent polynomial) — <https://pypi.org/project/refractiveindex/>
- Schott/general glass dispersion `n² = Σ Aᵢλ^{2i}` (Laurent polynomial) & Cauchy — the "sum of polynomials
  and inverse polynomials of wavelength" precedent — RP Photonics Sellmeier —
  <https://www.rp-photonics.com/sellmeier_formula.html> · Sellmeier (Wikipedia) —
  <https://en.wikipedia.org/wiki/Sellmeier_equation> · Cauchy (Wikipedia) —
  <https://en.wikipedia.org/wiki/Cauchy's_equation> · Edmund Optics, Dispersion —
  <https://www.edmundoptics.com/knowledge-center/application-notes/lasers/dispersion/>
- Tauc–Lorentz — <https://en.wikipedia.org/wiki/Tauc%E2%80%93Lorentz_model> · J.A. Woollam optical-constant
  model families — <https://www.jawoollam.com/resources/ellipsometry-tutorial/optical-constants>
- **Forouhi–Bloomer** (NEW `ForouhiBloomer` case) — Forouhi & Bloomer, *Phys. Rev. B* **34**, 7018 (1986) —
  <https://link.aps.org/doi/10.1103/PhysRevB.34.7018> · crystalline follow-up *Phys. Rev. B* **38**, 1865
  (1988) · overview — <https://en.wikipedia.org/wiki/Forouhi%E2%80%93Bloomer_model>
- **Brendel–Bormann / Lorentz–Drude** (NEW `BrendelBormann` case) — Rakić et al. 1998 —
  <https://pubmed.ncbi.nlm.nih.gov/18286006/> · Au Rakic-LD/BB (RII) —
  <https://refractiveindex.info/?shelf=main&book=Au&page=Rakic-LD>

**Gyration tensor / optical activity by symmetry class**
- Optical activity of time-invariant crystals — tensor forms by point-group family (arXiv:2501.03684) —
  <https://arxiv.org/abs/2501.03684> · <https://arxiv.org/html/2501.03684v2>
- **Quartz gyration (class 32; g11 ≈ +5.9×10⁻⁵, g33 ≈ −10.1×10⁻⁵ at 24 °C)** — *Appl. Opt.* **48**(28), 5307
  (2009) — <https://opg.optica.org/ao/abstract.cfm?uri=ao-48-28-5307> · *J. Opt. Soc. Am. B* **15**(3), 1147
  (1998) — <https://opg.optica.org/josab/abstract.cfm?uri=josab-15-3-1147>
- J. F. Nye, *Physical Properties of Crystals* (canonical gyration tensor forms) —
  <https://books.google.com/books/about/Physical_Properties_of_Crystals.html?id=ugwql-uVB44C>
- International Tables for Crystallography §3.2 (point groups; enantiomorphic enumeration) —
  <https://onlinelibrary.wiley.com/iucr/itc/Ac/ch3o2v0001/sec3o2o2o1/> · Enantiomorphic point groups (UCL
  PDNN) — <http://pd.chem.ucl.ac.uk/pdnn/symm2/enantio1.htm>

**Magnetic permeability μ / magneto-optics**
- Polder tensor (the gyromagnetic μ form) — <https://en.wikipedia.org/wiki/Polder_tensor>
- Landau–Lifshitz μ=1-at-optical-frequencies argument (PNAS) —
  <https://www.pnas.org/doi/full/10.1073/pnas.0808478106>
- Optical gyromagnetic properties in a magneto-plasmonic metamaterial (Nature Communications) —
  <https://www.nature.com/articles/s41467-022-29452-9>

**Multilayer/coating & progressive-disclosure UX**
- FilmStar DESIGN — Layers / Groups / repeated layer groups — <https://www.ftgsoftware.com/design.htm>
- OpTaliX — Coating Formula Editor, `(HL)^K` shorthand — <https://www.optenso.com/optix/ex_coat.html>
- RP Coating — parameterized multilayer / live period — <https://www.rp-photonics.com/rp_coating.html> ·
  Bragg mirrors / DBR / QWOT — <https://www.rp-photonics.com/bragg_mirrors.html>
- JML Optical — Quarter-Wave Optical Thickness (n·d = λ/4) —
  <https://www.jmloptical.com/technical-resources/glossary/quarter-wave-optical-thickness/>
- Nielsen Norman Group — Progressive Disclosure — <https://www.nngroup.com/articles/progressive-disclosure/> ·
  Microsoft Learn — Progressive Disclosure Controls (remove-don't-disable, persist state) —
  <https://learn.microsoft.com/en-us/windows/win32/uxguide/ctrl-progressive-disclosure-controls>
