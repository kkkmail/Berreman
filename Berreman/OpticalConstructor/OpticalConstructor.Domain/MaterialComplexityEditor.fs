/// Spec 0033 (023) — the pure, Avalonia-free MATERIAL edit model over
/// `MaterialComplexity` (MaterialLibrary.fs): the progressive-unlock ladder of
/// spec Part F. It mirrors the message-DU discipline of the step-21
/// `SampleStackEditor` (state + one message DU + a `Result`-returning apply),
/// so the Material editor window is a thin projection and every behaviour is
/// testable without a window.
///
/// The ladder is LOSSLESS BY CONSTRUCTION: the state stores every facet
/// independently of its unlock toggle, and `toComplexity` reads a facet only
/// while its toggle is on — so unchecking a toggle restores that aspect's
/// default without touching anything else, and re-checking restores the user's
/// edits. The derivation targets ONLY the serializable value trees of
/// `Berreman/Dispersion.fs` (`ConstantEpsValue` / `EpsDispersiveValue` /
/// `RhoWithDispValue` / `MuWithDispValue`); no tensor math is re-derived here.
///
/// The advisory gain rule (`Validation.imaginaryIndexGainWarning`,
/// OpticalConstructor.Ui/Validation.fs:92) is RESTATED here, not imported:
/// Domain cannot reference Ui (the step-21 `validateRepeatCount` precedent),
/// and Ui is outside this slice's `touches`, so the spec's "move with the
/// editors" real-move is deferred; the rule text and semantics are identical.
module OpticalConstructor.Domain.MaterialComplexityEditor

open System
open Berreman.MathNetNumericsMath
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.DispersionModels
open OpticalConstructor.Domain.MaterialLibrary

/// The anisotropy 3-way choice: how many independent principal axes the eps
/// carries. Selects the `ConstantEpsValue` shape (non-dispersive) or the
/// `EpsDispersiveValue` flavor (dispersive).
type Anisotropy =
    | Isotropic
    | Uniaxial
    | Biaxial

/// The absorbing unlock: transparent (real n) vs absorbing (complex n + ik)
/// constant cases. A named two-case DU, never a naked bool.
type Transparency =
    | Transparent
    | Absorbing

/// The dispersive unlock: one constant `ConstantEpsValue` vs the wavelength
/// segment list.
type DispersionChoice =
    | NonDispersive
    | DispersiveSegments

/// The OPTIONAL optically-active unlock (`MaterialComplexity.active`).
type ActivityChoice =
    | ActivityOff
    | ActivityOn

/// The OPTIONAL magnetic (Polder mu) unlock (`MaterialComplexity.magnetic`).
type MagneticChoice =
    | MagneticOff
    | MagneticOn

/// The Constant vs Dispersive sub-branch of the OPTIONAL activity / magnetic
/// rungs — the eps ladder's `DispersionChoice`, but for the SCALAR gyration /
/// Polder components: each component is one `DispersionFormula` (no segments).
/// A named two-case DU, never a naked bool.
type ComponentDispersion =
    | ConstantComponents
    | DispersiveComponents

/// The Polder-mu panel's shape choice: a scalar permeability or the full
/// gyromagnetic tensor with its magnetization axis.
type MuKind =
    | ScalarMuKind
    | GyromagneticMuKind

/// Which principal-index slot an edit targets. The slots map onto the active
/// axes of the anisotropy choice: isotropic reads the first only; uniaxial
/// reads (ordinary, extraordinary) from the first two; biaxial reads all three.
type PrincipalAxisSlot =
    | FirstAxis
    | SecondAxis
    | ThirdAxis

/// One independent component of a symmetric gyration tensor (spec 0033 gap G9 —
/// the missing per-component entry). Only the subset a class's point-group
/// symmetry admits is ever exposed (see `gyrationComponents`); a free 3×3 is
/// never offered (spec 0033 §0.4).
type GyrationComponent =
    | G11
    | G22
    | G33
    | G12
    | G13
    | G23

/// One dispersion segment under edit: the shared `wavelengthInterval` bounds
/// plus one `DispersionModel` PER AXIS SLOT (so an existing per-axis segment
/// tree seeds losslessly through the raw `SumOfTerms` escape hatch). The
/// active anisotropy decides how many of the three are read at derivation.
type EditSegment =
    {
        interval : WaveLengthInterval
        model1 : DispersionModel
        model2 : DispersionModel
        model3 : DispersionModel
    }

/// The editor state: every ladder facet stored INDEPENDENTLY of its toggle
/// (the lossless-restore mechanic — see the module doc).
type MaterialComplexityEditState =
    {
        anisotropy : Anisotropy
        transparency : Transparency
        dispersion : DispersionChoice
        /// The per-slot constant complex indices (n + ik); the imaginary part
        /// is read only while `transparency = Absorbing`.
        index1 : ComplexRefractionIndex
        index2 : ComplexRefractionIndex
        index3 : ComplexRefractionIndex
        /// The dispersive segment list (read only while
        /// `dispersion = DispersiveSegments`; never empty under the message arms).
        segments : EditSegment list
        activity : ActivityChoice
        /// The activity rung's Constant vs Dispersive sub-branch (read only while
        /// `ActivityOn`; selects the `gyration` or the `gyrationDispersion` facet).
        activityDispersion : ComponentDispersion
        /// The symmetry-class gyration CONSTANT facet (read while `ActivityOn` +
        /// `ConstantComponents`).
        gyration : GyrationClass<RhoValue>
        /// The symmetry-class gyration DISPERSIVE facet — one `DispersionFormula`
        /// per symmetry-allowed component, kept class-synced with `gyration` (read
        /// while `ActivityOn` + `DispersiveComponents`). Stored independently of the
        /// constant facet, so unchecking Dispersive restores the constant losslessly.
        gyrationDispersion : GyrationClass<DispersionFormula>
        hand : Handedness
        magnetic : MagneticChoice
        /// The magnetic rung's Constant vs Dispersive sub-branch (read only while
        /// `MagneticOn`; selects the `polder` or the `polderDispersion` facet).
        magneticDispersion : ComponentDispersion
        muKind : MuKind
        /// The Polder CONSTANT components (read while `MagneticOn` +
        /// `ConstantComponents`; the scalar kind reads `muDiagonal` alone).
        polder : PolderValue<MuValue>
        /// The Polder DISPERSIVE components — one `DispersionFormula` each (read
        /// while `MagneticOn` + `DispersiveComponents`; always the full Polder
        /// tensor, as the engine's `MuWithDispValue` carries no scalar dispersive
        /// case). Stored independently of the constant facet (lossless uncheck).
        polderDispersion : PolderValue<DispersionFormula>
    }

/// The typed rejections of the edit arms (errors as values, each case carrying a
/// diagnostic `reason` — never a throw). Derivation (`toComplexity`) no longer
/// rejects: every `DispersionModel` lowers (the transcendental cases through the
/// evaluated segment), so there is no `SegmentNotLowerable`.
type MaterialComplexityEditError =
    /// A segment edit aimed at an index outside the segment list.
    | NoSuchSegment of reason : string
    /// The dispersive eps needs at least one segment (the engine's segment
    /// selection extrapolates from the topmost segment and cannot run on none).
    | LastSegmentNotRemovable of reason : string

/// The editor's message DU: one arm per ladder operation, applied by
/// `applyMaterialComplexityMsg`.
type MaterialComplexityMsg =
    | ChooseAnisotropy of Anisotropy
    | SetTransparency of Transparency
    | SetDispersion of DispersionChoice
    | SetPrincipalIndex of PrincipalAxisSlot * ComplexRefractionIndex
    /// Append one default segment to the list.
    | AddSegment
    | RemoveSegment of segmentIndex : int
    | SetSegmentInterval of segmentIndex : int * WaveLengthInterval
    /// Choose a segment's dispersion model KIND (applied to every axis slot; a
    /// same-kind re-pick keeps the current coefficients).
    | ChooseSegmentModel of segmentIndex : int * DispersionModel
    /// Replace a segment's dispersion model UNCONDITIONALLY on every axis slot
    /// (spec 0033 gap G7 — a coefficient edit rebuilds the model in place and
    /// must NOT be discarded as a same-kind no-op the way `ChooseSegmentModel`
    /// treats a re-pick).
    | SetSegmentModel of segmentIndex : int * DispersionModel
    /// Choose the model KIND of ONE principal axis of a segment (spec 0033
    /// comment 009 — uniaxial / biaxial dispersive media carry an independent
    /// formula per axis); a same-kind re-pick keeps that axis's coefficients.
    | ChooseSegmentAxisModel of segmentIndex : int * axis : PrincipalAxisSlot * DispersionModel
    /// Replace ONE principal axis's model unconditionally (a per-axis coefficient
    /// edit — spec 0033 comment 009 + the gap-G7 coefficient surface).
    | SetSegmentAxisModel of segmentIndex : int * axis : PrincipalAxisSlot * DispersionModel
    | SetActivity of ActivityChoice
    /// Flip the activity rung's Constant vs Dispersive sub-branch (spec Part C —
    /// the gyration analogue of `SetDispersion`).
    | SetActivityDispersion of ComponentDispersion
    | ChooseGyrationClass of GyrationClass<RhoValue>
    /// Set one symmetry-allowed component of the current gyration tensor
    /// (spec 0033 gap G9 — the per-component entry).
    | SetGyrationComponent of GyrationComponent * RhoValue
    /// Set one symmetry-allowed component's DISPERSIVE facet — the scalar
    /// `DispersionFormula` the raw `SumOfTerms` surface edits (spec Part C).
    | SetGyrationComponentDispersion of GyrationComponent * DispersionFormula
    | SetHandedness of Handedness
    | SetMagnetic of MagneticChoice
    /// Flip the magnetic rung's Constant vs Dispersive sub-branch (spec Part C).
    | SetMagneticDispersion of ComponentDispersion
    | SetMuKind of MuKind
    | SetMuDiagonal of MuValue
    | SetMuParallel of MuValue
    | SetMuGyration of MuValue
    /// Set the DISPERSIVE facet of one Polder component — the scalar
    /// `DispersionFormula` the raw `SumOfTerms` surface edits (spec Part C).
    | SetMuDiagonalDispersion of DispersionFormula
    | SetMuParallelDispersion of DispersionFormula
    | SetMuGyrationDispersion of DispersionFormula
    | ChooseGyrationAxis of GyrationAxis

// ---------------------------------------------------------------------------
// Defaults (the ladder's "simplest material" and the per-rung seeds).
// ---------------------------------------------------------------------------

/// The default real index of a fresh material (an ordinary glass-like value).
let defaultIndexValue : float = 1.5

/// The default gyration component of a freshly-unlocked activity rung — the
/// active-crystal built-in's magnitude (`MaterialLibrary.activeCrystalComplexity`).
let defaultGyrationComponent : RhoValue = RhoValue 1.5e-6

/// A real constant `DispersionFormula` over an nm-reduced abscissa (the raw
/// `SumOfTerms` default and nothing else — models carry their own scales).
let private constantFormula (value : float) : DispersionFormula =
    {
        terms = [ { lambda = 0.0; coefficients = [| value |]; power = 1; multiplier = 1.0 } ]
        wavelengthScale = float (toMeters Nanometer 1.0)
    }

/// The default dispersive gyration component: the constant active magnitude as a
/// flat `DispersionFormula`, so switching Dispersive on starts from the constant
/// value (the raw `SumOfTerms` identity the segment ladder also seeds from).
let defaultGyrationFormula : DispersionFormula = constantFormula defaultGyrationComponent.value

/// The default dispersive Polder facet: each component the constant Polder default
/// (μ 1.0, μ∥ 1.0, gyration 0.1) as a flat `DispersionFormula`, so switching
/// Dispersive on starts from the constant value.
let defaultPolderDispersion : PolderValue<DispersionFormula> =
    {
        muDiagonal = constantFormula 1.0
        muParallel = constantFormula 1.0
        gyration = constantFormula 0.1
        axis = GyrationAxis.defaultValue
    }

/// The dispersion-model choices the segment picker offers, each seeded with a
/// representative published/default coefficient set (the Forouhi–Bloomer set is
/// Horiba TN13 a-Si and the Brendel–Bormann set is Rakić 1998 gold — the same
/// sets the AC-B6 tests pin). `ConstantNK` heads the list and is the default
/// segment model; `SumOfTerms` is the raw escape hatch (identity under
/// `toEpsAxis`).
let defaultModelChoices : DispersionModel list =
    [
        ConstantNK { n = defaultIndexValue; k = 0.0; wavelengthUnit = Nanometer; thermoOptic = None }
        // Length-based models are seeded in NANOMETRES (spec 0033 comment 009): the BK7 / a-glass
        // coefficients converted from their conventional µm form — the wavelength-squared / -fourth
        // terms scale by 1e6 / 1e12 so the evaluated index is identical, just displayed in nm.
        Cauchy { a = 1.5; b = 0.004 * 1.0e6; c = 0.0; wavelengthUnit = Nanometer; thermoOptic = None }
        Sellmeier
            {
                b = [ 1.03961212; 0.231792344; 1.01046945 ]
                c = [ 0.00600069867 * 1.0e6; 0.0200179144 * 1.0e6; 103.560653 * 1.0e6 ]
                wavelengthUnit = Nanometer
                thermoOptic = None
            }
        Lorentz { epsInf = 2.25; strength = [ 1.0 ]; resonance = [ 4.0 ]; damping = [ 0.1 ]; wavelengthUnit = ElectronVolt; thermoOptic = None }
        Drude { epsInf = 1.0; plasmaFrequency = 9.03; dampingFrequency = 0.05; wavelengthUnit = ElectronVolt; thermoOptic = None }
        TaucLorentz { epsInf = 1.0; amplitude = 100.0; resonance = 3.6; broadening = 2.0; bandGap = 1.5; wavelengthUnit = ElectronVolt; thermoOptic = None }
        GaussianOscillator { epsInf = 2.0; amplitude = 0.5; energy = 3.0; broadening = 0.5; wavelengthUnit = ElectronVolt; thermoOptic = None }
        ForouhiBloomer { nInf = sqrt 3.453; a = 0.865; b = 6.703; c = 13.237; bandGap = 0.906; wavelengthUnit = ElectronVolt; thermoOptic = None }
        BrendelBormann
            {
                plasmaFrequency = 9.03
                intrabandStrength = 0.770
                intrabandDamping = 0.050
                strength = [ 0.054; 0.050; 0.312; 0.719; 1.648 ]
                resonance = [ 0.218; 2.885; 4.069; 6.137; 27.97 ]
                damping = [ 0.074; 0.035; 0.083; 0.125; 0.179 ]
                broadening = [ 0.742; 0.349; 0.830; 1.246; 1.795 ]
                wavelengthUnit = ElectronVolt
                thermoOptic = None
            }
        SumOfTerms (RealNK (constantFormula defaultIndexValue, constantFormula 0.0))
    ]

/// A picker-stable code for a model's KIND (its coefficient values do not
/// change the code — the derived automation-id key).
let modelKindCode (model : DispersionModel) : string =
    match model with
    | Sellmeier _ -> "Sellmeier"
    | Cauchy _ -> "Cauchy"
    | Lorentz _ -> "Lorentz"
    | Drude _ -> "Drude"
    | TaucLorentz _ -> "TaucLorentz"
    | GaussianOscillator _ -> "GaussianOscillator"
    | ForouhiBloomer _ -> "ForouhiBloomer"
    | BrendelBormann _ -> "BrendelBormann"
    | ConstantNK _ -> "ConstantNK"
    | SumOfTerms _ -> "SumOfTerms"

/// A human label for a model's kind (the picker option text).
let modelKindLabel (model : DispersionModel) : string =
    match model with
    | Sellmeier _ -> "Sellmeier"
    | Cauchy _ -> "Cauchy"
    | Lorentz _ -> "Lorentz"
    | Drude _ -> "Drude"
    | TaucLorentz _ -> "Tauc–Lorentz"
    | GaussianOscillator _ -> "Gaussian"
    | ForouhiBloomer _ -> "Forouhi–Bloomer"
    | BrendelBormann _ -> "Brendel–Bormann"
    | ConstantNK _ -> "Constant n + ik"
    | SumOfTerms _ -> "Sum of terms (raw)"

/// The default segment: the visible band, a flat `ConstantNK` on every axis.
let defaultSegment : EditSegment =
    let model = List.head defaultModelChoices
    {
        interval = { lower = toWaveLength Nanometer 400.0; upper = toWaveLength Nanometer 700.0 }
        model1 = model
        model2 = model
        model3 = model
    }

/// The gyration classes the anisotropy choice offers (spec Part F; spec 0033
/// gap G9). The optical-activity gyration tensor gᵢⱼ is a SYMMETRIC second-rank
/// AXIAL (pseudo-)tensor; its independent components are fixed by the crystal's
/// point group, and only the rotation-producing (enantiomorphic / gyrotropic)
/// classes appear here — never a free 3×3 (spec 0033 §0.4). The optical
/// anisotropy the editor already knows (isotropic / uniaxial / biaxial) is a
/// direct read-off of the crystal SYSTEM, which in turn fixes which gyration
/// forms are physically admissible, so the picker is CONSTRAINED by it:
///
///   • ISOTROPIC optics ⇐ CUBIC system. Classes 23 and 432 carry an isotropic
///     gyration g·I (diag(g, g, g)) → `CubicActive`.
///
///   • UNIAXIAL optics ⇐ TETRAGONAL / TRIGONAL / HEXAGONAL systems. The
///     enantiomorphic classes 3, 32, 4, 422, 6, 622 all carry the diagonal form
///     diag(g₁₁, g₁₁, g₃₃) → `UniaxialActive` (the engine's `type_3_4_6_Crystal`
///     builder; quartz, class 32, is the worked example — g₁₁ ≈ +5.9×10⁻⁵,
///     g₃₃ ≈ −10.1×10⁻⁵ at 24 °C).
///
///   • BIAXIAL optics ⇐ ORTHORHOMBIC / MONOCLINIC / TRICLINIC systems:
///       – 222 (orthorhombic)  → diag(g₁₁, g₂₂, g₃₃)               → `Orthorhombic222`
///       – mm2 (orthorhombic)  → a single off-diagonal g₁₂          → `PlanarActive`
///       – 2   (monoclinic)    → g₁₁, g₂₂, g₃₃, g₁₃                 → `Monoclinic2`
///       – m   (monoclinic)    → g₁₂, g₂₃                          → `MonoclinicM`
///       – 1   (triclinic)     → the full symmetric tensor (6 comp) → `Triclinic1`
///
/// spec 0033 gap G9: `PlanarActive` (class mm2) was previously offered by NO
/// anisotropy and was therefore unreachable, even though the seeded "Active
/// (gyrotropic) crystal" built-in uses it (with a biaxial-transparent eps —
/// `MaterialLibrary.fs`); mm2 is orthorhombic, hence optically BIAXIAL, so it
/// belongs on the biaxial list, which also makes that built-in round-trip
/// through `ofComplexity`/`toComplexity` instead of snapping to another class.
///
/// Every offered class is rotation-producing (`GyrationClass` deliberately holds
/// no centrosymmetric / non-gyrotropic case); were a choice to offer none, the
/// activity toggle itself is REMOVED (spec Part F — remove, don't grey).
///
/// References (gyration-tensor forms by point group):
///   • J. F. Nye, *Physical Properties of Crystals* (Oxford, 1985), Ch. XIV
///     (canonical gyration-tensor forms by class).
///   • "Optical activity of time-invariant crystals" — tensor forms by
///     point-group family, arXiv:2501.03684 (https://arxiv.org/abs/2501.03684).
///   • International Tables for Crystallography, Vol. A, §3.2 (point groups;
///     enantiomorphic enumeration).
///   • Quartz gyration (class 32): *Appl. Opt.* 48(28), 5307 (2009)
///     (https://opg.optica.org/ao/abstract.cfm?uri=ao-48-28-5307).
let availableGyrationClasses (anisotropy : Anisotropy) : GyrationClass<RhoValue> list =
    let g = defaultGyrationComponent
    match anisotropy with
    | Isotropic -> [ CubicActive g ]                                  // cubic 23 / 432
    | Uniaxial -> [ UniaxialActive { g11 = g; g33 = g } ]             // trig / tet / hex 3,32,4,422,6,622
    | Biaxial ->
        [
            Orthorhombic222 { g11 = g; g22 = g; g33 = g }             // orthorhombic 222
            PlanarActive g                                           // orthorhombic mm2 (single g₁₂)
            Monoclinic2 { g11 = g; g22 = g; g33 = g; g13 = g }        // monoclinic 2
            MonoclinicM { g12 = g; g23 = g }                          // monoclinic m
            Triclinic1 { g11 = g; g22 = g; g33 = g; g23 = g; g13 = g; g12 = g }  // triclinic 1
        ]

/// A picker-stable code for a gyration CLASS (component values do not change
/// the code — the derived automation-id key).
let gyrationClassCode (gyration : GyrationClass<'g>) : string =
    match gyration with
    | CubicActive _ -> "Cubic"
    | UniaxialActive _ -> "Uniaxial"
    | PlanarActive _ -> "Planar"
    | Orthorhombic222 _ -> "Orthorhombic222"
    | Monoclinic2 _ -> "Monoclinic2"
    | MonoclinicM _ -> "MonoclinicM"
    | Triclinic1 _ -> "Triclinic1"

/// A human label for a gyration class (the picker option text).
let gyrationClassLabel (gyration : GyrationClass<'g>) : string =
    match gyration with
    | CubicActive _ -> "Cubic 23 / 432"
    | UniaxialActive _ -> "Uniaxial 3 / 4 / 6 (g₁₁, g₃₃)"
    | PlanarActive _ -> "Planar mm2 (g₁₂)"
    | Orthorhombic222 _ -> "Orthorhombic 222"
    | Monoclinic2 _ -> "Monoclinic 2"
    | MonoclinicM _ -> "Monoclinic m"
    | Triclinic1 _ -> "Triclinic 1"

/// A picker-stable code for a gyration component (the derived automation-id key).
let gyrationComponentCode (comp : GyrationComponent) : string =
    match comp with
    | G11 -> "g11"
    | G22 -> "g22"
    | G33 -> "g33"
    | G12 -> "g12"
    | G13 -> "g13"
    | G23 -> "g23"

/// A human label for a gyration component (the entry-box caption).
let gyrationComponentLabel (comp : GyrationComponent) : string =
    match comp with
    | G11 -> "g₁₁"
    | G22 -> "g₂₂"
    | G33 -> "g₃₃"
    | G12 -> "g₁₂"
    | G13 -> "g₁₃"
    | G23 -> "g₂₃"

/// The editable components of a gyration class, IN DISPLAY ORDER, paired with
/// their current value. Exactly the components the class's symmetry admits
/// appear — one for the single-valued cubic/planar forms, two for uniaxial /
/// monoclinic-m, and up to the full six for triclinic (spec 0033 gap G9).
/// Generic over the component value (`RhoValue` for the constant facet,
/// `DispersionFormula` for the dispersive facet — spec Part C).
let gyrationComponents (gyration : GyrationClass<'g>) : (GyrationComponent * 'g) list =
    match gyration with
    | CubicActive g -> [ (G11, g) ]
    | UniaxialActive u -> [ (G11, u.g11); (G33, u.g33) ]
    | PlanarActive g12 -> [ (G12, g12) ]
    | Orthorhombic222 o -> [ (G11, o.g11); (G22, o.g22); (G33, o.g33) ]
    | Monoclinic2 m -> [ (G11, m.g11); (G22, m.g22); (G33, m.g33); (G13, m.g13) ]
    | MonoclinicM m -> [ (G12, m.g12); (G23, m.g23) ]
    | Triclinic1 t -> [ (G11, t.g11); (G22, t.g22); (G33, t.g33); (G13, t.g13); (G23, t.g23); (G12, t.g12) ]

/// Set ONE component of the gyration tensor to `value`, keeping the class and
/// its other components. A component the class does not carry is a no-op — the
/// symmetry forbids it and the picker never offers it (spec 0033 gap G9).
/// Generic over the component value (`RhoValue` constant / `DispersionFormula`
/// dispersive — spec Part C).
let setGyrationComponent (comp : GyrationComponent) (value : 'g) (gyration : GyrationClass<'g>) : GyrationClass<'g> =
    match gyration with
    | CubicActive _ ->
        match comp with
        | G11 -> CubicActive value
        | _ -> gyration
    | UniaxialActive u ->
        match comp with
        | G11 -> UniaxialActive { u with g11 = value }
        | G33 -> UniaxialActive { u with g33 = value }
        | _ -> gyration
    | PlanarActive _ ->
        match comp with
        | G12 -> PlanarActive value
        | _ -> gyration
    | Orthorhombic222 o ->
        match comp with
        | G11 -> Orthorhombic222 { o with g11 = value }
        | G22 -> Orthorhombic222 { o with g22 = value }
        | G33 -> Orthorhombic222 { o with g33 = value }
        | _ -> gyration
    | Monoclinic2 m ->
        match comp with
        | G11 -> Monoclinic2 { m with g11 = value }
        | G22 -> Monoclinic2 { m with g22 = value }
        | G33 -> Monoclinic2 { m with g33 = value }
        | G13 -> Monoclinic2 { m with g13 = value }
        | _ -> gyration
    | MonoclinicM m ->
        match comp with
        | G12 -> MonoclinicM { m with g12 = value }
        | G23 -> MonoclinicM { m with g23 = value }
        | _ -> gyration
    | Triclinic1 t ->
        // Triclinic 1 carries all six components — the match is exhaustive.
        match comp with
        | G11 -> Triclinic1 { t with g11 = value }
        | G22 -> Triclinic1 { t with g22 = value }
        | G33 -> Triclinic1 { t with g33 = value }
        | G13 -> Triclinic1 { t with g13 = value }
        | G23 -> Triclinic1 { t with g23 = value }
        | G12 -> Triclinic1 { t with g12 = value }

/// The default edit state — the ladder's ground floor: transparent, isotropic,
/// non-dispersive, no optional aspect unlocked; every facet pre-seeded so any
/// rung unlocks into a sensible value.
let defaultState : MaterialComplexityEditState =
    {
        anisotropy = Isotropic
        transparency = Transparent
        dispersion = NonDispersive
        index1 = ComplexRefractionIndex (createComplex defaultIndexValue 0.0)
        index2 = ComplexRefractionIndex (createComplex defaultIndexValue 0.0)
        index3 = ComplexRefractionIndex (createComplex defaultIndexValue 0.0)
        segments = [ defaultSegment ]
        activity = ActivityOff
        activityDispersion = ConstantComponents
        gyration = CubicActive defaultGyrationComponent
        gyrationDispersion = CubicActive defaultGyrationFormula
        hand = RightHanded
        magnetic = MagneticOff
        magneticDispersion = ConstantComponents
        muKind = ScalarMuKind
        polder =
            {
                muDiagonal = MuValue 1.0
                muParallel = MuValue 1.0
                gyration = MuValue 0.1
                axis = GyrationAxis.defaultValue
            }
        polderDispersion = defaultPolderDispersion
    }

/// The simplest material the ladder derives: transparent, isotropic,
/// non-dispersive — absent optional aspects mean the engine's vacuum μ/ρ
/// defaults (`MaterialComplexity.toProperties`).
let defaultComplexity : MaterialComplexity =
    {
        eps = EpsWithoutDispValue (IsotropicTransparent (RefractionIndex defaultIndexValue))
        magnetic = None
        active = None
    }

// ---------------------------------------------------------------------------
// The advisory gain rule (restated — see the module doc).
// ---------------------------------------------------------------------------

/// Physical-sanity warning: a finite, negative imaginary refractive index
/// implies optical GAIN (an amplifying medium) rather than absorption. The
/// `Validation.imaginaryIndexGainWarning` rule (Ui/Validation.fs:92), restated
/// with identical semantics and message; advisory — it never gates a save.
let imaginaryIndexGainWarning (k : float) : string option =
    if Double.IsFinite k && k < 0.0 then
        Some "Imaginary refractive index k < 0 implies gain (an amplifying medium), not absorption."
    else None

// ---------------------------------------------------------------------------
// applyMaterialComplexityMsg — one message onto one immutable transform.
// ---------------------------------------------------------------------------

/// Whether the class is one the anisotropy choice offers (compared by class
/// CASE — component values are irrelevant to the constraint).
let private isOffered (anisotropy : Anisotropy) (gyration : GyrationClass<RhoValue>) : bool =
    availableGyrationClasses anisotropy
    |> List.exists (fun offered -> gyrationClassCode offered = gyrationClassCode gyration)

/// Keep an offered class; snap an un-offered one to the choice's first offered
/// class (applied when the activity rung UNLOCKS or the anisotropy changes
/// under an unlocked rung — never during `ofComplexity` seeding, which must
/// stay verbatim).
let private snapGyration (anisotropy : Anisotropy) (gyration : GyrationClass<RhoValue>) : GyrationClass<RhoValue> =
    if isOffered anisotropy gyration then gyration
    else
        match availableGyrationClasses anisotropy with
        | first :: _ -> first
        | [] -> gyration

/// Re-shape the dispersive gyration facet to the constant facet's symmetry class,
/// keeping the two class-synced (the class picker is driven by the single constant
/// `gyration`). A genuine class change resets each dispersive component to the
/// default formula — exactly as a class change resets the constant components to
/// the default magnitude (`availableGyrationClasses`); a same-class re-pick keeps
/// the user's edited formulas (the lossless discipline).
let private syncGyrationDispersion (gyration : GyrationClass<RhoValue>) (gyrationDispersion : GyrationClass<DispersionFormula>) : GyrationClass<DispersionFormula> =
    if gyrationClassCode gyration = gyrationClassCode gyrationDispersion then gyrationDispersion
    else gyration.map (fun _ -> defaultGyrationFormula)

let private checkSegmentIndex (segmentIndex : int) (segments : EditSegment list) : Result<unit, MaterialComplexityEditError> =
    if segmentIndex >= 0 && segmentIndex < List.length segments then Ok ()
    else Error (NoSuchSegment $"segment index %d{segmentIndex} is out of range (%d{List.length segments} segments)")

let private mapSegment (segmentIndex : int) (f : EditSegment -> EditSegment) (segments : EditSegment list) : EditSegment list =
    segments |> List.mapi (fun i seg -> if i = segmentIndex then f seg else seg)

/// The dispersion model a segment carries on one principal-axis slot (spec 0033
/// comment 009): isotropic reads the first only; uniaxial the first two (ordinary,
/// extraordinary); biaxial all three (x, y, z). Public so the editor view reads a
/// per-axis picker's current model.
let axisModelOf (slot : PrincipalAxisSlot) (seg : EditSegment) : DispersionModel =
    match slot with
    | FirstAxis -> seg.model1
    | SecondAxis -> seg.model2
    | ThirdAxis -> seg.model3

let private setAxisModel (slot : PrincipalAxisSlot) (model : DispersionModel) (seg : EditSegment) : EditSegment =
    match slot with
    | FirstAxis -> { seg with model1 = model }
    | SecondAxis -> { seg with model2 = model }
    | ThirdAxis -> { seg with model3 = model }

let applyMaterialComplexityMsg
    (msg : MaterialComplexityMsg)
    (state : MaterialComplexityEditState)
    : Result<MaterialComplexityEditState, MaterialComplexityEditError> =
    match msg with
    | ChooseAnisotropy anisotropy ->
        let gyration =
            match state.activity with
            | ActivityOn -> snapGyration anisotropy state.gyration
            | ActivityOff -> state.gyration
        Ok
            { state with
                anisotropy = anisotropy
                gyration = gyration
                gyrationDispersion = syncGyrationDispersion gyration state.gyrationDispersion }
    | SetTransparency transparency -> Ok { state with transparency = transparency }
    | SetDispersion dispersion -> Ok { state with dispersion = dispersion }
    | SetPrincipalIndex (slot, index) ->
        match slot with
        | FirstAxis -> Ok { state with index1 = index }
        | SecondAxis -> Ok { state with index2 = index }
        | ThirdAxis -> Ok { state with index3 = index }
    | AddSegment -> Ok { state with segments = state.segments @ [ defaultSegment ] }
    | RemoveSegment segmentIndex ->
        checkSegmentIndex segmentIndex state.segments
        |> Result.bind (fun () ->
            match state.segments with
            | [ _ ] -> Error (LastSegmentNotRemovable "the segment list cannot be emptied — a dispersive eps needs at least one segment")
            | _ ->
                let segments =
                    state.segments
                    |> List.indexed
                    |> List.filter (fun (i, _) -> i <> segmentIndex)
                    |> List.map snd
                Ok { state with segments = segments })
    | SetSegmentInterval (segmentIndex, interval) ->
        checkSegmentIndex segmentIndex state.segments
        |> Result.map (fun () -> { state with segments = mapSegment segmentIndex (fun seg -> { seg with interval = interval }) state.segments })
    | ChooseSegmentModel (segmentIndex, model) ->
        checkSegmentIndex segmentIndex state.segments
        |> Result.map (fun () ->
            let pick (seg : EditSegment) : EditSegment =
                // A same-kind re-pick keeps the segment's current coefficients
                // (notably the seeded raw SumOfTerms axis data).
                if modelKindCode seg.model1 = modelKindCode model then seg
                else { seg with model1 = model; model2 = model; model3 = model }
            { state with segments = mapSegment segmentIndex pick state.segments })
    | SetSegmentModel (segmentIndex, model) ->
        // A coefficient edit (spec 0033 gap G7): store the rebuilt model on
        // every axis slot verbatim — no same-kind short-circuit, so the new
        // coefficients survive.
        checkSegmentIndex segmentIndex state.segments
        |> Result.map (fun () ->
            { state with segments = mapSegment segmentIndex (fun seg -> { seg with model1 = model; model2 = model; model3 = model }) state.segments })
    | ChooseSegmentAxisModel (segmentIndex, slot, model) ->
        // A per-axis KIND pick (spec 0033 comment 009): a same-kind re-pick keeps
        // that axis's current coefficients.
        checkSegmentIndex segmentIndex state.segments
        |> Result.map (fun () ->
            let pick (seg : EditSegment) : EditSegment =
                if modelKindCode (axisModelOf slot seg) = modelKindCode model then seg
                else setAxisModel slot model seg
            { state with segments = mapSegment segmentIndex pick state.segments })
    | SetSegmentAxisModel (segmentIndex, slot, model) ->
        // A per-axis coefficient edit: store that axis's rebuilt model verbatim.
        checkSegmentIndex segmentIndex state.segments
        |> Result.map (fun () ->
            { state with segments = mapSegment segmentIndex (setAxisModel slot model) state.segments })
    | SetActivity ActivityOn ->
        let gyration = snapGyration state.anisotropy state.gyration
        Ok
            { state with
                activity = ActivityOn
                gyration = gyration
                gyrationDispersion = syncGyrationDispersion gyration state.gyrationDispersion }
    | SetActivity ActivityOff -> Ok { state with activity = ActivityOff }
    | SetActivityDispersion choice -> Ok { state with activityDispersion = choice }
    | ChooseGyrationClass gyration ->
        // A same-class re-pick keeps the current components (constant AND dispersive).
        if gyrationClassCode gyration = gyrationClassCode state.gyration then Ok state
        else Ok { state with gyration = gyration; gyrationDispersion = syncGyrationDispersion gyration state.gyrationDispersion }
    | SetGyrationComponent (comp, value) ->
        Ok { state with gyration = setGyrationComponent comp value state.gyration }
    | SetGyrationComponentDispersion (comp, formula) ->
        Ok { state with gyrationDispersion = setGyrationComponent comp formula state.gyrationDispersion }
    | SetHandedness hand -> Ok { state with hand = hand }
    | SetMagnetic magnetic -> Ok { state with magnetic = magnetic }
    | SetMagneticDispersion choice -> Ok { state with magneticDispersion = choice }
    | SetMuKind muKind -> Ok { state with muKind = muKind }
    | SetMuDiagonal value -> Ok { state with polder = { state.polder with muDiagonal = value } }
    | SetMuParallel value -> Ok { state with polder = { state.polder with muParallel = value } }
    | SetMuGyration value -> Ok { state with polder = { state.polder with gyration = value } }
    | SetMuDiagonalDispersion formula -> Ok { state with polderDispersion = { state.polderDispersion with muDiagonal = formula } }
    | SetMuParallelDispersion formula -> Ok { state with polderDispersion = { state.polderDispersion with muParallel = formula } }
    | SetMuGyrationDispersion formula -> Ok { state with polderDispersion = { state.polderDispersion with gyration = formula } }
    // The magnetization axis is a single physical choice — set it on BOTH facets so
    // a constant⇄dispersive toggle keeps the same tensor geometry.
    | ChooseGyrationAxis axis ->
        Ok
            { state with
                polder = { state.polder with axis = axis }
                polderDispersion = { state.polderDispersion with axis = axis } }

// ---------------------------------------------------------------------------
// toComplexity — the pure derivation onto the serializable value trees.
// ---------------------------------------------------------------------------

let private realIndex (index : ComplexRefractionIndex) : RefractionIndex =
    RefractionIndex index.value.Real

/// The constant eps of the current (anisotropy × transparency) rung.
let private constantEps (state : MaterialComplexityEditState) : ConstantEpsValue =
    match state.anisotropy, state.transparency with
    | Isotropic, Transparent -> IsotropicTransparent (realIndex state.index1)
    | Isotropic, Absorbing -> IsotropicAbsorbing state.index1
    | Uniaxial, Transparent -> UniaxialTransparent (realIndex state.index1, realIndex state.index2)
    | Uniaxial, Absorbing -> UniaxialAbsorbing (state.index1, state.index2)
    | Biaxial, Transparent -> BiaxialTransparent (realIndex state.index1, realIndex state.index2, realIndex state.index3)
    | Biaxial, Absorbing -> BiaxialAbsorbing (state.index1, state.index2, state.index3)

/// The dispersive eps of the current anisotropy: each edit segment lowers its
/// active axis models into the engine's per-symmetry segment records through
/// `DispersionModels.toEpsAxis`. Lowering is TOTAL — every model lowers, the
/// transcendental cases to the evaluated segment — so this never fails.
let private dispersiveEps (state : MaterialComplexityEditState) : EpsDispersiveValue =
    match state.anisotropy with
    | Isotropic ->
        state.segments
        |> List.map (fun seg -> { wavelengthInterval = seg.interval; dispersion = toEpsAxis seg.model1 })
        |> IsotropicDispersive
    | Uniaxial ->
        state.segments
        |> List.map (fun seg ->
            {
                wavelengthInterval = seg.interval
                ordinaryDispersion = toEpsAxis seg.model1
                extraordinaryDispersion = toEpsAxis seg.model2
            })
        |> UniaxialDispersive
    | Biaxial ->
        state.segments
        |> List.map (fun seg ->
            {
                wavelengthInterval = seg.interval
                xDispersion = toEpsAxis seg.model1
                yDispersion = toEpsAxis seg.model2
                zDispersion = toEpsAxis seg.model3
            })
        |> BiaxialDispersive

/// Derive the `MaterialComplexity` the state currently denotes. Each toggle
/// off means its aspect's DEFAULT (constant eps / absent option) regardless of
/// the stored facet — the lossless-uncheck mechanic. Derivation never fails now
/// (every segment model lowers, the transcendental cases to the evaluated
/// segment); the `Result` return is kept for the consumer contract, mirroring
/// `ofComplexity`.
let toComplexity (state : MaterialComplexityEditState) : Result<MaterialComplexity, MaterialComplexityEditError> =
    let eps =
        match state.dispersion with
        | NonDispersive -> EpsWithoutDispValue (constantEps state)
        | DispersiveSegments -> EpsWithDispValue (dispersiveEps state)
    Ok
        {
            eps = eps
            magnetic =
                match state.magnetic with
                | MagneticOff -> None
                | MagneticOn ->
                    match state.magneticDispersion with
                    | ConstantComponents ->
                        match state.muKind with
                        | ScalarMuKind -> Some (MuWithoutDispValue (ScalarMu state.polder.muDiagonal))
                        | GyromagneticMuKind -> Some (MuWithoutDispValue (GyromagneticMu state.polder))
                    // The dispersive branch is always the full Polder tensor — the engine's
                    // `MuWithDispValue` carries no scalar dispersive case; `toMuWithDisp`
                    // evaluates each component's formula per wavelength.
                    | DispersiveComponents -> Some (MuWithDispValue state.polderDispersion)
            active =
                match state.activity with
                | ActivityOff -> None
                | ActivityOn ->
                    match state.activityDispersion with
                    | ConstantComponents -> Some (RhoWithoutDispValue { gyration = state.gyration; hand = state.hand })
                    // `toRhoWithDisp` evaluates each component's formula per wavelength and
                    // assembles the symmetry-class gyration tensor.
                    | DispersiveComponents -> Some (RhoWithDispValue { gyration = state.gyrationDispersion; hand = state.hand })
        }

// ---------------------------------------------------------------------------
// ofComplexity — seed the ladder from an existing entry's edit model.
// ---------------------------------------------------------------------------

let private complexIndexOf (n : RefractionIndex) : ComplexRefractionIndex =
    ComplexRefractionIndex (createComplex n.value 0.0)

/// Seed a segment's axis slots from per-axis engine term data through the raw
/// `SumOfTerms` escape hatch — the identity under `toEpsAxis`, so the
/// round-trip back through `toComplexity` is value-identical.
let private seedSegment
    (interval : WaveLengthInterval)
    (axis1 : EpsAxisDispersion)
    (axis2 : EpsAxisDispersion)
    (axis3 : EpsAxisDispersion)
    : EditSegment =
    {
        interval = interval
        model1 = SumOfTerms axis1
        model2 = SumOfTerms axis2
        model3 = SumOfTerms axis3
    }

/// Whether a complexity can be LOSSLESSLY edited through this ladder (spec 0040
/// step 004). A dispersive eps segment lowered to the OPAQUE evaluated case
/// (`EpsAxisEvaluated` — a coded closure with no editable term coefficients, the
/// form the re-seeded engine presets Silicon / Langasite carry) cannot be
/// re-derived into model coefficients, so an editor seeded from it would surface a
/// misleading `SumOfTerms` segment the user cannot meaningfully change. Such an
/// entry opens VIEW-ONLY instead: the host editability seams
/// (`MaterialEditorView.init`, `MaterialsWindowView.editableSelection`) gate on
/// this predicate rather than on `complexity` presence alone. A constant or
/// finite-term (`RealNK` / `ComplexEps`) dispersion is fully editable.
let isEditableComplexity (complexity : MaterialComplexity) : bool =
    let axisIsEvaluated (axis : EpsAxisDispersion) : bool =
        match axis with
        | EpsAxisEvaluated _ -> true
        | RealNK _ | ComplexEps _ -> false
    match complexity.eps with
    | EpsWithoutDispValue _ -> true
    | EpsWithDispValue dispersive ->
        let axes =
            match dispersive with
            | IsotropicDispersive segments -> segments |> List.map (fun s -> s.dispersion)
            | UniaxialDispersive segments -> segments |> List.collect (fun s -> [ s.ordinaryDispersion; s.extraordinaryDispersion ])
            | BiaxialDispersive segments -> segments |> List.collect (fun s -> [ s.xDispersion; s.yDispersion; s.zDispersion ])
        not (axes |> List.exists axisIsEvaluated)

/// Seed the editor from an existing `MaterialComplexity` (the entry's editable
/// source of truth). Verbatim — no snapping, no clamping — so
/// `toComplexity (ofComplexity c) = Ok c` value-identically. A dispersive
/// gyration (`RhoWithDispValue`) or Polder μ (`MuWithDispValue`) seeds its
/// per-component `DispersionFormula` facet directly (spec Part C — the
/// `UnsupportedComplexity` view-only fallback is gone, §0.2). Seeding never
/// fails; the `Result` return is kept for the consumer contract.
let ofComplexity (complexity : MaterialComplexity) : Result<MaterialComplexityEditState, MaterialComplexityEditError> =
    let epsSeed =
        match complexity.eps with
        | EpsWithoutDispValue constant ->
            let anisotropy, transparency, i1, i2, i3 =
                match constant with
                | IsotropicTransparent n ->
                    let i = complexIndexOf n
                    Isotropic, Transparent, i, i, i
                | IsotropicAbsorbing n -> Isotropic, Absorbing, n, n, n
                | UniaxialTransparent (nO, nE) ->
                    let o = complexIndexOf nO
                    Uniaxial, Transparent, o, complexIndexOf nE, o
                | UniaxialAbsorbing (nO, nE) -> Uniaxial, Absorbing, nO, nE, nO
                | BiaxialTransparent (n1, n2, n3) ->
                    Biaxial, Transparent, complexIndexOf n1, complexIndexOf n2, complexIndexOf n3
                | BiaxialAbsorbing (n1, n2, n3) -> Biaxial, Absorbing, n1, n2, n3
            Ok (anisotropy, transparency, NonDispersive, i1, i2, i3, defaultState.segments)
        | EpsWithDispValue dispersive ->
            let anisotropy, segments =
                match dispersive with
                | IsotropicDispersive segs ->
                    Isotropic, segs |> List.map (fun s -> seedSegment s.wavelengthInterval s.dispersion s.dispersion s.dispersion)
                | UniaxialDispersive segs ->
                    Uniaxial, segs |> List.map (fun s -> seedSegment s.wavelengthInterval s.ordinaryDispersion s.extraordinaryDispersion s.ordinaryDispersion)
                | BiaxialDispersive segs ->
                    Biaxial, segs |> List.map (fun s -> seedSegment s.wavelengthInterval s.xDispersion s.yDispersion s.zDispersion)
            Ok (anisotropy, defaultState.transparency, DispersiveSegments, defaultState.index1, defaultState.index2, defaultState.index3, segments)
    epsSeed
    |> Result.map (fun (anisotropy, transparency, dispersion, index1, index2, index3, segments) ->
        // Active: a constant gyration seeds the CONSTANT facet verbatim (the dispersive
        // facet re-shapes to its class with default formulas); a dispersive gyration
        // seeds the DISPERSIVE facet verbatim (the constant facet takes the class's
        // default magnitudes) — either way the two facets stay class-synced.
        let activity, activityDispersion, gyration, gyrationDispersion, hand =
            match complexity.active with
            | None ->
                ActivityOff, defaultState.activityDispersion, defaultState.gyration, defaultState.gyrationDispersion, defaultState.hand
            | Some (RhoWithoutDispValue g) ->
                ActivityOn, ConstantComponents, g.gyration, syncGyrationDispersion g.gyration defaultState.gyrationDispersion, g.hand
            | Some (RhoWithDispValue g) ->
                ActivityOn, DispersiveComponents, g.gyration.map (fun _ -> defaultGyrationComponent), g.gyration, g.hand
        // Magnetic: a constant μ seeds the CONSTANT Polder facet verbatim; a dispersive
        // μ seeds the DISPERSIVE facet verbatim (the constant facet takes the default,
        // its axis matched to the dispersive tensor's).
        let magnetic, magneticDispersion, muKind, polder, polderDispersion =
            match complexity.magnetic with
            | None ->
                MagneticOff, defaultState.magneticDispersion, defaultState.muKind, defaultState.polder, defaultState.polderDispersion
            | Some (MuWithoutDispValue (ScalarMu mu)) ->
                MagneticOn, ConstantComponents, ScalarMuKind, { defaultState.polder with muDiagonal = mu }, defaultState.polderDispersion
            | Some (MuWithoutDispValue (GyromagneticMu polder)) ->
                MagneticOn, ConstantComponents, GyromagneticMuKind, polder, defaultState.polderDispersion
            | Some (MuWithDispValue polderF) ->
                MagneticOn, DispersiveComponents, defaultState.muKind, { defaultState.polder with axis = polderF.axis }, polderF
        {
            anisotropy = anisotropy
            transparency = transparency
            dispersion = dispersion
            index1 = index1
            index2 = index2
            index3 = index3
            segments = segments
            activity = activity
            activityDispersion = activityDispersion
            gyration = gyration
            gyrationDispersion = gyrationDispersion
            hand = hand
            magnetic = magnetic
            magneticDispersion = magneticDispersion
            muKind = muKind
            polder = polder
            polderDispersion = polderDispersion
        })
