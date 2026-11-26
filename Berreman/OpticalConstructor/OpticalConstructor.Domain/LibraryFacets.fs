namespace OpticalConstructor.Domain

open Berreman.Media
open Berreman.Dispersion
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.Facets
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.DispersionModels
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.MaterialComplexityEditor

/// Spec 0038 Part D (step 011): the two CONCRETE facet catalogues as data —
/// the MATERIAL facets over `MaterialEntry` and the LIBRARY-ENTRY facets over
/// `LibraryEntry` — on top of step 009's generic engine (`Facets`) and step
/// 010's numeric buckets (`FacetBuckets`). Pure, Avalonia-free; Part E's
/// `FacetedTreeControls` renders these catalogues in both windows.
///
/// Discipline notes:
///   • Facet KEYS are stable ids; discrete VALUE keys double as branch labels
///     (step 009), so every vocabulary string here is human-readable.
///   • The four single-valued classifiers — Category, Anisotropy, constant-vs-
///     dispersive (Dispersion) and Transparency — apply to EVERY entry (TOTAL
///     facets, spec 0040 Part B / operator 010/Q2): Category/Dispersion read
///     `category`/`properties`; Anisotropy reads the eps VALUE TREE case; and
///     Transparency classifies a constant eps value or lands a dispersive
///     material under Absorbing. So over any population these four partition it —
///     their branch counts sum to the item count. The remaining DEPENDENT facets
///     vanish entirely when inapplicable (`appliesTo`): the dispersion-model,
///     optical-activity and magnetic facets read the serializable VALUE TREES
///     (`MaterialComplexity` — `EpsDispersiveValue` / `RhoWithDispValue` /
///     `MuWithDispValue`), so an entry that carries no such sub-tree offers none
///     of them — its absence is data, not a closure.
///   • Physics-derived vocabularies REUSE the editor's seams
///     (`availableGyrationClasses`, `GyrationClass.classLabel`, `Anisotropy`,
///     `Transparency`, `MuKind`) — never re-derived (§D.0).
module LibraryFacets =

    // =======================================================================
    // Material facet keys (stable attribute ids).
    // =======================================================================

    let materialCategoryKey : AttributeKey = AttributeKey "material-category"
    let materialAnisotropyKey : AttributeKey = AttributeKey "material-anisotropy"
    let materialDispersionKey : AttributeKey = AttributeKey "material-dispersion"
    let materialTransparencyKey : AttributeKey = AttributeKey "material-transparency"
    let materialDispersionModelKey : AttributeKey = AttributeKey "material-dispersion-model"
    let materialGyrationClassKey : AttributeKey = AttributeKey "material-gyration-class"
    let materialHandednessKey : AttributeKey = AttributeKey "material-handedness"
    let materialMagneticKey : AttributeKey = AttributeKey "material-magnetic"

    // =======================================================================
    // Discrete-value vocabularies (each key doubles as the branch label).
    // =======================================================================

    type CategoryName with

        member this.facetKey : DiscreteKey = DiscreteKey this.value

    type Anisotropy with

        /// The branch key of an anisotropy (the editor's `Anisotropy` DU).
        member this.facetKey : DiscreteKey =
            match this with
            | Isotropic -> DiscreteKey "Isotropic"
            | Uniaxial -> DiscreteKey "Uniaxial"
            | Biaxial -> DiscreteKey "Biaxial"

    type MaterialDispersion with

        /// The branch key of the constant-vs-dispersive classification.
        member this.facetKey : DiscreteKey =
            match this with
            | ConstantMaterial -> DiscreteKey "Constant"
            | DispersiveMaterial -> DiscreteKey "Dispersive"

    type Transparency with

        /// The branch key of a transparency (the editor's `Transparency` DU).
        member this.facetKey : DiscreteKey =
            match this with
            | Transparent -> DiscreteKey "Transparent"
            | Absorbing -> DiscreteKey "Absorbing"

    type EpsAxisDispersion with

        /// The branch key of one per-axis/segment dispersion. A stored entry does
        /// NOT carry the ten `DispersionModel` kinds: the editor lowers every
        /// model through `toEpsAxis` at save (spec 0033), so the segments hold
        /// `EpsAxisDispersion` term data and the analytic kind is not recoverable
        /// (Sellmeier lowers to complex-ε terms, the transcendental four to their
        /// laws); recognizing term shapes back into model kinds would re-derive
        /// physics (§D.0 forbids it). The facet therefore classifies the storable
        /// shapes: the two term-data shapes, a tabulated n,k table (`TabulatedNK`,
        /// spec 0050 step 2), and one "Evaluated (transcendental)" key shared by the
        /// four transcendental laws (steps 5 and 7) and the coded presets' published
        /// laws (step 8); `DispersionModel.facetKey` (below) pins how each
        /// `DispersionModel` case maps onto this vocabulary.
        member this.facetKey : DiscreteKey =
            match this with
            | RealNK _ -> DiscreteKey "Real n/k terms"
            | ComplexEps _ -> DiscreteKey "Complex ε terms"
            | TabulatedNK _ -> DiscreteKey "Tabulated n/k"
            | TaucLorentzAxis _
            | GaussianOscillatorAxis _
            | ForouhiBloomerAxis _
            | BrendelBormannAxis _
            | PublishedLawAxis _ -> DiscreteKey "Evaluated (transcendental)"

    type DispersionModel with

        /// The vocabulary key the model lands on once lowered
        /// (`toEpsAxis` — the same lowering the editor performs at save):
        /// `ConstantNK`/`Cauchy`/raw real-n/k `SumOfTerms` → real n/k terms;
        /// `Sellmeier`/`Lorentz`/`Drude`/raw complex-ε `SumOfTerms` → complex ε
        /// terms; the transcendental four (`TaucLorentz`/`GaussianOscillator`/
        /// `ForouhiBloomer`/`BrendelBormann`) → their laws' "Evaluated (transcendental)" key.
        member this.facetKey : DiscreteKey = this.toEpsAxis.facetKey

    type Handedness with

        /// The branch key of a handedness (`Berreman.Dispersion.Handedness`).
        member this.facetKey : DiscreteKey =
            match this with
            | LeftHanded -> DiscreteKey "Left-handed"
            | RightHanded -> DiscreteKey "Right-handed"

    type GyrationClass<'g> with

        /// The branch key of a gyration symmetry class — the editor's picker
        /// label (`classLabel`), so the facet names classes exactly as the
        /// Material editor does.
        member this.facetKey : DiscreteKey = DiscreteKey this.classLabel

    /// The class-key vocabulary the gyration facet can ever offer: the union
    /// of the classes `availableGyrationClasses` offers across the three
    /// anisotropies (§D.0 — physics-derived offers reuse the editor's seam,
    /// never re-derived or hand-listed).
    let offeredGyrationClassKeys : DiscreteKey list =
        [ Isotropic; Uniaxial; Biaxial ]
        |> List.collect availableGyrationClasses
        |> List.map (fun gyration -> gyration.facetKey)
        |> List.distinct

    type MuKind with

        /// The branch key of the magnetic shape (the editor's `MuKind` DU).
        member this.facetKey : DiscreteKey =
            match this with
            | ScalarMuKind -> DiscreteKey "Scalar"
            | GyromagneticMuKind -> DiscreteKey "Gyromagnetic"

    // =======================================================================
    // Value-tree readers: what a facet classifies, when present.
    // =======================================================================

    type MaterialComplexity with

        /// Classifies the eps encoding: the Built-in catalogue's "Uniaxial crystal" uses
        /// per-axis BiaxialTransparent values and therefore classifies Biaxial (spec 0050 §3.3).
        member this.anisotropy : Anisotropy =
            match this.eps with
            | EpsWithoutDispValue constant ->
                match constant with
                | IsotropicTransparent _ | IsotropicAbsorbing _ -> Isotropic
                | UniaxialTransparent _ | UniaxialAbsorbing _ -> Uniaxial
                | BiaxialTransparent _ | BiaxialAbsorbing _ -> Biaxial
            | EpsWithDispValue dispersive ->
                match dispersive with
                | IsotropicDispersive _ -> Isotropic
                | UniaxialDispersive _ -> Uniaxial
                | BiaxialDispersive _ -> Biaxial

    type CrystalParameters with

        /// The crystal's anisotropy: a triclinic crystal, absorbing or not, is biaxial (spec 0050 S11.1).
        member this.anisotropy : Anisotropy =
            match this with
            | UniaxialCrystal _ -> Uniaxial
            | TriclinicCrystal _
            | AbsorbingTriclinicCrystal _ -> Biaxial

        /// The crystal's transparency: only the absorbing triclinic record carries an ε″ (spec 0050 S11.1).
        member this.transparency : Transparency =
            match this with
            | UniaxialCrystal _
            | TriclinicCrystal _ -> Transparent
            | AbsorbingTriclinicCrystal _ -> Absorbing

    type MaterialPhysics with

        /// The anisotropy the physics denotes (spec 0050 S11.1).
        member this.anisotropy : Anisotropy =
            match this with
            | ComplexityPhysics complexity -> complexity.anisotropy
            | CrystalParameterPhysics crystal -> crystal.anisotropy

    type ConstantEpsValue with

        /// The transparency the CONSTANT eps value denotes (the editor's
        /// `Transparency` DU over the `ConstantEpsValue` case).
        member this.transparency : Transparency =
            match this with
            | IsotropicTransparent _ | UniaxialTransparent _ | BiaxialTransparent _ -> Transparent
            | IsotropicAbsorbing _ | UniaxialAbsorbing _ | BiaxialAbsorbing _ -> Absorbing

    /// A crystal's physics is parameters, not the ladder's value trees, so every reader below answers
    /// `None` for it (spec 0050 S11.1).
    type MaterialEntry with

        /// The constant eps value tree of the entry, when it carries one.
        member private entry.constantEpsOpt : ConstantEpsValue option =
            match entry.physics with
            | ComplexityPhysics complexity ->
                match complexity.eps with
                | EpsWithoutDispValue constant -> Some constant
                | EpsWithDispValue _ -> None
            | CrystalParameterPhysics _ -> None

        /// The dispersive eps segment tree of the entry, when it carries one.
        member private entry.dispersiveEpsOpt : EpsDispersiveValue option =
            match entry.physics with
            | ComplexityPhysics complexity ->
                match complexity.eps with
                | EpsWithDispValue dispersive -> Some dispersive
                | EpsWithoutDispValue _ -> None
            | CrystalParameterPhysics _ -> None

        /// The optical-activity value tree of the entry, when it carries one.
        member private entry.activeOpt : RhoWithDispValue option =
            match entry.physics with
            | ComplexityPhysics complexity -> complexity.active
            | CrystalParameterPhysics _ -> None

        /// The magnetic (Polder μ) value tree of the entry, when it carries one.
        member private entry.magneticOpt : MuWithDispValue option =
            match entry.physics with
            | ComplexityPhysics complexity -> complexity.magnetic
            | CrystalParameterPhysics _ -> None

    // =======================================================================
    // The MATERIAL facet catalogue (over MaterialEntry).
    // =======================================================================

    /// Category applies to every entry; names resolve through the categories given (spec 0050 S1.7).
    let private materialCategoryDef (categories : MaterialCategory list) : AttributeDef<MaterialEntry> =
        {
            key = materialCategoryKey
            name = (FacetCaption "Category")
            kind = DiscreteAttribute
            appliesTo = fun _ -> ApplicableAttribute
            extract = fun entry -> [ DiscreteValue (entry.category.categoryName categories).facetKey ]
        }

    /// Anisotropy — Isotropic/Uniaxial/Biaxial from the physics. TOTAL
    /// (spec 0040 Part B / operator 010/Q2): every entry carries a physics, so the facet
    /// applies to ALL of them and `extract` reads `physics.anisotropy` for each (spec 0050 S11.1).
    let private materialAnisotropyDef : AttributeDef<MaterialEntry> =
        {
            key = materialAnisotropyKey
            name = (FacetCaption "Anisotropy")
            kind = DiscreteAttribute
            appliesTo = fun _ -> ApplicableAttribute
            extract = fun entry -> [ DiscreteValue entry.physics.anisotropy.facetKey ]
        }

    /// Constant vs dispersive — from the engine `properties`
    /// (`MaterialEntry.dispersion`), so it applies to EVERY entry, coded presets
    /// included (silicon/langasite classify Dispersive, the vacuum spacer
    /// Constant).
    let private materialDispersionDef : AttributeDef<MaterialEntry> =
        {
            key = materialDispersionKey
            name = (FacetCaption "Dispersion")
            kind = DiscreteAttribute
            appliesTo = fun _ -> ApplicableAttribute
            extract = fun entry -> [ DiscreteValue entry.dispersion.facetKey ]
        }

    /// Transparent vs absorbing — TOTAL (spec 0040 Part B / operator 010/Q2),
    /// classified via `MaterialEntry.dispersion` (the constant-vs-dispersive
    /// discriminator): a CONSTANT material reads Transparent/Absorbing from its
    /// `ConstantEpsValue` case (`ConstantEpsValue.transparency`), while every DISPERSIVE
    /// material lands under Absorbing (its wavelength-dependent eps is a segment
    /// tree, not a single value, so it cannot be a constant transparent). The
    /// defensive constant-without-value-tree case also folds to Absorbing, so
    /// the facet yields exactly one branch value for every entry. A crystal reads
    /// its own transparency (spec 0050 S11.1).
    let private materialTransparencyDef : AttributeDef<MaterialEntry> =
        {
            key = materialTransparencyKey
            name = (FacetCaption "Transparency")
            kind = DiscreteAttribute
            appliesTo = fun _ -> ApplicableAttribute
            extract =
                fun entry ->
                    match entry.physics with
                    | ComplexityPhysics _ ->
                        match entry.dispersion, entry.constantEpsOpt with
                        | ConstantMaterial, Some constant -> [ DiscreteValue constant.transparency.facetKey ]
                        | ConstantMaterial, None
                        | DispersiveMaterial, _ -> [ DiscreteValue Absorbing.facetKey ]
                    | CrystalParameterPhysics crystal -> [ DiscreteValue crystal.transparency.facetKey ]
        }

    /// Dispersion model — MULTI-VALUED per axis/segment (a dispersive
    /// biaxial entry carries one classification per axis per segment, so
    /// counts need not sum to the total); offered only when the entry
    /// carries a dispersive eps segment tree. See `EpsAxisDispersion.facetKey` for
    /// why the vocabulary is the four storable `EpsAxisDispersion` shapes
    /// rather than the ten analytic kinds.
    let private materialDispersionModelDef : AttributeDef<MaterialEntry> =
        {
            key = materialDispersionModelKey
            name = (FacetCaption "Dispersion model")
            kind = DiscreteAttribute
            appliesTo = fun entry -> Applicability.ofBool (entry.dispersiveEpsOpt |> Option.isSome)
            extract =
                fun entry ->
                    match entry.dispersiveEpsOpt with
                    | Some dispersive ->
                        dispersive.axisDispersions
                        |> List.map (fun axis -> DiscreteValue axis.facetKey)
                    | None -> []
        }

    /// Optical-activity symmetry class — offered only when active; the
    /// branch keys are the editor picker's class labels and the offerable
    /// vocabulary is `offeredGyrationClassKeys` (via
    /// `availableGyrationClasses` — never re-derived). `GyrationClass.facetKey` is
    /// generic over the component, so the constant and dispersive gyration
    /// facets classify identically. A published gyration law (spec 0050 step 10)
    /// is active but has no editor class, so it extracts no value.
    let private materialGyrationClassDef : AttributeDef<MaterialEntry> =
        {
            key = materialGyrationClassKey
            name = (FacetCaption "Optical activity")
            kind = DiscreteAttribute
            appliesTo = fun entry -> Applicability.ofBool (entry.activeOpt |> Option.isSome)
            extract =
                fun entry ->
                    match entry.activeOpt with
                    | Some (RhoWithoutDispValue gyrotropic) -> [ DiscreteValue gyrotropic.gyration.facetKey ]
                    | Some (RhoWithDispValue gyrotropic) -> [ DiscreteValue gyrotropic.gyration.facetKey ]
                    | Some (PublishedLawRho _)
                    | None -> []
        }

    /// Handedness — offered only when active (the enantiomorph sign of the
    /// gyration tensor). A published gyration law (spec 0050 step 10) carries no
    /// handedness field, so it extracts no value.
    let private materialHandednessDef : AttributeDef<MaterialEntry> =
        {
            key = materialHandednessKey
            name = (FacetCaption "Handedness")
            kind = DiscreteAttribute
            appliesTo = fun entry -> Applicability.ofBool (entry.activeOpt |> Option.isSome)
            extract =
                fun entry ->
                    match entry.activeOpt with
                    | Some (RhoWithoutDispValue gyrotropic) -> [ DiscreteValue gyrotropic.hand.facetKey ]
                    | Some (RhoWithDispValue gyrotropic) -> [ DiscreteValue gyrotropic.hand.facetKey ]
                    | Some (PublishedLawRho _)
                    | None -> []
        }

    /// Magnetic — scalar vs gyromagnetic, offered only when magnetic. The
    /// dispersive Polder case is always the full tensor (the engine's
    /// `MuWithDispValue` carries no scalar dispersive case), so it
    /// classifies Gyromagnetic.
    let private materialMagneticDef : AttributeDef<MaterialEntry> =
        {
            key = materialMagneticKey
            name = (FacetCaption "Magnetic")
            kind = DiscreteAttribute
            appliesTo = fun entry -> Applicability.ofBool (entry.magneticOpt |> Option.isSome)
            extract =
                fun entry ->
                    match entry.magneticOpt with
                    | Some (MuWithoutDispValue (ScalarMu _)) -> [ DiscreteValue ScalarMuKind.facetKey ]
                    | Some (MuWithoutDispValue (GyromagneticMu _)) -> [ DiscreteValue GyromagneticMuKind.facetKey ]
                    | Some (MuWithDispValue _) -> [ DiscreteValue GyromagneticMuKind.facetKey ]
                    | None -> []
        }

    /// The material facets in default order, with category names resolved through the categories given.
    let materialFacets (categories : MaterialCategory list) : AttributeDef<MaterialEntry> list =
        [
            materialCategoryDef categories
            materialAnisotropyDef
            materialDispersionDef
            materialTransparencyDef
            materialDispersionModelDef
            materialGyrationClassDef
            materialHandednessDef
            materialMagneticDef
        ]

    // =======================================================================
    // Library-entry facet keys.
    // =======================================================================

    let entryKindFacetKey : AttributeKey = AttributeKey "entry-kind"
    let polarizerCategoryFacetKey : AttributeKey = AttributeKey "polarizer-category"
    let sampleSubstrateMaterialKey : AttributeKey = AttributeKey "sample-substrate-material"
    let sampleHasThinFilmsKey : AttributeKey = AttributeKey "sample-has-thin-films"
    let sampleFilmMaterialKey : AttributeKey = AttributeKey "sample-film-material"
    let sampleFilmThicknessKey : AttributeKey = AttributeKey "sample-film-thickness"

    type LibraryEntry with

        /// The branch key of the entry's kind.
        member this.kindFacetKey : DiscreteKey =
            match this with
            | SampleItem _ -> DiscreteKey "Sample"
            | SourceItem _ -> DiscreteKey "Source"
            | DetectorItem _ -> DiscreteKey "Detector"
            | PolarizerItem _ -> DiscreteKey "Polarizer"

    type PolarizerCategory with

        /// The branch key of a polarizer category (spec 0038 Part F — `PolarizerCategory` is the
        /// polarizer facet value; the DU's own label doubles as the branch label).
        member this.facetKey : DiscreteKey = DiscreteKey this.label

    type EntryName with

        /// A material's display name as the branch key of a material facet.
        member this.facetKey : DiscreteKey = DiscreteKey this.value

    /// The two vocabulary keys of the thin-films facet.
    let hasThinFilmsValue : DiscreteKey = DiscreteKey "Has thin films"
    let noThinFilmsValue : DiscreteKey = DiscreteKey "No thin films"

    // =======================================================================
    // Sample-structure readers (private).
    // =======================================================================

    /// Every constituent material ENTRY of a sample: the film layers, the
    /// substrate plate, and the lower half-space
    /// (`SampleStructure.referencedMaterials` — the existing constituents
    /// seam), resolved through the supplied material corpus. An id the
    /// corpus does not hold contributes nothing — it cannot be classified.
    let private constituentEntries (materials : MaterialEntry list) (sample : Sample) : MaterialEntry list =
        sample.structure.referencedMaterials
        |> Set.toList
        |> List.choose (fun id -> materials |> List.tryFind (fun m -> m.id = id))

    type SampleStructure with

        /// The film layers of the structure, a `Repeated` group's cell counted
        /// once — repetition adds no new material and no new thickness value
        /// (the engine dedups per item anyway).
        member private structure.facetFilmLayers : SampleLayer list =
            structure.films
            |> List.collect (fun item ->
                match item with
                | SingleLayer layer -> [ layer ]
                | Repeated group -> group.cell)

    /// The display name a material id resolves to through the corpus.
    let private materialName (materials : MaterialEntry list) (id : MaterialId) : EntryName option =
        materials |> List.tryFind (fun m -> m.id = id) |> Option.map (fun m -> m.name)

    type SampleLayer with

        /// The layer's finite thickness as a length in NANOMETERS (the magnitude
        /// unit step 010's buckets label with) through the sole `Units` seam;
        /// a semi-infinite layer has no bucketable magnitude.
        member private layer.thicknessMagnitude : UnitMagnitude option =
            match layer.thickness with
            | Thickness.Thickness meters -> Some (fromMeters Nanometer meters)
            | Thickness.Infinity -> None

    // =======================================================================
    // The LIBRARY-ENTRY facet catalogue (over LibraryEntry).
    // =======================================================================

    /// The kind facet — always offered, over the whole entry corpus.
    let private entryKindDef : AttributeDef<LibraryEntry> =
        {
            key = entryKindFacetKey
            name = (FacetCaption "Kind")
            kind = DiscreteAttribute
            appliesTo = fun _ -> ApplicableAttribute
            extract = fun entry -> [ DiscreteValue entry.kindFacetKey ]
        }

    /// The polarizer-category facet (spec 0038 Part F): `PolarizerCategory` as the facet value,
    /// offered only for polarizer entries — every other kind is inapplicable, so the facet vanishes
    /// entirely over a polarizer-free population.
    let private polarizerCategoryDef : AttributeDef<LibraryEntry> =
        {
            key = polarizerCategoryFacetKey
            name = (FacetCaption "Polarizer type")
            kind = DiscreteAttribute
            appliesTo =
                fun entry ->
                    match entry with
                    | PolarizerItem _ -> ApplicableAttribute
                    | SampleItem _ | SourceItem _ | DetectorItem _ -> InapplicableAttribute
            extract =
                fun entry ->
                    match entry with
                    | PolarizerItem p -> [ DiscreteValue p.category.facetKey ]
                    | SampleItem _ | SourceItem _ | DetectorItem _ -> []
        }

    /// One material facet lifted to samples (§7.4): applicable when ANY
    /// constituent material has the facet applicable, and extraction is the
    /// DISTINCT UNION over the applicable constituents — any constituent
    /// matches. Non-sample entries never offer a material facet. The lifted
    /// def keeps the material facet's key and name.
    let private liftToSamples (materials : MaterialEntry list) (def : AttributeDef<MaterialEntry>) : AttributeDef<LibraryEntry> =
        {
            key = def.key
            name = def.name
            kind = def.kind
            appliesTo =
                fun entry ->
                    match entry with
                    | SampleItem sample ->
                        Applicability.ofBool (not (List.isEmpty (def.applicableItems (constituentEntries materials sample))))
                    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> InapplicableAttribute
            extract =
                fun entry ->
                    match entry with
                    | SampleItem sample ->
                        def.applicableItems (constituentEntries materials sample)
                        |> List.collect def.extract
                        |> List.distinct
                    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> []
        }

    /// Substrate material — offered only for the Plate geometry (operator,
    /// 003/Q10; the Wedge geometry was removed, spec 0038 comment 007): the
    /// `structure.substrate` layer's material, keyed by its display name
    /// through the corpus.
    let private substrateMaterialDef (materials : MaterialEntry list) : AttributeDef<LibraryEntry> =
        {
            key = sampleSubstrateMaterialKey
            name = (FacetCaption "Substrate material")
            kind = DiscreteAttribute
            appliesTo =
                fun entry ->
                    match entry with
                    | SampleItem sample ->
                        match sample.substrate with
                        | ThickPlate -> ApplicableAttribute
                        | ThinFilm
                        | Mirror -> InapplicableAttribute
                    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> InapplicableAttribute
            extract =
                fun entry ->
                    match entry with
                    | SampleItem sample ->
                        sample.structure.substrate
                        |> Option.toList
                        |> List.choose (fun layer -> materialName materials layer.materialId.materialId)
                        |> List.map (fun name -> DiscreteValue name.facetKey)
                    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> []
        }

    /// 'Has thin films' — derived from `structure.films` non-empty,
    /// INDEPENDENT of `SubstrateKind` (operator, 003/Q11): a Plate can carry
    /// films in the model, so the facet applies to every sample.
    let private hasThinFilmsDef : AttributeDef<LibraryEntry> =
        {
            key = sampleHasThinFilmsKey
            name = (FacetCaption "Thin films")
            kind = DiscreteAttribute
            appliesTo =
                fun entry ->
                    match entry with
                    | SampleItem _ -> ApplicableAttribute
                    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> InapplicableAttribute
            extract =
                fun entry ->
                    match entry with
                    | SampleItem sample ->
                        match sample.structure.films with
                        | [] -> [ DiscreteValue noThinFilmsValue ]
                        | _ -> [ DiscreteValue hasThinFilmsValue ]
                    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> []
        }

    /// Film material(s) — corpus-derived display names, MULTI-VALUED (one
    /// per distinct film material; multiselect is the engine's discrete
    /// key-set OR within the facet), name-sorted by the engine's structural
    /// branch sort. Offered only when the sample carries films.
    let private filmMaterialDef (materials : MaterialEntry list) : AttributeDef<LibraryEntry> =
        {
            key = sampleFilmMaterialKey
            name = (FacetCaption "Film material")
            kind = DiscreteAttribute
            appliesTo =
                fun entry ->
                    match entry with
                    | SampleItem sample -> Applicability.ofBool (not (List.isEmpty sample.structure.films))
                    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> InapplicableAttribute
            extract =
                fun entry ->
                    match entry with
                    | SampleItem sample ->
                        sample.structure.facetFilmLayers
                        |> List.choose (fun layer -> materialName materials layer.materialId.materialId)
                        |> List.distinct
                        |> List.map (fun name -> DiscreteValue name.facetKey)
                    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> []
        }

    /// Per-film thickness — a `NumericAttribute` over PER-LAYER thicknesses
    /// in nm (operator, 003/Q12: per-layer, not period or total; a
    /// `Repeated` cell's layers count once). Step 010's `bucketsFor` buckets
    /// these magnitudes over the currently constrained population, and a
    /// bucket applies as an ordinary `NumericRangeSelection` chip.
    let private filmThicknessDef : AttributeDef<LibraryEntry> =
        {
            key = sampleFilmThicknessKey
            name = (FacetCaption "Film thickness")
            kind = NumericAttribute
            appliesTo =
                fun entry ->
                    match entry with
                    | SampleItem sample -> Applicability.ofBool (not (List.isEmpty sample.structure.films))
                    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> InapplicableAttribute
            extract =
                fun entry ->
                    match entry with
                    | SampleItem sample ->
                        sample.structure.facetFilmLayers
                        |> List.choose (fun layer -> layer.thicknessMagnitude)
                        |> List.map NumericValue
                    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> []
        }

    /// Library facets in default order: kind, polarizer category, lifted material facets, then sample structure
    /// (spec 0038 Part F). Constituent entries resolve through materials; category names through categories
    /// (spec 0050 S1.7).
    let libraryFacets (categories : MaterialCategory list) (materials : MaterialEntry list) : AttributeDef<LibraryEntry> list =
        [ entryKindDef; polarizerCategoryDef ]
        @ (materialFacets categories |> List.map (liftToSamples materials))
        @ [
            substrateMaterialDef materials
            hasThinFilmsDef
            filmMaterialDef materials
            filmThicknessDef
        ]
