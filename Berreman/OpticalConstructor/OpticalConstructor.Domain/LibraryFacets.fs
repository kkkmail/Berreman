namespace OpticalConstructor.Domain

open Berreman.Media
open Berreman.Dispersion
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.Facets
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.DispersionModels
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
///   • Dependent facets vanish entirely when inapplicable (`appliesTo`): the
///     physics facets read the entry's serializable VALUE TREES
///     (`MaterialComplexity` — `ConstantEpsValue` / `EpsDispersiveValue` /
///     `RhoWithDispValue` / `MuWithDispValue`), so a coded engine preset
///     (`complexity = None`: silicon, langasite, the vacuum spacer) offers
///     none of them — its physics is a closure, not data. The category and
///     constant-vs-dispersive facets read `category`/`properties` and apply
///     to every entry.
///   • Physics-derived vocabularies REUSE the editor's seams
///     (`availableGyrationClasses`, `gyrationClassLabel`, `Anisotropy`,
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

    /// The branch key of an anisotropy (the editor's `Anisotropy` DU).
    let anisotropyKey (anisotropy : Anisotropy) : DiscreteKey =
        match anisotropy with
        | Isotropic -> DiscreteKey "Isotropic"
        | Uniaxial -> DiscreteKey "Uniaxial"
        | Biaxial -> DiscreteKey "Biaxial"

    /// The branch key of the constant-vs-dispersive classification
    /// (`MaterialLibrary.MaterialDispersion`).
    let dispersionKey (dispersion : MaterialDispersion) : DiscreteKey =
        match dispersion with
        | ConstantMaterial -> DiscreteKey "Constant"
        | DispersiveMaterial -> DiscreteKey "Dispersive"

    /// The branch key of a transparency (the editor's `Transparency` DU).
    let transparencyKey (transparency : Transparency) : DiscreteKey =
        match transparency with
        | Transparent -> DiscreteKey "Transparent"
        | Absorbing -> DiscreteKey "Absorbing"

    /// The branch key of one per-axis/segment dispersion. A stored entry does
    /// NOT carry the ten `DispersionModel` kinds: the editor lowers every
    /// model through `toEpsAxis` at save (spec 0033), so the segments hold
    /// `EpsAxisDispersion` term data and the analytic kind is not recoverable
    /// (Sellmeier lowers to complex-ε terms, the transcendental four to an
    /// opaque closure); recognizing term shapes back into model kinds would
    /// re-derive physics (§D.0 forbids it). The facet therefore classifies
    /// the three storable shapes; `modelDispersionKey` (below) pins how each
    /// `DispersionModel` case maps onto this vocabulary.
    let dispersionModelKey (axis : EpsAxisDispersion) : DiscreteKey =
        match axis with
        | RealNK _ -> DiscreteKey "Real n/k terms"
        | ComplexEps _ -> DiscreteKey "Complex ε terms"
        | EpsAxisEvaluated _ -> DiscreteKey "Evaluated (transcendental)"

    /// The vocabulary key a `DispersionModel` case lands on once lowered
    /// (`toEpsAxis` — the same lowering the editor performs at save):
    /// `ConstantNK`/`Cauchy`/raw real-n/k `SumOfTerms` → real n/k terms;
    /// `Sellmeier`/`Lorentz`/`Drude`/raw complex-ε `SumOfTerms` → complex ε
    /// terms; the transcendental four (`TaucLorentz`/`GaussianOscillator`/
    /// `ForouhiBloomer`/`BrendelBormann`) → evaluated.
    let modelDispersionKey (model : DispersionModel) : DiscreteKey =
        dispersionModelKey (toEpsAxis model)

    /// The branch key of a handedness (`Berreman.Dispersion.Handedness`).
    let handednessKey (hand : Handedness) : DiscreteKey =
        match hand with
        | LeftHanded -> DiscreteKey "Left-handed"
        | RightHanded -> DiscreteKey "Right-handed"

    /// The branch key of a gyration symmetry class — the editor's picker
    /// label (`gyrationClassLabel`), so the facet names classes exactly as
    /// the Material editor does.
    let gyrationClassKey (gyration : GyrationClass<'g>) : DiscreteKey =
        DiscreteKey (gyrationClassLabel gyration)

    /// The class-key vocabulary the gyration facet can ever offer: the union
    /// of the classes `availableGyrationClasses` offers across the three
    /// anisotropies (§D.0 — physics-derived offers reuse the editor's seam,
    /// never re-derived or hand-listed).
    let offeredGyrationClassKeys : DiscreteKey list =
        [ Isotropic; Uniaxial; Biaxial ]
        |> List.collect availableGyrationClasses
        |> List.map gyrationClassKey
        |> List.distinct

    /// The branch key of the magnetic shape (the editor's `MuKind` DU).
    let muKindKey (muKind : MuKind) : DiscreteKey =
        match muKind with
        | ScalarMuKind -> DiscreteKey "Scalar"
        | GyromagneticMuKind -> DiscreteKey "Gyromagnetic"

    // =======================================================================
    // Value-tree readers (private): what a facet classifies, when present.
    // =======================================================================

    /// The anisotropy an eps VALUE TREE denotes — the shape is data
    /// (`ConstantEpsValue` / `EpsDispersiveValue` cases), no physics is
    /// re-derived. Note the classification is of the ENCODING: the seeded
    /// "Uniaxial crystal" built-in is stored as per-axis `BiaxialTransparent`
    /// values (`MaterialLibrary.fs` seed comment) and so classifies Biaxial.
    let anisotropyOf (complexity : MaterialComplexity) : Anisotropy =
        match complexity.eps with
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

    /// The transparency a CONSTANT eps value denotes (the editor's
    /// `Transparency` DU over the `ConstantEpsValue` case).
    let transparencyOf (constant : ConstantEpsValue) : Transparency =
        match constant with
        | IsotropicTransparent _ | UniaxialTransparent _ | BiaxialTransparent _ -> Transparent
        | IsotropicAbsorbing _ | UniaxialAbsorbing _ | BiaxialAbsorbing _ -> Absorbing

    /// The constant eps value tree of an entry, when it carries one.
    let private constantEpsOf (entry : MaterialEntry) : ConstantEpsValue option =
        match entry.complexity with
        | Some complexity ->
            match complexity.eps with
            | EpsWithoutDispValue constant -> Some constant
            | EpsWithDispValue _ -> None
        | None -> None

    /// The dispersive eps segment tree of an entry, when it carries one.
    let private dispersiveEpsOf (entry : MaterialEntry) : EpsDispersiveValue option =
        match entry.complexity with
        | Some complexity ->
            match complexity.eps with
            | EpsWithDispValue dispersive -> Some dispersive
            | EpsWithoutDispValue _ -> None
        | None -> None

    /// The optical-activity value tree of an entry, when it carries one.
    let private activeOf (entry : MaterialEntry) : RhoWithDispValue option =
        match entry.complexity with
        | Some complexity -> complexity.active
        | None -> None

    /// The magnetic (Polder μ) value tree of an entry, when it carries one.
    let private magneticOf (entry : MaterialEntry) : MuWithDispValue option =
        match entry.complexity with
        | Some complexity -> complexity.magnetic
        | None -> None

    /// Every per-axis/segment dispersion of a dispersive eps, in segment
    /// order — the symmetry decides how many axes each segment carries
    /// (isotropic 1, uniaxial 2, biaxial 3), exactly the slots the editor's
    /// `toComplexity` writes. The MULTI-VALUED source of the dispersion-model
    /// facet, so branch counts need not sum to the total.
    let private axisDispersions (dispersive : EpsDispersiveValue) : EpsAxisDispersion list =
        match dispersive with
        | IsotropicDispersive segments ->
            segments |> List.map (fun s -> s.dispersion)
        | UniaxialDispersive segments ->
            segments |> List.collect (fun s -> [ s.ordinaryDispersion; s.extraordinaryDispersion ])
        | BiaxialDispersive segments ->
            segments |> List.collect (fun s -> [ s.xDispersion; s.yDispersion; s.zDispersion ])

    let private applicableWhen (condition : bool) : Applicability =
        if condition then ApplicableAttribute else InapplicableAttribute

    // =======================================================================
    // The MATERIAL facet catalogue (over MaterialEntry).
    // =======================================================================

    /// Category — applies to every entry; the value set is corpus-derived
    /// (branches come from the filtered population), names resolved through
    /// the seeded catalogue (`standardCategories` via `categoryName`).
    let private materialCategoryDef : AttributeDef<MaterialEntry> =
        {
            key = materialCategoryKey
            name = "Category"
            kind = DiscreteAttribute
            appliesTo = fun _ -> ApplicableAttribute
            extract = fun entry -> [ DiscreteValue (DiscreteKey (categoryName entry.category)) ]
        }

    /// Anisotropy — Isotropic/Uniaxial/Biaxial from the eps value tree;
    /// inapplicable to the coded presets (no value tree to classify).
    let private materialAnisotropyDef : AttributeDef<MaterialEntry> =
        {
            key = materialAnisotropyKey
            name = "Anisotropy"
            kind = DiscreteAttribute
            appliesTo =
                fun entry ->
                    match entry.complexity with
                    | Some _ -> ApplicableAttribute
                    | None -> InapplicableAttribute
            extract =
                fun entry ->
                    match entry.complexity with
                    | Some complexity -> [ DiscreteValue (anisotropyKey (anisotropyOf complexity)) ]
                    | None -> []
        }

    /// Constant vs dispersive — from the engine `properties`
    /// (`materialDispersion`), so it applies to EVERY entry, coded presets
    /// included (silicon/langasite classify Dispersive, the vacuum spacer
    /// Constant).
    let private materialDispersionDef : AttributeDef<MaterialEntry> =
        {
            key = materialDispersionKey
            name = "Dispersion"
            kind = DiscreteAttribute
            appliesTo = fun _ -> ApplicableAttribute
            extract = fun entry -> [ DiscreteValue (dispersionKey (materialDispersion entry)) ]
        }

    /// Transparent vs absorbing — offered for CONSTANT materials only
    /// (operator, 003/Q9), from the `ConstantEpsValue` case. Both conditions
    /// gate it: the entry classifies `ConstantMaterial` AND carries a
    /// constant eps value tree — so the constant CODED preset (the vacuum
    /// spacer, `complexity = None`) does not offer it either.
    let private materialTransparencyDef : AttributeDef<MaterialEntry> =
        {
            key = materialTransparencyKey
            name = "Transparency"
            kind = DiscreteAttribute
            appliesTo =
                fun entry ->
                    match materialDispersion entry, constantEpsOf entry with
                    | ConstantMaterial, Some _ -> ApplicableAttribute
                    | ConstantMaterial, None | DispersiveMaterial, _ -> InapplicableAttribute
            extract =
                fun entry ->
                    match constantEpsOf entry with
                    | Some constant -> [ DiscreteValue (transparencyKey (transparencyOf constant)) ]
                    | None -> []
        }

    /// Dispersion model — MULTI-VALUED per axis/segment (a dispersive
    /// biaxial entry carries one classification per axis per segment, so
    /// counts need not sum to the total); offered only when the entry
    /// carries a dispersive eps segment tree. See `dispersionModelKey` for
    /// why the vocabulary is the three storable `EpsAxisDispersion` shapes
    /// rather than the ten analytic kinds.
    let private materialDispersionModelDef : AttributeDef<MaterialEntry> =
        {
            key = materialDispersionModelKey
            name = "Dispersion model"
            kind = DiscreteAttribute
            appliesTo = fun entry -> applicableWhen (dispersiveEpsOf entry |> Option.isSome)
            extract =
                fun entry ->
                    match dispersiveEpsOf entry with
                    | Some dispersive ->
                        axisDispersions dispersive
                        |> List.map (fun axis -> DiscreteValue (dispersionModelKey axis))
                    | None -> []
        }

    /// Optical-activity symmetry class — offered only when active; the
    /// branch keys are the editor picker's class labels and the offerable
    /// vocabulary is `offeredGyrationClassKeys` (via
    /// `availableGyrationClasses` — never re-derived). `gyrationClassKey` is
    /// generic over the component, so the constant and dispersive gyration
    /// facets classify identically.
    let private materialGyrationClassDef : AttributeDef<MaterialEntry> =
        {
            key = materialGyrationClassKey
            name = "Optical activity"
            kind = DiscreteAttribute
            appliesTo = fun entry -> applicableWhen (activeOf entry |> Option.isSome)
            extract =
                fun entry ->
                    match activeOf entry with
                    | Some (RhoWithoutDispValue gyrotropic) -> [ DiscreteValue (gyrationClassKey gyrotropic.gyration) ]
                    | Some (RhoWithDispValue gyrotropic) -> [ DiscreteValue (gyrationClassKey gyrotropic.gyration) ]
                    | None -> []
        }

    /// Handedness — offered only when active (the enantiomorph sign of the
    /// gyration tensor).
    let private materialHandednessDef : AttributeDef<MaterialEntry> =
        {
            key = materialHandednessKey
            name = "Handedness"
            kind = DiscreteAttribute
            appliesTo = fun entry -> applicableWhen (activeOf entry |> Option.isSome)
            extract =
                fun entry ->
                    match activeOf entry with
                    | Some (RhoWithoutDispValue gyrotropic) -> [ DiscreteValue (handednessKey gyrotropic.hand) ]
                    | Some (RhoWithDispValue gyrotropic) -> [ DiscreteValue (handednessKey gyrotropic.hand) ]
                    | None -> []
        }

    /// Magnetic — scalar vs gyromagnetic, offered only when magnetic. The
    /// dispersive Polder case is always the full tensor (the engine's
    /// `MuWithDispValue` carries no scalar dispersive case), so it
    /// classifies Gyromagnetic.
    let private materialMagneticDef : AttributeDef<MaterialEntry> =
        {
            key = materialMagneticKey
            name = "Magnetic"
            kind = DiscreteAttribute
            appliesTo = fun entry -> applicableWhen (magneticOf entry |> Option.isSome)
            extract =
                fun entry ->
                    match magneticOf entry with
                    | Some (MuWithoutDispValue (ScalarMu _)) -> [ DiscreteValue (muKindKey ScalarMuKind) ]
                    | Some (MuWithoutDispValue (GyromagneticMu _)) -> [ DiscreteValue (muKindKey GyromagneticMuKind) ]
                    | Some (MuWithDispValue _) -> [ DiscreteValue (muKindKey GyromagneticMuKind) ]
                    | None -> []
        }

    /// The MATERIAL facet catalogue, in default representation order.
    let materialFacets : AttributeDef<MaterialEntry> list =
        [
            materialCategoryDef
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

    /// The branch key of a library entry's kind.
    let entryKindKey (entry : LibraryEntry) : DiscreteKey =
        match entry with
        | SampleItem _ -> DiscreteKey "Sample"
        | SourceItem _ -> DiscreteKey "Source"
        | DetectorItem _ -> DiscreteKey "Detector"
        | PolarizerItem _ -> DiscreteKey "Polarizer"

    /// The branch key of a polarizer category (spec 0038 Part F — `PolarizerCategory` is the
    /// polarizer facet value; the DU's own label doubles as the branch label).
    let polarizerCategoryKey (category : PolarizerCategory) : DiscreteKey =
        DiscreteKey category.label

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

    /// The film layers of a structure, a `Repeated` group's cell counted
    /// once — repetition adds no new material and no new thickness value
    /// (the engine dedups per item anyway).
    let private filmLayers (structure : SampleStructure) : SampleLayer list =
        structure.films
        |> List.collect (fun item ->
            match item with
            | SingleLayer layer -> [ layer ]
            | Repeated group -> group.cell)

    /// The display name a material id resolves to through the corpus.
    let private materialName (materials : MaterialEntry list) (id : MaterialId) : string option =
        materials |> List.tryFind (fun m -> m.id = id) |> Option.map (fun m -> m.name)

    let private isApplicableTo (def : AttributeDef<MaterialEntry>) (entry : MaterialEntry) : bool =
        match def.appliesTo entry with
        | ApplicableAttribute -> true
        | InapplicableAttribute -> false

    /// A layer's finite thickness as a length in NANOMETERS (the magnitude
    /// unit step 010's buckets label with) through the sole `Units` seam;
    /// a semi-infinite layer has no bucketable magnitude.
    let private thicknessNm (layer : SampleLayer) : double option =
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
            name = "Kind"
            kind = DiscreteAttribute
            appliesTo = fun _ -> ApplicableAttribute
            extract = fun entry -> [ DiscreteValue (entryKindKey entry) ]
        }

    /// The polarizer-category facet (spec 0038 Part F): `PolarizerCategory` as the facet value,
    /// offered only for polarizer entries — every other kind is inapplicable, so the facet vanishes
    /// entirely over a polarizer-free population.
    let private polarizerCategoryDef : AttributeDef<LibraryEntry> =
        {
            key = polarizerCategoryFacetKey
            name = "Polarizer type"
            kind = DiscreteAttribute
            appliesTo =
                fun entry ->
                    match entry with
                    | PolarizerItem _ -> ApplicableAttribute
                    | SampleItem _ | SourceItem _ | DetectorItem _ -> InapplicableAttribute
            extract =
                fun entry ->
                    match entry with
                    | PolarizerItem p -> [ DiscreteValue (polarizerCategoryKey p.category) ]
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
                        applicableWhen (constituentEntries materials sample |> List.exists (isApplicableTo def))
                    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> InapplicableAttribute
            extract =
                fun entry ->
                    match entry with
                    | SampleItem sample ->
                        constituentEntries materials sample
                        |> List.filter (isApplicableTo def)
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
            name = "Substrate material"
            kind = DiscreteAttribute
            appliesTo =
                fun entry ->
                    match entry with
                    | SampleItem sample ->
                        match sample.substrate with
                        | Plate -> ApplicableAttribute
                        | ThinFilm -> InapplicableAttribute
                    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> InapplicableAttribute
            extract =
                fun entry ->
                    match entry with
                    | SampleItem sample ->
                        sample.structure.substrate
                        |> Option.toList
                        |> List.choose (fun layer -> materialName materials layer.materialId.materialId)
                        |> List.map (fun name -> DiscreteValue (DiscreteKey name))
                    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> []
        }

    /// 'Has thin films' — derived from `structure.films` non-empty,
    /// INDEPENDENT of `SubstrateKind` (operator, 003/Q11): a Plate can carry
    /// films in the model, so the facet applies to every sample.
    let private hasThinFilmsDef : AttributeDef<LibraryEntry> =
        {
            key = sampleHasThinFilmsKey
            name = "Thin films"
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
            name = "Film material"
            kind = DiscreteAttribute
            appliesTo =
                fun entry ->
                    match entry with
                    | SampleItem sample -> applicableWhen (not (List.isEmpty sample.structure.films))
                    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> InapplicableAttribute
            extract =
                fun entry ->
                    match entry with
                    | SampleItem sample ->
                        filmLayers sample.structure
                        |> List.choose (fun layer -> materialName materials layer.materialId.materialId)
                        |> List.distinct
                        |> List.map (fun name -> DiscreteValue (DiscreteKey name))
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
            name = "Film thickness"
            kind = NumericAttribute
            appliesTo =
                fun entry ->
                    match entry with
                    | SampleItem sample -> applicableWhen (not (List.isEmpty sample.structure.films))
                    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> InapplicableAttribute
            extract =
                fun entry ->
                    match entry with
                    | SampleItem sample ->
                        filmLayers sample.structure
                        |> List.choose thicknessNm
                        |> List.map NumericValue
                    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> []
        }

    /// The LIBRARY-ENTRY facet catalogue over a material corpus (the corpus
    /// resolves constituent ids to entries and display names), in default
    /// representation order: the kind facet, the polarizer-category facet
    /// (spec 0038 Part F — the non-sample facet, right after kind), every
    /// material facet lifted to samples as the distinct union over
    /// constituent materials, then the sample-structural facets.
    let libraryFacets (materials : MaterialEntry list) : AttributeDef<LibraryEntry> list =
        [ entryKindDef; polarizerCategoryDef ]
        @ (materialFacets |> List.map (liftToSamples materials))
        @ [
            substrateMaterialDef materials
            hasThinFilmsDef
            filmMaterialDef materials
            filmThicknessDef
        ]
