namespace OpticalConstructor.Domain

open System
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalProperties.Dispersive
open OpticalProperties.Standard
open OpticalProperties.Active

/// The searchable, categorised materials library aggregate (§D.8). A `MaterialEntry`
/// pairs display metadata with the engine's `OpticalPropertiesWithDisp` (produced by
/// D.6/D.7); the `MaterialLibrary` holds the entry list plus pure linear search
/// (no indexing/caching — out of scope). The by-id `resolveMaterial` is the single
/// resolution seam Part B §B.6 (slice 005) and Part J §J.4 (slice 014) delegate to.
/// Built-in entries REUSE the already-coded engine presets (`siliconOpticalProperties`,
/// `langasiteOpticalProperties`, the `Standard` glass/crystal presets) — no dispersion
/// is re-derived here.
module MaterialLibrary =

    /// Elevated material-entry identity (spec 0033 step 002): a Guid-backed single-case DU — no raw
    /// string material id appears in a domain record or public signature. `create` MINTS a fresh id
    /// (imports, user-authored entries); the built-in seeds parse FIXED literal Guids (`MaterialIds`)
    /// so identity is deterministic across runs; `tryCreate` parses the Guid string form at a genuine
    /// IO boundary (library JSON, the drag payload) — an unparsable string is `None`, never a throw
    /// (no legacy string-id path).
    type MaterialId =
        | MaterialId of Guid

        member this.value = let (MaterialId g) = this in g
        static member create () : MaterialId = Guid.NewGuid() |> MaterialId

        static member tryCreate (s : string) : MaterialId option =
            match Guid.TryParse s with
            | true, g -> Some (MaterialId g)
            | _ -> None

    /// Module-level parse helper for `MaterialId` (the case name and type name collide, so a
    /// QUALIFIED `MaterialLibrary.MaterialId.tryCreate` resolves to the case rather than the static
    /// member — the same collision `Library.elementId` documents; this helper is the unambiguous
    /// call site for qualified callers, e.g. the drag-payload boundary).
    let tryMaterialId (s : string) : MaterialId option = MaterialId.tryCreate s

    /// Module-level mint helper for `MaterialId` (same case/type name collision as `tryMaterialId`).
    let newMaterialId () : MaterialId = MaterialId.create ()

    /// The FIXED ids of the built-in library entries (spec 0033 step 002): literal Guids parsed once,
    /// so the seeds, the sample structures (`Library.SeedSamples`), the grouping trees, and the tests
    /// all reference the SAME deterministic identity across runs. A new built-in adds a new literal
    /// here — seed construction never calls `MaterialId.create`.
    module MaterialIds =
        let silicon : MaterialId = Guid.Parse "0f698851-dca1-412a-ad46-fac413496667" |> MaterialId
        let langasite : MaterialId = Guid.Parse "e95f01fb-0442-42c4-ba69-74d82f52e544" |> MaterialId
        let glass152 : MaterialId = Guid.Parse "55750ee2-358c-40d4-ab6c-06fc144adbf8" |> MaterialId
        let glass150 : MaterialId = Guid.Parse "0eedbd4b-ad1c-4bfe-8f6d-ae573b4a422d" |> MaterialId
        let glass175 : MaterialId = Guid.Parse "7bd71d63-98a6-4096-849d-b0c95e876966" |> MaterialId
        let glass200 : MaterialId = Guid.Parse "077d0db9-a2e2-45da-a76f-5ddedfcf0dc5" |> MaterialId
        let uniaxialCrystal : MaterialId = Guid.Parse "749465a5-fece-4d8d-9ea9-c626613614be" |> MaterialId
        let biaxialCrystal : MaterialId = Guid.Parse "4381fb35-662c-4dc9-80a4-c1a88125f5e5" |> MaterialId
        let vacuum : MaterialId = Guid.Parse "4355ad6d-b743-4ca9-8365-7ce9f585a7a0" |> MaterialId
        let euvMolybdenum : MaterialId = Guid.Parse "cff60a4d-6c57-4a99-84ae-42533351de2c" |> MaterialId
        let euvSilicon : MaterialId = Guid.Parse "43075352-cb2a-41ed-b53b-76e7166ece57" |> MaterialId
        let activeCrystal : MaterialId = Guid.Parse "a8dfa59c-2e95-4e3a-bfa6-b7e7e12ef58f" |> MaterialId

    /// Elevated material-category identity (spec 0035 step 001): a Guid-backed single-case DU,
    /// mirroring `MaterialId` (:27). `create` MINTS a fresh id (a user-authored category); the
    /// seeded built-ins parse FIXED literal Guids (`CategoryIds`) so a category's identity is
    /// deterministic across runs and a persisted entry resolves to the SAME category.
    type CategoryId =
        | CategoryId of Guid

        member this.value = let (CategoryId g) = this in g
        static member create () : CategoryId = Guid.NewGuid() |> CategoryId

    /// Whether a category is offered as a target in the create/edit picker (spec 0035 step 001):
    /// a named two-case DU, never a naked `bool`. A `HiddenOnCreate` category (Vacuum) categorises
    /// seed entries the structural multilayer stacks reference but is NOT a user-selectable
    /// creation target — it is removed from the picker, not greyed.
    type CategoryVisibility =
        | SelectableOnCreate
        | HiddenOnCreate

    /// Whether a category ships with the app or was authored by a user (spec 0035 step 001): a
    /// named two-case DU, never a naked `bool`.
    type CategoryOrigin =
        | BuiltInCategory
        | UserCategory

    /// Material category as DATA (spec 0035 step 001): the closed `MaterialCategory` union is
    /// replaced by a record so categories are a seeded, extensible catalogue resolved by id — not
    /// a fixed compile-time set. A category `name` is resolved through the catalogue by
    /// `CategoryId` (the sole seam consumers read); `visibility` and `origin` are named DUs. No
    /// domain record or public signature carries a closed union — `MaterialEntry.category` and
    /// `MaterialQuery.category` are `CategoryId`-typed.
    type MaterialCategory =
        {
            id : CategoryId
            name : string
            visibility : CategoryVisibility
            origin : CategoryOrigin
        }

    /// The FIXED ids of the built-in categories (spec 0035 step 001): literal Guids parsed once
    /// (mirroring `MaterialIds` at :51), so the seeded catalogue, the entry seeds, and the tests
    /// all reference the SAME deterministic identity across runs. A new built-in category adds a
    /// new literal here; catalogue construction never calls `CategoryId.create`.
    module CategoryIds =
        let glass : CategoryId = Guid.Parse "2a4b6c8d-1e3f-4a5b-8c7d-9e0f1a2b3c4d" |> CategoryId
        let metal : CategoryId = Guid.Parse "3b5c7d9e-2f4a-5b6c-9d8e-0f1a2b3c4d5e" |> CategoryId
        let semiconductor : CategoryId = Guid.Parse "4c6d8e0f-3a5b-6c7d-ae9f-1a2b3c4d5e6f" |> CategoryId
        let crystal : CategoryId = Guid.Parse "5d7e9f1a-4b6c-7d8e-bf0a-2b3c4d5e6f70" |> CategoryId
        let vacuum : CategoryId = Guid.Parse "6e8fa02b-5c7d-8e9f-c01b-3c4d5e6f7081" |> CategoryId

    /// The seeded built-in category catalogue (spec 0035 step 001). Glass/Metal/Semiconductor/
    /// Crystal are `SelectableOnCreate` creation targets; `Vacuum` is `HiddenOnCreate` — it
    /// categorises the vacuum spacer entry the structural multilayer seeds reference but is not
    /// offered in the create picker. Every seed is `BuiltInCategory`.
    let standardCategories : MaterialCategory list =
        [
            { id = CategoryIds.glass; name = "Glass"; visibility = SelectableOnCreate; origin = BuiltInCategory }
            { id = CategoryIds.metal; name = "Metal"; visibility = SelectableOnCreate; origin = BuiltInCategory }
            { id = CategoryIds.semiconductor; name = "Semiconductor"; visibility = SelectableOnCreate; origin = BuiltInCategory }
            { id = CategoryIds.crystal; name = "Crystal"; visibility = SelectableOnCreate; origin = BuiltInCategory }
            { id = CategoryIds.vacuum; name = "Vacuum"; visibility = HiddenOnCreate; origin = BuiltInCategory }
        ]

    /// Resolve a category record by its id through the seeded catalogue (spec 0035 step 001): the
    /// single name-resolution seam consumers read instead of matching a closed union. An id absent
    /// from the catalogue is `None`, never a throw.
    let tryFindCategory (id : CategoryId) : MaterialCategory option =
        standardCategories |> List.tryFind (fun c -> c.id = id)

    /// The display name of a category id, resolved through the catalogue (spec 0035 step 001); an
    /// id absent from the catalogue falls back to its Guid string form (a diagnostic, never a throw).
    let categoryName (id : CategoryId) : string =
        match tryFindCategory id with
        | Some c -> c.name
        | None -> string id.value

    /// Resolve a category record by its display name through the catalogue (spec 0035 step 001):
    /// the inverse of `categoryName`, used at the persisted/wire boundary (library JSON). An
    /// unknown name is `None`, never a throw.
    let tryFindCategoryByName (name : string) : MaterialCategory option =
        standardCategories |> List.tryFind (fun c -> c.name = name)

    /// The editable material-complexity option tree (spec 0033 step 013, §B / the
    /// Part F progressive ladder): the serializable edit model a material's engine
    /// properties are BUILT from. `eps` is ALWAYS present; the magnetic (Polder μ)
    /// and active (gyration ρ) aspects are OPTIONAL — an absent option means the
    /// engine's vacuum default, so the simplest material stays "transparent,
    /// isotropic, non-dispersive" and every richer feature lifts exactly one aspect
    /// off its default.
    type MaterialComplexity =
        {
            eps : EpsWithDispValue
            magnetic : MuWithDispValue option
            active : RhoWithDispValue option
        }

        /// Pure composition to the engine's `OpticalPropertiesWithDisp`
        /// (`Dispersion.fs:55`): eps through `EpsWithDispValue.toEpsWithDisp`, absent
        /// magnetic/active options defaulted to the vacuum μ/ρ through the single
        /// vacuum-convention site (`DispersionModels.isotropicProperties`), present
        /// options assembled through `toMuWithDisp` / `toRhoWithDisp` (the latter is
        /// the `OpticalProperties/Active.fs` type extension, opened above).
        member this.toProperties : OpticalPropertiesWithDisp =
            let defaults = DispersionModels.isotropicProperties this.eps.toEpsWithDisp
            let muWithDisp =
                match this.magnetic with
                | Some m -> m.toMuWithDisp
                | None -> defaults.muWithDisp
            let rhoWithDisp =
                match this.active with
                | Some a -> a.toRhoWithDisp
                | None -> defaults.rhoWithDisp
            { defaults with muWithDisp = muWithDisp; rhoWithDisp = rhoWithDisp }

    /// A library entry: a stable id (the `materialEntry` id, §A.7), a display name,
    /// a category, an optional description (mirroring the engine's `description`
    /// fields, e.g. `OpticalSystemWithDisp.description`, `Dispersion.fs:141`), the
    /// engine's `OpticalPropertiesWithDisp` (`Dispersion.fs:53`), and the optional
    /// edit model (spec 0033 step 013): `Some complexity` is the EDITABLE source of
    /// truth — `properties` IS `complexity.toProperties` (the seeds construct it so;
    /// the Part F editor re-derives it at save) — while `None` marks an
    /// engine-preset entry whose physics is coded rather than data (silicon /
    /// langasite / the vacuum spacer), shown view-only.
    type MaterialEntry =
        {
            id : MaterialId
            name : string
            category : CategoryId
            description : string option
            properties : OpticalPropertiesWithDisp
            complexity : MaterialComplexity option
        }

    /// Net-new error channel for material resolution (errors as values, §0). Returned
    /// — never thrown — by `resolveMaterial` on an unknown id; the case carries a
    /// diagnostic `reason` (spec 0033 step 002) naming the id's Guid string form.
    /// Extended (spec 0033 step 003, the `MaterialProxy` write-seam) with the mutating
    /// outcomes: adding an entry under an id the library already holds, removing an
    /// entry a sample structure still references, and rejecting a malformed entry.
    /// Every case carries a diagnostic `reason` — a bare error case is useless in a log.
    type MaterialError =
        | UnknownMaterialId of reason : string
        | DuplicateMaterialId of reason : string
        | MaterialStillReferenced of reason : string
        | InvalidMaterial of reason : string

    /// The in-memory, additive material library (§D.8). Persistence of an entry is the
    /// JSON `materialEntry` `$def` (§A.7); a shareable library FILE format is Part I §I.8.
    type MaterialLibrary =
        {
            entries : MaterialEntry list
        }

    /// Linear category filter (§D.8 — `List.filter`, no index): matches entries by `CategoryId`
    /// (spec 0035 step 001), not by a closed union.
    let byCategory (category : CategoryId) (lib : MaterialLibrary) : MaterialEntry list =
        lib.entries |> List.filter (fun e -> e.category = category)

    /// Linear case-insensitive name search (§D.8).
    let byNameContains (fragment : string) (lib : MaterialLibrary) : MaterialEntry list =
        lib.entries
        |> List.filter (fun e -> e.name.IndexOf(fragment, StringComparison.OrdinalIgnoreCase) >= 0)

    /// The single by-id material-resolution seam (§D.8). Looks the entry up by id,
    /// then evaluates its `OpticalPropertiesWithDisp` at the supplied `WaveLength`
    /// through the engine `getEps` path (`OpticalPropertiesWithDisp.getProperties`,
    /// `Dispersion.fs:60`, which calls `getEps`/`getMu`/`getRho`) to produce the
    /// concrete tensor record `OpticalProperties` (`MaterialProperties.fs:165`). A
    /// `WaveLength` is REQUIRED because a dispersive entry has no single tensor until
    /// a wavelength is chosen. Unknown ids return `Error (UnknownMaterialId _)` — the
    /// function never throws. Slices 005/014 call THIS rather than rebuilding tensors.
    /// The by-id DISPERSIVE material-resolution seam (spec 0033 step 001): the entry's
    /// `OpticalPropertiesWithDisp` itself, unevaluated, for callers that resolve once and evaluate per
    /// wavelength (`Propagation.resolveSampleMaterials`). Unknown ids return
    /// `Error (UnknownMaterialId _)` — the function never throws.
    let resolveMaterialWithDisp (lib : MaterialLibrary) (id : MaterialId) : Result<OpticalPropertiesWithDisp, MaterialError> =
        match lib.entries |> List.tryFind (fun e -> e.id = id) with
        | Some e -> Ok e.properties
        | None -> Error (UnknownMaterialId $"unknown material id '%s{string id.value}'")

    let resolveMaterial (lib : MaterialLibrary) (id : MaterialId) (w : WaveLength) : Result<OpticalProperties, MaterialError> =
        resolveMaterialWithDisp lib id |> Result.map (fun p -> p.getProperties w)

    /// A constant-eps complexity with no magnetic/active aspect — the shape shared by
    /// every re-expressed transparent/absorbing built-in below.
    let private constantComplexity (eps : ConstantEpsValue) : MaterialComplexity =
        {
            eps = EpsWithoutDispValue eps
            magnetic = None
            active = None
        }

    // ==========================================================================
    // Complexities of the re-expressed built-ins (spec 0033 step 013). Each is the
    // editable source of truth of its entry: the entry sets
    // properties = complexity.toProperties, so the sync invariant holds by
    // construction at seed time, and each construction reuses the preset constants
    // and the preset arithmetic path so the seeded tensors stay VALUE-IDENTICAL to
    // the original engine presets (PropagationTests pins the seeded systems with
    // exact equality). The uniaxial and active crystals are encoded as
    // BiaxialTransparent per-axis values, NOT UniaxialTransparent: the engine maps
    // UniaxialTransparent to the (ordinary, extraordinary, ordinary) diagonal
    // (Dispersion.fs:297), but Eps.uniaxialCrystal is diag(1.5², 1.65², 1.65²)
    // (unique axis x) and planarCrystal is diag(n₁₁², n₁₁², n₃₃²) (unique axis z) —
    // neither has the (o, e, o) shape, and the AC-B7 acceptance (reproduce the
    // original preset's tensors) wins over the case name.
    // ==========================================================================

    let private glass152Complexity : MaterialComplexity =
        constantComplexity (IsotropicTransparent RefractionIndex.transparentGlass)

    let private glass150Complexity : MaterialComplexity =
        constantComplexity (IsotropicTransparent RefractionIndex.transparentGlass150)

    let private glass175Complexity : MaterialComplexity =
        constantComplexity (IsotropicTransparent RefractionIndex.transparentGlass175)

    let private glass200Complexity : MaterialComplexity =
        constantComplexity (IsotropicTransparent RefractionIndex.transparentGlass200)

    let private uniaxialCrystalComplexity : MaterialComplexity =
        constantComplexity (BiaxialTransparent (RefractionIndex 1.5, RefractionIndex 1.65, RefractionIndex 1.65))

    let private biaxialCrystalComplexity : MaterialComplexity =
        constantComplexity (BiaxialTransparent (RefractionIndex 1.5, RefractionIndex 1.65, RefractionIndex 1.75))

    let private euvMolybdenumComplexity : MaterialComplexity =
        Berreman.MathNetNumericsMath.createComplex (1.0 - Eps.euvMolybdenumDelta) Eps.euvMolybdenumBeta
        |> ComplexRefractionIndex
        |> IsotropicAbsorbing
        |> constantComplexity

    let private euvSiliconComplexity : MaterialComplexity =
        Berreman.MathNetNumericsMath.createComplex (1.0 - Eps.euvSiliconDelta) Eps.euvSiliconBeta
        |> ComplexRefractionIndex
        |> IsotropicAbsorbing
        |> constantComplexity

    /// The per-axis indices go through the SAME EpsValue → sqrt round-trip the
    /// engine's `planarCrystal` preset performs, so the rebuilt eps is
    /// value-identical to the original (not merely within tolerance).
    let private activeCrystalComplexity : MaterialComplexity =
        let n11 = (RefractionIndex 2.315 |> EpsValue.fromRefractionIndex).refractionIndex
        let n33 = (RefractionIndex 2.226 |> EpsValue.fromRefractionIndex).refractionIndex
        {
            eps = EpsWithoutDispValue (BiaxialTransparent (n11, n11, n33))
            magnetic = None
            active = Some (RhoWithoutDispValue { gyration = PlanarActive (RhoValue 1.5e-6); hand = RightHanded })
        }

    /// Built-in entries (§D.8). `Silicon`/`Langasite` wrap their engine presets as-is
    /// (`Dispersive.fs:98,99` — dispersion coded, not data — so `complexity = None`,
    /// view-only), as does the vacuum spacer; the nine expressible entries are
    /// re-expressed as complexities (spec 0033 step 013) with
    /// `properties = complexity.toProperties`, which the AC-B7 tests pin against the
    /// original `Standard.fs`/`Active.fs` presets at reference wavelengths.
    let builtInEntries : MaterialEntry list =
        [
            {
                id = MaterialIds.silicon
                name = "Silicon"
                category = CategoryIds.semiconductor
                description = Some "Crystalline silicon (engine preset Silicon)."
                properties = siliconOpticalProperties
                complexity = None
            }
            {
                id = MaterialIds.langasite
                name = "Langasite (La3Ga5SiO14)"
                category = CategoryIds.crystal
                description = Some "Langasite, optically active uniaxial crystal (engine preset Langasite)."
                properties = langasiteOpticalProperties
                complexity = None
            }
            {
                id = MaterialIds.glass152
                name = "Transparent glass (n = 1.52)"
                category = CategoryIds.glass
                description = Some "Standard transparent glass preset."
                properties = glass152Complexity.toProperties
                complexity = Some glass152Complexity
            }
            {
                id = MaterialIds.glass150
                name = "Transparent glass (n = 1.50)"
                category = CategoryIds.glass
                description = None
                properties = glass150Complexity.toProperties
                complexity = Some glass150Complexity
            }
            {
                id = MaterialIds.glass175
                name = "Transparent glass (n = 1.75)"
                category = CategoryIds.glass
                description = None
                properties = glass175Complexity.toProperties
                complexity = Some glass175Complexity
            }
            {
                id = MaterialIds.glass200
                name = "Transparent glass (n = 2.00)"
                category = CategoryIds.glass
                description = None
                properties = glass200Complexity.toProperties
                complexity = Some glass200Complexity
            }
            {
                id = MaterialIds.uniaxialCrystal
                name = "Uniaxial crystal"
                category = CategoryIds.crystal
                description = Some "Standard uniaxial crystal preset."
                properties = uniaxialCrystalComplexity.toProperties
                complexity = Some uniaxialCrystalComplexity
            }
            {
                id = MaterialIds.biaxialCrystal
                name = "Biaxial crystal"
                category = CategoryIds.crystal
                description = Some "Standard biaxial crystal preset."
                properties = biaxialCrystalComplexity.toProperties
                complexity = Some biaxialCrystalComplexity
            }
            {
                id = MaterialIds.vacuum
                name = "Vacuum"
                category = CategoryIds.vacuum
                description = Some "Vacuum (n = 1) — the spacer material of the structural multilayer stacks."
                properties = OpticalProperties.vacuum.dispersive
                complexity = None
            }
            {
                id = MaterialIds.euvMolybdenum
                name = "Molybdenum (Mo, EUV)"
                category = CategoryIds.metal
                description = Some "Molybdenum for EUV multilayers (engine preset, complex n around 10–13.5 nm)."
                properties = euvMolybdenumComplexity.toProperties
                complexity = Some euvMolybdenumComplexity
            }
            {
                id = MaterialIds.euvSilicon
                name = "Silicon (Si, EUV)"
                category = CategoryIds.semiconductor
                description = Some "Silicon for EUV multilayers (engine preset, complex n around 10–13.5 nm)."
                properties = euvSiliconComplexity.toProperties
                complexity = Some euvSiliconComplexity
            }
            {
                id = MaterialIds.activeCrystal
                name = "Active (gyrotropic) crystal"
                category = CategoryIds.crystal
                description = Some "Planar active (gyrotropic) crystal: n₁₁ = 2.315, n₃₃ = 2.226, optical-activity ρ₁₂ = 1.5e-6 (from ActiveCrystal.fsx)."
                properties = activeCrystalComplexity.toProperties
                complexity = Some activeCrystalComplexity
            }
        ]

    /// The default in-memory library seeded with the built-in entries (§D.8).
    let standard : MaterialLibrary = { entries = builtInEntries }

    /// The dispersion facet of a materials search (spec 0033 step 003): match every entry,
    /// only entries whose optical properties depend on wavelength, or only
    /// wavelength-independent entries. A three-case DU — never a `bool option` — so a match
    /// site reads as prose and a future facet (e.g. tabulated-only) is a non-breaking
    /// addition.
    type DispersionFilter =
        | AnyDispersion
        | OnlyDispersive
        | OnlyNonDispersive

    /// A materials-library search query (spec 0033 step 003): a case-insensitive name
    /// fragment (empty matches all — the `byNameContains` semantics), an optional category
    /// `CategoryId` (spec 0035 step 001), and the dispersion facet. The query is DATA, so the materials
    /// panel drives one search seam (`MaterialProxy.searchMaterials`) instead of composing
    /// ad-hoc filter calls.
    type MaterialQuery =
        {
            text : string
            category : CategoryId option
            dispersion : DispersionFilter
        }

        /// The match-everything query (a search UI's initial state).
        static member empty : MaterialQuery =
            {
                text = ""
                category = None
                dispersion = AnyDispersion
            }

    /// The mutating materials write-seam (spec 0033 steps 003/006, contract STORE_XDUO_0001 —
    /// IMPLEMENTED lifecycle): the functional-proxy convention `LibraryProxy` set
    /// (`ElementId.fs`), a record of camelCase `Result`-returning functions. A test
    /// substitutes a stub of the SAME shape. Function-valued fields have no structural
    /// equality, so the proxy compares by reference — a host model holding one keeps its
    /// (Elmish-required) equality. The real, stateful in-memory store behind this surface is
    /// `MaterialProxy.createInMemory` (a type augmentation in `ElementId.fs`: its
    /// `samplesReferencing` parameter is `Sample`-typed, and `Sample` compiles after this
    /// file).
    [<ReferenceEquality>]
    type MaterialProxy =
        {
            listMaterials : unit -> Result<MaterialEntry list, MaterialError>
            searchMaterials : MaterialQuery -> Result<MaterialEntry list, MaterialError>
            tryGetMaterial : MaterialId -> Result<MaterialEntry option, MaterialError>
            addMaterial : MaterialEntry -> Result<unit, MaterialError>
            updateMaterial : MaterialEntry -> Result<unit, MaterialError>
            removeMaterial : MaterialId -> Result<unit, MaterialError>
        }

    /// Whether an entry's optical properties actually depend on wavelength — any component
    /// still carrying a function case (`EpsWithDisp` / `MuWithDisp` / `RhoWithDisp`; every
    /// dispersive built-in carries the eps func case). The classification `DispersionFilter`
    /// matches against; private to the pure search seam (`byQuery`).
    let private hasDispersion (e : MaterialEntry) : bool =
        match e.properties.epsWithDisp, e.properties.muWithDisp, e.properties.rhoWithDisp with
        | EpsWithoutDisp _, MuWithoutDisp _, RhoWithoutDisp _ -> false
        | _ -> true

    /// The pure materials search (spec 0033 steps 003/006): the case-insensitive
    /// name-fragment filter (`byNameContains` — empty matches all), then the optional
    /// category facet (`byCategory`), then the `DispersionFilter` facet. The one query seam
    /// every producer of the `MaterialProxy` shape answers `searchMaterials` from — the real
    /// store (`MaterialProxy.createInMemory`, `ElementId.fs`) applies it to its current
    /// entries.
    let byQuery (q : MaterialQuery) (lib : MaterialLibrary) : MaterialEntry list =
        let byText = byNameContains q.text lib
        let byCat =
            match q.category with
            | Some c -> byCategory c { entries = byText }
            | None -> byText
        match q.dispersion with
        | AnyDispersion -> byCat
        | OnlyDispersive -> byCat |> List.filter hasDispersion
        | OnlyNonDispersive -> byCat |> List.filter (hasDispersion >> not)

    /// The blank-name validation the store's write functions share (spec 0033 steps 003/006):
    /// a `MaterialEntry` whose display name is empty/whitespace is `InvalidMaterial`. Not
    /// private: the real store is a type augmentation in `ElementId.fs`, and an optional
    /// extension in another file cannot reach a module-private binding.
    let validateEntry (entry : MaterialEntry) : Result<unit, MaterialError> =
        if String.IsNullOrWhiteSpace entry.name
        then Error (InvalidMaterial $"material '%s{string entry.id.value}' has a blank name")
        else Ok ()
