namespace OpticalConstructor.Domain

open System
open System.Numerics
open Berreman.Constants
open Berreman.MathNetNumericsMath
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalProperties.Dispersive
open OpticalProperties.Standard
open OpticalProperties.Active
open OpticalConstructor.Domain.DispersionModels

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

    /// Module-level mint helper for `MaterialId` (the case name and type name collide, so a QUALIFIED
    /// `MaterialLibrary.MaterialId.create` resolves to the case rather than the static member).
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
        let vacuumMaterial : MaterialId = Guid.Parse "4355ad6d-b743-4ca9-8365-7ce9f585a7a0" |> MaterialId
        let euvMolybdenum : MaterialId = Guid.Parse "cff60a4d-6c57-4a99-84ae-42533351de2c" |> MaterialId
        let euvSilicon : MaterialId = Guid.Parse "43075352-cb2a-41ed-b53b-76e7166ece57" |> MaterialId
        let activeCrystal : MaterialId = Guid.Parse "a8dfa59c-2e95-4e3a-bfa6-b7e7e12ef58f" |> MaterialId

    /// A name search's fragment (spec 0033 steps 003/004): matched ignoring case, and the empty
    /// fragment matches every name.
    type NameFragment =
        | NameFragment of string

        member this.value = let (NameFragment s) = this in s

    /// The display name of a library entry: a material, or a Library sample, source, detector or
    /// polarizer preset.
    type EntryName =
        | EntryName of string

        member this.value = let (EntryName s) = this in s

        member this.toOpticalSystemDescription : Berreman.Media.OpticalSystemDescription =
            Berreman.Media.OpticalSystemDescription this.value

    /// What a material entry or a Library sample is, in prose.
    type EntryDescription =
        | EntryDescription of string

        member this.value = let (EntryDescription s) = this in s

    /// A material category's display name (spec 0035 step 001): also the library JSON's `category`.
    type CategoryName =
        | CategoryName of string

        member this.value = let (CategoryName s) = this in s

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
            name : CategoryName
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
        let vacuumCategory : CategoryId = Guid.Parse "6e8fa02b-5c7d-8e9f-c01b-3c4d5e6f7081" |> CategoryId

    /// The seeded built-in category catalogue (spec 0035 step 001). Glass/Metal/Semiconductor/
    /// Crystal are `SelectableOnCreate` creation targets; `Vacuum` is `HiddenOnCreate` — it
    /// categorises the vacuum spacer entry the structural multilayer seeds reference but is not
    /// offered in the create picker. Every seed is `BuiltInCategory`.
    let standardCategories : MaterialCategory list =
        [
            { id = CategoryIds.glass; name = CategoryName "Glass"; visibility = SelectableOnCreate; origin = BuiltInCategory }
            { id = CategoryIds.metal; name = CategoryName "Metal"; visibility = SelectableOnCreate; origin = BuiltInCategory }
            { id = CategoryIds.semiconductor; name = CategoryName "Semiconductor"; visibility = SelectableOnCreate; origin = BuiltInCategory }
            { id = CategoryIds.crystal; name = CategoryName "Crystal"; visibility = SelectableOnCreate; origin = BuiltInCategory }
            { id = CategoryIds.vacuumCategory; name = CategoryName "Vacuum"; visibility = HiddenOnCreate; origin = BuiltInCategory }
        ]

    /// Resolve an id through the caller's live category store's listCategories; absent ids return None
    /// (spec 0035 step 001; spec 0050 S1.7).
    let tryFindCategory (categories : MaterialCategory list) (id : CategoryId) : MaterialCategory option =
        categories |> List.tryFind (fun c -> c.id = id)

    type CategoryId with

        /// Resolve through the caller's live category store's listCategories, or return the id's GUID
        /// label when absent (spec 0035 step 001; spec 0050 S1.7).
        member this.categoryName (categories : MaterialCategory list) : CategoryName =
            match tryFindCategory categories this with
            | Some c -> c.name
            | None -> CategoryName (string this.value)

    /// Resolve a name through the caller's live category store's listCategories; unknown names return None
    /// (spec 0035 step 001; spec 0050 S1.7).
    let tryFindCategoryByName (categories : MaterialCategory list) (name : CategoryName) : MaterialCategory option =
        categories |> List.tryFind (fun c -> c.name = name)

    /// The category write-seam error channel (spec 0035 step 002, contract STORE_XDUO_0003 —
    /// DECLARED lifecycle): errors as values, each case carrying a diagnostic `reason` — a bare
    /// error case is useless in a log. Mirrors `MaterialError` (below) and `SampleError`
    /// (`ElementId.fs`): an unknown id on update/remove, adding a category under an id the
    /// catalogue already holds, and removing a category a material entry still references. Adds
    /// `BuiltInNotRemovable` — a `BuiltInCategory` is shipped with the app and MUST NOT be deleted
    /// (only a `UserCategory` may be removed) — and `InvalidCategory` for a malformed (blank-name)
    /// category.
    type CategoryError =
        | UnknownCategoryIdErr of reason : string
        | DuplicateCategoryIdErr of reason : string
        | CategoryStillReferencedErr of reason : string
        | BuiltInNotRemovableErr of reason : string
        | InvalidCategoryErr of reason : string

    /// The mutating category write-seam (spec 0035 step 002, contract STORE_XDUO_0003 — DECLARED
    /// lifecycle): the functional-proxy convention `LibraryProxy`/`MaterialProxy`/`SampleProxy`
    /// set (`ElementId.fs`; `MaterialProxy` below), a record of camelCase `Result`-returning
    /// functions. A test substitutes a stub of the SAME shape. Function-valued fields have no
    /// structural equality, so the proxy compares by reference — a host model holding one keeps
    /// its (Elmish-required) equality, comparing the proxy by identity. This step ships the
    /// DECLARED surface, a mock, and a mock-driven test only; the real, stateful in-memory store
    /// behind this surface (`CategoryProxy.createInMemory`) is the later `IMPLEMENT_CONTRACT
    /// STORE_XDUO_0003` step.
    [<ReferenceEquality>]
    type CategoryProxy =
        {
            listCategories : unit -> Result<MaterialCategory list, CategoryError>
            addCategory : MaterialCategory -> Result<unit, CategoryError>
            updateCategory : MaterialCategory -> Result<unit, CategoryError>
            removeCategory : CategoryId -> Result<unit, CategoryError>
        }

    type MaterialCategory with

        /// The blank-name validation the store's write functions share (spec 0035 step 002): a
        /// category whose display name is empty/whitespace is `InvalidCategoryErr` — the
        /// `MaterialEntry.validate` precedent (below).
        member this.validate : Result<unit, CategoryError> =
            if String.IsNullOrWhiteSpace this.name.value
            then Error (InvalidCategoryErr $"category '%s{string this.id.value}' has a blank name")
            else Ok ()

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
        /// vacuum-convention site (`EpsWithDisp.isotropicProperties`), present
        /// options assembled through `toMuWithDisp` / `toRhoWithDisp` (the latter is
        /// the `OpticalProperties/Active.fs` type extension, opened above).
        member this.toProperties : OpticalPropertiesWithDisp =
            let defaults = this.eps.toEpsWithDisp.isotropicProperties
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
    /// fields, e.g. `OpticalSystemWithDisp.description`, `Dispersion.fs:141`), and its
    /// physics, the `complexity` value tree (spec 0050 step 13, 001 §B). Every field is
    /// data, so entries compare structurally.
    type MaterialEntry =
        {
            id : MaterialId
            name : EntryName
            category : CategoryId
            description : EntryDescription option
            complexity : MaterialComplexity
        }

        /// The entry's engine composition (`OpticalPropertiesWithDisp`, `Dispersion.fs:53`),
        /// derived from its complexity on every read.
        member this.properties : OpticalPropertiesWithDisp = this.complexity.toProperties

    /// Net-new error channel for material resolution (errors as values, §0). Returned
    /// — never thrown — by `resolveMaterial` on an unknown id; the case carries a
    /// diagnostic `reason` (spec 0033 step 002) naming the id's Guid string form.
    /// Extended (spec 0033 step 003, the `MaterialProxy` write-seam) with the mutating
    /// outcomes: adding an entry under an id the library already holds, removing an
    /// entry a sample structure still references, and rejecting a malformed entry.
    /// Every case carries a diagnostic `reason` — a bare error case is useless in a log.
    type MaterialError =
        | UnknownMaterialIdErr of reason : string
        | DuplicateMaterialIdErr of reason : string
        | MaterialStillReferencedErr of reason : string
        | InvalidMaterialErr of reason : string
        /// Spec 0038 Part H (step 021): removal is additionally hard-blocked when ANY version of
        /// the material is currently bound by a live experiment (the injected `VersionsInUse`
        /// seam). A used version can never be deleted; the case names the offending versions so
        /// the refusal is actionable in a log / status line.
        | MaterialVersionInUseErr of reason : string

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
    let byNameContains (fragment : NameFragment) (lib : MaterialLibrary) : MaterialEntry list =
        lib.entries
        |> List.filter (fun e -> e.name.value.IndexOf(fragment.value, StringComparison.OrdinalIgnoreCase) >= 0)

    /// The single by-id material-resolution seam (§D.8). Looks the entry up by id,
    /// then evaluates its `OpticalPropertiesWithDisp` at the supplied `WaveLength`
    /// through the engine `getEps` path (`OpticalPropertiesWithDisp.getProperties`,
    /// `Dispersion.fs:60`, which calls `getEps`/`getMu`/`getRho`) to produce the
    /// concrete tensor record `OpticalProperties` (`MaterialProperties.fs:165`). A
    /// `WaveLength` is REQUIRED because a dispersive entry has no single tensor until
    /// a wavelength is chosen. Unknown ids return `Error (UnknownMaterialIdErr _)` — the
    /// function never throws. Slices 005/014 call THIS rather than rebuilding tensors.
    /// The by-id DISPERSIVE material-resolution seam (spec 0033 step 001): the entry's
    /// `OpticalPropertiesWithDisp` itself, unevaluated, for callers that resolve once and evaluate per
    /// wavelength (`Propagation.resolveSampleMaterials`). Unknown ids return
    /// `Error (UnknownMaterialIdErr _)` — the function never throws.
    let resolveMaterialWithDisp (lib : MaterialLibrary) (id : MaterialId) : Result<OpticalPropertiesWithDisp, MaterialError> =
        match lib.entries |> List.tryFind (fun e -> e.id = id) with
        | Some e -> Ok e.properties
        | None -> Error (UnknownMaterialIdErr $"unknown material id '%s{string id.value}'")

    let resolveMaterial (lib : MaterialLibrary) (id : MaterialId) (w : WaveLength) : Result<OpticalProperties, MaterialError> =
        resolveMaterialWithDisp lib id |> Result.map (fun p -> p.getProperties w)

    type ConstantEpsValue with

        /// A constant-eps complexity with no magnetic/active aspect — the shape shared by
        /// every re-expressed transparent/absorbing built-in below.
        member private eps.constantComplexity : MaterialComplexity =
            {
                eps = EpsWithoutDispValue eps
                magnetic = None
                active = None
            }

    // ==========================================================================
    // Complexities of the re-expressed built-ins (spec 0033 step 013). Each is its
    // entry's physics, and each construction reuses the preset constants
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
        (IsotropicTransparent RefractionIndex.transparentGlass).constantComplexity

    let private glass150Complexity : MaterialComplexity =
        (IsotropicTransparent RefractionIndex.transparentGlass150).constantComplexity

    let private glass175Complexity : MaterialComplexity =
        (IsotropicTransparent RefractionIndex.transparentGlass175).constantComplexity

    let private glass200Complexity : MaterialComplexity =
        (IsotropicTransparent RefractionIndex.transparentGlass200).constantComplexity

    let private uniaxialCrystalComplexity : MaterialComplexity =
        (BiaxialTransparent { nx = RefractionIndex 1.5; ny = RefractionIndex 1.65; nz = RefractionIndex 1.65 }).constantComplexity

    let private biaxialCrystalComplexity : MaterialComplexity =
        (BiaxialTransparent { nx = RefractionIndex 1.5; ny = RefractionIndex 1.65; nz = RefractionIndex 1.75 }).constantComplexity

    let private euvMolybdenumComplexity : MaterialComplexity =
        (Complex.create (1.0 - Eps.euvMolybdenumDelta) Eps.euvMolybdenumBeta |> ComplexRefractionIndex |> IsotropicAbsorbing).constantComplexity

    let private euvSiliconComplexity : MaterialComplexity =
        (Complex.create (1.0 - Eps.euvSiliconDelta) Eps.euvSiliconBeta |> ComplexRefractionIndex |> IsotropicAbsorbing).constantComplexity

    /// The per-axis indices go through the SAME EpsValue → sqrt round-trip the
    /// engine's `planarCrystal` preset performs, so the rebuilt eps is
    /// value-identical to the original (not merely within tolerance).
    let private activeCrystalComplexity : MaterialComplexity =
        let n11 = (RefractionIndex 2.315 |> EpsValue.fromRefractionIndex).refractionIndex
        let n33 = (RefractionIndex 2.226 |> EpsValue.fromRefractionIndex).refractionIndex
        {
            eps = EpsWithoutDispValue (BiaxialTransparent { nx = n11; ny = n11; nz = n33 })
            magnetic = None
            active = Some (RhoWithoutDispValue { gyration = PlanarActive (RhoValue 1.5e-6); hand = RightHanded })
        }

    // ==========================================================================
    // The three coded-preset complexities (spec 0040 step 004, Part B). Silicon,
    // Langasite and the Vacuum spacer carry a value tree that ENCODES their class as
    // data — Silicon Isotropic, Langasite Uniaxial (retaining its optical activity),
    // Vacuum Isotropic + Transparent — as every other built-in does, so the facets sum
    // to the population (spec 0040 Part B.0).
    //
    // Silicon's and Langasite's dispersion is the published law their engine preset
    // evaluates (`PublishedLawAxis`, spec 0050 step 8): one closed form, in the core.
    // The anisotropy is read from the tree's CASE, never by evaluating ε (spec 0040
    // Part B.1). A published law has no editable coefficients, so these entries open
    // VIEW-ONLY (`MaterialComplexity.isEditable`).
    // ==========================================================================

    /// The band of a coded preset's single segment (spec 0040 step 004): a published law
    /// holds at every wavelength, extrapolating as the engine preset does, so the band is
    /// unbounded and a coded preset never flags out of band. `1.0e12<nm>` is 1 km.
    let private codedPresetInterval : WaveLengthInterval =
        { lower = WaveLength.nm 0.0<nm>; upper = WaveLength.nm 1.0e12<nm> }

    /// Silicon (spec 0040 step 004): Isotropic, dispersive, absorbing. Its axis is the
    /// published law `siliconOpticalProperties` evaluates (spec 0050 step 8).
    let private siliconComplexity : MaterialComplexity =
        {
            eps = EpsWithDispValue (IsotropicDispersive [ { wavelengthInterval = codedPresetInterval; dispersion = PublishedLawAxis SiliconIndexLaw } ])
            magnetic = None
            active = None
        }

    /// Langasite (spec 0040 step 004): Uniaxial, dispersive, optically active. Its axes are
    /// the published ordinary and extraordinary laws `langasiteOpticalProperties` evaluates
    /// (spec 0050 step 8). The gyration stays the representative constant one of class
    /// 3/4/6 its seed carries, so no library number moves; the engine preset's ρ is the
    /// published law `PublishedLawRho LangasiteGyrationLaw` (spec 0050 step 10).
    let private langasiteComplexity : MaterialComplexity =
        {
            eps =
                EpsWithDispValue
                    (UniaxialDispersive
                        [ {
                            wavelengthInterval = codedPresetInterval
                            ordinaryDispersion = PublishedLawAxis LangasiteOrdinaryIndexLaw
                            extraordinaryDispersion = PublishedLawAxis LangasiteExtraordinaryIndexLaw
                          } ])
            magnetic = None
            active = Some (RhoWithoutDispValue { gyration = UniaxialActive { g11 = RhoValue 1.0e-5; g33 = RhoValue 1.0e-5 }; hand = RightHanded })
        }

    /// The Vacuum spacer (spec 0040 step 004): Isotropic, Transparent, constant n = 1.
    /// `(IsotropicTransparent RefractionIndex.vacuum).constantComplexity.toProperties`
    /// is byte-identical to `OpticalProperties.vacuum` (`Eps.vacuum` = the identity =
    /// `Eps.fromRefractionIndex 1.0`), so the multilayer spacer physics is unchanged.
    let private vacuumComplexity : MaterialComplexity =
        (IsotropicTransparent RefractionIndex.vacuum).constantComplexity

    /// Built-in entries (§D.8). Each entry's physics is its complexity (spec 0040 step
    /// 004 re-seeds Silicon / Langasite / the Vacuum spacer, so their single-valued
    /// class is DATA the facets read; the nine already-expressible
    /// entries were re-expressed under spec 0033 step 013). The AC-B7 tests pin the
    /// re-expressed seeds against the original `Standard.fs`/`Active.fs` presets at
    /// reference wavelengths; the Silicon reproduction is pinned to the engine preset.
    let builtInEntries : MaterialEntry list =
        [
            {
                id = MaterialIds.silicon
                name = EntryName "Silicon"
                category = CategoryIds.semiconductor
                description = Some (EntryDescription "Crystalline silicon (engine preset Silicon).")
                complexity = siliconComplexity
            }
            {
                id = MaterialIds.langasite
                name = EntryName "Langasite (La3Ga5SiO14)"
                category = CategoryIds.crystal
                description = Some (EntryDescription "Langasite, optically active uniaxial crystal (engine preset Langasite).")
                complexity = langasiteComplexity
            }
            {
                id = MaterialIds.glass152
                name = EntryName "Transparent glass (n = 1.52)"
                category = CategoryIds.glass
                description = Some (EntryDescription "Standard transparent glass preset.")
                complexity = glass152Complexity
            }
            {
                id = MaterialIds.glass150
                name = EntryName "Transparent glass (n = 1.50)"
                category = CategoryIds.glass
                description = None
                complexity = glass150Complexity
            }
            {
                id = MaterialIds.glass175
                name = EntryName "Transparent glass (n = 1.75)"
                category = CategoryIds.glass
                description = None
                complexity = glass175Complexity
            }
            {
                id = MaterialIds.glass200
                name = EntryName "Transparent glass (n = 2.00)"
                category = CategoryIds.glass
                description = None
                complexity = glass200Complexity
            }
            {
                id = MaterialIds.uniaxialCrystal
                name = EntryName "Uniaxial crystal"
                category = CategoryIds.crystal
                description = Some (EntryDescription "Standard uniaxial crystal preset.")
                complexity = uniaxialCrystalComplexity
            }
            {
                id = MaterialIds.biaxialCrystal
                name = EntryName "Biaxial crystal"
                category = CategoryIds.crystal
                description = Some (EntryDescription "Standard biaxial crystal preset.")
                complexity = biaxialCrystalComplexity
            }
            {
                id = MaterialIds.vacuumMaterial
                name = EntryName "Vacuum"
                category = CategoryIds.vacuumCategory
                description = Some (EntryDescription "Vacuum (n = 1) — the spacer material of the structural multilayer stacks.")
                complexity = vacuumComplexity
            }
            {
                id = MaterialIds.euvMolybdenum
                name = EntryName "Molybdenum (Mo, EUV)"
                category = CategoryIds.metal
                description = Some (EntryDescription "Molybdenum for EUV multilayers (engine preset, complex n around 10–13.5 nm).")
                complexity = euvMolybdenumComplexity
            }
            {
                id = MaterialIds.euvSilicon
                name = EntryName "Silicon (Si, EUV)"
                category = CategoryIds.semiconductor
                description = Some (EntryDescription "Silicon for EUV multilayers (engine preset, complex n around 10–13.5 nm).")
                complexity = euvSiliconComplexity
            }
            {
                id = MaterialIds.activeCrystal
                name = EntryName "Active (gyrotropic) crystal"
                category = CategoryIds.crystal
                description = Some (EntryDescription "Planar active (gyrotropic) crystal: n₁₁ = 2.315, n₃₃ = 2.226, optical-activity ρ₁₂ = 1.5e-6 (from ActiveCrystal.fsx).")
                complexity = activeCrystalComplexity
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
            text : NameFragment
            category : CategoryId option
            dispersion : DispersionFilter
        }

        /// The match-everything query (a search UI's initial state).
        static member empty : MaterialQuery =
            {
                text = NameFragment ""
                category = None
                dispersion = AnyDispersion
            }

    /// A monotonically increasing version number of a library entry (spec 0038 Part H, step 020).
    /// A single-case `int` DU — no naked int stands in for a version. `first` is the version every
    /// entry starts at; `.next` mints the successor the store freezes onto when a USED version's
    /// physics changes; `.value` is the wire/disk integer, reached only at the IO boundary. Moved
    /// here from `Lifecycle.fs` at step 021 because the versioned `MaterialProxy` surface below
    /// references it (through `MaterialVersionId`), and the record is re-typed IN PLACE in this
    /// file, which compiles before `Lifecycle.fs`; `Lifecycle.fs` reuses it through its existing
    /// `open …MaterialLibrary`.
    type VersionNumber =
        | VersionNumber of int

        member this.value = let (VersionNumber v) = this in v

        /// The successor version — one past this one. Pure; `VersionNumber.first.next` is version 2.
        member this.next : VersionNumber = VersionNumber (this.value + 1)

        /// The version every entry starts at (1). New entries and the re-expressed seeds begin here;
        /// the store never fabricates a version number by hand.
        static member first : VersionNumber = VersionNumber 1

    /// A specific version of a material entry (spec 0038 Part H): the material identity plus the
    /// version number. This is the exact key a versioned experiment binding pins (step 25) — a
    /// binding references the precise version it was built against, not merely the material, so a
    /// later mint of the material's `.next` version does not silently rewrite an existing
    /// experiment's physics. Moved here from `Lifecycle.fs` at step 021 alongside `VersionNumber`
    /// (it needs only `MaterialId`, defined above) so the store's `resolveVersion` field can name it
    /// while the record stays in place in this file.
    type MaterialVersionId =
        {
            materialId : MaterialId
            version : VersionNumber
        }

        /// The version-ONE reference of a material (spec 0038 step 022) — the pin the seeded
        /// samples and freshly-authored sample layers carry. At this in-memory step every material
        /// lives at version 1 (an unused version's edit mutates in place; a mint needs a USED
        /// version, and `VersionsInUse` is `empty` until step 25), so `firstOf` IS the material's
        /// current latest version. Centralised so step 25 (real `VersionsInUse`) has ONE place to
        /// revisit pin-latest — the seeds and the editor both pin through here.
        static member firstOf (materialId : MaterialId) : MaterialVersionId =
            { materialId = materialId; version = VersionNumber.first }

    /// Whether a store listing shows only the LIVE (latest-active) entries or also the retired
    /// ones (spec 0038 Part H, step 021): a named two-case DU, never a naked bool. `ActiveOnly` —
    /// the default the offers use (pickers, Select mode, facet counts) — yields the latest version
    /// of each material only when that version is active; `IncludeInactive` additionally yields
    /// materials whose latest version is inactive or superseded (the Materials/Library window's
    /// show-inactive toggle). Shared by both catalogue stores.
    type InactiveVisibility =
        | ActiveOnly
        | IncludeInactive

    /// The mutating materials write-seam (spec 0033 steps 003/006, contract STORE_XDUO_0001 —
    /// IMPLEMENTED lifecycle; re-typed IN PLACE at spec 0038 Part H step 021 into the VERSIONED
    /// surface): the functional-proxy convention `LibraryProxy` set (`ElementId.fs`), a record of
    /// camelCase `Result`-returning functions. A test substitutes a stub of the SAME shape.
    /// Function-valued fields have no structural equality, so the proxy compares by reference — a
    /// host model holding one keeps its (Elmish-required) equality. The store is now versions per
    /// `MaterialId`:
    ///
    /// - `listMaterials scope` — the LATEST version of each material, filtered by `scope`
    ///   (`ActiveOnly` for offers, `IncludeInactive` for the show-inactive toggle);
    /// - `searchMaterials q` — the pure `byQuery` facets over the latest-ACTIVE versions (offers);
    /// - `tryGetMaterial id` — the latest version's entry, if any;
    /// - `resolveVersion mvid` — the EXACT version's entry, IGNORING lifecycle (reference
    ///   resolution: an experiment binding a superseded/inactive version still resolves it);
    /// - `saveMaterial entry` — applies the step-20 shared `decideVersioning` rule against the
    ///   injected `VersionsInUse` (a new material inserts version 1; an existing one mutates in
    ///   place, mints the next version, or is a no-op);
    /// - `markMaterialInactive` / `markMaterialActive` — retire / revive the latest version;
    /// - `supersedeMaterial` — retire the latest version (a superseded version behaves as inactive);
    /// - `removeMaterial id` — keeps the `MaterialStillReferencedErr` hard-block AND additionally
    ///   refuses any material carrying a used version (`MaterialVersionInUseErr`).
    ///
    /// The real, stateful in-memory store behind this surface is `MaterialProxy.createInMemory`
    /// (a type augmentation in `MaterialStore.fs`, after `Lifecycle.fs`: it needs both the shared
    /// `decideVersioning` rule and its `samplesReferencing` parameter's `Sample` type).
    [<ReferenceEquality>]
    type MaterialProxy =
        {
            listMaterials : InactiveVisibility -> Result<MaterialEntry list, MaterialError>
            searchMaterials : MaterialQuery -> Result<MaterialEntry list, MaterialError>
            tryGetMaterial : MaterialId -> Result<MaterialEntry option, MaterialError>
            resolveVersion : MaterialVersionId -> Result<MaterialEntry option, MaterialError>
            saveMaterial : MaterialEntry -> Result<unit, MaterialError>
            markMaterialInactive : MaterialId -> Result<unit, MaterialError>
            markMaterialActive : MaterialId -> Result<unit, MaterialError>
            supersedeMaterial : MaterialId -> Result<unit, MaterialError>
            removeMaterial : MaterialId -> Result<unit, MaterialError>
        }

    /// Whether an entry's optical properties actually depend on wavelength (spec 0038
    /// step 011): a named two-case DU, never a naked bool. A `DispersiveMaterial` has any
    /// component still carrying a function case (`EpsWithDisp` / `MuWithDisp` /
    /// `RhoWithDisp`; every dispersive built-in carries the eps func case). This is the
    /// classification `DispersionFilter` matches against AND the constant-vs-dispersive
    /// facet extractor (`LibraryFacets`) keys.
    type MaterialDispersion =
        | ConstantMaterial
        | DispersiveMaterial

    type MaterialEntry with

        /// The entry's wavelength dependence, classified from its engine `properties` (see
        /// `MaterialDispersion`; the step-011 facet catalogue keys it).
        member this.dispersion : MaterialDispersion =
            let properties = this.properties
            match properties.epsWithDisp, properties.muWithDisp, properties.rhoWithDisp with
            | EpsWithoutDisp _, MuWithoutDisp _, RhoWithoutDisp _ -> ConstantMaterial
            | _ -> DispersiveMaterial

        /// The blank-name validation the store's write functions share (spec 0033 steps 003/006):
        /// an entry whose display name is empty/whitespace is `InvalidMaterialErr`.
        member this.validate : Result<unit, MaterialError> =
            if String.IsNullOrWhiteSpace this.name.value
            then Error (InvalidMaterialErr $"material '%s{string this.id.value}' has a blank name")
            else Ok ()

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
        | OnlyDispersive -> byCat |> List.filter (fun e -> e.dispersion = DispersiveMaterial)
        | OnlyNonDispersive -> byCat |> List.filter (fun e -> e.dispersion = ConstantMaterial)

    /// The real, stateful in-memory category store behind the write-seam (spec 0035 step 003 —
    /// IMPLEMENT_CONTRACT STORE_XDUO_0003; replaces the step-002 declared mock). `createInMemory`
    /// closes over a `ref` `Map<CategoryId, MaterialCategory>` seeded from `standardCategories` —
    /// the elevated `CategoryId` is the Map key directly (the `SampleProxy` / `MaterialProxy`
    /// `createInMemory` precedent, `ElementId.fs`). Mutation stays INSIDE the closure (the IO
    /// boundary), so the logic holding the proxy stays pure: `listCategories` answers from the
    /// current map; `addCategory` keeps the step-002 blank-name validation (`InvalidCategoryErr`)
    /// then hard-blocks an id the store already holds (`DuplicateCategoryIdErr`); `updateCategory`
    /// validates then replaces a known id and rejects an unknown one (`UnknownCategoryIdErr`) —
    /// built-ins ARE renamable, so there is no origin guard on update; `removeCategory` refuses a
    /// `BuiltInCategory` outright (`BuiltInNotRemovableErr`) and, for a `UserCategory`, consults
    /// `materialsReferencingCategory` and returns `CategoryStillReferencedErr` NAMING the referencing
    /// materials whenever any remain — it never cascades and never silently deletes — else removes
    /// it. Deterministic under test — every category carries its own id, no clock, no IO. (An
    /// INTRINSIC augmentation staying in this file, mirroring `SampleProxy.createInMemory`: the
    /// referencing lookup is `MaterialEntry`-typed and `MaterialEntry` compiles above, so — unlike
    /// `MaterialProxy.createInMemory`, whose `Sample`-typed lookup forces it into `ElementId.fs` —
    /// this store needs nothing declared later. At composition the lookup is
    /// `materialsReferencingCategory` below, backed by the step-006 `MaterialProxy` store.)
    type CategoryProxy with

        /// The shared in-memory category-store body (spec 0038 step 042 — IMPLEMENT_CONTRACT
        /// STORE_XDUO_0008), parameterised by the INITIAL catalogue so the SELF-SEEDING
        /// `createInMemory` and the EMPTY-store `createInMemoryEmpty` (which the seeding pipeline
        /// fills) never drift — everything below the seed line is identical.
        static member private createInMemoryStore
            (initialCategories : MaterialCategory list)
            (materialsReferencingCategory : CategoryId -> MaterialEntry list)
            : CategoryProxy =
            let store = ref (initialCategories |> List.map (fun c -> c.id, c) |> Map.ofList)
            let currentCategories () : MaterialCategory list =
                store.Value |> Map.toList |> List.map snd
            let unknown (id : CategoryId) : CategoryError =
                UnknownCategoryIdErr $"unknown category id '%s{string id.value}'"
            {
                listCategories = fun () -> Ok (currentCategories ())
                addCategory =
                    fun category ->
                        category.validate
                        |> Result.bind (fun () ->
                            match store.Value |> Map.tryFind category.id with
                            | Some existing ->
                                Error (DuplicateCategoryIdErr $"category id '%s{string category.id.value}' already names '%s{existing.name.value}'")
                            | None ->
                                store.Value <- store.Value |> Map.add category.id category
                                Ok ())
                updateCategory =
                    fun category ->
                        category.validate
                        |> Result.bind (fun () ->
                            match store.Value |> Map.tryFind category.id with
                            | Some _ ->
                                store.Value <- store.Value |> Map.add category.id category
                                Ok ()
                            | None -> Error (unknown category.id))
                removeCategory =
                    fun id ->
                        match store.Value |> Map.tryFind id with
                        | Some category ->
                            match category.origin with
                            | BuiltInCategory ->
                                Error (BuiltInNotRemovableErr $"category '%s{category.name.value}' ('%s{string id.value}') ships with the app and cannot be removed")
                            | UserCategory ->
                                match materialsReferencingCategory id with
                                | [] ->
                                    store.Value <- store.Value |> Map.remove id
                                    Ok ()
                                | referencing ->
                                    let names =
                                        referencing
                                        |> List.map (fun e -> $"'%s{e.name.value}'")
                                        |> List.sort
                                        |> String.concat ", "
                                    Error (CategoryStillReferencedErr $"category '%s{category.name.value}' ('%s{string id.value}') is still referenced by %d{List.length referencing} material(s): %s{names}")
                        | None -> Error (unknown id)
            }

        /// In-memory category store initially filled with `standardCategories` (spec 0035 step 003).
        static member createInMemory (materialsReferencingCategory : CategoryId -> MaterialEntry list) : CategoryProxy =
            CategoryProxy.createInMemoryStore standardCategories materialsReferencingCategory

        /// Empty category store for CatalogueImportProxy; the catalogue import supplies its entries.
        /// Shares the seeded store's write/remove behavior (spec 0038 step 042; spec 0050 §3.4).
        static member createInMemoryEmpty (materialsReferencingCategory : CategoryId -> MaterialEntry list) : CategoryProxy =
            CategoryProxy.createInMemoryStore [] materialsReferencingCategory

    /// The composition-root referencing lookup for `CategoryProxy.createInMemory` (spec 0035 step
    /// 003): every material entry the materials store currently holds whose `category` is the given
    /// id. Backed by the LIVE step-006 `MaterialProxy` store (the `samplesReferencing` precedent,
    /// `ElementId.fs`) — once the referencing materials are re-categorised or removed the category
    /// becomes removable; there is no snapshot to refresh. The in-memory `listMaterials` is total
    /// (always `Ok`); the signature carries no error channel, so a future store whose listing can
    /// fail must supply its own conservative lookup instead of this one.
    let materialsReferencingCategory (materials : MaterialProxy) (id : CategoryId) : MaterialEntry list =
        // `IncludeInactive` so a category is held even by a material whose latest version is
        // retired — removing the category would otherwise orphan the inactive entry (spec 0038
        // step 021; the block reads the LATEST version of each material, superseded older
        // versions are view-only and never re-categorised).
        match materials.listMaterials IncludeInactive with
        | Ok all -> all |> List.filter (fun e -> e.category = id)
        | Error _ -> []
