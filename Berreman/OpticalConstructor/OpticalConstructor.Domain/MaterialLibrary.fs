namespace OpticalConstructor.Domain

open System
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalProperties.Active
open OpticalConstructor.Domain.DispersionModels
open OpticalConstructor.Domain.CrystalPhysics

/// Material identities, editable physics, pure search and store contracts (§D.8; spec 0050 §3.3).
/// Built-in entries are loaded from Catalogue/Builtin through CatalogueImportProxy.
module MaterialLibrary =

    /// Stable material identity; create mints a new id and tryCreate parses an external Guid string.
    /// MaterialIds names the Built-in catalogue's identities (spec 0033 step 002; spec 0050 §3.3).
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

    /// Well-known identities of the Built-in catalogue's materials (spec 0033 step 002; spec 0050 §3.3).
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

    /// Stable category identity; create mints a new id for a user-authored category.
    /// CategoryIds names the Built-in catalogue's identities (spec 0035 step 001; spec 0050 §3.3).
    type CategoryId =
        | CategoryId of Guid

        member this.value = let (CategoryId g) = this in g
        static member create () : CategoryId = Guid.NewGuid() |> CategoryId

    /// Whether the create/edit picker offers a category; HiddenOnCreate omits it entirely
    /// (spec 0035 step 001; spec 0050 §3.3).
    type CategoryVisibility =
        | SelectableOnCreate
        | HiddenOnCreate

    /// Whether a category ships with the app or was authored by a user (spec 0035 step 001): a
    /// named two-case DU, never a naked `bool`.
    type CategoryOrigin =
        | BuiltInCategory
        | UserCategory

    /// Extensible category data resolved by CategoryId through the live category store
    /// (spec 0035 step 001; spec 0050 §3.3).
    type MaterialCategory =
        {
            id : CategoryId
            name : CategoryName
            visibility : CategoryVisibility
            origin : CategoryOrigin
        }

    /// Well-known identities of the Built-in catalogue's categories (spec 0035 step 001; spec 0050 §3.3).
    module CategoryIds =
        let glass : CategoryId = Guid.Parse "2a4b6c8d-1e3f-4a5b-8c7d-9e0f1a2b3c4d" |> CategoryId
        let metal : CategoryId = Guid.Parse "3b5c7d9e-2f4a-5b6c-9d8e-0f1a2b3c4d5e" |> CategoryId
        let semiconductor : CategoryId = Guid.Parse "4c6d8e0f-3a5b-6c7d-ae9f-1a2b3c4d5e6f" |> CategoryId
        let crystal : CategoryId = Guid.Parse "5d7e9f1a-4b6c-7d8e-bf0a-2b3c4d5e6f70" |> CategoryId
        let vacuumCategory : CategoryId = Guid.Parse "6e8fa02b-5c7d-8e9f-c01b-3c4d5e6f7081" |> CategoryId

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

    /// Category failures identify the refused operation, referenced entry or unavailable store
    /// (spec 0035 step 002; spec 0050 §3.4, S2.2).
    type CategoryError =
        | UnknownCategoryIdErr of reason : string
        | DuplicateCategoryIdErr of reason : string
        | CategoryStillReferencedErr of reason : string
        | BuiltInNotRemovableErr of reason : string
        | InvalidCategoryErr of reason : string
        | CategoryStoreUnavailableErr of reason : string

    /// Category reads and validated writes, with typed refusals (spec 0035 step 002; spec 0050 §3.4).
    /// Stores: CategoryProxy.createInMemory and Storage's CategoryProxy.createInDatabase.
    /// Function-valued fields make proxy equality reference-based.
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

    /// The forms a material's physics can take: the complexity tree or a crystal-parameter record
    /// (spec 0050 S11.1).
    type MaterialPhysics =
        | ComplexityPhysics of complexity : MaterialComplexity
        | CrystalParameterPhysics of crystal : CrystalParameters

        /// The physics' engine composition (spec 0050 S11.1).
        member this.toProperties : OpticalPropertiesWithDisp =
            match this with
            | ComplexityPhysics complexity -> complexity.toProperties
            | CrystalParameterPhysics crystal -> crystal.toProperties

    /// A library entry: a stable id (the `materialEntry` id, §A.7), a display name,
    /// a category, an optional description (mirroring the engine's `description`
    /// fields, e.g. `OpticalSystemWithDisp.description`, `Dispersion.fs:141`), and its
    /// physics (spec 0050 step 13, S11.1). Every field is data, so entries compare structurally.
    type MaterialEntry =
        {
            id : MaterialId
            name : EntryName
            category : CategoryId
            description : EntryDescription option
            physics : MaterialPhysics
        }

        /// The entry's engine composition (`OpticalPropertiesWithDisp`, `Dispersion.fs:53`),
        /// derived from its physics on every read (spec 0050 S11.1).
        member this.properties : OpticalPropertiesWithDisp = this.physics.toProperties

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
        /// Removal is refused while any version is pinned (spec 0038 Part H; spec 0050 S2.2).
        | MaterialVersionPinnedErr of reason : string
        | MaterialStoreUnavailableErr of reason : string

    type MaterialError with

        /// Reports an unresolved material identity (spec 0050 S2.2).
        static member unknownMaterial (id : MaterialId) : MaterialError =
            UnknownMaterialIdErr $"unknown material id '%s{string id.value}'"

        /// Reports removal blocked by sample references, with quoted, sorted names (spec 0050 S2.2).
        static member stillReferenced (entry : MaterialEntry) (samples : EntryName list) : MaterialError =
            let names =
                samples
                |> List.map (fun name -> $"'%s{name.value}'")
                |> List.sort
                |> String.concat ", "
            MaterialStillReferencedErr $"material '%s{entry.name.value}' ('%s{string entry.id.value}') is still referenced by %d{List.length samples} sample(s): %s{names}"

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
        | None -> Error (MaterialError.unknownMaterial id)

    let resolveMaterial (lib : MaterialLibrary) (id : MaterialId) (w : WaveLength) : Result<OpticalProperties, MaterialError> =
        resolveMaterialWithDisp lib id |> Result.map (fun p -> p.getProperties w)

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

    /// Monotonically increasing library version: entries start at first and pinned physics edits
    /// mint next (spec 0038 Part H; spec 0050 S2.2). The wire/disk integer is value.
    type VersionNumber =
        | VersionNumber of int

        member this.value = let (VersionNumber v) = this in v

        /// The successor version — one past this one. Pure; `VersionNumber.first.next` is version 2.
        member this.next : VersionNumber = VersionNumber (this.value + 1)

        /// The first version of a newly stored entry.
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

        /// References version one of a material, regardless of its latest stored version or pin
        /// (spec 0038 step 022; spec 0050 §3.3).
        static member firstOf (materialId : MaterialId) : MaterialVersionId =
            { materialId = materialId; version = VersionNumber.first }

    type MaterialError with

        /// Reports an unresolved material version (spec 0050 S2.2).
        static member unknownMaterialVersion (version : MaterialVersionId) : MaterialError =
            UnknownMaterialIdErr $"unknown material version '%s{string version.materialId.value}' v%d{version.version.value}"

        /// Reports removal blocked by pins, with ascending version numbers (spec 0050 S2.2).
        static member versionsPinned (entry : MaterialEntry) (versions : VersionNumber list) : MaterialError =
            let versionList =
                versions
                |> List.map (fun version -> version.value)
                |> List.sort
                |> List.map (fun n -> $"v%d{n}")
                |> String.concat ", "
            MaterialVersionPinnedErr $"material '%s{entry.name.value}' ('%s{string entry.id.value}') has %d{List.length versions} version(s) pinned: %s{versionList}"

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
    ///   latest version's pin (a new material inserts version 1; an existing one mutates in
    ///   place, mints the next version, or is a no-op);
    /// - `markMaterialInactive` / `markMaterialActive` — retire / revive the latest version;
    /// - `supersedeMaterial` — retire the latest version (a superseded version behaves as inactive);
    /// - `pinMaterialVersion` — pins one version permanently (spec 0050 S2.2);
    /// - `removeMaterial id` — keeps the `MaterialStillReferencedErr` hard-block AND additionally
    ///   refuses any material carrying a pinned version (`MaterialVersionPinnedErr`).
    ///
    /// Implemented by `MaterialProxy.createInMemory` and the SQLite
    /// `MaterialProxy.createInDatabase` store (spec 0050 S2.2).
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
            pinMaterialVersion : MaterialVersionId -> Result<unit, MaterialError>
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

    /// Filters by case-insensitive name fragment, optional category and dispersion
    /// (spec 0033 steps 003/006); an empty name fragment matches every entry.
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

    type CategoryError with

        /// Reports an unresolved category identity (spec 0050 S2.2).
        static member unknownCategory (id : CategoryId) : CategoryError =
            UnknownCategoryIdErr $"unknown category id '%s{string id.value}'"

        /// Reports a category identity already assigned to an existing category (spec 0050 S2.2).
        static member duplicateCategory (existing : MaterialCategory) : CategoryError =
            DuplicateCategoryIdErr $"category id '%s{string existing.id.value}' already names '%s{existing.name.value}'"

        /// Reports removal blocked by a category's built-in origin (spec 0050 S2.2).
        static member builtInNotRemovable (category : MaterialCategory) : CategoryError =
            BuiltInNotRemovableErr $"category '%s{category.name.value}' ('%s{string category.id.value}') ships with the app and cannot be removed"

        /// Reports removal blocked by material references, with quoted, sorted names (spec 0050 S2.2).
        static member stillReferenced (category : MaterialCategory) (materials : EntryName list) : CategoryError =
            let names =
                materials
                |> List.map (fun name -> $"'%s{name.value}'")
                |> List.sort
                |> String.concat ", "
            CategoryStillReferencedErr $"category '%s{category.name.value}' ('%s{string category.id.value}') is still referenced by %d{List.length materials} material(s): %s{names}"

    type CategoryProxy with

        static member private createInMemoryStore
            (initialCategories : MaterialCategory list)
            (materialsReferencingCategory : CategoryId -> MaterialEntry list)
            : CategoryProxy =
            let store = ref (initialCategories |> List.map (fun c -> c.id, c) |> Map.ofList)
            let currentCategories () : MaterialCategory list =
                store.Value |> Map.toList |> List.map snd
            {
                listCategories = fun () -> Ok (currentCategories ())
                addCategory =
                    fun category ->
                        category.validate
                        |> Result.bind (fun () ->
                            match store.Value |> Map.tryFind category.id with
                            | Some existing ->
                                Error (CategoryError.duplicateCategory existing)
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
                            | None -> Error (CategoryError.unknownCategory category.id))
                removeCategory =
                    fun id ->
                        match store.Value |> Map.tryFind id with
                        | Some category ->
                            match category.origin with
                            | BuiltInCategory ->
                                Error (CategoryError.builtInNotRemovable category)
                            | UserCategory ->
                                match materialsReferencingCategory id with
                                | [] ->
                                    store.Value <- store.Value |> Map.remove id
                                    Ok ()
                                | referencing ->
                                    let materials = referencing |> List.map (fun e -> e.name)
                                    Error (CategoryError.stillReferenced category materials)
                        | None -> Error (CategoryError.unknownCategory id)
            }

        /// Empty category store filled through validated writes (spec 0038 step 042; spec 0050 §3.4).
        /// Refuses duplicate ids and removal of built-in or referenced categories; built-ins remain renamable.
        static member createInMemory (materialsReferencingCategory : CategoryId -> MaterialEntry list) : CategoryProxy =
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
