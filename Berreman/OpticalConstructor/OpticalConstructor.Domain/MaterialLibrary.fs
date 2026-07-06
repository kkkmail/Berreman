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

    /// Material category for filtering (§D.8). `Vacuum` (spec 0033 step 001) categorises the
    /// vacuum spacer entry the structural multilayer seeds reference.
    type MaterialCategory =
        | Glass
        | Metal
        | Semiconductor
        | Crystal
        | Vacuum

    /// A library entry: a stable id (the `materialEntry` id, §A.7), a display name,
    /// a category, an optional description (mirroring the engine's `description`
    /// fields, e.g. `OpticalSystemWithDisp.description`, `Dispersion.fs:141`), and the
    /// engine's `OpticalPropertiesWithDisp` (`Dispersion.fs:53`).
    type MaterialEntry =
        {
            id : MaterialId
            name : string
            category : MaterialCategory
            description : string option
            properties : OpticalPropertiesWithDisp
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

    /// Linear category filter (§D.8 — `List.filter`, no index).
    let byCategory (category : MaterialCategory) (lib : MaterialLibrary) : MaterialEntry list =
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
        | None -> Error (UnknownMaterialId (sprintf "unknown material id '%s'" (string id.value)))

    let resolveMaterial (lib : MaterialLibrary) (id : MaterialId) (w : WaveLength) : Result<OpticalProperties, MaterialError> =
        resolveMaterialWithDisp lib id |> Result.map (fun p -> p.getProperties w)

    /// Built-in entries (§D.8). Each wraps an existing engine preset as-is; none
    /// re-derives dispersion. `Silicon`/`Langasite` come from `Dispersive.fs:98,99`;
    /// the glass/crystal presets from `Standard.fs:64-70` (`.dispersive` lifts a
    /// non-dispersive `OpticalProperties` to `OpticalPropertiesWithDisp` as
    /// `EpsWithoutDisp`).
    let builtInEntries : MaterialEntry list =
        [
            {
                id = MaterialIds.silicon
                name = "Silicon"
                category = Semiconductor
                description = Some "Crystalline silicon (engine preset Silicon)."
                properties = siliconOpticalProperties
            }
            {
                id = MaterialIds.langasite
                name = "Langasite (La3Ga5SiO14)"
                category = Crystal
                description = Some "Langasite, optically active uniaxial crystal (engine preset Langasite)."
                properties = langasiteOpticalProperties
            }
            {
                id = MaterialIds.glass152
                name = "Transparent glass (n = 1.52)"
                category = Glass
                description = Some "Standard transparent glass preset."
                properties = OpticalProperties.transparentGlass.dispersive
            }
            {
                id = MaterialIds.glass150
                name = "Transparent glass (n = 1.50)"
                category = Glass
                description = None
                properties = OpticalProperties.transparentGlass150.dispersive
            }
            {
                id = MaterialIds.glass175
                name = "Transparent glass (n = 1.75)"
                category = Glass
                description = None
                properties = OpticalProperties.transparentGlass175.dispersive
            }
            {
                id = MaterialIds.glass200
                name = "Transparent glass (n = 2.00)"
                category = Glass
                description = None
                properties = OpticalProperties.transparentGlass200.dispersive
            }
            {
                id = MaterialIds.uniaxialCrystal
                name = "Uniaxial crystal"
                category = Crystal
                description = Some "Standard uniaxial crystal preset."
                properties = OpticalProperties.uniaxialCrystal.dispersive
            }
            {
                id = MaterialIds.biaxialCrystal
                name = "Biaxial crystal"
                category = Crystal
                description = Some "Standard biaxial crystal preset."
                properties = OpticalProperties.biaxialCrystal.dispersive
            }
            {
                id = MaterialIds.vacuum
                name = "Vacuum"
                category = Vacuum
                description = Some "Vacuum (n = 1) — the spacer material of the structural multilayer stacks."
                properties = OpticalProperties.vacuum.dispersive
            }
            {
                id = MaterialIds.euvMolybdenum
                name = "Molybdenum (Mo, EUV)"
                category = Metal
                description = Some "Molybdenum for EUV multilayers (engine preset, complex n around 10–13.5 nm)."
                properties = OpticalProperties.euvMolybdenum.dispersive
            }
            {
                id = MaterialIds.euvSilicon
                name = "Silicon (Si, EUV)"
                category = Semiconductor
                description = Some "Silicon for EUV multilayers (engine preset, complex n around 10–13.5 nm)."
                properties = OpticalProperties.euvSilicon.dispersive
            }
            {
                id = MaterialIds.activeCrystal
                name = "Active (gyrotropic) crystal"
                category = Crystal
                description = Some "Planar active (gyrotropic) crystal: n₁₁ = 2.315, n₃₃ = 2.226, optical-activity ρ₁₂ = 1.5e-6 (from ActiveCrystal.fsx)."
                properties =
                    (OpticalProperties.planarCrystal
                        (RefractionIndex 2.315 |> EpsValue.fromRefractionIndex)
                        (RefractionIndex 2.226 |> EpsValue.fromRefractionIndex)
                        (RhoValue 1.5e-6)).dispersive
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
    /// fragment (empty matches all — the `byNameContains` semantics), an optional
    /// `MaterialCategory`, and the dispersion facet. The query is DATA, so the materials
    /// panel drives one search seam (`MaterialProxy.searchMaterials`) instead of composing
    /// ad-hoc filter calls.
    type MaterialQuery =
        {
            text : string
            category : MaterialCategory option
            dispersion : DispersionFilter
        }

        /// The match-everything query (a search UI's initial state).
        static member empty : MaterialQuery =
            {
                text = ""
                category = None
                dispersion = AnyDispersion
            }

    /// The mutating materials write-seam (spec 0033 step 003, contract STORE_XDUO_0001 —
    /// DECLARED lifecycle): the functional-proxy convention `LibraryProxy` set
    /// (`ElementId.fs`), a record of camelCase `Result`-returning functions. A test
    /// substitutes a stub of the SAME shape. Function-valued fields have no structural
    /// equality, so the proxy compares by reference — a host model holding one keeps its
    /// (Elmish-required) equality. The real, persisting store behind this surface is the
    /// later IMPLEMENT_CONTRACT step (`OpticalConstructor.Storage`); until then the only
    /// producers are the in-memory mock below and test stubs.
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
    /// still carrying a function case (`EpsWithDisp` / `MuWithDisp` / `RhoWithDisp`). The
    /// classification `DispersionFilter` matches against; private to the mock.
    let private hasDispersion (e : MaterialEntry) : bool =
        match e.properties.epsWithDisp, e.properties.muWithDisp, e.properties.rhoWithDisp with
        | EpsWithoutDisp _, MuWithoutDisp _, RhoWithoutDisp _ -> false
        | _ -> true

    /// The blank-name validation the mock's write functions share (spec 0033 step 003):
    /// a `MaterialEntry` whose display name is empty/whitespace is `InvalidMaterial`.
    let private validateEntry (entry : MaterialEntry) : Result<unit, MaterialError> =
        if String.IsNullOrWhiteSpace entry.name
        then Error (InvalidMaterial (sprintf "material '%s' has a blank name" (string entry.id.value)))
        else Ok ()

    /// The in-memory mock `MaterialProxy` (spec 0033 step 003): an inline stub record over
    /// the FIXED built-in entry list — no IO, no mutation, deterministic for tests. The
    /// read functions answer from the fixed list (search reuses the §D.8 linear filters);
    /// the WRITE functions validate against it and return the typed outcome WITHOUT
    /// persisting anything: `addMaterial` rejects an id the list already holds
    /// (`DuplicateMaterialId`), `addMaterial`/`updateMaterial` reject a blank name
    /// (`InvalidMaterial`), `updateMaterial`/`removeMaterial` reject an unknown id
    /// (`UnknownMaterialId`). The real, persisting store is the later IMPLEMENT_CONTRACT
    /// step, leaving callers of this shape unchanged.
    let createInMemory () : MaterialProxy =
        let lib = standard
        let tryFind (id : MaterialId) : MaterialEntry option =
            lib.entries |> List.tryFind (fun e -> e.id = id)
        let unknown (id : MaterialId) : MaterialError =
            UnknownMaterialId (sprintf "unknown material id '%s'" (string id.value))
        {
            listMaterials = fun () -> Ok lib.entries
            searchMaterials =
                fun q ->
                    let byText = byNameContains q.text lib
                    let byCat =
                        match q.category with
                        | Some c -> byText |> List.filter (fun e -> e.category = c)
                        | None -> byText
                    match q.dispersion with
                    | AnyDispersion -> Ok byCat
                    | OnlyDispersive -> Ok (byCat |> List.filter hasDispersion)
                    | OnlyNonDispersive -> Ok (byCat |> List.filter (hasDispersion >> not))
            tryGetMaterial = fun id -> Ok (tryFind id)
            addMaterial =
                fun entry ->
                    validateEntry entry
                    |> Result.bind (fun () ->
                        match tryFind entry.id with
                        | Some existing ->
                            Error (DuplicateMaterialId (sprintf "material id '%s' already names '%s'" (string entry.id.value) existing.name))
                        | None -> Ok ())
            updateMaterial =
                fun entry ->
                    validateEntry entry
                    |> Result.bind (fun () ->
                        match tryFind entry.id with
                        | Some _ -> Ok ()
                        | None -> Error (unknown entry.id))
            removeMaterial =
                fun id ->
                    match tryFind id with
                    | Some _ -> Ok ()
                    | None -> Error (unknown id)
        }
