namespace OpticalConstructor.Domain

open System
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalProperties.Dispersive
open OpticalProperties.Standard

/// The searchable, categorised materials library aggregate (§D.8). A `MaterialEntry`
/// pairs display metadata with the engine's `OpticalPropertiesWithDisp` (produced by
/// D.6/D.7); the `MaterialLibrary` holds the entry list plus pure linear search
/// (no indexing/caching — out of scope). The by-id `resolveMaterial` is the single
/// resolution seam Part B §B.6 (slice 005) and Part J §J.4 (slice 014) delegate to.
/// Built-in entries REUSE the already-coded engine presets (`siliconOpticalProperties`,
/// `langasiteOpticalProperties`, the `Standard` glass/crystal presets) — no dispersion
/// is re-derived here.
module MaterialLibrary =

    /// Material category for filtering (§D.8).
    type MaterialCategory =
        | Glass
        | Metal
        | Semiconductor
        | Crystal

    /// A library entry: a stable id (the `materialEntry` id, §A.7), a display name,
    /// a category, an optional description (mirroring the engine's `description`
    /// fields, e.g. `OpticalSystemWithDisp.description`, `Dispersion.fs:141`), and the
    /// engine's `OpticalPropertiesWithDisp` (`Dispersion.fs:53`).
    type MaterialEntry =
        {
            id : string
            name : string
            category : MaterialCategory
            description : string option
            properties : OpticalPropertiesWithDisp
        }

    /// Net-new error channel for material resolution (errors as values, §0). Returned
    /// — never thrown — by `resolveMaterial` on an unknown id.
    type MaterialError =
        | UnknownMaterialId of string

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
    let resolveMaterial (lib : MaterialLibrary) (id : string) (w : WaveLength) : Result<OpticalProperties, MaterialError> =
        match lib.entries |> List.tryFind (fun e -> e.id = id) with
        | Some e -> Ok (e.properties.getProperties w)
        | None -> Error (UnknownMaterialId id)

    /// Built-in entries (§D.8). Each wraps an existing engine preset as-is; none
    /// re-derives dispersion. `Silicon`/`Langasite` come from `Dispersive.fs:98,99`;
    /// the glass/crystal presets from `Standard.fs:64-70` (`.dispersive` lifts a
    /// non-dispersive `OpticalProperties` to `OpticalPropertiesWithDisp` as
    /// `EpsWithoutDisp`).
    let builtInEntries : MaterialEntry list =
        [
            {
                id = "silicon"
                name = "Silicon"
                category = Semiconductor
                description = Some "Crystalline silicon (engine preset Silicon)."
                properties = siliconOpticalProperties
            }
            {
                id = "langasite"
                name = "Langasite (La3Ga5SiO14)"
                category = Crystal
                description = Some "Langasite, optically active uniaxial crystal (engine preset Langasite)."
                properties = langasiteOpticalProperties
            }
            {
                id = "glass-1.52"
                name = "Transparent glass (n = 1.52)"
                category = Glass
                description = Some "Standard transparent glass preset."
                properties = OpticalProperties.transparentGlass.dispersive
            }
            {
                id = "glass-1.50"
                name = "Transparent glass (n = 1.50)"
                category = Glass
                description = None
                properties = OpticalProperties.transparentGlass150.dispersive
            }
            {
                id = "glass-1.75"
                name = "Transparent glass (n = 1.75)"
                category = Glass
                description = None
                properties = OpticalProperties.transparentGlass175.dispersive
            }
            {
                id = "glass-2.00"
                name = "Transparent glass (n = 2.00)"
                category = Glass
                description = None
                properties = OpticalProperties.transparentGlass200.dispersive
            }
            {
                id = "uniaxial-crystal"
                name = "Uniaxial crystal"
                category = Crystal
                description = Some "Standard uniaxial crystal preset."
                properties = OpticalProperties.uniaxialCrystal.dispersive
            }
            {
                id = "biaxial-crystal"
                name = "Biaxial crystal"
                category = Crystal
                description = Some "Standard biaxial crystal preset."
                properties = OpticalProperties.biaxialCrystal.dispersive
            }
        ]

    /// The default in-memory library seeded with the built-in entries (§D.8).
    let standard : MaterialLibrary = { entries = builtInEntries }
