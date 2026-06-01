/// §J.11 — in-app help, glossary & onboarding gallery [Standard] (010 Part II §1).
/// `Help` delivers a material/units glossary and field tooltips, plus a gallery of
/// worked examples shipped as getting-started sample projects. The gallery is built
/// from the engine's existing example scripts in `Analytics/Examples/` (the
/// AR/multilayer, DBR/EUV Mo-Si, active-crystal, wedge, and dispersive-glass cases);
/// each entry opens as a normal `OpticalConstructorProject` through the SAME
/// schema-validated open path a normal project takes (`Templates.loadTemplate`, the
/// validate-on-load core `ProjectFile.openProject` also runs — §I.3 / §A.7). The
/// `.fsx` files are the DESIGN REFERENCE only and are NEVER executed at runtime.
///
/// The units glossary STATES the §A.10 boundary conversions (eV via
/// `E[eV] = 1239.84 / λ[nm]`, cm⁻¹ via wavenumber) by CALLING the sole conversion
/// seam `Domain.Units` — it introduces no second conversion implementation.
/// Tooltips are static strings co-located here; an authorable/markdown
/// help-content pipeline is OUT OF SCOPE (§J.11). This module carries no Avalonia
/// type — the view binding is a later UI-wiring slice.
module OpticalConstructor.Ui.Help

open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open OpticalProperties.Standard
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.BeamTree
open OpticalConstructor.Domain.Project
open OpticalConstructor.Storage.Errors

// ---------------------------------------------------------------------------
// Units & material glossary (R-2). The eV / cm⁻¹ entries STATE the §A.10 boundary
// conversions by reusing `Domain.Units` — the literal `evNmProduct` and the
// `toMeters`/`fromMeters` seam — never a second conversion implementation.
// ---------------------------------------------------------------------------

/// One glossary entry: a term and its definition (static help content).
type GlossaryEntry =
    {
        term : string
        definition : string
    }

/// The photon energy in eV of a vacuum wavelength given in nm, via the SOLE
/// conversion seam (`Units.toMeters`/`fromMeters`, §A.10 / D.2). The glossary
/// states the eV relation by CALLING this boundary conversion, so the load-bearing
/// `evNmProduct` literal lives in exactly one place (`Units.fs`).
let photonEnergyEv (wavelengthNm : float) : float =
    Units.fromMeters Units.ElectronVolt (Units.toMeters Units.Nanometer wavelengthNm)

/// The wavenumber in cm⁻¹ of a vacuum wavelength given in nm, via the SAME seam
/// (`Units.fromMeters Wavenumber`). No second wavenumber formula is introduced.
let wavenumberPerCm (wavelengthNm : float) : float =
    Units.fromMeters Units.Wavenumber (Units.toMeters Units.Nanometer wavelengthNm)

/// The §A.10 units glossary (R-2). The eV and cm⁻¹ entries quote the canonical
/// boundary relations and cite the single `Domain.Units` seam that realises them
/// (the `evNmProduct` literal is surfaced from `Units`, not re-typed here).
let unitsGlossary : GlossaryEntry list =
    [
        { term = "Wavelength (λ)"
          definition = "The vacuum wavelength of the light. Stored canonical-SI in meters via WaveLength.value; entered/displayed in nm, µm, Å, or m at the UI boundary only." }
        { term = "Electron-volt (eV)"
          definition = sprintf "Photon energy. E[eV] = %g / λ[nm] (Domain.Units.evNmProduct); converted through Units.toMeters/fromMeters ElectronVolt — no second implementation." Units.evNmProduct }
        { term = "Wavenumber (cm⁻¹)"
          definition = "Spatial frequency ν̃ = 1 / λ. ν̃[cm⁻¹] = 1e7 / λ[nm]; λ[nm] = 1e7 / ν̃[cm⁻¹], via Domain.Units.toMeters/fromMeters Wavenumber." }
        { term = "Thickness"
          definition = "Layer thickness, stored canonical-SI in meters; Thickness.Infinity denotes a semi-infinite half-space (incident/exit medium or substrate)." }
        { term = "Refraction index (n)"
          definition = "Complex refractive index n = n′ + i·k of a material; k < 0 denotes optical gain (flagged by the J.9 validation seam)." }
    ]

/// A material glossary entry over the engine's stable material ids (curated colours
/// live in `Schematic.colorForMaterial`); static help content only.
let materialGlossary : GlossaryEntry list =
    [
        { term = "Vacuum / air"; definition = "n = 1, the default incident/exit medium (OpticalProperties.vacuum)." }
        { term = "Transparent glass"; definition = "Isotropic dielectric, n ≈ 1.52 (OpticalProperties.transparentGlass)." }
        { term = "Uniaxial crystal"; definition = "Birefringent, one optic axis (nₒ ≠ nₑ) — waveplates/retarders (OpticalProperties.uniaxialCrystal)." }
        { term = "Biaxial crystal"; definition = "Birefringent, two optic axes (n₁ ≠ n₂ ≠ n₃) (OpticalProperties.biaxialCrystal)." }
    ]

// ---------------------------------------------------------------------------
// Field tooltips (R-2). Static strings co-located here — no authorable/markdown
// content pipeline (out of scope §J.11).
// ---------------------------------------------------------------------------

/// One field tooltip: the field/control id and its static hint text.
type Tooltip =
    {
        field : string
        text : string
    }

/// The static field tooltips surfaced beside the editors (R-2).
let tooltips : Tooltip list =
    [
        { field = "incidenceAngle"; text = "Angle of the incident beam from the surface normal (0° = normal incidence)." }
        { field = "polarization"; text = "Linear polarization azimuth; s and p are the orthogonal reference states." }
        { field = "ellipticity"; text = "Polarization ellipticity; 0 = linear, ±1 = circular." }
        { field = "thickness"; text = "Film thickness in the layer's display unit; stored canonical-SI in meters." }
        { field = "wavelength"; text = "Vacuum wavelength; also expressible as eV or cm⁻¹ at the boundary (see the units glossary)." }
        { field = "wedgeAngle"; text = "Substrate wedge angle; 0° = a flat parallel plate." }
    ]

// ---------------------------------------------------------------------------
// Onboarding gallery (R-2). Shipped sample projects corresponding to the named
// `Analytics/Examples/*.fsx` cases. The gallery builds its samples through the
// SAME starting-project scaffold the template wizard uses — `Templates.film`,
// `Templates.systemOf`, `Templates.projectOf`, `Templates.glassPlate`,
// `Templates.glass`, `Templates.air`, and the single `Templates.defaultLight`
// (§A.7) — rather than forking those helpers (§0 constraint 2; reuse finding F1).
// Only the gallery-specific stacks, the `sourceScript` label, and `GalleryEntry`
// live here. The `.fsx` path is the design reference label only and is NEVER
// executed at runtime.
// ---------------------------------------------------------------------------

/// Multilayer thin-film AR/interference stack (design ref: `MultilayerThinFilm.fsx`):
/// alternating quarter-wave glass/vacuum films on a glass plate.
let private multilayerThinFilm () : OpticalConstructorProject =
    let cell = [ Templates.film Templates.glass 99.0; Templates.film Templates.air 150.0 ]
    Templates.systemOf "Multilayer thin film" (RepeatBuilder.expand cell 5) (Some Templates.glassPlate)
    |> Templates.projectOf

/// EUV Mo/Si distributed Bragg reflector (design ref: `MultilayerThinFilm_EUV.fsx`):
/// a periodic Mo/Si pair stack via `RepeatBuilder.expand` (§J.2), not an inline loop.
let private euvMoSi () : OpticalConstructorProject =
    let molybdenum = OpticalProperties.fromRefractionIndex (RefractionIndex 0.92)
    let silicon = OpticalProperties.fromRefractionIndex (RefractionIndex 1.00)
    let cell = [ Templates.film molybdenum 2.65; Templates.film silicon 2.65 ]
    Templates.systemOf "EUV Mo/Si multilayer mirror" (RepeatBuilder.expand cell 40) (Some Templates.glassPlate)
    |> Templates.projectOf

/// Active / birefringent crystal plate (design ref: `ActiveCrystal.fsx`): a thick
/// biaxial-crystal plate. (The gyrotropy of the .fsx case is its design reference;
/// the shipped sample is the equivalent crystal stack the editors then refine.)
let private activeCrystal () : OpticalConstructorProject =
    Templates.systemOf "Active crystal plate" [ Templates.film OpticalProperties.biaxialCrystal 1.0e7 ] None
    |> Templates.projectOf

/// Wedged crystal substrate (design ref: `Wedge_BiaxialCrystal.fsx`/`Wedge_Glass.fsx`):
/// a biaxial-crystal layer over a wedge substrate with a non-zero wedge angle.
let private wedge () : OpticalConstructorProject =
    let wedgeSubstrate =
        Wedge { layer = { properties = Templates.glass; thickness = Thickness.mm 1.0<mm> }; angle = WedgeAngle (Angle.degree 5.0) }
    Templates.systemOf "Wedged plate" [ Templates.film OpticalProperties.biaxialCrystal 2000.0 ] (Some wedgeSubstrate)
    |> Templates.projectOf

/// Dispersive-glass plate (design ref: `Glass_Dispersive.fsx`): a glass film on a
/// glass plate. (Wavelength dispersion is the .fsx design reference; the sample is
/// the equivalent glass stack.)
let private glassDispersive () : OpticalConstructorProject =
    Templates.systemOf "Dispersive glass" [ Templates.film Templates.glass 1000.0 ] (Some Templates.glassPlate)
    |> Templates.projectOf

/// A gallery entry: its title, the `Analytics/Examples/*.fsx` it is sourced from
/// (a design-reference LABEL, never executed), and its sample-project factory.
type GalleryEntry =
    {
        title : string
        sourceScript : string
        build : unit -> OpticalConstructorProject
    }

/// The shipped onboarding gallery (R-2): one sample per named example case.
let gallery : GalleryEntry list =
    [
        { title = "Multilayer thin film"; sourceScript = "MultilayerThinFilm.fsx"; build = multilayerThinFilm }
        { title = "EUV Mo/Si multilayer"; sourceScript = "MultilayerThinFilm_EUV.fsx"; build = euvMoSi }
        { title = "Active crystal";        sourceScript = "ActiveCrystal.fsx"; build = activeCrystal }
        { title = "Wedged plate";          sourceScript = "Wedge_BiaxialCrystal.fsx"; build = wedge }
        { title = "Dispersive glass";      sourceScript = "Glass_Dispersive.fsx"; build = glassDispersive }
    ]

/// Open a gallery entry's shipped sample project through the SAME schema-validated
/// open path a normal project takes (AC-J11): reuse `Templates.loadTemplate`, which
/// routes the literal project through `ProjectJson.serializeProject >>
/// deserializeProject` — the validate-on-load core `ProjectFile.openProject` also
/// runs (§I.3 / §A.7). The `.fsx` design reference is NEVER executed.
let openEntry (entry : GalleryEntry) : Result<OpticalConstructorProject, StorageError> =
    Templates.loadTemplate entry.build
