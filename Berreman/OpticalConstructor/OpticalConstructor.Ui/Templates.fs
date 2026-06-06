/// §J.3 — stack templates & wizards [Standard] (010 Part II §1). A fixed,
/// code-defined set of starting designs — AR coating, bandpass filter, beam
/// splitter, polarizer, waveplate, DBR — each a pure `unit ->
/// OpticalConstructorProject` (the §A.7 root aggregate) that pre-populates a
/// starting design then hands it to the ordinary editors. Each template builds
/// its stack from real reused `Layer`/`OpticalSystem` values (`Media.fs:24,94`)
/// over the engine's own `OpticalProperties` presets (`Standard.fs`), and the
/// periodic DBR stack is produced via `RepeatBuilder.expand` (§J.2) — never a
/// duplicated period loop.
///
/// Templates are F# literals: they read NOTHING from disk, are not user-editable
/// template files, and introduce no template-definition schema. Loading a
/// template goes through the SAME schema-validated project-open path as a normal
/// project (`loadTemplate` below routes through the slice-003 validate-on-load
/// core that `ProjectFile.openProject` also uses), so §A.7 schema validation
/// applies uniformly.
module OpticalConstructor.Ui.Templates

open Berreman.Constants
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open OpticalProperties.Standard
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.BeamTree
open OpticalConstructor.Domain.Project
open OpticalConstructor.Storage
open OpticalConstructor.Storage.Errors

// ---------------------------------------------------------------------------
// Shared building blocks. Real reused engine values only — no parallel material
// model, no re-derived dispersion (a template is a starting set of ordinary
// Layer/OpticalSystem records the editors then operate on).
//
// These are the canonical "wrap an OpticalSystem as a starting project" scaffold
// (§A.7). They are NON-private so the §J.11 onboarding gallery (`Help.fs`) builds
// its shipped samples through the SAME factories rather than forking them — the
// scaffold (and its single `defaultLight`) lives in exactly one place (§0
// constraint 2; reuse finding F1 of the cycle-1 review).
// ---------------------------------------------------------------------------

/// Incident (air) and a common glass exit/substrate medium, from the engine presets.
let air : OpticalProperties = OpticalProperties.vacuum
let glass : OpticalProperties = OpticalProperties.transparentGlass

/// A high / low index coating material (TiO2-like / MgF2-like) built through the
/// engine's own `OpticalProperties.fromRefractionIndex` constructor — index→tensor
/// construction is consumed, never re-implemented.
let private highIndex : OpticalProperties = OpticalProperties.fromRefractionIndex (RefractionIndex 2.30)
let private lowIndex  : OpticalProperties = OpticalProperties.fromRefractionIndex (RefractionIndex 1.38)

/// A film of `nm` nanometres of `props`; the thickness is stored canonical-SI
/// through the engine `Thickness.nm` constructor (`Media.fs:16`).
let film (props : OpticalProperties) (nm : float) : Layer =
    { properties = props; thickness = Thickness.nm (nm * 1.0<nm>) }

/// A 1 mm glass plate substrate (a real `Substrate.Plate`, `Media.fs:40`).
let glassPlate : Substrate =
    Plate { properties = glass; thickness = Thickness.mm 1.0<mm> }

/// The single default source for a starting project: a 550 nm normal-incidence
/// vacuum source (`IncidentLightInfo.create`); editors then refine it. Shared by
/// the template wizard AND the onboarding gallery so there is ONE default
/// wavelength, not the 600/550 drift the cycle-1 review flagged (F1).
let defaultLight : IncidentLightInfo = IncidentLightInfo.create (WaveLength.nm 550.0<nm>)

/// Wrap a single `OpticalSystem` as a one-`Sample`-node project (§A.7): the beam
/// tree's root is the sample over `sys`; `systems` holds it; no sources yet.
let projectOf (sys : OpticalSystem) : OpticalConstructorProject =
    let root =
        {
            element = Sample sys
            system = sys
            incident = defaultLight
            children = Map.empty
            defaultUnit = Nanometer
        }
    { beamTree = { root = root }; systems = [ sys ]; sources = []; placements = [] }

/// Assemble an `OpticalSystem` from films + optional substrate, air-incident over
/// a glass exit medium (`Media.fs:94`).
let systemOf (description : string) (films : Layer list) (substrate : Substrate option) : OpticalSystem =
    {
        description = Some description
        upper = air
        films = films
        substrate = substrate
        lower = glass
    }

// ---------------------------------------------------------------------------
// The DBR periodic stack (§J.2 reuse). The unit cell and period count are module
// values so the DBR template's films are provably `RepeatBuilder.expand dbrCell
// dbrPeriods` (AC-J3) and never an inline duplicated loop.
// ---------------------------------------------------------------------------

/// The DBR unit cell: a high-index then a low-index quarter-wave pair.
let dbrCell : Layer list = [ film highIndex 60.0; film lowIndex 100.0 ]

/// The DBR period count (number of high/low pairs).
let dbrPeriods : int = 8

// ---------------------------------------------------------------------------
// The six code-defined templates (R-3). Each is a pure unit -> project.
// ---------------------------------------------------------------------------

/// Anti-reflection coating: a single quarter-wave low-index layer on a glass plate.
let arCoating () : OpticalConstructorProject =
    systemOf "AR coating" [ film lowIndex 99.0 ] (Some glassPlate) |> projectOf

/// Bandpass filter: a quarter-wave H/L stack around a half-wave low-index spacer.
let bandpassFilter () : OpticalConstructorProject =
    let films =
        [ film highIndex 60.0; film lowIndex 100.0
          film lowIndex 200.0      // half-wave spacer (cavity)
          film highIndex 60.0; film lowIndex 100.0 ]
    systemOf "Bandpass filter" films (Some glassPlate) |> projectOf

/// Beam splitter: a single partially-reflecting high-index layer on a glass plate.
let beamSplitter () : OpticalConstructorProject =
    systemOf "Beam splitter" [ film highIndex 120.0 ] (Some glassPlate) |> projectOf

/// Polarizer: a birefringent uniaxial-crystal layer (engine `uniaxialCrystal` preset).
let polarizer () : OpticalConstructorProject =
    systemOf "Polarizer" [ film OpticalProperties.uniaxialCrystal 2000.0 ] None |> projectOf

/// Waveplate: a uniaxial-crystal retarder layer (engine `uniaxialCrystal` preset).
let waveplate () : OpticalConstructorProject =
    systemOf "Waveplate" [ film OpticalProperties.uniaxialCrystal 1375.0 ] None |> projectOf

/// Distributed Bragg reflector: a periodic H/L stack produced via
/// `RepeatBuilder.expand` (§J.2), NOT an inline duplicated loop.
let dbr () : OpticalConstructorProject =
    systemOf "Distributed Bragg reflector" (RepeatBuilder.expand dbrCell dbrPeriods) (Some glassPlate) |> projectOf

/// A named template entry for the wizard palette: its title and its factory.
type TemplateEntry =
    {
        title : string
        build : unit -> OpticalConstructorProject
    }

/// The fixed set of six starting designs surfaced by the template wizard (R-3).
let all : TemplateEntry list =
    [
        { title = "AR coating";       build = arCoating }
        { title = "Bandpass filter";  build = bandpassFilter }
        { title = "Beam splitter";    build = beamSplitter }
        { title = "Polarizer";        build = polarizer }
        { title = "Waveplate";        build = waveplate }
        { title = "DBR";              build = dbr }
    ]

// ---------------------------------------------------------------------------
// Loading a template through the schema-validated open path (R-3).
// ---------------------------------------------------------------------------

/// Load a template's project through the SAME schema-validated open path a normal
/// project takes (§I.3): serialize the literal project to canonical JSON and bind
/// it back through `ProjectJson.deserializeProject`, the validate-on-load core
/// that `ProjectFile.openProject` also runs (open = `ReadAllText >>
/// deserializeProject`). This applies the §A.7 schema uniformly to templates
/// without inventing a private deserialize — and without a disk round-trip.
let loadTemplate (build : unit -> OpticalConstructorProject) : Result<OpticalConstructorProject, StorageError> =
    build ()
    |> ProjectJson.serializeProject
    |> Result.bind ProjectJson.deserializeProject
