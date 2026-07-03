namespace OpticalConstructor.Domain

open Berreman.Constants         // the nm / mm units of measure
open Berreman.Fields            // WaveLength
open Berreman.Media             // Thickness
open OpticalConstructor.Domain.Placement   // CatalogueKind

/// Spec 0027 (024) — the Library domain: an elevated, serializable `ElementId` (the stable
/// table-element identity, distinct from `valueId`), the kind-constrained Library presets
/// (Sample / Source / Detector / Polarizer), the one-or-more grouping-tree representations, and
/// the mock `LibraryProxy` (the functional-proxy seam — a record of `Result`-returning camelCase
/// functions, built by `createInMemory` closing over the seeded collection; a real, disk-backed
/// `create` would later live in `OpticalConstructor.Storage`, leaving the bay/logic unchanged).
/// Every primitive is elevated: ids/quantities are single-case DUs, the kinds are DUs (no enums),
/// and the error channel carries a `reason`.
module Library =

    /// Elevated, serializable table-element identity (spec §1 / §5). Distinct from `valueId` (which
    /// binds an element to a Library entry); the `id` is the element's own stable identity that
    /// experiments and setup steps reference (never an in-memory element reference, which cannot be
    /// serialized).
    type ElementId =
        | ElementId of string

        member this.value = let (ElementId s) = this in s
        static member create (s : string) : ElementId = ElementId s

    /// Module-level constructor for `ElementId` (the case name and type name collide, so a qualified
    /// `Library.ElementId.create` resolves to the case rather than the static member; this helper is the
    /// unambiguous call site used by the host).
    let elementId (s : string) : ElementId = ElementId s

    /// Whether a sample's geometry is a thin film, a thick plate, or a wedge (spec §2a). A DU, not a
    /// bool/enum, so the sample editor can add geometries case-by-case (compiler-guided).
    type SubstrateKind =
        | ThinFilm
        | Plate
        | Wedge

    /// A cut-out plate (spec §2a): a material cut to a thickness/plate geometry → Layer(s)/an
    /// `OpticalSystem` (the mapping to the engine is Phase 3). `materialId` keys into the existing
    /// `MaterialLibrary`.
    type Sample =
        {
            id : string
            name : string
            materialId : string
            thickness : Thickness
            substrate : SubstrateKind
            /// A human-readable description of what the sample IS — materials + thicknesses + stack —
            /// shown in the Details element-view and the Library confirm step. Multilayer samples spell
            /// out the repeating unit and layer count rather than a per-layer list.
            description : string
        }

    /// A monochromatic source preset — defines λ (spec Q3: source = wavelength).
    type SourcePreset =
        {
            id : string
            name : string
            waveLength : WaveLength
        }

    /// The detector type fixes the measurement (spec §4): Intensity records S0; Ellipsometer records
    /// Ψ/Δ. A DU, not an enum, so non-ideal/compound detectors are a non-breaking addition later.
    type DetectorKind =
        | Intensity
        | Ellipsometer

    type DetectorPreset =
        {
            id : string
            name : string
            kind : DetectorKind
        }

    /// Ideal polarizers only now (spec Q2); non-ideal variants are added later. The circular cases
    /// carry handedness (left / right).
    type PolarizerKind =
        | IdealLinear
        | IdealCircularLeft
        | IdealCircularRight

    type PolarizerPreset =
        {
            id : string
            name : string
            kind : PolarizerKind
        }

    /// The choosable, kind-constrained Library things (spec §2a). The entry id IS the `valueId`
    /// written onto the bound table element.
    type LibraryEntry =
        | SampleItem of Sample
        | SourceItem of SourcePreset
        | DetectorItem of DetectorPreset
        | PolarizerItem of PolarizerPreset

        /// The entry id (== the `valueId` written onto the bound table element).
        member this.entryId : string =
            match this with
            | SampleItem s -> s.id
            | SourceItem s -> s.id
            | DetectorItem d -> d.id
            | PolarizerItem p -> p.id

        member this.displayName : string =
            match this with
            | SampleItem s -> s.name
            | SourceItem s -> s.name
            | DetectorItem d -> d.name
            | PolarizerItem p -> p.name

        /// The FULL, human-readable description of what this entry IS (spec 0027 / 026 — the Library
        /// confirm step and the Details element-view show it before / after binding). A sample carries its
        /// own curated `description` (materials + thicknesses + stack); the other presets get prose built
        /// from their kind / wavelength so a bare "Ideal linear polarizer" is never the whole story.
        member this.fullDescription : string =
            match this with
            | SampleItem s -> s.description
            | SourceItem s ->
                let wNm = s.waveLength.value / nmToMeter / oneNanometer
                sprintf "Monochromatic light source at %g nm." wNm
            | DetectorItem d ->
                match d.kind with
                | Intensity -> "Intensity detector — records the transmitted irradiance S₀."
                | Ellipsometer -> "Ellipsometer — records the ellipsometric angles Ψ and Δ."
            | PolarizerItem p ->
                match p.kind with
                | IdealLinear -> "Ideal linear polarizer — transmits the linear component along its R1 orientation."
                | IdealCircularLeft -> "Ideal circular polarizer (left-handed) — transmits left-circular light."
                | IdealCircularRight -> "Ideal circular polarizer (right-handed) — transmits right-circular light."

        /// The catalogue kinds this entry is valid for (kind-constrained selection, §2a). A polarizer
        /// entry serves either the LinearPolarizer role (the ideal LP) or the CircularPolarizer role
        /// (the two CP presets) — never both.
        member this.forKinds : CatalogueKind list =
            match this with
            | SampleItem _ -> [ CatalogueKind.Sample ]
            | SourceItem _ -> [ LightSource ]
            | DetectorItem _ -> [ Detector ]
            | PolarizerItem p ->
                match p.kind with
                | IdealLinear -> [ LinearPolarizer ]
                | IdealCircularLeft | IdealCircularRight -> [ CircularPolarizer ]

    /// A node-path label (a tree grouping level: "Samples", "Glass", a glass kind, …). Elevated so a
    /// label is never a bare string in the domain.
    type TreeLabel =
        | TreeLabel of string

        member this.value = let (TreeLabel s) = this in s

    /// One tree representation (R3): a labelled grouping with either child groups or leaf entries.
    /// Leaves carry the entry id (= `valueId`); the host resolves the full entry via `tryGetEntry`.
    type LibraryTreeNode =
        | Group of label : TreeLabel * children : LibraryTreeNode list
        | Leaf of label : TreeLabel * entryId : string

    /// One named Library representation (R3 — there may be more than one tree, by different
    /// groupings).
    type LibraryTree =
        {
            name : string
            root : LibraryTreeNode
        }

    /// The Library error channel (errors as values; each case carries a `reason`).
    type LibraryError =
        | UnknownEntryId of reason : string
        | NoEntriesForKind of reason : string
        | LibraryUnavailable of reason : string

    /// The mock Library IO seam (the functional-proxy convention): a record of camelCase
    /// `Result`-returning functions. A test substitutes a stub of the SAME shape. Function-valued
    /// fields have no structural equality, so the proxy compares by reference — this lets a host model
    /// that holds a proxy keep its (Elmish-required) equality, comparing the proxy by identity.
    [<ReferenceEquality>]
    type LibraryProxy =
        {
            entriesForKind : CatalogueKind -> Result<LibraryEntry list, LibraryError>
            libraryTrees : unit -> Result<LibraryTree list, LibraryError>
            tryGetEntry : string -> Result<LibraryEntry option, LibraryError>
        }

    /// The seeded Library entries (spec §2a "Seeded entries"): samples (glass plate → second
    /// thickness → thin film → a quarter-wave multilayer placeholder), the two detectors, one ideal
    /// LP + two ideal CP, and one monochromatic source.
    let seedEntries : LibraryEntry list =
        [
            SampleItem
                {
                    id = "sample-glass-1mm"
                    name = "Glass plate (n=1.52, 1 mm)"
                    materialId = "glass-1.52"
                    thickness = Thickness.mm 1.0<mm>
                    substrate = Plate
                    description = "Single transparent-glass plate, n = 1.52, thickness 1 mm, in vacuum."
                }
            SampleItem
                {
                    id = "sample-glass-2mm"
                    name = "Glass plate (n=1.52, 2 mm)"
                    materialId = "glass-1.52"
                    thickness = Thickness.mm 2.0<mm>
                    substrate = Plate
                    description = "Single transparent-glass plate, n = 1.52, thickness 2 mm, in vacuum."
                }
            SampleItem
                {
                    id = "sample-glass-film-600"
                    name = "Glass thin film (n=1.75, 600 nm)"
                    materialId = "glass-1.75"
                    thickness = Thickness.nm 600.0<nm>
                    substrate = ThinFilm
                    description = "Single transparent-glass thin film, n = 1.75, thickness 600 nm, between vacuum."
                }
            SampleItem
                {
                    id = "sample-glass-vacuum"
                    name = "Glass / vacuum interface (n=1.50)"
                    materialId = "glass-1.50"
                    thickness = Thickness.mm 1.0<mm>
                    substrate = Plate
                    description = "Semi-infinite transparent-glass / vacuum interface, n = 1.50 — the Fresnel / total-reflection demo."
                }
            SampleItem
                {
                    id = "sample-glass-film-200"
                    name = "Glass thin film (n=1.52, 200 nm)"
                    materialId = "glass-1.52"
                    thickness = Thickness.nm 200.0<nm>
                    substrate = ThinFilm
                    description = "Single transparent-glass thin film, n = 1.52, 200 nm, between vacuum."
                }
            SampleItem
                {
                    id = "sample-multilayer-qw"
                    name = "Quarter-wave glass/vacuum multilayer (41 layers)"
                    materialId = "glass-1.52"
                    thickness = Thickness.nm 100.0<nm>
                    substrate = ThinFilm
                    description = "41-layer quarter-wave stack: alternating glass (n=1.52) and vacuum λ/4 films for 600 nm, 21 glass + 20 vacuum layers."
                }
            SampleItem
                {
                    id = "sample-euv-mosi"
                    name = "EUV Mo/Si multilayer (100 pairs)"
                    materialId = "euv-mo-si"
                    thickness = Thickness.nm 2.65<nm>
                    substrate = ThinFilm
                    description = "EUV reflective multilayer: 100 Mo/Si bilayers, each layer 2.65 nm (λ/4 at 10.6 nm), on vacuum."
                }
            SampleItem
                {
                    id = "sample-uniaxial"
                    name = "Uniaxial crystal film (1 µm)"
                    materialId = "uniaxial-crystal"
                    thickness = Thickness.nm 1000.0<nm>
                    substrate = ThinFilm
                    description = "Uniaxial crystal thin film, nₒ = 1.5, nₑ = 1.65, thickness 1 µm, between vacuum."
                }
            SampleItem
                {
                    id = "sample-biaxial"
                    name = "Biaxial crystal film (1 µm)"
                    materialId = "biaxial-crystal"
                    thickness = Thickness.nm 1000.0<nm>
                    substrate = ThinFilm
                    description = "Biaxial crystal thin film, n = (1.5, 1.65, 1.75), thickness 1 µm, between vacuum."
                }
            SampleItem
                {
                    id = "sample-active-crystal"
                    name = "Active gyrotropic crystal plate (1 cm)"
                    materialId = "active-crystal"
                    thickness = Thickness.oneCentiMeter
                    substrate = Plate
                    description = "Planar active (gyrotropic) crystal plate, n₁₁ = 2.315, n₃₃ = 2.226, optical-activity ρ₁₂ = 1.5e-6, thickness 1 cm."
                }
            SampleItem
                {
                    id = "sample-langasite-silicon"
                    name = "Langasite film on silicon (10 µm, dispersive)"
                    materialId = "langasite-on-silicon"
                    thickness = Thickness.mm 0.01<mm>
                    substrate = ThinFilm
                    description = "Dispersive langasite thin film (10 µm) on a silicon substrate — wavelength-dependent n, k."
                }
            DetectorItem { id = "det-intensity"; name = "Intensity detector"; kind = Intensity }
            DetectorItem { id = "det-ellipsometer"; name = "Ellipsometer"; kind = Ellipsometer }
            PolarizerItem { id = "pol-lp"; name = "Ideal linear polarizer"; kind = IdealLinear }
            PolarizerItem { id = "pol-cp-left"; name = "Ideal circular polarizer (left)"; kind = IdealCircularLeft }
            PolarizerItem { id = "pol-cp-right"; name = "Ideal circular polarizer (right)"; kind = IdealCircularRight }
            SourceItem { id = "src-600"; name = "Monochromatic 600 nm"; waveLength = WaveLength.nm 600.0<nm> }
        ]

    /// One canonical grouping tree (R3): the entries organised by kind, with the two glass plates
    /// nested under their shared material (the "same glass, different thickness" grouping §2a).
    let private seedTrees : LibraryTree list =
        [
            {
                name = "By kind"
                root =
                    Group
                        (TreeLabel "Library",
                         [
                             Group
                                 (TreeLabel "Samples",
                                  [
                                      Group
                                          (TreeLabel "Glass (n=1.52)",
                                           [
                                               Leaf (TreeLabel "1 mm plate", "sample-glass-1mm")
                                               Leaf (TreeLabel "2 mm plate", "sample-glass-2mm")
                                               Leaf (TreeLabel "200 nm film", "sample-glass-film-200")
                                           ])
                                      Leaf (TreeLabel "Glass film (n=1.75)", "sample-glass-film-600")
                                      Leaf (TreeLabel "Glass / vacuum interface (n=1.50)", "sample-glass-vacuum")
                                      Group
                                          (TreeLabel "Multilayers",
                                           [
                                               Leaf (TreeLabel "Quarter-wave glass/vacuum (41)", "sample-multilayer-qw")
                                               Leaf (TreeLabel "EUV Mo/Si (100 pairs)", "sample-euv-mosi")
                                           ])
                                      Group
                                          (TreeLabel "Crystals",
                                           [
                                               Leaf (TreeLabel "Uniaxial film", "sample-uniaxial")
                                               Leaf (TreeLabel "Biaxial film", "sample-biaxial")
                                               Leaf (TreeLabel "Active gyrotropic plate", "sample-active-crystal")
                                           ])
                                      Group
                                          (TreeLabel "Dispersive",
                                           [
                                               Leaf (TreeLabel "Langasite on silicon", "sample-langasite-silicon")
                                           ])
                                  ])
                             Group (TreeLabel "Sources", [ Leaf (TreeLabel "600 nm", "src-600") ])
                             Group
                                 (TreeLabel "Detectors",
                                  [
                                      Leaf (TreeLabel "Intensity", "det-intensity")
                                      Leaf (TreeLabel "Ellipsometer", "det-ellipsometer")
                                  ])
                             Group
                                 (TreeLabel "Polarizers",
                                  [
                                      Leaf (TreeLabel "Linear", "pol-lp")
                                      Leaf (TreeLabel "Circular (L)", "pol-cp-left")
                                      Leaf (TreeLabel "Circular (R)", "pol-cp-right")
                                  ])
                         ])
            }
        ]

    /// The in-memory mock proxy (spec §3 / Q7): closes over the seeded collection and tree, no IO,
    /// deterministic for tests. A real disk-backed `create` would live in `OpticalConstructor.Storage`.
    let createInMemory () : LibraryProxy =
        let entries = seedEntries
        let trees = seedTrees
        {
            entriesForKind = fun kind -> Ok (entries |> List.filter (fun e -> e.forKinds |> List.contains kind))
            libraryTrees = fun () -> Ok trees
            tryGetEntry = fun id -> Ok (entries |> List.tryFind (fun e -> e.entryId = id))
        }

/// Spec 0027 (024) Phase 2 — the Experiments domain: an `Experiment` (a choice of WHICH element's R1
/// makes a full circle, by serializable `ElementId`), the ordered `ExperimentSet` (setup steps before
/// each experiment), and the mock `ExperimentProxy` (the same functional-proxy seam as the Library —
/// a record of `Result`-returning camelCase functions, built by `createInMemory` closing over the
/// seeded sets; a real disk-backed `create` would later live in `OpticalConstructor.Storage`). The
/// experiment / setup-step types are single-case DUs NOW, so adding kinds later is compiler-guided
/// (the existing match sites are flagged), and an experiment references an element by its serializable
/// `id`, never by an in-memory reference (so the set survives save-load).
module Experiments =

    open Library

    /// An experiment (spec §2b / Q4): WHICH element makes a 1-D sweep, named by its serializable
    /// `ElementId`. The three kinds (added spec 0027 / 026): rotate the swept element's R1 a full circle
    /// (the rotating-analyzer / Malus measurement); vary the swept element's R2 (incidence) over 0…89°
    /// (drawn to 90 — 90 itself is not computable, spec 026); vary the wavelength over a user range.
    type Experiment =
        | RotateR1FullCircle of ElementId
        | SweepR2 of ElementId
        | SweepWaveLength of ElementId

        /// The element this experiment sweeps (by id).
        member this.sweptElement : ElementId =
            match this with
            | RotateR1FullCircle id
            | SweepR2 id
            | SweepWaveLength id -> id

        /// A short, human-readable description (the bay readout uses this).
        member this.description : string =
            match this with
            | RotateR1FullCircle id -> sprintf "rotate %s R1 over 0…360°" id.value
            | SweepR2 id -> sprintf "sweep %s incidence (R2) over 0…90°" id.value
            | SweepWaveLength id -> sprintf "sweep wavelength for %s" id.value

    /// A setup step applied before an experiment (spec §2b). A single-case DU so steps grow
    /// compiler-guided (swap LP→CP, set incidence, … are future cases): set an element's R1 angle.
    type SetupStep =
        | SetElementR1 of element : ElementId * degrees : float

    /// An ordered experiment set: each experiment preceded by its setup steps (spec §2b). Build / edit
    /// only now — no solve (that is Phase 3).
    type ExperimentSet =
        {
            id : string
            name : string
            steps : (SetupStep list * Experiment) list
        }

    /// The Experiments error channel (errors as values; each case carries a `reason`).
    type ExperimentError =
        | UnknownExperimentSet of reason : string
        | ExperimentUnavailable of reason : string

    /// The mock Experiments IO seam (the functional-proxy convention): a record of camelCase
    /// `Result`-returning functions. A test substitutes a stub of the SAME shape. Function-valued
    /// fields have no structural equality, so the proxy compares by reference — letting a host model
    /// that holds the proxy keep its (Elmish-required) equality.
    [<ReferenceEquality>]
    type ExperimentProxy =
        {
            listExperimentSets : unit -> Result<ExperimentSet list, ExperimentError>
            tryGetExperimentSet : string -> Result<ExperimentSet option, ExperimentError>
        }

    /// The seeded experiment sets (the editable / listable templates — build / edit, no solve yet). One
    /// canonical demo: rotate the analyzer's R1 a full circle (the rotating-analyzer measurement §2b).
    /// The referenced element id ("analyzer") is the template's placeholder; the bay lets the user pick
    /// WHICH present element is actually swept (by its live id) from the scene.
    let seedExperimentSets : ExperimentSet list =
        [
            {
                id = "exp-rotate-analyzer"
                name = "Rotate analyzer (full circle)"
                steps = [ ([], RotateR1FullCircle (ElementId.create "analyzer")) ]
            }
            {
                id = "exp-set-input-then-rotate"
                name = "Set input 45°, rotate analyzer"
                steps =
                    [
                        ([ SetElementR1 (ElementId.create "input", 45.0) ], RotateR1FullCircle (ElementId.create "analyzer"))
                    ]
            }
        ]

    /// The in-memory mock proxy (spec §3 / Q7): closes over the seeded sets, no IO, deterministic for
    /// tests. A real disk-backed `create` would live in `OpticalConstructor.Storage`, leaving the
    /// bay / logic unchanged.
    let createInMemory () : ExperimentProxy =
        let sets = seedExperimentSets
        {
            listExperimentSets = fun () -> Ok sets
            tryGetExperimentSet = fun id -> Ok (sets |> List.tryFind (fun s -> s.id = id))
        }
