namespace OpticalConstructor.Domain

open System                     // Guid (the SampleId backing + the fixed seed literals)
open Berreman.Constants         // the nm / mm units of measure
open Berreman.Geometry          // RotationConvention / Angle / Rotation (CrystalOrientation, spec 0033 step 020)
open Berreman.Fields            // WaveLength
open Berreman.Media             // Thickness
open OpticalConstructor.Domain.Placement   // CatalogueKind
open OpticalConstructor.Domain.MaterialLibrary   // MaterialId / MaterialIds (spec 0033 step 002); the materials store seam (step 006)

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

    /// Elevated sample identity (spec 0033 step 002): a Guid-backed single-case DU — no raw string
    /// sample id appears in a domain record. `create` MINTS a fresh id; the seeded samples parse FIXED
    /// literal Guids (`SeedSamples`) so the grouping trees and the tests stay deterministic across
    /// runs. The Selector `valueId` binding seam stays a string: `LibraryEntry.entryId` carries the
    /// Guid STRING form.
    type SampleId =
        | SampleId of Guid

        member this.value = let (SampleId g) = this in g
        static member create () : SampleId = Guid.NewGuid() |> SampleId

    /// Module-level mint helper for `SampleId` (the case name and type name collide — the same
    /// collision `elementId` documents; this is the unambiguous call site for qualified callers).
    let newSampleId () : SampleId = SampleId.create ()

    /// Whether a sample's geometry is a thin film, a thick plate, or a wedge (spec §2a). A DU, not a
    /// bool/enum, so the sample editor can add geometries case-by-case (compiler-guided).
    type SubstrateKind =
        | ThinFilm
        | Plate
        | Wedge

    /// How a layer's crystal tensors are oriented relative to the lab frame (spec 0033 step 020):
    /// `PrimaryAxes` keeps the material's own principal axes (the identity — tensors exactly as
    /// stored); `EulerRotation` orients them by Euler angles under an explicit engine
    /// `RotationConvention`. This is DATA on the layer — nothing is stored rotated; the step-1
    /// system builder applies the rotation via `Layer.rotate` when it assembles the engine system.
    type CrystalOrientation =
        | PrimaryAxes
        | EulerRotation of convention : RotationConvention * phi : Angle * theta : Angle * psi : Angle

        /// The engine rotation this orientation denotes: `PrimaryAxes` is the identity;
        /// `EulerRotation` builds `Rotation.create convention phi theta psi`. The named engine
        /// shortcuts (`Rotation.rotatePiX`, `Rotation.rotateHalfPiY`, …) remain available for tests.
        member this.toRotation : Rotation =
            match this with
            | PrimaryAxes -> RealMatrix3x3.identity |> Rotation
            | EulerRotation (convention, phi, theta, psi) -> Rotation.create convention phi theta psi |> Rotation

    /// One physical layer of a sample's stack (spec 0033 step 001 — the stack is DATA): a material
    /// reference plus a thickness, plus the crystal orientation of its tensors (spec 0033 step 020 —
    /// `PrimaryAxes` unless the sample says otherwise; the rotation is applied at system-build time,
    /// never stored). `materialId` is the elevated `MaterialLibrary.MaterialId`
    /// (spec 0033 step 002) — the same key `resolveMaterialWithDisp` looks up.
    type SampleLayer =
        {
            materialId : MaterialId
            thickness : Thickness
            orientation : CrystalOrientation
        }

    /// A repeated unit cell (period) of layers — the DBR / Bragg / EUV-Mo–Si shape. `cell` is the
    /// ordered unit cell; `count` the number of periods.
    type PeriodGroup =
        {
            cell : SampleLayer list
            count : int
        }

    /// One item of a sample's film stack: a single layer, or a repeated period group.
    type StackItem =
        | SingleLayer of SampleLayer
        | Repeated of PeriodGroup

    /// A sample's full material structure (spec 0033 step 001): the film stack (top to bottom), an
    /// optional thick substrate plate, and the lower half-space material (`None` = vacuum). This is the
    /// DATA the engine mapping expands — no per-sample-id branching anywhere downstream.
    type SampleStructure =
        {
            films : StackItem list
            substrate : SampleLayer option
            lower : MaterialId option
        }

        /// The flattened film layers in order — each `Repeated` expands to `count` copies of its cell
        /// (mirrors `RepeatBuilder.expand`: `List.replicate count cell |> List.concat`). Pure.
        member this.expandedFilms : SampleLayer list =
            this.films
            |> List.collect (fun item ->
                match item with
                | SingleLayer l -> [ l ]
                | Repeated g -> List.replicate g.count g.cell |> List.concat)

        /// Every material id the structure references (spec 0033 step 006): each film layer's
        /// material (a `Repeated` group's cell counted once — repetition adds no new
        /// reference), the substrate plate's material, and the lower half-space material.
        /// Pure — the referencing lookup `MaterialProxy.removeMaterial` consults is built
        /// over this (`samplesReferencing`).
        member this.referencedMaterials : Set<MaterialId> =
            let filmMaterialIds =
                this.films
                |> List.collect (fun item ->
                    match item with
                    | SingleLayer l -> [ l.materialId ]
                    | Repeated g -> g.cell |> List.map (fun l -> l.materialId))
            let substrateMaterialIds =
                this.substrate |> Option.toList |> List.map (fun l -> l.materialId)
            let lowerMaterialIds = this.lower |> Option.toList
            filmMaterialIds @ substrateMaterialIds @ lowerMaterialIds |> Set.ofList

    /// A cut-out plate (spec §2a): a material structure cut to a plate / thin-film geometry → Layer(s)/an
    /// `OpticalSystem` (the mapping to the engine is Phase 3). The `structure` is the material facet
    /// (what the sample is made of, as data); `substrate` stays the geometry facet.
    type Sample =
        {
            id : SampleId
            name : string
            structure : SampleStructure
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

        /// The entry id (== the `valueId` written onto the bound table element). A sample's elevated
        /// `SampleId` crosses this seam as its Guid STRING form (spec 0033 step 002) so the Selector
        /// `valueId` binding stays a plain string.
        member this.entryId : string =
            match this with
            | SampleItem s -> string s.id.value
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
                $"Monochromatic light source at %g{wNm} nm."
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

    /// A samples-library search query (spec 0033 step 004): a case-insensitive name fragment
    /// (empty matches all) and an optional `SubstrateKind` facet. The query is DATA, so the
    /// samples panel drives one search seam (`SampleProxy.searchSamples`) instead of composing
    /// ad-hoc filter calls — the `MaterialQuery` convention (`MaterialLibrary.fs`).
    type SampleQuery =
        {
            text : string
            substrate : SubstrateKind option
        }

        /// The match-everything query (a search UI's initial state).
        static member empty : SampleQuery =
            {
                text = ""
                substrate = None
            }

    /// The samples write-seam error channel (errors as values; each case carries a diagnostic
    /// `reason` — a bare error case is useless in a log): an unknown id on lookup / update /
    /// remove, adding a sample under an id the library already holds, and rejecting a
    /// malformed sample.
    type SampleError =
        | UnknownSampleId of reason : string
        | DuplicateSampleId of reason : string
        | InvalidSample of reason : string

    /// The mutating samples write-seam (spec 0033 steps 004/005, contract STORE_XDUO_0002 —
    /// IMPLEMENTED lifecycle): the same functional-proxy shape as step 003's `MaterialProxy`
    /// (`MaterialLibrary.fs`) — a record of camelCase `Result`-returning functions; a test
    /// substitutes a stub of the SAME shape. Function-valued fields have no structural equality,
    /// so the proxy compares by reference — a host model holding one keeps its (Elmish-required)
    /// equality. The real, stateful in-memory store behind this surface is
    /// `SampleProxy.createInMemory` (declared as a type augmentation below `seedEntries`, which
    /// seeds it).
    [<ReferenceEquality>]
    type SampleProxy =
        {
            listSamples : unit -> Result<Sample list, SampleError>
            searchSamples : SampleQuery -> Result<Sample list, SampleError>
            tryGetSample : SampleId -> Result<Sample option, SampleError>
            addSample : Sample -> Result<unit, SampleError>
            updateSample : Sample -> Result<unit, SampleError>
            removeSample : SampleId -> Result<unit, SampleError>
        }

    /// A single-layer thin-film structure between vacuum (the common seed shape).
    let private filmStructure (materialId : MaterialId) (thickness : Thickness) : SampleStructure =
        {
            films = [ SingleLayer { materialId = materialId; thickness = thickness; orientation = PrimaryAxes } ]
            substrate = None
            lower = None
        }

    /// A thick-plate structure in vacuum (films empty; the plate is the substrate layer).
    let private plateStructure (materialId : MaterialId) (thickness : Thickness) : SampleStructure =
        {
            films = []
            substrate = Some { materialId = materialId; thickness = thickness; orientation = PrimaryAxes }
            lower = None
        }

    /// λ/4 film thicknesses of the 600 nm quarter-wave stack (glass n = 1.52; vacuum n = 1).
    let private qwGlassThickness : Thickness = Thickness.nm ((600.0 / 1.52 / 4.0) * oneNanometer)
    let private qwVacuumThickness : Thickness = Thickness.nm ((600.0 / 1.00 / 4.0) * oneNanometer)

    /// λ/4 at 10.6 nm — each EUV Mo/Si layer (2.65 nm, from MultilayerThinFilm_EUV.fsx).
    let private euvLayerThickness : Thickness = Thickness.nm (10.6 / 4.0 * oneNanometer)

    /// A foldable STARTER multilayer structure (spec 0035 step 014): ONE `Repeated` period of a
    /// two-layer glass (n=1.52) / vacuum cell at the 600 nm λ/4 thicknesses (the `multilayerQw`
    /// seed's unit cell), `count = 1` — the minimal seed the Sample editor's Make-multilayer path
    /// opens onto, ready for the `SampleStackEditor` K-stepper (`SetRepeatCount`) to build up into a
    /// full stack. This is bare structure DATA, NOT a seeded `Sample`: the editor opens a brand-new
    /// sample over it, so Save mints a fresh `SampleId` (`SampleId.create`) rather than reusing a
    /// seeded id.
    let starterMultilayerStructure : SampleStructure =
        {
            films =
                [
                    Repeated
                        {
                            cell =
                                [
                                    { materialId = MaterialIds.glass152; thickness = qwGlassThickness; orientation = PrimaryAxes }
                                    { materialId = MaterialIds.vacuum; thickness = qwVacuumThickness; orientation = PrimaryAxes }
                                ]
                            count = 1
                        }
                ]
            substrate = None
            lower = None
        }

    /// The seeded samples (spec §2a), let-bound so `seedEntries`, the grouping tree, and the tests
    /// all reference the SAME values programmatically — the id literals are never repeated (spec 0033
    /// step 002). Each id is a FIXED literal Guid parsed at seed construction, deterministic across
    /// runs; a new seeded sample adds a new literal here — seed construction never calls
    /// `SampleId.create`.
    module SeedSamples =

        let glassPlate1mm : Sample =
            {
                id = Guid.Parse "a8ceb21b-0719-4553-8f07-e782cb206800" |> SampleId
                name = "Glass plate (n=1.52, 1 mm)"
                structure = plateStructure MaterialIds.glass152 (Thickness.mm 1.0<mm>)
                substrate = Plate
                description = "Single transparent-glass plate, n = 1.52, thickness 1 mm, in vacuum."
            }

        let glassPlate2mm : Sample =
            {
                id = Guid.Parse "962a3eff-4c25-467e-9739-a5ff8922cac3" |> SampleId
                name = "Glass plate (n=1.52, 2 mm)"
                structure = plateStructure MaterialIds.glass152 (Thickness.mm 2.0<mm>)
                substrate = Plate
                description = "Single transparent-glass plate, n = 1.52, thickness 2 mm, in vacuum."
            }

        let glassFilm600 : Sample =
            {
                id = Guid.Parse "66cc0291-7b61-42c1-933e-5c39f2c41917" |> SampleId
                name = "Glass thin film (n=1.75, 600 nm)"
                structure = filmStructure MaterialIds.glass175 (Thickness.nm 600.0<nm>)
                substrate = ThinFilm
                description = "Single transparent-glass thin film, n = 1.75, thickness 600 nm, between vacuum."
            }

        let glassVacuum : Sample =
            {
                id = Guid.Parse "b880a749-812d-415c-b1fb-6041360e85ee" |> SampleId
                name = "Glass / vacuum interface (n=1.50)"
                structure = plateStructure MaterialIds.glass150 (Thickness.mm 1.0<mm>)
                substrate = Plate
                description = "Semi-infinite transparent-glass / vacuum interface, n = 1.50 — the Fresnel / total-reflection demo."
            }

        let glassFilm200 : Sample =
            {
                id = Guid.Parse "9f49dcfa-dede-4858-8757-216443deeba3" |> SampleId
                name = "Glass thin film (n=1.52, 200 nm)"
                structure = filmStructure MaterialIds.glass152 (Thickness.nm 200.0<nm>)
                substrate = ThinFilm
                description = "Single transparent-glass thin film, n = 1.52, 200 nm, between vacuum."
            }

        let multilayerQw : Sample =
            {
                id = Guid.Parse "80e5b7b0-8f10-42f6-9d87-bae692454fd5" |> SampleId
                name = "Quarter-wave glass/vacuum multilayer (41 layers)"
                structure =
                    {
                        films =
                            [
                                Repeated
                                    {
                                        cell =
                                            [
                                                { materialId = MaterialIds.glass152; thickness = qwGlassThickness; orientation = PrimaryAxes }
                                                { materialId = MaterialIds.vacuum; thickness = qwVacuumThickness; orientation = PrimaryAxes }
                                            ]
                                        count = 20
                                    }
                                SingleLayer { materialId = MaterialIds.glass152; thickness = qwGlassThickness; orientation = PrimaryAxes }
                            ]
                        substrate = None
                        lower = None
                    }
                substrate = ThinFilm
                description = "41-layer quarter-wave stack: alternating glass (n=1.52) and vacuum λ/4 films for 600 nm, 21 glass + 20 vacuum layers."
            }

        let euvMoSi : Sample =
            {
                id = Guid.Parse "ea947362-10cf-46e8-ad03-c267fba9de50" |> SampleId
                name = "EUV Mo/Si multilayer (100 pairs)"
                structure =
                    {
                        films =
                            [
                                Repeated
                                    {
                                        cell =
                                            [
                                                { materialId = MaterialIds.euvMolybdenum; thickness = euvLayerThickness; orientation = PrimaryAxes }
                                                { materialId = MaterialIds.euvSilicon; thickness = euvLayerThickness; orientation = PrimaryAxes }
                                            ]
                                        count = 100
                                    }
                            ]
                        substrate = None
                        lower = None
                    }
                substrate = ThinFilm
                description = "EUV reflective multilayer: 100 Mo/Si bilayers, each layer 2.65 nm (λ/4 at 10.6 nm), on vacuum."
            }

        let uniaxial : Sample =
            {
                id = Guid.Parse "2669348a-029b-48db-b39e-02f78e5aa1ab" |> SampleId
                name = "Uniaxial crystal film (1 µm)"
                structure = filmStructure MaterialIds.uniaxialCrystal (Thickness.nm 1000.0<nm>)
                substrate = ThinFilm
                description = "Uniaxial crystal thin film, nₒ = 1.5, nₑ = 1.65, thickness 1 µm, between vacuum."
            }

        let biaxial : Sample =
            {
                id = Guid.Parse "53b01f16-faf9-4413-a419-14592670a03c" |> SampleId
                name = "Biaxial crystal film (1 µm)"
                structure = filmStructure MaterialIds.biaxialCrystal (Thickness.nm 1000.0<nm>)
                substrate = ThinFilm
                description = "Biaxial crystal thin film, n = (1.5, 1.65, 1.75), thickness 1 µm, between vacuum."
            }

        let activeCrystal : Sample =
            {
                id = Guid.Parse "1c170dcc-0528-466e-98a7-cabfdaf9007a" |> SampleId
                name = "Active gyrotropic crystal plate (1 cm)"
                structure = plateStructure MaterialIds.activeCrystal Thickness.oneCentiMeter
                substrate = Plate
                description = "Planar active (gyrotropic) crystal plate, n₁₁ = 2.315, n₃₃ = 2.226, optical-activity ρ₁₂ = 1.5e-6, thickness 1 cm."
            }

        let langasiteSilicon : Sample =
            {
                id = Guid.Parse "8fa9867a-b1dd-4571-a83c-46a0acf16b4f" |> SampleId
                name = "Langasite film on silicon (10 µm, dispersive)"
                structure =
                    {
                        films = [ SingleLayer { materialId = MaterialIds.langasite; thickness = Thickness.mm 0.01<mm>; orientation = PrimaryAxes } ]
                        substrate = None
                        lower = Some MaterialIds.silicon
                    }
                substrate = ThinFilm
                description = "Dispersive langasite thin film (10 µm) on a silicon substrate — wavelength-dependent n, k."
            }

        /// All seeded samples in Library display order (the order `seedEntries` lists them).
        let all : Sample list =
            [
                glassPlate1mm; glassPlate2mm; glassFilm600; glassVacuum; glassFilm200
                multilayerQw; euvMoSi; uniaxial; biaxial; activeCrystal; langasiteSilicon
            ]

    /// The seeded Library entries (spec §2a "Seeded entries"): samples (glass plate → second
    /// thickness → thin film → a quarter-wave multilayer placeholder), the two detectors, one ideal
    /// LP + two ideal CP, and one monochromatic source. Every sample's stack is DATA (spec 0033
    /// step 001) — the multilayers are `Repeated` period groups, never a per-id special case — and
    /// the samples are the named `SeedSamples` values (spec 0033 step 002).
    let seedEntries : LibraryEntry list =
        (SeedSamples.all |> List.map SampleItem)
        @ [
            DetectorItem { id = "det-intensity"; name = "Intensity detector"; kind = Intensity }
            DetectorItem { id = "det-ellipsometer"; name = "Ellipsometer"; kind = Ellipsometer }
            PolarizerItem { id = "pol-lp"; name = "Ideal linear polarizer"; kind = IdealLinear }
            PolarizerItem { id = "pol-cp-left"; name = "Ideal circular polarizer (left)"; kind = IdealCircularLeft }
            PolarizerItem { id = "pol-cp-right"; name = "Ideal circular polarizer (right)"; kind = IdealCircularRight }
            SourceItem { id = "src-600"; name = "Monochromatic 600 nm"; waveLength = WaveLength.nm 600.0<nm> }
        ]

    /// A grouping-tree leaf for a seeded sample: references the sample VALUE programmatically
    /// (spec 0033 step 002 — the id literal lives only in `SeedSamples`), carrying its Guid-string
    /// entry id exactly as `tryGetEntry` resolves it.
    let private sampleLeaf (label : string) (s : Sample) : LibraryTreeNode =
        Leaf (TreeLabel label, (SampleItem s).entryId)

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
                                               sampleLeaf "1 mm plate" SeedSamples.glassPlate1mm
                                               sampleLeaf "2 mm plate" SeedSamples.glassPlate2mm
                                               sampleLeaf "200 nm film" SeedSamples.glassFilm200
                                           ])
                                      sampleLeaf "Glass film (n=1.75)" SeedSamples.glassFilm600
                                      sampleLeaf "Glass / vacuum interface (n=1.50)" SeedSamples.glassVacuum
                                      Group
                                          (TreeLabel "Multilayers",
                                           [
                                               sampleLeaf "Quarter-wave glass/vacuum (41)" SeedSamples.multilayerQw
                                               sampleLeaf "EUV Mo/Si (100 pairs)" SeedSamples.euvMoSi
                                           ])
                                      Group
                                          (TreeLabel "Crystals",
                                           [
                                               sampleLeaf "Uniaxial film" SeedSamples.uniaxial
                                               sampleLeaf "Biaxial film" SeedSamples.biaxial
                                               sampleLeaf "Active gyrotropic plate" SeedSamples.activeCrystal
                                           ])
                                      Group
                                          (TreeLabel "Dispersive",
                                           [
                                               sampleLeaf "Langasite on silicon" SeedSamples.langasiteSilicon
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

    /// The write-seam validation the samples store's write functions share. Two rules, in order:
    /// (spec 0033 steps 004/005) a `Sample` whose display name is empty/whitespace is
    /// `InvalidSample`; and (spec 0035 step 012) a `Sample` whose structure carries NO films AND
    /// NO substrate (`films = []`, `substrate = None` on `SampleStructure`) is structurally empty —
    /// there is nothing for the engine mapping to expand — and is likewise `InvalidSample`, even
    /// when its name is non-blank. Both `addSample` and `updateSample` run this, so the guard holds
    /// on every save.
    let private validateSample (s : Sample) : Result<unit, SampleError> =
        let structurallyEmpty =
            match s.structure.films, s.structure.substrate with
            | [], None -> true
            | _ -> false
        if String.IsNullOrWhiteSpace s.name
        then Error (InvalidSample $"sample '%s{string s.id.value}' has a blank name")
        elif structurallyEmpty
        then Error (InvalidSample $"sample '%s{string s.id.value}' has no films and no substrate")
        else Ok ()

    /// The real, stateful in-memory samples store behind the write-seam (spec 0033 step 005 —
    /// IMPLEMENT_CONTRACT STORE_XDUO_0002; replaces the step-004 validate-only mock).
    /// `createInMemory` closes over a `ref` `Map<SampleId, Sample>` seeded from the samples in
    /// `seedEntries` — the elevated `SampleId` is the Map key directly. Mutation stays INSIDE
    /// the closure (the IO boundary), so the logic holding the proxy stays pure: reads answer
    /// from the current map; `searchSamples` matches the name fragment case-insensitively, then
    /// the `SubstrateKind` facet; `addSample` persists a fresh sample and rejects an id the
    /// store already holds (`DuplicateSampleId`); `updateSample` replaces a known sample and
    /// rejects an unknown id (`UnknownSampleId`); `removeSample` deletes a known id and rejects
    /// an unknown one; both writes keep the step-004 blank-name validation (`InvalidSample`).
    /// Deterministic under test — every entry carries its own id, no clock, no IO. (A static
    /// member, not a module `let`: `createInMemory` at module level already builds the
    /// `LibraryProxy`; the augmentation sits here because it needs `seedEntries` above.)
    type SampleProxy with

        static member createInMemory () : SampleProxy =
            let seeded =
                seedEntries
                |> List.choose (fun e ->
                    match e with
                    | SampleItem s -> Some (s.id, s)
                    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> None)
            let store = ref (Map.ofList seeded)
            let currentSamples () : Sample list =
                store.Value |> Map.toList |> List.map snd
            let unknown (id : SampleId) : SampleError =
                UnknownSampleId $"unknown sample id '%s{string id.value}'"
            {
                listSamples = fun () -> Ok (currentSamples ())
                searchSamples =
                    fun q ->
                        let byText =
                            currentSamples ()
                            |> List.filter (fun s -> s.name.IndexOf(q.text, StringComparison.OrdinalIgnoreCase) >= 0)
                        match q.substrate with
                        | Some kind -> Ok (byText |> List.filter (fun s -> s.substrate = kind))
                        | None -> Ok byText
                tryGetSample = fun id -> Ok (store.Value |> Map.tryFind id)
                addSample =
                    fun sample ->
                        validateSample sample
                        |> Result.bind (fun () ->
                            match store.Value |> Map.tryFind sample.id with
                            | Some existing ->
                                Error (DuplicateSampleId $"sample id '%s{string sample.id.value}' already names '%s{existing.name}'")
                            | None ->
                                store.Value <- store.Value |> Map.add sample.id sample
                                Ok ())
                updateSample =
                    fun sample ->
                        validateSample sample
                        |> Result.bind (fun () ->
                            match store.Value |> Map.tryFind sample.id with
                            | Some _ ->
                                store.Value <- store.Value |> Map.add sample.id sample
                                Ok ()
                            | None -> Error (unknown sample.id))
                removeSample =
                    fun id ->
                        match store.Value |> Map.tryFind id with
                        | Some _ ->
                            store.Value <- store.Value |> Map.remove id
                            Ok ()
                        | None -> Error (unknown id)
            }

    /// The real, stateful in-memory materials store behind the write-seam (spec 0033 step 006
    /// — IMPLEMENT_CONTRACT STORE_XDUO_0001; replaces the step-003 validate-only mock).
    /// `createInMemory` closes over a `ref` `Map<MaterialId, MaterialEntry>` seeded from
    /// `MaterialLibrary.builtInEntries` — the elevated `MaterialId` is the Map key directly.
    /// Mutation stays INSIDE the closure (the IO boundary), so logic holding the proxy stays
    /// pure: reads answer from the current map; `searchMaterials` answers through the pure
    /// query seam (`byQuery` — the §D.8 `byNameContains`/`byCategory` filters plus the
    /// `DispersionFilter` facet); `addMaterial` persists a fresh entry and hard-blocks an id
    /// the store already holds (`DuplicateMaterialId`); `updateMaterial` replaces a known
    /// entry and rejects an unknown one; `removeMaterial` consults `samplesReferencing` and
    /// returns `MaterialStillReferenced` NAMING the referencing samples whenever any remain —
    /// it never cascades and never silently deletes; both writes keep the step-003 blank-name
    /// validation (`InvalidMaterial`). Deterministic under test — every entry carries its own
    /// id, no clock, no IO. (A type augmentation HERE, not beside the type in
    /// `MaterialLibrary.fs`: the referencing lookup is `Sample`-typed, and `Sample` compiles
    /// after that file. At composition the lookup is `samplesReferencing` below, backed by
    /// the step-005 `SampleProxy` store.)
    type MaterialProxy with

        static member createInMemory (samplesReferencing : MaterialId -> Sample list) : MaterialProxy =
            let store = ref (builtInEntries |> List.map (fun e -> e.id, e) |> Map.ofList)
            let currentEntries () : MaterialEntry list =
                store.Value |> Map.toList |> List.map snd
            let unknown (id : MaterialId) : MaterialError =
                UnknownMaterialId $"unknown material id '%s{string id.value}'"
            {
                listMaterials = fun () -> Ok (currentEntries ())
                searchMaterials = fun q -> Ok (byQuery q { entries = currentEntries () })
                tryGetMaterial = fun id -> Ok (store.Value |> Map.tryFind id)
                addMaterial =
                    fun entry ->
                        validateEntry entry
                        |> Result.bind (fun () ->
                            match store.Value |> Map.tryFind entry.id with
                            | Some existing ->
                                Error (DuplicateMaterialId $"material id '%s{string entry.id.value}' already names '%s{existing.name}'")
                            | None ->
                                store.Value <- store.Value |> Map.add entry.id entry
                                Ok ())
                updateMaterial =
                    fun entry ->
                        validateEntry entry
                        |> Result.bind (fun () ->
                            match store.Value |> Map.tryFind entry.id with
                            | Some _ ->
                                store.Value <- store.Value |> Map.add entry.id entry
                                Ok ()
                            | None -> Error (unknown entry.id))
                removeMaterial =
                    fun id ->
                        match store.Value |> Map.tryFind id with
                        | Some entry ->
                            match samplesReferencing id with
                            | [] ->
                                store.Value <- store.Value |> Map.remove id
                                Ok ()
                            | referencing ->
                                let names =
                                    referencing
                                    |> List.map (fun s -> $"'%s{s.name}'")
                                    |> List.sort
                                    |> String.concat ", "
                                Error (MaterialStillReferenced $"material '%s{entry.name}' ('%s{string id.value}') is still referenced by %d{List.length referencing} sample(s): %s{names}")
                        | None -> Error (unknown id)
            }

    /// The composition-root referencing lookup for `MaterialProxy.createInMemory` (spec 0033
    /// step 006): every sample the samples store currently holds whose structure references
    /// the material. Backed by the LIVE step-005 `SampleProxy` store — once the referencing
    /// samples are removed the material becomes removable; there is no snapshot to refresh.
    /// The in-memory `listSamples` is total (always `Ok`); the signature carries no error
    /// channel, so a future store whose listing can fail must supply its own conservative
    /// lookup instead of this one.
    let samplesReferencing (samples : SampleProxy) (id : MaterialId) : Sample list =
        match samples.listSamples () with
        | Ok all -> all |> List.filter (fun s -> s.structure.referencedMaterials |> Set.contains id)
        | Error _ -> []

/// Spec 0027 (028) — the Experiments domain, redesigned around a multi-step, EDITABLE experiment built
/// from the live setup:
///   1. the SETUP is the scene itself (the elements, plus their bound materials / geometry);
///   2. the user picks WHICH element to vary — and the element's CATALOGUE KIND determines what may be
///      varied. This is DATA (`variablesFor`), attached to the element through its kind, NOT hard-coded at
///      the bay: a light source varies only its wavelength, a polarizer only its R1, a sample its R1 and R2
///      (for now), and other kinds nothing yet;
///   3. the user picks what to CAPTURE (transmitted / reflected / both — replacing the old hard-coded
///      T-only for samples and R for mirrors), sets the numeric range, and CONFIRMS, which ADDS the
///      experiment to a collection that persists. Experiments can be EDITED (re-confirm updates in place —
///      never duplicates an already-added experiment) and REMOVED.
/// Every primitive is elevated (the variable, the measurement, the id and the range are DUs / records, not
/// bare strings / floats), and an experiment references its element by the serializable `ElementId` so it
/// survives save-load. The mock `ExperimentProxy` (the functional-proxy seam) lists seed template
/// experiments; a real disk-backed `create` would later live in `OpticalConstructor.Storage`.
module Experiments =

    open Placement
    open Library

    /// A quantity an experiment can vary on an element (spec 028). An elevated DU — not an enum / string.
    type VariableParameter =
        | VaryWaveLength
        | VaryR1
        | VaryR2

        /// A short, stable code (the automation ids and the Controls-layer mirror map back through this).
        member this.code : string =
            match this with
            | VaryWaveLength -> "wavelength"
            | VaryR1 -> "r1"
            | VaryR2 -> "r2"

        /// A human-readable name for the varied quantity.
        member this.label : string =
            match this with
            | VaryWaveLength -> "Wavelength"
            | VaryR1 -> "Rotation R1"
            | VaryR2 -> "Incidence R2"

        /// The display unit of the varied quantity.
        member this.unitLabel : string =
            match this with
            | VaryWaveLength -> "nm"
            | VaryR1 | VaryR2 -> "°"

    /// What each catalogue kind permits varying (spec 028) — declared HERE as DATA (attached to the element
    /// through its kind) so the Experiments bay READS the allowed set rather than hard-coding it. A light
    /// source varies only its wavelength; a linear / circular polarizer only its R1; a sample its R1 and R2
    /// (for now); a lens / mirror / detector expose nothing to vary yet (future kinds add cases here).
    let variablesFor (kind : CatalogueKind) : VariableParameter list =
        match kind with
        | LightSource -> [ VaryWaveLength ]
        | LinearPolarizer
        | CircularPolarizer -> [ VaryR1 ]
        | Sample -> [ VaryR1; VaryR2 ]
        | Lens
        | FlatMirror
        | CurvedMirror
        | Detector -> []

    /// What an experiment CAPTURES at the detector (spec 028): the transmitted branch, the reflected
    /// branch, or both. A DU — not a bool / enum — that REPLACES the previously hard-coded "always T"
    /// (samples) and "mirror ⇒ R"; it defaults from the varied element's `Emission` via `ofEmission`.
    type MeasurementMode =
        | CaptureTransmitted
        | CaptureReflected
        | CaptureBoth

        member this.code : string =
            match this with
            | CaptureTransmitted -> "t"
            | CaptureReflected -> "r"
            | CaptureBoth -> "both"

        member this.label : string =
            match this with
            | CaptureTransmitted -> "Transmitted (T)"
            | CaptureReflected -> "Reflected (R)"
            | CaptureBoth -> "Both (T + R)"

        member this.capturesTransmitted : bool =
            match this with
            | CaptureTransmitted | CaptureBoth -> true
            | CaptureReflected -> false

        member this.capturesReflected : bool =
            match this with
            | CaptureReflected | CaptureBoth -> true
            | CaptureTransmitted -> false

        /// The natural default measurement for an element, from its emission metadata (spec 028: a mirror
        /// emits only its reflected branch, everything else both — so a varied mirror defaults to R, a
        /// varied sample to both; the user can still switch it).
        static member ofEmission (e : Emission) : MeasurementMode =
            match e.emitsReflected, e.emitsTransmitted with
            | true, true -> CaptureBoth
            | true, false -> CaptureReflected
            | false, _ -> CaptureTransmitted

    /// A numeric variation range: [min, max] over `points` samples, in the varied quantity's display unit.
    type VariableRange =
        {
            min : float
            max : float
            points : int
        }

        /// The default range for a variable (spec 028): R1 0…360° (73 pts); R2 0…89° incidence (91 pts —
        /// 90° itself is not computable, so the top is 89, drawn to 90 by the chart); wavelength 200…800 nm
        /// (91 pts).
        static member forVariable (v : VariableParameter) : VariableRange =
            match v with
            | VaryR1 -> { min = 0.0; max = 360.0; points = 73 }
            | VaryR2 -> { min = 0.0; max = 89.0; points = 91 }
            | VaryWaveLength -> { min = 200.0; max = 800.0; points = 91 }

    /// A stable identity for an experiment in the collection (a monotonic int — deterministic, so add /
    /// edit / remove are unit-testable without a Guid clock).
    type ExperimentId =
        | ExperimentId of int

        member this.value = let (ExperimentId i) = this in i
        static member create (i : int) : ExperimentId = ExperimentId i

    /// A fully-specified, editable experiment (spec 028): the varied element (by serializable id, plus a
    /// human label captured at add-time so the row survives the element's removal), the varied quantity,
    /// the capture mode, and the numeric range.
    type Experiment =
        {
            id : ExperimentId
            elementId : ElementId
            elementLabel : string
            variable : VariableParameter
            measurement : MeasurementMode
            range : VariableRange
        }

        /// A short, human-readable description (the collection row + readout use this).
        member this.description : string =
            $"%s{this.elementLabel}: vary %s{this.variable.label} over %g{this.range.min}…%g{this.range.max} %s{this.variable.unitLabel} (%d{this.range.points} pts), capture %s{this.measurement.label}"

    /// The in-progress experiment being built or edited (spec 028, the multi-step editor). `elementId` /
    /// `variable` are `None` until chosen; `commit` needs both. When `editingId` is `Some` the next
    /// `commit` UPDATES that stored experiment (never duplicates); otherwise it APPENDS a new one.
    type ExperimentDraft =
        {
            elementId : ElementId option
            elementLabel : string
            variable : VariableParameter option
            measurement : MeasurementMode
            range : VariableRange
            editingId : ExperimentId option
        }

        static member empty : ExperimentDraft =
            {
                elementId = None
                elementLabel = ""
                variable = None
                measurement = CaptureTransmitted
                range = VariableRange.forVariable VaryR1
                editingId = None
            }

    /// The editable collection of experiments plus the id counter and the live draft (spec 028). Pure —
    /// choose / commit / edit / remove are unit-tested without any UI.
    type ExperimentCollection =
        {
            experiments : Experiment list
            nextId : int
            draft : ExperimentDraft
        }

        static member empty : ExperimentCollection =
            { experiments = []; nextId = 1; draft = ExperimentDraft.empty }

    /// Whether the draft is complete enough to commit (an element AND a variable are chosen).
    let canCommit (c : ExperimentCollection) : bool =
        match c.draft.elementId, c.draft.variable with
        | Some _, Some _ -> true
        | _ -> false

    /// Start a brand-new draft (clears the editor and the editing cursor).
    let newDraft (c : ExperimentCollection) : ExperimentCollection =
        { c with draft = ExperimentDraft.empty }

    /// Choose the element to vary (spec 028 step 2). The allowed variables (`variablesFor kind`) and the
    /// default measurement (from the element's emission) are supplied by the host; the draft's variable
    /// defaults to the first allowed one (`None` when the element exposes nothing to vary), and its range
    /// to that variable's default.
    let chooseElement
        (id : ElementId)
        (label : string)
        (allowed : VariableParameter list)
        (defaultMeasurement : MeasurementMode)
        (c : ExperimentCollection) : ExperimentCollection =
        let variable = List.tryHead allowed
        let range = variable |> Option.map VariableRange.forVariable |> Option.defaultValue c.draft.range
        { c with
            draft =
                { c.draft with
                    elementId = Some id
                    elementLabel = label
                    variable = variable
                    measurement = defaultMeasurement
                    range = range } }

    /// Choose the varied quantity (spec 028). Resets the range to that variable's default.
    let chooseVariable (v : VariableParameter) (c : ExperimentCollection) : ExperimentCollection =
        { c with draft = { c.draft with variable = Some v; range = VariableRange.forVariable v } }

    /// Choose what the experiment captures (T / R / both, spec 028).
    let chooseMeasurement (m : MeasurementMode) (c : ExperimentCollection) : ExperimentCollection =
        { c with draft = { c.draft with measurement = m } }

    let setRangeMin (v : float) (c : ExperimentCollection) : ExperimentCollection =
        { c with draft = { c.draft with range = { c.draft.range with min = v } } }

    let setRangeMax (v : float) (c : ExperimentCollection) : ExperimentCollection =
        { c with draft = { c.draft with range = { c.draft.range with max = v } } }

    let setRangePoints (n : int) (c : ExperimentCollection) : ExperimentCollection =
        { c with draft = { c.draft with range = { c.draft.range with points = max 2 n } } }

    /// Confirm the draft (spec 028 step 3). From a fresh draft this APPENDS a new experiment (minting the
    /// next id) and leaves the draft EDITING it, so a follow-up confirm UPDATES rather than duplicating an
    /// already-added experiment; while editing an existing one it updates that experiment in place. Inert
    /// when the draft is incomplete.
    let commit (c : ExperimentCollection) : ExperimentCollection =
        match c.draft.elementId, c.draft.variable with
        | Some elementId, Some variable ->
            match c.draft.editingId with
            | Some existing ->
                let experiments =
                    c.experiments
                    |> List.map (fun e ->
                        if e.id = existing then
                            { e with
                                elementId = elementId
                                elementLabel = c.draft.elementLabel
                                variable = variable
                                measurement = c.draft.measurement
                                range = c.draft.range }
                        else e)
                { c with experiments = experiments }
            | None ->
                let exp =
                    {
                        id = ExperimentId c.nextId
                        elementId = elementId
                        elementLabel = c.draft.elementLabel
                        variable = variable
                        measurement = c.draft.measurement
                        range = c.draft.range
                    }
                { c with
                    experiments = c.experiments @ [ exp ]
                    nextId = c.nextId + 1
                    draft = { c.draft with editingId = Some exp.id } }
        | _ -> c

    /// Load an existing experiment into the draft for editing (spec 028: "if the user chooses an experiment
    /// then the user can change anything in the experiment"). A later `commit` updates it in place.
    let edit (id : ExperimentId) (c : ExperimentCollection) : ExperimentCollection =
        match c.experiments |> List.tryFind (fun e -> e.id = id) with
        | Some e ->
            { c with
                draft =
                    {
                        elementId = Some e.elementId
                        elementLabel = e.elementLabel
                        variable = Some e.variable
                        measurement = e.measurement
                        range = e.range
                        editingId = Some e.id
                    } }
        | None -> c

    /// Remove an experiment from the collection (spec 028). If it was the one being edited the draft resets.
    let remove (id : ExperimentId) (c : ExperimentCollection) : ExperimentCollection =
        let experiments = c.experiments |> List.filter (fun e -> e.id <> id)
        let draft = if c.draft.editingId = Some id then ExperimentDraft.empty else c.draft
        { c with experiments = experiments; draft = draft }

    /// The Experiments error channel (errors as values; each case carries a `reason`).
    type ExperimentError =
        | UnknownExperiment of reason : string
        | ExperimentUnavailable of reason : string

    /// The mock Experiments IO seam (the functional-proxy convention): a record of camelCase
    /// `Result`-returning functions. A test substitutes a stub of the SAME shape. Function-valued
    /// fields have no structural equality, so the proxy compares by reference — letting a host model
    /// that holds the proxy keep its (Elmish-required) equality.
    [<ReferenceEquality>]
    type ExperimentProxy =
        {
            listExperiments : unit -> Result<Experiment list, ExperimentError>
            tryGetExperiment : int -> Result<Experiment option, ExperimentError>
        }

    /// Seed template experiments (listable; the live bay builds experiments against the present scene). The
    /// referenced element id ("analyzer") is the template's placeholder.
    let seedExperiments : Experiment list =
        [
            {
                id = ExperimentId 1
                elementId = ElementId.create "analyzer"
                elementLabel = "Analyzer"
                variable = VaryR1
                measurement = CaptureTransmitted
                range = VariableRange.forVariable VaryR1
            }
        ]

    /// The in-memory mock proxy (spec §3 / Q7): closes over the seed templates, no IO, deterministic for
    /// tests. A real disk-backed `create` would live in `OpticalConstructor.Storage`, leaving the
    /// bay / logic unchanged.
    let createInMemory () : ExperimentProxy =
        let seeds = seedExperiments
        {
            listExperiments = fun () -> Ok seeds
            tryGetExperiment = fun i -> Ok (seeds |> List.tryFind (fun e -> e.id.value = i))
        }
