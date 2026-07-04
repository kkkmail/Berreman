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
            sprintf "%s: vary %s over %g…%g %s (%d pts), capture %s"
                this.elementLabel this.variable.label this.range.min this.range.max
                this.variable.unitLabel this.range.points this.measurement.label

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
