namespace OpticalConstructor.Domain

open System
open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary

/// Table-element and library-entry identities, sample structures, presets, grouping trees and store seams
/// (spec 0027; spec 0050 §3.3). Built-in entries come from Catalogue/Builtin through CatalogueImportProxy.
module Library =

    /// Elevated, serializable table-element identity (spec §1 / §5). Distinct from `valueId` (which
    /// binds an element to a Library entry); the `id` is the element's own stable identity that
    /// experiments and setup steps reference (never an in-memory element reference, which cannot be
    /// serialized).
    type ElementId =
        | ElementId of string

        member this.value = let (ElementId s) = this in s
        static member create (s : string) : ElementId = ElementId s

    /// Stable sample identity; create mints a new id and toElementValueId supplies its library binding.
    /// SampleIds names the Built-in catalogue's identities (spec 0033 step 002; spec 0050 §3.3).
    type SampleId =
        | SampleId of Guid

        member this.value = let (SampleId g) = this in g
        static member create () : SampleId = Guid.NewGuid() |> SampleId

        /// The sample as a Library entry id: its Guid string form (spec 0033 step 002).
        member this.toElementValueId : ElementValueId = ElementValueId (string this.value)

    /// Module-level mint helper for `SampleId` (the case name and type name collide — so a qualified
    /// `Library.SampleId.create` resolves to the case rather than the static member).
    let newSampleId () : SampleId = SampleId.create ()

    /// Well-known identities of the Built-in catalogue's samples (spec 0050 §3.3).
    module SampleIds =
        let glassPlate1mmId : SampleId = Guid.Parse "a8ceb21b-0719-4553-8f07-e782cb206800" |> SampleId
        let glassPlate2mmId : SampleId = Guid.Parse "962a3eff-4c25-467e-9739-a5ff8922cac3" |> SampleId
        let glassFilm600Id : SampleId = Guid.Parse "66cc0291-7b61-42c1-933e-5c39f2c41917" |> SampleId
        let glassVacuumId : SampleId = Guid.Parse "b880a749-812d-415c-b1fb-6041360e85ee" |> SampleId
        let glassFilm200Id : SampleId = Guid.Parse "9f49dcfa-dede-4858-8757-216443deeba3" |> SampleId
        let multilayerQwId : SampleId = Guid.Parse "80e5b7b0-8f10-42f6-9d87-bae692454fd5" |> SampleId
        let euvMoSiId : SampleId = Guid.Parse "ea947362-10cf-46e8-ad03-c267fba9de50" |> SampleId
        let uniaxialId : SampleId = Guid.Parse "2669348a-029b-48db-b39e-02f78e5aa1ab" |> SampleId
        let biaxialId : SampleId = Guid.Parse "53b01f16-faf9-4413-a419-14592670a03c" |> SampleId
        let activeCrystalPlateId : SampleId = Guid.Parse "1c170dcc-0528-466e-98a7-cabfdaf9007a" |> SampleId
        let langasiteSiliconId : SampleId = Guid.Parse "8fa9867a-b1dd-4571-a83c-46a0acf16b4f" |> SampleId

    /// Well-known identities of the Built-in catalogue's source, detector and polarizer presets (spec 0050 §3.3).
    module PresetIds =
        let source600 : ElementValueId = ElementValueId "0c4d0c67-137f-4c9f-8f31-d8837b725dc5"

        let heNe633 : ElementValueId = ElementValueId "a77786fe-f38e-4af3-8cfc-bd94be27567e"
        let heNe543 : ElementValueId = ElementValueId "ba473d8c-3eff-449c-84df-2e2e759c9306"
        let argon488 : ElementValueId = ElementValueId "66b97a5c-5564-440c-9076-7c92c0148529"
        let argon515 : ElementValueId = ElementValueId "5820d1d3-f4d0-4c99-ba4e-2642734e82a2"
        let heCd442 : ElementValueId = ElementValueId "035b5b32-d944-4150-8fa0-5d0f93aaa871"
        let heCd325 : ElementValueId = ElementValueId "f92c2370-a8e6-4cac-ad1f-42e6fa7a0f5e"
        let diode405 : ElementValueId = ElementValueId "d4d22b59-58ee-483b-996c-ae8dc1a76705"
        let ruby694 : ElementValueId = ElementValueId "657aad0a-943e-443e-8d18-868d5655ca75"
        let tiSapphire800 : ElementValueId = ElementValueId "209ac4b5-fb84-4be8-a764-32d320522c42"
        let ndYag1064 : ElementValueId = ElementValueId "6a34b9ee-3f1e-4409-9118-6eb34d5b1a93"
        let ndYag532 : ElementValueId = ElementValueId "9fee5fb8-2789-484f-8cde-569095cb13ef"
        let ndYag355 : ElementValueId = ElementValueId "b1838994-8787-47f4-889a-dca3a4978d3e"
        let ndYag266 : ElementValueId = ElementValueId "3e8195ab-c4a1-45b1-83fb-f7a7956a020e"
        let erbium1550 : ElementValueId = ElementValueId "361bf54d-8a62-4ed2-ad65-2d919db4879e"
        let co2Laser10600 : ElementValueId = ElementValueId "7644ce3a-efa2-4e62-8ff0-50622d962e4d"

        let mercuryG436 : ElementValueId = ElementValueId "f92b95e0-9077-4d0a-b506-7eebd0671f99"
        let mercuryH405 : ElementValueId = ElementValueId "08632463-ddb8-4a2b-a9e2-0b7c0e1d6f51"
        let mercuryI365 : ElementValueId = ElementValueId "5a71721a-c4c4-475f-ace9-18531a0e8c40"
        let krf248 : ElementValueId = ElementValueId "68c23345-c207-4dd1-9c67-bd82d2de4ab8"
        let arf193 : ElementValueId = ElementValueId "a890f53e-4727-47f7-8392-5774c58219f9"
        let f2Excimer157 : ElementValueId = ElementValueId "2f7a45ee-4fa2-4dd3-b0c5-f81e44eaa1c9"
        let euv13p5 : ElementValueId = ElementValueId "eba5ec72-b129-416f-93bd-858b92116fe3"
        let beuv6p7 : ElementValueId = ElementValueId "f7a747e7-a0da-4fae-a28e-0abd36d32b08"

        let xenonMonochromator : ElementValueId = ElementValueId "d09c967d-18f5-437e-b119-64f645e5b431"
        let tiSapphireTunable : ElementValueId = ElementValueId "cb376458-dc3c-41f9-8a5f-54f0f575978b"
        let supercontinuum : ElementValueId = ElementValueId "090b09f6-99f4-403b-b25f-e085383215e1"
        let euvBeamline : ElementValueId = ElementValueId "28ec84c4-fd9b-49e9-878e-0ef731be686f"

        let detectorIntensity : ElementValueId = ElementValueId "ffa07a4d-1fd4-4e71-83e9-d15f5dfa25b6"
        let detectorEllipsometer : ElementValueId = ElementValueId "0340c670-c323-40c7-9b00-5af7a1ec41af"
        let polarizerLp : ElementValueId = ElementValueId "f1d38e6a-16e7-405d-9678-45c990f5a155"
        let polarizerCpLeft : ElementValueId = ElementValueId "7fb025f4-d117-4025-9008-d11f2132c34a"
        let polarizerCpRight : ElementValueId = ElementValueId "83c1e5a0-48da-43df-bcdd-8d0b37295811"

    /// A specific version of a sample entry (spec 0038 Part H): the sample identity plus the version
    /// number. Mirrors `MaterialVersionId` (`MaterialLibrary.fs`) — a versioned binding (an
    /// experiment descriptor, step 25) pins the exact version it was built against, so a later mint
    /// of the sample's `.next` version never silently rewrites an existing experiment's physics.
    /// Moved here from `Lifecycle.fs` at step 022 (which compiles AFTER this file) so the versioned
    /// `SampleProxy` record below can name it while the record stays re-typed IN PLACE — the exact
    /// move step 021 made for `MaterialVersionId` (a version-id type lives with its identity type).
    /// `VersionNumber` is in scope through the `open …MaterialLibrary` above.
    type SampleVersionId =
        {
            sampleId : SampleId
            version : VersionNumber
        }

        /// References version one of a sample, regardless of its latest stored version or pin
        /// (spec 0038 step 025; spec 0050 §3.3).
        static member firstOf (sampleId : SampleId) : SampleVersionId =
            { sampleId = sampleId; version = VersionNumber.first }

    /// Whether a sample's geometry is a thin film (0+ layers on a semi-infinite substrate), a thick plate
    /// (0+ layers on a specified substrate plate), or a MIRROR (a reflective stack — spec 0046: the ONLY
    /// geometry observed in reflection alone; a thin film and a plate both reflect AND transmit). A DU,
    /// not a bool/enum, so the sample editor can add geometries case-by-case
    /// (compiler-guided). The former `Wedge` case was removed (spec 0038 comment 007): a thin film
    /// already covers the semi-infinite case it duplicated. (This is the display/search geometry facet,
    /// distinct from the engine `Berreman.Media.Substrate` whose `Wedge` case is unaffected.)
    type SubstrateKind =
        | ThinFilm
        | ThickPlate
        | Mirror

        /// The geometry-constrained DEFAULT supported emission (spec 0040 Part D.2; spec 0046 — operator:
        /// locked R-only belongs to MIRRORS alone): a `Mirror` is observed in reflection only, so it
        /// supports `EmitReflectedOnly`; a `ThinFilm` (light also enters its semi-infinite substrate) and a
        /// `ThickPlate` both reflect and transmit, so they default to `EmitBoth`.
        member kind.defaultSupportedEmission : Emission =
            match kind with
            | Mirror -> EmitReflectedOnly
            | ThinFilm
            | ThickPlate -> EmitBoth

    /// The Euler angles that orient a layer's crystal tensors, under an explicit engine convention.
    type EulerAngles =
        {
            convention : RotationConvention
            phi : Angle
            theta : Angle
            psi : Angle
        }

    /// How a layer's crystal tensors are oriented relative to the lab frame (spec 0033 step 020):
    /// `PrimaryAxes` keeps the material's own principal axes (the identity — tensors exactly as
    /// stored); `EulerRotation` orients them by Euler angles under an explicit engine
    /// `RotationConvention`. This is DATA on the layer — nothing is stored rotated; the step-1
    /// system builder applies the rotation via `Layer.rotate` when it assembles the engine system.
    type CrystalOrientation =
        | PrimaryAxes
        | EulerRotation of EulerAngles

        /// The engine rotation this orientation denotes: `PrimaryAxes` is the identity;
        /// `EulerRotation` builds `Rotation.create convention phi theta psi`. The named engine
        /// shortcuts (`Rotation.rotatePiX`, `Rotation.rotateHalfPiY`, …) remain available for tests.
        member this.toRotation : Rotation =
            match this with
            | PrimaryAxes -> RealMatrix3x3.identity |> Rotation
            | EulerRotation angles -> Rotation.create angles.convention angles.phi angles.theta angles.psi |> Rotation

    /// One physical layer of a sample's stack (spec 0033 step 001 — the stack is DATA): a material
    /// reference plus a thickness, plus the crystal orientation of its tensors (spec 0033 step 020 —
    /// `PrimaryAxes` unless the sample says otherwise; the rotation is applied at system-build time,
    /// never stored). `materialId` is the elevated `MaterialLibrary.MaterialId`
    /// (spec 0033 step 002) — the same key `resolveMaterialWithDisp` looks up.
    type SampleLayer =
        {
            /// Pins the material version so later material edits preserve this layer's physics
            /// (spec 0038 step 022; spec 0050 §3.3).
            materialId : MaterialVersionId
            thickness : Thickness
            orientation : CrystalOrientation
        }

    /// How many times a period group repeats its unit cell.
    type PeriodCount =
        | PeriodCount of int

        member this.value = let (PeriodCount n) = this in n

    /// A repeated unit cell (period) of layers — the DBR / Bragg / EUV-Mo–Si shape. `cell` is the
    /// ordered unit cell; `count` the number of periods.
    type PeriodGroup =
        {
            cell : SampleLayer list
            count : PeriodCount
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
            /// Pins the lower half-space's material version; None denotes vacuum
            /// (spec 0038 step 022; spec 0050 §3.3).
            lower : MaterialVersionId option
        }

        /// The flattened film layers in order — each `Repeated` expands to `count` copies of its cell
        /// (mirrors `RepeatBuilder.expand`: `List.replicate count cell |> List.concat`). Pure.
        member this.expandedFilms : SampleLayer list =
            this.films
            |> List.collect (fun item ->
                match item with
                | SingleLayer l -> [ l ]
                | Repeated g -> List.replicate g.count.value g.cell |> List.concat)

        /// Every referenced material version, counting each repeated cell once (spec 0050).
        member this.referencedMaterialVersions : Set<MaterialVersionId> =
            let filmMaterialVersions =
                this.films
                |> List.collect (fun item ->
                    match item with
                    | SingleLayer l -> [ l.materialId ]
                    | Repeated g -> g.cell |> List.map (fun l -> l.materialId))
            let substrateMaterialVersions =
                this.substrate |> Option.toList |> List.map (fun l -> l.materialId)
            let lowerMaterialVersions = this.lower |> Option.toList
            filmMaterialVersions @ substrateMaterialVersions @ lowerMaterialVersions |> Set.ofList

        /// Projects referencedMaterialVersions to material identities (spec 0033 step 006; spec 0050).
        member this.referencedMaterials : Set<MaterialId> =
            this.referencedMaterialVersions |> Set.map (fun materialVersion -> materialVersion.materialId)

    /// A cut-out plate (spec §2a): a material structure cut to a plate / thin-film geometry → Layer(s)/an
    /// `OpticalSystem` (the mapping to the engine is Phase 3). The `structure` is the material facet
    /// (what the sample is made of, as data); `substrate` stays the geometry facet.
    type Sample =
        {
            id : SampleId
            name : EntryName
            structure : SampleStructure
            substrate : SubstrateKind
            /// A human-readable description of what the sample IS — materials + thicknesses + stack —
            /// shown in the Details element-view and the Library confirm step. Multilayer samples spell
            /// out the repeating unit and layer count rather than a per-layer list.
            description : EntryDescription
            /// The ray groups this sample can emit, geometry-constrained (spec 0040 Part D.2 / operator
            /// 010/Q3 Part D.2). Reuses the shared `Emission` DU (`Placement.fs`), so the "both groups
            /// off" state stays unrepresentable. The invariant is DATA (spec 0046): a `Mirror` is pinned to
            /// `EmitReflectedOnly` and can never clear its R branch; a `ThinFilm` and a `ThickPlate` both reflect
            /// AND transmit, default to `EmitBoth`, and are constrainable to R-only or T-only through
            /// `Emission`'s smart setters. Defaulted by `SubstrateKind.defaultSupportedEmission` and
            /// mutated by `withSupportedEmission`, both of which route through `constrainEmission`.
            supportedEmission : Emission
        }

    /// Force a candidate supported emission back inside its geometry's constraint (spec 0040 Part D.2,
    /// spec 0046). Under `Mirror` the reflected-only rule is ABSOLUTE: any candidate collapses to
    /// `EmitReflectedOnly`, so a mirror can never clear its R branch (nor gain a transmitted branch).
    /// Under `ThinFilm` and `ThickPlate` the candidate passes through untouched, so the existing
    /// `Emission.withReflected` / `Emission.withTransmitted` setters constrain them to R-only or T-only
    /// freely. This is the single enforcement point every emission change routes through
    /// (`withSupportedEmission`).
    let constrainEmission (kind : SubstrateKind) (candidate : Emission) : Emission =
        match kind with
        | Mirror -> EmitReflectedOnly
        | ThinFilm
        | ThickPlate -> candidate

    /// Set a sample's `supportedEmission`, re-imposing its geometry constraint (spec 0040 Part D.2):
    /// the candidate is routed through `constrainEmission s.substrate`, so a `Mirror` sample stays
    /// `EmitReflectedOnly` no matter what is offered (unclearable) and a `ThinFilm` / `ThickPlate` takes the
    /// candidate as given. The single write path that keeps the field's invariant intact.
    let withSupportedEmission (candidate : Emission) (s : Sample) : Sample =
        { s with supportedEmission = constrainEmission s.substrate candidate }

    /// Protected presets cannot be deleted, inactivated or superseded by the user.
    /// Samples remain user-managed, including the Built-in catalogue's examples (spec 0038 Part F; spec 0050 §3.3).
    type EntryProtection =
        | ProtectedBuiltIn
        | UserManaged

        /// The 1-based database tag (spec 0050).
        member this.value : int =
            match this with
            | ProtectedBuiltIn -> 1
            | UserManaged -> 2

        /// Reads a database tag, rejecting unknown values (spec 0050).
        static member tryOfDb (v : int) : EntryProtection option =
            match v with
            | 1 -> Some ProtectedBuiltIn
            | 2 -> Some UserManaged
            | _ -> None

    /// How a light source's wavelength may be set (spec 0046): a FIXED line, or TUNABLE anywhere between
    /// two limits (a monochromator, a tunable laser, a synchrotron beamline). An experiment may vary only a
    /// tunable source's wavelength, and only within its limits.
    type SourceTuning =
        | FixedLine
        | TunableBetween of Berreman.Dispersion.WaveLengthInterval

    /// A monochromatic source preset — defines λ (spec Q3: source = wavelength). `waveLength` is the line
    /// of a fixed source, or the setting a tunable source runs at unless an experiment says otherwise.
    type SourcePreset =
        {
            id : ElementValueId
            name : EntryName
            waveLength : WaveLength
            tuning : SourceTuning
            protection : EntryProtection
        }

        /// The tunable limits, or `None` for a fixed line (spec 0046).
        member this.tunableLimitsOpt : Berreman.Dispersion.WaveLengthInterval option =
            match this.tuning with
            | FixedLine -> None
            | TunableBetween limits -> Some limits

    /// The detector type fixes the measurement (spec §4): Intensity records S0; Ellipsometer records
    /// Ψ/Δ. A DU, not an enum, so non-ideal/compound detectors are a non-breaking addition later.
    type DetectorKind =
        | Intensity
        | Ellipsometer

    type DetectorPreset =
        {
            id : ElementValueId
            name : EntryName
            kind : DetectorKind
            protection : EntryProtection
        }

    /// The ideal polarizer kinds the pipeline synthesizes analytically (spec Q2). The circular cases
    /// carry handedness (left / right).
    type PolarizerKind =
        | IdealLinear
        | IdealCircularLeft
        | IdealCircularRight

    /// One component of a compound constant-Mueller polarizer (spec 0038 Part F): a 4×4 Mueller
    /// matrix stored at REFERENCE orientation plus the component's fixed angular offset within the
    /// compound. Nothing is stored rotated — the Stokes/Mueller pipeline rotates the component at
    /// run time as R(−θ)·M·R(θ) (`MuellerComponent.offsetMueller`).
    type MuellerComponent =
        {
            matrix : MuellerMatrix
            offset : Angle
        }

    /// A polarizer's physics as DATA (spec 0038 Part F): `ComputedIdeal` synthesizes through the
    /// EXISTING `Propagation.inputStokes` / `analyzerMueller` exactly as today; `ConstantMueller`
    /// is an ORDERED component list (light-traversal order — the first component is the first
    /// surface light hits) whose compound matrix the Stokes/Mueller pipeline evaluates on demand
    /// (`Propagation.compoundMueller`). ConstantMueller entries live ONLY in the Stokes/Mueller
    /// pipeline — they never enter the Berreman stack; their dedicated editor is a non-breaking
    /// future addition this DU enables.
    type PolarizerBehavior =
        | ComputedIdeal of PolarizerKind
        | ConstantMueller of MuellerComponent list

    /// The polarizer classification facet value (spec 0038 Part F — the Library window's polarizer
    /// facet): linear, circular, the two compound orders (named in light-traversal order), or a
    /// custom Mueller stack.
    type PolarizerCategory =
        | LpCategory
        | CpCategory
        | LpCpCategory
        | CpLpCategory
        | CustomMueller

        /// The human-readable facet/branch label (the discrete facet key doubles as the label).
        member this.label : string =
            match this with
            | LpCategory -> "Linear"
            | CpCategory -> "Circular"
            | LpCpCategory -> "Linear + circular"
            | CpLpCategory -> "Circular + linear"
            | CustomMueller -> "Custom Mueller"

    type PolarizerPreset =
        {
            id : ElementValueId
            name : EntryName
            behavior : PolarizerBehavior
            category : PolarizerCategory
            protection : EntryProtection
        }

    /// The choosable, kind-constrained Library things (spec §2a). The entry id IS the `valueId`
    /// written onto the bound table element.
    type LibraryEntry =
        | SampleItem of Sample
        | SourceItem of SourcePreset
        | DetectorItem of DetectorPreset
        | PolarizerItem of PolarizerPreset

        /// The entry id (== the `valueId` written onto the bound table element): always the string of
        /// a GUID — a sample's `SampleId`, a preset's `PresetIds` member, a user source's minted
        /// `newSourceId` (spec 0033 step 002; spec 0050 §3.3).
        member this.entryId : ElementValueId =
            match this with
            | SampleItem s -> s.id.toElementValueId
            | SourceItem s -> s.id
            | DetectorItem d -> d.id
            | PolarizerItem p -> p.id

        /// The entry's name (what the Library window's text filter matches).
        member this.entryName : EntryName =
            match this with
            | SampleItem s -> s.name
            | SourceItem s -> s.name
            | DetectorItem d -> d.name
            | PolarizerItem p -> p.name

        member this.displayName : string = this.entryName.value

        /// Presets carry protection as data; all samples remain editable, including Built-in examples
        /// (spec 0038 Part F; spec 0050 §3.3).
        member this.protection : EntryProtection =
            match this with
            | SampleItem _ -> UserManaged
            | SourceItem s -> s.protection
            | DetectorItem d -> d.protection
            | PolarizerItem p -> p.protection

        /// The FULL, human-readable description of what this entry IS (spec 0027 / 026 — the Library
        /// confirm step and the Details element-view show it before / after binding). A sample carries its
        /// own curated `description` (materials + thicknesses + stack); the other presets get prose built
        /// from their kind / wavelength so a bare "Ideal linear polarizer" is never the whole story.
        member this.fullDescription : string =
            match this with
            | SampleItem s -> s.description.value
            | SourceItem s ->
                let toNm (w : WaveLength) : float = w.value / nmToMeter / oneNanometer
                let wNm = toNm s.waveLength
                match s.tunableLimitsOpt with
                | Some limits ->
                    let lo = toNm limits.lower
                    let hi = toNm limits.upper
                    $"Tunable monochromatic light source, %g{lo}–%g{hi} nm (set to %g{wNm} nm unless an experiment varies or sets it)."
                | None -> $"Monochromatic light source at %g{wNm} nm (a fixed line)."
            | DetectorItem d ->
                match d.kind with
                | Intensity -> "Intensity detector — records the transmitted irradiance S₀."
                | Ellipsometer -> "Ellipsometer — records the ellipsometric angles Ψ and Δ."
            | PolarizerItem p ->
                match p.behavior with
                | ComputedIdeal IdealLinear -> "Ideal linear polarizer — transmits the linear component along its R1 orientation."
                | ComputedIdeal IdealCircularLeft -> "Ideal circular polarizer (left-handed) — transmits left-circular light."
                | ComputedIdeal IdealCircularRight -> "Ideal circular polarizer (right-handed) — transmits right-circular light."
                | ConstantMueller components ->
                    $"%s{p.category.label} polarizer — %d{List.length components} constant Mueller component(s) at fixed offsets, evaluated in the Stokes/Mueller pipeline."

        /// The catalogue kinds this entry is valid for (kind-constrained selection, §2a). A polarizer
        /// entry serves the role its CATEGORY names: linear (the ideal LP), circular (the two CP
        /// presets), or — for the compound/custom categories (spec 0038 Part F) — either role.
        member this.forKinds : CatalogueKind list =
            match this with
            | SampleItem _ -> [ CatalogueKind.Sample ]
            | SourceItem _ -> [ LightSource ]
            | DetectorItem _ -> [ Detector ]
            | PolarizerItem p ->
                match p.category with
                | LpCategory -> [ LinearPolarizer ]
                | CpCategory -> [ CircularPolarizer ]
                | LpCpCategory | CpLpCategory | CustomMueller -> [ LinearPolarizer; CircularPolarizer ]

    /// A bound sample supplies its geometry-constrained emission; other entries keep the placement's fallback
    /// (spec 0040 Part D.3). Experiment measurement mode follows the resulting emission.
    let placementEmissionForEntry (fallback : Emission) (entry : LibraryEntry) : Emission =
        match entry with
        | SampleItem s -> s.supportedEmission
        | SourceItem _ | DetectorItem _ | PolarizerItem _ -> fallback

    /// A node-path label (a tree grouping level: "Samples", "Glass", a glass kind, …). Elevated so a
    /// label is never a bare string in the domain.
    type TreeLabel =
        | TreeLabel of string

        member this.value = let (TreeLabel s) = this in s

    type EntryName with

        /// An entry's name as the label of its tree leaf.
        member this.toTreeLabel : TreeLabel = TreeLabel this.value

    /// One tree representation (R3): a labelled grouping with either child groups or leaf entries.
    /// Leaves carry the entry id (= `valueId`); the host resolves the full entry via `tryGetEntry`.
    type LibraryTreeNode =
        | Group of label : TreeLabel * children : LibraryTreeNode list
        | Leaf of label : TreeLabel * entryId : ElementValueId

    /// A Library representation's name (R3): its grouping, e.g. "By kind".
    type TreeName =
        | TreeName of string

        member this.value = let (TreeName s) = this in s

    /// One named Library representation (R3 — there may be more than one tree, by different
    /// groupings).
    type LibraryTree =
        {
            name : TreeName
            root : LibraryTreeNode
        }

        /// Derives By kind from the supplied entries, ordered by kind then facet (spec 0050 S1.7).
        /// Omits empty groups and preserves input order within each group's leaves.
        static member byKind (entries : LibraryEntry list) : LibraryTree =
            let group (label : TreeLabel) (children : LibraryTreeNode list) : LibraryTreeNode option =
                match children with
                | [] -> None
                | _ -> Some (Group (label, children))

            let leaves =
                entries
                |> List.map (fun entry -> entry, Leaf (entry.entryName.toTreeLabel, entry.entryId))

            let sampleGroups =
                leaves
                |> List.choose (fun (entry, leaf) ->
                    match entry with
                    | SampleItem sample -> Some (sample.substrate, leaf)
                    | _ -> None)
                |> List.groupBy fst
                |> List.sortBy fst
                |> List.choose (fun (kind, samples) ->
                    let label =
                        match kind with
                        | ThinFilm -> TreeLabel "Thin films"
                        | ThickPlate -> TreeLabel "Thick plates"
                        | Mirror -> TreeLabel "Mirrors"
                    group label (samples |> List.map snd))

            let fixedSources =
                leaves
                |> List.choose (fun (entry, leaf) ->
                    match entry with
                    | SourceItem source ->
                        match source.tuning with
                        | FixedLine -> Some leaf
                        | TunableBetween _ -> None
                    | _ -> None)
            let tunableSources =
                leaves
                |> List.choose (fun (entry, leaf) ->
                    match entry with
                    | SourceItem source ->
                        match source.tuning with
                        | FixedLine -> None
                        | TunableBetween _ -> Some leaf
                    | _ -> None)
            let sourceGroups =
                [ group (TreeLabel "Fixed lines") fixedSources; group (TreeLabel "Tunable") tunableSources ]
                |> List.choose id

            let detectorLeaves =
                leaves
                |> List.choose (fun (entry, leaf) ->
                    match entry with
                    | DetectorItem _ -> Some leaf
                    | _ -> None)

            let polarizerGroups =
                leaves
                |> List.choose (fun (entry, leaf) ->
                    match entry with
                    | PolarizerItem polarizer -> Some (polarizer.category, leaf)
                    | _ -> None)
                |> List.groupBy fst
                |> List.sortBy fst
                |> List.choose (fun (category, polarizers) ->
                    let label = TreeLabel category.label
                    group label (polarizers |> List.map snd))

            let groups =
                [
                    group (TreeLabel "Samples") sampleGroups
                    group (TreeLabel "Sources") sourceGroups
                    group (TreeLabel "Detectors") detectorLeaves
                    group (TreeLabel "Polarizers") polarizerGroups
                ]
                |> List.choose id
            {
                name = TreeName "By kind"
                root = Group (TreeLabel "Library", groups)
            }

    /// The Library error channel (errors as values; each case carries a `reason`).
    type LibraryError =
        | UnknownEntryIdErr of reason : string
        | NoEntriesForKindErr of reason : string
        | LibraryUnavailableErr of reason : string

    /// Library reads by kind, grouping tree and identity, with typed failures.
    /// Function-valued fields make proxy equality reference-based.
    [<ReferenceEquality>]
    type LibraryProxy =
        {
            entriesForKind : CatalogueKind -> Result<LibraryEntry list, LibraryError>
            libraryTrees : unit -> Result<LibraryTree list, LibraryError>
            tryGetEntry : ElementValueId -> Result<LibraryEntry option, LibraryError>
        }

    /// A samples-library search query (spec 0033 step 004): a case-insensitive name fragment
    /// (empty matches all) and an optional `SubstrateKind` facet. The query is DATA, so the
    /// samples panel drives one search seam (`SampleProxy.searchSamples`) instead of composing
    /// ad-hoc filter calls — the `MaterialQuery` convention (`MaterialLibrary.fs`).
    type SampleQuery =
        {
            text : NameFragment
            substrate : SubstrateKind option
        }

        /// The match-everything query (a search UI's initial state).
        static member empty : SampleQuery =
            {
                text = NameFragment ""
                substrate = None
            }

        /// Filters names case-insensitively and optionally by substrate, preserving order (spec 0050 S2.3).
        member this.selectFrom (samples : Sample list) : Sample list =
            let byText =
                samples
                |> List.filter (fun s -> s.name.value.IndexOf(this.text.value, StringComparison.OrdinalIgnoreCase) >= 0)
            match this.substrate with
            | Some kind -> byText |> List.filter (fun s -> s.substrate = kind)
            | None -> byText

    /// Sample-store failures carry diagnostic reasons for invalid samples, unknown or duplicate
    /// identities, pinned-version removal refusals and unavailable storage.
    type SampleError =
        | UnknownSampleIdErr of reason : string
        | DuplicateSampleIdErr of reason : string
        | InvalidSampleErr of reason : string
        /// Removal is refused while any version is pinned (spec 0038 Part H; spec 0050 S2.2).
        | SampleVersionPinnedErr of reason : string
        | SampleStoreUnavailableErr of reason : string

    type SampleError with

        /// Reports an unresolved sample identity (spec 0050 S2.3).
        static member unknownSample (id : SampleId) : SampleError =
            UnknownSampleIdErr $"unknown sample id '%s{string id.value}'"

        /// Reports an unresolved sample version (spec 0050 S2.3).
        static member unknownSampleVersion (version : SampleVersionId) : SampleError =
            UnknownSampleIdErr $"unknown sample version '%s{string version.sampleId.value}' v%d{version.version.value}"

        /// Reports removal blocked by pins, with ascending version numbers (spec 0050 S2.3).
        static member versionsPinned (sample : Sample) (versions : VersionNumber list) : SampleError =
            let versionList =
                versions
                |> List.map (fun version -> version.value)
                |> List.sort
                |> List.map (fun n -> $"v%d{n}")
                |> String.concat ", "
            SampleVersionPinnedErr $"sample '%s{sample.name.value}' ('%s{string sample.id.value}') has %d{List.length versions} version(s) pinned: %s{versionList}"

    /// The mutating samples write-seam (spec 0033 steps 004/005, contract STORE_XDUO_0002 —
    /// IMPLEMENTED lifecycle; re-typed IN PLACE at spec 0038 Part H step 022 into the VERSIONED
    /// surface, the exact mirror of step 021's `MaterialProxy`): the functional-proxy convention —
    /// a record of camelCase `Result`-returning functions; a test substitutes a stub of the SAME
    /// shape. Function-valued fields have no structural equality, so the proxy compares by reference
    /// — a host model holding one keeps its (Elmish-required) equality. The store is now versions
    /// per `SampleId`:
    ///
    /// - `listSamples scope` — the LATEST version of each sample, filtered by `scope` (`ActiveOnly`
    ///   for offers, `IncludeInactive` for the show-inactive toggle);
    /// - `searchSamples q` — the name / substrate facets over the latest-ACTIVE versions (offers);
    /// - `tryGetSample id` — the latest version's sample, if any;
    /// - `resolveVersion svid` — the EXACT version's sample, IGNORING lifecycle (reference
    ///   resolution: an experiment binding a superseded/inactive version still resolves it);
    /// - `saveSample sample` — applies the step-20 shared `decideVersioning` rule against the
    ///   latest version's pin (a new sample inserts version 1; an existing one mutates in
    ///   place, mints the next version, or is a no-op) — it replaces the old `addSample`/`updateSample`;
    /// - `markSampleInactive` / `markSampleActive` — retire / revive the latest version;
    /// - `supersedeSample` — retire the latest version (a superseded version behaves as inactive);
    /// - `pinSampleVersion` — pins one version permanently (spec 0050 S2.2);
    /// - `removeSample id` — deletes an unpinned sample and refuses any sample carrying a pinned
    ///   version (`SampleVersionPinnedErr`); an unknown id is `UnknownSampleIdErr`.
    ///
    /// Implementations are `SampleProxy.createInMemory` in Domain's `SampleStore.fs` and
    /// `SampleProxy.createInDatabase` in Storage's `SampleDatabaseStore.fs` (spec 0050 S2.3).
    [<ReferenceEquality>]
    type SampleProxy =
        {
            listSamples : InactiveVisibility -> Result<Sample list, SampleError>
            searchSamples : SampleQuery -> Result<Sample list, SampleError>
            tryGetSample : SampleId -> Result<Sample option, SampleError>
            resolveVersion : SampleVersionId -> Result<Sample option, SampleError>
            saveSample : Sample -> Result<unit, SampleError>
            markSampleInactive : SampleId -> Result<unit, SampleError>
            markSampleActive : SampleId -> Result<unit, SampleError>
            supersedeSample : SampleId -> Result<unit, SampleError>
            pinSampleVersion : SampleVersionId -> Result<unit, SampleError>
            removeSample : SampleId -> Result<unit, SampleError>
        }

    /// User-source failures, including protected writes, pinned removal and an unavailable store
    /// (spec 0046; spec 0050 §3.4, S2.3).
    type SourceError =
        | UnknownSourceIdErr of reason : string
        | InvalidSourceErr of reason : string
        | ProtectedSourceRefusedErr of reason : string
        | SourceVersionPinnedErr of reason : string
        | SourceStoreUnavailableErr of reason : string

        member this.reason : string =
            match this with
            | UnknownSourceIdErr reason
            | InvalidSourceErr reason
            | ProtectedSourceRefusedErr reason
            | SourceVersionPinnedErr reason
            | SourceStoreUnavailableErr reason -> reason

    type SourceError with

        /// Reports an id absent from the user-sources store (spec 0050 S2.3).
        static member unknownSource (id : ElementValueId) : SourceError =
            UnknownSourceIdErr $"unknown source id '{id.value}'"

    /// Unversioned user-source access by entry id: SourceProxy.createInMemory lists in creation order;
    /// SourceProxy.createInDatabase lists the latest versions by name, then id (spec 0046; spec 0050 §3.4, S2.3).
    /// SourceVersionPinnedErr and SourceStoreUnavailableErr never arise in memory, which has no pin or database.
    [<ReferenceEquality>]
    type SourceProxy =
        {
            listSources : unit -> Result<SourcePreset list, SourceError>
            tryGetSource : ElementValueId -> Result<SourcePreset option, SourceError>
            saveSource : SourcePreset -> Result<unit, SourceError>
            removeSource : ElementValueId -> Result<unit, SourceError>
        }

    /// Mint the entry id of a NEW user source (spec 0046) — at the editor's window open, like a new
    /// sample's `SampleId`, so the launcher keys the editor by the id Save persists under. The id is a
    /// plain GUID string, as a sample's is (`SampleId.toElementValueId`; spec 0050 §3.3).
    let newSourceId () : ElementValueId = ElementValueId (string (Guid.NewGuid ()))

    /// λ/4 film thicknesses of the 600 nm quarter-wave stack (glass n = 1.52; vacuum n = 1).
    let private qwGlassThickness : Thickness = Thickness.nm ((600.0 / 1.52 / 4.0) * oneNanometer)
    let private qwVacuumThickness : Thickness = Thickness.nm ((600.0 / 1.00 / 4.0) * oneNanometer)

    /// Starts the Sample editor with one glass/vacuum period at 600 nm quarter-wave thicknesses.
    /// The editor gives the new sample its own identity (spec 0035 step 014; spec 0050 S1.7).
    let starterMultilayerStructure : SampleStructure =
        {
            films =
                [
                    Repeated
                        {
                            cell =
                                [
                                    { materialId = MaterialVersionId.firstOf MaterialIds.glass152; thickness = qwGlassThickness; orientation = PrimaryAxes }
                                    { materialId = MaterialVersionId.firstOf MaterialIds.vacuumMaterial; thickness = qwVacuumThickness; orientation = PrimaryAxes }
                                ]
                            count = PeriodCount 1
                        }
                ]
            substrate = None
            lower = None
        }

    /// Bundles library reads with the write seam used by CatalogueImportProxy
    /// (spec 0038 step 042; spec 0050 §3.4).
    [<ReferenceEquality>]
    type LibraryEntryStore =
        {
            proxy : LibraryProxy
            addEntry : LibraryEntry -> Result<unit, LibraryError>
        }

    /// Empty store for the catalogue import through CatalogueImportProxy; entries retain insertion order.
    /// Duplicate ids return LibraryUnavailableErr; By kind is derived from held entries (spec 0050 S1.7).
    let createInMemory () : LibraryEntryStore =
        let store = ref ([] : LibraryEntry list)
        let proxy =
            {
                entriesForKind = fun kind -> Ok (store.Value |> List.filter (fun e -> e.forKinds |> List.contains kind))
                libraryTrees = fun () -> Ok [ LibraryTree.byKind store.Value ]
                tryGetEntry = fun id -> Ok (store.Value |> List.tryFind (fun e -> e.entryId = id))
            }
        let addEntry (entry : LibraryEntry) : Result<unit, LibraryError> =
            match store.Value |> List.tryFind (fun e -> e.entryId = entry.entryId) with
            | Some _ -> Error (LibraryUnavailableErr $"library entry '%s{entry.entryId.value}' is already present")
            | None ->
                store.Value <- store.Value @ [ entry ]
                Ok ()
        {
            proxy = proxy
            addEntry = addEntry
        }

    type Sample with

        /// The write-seam validation the samples store's `saveSample` runs. Three rules, in order:
        /// (spec 0033 steps 004/005) a blank display name is `InvalidSampleErr`; (spec 0035 step 012)
        /// a structure with NO films AND NO substrate is structurally empty — nothing for the engine
        /// mapping to expand — and is likewise `InvalidSampleErr`; and (spec 0040 Part D.1) a
        /// `ThickPlate`-geometry sample whose `structure.substrate` is `None` has no substrate plate to
        /// specify — a plate MUST carry one (spec Part D.0) — and is `InvalidSampleErr`. A `ThinFilm`
        /// is unaffected by the third rule (its lower half-space is semi-infinite).
        member s.validate : Result<unit, SampleError> =
            let structurallyEmpty =
                match s.structure.films, s.structure.substrate with
                | [], None -> true
                | _ -> false
            let plateMissingSubstrate =
                match s.substrate, s.structure.substrate with
                | ThickPlate, None -> true
                | _ -> false
            if String.IsNullOrWhiteSpace s.name.value
            then Error (InvalidSampleErr $"sample '%s{string s.id.value}' has a blank name")
            elif structurallyEmpty
            then Error (InvalidSampleErr $"sample '%s{string s.id.value}' has no films and no substrate")
            elif plateMissingSubstrate
            then Error (InvalidSampleErr $"plate sample '%s{string s.id.value}' has no substrate plate specified")
            else Ok ()

    /// Finds active samples referencing a material; pinned material versions block removal separately
    /// (spec 0033; spec 0038 Part H; spec 0050 S2.2). Requires a total in-memory listing;
    /// a fallible store must supply its own conservative lookup.
    let samplesReferencing (samples : SampleProxy) (id : MaterialId) : Sample list =
        match samples.listSamples ActiveOnly with
        | Ok all -> all |> List.filter (fun s -> s.structure.referencedMaterials |> Set.contains id)
        | Error _ -> []

    type SourcePreset with

        /// The write-seam validation the user-sources store's `saveSource` runs (spec 0046): a blank
        /// name, a blank id, or a wavelength that is not a positive finite length is `InvalidSourceErr`;
        /// a `ProtectedBuiltIn` source is refused (only user-managed sources are stored here).
        member s.validate : Result<unit, SourceError> =
            let positive (w : WaveLength) : bool =
                let metres = w.value / 1.0<meter>
                not (Double.IsNaN metres || Double.IsInfinity metres) && metres > 0.0
            let tuningProblem =
                match s.tuning with
                | FixedLine -> None
                | TunableBetween limits ->
                    let minimum = limits.lower
                    let maximum = limits.upper
                    if not (positive minimum && positive maximum) then Some "needs positive tunable limits"
                    elif minimum.value >= maximum.value then Some "needs a lower tunable limit below the upper one"
                    elif s.waveLength.value < minimum.value || s.waveLength.value > maximum.value then Some "must be set within its tunable limits"
                    else None
            if String.IsNullOrWhiteSpace s.id.value then Error (InvalidSourceErr "a source needs an id")
            elif String.IsNullOrWhiteSpace s.name.value then Error (InvalidSourceErr $"source '%s{s.id.value}' has a blank name")
            elif not (positive s.waveLength) then
                Error (InvalidSourceErr $"source '%s{s.name.value}' needs a positive wavelength")
            elif Option.isSome tuningProblem then
                Error (InvalidSourceErr $"source '%s{s.name.value}' %s{tuningProblem |> Option.defaultValue String.Empty}")
            else
                match s.protection with
                | ProtectedBuiltIn -> Error (ProtectedSourceRefusedErr $"source '%s{s.name.value}' is a protected built-in — it is not stored with the user sources")
                | UserManaged -> Ok ()

    type SourceProxy with

        /// The in-memory user-sources store (spec 0046): empty at start (§0.2 — nothing is persisted
        /// across app runs), `saveSource` inserts or replaces by id, `removeSource` deletes. Mutation
        /// stays INSIDE the closure (the IO boundary), so logic holding the proxy stays pure.
        static member createInMemory () : SourceProxy =
            let store = ref ([] : SourcePreset list)
            {
                listSources = fun () -> Ok store.Value
                tryGetSource = fun id -> Ok (store.Value |> List.tryFind (fun s -> s.id = id))
                saveSource =
                    fun source ->
                        source.validate
                        |> Result.map (fun () ->
                            match store.Value |> List.exists (fun s -> s.id = source.id) with
                            | true -> store.Value <- store.Value |> List.map (fun s -> if s.id = source.id then source else s)
                            | false -> store.Value <- store.Value @ [ source ])
                removeSource =
                    fun id ->
                        match store.Value |> List.exists (fun s -> s.id = id) with
                        | true ->
                            store.Value <- store.Value |> List.filter (fun s -> s.id <> id)
                            Ok ()
                        | false -> Error (SourceError.unknownSource id)
            }

    /// Composes the catalogue preset store with live samples and user sources (spec 0046; spec 0050 §3.4).
    /// Offers active samples, resolves retired samples, appends user sources after presets, and derives By kind
    /// from offered entries, preserving the first preset listing error (spec 0050 S1.7).
    let liveLibrary (presets : LibraryProxy) (samples : SampleProxy) (sources : SourceProxy) : LibraryProxy =
        let liveSamples () : LibraryEntry list =
            match samples.listSamples ActiveOnly with
            | Ok all -> all |> List.map SampleItem
            | Error _ -> []
        let userSources () : LibraryEntry list =
            match sources.listSources () with
            | Ok all -> all |> List.map SourceItem
            | Error _ -> []
        let isSample (e : LibraryEntry) : bool =
            match e with
            | SampleItem _ -> true
            | SourceItem _ | DetectorItem _ | PolarizerItem _ -> false
        let entriesForKind (kind : CatalogueKind) : Result<LibraryEntry list, LibraryError> =
            presets.entriesForKind kind
            |> Result.map (fun presetEntries ->
                let ofKind (entries : LibraryEntry list) = entries |> List.filter (fun e -> e.forKinds |> List.contains kind)
                ofKind (liveSamples ()) @ (presetEntries |> List.filter (isSample >> not)) @ ofKind (userSources ())
                |> List.distinctBy (fun e -> e.entryId))
        {
            entriesForKind = entriesForKind
            libraryTrees =
                fun () ->
                    [ CatalogueKind.Sample; LightSource; Detector; LinearPolarizer; CircularPolarizer ]
                    |> List.fold (fun collected kind ->
                        collected
                        |> Result.bind (fun entries ->
                            entriesForKind kind |> Result.map (fun offered -> entries @ offered))) (Ok [])
                    |> Result.map (fun entries ->
                        let distinctEntries = entries |> List.distinctBy (fun entry -> entry.entryId)
                        [ LibraryTree.byKind distinctEntries ])
            tryGetEntry =
                fun id ->
                    let userSource =
                        match sources.tryGetSource id with
                        | Ok found -> found |> Option.map SourceItem
                        | Error _ -> None
                    let liveSample =
                        match Guid.TryParse id.value with
                        | true, guid ->
                            match samples.tryGetSample (SampleId guid) with
                            | Ok found -> found |> Option.map SampleItem
                            | Error _ -> None
                        | false, _ -> None
                    match userSource, liveSample with
                    | Some entry, _ | None, Some entry -> Ok (Some entry)
                    | None, None -> presets.tryGetEntry id
        }
