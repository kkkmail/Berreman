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

    /// Elevated sample identity (spec 0033 step 002): a Guid-backed single-case DU — no raw string
    /// sample id appears in a domain record. `create` MINTS a fresh id; the seeded samples parse FIXED
    /// literal Guids (`SampleIds`) so the grouping trees and the tests stay deterministic across
    /// runs. The Selector `valueId` binding seam stays a string: `LibraryEntry.entryId` carries the
    /// Guid STRING form.
    type SampleId =
        | SampleId of Guid

        member this.value = let (SampleId g) = this in g
        static member create () : SampleId = Guid.NewGuid() |> SampleId

        /// The sample as a Library entry id: its Guid string form (spec 0033 step 002).
        member this.toElementValueId : ElementValueId = ElementValueId (string this.value)

    /// Module-level mint helper for `SampleId` (the case name and type name collide — so a qualified
    /// `Library.SampleId.create` resolves to the case rather than the static member).
    let newSampleId () : SampleId = SampleId.create ()

    /// The frozen identity of every seeded sample: the one place a built-in sample's fixed Guid
    /// literal is written (spec 0050 §3.3).
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

    /// The frozen identity of every seeded light-source, detector and polarizer preset: the one place
    /// a built-in preset's entry id is written (spec 0050 §3.3).
    module PresetIds =
        let source600 : ElementValueId = ElementValueId "src-600"

        let heNe633 : ElementValueId = ElementValueId "src-hene-633"
        let heNe543 : ElementValueId = ElementValueId "src-hene-543"
        let argon488 : ElementValueId = ElementValueId "src-ar-488"
        let argon515 : ElementValueId = ElementValueId "src-ar-515"
        let heCd442 : ElementValueId = ElementValueId "src-hecd-442"
        let heCd325 : ElementValueId = ElementValueId "src-hecd-325"
        let diode405 : ElementValueId = ElementValueId "src-diode-405"
        let ruby694 : ElementValueId = ElementValueId "src-ruby-694"
        let tiSapphire800 : ElementValueId = ElementValueId "src-tisa-800"
        let ndYag1064 : ElementValueId = ElementValueId "src-ndyag-1064"
        let ndYag532 : ElementValueId = ElementValueId "src-ndyag-532"
        let ndYag355 : ElementValueId = ElementValueId "src-ndyag-355"
        let ndYag266 : ElementValueId = ElementValueId "src-ndyag-266"
        let erbium1550 : ElementValueId = ElementValueId "src-er-1550"
        let co2Laser10600 : ElementValueId = ElementValueId "src-co2-10600"

        let mercuryG436 : ElementValueId = ElementValueId "src-hg-g-436"
        let mercuryH405 : ElementValueId = ElementValueId "src-hg-h-405"
        let mercuryI365 : ElementValueId = ElementValueId "src-hg-i-365"
        let krf248 : ElementValueId = ElementValueId "src-krf-248"
        let arf193 : ElementValueId = ElementValueId "src-arf-193"
        let f2Excimer157 : ElementValueId = ElementValueId "src-f2-157"
        let euv13p5 : ElementValueId = ElementValueId "src-euv-13p5"
        let beuv6p7 : ElementValueId = ElementValueId "src-beuv-6p7"

        let xenonMonochromator : ElementValueId = ElementValueId "src-xe-mono"
        let tiSapphireTunable : ElementValueId = ElementValueId "src-tisa-tunable"
        let supercontinuum : ElementValueId = ElementValueId "src-supercontinuum"
        let euvBeamline : ElementValueId = ElementValueId "src-euv-beamline"

        let detectorIntensity : ElementValueId = ElementValueId "det-intensity"
        let detectorEllipsometer : ElementValueId = ElementValueId "det-ellipsometer"
        let polarizerLp : ElementValueId = ElementValueId "pol-lp"
        let polarizerCpLeft : ElementValueId = ElementValueId "pol-cp-left"
        let polarizerCpRight : ElementValueId = ElementValueId "pol-cp-right"

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

        /// The version-ONE reference of a sample (spec 0038 step 025) — the pin a freshly-bound
        /// experiment descriptor carries. At this in-memory step every sample lives at version 1
        /// (`SampleStore` seeds v1 active; a mint needs a USED version, and until an experiment binds
        /// one the `VersionsInUse` set is empty), so `firstOf` IS the sample's current latest version.
        /// Mirrors `MaterialVersionId.firstOf` so the sample layer / experiment binding pin through ONE
        /// helper — step 25 (real `VersionsInUse`) has a single place to revisit pin-latest.
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
            /// The pinned material VERSION this layer is built against (spec 0038 Part H, step 022 —
            /// references point at a version, not merely a material): re-typed IN PLACE from
            /// `MaterialId` to `MaterialVersionId`, resolved through the versioned material store's
            /// by-version `resolveVersion` (`Propagation.resolveMaterialVersion`), so a later mint of
            /// the material's `.next` version never rewrites this layer's physics. The seeds and
            /// freshly-authored layers pin version one (`MaterialVersionId.firstOf`).
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
            /// The lower half-space material VERSION (`None` = vacuum). Re-typed IN PLACE from
            /// `MaterialId option` to `MaterialVersionId option` at step 022 — every material
            /// reference a sample carries points at a version (spec 0038 Part H), resolved through
            /// the by-version resolve; the seeds pin version one (`MaterialVersionId.firstOf`).
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

        /// Every material id the structure references (spec 0033 step 006): each film layer's
        /// material (a `Repeated` group's cell counted once — repetition adds no new
        /// reference), the substrate plate's material, and the lower half-space material.
        /// Pure — the referencing lookup `MaterialProxy.removeMaterial` consults is built
        /// over this (`samplesReferencing`).
        /// Every material IDENTITY the structure references (spec 0033 step 006; re-based on the
        /// versioned layers at step 022 — the removal hard-block is per material identity, so this
        /// projects each pinned `MaterialVersionId` down to its `MaterialId`, dropping the version).
        member this.referencedMaterials : Set<MaterialId> =
            let filmMaterialIds =
                this.films
                |> List.collect (fun item ->
                    match item with
                    | SingleLayer l -> [ l.materialId.materialId ]
                    | Repeated g -> g.cell |> List.map (fun l -> l.materialId.materialId))
            let substrateMaterialIds =
                this.substrate |> Option.toList |> List.map (fun l -> l.materialId.materialId)
            let lowerMaterialIds = this.lower |> Option.toList |> List.map (fun mvid -> mvid.materialId)
            filmMaterialIds @ substrateMaterialIds @ lowerMaterialIds |> Set.ofList

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

    /// Whether a library entry is a seeded built-in the user must not delete, inactivate, or
    /// supersede, or an ordinary user-managed entry (spec 0038 Part F — the notion "ideal element"
    /// disappears; the seeded presets become ordinary PROTECTED entries). A two-case DU, not a bool:
    /// protection is DATA on each preset record so a future user-created preset is `UserManaged`
    /// without a type change. Samples carry no field — every sample (seeded or user-created) is
    /// `UserManaged` (they are editable examples — recorded interpretation, spec F.0).
    type EntryProtection =
        | ProtectedBuiltIn
        | UserManaged

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

        /// The entry id (== the `valueId` written onto the bound table element). A sample's elevated
        /// `SampleId` crosses this seam as its Guid STRING form (spec 0033 step 002).
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

        /// Whether this entry is a protected built-in or user-managed (spec 0038 Part F). The
        /// presets carry protection as data; a sample is ALWAYS `UserManaged` — seeded samples are
        /// editable examples (recorded interpretation, spec F.0).
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

    /// Spec 0040 Part D.3: the placement emission a table element takes when bound to a library ENTRY.
    /// A bound SAMPLE bounds the placed element by its geometry-constrained `supportedEmission` (a
    /// `Mirror` reflects only, a `ThinFilm` / `ThickPlate` emits both by default, or the R/T-only it was constrained
    /// to) rather than the generic per-kind `Placement.defaultEmission` (which hands every non-mirror
    /// element `EmitBoth`). A source / detector / polarizer carries no such geometry constraint, so a
    /// placement bound to one keeps the `fallback` emission its catalogue kind already gave it. Pure —
    /// the host seeds a placed sample's emission through this, and the experiment's default
    /// `MeasurementMode` then follows via `Experiments.MeasurementMode.ofEmission`.
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

    /// The Library error channel (errors as values; each case carries a `reason`).
    type LibraryError =
        | UnknownEntryIdErr of reason : string
        | NoEntriesForKindErr of reason : string
        | LibraryUnavailableErr of reason : string

    /// The mock Library IO seam (the functional-proxy convention): a record of camelCase
    /// `Result`-returning functions. A test substitutes a stub of the SAME shape. Function-valued
    /// fields have no structural equality, so the proxy compares by reference — this lets a host model
    /// that holds a proxy keep its (Elmish-required) equality, comparing the proxy by identity.
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

    /// The samples write-seam error channel (errors as values; each case carries a diagnostic
    /// `reason` — a bare error case is useless in a log): an unknown id on lookup / update /
    /// remove, adding a sample under an id the library already holds, and rejecting a
    /// malformed sample.
    type SampleError =
        | UnknownSampleIdErr of reason : string
        | DuplicateSampleIdErr of reason : string
        | InvalidSampleErr of reason : string
        /// Spec 0038 Part H (step 022): removal is additionally hard-blocked when ANY version of the
        /// sample is currently bound by a live experiment (the injected `VersionsInUse` seam — the
        /// mirror of the material store's `MaterialVersionInUseErr`). A used version can never be
        /// deleted; the case names the offending versions so the refusal is actionable in a log.
        | SampleVersionInUseErr of reason : string

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
    ///   injected `VersionsInUse` (a new sample inserts version 1; an existing one mutates in
    ///   place, mints the next version, or is a no-op) — it replaces the old `addSample`/`updateSample`;
    /// - `markSampleInactive` / `markSampleActive` — retire / revive the latest version;
    /// - `supersedeSample` — retire the latest version (a superseded version behaves as inactive);
    /// - `removeSample id` — deletes an unused sample and refuses any sample carrying a used
    ///   version (`SampleVersionInUseErr`); an unknown id is `UnknownSampleIdErr`.
    ///
    /// The real, stateful in-memory store behind this surface is `SampleProxy.createInMemory` (a
    /// type augmentation in `SampleStore.fs`, after `Lifecycle.fs`: it needs the shared
    /// `decideVersioning` rule and the `VersionsInUse` seam).
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
            removeSample : SampleId -> Result<unit, SampleError>
        }

    /// The user-sources write-seam error channel (spec 0046; errors as values, each case carrying a
    /// diagnostic `reason`): an unknown id on remove, a malformed source, and an attempt to store a
    /// protected built-in (the seeded presets live in the read-only `LibraryProxy`, never here).
    type SourceError =
        | UnknownSourceIdErr of reason : string
        | InvalidSourceErr of reason : string
        | ProtectedSourceRefusedErr of reason : string

        member this.reason : string =
            match this with
            | UnknownSourceIdErr reason
            | InvalidSourceErr reason
            | ProtectedSourceRefusedErr reason -> reason

    /// The mutating USER-SOURCES write-seam (spec 0046 — the Library's source editor): the
    /// functional-proxy convention, a record of camelCase `Result`-returning functions a test
    /// substitutes with stubs of the SAME shape (reference-compared, so a model holding one keeps
    /// its equality). It holds only `UserManaged` sources; the seeded protected presets stay in the
    /// read-only `LibraryProxy`. Unversioned: an experiment binds a source by its entry id.
    ///
    /// - `listSources` — every user source, in creation order;
    /// - `tryGetSource id` — the user source with this entry id, if any;
    /// - `saveSource source` — insert a new source or replace the one with the same id (validated);
    /// - `removeSource id` — delete a user source; an unknown id is `UnknownSourceIdErr`.
    [<ReferenceEquality>]
    type SourceProxy =
        {
            listSources : unit -> Result<SourcePreset list, SourceError>
            tryGetSource : ElementValueId -> Result<SourcePreset option, SourceError>
            saveSource : SourcePreset -> Result<unit, SourceError>
            removeSource : ElementValueId -> Result<unit, SourceError>
        }

    /// Mint the entry id of a NEW user source (spec 0046) — at the editor's window open, like a new
    /// sample's `SampleId`, so the launcher keys the editor by the id Save persists under.
    let newSourceId () : ElementValueId = ElementValueId $"src-%s{Guid.NewGuid().ToString()}"

    /// A single-layer thin-film structure between vacuum (the common seed shape). The layer pins
    /// version one of the material (spec 0038 step 022 — the seeds carry version-pinned references).
    let private filmStructure (materialId : MaterialId) (thickness : Thickness) : SampleStructure =
        {
            films = [ SingleLayer { materialId = MaterialVersionId.firstOf materialId; thickness = thickness; orientation = PrimaryAxes } ]
            substrate = None
            lower = None
        }

    /// A thick-plate structure in vacuum (films empty; the plate is the substrate layer). The plate
    /// pins version one of the material (spec 0038 step 022).
    let private plateStructure (materialId : MaterialId) (thickness : Thickness) : SampleStructure =
        {
            films = []
            substrate = Some { materialId = MaterialVersionId.firstOf materialId; thickness = thickness; orientation = PrimaryAxes }
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
                                    { materialId = MaterialVersionId.firstOf MaterialIds.glass152; thickness = qwGlassThickness; orientation = PrimaryAxes }
                                    { materialId = MaterialVersionId.firstOf MaterialIds.vacuumMaterial; thickness = qwVacuumThickness; orientation = PrimaryAxes }
                                ]
                            count = PeriodCount 1
                        }
                ]
            substrate = None
            lower = None
        }

    /// The seeded samples (spec §2a), let-bound so `seedEntries`, the grouping tree, and the tests
    /// all reference the SAME values programmatically (spec 0033 step 002). Each id is its FIXED
    /// literal Guid in `SampleIds` (spec 0050 §3.3), deterministic across runs; seed construction
    /// never calls `SampleId.create`.
    module SeedSamples =

        let glassPlate1mm : Sample =
            {
                id = SampleIds.glassPlate1mmId
                name = EntryName "Glass plate (n=1.52, 1 mm)"
                structure = plateStructure MaterialIds.glass152 (Thickness.mm 1.0<mm>)
                substrate = ThickPlate
                description = EntryDescription "Single transparent-glass plate, n = 1.52, thickness 1 mm, in vacuum."
                supportedEmission = ThickPlate.defaultSupportedEmission
            }

        let glassPlate2mm : Sample =
            {
                id = SampleIds.glassPlate2mmId
                name = EntryName "Glass plate (n=1.52, 2 mm)"
                structure = plateStructure MaterialIds.glass152 (Thickness.mm 2.0<mm>)
                substrate = ThickPlate
                description = EntryDescription "Single transparent-glass plate, n = 1.52, thickness 2 mm, in vacuum."
                supportedEmission = ThickPlate.defaultSupportedEmission
            }

        let glassFilm600 : Sample =
            {
                id = SampleIds.glassFilm600Id
                name = EntryName "Glass thin film (n=1.75, 600 nm)"
                structure = filmStructure MaterialIds.glass175 (Thickness.nm 600.0<nm>)
                substrate = ThinFilm
                description = EntryDescription "Single transparent-glass thin film, n = 1.75, thickness 600 nm, between vacuum."
                supportedEmission = ThinFilm.defaultSupportedEmission
            }

        let glassVacuum : Sample =
            {
                id = SampleIds.glassVacuumId
                name = EntryName "Glass / vacuum interface (n=1.50)"
                structure = plateStructure MaterialIds.glass150 (Thickness.mm 1.0<mm>)
                substrate = ThickPlate
                description = EntryDescription "Semi-infinite transparent-glass / vacuum interface, n = 1.50 — the Fresnel / total-reflection demo."
                supportedEmission = ThickPlate.defaultSupportedEmission
            }

        let glassFilm200 : Sample =
            {
                id = SampleIds.glassFilm200Id
                name = EntryName "Glass thin film (n=1.52, 200 nm)"
                structure = filmStructure MaterialIds.glass152 (Thickness.nm 200.0<nm>)
                substrate = ThinFilm
                description = EntryDescription "Single transparent-glass thin film, n = 1.52, 200 nm, between vacuum."
                supportedEmission = ThinFilm.defaultSupportedEmission
            }

        let multilayerQw : Sample =
            {
                id = SampleIds.multilayerQwId
                name = EntryName "Quarter-wave glass/vacuum multilayer (41 layers)"
                structure =
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
                                        count = PeriodCount 20
                                    }
                                SingleLayer { materialId = MaterialVersionId.firstOf MaterialIds.glass152; thickness = qwGlassThickness; orientation = PrimaryAxes }
                            ]
                        substrate = None
                        lower = None
                    }
                substrate = ThinFilm
                description = EntryDescription "41-layer quarter-wave stack: alternating glass (n=1.52) and vacuum λ/4 films for 600 nm, 21 glass + 20 vacuum layers."
                supportedEmission = ThinFilm.defaultSupportedEmission
            }

        let euvMoSi : Sample =
            {
                id = SampleIds.euvMoSiId
                name = EntryName "EUV Mo/Si multilayer (100 pairs)"
                structure =
                    {
                        films =
                            [
                                Repeated
                                    {
                                        cell =
                                            [
                                                { materialId = MaterialVersionId.firstOf MaterialIds.euvMolybdenum; thickness = euvLayerThickness; orientation = PrimaryAxes }
                                                { materialId = MaterialVersionId.firstOf MaterialIds.euvSilicon; thickness = euvLayerThickness; orientation = PrimaryAxes }
                                            ]
                                        count = PeriodCount 100
                                    }
                            ]
                        substrate = None
                        lower = None
                    }
                substrate = Mirror
                description = EntryDescription "EUV reflective multilayer: 100 Mo/Si bilayers, each layer 2.65 nm (λ/4 at 10.6 nm), on vacuum."
                supportedEmission = Mirror.defaultSupportedEmission
            }

        let uniaxial : Sample =
            {
                id = SampleIds.uniaxialId
                name = EntryName "Uniaxial crystal film (1 µm)"
                structure = filmStructure MaterialIds.uniaxialCrystal (Thickness.nm 1000.0<nm>)
                substrate = ThinFilm
                description = EntryDescription "Uniaxial crystal thin film, nₒ = 1.5, nₑ = 1.65, thickness 1 µm, between vacuum."
                supportedEmission = ThinFilm.defaultSupportedEmission
            }

        let biaxial : Sample =
            {
                id = SampleIds.biaxialId
                name = EntryName "Biaxial crystal film (1 µm)"
                structure = filmStructure MaterialIds.biaxialCrystal (Thickness.nm 1000.0<nm>)
                substrate = ThinFilm
                description = EntryDescription "Biaxial crystal thin film, n = (1.5, 1.65, 1.75), thickness 1 µm, between vacuum."
                supportedEmission = ThinFilm.defaultSupportedEmission
            }

        let activeCrystalPlate : Sample =
            {
                id = SampleIds.activeCrystalPlateId
                name = EntryName "Active gyrotropic crystal plate (1 cm)"
                structure = plateStructure MaterialIds.activeCrystal Thickness.oneCentiMeter
                substrate = ThickPlate
                description = EntryDescription "Planar active (gyrotropic) crystal plate, n₁₁ = 2.315, n₃₃ = 2.226, optical-activity ρ₁₂ = 1.5e-6, thickness 1 cm."
                supportedEmission = ThickPlate.defaultSupportedEmission
            }

        let langasiteSilicon : Sample =
            {
                id = SampleIds.langasiteSiliconId
                name = EntryName "Langasite film on silicon (10 µm, dispersive)"
                structure =
                    {
                        films = [ SingleLayer { materialId = MaterialVersionId.firstOf MaterialIds.langasite; thickness = Thickness.mm 0.01<mm>; orientation = PrimaryAxes } ]
                        substrate = None
                        lower = Some (MaterialVersionId.firstOf MaterialIds.silicon)
                    }
                substrate = ThinFilm
                description = EntryDescription "Dispersive langasite thin film (10 µm) on a silicon substrate — wavelength-dependent n, k."
                supportedEmission = ThinFilm.defaultSupportedEmission
            }

        /// All seeded samples in Library display order (the order `seedEntries` lists them).
        let allSeedSamples : Sample list =
            [
                glassPlate1mm; glassPlate2mm; glassFilm600; glassVacuum; glassFilm200
                multilayerQw; euvMoSi; uniaxial; biaxial; activeCrystalPlate; langasiteSilicon
            ]

    /// The seeded light sources (spec 0046): the 600 nm reference source, well-known lasers, and the
    /// UV / EUV lithography sources — every one a PROTECTED monochromatic preset (a source is its
    /// wavelength, spec Q3). Commonly quoted line wavelengths; the excimer and mercury lines are the
    /// ones the lithography steppers are named after.
    module SeedSources =

        let private source (id : ElementValueId) (name : EntryName) (waveLength : WaveLength) : SourcePreset =
            { id = id; name = name; waveLength = waveLength; tuning = FixedLine; protection = ProtectedBuiltIn }

        let private tunable (id : ElementValueId) (name : EntryName) (minimum : WaveLength) (maximum : WaveLength) (setting : WaveLength) : SourcePreset =
            {
                id = id
                name = name
                waveLength = setting
                tuning = TunableBetween { Berreman.Dispersion.WaveLengthInterval.lower = minimum; upper = maximum }
                protection = ProtectedBuiltIn
            }

        /// The reference source every scene defaulted to before spec 0046.
        let reference600 : SourcePreset = source PresetIds.source600 (EntryName "Monochromatic 600 nm") (WaveLength.nm 600.0<nm>)

        /// Well-known lasers, by gain medium (the harmonics of Nd:YAG included).
        let lasers : SourcePreset list =
            [
                source PresetIds.heNe633 (EntryName "HeNe laser 632.8 nm") (WaveLength.nm 632.8<nm>)
                source PresetIds.heNe543 (EntryName "HeNe laser 543.5 nm (green)") (WaveLength.nm 543.5<nm>)
                source PresetIds.argon488 (EntryName "Argon-ion laser 488 nm") (WaveLength.nm 488.0<nm>)
                source PresetIds.argon515 (EntryName "Argon-ion laser 514.5 nm") (WaveLength.nm 514.5<nm>)
                source PresetIds.heCd442 (EntryName "HeCd laser 441.6 nm") (WaveLength.nm 441.6<nm>)
                source PresetIds.heCd325 (EntryName "HeCd laser 325 nm (UV)") (WaveLength.nm 325.0<nm>)
                source PresetIds.diode405 (EntryName "GaN diode laser 405 nm") (WaveLength.nm 405.0<nm>)
                source PresetIds.ruby694 (EntryName "Ruby laser 694.3 nm") (WaveLength.nm 694.3<nm>)
                source PresetIds.tiSapphire800 (EntryName "Ti:sapphire laser 800 nm") (WaveLength.nm 800.0<nm>)
                source PresetIds.ndYag1064 (EntryName "Nd:YAG laser 1064 nm") (WaveLength.nm 1064.0<nm>)
                source PresetIds.ndYag532 (EntryName "Nd:YAG ×2 532 nm") (WaveLength.nm 532.0<nm>)
                source PresetIds.ndYag355 (EntryName "Nd:YAG ×3 355 nm (UV)") (WaveLength.nm 355.0<nm>)
                source PresetIds.ndYag266 (EntryName "Nd:YAG ×4 266 nm (DUV)") (WaveLength.nm 266.0<nm>)
                source PresetIds.erbium1550 (EntryName "Erbium fibre laser 1550 nm") (WaveLength.nm 1550.0<nm>)
                source PresetIds.co2Laser10600 (EntryName "CO₂ laser 10.6 µm") (WaveLength.nm 10600.0<nm>)
            ]

        /// The UV / EUV lithography sources: the mercury-lamp lines, the DUV excimer lasers, and the
        /// tin-plasma EUV source with its beyond-EUV successor.
        let lithography : SourcePreset list =
            [
                source PresetIds.mercuryG436 (EntryName "Mercury g-line 435.8 nm") (WaveLength.nm 435.8<nm>)
                source PresetIds.mercuryH405 (EntryName "Mercury h-line 404.7 nm") (WaveLength.nm 404.7<nm>)
                source PresetIds.mercuryI365 (EntryName "Mercury i-line 365 nm") (WaveLength.nm 365.0<nm>)
                source PresetIds.krf248 (EntryName "KrF excimer 248 nm (DUV)") (WaveLength.nm 248.4<nm>)
                source PresetIds.arf193 (EntryName "ArF excimer 193 nm (DUV, dry / immersion)") (WaveLength.nm 193.4<nm>)
                source PresetIds.f2Excimer157 (EntryName "F₂ excimer 157 nm (VUV)") (WaveLength.nm 157.6<nm>)
                source PresetIds.euv13p5 (EntryName "EUV 13.5 nm (Sn plasma)") (WaveLength.nm 13.5<nm>)
                source PresetIds.beuv6p7 (EntryName "BEUV 6.7 nm (beyond EUV)") (WaveLength.nm 6.7<nm>)
            ]

        /// Tunable sources (spec 0046): an experiment varies their wavelength within their limits, or sets
        /// it there when it varies something else.
        let tunables : SourcePreset list =
            [
                tunable PresetIds.xenonMonochromator (EntryName "Xenon lamp + monochromator, 190–1000 nm") (WaveLength.nm 190.0<nm>) (WaveLength.nm 1000.0<nm>) (WaveLength.nm 600.0<nm>)
                tunable PresetIds.tiSapphireTunable (EntryName "Ti:sapphire laser, tunable 700–1000 nm") (WaveLength.nm 700.0<nm>) (WaveLength.nm 1000.0<nm>) (WaveLength.nm 800.0<nm>)
                tunable PresetIds.supercontinuum (EntryName "Supercontinuum source, 450–2400 nm") (WaveLength.nm 450.0<nm>) (WaveLength.nm 2400.0<nm>) (WaveLength.nm 600.0<nm>)
                tunable PresetIds.euvBeamline (EntryName "EUV beamline (synchrotron), 5–20 nm") (WaveLength.nm 5.0<nm>) (WaveLength.nm 20.0<nm>) (WaveLength.nm 13.5<nm>)
            ]

        /// Every seeded source, in Library display order.
        let allSeedSources : SourcePreset list = reference600 :: lasers @ lithography @ tunables

    /// The seeded Library entries (spec §2a "Seeded entries"): samples (glass plate → second
    /// thickness → thin film → a quarter-wave multilayer placeholder), the two detectors, one ideal
    /// LP + two ideal CP, and the seeded sources (`SeedSources`). Every sample's stack is DATA (spec 0033
    /// step 001) — the multilayers are `Repeated` period groups, never a per-id special case — and
    /// the samples are the named `SeedSamples` values (spec 0033 step 002). The six presets are
    /// ordinary PROTECTED entries (spec 0038 Part F — `ProtectedBuiltIn`); the seeded samples stay
    /// `UserManaged` (editable examples). The three ideals are `ComputedIdeal` behaviour data that
    /// still synthesizes through `Propagation.inputStokes` / `analyzerMueller` exactly as before.
    let seedEntries : LibraryEntry list =
        (SeedSamples.allSeedSamples |> List.map SampleItem)
        @ [
            DetectorItem { id = PresetIds.detectorIntensity; name = EntryName "Intensity detector"; kind = Intensity; protection = ProtectedBuiltIn }
            DetectorItem { id = PresetIds.detectorEllipsometer; name = EntryName "Ellipsometer"; kind = Ellipsometer; protection = ProtectedBuiltIn }
            PolarizerItem { id = PresetIds.polarizerLp; name = EntryName "Ideal linear polarizer"; behavior = ComputedIdeal IdealLinear; category = LpCategory; protection = ProtectedBuiltIn }
            PolarizerItem { id = PresetIds.polarizerCpLeft; name = EntryName "Ideal circular polarizer (left)"; behavior = ComputedIdeal IdealCircularLeft; category = CpCategory; protection = ProtectedBuiltIn }
            PolarizerItem { id = PresetIds.polarizerCpRight; name = EntryName "Ideal circular polarizer (right)"; behavior = ComputedIdeal IdealCircularRight; category = CpCategory; protection = ProtectedBuiltIn }
        ]
        // Spec 0046: the seeded sources — the 600 nm reference, the lasers, the lithography sources.
        @ (SeedSources.allSeedSources |> List.map SourceItem)

    /// A grouping-tree leaf for a seeded sample: references the sample VALUE programmatically
    /// (spec 0033 step 002 — the id literal lives only in `SampleIds`), carrying its Guid-string
    /// entry id exactly as `tryGetEntry` resolves it.
    let private sampleLeaf (label : TreeLabel) (s : Sample) : LibraryTreeNode =
        Leaf (label, (SampleItem s).entryId)

    /// One canonical grouping tree (R3): the entries organised by kind, with the two glass plates
    /// nested under their shared material (the "same glass, different thickness" grouping §2a).
    let private seedTrees : LibraryTree list =
        [
            {
                name = TreeName "By kind"
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
                                               sampleLeaf (TreeLabel "1 mm plate") SeedSamples.glassPlate1mm
                                               sampleLeaf (TreeLabel "2 mm plate") SeedSamples.glassPlate2mm
                                               sampleLeaf (TreeLabel "200 nm film") SeedSamples.glassFilm200
                                           ])
                                      sampleLeaf (TreeLabel "Glass film (n=1.75)") SeedSamples.glassFilm600
                                      sampleLeaf (TreeLabel "Glass / vacuum interface (n=1.50)") SeedSamples.glassVacuum
                                      Group
                                          (TreeLabel "Multilayers",
                                           [
                                               sampleLeaf (TreeLabel "Quarter-wave glass/vacuum (41)") SeedSamples.multilayerQw
                                               sampleLeaf (TreeLabel "EUV Mo/Si (100 pairs)") SeedSamples.euvMoSi
                                           ])
                                      Group
                                          (TreeLabel "Crystals",
                                           [
                                               sampleLeaf (TreeLabel "Uniaxial film") SeedSamples.uniaxial
                                               sampleLeaf (TreeLabel "Biaxial film") SeedSamples.biaxial
                                               sampleLeaf (TreeLabel "Active gyrotropic plate") SeedSamples.activeCrystalPlate
                                           ])
                                      Group
                                          (TreeLabel "Dispersive",
                                           [
                                               sampleLeaf (TreeLabel "Langasite on silicon") SeedSamples.langasiteSilicon
                                           ])
                                  ])
                             Group
                                 (TreeLabel "Sources",
                                  [
                                      Leaf (TreeLabel "600 nm", SeedSources.reference600.id)
                                      Group (TreeLabel "Lasers", SeedSources.lasers |> List.map (fun src -> Leaf (src.name.toTreeLabel, src.id)))
                                      Group (TreeLabel "Lithography (UV / EUV)", SeedSources.lithography |> List.map (fun src -> Leaf (src.name.toTreeLabel, src.id)))
                                      Group (TreeLabel "Tunable", SeedSources.tunables |> List.map (fun src -> Leaf (src.name.toTreeLabel, src.id)))
                                  ])
                             Group
                                 (TreeLabel "Detectors",
                                  [
                                      Leaf (TreeLabel "Intensity", PresetIds.detectorIntensity)
                                      Leaf (TreeLabel "Ellipsometer", PresetIds.detectorEllipsometer)
                                  ])
                             Group
                                 (TreeLabel "Polarizers",
                                  [
                                      Leaf (TreeLabel "Linear", PresetIds.polarizerLp)
                                      Leaf (TreeLabel "Circular (L)", PresetIds.polarizerCpLeft)
                                      Leaf (TreeLabel "Circular (R)", PresetIds.polarizerCpRight)
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

    /// The EMPTY, WRITABLE in-memory library-entry store for the seeding pipeline (spec 0038 step
    /// 042 — IMPLEMENT_CONTRACT STORE_XDUO_0008). The read-only seeded `createInMemory` above closes
    /// over an immutable `seedEntries` and so has NO write verb; the seeding pipeline needs one.
    /// `createInMemoryEmpty` bundles the read-only `LibraryProxy` (over a shared mutable list) with
    /// the `addEntry` write verb the pipeline pushes each `LibraryEntry` through — the analogue of
    /// the catalogue stores' add verbs (`addCategory` / `saveMaterial` / `saveSample`). A future DB
    /// cycle swaps this whole builder, leaving the `SeedingProxy` that adapts `addEntry` unchanged.
    [<ReferenceEquality>]
    type LibraryEntryStore =
        {
            proxy : LibraryProxy
            addEntry : LibraryEntry -> Result<unit, LibraryError>
        }

    /// Build the empty writable library-entry store: `addEntry` appends a new entry (preserving
    /// insertion order, so `entriesForKind` / `tryGetEntry` read exactly as the seeded store does)
    /// and rejects a duplicate entry id (`LibraryUnavailableErr` — the only fitting existing case;
    /// never a throw). Mutation stays INSIDE the closure (the IO boundary), so logic holding the
    /// proxy stays pure. `libraryTrees` is empty until a store cycle seeds grouping trees — the
    /// seed-push pipeline fills entries, not trees.
    let createInMemoryEmpty () : LibraryEntryStore =
        let store = ref ([] : LibraryEntry list)
        let proxy =
            {
                entriesForKind = fun kind -> Ok (store.Value |> List.filter (fun e -> e.forKinds |> List.contains kind))
                libraryTrees = fun () -> Ok []
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

    // The real, stateful in-memory samples store behind the write-seam (STORE_XDUO_0002) moved to
    // `SampleStore.fs` at spec 0038 Part H step 022 (mirroring the material store's step-021 move):
    // the VERSIONED store needs the shared `decideVersioning` rule and the `VersionsInUse` seam
    // (`Lifecycle.fs`, which compiles AFTER this file because it needs `SampleId` from `Library`
    // above), so its `createInMemory` augmentation can no longer live here. The `SampleProxy` record
    // itself stays re-typed IN PLACE above; `Sample.validate` (just above) is shared by the
    // store; `samplesReferencing` (its composition lookup) stays below (it is `SampleProxy`-typed).

    /// The composition-root referencing lookup for `MaterialProxy.createInMemory` (spec 0033
    /// step 006): every sample the samples store currently holds whose structure references
    /// the material. Backed by the LIVE `SampleProxy` store — once the referencing samples are
    /// removed the material becomes removable; there is no snapshot to refresh. Lists the
    /// latest-ACTIVE versions (`ActiveOnly`): a retired sample no longer offers new use, so it no
    /// longer holds a material from removal (the material's own used-version block still guards any
    /// version a live experiment binds). The in-memory `listSamples` is total (always `Ok`); the
    /// signature carries no error channel, so a future store whose listing can fail must supply its
    /// own conservative lookup instead of this one.
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
                        | false -> Error (UnknownSourceIdErr $"unknown source id '%s{id.value}'")
            }

    /// The LIVE Library (spec 0046): the read-only seeded `presets` composed with the app-scope
    /// SAMPLES and USER-SOURCES stores, so an entry created in the Library window (a sample through
    /// the Sample editor, a source through the source editor) is offered for its kind AND resolves
    /// once a table element is bound to it. Samples come from the live store (the latest ACTIVE
    /// versions are offered; `tryGetEntry` resolves any latest version, so a bound retired sample
    /// still resolves), user sources follow the seeded ones, the other presets are the seeds'.
    /// `libraryTrees` stays the seeded grouping. A proxy listing error passes through as
    /// `LibraryUnavailableErr`.
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
        {
            entriesForKind =
                fun kind ->
                    presets.entriesForKind kind
                    |> Result.map (fun seeded ->
                        let ofKind (entries : LibraryEntry list) = entries |> List.filter (fun e -> e.forKinds |> List.contains kind)
                        ofKind (liveSamples ()) @ (seeded |> List.filter (isSample >> not)) @ ofKind (userSources ())
                        |> List.distinctBy (fun e -> e.entryId))
            libraryTrees = presets.libraryTrees
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
