namespace OpticalConstructor.Domain

open System.Numerics
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Fields
open Berreman.Media
open Berreman.Solvers
open Berreman.FieldFunctions            // Solution.func (the engine's flux T, spec 0046)
open Berreman.Constants
open Berreman.Dispersion
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library

/// What the propagation pipeline reads off the engine's types, as members every holder of one finds (spec 0048
/// E017, ELEV013): the Stokes components and readings, a Mueller element, the Stokes rotation and a layer's
/// thickness. `RealMatrix4x4` / `RealVector4` indexing happens here and nowhere else.
[<AutoOpen>]
module PropagationReadouts =

    /// The intensity an intensity detector records: S₀ of the Stokes vector arriving at it, relative to the source's
    /// intensity reference (spec 0046; spec 0050 S8.1).
    type DetectorIntensity =
        | DetectorIntensity of float

        member this.value = let (DetectorIntensity i) = this in i

    type StokesVector with

        /// S₀, the intensity component (the IO seam to the engine's `RealVector4`, as are S₁ … S₃).
        member this.s0 : float = let (StokesVector (RealVector4 rv)) = this in rv.[0]

        member this.s1 : float = let (StokesVector (RealVector4 rv)) = this in rv.[1]

        member this.s2 : float = let (StokesVector (RealVector4 rv)) = this in rv.[2]

        member this.s3 : float = let (StokesVector (RealVector4 rv)) = this in rv.[3]

        /// What an intensity detector records — this vector's S₀ (spec §4).
        member this.intensity : DetectorIntensity = DetectorIntensity this.s0

        /// Ellipsometric Ψ, Δ from the normalized Stokes vector — what an ellipsometer reads (spec §4). For the
        /// convention
        ///   S1 = −cos 2Ψ ,  S2 = sin 2Ψ cos Δ ,  S3 = −sin 2Ψ sin Δ
        /// the inverse is  2Ψ = atan2( √(S2²+S3²), −S1 ) ,  Δ = atan2( −S3, S2 ).
        member this.psiDelta : Analytics.AnalysisFunctions.PsiDelta =
            let s0v = this.s0
            let n = if abs s0v > 1e-12 then s0v else 1.0
            let s1n, s2n, s3n = this.s1 / n, this.s2 / n, this.s3 / n
            let twoPsi = atan2 (sqrt (s2n * s2n + s3n * s3n)) (- s1n)
            let delta = atan2 (- s3n) s2n
            { psi = Angle (twoPsi / 2.0); delta = Angle delta }

    type MuellerMatrix with

        /// One element, row `i`, column `j` (the read seam to the engine's `RealMatrix4x4`; tests compare
        /// matrices through it).
        member this.element (i : int) (j : int) : float =
            let (MuellerMatrix m) = this
            m.[i, j]

    type Angle with

        /// The standard Stokes rotation matrix R(θ) (frame rotation by θ):
        ///   [[1,0,0,0],[0,cos2θ,sin2θ,0],[0,−sin2θ,cos2θ,0],[0,0,0,1]].
        /// The sign convention is pinned by `Propagation.rotateMueller`: R(−θ)·LP₀·R(θ) must reproduce
        /// `analyzerMueller IdealLinear θ` exactly.
        member this.rotationMueller : MuellerMatrix =
            let c = cos (2.0 * this.value)
            let s = sin (2.0 * this.value)
            [ [ 1.0; 0.0; 0.0; 0.0 ]
              [ 0.0; c; s; 0.0 ]
              [ 0.0; -s; c; 0.0 ]
              [ 0.0; 0.0; 0.0; 1.0 ] ]
            |> RealMatrix4x4.create
            |> MuellerMatrix

    type Thickness with

        /// The thickness in metres of a finite layer, or `None` for a semi-infinite (`Infinity`) half-space /
        /// plate (spec 0027 / 026 — the Details band view reads thicknesses without naming the engine's
        /// `Thickness` DU, which collides with `Avalonia.Thickness` in the UI layer).
        member this.metersOpt : float option =
            match this with
            | Thickness.Infinity -> None
            | Thickness.Thickness d -> Some (d / 1.0<meter>)


/// Spec 0027 (024) Phase 3 — the pure Mueller-matrix / Stokes-vector propagation pipeline (the only new
/// physics-plumbing module). It composes the EXISTING engine (the sample's Mueller matrix comes from
/// `OpticalSystemSolver`, never reinvented) with the standard ideal-polarizer Mueller matrices (written
/// here as 4×4 constants). The propagation is:
///
///   source            → λ
///   input LP / CP     → the input Stokes vector  SV_in  (rotated by the input polarizer's R1)
///   sample            → the engine's Mueller matrix  MM_sample  at (λ, incidence)
///   SV propagation    → SV_out = MM_sample · SV_in
///   analyzer (LP/CP)  → SV_det = MM_analyzer · SV_out  (its Mueller matrix rotates with its R1)
///   intensity detector→ records S0
///
/// Running `RotateR1FullCircle` over the analyzer's R1 (0…360°) yields the (angleDeg, intensity) curve;
/// with an identity (absent) sample and an ideal LP input + LP analyzer this is exactly the Malus law.
/// Every primitive is elevated (`PsiDelta`, `IntensityCurve` are records, not bare tuples/floats); the
/// matrices are written through `RealMatrix4x4.create` and read through `PropagationReadouts`.
module Propagation =

    /// The fully-polarized, unit-intensity Stokes vector produced by an IDEAL input polarizer at
    /// orientation `theta` (the input polarizer's R1, in radians via `Angle.value`):
    ///   ideal linear at θ : [1; cos 2θ; sin 2θ; 0]
    ///   ideal circular (left)  : [1; 0; 0; +1]
    ///   ideal circular (right) : [1; 0; 0; −1]
    /// (left = +S3 here — pick one handedness convention and stay consistent.)
    let inputStokes (kind : PolarizerKind) (theta : Angle) : StokesVector =
        let t2 = 2.0 * theta.value
        match kind with
        | IdealLinear -> StokesVector.create [ 1.0; cos t2; sin t2; 0.0 ]
        | IdealCircularLeft -> StokesVector.create [ 1.0; 0.0; 0.0; 1.0 ]
        | IdealCircularRight -> StokesVector.create [ 1.0; 0.0; 0.0; -1.0 ]

    /// Unpolarized natural light (no input polarizer present — nothing is synthesized; spec R1).
    let unpolarizedStokes : StokesVector = StokesVector.create [ 1.0; 0.0; 0.0; 0.0 ]

    /// The Mueller matrix of an IDEAL polarizer / analyzer at orientation `theta` (its R1). The linear case
    /// is the standard ½·[[1,c,s,0],[c,c²,cs,0],[s,cs,s²,0],[0,0,0,0]] (c = cos2θ, s = sin2θ); the circular
    /// cases are the standard ½ circular-polarizer matrices (left = +S3, right = −S3).
    let analyzerMueller (kind : PolarizerKind) (theta : Angle) : MuellerMatrix =
        let c = cos (2.0 * theta.value)
        let s = sin (2.0 * theta.value)
        let h = 0.5
        match kind with
        | IdealLinear ->
            [ [ h; h * c; h * s; 0.0 ]
              [ h * c; h * c * c; h * c * s; 0.0 ]
              [ h * s; h * c * s; h * s * s; 0.0 ]
              [ 0.0; 0.0; 0.0; 0.0 ] ]
            |> RealMatrix4x4.create
            |> MuellerMatrix
        | IdealCircularLeft ->
            [ [ h; 0.0; 0.0; h ]
              [ 0.0; 0.0; 0.0; 0.0 ]
              [ 0.0; 0.0; 0.0; 0.0 ]
              [ h; 0.0; 0.0; h ] ]
            |> RealMatrix4x4.create
            |> MuellerMatrix
        | IdealCircularRight ->
            [ [ h; 0.0; 0.0; -h ]
              [ 0.0; 0.0; 0.0; 0.0 ]
              [ 0.0; 0.0; 0.0; 0.0 ]
              [ -h; 0.0; 0.0; h ] ]
            |> RealMatrix4x4.create
            |> MuellerMatrix

    /// The identity Mueller matrix — the "no sample present" pass-through (spec R1: an absent sample is
    /// skipped, not synthesized). With it the rotating-analyzer curve reduces to the Malus law.
    let identityMueller : MuellerMatrix =
        [ [ 1.0; 0.0; 0.0; 0.0 ]
          [ 0.0; 1.0; 0.0; 0.0 ]
          [ 0.0; 0.0; 1.0; 0.0 ]
          [ 0.0; 0.0; 0.0; 1.0 ] ]
        |> RealMatrix4x4.create
        |> MuellerMatrix

    // -----------------------------------------------------------------------------------------------------
    // Spec 0038 Part F (step 014) — constant-Mueller polarizer evaluation. `PolarizerBehavior` is DATA in
    // the Library domain; its evaluation lives ONLY here, in the Stokes/Mueller pipeline (a ConstantMueller
    // entry never enters the Berreman stack). `ComputedIdeal` keeps synthesizing through the EXISTING
    // `analyzerMueller` above, exactly as before. R(θ) is `Angle.rotationMueller`.
    // -----------------------------------------------------------------------------------------------------

    /// A polarizing element stored at reference orientation, physically rotated to θ (spec 0038 Part F):
    /// R(−θ)·M·R(θ).
    let rotateMueller (theta : Angle) (m : MuellerMatrix) : MuellerMatrix =
        (Angle (- theta.value)).rotationMueller * (m * theta.rotationMueller)

    type MuellerComponent with

        /// This compound component at its own fixed offset within the compound: the stored
        /// reference-orientation matrix rotated by `offset` — R(−offset)·M·R(offset).
        member this.offsetMueller : MuellerMatrix = rotateMueller this.offset this.matrix

    /// The compound's Mueller matrix: the ORDERED product of the offset-rotated components. The list is
    /// in light-traversal order (the FIRST component is the first surface light hits), so the product is
    /// Mₙ·…·M₂·M₁ and `compoundMueller components * sv` applies the first component first.
    let compoundMueller (components : MuellerComponent list) : MuellerMatrix =
        components |> List.fold (fun acc c -> c.offsetMueller * acc) identityMueller

    /// A polarizer behaviour's Mueller matrix at the element's live orientation `theta` (its R1):
    /// `ComputedIdeal` synthesizes through the EXISTING `analyzerMueller` exactly as today;
    /// `ConstantMueller` rotates the WHOLE compound by θ — algebraically identical to rotating each
    /// component by (θ + offset), since R(a)·R(b) = R(a+b).
    let behaviorMueller (behavior : PolarizerBehavior) (theta : Angle) : MuellerMatrix =
        match behavior with
        | ComputedIdeal kind -> analyzerMueller kind theta
        | ConstantMueller components -> rotateMueller theta (compoundMueller components)

    /// One resolved layer of a sample (spec 0033 step 020): the dispersive engine layer plus the
    /// crystal orientation the system builder applies AT BUILD TIME — nothing is stored rotated.
    type ResolvedLayer =
        {
            layerWithDisp : LayerWithDisp
            orientation : CrystalOrientation
        }

        /// The engine layer at the run wavelength with the orientation applied: `PrimaryAxes` builds
        /// the unrotated layer (tensors exactly as stored); an `EulerRotation` rotates it via
        /// `Layer.rotate` (→ `OpticalProperties.rotate`), exactly as rotating the layer directly
        /// would (the `ActiveCrystalComparison.fsx` plate-rotation precedent).
        member this.getLayer (w : WaveLength) : Layer =
            let layer = this.layerWithDisp.getLayer w
            match this.orientation with
            | PrimaryAxes -> layer
            | EulerRotation _ -> layer.rotate this.orientation.toRotation

    /// A Library `Sample` resolved against the material library (spec 0033 step 001): every referenced
    /// material carried as its DISPERSIVE engine properties (`ResolvedLayer` — resolved once, evaluated
    /// per wavelength, its crystal orientation applied at build time), the film stack already expanded
    /// from its `StackItem`s, and the lower half-space defaulting to vacuum. `sampleToSystem` over this
    /// is TOTAL — resolution (and its typed error) happened up front in `resolveSampleMaterials`.
    type ResolvedSample =
        {
            name : EntryName
            films : ResolvedLayer list
            substrate : ResolvedLayer option
            lower : OpticalPropertiesWithDisp
        }

    /// Resolve a pinned material VERSION to its dispersive engine properties through the versioned
    /// material store's by-version resolve (spec 0038 Part H, step 022): the layer's pinned
    /// `MaterialVersionId` resolves to the EXACT version it was built against, IGNORING lifecycle, so
    /// a later mint of the material's `.next` version never rewrites an existing sample's physics. An
    /// unresolved version (unknown id, or a version the store never held) is a typed
    /// `Error (UnknownMaterialId _)` — never a fallback.
    let resolveMaterialVersion (materials : MaterialProxy) (mvid : MaterialVersionId) : Result<OpticalPropertiesWithDisp, MaterialError> =
        materials.resolveVersion mvid
        |> Result.bind (fun entryOpt ->
            match entryOpt with
            | Some e -> Ok e.properties
            | None -> Error (MaterialError.unknownMaterialVersion mvid))

    /// Resolve every material a sample's structure references to its `OpticalPropertiesWithDisp`
    /// through the VERSIONED material store's by-version resolve (spec 0033 step 001; re-based on
    /// versioned references at spec 0038 step 022). Each layer pins a `MaterialVersionId`, resolved
    /// through `resolveMaterialVersion` so a pinned version keeps resolving after the material
    /// evolves. An unknown/absent version is a typed `Error (UnknownMaterialId _)` — never a
    /// fallback. Hosts call this ONCE per run and surface the error as a message.
    let resolveSampleMaterials (materials : MaterialProxy) (sample : Sample) : Result<ResolvedSample, MaterialError> =
        let resolveLayer (l : SampleLayer) : Result<ResolvedLayer, MaterialError> =
            resolveMaterialVersion materials l.materialId
            |> Result.map (fun p ->
                {
                    layerWithDisp = { propertiesWithDisp = p; thickness = l.thickness }
                    orientation = l.orientation
                })
        let rec resolveFilms (pending : SampleLayer list) (acc : ResolvedLayer list) : Result<ResolvedLayer list, MaterialError> =
            match pending with
            | [] -> Ok (List.rev acc)
            | l :: rest ->
                match resolveLayer l with
                | Ok r -> resolveFilms rest (r :: acc)
                | Error e -> Error e
        match resolveFilms sample.structure.expandedFilms [] with
        | Error e -> Error e
        | Ok films ->
            let substrateResult =
                match sample.structure.substrate with
                | None -> Ok None
                | Some l -> resolveLayer l |> Result.map Some
            match substrateResult with
            | Error e -> Error e
            | Ok substrate ->
                let lowerResult =
                    match sample.structure.lower with
                    | None -> Ok OpticalProperties.vacuum.dispersive
                    | Some mvid -> resolveMaterialVersion materials mvid
                match lowerResult with
                | Error e -> Error e
                | Ok lower -> Ok { name = sample.name; films = films; substrate = substrate; lower = lower }

    /// Map a RESOLVED sample to an engine `OpticalSystem` at the run wavelength `w` (the wavelength only
    /// matters for the dispersive materials; the rest ignore it): evaluate each material at `w`, apply
    /// each layer's crystal orientation (spec 0033 step 020 — `ResolvedLayer.getLayer` rotates a
    /// non-identity orientation via `Layer.rotate`; `PrimaryAxes` builds the stored tensors), and
    /// assemble films / substrate plate / lower half-space in vacuum. TOTAL over the expanded
    /// structure — no per-sample-id branching.
    let sampleToSystem (sample : ResolvedSample) (w : WaveLength) : OpticalSystem =
        {
            description = Some sample.name.toOpticalSystemDescription
            upper = OpticalProperties.vacuum
            films = sample.films |> List.map (fun f -> f.getLayer w)
            substrate = sample.substrate |> Option.map (fun s -> Substrate.Plate (s.getLayer w))
            lower = sample.lower.getProperties w
        }

    /// Which branch of the sample an experiment captures (spec 028): the transmitted or the reflected
    /// Mueller matrix from the engine. `Experiments.MeasurementMode.CaptureBoth` drives one curve per
    /// branch at the host.
    type Branch =
        | BranchTransmitted
        | BranchReflected

    /// The sample's Mueller matrix for a branch, from the EXISTING engine (no new physics): the same solve
    /// as before, reading the transmitted or the reflected Mueller matrix.
    let sampleMueller (branch : Branch) (sample : ResolvedSample) (w : WaveLength) (inc : IncidenceAngle) : MuellerMatrix =
        let info = { (IncidentLightInfo.createInclined w inc) with refractionIndex = RefractionIndex.vacuum }
        let solver = OpticalSystemSolver(info, sampleToSystem sample w)
        match branch with
        | BranchTransmitted -> solver.muellerMatrixT ()
        | BranchReflected -> solver.muellerMatrixR ()

    /// The sample's Mueller matrix for a branch with the sample SPUN by `spin` about its own normal (spec
    /// 0046 — the sample's R1): every layer's tensors are turned about the sample normal (the engine's z)
    /// before the solve, so an anisotropic sample's axes move against the fixed plane of incidence; an
    /// isotropic one is unchanged.
    let sampleMuellerSpun (branch : Branch) (sample : ResolvedSample) (w : WaveLength) (inc : IncidenceAngle) (spin : Angle) : MuellerMatrix =
        let info = { (IncidentLightInfo.createInclined w inc) with refractionIndex = RefractionIndex.vacuum }
        let system = sampleToSystem sample w
        let spun =
            if spin.value = 0.0 then system
            else
                let turn (l : Layer) : Layer = l.rotate (Rotation.rotateZ spin)
                { system with
                    films = system.films |> List.map turn
                    substrate =
                        system.substrate
                        |> Option.map (fun s ->
                            match s with
                            | Substrate.Plate l -> Substrate.Plate (turn l)
                            | other -> other) }
        let solver = OpticalSystemSolver(info, spun)
        match branch with
        | BranchTransmitted ->
            // The engine builds its Mueller matrices from field AMPLITUDES (|E|²), while its T is the energy
            // FLUX; the two differ by the exit medium's n·cosθ factor whenever that medium is not vacuum (a
            // film on glass, a substrate) — and, in an ABSORBING exit medium at oblique incidence, differently
            // for s and p. So the matrix is corrected by an s/p diattenuator D on its input side whose two
            // transmittances are the engine's own flux-to-amplitude ratios for its s and p inputs (whose
            // Stokes vectors are [1, ±1, 0, 0]): S₀ then reads exactly the engine's T for either input.
            let mm = solver.muellerMatrixT ()
            let flux (s : OpticalSystemSolver) : float = (s.solution.func OpticalFunction.T |> Option.defaultValue OpticalFunctionValue.zero).value
            let ratio (fluxValue : float) (amplitude : float) : float = if amplitude > 1.0e-300 then fluxValue / amplitude else 1.0
            let m00 = mm.element 0 0
            let m01 = mm.element 0 1
            let fs = ratio (flux (solver.solutionS ())) (m00 + m01)
            let fp = ratio (flux (solver.solutionP ())) (m00 - m01)
            let mean = (fs + fp) / 2.0
            let half = (fs - fp) / 2.0
            let cross = sqrt (max 0.0 (fs * fp))
            let diattenuator =
                [ [ mean; half; 0.0; 0.0 ]
                  [ half; mean; 0.0; 0.0 ]
                  [ 0.0; 0.0; cross; 0.0 ]
                  [ 0.0; 0.0; 0.0; cross ] ]
                |> RealMatrix4x4.create
                |> MuellerMatrix
            mm * diattenuator
        | BranchReflected -> solver.muellerMatrixR ()

    /// The sample's transmitted-branch Mueller matrix from the EXISTING engine (no new physics). Solves the
    /// mapped `OpticalSystem` at the given wavelength / incidence angle in vacuum.
    let sampleMuellerT (sample : ResolvedSample) (w : WaveLength) (inc : IncidenceAngle) : MuellerMatrix =
        sampleMueller BranchTransmitted sample w inc

    /// The sample's reflected-branch Mueller matrix from the EXISTING engine (spec 028 — the reflected
    /// capture, the counterpart of `sampleMuellerT`).
    let sampleMuellerR (sample : ResolvedSample) (w : WaveLength) (inc : IncidenceAngle) : MuellerMatrix =
        sampleMueller BranchReflected sample w inc

    /// The full SV propagation: SV_out = MM_sample · SV_in, then SV_det = MM_analyzer · SV_out (spec §1/§4).
    let propagate (svIn : StokesVector) (mmSample : MuellerMatrix) (mmAnalyzer : MuellerMatrix) : StokesVector =
        mmAnalyzer * (mmSample * svIn)

    /// One point of a computed curve: the swept value `x` (in its display unit — degrees for an angle, nm for a
    /// wavelength) and what was read there, `y` (an intensity, or Ψ / Δ in degrees).
    [<Softellect.Sys.NumericCarrier("an element of a computed sweep curve (R3, a sweep array): the swept x and the read y of every point a sweep solved or a run exported")>]
    type CurvePoint =
        {
            x : float
            y : float
        }

    /// One end of a swept range, in the swept quantity's display unit: degrees for an angle, nm for a wavelength.
    type SweptValue =
        | SweptValue of float

        member this.value = let (SweptValue v) = this in v

        /// The value as a magnitude in its display unit (nm for a wavelength sweep).
        member this.toUnitMagnitude : Units.UnitMagnitude = Units.UnitMagnitude this.value

    /// How many points a sweep samples its range at, both ends included (fewer than two are read as two).
    type CurvePointCount =
        | CurvePointCount of int

        member this.value = let (CurvePointCount n) = this in n

    /// The rotating-analyzer experiment result: the (angle in degrees, intensity) points over the analyzer's R1
    /// across the full circle (the `RotateR1FullCircle` experiment, spec §2b).
    type IntensityCurve =
        {
            points : CurvePoint list
        }

    /// An ellipsometer sweep's two parallel curves: Ψ and Δ, in degrees, over the same swept values.
    type PsiDeltaCurves =
        {
            psiCurve : CurvePoint list
            deltaCurve : CurvePoint list
        }

    /// Sweep the analyzer's R1 across an explicit angle range [low, high] in degrees (inclusive) and record S0
    /// at each angle (spec 028 — the varied R1 range is user-editable). `rotatingAnalyzerCurve` is the
    /// full-circle 0…360° special case.
    let rotatingAnalyzerCurveRange
        (svIn : StokesVector)
        (mmSample : MuellerMatrix)
        (analyzerKind : PolarizerKind)
        (low : SweptValue)
        (high : SweptValue)
        (count : CurvePointCount) : IntensityCurve =
        let n = max 2 count.value
        let points =
            [
                for k in 0 .. n - 1 ->
                    let deg = low.value + (high.value - low.value) * float k / float (n - 1)
                    let mm = analyzerMueller analyzerKind (Angle.degree (deg * 1.0<deg>))
                    { x = deg; y = (propagate svIn mmSample mm).s0 }
            ]
        { points = points }

    /// Sweep the analyzer's R1 across 0…360° (inclusive) and record S0 at each angle. With an identity
    /// sample, an ideal-LP input, and an ideal-LP analyzer this traces Malus' law I = I₀ cos²θ.
    let rotatingAnalyzerCurve
        (svIn : StokesVector)
        (mmSample : MuellerMatrix)
        (analyzerKind : PolarizerKind)
        (count : CurvePointCount) : IntensityCurve =
        rotatingAnalyzerCurveRange svIn mmSample analyzerKind (SweptValue 0.0) (SweptValue 360.0) count

    /// Intensity through an OPTIONAL analyzer: with an analyzer, S0 of MM_analyzer·(MM_sample·SV_in); without
    /// one, S0 of MM_sample·SV_in (no analyzer present — nothing synthesized; spec R1).
    let intensityThroughAnalyzerOpt
        (svIn : StokesVector)
        (mmSample : MuellerMatrix)
        (analyzer : (PolarizerKind * Angle) option) : DetectorIntensity =
        match analyzer with
        | Some (kind, theta) -> (propagate svIn mmSample (analyzerMueller kind theta)).intensity
        | None -> (mmSample * svIn).intensity

    /// The top of the incidence (R2) sweep, in degrees — the last computed point is at 89° (90° is not
    /// computable; the chart axis is drawn to 90 by the UI layer, spec 026).
    let r2SweepMaxDegrees : SweptValue = SweptValue 89.0

    /// One incidence (R2) sweep over [low, high] in degrees (spec 028 — the range is user-editable, and the
    /// branch selects the transmitted or reflected sample Mueller matrix): at each incidence re-solve the sample
    /// Mueller matrix and read the intensity through the optional analyzer. The angle in DEGREES is the x-value;
    /// the caller clamps the top below 90° (90° itself is not computable, spec 026).
    let r2SweepCurve
        (branch : Branch)
        (svIn : StokesVector)
        (sample : ResolvedSample)
        (w : WaveLength)
        (analyzer : (PolarizerKind * Angle) option)
        (low : SweptValue)
        (high : SweptValue)
        (count : CurvePointCount) : CurvePoint list =
        let n = max 2 count.value
        let lo = min low.value high.value
        let hi = max low.value high.value
        [
            for k in 0 .. n - 1 ->
                let deg = lo + (hi - lo) * float k / float (n - 1)
                let inc = IncidenceAngle.create (Angle.degree (deg * 1.0<deg>))
                let mm = sampleMueller branch sample w inc
                { x = deg; y = (intensityThroughAnalyzerOpt svIn mm analyzer).value }
        ]

    // -----------------------------------------------------------------------------------------------------
    // Spec 0027 (026) — the ellipsometer and wavelength sweep builders. Each re-solves the sample at the swept
    // value and returns two parallel Ψ/Δ curves (in DEGREES, read by `StokesVector.psiDelta`) or, for the
    // intensity wavelength branch, one (wNm, intensity) curve.
    // -----------------------------------------------------------------------------------------------------

    /// One incidence (R2) sweep for an ELLIPSOMETER over [low, high] in degrees (spec 028 — user range + branch):
    /// Ψ and Δ (in DEGREES) of the sample output at each incidence, as two parallel curves.
    let r2SweepPsiDelta
        (branch : Branch)
        (svIn : StokesVector)
        (sample : ResolvedSample)
        (w : WaveLength)
        (low : SweptValue)
        (high : SweptValue)
        (count : CurvePointCount) : PsiDeltaCurves =
        let n = max 2 count.value
        let lo = min low.value high.value
        let hi = max low.value high.value
        let psi, delta =
            [
                for k in 0 .. n - 1 ->
                    let deg = lo + (hi - lo) * float k / float (n - 1)
                    let inc = IncidenceAngle.create (Angle.degree (deg * 1.0<deg>))
                    let pd = (sampleMueller branch sample w inc * svIn).psiDelta
                    { x = deg; y = pd.psi.degrees }, { x = deg; y = pd.delta.degrees }
            ]
            |> List.unzip
        { psiCurve = psi; deltaCurve = delta }

    /// One wavelength sweep over [low, high] in nm (x in NM; spec 028 — branch selects T / R): re-solve the
    /// sample Mueller matrix at each wavelength and read the intensity through the optional analyzer.
    let waveLengthSweepIntensity
        (branch : Branch)
        (svIn : StokesVector)
        (sample : ResolvedSample)
        (inc : IncidenceAngle)
        (analyzer : (PolarizerKind * Angle) option)
        (low : SweptValue)
        (high : SweptValue)
        (count : CurvePointCount) : CurvePoint list =
        let n = max 2 count.value
        let lo = min low.value high.value
        let hi = max low.value high.value
        [
            for k in 0 .. n - 1 ->
                let wNm = lo + (hi - lo) * float k / float (n - 1)
                let w = WaveLength.nm (wNm * 1.0<nm>)
                let mm = sampleMueller branch sample w inc
                { x = wNm; y = (intensityThroughAnalyzerOpt svIn mm analyzer).value }
        ]

    /// One wavelength sweep for an ELLIPSOMETER over [low, high] in nm (x in NM; spec 028 — branch selects
    /// T / R): Ψ and Δ (in DEGREES) of the sample output, returned as two parallel curves.
    let waveLengthSweepPsiDelta
        (branch : Branch)
        (svIn : StokesVector)
        (sample : ResolvedSample)
        (inc : IncidenceAngle)
        (low : SweptValue)
        (high : SweptValue)
        (count : CurvePointCount) : PsiDeltaCurves =
        let n = max 2 count.value
        let lo = min low.value high.value
        let hi = max low.value high.value
        let psi, delta =
            [
                for k in 0 .. n - 1 ->
                    let wNm = lo + (hi - lo) * float k / float (n - 1)
                    let w = WaveLength.nm (wNm * 1.0<nm>)
                    let pd = (sampleMueller branch sample w inc * svIn).psiDelta
                    { x = wNm; y = pd.psi.degrees }, { x = wNm; y = pd.delta.degrees }
            ]
            |> List.unzip
        { psiCurve = psi; deltaCurve = delta }
