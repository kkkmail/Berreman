namespace OpticalConstructor.Domain

open System.Numerics
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Fields
open Berreman.Media
open Berreman.Solvers
open Berreman.Constants
open Berreman.Dispersion
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library

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
/// Every primitive is elevated (`PsiDelta`, `IntensityCurve` are records, not bare tuples/floats);
/// `RealMatrix4x4` / `StokesVector` indexing is the single IO seam to the engine's backing types.
module Propagation =

    /// Build a `MuellerMatrix` from an explicit 4×4 of real rows (the same `RealMatrix` backing the engine
    /// uses). This is the one engine-API seam the ideal-polarizer matrices are written through.
    let muellerOfRows (rows : float list list) : MuellerMatrix =
        rows |> RealMatrix.create |> RealMatrix4x4 |> MuellerMatrix

    /// Read one element out of a `MuellerMatrix` (the read seam mirroring `muellerOfRows` — the IO
    /// seam to the engine's `RealMatrix4x4`; tests compare matrices through this).
    let muellerElement (mm : MuellerMatrix) (i : int) (j : int) : float =
        let (MuellerMatrix m) = mm
        m.[i, j]

    /// Read S0..S3 out of a `StokesVector` (the IO seam to the engine's `RealVector4`).
    let stokesComponents (sv : StokesVector) : float * float * float * float =
        let (StokesVector (RealVector4 rv)) = sv
        rv.[0], rv.[1], rv.[2], rv.[3]

    /// The intensity component S0 of a Stokes vector (what an intensity detector records).
    let s0 (sv : StokesVector) : float =
        let (a, _, _, _) = stokesComponents sv
        a

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
            muellerOfRows
                [ [ h; h * c; h * s; 0.0 ]
                  [ h * c; h * c * c; h * c * s; 0.0 ]
                  [ h * s; h * c * s; h * s * s; 0.0 ]
                  [ 0.0; 0.0; 0.0; 0.0 ] ]
        | IdealCircularLeft ->
            muellerOfRows
                [ [ h; 0.0; 0.0; h ]
                  [ 0.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 0.0; 0.0; 0.0 ]
                  [ h; 0.0; 0.0; h ] ]
        | IdealCircularRight ->
            muellerOfRows
                [ [ h; 0.0; 0.0; -h ]
                  [ 0.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 0.0; 0.0; 0.0 ]
                  [ -h; 0.0; 0.0; h ] ]

    /// The identity Mueller matrix — the "no sample present" pass-through (spec R1: an absent sample is
    /// skipped, not synthesized). With it the rotating-analyzer curve reduces to the Malus law.
    let identityMueller : MuellerMatrix =
        muellerOfRows
            [ [ 1.0; 0.0; 0.0; 0.0 ]
              [ 0.0; 1.0; 0.0; 0.0 ]
              [ 0.0; 0.0; 1.0; 0.0 ]
              [ 0.0; 0.0; 0.0; 1.0 ] ]

    // -----------------------------------------------------------------------------------------------------
    // Spec 0038 Part F (step 014) — constant-Mueller polarizer evaluation. `PolarizerBehavior` is DATA in
    // the Library domain; its evaluation lives ONLY here, in the Stokes/Mueller pipeline (a ConstantMueller
    // entry never enters the Berreman stack). `ComputedIdeal` keeps synthesizing through the EXISTING
    // `inputStokes` / `analyzerMueller` above, exactly as before.
    // -----------------------------------------------------------------------------------------------------

    /// The standard Stokes rotation matrix R(θ) (frame rotation by θ):
    ///   [[1,0,0,0],[0,cos2θ,sin2θ,0],[0,−sin2θ,cos2θ,0],[0,0,0,1]].
    /// The sign convention is pinned by `rotateMueller`: R(−θ)·LP₀·R(θ) must reproduce
    /// `analyzerMueller IdealLinear θ` exactly.
    let rotationMueller (theta : Angle) : MuellerMatrix =
        let c = cos (2.0 * theta.value)
        let s = sin (2.0 * theta.value)
        muellerOfRows
            [ [ 1.0; 0.0; 0.0; 0.0 ]
              [ 0.0; c; s; 0.0 ]
              [ 0.0; -s; c; 0.0 ]
              [ 0.0; 0.0; 0.0; 1.0 ] ]

    /// A polarizing element stored at reference orientation, physically rotated to θ (spec 0038 Part F):
    /// R(−θ)·M·R(θ).
    let rotateMueller (theta : Angle) (m : MuellerMatrix) : MuellerMatrix =
        rotationMueller (Angle (- theta.value)) * (m * rotationMueller theta)

    /// One compound component at its own fixed offset within the compound: the stored
    /// reference-orientation matrix rotated by `offset` — R(−offset)·M·R(offset).
    let componentMueller (c : MuellerComponent) : MuellerMatrix =
        rotateMueller c.offset c.matrix

    /// The compound's Mueller matrix: the ORDERED product of the offset-rotated components. The list is
    /// in light-traversal order (the FIRST component is the first surface light hits), so the product is
    /// Mₙ·…·M₂·M₁ and `compoundMueller components * sv` applies the first component first.
    let compoundMueller (components : MuellerComponent list) : MuellerMatrix =
        components |> List.fold (fun acc c -> componentMueller c * acc) identityMueller

    /// A polarizer behaviour's Mueller matrix at the element's live orientation `theta` (its R1):
    /// `ComputedIdeal` synthesizes through the EXISTING `analyzerMueller` exactly as today;
    /// `ConstantMueller` rotates the WHOLE compound by θ — algebraically identical to rotating each
    /// component by (θ + offset), since R(a)·R(b) = R(a+b).
    let behaviorMueller (behavior : PolarizerBehavior) (theta : Angle) : MuellerMatrix =
        match behavior with
        | ComputedIdeal kind -> analyzerMueller kind theta
        | ConstantMueller components -> rotateMueller theta (compoundMueller components)

    /// The input Stokes vector a polarizer behaviour produces at orientation `theta` (spec 0038 Part F):
    /// `ComputedIdeal` keeps the EXISTING unit-intensity `inputStokes` synthesis exactly as today;
    /// `ConstantMueller` applies the rotated compound to unpolarized natural light — un-normalized, so
    /// the compound's own throughput attenuates S0 (there is no analytic kind to normalize by).
    let behaviorInputStokes (behavior : PolarizerBehavior) (theta : Angle) : StokesVector =
        match behavior with
        | ComputedIdeal kind -> inputStokes kind theta
        | ConstantMueller _ -> behaviorMueller behavior theta * unpolarizedStokes

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
            name : string
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
            | None -> Error (UnknownMaterialId $"unknown material version '%s{string mvid.materialId.value}' v%d{mvid.version.value}"))

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
            description = Some sample.name
            upper = OpticalProperties.vacuum
            films = sample.films |> List.map (fun f -> f.getLayer w)
            substrate = sample.substrate |> Option.map (fun s -> Substrate.Plate (s.getLayer w))
            lower = sample.lower.getProperties w
        }

    /// The thickness in metres of a finite layer, or `None` for a semi-infinite (`Infinity`) half-space /
    /// plate (spec 0027 / 026 — the Details band view reads thicknesses without touching the engine's
    /// `Thickness` DU, which collides with `Avalonia.Thickness` in the UI layer).
    let thicknessMeters (t : Thickness) : float option =
        match t with
        | Thickness.Infinity -> None
        | Thickness.Thickness d -> Some (d / 1.0<meter>)

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

    /// The intensity an intensity detector records — the detector Stokes vector's S0 (spec §4).
    let intensity (svDet : StokesVector) : float = s0 svDet

    /// The rotating-analyzer experiment result: the (angleDeg, intensity) samples over the analyzer's R1
    /// across the full circle (the `RotateR1FullCircle` experiment, spec §2b).
    type IntensityCurve =
        {
            points : (float * float) list
        }

    /// Sweep the analyzer's R1 across an explicit angle range [loDeg, hiDeg] (inclusive) and record S0 at
    /// each angle (spec 028 — the varied R1 range is user-editable). `rotatingAnalyzerCurve` is the
    /// full-circle 0…360° special case.
    let rotatingAnalyzerCurveRange
        (svIn : StokesVector)
        (mmSample : MuellerMatrix)
        (analyzerKind : PolarizerKind)
        (loDeg : float)
        (hiDeg : float)
        (numPoints : int) : IntensityCurve =
        let n = max 2 numPoints
        let points =
            [
                for k in 0 .. n - 1 ->
                    let deg = loDeg + (hiDeg - loDeg) * float k / float (n - 1)
                    let mm = analyzerMueller analyzerKind (Angle.degree deg)
                    deg, intensity (propagate svIn mmSample mm)
            ]
        { points = points }

    /// Sweep the analyzer's R1 across 0…360° (inclusive) and record S0 at each angle. With an identity
    /// sample, an ideal-LP input, and an ideal-LP analyzer this traces Malus' law I = I₀ cos²θ.
    let rotatingAnalyzerCurve
        (svIn : StokesVector)
        (mmSample : MuellerMatrix)
        (analyzerKind : PolarizerKind)
        (numPoints : int) : IntensityCurve =
        rotatingAnalyzerCurveRange svIn mmSample analyzerKind 0.0 360.0 numPoints

    /// Intensity through an OPTIONAL analyzer: with an analyzer, S0 of MM_analyzer·(MM_sample·SV_in); without
    /// one, S0 of MM_sample·SV_in (no analyzer present — nothing synthesized; spec R1).
    let intensityThroughAnalyzerOpt
        (svIn : StokesVector)
        (mmSample : MuellerMatrix)
        (analyzer : (PolarizerKind * Angle) option) : float =
        match analyzer with
        | Some (kind, theta) -> intensity (propagate svIn mmSample (analyzerMueller kind theta))
        | None -> intensity (mmSample * svIn)

    /// The number of computed points in the incidence (R2) sweep — the last is at 89° (90° is not
    /// computable; the chart axis is drawn to 90 by the UI layer, spec 026).
    let r2SweepMaxDegrees : float = 89.0

    /// One incidence (R2) sweep over [loDeg, hiDeg] (n points; spec 028 — the range is user-editable, and
    /// the branch selects the transmitted or reflected sample Mueller matrix): at each incidence re-solve
    /// the sample Mueller matrix and read the intensity through the optional analyzer. The angle in DEGREES
    /// is the x-value; the caller clamps the top below 90° (90° itself is not computable, spec 026).
    let r2SweepCurve
        (branch : Branch)
        (svIn : StokesVector)
        (sample : ResolvedSample)
        (w : WaveLength)
        (analyzer : (PolarizerKind * Angle) option)
        (loDeg : float)
        (hiDeg : float)
        (numPoints : int) : (float * float) list =
        let n = max 2 numPoints
        let lo = min loDeg hiDeg
        let hi = max loDeg hiDeg
        [
            for k in 0 .. n - 1 ->
                let deg = lo + (hi - lo) * float k / float (n - 1)
                let inc = IncidenceAngle.create (Angle.degree deg)
                let mm = sampleMueller branch sample w inc
                deg, intensityThroughAnalyzerOpt svIn mm analyzer
        ]

    // -----------------------------------------------------------------------------------------------------
    // Phase 4 — Ellipsometer (Ψ / Δ readout). Scope: ONLY the ellipsometer detector readout.
    // -----------------------------------------------------------------------------------------------------

    /// The ellipsometric angles Ψ and Δ (the ellipsometer detector's reading, spec §4). Elevated to a
    /// record of `Angle`s — never a bare float pair.
    type PsiDelta =
        {
            psi : Angle
            delta : Angle
        }

    /// Ellipsometric Ψ, Δ from the normalized Stokes at the detector (spec §4). For the convention
    ///   S1 = −cos 2Ψ ,  S2 = sin 2Ψ cos Δ ,  S3 = −sin 2Ψ sin Δ
    /// the inverse is  2Ψ = atan2( √(S2²+S3²), −S1 ) ,  Δ = atan2( −S3, S2 ).
    let psiDeltaOfStokes (sv : StokesVector) : PsiDelta =
        let s0v, s1, s2, s3 = stokesComponents sv
        let n = if abs s0v > 1e-12 then s0v else 1.0
        let s1n, s2n, s3n = s1 / n, s2 / n, s3 / n
        let twoPsi = atan2 (sqrt (s2n * s2n + s3n * s3n)) (- s1n)
        let delta = atan2 (- s3n) s2n
        { psi = Angle (twoPsi / 2.0); delta = Angle delta }

    /// The ellipsometer detector readout for a propagated detector Stokes vector (spec §4).
    let ellipsometerReadout (svDet : StokesVector) : PsiDelta = psiDeltaOfStokes svDet

    // -----------------------------------------------------------------------------------------------------
    // Spec 0027 (026) — the ellipsometer and wavelength sweep builders (placed after `ellipsometerReadout`
    // so they can use it). Each re-solves the sample at the swept value and returns two parallel Ψ/Δ curves
    // (in DEGREES) or, for the intensity wavelength branch, one (wNm, intensity) curve.
    // -----------------------------------------------------------------------------------------------------

    /// One incidence (R2) sweep for an ELLIPSOMETER over [loDeg, hiDeg] (spec 028 — user range + branch):
    /// Ψ and Δ (in DEGREES) of the sample output at each incidence, as two parallel (angleDeg, value) curves.
    let r2SweepPsiDelta
        (branch : Branch)
        (svIn : StokesVector)
        (sample : ResolvedSample)
        (w : WaveLength)
        (loDeg : float)
        (hiDeg : float)
        (numPoints : int) : (float * float) list * (float * float) list =
        let n = max 2 numPoints
        let lo = min loDeg hiDeg
        let hi = max loDeg hiDeg
        [
            for k in 0 .. n - 1 ->
                let deg = lo + (hi - lo) * float k / float (n - 1)
                let inc = IncidenceAngle.create (Angle.degree deg)
                let pd = ellipsometerReadout (sampleMueller branch sample w inc * svIn)
                (deg, pd.psi.degrees), (deg, pd.delta.degrees)
        ]
        |> List.unzip

    /// One wavelength sweep over [loNm, hiNm] (n points; x in NM; spec 028 — branch selects T / R): re-solve
    /// the sample Mueller matrix at each wavelength and read the intensity through the optional analyzer.
    let waveLengthSweepIntensity
        (branch : Branch)
        (svIn : StokesVector)
        (sample : ResolvedSample)
        (inc : IncidenceAngle)
        (analyzer : (PolarizerKind * Angle) option)
        (loNm : float)
        (hiNm : float)
        (numPoints : int) : (float * float) list =
        let n = max 2 numPoints
        let lo = min loNm hiNm
        let hi = max loNm hiNm
        [
            for k in 0 .. n - 1 ->
                let wNm = lo + (hi - lo) * float k / float (n - 1)
                let w = WaveLength.nm (wNm * 1.0<nm>)
                let mm = sampleMueller branch sample w inc
                wNm, intensityThroughAnalyzerOpt svIn mm analyzer
        ]

    /// One wavelength sweep for an ELLIPSOMETER over [loNm, hiNm] (x in NM; spec 028 — branch selects T / R):
    /// Ψ and Δ (in DEGREES) of the sample output, returned as two parallel (wNm, value) curves.
    let waveLengthSweepPsiDelta
        (branch : Branch)
        (svIn : StokesVector)
        (sample : ResolvedSample)
        (inc : IncidenceAngle)
        (loNm : float)
        (hiNm : float)
        (numPoints : int) : (float * float) list * (float * float) list =
        let n = max 2 numPoints
        let lo = min loNm hiNm
        let hi = max loNm hiNm
        [
            for k in 0 .. n - 1 ->
                let wNm = lo + (hi - lo) * float k / float (n - 1)
                let w = WaveLength.nm (wNm * 1.0<nm>)
                let pd = ellipsometerReadout (sampleMueller branch sample w inc * svIn)
                (wNm, pd.psi.degrees), (wNm, pd.delta.degrees)
        ]
        |> List.unzip
