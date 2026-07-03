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
open OpticalProperties.Standard
open OpticalProperties.Active
open OpticalProperties.Dispersive
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

    /// Resolve a Library `Sample`'s material id to engine `OpticalProperties`. Kept total and pure: the
    /// seeded materials map to their standard presets; anything else falls back to transparent glass.
    let private propertiesOf (materialId : string) : OpticalProperties =
        match materialId with
        | "glass-1.50" -> OpticalProperties.transparentGlass150
        | "glass-1.52" -> OpticalProperties.transparentGlass
        | "glass-1.75" -> OpticalProperties.transparentGlass175
        | "uniaxial-crystal" -> OpticalProperties.uniaxialCrystal
        | "biaxial-crystal" -> OpticalProperties.biaxialCrystal
        | _ -> OpticalProperties.transparentGlass

    /// The active (gyrotropic) planar-crystal properties of the seeded active-crystal sample (from
    /// `ActiveCrystal.fsx`): n₁₁ = 2.315, n₃₃ = 2.226, optical-activity ρ₁₂ = 1.5e-6.
    let private activeCrystalProperties : OpticalProperties =
        let e11 = RefractionIndex 2.315 |> EpsValue.fromRefractionIndex
        let e33 = RefractionIndex 2.226 |> EpsValue.fromRefractionIndex
        let g12 = RhoValue 1.5e-6
        OpticalProperties.planarCrystal e11 e33 g12

    /// The dispersive langasite-on-silicon system (from `LangasiteOnSilicon.fsx`): a langasite thin film
    /// over a silicon substrate, both wavelength-dependent. Evaluated at the run wavelength by `getSystem`.
    let private langasiteOnSiliconWithDisp (thickness : Thickness) : OpticalSystemWithDisp =
        {
            description = Some "Langasite thin film on silicon substrate."
            upperWithDisp = OpticalProperties.vacuum.dispersive
            filmsWithDisp = [ { propertiesWithDisp = langasiteOpticalProperties; thickness = thickness } ]
            substrateWithDisp = None
            lowerWithDisp = siliconOpticalProperties
        }

    /// Map a Library `Sample` to an engine `OpticalSystem`, evaluated at the run wavelength `w` (the
    /// wavelength only matters for the dispersive samples; the rest ignore it). Each seeded sample id keys
    /// a real engine system: the quarter-wave id builds a 41-layer glass/vacuum λ/4 stack, the EUV id a
    /// 100-pair Mo/Si stack, the crystal ids single anisotropic films, the active id a gyrotropic plate,
    /// the dispersive langasite id is evaluated via `getSystem`; everything else is the default thin-film /
    /// plate built from `propertiesOf`. (`Library.SubstrateKind` is qualified so it cannot collide with
    /// `Berreman.Media.Substrate`'s `Plate` / `Wedge` cases.)
    let sampleToSystem (sample : Sample) (w : WaveLength) : OpticalSystem =
        match sample.id with
        | "sample-multilayer-qw" ->
            // 41-layer λ/4 stack at 600 nm: 21 glass + 20 vacuum, ending on glass (alternation start/end).
            let thickness1 = Thickness.nm ((600.0 / 1.52 / 4.0) * oneNanometer)
            let thickness2 = Thickness.nm ((600.0 / 1.00 / 4.0) * oneNanometer)
            let pairs =
                [ for _ in 1 .. 20 ->
                    [ { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                      { properties = OpticalProperties.vacuum; thickness = thickness2 } ] ]
                |> List.concat
            {
                description = Some sample.name
                upper = OpticalProperties.vacuum
                films = pairs @ [ { properties = OpticalProperties.transparentGlass; thickness = thickness1 } ]
                substrate = None
                lower = OpticalProperties.vacuum
            }
        | "sample-euv-mosi" ->
            // 100 Mo/Si bilayers, each layer λ/4 at 10.6 nm = 2.65 nm (from MultilayerThinFilm_EUV.fsx).
            let thickness = Thickness.nm (10.6 / 4.0 * oneNanometer)
            let films =
                [ { properties = OpticalProperties.euvMolybdenum; thickness = thickness }
                  { properties = OpticalProperties.euvSilicon; thickness = thickness } ]
                |> List.replicate 100
                |> List.concat
            {
                description = Some sample.name
                upper = OpticalProperties.vacuum
                films = films
                substrate = None
                lower = OpticalProperties.vacuum
            }
        | "sample-active-crystal" ->
            {
                description = Some sample.name
                upper = OpticalProperties.vacuum
                films = []
                substrate = Some (Substrate.Plate { properties = activeCrystalProperties; thickness = sample.thickness })
                lower = OpticalProperties.vacuum
            }
        | "sample-langasite-silicon" ->
            // The langasite system has no wedge substrate, so the wedge-angle argument is unused.
            (langasiteOnSiliconWithDisp sample.thickness).getSystem w WedgeAngle.defaultValue
        | _ ->
            let props = propertiesOf sample.materialId
            match sample.substrate with
            | Library.ThinFilm ->
                {
                    description = Some sample.name
                    upper = OpticalProperties.vacuum
                    films = [ { properties = props; thickness = sample.thickness } ]
                    substrate = None
                    lower = OpticalProperties.vacuum
                }
            | Library.Plate
            | Library.Wedge ->
                {
                    description = Some sample.name
                    upper = OpticalProperties.vacuum
                    films = []
                    substrate = Some (Substrate.Plate { properties = props; thickness = sample.thickness })
                    lower = OpticalProperties.vacuum
                }

    /// The thickness in metres of a finite layer, or `None` for a semi-infinite (`Infinity`) half-space /
    /// plate (spec 0027 / 026 — the Details band view reads thicknesses without touching the engine's
    /// `Thickness` DU, which collides with `Avalonia.Thickness` in the UI layer).
    let thicknessMeters (t : Thickness) : float option =
        match t with
        | Thickness.Infinity -> None
        | Thickness.Thickness d -> Some (d / 1.0<meter>)

    /// The sample's transmitted-branch Mueller matrix from the EXISTING engine (no new physics). Solves the
    /// mapped `OpticalSystem` at the given wavelength / incidence angle in vacuum.
    let sampleMuellerT (sample : Sample) (w : WaveLength) (inc : IncidenceAngle) : MuellerMatrix =
        let info = { (IncidentLightInfo.createInclined w inc) with refractionIndex = RefractionIndex.vacuum }
        OpticalSystemSolver(info, sampleToSystem sample w).muellerMatrixT ()

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

    /// Sweep the analyzer's R1 across 0…360° (inclusive) and record S0 at each angle. With an identity
    /// sample, an ideal-LP input, and an ideal-LP analyzer this traces Malus' law I = I₀ cos²θ.
    let rotatingAnalyzerCurve
        (svIn : StokesVector)
        (mmSample : MuellerMatrix)
        (analyzerKind : PolarizerKind)
        (numPoints : int) : IntensityCurve =
        let n = max 2 numPoints
        let points =
            [
                for k in 0 .. n - 1 ->
                    let deg = 360.0 * float k / float (n - 1)
                    let mm = analyzerMueller analyzerKind (Angle.degree deg)
                    deg, intensity (propagate svIn mmSample mm)
            ]
        { points = points }

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

    /// One incidence (R2) sweep: at each incidence 0…89° (n points) re-solve the sample Mueller matrix and
    /// read the intensity through the optional analyzer. The angle in DEGREES is the x-value; the chart
    /// layer extends the x-axis to 90 (90° itself is not computed, spec 026).
    let r2SweepCurve
        (svIn : StokesVector)
        (sample : Sample)
        (w : WaveLength)
        (analyzer : (PolarizerKind * Angle) option)
        (numPoints : int) : (float * float) list =
        let n = max 2 numPoints
        [
            for k in 0 .. n - 1 ->
                let deg = r2SweepMaxDegrees * float k / float (n - 1)
                let inc = IncidenceAngle.create (Angle.degree deg)
                let mm = sampleMuellerT sample w inc
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

    /// One incidence (R2) sweep for an ELLIPSOMETER: Ψ and Δ (in DEGREES) of the sample output at each
    /// incidence 0…89°, returned as two parallel (angleDeg, value) curves.
    let r2SweepPsiDelta
        (svIn : StokesVector)
        (sample : Sample)
        (w : WaveLength)
        (numPoints : int) : (float * float) list * (float * float) list =
        let n = max 2 numPoints
        [
            for k in 0 .. n - 1 ->
                let deg = r2SweepMaxDegrees * float k / float (n - 1)
                let inc = IncidenceAngle.create (Angle.degree deg)
                let pd = ellipsometerReadout (sampleMuellerT sample w inc * svIn)
                (deg, pd.psi.degrees), (deg, pd.delta.degrees)
        ]
        |> List.unzip

    /// One wavelength sweep over [loNm, hiNm] (n points; x in NM): re-solve the sample Mueller matrix at
    /// each wavelength and read the intensity through the optional analyzer.
    let waveLengthSweepIntensity
        (svIn : StokesVector)
        (sample : Sample)
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
                let mm = sampleMuellerT sample w inc
                wNm, intensityThroughAnalyzerOpt svIn mm analyzer
        ]

    /// One wavelength sweep for an ELLIPSOMETER: Ψ and Δ (in DEGREES) of the sample output over [loNm, hiNm]
    /// (x in NM), returned as two parallel (wNm, value) curves.
    let waveLengthSweepPsiDelta
        (svIn : StokesVector)
        (sample : Sample)
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
                let pd = ellipsometerReadout (sampleMuellerT sample w inc * svIn)
                (wNm, pd.psi.degrees), (wNm, pd.delta.degrees)
        ]
        |> List.unzip
