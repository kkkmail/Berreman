namespace BerremanTests

open Berreman.Constants                                // the nm / mkm / mm units of measure
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.Solvers
open OpticalProperties.Active
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization
open Xunit
open BerremanTests.MatrixComparison

/// Spec 0044, manual task 007 — the inverse problem run on REAL measured data, not on a synthetic round
/// trip. The sample is a langasite (La3Ga5SiO14) crystal plate, 3 mm thick, z-cut (cut in the direction
/// of the optical axis), transparent at the measurement wavelength of 633 nm. The polarizer is fixed at
/// 90 degrees and the analyzer is stepped through six angles; the recorded quantity is the transmitted
/// intensity in ARBITRARY units:
///
///     analyzer angle (deg, "90-X" = polarizer 90, analyzer X)     0          90         30         120        60         150
///     transmitted intensity (arb. units)                          31.78297806 28.94511795 6.12864399 56.37913895 3.643184185 56.16106415
///
/// WHAT THIS MEASUREMENT CAN AND CANNOT DETERMINE — the point of the task is to fit only what the data
/// actually constrains, and to assert the rest is structurally or practically invisible:
///
///   * g11 (the transverse gyration component) is what the measurement is ABOUT. Propagating along the
///     optic axis, the plate is a pure optical rotator, and the six intensities trace out a Malus-law
///     sinusoid whose PHASE is the rotation. This is the one parameter the fit recovers.
///
///   * n_e and g33 are STRUCTURALLY invisible: at normal incidence on a z-cut plate the wave sees only
///     the transverse tensor components, and the crystal 3-axis is parallel to the propagation direction.
///     Both are fixed (n_e at the library Sellmeier value, g33 at the library dispersive value) and the
///     invisibility is asserted, not assumed.
///
///   * Absorption is zero by premise (the crystal is stated to be transparent), so there is nothing to
///     fit there.
///
///   * n_o enters only through the Fresnel / multiple-reflection channel: with numberOfReflections = 3
///     the plate sums emerging beams whose relative weights depend on the surface reflectance, hence on
///     n_o. That channel is real but measured to be eight orders of magnitude weaker than the g11
///     channel per comparably scaled parameter step (asserted below through the residual Jacobian),
///     while the data visibly carries noise at the percent level — the
///     three crossed-analyzer pairs (0,90), (30,120), (60,150), which any pure rotator forces to equal
///     sums, measure 60.728 / 62.508 / 59.804, a ~2.5 % spread. And the arbitrary absolute intensity
///     scale destroys most of what little absolute-level information the Fresnel channel would carry.
///     n_o is therefore FIXED at the library Sellmeier value, and the observability fact below quantifies
///     why fitting it would be fitting noise.
///
/// A CONVENTION WARNING (the short version of the long one in MuellerInverse.fs's module header, which
/// any reader of the recovered g11 must have). The engine's Rho is the BI-ANISOTROPIC (Tellegen-Post)
/// magnetoelectric tensor of D = eps E + rho H, NOT the crystallographic gyration tensor g of
/// D = eps E + i (G x E). For propagation along z the engine's behaviour, measured by MuellerInverseTests,
/// is exactly
///
///     specific rotation = 2 pi g11 / lambda      (driven by the TRANSVERSE Rho component g11)
///
/// independent of g33 and of the refractive index. The crystallographic convention would instead give
/// pi g33 / (n lambda). The g11 recovered below is in the ENGINE's convention; converting to a
/// literature-style crystallographic gyration is a different number answering a different question.
///
/// THE ARBITRARY INTENSITY SCALE is eliminated by dividing both the model and the measured intensity
/// vectors by their own sums before differencing (see normalizedResidual). The alternative — estimating
/// the scale analytically as (sum of measured)/(sum of model) at each residual evaluation — minimizes
/// the same functional to leading order but leaves the residual carrying an absolute-level component
/// that the data cannot support; normalizing both sides makes the residual exactly invariant under any
/// common multiplicative factor (source brightness, detector gain, exposure), which is the honest
/// statement of "arbitrary units".
///
/// This is REAL data: there is no ground truth to round-trip against. The recovered constants ARE the
/// result, and per the spec §9 protocol they were harvested from a deliberate fail-first run (the
/// placeholder expectations below forced assertion failures whose messages print the fitted values to
/// full precision) and then plugged back in. The facts assert internal consistency — the fit converged,
/// the recovered parameters reproduce the measurements within the noise the data itself carries, and the
/// observability structure is what the physics says it must be — never agreement with literature
/// gyration values.
type LangasiteInverseTests() =

    /// Radians per degree, reached by its full module path (mirrors `MuellerInverseTests`). Bound here,
    /// ahead of the members (FS0960: in a class type every `let` binding precedes the first member).
    let degree = Berreman.MathNetNumericsMath.degree

    // =================================================================================================
    // The sample, the wavelength, and the FIXED material constants.
    //
    // The refractive indices come from the library's own langasite Sellmeier formulas
    // (OpticalProperties/Dispersive.fs, type Langasite) evaluated at 633 nm — extracted once and
    // hard-coded here with provenance, exactly as MuellerInverseTests hard-codes the quartz constants,
    // so the test does not depend on the dispersive-evaluation plumbing. Note that the doc comments on
    // the two Sellmeier functions there are SWAPPED (the function named `refrIndexLa3Ga5SiO14Ordinary`
    // carries the comment "extraordinary" and vice versa); the values below are assigned by physics —
    // langasite is a positive uniaxial crystal, n_e > n_o — and match the published values
    // n_o ~= 1.899, n_e ~= 1.911 at 633 nm.
    // =================================================================================================

    /// He-Ne red line, the wavelength of the measurement.
    let waveLength = WaveLength.nm 633.0<nm>

    /// Library langasite ordinary Sellmeier index at 633 nm (`refrIndexLa3Ga5SiO14Ordinary`).
    /// FIXED, not fitted: see the class header for why this channel is unobservable against the noise.
    let langasiteOrdinaryIndex = RefractionIndex 1.8994047902564992

    /// Library langasite extraordinary Sellmeier index at 633 nm (`refrIndexLa3Ga5SiO14ExtraOrdinary`).
    /// FIXED: structurally invisible at z-cut normal incidence (asserted below), so any reasonable value
    /// serves; the library value keeps the model physical.
    let langasiteExtraordinaryIndex = RefractionIndex 1.910804133515858

    /// FIXED g33, taken from the library's dispersive langasite formula (`g33La3Ga5SiO14`) at 633 nm.
    /// Structurally invisible at z-cut normal incidence — the crystal 3-axis is the propagation
    /// direction — so the fitted result is exactly independent of this value; the library number is used
    /// for definiteness only.
    let langasiteG33Fixed = RhoValue 2.025648031646854e-05

    /// The plate: 3 mm, z-cut, measured at normal incidence in transmission.
    let plateThickness = Thickness.mm 3.0<mm>

    /// Spec R4 physics choice, mirrored from MuellerInverseTests: the plate solver sums the Mueller
    /// matrices of the emerging beams (direct plus internally reflected), which is what a real plate
    /// with uncoated faces produces. Stated explicitly at the composition root, not defaulted.
    let solverParameters : SolverParameters = { numberOfReflections = 3 }

    /// The single measurement configuration: z-cut, normal incidence, transmission. Six analyzer angles
    /// are six READINGS of this one configuration, not six configurations.
    let zCutConfiguration : MeasurementConfiguration =
        {
            cut = ZCut
            thickness = plateThickness
            incidenceAngle = IncidenceAngle.normal
            azimuth = SampleAzimuth.degree 0.0
            observable = TransmittedMueller
            waveLength = waveLength
        }

    /// Build the engine's optical properties for a uniaxial gyrotropic (class 3/32/4/422/6/622) crystal
    /// from the four material parameters. Langasite is point group 32, like quartz, so the same
    /// `type_3_4_6_Crystal` builder applies: diagonal eps = diag(n_o^2, n_o^2, n_e^2), diagonal
    /// Rho = diag(g11, g11, g33).
    let buildLangasite (p : UniaxialParameters) : OpticalProperties =
        OpticalProperties.type_3_4_6_Crystal
            (EpsValue.fromRefractionIndex p.ordinaryIndex)
            (EpsValue.fromRefractionIndex p.extraordinaryIndex)
            p.g11
            p.g33

    /// The real Berreman-backed forward model, wired once for the whole class.
    let forward = createBerremanForward buildLangasite solverParameters

    // =================================================================================================
    // The measured data, in the order of the task's table.
    // =================================================================================================

    /// The analyzer angles the six intensities were recorded at (the polarizer stayed at 90 degrees).
    let analyzerAnglesDeg = [| 0.0; 90.0; 30.0; 120.0; 60.0; 150.0 |]

    /// The measured transmitted intensities, arbitrary units, aligned with `analyzerAnglesDeg`.
    let measuredIntensities =
        [| 31.78297806; 28.94511795; 6.12864399; 56.37913895; 3.643184185; 56.16106415 |]

    /// The polarizer's fixed angle.
    let polarizerAngleDeg = 90.0

    // =================================================================================================
    // The Stokes/Mueller plumbing: polarizer -> sample -> analyzer -> S0.
    //
    // The polarizer and analyzer are the SAME ideal-linear-polarizer Mueller matrix (the engine's own
    // `Propagation.analyzerMueller IdealLinear`, the standard
    // 1/2 * [[1,c,s,0],[c,c^2,cs,0],[s,cs,s^2,0],[0,0,0,0]] with c = cos 2t, s = sin 2t) applied to
    // unpolarized unit-intensity light. The detector records S0 of what emerges from the analyzer.
    // Nothing here is hand-rolled: the conventions are the Domain module's, and the Malus-law fact below
    // pins them numerically before any fit trusts them.
    // =================================================================================================

    /// The transmitted intensity the detector records for one analyzer angle.
    let transmittedIntensity (sample : MuellerMatrix) (analyzerAngleDeg : float) : float =
        let polarizer = Propagation.analyzerMueller Library.IdealLinear (Angle.degree polarizerAngleDeg)
        let analyzer = Propagation.analyzerMueller Library.IdealLinear (Angle.degree analyzerAngleDeg)
        analyzer * (sample * (polarizer * Propagation.unpolarizedStokes)) |> Propagation.s0

    /// The model's six intensities for a material parameter set — one forward solve, six analyzer angles.
    let modelIntensities (p : UniaxialParameters) : Result<float[], ForwardModelError> =
        forward.muellerOf zCutConfiguration p
        |> Result.map (fun sample -> [| for a in analyzerAnglesDeg -> transmittedIntensity sample a |])

    /// The residual the fit minimizes: model-minus-measured with the arbitrary absolute scale eliminated
    /// from BOTH sides by dividing each intensity vector by its own sum (see the class header for why
    /// this normalization and not an analytically estimated scale factor).
    ///
    /// The closure must be TOTAL and of fixed length, because the optimizer explores freely: a forward
    /// failure returns a large finite penalty of the correct length rather than throwing (which would
    /// abort the fit) or returning a short vector (which would silently change the problem).
    let normalizedResidual (p : UniaxialParameters) : float[] =
        match modelIntensities p with
        | Error _ -> Array.create analyzerAnglesDeg.Length 1.0e3
        | Ok model ->
            let modelSum = Array.sum model
            let measuredSum = Array.sum measuredIntensities
            Array.map2 (fun m d -> m / modelSum - d / measuredSum) model measuredIntensities

    // =================================================================================================
    // The start guess for g11: a closed-form estimate from the data itself.
    //
    // For a pure rotator with polarizer at 90 degrees the transmitted intensity is
    // I(theta) ~ cos^2(90 + rho - theta) = a + b cos 2theta + c sin 2theta, a LINEAR model in
    // (1, cos 2theta, sin 2theta). Least-squares on the six readings gives
    //
    //     a = 30.5067, b = 1.3239, c = -29.6667
    //     amplitude A = 2 sqrt(b^2 + c^2) = 59.3924
    //     maximum at analyzer angle phi = atan2(c, b) / 2 = -43.72 deg == 136.28 deg (mod 180)
    //     offset a - A/2 = 0.810  (~1.4 % of A: the visible noise/background floor)
    //
    // so the plate rotates by |rho| = |phi - 90| = 46.2775 deg over 3 mm (15.43 deg/mm), and the engine's
    // law rho = 2 pi g11 d / lambda gives |g11| = rho lambda / (2 pi d) = 2.7124e-5. The SIGN is fixed by
    // the engine's rotation convention, which the convention fact below pins first: a positive g11 moves
    // the transmission maximum toward SMALLER analyzer angles (sample Mueller = rotationMueller(+psi)
    // rotates the Stokes azimuth by -psi), so the observed maximum at 136.28 deg = 90 + 46.28 calls for a
    // NEGATIVE g11. Should that convention reading ever flip, the fit still converges to an equivalent
    // minimum — the cos^2 intensity pattern is periodic in the rotation with period 180 deg, so rotations
    // rho and rho + 180k are indistinguishable and every g11 + k lambda/(2d) is the same physics.
    // =================================================================================================

    /// The closed-form start guess: -2.7124e-5, derived above.
    let g11Start = RhoValue -2.7123780914857123e-05

    /// The gyration scale: one unit of the scaled fit coordinate is 1e-5 of g11. ALGLIB's
    /// Levenberg-Marquardt differentiates with a FIXED ABSOLUTE step of 1e-6 (see ParameterScaling in
    /// MuellerInverse.fs), so the fit searches the dimensionless x in g11 = g11Start + x * g11Scale;
    /// `toScaled`/`ofScaled` are the fixed 4-vector version and this fit has one free parameter, hence
    /// this small local scaling instead.
    let g11Scale = 1.0e-5

    /// Material parameters for a scaled g11 coordinate: everything else fixed at the values above.
    let parametersOfScaledG11 (x : float) : UniaxialParameters =
        {
            ordinaryIndex = langasiteOrdinaryIndex
            extraordinaryIndex = langasiteExtraordinaryIndex
            g11 = g11Start.value + x * g11Scale |> RhoValue
            g33 = langasiteG33Fixed
        }

    /// Material parameters for a physical g11 value (used by the convention and invisibility facts).
    let parametersOfG11 (g11 : RhoValue) : UniaxialParameters =
        {
            ordinaryIndex = langasiteOrdinaryIndex
            extraordinaryIndex = langasiteExtraordinaryIndex
            g11 = g11
            g33 = langasiteG33Fixed
        }

    /// The engine sample Mueller matrix for a parameter set, failing loudly on a forward error (these
    /// facts drive the model at benign parameter values, so a failure here is a defect, not data).
    let sampleMueller (p : UniaxialParameters) : MuellerMatrix =
        match forward.muellerOf zCutConfiguration p with
        | Ok m -> m
        | Error e -> failwith $"the forward model failed: %A{e}"

    /// The one-parameter fit, run once for the whole class (lazy, so the convention and observability
    /// facts that do not need it do not pay for it, and the facts that do share one solution).
    let g11Fit =
        lazy
            (
            let solver = MuellerInverseSolver.createAlglibLevenbergMarquardt ()
            let request : NonlinearRequest =
                {
                    residual = fun (v : float[]) -> normalizedResidual (parametersOfScaledG11 v.[0])
                    initial = [| 0.0 |]
                    // +-50 scaled units = +-5e-4 in g11: brackets any physically sensible excursion while
                    // keeping the search far from any degenerate region.
                    lowerBounds = [| -50.0 |]
                    upperBounds = [| 50.0 |]
                    maxIterations = 400
                    epsX = 1.0e-12
                }
            match solver.solveNonlinearLeastSquares request with
            | Ok solution -> solution
            | Error e -> failwith $"the g11 fit failed: %A{e}"
            )

    /// The recovered g11 in physical units.
    let fittedG11 = g11Start.value + g11Fit.Value.solution.[0] * g11Scale

    /// The plate thickness in meters, for rotation-per-length reporting. Both lengths carry the engine's
    /// `meter` unit, so the ratio is dimensionless by construction.
    let platesPerWave =
        match plateThickness with
        | Thickness t -> t / waveLength.value
        | Infinity -> failwith "the plate must have a finite thickness"

    /// The optical rotation over the plate that a g11 produces under the engine's law, in degrees.
    let rotationDegOfG11 (g11 : float) : float =
        2.0 * System.Math.PI * g11 * platesPerWave / degree

    /// The CRYSTALLOGRAPHIC reading of the same gyration constant: pi * g / (n_o * lambda), in deg/mm.
    /// This is the other side of the convention warning in the class header (the full essay is in
    /// MuellerInverse.fs's module header): the engine reads the gyration as the bi-anisotropic Rho of
    /// D = eps E + rho H and rotates by 2 pi g11 / lambda, while the crystallographic gyration tensor of
    /// D = eps E + i (G x E) reads the same constant as pi g / (n_o lambda) — smaller by exactly 2 n_o.
    /// This reading, not the engine's, is the number comparable to literature rotatory powers.
    /// The wavelength's `meter` unit is dropped at the arithmetic seam, mirroring Dispersive.fs's own
    /// `w.value / 1.0<meter>` idiom.
    let crystallographicRotationDegPerMmOfG11 (g11 : float) : float =
        let lambdaMeters = waveLength.value / 1.0<meter>
        System.Math.PI * g11 / (lambdaMeters * langasiteOrdinaryIndex.value) / degree / 1000.0

    // =================================================================================================
    // SPEC §9 PROTOCOL — HARVESTED EXPECTATIONS.
    //
    // These constants were placeholders on the first run (0.0 and 1e-12 — deliberately wrong). The
    // assertion messages printed the fitted values to full precision (G17); the harvested numbers were
    // then plugged in here, and the tolerances pinned from the observed fit quality. The observed values
    // are recorded in the facts' comments below.
    // =================================================================================================

    /// The recovered g11 (the engine's Rho convention), harvested from the fail-first run:
    /// fitted g11 = -2.7123780914982785e-05, i.e. a rotation of -46.27754089760095 deg over the 3 mm
    /// plate (-15.425846965866983 deg/mm). The LM refinement moved the closed-form start guess by only
    /// 1.3e-13 (5e-9 relative): the linear closed-form estimate and the full nonlinear fit against the
    /// multiple-reflection Berreman model agree, which is itself a consistency result.
    let expectedG11 = RhoValue -2.7123780914982785e-05

    /// The crystallographic reading pi * g / (n_o * lambda) of the recovered g11, in deg/mm, harvested
    /// from the fail-first run: -4.0607055023231382 deg/mm (the engine's reading of the same constant is
    /// -15.425846965866983 deg/mm; the ratio is exactly 2 n_o = 3.7988095805129984, as the two formulas
    /// require). This is the literature-convention rotatory power; see
    /// crystallographicRotationDegPerMmOfG11 and the convention warning it references.
    let expectedCrystallographicRotationDegPerMm = -4.0607055023231382

    /// The acceptance band on the worst normalized-intensity residual at the solution. Observed on the
    /// fail-first run: 7.7519393316085039e-03 (see the reproduction fact for all six). Real noisy data
    /// cannot be reproduced to machine precision — the data's own crossed-pair sums disagree by ~2.5 % —
    /// so the band is pinned at ~2x the observed worst value, per the spec §9 protocol.
    let allowedWorstNormalizedResidual = 1.6e-2

    // =================================================================================================
    // Convention-pinning facts: the analyzer chain, and the engine's rotation direction.
    // =================================================================================================

    [<Fact>]
    member _.``with no sample the analyzer chain obeys the Malus law`` () =
        // The polarizer/analyzer/intensity plumbing is trusted with a fit only after its conventions are
        // pinned against a closed form. With an identity sample and a polarizer at 90 degrees the chain
        // must reproduce I(theta) = 1/2 cos^2(90 - theta): maximum at the parallel analyzer (90 deg),
        // extinction at the crossed one (0 deg).
        let sample = Propagation.identityMueller
        let intensityAt (a : float) = transmittedIntensity sample a
        let expectedAt (a : float) = 0.5 * (cos ((polarizerAngleDeg - a) * degree) ** 2.0)

        for a in [ 0.0; 30.0; 45.0; 60.0; 90.0; 120.0; 150.0 ] do
            let actual = intensityAt a
            let expected = expectedAt a
            Assert.True(
                abs (actual - expected) < allowedDiff,
                $"Malus law at analyzer {a} deg: expected {expected}, got {actual}")

        // And the two cardinal readings explicitly, since the whole fit hangs on them: parallel passes,
        // crossed extinguishes.
        Assert.True(intensityAt 90.0 > 0.49, $"parallel intensity = {intensityAt 90.0}, expected ~0.5")
        Assert.True(intensityAt 0.0 < 0.01, $"crossed intensity = {intensityAt 0.0}, expected ~0")

    [<Fact>]
    member _.``a positive g11 moves the transmission maximum toward smaller analyzer angles`` () =
        // The engine's rotation SIGN, pinned against the Stokes convention before the fit's sign means
        // anything. The engine's law gives rotation magnitude psi = 2 pi g11 d / lambda; a g11 chosen for
        // exactly 30 degrees over the 3 mm plate must move the transmission maximum 30 degrees away from
        // the 90-degree Malus peak. The two candidate peaks are 60 and 120 degrees, so comparing the two
        // intensities reads the sign directly.
        //
        // OBSERVED (and pinned): the engine's sample Mueller for g11 > 0 acts as
        // Propagation.rotationMueller(+psi), which rotates the Stokes azimuth by -psi, so the maximum
        // moves toward SMALLER analyzer angles and I(60) > I(120). This is the sign the start guess and
        // the fitted g11 rely on.
        let g11ForThirtyDegrees = RhoValue ((System.Math.PI / 6.0) / (2.0 * System.Math.PI * platesPerWave))
        let sample = sampleMueller (parametersOfG11 g11ForThirtyDegrees)
        let at60 = transmittedIntensity sample 60.0
        let at120 = transmittedIntensity sample 120.0

        Assert.True(at60 > at120, $"g11 > 0: I(60) = {at60}, I(120) = {at120}; the maximum must move toward 60 deg")

        // Flipping the sign of g11 must flip the direction of the shift.
        let flipped = sampleMueller (parametersOfG11 (RhoValue -g11ForThirtyDegrees.value))
        let flippedAt60 = transmittedIntensity flipped 60.0
        let flippedAt120 = transmittedIntensity flipped 120.0
        Assert.True(flippedAt120 > flippedAt60, $"g11 < 0: I(60) = {flippedAt60}, I(120) = {flippedAt120}; the maximum must move toward 120 deg")

    // =================================================================================================
    // Structural invisibility: what this measurement cannot see at all.
    // =================================================================================================

    [<Fact>]
    member _.``n_e and g33 are structurally invisible at z-cut normal incidence`` () =
        // The forward-model half of the observability claim (the Jacobian half is below). Propagating
        // along the optic axis, the wave sees only the transverse tensor components; the crystal 3-axis
        // is parallel to the propagation direction, so neither eps_33 (n_e) nor rho_33 (g33) has ANY
        // effect. Doubling n_e, zeroing g33, or flipping its sign must change none of the six model
        // intensities. This mirrors the C1 ablation in MuellerInverseTests, asserted at the intensity
        // level this task's data actually lives at.
        //
        // OBSERVED: the invisibility is exact down to the solver's own numerical floor, not to the last
        // bit — DOUBLING n_e moved the worst of the six intensities by 2.65e-12, and both g33 variants
        // by less than 1e-12, against intensities of order 0.1. That is eleven orders of magnitude below
        // the signal: round-off inside the eigensolver, not physics. The band is pinned at 1e-9, three
        // orders above the observed floor, so a genuine coupling (a tilted cut, a misrouted tensor) would
        // still fail loudly.
        let baseline = parametersOfG11 g11Start
        let variants =
            [ "n_e doubled", { baseline with extraordinaryIndex = RefractionIndex (2.0 * langasiteExtraordinaryIndex.value) }
              "g33 zeroed", { baseline with g33 = RhoValue 0.0 }
              "g33 sign-flipped", { baseline with g33 = RhoValue -langasiteG33Fixed.value } ]

        let baselineIntensities =
            match modelIntensities baseline with
            | Ok m -> m
            | Error e -> failwith $"the forward model failed: %A{e}"

        for (label, variant) in variants do
            match modelIntensities variant with
            | Ok varied ->
                let worst = Array.map2 (fun b v -> abs (b - v)) baselineIntensities varied |> Array.max
                Assert.True(worst < 1.0e-9, $"{label}: worst intensity change = {worst}, expected invisibility to the solver's numerical floor")
            | Error e -> Assert.Fail($"{label}: the forward model failed: %A{e}")

    // =================================================================================================
    // The fit on the real data, and what it recovered.
    // =================================================================================================

    [<Fact>]
    member _.``the langasite plate measurement determines g11`` () =
        // The deliverable: fit g11 (the only parameter this measurement determines) to the REAL data.
        //
        // OBSERVED (fail-first run, harvested into expectedG11 above): fitted g11 =
        // -2.7123780914982785e-05, a rotation of -46.27754089760095 deg over the 3 mm plate
        // (-15.425846965866983 deg/mm), reached in 2 LM iterations from the closed-form start. The
        // negative sign is physical in the engine's convention: the transmission maximum sits at
        // 136.28 deg = 90 + 46.28, and a positive g11 moves the maximum toward SMALLER analyzer angles
        // (the convention fact above), so this plate is the opposite enantiomorph's sign from positive
        // g11. The crystallographic reading of the same constant (harvested into
        // expectedCrystallographicRotationDegPerMm above) is -4.0607055023231382 deg/mm.
        //
        // The acceptance tolerance of 5e-10 absolute is ~2e-5 relative: the fit is a deterministic
        // optimizer run against a fixed model, so the same code recovers the same value to far better
        // than this; the band exists to catch a real regression (a changed forward model, a changed
        // optimizer), not to measure noise.
        let solution = g11Fit.Value
        Assert.True(solution.iterations > 0, "the fit should have taken at least one step")

        let rotationDeg = rotationDegOfG11 fittedG11
        let rotationDegPerMm = rotationDeg / 3.0

        // A physics sanity guard, independent of the plugged constant: the rotation modulo the 180-degree
        // cos^2 degeneracy must land near the closed-form estimate of +-46.3 deg over the plate
        // (~15.4 deg/mm). A fit that converged to a wildly different basin fails here even before the
        // recovered constant is compared.
        let rotationMod180 = rotationDeg - 180.0 * floor (rotationDeg / 180.0 + 0.5)
        Assert.True(
            abs rotationMod180 > 40.0 && abs rotationMod180 < 52.0,
            $"rotation mod 180 deg = {rotationMod180} over the plate ({rotationDegPerMm} deg/mm raw); expected +-46.3 deg mod 180")

        Assert.True(
            abs (fittedG11 - expectedG11.value) < 5.0e-10,
            $"recovered g11 = {fittedG11:G17} (expected {expectedG11.value:G17}); rotation = {rotationDeg} deg over 3 mm = {rotationDegPerMm} deg/mm; iterations = {solution.iterations}")

        // The crystallographic reading of the same recovered constant — pi * g / (n_o * lambda), the
        // literature-convention rotatory power (see crystallographicRotationDegPerMmOfG11 and the
        // convention warning it references). Asserted as its own plugged-in constant, harvested through
        // the same fail-first protocol as expectedG11; the tolerance is pinned at ~2.5e-5 relative, the
        // same looseness as the g11 band it derives from.
        let crystallographicDegPerMm = crystallographicRotationDegPerMmOfG11 fittedG11
        Assert.True(
            abs (crystallographicDegPerMm - expectedCrystallographicRotationDegPerMm) < 1.0e-4,
            $"crystallographic rotatory power pi*g11/(n_o*lambda) = {crystallographicDegPerMm:G17} deg/mm (expected {expectedCrystallographicRotationDegPerMm:G17}); engine reading = {rotationDegPerMm} deg/mm; ratio = {rotationDegPerMm / crystallographicDegPerMm} (must be 2 n_o = {2.0 * langasiteOrdinaryIndex.value})")

    [<Fact>]
    member _.``the recovered parameters reproduce all six measured intensities within the noise the data carries`` () =
        // The recovered constants must REPRODUCE THE MEASUREMENTS, not merely sit at a converged point.
        // With real, visibly noisy data the residuals are NOT machine-precision and are not expected to
        // be: the three crossed-analyzer pair sums disagree by ~2.5 % (60.728 / 62.508 / 59.804), so the
        // data itself carries a percent-level floor that no choice of the single free parameter g11 can
        // fit away. The closed-form Malus fit to the same data left an rms residual of 0.569 in the
        // data's arbitrary units (~1 % of the 59.4 amplitude); the engine model should land in the same
        // place, and small departures from a perfect sinusoid (the multiple-reflection beams) are all it
        // can add.
        //
        // OBSERVED (fail-first run, pinned into allowedWorstNormalizedResidual above at ~2x):
        //
        //     worst normalized residual = 7.7519393316085039e-03
        //     per-angle:  0 deg: +4.57234e-04   90 deg: +1.10133e-03   30 deg: -7.29494e-03
        //               120 deg: -8.69433e-04   60 deg: -1.14613e-03  150 deg: +7.75194e-03
        //
        // That is a sub-percent reproduction of real, visibly noisy data — at the level of the data's own
        // crossed-pair inconsistency, exactly as expected: the residual is the DATA's noise floor, not a
        // model defect. The sign pattern (the two pair members at 30/150 carrying the large opposite
        // residuals) is the pair-sum spread showing through.
        let solution = g11Fit.Value
        let residuals = normalizedResidual (parametersOfG11 (RhoValue fittedG11))
        let worst = residuals |> Array.map abs |> Array.max
        let report =
            System.String.Join(
                "; ",
                Array.map3 (fun a r m -> $"{a} deg: r = {r:G6}, measured = {m}") analyzerAnglesDeg residuals measuredIntensities)

        Assert.True(solution.finalResiduals.Length = analyzerAnglesDeg.Length, "the fit residual vector must have one entry per analyzer angle")
        Assert.True(
            worst < allowedWorstNormalizedResidual,
            $"worst normalized residual at the solution = {worst:G17} (allowed {allowedWorstNormalizedResidual}); per-angle: {report}")

    // =================================================================================================
    // Observability: why g11 alone is fitted, in numbers.
    // =================================================================================================

    [<Fact>]
    member _.``the residual Jacobian shows n_o is unobservable against the data's noise floor`` () =
        // The quantitative justification for fixing n_o. A two-parameter residual in (n_o, g11) — each
        // scaled to O(1) exactly as the 4-parameter MuellerInverse.ParameterScaling would scale them,
        // 1e-3 in index and 1e-5 in gyration per unit — has its Jacobian evaluated at the solution, and
        // the column norms measure how much the residual moves when each parameter moves.
        //
        // The scaling is centred so that neither coordinate of the evaluation point is zero: that detail
        // is load-bearing, because FitQuality.residualJacobian steps each parameter by
        // sqrt(eps) * max(|x_p|, 1e-12), which collapses to ~1e-20 at exactly x = 0 and would report
        // both columns as dead.
        //
        // OBSERVED (harvested from the fail-first run):
        //
        //     n_o column norm = 9.935623171437462e-10    (per 1e-3 of index)
        //     g11 column norm = 0.17192411098284327      (per 1e-5 of gyration)
        //     condition number = 2.2953598881612307e+08
        //
        // The reading: the n_o channel (the Fresnel / multiple-reflection weighting of the summed beams)
        // moves the normalized residual by ~1e-9 per 1e-3 of index, while the data's own noise floor is
        // 7.75e-3 in the same normalized units (the worst residual at the solution, above). Shifting the
        // residual by the noise floor through n_o alone would take an index change of ~7.8e+6 — seven
        // orders beyond anything physical — while the same floor corresponds to only ~4.5e-7 in g11
        // (~1.7 % of the recovered value). The n_o channel is real (its norm is not zero, and it sits
        // ~100x above the 1e-11 solver floor measured in the invisibility fact) but carries no usable
        // information: that is what "n_o is not determinable from this measurement set" means
        // quantitatively, and why g11 alone is fitted.
        let indexCentre = langasiteOrdinaryIndex.value - 1.0e-2
        let indexScale = 1.0e-3
        let g11CentreForJacobian = 0.0

        let residual2 (v : float[]) : float[] =
            let p : UniaxialParameters =
                {
                    ordinaryIndex = RefractionIndex (indexCentre + v.[0] * indexScale)
                    extraordinaryIndex = langasiteExtraordinaryIndex
                    g11 = RhoValue (g11CentreForJacobian + v.[1] * g11Scale)
                    g33 = langasiteG33Fixed
                }
            normalizedResidual p

        let atSolution =
            [| (langasiteOrdinaryIndex.value - indexCentre) / indexScale
               (fittedG11 - g11CentreForJacobian) / g11Scale |]
        Assert.True(atSolution |> Array.forall (fun x -> abs x > 1.0e-6), "the Jacobian must not be evaluated at the origin of the scaled space")

        let jacobian = FitQuality.residualJacobian residual2 atSolution
        let norms = jacobianColumnNorms jacobian
        let condition = jacobianCondition jacobian

        // g11 must be strongly constrained — this is the channel the measurement measures. Observed
        // 0.172; the band is pinned two orders below.
        Assert.True(norms.[1] > 1.0e-3, $"g11 column norm = {norms.[1]}; the g11 channel must be alive")

        // n_o must retain a non-zero sensitivity (the multiple-reflection channel is real) but be far
        // weaker than g11's. Observed norm 9.94e-10, observed ratio 5.8e-9; the bands are pinned well
        // above the solver floor and well below any useful observability, so a structural change in
        // either direction (a dead Fresnel channel, or a suddenly informative one) fails.
        Assert.True(norms.[0] > 1.0e-12, $"n_o column norm = {norms.[0]}; the Fresnel channel should be real, not exactly zero")
        Assert.True(
            norms.[0] < norms.[1] * 1.0e-6,
            $"n_o column norm = {norms.[0]}, g11 column norm = {norms.[1]}; n_o must be far more weakly constrained")

        // With one strong and one nearly-dead direction the two-parameter problem is ill conditioned.
        Assert.True(System.Double.IsFinite condition, $"the Jacobian condition number must be finite, got {condition}")
        Assert.True(
            condition > 1.0e6,
            $"Jacobian condition number = {condition}; observed 2.3e8, the large anisotropy of the two channels must show")
