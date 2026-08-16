namespace BerremanTests

open Berreman.Constants                                // the nm / mkm / mm units of measure
open Berreman.MathNetNumericsMath                      // ComplexMatrix — the read seam into the engine's tensors
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
open BerremanTests.InverseFitHarness

/// Spec 0044, manual task 012 — the inverse problem for the HARDEST transparent crystal there is: a
/// biaxial, optically active, TRICLINIC one.
///
/// WHY TRICLINIC. `MuellerInverseTests` recovers four constants from a uniaxial crystal, where symmetry
/// does most of the work: two of the three principal indices are equal and the gyration tensor is
/// diagonal with only two distinct entries. Class 1 has no symmetry element at all beyond the identity,
/// so nothing constrains either tensor. ε contributes THREE independent principal refractive indices —
/// every triclinic crystal is biaxial — and the gyration tensor, being a symmetric second-rank tensor,
/// contributes all SIX of its independent components. NINE unknowns, and no two of them equal by
/// construction. That is the maximum a transparent, non-magnetic, non-dispersive crystal can present,
/// and every other optically active class is a constrained special case of it: monoclinic 2 zeroes g₁₂
/// and g₂₃, orthorhombic 222 zeroes all three off-diagonals, and the uniaxial classes additionally force
/// g₁₁ = g₂₂ and n₁ = n₂.
///
/// WHAT IS NEW BESIDES THE COUNT, and it is not just "more of the same":
///
///   * THREE principal cuts are needed, not two, and they buy the tensor DIAGONALS. A wave sees only the
///     components TRANSVERSE to its own propagation direction. For a uniaxial crystal the 1- and 2-axes
///     are equivalent, so a z-cut and an x-cut exhaust the distinct looks; here each of the three cuts
///     puts a different crystal axis along the normal and so contributes its own pair of principal
///     indices and its own pair of gyration diagonal components. Sample azimuth cannot substitute,
///     because rotating a plate about its own normal never changes which crystal axis lies along it.
///     `YCut` exists for exactly this reason.
///
///   * AND THE CUTS ARE NOT ENOUGH. All three of them, at normal incidence, leave every OFF-DIAGONAL
///     gyration component two orders of magnitude below the diagonal ones — including g₁₂, which is
///     transverse on a z-cut and might therefore be expected to show. It is OBLIQUE incidence that makes
///     them observable, because the engine's ρ couples through the wave's longitudinal field component,
///     which only exists off-normal. Cutting more plates does not substitute for tilting them, and vice
///     versa. This was measured, not predicted — the first draft of the ablation fact below guessed
///     wrongly — and it is now asserted in both directions.
///
///   * The OPTIC AXES are reachable, and are the only place the optical activity is seen cleanly. A
///     biaxial crystal has two directions along which the linear birefringence vanishes identically; a
///     wave down one of them is a pure circular retarder, which is the biaxial analogue of the uniaxial
///     C1 configuration. They sit at 36.70 deg from the 1-axis for this material, and Snell's law caps
///     the internal angle of a plate at arcsin(1/n) = 38.64 deg — so an x-cut plate CAN just reach one
///     and a z-cut plate (which would need 53.30 deg) never can. That is asserted below, and it is what
///     the steepest configurations in the measurement set are for.
///
/// THE GROUND TRUTH, and exactly how much of it is real. The three principal refractive indices are
/// lithium triborate's (LiB₃O₅, LBO) at 632.8 nm, evaluated from the standard Sellmeier set
///
///     n_x² = 2.4542 + 0.0113/(λ² − 0.0114) − 0.0139 λ²
///     n_y² = 2.5390 + 0.0128/(λ² − 0.0119) − 0.0185 λ²
///     n_z² = 2.5865 + 0.0131/(λ² − 0.0122) − 0.0186 λ²      (λ in µm)
///
/// and the gyration components are of the magnitude Shopa, Ftomyn & Shopa measured for that same crystal
/// by dual-wavelength polarimetry at 633 nm (g₁₂ = 4.31e-5, and an optical rotatory power of 7.06 deg/mm
/// along an optic axis, J. Appl. Cryst. 56, 2023).
///
/// LBO itself is class mm2 and NON-enantiomorphous: its symmetry allows exactly ONE gyration component,
/// the g₁₂ that was measured. No real crystal has a published, complete six-component gyration tensor —
/// the most complete measurements in the literature are the four-component monoclinic ones (Glazer's
/// group's `tilter` method on tartaric acid and related crystals) — so the remaining five components
/// here are SYNTHETIC: chosen of the same order as the measured one, with distinct magnitudes and mixed
/// signs so that no two of the nine unknowns are degenerate. This is stated loudly rather than glossed,
/// and it changes nothing about what the facts below establish: the forward data is generated FROM these
/// values, so they are the ground truth BY DEFINITION, and these facts measure the INVERSE MACHINERY,
/// not the physics — exactly as the quartz facts in `MuellerInverseTests` do, and for the same reason.
///
/// A CONVENTION WARNING, the short form of the long one in `MuellerInverse.fs`'s module header. The
/// engine's `Rho` is the BI-ANISOTROPIC (Tellegen–Post) magnetoelectric tensor of D = εE + ρH, NOT the
/// crystallographic gyration tensor g of D = εE + i(G × E). The two share the symbol g in the literature
/// and in `Active.Rho.type_1_Crystal`, but they are different parameterizations, and anyone quoting a
/// gyration value out of this pipeline must know which one they have.
///
/// MANUAL TASK 014 added the noisy half, at the end of the file. The measurement is no longer assumed
/// perfect: recorded rotation angles are wrong by up to 0.2°, the receiver misreads each normalized
/// Mueller element by up to 0.005 of full scale, and eight fixed seeds give eight complete experiments
/// whose spread IS the uncertainty. Those facts fit from a blind ISOTROPIC `n = 1.5`, `g = 0` start.
/// Their headline is a negative one, and worth knowing before reading them: the three indices survive at
/// 0.09 % and the three birefringences at 0.04-0.10 %, but the six gyration components come back with
/// error bars of the same order as their own values. The cause is structural — see the commentary in
/// that fact — and it is the price of the crystal being biaxial.
///
/// The fit driver, the residual, the scaling, the measurement-error model and the acceptance arithmetic
/// are all shared with `MuellerInverseTests` through `InverseFitHarness`; nothing in this file
/// re-implements any of it. That is only possible because `MuellerInverse` is generic in its parameter
/// set — see `ParameterAxis`.
type BiaxialInverseTests() =

    /// Radians per degree, reached by its full module path. Bound here, ahead of the members (FS0960: in
    /// a class type every `let` binding precedes the first member).
    let degree = Berreman.MathNetNumericsMath.degree

    // =================================================================================================
    // Ground truth. Spec R3 puts the material in the TEST rather than in the OpticalProperties library,
    // which has no biaxial gyrotropic material and should not grow one for a single test.
    // =================================================================================================

    /// He-Ne. The same line `MuellerInverseTests` uses, and the line the LBO optical-rotation measurement
    /// this material's gyration magnitudes are anchored on was made at.
    let waveLength = WaveLength.nm 632.8<nm>

    /// LBO's x-axis Sellmeier index at 0.6328 µm. The SMALLEST of the three.
    let index1 = RefractionIndex 1.574064836

    /// LBO's y-axis Sellmeier index at 0.6328 µm — the INTERMEDIATE index, and the one that decides
    /// where the optic axes lie.
    let index2 = RefractionIndex 1.601416913

    /// LBO's z-axis Sellmeier index at 0.6328 µm. The LARGEST of the three. The birefringences are
    /// therefore n₂ − n₁ = 0.027352, n₃ − n₂ = 0.014997 and n₃ − n₁ = 0.042349 — three different numbers,
    /// which is what "biaxial" means and what a uniaxial crystal cannot present.
    let index3 = RefractionIndex 1.616414016

    /// The gyration component Shopa et al. measured for LBO at 633 nm, used here as the ANCHOR that sets
    /// the scale of all six. Everything below it is synthetic (see the class header).
    let g11 = RhoValue 4.31e-5

    let g22 = RhoValue 6.90e-5
    let g33 = RhoValue -2.85e-5
    let g23 = RhoValue 1.72e-5
    let g13 = RhoValue -3.64e-5
    let g12 = RhoValue 2.46e-5

    /// The nine-parameter ground truth the forward data is generated from and the fit must recover.
    let triclinic : TriclinicParameters =
        {
            index1 = index1
            index2 = index2
            index3 = index3
            g11 = g11
            g22 = g22
            g33 = g33
            g23 = g23
            g13 = g13
            g12 = g12
        }

    /// Build the engine's optical properties for a triclinic (class 1) gyrotropic crystal from the nine
    /// unknowns. This is the ONE material-specific function in the whole pipeline, and the reason
    /// `MuellerInverse` takes it as a parameter rather than knowing it.
    ///
    /// It delegates entirely to the engine's own builders — `Eps.fromRefractionIndex` for the biaxial
    /// permittivity and `Active.Rho.type_1_Crystal` for the full symmetric gyration tensor — so neither
    /// diag(n₁², n₂², n₃²) nor the six-component ρ layout is re-derived here.
    let buildTriclinic (p : TriclinicParameters) : OpticalProperties =
        {
            eps = Eps.fromRefractionIndex (p.index1, p.index2, p.index3)
            mu = Mu.vacuum
            rho = Rho.type_1_Crystal p.g11 p.g22 p.g33 p.g23 p.g13 p.g12
        }

    /// Spec R4, mirrored from `MuellerInverseTests`: three internal reflections, stated explicitly at the
    /// composition root rather than inherited from `SolverParameters.defaultValue`, because
    /// `numberOfReflections` changes the physics being modelled rather than tuning it.
    let solverParameters : SolverParameters = { numberOfReflections = 3 }

    /// The real Berreman-backed forward model, wired once for the whole class.
    let forward = createBerremanForward buildTriclinic solverParameters

    // =================================================================================================
    // The measurement set.
    //
    // THICKNESS FIRST, because it is the design variable that decides whether the fit is possible at
    // all. Linear retardance enters the model through cos and sin of 2π Δn d / λ, so a plate carrying
    // many waves gives a residual surface that oscillates into a dense forest of local minima roughly
    // half a fringe apart, and a start guess more than about half a fringe away lands in the wrong one.
    // The largest birefringence here is n₃ − n₁ = 0.042349 — five times quartz's — so the plate has to be
    // FIVE TIMES THINNER than the 20 µm the uniaxial suite uses to stay in the same sub-wave regime:
    // 4 µm is 0.268 wave, against that suite's 0.286.
    //
    // Every configuration therefore uses the same 4 µm plate. Unlike the uniaxial case there is no
    // retardance-free principal cut to spend thickness on: a biaxial crystal has linear birefringence
    // along EVERY principal axis, and the only directions free of it are the two optic axes, which are
    // oblique.
    //
    // THE CUTS. Three principal cuts, because three different transverse planes are needed:
    //
    //   Z-cut, normal incidence: propagation along the crystal 3-axis. Transverse plane (1,2), so this
    //          cut sees n₁, n₂, g₁₁, g₂₂ and g₁₂.
    //   X-cut, normal incidence: propagation along the crystal 1-axis. Transverse plane (2,3): n₂, n₃,
    //          g₂₂, g₃₃, g₂₃.
    //   Y-cut, normal incidence: propagation along the crystal 2-axis. Transverse plane (1,3): n₁, n₃,
    //          g₁₁, g₃₃, g₁₃ — and it is the ONLY cut in which g₁₃ is transverse at normal incidence.
    //
    // Four azimuths per cut, because rotating the sample about its own normal changes which linear
    // combination of the transverse components the polarimeter projects onto, even though it cannot
    // change which components are transverse at all.
    //
    // THE OBLIQUE SWEEPS do three things at once: they supply the Fresnel/angle information that pins
    // the ABSOLUTE index level (which the retardances alone constrain only weakly), they tilt components
    // that are longitudinal at normal incidence into the transverse plane, and — at the steepest angles
    // on the x-cut — they approach an OPTIC AXIS, where the linear birefringence vanishes and what is
    // left is a nearly pure optical rotation. Reflection is included as well as transmission because it
    // carries index information that never traverses the sample.
    // =================================================================================================

    /// A 4 µm plate — 0.268 wave of linear retardance at the largest birefringence, the sub-wave regime
    /// the whole design depends on.
    let plate = Thickness.mkm 4.0<mkm>

    let configuration (cut : SampleCut) (incidenceDeg : double) (azimuthDeg : double) (observable : Observable) : MeasurementConfiguration =
        configurationOf cut plate incidenceDeg azimuthDeg observable waveLength

    /// The three principal cuts, in the order the transverse planes were listed above.
    let principalCuts : SampleCut list = [ ZCut; XCut; YCut ]

    /// B1 — three principal cuts, normal incidence, four azimuths, transmission. The backbone: each cut
    /// contributes its own transverse pair of indices and its own three gyration components.
    let b1 : MeasurementConfiguration list =
        [ for cut in principalCuts do
            for azimuth in [ 0.0; 22.5; 45.0; 67.5 ] ->
                configuration cut 0.0 azimuth TransmittedMueller ]

    /// B2 — three principal cuts, oblique sweep, two azimuths, transmission and reflection. Absolute
    /// index leverage, and the mixing that makes each cut's LONGITUDINAL components partially visible.
    let b2 : MeasurementConfiguration list =
        [ for cut in principalCuts do
            for incidence in [ 25.0; 55.0 ] do
                for azimuth in [ 0.0; 90.0 ] do
                    for observable in [ TransmittedMueller; ReflectedMueller ] ->
                        configuration cut incidence azimuth observable ]

    /// B3 — the optic-axis approach: an x-cut plate at 72 deg external incidence, which Snell's law turns
    /// into ~36.7 deg internally, the angle at which the linear birefringence of this material vanishes.
    /// Both azimuths are taken because the two optic axes sit on opposite sides of the 1-axis and a
    /// triclinic crystal has no symmetry making them equivalent.
    let b3 : MeasurementConfiguration list =
        [ for azimuth in [ 0.0; 180.0 ] do
            for observable in [ TransmittedMueller; ReflectedMueller ] ->
                configuration XCut 72.0 azimuth observable ]

    let fullConfigurations = b1 @ b2 @ b3

    /// Generate the synthetic "measured" data for a configuration set from the ground-truth parameters.
    let observe (configurations : MeasurementConfiguration list) : MuellerObservation list =
        observeWith forward triclinic configurations

    /// The dimensionless fit space, specialized to this material. Identical in spirit and in numbers to
    /// the uniaxial one — 1e-3 per unit of refractive index, 1e-5 per unit of gyration — because the two
    /// materials' parameters live at the same physical scales and ALGLIB's fixed 1e-6 differentiation
    /// step has to be a small perturbation of all nine at once.
    let scalingAround (centre : TriclinicParameters) : ParameterScaling<TriclinicParameters> =
        {
            axes = TriclinicParameters.axes
            centre = centre
            scale =
                {
                    index1 = RefractionIndex 1.0e-3
                    index2 = RefractionIndex 1.0e-3
                    index3 = RefractionIndex 1.0e-3
                    g11 = RhoValue 1.0e-5
                    g22 = RhoValue 1.0e-5
                    g33 = RhoValue 1.0e-5
                    g23 = RhoValue 1.0e-5
                    g13 = RhoValue 1.0e-5
                    g12 = RhoValue 1.0e-5
                }
        }

    /// The start guess the fit facts run from: every gyration component 25-35 % wrong in ALTERNATING
    /// directions, and the three indices off by +0.2 %, +0.1 % and +0.15 %.
    ///
    /// The perturbation is large where it can be and honest about where it cannot, exactly as in the
    /// uniaxial suite. A third of the way out on all six gyration components is far outside any
    /// experimental uncertainty, and alternating the direction stops the start from being a uniform
    /// rescaling of the truth, which a fit could exploit. The index perturbations look small in relative
    /// terms and are not: against birefringences of 0.015-0.042 they move n₂ − n₁ by −5.7 % and n₃ − n₂
    /// by +5.5 %, and birefringence, not absolute index, is what the retardance depends on. They cannot
    /// be larger for the structural reason the thickness commentary above gives — half a fringe is the
    /// basin, and at 0.268 wave that is roughly a factor of two in any one birefringence.
    let perturbedStart : TriclinicParameters =
        {
            index1 = RefractionIndex (index1.value * 1.0020)
            index2 = RefractionIndex (index2.value * 1.0010)
            index3 = RefractionIndex (index3.value * 1.0015)
            g11 = RhoValue (g11.value * 1.30)
            g22 = RhoValue (g22.value * 0.70)
            g33 = RhoValue (g33.value * 1.25)
            g23 = RhoValue (g23.value * 0.75)
            g13 = RhoValue (g13.value * 1.35)
            g12 = RhoValue (g12.value * 0.65)
        }

    /// Generous box bounds in SCALED units: ±50 is ±0.05 in refractive index and ±5e-4 in gyration, which
    /// brackets any physically sensible excursion from the start while keeping the search out of the
    /// degenerate region around n = 0. The truth sits within 4 scaled units of the start in every
    /// coordinate, so the box is not what is being tested.
    let box = SearchBox 50.0

    /// The optic-axis half-angle of a biaxial crystal, measured from the LARGEST-index axis and lying in
    /// the plane of the largest and smallest: tan²V = (n₂² − n₁²)/(n₃² − n₂²). Computed from the
    /// parameters rather than hard-coded, so the geometry fact below cannot drift away from the material.
    let opticAxisFromLargest (p : TriclinicParameters) : Angle =
        let sq (n : RefractionIndex) = n.value * n.value
        atan (sqrt ((sq p.index2 - sq p.index1) / (sq p.index3 - sq p.index2))) |> Angle

    /// Read the diagonal of the engine's ε as three plain numbers.
    let epsDiagonal (p : OpticalProperties) : float list =
        let (Eps (ComplexMatrix3x3 (ComplexMatrix e))) = p.eps
        [ e.[0, 0].Real; e.[1, 1].Real; e.[2, 2].Real ]

    /// Read the whole imaginary part of the engine's ρ as a 3×3 of plain numbers — the gyration tensor as
    /// the lab frame sees it. The FULL tensor and not just its diagonal, because a triclinic sample's
    /// off-diagonals are exactly what a wrong rotation would scramble.
    let rhoImaginary (p : OpticalProperties) : float list list =
        let (Rho (ComplexMatrix3x3 (ComplexMatrix r))) = p.rho
        [ for i in 0 .. 2 -> [ for j in 0 .. 2 -> r.[i, j].Imaginary ] ]

    // =================================================================================================
    // Manual task 014 — the NOISY facts.
    //
    // Everything above this line feeds the fit data the forward model produced exactly, at angles the
    // sample was exactly at. That measures the INVERSE MACHINERY on nine unknowns. What follows measures
    // the EXPERIMENT: what a decent-but-ordinary optical bench gets out of a triclinic crystal, and with
    // what error bar.
    //
    // The measurement-error model — what is perturbed, by how much, and where those magnitudes come
    // from — lives in `InverseFitHarness` and is pinned by `MuellerInverseTests`. It describes the
    // APPARATUS rather than the sample, so this suite measures its crystal on exactly the same bench,
    // with exactly the same eight seeds, which is what makes the two ensembles comparable at all.
    //
    // THE QUESTION THIS SUITE ADDS. The uniaxial ensemble showed that four constants survive a 0.2 deg /
    // 0.005 bench with the two refractive indices at ~0.05 % and the poorly-determined gyration component
    // at ~4.6 %. Nine unknowns is a different proposition: there are three indices to separate rather
    // than two, three gyration diagonal components, and three off-diagonal ones that the noiseless
    // ablation showed are only visible at all through oblique incidence. Whether they survive real
    // measurement error — and if so, in what order — is not something the noiseless facts can answer.
    // =================================================================================================

    /// The BLIND start guess the noisy facts fit from: an ISOTROPIC `n = 1.5` with no optical activity
    /// at all — nine unknowns started from a guess that knows nothing about the material beyond "some
    /// ordinary transparent glass-like solid".
    ///
    /// It is a far harder start than `perturbedStart`, and harder than the uniaxial suite's blind start
    /// too. All three indices are 4.7 %, 6.3 % and 7.2 % low; ALL THREE birefringences start at exactly
    /// zero instead of 5 % wrong; and all six gyration components start at exactly zero instead of
    /// 25-35 % wrong. Starting isotropic also means the crystal has no preferred axes at the first step,
    /// so the three index directions are as nearly degenerate there as they can be.
    ///
    /// It is nonetheless legitimate rather than lucky, for the structural reason the thickness
    /// commentary gives: linear retardance enters through cos and sin of 2 pi dn d / lambda, so what
    /// matters is the distance in FRINGES, and the three true birefringences are 0.173, 0.095 and 0.268
    /// wave on a 4 um plate. Zero is under a third of a fringe from the largest of them and closer still
    /// to the others — inside the same basin of attraction. The control fact below asserts exactly this
    /// by recovering all nine constants to machine precision from this start on NOISELESS data, which is
    /// what makes the noisy ensemble's scatter attributable to the noise rather than to the start.
    let blindStart : TriclinicParameters =
        {
            index1 = RefractionIndex 1.5
            index2 = RefractionIndex 1.5
            index3 = RefractionIndex 1.5
            g11 = RhoValue 0.0
            g22 = RhoValue 0.0
            g33 = RhoValue 0.0
            g23 = RhoValue 0.0
            g13 = RhoValue 0.0
            g12 = RhoValue 0.0
        }

    /// The box for the blind start. It must be wide enough to CONTAIN the truth: n3 is 116.4 scaled units
    /// from 1.5, so the +-50 box that comfortably brackets `perturbedStart` would exclude the answer and
    /// the fit would converge against a bound. +-200 is +-0.2 in refractive index and +-2e-3 in gyration —
    /// still physically sensible, and still far from the degenerate n = 0.
    let wideBox = SearchBox 200.0

    /// Run ONE noisy experiment end to end: generate its data on the shared bench, fit it from the blind
    /// start, and keep everything the ensemble will want to ask it afterwards.
    let recoverFromNoisy (noise : NoiseParam) (seed : NoiseSeed) : NoisyExperiment<TriclinicParameters> =
        let observations = noisyObserveWith forward triclinic noise seed fullConfigurations
        {
            seed = seed
            fit = fitObservations forward wideBox observations (scalingAround blindStart)
        }

    /// The quantities the ensemble reports on: the nine fitted unknowns, plus the THREE birefringences.
    ///
    /// The birefringences are not fit parameters and are the reason this list is not simply the axis
    /// list. Linear retardance depends on the DIFFERENCES of the indices, not on any index alone, and
    /// those differences are 0.015-0.042 against absolute indices of ~1.6 — so the data pins them far
    /// harder than it pins the common level, the three indices wander together from experiment to
    /// experiment, and an ensemble that reported only the nine parameters would understate this
    /// experiment by roughly the ratio of those two scales.
    let recoveredQuantities : RecoveredQuantity<TriclinicParameters> list =
        [ for a in TriclinicParameters.axes -> RecoveredQuantity.ofAxis a ]
        @ [ RecoveredQuantity.derived "n2 - n1" (fun p -> p.index2.value - p.index1.value)
            RecoveredQuantity.derived "n3 - n2" (fun p -> p.index3.value - p.index2.value)
            RecoveredQuantity.derived "n3 - n1" (fun p -> p.index3.value - p.index1.value) ]

    // =================================================================================================
    // The forward-side facts: the tensors, the orientation machinery, and the geometry the design rests
    // on. Nothing is fitted here.
    // =================================================================================================

    [<Fact>]
    member _.``the triclinic builder produces a biaxial permittivity and a FULL symmetric gyration tensor`` () =
        // The material assembly, pinned before anything is propagated through it. Three distinct
        // principal indices is what makes the crystal biaxial; a full symmetric ρ with six distinct
        // entries is what makes the inverse problem nine-dimensional.
        let props = buildTriclinic triclinic

        let expectedEps = [ index1.value ** 2.0; index2.value ** 2.0; index3.value ** 2.0 ]
        List.zip expectedEps (epsDiagonal props)
        |> List.iteri (fun i (e, a) -> Assert.True(abs (e - a) < 1.0e-12, $"eps[{i},{i}]: expected {e}, got {a}"))

        // Three DIFFERENT principal indices — the defining property, asserted rather than assumed.
        Assert.True(
            index1.value < index2.value && index2.value < index3.value,
            $"the principal indices must be distinct and ordered: {index1.value}, {index2.value}, {index3.value}")

        // The gyration tensor is the symmetric six-component object class 1 allows, laid out as
        // [[g11, g12, g13], [g12, g22, g23], [g13, g23, g33]].
        let expectedRho =
            [ [ g11.value; g12.value; g13.value ]
              [ g12.value; g22.value; g23.value ]
              [ g13.value; g23.value; g33.value ] ]
        List.zip expectedRho (rhoImaginary props)
        |> List.iteri (fun i (er, ar) ->
            List.zip er ar
            |> List.iteri (fun j (e, a) -> Assert.True(abs (e - a) < 1.0e-18, $"rho[{i},{j}]: expected {e}, got {a}")))

        // And the six independent components are genuinely six DIFFERENT numbers, so that a fit which
        // recovered one of them and copied it into another would fail rather than pass.
        let magnitudes = [ g11; g22; g33; g23; g13; g12 ] |> List.map (fun (g : RhoValue) -> abs g.value)
        Assert.Equal(6, magnitudes |> List.distinct |> List.length)

    [<Fact>]
    member _.``orientForCut carries each of the three principal axes onto the surface normal, gyration tensor and all`` () =
        // The orientation machinery, and the reason `YCut` had to exist. For a UNIAXIAL crystal the 1-
        // and 2-axes are equivalent, so a z-cut and an x-cut exhaust the distinct principal looks and a
        // y-cut would duplicate the x-cut. Here all three are different, and the permittivity diagonal
        // says so directly: each cut brings a different index into the LONGITUDINAL slot.
        //
        // The gyration tensor is checked as a whole rather than on its diagonal. Rotating ε without
        // rotating ρ — or rotating ρ by the transpose — is precisely the class of bug a diagonal-only
        // check cannot see and a full symmetric tensor cannot hide, which is why routing both through the
        // engine's own OpticalProperties.rotate is the design and this fact is its guard.
        let props = buildTriclinic triclinic
        let e1 = index1.value ** 2.0
        let e2 = index2.value ** 2.0
        let e3 = index3.value ** 2.0

        let assertDiagonal (label : string) (expected : float list) (actual : float list) =
            List.zip expected actual
            |> List.iteri (fun i (e, a) -> Assert.True(abs (e - a) < 1.0e-9, $"{label}[{i}]: expected {e}, got {a}"))

        let oriented (cut : SampleCut) = orientForCut cut (SampleAzimuth.degree 0.0) props

        // Z-cut: the crystal frame IS the lab frame, so the 3-axis is along the normal and the
        // transverse plane is (1,2).
        assertDiagonal "z-cut eps" [ e1; e2; e3 ] (epsDiagonal (oriented ZCut))

        // X-cut: a 90 deg rotation about y carries the crystal 3-axis onto the lab 1-axis, so the LAB
        // 3-slot — the propagation direction — now holds the crystal 1 index and the transverse plane
        // is (2,3).
        assertDiagonal "x-cut eps" [ e3; e2; e1 ] (epsDiagonal (oriented XCut))

        // Y-cut: a 90 deg rotation about x carries the crystal 3-axis onto the lab 2-axis, so the lab
        // 3-slot holds the crystal 2 index and the transverse plane is (1,3) — the one neither other cut
        // provides, and the only one in which g13 is transverse.
        assertDiagonal "y-cut eps" [ e1; e3; e2 ] (epsDiagonal (oriented YCut))

        // The three cuts genuinely differ: no two put the same index along the propagation direction.
        let longitudinal = [ for cut in principalCuts -> List.item 2 (epsDiagonal (oriented cut)) ]
        Assert.Equal(3, longitudinal |> List.distinct |> List.length)

        // INVARIANTS of a correct conjugation, which no permutation of components can fake and a
        // transposed or half-applied rotation would break: ρ stays symmetric, its trace is unchanged, and
        // so is its Frobenius norm.
        let trueTrace = g11.value + g22.value + g33.value
        let trueNorm =
            sqrt (
                g11.value ** 2.0 + g22.value ** 2.0 + g33.value ** 2.0
                + 2.0 * (g12.value ** 2.0 + g13.value ** 2.0 + g23.value ** 2.0))
        for cut in principalCuts do
            let r = rhoImaginary (oriented cut)
            let element i j = List.item j (List.item i r)
            for i in 0 .. 2 do
                for j in i + 1 .. 2 do
                    Assert.True(
                        abs (element i j - element j i) < 1.0e-18,
                        $"%A{cut}: the rotated gyration tensor must stay symmetric, rho[{i},{j}] = {element i j} vs rho[{j},{i}] = {element j i}")
            let trace = element 0 0 + element 1 1 + element 2 2
            let norm = sqrt ([ for i in 0 .. 2 do for j in 0 .. 2 -> element i j ** 2.0 ] |> List.sum)
            Assert.True(abs (trace - trueTrace) < 1.0e-17, $"%A{cut}: rotated rho trace = {trace}, expected {trueTrace}")
            Assert.True(abs (norm - trueNorm) < 1.0e-17, $"%A{cut}: rotated rho Frobenius norm = {norm}, expected {trueNorm}")

        // And the three cuts present three DIFFERENT transverse gyration blocks — the physical claim the
        // whole three-cut design rests on. The (0,1) element is the transverse off-diagonal the
        // polarimeter is most directly sensitive to.
        let transverseOffDiagonal (cut : SampleCut) = List.item 1 (List.item 0 (rhoImaginary (oriented cut)))
        let offDiagonals = [ for cut in principalCuts -> transverseOffDiagonal cut ]
        Assert.Equal(3, offDiagonals |> List.map abs |> List.distinct |> List.length)

    [<Fact>]
    member _.``the triclinic parameter set is a strict generalization of the uniaxial one`` () =
        // The bridge to the suite that is already trusted. Setting n1 = n2, g11 = g22 and every
        // off-diagonal to zero turns the class-1 material into exactly the uniaxial gyrotropic material
        // `MuellerInverseTests` fits, so the two builders must produce the SAME Mueller matrix for the
        // same configuration.
        //
        // This is worth a fact rather than a comment because the nine-parameter machinery is new and the
        // four-parameter one is not: if the generalization has an index transposed, or `type_1_Crystal`
        // lays its components out differently from `type_3_4_6_Crystal`, this fails and every other fact
        // in this file becomes untrustworthy at the same moment.
        let quartzOrdinary = RefractionIndex 1.542606
        let quartzExtraordinary = RefractionIndex 1.551651
        let quartzG11 = RhoValue 5.9e-5
        let quartzG33 = RhoValue -10.1e-5

        let asUniaxial : UniaxialParameters =
            {
                ordinaryIndex = quartzOrdinary
                extraordinaryIndex = quartzExtraordinary
                g11 = quartzG11
                g33 = quartzG33
            }

        let asTriclinic : TriclinicParameters =
            {
                index1 = quartzOrdinary
                index2 = quartzOrdinary
                index3 = quartzExtraordinary
                g11 = quartzG11
                g22 = quartzG11
                g33 = quartzG33
                g23 = RhoValue 0.0
                g13 = RhoValue 0.0
                g12 = RhoValue 0.0
            }

        let buildUniaxial (p : UniaxialParameters) : OpticalProperties =
            OpticalProperties.type_3_4_6_Crystal
                (EpsValue.fromRefractionIndex p.ordinaryIndex)
                (EpsValue.fromRefractionIndex p.extraordinaryIndex)
                p.g11
                p.g33

        let uniaxialForward = createBerremanForward buildUniaxial solverParameters

        // A handful of configurations spanning all three cuts, normal and oblique, transmission and
        // reflection — enough that a discrepancy in any tensor slot has somewhere to show.
        let probes =
            [ configuration ZCut 0.0 0.0 TransmittedMueller
              configuration XCut 0.0 30.0 TransmittedMueller
              configuration YCut 0.0 30.0 TransmittedMueller
              configuration ZCut 45.0 20.0 TransmittedMueller
              configuration XCut 55.0 90.0 ReflectedMueller ]

        for c in probes do
            match uniaxialForward.muellerOf c asUniaxial, forward.muellerOf c asTriclinic with
            | Ok expected, Ok actual -> assertMuellerEqual expected actual
            | other -> Assert.Fail($"both builders must solve %A{c.cut}: %A{other}")

    [<Fact>]
    member _.``an optic axis is reachable from the x-cut and unreachable from the z-cut, which is what the steep configurations are for`` () =
        // The biaxial analogue of the uniaxial suite's Snell fact, and the quantitative justification for
        // the B3 configurations.
        //
        // A biaxial crystal has two directions — the optic axes — along which the two transverse
        // principal indices coincide, so the linear birefringence vanishes identically and the sample is
        // a PURE optical rotator. They lie in the plane of the largest and smallest index, at
        // tan²V = (n₂² − n₁²)/(n₃² − n₂²) from the largest-index axis. That is the only place a biaxial
        // crystal shows its optical activity uncluttered, so whether the experiment can reach it is a
        // design question, not a curiosity.
        //
        // Snell's law answers it. However far a plate is tilted, its internal angle saturates at
        // arcsin(1/n) — and the answer differs by cut, which is the point.
        let fromLargest = opticAxisFromLargest triclinic
        let fromSmallest = Angle.degree 90.0 - fromLargest
        let cap = asin (1.0 / index2.value) / degree

        // Pinned per the spec §9 protocol from the computed geometry of this material.
        Assert.True(abs (fromLargest.degrees - 53.299) < 0.01, $"optic axis from the n3 axis = {fromLargest.degrees} deg")
        Assert.True(abs (fromSmallest.degrees - 36.701) < 0.01, $"optic axis from the n1 axis = {fromSmallest.degrees} deg")
        Assert.True(abs (cap - 38.642) < 0.01, $"internal angle cap = {cap} deg")

        // A z-cut plate propagates along the crystal 3-axis and would have to bend 53.30 deg to reach an
        // optic axis. It cannot: Snell caps it at 38.64 deg.
        Assert.True(
            fromLargest.degrees > cap,
            $"a z-cut plate must NOT be able to reach an optic axis: needs {fromLargest.degrees} deg, capped at {cap} deg")

        // An x-cut plate propagates along the crystal 1-axis and needs only 36.70 deg. It can — with
        // 1.9 deg to spare, which is why B3 exists and why it sits on the x-cut.
        Assert.True(
            fromSmallest.degrees < cap,
            $"an x-cut plate must be able to reach an optic axis: needs {fromSmallest.degrees} deg, capped at {cap} deg")

    [<Fact>]
    member _.``every configuration produces a physically realizable, essentially non-depolarizing Mueller matrix`` () =
        // Two invariants over the whole measurement set, mirroring the uniaxial suite.
        //
        // REALIZABILITY is absolute: a negative Cloude eigenvalue would mean the forward model had
        // produced a matrix no real optical element could produce, and no tolerance can excuse that.
        //
        // NON-DEPOLARIZATION is deliberately NOT asserted strictly. With numberOfReflections = 3 the
        // plate solver sums the Mueller matrices of the emerging beams INCOHERENTLY, and a sum of
        // Mueller-Jones matrices is in general depolarizing, so the depolarization index is below 1 BY
        // CONSTRUCTION. Reflection depolarizes far more than transmission because it has no dominant
        // term — the front-surface reflection and the internally-reflected beam are comparable in
        // amplitude while transforming polarization quite differently — hence two bands rather than one
        // slack one.
        let observations = observe fullConfigurations
        Assert.True(observations.Length > 30, $"the measurement set should be substantial, got {observations.Length}")

        let mutable worstTransmitted = 0.0
        let mutable worstOverall = 0.0
        for o in observations do
            let smallest = cloudeEigenvalues o.measured |> List.min
            Assert.True(
                smallest > -1.0e-9,
                $"%A{o.configuration.cut} at {o.configuration.incidenceAngle.value / degree} deg: negative Cloude eigenvalue {smallest} — the matrix is not physically realizable")

            match depolarizationIndex o.measured with
            | Ok di ->
                Assert.True(di <= 1.0 + 1.0e-9, $"depolarization index {di} exceeds 1, which is impossible")
                worstOverall <- max worstOverall (1.0 - di)
                if o.configuration.observable = TransmittedMueller then
                    worstTransmitted <- max worstTransmitted (1.0 - di)
            | Error e -> Assert.Fail($"expected a depolarization index, got %A{e}")

        // Bands pinned per the spec §9 protocol at ~1.8x and ~1.3x the observed worst values (0.0273 in
        // transmission, 0.3919 overall). Both are within a few per cent of what the uniaxial suite
        // measures for quartz (0.0295 and 0.416), which is the expected answer: the depolarization comes
        // from summing the plate's emerging beams, and that is governed by the SURFACE REFLECTANCE, which
        // these two materials share to within a few per cent of index.
        Assert.True(
            worstTransmitted < 0.05,
            $"worst transmission depolarization = {worstTransmitted}; the modelled multiple reflections should contribute a few percent at most")
        Assert.True(
            worstOverall < 0.5,
            $"worst depolarization over the whole set = {worstOverall}; reflection from a plate depolarizes strongly, but not without limit")

    // =================================================================================================
    // The inverse facts: the nine-parameter round trip, its identifiability, and the ablation.
    // =================================================================================================

    [<Fact>]
    member _.``the scaled parameter space round-trips all nine coordinates and a unit step moves exactly one`` () =
        // The guard on the generic `ParameterAxis` machinery, and the nine-parameter analogue of the
        // uniaxial suite's scaling fact.
        //
        // It matters more here than there. The uniaxial `toScaled`/`ofScaled` were written out by hand,
        // four fields at a time, and an error would have been visible on the page. These are a FOLD over
        // an axis list, so an off-by-one in the index, a `read` pointed at the wrong field, or two axes
        // accidentally sharing a `write` would all produce a mapping that is smooth, total, and wrong —
        // and a fit over a wrong mapping converges confidently to a wrong answer instead of failing.
        let scaling = scalingAround triclinic

        Assert.Equal(9, scaling.dimension)
        Assert.Equal<float[]>(Array.zeroCreate 9, toScaled scaling triclinic)

        // An arbitrary offset: every coordinate moved by a different amount, so a transposition between
        // any two of the nine is visible.
        let offsets = [| 3.5; -1.25; 0.75; 2.0; -4.0; 1.5; -0.5; 6.25; -2.75 |]
        let offset = ofScaled scaling offsets
        let roundTripped = toScaled scaling offset
        for i in 0 .. 8 do
            Assert.True(abs (roundTripped.[i] - offsets.[i]) < 1.0e-9, $"coordinate {i} did not round-trip: {roundTripped.[i]} vs {offsets.[i]}")

        // A UNIT step in each scaled coordinate must move exactly that ONE physical parameter, by exactly
        // its scale, and leave the other eight untouched. This is the property the optimizer relies on
        // and the one a shared `write` would break.
        for (i, axis) in List.indexed scaling.axes do
            let step = Array.init 9 (fun k -> if k = i then 1.0 else 0.0)
            let moved = ofScaled scaling step
            for (k, other) in List.indexed scaling.axes do
                let before = other.read triclinic
                let after = other.read moved
                let expected = if k = i then before + other.read scaling.scale else before
                Assert.True(
                    abs (after - expected) < 1.0e-15 * max 1.0 (abs expected),
                    $"a unit step in {axis.name.value} moved {other.name.value} to {after}, expected {expected}")

    [<Fact>]
    member _.``the full biaxial measurement set recovers all NINE material constants from a perturbed start`` () =
        // THE DELIVERABLE of manual task 012: from noiseless synthetic data generated by the forward
        // model, recover every one of the nine optical constants of a triclinic optically active crystal
        // — three principal refractive indices and all six independent gyration-tensor components —
        // starting from a deliberately wrong guess.
        //
        // This is the biaxial counterpart of the uniaxial suite's C1-C4 round trip, and it runs on the
        // same fit driver, the same residual and the same acceptance arithmetic. What differs is the
        // MATERIAL and the MEASUREMENT SET, which is the whole point of having generalized the machinery
        // rather than copied it.
        let observations = observe fullConfigurations
        let scaling = scalingAround perturbedStart
        let fit = fitObservations forward box observations scaling

        // The fit must actually have moved: a "recovery" that never left the start point would pass a
        // sloppy tolerance while proving nothing.
        Assert.True(fit.solution.iterations > 0, "the fit should have taken at least one step")

        let errors = recoveryErrors scaling triclinic fit.recovered
        let report = $"{describeErrors errors}; chi2 {fit.chiSquared}; {fit.solution.iterations} iterations"

        // Bands pinned per the spec §9 protocol at ~22x the observed worst relative error of 4.6e-12
        // (g13) and ~8000x the observed final chi-squared of 1.2e-25. With noiseless data generated by
        // the very model being fitted, the optimizer recovers every constant to essentially machine
        // precision, so anything looser would not be measuring the fit at all.
        //
        // The three indices come back at ~2e-15 and the six gyration components at 4e-15 to 5e-12; the
        // spread is the price of the gyration columns being ~100x weaker than the index columns, which
        // is exactly what the identifiability fact below measures. Twelve iterations — one FEWER than
        // the four-parameter uniaxial fit takes — which says that what costs a fit its convergence is
        // the conditioning of the measurement set, not the count of unknowns.
        for (name, err) in errors do
            Assert.True(err.value < 1.0e-10, $"{name.value} was not recovered: {report}")

        // The residual at the solution must be essentially zero: the data is noiseless and was generated
        // by the very model being fitted, so anything else means the fit stopped short of the truth.
        Assert.True(fit.chiSquared < 1.0e-21, $"final chi-squared out of band: {report}")

        // And the recovered parameters must REPRODUCE THE MEASUREMENTS, not merely sit near the truth.
        match forwardModels forward fit.recovered observations with
        | Ok models ->
            match residualVector observations models with
            | Ok r ->
                // Band pinned per the §9 protocol at ~100x the observed worst element residual of 1.0e-13.
                let worst = r |> Array.map abs |> Array.max
                Assert.True(worst < 1.0e-11, $"worst normalized Mueller element residual at the solution = {worst}")
            | Error e -> Assert.Fail($"expected a residual vector, got %A{e}")
        | Error e -> Assert.Fail($"expected forward models at the solution, got %A{e}")

        // Guard against a vacuous pass: the start point must NOT already satisfy the acceptance band, in
        // ANY of the nine coordinates. It is out by 25-35 % on the six gyration components and by
        // 0.1-0.2 % on the three indices — the weakest of those is still six orders of magnitude outside
        // the 1e-10 band the fit is being held to.
        let startErrors = recoveryErrors scaling triclinic perturbedStart
        Assert.True(
            startErrors |> List.forall (fun (_, e) -> e.value > 1.0e-4),
            $"the start guess must be outside the acceptance band in every coordinate: {describeErrors startErrors}")

    [<Fact>]
    member _.``the full biaxial measurement set is well conditioned and every one of the nine parameters is observable`` () =
        // Identifiability for nine unknowns, asserted through the two quantities that survive noiseless
        // data: the Jacobian column norms (how much the residual moves when one parameter moves) and the
        // condition number (the ratio of the most- to the least-constrained direction in parameter
        // space). Both depend on J alone, which is why they survive where FitQuality's covariance does
        // not — see finding F3 in the implementation log.
        //
        // No fit is run here. Identifiability is a property of the Jacobian at the solution, and for
        // noiseless synthetic data the solution IS the ground truth, so the Jacobian is evaluated there
        // directly. That makes this a statement about the EXPERIMENT DESIGN rather than about whether one
        // optimizer run converged, and it keeps it cheap.
        //
        // The trap the uniaxial suite records applies here too: FitQuality.residualJacobian steps each
        // parameter by sqrt(eps) * max(|x_p|, 1e-12), which is RELATIVE and collapses to ~1e-20 at
        // exactly x = 0, so the Jacobian must not be evaluated at the origin of the scaled space.
        // Centring on the perturbed start puts the truth at a non-zero scaled point in all nine.
        let scaling = scalingAround perturbedStart
        let residual = residualFor forward (observe fullConfigurations) scaling
        let truthPoint = toScaled scaling triclinic
        Assert.True(
            truthPoint |> Array.forall (fun x -> abs x > 1.0e-6),
            "the Jacobian must not be evaluated at the origin of the scaled space")

        let jacobian = FitQuality.residualJacobian residual truthPoint
        let norms = jacobianColumnNorms jacobian
        let report = describeColumnNorms scaling.axes norms

        // Every parameter must actually move the residual — this is what "observable" means, and it is
        // the property the ablation fact below shows can fail.
        for (i, axis) in List.indexed scaling.axes do
            Assert.True(norms.[i] > 1.0e-6, $"{axis.name.value} is unobservable in the full measurement set: {report}")

        // Bands pinned per the §9 protocol at ~2.6x the observed condition number of 382.5 — which is,
        // remarkably, no worse than the 436 the FOUR-parameter uniaxial suite measures. Nine unknowns
        // are not intrinsically harder to condition than four; what matters is whether the measurement
        // set gives each of them somewhere to act, and the three-cut plus oblique design does.
        let condition = jacobianCondition jacobian
        Assert.True(System.Double.IsFinite condition, $"the Jacobian condition number must be finite, got {condition}")
        Assert.True(condition < 1.0e3, $"Jacobian condition number = {condition}; {report}")

        // The three gyration DIAGONAL components (|J| ~ 2.4e-3) and the three OFF-DIAGONAL ones
        // (~1.0e-3) are within a factor of three of each other, which is what makes the nine-parameter
        // fit well posed rather than merely soluble. The ablation fact below shows that this costs an
        // oblique sweep: at normal incidence the off-diagonals are two orders weaker.
        let gyrationNorms = [ for name in [ "g11"; "g22"; "g33"; "g23"; "g13"; "g12" ] -> norms.[scaling.axes |> List.findIndex (fun a -> a.name.value = name)] ]
        Assert.True(
            List.max gyrationNorms < 3.0 * List.min gyrationNorms,
            $"the six gyration components should be comparably constrained: {report}")

    [<Fact>]
    member _.``the cuts buy the gyration DIAGONAL and only the tilt buys its OFF-DIAGONAL`` () =
        // The ablation, and the executable answer to "what experiments are needed" for a biaxial crystal.
        // Asserting the FAILURES is what turns the measurement design from an assumption into a result,
        // and what it shows here was MEASURED rather than predicted — the first draft of this fact
        // guessed wrongly, which is the whole reason for asserting it.
        //
        // The result is that the design has TWO independent levers, and each buys something the other
        // cannot:
        //
        //   THE CUTS buy the diagonal. A wave sees only the tensor components TRANSVERSE to its own
        //   propagation direction, so a z-cut plate at normal incidence acts on the (1,2) block and is
        //   blind to n3 and g33 entirely. Each further principal cut brings one more principal index and
        //   one more gyration diagonal component into view. Three cuts, three indices, three diagonals.
        //
        //   THE TILT buys the off-diagonal. This is the part that is NOT obvious, and it is a statement
        //   about the engine's constitutive convention rather than about geometry: at normal incidence on
        //   ANY principal cut, all three off-diagonal gyration components are invisible — including g12,
        //   which is transverse on a z-cut and might therefore be expected to show. Even all three cuts
        //   together leave them two orders of magnitude below the diagonal ones. It is oblique incidence
        //   that finally makes them observable, because the engine's rho couples through the wave's
        //   LONGITUDINAL field component, which only exists off-normal.
        //
        // The practical consequence for anyone designing this experiment: cutting more plates does not
        // substitute for tilting them, and tilting does not substitute for cutting. Both are required,
        // which is exactly the shape of the measurement set above.
        let scaling = scalingAround perturbedStart
        let truthPoint = toScaled scaling triclinic

        let normsFor (configurations : MeasurementConfiguration list) : float[] =
            jacobianColumnNorms (FitQuality.residualJacobian (residualFor forward (observe configurations) scaling) truthPoint)

        let normalOn (cut : SampleCut) =
            [ for azimuth in [ 0.0; 22.5; 45.0; 67.5 ] -> configuration cut 0.0 azimuth TransmittedMueller ]

        let zOnly = normalOn ZCut
        let zAndX = zOnly @ normalOn XCut
        let everyCutNormal = zAndX @ normalOn YCut

        let zNorms = normsFor zOnly
        let zxNorms = normsFor zAndX
        let cutsNorms = normsFor everyCutNormal
        let fullNorms = normsFor fullConfigurations

        let describe (label : string) (norms : float[]) = $"{label}: {describeColumnNorms scaling.axes norms}"
        let zLine = describe "z" zNorms
        let zxLine = describe "z+x" zxNorms
        let cutsLine = describe "three cuts, normal only" cutsNorms
        let fullLine = describe "full set" fullNorms
        let report = $"{zLine}  ||  {zxLine}  ||  {cutsLine}  ||  {fullLine}"

        let indexOf (name : string) = scaling.axes |> List.findIndex (fun a -> a.name.value = name)
        let normOf (norms : float[]) (name : string) = norms.[indexOf name]

        // The dividing line between "acts on the residual" and "is differencing noise". The observed
        // separation is three orders wide — dead columns come out at 1.3e-7 to 5.9e-6, live ones at
        // 9.8e-4 and above — so this threshold sits an order clear of both.
        let floor = 1.0e-4

        // ---- z-cut alone: FIVE of the nine unknowns have nothing to act on.
        for name in [ "n1"; "n2"; "g11"; "g22" ] do
            Assert.True(normOf zNorms name > floor, $"{name} must be observable from the z-cut. {report}")
        for name in [ "n3"; "g33"; "g23"; "g13"; "g12" ] do
            Assert.True(normOf zNorms name < floor, $"{name} must be unobservable from the z-cut alone. {report}")

        // ---- adding the x-cut opens the (2,3) block: n3 and g33 come alive, the off-diagonals do not.
        for name in [ "n3"; "g33" ] do
            Assert.True(normOf zxNorms name > floor, $"adding the x-cut must make {name} observable. {report}")
        for name in [ "g23"; "g13"; "g12" ] do
            Assert.True(normOf zxNorms name < floor, $"{name} must still be unobservable from two cuts. {report}")

        // ---- all three cuts, still at normal incidence: every index and every gyration DIAGONAL
        // component is now strongly constrained, and every OFF-DIAGONAL one is still dead. This is the
        // measurement the naive reading of "a wave sees the transverse components" predicts would work,
        // and it does not.
        for name in [ "n1"; "n2"; "n3"; "g11"; "g22"; "g33" ] do
            Assert.True(normOf cutsNorms name > floor, $"three cuts must make {name} observable. {report}")
        for name in [ "g23"; "g13"; "g12" ] do
            Assert.True(
                normOf cutsNorms name < floor,
                $"{name} must STILL be unobservable from three cuts at normal incidence — the tilt is what buys it. {report}")

        // ---- the full set adds oblique incidence and reflection, and that is what finally does it.
        for (i, axis) in List.indexed scaling.axes do
            Assert.True(fullNorms.[i] > floor, $"the full set must make {axis.name.value} observable. {report}")

        // And it does not merely lift the off-diagonals over a threshold: it lifts them by one to two
        // orders of magnitude — 63x for g23, 274x for g13, 177x for g12 — to within a factor of three of
        // the diagonal components. Band pinned per the §9 protocol at 25x, well clear of the weakest of
        // those and far above anything a residual floor could produce.
        for name in [ "g23"; "g13"; "g12" ] do
            Assert.True(
                normOf fullNorms name > 25.0 * normOf cutsNorms name,
                $"oblique incidence must transform {name} from noise into a measurement. {report}")

    [<Fact>]
    member _.``a blind isotropic n = 1.5 start still recovers all nine constants from noiseless data`` () =
        // THE CONTROL for the noisy ensemble, and the fact that makes its numbers mean something.
        //
        // The ensemble below fits from `blindStart`, so its scatter could in principle be the fault of
        // the START rather than of the NOISE — and with nine unknowns started from an isotropic guess
        // that is a live worry, not a formality: at n1 = n2 = n3 the crystal has no preferred axes at
        // all, and the three index directions are as nearly degenerate as they can be. This fact removes
        // the possibility: run the identical fit, from the identical start, over the identical box, on
        // data with NO noise in it, and every one of the nine constants comes back to essentially
        // machine precision. Whatever the ensemble's scatter is, it is therefore measurement error.
        let observations = observe fullConfigurations
        let scaling = scalingAround blindStart
        let fit = fitObservations forward wideBox observations scaling

        let errors = recoveryErrors scaling triclinic fit.recovered
        let report = describeErrors errors + $"; chi2 {fit.chiSquared}; {fit.solution.iterations} iterations"
        Assert.True(fit.solution.iterations > 0, $"the fit should have taken at least one step: {report}")

        // Bands pinned per the spec §9 protocol from the observed values — the SAME bands the
        // perturbed-start fact uses, because the outcome is the same: with noiseless data generated by
        // the very model being fitted, the optimizer walks all the way to machine precision from either
        // start. The three indices come back at ~3e-15 and the six gyration components at 3e-13 to
        // 1.2e-12, at a final chi-squared of 8.8e-26, after 15 iterations — against 12 from the near
        // start. Three extra iterations is the entire price of knowing nothing about the material.
        for (name, err) in errors do
            Assert.True(err.value < 1.0e-10, $"{name.value} was not recovered from the blind start: {report}")

        Assert.True(fit.chiSquared < 1.0e-21, $"final chi-squared out of band: {report}")

        // Guard against a vacuous pass: the blind start must be genuinely far from the answer in every
        // one of the nine coordinates.
        let startErrors = recoveryErrors scaling triclinic blindStart
        Assert.True(
            startErrors |> List.forall (fun (_, e) -> e.value > 1.0e-2),
            $"the blind start must be far from the truth: {describeErrors startErrors}")

    [<Fact>]
    member _.``an ensemble of noisy experiments recovers all nine constants, and the birefringences survive far better`` () =
        // THE DELIVERABLE of manual task 014: what a decent-but-ordinary optical bench actually gets out
        // of a TRICLINIC crystal — nine optical constants at once — and, the part a single measurement
        // cannot answer, with what error bar.
        //
        // Each seed is one complete experiment: 40 configurations, each measured at angles that differ
        // from the recorded ones by up to 0.2 deg, each read back by a detector good to 0.005 of full
        // scale. The fit then runs from the blind isotropic start against the RECORDED angles. Eight such
        // experiments give eight independent answers, and their spread is the uncertainty.
        //
        // WHY BOTH A BIAS AND A SCATTER ARE REPORTED, and why they differ in kind: see the commentary in
        // `InverseFitHarness`. In short, the detector error averages away and the rotation error is fixed
        // within one experiment and so displaces its answer systematically — whether it also averages
        // ACROSS experiments is the question an ensemble exists to answer.
        //
        // RUNTIME. Eight full nine-parameter fits from a blind start dominate this class. They are run
        // SERIALLY: the uniaxial suite measured concurrency on the same forward model and found it slower
        // rather than faster, because the solves allocate heavily and extra threads buy contention.
        let experiments = ensembleSeeds |> List.map (recoverFromNoisy NoiseParam.decentLab)

        for e in experiments do
            Assert.True(e.fit.solution.iterations > 0, $"the fit for seed {e.seed.value} never took a step")

        let table = recoveredQuantities |> List.map (statisticsFor triclinic (experiments |> List.map (fun e -> e.fit.recovered)))
        let residualLevels =
            [ for e in experiments ->
                $"seed {e.seed.value}: chi2 {e.fit.chiSquared}, rms {e.fit.rmsResidual}, {e.fit.solution.iterations} iterations" ]
        let report =
            System.String.Join("; ", [ for row in table -> row.describe ])
            + " || "
            + System.String.Join("; ", residualLevels)

        // NON-VACUITY, asserted before anything else. If the noise had failed to reach the data every
        // seed would return the same answer, the scatter would be zero, and every band below would pass
        // while measuring nothing at all.
        for row in table do
            Assert.True(
                row.scatter.value > 0.0,
                $"seed-to-seed scatter of {row.quantity.name.value} is zero — the noise never reached the fit: {report}")

        // ------------------------------------------------------- the indices and the birefringences
        //
        // These are the quantities a RELATIVE band means something for. Bands pinned per the spec §9
        // protocol at ~2.5x the observed values: the three indices come out at 0.088-0.091 % and the
        // three birefringences at 0.038-0.097 %.
        for name in [ "n1"; "n2"; "n3" ] do
            let row = rowFor table name
            Assert.True(row.scatter.value < 2.5e-3, $"{name} scatter {row.scatter.value} out of band: {report}")
            Assert.True(row.worst.value < 4.0e-3, $"{name} worst-case {row.worst.value} out of band: {report}")

        for name in [ "n2 - n1"; "n3 - n2"; "n3 - n1" ] do
            let row = rowFor table name
            Assert.True(row.scatter.value < 2.5e-3, $"{name} scatter {row.scatter.value} out of band: {report}")
            Assert.True(row.worst.value < 6.0e-3, $"{name} worst-case {row.worst.value} out of band: {report}")

        // ------------------------------------------------------------------ the gyration tensor
        //
        // THE HEADLINE RESULT, and it is a negative one, stated as such: on this bench the gyration
        // tensor of this crystal is at the EDGE OF MEASURABILITY. The relative scatters run from 21 % to
        // 187 %, and the worst single experiment misses g23 by four times its own value. The three
        // indices, measured in the very same experiments, come out at 0.09 %.
        //
        // WHAT IS ACTUALLY GOING ON is visible only in ABSOLUTE units, and it is the reason this fact
        // bands them that way. Every one of the six components is determined to ~1.3e-5 … 3.2e-5,
        // whatever its own value happens to be — the uncertainty is a property of the BENCH and the
        // sample geometry, not of the component. The relative figures are then just that absolute
        // uncertainty divided by six different numbers, and g23 looks worst only because at 1.72e-5 it
        // is the smallest of the six.
        //
        // AND THE CAUSE IS THE PLATE THICKNESS, which is forced by the crystal being biaxial. Circular
        // retardance grows as g·d, so the gyration sensitivity is proportional to thickness. The uniaxial
        // suite gets its g11 to an ABSOLUTE 7.8e-8 on the same bench — 190 times better — because its C1
        // configuration is a 1 mm plate that carries no linear retardance at all. A biaxial crystal has
        // linear birefringence along every principal axis, so no such plate exists: every configuration
        // here has to be 4 µm to stay sub-wave, 250 times thinner, and the 190-fold penalty is almost
        // exactly that ratio. The optical activity of a biaxial crystal is hard to measure for a
        // structural reason, not an incidental one.
        //
        // Bands pinned per the §9 protocol at ~2x the observed absolute scatters.
        let gyrationNames = [ "g11"; "g22"; "g33"; "g23"; "g13"; "g12" ]
        let gyrationScatters = [ for name in gyrationNames -> (rowFor table name).absoluteScatter ]
        for (name, scatter) in List.zip gyrationNames gyrationScatters do
            Assert.True(scatter < 6.0e-5, $"{name} absolute scatter {scatter} out of band: {report}")

        // The uncertainty really is uniform across the six, which is the claim that "it is the bench, not
        // the component" rests on: the widest is 2.5x the narrowest, against component VALUES that span
        // a factor of four.
        Assert.True(
            List.max gyrationScatters < 4.0 * List.min gyrationScatters,
            $"the six gyration components should carry comparable ABSOLUTE uncertainty: {report}")

        // And the negative result is asserted rather than left implicit, so that a design change which
        // fixed it would announce itself here: at least one component is quoted with an error bar of the
        // same order as its own value.
        Assert.True(
            gyrationNames |> List.exists (fun name -> (rowFor table name).scatter.value > 0.5),
            $"this bench is expected NOT to determine the gyration tensor usefully: {report}")

        // THE ERRORS ARE RANDOM, NOT SYSTEMATIC — a result rather than an assumption, and the reason the
        // acceptance test is a band on the SCATTER rather than on any single experiment. The yardstick is
        // the standard error of the ensemble MEAN: a departure smaller than that is the random error not
        // yet averaged away, not evidence of anything systematic.
        for row in table do
            let allowance = 3.0 * (standardErrorOfMean (List.length ensembleSeeds) row).value
            Assert.True(
                row.bias.value < allowance,
                $"{row.quantity.name.value} bias {row.bias.value} exceeds 3 standard errors of the mean ({allowance}): {report}")

        // ------------------------------------------------------------------ what the design determines
        //
        // THE BIREFRINGENCES SURVIVE FAR BETTER THAN THE INDICES, in ABSOLUTE terms. This is the same
        // effect the uniaxial suite records, and for a biaxial crystal there are three of them rather
        // than one. Linear retardance depends on the DIFFERENCES, so the data pins those far harder than
        // the common index level and the three indices wander together from experiment to experiment.
        //
        // The comparison must be absolute. Relatively the differences look no better — they are being
        // divided by values 40-100 times smaller — which is exactly the arithmetic that hides the effect.
        // Measured: the three indices all scatter by ~1.42e-3 in absolute terms, and the three
        // DIFFERENCES by 1.05e-5, 1.46e-5 and 1.57e-5 — 91 to 135 times better. Band pinned at 50x, the
        // same figure the uniaxial suite uses for its single birefringence.
        let indexRows = [ for name in [ "n1"; "n2"; "n3" ] -> rowFor table name ]
        let worstIndexScatter = indexRows |> List.map (fun row -> row.absoluteScatter) |> List.max
        for name in [ "n2 - n1"; "n3 - n2"; "n3 - n1" ] do
            Assert.True(
                (rowFor table name).absoluteScatter < worstIndexScatter / 50.0,
                $"{name} must be determined far better than any single index in ABSOLUTE terms: {report}")

        // ------------------------------------------------------- the fit's OWN account of its error
        //
        // The same cross-check the uniaxial suite makes, on nine parameters instead of four: a covariance
        // estimate is a PREDICTION ABOUT REPEAT EXPERIMENTS, and this ensemble is exactly a set of repeat
        // experiments, so the first seed's own reported standard error can be held against the spread the
        // other seven actually produced. There are very few places where that comparison is available at
        // all.
        let first = List.head experiments
        let quality = FitQuality.reportFrom first.fit.residual first.fit.solution.solution first.fit.solution.finalResiduals

        let predictions = predictedRelativeErrors triclinic first.fit quality
        let observedScatter (name : ParameterName) : RelativeError = (rowFor table name.value).scatter
        let comparison =
            System.String.Join(
                "; ",
                [ for (name, predicted) in predictions ->
                    let observed = observedScatter name
                    $"{name.value}: predicted {predicted.value}, observed {observed.value}, ratio {predicted.value / observed.value}" ])

        // Finding F3's failure mode is gone once real noise is present: every reported standard error is
        // finite and strictly positive, and the reduced chi-squared is the noise level rather than a
        // denormal.
        for (name, predicted) in predictions do
            Assert.True(System.Double.IsFinite predicted.value && predicted.value > 0.0, $"{name.value} has no usable standard error: {comparison}")
        Assert.True(
            quality.reducedChiSquared > 1.0e-12,
            $"reduced chi-squared = {quality.reducedChiSquared}, which is the underflowing noiseless regime F3 describes")

        // AND IT IS RIGHT TO WITHIN A FACTOR OF ~2, ACROSS FOUR DECADES OF UNCERTAINTY. The predictions
        // range from 5.5e-4 (the indices) to 2.0 (g23) — that is, from "one part in two thousand" to
        // "twice the value itself" — and every one of the nine lands between 0.62x and 2.40x of what the
        // ensemble actually produced. Band pinned per the §9 protocol at [0.3, 4.0], which keeps a
        // factor of ~1.7 of margin at both ends.
        //
        // The tolerance is looser than the uniaxial suite's [0.5, 2.0], and the reason is worth
        // recording rather than hiding. A covariance estimate is a LINEARIZATION about the solution, and
        // it is exact only where the uncertainty is small enough for that linearization to hold. Here it
        // holds beautifully for the indices (0.09 % errors) and progressively less well for the gyration
        // components, whose error bars are of the same order as their own values — g12, the worst, is
        // predicted 2.4x too pessimistically. Ratios drifting with the size of the error is exactly what
        // a linearized estimate of a nonlinear problem should do.
        for (name, predicted) in predictions do
            let ratio = predicted.value / (observedScatter name).value
            Assert.True(
                ratio > 0.3 && ratio < 4.0,
                $"the covariance-predicted error for {name.value} disagrees with the ensemble: {comparison}")

        // A SHARPER STATEMENT that the three indices support and no single one of them could. Their
        // ratios are 0.6182, 0.6197 and 0.6208 — identical to four parts in a thousand — so the
        // covariance is not merely approximately right for them, it is wrong by the SAME factor for all
        // three. That is the signature of a discrepancy in the ERROR MODEL rather than in any one
        // parameter: the covariance treats the whole residual as random noise, and part of it is the
        // rotation-angle error, which is fixed within an experiment and therefore inflates the real
        // experiment-to-experiment scatter above what the residual alone predicts.
        let indexRatios =
            [ for (name, predicted) in predictions do
                if List.contains name.value [ "n1"; "n2"; "n3" ] then
                    predicted.value / (observedScatter name).value ]
        Assert.True(
            List.max indexRatios < 1.05 * List.min indexRatios,
            $"the three indices must be mispredicted by the SAME factor: %A{indexRatios}; {comparison}")

        // And the interval the single experiment would have QUOTED must contain the truth — the question
        // an experimenter actually asks of a fit report — in all nine coordinates.
        let truthPoint = toScaled first.fit.scaling triclinic
        let intervals =
            [ for (i, a) in List.indexed first.fit.scaling.axes ->
                let (lo, hi) = quality.confidenceIntervals.[i]
                a.name, lo <= truthPoint.[i] && truthPoint.[i] <= hi ]
        let missed = System.String.Join(", ", [ for (name, covered) in intervals do if not covered then name.value ])
        Assert.True(intervals |> List.forall snd, $"the 95%% intervals missed {missed}: {comparison}")
