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
/// The fit driver, the residual, the scaling and the acceptance arithmetic are all shared with
/// `MuellerInverseTests` through `InverseFitHarness`; nothing in this file re-implements any of it. That
/// is only possible because `MuellerInverse` is generic in its parameter set — see `ParameterAxis`.
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
