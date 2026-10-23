namespace BerremanTests

open Berreman.Constants
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.Solvers
open OpticalProperties.Active
open OpticalConstructor.Domain.MuellerInverse

/// Spec 0044 — the TRICLINIC (class 1) gyrotropic sample every biaxial suite in this assembly shares:
/// its ground-truth constants, the builder that turns nine numbers into engine tensors, its fit scaling,
/// and the pure geometry that follows from the constants.
///
/// It lives in its own module because more than one suite now measures this same crystal with DIFFERENT
/// EXPERIMENTS — `BiaxialInverseTests` with the theoretically-clean 4 µm design and
/// `FeasibleBiaxialInverseTests` with the experimentally realizable one — and the whole point of that
/// comparison is that the SAMPLE is held fixed while the measurement set changes. Duplicating the
/// constants would silently destroy the comparison the moment one copy was edited.
///
/// Spec R3 keeps the material in the TEST assembly rather than in the `OpticalProperties` library, which
/// has no biaxial gyrotropic material and should not grow one for a test.
///
/// WHY TRICLINIC. Class 1 has no symmetry element beyond the identity, so nothing constrains either
/// tensor: ε contributes THREE independent principal refractive indices (every triclinic crystal is
/// biaxial) and the gyration tensor, being symmetric, contributes all SIX of its independent components.
/// Nine unknowns — the maximum a transparent, non-magnetic, non-dispersive crystal can present.
///
/// THE GROUND TRUTH, and exactly how much of it is real. The three principal refractive indices are
/// lithium triborate's (LiB₃O₅, LBO) at 632.8 nm, evaluated from the standard Sellmeier set
///
///     n_x² = 2.4542 + 0.0113/(λ² − 0.0114) − 0.0139 λ²
///     n_y² = 2.5390 + 0.0128/(λ² − 0.0119) − 0.0185 λ²
///     n_z² = 2.5865 + 0.0131/(λ² − 0.0122) − 0.0186 λ²      (λ in µm)
///
/// and the gyration magnitudes are anchored on Shopa, Ftomyn & Shopa's measurement of that same crystal
/// (g₁₂ = 4.31e-5, optical rotatory power 7.06 deg/mm along an optic axis, J. Appl. Cryst. 56, 2023).
/// LBO is class mm2 and non-enantiomorphous, so its symmetry allows exactly the one component that was
/// measured; the other five here are SYNTHETIC, of the same order, with distinct magnitudes and mixed
/// signs so that no two of the nine unknowns are degenerate. The forward data is generated FROM these
/// values, so they are the ground truth by definition and the facts built on them measure the INVERSE
/// MACHINERY rather than the physics.
///
/// A CONVENTION WARNING: the engine's `Rho` is the BI-ANISOTROPIC (Tellegen–Post) magnetoelectric tensor
/// of D = εE + ρH, NOT the crystallographic gyration tensor g of D = εE + i(G × E). They share the
/// symbol g in the literature and in `Active.Rho.type_1_Crystal` but are different parameterizations.
module BiaxialSample =

    /// He-Ne — the line the LBO gyration measurement this sample is anchored on was made at, and the
    /// reference wavelength for every constant below.
    let referenceWaveLength = WaveLength.nm 632.8<nm>

    /// LBO's x-axis Sellmeier index at 0.6328 µm. The SMALLEST of the three.
    let index1 = RefractionIndex 1.574064836

    /// LBO's y-axis Sellmeier index at 0.6328 µm — the INTERMEDIATE index, and the one that decides
    /// where the optic axes lie.
    let index2 = RefractionIndex 1.601416913

    /// LBO's z-axis Sellmeier index at 0.6328 µm. The LARGEST. The three birefringences are therefore
    /// n₂ − n₁ = 0.027352, n₃ − n₂ = 0.014997 and n₃ − n₁ = 0.042349 — three different numbers, which is
    /// what "biaxial" means and what a uniaxial crystal cannot present.
    let index3 = RefractionIndex 1.616414016

    /// The gyration component Shopa et al. measured for LBO at 633 nm, used as the ANCHOR that sets the
    /// scale of all six. Everything below it is synthetic (see the module header).
    let g11 = RhoValue 4.31e-5

    let g22 = RhoValue 6.90e-5
    let g33 = RhoValue -2.85e-5
    let g23 = RhoValue 1.72e-5
    let g13 = RhoValue -3.64e-5
    let g12 = RhoValue 2.46e-5

    /// The nine-parameter ground truth the forward data is generated from and every fit must recover.
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
    /// unknowns — the ONE material-specific function in the whole pipeline, and the reason
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

    /// Spec R4: three internal reflections, stated explicitly at the composition root rather than
    /// inherited from `SolverParameters.defaultValue`, because `numberOfReflections` changes the physics
    /// being modelled rather than tuning it.
    let solverParameters : SolverParameters = { numberOfReflections = 3 }

    /// The dimensionless fit space for this material: 1e-3 per unit of refractive index, 1e-5 per unit
    /// of gyration. ALGLIB's Levenberg–Marquardt differentiates with a FIXED ABSOLUTE step of 1e-6, so
    /// those units make that step a 1e-9 perturbation of n and a 1e-11 perturbation of g — small enough
    /// to be genuinely linear and large enough to move the Mueller elements far above the solver's own
    /// numerical noise.
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

    /// The optic-axis half-angle of a biaxial crystal, measured from the LARGEST-index axis and lying in
    /// the plane of the largest and smallest: `tan²V = (n₂² − n₁²)/(n₃² − n₂²)`. Computed from the
    /// parameters rather than hard-coded, so nothing can drift away from the material.
    ///
    /// This is the single most useful number about a biaxial crystal for experiment design: along an
    /// optic axis the two transverse principal indices coincide, the linear birefringence vanishes
    /// IDENTICALLY, and the sample is a pure circular retarder. It is the only direction in which a
    /// biaxial crystal shows its optical activity uncluttered.
    let opticAxisFromLargest (p : TriclinicParameters) : Angle =
        let sq (n : RefractionIndex) = n.value * n.value
        atan (sqrt ((sq p.index2 - sq p.index1) / (sq p.index3 - sq p.index2))) |> Angle

    /// The optic-axis half-angle for the WAVE NORMAL — the BINORMAL — again measured from the
    /// largest-index axis: `tan²V = (n₁⁻² − n₂⁻²) / (n₂⁻² − n₃⁻²)`.
    ///
    /// A biaxial crystal has TWO pairs of "optic axes" and the literature calls both by that name. The
    /// BIRADIALS (`opticAxisFromLargest` above) are the directions along which the two RAY velocities
    /// coincide; the BINORMALS are the directions along which the two WAVE-NORMAL velocities coincide,
    /// and it is the binormals along which the linear birefringence of a plane wave vanishes.
    ///
    /// The distinction is not academic here. For this material the two differ by 0.73 deg, and on the
    /// 100 µm plate `FeasibleBiaxialInverseTests` uses that is 29.6 deg of stray linear retardance
    /// against 1.1 deg — the difference between a pure circular retarder and a strongly elliptical one.
    /// Any plate cut normal to a BIRADIAL is simply not an optic-axis plate. That suite scans the cut
    /// angle and asserts the measured minimum lands here rather than on the biradial.
    let opticAxisWaveNormal (p : TriclinicParameters) : Angle =
        let inverseSq (n : RefractionIndex) = 1.0 / (n.value * n.value)
        atan (sqrt ((inverseSq p.index1 - inverseSq p.index2) / (inverseSq p.index2 - inverseSq p.index3)))
        |> Angle

    /// Read the diagonal of the engine's ε as three plain numbers.
    let epsDiagonal (p : OpticalProperties) : float list =
        let (Eps (ComplexMatrix3x3 (ComplexMatrix e))) = p.eps
        [ e.[0, 0].Real; e.[1, 1].Real; e.[2, 2].Real ]

    /// Read the whole imaginary part of the engine's ρ as a 3×3 of plain numbers — the gyration tensor
    /// as the lab frame sees it. The FULL tensor and not just its diagonal, because a triclinic sample's
    /// off-diagonals are exactly what a wrong rotation would scramble.
    let rhoImaginary (p : OpticalProperties) : float list list =
        let (Rho (ComplexMatrix3x3 (ComplexMatrix r))) = p.rho
        [ for i in 0 .. 2 -> [ for j in 0 .. 2 -> r.[i, j].Imaginary ] ]

    /// The quantities an ensemble reports on: the nine fitted unknowns, plus the THREE birefringences.
    ///
    /// The birefringences are not fit parameters and are the reason this list is not simply the axis
    /// list. Linear retardance depends on the DIFFERENCES of the indices, not on any index alone, and
    /// those differences are 0.015-0.042 against absolute indices of ~1.6 — so the data pins them far
    /// harder than it pins the common level, the three indices wander together, and a report listing
    /// only the nine parameters would understate the experiment by roughly the ratio of those scales.
    let recoveredQuantities : RecoveredQuantity<TriclinicParameters> list =
        [ for a in TriclinicParameters.axes -> RecoveredQuantity.ofAxis a ]
        @ [ RecoveredQuantity.derived "n2 - n1" (fun p -> p.index2.value - p.index1.value)
            RecoveredQuantity.derived "n3 - n2" (fun p -> p.index3.value - p.index2.value)
            RecoveredQuantity.derived "n3 - n1" (fun p -> p.index3.value - p.index1.value) ]
