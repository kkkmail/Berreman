namespace BerremanTests

open System.Numerics
open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Domain.MuellerInverse
open BerremanTests.InverseFitHarness
open BerremanTests.BiaxialSample

/// Spec 0044 — the ABSORBING triclinic sample both absorbing suites share: its ground-truth constants,
/// the builder that turns fifteen numbers into engine tensors, its fit scaling, and the measurement set.
///
/// It lives in its own module for the same reason `BiaxialSample` does: `AbsorbingBiaxialInverseTests`
/// and `AbsorbingBiaxialNoisyTests` measure the SAME crystal with the same experiment and differ only in
/// whether the data carries measurement error. Duplicating the constants would destroy that comparison
/// the moment one copy was edited.
///
/// THE SAMPLE is the transparent twin `BiaxialSample.triclinic` with absorption added: its three
/// principal refractive indices and its six gyration components are kept EXACTLY, so the two are the
/// same crystal observed at two ends of an absorption edge. See `AbsorbingBiaxialInverseTests` for the
/// parameter count, the reason ε′ and ε″ do not share principal axes, and the measurement that fixed the
/// count at fifteen rather than twenty-one.
module AbsorbingBiaxialSample =

    /// ε″ diagonal components, of order 1e-3 — k ≈ 3e-4, α ≈ 80 cm⁻¹, and a measured 24-47 % throughput
    /// across the configuration set. Squarely the "near the absorption edge but still comfortably
    /// measurable in transmission" regime the literature puts at α ≈ 100 cm⁻¹.
    let epsIm11 = EpsValue 1.20e-3
    let epsIm22 = EpsValue 1.80e-3
    let epsIm33 = EpsValue 0.90e-3

    /// ε″ OFF-diagonal components IN THE ε′ FRAME. These are the parameters that make the crystal
    /// genuinely triclinic-absorbing: non-zero here means absorption and refraction do not share
    /// principal axes. At a third of the diagonal scale they put the principal absorption axes 42.2,
    /// 42.8 and 26.5 deg from the crystal axes — a strongly misaligned crystal, not one that is
    /// nominally general and numerically almost orthorhombic.
    let epsIm23 = EpsValue 0.35e-3
    let epsIm13 = EpsValue -0.25e-3
    let epsIm12 = EpsValue 0.45e-3

    /// The fifteen-parameter ground truth. Its real part is `BiaxialSample.triclinic` exactly.
    let absorbing : AbsorbingTriclinicParameters =
        {
            index1 = index1
            index2 = index2
            index3 = index3
            epsIm11 = epsIm11
            epsIm22 = epsIm22
            epsIm33 = epsIm33
            epsIm23 = epsIm23
            epsIm13 = epsIm13
            epsIm12 = epsIm12
            g11 = g11
            g22 = g22
            g33 = g33
            g23 = g23
            g13 = g13
            g12 = g12
        }

    /// Build the engine's optical properties for an absorbing triclinic gyrotropic crystal.
    ///
    /// Neither tensor can go through a named convenience constructor. `Eps.fromComplexRefractionIndex`
    /// only ever produces a DIAGONAL matrix, so a complex symmetric ε with off-diagonal imaginary parts
    /// has to be reached through the general `Eps.create`, which takes the complex entries directly.
    ///
    /// ρ stays purely IMAGINARY. Its real part would be the natural home for a circular-dichroism
    /// gyration g″, and it is not used, because that coupling is measured to have no first-order effect
    /// in this engine — see the convention fact in `AbsorbingBiaxialInverseTests`.
    let buildAbsorbing (p : AbsorbingTriclinicParameters) : OpticalProperties =
        let e (re : double) (im : double) = Complex(re, im)
        let n1Sq = p.index1.value * p.index1.value
        let n2Sq = p.index2.value * p.index2.value
        let n3Sq = p.index3.value * p.index3.value
        {
            eps =
                [ [ e n1Sq p.epsIm11.value; e 0.0 p.epsIm12.value; e 0.0 p.epsIm13.value ]
                  [ e 0.0 p.epsIm12.value; e n2Sq p.epsIm22.value; e 0.0 p.epsIm23.value ]
                  [ e 0.0 p.epsIm13.value; e 0.0 p.epsIm23.value; e n3Sq p.epsIm33.value ] ]
                |> Eps.create
            mu = Mu.vacuum
            rho =
                [ [ e 0.0 p.g11.value; e 0.0 p.g12.value; e 0.0 p.g13.value ]
                  [ e 0.0 p.g12.value; e 0.0 p.g22.value; e 0.0 p.g23.value ]
                  [ e 0.0 p.g13.value; e 0.0 p.g23.value; e 0.0 p.g33.value ] ]
                |> Rho.create
        }

    /// The dimensionless fit space for fifteen parameters. 1e-3 per unit of refractive index and 1e-5 per
    /// unit of gyration, exactly as the transparent suites use; 1e-4 per unit of ε″, which puts the truth
    /// at ~10 scaled units and makes ALGLIB's fixed 1e-6 differentiation step a 1e-10 perturbation of ε″
    /// — small enough to be linear, large enough to move the Mueller elements far above the solver's
    /// numerical floor.
    let absorbingScalingAround (centre : AbsorbingTriclinicParameters) : ParameterScaling<AbsorbingTriclinicParameters> =
        {
            axes = AbsorbingTriclinicParameters.axes
            centre = centre
            scale =
                {
                    index1 = RefractionIndex 1.0e-3
                    index2 = RefractionIndex 1.0e-3
                    index3 = RefractionIndex 1.0e-3
                    epsIm11 = EpsValue 1.0e-4
                    epsIm22 = EpsValue 1.0e-4
                    epsIm33 = EpsValue 1.0e-4
                    epsIm23 = EpsValue 1.0e-4
                    epsIm13 = EpsValue 1.0e-4
                    epsIm12 = EpsValue 1.0e-4
                    g11 = RhoValue 1.0e-5
                    g22 = RhoValue 1.0e-5
                    g33 = RhoValue 1.0e-5
                    g23 = RhoValue 1.0e-5
                    g13 = RhoValue 1.0e-5
                    g12 = RhoValue 1.0e-5
                }
        }

    // =================================================================================================
    // The measurement set — the feasible design from manual task 016 part 1.
    // =================================================================================================

    /// The vendor minimum for LBO, and the plate manual task 016 part 1 established as the thinnest one
    /// that actually exists.
    let plate = Thickness.mkm 100.0<mkm>

    let waveLength = referenceWaveLength

    /// The transparent twin's binormal. In an ABSORBING crystal the two optic axes split into four
    /// singular optical axes, so this cut is only approximately retardance-free here; at ε″/ε′ ~ 1e-3 the
    /// splitting is far below the alignment tolerance the plate already carries.
    let opticAxisCut = TiltedCut (opticAxisWaveNormal triclinic)
    let otherOpticAxisCut = TiltedCut -(opticAxisWaveNormal triclinic)

    let configuration (cut : SampleCut) (incidenceDeg : double) (azimuthDeg : double) (observable : Observable) : MeasurementConfiguration =
        configurationOf cut plate incidenceDeg azimuthDeg observable waveLength

    /// A1 — the optic-axis anchor: approximately retardance-free, so the circular effects are seen least
    /// cluttered.
    let a1 : MeasurementConfiguration list =
        [ for cut in [ opticAxisCut; otherOpticAxisCut ] do
            for azimuth in [ 0.0; 22.5; 45.0; 67.5 ] ->
                configuration cut 0.0 azimuth TransmittedMueller ]

    /// A2 — the tilt sweep on the optic-axis plate: retardance grows continuously from zero, so the order
    /// is never ambiguous, and the angle diversity is what the gyration off-diagonals need.
    let a2 : MeasurementConfiguration list =
        [ for incidence in [ 20.0; 40.0; 60.0 ] do
            for azimuth in [ 0.0; 90.0 ] do
                for observable in [ TransmittedMueller; ReflectedMueller ] ->
                    configuration opticAxisCut incidence azimuth observable ]

    /// A3 — the three principal cuts, normal and oblique, transmission and reflection. For an absorbing
    /// crystal these carry more than they do for a transparent one: the LINEAR DICHROISM of each
    /// transverse plane, which is what makes ε″ identifiable component by component.
    let a3 : MeasurementConfiguration list =
        [ for cut in [ ZCut; XCut; YCut ] do
            for incidence in [ 0.0; 45.0 ] do
                for azimuth in [ 0.0; 90.0 ] do
                    for observable in [ TransmittedMueller; ReflectedMueller ] ->
                        configuration cut incidence azimuth observable ]

    let fullConfigurations = a1 @ a2 @ a3

    /// The start guess both absorbing suites fit from: every ε″ component 20-30 % wrong, every gyration
    /// component 25-35 % wrong, and the three indices off by +0.20 %, +0.10 %, +0.15 % — the same index
    /// perturbation the transparent suites use, which is the one that has to stay inside half a fringe.
    ///
    /// It is a PERTURBED start and not a blind one, and that is a measured constraint rather than a
    /// preference. A blind isotropic start (n = 1.5 everywhere, uniform absorption, no optical activity)
    /// was tried and does NOT converge for fifteen unknowns: 240 iterations, a final chi-squared of 36.8
    /// against the 1e-23 a converged fit reaches, and g23 out by a factor of six. Nine transparent
    /// unknowns tolerate a blind start; fifteen absorbing ones do not.
    let perturbedStart : AbsorbingTriclinicParameters =
        {
            index1 = RefractionIndex (index1.value * 1.0020)
            index2 = RefractionIndex (index2.value * 1.0010)
            index3 = RefractionIndex (index3.value * 1.0015)
            epsIm11 = EpsValue (epsIm11.value * 1.25)
            epsIm22 = EpsValue (epsIm22.value * 0.80)
            epsIm33 = EpsValue (epsIm33.value * 1.20)
            epsIm23 = EpsValue (epsIm23.value * 0.75)
            epsIm13 = EpsValue (epsIm13.value * 1.30)
            epsIm12 = EpsValue (epsIm12.value * 0.78)
            g11 = RhoValue (g11.value * 1.30)
            g22 = RhoValue (g22.value * 0.70)
            g33 = RhoValue (g33.value * 1.25)
            g23 = RhoValue (g23.value * 0.75)
            g13 = RhoValue (g13.value * 1.35)
            g12 = RhoValue (g12.value * 0.65)
        }

    /// Generous box bounds in SCALED units: ±50 is ±0.05 in refractive index, ±5e-3 in ε″ and ±5e-4 in
    /// gyration, which brackets any physically sensible excursion from the start.
    let searchBox = SearchBox 50.0
