namespace BerremanTestSupport

open Berreman.Constants
open Berreman.Media
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization.MuellerFitting
open BerremanTestSupport.BiaxialSample

/// Spec 0044's biaxial measurement design for the triclinic sample, shared by `BiaxialInverseTests` and the Planner's
/// acceptance facts (spec 0050 S14.1).
module BiaxialDesign =

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
        configurationOf cut plate (incidenceDeg * 1.0<deg>) (azimuthDeg * 1.0<deg>) observable referenceWaveLength

    /// The three principal cuts, in the order the transverse planes were listed above.
    let principalCuts : SampleCut list = [ ZCut; XCut; YCut ]

    /// One cut at normal incidence, four azimuths, transmission — B1's block for that cut.
    let normalIncidenceOn (cut : SampleCut) : MeasurementConfiguration list =
        [ for azimuth in [ 0.0; 22.5; 45.0; 67.5 ] -> configuration cut 0.0 azimuth TransmittedMueller ]

    /// B1 — three principal cuts, normal incidence, four azimuths, transmission. The backbone: each cut
    /// contributes its own transverse pair of indices and its own three gyration components.
    let b1 : MeasurementConfiguration list = principalCuts |> List.collect normalIncidenceOn

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
