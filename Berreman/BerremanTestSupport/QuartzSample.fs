namespace BerremanTestSupport

open Berreman.Constants
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization.MuellerFitting

/// Spec 0044's quartz ground truth and its C1-C4 measurement design, shared by `MuellerInverseTests` and the
/// Planner's acceptance facts (spec 0050 S14.1).
module QuartzSample =

    // =================================================================================================
    // Ground truth: quartz at 632.8 nm.
    //
    // Spec R3 puts the material in test code rather than in the OpticalProperties library. The library has
    // no quartz: the gyration constants exist only as a fixture in RhoWithDispValueTests and as a doc
    // comment in MaterialComplexityEditor, and there is no quartz permittivity anywhere. Rather than add a
    // shipped material for a single test, the constants live here with their provenance.
    //
    // Two things a reader must know about these numbers, so that nobody mistakes a synthetic round trip
    // for a literature validation:
    //
    //   * g33 agrees with the literature almost exactly (10.1 here vs Arteaga/Canillas/Jellison's
    //     10.06 ± 0.07 e-5 at 632.8 nm), but g11 sits at the HIGH end (5.9 vs 4.8 ± 0.5 e-5; ratio -0.584
    //     vs -0.486). Since the forward data is generated FROM these values, they are the ground truth by
    //     definition and the discrepancy cannot affect pass or fail.
    //
    //   * Handedness is one overall sign. Active.assembleRho applies hand.sign to every component, so the
    //     left-handed enantiomorph is the negation of this set. These are the right-handed values.
    // =================================================================================================

    /// He-Ne. The wavelength the quartz literature is densest at, and the one every constant below is
    /// quoted for.
    let waveLength = WaveLength.nm 632.8<nm>

    /// Ghosh 1999 ordinary Sellmeier for alpha-quartz, evaluated at 0.6328 um.
    let quartzOrdinaryIndex = RefractionIndex 1.542606

    /// Ghosh 1999 extraordinary Sellmeier for alpha-quartz, evaluated at 0.6328 um. The birefringence is
    /// therefore n_e - n_o = 0.009045.
    let quartzExtraordinaryIndex = RefractionIndex 1.551651

    /// Gyration component perpendicular to the optic axis (class 32, 24 C). Same value as the existing
    /// RhoWithDispValueTests fixture.
    let quartzG11 = RhoValue 5.9e-5

    /// Gyration component along the optic axis. Same value as the existing RhoWithDispValueTests fixture.
    let quartzG33 = RhoValue -10.1e-5

    /// The ground-truth parameter set the forward data is generated from and the fit must recover.
    let quartz : UniaxialParameters =
        {
            ordinaryIndex = quartzOrdinaryIndex
            extraordinaryIndex = quartzExtraordinaryIndex
            g11 = quartzG11
            g33 = quartzG33
        }

    // =================================================================================================
    // The measurement set (spec §3.4, C1-C4).
    //
    // Why more than one configuration is needed at all. A wave only ever sees the components of the
    // material tensors TRANSVERSE to its own propagation direction, so no single propagation direction can
    // constrain the whole tensor:
    //
    //   z-cut, normal incidence (C1): the wave travels along the optic axis. The two transverse directions
    //           are equivalent, so there is NO linear birefringence at all and the plate is a pure
    //           circular retarder. The transverse Rho components are both g11, so this configuration
    //           measures g11 cleanly and is completely BLIND to g33 (asserted by `MuellerInverseTests`' ablation fact).
    //
    //   x-cut, normal incidence (C3): the wave travels across the optic axis. Now one transverse direction
    //           is ordinary and the other extraordinary, so linear birefringence is maximal and buries the
    //           much smaller optical activity - and the transverse Rho components are g11 and g33, so this
    //           is the configuration that makes g33 reachable at all. It is the hard measurement, exactly
    //           as the analogous case is in the literature.
    //
    // The trap: those two regimes CANNOT be reached from one sample by tilting it. Snell's law caps the
    // internal angle at arcsin(1/n_o) = 40.41 deg for quartz, so a z-cut plate never gets the wave more
    // than 40 deg off its optic axis however far it is tilted. That is why C3/C4 exist, and it is the same
    // constraint that makes the transverse gyration component the poorly-determined one in published
    // measurements (Arteaga/Canillas/Jellison report their axial component to 0.7 % and their transverse
    // one only to ~10 % from a single z-cut crystal). `MuellerInverseTests`' ablation fact asserts the resulting
    // unobservability directly rather than assuming it.
    //
    // Thickness is KNOWN (spec R2) and is not fitted; it is still a DESIGN variable, because it controls
    // the shape of the chi-squared surface:
    //
    //   * C1 is 1.0 mm. It carries NO linear retardance (normal incidence on a z-cut), so thickness costs
    //     nothing here and buys a large, well-conditioned optical rotation of 18.62 deg.
    //   * Every configuration that DOES carry linear retardance is 20 um, which is 0.286 wave at 632.8 nm.
    //     Sub-wave retardance keeps the residual smooth and unimodal in n_e - n_o; at 1 mm the same
    //     configuration would be 14.29 waves and the residual would oscillate into a dense forest of local
    //     minima roughly half a fringe apart.
    //
    // NOTE, and a correction to spec §3.4 as written: the spec put C2 (z-cut, OBLIQUE incidence) at
    // 1.0 mm, on the reasoning that a z-cut plate carries no linear retardance. That reasoning holds only
    // at normal incidence. Tilted, the internal angle is non-zero, the effective birefringence grows as
    // sin^2 t, and a 1 mm z-cut plate at 70 deg incidence carries ~5.9 waves - exactly the multimodality
    // §3.3 exists to avoid. C2 therefore uses the same 20 um plate, which puts it at ~0.12 wave. This
    // applies the spec's own stated rule to a configuration the spec mis-classified.
    // =================================================================================================

    /// A 1.0 mm plate — used only where there is no linear retardance to alias.
    let thickPlate = Thickness.mm 1.0<mm>

    /// A 20 um plate — sub-wave linear retardance, used by every retardance-bearing configuration.
    let thinPlate = Thickness.mkm 20.0<mkm>

    let configuration (cut : SampleCut) (thickness : Thickness) (incidenceDeg : double) (azimuthDeg : double) (observable : Observable) : MeasurementConfiguration =
        configurationOf cut thickness (incidenceDeg * 1.0<deg>) (azimuthDeg * 1.0<deg>) observable waveLength

    /// C1 — z-cut, normal incidence, 1.0 mm. The anchor: a pure optical rotator, and the configuration
    /// that fixes g11 almost independently of everything else — while being completely blind to g33.
    let c1 : MeasurementConfiguration list =
        [ configuration ZCut thickPlate 0.0 0.0 TransmittedMueller ]

    /// C2 — z-cut, oblique incidence sweep, both azimuths, transmission and reflection. Supplies the
    /// Fresnel/angle information that pins the absolute indices, and lets g33 begin to enter as the
    /// internal angle tilts the crystal 3-axis into the transverse plane — weakly, since Snell caps that
    /// tilt at 40.4 deg.
    let c2 : MeasurementConfiguration list =
        [ for incidence in [ 10.0; 30.0; 50.0; 70.0 ] do
            for azimuth in [ 0.0; 45.0 ] do
                for observable in [ TransmittedMueller; ReflectedMueller ] ->
                    configuration ZCut thinPlate incidence azimuth observable ]

    /// C3 — x-cut, normal incidence, four azimuths. The configuration that makes g33 identifiable at all:
    /// the optic axis lies in the surface plane, so the crystal 3-axis is transverse to the propagation
    /// direction and its Rho component finally enters.
    let c3 : MeasurementConfiguration list =
        [ for azimuth in [ 0.0; 22.5; 45.0; 67.5 ] ->
            configuration XCut thinPlate 0.0 azimuth TransmittedMueller ]

    /// C4 — x-cut, oblique sweep, both azimuths, transmission and reflection. A second independent look at
    /// g33 and extra index/angle leverage.
    let c4 : MeasurementConfiguration list =
        [ for incidence in [ 15.0; 45.0 ] do
            for azimuth in [ 0.0; 45.0 ] do
                for observable in [ TransmittedMueller; ReflectedMueller ] ->
                    configuration XCut thinPlate incidence azimuth observable ]

    let fullConfigurations = c1 @ c2 @ c3 @ c4
