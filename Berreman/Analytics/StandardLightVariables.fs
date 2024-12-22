namespace Analytics

open Berreman.Fields
open Variables
open Berreman.Geometry
open Berreman.Constants

module StandardLightVariables =

    /// Incidence angle variable from 0 to 40 degrees.
    let incidenceAngle40 n = Range<_>.create n IncidenceAngle.normal (IncidenceAngle.create (Angle.degree 40.0))


    /// Incidence angle variable from 0 to 60 degrees.
    let incidenceAngle60 n = Range<_>.create n IncidenceAngle.normal (IncidenceAngle.create (Angle.degree 60.0))


    /// Incidence angle variable from 0 to 80 degrees.
    let incidenceAngle80 n = Range<_>.create n IncidenceAngle.normal IncidenceAngle.maxValue80

    //===========================================================

    /// Incidence angle variable from 0 to 89 degrees.
    let incidenceAngle n = Range<_>.create n IncidenceAngle.normal IncidenceAngle.maxValue


    /// Incidence angle from 0 to 40 degrees.
    let incidenceAngleRange40 n = incidenceAngle40 n |> IncidenceAngleRange


    /// Incidence angle from 0 to 60 degrees.
    let incidenceAngleRange60 n = incidenceAngle60 n |> IncidenceAngleRange


    /// Incidence angle from 0 to 80 degrees.
    let incidenceAngleRange80 n = incidenceAngle80 n |> IncidenceAngleRange


    /// Incidence angle from 0 to 89 degrees.
    let incidenceAngleRange n = incidenceAngle n |> IncidenceAngleRange

    //===========================================================

    /// Ellipticity variable from 0 to 1.
    let ellipticity n = Range<_>.create n Ellipticity.defaultValue Ellipticity.maxValue

    /// Ellipticity variable from 0 to 1.
    let ellipticityRange n = ellipticity n |> EllipticityRange

    //===========================================================

    /// Polarization variable from 0 to 90 degrees.
    let polarization n = Range<_>.create n Polarization.s Polarization.p

    /// Polarization variable from 0 to 90 degrees.
    let polarizationRange n = polarization n |> PolarizationRange

    //===========================================================

    /// Wavelength variable from 200 to 800 nm.
    let wavelength200to800 n = Range<_>.create n (WaveLength.nm 200.0<nm>) (WaveLength.nm 800.0<nm>)

    /// Wavelength variable from 200 to 800 nm.
    let wavelength200to800Range n = wavelength200to800 n |> WaveLengthRange


    /// Wavelength variable from 500 to 700 nm.
    let wavelength500to700 n = Range<_>.create n (WaveLength.nm 500.0<nm>) (WaveLength.nm 700.0<nm>)

    /// Wavelength variable from 500 to 700 nm.
    let wavelength500to700Range n = wavelength500to700 n |> WaveLengthRange


    /// Wavelength variable from 250 to 600 nm.
    let wavelength250to600 n = Range<_>.create n (WaveLength.nm 250.0<nm>) (WaveLength.nm 600.0<nm>)

    /// Wavelength variable from 250 to 600 nm.
    let wavelength250to600Range n = wavelength250to600 n |> WaveLengthRange

    // ========================================================
    // EUV

    /// Wavelength variable from 200 to 800 nm.
    let wavelength05to20 n = Range<_>.create n (WaveLength.nm 5.0<nm>) (WaveLength.nm 20.0<nm>)

    /// Wavelength variable from 200 to 800 nm.
    let wavelength05to20Range n = wavelength05to20 n |> WaveLengthRange
