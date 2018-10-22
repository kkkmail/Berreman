namespace Analytics

open Berreman.Fields
open Variables

module StandardLightVariables = 
    /// Incident light variable from 0 to 89 degrees
    let incidenceAngle n = Range<_>.create n IncidenceAngle.normal IncidenceAngle.maxValue
    let incidenceAngleRange n = incidenceAngle n |> IncidenceAngleRange


    /// Ellipticity variable from 0 to 1.
    let ellipticity n = Range<_>.create n Ellipticity.defaultValue Ellipticity.maxValue
    let ellipticityRange n = ellipticity n |> EllipticityRange


    /// Polarization variable from 0 to 90 degrees.
    let polarization n = Range<_>.create n Polarization.s Polarization.p
    let polarizationRange n = polarization n |> PolarizationRange


    /// Wavelength variable from 200 to 800 nm.
    let wavelength200to800 n = Range<_>.create n (WaveLength.nm 200.0) (WaveLength.nm 800.0)
    let wavelength200to800Range n = wavelength200to800 n |> WaveLengthRange


    /// Wavelength variable from 500 to 700 nm.
    let wavelength500to700 n = Range<_>.create n (WaveLength.nm 500.0) (WaveLength.nm 700.0)
    let wavelength500to700Range n = wavelength500to700 n |> WaveLengthRange


    /// Wavelength variable from 250 to 600 nm.
    let wavelength250to600 n = Range<_>.create n (WaveLength.nm 250.0) (WaveLength.nm 600.0)
    let wavelength250to600Range n = wavelength250to600 n |> WaveLengthRange
