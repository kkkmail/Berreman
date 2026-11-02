namespace Analytics

open Berreman.Fields
open Variables
open Berreman.Geometry
open Berreman.Constants

module StandardLightVariables =

    type NumberOfPoints with

        /// Incidence angle variable from 0 to 40 degrees.
        member this.incidenceAngle40 : Range<IncidenceAngle> = Range<_>.create this IncidenceAngle.normal (IncidenceAngle.create (Angle.degree 40.0<deg>))


        /// Incidence angle variable from 0 to 60 degrees.
        member this.incidenceAngle60 : Range<IncidenceAngle> = Range<_>.create this IncidenceAngle.normal (IncidenceAngle.create (Angle.degree 60.0<deg>))


        /// Incidence angle variable from 0 to 80 degrees.
        member this.incidenceAngle80 : Range<IncidenceAngle> = Range<_>.create this IncidenceAngle.normal IncidenceAngle.maxValue80

        //===========================================================

        /// Incidence angle variable from 0 to 89 degrees.
        member this.incidenceAngle : Range<IncidenceAngle> = Range<_>.create this IncidenceAngle.normal IncidenceAngle.maxValue


        /// Incidence angle from 0 to 40 degrees.
        member this.incidenceAngleRange40 : RangedVariable = this.incidenceAngle40 |> IncidenceAngleRange


        /// Incidence angle from 0 to 60 degrees.
        member this.incidenceAngleRange60 : RangedVariable = this.incidenceAngle60 |> IncidenceAngleRange


        /// Incidence angle from 0 to 80 degrees.
        member this.incidenceAngleRange80 : RangedVariable = this.incidenceAngle80 |> IncidenceAngleRange


        /// Incidence angle from 0 to 89 degrees.
        member this.incidenceAngleRange : RangedVariable = this.incidenceAngle |> IncidenceAngleRange

        //===========================================================

        /// Ellipticity variable from 0 to 1.
        member this.ellipticity : Range<Ellipticity> = Range<_>.create this Ellipticity.defaultValue Ellipticity.maxValue

        /// Ellipticity variable from 0 to 1.
        member this.ellipticityRange : RangedVariable = this.ellipticity |> EllipticityRange

        //===========================================================

        /// Polarization variable from 0 to 90 degrees.
        member this.polarization : Range<Polarization> = Range<_>.create this Polarization.s Polarization.p

        /// Polarization variable from 0 to 90 degrees.
        member this.polarizationRange : RangedVariable = this.polarization |> PolarizationRange

        //===========================================================

        /// Wavelength variable from 200 to 800 nm.
        member this.wavelength200to800 : Range<WaveLength> = Range<_>.create this (WaveLength.nm 200.0<nm>) (WaveLength.nm 800.0<nm>)

        /// Wavelength variable from 200 to 800 nm.
        member this.wavelength200to800Range : RangedVariable = this.wavelength200to800 |> WaveLengthRange


        /// Wavelength variable from 500 to 700 nm.
        member this.wavelength500to700 : Range<WaveLength> = Range<_>.create this (WaveLength.nm 500.0<nm>) (WaveLength.nm 700.0<nm>)

        /// Wavelength variable from 500 to 700 nm.
        member this.wavelength500to700Range : RangedVariable = this.wavelength500to700 |> WaveLengthRange


        /// Wavelength variable from 250 to 600 nm.
        member this.wavelength250to600 : Range<WaveLength> = Range<_>.create this (WaveLength.nm 250.0<nm>) (WaveLength.nm 600.0<nm>)

        /// Wavelength variable from 250 to 600 nm.
        member this.wavelength250to600Range : RangedVariable = this.wavelength250to600 |> WaveLengthRange

        // ========================================================
        // EUV

        /// Wavelength variable from 5 to 20 nm.
        member this.wavelength05to20 : Range<WaveLength> = Range<_>.create this (WaveLength.nm 5.0<nm>) (WaveLength.nm 20.0<nm>)

        /// Wavelength variable from 5 to 20 nm.
        member this.wavelength05to20Range : RangedVariable = this.wavelength05to20 |> WaveLengthRange
