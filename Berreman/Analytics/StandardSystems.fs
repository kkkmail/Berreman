namespace Analytics

open FSharp.Collections.ParallelSeq
open System.Numerics
open MathNet.Numerics.LinearAlgebra
open Berreman.MathNetNumericsMath
open Berreman.MatrixExp
open Berreman.Constants
open Berreman.Fields
open Berreman.BerremanMatrix
open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.Dispersion
open Berreman.Solvers
open Berreman.FieldFunctions
open OpticalProperties.Standard
open OpticalProperties.Dispersive
open Berreman
open Variables

module StandardSystems = 
    /// Incident light variable from 0 to 89 degrees
    let incidenceAngleRange n = IncidenceAngleRange (Range<_>.create n IncidenceAngle.normal IncidenceAngle.maxValue)

    /// Ellipticity variable from 0 to 1.
    let ellipticityRange n = EllipticityRange (Range<_>.create n Ellipticity.defaultValue Ellipticity.maxValue)

    /// Polarization variable from 0 to 90 degrees.
    let polarizationRange n = PolarizationRange (Range<_>.create n Polarization.s Polarization.p)

    /// Wavelength variable from 200 to 800 nm.
    let wavelength200to800 n = Range<_>.create n (WaveLength.nm 200.0) (WaveLength.nm 800.0)
    let wavelength200to800Range n = WaveLengthRange (Range<_>.create n (WaveLength.nm 200.0) (WaveLength.nm 800.0))

    /// Wavelength variable from 500 to 700 nm.
    let wavelength500to700 n = Range<_>.create n (WaveLength.nm 500.0) (WaveLength.nm 700.0)
    let wavelength500to700Range n = WaveLengthRange (Range<_>.create n (WaveLength.nm 500.0) (WaveLength.nm 700.0))

    /// Wavelength variable from 250 to 600 nm.
    let wavelength250to600 n = Range<_>.create n (WaveLength.nm 250.0) (WaveLength.nm 600.0)
    let wavelength250to600Range n = WaveLengthRange (Range<_>.create n (WaveLength.nm 250.0) (WaveLength.nm 600.0))

    /// Vacuum / standard transparent glass system with s polarized light falling at normal.
    let transpGlass600nmNormalLPs = 
        { incidentLightInfo = light600nmNormalLPs; opticalSystem = BaseOpticalSystem.transparentGlassSystem.fullSystem.dispersive }

    /// Vacuum / standard transparent glass system with s polarized light falling at angleDegree.
    let transpGlass600nmInclindedLPs angleDegree = 
        { incidentLightInfo = light600nmInclinedDegreelLPs angleDegree; opticalSystem = BaseOpticalSystem.transparentGlassSystem.fullSystem.dispersive }

    /// Vacuum / standard transparent glass film / vacuum system with s polarized light falling at normal.
    let transparentGassFilm600nmNormalLPs thickness = 
        { incidentLightInfo = light600nmNormalLPs; opticalSystem = (BaseOpticalSystem.transparentGlasslFilmSystem thickness).fullSystem.dispersive }

    /// Vacuum / standard transparent glass film / vacuum system with s polarized light falling at angleDegree.
    let transparentGassFilm600nmInclindedLPs angleDegree thickness = 
        { incidentLightInfo = light600nmInclinedDegreelLPs angleDegree; opticalSystem = (BaseOpticalSystem.transparentGlasslFilmSystem thickness).fullSystem.dispersive }

    /// Vacuum / standard biaxial thin film / vacuum system with s polarized light falling at normal.
    let biaxialCrystalFilm600nmNormalLPs thickness = 
        { incidentLightInfo = light600nmNormalLPs; opticalSystem = (BaseOpticalSystem.biaxialCrystalFilmSystem thickness).fullSystem.dispersive }

    /// Vacuum / standard biaxial thin film / vacuum system with s polarized light falling at angleDegree.
    let biaxialCrystalFilm600nmInclindedLPs angleDegree thickness = 
        { incidentLightInfo = light600nmInclinedDegreelLPs angleDegree; opticalSystem = (BaseOpticalSystem.biaxialCrystalFilmSystem thickness).fullSystem.dispersive }


    /// Langasite thick plate of silicon sbstrate.
    let langasiteSubstrateOnSiliconSystem thickness = 
        {
            description = Some "Langasite thick plate of silicon sbstrate."
            upperWithDisp = OpticalProperties.vacuum.dispersive
            filmsWithDisp = []
            substrateWithDisp = 
                {
                    thickness = thickness
                    propertiesWithDisp = langasiteOpticalProperties
                }
                |> Some
            lowerWithDisp = siliconOpticalProperties
        }

    /// Langasite thin film of silicon sbstrate.
    let langasiteFilmOnSiliconSystem thickness = 
        {
            description = Some "Langasite thin film of silicon sbstrate."
            upperWithDisp = OpticalProperties.vacuum.dispersive
            filmsWithDisp = 
                [
                    {
                        thickness = thickness
                        propertiesWithDisp = langasiteOpticalProperties
                    }
                ]
            substrateWithDisp = None
            lowerWithDisp = siliconOpticalProperties
        }


    /// Langasite thick plate of silicon sbstrate.
    let langasiteSubstrateOnSilicon angleDegree thickness = 
        { 
            incidentLightInfo = light600nmInclinedDegreelLPs angleDegree
            opticalSystem = langasiteSubstrateOnSiliconSystem thickness
        }


    let langasiteFilmOnSilicon angleDegree thickness = 
        { 
            incidentLightInfo = light600nmInclinedDegreelLPs angleDegree
            opticalSystem = langasiteFilmOnSiliconSystem thickness 
        }
