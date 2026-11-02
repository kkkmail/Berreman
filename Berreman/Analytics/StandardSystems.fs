namespace Analytics

open Berreman.MaterialProperties
open Berreman.Media
open Berreman.Fields
open Berreman.Dispersion
open OpticalProperties.Standard
open OpticalProperties.Dispersive
open Variables

module StandardSystems =

    type FixedInfo with

        /// Vacuum / standard transparent glass system with some incident light.
        static member transparentGlass (light : IncidentLightInfo) : FixedInfo =
            { incidentLightInfo = light; opticalSystem = BaseOpticalSystem.transparentGlassSystem.fullSystem.dispersive }


    type OpticalSystemWithDisp with

        /// Langasite thick plate on silicon substrate.
        static member langasiteSubstrateOnSiliconSystem (thickness : Thickness) : OpticalSystemWithDisp =
            {
                description = Some (OpticalSystemDescription "Langasite thick plate on silicon substrate.")
                upperWithDisp = OpticalProperties.vacuum.dispersive
                filmsWithDisp = []
                substrateWithDisp =
                    {
                        thickness = thickness
                        propertiesWithDisp = langasiteOpticalProperties
                    }
                    |> PlateWithDisp
                    |> Some
                lowerWithDisp = siliconOpticalProperties
            }


        /// Langasite thin film on silicon substrate.
        static member langasiteFilmOnSiliconSystem (thickness : Thickness) : OpticalSystemWithDisp =
            {
                description = Some (OpticalSystemDescription "Langasite thin film on silicon substrate.")
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


    /// Vacuum / standard transparent glass film / vacuum system with some incident light.
    let transparentGlassFilm thickness light =
        { incidentLightInfo = light; opticalSystem = (BaseOpticalSystem.transparentGlassFilmSystem thickness).fullSystem.dispersive }


    /// Vacuum / standard biaxial thin film / vacuum system with some incident light.
    let biaxialCrystalFilm thickness light =
        { incidentLightInfo = light; opticalSystem = (BaseOpticalSystem.biaxialCrystalFilmSystem thickness).fullSystem.dispersive }


    /// Langasite thick plate on silicon substrate.
    let langasiteSubstrateOnSilicon thickness light =
        {
            incidentLightInfo = light
            opticalSystem = OpticalSystemWithDisp.langasiteSubstrateOnSiliconSystem thickness
        }


    /// Langasite thin film on silicon substrate.
    let langasiteFilmOnSilicon thickness light =
        {
            incidentLightInfo = light
            opticalSystem = OpticalSystemWithDisp.langasiteFilmOnSiliconSystem thickness
        }
