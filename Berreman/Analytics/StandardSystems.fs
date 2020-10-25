namespace Analytics

open Berreman.MaterialProperties
open Berreman.Media
open Berreman.Dispersion
open OpticalProperties.Standard
open OpticalProperties.Dispersive
open Variables

module StandardSystems =
    /// Vacuum / standard transparent glass system with some incident light.
    let transparentGlass light =
        { incidentLightInfo = light; opticalSystem = BaseOpticalSystem.transparentGlassSystem.fullSystem.dispersive }


    /// Vacuum / standard transparent glass film / vacuum system with some incident light.
    let transparentGlassFilm thickness light =
        { incidentLightInfo = light; opticalSystem = (BaseOpticalSystem.transparentGlasslFilmSystem thickness).fullSystem.dispersive }


    /// Vacuum / standard biaxial thin film / vacuum system with some incident light.
    let biaxialCrystalFilm thickness light =
        { incidentLightInfo = light; opticalSystem = (BaseOpticalSystem.biaxialCrystalFilmSystem thickness).fullSystem.dispersive }


    /// Langasite thick plate on silicon substrate.
    let langasiteSubstrateOnSiliconSystem thickness =
        {
            description = Some "Langasite thick plate on silicon substrate."
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
    let langasiteFilmOnSiliconSystem thickness =
        {
            description = Some "Langasite thin film on silicon substrate."
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


    /// Langasite thick plate on silicon substrate.
    let langasiteSubstrateOnSilicon thickness light =
        {
            incidentLightInfo = light
            opticalSystem = langasiteSubstrateOnSiliconSystem thickness
        }


    /// Langasite thin film on silicon substrate.
    let langasiteFilmOnSilicon thickness light =
        {
            incidentLightInfo = light
            opticalSystem = langasiteFilmOnSiliconSystem thickness
        }
