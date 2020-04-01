//===========================================================
#load "References.fsx"
//===========================================================
open Berreman.FieldFunctions
open Berreman.Media
open Berreman.Fields
open Berreman.Dispersion
open Berreman.MaterialProperties
open OpticalProperties.Standard
open Analytics.StandardLightVariables
open Analytics.StandardSystems
open Analytics.Charting
open Analytics.Variables
open OpticalProperties.Dispersive

//===========================================================
let fn = [ R; T ]

let numberOfPoints = 2000
let incidenceAngleDegree = 79.0
let thickness = Thickness.mm 0.001
let refractionIndex = RefractionIndex 1.5
let refractionIndex2 = RefractionIndex 2.25

let incidentLight = light600nmInclinedDegreelLPs incidenceAngleDegree

let wavelengthRange =
    Range<_>.create numberOfPoints (WaveLength.nm 300.0) (WaveLength.nm 700.0)
    |> WaveLengthRange


let incidenceAngleRange =
    Range<_>.create numberOfPoints IncidenceAngle.normal IncidenceAngle.maxValue
    |> IncidenceAngleRange


let ellipticityRange =
    Range<_>.create numberOfPoints Ellipticity.minValue Ellipticity.maxValue
    |> EllipticityRange


let polarizationRange =
    Range<_>.create numberOfPoints Polarization.s Polarization.p
    |> PolarizationRange


let opticalProperties refractionIndex =
    {
        epsWithDisp = (Eps.fromRefractionIndex refractionIndex).dispersive
        muWithDisp = Mu.vacuum.dispersive
        rhoWithDisp = Rho.vacuum.dispersive
    }


let getGlassInfo useThickPlate refractionIndex thickness light =
    let film =
        {
            thickness = thickness
            propertiesWithDisp = opticalProperties refractionIndex
        }

    {
        incidentLightInfo = light
        opticalSystem =
            {
                description = Some ("Glass " + (if useThickPlate then "substrate" else "film"))
                upperWithDisp = OpticalProperties.vacuum.dispersive
                filmsWithDisp = if useThickPlate then [] else [ film ]
                substrateWithDisp = if useThickPlate then Some film else None
                lowerWithDisp = OpticalProperties.vacuum.dispersive
            }
    }


let film = getGlassInfo false refractionIndex thickness incidentLight
let substrate = getGlassInfo true refractionIndex thickness incidentLight

let film2 = getGlassInfo false refractionIndex2 thickness incidentLight
let substrate2 = getGlassInfo true refractionIndex2 thickness incidentLight


#time
//plot film fn polarizationRange
//plot substrate fn polarizationRange
//plotComparison [ film; substrate ] fn ellipticityRange
plotComparison [ film; film2 ] fn incidenceAngleRange
#time
