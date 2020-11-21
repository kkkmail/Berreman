//===========================================================
#load "References.fsx"
//===========================================================
open Berreman.FieldFunctions
open Berreman.Media
open Berreman.Fields
open Berreman.Dispersion
open Berreman.MaterialProperties
open OpticalProperties.Standard
open Analytics.Charting
open Analytics.Variables

//===========================================================
let fn = [ R; T ]

let numberOfPoints = 2000
let incidenceAngleDegree = 79.0

type RefractionIndexThickness =
    | RefractionIndexThickness of RefractionIndex * Thickness


//let nh1 = RefractionIndexThickness (RefractionIndex 1.50, Thickness.mm (0.001))
let nh1 = RefractionIndexThickness (RefractionIndex 1.78052, Thickness.mm (0.001))
let nh2 = RefractionIndexThickness (RefractionIndex 2.25, Thickness.mm 1.00)


let incidentLight = light600nmInclinedDegreeLPs incidenceAngleDegree

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


let getGlassInfo useThickPlate nh1 nh2Opt light =
    let (RefractionIndexThickness (n1, h1)) = nh1

    let film1 =
        {
            thickness = h1
            propertiesWithDisp = opticalProperties n1
        }

    let film2Opt =
        match nh2Opt with
        | Some (RefractionIndexThickness (n2, h2)) ->
            {
                thickness = h2
                propertiesWithDisp = opticalProperties n2
            }
            |> Some
        | None -> None


    let (films, substr) =
        match useThickPlate, film2Opt with
        | false, None -> [ film1 ], None
        | false, Some film2 -> [ film1; film2 ], None
        | true, None -> [], film1 |> PlateWithDisp |> Some
        | true, Some film2 -> [ film1 ], film2 |> PlateWithDisp |> Some

    {
        incidentLightInfo = light
        opticalSystem =
            {
                description = Some ("Glass " + (if useThickPlate then "substrate" else "film"))
                upperWithDisp = OpticalProperties.vacuum.dispersive
                filmsWithDisp = films
                substrateWithDisp = substr
                lowerWithDisp = OpticalProperties.vacuum.dispersive
            }
    }


let film = getGlassInfo false nh1 None incidentLight
let substrate = getGlassInfo true nh1 None incidentLight
//let film = getGlassInfo false nh1 (Some nh2) incidentLight
//let substrate = getGlassInfo true nh1 (Some nh2) incidentLight


#time
//plot film fn polarizationRange
//plot substrate fn polarizationRange
//plotComparison [ film; substrate ] fn ellipticityRange
plotComparison [ film; substrate ] fn incidenceAngleRange
//plotComparison [ film; substrate ] fn wavelengthRange
#time
