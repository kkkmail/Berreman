//===========================================================
#load "References.fsx"
//===========================================================
open Berreman.Geometry
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
let incidenceAngleDegree = 0.0

type RefractionIndexThickness =
    | RefractionIndexThickness of RefractionIndex * Thickness


//let nh1 = RefractionIndexThickness (RefractionIndex 1.50, Thickness.mm (0.001))
let nh1 = RefractionIndexThickness (RefractionIndex 1.78052, Thickness.mm (0.001))
let nh2 = RefractionIndexThickness (RefractionIndex 2.25, Thickness.mm 1.00)


let incidentLight = light600nmInclinedDegreeLPs incidenceAngleDegree

let wavelengthRange =
    Range<_>.create numberOfPoints (WaveLength.nm 300.0) (WaveLength.nm 700.0)
    |> WaveLengthRange


let ellipticityRange =
    Range<_>.create numberOfPoints Ellipticity.minValue Ellipticity.maxValue
    |> EllipticityRange


let polarizationRange =
    Range<_>.create numberOfPoints Polarization.s Polarization.p
    |> PolarizationRange


let wedgeAngleRange =
    Range<_>.create numberOfPoints (0.0 |> Angle.degree |> WedgeAngle) (85.0 |> Angle.degree |> WedgeAngle)
    |> WedgeAngleRange


let opticalProperties refractionIndex =
    {
        epsWithDisp = (Eps.fromRefractionIndex refractionIndex).dispersive
        muWithDisp = Mu.vacuum.dispersive
        rhoWithDisp = Rho.vacuum.dispersive
    }


let getGlassInfo useThickPlate nh1 nh2Opt light angle =
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
        | true, Some film2 -> [ film1 ], { layerWithDisp = film2; angle = angle } |> WedgeWithDisp |> Some

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


let zeroDegreeWedge = (0.0 |> Angle.degree |> WedgeAngle)
let thirtyDegreeWedge = (30.0 |> Angle.degree |> WedgeAngle)


//let film = getGlassInfo false nh1 None incidentLight zeroDegreeWedge
//let substrate = getGlassInfo true nh1 None incidentLight thirtyDegreeWedge
let film = getGlassInfo false nh1 (Some nh2) incidentLight zeroDegreeWedge
let substrate = getGlassInfo true nh1 (Some nh2) incidentLight thirtyDegreeWedge


#time
plot substrate fn wedgeAngleRange
//plot substrate fn polarizationRange
plotComparison [ film; substrate ] fn ellipticityRange
//plotComparison [ film; substrate ] fn wavelengthRange
#time
