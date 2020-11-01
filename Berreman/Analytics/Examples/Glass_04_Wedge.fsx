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
open Analytics.Charting
open Analytics.Variables

//===========================================================
let fn = [ R; T ]

let numberOfPoints = 2000
let polarization = Polarization.p

let light = light600nmNormalLPs


let wedgeAngleRange =
    Range<_>.create numberOfPoints (0.0 |> Angle.degree |> WedgeAngle) (85.0 |> Angle.degree |> WedgeAngle)
    |> WedgeAngleRange


let wedgeInfo =
    {
        incidentLightInfo = { light with polarization = polarization }
        opticalSystem = OpticalSystem.wedgelGlass150System.dispersive
    }


#time
plot wedgeInfo fn wedgeAngleRange
#time
