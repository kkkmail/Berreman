//===========================================================
#load "References.fsx"
//===========================================================
open Berreman.Geometry
open Berreman.FieldFunctions
open Berreman.Media
open Berreman.Fields
open Berreman.Dispersion
open OpticalProperties.Standard
open Analytics.Charting
open Analytics.Variables

//===========================================================
let fn = [ R; T ]

let numberOfPoints = 2000
let polarization = 45.0 |> Angle.degree |> Polarization.create

let light = { light600nmNormalLPs with polarization = polarization }


let wedgeAngleRange =
    Range<_>.create numberOfPoints (0.0 |> Angle.degree |> WedgeAngle) (85.0 |> Angle.degree |> WedgeAngle)
    |> WedgeAngleRange


let wedgeInfo =
    {
        incidentLightInfo = light
        opticalSystem = (OpticalSystem.biaxialCrystalWedgeSystem Thickness.oneMilliMeter (40.0 |> Angle.degree |> WedgeAngle)).dispersive
    }


#time
plot wedgeInfo fn wedgeAngleRange
#time
