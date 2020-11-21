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
open OpticalProperties.Active
open Analytics.Charting
open Analytics.Variables

//===========================================================
let fn = [ Rs; Rp; Ts; Tp ]
//let fn = [ Rs; Rp ]

let e11 = 1.5 * 1.5 |> EpsValue
let e33 = 1.7 * 1.7 |> EpsValue
let r12 = 1.0e-4 |> RhoValue
let thickness = Thickness.OneMilliMeter

let numberOfPoints = 2000
let polarization = 45.0 |> Angle.degree |> Polarization.create

let light = { light600nmNormalLPs with polarization = polarization }
let d = "Planar active crystal."
let p = OpticalProperties.planarCrystal e11 e33 r12

let wedgeAngleRange =
    Range<_>.create numberOfPoints (0.0 |> Angle.degree |> WedgeAngle) (85.0 |> Angle.degree |> WedgeAngle)
    |> WedgeAngleRange


let wedgeInfo =
    {
        incidentLightInfo = light
        opticalSystem = (OpticalSystem.wedgeSystem p d thickness (40.0 |> Angle.degree |> WedgeAngle)).dispersive
    }


#time
plot wedgeInfo fn wedgeAngleRange
#time
