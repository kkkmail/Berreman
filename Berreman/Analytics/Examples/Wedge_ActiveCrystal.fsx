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
let fn = [ Is; Ip; Rs; Rp; Ts; Tp ]
//let fn = [ Rs; Rp ]

let e11 = 2.315 |> RefractionIndex |> EpsValue.fromRefractionIndex
let e33 = 2.226 |> RefractionIndex |> EpsValue.fromRefractionIndex
let r12 = 1.5e-5 |> RhoValue
let thickness = Thickness.oneCentiMeter
let wedgeAngle = 23.0 |> Angle.degree |> WedgeAngle

let numberOfPoints = 2000
//let polarization = 45.0 |> Angle.degree |> Polarization.create
let polarization = Polarization.s

let light = { light600nmNormalLPs with polarization = polarization }
let d = sprintf "Planar active crystal, r12 = %A, e11 = %A, e33 = %A." r12.value e11.value e33.value
let p = OpticalProperties.planarCrystal e11 e33 r12

let wedgeAngleRange =
    Range<_>.create numberOfPoints (0.0 |> Angle.degree |> WedgeAngle) (85.0 |> Angle.degree |> WedgeAngle)
    |> WedgeAngleRange

let polarizationRange =
    Range<_>.create numberOfPoints Polarization.minusP Polarization.p
    |> PolarizationRange

let wedgeInfo =
    {
        incidentLightInfo = light
        opticalSystem = (OpticalSystem.wedgeSystem p d thickness wedgeAngle).dispersive
    }


#time
plot wedgeInfo fn polarizationRange
#time
