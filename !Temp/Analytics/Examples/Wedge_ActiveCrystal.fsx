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
let fn1 = [ Ts; Tp ]

let e11 = 2.315 |> RefractionIndex |> EpsValue.fromRefractionIndex
let e33 = 2.226 |> RefractionIndex |> EpsValue.fromRefractionIndex
let r12 = 1.5e-6 |> RhoValue
let r12MaxVal = 5.0e-04
let thickness = Thickness.oneCentiMeter

let wedgeAngle = 23.0 |> Angle.degree |> WedgeAngle
//let wedgeAngle = 25.592 |> Angle.degree |> WedgeAngle
//let wedgeAngle = 25.5925 |> Angle.degree |> WedgeAngle

let numberOfPoints = 2000
//let polarization = 45.0 |> Angle.degree |> Polarization.create
let polarization = Polarization.s

let light = { light600nmNormalLPs with polarization = polarization }
let d = sprintf "Planar active crystal wedge %A degrees, r12 = %A, n11 = %A, n33 = %A." wedgeAngle.degrees r12.value e11.refractionIndex.value e33.refractionIndex.value
let d1 = sprintf "Planar active crystal wedge %A degrees, n11 = %A, n33 = %A." wedgeAngle.degrees e11.refractionIndex.value e33.refractionIndex.value
let p = OpticalProperties.planarCrystal e11 e33 r12

let wedgeAngleRange =
    Range<_>.create numberOfPoints (0.0 |> Angle.degree |> WedgeAngle) (85.0 |> Angle.degree |> WedgeAngle)
    |> WedgeAngleRange

let polarizationRange =
    Range<_>.create numberOfPoints Polarization.minusP Polarization.p
    |> PolarizationRange


let updateG12 (o: OpticalSystem) r =
    match o.substrate with
    | Some (Wedge w) ->
        { o with substrate = { w with layer = { w.layer with properties = { w.layer.properties with rho = r |> RhoValue |> Rho.planarCrystal } } } |> Wedge |> Some}
    | _ -> o

let g12Range =
    {
        variableName = "g12"
        range =  Range<_>.create numberOfPoints 0.0 r12MaxVal
        scale = 1.0
        getSys = updateG12
    }
    |> ArbitraryVariableRange

let wedgeInfo =
    {
        incidentLightInfo = light
        opticalSystem = (OpticalSystem.wedgeSystem p d thickness wedgeAngle).dispersive
    }

let wedgeInfo1 =
    {
        incidentLightInfo = light
        opticalSystem = (OpticalSystem.wedgeSystem p d1 thickness wedgeAngle).dispersive
    }


#time
//plot wedgeInfo fn polarizationRange
plot wedgeInfo1 fn1 g12Range
#time
