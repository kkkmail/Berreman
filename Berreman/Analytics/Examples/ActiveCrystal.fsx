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

let numberOfPoints = 2000

//let polarization = 45.0 |> Angle.degree |> Polarization.create
let polarization = Polarization.s

let light = { light600nmNormalLPs with polarization = polarization }
let d = sprintf "Planar active crystal plate, r12 = %A, n11 = %A, n33 = %A." r12.value e11.refractionIndex.value e33.refractionIndex.value
let d1 = sprintf "Planar active crystal plate, n11 = %A, n33 = %A." e11.refractionIndex.value e33.refractionIndex.value
let p = OpticalProperties.planarCrystal e11 e33 r12

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

let plateInfo =
    {
        incidentLightInfo = light
        opticalSystem = (OpticalSystem.plateSystem p d thickness).dispersive
    }

let plateInfo1 =
    {
        incidentLightInfo = light
        opticalSystem = (OpticalSystem.plateSystem p d1 thickness).dispersive
    }


#time
//plot plateInfo fn polarizationRange
plot plateInfo1 fn1 g12Range
#time
