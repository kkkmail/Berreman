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

let g11 = 1.0e-4 |> RhoValue
let g12 = 1.0e-3 |> RhoValue
let g33 = -2.0e-4 |> RhoValue

let g12MaxVal = 5.0e-04
let g11MaxVal = 5.0e-04
let g33MaxVal = 5.0e-04

let thickness = Thickness.oneMilliMeter

let numberOfPoints = 2000
//let polarization = 45.0 |> Angle.degree |> Polarization.create
let polarization = Polarization.s

let light = { light600nmNormalLPs with polarization = polarization }
let d = sprintf "Planar active crystal plate, r12 = %A, n11 = %A, n33 = %A." g12.value e11.refractionIndex.value e33.refractionIndex.value
let dPlanar = sprintf "Planar active crystal plate, n11 = %A, n33 = %A." e11.refractionIndex.value e33.refractionIndex.value
let dType3_4_6 = sprintf "3, 4, 6 active crystal plate, n11 = %A, n33 = %A." e11.refractionIndex.value e33.refractionIndex.value
let dType32_42_62 = sprintf "32, 42, 62 active crystal plate, n11 = %A, n33 = %A." e11.refractionIndex.value e33.refractionIndex.value

let planar = OpticalProperties.planarCrystal e11 e33 g12
let type3_4_6 = OpticalProperties.type_3_4_6_Crystal e11 e33 g11 g33
let type32_42_62 = OpticalProperties.type_32_42_62_Crystal e11 e33 g11 g12 g33


let polarizationRange =
    Range<_>.create numberOfPoints Polarization.minusP Polarization.p
    |> PolarizationRange


let incidenceAngleRange =
    Range<_>.create numberOfPoints IncidenceAngle.normal IncidenceAngle.maxValue
    |> IncidenceAngleRange


let updateRhoPlanar w g12 =
    { w with layer = { w.layer with properties = { w.layer.properties with rho = Rho.planarCrystal g12 } } }


let updateRhoType3_4_6 w g11 g33 =
    { w with layer = { w.layer with properties = { w.layer.properties with rho = Rho.type_3_4_6_Crystal g11 g33 } } }


let updateRhoType32_42_62 w g11 g12 g33 =
    { w with layer = { w.layer with properties = { w.layer.properties with rho = Rho.type_32_42_62_Crystal g11 g12 g33 } } }


let updateG12Planar (o: OpticalSystem) g12 =
    match o.substrate with
    | Some (Wedge w) -> { o with substrate = (w, RhoValue g12) ||> updateRhoPlanar |> Wedge |> Some}
    | _ -> o


let updateG11Type3_4_6 (o: OpticalSystem) g11 =
    match o.substrate with
    | Some (Wedge w) -> { o with substrate = (w, RhoValue g11, g33) |||> updateRhoType3_4_6 |> Wedge |> Some}
    | _ -> o


let updateG33Type3_4_6 (o: OpticalSystem) g33 =
    match o.substrate with
    | Some (Wedge w) -> { o with substrate = (w, g11, RhoValue g33) |||> updateRhoType3_4_6 |> Wedge |> Some}
    | _ -> o


let updateG11Type32_42_62 (o: OpticalSystem) g11 =
    match o.substrate with
    | Some (Wedge w) -> { o with substrate = updateRhoType32_42_62 w (RhoValue g11) g12 g33 |> Wedge |> Some}
    | _ -> o


let updateG12Type32_42_62 (o: OpticalSystem) g12 =
    match o.substrate with
    | Some (Wedge w) -> { o with substrate = updateRhoType32_42_62 w g11 (RhoValue g12) g33 |> Wedge |> Some}
    | _ -> o


let updateG33Type32_42_62 (o: OpticalSystem) g33 =
    match o.substrate with
    | Some (Wedge w) -> { o with substrate = updateRhoType32_42_62 w g11 g12 (RhoValue g33) |> Wedge |> Some}
    | _ -> o


let g11Range update =
    {
        variableName = "g11"
        range =  Range<_>.create numberOfPoints 0.0 g11MaxVal
        scale = 1.0
        getSys = update
    }
    |> ArbitraryVariableRange

let g12Range update =
    {
        variableName = "g12"
        range =  Range<_>.create numberOfPoints 0.0 g12MaxVal
        scale = 1.0
        getSys = update
    }
    |> ArbitraryVariableRange

let g33Range update =
    {
        variableName = "g33"
        range =  Range<_>.create numberOfPoints (-g33MaxVal) g33MaxVal
        scale = 1.0
        getSys = update
    }
    |> ArbitraryVariableRange

let plateInfoPlanar =
    {
        incidentLightInfo = light
        opticalSystem = (OpticalSystem.plateSystem planar d thickness).dispersive
    }

let plateInfoType3_4_6 =
    {
        incidentLightInfo = light
        opticalSystem = (OpticalSystem.plateSystem type3_4_6 dType3_4_6 thickness).dispersive
    }

let plateInfoType32_42_62 =
    {
        incidentLightInfo = light
        opticalSystem = (OpticalSystem.plateSystem type32_42_62 dType32_42_62 thickness).dispersive
    }

#time
plot plateInfoPlanar fn1 incidenceAngleRange
plot plateInfoType3_4_6 fn1 incidenceAngleRange
plot plateInfoType32_42_62 fn1 incidenceAngleRange
//===========================================================
//plot plateInfoPlanar fn1 (g12Range updateG12Planar)

//plot plateInfoType3_4_6 fn1 (g11Range updateG11Type3_4_6)
//plot plateInfoType3_4_6 fn1 (g33Range updateG33Type3_4_6)

//plot plateInfoType32_42_62 fn1 (g11Range updateG11Type32_42_62)
//plot plateInfoType32_42_62 fn1 (g12Range updateG12Type32_42_62)
//plot plateInfoType32_42_62 fn1 (g33Range updateG33Type32_42_62)
#time
