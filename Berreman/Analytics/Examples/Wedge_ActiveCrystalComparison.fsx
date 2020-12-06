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

let e11 = 1.5 |> RefractionIndex |> EpsValue.fromRefractionIndex
let e33 = 1.53 |> RefractionIndex |> EpsValue.fromRefractionIndex

let g11 = 5.0e-5 |> RhoValue
let g12 = 7.0e-6 |> RhoValue
let g33 = -5.0e-5 |> RhoValue

let g12MaxVal = 5.0e-04
let g11MaxVal = 5.0e-04
let g33MaxVal = 5.0e-04

let thickness = Thickness.oneCentiMeter

let wedgeAngle = 23.0 |> Angle.degree |> WedgeAngle
//let wedgeAngle = 25.592 |> Angle.degree |> WedgeAngle
//let wedgeAngle = 25.5925 |> Angle.degree |> WedgeAngle

let numberOfPoints = 2000
//let polarization = 45.0 |> Angle.degree |> Polarization.create
let polarization = Polarization.s

let light = { light600nmNormalLPs with polarization = polarization }
let d = sprintf "Planar active crystal wedge %A degrees, r12 = %A, n11 = %A, n33 = %A." wedgeAngle.degrees g12.value e11.refractionIndex.value e33.refractionIndex.value
let dPlanar = sprintf "Planar active crystal wedge %A degrees, n11 = %A, n33 = %A." wedgeAngle.degrees e11.refractionIndex.value e33.refractionIndex.value
let dType3_4_6 = sprintf "3, 4, 6 active crystal wedge %A degrees, n11 = %A, n33 = %A." wedgeAngle.degrees e11.refractionIndex.value e33.refractionIndex.value
let dType32_42_62 = sprintf "32, 42, 62 active crystal wedge %A degrees, n11 = %A, n33 = %A." wedgeAngle.degrees e11.refractionIndex.value e33.refractionIndex.value

let planar = OpticalProperties.planarCrystal e11 e33 g12
let type3_4_6 = OpticalProperties.type_3_4_6_Crystal e11 e33 g11 g33
let type32_42_62 = OpticalProperties.type_32_42_62_Crystal e11 e33 g11 g12 g33


let wedgeAngleRange =
    Range<_>.create numberOfPoints (0.0 |> Angle.degree |> WedgeAngle) (85.0 |> Angle.degree |> WedgeAngle)
    |> WedgeAngleRange


let polarizationRange =
    Range<_>.create numberOfPoints Polarization.minusP Polarization.p
    |> PolarizationRange


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

let wedgeInfoPlanar =
    {
        incidentLightInfo = light
        opticalSystem = (OpticalSystem.wedgeSystem planar d thickness wedgeAngle).dispersive
    }

let wedgeInfoType3_4_6 =
    {
        incidentLightInfo = light
        opticalSystem = (OpticalSystem.wedgeSystem type3_4_6 dType3_4_6 thickness wedgeAngle).dispersive
    }

let wedgeInfoType32_42_62 =
    {
        incidentLightInfo = light
        opticalSystem = (OpticalSystem.wedgeSystem type32_42_62 dType32_42_62 thickness wedgeAngle).dispersive
    }

#time
//plot wedgeInfo fn polarizationRange
//plot wedgeInfoPlanar fn1 (g12Range updateG12Planar)

//plot wedgeInfoType3_4_6 fn1 (g11Range updateG11Type3_4_6)
//plot wedgeInfoType3_4_6 fn1 (g33Range updateG33Type3_4_6)

plot wedgeInfoType32_42_62 fn1 (g11Range updateG11Type32_42_62)
plot wedgeInfoType32_42_62 fn1 (g12Range updateG12Type32_42_62)
plot wedgeInfoType32_42_62 fn1 (g33Range updateG33Type32_42_62)
#time
