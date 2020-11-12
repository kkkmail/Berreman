//===========================================================
#load "References.fsx"
//===========================================================
open Berreman.FieldFunctions
open Berreman.Media
open Berreman.Geometry
open Berreman.Fields
open Berreman.Dispersion
open Berreman.MaterialProperties
open OpticalProperties.Standard
open Analytics.Charting
open Analytics.Variables
open Berreman.MaterialProperties

//===========================================================
//let fn = [ R; T ]
//let fn = [ Rp; Rs; Tp; Ts]
let fn = [ Rp; Rs; ]
//let fn = [ Tp; Ts; ]

let numberOfPoints = 2000
let incidenceAngleDegree = 0.0
let polarization = 90.0 |> Angle.degree |> Polarization.create
//let polarization = Polarization.p

let incidentLight = { light600nmInclinedDegreeLPs incidenceAngleDegree with refractionIndex = RefractionIndex.transparentGlass150 }


let incidenceAngleRange =
    Range<_>.create numberOfPoints IncidenceAngle.normal (60.0 |> Angle.degree |> IncidenceAngle)
    |> IncidenceAngleRange


let ellipticityRange =
    Range<_>.create numberOfPoints Ellipticity.minValue Ellipticity.maxValue
    |> EllipticityRange


let polarizationRange =
    Range<_>.create numberOfPoints Polarization.s Polarization.p
    |> PolarizationRange


let getGlassInfo light =
    {
        incidentLightInfo = { light with polarization = polarization }
        opticalSystem =
        {
            description = None
            upperWithDisp = OpticalProperties.transparentGlass150.dispersive
            filmsWithDisp = []
            substrateWithDisp = None
            lowerWithDisp = OpticalProperties.vacuum.dispersive
        }
    }


let info = getGlassInfo incidentLight


#time
plot info fn incidenceAngleRange
#time
