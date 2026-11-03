//===========================================================
#load "References.fsx"
//===========================================================
open Berreman.FieldFunctions
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.Dispersion
open OpticalProperties.Standard
open Analytics.Variables
open Analytics.StandardLightVariables
open Analytics.Charting
open Berreman.Constants
//===========================================================
let fn = [ R; T ]

let numberOfPoints = NumberOfPoints 2000
let numberOfPoints3D = NumberOfPoints 100
let i = numberOfPoints.incidenceAngleRange
let e = numberOfPoints.ellipticityRange
let p = numberOfPoints.polarizationRange
let w = numberOfPoints.wavelength500to700Range

let i3D = numberOfPoints3D.incidenceAngleRange
let e3D = numberOfPoints3D.ellipticityRange
let p3D = numberOfPoints3D.polarizationRange
let w3D = numberOfPoints3D.wavelength200to800Range


let thickness1 = Thickness.nm ((600.0 / 1.75 / 4.0) * 1.0<nm>)
let thickness2 = Thickness.nm ((600.0 / 1.50 / 4.0) * 1.0<nm>)

let p12 =
    [
        { properties = OpticalProperties.transparentGlass175; thickness = thickness1 }
        { properties = OpticalProperties.transparentGlass150; thickness = thickness2 }
    ]

let noOfLayerPairs = 100

let filmSystem =
    {
        description = Some (OpticalSystemDescription "Multilayer thin film system.")
        upper = OpticalProperties.vacuum
        films = [ for _ in 1..noOfLayerPairs -> p12 ] |> List.concat
        lower = OpticalProperties.transparentGlass150
    }

let f = { incidentLightInfo = light600nmNormalLPs; opticalSystem = filmSystem.fullSystem.dispersive }

#time
plot f fn i
plot f fn w
#time

#time
//plot3D f fn p3D i3D
//plot3D f fn w3D i3D
//plot3D f fn e3D p3D
#time
