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
//===========================================================
let fn = [ R; T ]

let numberOfPoints = 1000
//let numberOfPoints3D = 100

let i = incidenceAngleRange60 numberOfPoints
//let e = ellipticityRange numberOfPoints
//let p = polarizationRange numberOfPoints
let w = wavelength05to20Range numberOfPoints

//let i3D = incidenceAngleRange80 numberOfPoints3D
//let e3D = ellipticityRange numberOfPoints3D
//let p3D = polarizationRange numberOfPoints3D
//let w3D = wavelength200to800Range numberOfPoints3D

let h1 = 6.7
let h2 = 6.7

//let h1 = 2.8
//let h2 = 2.8

//let h1 = 2.7
//let h2 = 2.7

//let h1 = 10.6 / 4.0
//let h2 = 10.6 / 4.0

//let h1 = 2.6
//let h2 = 2.6

//let h1 = 2.4
//let h2 = 2.4

//let h1 = 2.0
//let h2 = 2.0

//let light = light10nmNormalLPs
let light = light13p5nmNormalLPp

let thickness1 = Thickness.nm h1
let thickness2 = Thickness.nm h2

let noOfLayerPairs = 100

let description =
    $"Multilayer thin film system for EUV: {noOfLayerPairs} layer pairs, " +
    $"h1 = {h1} nm, h2 = {h2} nm, light = {light}."

let filmSystem =
    {
        description = Some description
        upper = OpticalProperties.vacuum
        films =
            [
                { properties = OpticalProperties.euvMolybdenum; thickness = thickness1 }
                { properties = OpticalProperties.euvSilicon; thickness = thickness2 }
            ]
            |> List.replicate noOfLayerPairs
            |> List.concat

        lower = OpticalProperties.vacuum
    }

let f = { incidentLightInfo = light; opticalSystem = filmSystem.fullSystem.dispersive }

#time
plot f fn i
plot f fn w
#time

#time
//plot3D f fn p3D i3D
//plot3D f fn w3D i3D
//plot3D f fn e3D p3D
#time
