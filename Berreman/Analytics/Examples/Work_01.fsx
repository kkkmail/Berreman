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
let numberOfPoints3D = 100

let i = incidenceAngleRange numberOfPoints
let e = ellipticityRange numberOfPoints
let p = polarizationRange numberOfPoints
let w = wavelength200to800Range numberOfPoints

let i3D = incidenceAngleRange numberOfPoints3D
let e3D = ellipticityRange numberOfPoints3D
let p3D = polarizationRange numberOfPoints3D
let w3D = wavelength200to800Range numberOfPoints3D


let thickness1 = Thickness.nm (600.0 / 1.52 / 4.0)
let thickness2 = Thickness.nm (600.0 / 1.00 / 4.0)

let filmSystem = 
    {
        description = Some "Multilayer thin film system."
        upper = OpticalProperties.vacuum
        films = 
            [ 
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
            ]
        lower = OpticalProperties.vacuum
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
