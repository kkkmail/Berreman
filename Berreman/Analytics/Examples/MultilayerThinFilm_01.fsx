//===========================================================
#load "References.fsx"
//===========================================================
open Berreman.FieldFunctions
open Berreman.MaterialProperties
open OpticalProperties.Standard
open Analytics.Charting
open Analytics.StandardSystems
open Analytics.Variables
open Berreman.Media
open Berreman.Fields
open Berreman.Dispersion
open OpticalProperties.Dispersive
//===========================================================
let fn = [ R; T ]
let numberOfPoints = 200

let i = incidenceAngleRange numberOfPoints
let e = ellipticityRange numberOfPoints
let p = polarizationRange numberOfPoints
let w = wavelength200to800Range numberOfPoints

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
plot f i fn
plot f w fn
#time

#time
//plot3D f p i fn
plot3D f w i fn
//plot3D f e p fn
#time
