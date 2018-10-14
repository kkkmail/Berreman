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

let numberOfPoints = 2000

let i = incidenceAngleRange numberOfPoints
let e = ellipticityRange numberOfPoints
let p =polarizationRange numberOfPoints
let w = wavelength250to600Range numberOfPoints

let thickness = Thickness.mm 0.01

//let ww = wavelength250to600 numberOfPoints

//plotN11 langasiteOpticalProperties ww
//plotXi11 langasiteOpticalProperties ww

//plotN22 langasiteOpticalProperties ww
//plotXi22 langasiteOpticalProperties ww

//plotRho11 langasiteOpticalProperties ww
//plotRho33 langasiteOpticalProperties ww

let f = langasiteFilmOnSilicon 0.0 thickness
let s = langasiteSubstrateOnSilicon 0.0 thickness

#time
plot f fn w
plot s fn w
plotComparison [ f; s ] fn w
#time
