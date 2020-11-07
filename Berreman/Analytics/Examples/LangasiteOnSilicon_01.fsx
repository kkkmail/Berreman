//===========================================================
#load "References.fsx"
//===========================================================
open Berreman.FieldFunctions
open Berreman.Media
open OpticalProperties.Standard
open Analytics.StandardLightVariables
open Analytics.StandardSystems
open Analytics.Charting

//===========================================================
let fn = [ R; T ]

let numberOfPoints = 2000
let incidenceAngleDegree = 79.0

let i = incidenceAngleRange numberOfPoints
let e = ellipticityRange numberOfPoints
let p = polarizationRange numberOfPoints
let w = wavelength250to600Range numberOfPoints

let thickness = Thickness.mm 0.01

//let ww = wavelength250to600 numberOfPoints

//plotN11 langasiteOpticalProperties ww
//plotXi11 langasiteOpticalProperties ww

//plotN22 langasiteOpticalProperties ww
//plotXi22 langasiteOpticalProperties ww

//plotRho11 langasiteOpticalProperties ww
//plotRho33 langasiteOpticalProperties ww

let film = langasiteFilmOnSilicon thickness light600nmNormalLPs
let substrate = langasiteSubstrateOnSilicon thickness light600nmNormalLPs

let film1 = langasiteFilmOnSilicon thickness (light600nmInclinedDegreeLPs incidenceAngleDegree)
let substrate1 = langasiteSubstrateOnSilicon thickness (light600nmInclinedDegreeLPs incidenceAngleDegree)

#time
plot film fn w
plot substrate fn w
plotComparison [ film; substrate ] fn w
plotComparison [ film1; substrate1 ] fn w
#time
