﻿namespace OpticalProperties

open Berreman.Constants
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Media
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.BerremanMatrix


// !!! DO NOT CHANGE ANY VALUES HERE !!!
// Standard optical properties without dispresion to be used in various simple calculations and tests.
// If some other values are desired, introduce another module and set the new values there.
module Standard =

    type RefractionIndex
        with 

        /// Standard trnasparent glass with refractive index 1.52.
        static member transparentGlass = RefractionIndex 1.52

    type Eps
        with 
        static member transparentGlass = RefractionIndex.transparentGlass |> Eps.fromRefractionIndex

        static member uniaxialCrystal = (RefractionIndex 1.5, RefractionIndex 1.65, RefractionIndex 1.65) |> Eps.fromRefractionIndex

        static member biaxialCrystal = (RefractionIndex 1.5, RefractionIndex 1.65, RefractionIndex 1.75) |> Eps.fromRefractionIndex


    type OpticalProperties
        with
        
        static member transparentGlass = Eps.transparentGlass |> OpticalProperties.fromEpsion
        static member uniaxialCrystal = Eps.uniaxialCrystal |> OpticalProperties.fromEpsion
        static member biaxialCrystal =  Eps.biaxialCrystal |> OpticalProperties.fromEpsion


    type BaseOpticalSystem
        with 

        /// Standard vacuum / transparent glass system.
        static member transparentGlassSystem = 
            {
                description = Some "Standard vacuum / transparent glass system."
                upper = OpticalProperties.vacuum
                films = []
                lower = OpticalProperties.transparentGlass
            }

        /// Standard vacuum / uniaxial crystal system.
        static member uniaxialCrystalSystem = 
            {
                description = Some "Standard vacuum / uniaxial crystal system."
                upper = OpticalProperties.vacuum
                films = []
                lower = OpticalProperties.uniaxialCrystal
            }

        /// Standard vacuum / biaxial crystal system.
        static member biaxialCrystalSystem = 
            {
                description = Some "Standard vacuum / biaxial crystal system."
                upper = OpticalProperties.vacuum
                films = []
                lower = OpticalProperties.biaxialCrystal
            }

        /// Standard vacuum / transparent glass film / vacuum system.
        static member transparentGlasslFilmSystem thickness = 
            {
                description = Some "Standard vacuum / transparent glass film / vacuum system."
                upper = OpticalProperties.vacuum
                films = [ { properties = OpticalProperties.transparentGlass; thickness = thickness } ]
                lower = OpticalProperties.vacuum
            }

        /// Standard vacuum / uniaxial crystal film / vacuum system.
        static member uniaxialCrystalFilmSystem thickness = 
            {
                description = Some "Standard vacuum / uniaxial crystal film / transparent glass system."
                upper = OpticalProperties.vacuum
                films = [ { properties = OpticalProperties.uniaxialCrystal; thickness = thickness } ]
                lower = OpticalProperties.vacuum
            }

        /// Standard vacuum / biaxial crystal film / vacuum system.
        static member biaxialCrystalFilmSystem thickness = 
            {
                description = Some "Standard vacuum / biaxial crystal film / transparent glass system."
                upper = OpticalProperties.vacuum
                films = [ { properties = OpticalProperties.biaxialCrystal; thickness = thickness } ]
                lower = OpticalProperties.vacuum
            }

    let private w600nm = 600.0

    /// 600 nm light falling at normal.
    let light600nmNormalLPs = WaveLength.nm w600nm |> IncidentLightInfo.create

    /// 600 nm light falling at some incidence angle (in degrees).
    let light600nmInclinedDegreelLPs angleDegree = 
        IncidentLightInfo.createInclined (WaveLength.nm w600nm) (Angle.degree angleDegree |> IncidenceAngle.create)
