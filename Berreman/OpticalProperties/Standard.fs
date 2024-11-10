namespace OpticalProperties

open Berreman.Constants
open Berreman.Geometry
open Berreman.Media
open Berreman.Fields
open Berreman.MaterialProperties
open MathNet.Numerics
open Berreman.MathNetNumericsMath

/// !!! DO NOT CHANGE ANY VALUES HERE !!!
/// Standard optical properties without dispersion to be used in various simple calculations and tests.
/// If some other values are desired, introduce another module and set the new values there OR add them at the end of this module.
module Standard =

    let private w10nm = 10.0
    let private w13p5nm = 13.5
    let private w600nm = 600.0


    /// 10 nm S-polarized light falling at normal.
    let light10nmNormalLPs = WaveLength.nm w10nm |> IncidentLightInfo.create


    /// 10 nm P-polarized light falling at normal.
    let light10nmNormalLPp = (WaveLength.nm w10nm |> IncidentLightInfo.create).p


    /// 13.5 nm S-polarized light falling at normal.
    let light13p5nmNormalLPs = WaveLength.nm w13p5nm |> IncidentLightInfo.create


    /// 13.2 nm P-polarized light falling at normal.
    let light13p5nmNormalLPp = (WaveLength.nm w13p5nm |> IncidentLightInfo.create).p


    /// 600 nm S-polarized light falling at normal.
    let light600nmNormalLPs = WaveLength.nm w600nm |> IncidentLightInfo.create


    /// 600 nm S-polarized light falling at some incidence angle (in degrees).
    let light600nmInclinedDegreeLPs angleDegree =
        IncidentLightInfo.createInclined (WaveLength.nm w600nm) (Angle.degree angleDegree |> IncidenceAngle.create)


    type RefractionIndex
        with

        /// Standard transparent glass with refractive index 1.52.
        static member transparentGlass = RefractionIndex 1.52

        static member transparentGlass150 = RefractionIndex 1.50
        static member transparentGlass175 = RefractionIndex 1.75
        static member transparentGlass200 = RefractionIndex 2.00
        static member getTransparentGlass n = RefractionIndex n

        //=======================================
        // Add any custom values after this line.
        //=======================================


    type Eps
        with
        static member transparentGlass = RefractionIndex.transparentGlass |> Eps.fromRefractionIndex
        static member transparentGlass150 = RefractionIndex.transparentGlass150 |> Eps.fromRefractionIndex
        static member transparentGlass175 = RefractionIndex.transparentGlass175 |> Eps.fromRefractionIndex
        static member transparentGlass200 = RefractionIndex.transparentGlass200 |> Eps.fromRefractionIndex
        static member uniaxialCrystal = (RefractionIndex 1.5, RefractionIndex 1.65, RefractionIndex 1.65) |> Eps.fromRefractionIndex
        static member biaxialCrystal = (RefractionIndex 1.5, RefractionIndex 1.65, RefractionIndex 1.75) |> Eps.fromRefractionIndex
        static member getTransparentGlass n = RefractionIndex.getTransparentGlass n |> Eps.fromRefractionIndex

        //=======================================
        // Add any custom values after this line.
        //=======================================

        // !!! NOT CONFIRMED !!!
        // EUV data, for wavelength around 10 - 13 nm.

        static member euvMolybdenumDelta = 0.043
        static member euvMolybdenumBeta = 0.016

        static member euvSiliconDelta = 0.065
        static member euvSiliconBeta = 0.005

        static member euvMolybdenum = (createComplex (1.0 - Eps.euvMolybdenumDelta) Eps.euvMolybdenumBeta) |> ComplexRefractionIndex |> Eps.fromComplexRefractionIndex
        static member euvSilicon = (createComplex  (1.0 - Eps.euvSiliconDelta) Eps.euvSiliconBeta) |> ComplexRefractionIndex |> Eps.fromComplexRefractionIndex


    type OpticalProperties
        with

        static member transparentGlass = Eps.transparentGlass |> OpticalProperties.fromEpsion
        static member transparentGlass150 = Eps.transparentGlass150 |> OpticalProperties.fromEpsion
        static member transparentGlass175 = Eps.transparentGlass175 |> OpticalProperties.fromEpsion
        static member transparentGlass200 = Eps.transparentGlass200 |> OpticalProperties.fromEpsion
        static member uniaxialCrystal = Eps.uniaxialCrystal |> OpticalProperties.fromEpsion
        static member biaxialCrystal = Eps.biaxialCrystal |> OpticalProperties.fromEpsion
        static member getTransparentGlass n = Eps.getTransparentGlass n |> OpticalProperties.fromEpsion

        //=======================================
        // Add any custom values after this line.
        //=======================================

        static member euvMolybdenum = Eps.euvMolybdenum |> OpticalProperties.fromEpsion
        static member euvSilicon = Eps.euvSilicon |> OpticalProperties.fromEpsion


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
        static member transparentGlassFilmSystem thickness =
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

        /// Standard vacuum / transparent glass system with refraction index n.
        static member getTransparentGlassSystem n =
            {
                description = Some "Standard vacuum / transparent glass system."
                upper = OpticalProperties.vacuum
                films = []
                lower = OpticalProperties.getTransparentGlass n
            }

        //=======================================
        // Add any custom values after this line.
        //=======================================


    type OpticalSystem
        with

        static member plateSystem properties description thickness =
            {
                description = Some description
                upper = OpticalProperties.vacuum
                films = []
                substrate =
                    { properties = properties; thickness = thickness }
                    |> Plate
                    |> Some
                lower = OpticalProperties.vacuum
            }

        /// Standard transparent glass / vacuum system for testing internal reflection.
        static member totalReflGlass150System =
            {
                description = Some "Standard transparent glass with n = 1.50 / vacuum system for testing internal reflection."
                upper = OpticalProperties.transparentGlass150
                films = []
                substrate = None
                lower = OpticalProperties.vacuum
            }


        /// Standard transparent glass / vacuum system for testing wedge reflection.
        static member getWedgeGlass150System thickness a =
            {
                description = Some "Standard transparent glass with n = 1.50 / vacuum system for testing wedge."
                upper = OpticalProperties.vacuum
                films = []
                substrate =
                    {
                        layer =
                            {
                                properties = OpticalProperties.transparentGlass150
                                thickness = thickness
                            }

                        angle = a
                    }
                    |> Wedge
                    |> Some
                lower = OpticalProperties.vacuum
            }

         static member getWedgeGlass150Thickness1mmSystem a = OpticalSystem.getWedgeGlass150System (1.0 * mm |> Thickness) a

        /// Standard transparent glass / vacuum system for testing wedge reflection.
        static member wedge40DegGlass150System =
            OpticalSystem.getWedgeGlass150System (1.0 * mm |> Thickness) (40.0 |> Angle.degree |> WedgeAngle)

        /// Standard transparent glass / vacuum system for testing wedge reflection.
        static member wedge50DegGlass150System =
            OpticalSystem.getWedgeGlass150System (1.0 * mm |> Thickness) (50.0 |> Angle.degree |> WedgeAngle)

        /// Standard vacuum / biaxial crystal substrate / vacuum system.
        static member biaxialCrystalSubstrateSystem thickness =
            {
                description = Some "Standard vacuum / biaxial crystal thick plate / vacuum system."
                upper = OpticalProperties.vacuum
                films = []
                substrate = { properties = OpticalProperties.biaxialCrystal; thickness = thickness } |> Plate |> Some
                lower = OpticalProperties.vacuum
            }

        static member wedgeSystem properties description thickness angle =
            {
                description = Some description
                upper = OpticalProperties.vacuum
                films = []
                substrate =
                    {
                        layer = { properties = properties; thickness = thickness }
                        angle = angle
                    }
                    |> Wedge
                    |> Some
                lower = OpticalProperties.vacuum
            }

        /// Standard vacuum / biaxial crystal wedge / vacuum system.
        static member biaxialCrystalWedgeSystem thickness angle =
            let d = "Standard vacuum / biaxial crystal wedge / vacuum system."
            OpticalSystem.wedgeSystem OpticalProperties.biaxialCrystal d thickness angle

        //=======================================
        // Add any custom values after this line.
        //=======================================
