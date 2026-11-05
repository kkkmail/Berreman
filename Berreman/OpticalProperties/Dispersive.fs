namespace OpticalProperties

open System.Numerics
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
open Berreman.Constants

module Dispersive =

    [<AbstractClass>]
    type DispersiveMaterial() =
        abstract member opticalProperties : OpticalPropertiesWithDisp


    /// La3Ga5SiO14
    type Langasite () =
        inherit DispersiveMaterial()

        /// Re nₒ + Re nₑ of the published laws, the factor g11 and g33 share (spec 0050 step 8).
        let realIndexSum (w : WaveLength) =
            (LangasiteOrdinaryIndexLaw.complexIndex w).value.Real + (LangasiteExtraordinaryIndexLaw.complexIndex w).value.Real


        /// La3Ga5SiO14, g11.
        let g11La3Ga5SiO14 (w : WaveLength) =
            let lambda = w.value / 1.0<meter>
            (lambda * ((6.278e-12 * lambda**2.0) / (-2.4335999999999998e-14 + lambda**2.0)**2.0 + 6.106e-12 / (-2.4335999999999998e-14 + lambda**2.0)) * realIndexSum w) / 2.0


        /// La3Ga5SiO14, g33.
        let g33La3Ga5SiO14 (w : WaveLength) =
            let lambda = w.value / 1.0<meter>
            (3.0359999999999996e-12 * lambda * realIndexSum w) / (-3.9204e-14 + lambda**2.0)


        let epsLa3Ga5SiO14 (w : WaveLength) =
            let nO = LangasiteOrdinaryIndexLaw.complexIndex w
            (nO, LangasiteExtraordinaryIndexLaw.complexIndex w, nO) |> Eps.fromComplexRefractionIndex


        let rhoLa3Ga5SiO14 lambda =
            [| (g11La3Ga5SiO14 lambda |> Complex.ofReal) * complexI; complexZero; (g33La3Ga5SiO14 lambda |> Complex.ofReal) * complexI|]
            |> complexFromDiagonal
            |> ComplexMatrix
            |> ComplexMatrix3x3
            |> Rho


        override _.opticalProperties =
            {
                epsWithDisp = epsLa3Ga5SiO14 |> EpsWithDisp
                muWithDisp = Mu.vacuum.dispersive
                rhoWithDisp = rhoLa3Ga5SiO14 |> RhoWithDisp
            }


    type Silicon () =
        inherit DispersiveMaterial()

        let epsSi (w : WaveLength) = SiliconIndexLaw.complexIndex w |> Eps.fromComplexRefractionIndex

        override _.opticalProperties =
            {
                epsWithDisp = epsSi |> EpsWithDisp
                muWithDisp = Mu.vacuum.dispersive
                rhoWithDisp = Rho.vacuum.dispersive
            }


    let siliconOpticalProperties = Silicon().opticalProperties
    let langasiteOpticalProperties = Langasite().opticalProperties
