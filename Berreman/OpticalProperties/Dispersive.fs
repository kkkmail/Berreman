namespace OpticalProperties

open System.Numerics
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion

module Dispersive =

    [<AbstractClass>]
    type DispersiveMaterial() =
        abstract member opticalProperties : OpticalPropertiesWithDisp


    /// La3Ga5SiO14
    type Langasite () =
        inherit DispersiveMaterial()

        let numberE = exp 1.0
        let re (x : Complex) = x.Real

        /// La3Ga5SiO14, extraordinary referaction index,
        /// 0.4 mkm < lambda < 1.0 mkm.
        let refrIndexLa3Ga5SiO14Ordinary (WaveLength lambda) =
            ((cplx 0.00005) * complexI) / (numberE**(1.2011325347955075e15 * (-2.8e-7 + lambda)**2.0) |> cplx) + (sqrt(1.0 + (2.4981088 * lambda**2.0) / (-1.6845031370328092e-14 + lambda**2.0)) |> cplx)


        /// La3Ga5SiO14, ordinary refreaction index,
        /// 0.4 mkm < lambda < 1.0 mkm.
        let refrIndexLa3Ga5SiO14ExtraOrdinary (WaveLength lambda) = 
            ((cplx 0.0001) * complexI) / (numberE**(1.2011325347955075e15 * (-2.8e-7 + lambda)**2.0) |> cplx) + (sqrt(1.0 + (2.5408145 * lambda**2.0) / (-1.6679115500522497e-14 + lambda**2.0)) |> cplx)


        /// La3Ga5SiO14, g11.
        let g11La3Ga5SiO14 (WaveLength lambda) = 
            (lambda * ((6.278e-12 * lambda**2.0) / (-2.4335999999999998e-14 + lambda**2.0)**2.0 + 6.106e-12 / (-2.4335999999999998e-14 + lambda**2.0)) * (re(((cplx 0.00005) * complexI) / (numberE**(1.2011325347955075e15 * (-2.8e-7 + lambda)**2.0) |> cplx) + (sqrt(1.0 + (2.4981088 * lambda**2.0) / (-1.6845031370328092e-14 + lambda**2.0)) |> cplx)) + re(((cplx 0.0001) * complexI) / (numberE**(1.2011325347955075e15 * (-2.8e-7 + lambda)**2.0) |> cplx) + (sqrt(1.0 + (2.5408145 * lambda**2.0) / (-1.6679115500522497e-14 + lambda**2.0)) |> cplx)))) / 2.0


        /// La3Ga5SiO14, g33.
        let g33La3Ga5SiO14 (WaveLength lambda) =
            (3.0359999999999996e-12 * lambda * (re(((cplx 0.00005) * complexI) / (numberE**(1.2011325347955075e15 * (-2.8e-7 + lambda)**2.0) |> cplx) + (sqrt(1.0 + (2.4981088 * lambda**2.0) / (-1.6845031370328092e-14 + lambda**2.0)) |> cplx)) + re(((cplx 0.0001) * complexI) / (numberE**(1.2011325347955075e15 * (-2.8e-7 + lambda)**2.0) |> cplx) + (sqrt(1.0 + (2.5408145 * lambda**2.0) / (-1.6679115500522497e-14 + lambda**2.0)) |> cplx)))) / (-3.9204e-14 + lambda**2.0)


        let epsLa3Ga5SiO14 lambda =
            let nVal1 = refrIndexLa3Ga5SiO14Ordinary lambda |> ComplexRefractionIndex
            let nVal2 = refrIndexLa3Ga5SiO14ExtraOrdinary lambda |> ComplexRefractionIndex
            let nVal3 = refrIndexLa3Ga5SiO14Ordinary lambda |> ComplexRefractionIndex
            (nVal1, nVal2, nVal3) |> Eps.fromComplexRefractionIndex


        let rhoLa3Ga5SiO14 lambda =
            [| (g11La3Ga5SiO14 lambda |> cplx) * complexI; complexZero; (g33La3Ga5SiO14 lambda |> cplx) * complexI|]
            |> complexFromDiagonal
            |> ComplexMatrix
            |> ComplexMatrix3x3
            |> Rho


        override __.opticalProperties=
            {
                epsWithDisp = epsLa3Ga5SiO14 |> EpsWithDisp
                muWithDisp = Mu.vacuum.dispersive
                rhoWithDisp = rhoLa3Ga5SiO14 |> RhoWithDisp
            }


    type Silicon () =
        inherit DispersiveMaterial()

        /// Refraction index.
        let nSi (WaveLength lmb) = 3.41696 - 2.09e7 * lmb ** 2.0 + 1.48e17 * lmb ** 4.0 + 0.013924/(-0.028 + 1.e12 * lmb ** 2.0) ** 2.0 + 0.138497/(-0.028 + 1.e12 * lmb ** 2.0)

        /// Absorption Coefficient.
        let xiSiFinal (WaveLength lmb) = (4.402681698765214e9 * lmb ** 2.0)/(0.0001 + (-0.12189462353667012 + 1.0e12 * lmb ** 2.0) ** 2.0)

        let refrIndexSi lambda = createComplex (nSi lambda) (xiSiFinal lambda) |> ComplexRefractionIndex
        let epsSi lambda = refrIndexSi lambda |> Eps.fromComplexRefractionIndex

        override __.opticalProperties=
            {
                epsWithDisp = epsSi |> EpsWithDisp
                muWithDisp = Mu.vacuum.dispersive
                rhoWithDisp = Rho.vacuum.dispersive
            }


    let siliconOpticalProperties = Silicon().opticalProperties
    let langasiteOpticalProperties = Langasite().opticalProperties
