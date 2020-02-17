namespace OpticalProperties

open System.Numerics
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion

module Dispersive =
    //let refrIndexSquared (WaveLength lambda) kCoeff (WaveLength lambdaNull) =
    //     1.0 + kCoeff * lambda * lambda / (lambda * lambda - lambdaNull * lambdaNull)


    //let sigmaAbsorption (WaveLength lambdaMidPoint) (WaveLength lambdaHalfWidth) = (lambdaHalfWidth - lambdaMidPoint) / (log 2.0);


    //let absorptionCoeff (WaveLength lambda) kAbsorption lambdaMidPoint lambdaHalfWidth =
    //    let (WaveLength lmbMid) = lambdaMidPoint
    //    kAbsorption * exp (-(lambda - lmbMid) ** 2.0 / (sigmaAbsorption lambdaMidPoint lambdaHalfWidth) ** 2.0)


    //let refrIndex lambda kCoeff lambdaNull kAbsorption lambdaMidPoint lambdaHalfWidth =
    //    (refrIndexSquared lambda kCoeff lambdaNull |> sqrt, absorptionCoeff lambda kAbsorption lambdaMidPoint lambdaHalfWidth)
    //    |> Complex


    //let gyration11Func (WaveLength lambda) refrIndAverageFunc a2Coeff a3Coeff (WaveLength lambda2Coeff) =
    //    (0.0, (lambda * (refrIndAverageFunc (WaveLength lambda)) * ((a2Coeff / (lambda ** 2.0 - lambda2Coeff * 2.0)) + (a3Coeff * lambda ** 2.0 / (lambda * 2.0 - lambda2Coeff ** 2.0) ** 2.0))))
    //    |> Complex


    //let gyration33Func (WaveLength lambda) refrIndAverageFunc a1Coeff (WaveLength lambda1Coeff) =
    //    (0.0, (lambda * (refrIndAverageFunc (WaveLength lambda)) * a1Coeff / (lambda ** 2.0 - lambda1Coeff ** 2.0)))
    //    |> Complex


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
        //let refrIndexLa3Ga5SiO14Ordinary lambda =
        //    refrIndex lambda 2.4981088 (WaveLength.mkm 0.12978841) 0.5e-4 (WaveLength.mkm 0.28) (WaveLength.mkm 0.3)

        let refrIndexLa3Ga5SiO14Ordinary (WaveLength lambda) =
            ((cplx 0.00005) * complexI) / (numberE**(1.2011325347955075e15 * (-2.8e-7 + lambda)**2.0) |> cplx) + (sqrt(1.0 + (2.4981088 * lambda**2.0) / (-1.6845031370328092e-14 + lambda**2.0)) |> cplx)


        /// La3Ga5SiO14, ordinary refreaction index,
        /// 0.4 mkm < lambda < 1.0 mkm.
        //let refrIndexLa3Ga5SiO14ExtraOrdinary lambda =
        //    refrIndex lambda 2.5408145 (WaveLength.mkm 0.12914765) 1.0e-4 (WaveLength.mkm 0.28) (WaveLength.mkm 0.3)

        let refrIndexLa3Ga5SiO14ExtraOrdinary (WaveLength lambda) = 
            ((cplx 0.0001) * complexI) / (numberE**(1.2011325347955075e15 * (-2.8e-7 + lambda)**2.0) |> cplx) + (sqrt(1.0 + (2.5408145 * lambda**2.0) / (-1.6679115500522497e-14 + lambda**2.0)) |> cplx)


        /// La3Ga5SiO14, average (real) refraction index.
        //let refrIndexLa3Ga5SiO14Average lambda = 
        //    ((refrIndexLa3Ga5SiO14Ordinary lambda) + (refrIndexLa3Ga5SiO14ExtraOrdinary lambda)).Real / 2.0


        /// La3Ga5SiO14, g11.
        //let g11La3Ga5SiO14 lambda =
        //    gyration11Func lambda refrIndexLa3Ga5SiO14Average 0.6106e-11 0.6278e-11 (WaveLength.mkm 0.156)

        let g11La3Ga5SiO14 (WaveLength lambda) = 
            (lambda * ((6.278e-12 * lambda**2.0) / (-2.4335999999999998e-14 + lambda**2.0)**2.0 + 6.106e-12 / (-2.4335999999999998e-14 + lambda**2.0)) * (re(((cplx 0.00005) * complexI) / (numberE**(1.2011325347955075e15 * (-2.8e-7 + lambda)**2.0) |> cplx) + (sqrt(1.0 + (2.4981088 * lambda**2.0) / (-1.6845031370328092e-14 + lambda**2.0)) |> cplx)) + re(((cplx 0.0001) * complexI) / (numberE**(1.2011325347955075e15 * (-2.8e-7 + lambda)**2.0) |> cplx) + (sqrt(1.0 + (2.5408145 * lambda**2.0) / (-1.6679115500522497e-14 + lambda**2.0)) |> cplx)))) / 2.0


        /// La3Ga5SiO14, g33.
        //let g33La3Ga5SiO14 lambda =
        //    gyration33Func lambda refrIndexLa3Ga5SiO14Average 0.6072e-11 (WaveLength.mkm 0.198)

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

        //let mkmVal = 1.0e-6

        /// Refraction index.
        //let lSi lambda = 1.0 / ((lambda / mkmVal) ** 2.0 - 0.028)

        //let aSi = 3.41696
        //let bSi = 0.138497
        //let c1Si = 0.013924
        //let d1Si = -0.0000209
        //let e1Si = 0.000000148

        //let nSi (WaveLength lambda) = aSi + bSi * (lSi lambda) + c1Si * (lSi lambda) ** 2.0 + d1Si * (lambda / mkmVal) ** 2.0 + e1Si * (lambda / mkmVal) ** 4.0;
        let nSi (WaveLength lmb) = 3.41696 - 2.09e7 * lmb ** 2.0 + 1.48e17 * lmb ** 4.0 + 0.013924/(-0.028 + 1.e12 * lmb ** 2.0) ** 2.0 + 0.138497/(-0.028 + 1.e12 * lmb ** 2.0)

        /// Absorption Coefficient.
        //let lambda1Si = WaveLength.mkm 0.3757
        //let kappa1Si = 1.32
        //let lambda2Si = WaveLength.mkm 0.589
        //let kappa2Si = 0.030104

        //let xiSi (WaveLength lambda) ko (WaveLength lambda0) eps = ko * (lambda / mkmVal) ** 2.0 / (eps + ((lambda / mkmVal) ** 2.0 - (lambda0 / mkmVal) ** 2.0) * 2.0)

        // For eps = 1.0e-4
        //let lambda0Si = 3.491341053759574e-7 |> WaveLength
        //let koSi = 0.004402681698765213

        //let xiSiFinal lambda = xiSi lambda koSi lambda0Si 1.0e-4
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
