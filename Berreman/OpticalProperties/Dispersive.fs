namespace OpticalProperties

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

        let epsLa3Ga5SiO14 (w : WaveLength) =
            let nO = LangasiteOrdinaryIndexLaw.complexIndex w
            (nO, LangasiteExtraordinaryIndexLaw.complexIndex w, nO) |> Eps.fromComplexRefractionIndex


        override _.opticalProperties =
            {
                epsWithDisp = epsLa3Ga5SiO14 |> EpsWithDisp
                muWithDisp = Mu.vacuum.dispersive
                rhoWithDisp = RhoWithDisp LangasiteGyrationLaw.rhoAt
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
