namespace OpticalProperties

open Berreman.Constants
open Berreman.Geometry
open Berreman.Media
open Berreman.Fields
open Berreman.MaterialProperties
open Standard

module OpticallyActive =

    let x = 1

    type Eps
        with

        static member getCubicCrystal (EpsValue e11) =
            Eps.fromRefractionIndex (e11 |> sqrt |> RefractionIndex)

        static member getPlanarCrystal (EpsValue e11) (EpsValue e33) =
            let n11 = e11 |> sqrt |> RefractionIndex
            let n33 = e33 |> sqrt |> RefractionIndex
            Eps.fromRefractionIndex(n11, n11, n33)


    type Rho
        with

        static member getCubicCrystal (RhoValue r11) =
            [
                [ r11; 0.0; 0.0 ]
                [ 0.0; r11; 0.0 ]
                [ 0.0; 0.0; r11 ]
            ]
            |> Rho.fromIm

        static member getPlanarCrystal (RhoValue r12) =
            [
                [  r12; 0.0; 0.0 ]
                [ -r12; 0.0; 0.0 ]
                [  0.0; 0.0; 0.0 ]
            ]
            |> Rho.fromIm


    type OpticalProperties
        with

        static member getCubicCrystal e11 r11 =
            {
                eps = Eps.getCubicCrystal e11
                mu = Mu.vacuum
                rho = Rho.getCubicCrystal r11
            }

        static member getPlanarCrystal e11 e33 r12 =
            {
                eps = Eps.getPlanarCrystal e11 e33
                mu = Mu.vacuum
                rho = Rho.getCubicCrystal r12
            }
