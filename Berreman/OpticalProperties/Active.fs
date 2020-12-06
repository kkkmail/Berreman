namespace OpticalProperties

open Berreman.Constants
open Berreman.Geometry
open Berreman.Media
open Berreman.Fields
open Berreman.MaterialProperties
open Standard

module Active =

    type Eps
        with

        static member cubicCrystal (EpsValue e11) =
            Eps.fromRefractionIndex (e11 |> sqrt |> RefractionIndex)

        static member planarCrystal (EpsValue e11) (EpsValue e33) =
            let n11 = e11 |> sqrt |> RefractionIndex
            let n33 = e33 |> sqrt |> RefractionIndex
            Eps.fromRefractionIndex(n11, n11, n33)

        static member type_3_4_6_Crystal e11 e33 = Eps.planarCrystal e11 e33
        static member type_32_42_62_Crystal e11 e33 = Eps.planarCrystal e11 e33


    type Rho
        with

        static member cubicCrystal (RhoValue g11) =
            [
                [ g11; 0.0; 0.0 ]
                [ 0.0; g11; 0.0 ]
                [ 0.0; 0.0; g11 ]
            ]
            |> Rho.fromIm

        static member planarCrystal (RhoValue g12) =
            [
                [  0.0; g12; 0.0 ]
                [ -g12; 0.0; 0.0 ]
                [  0.0; 0.0; 0.0 ]
            ]
            |> Rho.fromIm

        static member type_3_4_6_Crystal (RhoValue g11) (RhoValue g33) =
            [
                [  g11; 0.0; 0.0 ]
                [ 0.0; g11; 0.0 ]
                [  0.0; 0.0; g33 ]
            ]
            |> Rho.fromIm

        static member type_32_42_62_Crystal (RhoValue g11) (RhoValue g12) (RhoValue g33) =
            [
                [  g11; g12; 0.0 ]
                [ -g12; g11; 0.0 ]
                [  0.0; 0.0; g33 ]
            ]
            |> Rho.fromIm


    type OpticalProperties
        with

        static member cubicCrystal e11 g11 =
            {
                eps = Eps.cubicCrystal e11
                mu = Mu.vacuum
                rho = Rho.cubicCrystal g11
            }

        static member planarCrystal e11 e33 g12 =
            {
                eps = Eps.planarCrystal e11 e33
                mu = Mu.vacuum
                rho = Rho.planarCrystal g12
            }

        static member type_3_4_6_Crystal e11 e33 g11 g33 =
            {
                eps = Eps.type_3_4_6_Crystal e11 e33
                mu = Mu.vacuum
                rho = Rho.type_3_4_6_Crystal g11 g33
            }

        static member type_32_42_62_Crystal e11 e33 g11 g12 g33 =
            {
                eps = Eps.type_32_42_62_Crystal e11 e33
                mu = Mu.vacuum
                rho = Rho.type_32_42_62_Crystal g11 g12 g33
            }
