namespace OpticalProperties

open Berreman.Constants
open Berreman.Geometry
open Berreman.Media
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
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

        static member type_222_Crystal (RhoValue g11) (RhoValue g22) (RhoValue g33) =
            [
                [ g11; 0.0; 0.0 ]
                [ 0.0; g22; 0.0 ]
                [ 0.0; 0.0; g33 ]
            ]
            |> Rho.fromIm

        static member type_2_Crystal (RhoValue g11) (RhoValue g22) (RhoValue g33) (RhoValue g13) =
            [
                [ g11; 0.0; g13 ]
                [ 0.0; g22; 0.0 ]
                [ g13; 0.0; g33 ]
            ]
            |> Rho.fromIm

        static member type_m_Crystal (RhoValue g12) (RhoValue g23) =
            [
                [ 0.0; g12; 0.0 ]
                [ g12; 0.0; g23 ]
                [ 0.0; g23; 0.0 ]
            ]
            |> Rho.fromIm

        static member type_1_Crystal (RhoValue g11) (RhoValue g22) (RhoValue g33) (RhoValue g23) (RhoValue g13) (RhoValue g12) =
            [
                [ g11; g12; g13 ]
                [ g12; g22; g23 ]
                [ g13; g23; g33 ]
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


    /// Assembles the engine Rho for a constant symmetry-class gyration: the
    /// handedness applies ONE overall sign to every component, then the class
    /// routes to its crystal-class builder. UniaxialActive routes through the
    /// diagonal type_3_4_6_Crystal — NOT type_32_42_62_Crystal, which needs a
    /// g12 the symmetric two-component uniaxial record cannot supply.
    let private assembleRho (hand : Handedness) (gyration : GyrationClass<RhoValue>) : Rho =
        let signed (RhoValue g) = hand.sign * g |> RhoValue

        match gyration.map signed with
        | CubicActive g11 -> Rho.cubicCrystal g11
        | UniaxialActive u -> Rho.type_3_4_6_Crystal u.g11 u.g33
        | PlanarActive g12 -> Rho.planarCrystal g12
        | Orthorhombic222 o -> Rho.type_222_Crystal o.g11 o.g22 o.g33
        | Monoclinic2 m -> Rho.type_2_Crystal m.g11 m.g22 m.g33 m.g13
        | MonoclinicM m -> Rho.type_m_Crystal m.g12 m.g23
        | Triclinic1 t -> Rho.type_1_Crystal t.g11 t.g22 t.g33 t.g23 t.g13 t.g12


    type RhoWithDispValue
        with

        /// Builds the engine's RhoWithDisp: the constant case short-circuits to
        /// RhoWithoutDisp; the dispersive case evaluates each component's
        /// DispersionFormula at the wavelength and assembles per call.
        member this.toRhoWithDisp : RhoWithDisp =
            match this with
            | RhoWithoutDispValue g -> assembleRho g.hand g.gyration |> RhoWithoutDisp
            | RhoWithDispValue g ->
                RhoWithDisp (fun w -> g.gyration.map (fun (f : DispersionFormula) -> f.evaluate w |> RhoValue) |> assembleRho g.hand)
