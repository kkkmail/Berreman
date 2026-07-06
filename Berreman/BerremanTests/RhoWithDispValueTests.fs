namespace BerremanTests

open Berreman.Constants
open Berreman.MaterialProperties
open Berreman.Fields
open Berreman.Dispersion
open OpticalProperties.Active
open Xunit
open BerremanTests.MatrixComparison

/// Slice 009 (spec 0033 Part B): the serializable rho (gyration) tree. Every
/// GyrationClass case routed through RhoWithDispValue.toRhoWithDisp must
/// assemble the expected imaginary gyration matrix — expected matrices are
/// built through Rho.fromIm from raw literals, independently of the
/// crystal-class builders under test. Flipping Handedness must negate the
/// tensor (the enantiomorph is ONE overall sign flip of g), and the dispersive
/// case must evaluate each component's DispersionFormula at the wavelength.
type RhoWithDispValueTests() =
    let output = TestContext.Current.TestOutputHelper

    /// An arbitrary probe wavelength — the constant cases are wavelength-independent.
    let probe = WaveLength.nm 550.0<nm>

    /// Evaluates a value's engine closure at a wavelength.
    let rhoAt (v : RhoWithDispValue) (w : WaveLength) : Rho = v.toRhoWithDisp.getRho w

    /// A constant (non-dispersive) gyrotropic value.
    let constantRho (hand : Handedness) (gyration : GyrationClass<RhoValue>) : RhoWithDispValue =
        RhoWithoutDispValue { gyration = gyration; hand = hand }

    /// A constant real formula: value at every reduced wavelength x (µm).
    let constFormula (value : double) : DispersionFormula =
        {
            terms = [ { lambda = 0.0; coefficients = [| value |]; power = 1; multiplier = 1.0 } ]
            wavelengthScale = 1.0e-6
        }

    /// A linear real formula: a + b·x over the reduced wavelength x (µm).
    let linearFormula (a : double) (b : double) : DispersionFormula =
        {
            terms = [ { lambda = 0.0; coefficients = [| a; b |]; power = 1; multiplier = 1.0 } ]
            wavelengthScale = 1.0e-6
        }

    /// Quartz, class 32 (24 °C): g11 = +5.9e-5, g33 = −10.1e-5.
    let quartzG11 = 5.9e-5
    let quartzG33 = -10.1e-5

    let quartz : UniaxialGyration<RhoValue> =
        {
            g11 = RhoValue quartzG11
            g33 = RhoValue quartzG33
        }

    /// The diagonal tensor the quartz example must assemble, up to an overall sign s.
    let quartzExpected (s : double) : Rho =
        [
            [ s * quartzG11; 0.0; 0.0 ]
            [ 0.0; s * quartzG11; 0.0 ]
            [ 0.0; 0.0; s * quartzG33 ]
        ]
        |> Rho.fromIm

    let triclinic : Triclinic1Gyration<RhoValue> =
        {
            g11 = RhoValue 1.0e-5
            g22 = RhoValue 2.0e-5
            g33 = RhoValue 3.0e-5
            g23 = RhoValue 4.0e-5
            g13 = RhoValue 5.0e-5
            g12 = RhoValue 6.0e-5
        }

    /// The full symmetric tensor the triclinic record must assemble, up to an
    /// overall sign s.
    let triclinicExpected (s : double) : Rho =
        [
            [ s * 1.0e-5; s * 6.0e-5; s * 5.0e-5 ]
            [ s * 6.0e-5; s * 2.0e-5; s * 4.0e-5 ]
            [ s * 5.0e-5; s * 4.0e-5; s * 3.0e-5 ]
        ]
        |> Rho.fromIm


    [<Fact>]
    member _.``Cubic active class assembles the isotropic imaginary gyration matrix`` () =
        let g = 3.0e-5
        let result = rhoAt (constantRho RightHanded (CubicActive (RhoValue g))) probe

        let expected =
            [
                [ g; 0.0; 0.0 ]
                [ 0.0; g; 0.0 ]
                [ 0.0; 0.0; g ]
            ]
            |> Rho.fromIm

        verifyMatrixEqualityRho output result expected


    [<Fact>]
    member _.``Quartz class 32 gyration assembles diag of g11 g11 g33`` () =
        let result = rhoAt (constantRho RightHanded (UniaxialActive quartz)) probe
        verifyMatrixEqualityRho output result (quartzExpected 1.0)


    [<Fact>]
    member _.``Planar active class assembles the antisymmetric in-plane matrix`` () =
        let g12 = 2.5e-5
        let result = rhoAt (constantRho RightHanded (PlanarActive (RhoValue g12))) probe

        let expected =
            [
                [ 0.0; g12; 0.0 ]
                [ -g12; 0.0; 0.0 ]
                [ 0.0; 0.0; 0.0 ]
            ]
            |> Rho.fromIm

        verifyMatrixEqualityRho output result expected


    [<Fact>]
    member _.``Orthorhombic class 222 assembles the three-component diagonal matrix`` () =
        let gyration : Orthorhombic222Gyration<RhoValue> =
            {
                g11 = RhoValue 1.0e-5
                g22 = RhoValue (-2.0e-5)
                g33 = RhoValue 3.0e-5
            }

        let result = rhoAt (constantRho RightHanded (Orthorhombic222 gyration)) probe

        let expected =
            [
                [ 1.0e-5; 0.0; 0.0 ]
                [ 0.0; -2.0e-5; 0.0 ]
                [ 0.0; 0.0; 3.0e-5 ]
            ]
            |> Rho.fromIm

        verifyMatrixEqualityRho output result expected


    [<Fact>]
    member _.``Monoclinic class 2 assembles the diagonal plus g13 matrix`` () =
        let gyration : Monoclinic2Gyration<RhoValue> =
            {
                g11 = RhoValue 1.1e-5
                g22 = RhoValue 2.2e-5
                g33 = RhoValue 3.3e-5
                g13 = RhoValue 4.4e-5
            }

        let result = rhoAt (constantRho RightHanded (Monoclinic2 gyration)) probe

        let expected =
            [
                [ 1.1e-5; 0.0; 4.4e-5 ]
                [ 0.0; 2.2e-5; 0.0 ]
                [ 4.4e-5; 0.0; 3.3e-5 ]
            ]
            |> Rho.fromIm

        verifyMatrixEqualityRho output result expected


    [<Fact>]
    member _.``Monoclinic class m assembles the g12 g23 matrix`` () =
        let gyration : MonoclinicMGyration<RhoValue> =
            {
                g12 = RhoValue 1.2e-5
                g23 = RhoValue 2.3e-5
            }

        let result = rhoAt (constantRho RightHanded (MonoclinicM gyration)) probe

        let expected =
            [
                [ 0.0; 1.2e-5; 0.0 ]
                [ 1.2e-5; 0.0; 2.3e-5 ]
                [ 0.0; 2.3e-5; 0.0 ]
            ]
            |> Rho.fromIm

        verifyMatrixEqualityRho output result expected


    [<Fact>]
    member _.``Triclinic class 1 assembles the full symmetric matrix`` () =
        let result = rhoAt (constantRho RightHanded (Triclinic1 triclinic)) probe
        verifyMatrixEqualityRho output result (triclinicExpected 1.0)


    [<Fact>]
    member _.``Left handedness negates the quartz tensor`` () =
        let result = rhoAt (constantRho LeftHanded (UniaxialActive quartz)) probe
        verifyMatrixEqualityRho output result (quartzExpected (-1.0))


    [<Fact>]
    member _.``Left handedness negates every slot of the triclinic tensor`` () =
        let result = rhoAt (constantRho LeftHanded (Triclinic1 triclinic)) probe
        verifyMatrixEqualityRho output result (triclinicExpected (-1.0))


    [<Fact>]
    member _.``Dispersive gyration evaluates each component's formula at the wavelength`` () =
        // g11(x) = 2.0e-5 + 1.0e-5·x, g33(x) = −1.5e-5 (x in µm): at 500 nm
        // (x = 0.5) the assembled tensor is diag(2.5e-5, 2.5e-5, −1.5e-5).
        let gyration : UniaxialGyration<DispersionFormula> =
            {
                g11 = linearFormula 2.0e-5 1.0e-5
                g33 = constFormula (-1.5e-5)
            }

        let value = RhoWithDispValue { gyration = UniaxialActive gyration; hand = RightHanded }
        let result = rhoAt value (WaveLength.nm 500.0<nm>)

        let expected =
            [
                [ 2.5e-5; 0.0; 0.0 ]
                [ 0.0; 2.5e-5; 0.0 ]
                [ 0.0; 0.0; -1.5e-5 ]
            ]
            |> Rho.fromIm

        verifyMatrixEqualityRho output result expected


    [<Fact>]
    member _.``Dispersive left-handed gyration negates the evaluated tensor`` () =
        // g(x) = 1.0e-5 + 2.0e-5·x evaluates to 2.0e-5 at 500 nm (x = 0.5);
        // LeftHanded flips the assembled tensor to diag(−2.0e-5, …).
        let value = RhoWithDispValue { gyration = CubicActive (linearFormula 1.0e-5 2.0e-5); hand = LeftHanded }
        let result = rhoAt value (WaveLength.nm 500.0<nm>)

        let expected =
            [
                [ -2.0e-5; 0.0; 0.0 ]
                [ 0.0; -2.0e-5; 0.0 ]
                [ 0.0; 0.0; -2.0e-5 ]
            ]
            |> Rho.fromIm

        verifyMatrixEqualityRho output result expected
