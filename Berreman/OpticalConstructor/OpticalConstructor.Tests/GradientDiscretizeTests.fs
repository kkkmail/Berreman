namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Domain.BeamTree
open Xunit

/// Gradient-index auto-discretization — AC-B11. `discretize` turns one
/// continuously-varying-index layer into exactly `n` ordinary reused `Layer`
/// records whose thickness sum equals the total and that drop straight into
/// `OpticalSystem.films`, with no gradient-specific engine type.
module GradientDiscretizeTests =

    /// A simple varying-index profile producing valid `OpticalProperties`.
    let private profile (depth : float<meter>) : OpticalProperties =
        OpticalProperties.fromRefractionIndex (RefractionIndex (1.4 + (depth / 1.0<meter>) * 1.0e6))

    let private thicknessMeters (l : Layer) : float<meter> =
        match l.thickness with
        | Thickness t -> t
        | Infinity -> 0.0<meter>

    [<Fact>]
    let ``AC-B11 discretize yields exactly n equal-thickness reused Layers summing to total`` () =
        let total = 300.0e-9<meter>
        let n = 5
        let g = { totalThickness = total; subLayerCount = n; indexProfile = profile }
        let layers = discretize g

        Assert.Equal(n, List.length layers)

        let sum = layers |> List.sumBy thicknessMeters
        Assert.True(abs ((sum - total) / total) <= 1e-12, $"thickness sum {sum} <> total {total}")

        // The produced sub-layers are ordinary `Layer` records, assignable
        // straight into `OpticalSystem.films` — no gradient-specific engine type.
        let sys : OpticalSystem =
            {
                description = None
                upper = OpticalProperties.vacuum
                films = layers
                substrate = None
                lower = OpticalProperties.vacuum
            }
        Assert.Equal(n, List.length sys.films)

    [<Fact>]
    let ``AC-B11 discretize with subLayerCount = 1 returns a single full-thickness Layer`` () =
        let total = 120.0e-9<meter>
        let g = { totalThickness = total; subLayerCount = 1; indexProfile = profile }
        let layers = discretize g
        Assert.Equal(1, List.length layers)
        Assert.True(abs ((thicknessMeters layers.[0] - total) / total) <= 1e-12)

    [<Fact>]
    let ``discretize rejects subLayerCount < 1`` () =
        let g = { totalThickness = 100.0e-9<meter>; subLayerCount = 0; indexProfile = profile }
        Assert.Throws<System.ArgumentException>(fun () -> discretize g |> ignore) |> ignore
