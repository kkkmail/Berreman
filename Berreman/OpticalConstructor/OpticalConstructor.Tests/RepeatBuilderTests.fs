namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Ui
open Xunit

/// §J.2 repeat / period unit-cell builder tests (slice 014, AC-J2). Prove that
/// `RepeatBuilder.expand cell R` yields exactly N*R ordinary `Layer` records in
/// cell order, assignable straight into `OpticalSystem.films`, with NO new
/// period/super-layer type. Per the slice contract this file carries NO reference
/// to the slice-015 `Validation.fs`; the `R < 1` rejection (AC-J9) is owned and
/// tested in slice 015's `ValidationTests.fs`, not here.
module RepeatBuilderTests =

    let private hi = OpticalProperties.fromRefractionIndex (RefractionIndex 2.3)
    let private lo = OpticalProperties.fromRefractionIndex (RefractionIndex 1.38)

    let private layer (props : OpticalProperties) (t : float<nm>) : Layer =
        { properties = props; thickness = Thickness.nm t }

    // Distinct instances so identity proves order is preserved without aliasing.
    let private h = layer hi 60.0<nm>
    let private l = layer lo 100.0<nm>
    let private cell = [ h; l ]

    [<Fact>]
    let ``AC-J2 expand yields exactly N*R layers`` () =
        let r = RepeatBuilder.expand cell 5
        Assert.Equal(2 * 5, List.length r)

    [<Fact>]
    let ``AC-J2 expand preserves cell order across every period`` () =
        let r = RepeatBuilder.expand cell 3
        Assert.Equal<Layer list>([ h; l; h; l; h; l ], r)
        // Cell order holds in each period: even indices are the cell's first layer.
        r |> List.iteri (fun i x -> Assert.Equal((if i % 2 = 0 then h else l), x))

    [<Fact>]
    let ``AC-J2 a single repeat returns the cell unchanged`` () =
        Assert.Equal<Layer list>(cell, RepeatBuilder.expand cell 1)

    [<Fact>]
    let ``AC-J2 the expanded films are ordinary Layers assignable to OpticalSystem.films`` () =
        let films = RepeatBuilder.expand cell 4
        // No new period/super-layer type: the result is `Layer list` and drops
        // straight into the engine `OpticalSystem.films` field (Media.fs:98).
        let sys : OpticalSystem =
            {
                description = Some "DBR"
                upper = OpticalProperties.vacuum
                films = films
                substrate = None
                lower = OpticalProperties.vacuum
            }
        Assert.Equal(8, List.length sys.films)
        Assert.Equal<Layer list>(films, sys.films)
