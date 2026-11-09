namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain.ItemModels

/// The parameter vocabulary of items and item models in `ItemModels` (spec 0050 S7.1, 001 §I).
module ItemModelsTests =

    [<Fact>]
    let ``the moved parameter name and dial sign keep their values`` () : unit =
        Assert.Equal("c2.retardance", (ParameterName "c2.retardance").value)
        Assert.Equal<float>(1.0, DialPositive.value)
        Assert.Equal<float>(-1.0, DialNegative.value)

    /// A data projection over test-local values (spec 0050 001 §I): no bench noise model is exercised here.
    [<Fact>]
    let ``the detector noise axes keep their declared quantities`` () : unit =
        let rotation = ParameterValue 0.001
        let element = ParameterValue 0.02
        let throughput = ParameterValue 0.03

        let axes = ItemParameterAxis.detectorNoiseAxes rotation element throughput

        Assert.Equal<ParameterName list>(
            [ ParameterName "rotationUncertainty"; ParameterName "elementUncertainty"; ParameterName "throughputUncertainty" ],
            axes |> List.map _.name)
        Assert.Equal<ParameterKind list>([ AngleParameter; FractionParameter; FractionParameter ], axes |> List.map _.kind)
        Assert.Equal<float list>([ 0.001; 0.02; 0.03 ], axes |> List.map _.defaultValue.value)

        let kindsInTagOrder =
            [ AngleParameter; PhaseParameter; FractionParameter; ScaleParameter; MuellerElementParameter; ThicknessParameter; ReadingParameter ]

        Assert.Equal<int list>([ 1; 2; 3; 4; 5; 6; 7 ], kindsInTagOrder |> List.map _.value)
        Assert.Equal<ParameterKind option list>(kindsInTagOrder |> List.map Some, kindsInTagOrder |> List.map (fun k -> ParameterKind.tryOfWire k.value))
        Assert.Equal<ParameterKind option>(None, ParameterKind.tryOfWire 0)
        Assert.Equal<ParameterKind option>(None, ParameterKind.tryOfWire 8)
        Assert.Equal<string list>(
            [ "angle"; "phase"; "fraction"; "scale"; "Mueller element"; "thickness"; "reading" ],
            kindsInTagOrder |> List.map _.text)
