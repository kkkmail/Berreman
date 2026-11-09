namespace OpticalConstructor.Tests

open Xunit
open Berreman.Constants
open Berreman.Media
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels

/// The parameter vocabulary of items and item models in `ItemModels` (spec 0050 S7.1, 001 §I).
module ItemModelsTests =

    let private noiseRotation = ParameterValue 0.001
    let private noiseElement = ParameterValue 0.02
    let private noiseThroughput = ParameterValue 0.03

    /// Names and kinds compare by `=`; each default within 1e-15, as catalogue thicknesses are read back from text.
    let private assertAxesWithin (expected : ItemParameterAxis list) (actual : ItemParameterAxis list) : unit =
        Assert.Equal<(ParameterName * ParameterKind) list>(
            expected |> List.map (fun axis -> axis.name, axis.kind),
            actual |> List.map (fun axis -> axis.name, axis.kind))
        List.iter2
            (fun (want : ItemParameterAxis) (got : ItemParameterAxis) -> Assert.Equal(want.defaultValue.value, got.defaultValue.value, 1e-15))
            expected
            actual

    [<Fact>]
    let ``the moved parameter name and dial sign keep their values`` () : unit =
        Assert.Equal("c2.retardance", (ParameterName "c2.retardance").value)
        Assert.Equal<float>(1.0, DialPositive.value)
        Assert.Equal<float>(-1.0, DialNegative.value)

    /// A data projection over test-local values (spec 0050 001 §I): no bench noise model is exercised here.
    [<Fact>]
    let ``the detector noise axes keep their declared quantities`` () : unit =
        let axes = ItemParameterAxis.detectorNoiseAxes noiseRotation noiseElement noiseThroughput

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

    [<Fact>]
    let ``a source preset publishes the light-source parameters, unpolarized at unit scale`` () : unit =
        let expected : ItemParameterAxis list =
            [
                { name = ParameterName "azimuth"; kind = AngleParameter; defaultValue = ParameterValue 0.0 }
                { name = ParameterName "ellipticity"; kind = AngleParameter; defaultValue = ParameterValue 0.0 }
                { name = ParameterName "degreeOfPolarization"; kind = FractionParameter; defaultValue = ParameterValue 0.0 }
                { name = ParameterName "intensityScale"; kind = ScaleParameter; defaultValue = ParameterValue 1.0 }
            ]

        Assert.Equal<ItemParameterAxis list>(expected, (BuiltinCatalogue.source PresetIds.heNe633).parameterAxes)

    [<Fact>]
    let ``an ideal polarizer preset publishes no parameter`` () : unit =
        match BuiltinCatalogue.preset PresetIds.polarizerLp with
        | PolarizerItem ({ behavior = ComputedIdeal IdealLinear } as polarizer) ->
            Assert.Equal<ItemParameterAxis list>([], polarizer.parameterAxes)
        | other -> Assert.Fail($"the Built-in LP preset must be an ideal linear polarizer, got '{other.displayName}'")

    [<Fact>]
    let ``a sample publishes one thickness per finite layer`` () : unit =
        assertAxesWithin
            [ { name = ParameterName "substrate.thickness"; kind = ThicknessParameter; defaultValue = ParameterValue 1e-3 } ]
            (BuiltinCatalogue.sample SampleIds.glassPlate1mmId).parameterAxes
        assertAxesWithin
            [ { name = ParameterName "film1.thickness"; kind = ThicknessParameter; defaultValue = ParameterValue 6e-7 } ]
            (BuiltinCatalogue.sample SampleIds.glassFilm600Id).parameterAxes

        let layerOf (thickness : Thickness) : SampleLayer =
            { materialId = MaterialVersionId.firstOf MaterialIds.glass175; thickness = thickness; orientation = PrimaryAxes }

        let stacked =
            {
                BuiltinCatalogue.sample SampleIds.glassFilm600Id with
                    structure =
                        {
                            films =
                                [
                                    SingleLayer (layerOf (Thickness.nm 5.0<nm>))
                                    Repeated
                                        {
                                            cell = [ layerOf (Thickness.nm 10.0<nm>); layerOf (Thickness.nm 20.0<nm>) ]
                                            count = PeriodCount 3
                                        }
                                ]
                            substrate = None
                            lower = None
                        }
            }

        assertAxesWithin
            [
                { name = ParameterName "film1.thickness"; kind = ThicknessParameter; defaultValue = ParameterValue 5e-9 }
                { name = ParameterName "film2.layer1.thickness"; kind = ThicknessParameter; defaultValue = ParameterValue 1e-8 }
                { name = ParameterName "film2.layer2.thickness"; kind = ThicknessParameter; defaultValue = ParameterValue 2e-8 }
            ]
            stacked.parameterAxes

        let semiInfinite = Thickness.Infinity
        Assert.Equal<ParameterValue option>(None, semiInfinite.parameterValueOpt)

    [<Fact>]
    let ``a noise budget publishes its three uncertainties at its own values`` () : unit =
        let expected : ItemParameterAxis list =
            [
                { name = ParameterName "rotationUncertainty"; kind = AngleParameter; defaultValue = noiseRotation }
                { name = ParameterName "elementUncertainty"; kind = FractionParameter; defaultValue = noiseElement }
                { name = ParameterName "throughputUncertainty"; kind = FractionParameter; defaultValue = noiseThroughput }
            ]

        Assert.Equal<ItemParameterAxis list>(expected, ItemParameterAxis.detectorNoiseAxes noiseRotation noiseElement noiseThroughput)
