namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
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

    let private quarterWave = ParameterValue (Math.PI / 2.0)

    let private realLpByExtinction : OpticalForm = RealPolarizerModel ExtinctionRatioLeakage

    let private constantRetarder : OpticalForm = RetarderPlateModel ConstantRetardance

    /// The 001 §I circular polarizer: a real LP at 0°, then a constant retarder at 45°, in light order.
    let private circularPolarizer : OpticalForm =
        CompoundModel
            [
                { form = realLpByExtinction; offset = Angle.zero }
                { form = constantRetarder; offset = Angle.degree 45.0<deg> }
            ]

    [<Fact>]
    let ``a real linear polarizer publishes its leakage and its axis offset`` () : unit =
        assertAxesWithin
            [
                { name = ParameterName "extinctionRatio"; kind = FractionParameter; defaultValue = ParameterValue 0.0 }
                { name = ParameterName "axisOffset"; kind = AngleParameter; defaultValue = ParameterValue 0.0 }
            ]
            realLpByExtinction.parameterAxes
        assertAxesWithin
            [
                { name = ParameterName "diattenuation"; kind = FractionParameter; defaultValue = ParameterValue 1.0 }
                { name = ParameterName "axisOffset"; kind = AngleParameter; defaultValue = ParameterValue 0.0 }
            ]
            (RealPolarizerModel DiattenuationLeakage).parameterAxes
        Assert.Equal<ItemParameterAxis list>(realLpByExtinction.parameterAxes, (OpticalElementModel realLpByExtinction).parameterAxes)

    [<Fact>]
    let ``a retarder plate publishes its retardance at a quarter wave and its fast-axis offset`` () : unit =
        let expected : ItemParameterAxis list =
            [
                { name = ParameterName "retardance"; kind = PhaseParameter; defaultValue = quarterWave }
                { name = ParameterName "fastAxisOffset"; kind = AngleParameter; defaultValue = ParameterValue 0.0 }
            ]

        assertAxesWithin expected constantRetarder.parameterAxes
        assertAxesWithin expected (RetarderPlateModel (InverseWaveLengthRetardance (WaveLength.nm 632.8<nm>))).parameterAxes
        Assert.Equal<ItemParameterAxis list>(constantRetarder.parameterAxes, (OpticalElementModel constantRetarder).parameterAxes)

    [<Fact>]
    let ``a compound publishes its components' parameters under their light-order prefixes`` () : unit =
        assertAxesWithin
            [
                { name = ParameterName "c1.extinctionRatio"; kind = FractionParameter; defaultValue = ParameterValue 0.0 }
                { name = ParameterName "c1.axisOffset"; kind = AngleParameter; defaultValue = ParameterValue 0.0 }
                { name = ParameterName "c2.retardance"; kind = PhaseParameter; defaultValue = quarterWave }
                { name = ParameterName "c2.fastAxisOffset"; kind = AngleParameter; defaultValue = ParameterValue 0.0 }
            ]
            circularPolarizer.parameterAxes

        let blackBoxThenCircular : OpticalForm =
            CompoundModel
                [
                    { form = BlackBoxMuellerModel SixteenElementMueller; offset = Angle.zero }
                    { form = circularPolarizer; offset = Angle.zero }
                ]

        let names = blackBoxThenCircular.parameterAxes |> List.map _.name
        Assert.Contains(ParameterName "c2.c1.extinctionRatio", names)
        Assert.Equal(16 + 4, names.Length)
        Assert.Equal<ParameterName>(ParameterName "c1.m00", names.Head)
        Assert.Equal<ParameterName list>(
            [
                ParameterName "c2.c1.extinctionRatio"
                ParameterName "c2.c1.axisOffset"
                ParameterName "c2.c2.retardance"
                ParameterName "c2.c2.fastAxisOffset"
            ],
            names |> List.skip 16)
        Assert.Equal<ItemParameterAxis list>(circularPolarizer.parameterAxes, (OpticalElementModel circularPolarizer).parameterAxes)

    [<Fact>]
    let ``a black-box Mueller matrix publishes sixteen elements, or a scale and fifteen, at the identity`` () : unit =
        let one = ParameterValue 1.0
        let zero = ParameterValue 0.0

        let element (name : ParameterName) (defaultValue : ParameterValue) : ItemParameterAxis =
            { name = name; kind = MuellerElementParameter; defaultValue = defaultValue }

        let sixteen : ItemParameterAxis list =
            [
                element (ParameterName "m00") one
                element (ParameterName "m01") zero
                element (ParameterName "m02") zero
                element (ParameterName "m03") zero
                element (ParameterName "m10") zero
                element (ParameterName "m11") one
                element (ParameterName "m12") zero
                element (ParameterName "m13") zero
                element (ParameterName "m20") zero
                element (ParameterName "m21") zero
                element (ParameterName "m22") one
                element (ParameterName "m23") zero
                element (ParameterName "m30") zero
                element (ParameterName "m31") zero
                element (ParameterName "m32") zero
                element (ParameterName "m33") one
            ]

        Assert.Equal<ItemParameterAxis list>(sixteen, (BlackBoxMuellerModel SixteenElementMueller).parameterAxes)

        let scaleAndFifteen = (BlackBoxMuellerModel FifteenElementsAndScale).parameterAxes
        Assert.Equal<ItemParameterAxis list>(
            { name = ParameterName "scale"; kind = ScaleParameter; defaultValue = one } :: List.tail sixteen,
            scaleAndFifteen)
        Assert.DoesNotContain(ParameterName "m00", scaleAndFifteen |> List.map _.name)
        Assert.Equal(1 + 15, scaleAndFifteen.Length)

    [<Fact>]
    let ``an empty slot publishes nothing and each detector its gain, dark offset and noise as its form says`` () : unit =
        let noise = ItemParameterAxis.detectorNoiseAxes noiseRotation noiseElement noiseThroughput

        Assert.Equal<ItemParameterAxis list>([], EmptySlotModel.parameterAxes)
        Assert.Equal<ItemParameterAxis list>(
            [
                { name = ParameterName "gain"; kind = ScaleParameter; defaultValue = ParameterValue 1.0 }
                { name = ParameterName "darkOffset"; kind = ReadingParameter; defaultValue = ParameterValue 0.0 }
                { name = ParameterName "rotationUncertainty"; kind = AngleParameter; defaultValue = noiseRotation }
                { name = ParameterName "elementUncertainty"; kind = FractionParameter; defaultValue = noiseElement }
                { name = ParameterName "throughputUncertainty"; kind = FractionParameter; defaultValue = noiseThroughput }
            ],
            (IntensityDetectorModel noise).parameterAxes)
        Assert.Equal<ItemParameterAxis list>(noise, (IntensityDetectorModel noise).parameterAxes |> List.skip 2)
        Assert.Equal<ItemParameterAxis list>(noise, (MuellerPolarimeterModel noise).parameterAxes)
        Assert.Equal<ItemParameterAxis list>(noise, (EllipsometerModel noise).parameterAxes)

        // The binding forms publish nothing of their own: a binding of their kind supplies its entry's axes (S7.1).
        let source = BuiltinCatalogue.source PresetIds.heNe633
        let sample = BuiltinCatalogue.sample SampleIds.glassPlate1mmId

        Assert.Equal<ItemParameterAxis list>([], SourcePresetItemModel.parameterAxes)
        Assert.Equal<ItemParameterAxis list>([], IdealPolarizerItemModel.parameterAxes)
        Assert.Equal<ItemParameterAxis list>([], StackSampleItemModel.parameterAxes)
        Assert.Equal<ItemModelBindingKind option>(Some SourcePresetBinding, SourcePresetItemModel.bindingKindOpt)
        Assert.Equal<ItemModelBindingKind option>(Some PolarizerPresetBinding, IdealPolarizerItemModel.bindingKindOpt)
        Assert.Equal<ItemModelBindingKind option>(Some StackSampleBinding, StackSampleItemModel.bindingKindOpt)
        Assert.Equal<ItemModelBindingKind option>(None, EmptySlotModel.bindingKindOpt)
        Assert.Equal<ItemModelBindingKind option>(None, (IntensityDetectorModel noise).bindingKindOpt)
        Assert.Equal<ItemModelBindingKind option>(None, (OpticalElementModel circularPolarizer).bindingKindOpt)

        Assert.Equal<ItemParameterAxis list option>(
            Some source.parameterAxes,
            SourcePresetItemModel.tryBoundParameterAxes (SourcePresetBound source))
        Assert.Equal<ItemParameterAxis list option>(
            Some sample.parameterAxes,
            StackSampleItemModel.tryBoundParameterAxes (StackSampleBound sample))
        Assert.Equal<ItemParameterAxis list option>(None, StackSampleItemModel.tryBoundParameterAxes (SourcePresetBound source))
        Assert.Equal<ItemParameterAxis list option>(None, EmptySlotModel.tryBoundParameterAxes (StackSampleBound sample))

        match BuiltinCatalogue.preset PresetIds.polarizerLp with
        | PolarizerItem polarizer ->
            Assert.Equal<ItemParameterAxis list option>(
                Some polarizer.parameterAxes,
                IdealPolarizerItemModel.tryBoundParameterAxes (PolarizerPresetBound polarizer))
        | other -> Assert.Fail($"the Built-in LP preset must be a polarizer, got '{other.displayName}'")
