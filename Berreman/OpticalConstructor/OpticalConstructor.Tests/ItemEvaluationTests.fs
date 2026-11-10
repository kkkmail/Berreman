namespace OpticalConstructor.Tests

open System
open Xunit
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.ItemEvaluation

/// The numbers the item-model evaluator reads, in `ItemEvaluation` (spec 0050 S8.1; 001 §I, §N).
module ItemEvaluationTests =

    let private assertValue (expected : ParameterValue) (values : ParameterValues) (name : ParameterName) : unit =
        Assert.Equal<Result<ParameterValue, ItemEvaluationError>>(Ok expected, values.valueOf name)

    let private assertChecked (expected : Result<ParameterValues, ItemEvaluationError>) (actual : Result<ParameterValues, ItemEvaluationError>) : unit =
        Assert.Equal<Result<ParameterValues, ItemEvaluationError>>(expected, actual)

    let private outOfRange (reason : string) : Result<ParameterValues, ItemEvaluationError> = Error (ParameterOutOfRangeErr reason)

    [<Fact>]
    let ``parameter values are read from an item's parameter states`` () : unit =
        let parameters : Parameter list =
            [
                { name = ParameterName "a"; state = KnownParameter (ParameterValue 0.1) }
                {
                    name = ParameterName "b"
                    state =
                        UnknownParameter
                            {
                                start = ParameterValue 0.5
                                bounds = { lower = ParameterValue 0.0; upper = ParameterValue 1.0 }
                                priorOpt = None
                            }
                }
                {
                    name = ParameterName "c"
                    state =
                        CalibratedParameter
                            {
                                value = ParameterValue 0.25
                                sigma = ParameterSigma 0.01
                                fitRun = FitRunName "S@20261101T120000Z"
                            }
                }
            ]

        let values = ParameterValues.ofParameters parameters

        assertValue (ParameterValue 0.1) values (ParameterName "a")
        assertValue (ParameterValue 0.5) values (ParameterName "b")
        assertValue (ParameterValue 0.25) values (ParameterName "c")
        Assert.Equal<Result<ParameterValue, ItemEvaluationError>>(Error (MissingParameterValueErr "d"), values.valueOf (ParameterName "d"))

    [<Fact>]
    let ``defaults come from a model's axes and one value can be replaced`` () : unit =
        let axes = (OpticalElementModel (RetarderPlateModel ConstantRetardance)).parameterAxes
        let defaults = ParameterValues.defaultsOf axes

        assertValue (ParameterValue (Math.PI / 2.0)) defaults ParameterName.retardance
        assertValue (ParameterValue 0.0) defaults ParameterName.fastAxisOffset

        let quarter = ParameterValue (Math.PI / 4.0)
        let replaced = defaults.withValue ParameterName.retardance quarter

        assertValue quarter replaced ParameterName.retardance
        assertValue (ParameterValue 0.0) replaced ParameterName.fastAxisOffset
        assertValue (ParameterValue (Math.PI / 2.0)) defaults ParameterName.retardance

        // A name the values do not hold is added beside the others.
        let added = replaced.withValue ParameterName.dialZero (ParameterValue 0.3)
        assertValue (ParameterValue 0.3) added ParameterName.dialZero
        assertValue quarter added ParameterName.retardance

    [<Fact>]
    let ``values are checked against a model's axes and their kinds' ranges`` () : unit =
        let axes = SourcePreset.lightSourceAxes
        let defaults = ParameterValues.defaultsOf axes
        let checkedWith (name : ParameterName) (value : ParameterValue) : Result<ParameterValues, ItemEvaluationError> =
            (defaults.withValue name value).checkedFor axes

        assertChecked (Ok defaults) (defaults.checkedFor axes)

        assertChecked
            (outOfRange "degreeOfPolarization = 1.5 lies outside [0, 1]")
            (checkedWith ParameterName.degreeOfPolarization (ParameterValue 1.5))
        assertChecked
            (outOfRange "degreeOfPolarization = -0.1 lies outside [0, 1]")
            (checkedWith ParameterName.degreeOfPolarization (ParameterValue -0.1))
        assertChecked
            (outOfRange "intensityScale = -1 lies outside [0, ∞)")
            (checkedWith ParameterName.intensityScale (ParameterValue -1.0))
        assertChecked
            (outOfRange "intensityScale = Infinity lies outside [0, ∞)")
            (checkedWith ParameterName.intensityScale (ParameterValue Double.PositiveInfinity))
        assertChecked (outOfRange "azimuth = NaN lies outside (-∞, ∞)") (checkedWith ParameterName.azimuth (ParameterValue Double.NaN))

        // An angle takes any finite number: 7 rad is kept as given, not wrapped.
        let turned = defaults.withValue ParameterName.azimuth (ParameterValue 7.0)
        assertChecked (Ok turned) (turned.checkedFor axes)

        // The first failure in axis order is the one answered: azimuth leads intensityScale.
        let twice =
            (defaults.withValue ParameterName.intensityScale (ParameterValue -1.0)).withValue ParameterName.azimuth (ParameterValue Double.NaN)
        assertChecked (outOfRange "azimuth = NaN lies outside (-∞, ∞)") (twice.checkedFor axes)

        let withoutEllipticity = ParameterValues.defaultsOf (axes |> List.filter (fun axis -> axis.name <> ParameterName.ellipticity))
        assertChecked (Error (MissingParameterValueErr "ellipticity")) (withoutEllipticity.checkedFor axes)

        // A value the axes do not name is kept and ignored, though it lies outside a fraction's range.
        let extra = defaults.withValue ParameterName.extinctionRatio (ParameterValue 2.0)
        assertChecked (Ok extra) (extra.checkedFor axes)

        let plateAxes = (BuiltinCatalogue.sample SampleIds.glassPlate1mmId).parameterAxes
        let plate = ParameterValues.defaultsOf plateAxes
        assertChecked (Ok plate) (plate.checkedFor plateAxes)
        assertChecked
            (outOfRange "substrate.thickness = -0.001 lies outside [0, ∞)")
            ((plate.withValue ParameterName.substrateThickness (ParameterValue -1e-3)).checkedFor plateAxes)

    [<Fact>]
    let ``every evaluation error renders its condition and its payload`` () : unit =
        let cases : (ItemEvaluationError * string) list =
            [
                MissingParameterValueErr "ellipticity", "the values hold no number for the parameter: ellipticity"
                ParameterOutOfRangeErr "degreeOfPolarization = 1.5 lies outside [0, 1]",
                "the parameter value is outside its range: degreeOfPolarization = 1.5 lies outside [0, 1]"
            ]

        for error, expected in cases do
            Assert.Equal<string>(expected, error.describe)

        let descriptions = cases |> List.map (fun (error, _) -> error.describe)
        Assert.Equal(2, descriptions |> List.distinct |> List.length)
