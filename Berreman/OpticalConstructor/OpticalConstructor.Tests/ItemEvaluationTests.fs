namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.ExperimentRun
open OpticalConstructor.Domain.ItemEvaluation

/// The numbers the item-model evaluator reads, in `ItemEvaluation` (spec 0050 S8.1; 001 §I, §N).
module ItemEvaluationTests =

    let private assertValue (expected : ParameterValue) (values : ParameterValues) (name : ParameterName) : unit =
        Assert.Equal<Result<ParameterValue, ItemEvaluationError>>(Ok expected, values.valueOf name)

    let private assertChecked (expected : Result<ParameterValues, ItemEvaluationError>) (actual : Result<ParameterValues, ItemEvaluationError>) : unit =
        Assert.Equal<Result<ParameterValues, ItemEvaluationError>>(expected, actual)

    let private outOfRange (reason : string) : Result<ParameterValues, ItemEvaluationError> = Error (ParameterOutOfRangeErr reason)

    let private assertEmission (expected : Result<SourceEmission, ItemEvaluationError>) (actual : Result<SourceEmission, ItemEvaluationError>) : unit =
        Assert.Equal<Result<SourceEmission, ItemEvaluationError>>(expected, actual)

    let private sourceDefaults : ParameterValues = ParameterValues.defaultsOf SourcePreset.lightSourceAxes

    /// Unpolarized light at unit scale, what the light-source axes' defaults describe (spec 0050 S8.1).
    let private defaultEmission : SourceEmission =
        {
            azimuth = Angle.zero
            ellipticity = Angle.zero
            degreeOfPolarization = ParameterValue 0.0
            intensityScale = ParameterValue 1.0
        }

    let private stokesComponents (stokes : StokesVector) : float list = [ stokes.s0; stokes.s1; stokes.s2; stokes.s3 ]

    /// Each Stokes component within 1e-15 of the expected one (spec 0050 S8.1).
    let private assertStokesWithin (expected : StokesVector) (actual : StokesVector) : unit =
        List.zip (stokesComponents expected) (stokesComponents actual)
        |> List.iteri (fun i (e, a) -> Assert.True(abs (e - a) <= 1.0e-15, $"S{i}: expected {e:R}, got {a:R}"))

    let private emittedStokes (values : ParameterValues) : StokesVector =
        match SourceEmission.ofValues values with
        | Ok emission -> emission.stokes
        | Error error -> failwith error.describe

    /// The evaluation is a matrix equal to the expected one element by element, exactly (spec 0050 S8.1).
    let private assertMuellerExact (expected : MuellerMatrix) (actual : Result<MuellerMatrix, ItemEvaluationError>) : unit =
        match actual with
        | Ok matrix ->
            for i in 0 .. 3 do
                for j in 0 .. 3 do
                    let e = expected.element i j
                    let a = matrix.element i j
                    Assert.True((e = a), $"element ({i},{j}): expected {e:R}, got {a:R}")
        | Error error -> Assert.Fail error.describe

    let private assertUnmodelledBranch (reason : string) (actual : Result<MuellerMatrix, ItemEvaluationError>) : unit =
        match actual with
        | Error error -> Assert.Equal<ItemEvaluationError>(UnmodelledBranchErr reason, error)
        | Ok _ -> Assert.Fail $"expected UnmodelledBranchErr '{reason}', got a Mueller matrix"

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
        let polarizerReason = "'Ideal polarizer (preset binding)' (d47e5ce3-3e74-5058-848e-3aa54930e377) is an ideal polarizer"
        let cases : (ItemEvaluationError * string) list =
            [
                MissingParameterValueErr "ellipticity", "the values hold no number for the parameter: ellipticity"
                ParameterOutOfRangeErr "degreeOfPolarization = 1.5 lies outside [0, 1]",
                "the parameter value is outside its range: degreeOfPolarization = 1.5 lies outside [0, 1]"
                NotALightSourceErr polarizerReason, $"the model is not a light source: {polarizerReason}"
                UnmodelledBranchErr "the reflected branch of a polarizer preset, which only transmits",
                "the element has no such branch: the reflected branch of a polarizer preset, which only transmits"
            ]

        for error, expected in cases do
            Assert.Equal<string>(expected, error.describe)

        let descriptions = cases |> List.map (fun (error, _) -> error.describe)
        Assert.Equal(4, descriptions |> List.distinct |> List.length)

    [<Fact>]
    let ``the default source is unpolarized light at unit scale`` () : unit =
        assertEmission (Ok defaultEmission) (SourceEmission.ofValues sourceDefaults)

        // Exactly, not within a tolerance: p = 0 zeroes every polarized component.
        Assert.Equal<float list>([ 1.0; 0.0; 0.0; 0.0 ], stokesComponents defaultEmission.stokes)
        Assert.Equal<float list>(stokesComponents Propagation.unpolarizedStokes, stokesComponents (emittedStokes sourceDefaults))

    [<Fact>]
    let ``a source emits its polarization state times its intensity scale`` () : unit =
        let fullyPolarized = sourceDefaults.withValue ParameterName.degreeOfPolarization (ParameterValue 1.0)

        // Degree of polarization 1 at azimuth 30° and scale 2: twice an ideal linear polarizer's state at 30°.
        let linear30 = Propagation.inputStokes IdealLinear (Angle.degree 30.0<deg>)
        let scaledAt30 =
            (fullyPolarized.withValue ParameterName.azimuth (ParameterValue (Math.PI / 6.0))).withValue
                ParameterName.intensityScale
                (ParameterValue 2.0)
        assertStokesWithin (linear30 + linear30) (emittedStokes scaledAt30)

        // A positive ellipticity angle is +S₃, the handedness `inputStokes` calls left.
        assertStokesWithin
            (Propagation.inputStokes IdealCircularLeft (Angle.degree 0.0<deg>))
            (emittedStokes (fullyPolarized.withValue ParameterName.ellipticity (ParameterValue (Math.PI / 4.0))))
        assertStokesWithin
            (Propagation.inputStokes IdealCircularRight (Angle.degree 0.0<deg>))
            (emittedStokes (fullyPolarized.withValue ParameterName.ellipticity (ParameterValue (-Math.PI / 4.0))))

        // Half polarized at azimuth 0: the unpolarized half adds to S₀ only.
        assertStokesWithin
            (StokesVector.create [ 1.0; 0.5; 0.0; 0.0 ])
            (emittedStokes (sourceDefaults.withValue ParameterName.degreeOfPolarization (ParameterValue 0.5)))

    [<Fact>]
    let ``only a light-source model emits, and its values must be in range`` () : unit =
        // Every model is a shipped entry of step 210; every binding is a Built-in preset or sample.
        let sourceModel = BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId
        let polarizerModel = BuiltinCatalogue.itemModel ItemModelIds.idealPolarizerItemModelId
        let sampleModel = BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId
        let detectorModel = BuiltinCatalogue.itemModel ItemModelIds.intensityDetectorModelId
        let linearPolarizer =
            BuiltinCatalogue.presets ()
            |> List.pick (fun entry ->
                match entry with
                | PolarizerItem polarizer when polarizer.id = PresetIds.polarizerLp -> Some polarizer
                | PolarizerItem _ | SampleItem _ | SourceItem _ | DetectorItem _ -> None)
        let plate = BuiltinCatalogue.sample SampleIds.glassPlate1mmId

        let notALightSource (model : ItemModelVersion) (kind : string) : Result<SourceEmission, ItemEvaluationError> =
            Error (NotALightSourceErr $"'{model.entry.name.value}' ({model.entry.id.value}) is {kind}")

        // Whatever the values, a polarizer emits nothing: the refusal is the model's, not its numbers'.
        let polarizer = PolarizerPresetChoice (polarizerModel, linearPolarizer)
        assertEmission (notALightSource polarizerModel "an ideal polarizer") (polarizer.emissionWith sourceDefaults)
        assertEmission (notALightSource polarizerModel "an ideal polarizer") (polarizer.emissionWith (ParameterValues Map.empty))
        assertEmission
            (notALightSource sampleModel "a stack sample")
            ((StackSampleChoice (sampleModel, SampleVersionId.firstOf plate.id, plate)).emissionWith sourceDefaults)
        assertEmission (notALightSource detectorModel "an intensity detector") ((LibraryModelChoice detectorModel).emissionWith sourceDefaults)

        let heNe = SourcePresetChoice (sourceModel, BuiltinCatalogue.source PresetIds.heNe633)
        assertEmission (Ok defaultEmission) (heNe.emissionWith sourceDefaults)
        assertEmission
            (Error (ParameterOutOfRangeErr "degreeOfPolarization = 2 lies outside [0, 1]"))
            (heNe.emissionWith (sourceDefaults.withValue ParameterName.degreeOfPolarization (ParameterValue 2.0)))

        let withoutEllipticity =
            ParameterValues.defaultsOf (SourcePreset.lightSourceAxes |> List.filter (fun axis -> axis.name <> ParameterName.ellipticity))
        assertEmission (Error (MissingParameterValueErr "ellipticity")) (heNe.emissionWith withoutEllipticity)

    [<Fact>]
    let ``a polarizer preset is its behaviour's Mueller matrix at the element's rotation`` () : unit =
        // Two reference ideal LPs at offsets 20° and 65°, the compound `PolarizerBehaviorTests` builds.
        let referenceLp = Propagation.analyzerMueller IdealLinear Angle.zero
        let first : MuellerComponent = { matrix = referenceLp; offset = Angle.degree 20.0<deg> }
        let second : MuellerComponent = { matrix = referenceLp; offset = Angle.degree 65.0<deg> }
        let behaviors : PolarizerBehavior list =
            [
                ComputedIdeal IdealLinear
                ComputedIdeal IdealCircularLeft
                ComputedIdeal IdealCircularRight
                ConstantMueller [ first; second ]
            ]
        let rotations : Angle list = [ 0.0<deg>; 30.0<deg>; 45.0<deg>; 90.0<deg>; 135.0<deg>; 330.0<deg> ] |> List.map Angle.degree
        let at600 = WaveLength.nm 600.0<nm>

        for behavior in behaviors do
            let preset = BoundPolarizerPreset behavior

            for rotation in rotations do
                let transmitting = ElementOrientation.transmittingAt rotation
                assertMuellerExact (Propagation.behaviorMueller behavior rotation) (preset.muellerAt transmitting at600)

                // The incidence plays no part: the preset met obliquely is the same matrix.
                let oblique = { transmitting with incidence = IncidenceAngle.create (Angle.degree 40.0<deg>) }
                assertMuellerExact (Propagation.behaviorMueller behavior rotation) (preset.muellerAt oblique at600)

    [<Fact>]
    let ``an empty slot is the identity, and an element that only transmits has no reflected branch`` () : unit =
        let thirty = Angle.degree 30.0<deg>
        let at30 = ElementOrientation.transmittingAt thirty
        let at400 = WaveLength.nm 400.0<nm>
        let emptySlot : BoundOpticalElement = BoundEmptySlot
        let linearPolarizer : BoundOpticalElement = BoundPolarizerPreset (ComputedIdeal IdealLinear)

        Assert.Equal<ElementOrientation>(
            { rotation = thirty; incidence = IncidenceAngle.normal; branch = Propagation.BranchTransmitted },
            at30
        )

        assertMuellerExact Propagation.identityMueller (emptySlot.muellerAt at30 at400)

        let reflected = { at30 with branch = Propagation.BranchReflected }
        assertUnmodelledBranch "the reflected branch of an empty slot, which only transmits" (emptySlot.muellerAt reflected at400)
        assertUnmodelledBranch "the reflected branch of a polarizer preset, which only transmits" (linearPolarizer.muellerAt reflected at400)
