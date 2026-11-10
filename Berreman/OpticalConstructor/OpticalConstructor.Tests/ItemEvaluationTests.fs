namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open Berreman.Media
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.MaterialStore
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

    /// The evaluation is a matrix within 1e-12 of the expected elements, element by element (spec 0050 S8.1).
    let private assertElementsWithin (expectedAt : int -> int -> float) (actual : Result<MuellerMatrix, ItemEvaluationError>) : unit =
        match actual with
        | Ok matrix ->
            for i in 0 .. 3 do
                for j in 0 .. 3 do
                    let e = expectedAt i j
                    let a = matrix.element i j
                    Assert.True(abs (e - a) <= 1.0e-12, $"element ({i},{j}): expected {e:R}, got {a:R}")
        | Error error -> Assert.Fail error.describe

    let private assertMuellerWithin (expected : MuellerMatrix) (actual : Result<MuellerMatrix, ItemEvaluationError>) : unit =
        assertElementsWithin expected.element actual

    let private assertWithin (expected : float) (actual : float) (what : string) : unit =
        Assert.True(abs (expected - actual) <= 1.0e-12, $"{what}: expected {expected:R}, got {actual:R}")

    let private matrixOf (actual : Result<MuellerMatrix, ItemEvaluationError>) : MuellerMatrix =
        match actual with
        | Ok matrix -> matrix
        | Error error -> failwith error.describe

    /// The form bound to the values and evaluated on its transmitted branch at the rotation (spec 0050 S8.1).
    let private formMuellerAt (form : OpticalForm) (values : ParameterValues) (rotation : Angle) (w : WaveLength) : Result<MuellerMatrix, ItemEvaluationError> =
        BoundOpticalElement.ofOpticalForm values form
        |> Result.bind (fun bound -> bound.muellerAt (ElementOrientation.transmittingAt rotation) w)

    let private formDefaults (form : OpticalForm) : ParameterValues = ParameterValues.defaultsOf form.parameterAxes

    let private assertBound (expected : Result<BoundOpticalElement, ItemEvaluationError>) (actual : Result<BoundOpticalElement, ItemEvaluationError>) : unit =
        Assert.Equal<Result<BoundOpticalElement, ItemEvaluationError>>(expected, actual)

    let private at600 : WaveLength = WaveLength.nm 600.0<nm>

    /// The sample bound at the values over the store, evaluated at the orientation and λ (spec 0050 S8.1).
    let private stackSampleMuellerAt
        (materials : MaterialProxy)
        (values : ParameterValues)
        (sample : Sample)
        (orientation : ElementOrientation)
        (w : WaveLength)
        : Result<MuellerMatrix, ItemEvaluationError> =
        BoundOpticalElement.ofStackSample materials values sample
        |> Result.bind (fun bound -> bound.muellerAt orientation w)

    /// The sample's materials resolved as the table's run resolves them (spec 0050 S8.1; 001 §N).
    let private resolvedAsTheTable (materials : MaterialProxy) (sample : Sample) : Propagation.ResolvedSample =
        match Propagation.resolveSampleMaterials materials sample with
        | Ok resolved -> resolved
        | Error error -> failwith (SampleMaterialErr error).describe

    let private assertSample (expected : Result<Sample, ItemEvaluationError>) (actual : Result<Sample, ItemEvaluationError>) : unit =
        Assert.Equal<Result<Sample, ItemEvaluationError>>(expected, actual)

    let private sampledRotations : Angle list = [ 0.0<deg>; 30.0<deg>; 135.0<deg> ] |> List.map Angle.degree

    let private lpByExtinction : OpticalForm = RealPolarizerModel ExtinctionRatioLeakage

    /// A real LP by extinction ratio at offset 0°, then a constant retarder at offset 45°: at its defaults, an ideal LP
    /// and a quarter wave (spec 0050 S8.1).
    let private quarterWaveAfterLp : OpticalForm =
        let lp : CompoundComponent = { form = lpByExtinction; offset = Angle.zero }
        let retarder : CompoundComponent = { form = RetarderPlateModel ConstantRetardance; offset = Angle.degree 45.0<deg> }
        CompoundModel [ lp; retarder ]

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
        let unknownGlass = MaterialError.unknownMaterialVersion (MaterialVersionId.firstOf MaterialIds.glass152)
        let cases : (ItemEvaluationError * string) list =
            [
                MissingParameterValueErr "ellipticity", "the values hold no number for the parameter: ellipticity"
                ParameterOutOfRangeErr "degreeOfPolarization = 1.5 lies outside [0, 1]",
                "the parameter value is outside its range: degreeOfPolarization = 1.5 lies outside [0, 1]"
                NotALightSourceErr polarizerReason, $"the model is not a light source: {polarizerReason}"
                UnmodelledBranchErr "the reflected branch of a polarizer preset, which only transmits",
                "the element has no such branch: the reflected branch of a polarizer preset, which only transmits"
                SampleMaterialErr unknownGlass,
                "the sample's materials do not resolve: unknown material version '55750ee2-358c-40d4-ab6c-06fc144adbf8' v1"
                // Any material error's reason is the payload, not only an unknown version's.
                SampleMaterialErr (MaterialStoreUnavailableErr "the material store is closed"),
                "the sample's materials do not resolve: the material store is closed"
            ]

        for error, expected in cases do
            Assert.Equal<string>(expected, error.describe)

        let descriptions = cases |> List.map (fun (error, _) -> error.describe)
        Assert.Equal(6, descriptions |> List.distinct |> List.length)

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

    [<Fact>]
    let ``a real linear polarizer at its defaults is the ideal one, and its leakage passes crossed light`` () : unit =
        let byExtinction = formDefaults lpByExtinction
        let lpByDiattenuation = RealPolarizerModel DiattenuationLeakage
        let byDiattenuation = formDefaults lpByDiattenuation

        // Extinction ratio 0 and diattenuation 1 are both the ideal LP.
        for rotation in sampledRotations do
            let ideal = Propagation.analyzerMueller IdealLinear rotation
            assertMuellerWithin ideal (formMuellerAt lpByExtinction byExtinction rotation at600)
            assertMuellerWithin ideal (formMuellerAt lpByDiattenuation byDiattenuation rotation at600)

        // The axis offset (10°) adds to the element's rotation (30°).
        let offsetBy10 = byExtinction.withValue ParameterName.axisOffset (ParameterValue (Math.PI / 18.0))
        assertMuellerWithin
            (Propagation.analyzerMueller IdealLinear (Angle.degree 40.0<deg>))
            (formMuellerAt lpByExtinction offsetBy10 (Angle.degree 30.0<deg>) at600)

        // Crossed with an ideal LP at 90°, unpolarized light leaks half the extinction ratio.
        let leakyValues = byExtinction.withValue ParameterName.extinctionRatio (ParameterValue 0.01)
        let leaky = matrixOf (formMuellerAt lpByExtinction leakyValues Angle.zero at600)
        let crossed = Propagation.analyzerMueller IdealLinear (Angle.degree 90.0<deg>) * (leaky * Propagation.unpolarizedStokes)
        assertWithin 0.005 crossed.s0 "S0 through the leaky LP and a crossed ideal LP"

        // A diattenuation D is the extinction ratio (1 − D) / (1 + D).
        let diattenuated = byDiattenuation.withValue ParameterName.diattenuation (ParameterValue 0.98)
        let equivalent = byExtinction.withValue ParameterName.extinctionRatio (ParameterValue (0.02 / 1.98))

        for rotation in sampledRotations do
            assertMuellerWithin
                (matrixOf (formMuellerAt lpByExtinction equivalent rotation at600))
                (formMuellerAt lpByDiattenuation diattenuated rotation at600)

    [<Fact>]
    let ``a retarder plate is the retarder matrix at its fast axis, its retardance dispersed as its model says`` () : unit =
        let constant = RetarderPlateModel ConstantRetardance
        let sixtyDegrees =
            ((formDefaults constant).withValue ParameterName.retardance (ParameterValue (Math.PI / 3.0))).withValue
                ParameterName.fastAxisOffset
                (ParameterValue (Math.PI / 36.0))
        let at45 = MuellerReconstruction.retarderMueller (Angle.degree 45.0<deg>) (MuellerReconstruction.Retardance (Math.PI / 3.0))
        let at316 = WaveLength.nm 316.4<nm>
        let at633 = WaveLength.nm 632.8<nm>

        // A constant retardance is the same at every λ; the fast-axis offset (5°) adds to the rotation (40°).
        for w in [ at316; at600; WaveLength.nm 1550.0<nm> ] do
            assertMuellerWithin at45 (formMuellerAt constant sixtyDegrees (Angle.degree 40.0<deg>) w)

        // A quarter wave at 632.8 nm, dispersed as λ_ref / λ, is a half wave at 316.4 nm.
        let dispersion = InverseWaveLengthRetardance at633
        let zeroOrder = RetarderPlateModel dispersion
        let quarter = ParameterValue (Math.PI / 2.0)

        assertWithin Math.PI (quarter.retardanceAt dispersion at316).value "the dispersed retardance at 316.4 nm"
        assertWithin (Math.PI / 2.0) (quarter.retardanceAt dispersion at633).value "the dispersed retardance at 632.8 nm"
        assertWithin (Math.PI / 2.0) (quarter.retardanceAt ConstantRetardance at316).value "the constant retardance at 316.4 nm"

        for rotation in sampledRotations do
            assertMuellerWithin
                (MuellerReconstruction.retarderMueller rotation (MuellerReconstruction.Retardance Math.PI))
                (formMuellerAt zeroOrder (formDefaults zeroOrder) rotation at316)

    [<Fact>]
    let ``a black-box Mueller matrix is held at reference orientation and turned by the element's rotation`` () : unit =
        let sixteen = BlackBoxMuellerModel SixteenElementMueller
        let fifteen = BlackBoxMuellerModel FifteenElementsAndScale
        let referenceLp = Propagation.analyzerMueller IdealLinear Angle.zero
        let indices : int list = [ 0 .. 3 ]
        let elementName (i : int) (j : int) : ParameterName = ParameterName.muellerElement (ParameterNameIndex i) (ParameterNameIndex j)
        let holding (keep : int -> int -> bool) (valueAt : int -> int -> float) (start : ParameterValues) : ParameterValues =
            indices
            |> List.fold
                (fun (rows : ParameterValues) i ->
                    indices
                    |> List.filter (keep i)
                    |> List.fold (fun (held : ParameterValues) j -> held.withValue (elementName i j) (ParameterValue (valueAt i j))) rows)
                start

        // At its defaults the box is the identity, at any rotation.
        assertMuellerWithin Propagation.identityMueller (formMuellerAt sixteen (formDefaults sixteen) (Angle.degree 30.0<deg>) at600)

        // Holding the reference LP's sixteen elements, the box is the ideal LP at the element's rotation.
        let lpBox = holding (fun _ _ -> true) referenceLp.element (formDefaults sixteen)

        for rotation in sampledRotations do
            assertMuellerWithin (Propagation.analyzerMueller IdealLinear rotation) (formMuellerAt sixteen lpBox rotation at600)

        // A scale of 0.8 over the fifteen elements normalised by m00 (½) is 0.8 · 2 · the LP.
        let m00 = referenceLp.element 0 0
        let scaledBox =
            holding
                (fun i j -> i <> 0 || j <> 0)
                (fun i j -> referenceLp.element i j / m00)
                ((formDefaults fifteen).withValue ParameterName.blackBoxScale (ParameterValue 0.8))

        for rotation in sampledRotations do
            let lp = Propagation.analyzerMueller IdealLinear rotation
            assertElementsWithin (fun i j -> 0.8 * 2.0 * lp.element i j) (formMuellerAt fifteen scaledBox rotation at600)

    [<Fact>]
    let ``a compound applies its components in light order, each under its own prefixed parameters`` () : unit =
        let defaults = formDefaults quarterWaveAfterLp

        // The LP first, then the quarter wave at 45° to it: R(θ + 45°, π / 2) · LP(θ).
        for rotation in [ 0.0<deg>; 30.0<deg> ] |> List.map Angle.degree do
            let expected =
                MuellerReconstruction.retarderMueller (rotation + Angle.degree 45.0<deg>) (MuellerReconstruction.Retardance (Math.PI / 2.0))
                * Propagation.analyzerMueller IdealLinear rotation
            let compound = formMuellerAt quarterWaveAfterLp defaults rotation at600
            assertMuellerWithin expected compound

            // Unpolarized light leaves circular: |S₃| / S₀ = 1.
            let circular = matrixOf compound * Propagation.unpolarizedStokes
            assertWithin 1.0 (abs circular.s3 / circular.s0) "|S3| / S0 of unpolarized light through the compound"

        // As the second component after an identity black box, the inner LP's extinction ratio is c2.c1.extinctionRatio.
        let box : CompoundComponent = { form = BlackBoxMuellerModel SixteenElementMueller; offset = Angle.zero }
        let inner : CompoundComponent = { form = quarterWaveAfterLp; offset = Angle.zero }
        let nested = CompoundModel [ box; inner ]
        let nestedValues = (formDefaults nested).withValue (ParameterName "c2.c1.extinctionRatio") (ParameterValue 0.01)
        let innerValues = defaults.withValue (ParameterName "c1.extinctionRatio") (ParameterValue 0.01)
        let second = ParameterNameIndex 2

        Assert.Equal<Result<ParameterValues, ItemEvaluationError>>(
            Ok innerValues,
            nestedValues.forComponent second quarterWaveAfterLp.parameterAxes
        )

        let innerAlone = matrixOf (formMuellerAt quarterWaveAfterLp innerValues Angle.zero at600)
        assertMuellerWithin innerAlone (formMuellerAt nested nestedValues Angle.zero at600)

        // The leakage reached the inner LP: its m00 is ½ (1 + 0.01), the quarter wave keeping S₀.
        assertWithin 0.505 (innerAlone.element 0 0) "m00 of the leaky inner compound"

    [<Fact>]
    let ``a form is bound only with a value for each of its parameters, in range`` () : unit =
        let defaults = formDefaults lpByExtinction

        assertBound (Ok (BoundOpticalForm (lpByExtinction, defaults))) (BoundOpticalElement.ofOpticalForm defaults lpByExtinction)

        let withoutOffset =
            ParameterValues.defaultsOf (lpByExtinction.parameterAxes |> List.filter (fun axis -> axis.name <> ParameterName.axisOffset))
        assertBound (Error (MissingParameterValueErr "axisOffset")) (BoundOpticalElement.ofOpticalForm withoutOffset lpByExtinction)
        assertBound
            (Error (ParameterOutOfRangeErr "extinctionRatio = 1.2 lies outside [0, 1]"))
            (BoundOpticalElement.ofOpticalForm (defaults.withValue ParameterName.extinctionRatio (ParameterValue 1.2)) lpByExtinction)

        // A compound's values are checked under each component's prefixed names.
        let withoutRetardance =
            ParameterValues.defaultsOf (quarterWaveAfterLp.parameterAxes |> List.filter (fun axis -> axis.name <> ParameterName "c2.retardance"))
        let leakingTooMuch = (formDefaults quarterWaveAfterLp).withValue (ParameterName "c1.extinctionRatio") (ParameterValue 1.2)
        assertBound (Error (MissingParameterValueErr "c2.retardance")) (BoundOpticalElement.ofOpticalForm withoutRetardance quarterWaveAfterLp)
        assertBound
            (Error (ParameterOutOfRangeErr "c1.extinctionRatio = 1.2 lies outside [0, 1]"))
            (BoundOpticalElement.ofOpticalForm leakingTooMuch quarterWaveAfterLp)

        // A form passes the light it transmits: its reflected branch is unmodelled.
        let retarder = RetarderPlateModel ConstantRetardance

        match BoundOpticalElement.ofOpticalForm (formDefaults retarder) retarder with
        | Ok bound ->
            let at30 = ElementOrientation.transmittingAt (Angle.degree 30.0<deg>)
            let reflected : ElementOrientation = { at30 with branch = Propagation.BranchReflected }
            assertUnmodelledBranch "the reflected branch of an optical form, which only transmits" (bound.muellerAt reflected at600)
        | Error error -> Assert.Fail error.describe

        // A form built unchecked still answers the missing number, not a matrix.
        let unchecked : BoundOpticalElement = BoundOpticalForm (retarder, ParameterValues Map.empty)
        Assert.Equal<Result<MuellerMatrix, ItemEvaluationError>>(
            Error (MissingParameterValueErr "retardance"),
            unchecked.muellerAt (ElementOrientation.transmittingAt Angle.zero) at600
        )

    [<Fact>]
    let ``a stack sample at its own thicknesses is solved as the table solves it`` () : unit =
        let materials = BuiltinCatalogue.materialStore (fun _ -> [])
        let spin = Angle.degree 20.0<deg>
        let incidence = IncidenceAngle.create (Angle.degree 30.0<deg>)

        for id in [ SampleIds.glassPlate1mmId; SampleIds.glassFilm200Id ] do
            let sample = BuiltinCatalogue.sample id
            let defaults = ParameterValues.defaultsOf sample.parameterAxes
            let resolved = resolvedAsTheTable materials sample

            // At its own thicknesses the sample is unchanged.
            assertSample (Ok sample) (sample.withThicknessValues defaults)

            for branch in [ Propagation.BranchTransmitted; Propagation.BranchReflected ] do
                let orientation : ElementOrientation = { rotation = spin; incidence = incidence; branch = branch }
                assertMuellerExact
                    (Propagation.sampleMuellerSpun branch resolved at600 incidence spin)
                    (stackSampleMuellerAt materials defaults sample orientation at600)

    [<Fact>]
    let ``a stack sample's thickness parameters change the layers they name`` () : unit =
        let layerOf (material : MaterialId) (thickness : Thickness) : SampleLayer =
            { materialId = MaterialVersionId.firstOf material; thickness = thickness; orientation = PrimaryAxes }
        let film = layerOf MaterialIds.glass175 (Thickness.nm 5.0<nm>)
        let cellFirst = layerOf MaterialIds.glass152 (Thickness.nm 10.0<nm>)
        let cellSecond = layerOf MaterialIds.glass200 (Thickness.nm 20.0<nm>)
        let plate = layerOf MaterialIds.glass150 (Thickness.mm 1.0<mm>)

        // A 5 nm film, a 10 nm / 20 nm cell over three periods and a 1 mm plate, each layer of its own material.
        let structureOf (single : SampleLayer) (secondInCell : SampleLayer) (substrate : SampleLayer) : SampleStructure =
            {
                films = [ SingleLayer single; Repeated { cell = [ cellFirst; secondInCell ]; count = PeriodCount 3 } ]
                substrate = Some substrate
                lower = None
            }

        let stacked = { BuiltinCatalogue.sample SampleIds.glassFilm600Id with structure = structureOf film cellSecond plate }
        let defaults = ParameterValues.defaultsOf stacked.parameterAxes
        let film1 = ParameterName "film1.thickness"
        let changed =
            ((defaults.withValue film1 (ParameterValue 6.0e-9)).withValue (ParameterName "film2.layer2.thickness") (ParameterValue 2.5e-8)).withValue
                (ParameterName "substrate.thickness")
                (ParameterValue 2.0e-3)
        let sixNm = Thickness 6.0e-9<meter>
        let twentyFiveNm = Thickness 2.5e-8<meter>
        let twoMm = Thickness 2.0e-3<meter>
        let expected =
            { stacked with
                structure = structureOf { film with thickness = sixNm } { cellSecond with thickness = twentyFiveNm } { plate with thickness = twoMm } }

        let actual = stacked.withThicknessValues changed
        assertSample (Ok expected) actual

        // The cell is stored once, so its second layer is 25 nm in every period.
        match actual with
        | Ok held ->
            let ten = cellFirst.thickness
            Assert.Equal<Thickness list>(
                [ sixNm; ten; twentyFiveNm; ten; twentyFiveNm; ten; twentyFiveNm ],
                held.structure.expandedFilms |> List.map _.thickness
            )
        | Error error -> Assert.Fail error.describe

        assertSample
            (Error (ParameterOutOfRangeErr "film1.thickness = -1E-09 lies outside [0, ∞)"))
            (stacked.withThicknessValues (defaults.withValue film1 (ParameterValue -1.0e-9)))

        // A bound thickness reaches the solve: the 200 nm film at 300 nm is solved as a 300 nm film.
        let materials = BuiltinCatalogue.materialStore (fun _ -> [])
        let glassFilm200 = BuiltinCatalogue.sample SampleIds.glassFilm200Id
        let glassDefaults = ParameterValues.defaultsOf glassFilm200.parameterAxes
        let normal = ElementOrientation.transmittingAt Angle.zero
        let film300 =
            match glassFilm200.structure.films with
            | [ SingleLayer layer ] ->
                { glassFilm200 with
                    structure = { glassFilm200.structure with films = [ SingleLayer { layer with thickness = Thickness.nm 300.0<nm> } ] } }
            | _ -> failwith "the Built-in 200 nm glass film is one single film"
        let at300 = stackSampleMuellerAt materials (glassDefaults.withValue film1 (ParameterValue 3.0e-7)) glassFilm200 normal at600

        assertMuellerWithin
            (Propagation.sampleMuellerSpun Propagation.BranchTransmitted (resolvedAsTheTable materials film300) at600 IncidenceAngle.normal Angle.zero)
            at300

        let at200 = matrixOf (stackSampleMuellerAt materials glassDefaults glassFilm200 normal at600)
        let m00At300 = (matrixOf at300).element 0 0
        Assert.True(abs (m00At300 - at200.element 0 0) > 1.0e-6, $"m00 at 300 nm ({m00At300:R}) equals m00 at 200 nm")

    [<Fact>]
    let ``a sample whose material does not resolve answers SampleMaterialErr`` () : unit =
        let plate = BuiltinCatalogue.sample SampleIds.glassPlate1mmId
        let emptyStore = MaterialProxy.createInMemory (fun _ -> [])

        // The plate has no film, so its substrate plate is the first layer the store is asked for.
        match plate.structure.substrate with
        | Some substrate ->
            assertBound
                (Error (SampleMaterialErr (MaterialError.unknownMaterialVersion substrate.materialId)))
                (BoundOpticalElement.ofStackSample emptyStore (ParameterValues.defaultsOf plate.parameterAxes) plate)
        | None -> Assert.Fail "the Built-in 1 mm glass plate carries a substrate plate"
