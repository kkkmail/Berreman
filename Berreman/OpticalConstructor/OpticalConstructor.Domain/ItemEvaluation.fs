namespace OpticalConstructor.Domain

open System
open System.Globalization
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.ExperimentRun

/// The item-model evaluator, model × parameters × orientation × λ → Mueller matrix (spec 0050 S8.1; 001 §N).
module ItemEvaluation =

    /// Why an evaluation refused: the values hold no number for a parameter the model publishes, a number lies outside
    /// its kind's range, a model that is no light source was asked to emit, or an element was asked for a branch it
    /// does not have (spec 0050 S8.1).
    type ItemEvaluationError =
        | MissingParameterValueErr of reason : string
        | ParameterOutOfRangeErr of reason : string
        | NotALightSourceErr of reason : string
        | UnmodelledBranchErr of reason : string

        member this.describe : string =
            match this with
            | MissingParameterValueErr reason -> $"the values hold no number for the parameter: {reason}"
            | ParameterOutOfRangeErr reason -> $"the parameter value is outside its range: {reason}"
            | NotALightSourceErr reason -> $"the model is not a light source: {reason}"
            | UnmodelledBranchErr reason -> $"the element has no such branch: {reason}"

    /// The numbers an evaluation reads, by the name a model publishes, each in its kind's unit (spec 0050 S8.1; 001 §I).
    type ParameterValues =
        | ParameterValues of Map<ParameterName, ParameterValue>

        /// Each parameter's number from its state: a known value, an unknown's start, a calibrated value (spec 0050 S8.1).
        static member ofParameters (parameters : Parameter list) : ParameterValues =
            parameters
            |> List.map (fun parameter ->
                match parameter.state with
                | KnownParameter value -> parameter.name, value
                | UnknownParameter unknown -> parameter.name, unknown.start
                | CalibratedParameter calibrated -> parameter.name, calibrated.value)
            |> Map.ofList
            |> ParameterValues

        /// Each axis's default (spec 0050 S8.1).
        static member defaultsOf (axes : ItemParameterAxis list) : ParameterValues =
            axes |> List.map (fun axis -> axis.name, axis.defaultValue) |> Map.ofList |> ParameterValues

        /// The values with this one number replaced or added (spec 0050 S8.1).
        member this.withValue (name : ParameterName) (value : ParameterValue) : ParameterValues =
            let (ParameterValues values) = this
            values |> Map.add name value |> ParameterValues

        /// The number held under the name, or `MissingParameterValueErr` naming it (spec 0050 S8.1).
        member this.valueOf (name : ParameterName) : Result<ParameterValue, ItemEvaluationError> =
            let (ParameterValues values) = this
            match values |> Map.tryFind name with
            | Some value -> Ok value
            | None -> Error (MissingParameterValueErr name.value)

        /// The values when every axis holds a finite number in its kind's range (a fraction in [0, 1], a scale or a
        /// thickness at least 0), else the first failure in axis order; values no axis names are ignored (spec 0050 S8.1).
        member this.checkedFor (axes : ItemParameterAxis list) : Result<ParameterValues, ItemEvaluationError> =
            let check (axis : ItemParameterAxis) : Result<ParameterValues, ItemEvaluationError> =
                this.valueOf axis.name
                |> Result.bind (fun value ->
                    let number = value.value
                    let range, withinRange =
                        match axis.kind with
                        | FractionParameter -> "[0, 1]", 0.0 <= number && number <= 1.0
                        | ScaleParameter
                        | ThicknessParameter -> "[0, ∞)", 0.0 <= number
                        | AngleParameter
                        | PhaseParameter
                        | MuellerElementParameter
                        | ReadingParameter -> "(-∞, ∞)", true

                    if Double.IsFinite number && withinRange then Ok this
                    else
                        let rendered = number.ToString("R", CultureInfo.InvariantCulture)
                        Error (ParameterOutOfRangeErr $"{axis.name.value} = {rendered} lies outside {range}"))

            axes |> List.fold (fun checkedSoFar axis -> checkedSoFar |> Result.bind (fun _ -> check axis)) (Ok this)

        /// A compound component's values: each axis read under its `c<position>.` name and held under its own, so the
        /// component evaluates as a standalone form (spec 0050 S8.1).
        member this.forComponent (position : ParameterNameIndex) (axes : ItemParameterAxis list) : Result<ParameterValues, ItemEvaluationError> =
            let hold (held : Result<ParameterValues, ItemEvaluationError>) (axis : ItemParameterAxis) : Result<ParameterValues, ItemEvaluationError> =
                held
                |> Result.bind (fun componentValues ->
                    this.valueOf (axis.name.inComponent position) |> Result.map (fun value -> componentValues.withValue axis.name value))

            axes |> List.fold hold (Ok (ParameterValues Map.empty))

    type ParameterValue with

        /// The number read as radians, an `AngleParameter`'s unit (spec 0050 S8.1).
        member this.angle : Angle =
            let (ParameterValue radians) = this
            Angle radians

        /// The number as a retardance in radians at `w`: as given under `ConstantRetardance`, scaled by λ_ref / λ under
        /// `InverseWaveLengthRetardance λ_ref` (spec 0050 S8.1; step 208).
        member this.retardanceAt (dispersion : RetardanceDispersion) (w : WaveLength) : MuellerReconstruction.Retardance =
            let (ParameterValue radians) = this
            match dispersion with
            | ConstantRetardance -> MuellerReconstruction.Retardance radians
            | InverseWaveLengthRetardance reference -> MuellerReconstruction.Retardance (radians * (reference.value / w.value))

    type SourceEmission with

        /// The light the values describe once they hold every light-source axis in its range: the azimuth and the
        /// ellipticity read as angles, the degree of polarization and the intensity scale as given (spec 0050 S8.1).
        static member ofValues (values : ParameterValues) : Result<SourceEmission, ItemEvaluationError> =
            values.checkedFor SourcePreset.lightSourceAxes
            |> Result.bind (fun held ->
                held.valueOf ParameterName.azimuth
                |> Result.bind (fun azimuth ->
                    held.valueOf ParameterName.ellipticity
                    |> Result.bind (fun ellipticity ->
                        held.valueOf ParameterName.degreeOfPolarization
                        |> Result.bind (fun degreeOfPolarization ->
                            held.valueOf ParameterName.intensityScale
                            |> Result.map (fun intensityScale ->
                                let emission : SourceEmission =
                                    {
                                        azimuth = azimuth.angle
                                        ellipticity = ellipticity.angle
                                        degreeOfPolarization = degreeOfPolarization
                                        intensityScale = intensityScale
                                    }

                                emission)))))

    type ItemModelChoice with

        /// The source arm of the evaluator: a source preset choice emits over the item's values, its λ staying the
        /// preset's; every other choice answers `NotALightSourceErr` naming its model's kind (spec 0050 S8.1).
        member this.emissionWith (values : ParameterValues) : Result<SourceEmission, ItemEvaluationError> =
            let notALightSource (model : ItemModelVersion) : Result<SourceEmission, ItemEvaluationError> =
                let kind =
                    match model.entry.model with
                    | SourcePresetItemModel -> "a light-source model bound to no source preset"
                    | IdealPolarizerItemModel -> "an ideal polarizer"
                    | StackSampleItemModel -> "a stack sample"
                    | OpticalElementModel (RealPolarizerModel _) -> "a real linear polarizer"
                    | OpticalElementModel (RetarderPlateModel _) -> "a retarder plate"
                    | OpticalElementModel (BlackBoxMuellerModel _) -> "a black-box Mueller element"
                    | OpticalElementModel (CompoundModel _) -> "a compound element"
                    | EmptySlotModel -> "an empty slot"
                    | IntensityDetectorModel _ -> "an intensity detector"
                    | MuellerPolarimeterModel _ -> "a Mueller polarimeter"
                    | EllipsometerModel _ -> "an ellipsometer"

                Error (NotALightSourceErr $"'{model.entry.name.value}' ({model.entry.id.value}) is {kind}")

            match this with
            | SourcePresetChoice _ -> SourceEmission.ofValues values
            | PolarizerPresetChoice (model, _)
            | StackSampleChoice (model, _, _)
            | LibraryModelChoice model -> notALightSource model

    /// How an element meets the beam: its rotation about the beam (its R1, a sample's spin), the incidence at its drawn
    /// face, and the branch the light leaves it by (spec 0050 S8.1).
    type ElementOrientation =
        {
            rotation : Angle
            incidence : IncidenceAngle
            branch : Propagation.Branch
        }

        /// An element on the beam that transmits, at this rotation and normal incidence (spec 0050 S8.1).
        static member transmittingAt (rotation : Angle) : ElementOrientation =
            { rotation = rotation; incidence = IncidenceAngle.normal; branch = Propagation.BranchTransmitted }

    /// What an element evaluates from, its parameters bound: a polarizer preset, which publishes no parameter, an
    /// empty slot, the calibration reference, or an optical form with a value for each of its axes (spec 0050 S8.1;
    /// 001 §I).
    type BoundOpticalElement =
        | BoundPolarizerPreset of PolarizerBehavior
        | BoundEmptySlot
        | BoundOpticalForm of form : OpticalForm * values : ParameterValues

        /// The form bound once the values hold each of its axes in range (spec 0050 S8.1).
        static member ofOpticalForm (values : ParameterValues) (form : OpticalForm) : Result<BoundOpticalElement, ItemEvaluationError> =
            values.checkedFor form.parameterAxes |> Result.map (fun held -> BoundOpticalForm (form, held))

        /// The element arm of the evaluator: a preset is its behaviour at the rotation, the matrix the run law takes, an
        /// empty slot the identity, and a form its Stokes–Mueller matrix at the rotation and λ (spec 0050 S8.1; 001 §I,
        /// §N). Each only transmits and ignores the incidence.
        member this.muellerAt (orientation : ElementOrientation) (w : WaveLength) : Result<MuellerMatrix, ItemEvaluationError> =
            let rotation = orientation.rotation

            // Every result in order, or the first error: a black box's elements, a compound's components.
            let sequenced (results : Result<'Item, ItemEvaluationError> list) : Result<'Item list, ItemEvaluationError> =
                results
                |> List.fold (fun held result -> held |> Result.bind (fun read -> result |> Result.map (fun item -> item :: read))) (Ok [])
                |> Result.map List.rev

            match this, orientation.branch with
            | BoundPolarizerPreset behavior, Propagation.BranchTransmitted -> Ok (Propagation.behaviorMueller behavior rotation)
            | BoundEmptySlot, Propagation.BranchTransmitted -> Ok Propagation.identityMueller
            | BoundOpticalForm (RealPolarizerModel leakage, values), Propagation.BranchTransmitted ->
                // The minor transmittance r against a major one of 1, so the defaults (r = 0) are the ideal LP.
                let minorTransmittance : Result<ParameterValue, ItemEvaluationError> =
                    match leakage with
                    | ExtinctionRatioLeakage -> values.valueOf ParameterName.extinctionRatio
                    | DiattenuationLeakage ->
                        values.valueOf ParameterName.diattenuation
                        |> Result.map (fun (ParameterValue d) -> ParameterValue ((1.0 - d) / (1.0 + d)))

                minorTransmittance
                |> Result.bind (fun (ParameterValue r) ->
                    values.valueOf ParameterName.axisOffset
                    |> Result.map (fun axisOffset ->
                        let h = 0.5
                        let root = sqrt r

                        [ [ h * (1.0 + r); h * (1.0 - r); 0.0; 0.0 ]
                          [ h * (1.0 - r); h * (1.0 + r); 0.0; 0.0 ]
                          [ 0.0; 0.0; root; 0.0 ]
                          [ 0.0; 0.0; 0.0; root ] ]
                        |> RealMatrix4x4.create
                        |> MuellerMatrix
                        |> Propagation.rotateMueller (rotation + axisOffset.angle)))
            | BoundOpticalForm (RetarderPlateModel dispersion, values), Propagation.BranchTransmitted ->
                values.valueOf ParameterName.retardance
                |> Result.bind (fun retardance ->
                    values.valueOf ParameterName.fastAxisOffset
                    |> Result.map (fun fastAxisOffset ->
                        MuellerReconstruction.retarderMueller (rotation + fastAxisOffset.angle) (retardance.retardanceAt dispersion w)))
            | BoundOpticalForm (BlackBoxMuellerModel normalisation, values), Propagation.BranchTransmitted ->
                // Under `FifteenElementsAndScale`, m00 is 1 and the scale multiplies the whole matrix.
                let scale : Result<ParameterValue, ItemEvaluationError> =
                    match normalisation with
                    | SixteenElementMueller -> Ok (ParameterValue 1.0)
                    | FifteenElementsAndScale -> values.valueOf ParameterName.blackBoxScale
                let elementOf (row : ParameterNameIndex) (column : ParameterNameIndex) : Result<ParameterValue, ItemEvaluationError> =
                    match normalisation, row, column with
                    | FifteenElementsAndScale, ParameterNameIndex 0, ParameterNameIndex 0 -> Ok (ParameterValue 1.0)
                    | FifteenElementsAndScale, _, _
                    | SixteenElementMueller, _, _ -> values.valueOf (ParameterName.muellerElement row column)
                let indices : ParameterNameIndex list = [ 0 .. 3 ] |> List.map ParameterNameIndex
                let elements : Result<ParameterValue list, ItemEvaluationError> =
                    indices |> List.collect (fun row -> indices |> List.map (elementOf row)) |> sequenced

                scale
                |> Result.bind (fun (ParameterValue s) ->
                    elements
                    |> Result.map (fun held ->
                        held
                        |> List.map (fun (ParameterValue m) -> s * m)
                        |> List.chunkBySize 4
                        |> RealMatrix4x4.create
                        |> MuellerMatrix
                        |> Propagation.rotateMueller rotation))
            | BoundOpticalForm (CompoundModel components, values), Propagation.BranchTransmitted ->
                // Each component at rotation 0 and its own offset, in light order; a nested compound recurses here.
                let atReference = ElementOrientation.transmittingAt Angle.zero

                components
                |> List.mapi (fun k part ->
                    let position = ParameterNameIndex (k + 1)

                    values.forComponent position part.form.parameterAxes
                    |> Result.bind (fun componentValues ->
                        let bound : BoundOpticalElement = BoundOpticalForm (part.form, componentValues)
                        bound.muellerAt atReference w)
                    |> Result.map (fun matrix ->
                        let evaluated : MuellerComponent = { matrix = matrix; offset = part.offset }
                        evaluated))
                |> sequenced
                |> Result.map (fun parts -> Propagation.rotateMueller rotation (Propagation.compoundMueller parts))
            | BoundPolarizerPreset _, Propagation.BranchReflected ->
                Error (UnmodelledBranchErr "the reflected branch of a polarizer preset, which only transmits")
            | BoundEmptySlot, Propagation.BranchReflected ->
                Error (UnmodelledBranchErr "the reflected branch of an empty slot, which only transmits")
            | BoundOpticalForm _, Propagation.BranchReflected ->
                Error (UnmodelledBranchErr "the reflected branch of an optical form, which only transmits")
