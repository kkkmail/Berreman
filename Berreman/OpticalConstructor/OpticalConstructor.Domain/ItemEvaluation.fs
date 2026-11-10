namespace OpticalConstructor.Domain

open System
open System.Globalization
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit

/// The item-model evaluator, model × parameters × orientation × λ → Mueller matrix (spec 0050 S8.1; 001 §N).
module ItemEvaluation =

    /// Why an evaluation refused: the values hold no number for a parameter the model publishes, or a number lies
    /// outside its kind's range (spec 0050 S8.1).
    type ItemEvaluationError =
        | MissingParameterValueErr of reason : string
        | ParameterOutOfRangeErr of reason : string

        member this.describe : string =
            match this with
            | MissingParameterValueErr reason -> $"the values hold no number for the parameter: {reason}"
            | ParameterOutOfRangeErr reason -> $"the parameter value is outside its range: {reason}"

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
