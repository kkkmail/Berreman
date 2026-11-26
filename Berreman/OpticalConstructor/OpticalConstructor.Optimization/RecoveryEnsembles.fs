namespace OpticalConstructor.Optimization

open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization.MuellerFitting
open OpticalConstructor.Optimization.BenchNoise

/// What an ensemble of noisy experiments says about each recovered quantity: its mean, bias, scatter and worst case.
/// Moved from `BerremanTests/InverseFitHarness.fs` by spec 0050 S10.1 (specs/0044/.manual/011-noisy-measurement-tests.md).
module RecoveryEnsembles =

    /// A scalar an ensemble reports on: a fitted parameter, or a quantity derived from several. A difference of two
    /// strongly correlated parameters (a birefringence) is often determined far better than either, so report it too.
    [<ReferenceEquality>]
    type RecoveredQuantity<'Parameters> =
        {
            name : ParameterName
            valueIn : 'Parameters -> QuantityValue
        }

        /// The quantity that IS a fitted parameter.
        static member ofAxis (axis : ParameterAxis<'Parameters>) : RecoveredQuantity<'Parameters> =
            {
                name = axis.name
                valueIn = fun p -> QuantityValue (axis.read p)
            }

        /// A quantity computed from the fitted parameters, a birefringence say.
        static member derived (name : ParameterName) (valueIn : 'Parameters -> QuantityValue) : RecoveredQuantity<'Parameters> =
            {
                name = name
                valueIn = valueIn
            }

    /// What an ensemble of noisy experiments says about ONE recovered quantity: the error bar one experiment cannot give.
    [<ReferenceEquality>]
    type RecoveryStatistics<'Parameters> =
        {
            quantity : RecoveredQuantity<'Parameters>
            /// The ensemble mean, in the quantity's own units.
            mean : QuantityValue
            /// `|mean − truth| / |truth|`: where a systematic displacement would survive the average.
            bias : RelativeError
            /// The sample standard deviation over `|truth|`: the random error.
            scatter : RelativeError
            /// The same standard deviation in the quantity's own units. Compare an index with a difference of indices
            /// only this way: dividing each by its own value hides the difference's advantage.
            absoluteScatter : QuantityValue
            /// The worst single experiment's relative error.
            worst : RelativeError
        }

        /// One line of the ensemble table.
        member this.describe : string =
            $"{this.quantity.name.value}: mean {this.mean.value}, bias {this.bias.value}, scatter {this.scatter.value} "
            + $"(absolute {this.absoluteScatter.value}), worst {this.worst.value}"

    /// One noisy experiment: the seed that generated it and the fit it led to. The fit keeps its scaling and residual,
    /// so an ensemble can ask it for its own covariance without a second fit.
    [<ReferenceEquality>]
    type NoisyExperiment<'Parameters> =
        {
            seed : NoiseSeed
            fit : MuellerFitData<'Parameters>
        }

    /// How many experiments an ensemble holds.
    type EnsembleSize =
        | EnsembleSize of int

        member this.value : int = let (EnsembleSize n) = this in n

    /// One statistics row for a quantity over an ensemble's recovered parameter sets. The scatter is the
    /// Bessel-corrected (`n − 1`) sample standard deviation: an ensemble is a sample of possible experiments.
    let statisticsFor
        (truth : 'Parameters)
        (recovered : 'Parameters list)
        (quantity : RecoveredQuantity<'Parameters>)
        : RecoveryStatistics<'Parameters> =
        let trueValue = quantity.valueIn truth
        let values = recovered |> List.map quantity.valueIn
        let count = List.length values
        let mean = values |> List.map (fun v -> v.value) |> List.average |> QuantityValue
        let variance = (values |> List.sumBy (fun v -> (v.value - mean.value) ** 2.0)) / float (count - 1)
        let absoluteScatter = sqrt variance |> QuantityValue
        {
            quantity = quantity
            mean = mean
            bias = RelativeError.between mean trueValue
            scatter = absoluteScatter.value / abs trueValue.value |> RelativeErr
            absoluteScatter = absoluteScatter
            worst = values |> List.map (fun v -> RelativeError.between v trueValue) |> List.max
        }

    /// The standard error of an ensemble's mean, `scatter / sqrt n`: the yardstick a bias is measured against, since a
    /// smaller departure is random error not yet averaged away.
    let standardErrorOfMean (size : EnsembleSize) (row : RecoveryStatistics<'Parameters>) : RelativeError =
        row.scatter.value / sqrt (float size.value) |> RelativeErr

    /// A quantity's row in an ensemble table, by name, or `UnknownQuantityErr`. `RecoveredQuantity` holds a function
    /// and has no structural equality, so its name is its identity.
    let rowFor
        (table : RecoveryStatistics<'Parameters> list)
        (name : ParameterName)
        : Result<RecoveryStatistics<'Parameters>, MuellerFitError> =
        match table |> List.tryFind (fun row -> row.quantity.name = name) with
        | Some row -> Ok row
        | None -> Error (UnknownQuantityErr name)
