namespace OpticalConstructor.Optimization

open System
open System.Threading
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.StudyForward
open OpticalConstructor.Domain.FitRuns
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.MuellerFitting

/// The least-squares solves a fit's results are read from, and their profiles (spec 0050 S11.4).
module FittedProblems =

    type IterationWatch with

        /// This watch counting on from `earlier`: each count it is told reaches the observer as `earlier` plus that count,
        /// over the same cancellation; an unwatched solve stays unwatched (spec 0050 S11.3, S11.4).
        member this.after (earlier : IterationCount) : IterationWatch =
            match this with
            | UnwatchedIterations -> UnwatchedIterations
            | WatchedIterations observer ->
                WatchedIterations
                    {
                        reportIteration = fun own -> observer.reportIteration (IterationCount (earlier.value + own.value))
                        cancellation = observer.cancellation
                    }

        /// The token a solve under this watch reads to stop, none when unwatched (spec 0050 S11.3, S11.4).
        member internal this.cancellation : CancellationToken =
            match this with
            | WatchedIterations observer -> observer.cancellation
            | UnwatchedIterations -> CancellationToken.None

        /// This watch, also handing `tell` each count it is told: the count is kept where the solver tells it, so the
        /// next solve can count on from it (spec 0050 S11.3, S11.4).
        member internal this.tellingAlso (tell : IterationCount -> unit) : IterationWatch =
            match this with
            | UnwatchedIterations -> UnwatchedIterations
            | WatchedIterations observer ->
                WatchedIterations
                    {
                        reportIteration =
                            fun count ->
                                tell count
                                observer.reportIteration count
                        cancellation = observer.cancellation
                    }

    /// The position of one coordinate in a solve (spec 0050 S11.4).
    type FitCoordinateIndex =
        | FitCoordinateIndex of int

        member this.value : int = let (FitCoordinateIndex index) = this in index

    /// Why a coordinate's profile stopped (spec 0050 S11.4).
    type FitProfileError =
        /// The fit's watch asked the profile's re-fits to stop (spec 0050 S11.4).
        | ProfileCancelledErr of reason : string

        member this.describe : string =
            match this with
            | ProfileCancelledErr reason -> $"the profile re-fits were cancelled: {reason}"

    /// One coordinate's profile and the iterations its re-fits took, as they told the watch (spec 0050 S11.4).
    type ProfileRun =
        {
            points : ProfilePoint list
            iterations : IterationCount
        }

    /// One least-squares solve: `search.start` the solution it reached within its bounds, and coordinate k the physical
    /// value centreₖ + unitₖ · sₖ (spec 0050 S11.4).
    [<ReferenceEquality>]
    type FittedProblem =
        {
            subject : FitProblemSubject
            coordinates : FitCoordinate list
            residual : ScaledVector -> ResidualVector
            search : ScaledSearch
            centre : StudyParameterVector
            unit : StudyParameterVector
        }

        /// The residual about a frame offset by a hundred, y ↦ residual (start + (y − 100)), and the vector of hundreds
        /// that stands for the solution in it (spec 0050 S11.4).
        member private this.offsetFrame : Residual * DesignVector =
            let offset = 100.0
            let start = this.search.start.value

            let shifted (DesignVector y) : ResidualVector =
                Array.map2 (fun (solved : float) (moved : float) -> solved + (moved - offset)) start y |> ScaledVector |> this.residual

            shifted, Array.create start.Length offset |> DesignVector

        /// FitQuality's Jacobian in the offset frame: its step √ε · max(|x|, 1e-12) is then about 1.5e-6, the order of
        /// ALGLIB's own 1e-6, and a coordinate at zero is still perturbed (spec 0050 S11.4).
        member this.jacobian : ResidualJacobian =
            let shifted, solution = this.offsetFrame
            FitQuality.residualJacobian shifted solution

        /// FitQuality's report in the offset frame with the solve's own final residuals, read for its standard errors
        /// only: its parameters are the frame's (spec 0050 S11.4).
        member this.report : FitQuality.FitReport =
            let shifted, solution = this.offsetFrame
            FitQuality.reportFrom shifted solution (this.residual this.search.start)

        /// The solve's identifiability: its condition and correlation only where the Jacobian's rank is the coordinate
        /// count and the condition finite (spec 0050 S11.4).
        member this.quality : FitProblemQuality =
            let jacobian = this.jacobian
            let rank = jacobian.rank
            let condition = jacobian.conditionNumber
            let atSolution = this.residual this.search.start
            let chiSquared = FitChiSquared atSolution.sumOfSquares
            let rows = atSolution.value.Length
            let determined = rank.value = List.length this.coordinates && Double.IsFinite condition.value

            {
                subject = this.subject
                coordinates = this.coordinates
                rank = rank
                conditionOpt = if determined then Some condition else None
                columnNorms = JacobianColumnNorms jacobian.columnNorms
                correlationOpt = if determined then Some (CorrelationMatrix jacobian.correlation) else None
                chiSquared = chiSquared
                rmse = if rows = 0 then ResidualRmse 0.0 else ResidualRmse (sqrt (chiSquared.value / float rows))
            }

        /// Each coordinate's σ in physical units, unitₖ times the report's standard error k, where the rank is the
        /// coordinate count and the product finite (spec 0050 S11.4).
        member private this.sigmas : ParameterSigma option list =
            let count = List.length this.coordinates

            if this.jacobian.rank.value <> count then
                List.replicate count None
            else
                let standardErrors = this.report.standardErrors.value

                List.init count (fun k ->
                    let sigma = ParameterSigma (this.unit.value.[k] * standardErrors.[k])
                    if Double.IsFinite sigma.value then Some sigma else None)

        /// Coordinate k's σ in physical units, `None` where the solve leaves it undetermined (spec 0050 S11.4).
        member this.sigmaAt (coordinate : FitCoordinateIndex) : ParameterSigma option =
            this.sigmas |> List.tryItem coordinate.value |> Option.flatten

        /// Each entry of each `EntryCoordinate`, in order, with that coordinate's σ (spec 0050 S11.4).
        member this.entrySigmas : FitEntrySigma list =
            List.zip this.coordinates this.sigmas
            |> List.collect (fun (coordinate, sigmaOpt) ->
                match coordinate with
                | EntryCoordinate entries -> entries |> List.map (fun entry -> ({ entry = entry; sigmaOpt = sigmaOpt } : FitEntrySigma))
                | ElementCoordinate _
                | FringeVisibilityCoordinate -> [])

        /// The coordinate's χ² profile: x̂ + j · σ for j = −3 … 3 within its bounds, the middle the solve's own χ², or seven
        /// values across finite bounds without σ; each other point re-fits the rest held there, under `watch.after` of
        /// `earlier` and the re-fits before it (spec 0050 S11.4).
        member this.profileAt
            (coordinate : FitCoordinateIndex)
            (earlier : IterationCount)
            (watch : IterationWatch)
            : Result<ProfileRun, FitProfileError> =
            let k = coordinate.value
            let start = this.search.start
            let dimension = start.value.Length
            let solvedChiSquared = FitChiSquared (this.residual start).sumOfSquares

            // Each point's physical value and the scaled value it is held at, `None` for the solution itself.
            let planned : (ParameterValue * float option) list =
                if k < 0 || k >= dimension then
                    []
                else
                    let lower = this.search.lowerBounds.value.[k]
                    let upper = this.search.upperBounds.value.[k]
                    let centre = this.centre.value.[k]
                    let unit = this.unit.value.[k]

                    match this.sigmaAt coordinate with
                    | Some sigma ->
                        let fitted = centre + unit * start.value.[k]

                        [
                            for j in -3 .. 3 do
                                let value = fitted + float j * sigma.value
                                let scaled = (value - centre) / unit

                                if j = 0 then yield ParameterValue value, None
                                elif lower <= scaled && scaled <= upper then yield ParameterValue value, Some scaled
                        ]
                    | None when Double.IsFinite lower && Double.IsFinite upper ->
                        [
                            for i in 0 .. 6 do
                                let scaled = lower + float i / 6.0 * (upper - lower)
                                yield ParameterValue (centre + unit * scaled), Some scaled
                        ]
                    | None -> []

            let cancellation = watch.cancellation
            let told = ref earlier
            let telling = watch.tellingAlso (fun count -> told.Value <- count)

            let without (vector : ScaledVector) : ScaledVector = Array.removeAt k vector.value |> ScaledVector

            let refitAt (held : float) : ProfileOutcome =
                let heldIn (free : ScaledVector) : ScaledVector = Array.insertAt k held free.value |> ScaledVector

                if dimension = 1 then
                    FitChiSquared (this.residual (heldIn (ScaledVector [||]))).sumOfSquares |> ProfileConverged
                else
                    let others : ScaledSearch =
                        {
                            start = without start
                            lowerBounds = without this.search.lowerBounds
                            upperBounds = without this.search.upperBounds
                        }

                    match others.solveWith (telling.after told.Value) (heldIn >> this.residual) with
                    | Ok solution -> FitChiSquared solution.finalResiduals.sumOfSquares |> ProfileConverged
                    | Error error -> ProfileFailed error.failureText

            let total = planned.Length

            let rec pointsFrom
                (remaining : (ParameterValue * float option) list)
                (finished : ProfilePoint list)
                : Result<ProfilePoint list, FitProfileError> =
                match remaining with
                | [] -> Ok (List.rev finished)
                | (value, None) :: rest -> pointsFrom rest (({ value = value; outcome = ProfileConverged solvedChiSquared } : ProfilePoint) :: finished)
                | (value, Some held) :: rest ->
                    if cancellation.IsCancellationRequested then
                        Error (ProfileCancelledErr $"coordinate {k}: {finished.Length} of {total} point(s) done")
                    else
                        let outcome = refitAt held

                        if cancellation.IsCancellationRequested then
                            Error (ProfileCancelledErr $"coordinate {k}: {finished.Length} of {total} point(s) done, the last re-fit's result dropped")
                        else
                            pointsFrom rest (({ value = value; outcome = outcome } : ProfilePoint) :: finished)

            pointsFrom planned []
            |> Result.map (fun points -> ({ points = points; iterations = IterationCount (told.Value.value - earlier.value) } : ProfileRun))
