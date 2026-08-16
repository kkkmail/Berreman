namespace BerremanTests

open Berreman.Geometry
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization
open Xunit
open BerremanTests.MatrixComparison

/// A dimensionless error against a KNOWN true value: `|actual − expected| / |expected|`. Elevated
/// because every acceptance band in the inverse-problem suites is quoted in these units, and an absolute
/// difference read as a relative one — or the reverse — would be a silently passing test rather than a
/// compile error.
type RelativeError =
    | RelativeError of double

    /// The error as a fraction (the arithmetic seam).
    member this.value = let (RelativeError e) = this in e

    /// The error of a recovered value against the truth it is supposed to reproduce.
    static member between (actual : double) (expected : double) : RelativeError =
        abs (actual - expected) / abs expected |> RelativeError

/// The box the optimizer may search, as a half-width in SCALED units — so one unit is whatever the
/// `ParameterScaling.scale` says it is (1e-3 in refractive index and 1e-5 in gyration, throughout these
/// suites).
///
/// It is a parameter of the fit rather than a constant because start guesses sit at very different
/// distances from the truth. A box that comfortably brackets the truth from a 0.2 % perturbed start does
/// NOT bracket it from a blind `n = 1.5` start, and a box that silently excludes the answer produces a
/// converged-looking fit pinned against a bound.
type SearchBox =
    | SearchBox of double

    /// The half-width in scaled units (the arithmetic seam).
    member this.halfWidth = let (SearchBox h) = this in h

/// Everything a fit produced, and everything a diagnostic built on it needs afterwards: the recovered
/// constants, the solver's own account of the run, the scaling the vector lives in, and the residual
/// closure a Jacobian is taken of.
[<ReferenceEquality>]
type InverseFit<'Parameters> =
    {
        recovered : 'Parameters
        solution : NonlinearSolution
        scaling : ParameterScaling<'Parameters>
        residual : float[] -> float[]
    }

    /// Σ residualᵢ² at the solution.
    member this.chiSquared : double = this.solution.finalResiduals |> Array.sumBy (fun r -> r * r)

    /// The root-mean-square residual entry at the solution — the noise level, when there is noise.
    member this.rmsResidual : double =
        sqrt (this.chiSquared / float (max 1 this.solution.finalResiduals.Length))

// =====================================================================================================
// The MEASUREMENT-ERROR model (manual task 010, generalized for task 014).
//
// Everything above this line lets a suite feed its fit the data the forward model produced exactly, at
// angles the sample was exactly at. That measures the INVERSE MACHINERY. What follows measures the
// EXPERIMENT: what a real, decent-but-ordinary optical bench recovers, and — the point of the exercise —
// with what error bar.
//
// TWO error sources are modelled, because they are the two an angle-resolved Mueller measurement
// actually has, and they enter the problem at completely different places:
//
//   1. ROTATION-ANGLE ERROR (an error in the INDEPENDENT variable). The log says the sample sat at
//      70.0 deg incidence and 45.0 deg azimuth; it really sat at 70.0 +- e and 45.0 +- e. The forward
//      model is therefore evaluated at the TRUE (perturbed) angles, while the observation is filed
//      against the NOMINAL ones — because the nominal ones are all the experimenter has. Within one
//      experiment this does NOT average away: every configuration's angle error is fixed for the whole
//      of it, so the fit minimizes a model that is wrong in a fixed way and its answer is displaced
//      systematically. Whether those displacements also average ACROSS experiments is a question an
//      ensemble answers rather than assumes, which is why the statistics below report a bias as well as
//      a scatter.
//
//   2. DETECTOR ERROR (an error in the DEPENDENT variable). The receiver reads each element of the
//      normalized Mueller matrix a little high or a little low. This is ordinary measurement noise and
//      it does average away, in the usual 1/sqrt(N) manner, across the residual entries.
//
// WHAT IS NOT MODELLED, and deliberately: the plate thickness and the wavelength are taken as known
// exactly (spec R2 fixes thickness as a known per-configuration quantity, and a stabilized HeNe line is
// known far better than anything else here), and the ABSOLUTE throughput is irrelevant by construction
// because every comparison in `MuellerInverse` is between NORMALIZED matrices — source brightness,
// detector gain and exposure drop out exactly. Modelling a gain error would therefore be modelling
// nothing.
//
// THE MAGNITUDES are for a decent but ordinary optical lab, NOT for a metrology-grade instrument, and
// they were taken from what the equipment actually delivers:
//
//   * ROTATION, 0.2 deg max error. A plain manual rotation mount carries a scale graduated in 2 deg
//     steps; a high-precision manual mount reaches 5 arcmin (0.083 deg) on its vernier; a motorized
//     mount such as a DC-servo rotation stage quotes ~0.1 % on-axis accuracy but +-0.3 deg of backlash,
//     and published polarimeter builds report stage repeatability of 0.1-0.2 deg, with wave-plate mount
//     friction able to spoil a setting "by a degree or more". Research ellipsometers do reach
//     0.001-0.016 deg — which is exactly the high-precision regime this exercise is deliberately NOT
//     modelling. 0.2 deg sits where a careful worker with ordinary mounts actually lands.
//
//   * DETECTOR, 0.005 of full scale on each normalized element. Mueller-polarimetry error analyses put a
//     single-shot CCD pixel at ~0.01 of maximum intensity and an AVERAGED Mueller matrix at "the third
//     decimal place" for the mean and "the second decimal" for the per-pixel spread; silicon photodiode
//     power meters are +-3-5 % on ABSOLUTE calibration (irrelevant here, see above) but ~1 % on
//     linearity and far better on repeatability. 0.005 is between the averaged research figure of ~1e-3
//     and the single-shot figure of ~1e-2.
//
// Sources for both figures are recorded in the manual task folder alongside this work.
// =====================================================================================================

/// The seed that fixes ONE noisy realization of a whole experiment. Elevated so that a seed can never be
/// passed where an iteration count, a configuration count or a parameter index is expected.
type NoiseSeed =
    | NoiseSeed of int

    /// The seed value (the arithmetic seam, reached only where the generator is constructed).
    member this.value = let (NoiseSeed s) = this in s

/// The half-width of the uniform distribution a recorded ROTATION ANGLE's error is drawn from: the
/// experiment logs "20 degrees", and the sample really sat somewhere in 20 ± this.
type AngleUncertainty =
    | AngleUncertainty of Angle

    /// The uncertainty as the engine `Angle`.
    member this.angle = let (AngleUncertainty a) = this in a

    /// The uncertainty in radians (the arithmetic seam).
    member this.value = let (AngleUncertainty a) = this in a.value

    /// The uncertainty in degrees, which is how rotation-stage data sheets quote it.
    member this.degrees = let (AngleUncertainty a) = this in a.degrees

    /// Build an `AngleUncertainty` from a value given in degrees.
    static member degree (d : double) : AngleUncertainty = Angle.degree d |> AngleUncertainty

/// The half-width of the uniform distribution the DETECTOR's error on one NORMALIZED Mueller element is
/// drawn from. Normalized elements are bounded by 1, so this is directly a fraction of full scale: 0.005
/// means "the receiver reads each element to within half a percent of full scale".
type ElementUncertainty =
    | ElementUncertainty of double

    /// The uncertainty in units of the normalized element (the arithmetic seam).
    member this.value = let (ElementUncertainty u) = this in u

/// One measurement-error budget — everything about the apparatus that makes the recorded data differ
/// from what the material would produce in a perfect experiment.
///
/// Both fields are MAX errors, not standard deviations: each error is drawn from a uniform distribution
/// on `[−u, +u]`, so the corresponding standard deviation is `u/√3`. Uniform rather than Gaussian is the
/// honest choice for these two sources — a rotation stage's setting error is bounded by its mechanics
/// and a detector's quantization/linearity error is bounded by its specification, neither has a tail.
type NoiseParam =
    {
        /// Applied INDEPENDENTLY to the incidence angle and to the sample azimuth of every
        /// configuration: both are read off rotation mounts of the same grade.
        rotation : AngleUncertainty
        /// Applied independently to each of the 15 normalized Mueller elements other than `m₀₀`.
        element : ElementUncertainty
    }

    /// The measurement-error budget of the decent-but-ordinary bench described above. It is a property
    /// of the APPARATUS and not of the sample, so both inverse-problem suites measure their crystals on
    /// the same bench — which is what makes their ensembles directly comparable.
    static member decentLab : NoiseParam =
        {
            rotation = AngleUncertainty.degree 0.2
            element = ElementUncertainty 0.005
        }

    /// The budget in which nothing is uncertain — the control that turns every noisy path back into the
    /// noiseless one, so the noise machinery can be proved to be a pure overlay.
    static member noiseless : NoiseParam =
        {
            rotation = AngleUncertainty.degree 0.0
            element = ElementUncertainty 0.0
        }

/// Ambient randomness, injected as a provider record rather than reached for directly — the repository's
/// standing rule for ambient effects, and what makes a noisy experiment a PURE FUNCTION of its seed.
///
/// Every draw is uniform on `[−1, +1]`, so a caller multiplies by the max error of whatever it is
/// perturbing and never has to know what distribution it got.
[<ReferenceEquality>]
type UniformDeviateProvider =
    {
        nextDeviate : unit -> double
    }

    /// The real, seeded backend. `System.Random(seed)` is the same generator `Synthesis.fs` uses for its
    /// reproducible annealing / GA runs; its SEEDED constructor is documented as keeping the legacy
    /// algorithm precisely so that a seeded stream stays reproducible across runtime versions.
    static member fromSeed (seed : NoiseSeed) : UniformDeviateProvider =
        let rng = System.Random(seed.value)
        { nextDeviate = fun () -> 2.0 * rng.NextDouble() - 1.0 }

/// A scalar an experiment recovers: any one of the fitted unknowns, or any DERIVED quantity built from
/// them.
///
/// Derived quantities are not decoration. The most important number a biaxial or uniaxial experiment
/// produces is often a DIFFERENCE of two fitted parameters — linear retardance depends on `n_e − n_o`,
/// not on either index alone — and because that difference is one to two orders of magnitude smaller
/// than the indices themselves, the two are strongly correlated and the difference is determined far
/// better than either. An ensemble that reported only the fitted parameters would understate the
/// experiment by exactly that factor.
[<ReferenceEquality>]
type RecoveredQuantity<'Parameters> =
    {
        name : ParameterName
        valueIn : 'Parameters -> double
    }

    /// The quantity that IS a fitted parameter.
    static member ofAxis (axis : ParameterAxis<'Parameters>) : RecoveredQuantity<'Parameters> =
        {
            name = axis.name
            valueIn = axis.read
        }

    /// A quantity computed FROM the fitted parameters — a birefringence, say.
    static member derived (name : string) (valueIn : 'Parameters -> double) : RecoveredQuantity<'Parameters> =
        {
            name = ParameterName name
            valueIn = valueIn
        }

/// What an ensemble of noisy experiments says about ONE recovered quantity — the answer to "and what is
/// the error bar?", which a single noisy experiment cannot give.
[<ReferenceEquality>]
type RecoveryStatistics<'Parameters> =
    {
        /// Which quantity this row is about.
        quantity : RecoveredQuantity<'Parameters>
        /// The mean recovered value over the ensemble, in that quantity's own physical units (a
        /// refractive index for some of them and a gyration component for others, which is why it cannot
        /// be an elevated type here). Carried for the report, not asserted on directly.
        mean : double
        /// `|mean − truth| / |truth|` — what is left of the error once the ensemble has averaged, and
        /// therefore the place a SYSTEMATIC displacement would show up. It is measured rather than
        /// assumed to be zero: a rotation-angle error is genuinely systematic within one experiment,
        /// because the fit assumes angles the sample was never at, and whether it also survives the
        /// average is exactly the question.
        bias : RelativeError
        /// The sample standard deviation of the recovered values over `|truth|` — the RANDOM error, and
        /// the number an experimenter would quote as the "±" after a repeat-measurement study.
        scatter : RelativeError
        /// The same standard deviation in the quantity's OWN units. Carried alongside the relative one
        /// because a pair of indices and their DIFFERENCE cannot be compared relatively: the difference
        /// is one to two orders of magnitude smaller than either index, so dividing each by its own value
        /// flatters the indices by exactly that factor and hides the very effect the comparison exists to
        /// show.
        absoluteScatter : double
        /// The worst single experiment's relative error over the ensemble.
        worst : RelativeError
    }

    /// One line of the ensemble table.
    member this.describe : string =
        $"{this.quantity.name.value}: mean {this.mean}, bias {this.bias.value}, scatter {this.scatter.value} "
        + $"(absolute {this.absoluteScatter}), worst {this.worst.value}"

/// Everything ONE noisy experiment produced: which seed generated it, and the fit it led to. The fit
/// carries the scaling and the residual closure alongside the recovered constants, so an ensemble can
/// ask a single experiment what uncertainty IT thinks it has without paying for a second fit.
[<ReferenceEquality>]
type NoisyExperiment<'Parameters> =
    {
        seed : NoiseSeed
        fit : InverseFit<'Parameters>
    }

/// Spec 0044 — the machinery every inverse-problem suite in this assembly shares.
///
/// `MuellerInverseTests` (uniaxial quartz, four unknowns) and `BiaxialInverseTests` (triclinic, nine
/// unknowns) differ in their MATERIAL and in their MEASUREMENT SET, and in nothing else: the same
/// forward evaluation, the same residual, the same Levenberg–Marquardt call and the same acceptance
/// arithmetic drive both. That is only true because `MuellerInverse` is generic in `'Parameters`, and
/// this module is where that genericity is actually cashed in — it is what stops a second material from
/// being a copy of the first with the field names changed.
module InverseFitHarness =

    /// Build one measurement configuration from plain degrees. Reaching for degrees here rather than
    /// carrying `Angle` values through the test bodies keeps a configuration table readable as a table.
    let configurationOf
        (cut : SampleCut)
        (thickness : Thickness)
        (incidenceDeg : double)
        (azimuthDeg : double)
        (observable : Observable)
        (waveLength : WaveLength)
        : MeasurementConfiguration =
        {
            cut = cut
            thickness = thickness
            incidenceAngle = Angle.degree incidenceDeg |> IncidenceAngle.create
            azimuth = SampleAzimuth.degree azimuthDeg
            observable = observable
            waveLength = waveLength
        }

    /// Generate the synthetic "measured" data for a configuration set from known truth, failing the test
    /// on any forward-model error — a forward failure on the GROUND TRUTH is a broken test, not a datum.
    let observeWith
        (forward : ForwardModelProxy<'Parameters>)
        (truth : 'Parameters)
        (configurations : MeasurementConfiguration list)
        : MuellerObservation list =
        configurations
        |> List.map (fun c ->
            match forward.muellerOf c truth with
            | Ok m -> { configuration = c; measured = m }
            | Error e -> failwith $"the forward model failed to generate ground-truth data: %A{e}")

    /// The residual closure the optimizer drives: scaled vector -> normalized Mueller element
    /// differences.
    ///
    /// It must be TOTAL and of fixed length, because an optimizer explores freely and will hand it
    /// parameter values the forward model cannot solve. A forward failure therefore returns a large
    /// finite penalty of the correct length rather than throwing or returning a short vector: the first
    /// would abort the fit, and the second would silently change the problem being minimized.
    let residualFor
        (forward : ForwardModelProxy<'Parameters>)
        (observations : MuellerObservation list)
        (scaling : ParameterScaling<'Parameters>)
        : float[] -> float[] =
        let width = 15 * List.length observations
        fun (v : float[]) ->
            match forwardModels forward (ofScaled scaling v) observations with
            | Error _ -> Array.create width 1.0e3
            | Ok models ->
                match residualVector observations models with
                | Ok r -> r
                | Error _ -> Array.create width 1.0e3

    /// The dimensionless fit space centred on a START GUESS, so the initial scaled vector is exactly
    /// zero and `scale` sets what one unit of each coordinate means physically.
    ///
    /// The scale choice is the whole reason `ParameterScaling` exists. ALGLIB's Levenberg–Marquardt
    /// differentiates with a FIXED ABSOLUTE step of 1e-6, so a unit of 1e-3 in refractive index makes
    /// that step a 1e-9 perturbation of n, and a unit of 1e-5 in gyration makes it a 1e-11 perturbation
    /// of g. Both are small enough to be genuinely linear and large enough to move the Mueller elements
    /// far above the solver's own numerical noise. Handing the optimizer raw physical parameters instead
    /// would make the same 1e-6 step a 1 % perturbation of g and a 6e-7 relative perturbation of n —
    /// four orders of magnitude apart, and the former far outside the linear regime a Jacobian assumes.
    let scalingAround
        (axes : ParameterAxis<'Parameters> list)
        (scale : 'Parameters)
        (centre : 'Parameters)
        : ParameterScaling<'Parameters> =
        {
            axes = axes
            centre = centre
            scale = scale
        }

    /// THE CORE RUNNER. Recover the material parameters from a GIVEN set of observations, over a given
    /// scaling and a given box. Every fit in every inverse-problem suite goes through this one function,
    /// so two suites differ in their DATA and their MATERIAL and in nothing else.
    ///
    /// It deliberately takes observations rather than configurations: that is the seam noise enters
    /// through. A noisy experiment is the same fit run against data generated at slightly wrong angles
    /// and read back by a slightly wrong detector, and nothing about the fit itself changes.
    let fitObservations
        (forward : ForwardModelProxy<'Parameters>)
        (box : SearchBox)
        (observations : MuellerObservation list)
        (scaling : ParameterScaling<'Parameters>)
        : InverseFit<'Parameters> =
        let residual = residualFor forward observations scaling
        let solver = MuellerInverseSolver.createAlglibLevenbergMarquardt ()
        let request =
            {
                residual = residual
                initial = Array.zeroCreate scaling.dimension
                lowerBounds = Array.create scaling.dimension -box.halfWidth
                upperBounds = Array.create scaling.dimension box.halfWidth
                maxIterations = 400
                epsX = 1.0e-12
            }
        match solver.solveNonlinearLeastSquares request with
        | Ok solution ->
            {
                recovered = ofScaled scaling solution.solution
                solution = solution
                scaling = scaling
                residual = residual
            }
        | Error e -> failwith $"the inverse fit failed: %A{e}"

    /// The relative error of every fitted parameter against the truth, paired with its name.
    let recoveryErrors
        (scaling : ParameterScaling<'Parameters>)
        (truth : 'Parameters)
        (recovered : 'Parameters)
        : (ParameterName * RelativeError) list =
        scaling.axes |> List.map (fun a -> a.name, RelativeError.between (a.read recovered) (a.read truth))

    /// One line per parameter: name and relative error, joined for an assertion message.
    let describeErrors (errors : (ParameterName * RelativeError) list) : string =
        System.String.Join("; ", [ for (n, e) in errors -> $"{n.value} rel err {e.value}" ])

    /// One line per parameter: name and Jacobian column norm, joined for an assertion message.
    let describeColumnNorms (axes : ParameterAxis<'Parameters> list) (norms : float[]) : string =
        System.String.Join("; ", [ for (i, a) in List.indexed axes -> $"{a.name.value} |J| = {norms.[i]}" ])

    /// The same element-by-element Mueller compare loop `MuellerMatrixTests` and
    /// `MuellerReconstructionTests` use, over the shared `allowedDiff` tolerance — no hand-rolled
    /// epsilon.
    let assertMuellerEqual (expected : MuellerMatrix) (actual : MuellerMatrix) : unit =
        for i in 0 .. 3 do
            for j in 0 .. 3 do
                let e = Propagation.muellerElement expected i j
                let a = Propagation.muellerElement actual i j
                Assert.True(abs (e - a) < allowedDiff, $"M[{i},{j}]: expected {e}, got {a}")

    // -------------------------------------------------------------------------------- the noise model

    /// The FIXED seeds, one noisy experiment each. Nothing about the particular values matters; what
    /// matters is that they are fixed and distinct, so an entire ensemble — every angle error and every
    /// element error in every measurement of it — is reproducible bit for bit from this list alone.
    ///
    /// Both suites use the same eight, which is what makes their ensembles directly comparable.
    let ensembleSeeds : NoiseSeed list = [ 1; 2; 3; 4; 5; 6; 7; 8 ] |> List.map NoiseSeed

    /// Where the sample REALLY was when a nominal configuration was recorded.
    ///
    /// Both rotations are perturbed independently. The incidence angle is shifted through the engine's
    /// own `IncidenceAngle + Angle` operator rather than through `IncidenceAngle.create`: `create` folds
    /// its argument into [0, 90) modulo 90 deg, so a small NEGATIVE excursion from normal incidence would
    /// come back as ~89.8 deg rather than as a plate tilted 0.2 deg the other way. A signed excursion is
    /// the physically correct object here, and the operator is the seam that preserves it.
    let disturb (draws : UniformDeviateProvider) (noise : NoiseParam) (c : MeasurementConfiguration) : MeasurementConfiguration =
        let wobble () = noise.rotation.value * draws.nextDeviate () |> Angle.radian
        {
            c with
                incidenceAngle = c.incidenceAngle + wobble ()
                azimuth = c.azimuth.angle + wobble () |> SampleAzimuth
        }

    /// What the receiver REPORTS for a matrix the sample actually produced.
    ///
    /// The matrix is normalized first and each of the 15 elements other than `m₀₀` is then read a little
    /// high or a little low, independently. `m₀₀` is left at exactly 1 because it carries no information
    /// at all once normalized — an error in it is an error in absolute throughput, which every
    /// comparison in `MuellerInverse` divides out exactly.
    let misread (draws : UniformDeviateProvider) (noise : NoiseParam) (m : MuellerMatrix) : MuellerMatrix =
        match normalizeMueller m with
        | Ok n ->
            Propagation.muellerOfRows
                [ for i in 0 .. 3 ->
                    [ for j in 0 .. 3 ->
                        if i = 0 && j = 0 then 1.0
                        else Propagation.muellerElement n i j + noise.element.value * draws.nextDeviate () ] ]
        | Error e -> failwith $"a ground-truth matrix could not be normalized: %A{e}"

    /// ONE noisy realization of a whole experiment: for every configuration the sample truly sat at
    /// perturbed angles, the forward model is evaluated THERE, the detector then misreads the resulting
    /// matrix — and the observation is filed against the NOMINAL configuration, which is all the
    /// experimenter ever knows.
    ///
    /// That last step is the whole point of modelling angle error at all. Filing the data against the
    /// true angles would make the angle error invisible to the fit; filing it against the recorded ones
    /// is what a laboratory notebook actually contains.
    let noisyObserveWith
        (forward : ForwardModelProxy<'Parameters>)
        (truth : 'Parameters)
        (noise : NoiseParam)
        (seed : NoiseSeed)
        (configurations : MeasurementConfiguration list)
        : MuellerObservation list =
        let draws = UniformDeviateProvider.fromSeed seed
        configurations
        |> List.map (fun nominal ->
            match forward.muellerOf (disturb draws noise nominal) truth with
            | Ok m -> { configuration = nominal; measured = misread draws noise m }
            | Error e -> failwith $"the forward model failed to generate noisy data: %A{e}")

    /// Reduce an ensemble of recovered parameter sets to one statistics row per quantity.
    ///
    /// The scatter is the SAMPLE standard deviation (Bessel-corrected, `n − 1`), because the ensemble is
    /// a sample of possible experiments rather than the whole population of them — which is exactly what
    /// an experimenter repeating a measurement has.
    let statisticsFor
        (truth : 'Parameters)
        (recovered : 'Parameters list)
        (quantity : RecoveredQuantity<'Parameters>)
        : RecoveryStatistics<'Parameters> =
        let trueValue = quantity.valueIn truth
        let values = recovered |> List.map quantity.valueIn
        let count = List.length values
        let mean = List.average values
        let variance = (values |> List.sumBy (fun v -> (v - mean) ** 2.0)) / float (count - 1)
        {
            quantity = quantity
            mean = mean
            bias = RelativeError.between mean trueValue
            scatter = sqrt variance / abs trueValue |> RelativeError
            absoluteScatter = sqrt variance
            worst = values |> List.map (fun v -> RelativeError.between v trueValue) |> List.max
        }

    /// The standard error of the MEAN of an `n`-experiment ensemble, `scatter / sqrt n` — the yardstick a
    /// bias has to be measured against. A departure of the ensemble mean from the truth that is smaller
    /// than this is not evidence of a systematic error at all; it is the random error, not yet averaged
    /// away by a finite number of experiments.
    let standardErrorOfMean (count : int) (row : RecoveryStatistics<'Parameters>) : RelativeError =
        row.scatter.value / sqrt (float count) |> RelativeError

    /// Find a quantity's row in an ensemble table BY NAME. `RecoveredQuantity` holds a function and so
    /// has no structural equality; the name is the identity, and it comes from the material's own axis
    /// list rather than from a string typed at the call site.
    let rowFor (table : RecoveryStatistics<'Parameters> list) (name : string) : RecoveryStatistics<'Parameters> =
        match table |> List.tryFind (fun row -> row.quantity.name.value = name) with
        | Some row -> row
        | None -> failwith $"no ensemble row named {name}"

    /// What a SINGLE fit's own covariance says its uncertainty is, per fitted parameter, converted out
    /// of the scaled space into the same RELATIVE units an ensemble scatter is quoted in.
    ///
    /// This is the quantity that can be checked against an ensemble and almost never is: a covariance
    /// estimate is a PREDICTION ABOUT REPEAT EXPERIMENTS, and an ensemble is exactly a set of repeat
    /// experiments, so the two are comparable and their agreement is a real result rather than an
    /// assumption.
    let predictedRelativeErrors
        (truth : 'Parameters)
        (fit : InverseFit<'Parameters>)
        (quality : FitQuality.FitReport)
        : (ParameterName * RelativeError) list =
        fit.scaling.axes
        |> List.mapi (fun i a ->
            a.name,
            quality.standardErrors.[i] * abs (a.read fit.scaling.scale) / abs (a.read truth) |> RelativeError)

    /// The variance a fit's covariance predicts for a LINEAR COMBINATION of the fitted parameters,
    /// `wᵀ C w` — the only route this report offers to an error bar on a DERIVED quantity such as a
    /// birefringence.
    ///
    /// It is offered rather than trusted. When two parameters are strongly correlated the combination
    /// that isolates their difference is a near-total cancellation between comparable numbers, and the
    /// covariance it is formed from has already spent its precision inverting `JᵀJ`. Both suites measure
    /// what actually comes out; neither assumes it is usable.
    let combinationVariance (quality : FitQuality.FitReport) (weights : float[]) : double =
        let n = weights.Length
        [ for i in 0 .. n - 1 do
            for j in 0 .. n - 1 -> weights.[i] * quality.covariance.[i].[j] * weights.[j] ]
        |> List.sum

    /// The weight vector that picks out the DIFFERENCE of two named parameters, `plus − minus`, in the
    /// axis order of a given scaling. Built from the axis names rather than from indices typed at the
    /// call site, so it cannot silently address the wrong column.
    let differenceWeights (scaling : ParameterScaling<'Parameters>) (plus : string) (minus : string) : float[] =
        let index (name : string) =
            match scaling.axes |> List.tryFindIndex (fun a -> a.name.value = name) with
            | Some i -> i
            | None -> failwith $"no fitted parameter named {name}"
        let w = Array.zeroCreate scaling.dimension
        w.[index plus] <- 1.0
        w.[index minus] <- -1.0
        w
