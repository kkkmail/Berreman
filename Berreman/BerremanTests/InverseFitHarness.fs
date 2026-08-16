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
