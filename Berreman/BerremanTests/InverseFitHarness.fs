namespace BerremanTests

open Berreman.Geometry
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Domain
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization
open OpticalConstructor.Optimization.MuellerFitting
open OpticalConstructor.Optimization.BenchNoise
open Xunit
open BerremanTests.MatrixComparison

/// Spec 0044 — the machinery every inverse-problem suite in this assembly shares.
///
/// `MuellerInverseTests` (uniaxial quartz, four unknowns) and `BiaxialInverseTests` (triclinic, nine
/// unknowns) differ in their MATERIAL and in their MEASUREMENT SET, and in nothing else: the same
/// forward evaluation, the same residual, the same Levenberg–Marquardt call and the same acceptance
/// arithmetic drive both. That is only true because `MuellerInverse` is generic in `'Parameters`, and
/// this module is where that genericity is actually cashed in — it is what stops a second material from
/// being a copy of the first with the field names changed.
module InverseFitHarness =

    /// A moved product call's value, or the test fails with its typed error (spec 0050 S10.1).
    let expectOk (result : Result<'Value, MuellerFitError>) : 'Value =
        match result with
        | Ok value -> value
        | Error e -> failwith $"%A{e}"

    /// FitQuality's report on a fit, at its solution.
    let fitReportOf (fit : MuellerFitData<'Parameters>) : FitQuality.FitReport =
        let (ScaledVector solution) = fit.solution.solution
        FitQuality.reportFrom
            (fun (OptimizationInterface.DesignVector v) -> fit.residual (ScaledVector v))
            (OptimizationInterface.DesignVector solution)
            fit.solution.finalResiduals

    /// How hard the residual pushes back along ONE DIRECTION in parameter space — `‖J·v‖` for a unit
    /// direction `v`, computed from the residual Jacobian.
    ///
    /// A single Jacobian COLUMN answers "is this parameter observable?". This answers the sharper
    /// question "is this COMBINATION of parameters observable?", which is what an identifiability claim
    /// about a null direction needs: the isotropic part of an absorption tensor is not any one parameter,
    /// it is the sum of three of them, and it can be invisible while every individual column is alive.
    let directionalSensitivity (jacobian : ResidualJacobian) (direction : float[]) : double =
        let norm = sqrt (direction |> Array.sumBy (fun x -> x * x))
        let unit = direction |> Array.map (fun x -> x / norm)
        jacobian.value
        |> Array.sumBy (fun row -> (Array.map2 (*) row unit |> Array.sum) ** 2.0)
        |> sqrt

    /// The weight vector that picks out an equal-weighted SUM of the named parameters, in the axis order
    /// of a given scaling. Built from the axis names rather than from indices typed at the call site.
    let sumWeights (scaling : ParameterScaling<'Parameters>) (names : string list) : float[] =
        let w = Array.zeroCreate scaling.dimension
        for name in names do
            match scaling.axes |> List.tryFindIndex (fun a -> a.name.value = name) with
            | Some i -> w.[i] <- 1.0
            | None -> failwith $"no fitted parameter named {name}"
        w

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
                let e = expected.element i j
                let a = actual.element i j
                Assert.True(abs (e - a) < allowedDiff, $"M[{i},{j}]: expected {e}, got {a}")

    // -------------------------------------------------------------------------------- the noise model

    /// The FIXED seeds, one noisy experiment each. Nothing about the particular values matters; what
    /// matters is that they are fixed and distinct, so an entire ensemble — every angle error and every
    /// element error in every measurement of it — is reproducible bit for bit from this list alone.
    ///
    /// Both suites use the same eight, which is what makes their ensembles directly comparable.
    let ensembleSeeds : NoiseSeed list = [ 1; 2; 3; 4; 5; 6; 7; 8 ] |> List.map NoiseSeed

    /// What a SINGLE fit's own covariance says its uncertainty is, per fitted parameter, converted out
    /// of the scaled space into the same RELATIVE units an ensemble scatter is quoted in.
    ///
    /// This is the quantity that can be checked against an ensemble and almost never is: a covariance
    /// estimate is a PREDICTION ABOUT REPEAT EXPERIMENTS, and an ensemble is exactly a set of repeat
    /// experiments, so the two are comparable and their agreement is a real result rather than an
    /// assumption.
    let predictedRelativeErrors
        (truth : 'Parameters)
        (fit : MuellerFitData<'Parameters>)
        (quality : FitQuality.FitReport)
        : (ParameterName * RelativeError) list =
        fit.scaling.axes
        |> List.mapi (fun i a ->
            a.name,
            quality.standardErrors.value.[i] * abs (a.read fit.scaling.scale) / abs (a.read truth) |> RelativeErr)

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
            for j in 0 .. n - 1 -> weights.[i] * quality.covariance.value.[i].[j] * weights.[j] ]
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
