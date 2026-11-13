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
