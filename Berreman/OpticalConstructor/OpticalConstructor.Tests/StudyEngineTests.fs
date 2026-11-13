namespace OpticalConstructor.Tests

open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.MuellerFitting
open Xunit

/// The study engine's facts (spec 0050 S11.3).
module StudyEngineTests =

    /// r(v) = [v0 − 5], whose unbounded minimum is v0 = 5. Every point it is evaluated at is recorded, in order.
    let private offsetByFive (evaluated : ResizeArray<float[]>) (ScaledVector v) : ResidualVector =
        evaluated.Add(Array.copy v)
        ResidualVector [| v.[0] - 5.0 |]

    let private solvedFrom (search : ScaledSearch) : float[] * ResizeArray<float[]> =
        let evaluated = ResizeArray<float[]>()
        match search.solveWith UnwatchedIterations (offsetByFive evaluated) with
        | Ok solution -> solution.solution.value, evaluated
        | Error e -> failwith $"expected Ok, got %A{e}"

    [<Fact>]
    let ``a scaled search starts where it is told and keeps to its own box`` () =
        let narrow : ScaledSearch =
            { start = ScaledVector [| 0.5 |]; lowerBounds = ScaledVector [| -1.0 |]; upperBounds = ScaledVector [| 2.0 |] }
        let wide : ScaledSearch =
            { narrow with lowerBounds = ScaledVector [| -10.0 |]; upperBounds = ScaledVector [| 10.0 |] }

        let pinned, narrowEvaluated = solvedFrom narrow
        Assert.Equal(1, pinned.Length)
        Assert.Equal(2.0, pinned.[0], 1e-9)

        let free, wideEvaluated = solvedFrom wide
        Assert.Equal(1, free.Length)
        Assert.Equal(5.0, free.[0], 1e-9)

        // ALGLIB evaluates the start first, and its bounded LM (`minlmsetbc`) evaluates only inside the box or on it.
        let runs : (ScaledSearch * ResizeArray<float[]>) list = [ narrow, narrowEvaluated; wide, wideEvaluated ]
        for search, evaluated in runs do
            Assert.NotEmpty evaluated
            Assert.Equal<float[]>(search.start.value, evaluated.[0])
            for point in evaluated do
                Assert.InRange(point.[0], search.lowerBounds.value.[0], search.upperBounds.value.[0])

    [<Fact>]
    let ``a search box's centred search starts at zero within plus or minus its half-width`` () =
        let parameters = CrystalMaterialFixtures.uniaxialParameters
        let threeAxes = scalingAround (List.take 3 UniaxialParameters.axes) parameters parameters
        let search = (SearchBox 50.0).centredSearch threeAxes
        Assert.Equal<float[]>([| 0.0; 0.0; 0.0 |], search.start.value)
        Assert.Equal<float[]>([| -50.0; -50.0; -50.0 |], search.lowerBounds.value)
        Assert.Equal<float[]>([| 50.0; 50.0; 50.0 |], search.upperBounds.value)
