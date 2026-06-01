namespace OpticalConstructor.Optimization

open Berreman.Constants
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.DesignParameters
open OpticalConstructor.Optimization.MeritFunction

/// §G.9 — needle/tunnelling synthesis and global optimization (spec 0022 Part G
/// §G.9, [Standard]).
///
/// `needleInsertion` evaluates the merit at each candidate insertion depth in the
/// base `OpticalSystem.films` (`Media.fs:94`), inserts ONE `Layer` (`Media.fs:24`)
/// of small thickness at the position that most decreases the merit, then re-runs
/// §G.5 local refinement on the grown design — growing the layer count without
/// bounding the parameter space (010 §II.5). `tunnel` perturbs a converged design
/// to escape a local minimum and re-refines; combined with needle insertion these
/// are the two synthesis families 010 §II.5 names.
///
/// `simulatedAnnealing` and `geneticAlgorithm` are the two global methods for the
/// multimodal landscapes where refinement stalls (010 §II.5). ALGLIB provides no
/// annealing/GA routine, so per §G.9 they are explicit F# loops over the SAME
/// G.1 `Residual` closure (each candidate evaluated by the G.4 merit assembly);
/// the needle's re-refinement step still routes through ALGLIB `minlm` via §G.5/
/// §G.2 where that routine exists. Every loop observes a `shouldCancel` flag so a
/// §G.10 Cancel stops the G.9 loop cooperatively (AC-G7).
///
/// No parallel-evaluation infrastructure, distributed execution, or synthesis
/// strategy plugin system is added (§G.9 non-requirement) — only these families.
module Synthesis =

    // `sumSq` (Σ residualᵢ²) is the shared least-squares definition from
    // `OptimizationInterface` (reuse-critic F1) — not re-declared here.

    /// Scalar merit of a residual at a design vector — the G.4 weighted-LSQ sum of
    /// squares, the single objective both synthesis families minimize.
    let private merit (residual : Residual) (x : float[]) : float = sumSq (residual x)

    /// The merit of a CONCRETE system (no free parameters): evaluate the §G.4
    /// residual with an empty `DesignParameter` list / empty vector, so
    /// `applyVector [] [||] system = system`.
    let systemMerit (system : OpticalSystem) (targets : FitTarget list) : float =
        MeritFunction.buildResidual system [] targets [||] |> sumSq

    /// Clamp a vector into the box bounds (an `infinity` side leaves it untouched).
    let private clamp (bounds : ParameterBounds) (x : float[]) : float[] =
        Array.mapi (fun i v -> max bounds.lower.[i] (min bounds.upper.[i] v)) x

    /// A finite proposal span for parameter `i`: the bound width, or a fallback
    /// scaled by the current value when a side is unbounded.
    let private span (lo : float) (hi : float) (x : float) : float =
        if System.Double.IsInfinity lo || System.Double.IsInfinity hi then max 1.0 (abs x)
        else hi - lo

    let private thicknessMeters (fallback : float) (l : Layer) : float =
        match l.thickness with
        | Thickness t -> t / 1.0<meter>
        | Infinity -> fallback

    // ------------------------------------------------------------------- needle

    /// **Needle optimization (010 §II.5).** Try inserting a vanishingly-thin
    /// `Layer` of `needleMaterial` at every candidate depth (before each film and
    /// after the last), keep the position whose insertion most decreases the merit,
    /// then re-refine ALL film thicknesses of the grown design via §G.5 LM. The
    /// films list grows by exactly one `Layer`; because a needle of `needleThickness`
    /// ≈ 0 is optically near-absent, the grown design's merit starts ≈ the base
    /// merit and re-refinement only decreases it, so the post-insertion merit is no
    /// greater than the pre-insertion merit (AC-G6).
    let needleInsertion
        (needleMaterial : OpticalProperties)
        (needleThickness : float)
        (upperThickness : float)
        (baseSystem : OpticalSystem)
        (targets : FitTarget list)
        : Result<OpticalSystem * OptimizationResult, string> =
        let needle = { properties = needleMaterial; thickness = Thickness (needleThickness * 1.0<meter>) }
        let insertAt (p : int) : OpticalSystem =
            let before, after = List.splitAt p baseSystem.films
            { baseSystem with films = before @ [ needle ] @ after }

        let positions = [ 0 .. baseSystem.films.Length ]
        // Pick the insertion depth whose grown system has the lowest merit.
        let bestPosition =
            positions
            |> List.map (fun p -> p, systemMerit (insertAt p) targets)
            |> List.minBy snd
            |> fst
        let grown = insertAt bestPosition

        // Re-refine every film thickness of the grown design (unbounded layer
        // count is exactly what needle synthesis grows; lower bound 0 is the
        // negative-thickness prohibition, §G.3).
        let parameters = grown.films |> List.mapi (fun i _ -> DesignParameters.layerThickness i upperThickness)
        let initial = grown.films |> List.map (thicknessMeters needleThickness) |> Array.ofList
        LocalRefinement.refine grown parameters initial targets

    // ----------------------------------------------------------------- tunnelling

    /// **Tunnelling / gradual evolution (010 §II.5).** Perturb a converged design
    /// vector by an explicit offset to hop out of a local minimum, then re-refine
    /// (§G.5). Deterministic: the caller supplies the perturbation, so a stalled
    /// fit can be nudged reproducibly. Composes with `needleInsertion` (both grow /
    /// escape the same `OpticalSystem`/`DesignParameter` representation).
    let tunnel
        (perturbation : float[])
        (baseSystem : OpticalSystem)
        (parameters : DesignParameter list)
        (converged : float[])
        (targets : FitTarget list)
        : Result<OpticalSystem * OptimizationResult, string> =
        let perturbed =
            converged
            |> Array.mapi (fun i v -> v + (if i < perturbation.Length then perturbation.[i] else 0.0))
        LocalRefinement.refine baseSystem parameters perturbed targets

    // ------------------------------------------------------- simulated annealing

    /// Simulated-annealing schedule knobs. `seed` makes a run reproducible.
    type AnnealingConfig =
        {
            seed : int
            iterations : int
            initialTemperature : float
            coolingRate : float
            stepScale : float
        }

        static member defaultValue =
            { seed = 1; iterations = 1000; initialTemperature = 1.0; coolingRate = 0.997; stepScale = 0.1 }

    /// **Simulated annealing (010 §II.5).** A Metropolis loop over the G.1
    /// `Residual` closure: propose a bounded neighbour, accept downhill moves and
    /// uphill moves with probability exp(−ΔE/T), cool T geometrically, and return
    /// the best design seen. Checks `shouldCancel` each iteration (AC-G7).
    let simulatedAnnealing
        (config : AnnealingConfig)
        (shouldCancel : unit -> bool)
        (bounds : ParameterBounds)
        (residual : Residual)
        (initial : float[])
        : OptimizationResult =
        let rng = System.Random(config.seed)
        let n = initial.Length
        let mutable current = clamp bounds (Array.copy initial)
        let mutable currentE = merit residual current
        let mutable best = Array.copy current
        let mutable bestE = currentE
        let mutable temp = config.initialTemperature
        let mutable iter = 0
        let mutable cancelled = false
        while iter < config.iterations && not cancelled do
            if shouldCancel () then cancelled <- true
            else
                let candidate =
                    [| for i in 0 .. n - 1 ->
                        let w = span bounds.lower.[i] bounds.upper.[i] current.[i]
                        current.[i] + (rng.NextDouble() * 2.0 - 1.0) * config.stepScale * w |]
                    |> clamp bounds
                let candE = merit residual candidate
                let dE = candE - currentE
                if dE <= 0.0 || rng.NextDouble() < exp (-dE / max 1.0e-12 temp) then
                    current <- candidate
                    currentE <- candE
                    if candE < bestE then
                        best <- Array.copy candidate
                        bestE <- candE
                temp <- temp * config.coolingRate
                iter <- iter + 1
        {
            solution = best
            finalResiduals = residual best
            iterations = iter
            terminationReason =
                if cancelled then Failed "cancelled"
                elif iter >= config.iterations then MaxIterationsReached
                else Converged
            success = not cancelled
        }

    // -------------------------------------------------------- genetic algorithm

    /// Genetic / evolutionary-algorithm knobs. `seed` makes a run reproducible.
    type GeneticConfig =
        {
            seed : int
            generations : int
            populationSize : int
            mutationRate : float
            mutationScale : float
        }

        static member defaultValue =
            { seed = 1; generations = 200; populationSize = 24; mutationRate = 0.2; mutationScale = 0.1 }

    /// **Genetic / evolutionary algorithm (010 §II.5).** A bounded real-coded GA
    /// over the G.1 `Residual` closure: tournament selection, uniform crossover,
    /// Gaussian-ish bounded mutation, elitism. Returns the best design seen.
    /// Checks `shouldCancel` each generation (AC-G7).
    let geneticAlgorithm
        (config : GeneticConfig)
        (shouldCancel : unit -> bool)
        (bounds : ParameterBounds)
        (residual : Residual)
        (initial : float[])
        : OptimizationResult =
        let rng = System.Random(config.seed)
        let n = initial.Length
        let sampleGene i =
            let lo = bounds.lower.[i]
            let hi = bounds.upper.[i]
            if System.Double.IsInfinity lo || System.Double.IsInfinity hi then
                initial.[i] + (rng.NextDouble() * 2.0 - 1.0) * max 1.0 (abs initial.[i])
            else lo + rng.NextDouble() * (hi - lo)
        let randomIndividual () = [| for i in 0 .. n - 1 -> sampleGene i |]
        let score (x : float[]) = x, merit residual x
        let bestOf (s : (float[] * float)[]) = s |> Array.minBy snd
        // Seed the population with the (clamped) initial plus random individuals.
        let mutable scored =
            Array.init config.populationSize (fun k ->
                if k = 0 then clamp bounds (Array.copy initial) else randomIndividual ())
            |> Array.map score
        let mutable best = bestOf scored
        let mutable gen = 0
        let mutable cancelled = false
        let tournament () =
            let a = scored.[rng.Next scored.Length]
            let b = scored.[rng.Next scored.Length]
            if snd a <= snd b then fst a else fst b
        while gen < config.generations && not cancelled do
            if shouldCancel () then cancelled <- true
            else
                let nextPop =
                    Array.init config.populationSize (fun k ->
                        if k = 0 then fst best   // elitism keeps the incumbent
                        else
                            let p1 = tournament ()
                            let p2 = tournament ()
                            [| for i in 0 .. n - 1 ->
                                let gene = if rng.NextDouble() < 0.5 then p1.[i] else p2.[i]
                                if rng.NextDouble() < config.mutationRate then
                                    let w = span bounds.lower.[i] bounds.upper.[i] gene
                                    gene + (rng.NextDouble() * 2.0 - 1.0) * config.mutationScale * w
                                else gene |]
                            |> clamp bounds)
                scored <- nextPop |> Array.map score
                let genBest = bestOf scored
                if snd genBest < snd best then best <- genBest
                gen <- gen + 1
        let (bx, _) = best
        {
            solution = bx
            finalResiduals = residual bx
            iterations = gen
            terminationReason =
                if cancelled then Failed "cancelled"
                elif gen >= config.generations then MaxIterationsReached
                else Converged
            success = not cancelled
        }
