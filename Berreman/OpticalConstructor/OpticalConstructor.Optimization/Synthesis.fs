namespace OpticalConstructor.Optimization

open Berreman.Constants
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.DesignParameters
open OpticalConstructor.Optimization.MeritFunction
open OpticalConstructor.Optimization.FitQuality

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

    // `ResidualVector.sumOfSquares` (Σ residualᵢ²) is the shared least-squares definition from
    // `OptimizationInterface` (reuse-critic F1) — not re-declared here.

    /// The merit of a residual at a design vector — the G.4 weighted-LSQ sum of squares (the §G.8 χ²), the
    /// single objective both synthesis families minimize.
    let private merit (residual : Residual) (x : DesignVector) : ChiSquared = ChiSquared (residual x).sumOfSquares

    /// The merit of a CONCRETE system (no free parameters): evaluate the §G.4
    /// residual with an empty `DesignParameter` list / empty vector, so
    /// `applyVector [] DesignVector.empty system = system`.
    let systemMerit (system : OpticalSystem) (targets : FitTarget list) : ChiSquared =
        merit (MeritFunction.buildResidual system [] targets) DesignVector.empty

    type ParameterBounds with

        /// The design clamped into the box (an `infinity` side leaves it untouched).
        member this.clamp (x : DesignVector) : DesignVector =
            let (DesignVector lower) = this.lower
            let (DesignVector upper) = this.upper
            let (DesignVector v) = x
            v |> Array.mapi (fun i e -> max lower.[i] (min upper.[i] e)) |> DesignVector

        /// A finite proposal span for parameter `i` at value `x`: the bound width, or a fallback
        /// scaled by the current value when a side is unbounded.
        member this.proposalSpan (i : int) (x : float) : float =
            let (DesignVector lower) = this.lower
            let (DesignVector upper) = this.upper
            let lo = lower.[i]
            let hi = upper.[i]
            if System.Double.IsInfinity lo || System.Double.IsInfinity hi then max 1.0 (abs x)
            else hi - lo

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
        (needleThickness : Thickness)
        (upperThickness : Thickness)
        (baseSystem : OpticalSystem)
        (targets : FitTarget list)
        : Result<OpticalSystem * OptimizationResult, OptimizationError> =
        let needle : Layer = { properties = needleMaterial; thickness = needleThickness }
        let insertAt (p : int) : OpticalSystem =
            let before, after = List.splitAt p baseSystem.films
            { baseSystem with films = before @ [ needle ] @ after }

        let positions = [ 0 .. baseSystem.films.Length ]
        // Pick the insertion depth whose grown system has the lowest merit.
        let bestPosition =
            positions
            |> List.map (fun p -> let (ChiSquared m) = systemMerit (insertAt p) targets in p, m)
            |> List.minBy snd
            |> fst
        let grown = insertAt bestPosition

        // Re-refine every film thickness of the grown design (unbounded layer
        // count is exactly what needle synthesis grows; lower bound 0 is the
        // negative-thickness prohibition, §G.3). A semi-infinite film starts at the needle's thickness.
        let parameters = grown.films |> List.mapi (fun i _ -> DesignParameters.layerThickness (FilmIndex i) upperThickness)
        let initial =
            grown.films
            |> List.map (fun film ->
                match film.thickness with
                | Infinity -> needleThickness.designValue
                | Thickness _ -> film.thickness.designValue)
            |> List.map (fun (DesignValue v) -> v)
            |> Array.ofList
            |> DesignVector
        LocalRefinement.refine grown parameters initial targets

    // ----------------------------------------------------------------- tunnelling

    /// **Tunnelling / gradual evolution (010 §II.5).** Perturb a converged design
    /// vector by an explicit offset to hop out of a local minimum, then re-refine
    /// (§G.5). Deterministic: the caller supplies the perturbation, so a stalled
    /// fit can be nudged reproducibly. Composes with `needleInsertion` (both grow /
    /// escape the same `OpticalSystem`/`DesignParameter` representation).
    let tunnel
        (perturbation : DesignVector)
        (baseSystem : OpticalSystem)
        (parameters : DesignParameter list)
        (converged : DesignVector)
        (targets : FitTarget list)
        : Result<OpticalSystem * OptimizationResult, OptimizationError> =
        let (DesignVector offsets) = perturbation
        let (DesignVector start) = converged
        let perturbed =
            start
            |> Array.mapi (fun i v -> v + (if i < offsets.Length then offsets.[i] else 0.0))
            |> DesignVector
        LocalRefinement.refine baseSystem parameters perturbed targets

    // ------------------------------------------------------------- search knobs

    /// The seed that makes a stochastic search reproducible.
    type RandomSeed =
        | RandomSeed of int

        member this.value = let (RandomSeed s) = this in s

    /// Simulated annealing's starting temperature, in merit units: the uphill move the Metropolis rule
    /// still accepts with probability 1/e.
    type AnnealingTemperature =
        | AnnealingTemperature of float

        member this.value = let (AnnealingTemperature t) = this in t

    /// The factor the annealing temperature is multiplied by after every iteration (geometric cooling).
    type CoolingRate =
        | CoolingRate of float

        member this.value = let (CoolingRate r) = this in r

    /// A proposal step as a fraction of the parameter's span (`ParameterBounds.proposalSpan`).
    type ProposalScale =
        | ProposalScale of float

        member this.value = let (ProposalScale s) = this in s

    /// The number of individuals in every generation of the genetic algorithm.
    type PopulationSize =
        | PopulationSize of int

        member this.value = let (PopulationSize n) = this in n

    /// The probability that one gene of a genetic-algorithm child is mutated.
    type MutationRate =
        | MutationRate of float

        member this.value = let (MutationRate r) = this in r

    /// What a §G.9 loop's cancellation flag answers, polled once per iteration or generation (AC-G7).
    type SearchCancellation =
        | SearchCancelRequested
        | SearchNotCancelled

    // ------------------------------------------------------- simulated annealing

    /// Simulated-annealing schedule knobs. `seed` makes a run reproducible.
    type AnnealingConfig =
        {
            seed : RandomSeed
            iterations : IterationCount
            initialTemperature : AnnealingTemperature
            coolingRate : CoolingRate
            stepScale : ProposalScale
        }

        static member defaultValue =
            {
                seed = RandomSeed 1
                iterations = IterationCount 1000
                initialTemperature = AnnealingTemperature 1.0
                coolingRate = CoolingRate 0.997
                stepScale = ProposalScale 0.1
            }

    /// **Simulated annealing (010 §II.5).** A Metropolis loop over the G.1
    /// `Residual` closure: propose a bounded neighbour, accept downhill moves and
    /// uphill moves with probability exp(−ΔE/T), cool T geometrically, and return
    /// the best design seen. Checks `shouldCancel` each iteration (AC-G7).
    let simulatedAnnealing
        (config : AnnealingConfig)
        (shouldCancel : unit -> SearchCancellation)
        (bounds : ParameterBounds)
        (residual : Residual)
        (initial : DesignVector)
        : OptimizationResult =
        let (RandomSeed seed) = config.seed
        let (IterationCount iterations) = config.iterations
        let (AnnealingTemperature initialTemperature) = config.initialTemperature
        let (CoolingRate coolingRate) = config.coolingRate
        let (ProposalScale stepScale) = config.stepScale
        let rng = System.Random(seed)
        let (DesignVector start) = initial
        let n = start.Length
        let mutable current = bounds.clamp initial
        let (ChiSquared startE) = merit residual current
        let mutable currentE = startE
        let mutable best = current.copy
        let mutable bestE = currentE
        let mutable temp = initialTemperature
        let mutable iter = 0
        let mutable cancelled = false
        while iter < iterations && not cancelled do
            match shouldCancel () with
            | SearchCancelRequested -> cancelled <- true
            | SearchNotCancelled ->
                let (DesignVector cur) = current
                let candidate =
                    [| for i in 0 .. n - 1 ->
                        let w = bounds.proposalSpan i cur.[i]
                        cur.[i] + (rng.NextDouble() * 2.0 - 1.0) * stepScale * w |]
                    |> DesignVector
                    |> bounds.clamp
                let (ChiSquared candE) = merit residual candidate
                let dE = candE - currentE
                if dE <= 0.0 || rng.NextDouble() < exp (-dE / max 1.0e-12 temp) then
                    current <- candidate
                    currentE <- candE
                    if candE < bestE then
                        best <- candidate.copy
                        bestE <- candE
                temp <- temp * coolingRate
                iter <- iter + 1
        {
            solution = best
            finalResiduals = residual best
            iterations = IterationCount iter
            terminationReason =
                if cancelled then SearchCancelled
                elif iter >= iterations then MaxIterationsReached
                else Converged
        }

    // -------------------------------------------------------- genetic algorithm

    /// Genetic / evolutionary-algorithm knobs. `seed` makes a run reproducible.
    type GeneticConfig =
        {
            seed : RandomSeed
            generations : IterationCount
            populationSize : PopulationSize
            mutationRate : MutationRate
            mutationScale : ProposalScale
        }

        static member defaultValue =
            {
                seed = RandomSeed 1
                generations = IterationCount 200
                populationSize = PopulationSize 24
                mutationRate = MutationRate 0.2
                mutationScale = ProposalScale 0.1
            }

    /// **Genetic / evolutionary algorithm (010 §II.5).** A bounded real-coded GA
    /// over the G.1 `Residual` closure: tournament selection, uniform crossover,
    /// Gaussian-ish bounded mutation, elitism. Returns the best design seen.
    /// Checks `shouldCancel` each generation (AC-G7).
    let geneticAlgorithm
        (config : GeneticConfig)
        (shouldCancel : unit -> SearchCancellation)
        (bounds : ParameterBounds)
        (residual : Residual)
        (initial : DesignVector)
        : OptimizationResult =
        let (RandomSeed seed) = config.seed
        let (IterationCount generations) = config.generations
        let (PopulationSize populationSize) = config.populationSize
        let (MutationRate mutationRate) = config.mutationRate
        let (ProposalScale mutationScale) = config.mutationScale
        let rng = System.Random(seed)
        let (DesignVector start) = initial
        let n = start.Length
        let (DesignVector lower) = bounds.lower
        let (DesignVector upper) = bounds.upper
        let sampleGene i =
            let lo = lower.[i]
            let hi = upper.[i]
            if System.Double.IsInfinity lo || System.Double.IsInfinity hi then
                start.[i] + (rng.NextDouble() * 2.0 - 1.0) * max 1.0 (abs start.[i])
            else lo + rng.NextDouble() * (hi - lo)
        let randomIndividual () = [| for i in 0 .. n - 1 -> sampleGene i |] |> DesignVector
        let score (x : DesignVector) = let (ChiSquared e) = merit residual x in x, e
        let bestOf (s : (DesignVector * float)[]) = s |> Array.minBy snd
        // Seed the population with the (clamped) initial plus random individuals.
        let mutable scored =
            Array.init populationSize (fun k ->
                if k = 0 then bounds.clamp initial else randomIndividual ())
            |> Array.map score
        let mutable best = bestOf scored
        let mutable gen = 0
        let mutable cancelled = false
        let tournament () =
            let a = scored.[rng.Next scored.Length]
            let b = scored.[rng.Next scored.Length]
            if snd a <= snd b then fst a else fst b
        while gen < generations && not cancelled do
            match shouldCancel () with
            | SearchCancelRequested -> cancelled <- true
            | SearchNotCancelled ->
                let nextPop =
                    Array.init populationSize (fun k ->
                        if k = 0 then fst best   // elitism keeps the incumbent
                        else
                            let (DesignVector p1) = tournament ()
                            let (DesignVector p2) = tournament ()
                            [| for i in 0 .. n - 1 ->
                                let gene = if rng.NextDouble() < 0.5 then p1.[i] else p2.[i]
                                if rng.NextDouble() < mutationRate then
                                    let w = bounds.proposalSpan i gene
                                    gene + (rng.NextDouble() * 2.0 - 1.0) * mutationScale * w
                                else gene |]
                            |> DesignVector
                            |> bounds.clamp)
                scored <- nextPop |> Array.map score
                let genBest = bestOf scored
                if snd genBest < snd best then best <- genBest
                gen <- gen + 1
        let (bx, _) = best
        {
            solution = bx
            finalResiduals = residual bx
            iterations = IterationCount gen
            terminationReason =
                if cancelled then SearchCancelled
                elif gen >= generations then MaxIterationsReached
                else Converged
        }
