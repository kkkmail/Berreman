namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.Fields
open Berreman.Solvers
open Berreman.FieldFunctions
open Analytics
open OpticalConstructor.Optimization
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.Ellipsometry
open OpticalConstructor.Optimization.MeritFunction
open Xunit

/// Part G §G.4–G.7 fitting-core tests (slice 010). Prove the two slice ACs in
/// the gated `OpticalConstructor.Tests` host:
///   AC-G3 — a [Core] bounded local-refinement run yields no negative
///           `Thickness` and drives each `Inequality` target's hinge to zero.
///   AC-G4 — inverse fitting from measured R CSV stores sample points in meters
///           via `WaveLength.value` and reduces χ² below its initial value.
/// Plus a §G.6 check that the net-new Ψ/Δ evaluator SHARES the §F.5 `psiDelta`
/// derivation rather than forking it.
module LocalRefinementTests =

    // ----------------------------------------------------------------- fixtures

    let private glass = OpticalProperties.fromRefractionIndex (RefractionIndex.create 1.52)

    /// Vacuum / glass film / vacuum — a single dielectric film between identical
    /// media, whose normal-incidence reflectance is an interference function of
    /// the film thickness (zero at the half-wave "absentee" thicknesses).
    let private mkSystem (thickness : float<nm>) : OpticalSystem =
        {
            description = None
            upper = OpticalProperties.vacuum
            films = [ { properties = glass; thickness = Thickness.nm thickness } ]
            substrate = None
            lower = OpticalProperties.vacuum
        }

    // Incident-light fixtures reuse the engine's first-class constructors
    // (`IncidentLightInfo.create`/`createInclined`, `Fields.fs:375-391`) rather
    // than hand-rolling the five-field record literal (reuse critic F2): `create w`
    // IS "default light at wavelength w" (vacuum, normal incidence, default
    // polarization/ellipticity), and `createInclined w a` is the same at angle a.

    /// Diagonal (45°) linear polarization carries EQUAL s- and p-components, so the
    /// reflected field's `amplitudeP`/`amplitudeS` both respond and ρ = r_p/r_s — and
    /// hence Ψ = atan|ρ| — actually depends on the stack. The engine default
    /// (`Polarization.defaultValue = Polarization.s`, `Fields.fs:321-322`) is pure
    /// s: on isotropic media the reflected p-amplitude is 0, so ρ ≡ 0 and Ψ ≡ 0
    /// regardless of thickness — useless as a fit target. See impl-log Gotchas.
    let private diagonalLight (a : IncidenceAngle) (w : WaveLength) : IncidentLightInfo =
        { IncidentLightInfo.createInclined w a with polarization = Polarization.create (Angle.degree 45.0) }

    let private reflectanceOf (system : OpticalSystem) (w : WaveLength) : float =
        OpticalSystemSolver(IncidentLightInfo.create w, system).solution.func R |> Option.get

    let private ssr (r : float[]) = r |> Array.sumBy (fun v -> v * v)

    // ----------------------------------------------------------------- AC-G3

    [<Fact>]
    let ``AC-G3 bounded refinement yields no negative thickness and drives the inequality hinge to zero`` () =
        // Lower-bounded thickness of 0 (the negative-thickness prohibition IS the
        // §G.3 lower bound), upper bound 1 µm.
        let parameters = [ DesignParameters.layerThickness 0 1.0e-6 ]
        let baseSystem = mkSystem 150.0<nm>
        let initial = [| 150.0e-9 |]   // canonical meters

        // One-sided target: R ≤ 0.5% at 600 nm. The half-wave thickness drives R
        // to ~0, so the hinge is satisfiable.
        let bound = 5.0e-3
        let target =
            {
                quantity = Photometric R
                samplePoint = IncidentLightInfo.create (WaveLength.nm 600.0<nm>)
                desiredValue = 0.0
                weight = 1.0
                tolerance = 1.0
                kind = Inequality (LessOrEqual bound)
            }

        match LocalRefinement.refine baseSystem parameters initial [ target ] with
        | Ok (refined, result) ->
            Assert.True(result.success)

            // No negative thickness in the refined system (the lower-bound-0
            // ParameterBounds, honoured by minlm, guarantees this).
            for film in refined.films do
                match film.thickness with
                | Thickness t -> Assert.True(t / 1.0<meter> >= 0.0, "refined thickness must be non-negative")
                | Infinity -> failwith "refined film thickness should be finite, not Infinity"

            // The inequality target's hinge is driven to zero within tolerance:
            // evaluate the SAME modelValue path at the refined solution.
            let model = MeritFunction.modelValue baseSystem parameters result.solution target
            let hinge = max 0.0 (model - bound)
            Assert.True(hinge < 1.0e-2, sprintf "expected hinge ~0, got %g (R = %g)" hinge model)
        | Error e -> failwith $"expected a refined system, got Error {e}"

    // ----------------------------------------------------------------- AC-G4

    [<Fact>]
    let ``AC-G4 inverse fit stores sample points in meters and reduces chi-squared`` () =
        // Synthetic "measured" R spectrum from a known 250 nm film.
        let trueSystem = mkSystem 250.0<nm>
        let waveNm = [ 450.0; 500.0; 550.0; 600.0; 650.0 ]
        let csvText =
            let rows =
                waveNm
                |> List.map (fun nm ->
                    let r = reflectanceOf trueSystem (WaveLength.nm (nm * 1.0<nm>))
                    sprintf "%g,%g" nm r)
            String.concat "\n" ("wavelength_nm,R" :: rows)

        let baseLight = IncidentLightInfo.create (WaveLength.nm 500.0<nm>)
        let baseSystem = mkSystem 1.0<nm>   // thickness slot; applyVector overrides it
        let parameters = [ DesignParameters.layerThickness 0 1.0e-6 ]
        let initial = [| 200.0e-9 |]        // start away from the true 250 nm

        match InverseFit.parseMeasurementCsv (Photometric R) baseLight csvText with
        | Ok targets ->
            // Sample points stored canonical-SI: `WaveLength.value` is meters.
            Assert.Equal(waveNm.Length, targets.Length)
            List.zip waveNm targets
            |> List.iter (fun (nm, t) ->
                Assert.Equal(nm * 1.0e-9, t.waveLength.value / 1.0<meter>, 12))

            // χ² before the fit, from the SAME residual the optimizer minimizes.
            let chiInitial = MeritFunction.buildResidual baseSystem parameters targets initial |> ssr

            match InverseFit.invertFromCsv (Photometric R) baseLight baseSystem parameters initial csvText with
            | Ok (refined, result) ->
                let chiFinal = ssr result.finalResiduals
                Assert.True(chiFinal < chiInitial, sprintf "expected χ² to drop: initial %g, final %g" chiInitial chiFinal)

                // Fold-back through the §G.3 mapping produced a real system with a
                // non-negative refined thickness.
                match refined.films.[0].thickness with
                | Thickness t -> Assert.True(t / 1.0<meter> >= 0.0)
                | Infinity -> failwith "refined film thickness should be finite, not Infinity"
            | Error e -> failwith $"expected a refined system, got Error {e}"
        | Error e -> failwith $"expected parsed targets, got Error {e}"

    // ----------------------------------------------------------------- §G.6 sharing

    [<Fact>]
    let ``G6 the Psi/Delta evaluator shares the F5 psiDelta derivation`` () =
        // Off-normal, 45°-polarized incidence so r_p and r_s differ and ρ = r_p/r_s
        // is non-trivial (a pure-s default would collapse ρ to 0 — see fixtures).
        let light = diagonalLight (Angle.degree 50.0 |> IncidenceAngle) (WaveLength.nm 600.0<nm>)
        let solution = OpticalSystemSolver(light, mkSystem 250.0<nm>).solution
        let (psi, delta) = AnalysisFunctions.psiDelta solution

        // The net-new EllipsometricFunction evaluator returns exactly the shared
        // §F.5 quantities — it does not re-derive an independent Ψ/Δ.
        Assert.Equal(psi, Psi.evaluate solution, 12)
        Assert.Equal(delta, Delta.evaluate solution, 12)

    // ------------------------------------------------ §G.6/§G.4 ellipsometric residual

    [<Fact>]
    let ``G6 an ellipsometric Psi FitTarget drives buildResidual and refine`` () =
        // R-3 requires Ψ/Δ to compose identically to photometric targets through
        // FitTarget; this drives an `Ellipsometric` target through buildResidual
        // AND refine (the net-new surface the photometric ACs do not exercise).
        // Off-normal, 45°-polarized incidence so ρ = r_p/r_s — hence Ψ — varies
        // with thickness (a pure-s default would collapse ρ to 0 — see fixtures).
        let angle = Angle.degree 50.0 |> IncidenceAngle
        let light = diagonalLight angle (WaveLength.nm 600.0<nm>)
        let trueSystem = mkSystem 250.0<nm>
        let desiredPsi = Psi.evaluate (OpticalSystemSolver(light, trueSystem).solution)

        let baseSystem = mkSystem 1.0<nm>
        let parameters = [ DesignParameters.layerThickness 0 1.0e-6 ]
        let initial = [| 220.0e-9 |]   // start away from the true 250 nm

        let target =
            {
                quantity = Ellipsometric Psi
                samplePoint = light
                desiredValue = desiredPsi
                weight = 1.0
                tolerance = 1.0
                kind = Equality
            }

        // buildResidual actually evaluates the ellipsometric modelValue path:
        // a non-zero start residual proves the Ψ target is wired, not stubbed.
        let chiInitial = MeritFunction.buildResidual baseSystem parameters [ target ] initial |> ssr
        Assert.True(chiInitial > 0.0, "the ellipsometric residual must be non-trivial at the start")

        match LocalRefinement.refine baseSystem parameters initial [ target ] with
        | Ok (_, result) ->
            let chiFinal = ssr result.finalResiduals
            Assert.True(chiFinal < chiInitial, sprintf "expected χ² to drop: initial %g, final %g" chiInitial chiFinal)
        | Error e -> failwith $"expected a refined system, got Error {e}"

    // ------------------------------------------------ §G.7 ellipsometric CSV path

    [<Fact>]
    let ``G7 inverse fit drives the MeasuredEllipsometric CSV path`` () =
        // R-4's MeasuredEllipsometric import case (InverseFit.fs) is net-new
        // surface the photometric AC-G4 does not touch. Synthesize a Ψ spectrum
        // from a known 250 nm film at fixed off-normal geometry, then fit thickness.
        let angle = Angle.degree 50.0 |> IncidenceAngle
        let trueSystem = mkSystem 250.0<nm>
        let waveNm = [ 450.0; 500.0; 550.0; 600.0; 650.0 ]
        let psiAt (nm : float) =
            let light = diagonalLight angle (WaveLength.nm (nm * 1.0<nm>))
            Psi.evaluate (OpticalSystemSolver(light, trueSystem).solution)
        let csvText =
            let rows = waveNm |> List.map (fun nm -> sprintf "%g,%g" nm (psiAt nm))
            String.concat "\n" ("wavelength_nm,Psi" :: rows)

        let baseLight = diagonalLight angle (WaveLength.nm 500.0<nm>)
        let baseSystem = mkSystem 1.0<nm>
        let parameters = [ DesignParameters.layerThickness 0 1.0e-6 ]
        let initial = [| 220.0e-9 |]   // start away from the true 250 nm

        match InverseFit.parseMeasurementCsv (Ellipsometric Psi) baseLight csvText with
        | Ok targets ->
            Assert.Equal(waveNm.Length, targets.Length)

            // The MeasuredEllipsometric column maps onto the `Ellipsometric` quantity
            // (not photometric), and each sample point is stored canonical-SI (meters).
            List.zip waveNm targets
            |> List.iter (fun (nm, t) ->
                match t.quantity with
                | Ellipsometric Psi -> ()
                | other -> failwith $"expected Ellipsometric Psi, got {other}"
                Assert.Equal(nm * 1.0e-9, t.waveLength.value / 1.0<meter>, 12))

            // The ellipsometric residual built from these targets is well-posed:
            // it is (near-)zero at the true 250 nm that generated the data and
            // strictly larger at the 220 nm start, so the MeasuredEllipsometric
            // targets are genuinely sensitive to the regressed thickness — the
            // composition through buildResidual is real, not stubbed. (LM descent
            // itself is proven by the G6 single-point fit and AC-G4; fit quality
            // over the full multi-point path is slice 011, §G.8.)
            let chiAtStart = MeritFunction.buildResidual baseSystem parameters targets [| 220.0e-9 |] |> ssr
            let chiAtTrue = MeritFunction.buildResidual baseSystem parameters targets [| 250.0e-9 |] |> ssr
            Assert.True(chiAtTrue < chiAtStart, sprintf "ellipsometric residual must be sensitive to thickness: true %g, start %g" chiAtTrue chiAtStart)
            Assert.True(chiAtTrue < 1.0e-9, sprintf "synthetic data must be reproduced at the true thickness, got χ² %g" chiAtTrue)

            // Drive the full MeasuredEllipsometric CSV → FitTargets → §G.5 refine
            // path end to end and fold the solution back through the §G.3 mapping.
            match InverseFit.invertFromCsv (Ellipsometric Psi) baseLight baseSystem parameters initial csvText with
            | Ok (refined, _) ->
                match refined.films.[0].thickness with
                | Thickness t -> Assert.True(t / 1.0<meter> >= 0.0, "refined thickness must be non-negative")
                | Infinity -> failwith "refined film thickness should be finite, not Infinity"
            | Error e -> failwith $"expected a refined system, got Error {e}"
        | Error e -> failwith $"expected parsed targets, got Error {e}"
