namespace OpticalConstructor.Optimization

open Berreman.Constants
open Berreman.Geometry
open Berreman.Media
open Berreman.MaterialProperties
open Berreman.Dispersion
open Berreman.Fields
open OpticalConstructor.Optimization.OptimizationInterface

/// §G.3 — the flat `float[]` ⇄ `OpticalSystem` design-variable mapping
/// (spec 0022 Part G §G.3).
///
/// Reuses the existing scalar parametrization seam
/// `ArbitraryVariable.getSys : OpticalSystem -> double -> OpticalSystem`
/// (`Variables.fs:37`, which `calculate` already calls at `Variables.fs:263`)
/// rather than inventing a new mechanism. Every mapping is an explicit F#
/// closure the reader can follow — NO reflection-based field setting (§0
/// locality rule). Negative-thickness prohibition and physical n-range bounding
/// (010 §II.5) are expressed as the G.1 `ParameterBounds` (lower bound 0 for
/// thickness), not as a separate validator.
module DesignParameters =

    /// One scalar design variable: a name, a `getSys` closure with the same
    /// signature as `ArbitraryVariable.getSys`, and the per-parameter SI bound
    /// pair (`lower`/`upper`, canonical units, §0 constraint 3).
    type DesignParameter =
        {
            name : string
            getSys : OpticalSystem -> double -> OpticalSystem
            lower : float
            upper : float
        }

    /// Layer-thickness design variable: rebuild film `index`'s `Thickness` from
    /// `v` (already in canonical meters, §0 constraint 3) as
    /// `Thickness (v * 1.0<meter>)` (`Media.fs:12`). The lower bound is fixed at
    /// 0 — that bound IS the negative-thickness prohibition (R-3), not a validator.
    let layerThickness (index : int) (upperMeters : float) : DesignParameter =
        {
            name = sprintf "film[%d].thickness" index
            getSys =
                fun (sys : OpticalSystem) (v : double) ->
                    let films =
                        sys.films
                        |> List.mapi (fun i (l : Layer) ->
                            if i = index then { l with thickness = Thickness (v * 1.0<meter>) } else l)
                    { sys with films = films }
            lower = 0.0
            upper = upperMeters
        }

    /// Wedge-angle design variable: set the substrate `WedgeLayer.angle`
    /// (`Media.fs:34`) to `v` radians. A non-wedge substrate is left untouched.
    let wedgeAngle (lower : float, upper : float) : DesignParameter =
        {
            name = "substrate.wedgeAngle"
            getSys =
                fun (sys : OpticalSystem) (v : double) ->
                    match sys.substrate with
                    | Some (Wedge w) -> { sys with substrate = Some (Wedge { w with angle = WedgeAngle (Angle v) }) }
                    | _ -> sys
            lower = lower
            upper = upper
        }

    /// Dispersion-coefficient design variable (R-3): substitute the coefficient
    /// `v` into the supplied `OpticalPropertiesWithDisp` (`Dispersion.fs:53`) via
    /// the explicit `setCoeff` closure, then resolve the target film's properties
    /// at `waveLength` through `OpticalPropertiesWithDisp.getProperties`. No
    /// reflection — `setCoeff` rebuilds the WithDisp record from the new value.
    let dispersionCoefficient
        (name : string)
        (index : int)
        (model : OpticalPropertiesWithDisp)
        (setCoeff : double -> OpticalPropertiesWithDisp -> OpticalPropertiesWithDisp)
        (waveLength : WaveLength)
        (lower : float, upper : float) : DesignParameter =
        {
            name = name
            getSys =
                fun (sys : OpticalSystem) (v : double) ->
                    let resolved = (setCoeff v model).getProperties waveLength
                    let films =
                        sys.films
                        |> List.mapi (fun i (l : Layer) ->
                            if i = index then { l with properties = resolved } else l)
                    { sys with films = films }
            lower = lower
            upper = upper
        }

    /// Fold an ordered `DesignParameter list` and a flat `float[]` solution
    /// vector left over a base `OpticalSystem`, applying each parameter in
    /// vector order (R-3). `vector` MUST be at least as long as `parameters`.
    let applyVector (parameters : DesignParameter list) (vector : float[]) (baseSystem : OpticalSystem) : OpticalSystem =
        parameters
        |> List.indexed
        |> List.fold (fun (sys : OpticalSystem) (i, p : DesignParameter) -> p.getSys sys vector.[i]) baseSystem

    /// Assemble the G.1 `ParameterBounds` from the per-parameter SI bound pairs,
    /// in vector order — so the negative-thickness / n-range bounds (010 §II.5)
    /// are expressed as box bounds, not a separate validator.
    let bounds (parameters : DesignParameter list) : ParameterBounds =
        {
            lower = parameters |> List.map (fun p -> p.lower) |> Array.ofList
            upper = parameters |> List.map (fun p -> p.upper) |> Array.ofList
        }
