namespace OpticalConstructor.Optimization

open Berreman.Constants
open Berreman.Geometry
open Berreman.Media
open Berreman.MaterialProperties
open Berreman.Dispersion
open Berreman.Fields
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization.OptimizationInterface

/// §G.3 — the flat design-vector ⇄ `OpticalSystem` design-variable mapping
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

    /// One design parameter's value, in its canonical unit (§0 constraint 3): meters for a thickness,
    /// radians for an angle, the coefficient's own unit for a dispersion coefficient.
    type DesignValue =
        | DesignValue of float

        member this.value = let (DesignValue v) = this in v

    /// The position of a film in `OpticalSystem.films`, counted from 0.
    type FilmIndex =
        | FilmIndex of int

        member this.value = let (FilmIndex i) = this in i

    type Thickness with

        /// The thickness in meters, the design value of a layer-thickness parameter; `infinity` for an
        /// infinite thickness (an unbounded side, §G.1).
        member this.designValue : DesignValue =
            match this with
            | Thickness t -> DesignValue (t / 1.0<meter>)
            | Infinity -> DesignValue infinity

    /// One scalar design variable: a name, a `getSys` closure with the shape of
    /// `ArbitraryVariable.getSys`, and the per-parameter SI bound pair
    /// (`lower`/`upper`, canonical units, §0 constraint 3).
    type DesignParameter =
        {
            name : ParameterName
            getSys : OpticalSystem -> DesignValue -> OpticalSystem
            lower : DesignValue
            upper : DesignValue
        }

    /// Layer-thickness design variable: rebuild film `index`'s `Thickness` from
    /// `v` (already in canonical meters, §0 constraint 3) as
    /// `Thickness (v * 1.0<meter>)` (`Media.fs:12`). The lower bound is fixed at
    /// 0 — that bound IS the negative-thickness prohibition (R-3), not a validator.
    let layerThickness (index : FilmIndex) (upper : Thickness) : DesignParameter =
        let (FilmIndex filmIndex) = index
        {
            name = ParameterName $"film[%d{filmIndex}].thickness"
            getSys =
                fun (sys : OpticalSystem) (DesignValue v) ->
                    let films =
                        sys.films
                        |> List.mapi (fun i (l : Layer) ->
                            if i = filmIndex then { l with thickness = Thickness (v * 1.0<meter>) } else l)
                    { sys with films = films }
            lower = DesignValue 0.0
            upper = upper.designValue
        }

    /// Wedge-angle design variable: set the substrate `WedgeLayer.angle`
    /// (`Media.fs:34`) to `v` radians. A non-wedge substrate is left untouched.
    let wedgeAngle (lower : DesignValue, upper : DesignValue) : DesignParameter =
        {
            name = ParameterName "substrate.wedgeAngle"
            getSys =
                fun (sys : OpticalSystem) (DesignValue v) ->
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
        (name : ParameterName)
        (index : FilmIndex)
        (model : OpticalPropertiesWithDisp)
        (setCoeff : DesignValue -> OpticalPropertiesWithDisp -> OpticalPropertiesWithDisp)
        (waveLength : WaveLength)
        (lower : DesignValue, upper : DesignValue) : DesignParameter =
        let (FilmIndex filmIndex) = index
        {
            name = name
            getSys =
                fun (sys : OpticalSystem) (v : DesignValue) ->
                    let resolved = (setCoeff v model).getProperties waveLength
                    let films =
                        sys.films
                        |> List.mapi (fun i (l : Layer) ->
                            if i = filmIndex then { l with properties = resolved } else l)
                    { sys with films = films }
            lower = lower
            upper = upper
        }

    /// Fold an ordered `DesignParameter list` and a design vector left over a base `OpticalSystem`,
    /// applying each parameter in vector order (R-3). `vector` MUST be at least as long as `parameters`.
    let applyVector (parameters : DesignParameter list) (vector : DesignVector) (baseSystem : OpticalSystem) : OpticalSystem =
        let (DesignVector values) = vector
        parameters
        |> List.indexed
        |> List.fold (fun (sys : OpticalSystem) (i, p : DesignParameter) -> p.getSys sys (DesignValue values.[i])) baseSystem

    /// Assemble the G.1 `ParameterBounds` from the per-parameter SI bound pairs,
    /// in vector order — so the negative-thickness / n-range bounds (010 §II.5)
    /// are expressed as box bounds, not a separate validator.
    let bounds (parameters : DesignParameter list) : ParameterBounds =
        let side (pick : DesignParameter -> DesignValue) : DesignVector =
            parameters |> List.map (fun p -> let (DesignValue v) = pick p in v) |> Array.ofList |> DesignVector
        {
            lower = side (fun p -> p.lower)
            upper = side (fun p -> p.upper)
        }
