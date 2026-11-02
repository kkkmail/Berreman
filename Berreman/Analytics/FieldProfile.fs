namespace Analytics

open Berreman.Constants
open Berreman.Fields
open Berreman.Media
open Berreman.BerremanMatrix
open Berreman.FieldFunctions
open Berreman.Solvers
open Analytics.AnalysisFunctions

/// In-layer field / EFI depth profile and per-layer energy breakdown (Part F §F.6, §F.7).
/// Both walks reuse the existing inter-element propagation seam `EmField.propagate`
/// (BerremanMatrix.fs:224) and the base solution from `BaseOpticalSystemSolver`
/// (Solvers.fs:115); neither re-solves the 4×4 system per depth sample. The depth axis is
/// stored in canonical SI (meters) — nm/µm display is the UI boundary's concern.
///
/// NOTE: the engine flags `EmField.propagate` as "Does not work properly yet"
/// (BerremanMatrix.fs:221). That limitation is carried forward verbatim — this module
/// wires the profile/absorptance through the seam and MUST NOT attempt to rewrite it, so
/// the |E|² and per-layer magnitudes are structural, not physically validated, here.
module FieldProfile =

    /// How many sub-steps `fieldDepthProfile` cuts each film into; below 1 counts as 1.
    type SamplesPerLayer =
        | SamplesPerLayer of int

        member this.value = let (SamplesPerLayer n) = this in n


    /// One sample of the in-layer field profile: its depth below the top of the first film, and |E|² there.
    [<Softellect.Sys.NumericCarrier("an element of the field depth profile (R3, a profile array): the depth and |E|² at every sub-step of the walk, read as raw numbers by the series adapter")>]
    type FieldDepthPoint =
        {
            depth : float<meter>
            electricFieldSquared : float
        }


    /// §F.6 — |E|² versus depth, sampled by stepping the transmitted base-solution
    /// `EmField` through each film. Each `Layer` is cut into `samplesPerLayer` sub-steps
    /// of its `Thickness`; the field is propagated one sub-step at a time and |E|² read
    /// from `EmField.e` at each cumulative depth. Returns
    /// `samplesPerLayer × (film count)` samples with monotonically non-decreasing depths.
    let fieldDepthProfile
        (system : OpticalSystem)
        (info : IncidentLightInfo)
        (samplesPerLayer : SamplesPerLayer)
        : FieldDepthPoint list =

        let entry = BaseOpticalSystemSolver(info, system.baseSystem).emSys.transmitted
        let n = max 1 samplesPerLayer.value

        let stepLayer (acc : FieldDepthPoint list, depth : float<meter>, field : EmField) (layer : Layer) =
            let subThickness =
                match layer.thickness with
                | Thickness h -> h / float n
                | Infinity -> 0.0<meter>

            let subLayer = { layer with thickness = Thickness subThickness }

            let rec loop i (acc, depth : float<meter>, field : EmField) =
                if i >= n then (acc, depth, field)
                else
                    let field = field.propagate subLayer
                    let depth = depth + subThickness
                    let (E e) = field.e
                    let eSq = e.norm * e.norm
                    loop (i + 1) ({ depth = depth; electricFieldSquared = eSq } :: acc, depth, field)

            loop 0 (acc, depth, field)

        let (samples, _, _) =
            system.films
            |> List.fold stepLayer ([], 0.0<meter>, entry)

        samples |> List.rev

    /// §F.7 — per-layer absorptance from the drop in normal Poynting flux across each
    /// film, using the same `EmField.propagate` walk as §F.6 and normalising the normal
    /// flux against the incident field via `EmField.intensity` (FieldFunctions.fs:69).
    let layerAbsorptance
        (system : OpticalSystem)
        (info : IncidentLightInfo)
        : (Layer * AbsorptanceValue) list =

        let emSys = BaseOpticalSystemSolver(info, system.baseSystem).emSys
        let incident = emSys.incident
        let entry = emSys.transmitted

        let stepLayer (acc : (Layer * AbsorptanceValue) list, field : EmField) (layer : Layer) =
            let (OpticalFunctionValue before) = field.intensity incident
            let field = field.propagate layer
            let (OpticalFunctionValue after) = field.intensity incident
            ((layer, AbsorptanceValue (before - after)) :: acc, field)

        let (results, _) =
            system.films
            |> List.fold stepLayer ([], entry)

        results |> List.rev

    /// §F.7 — total absorbed power. Anchored to the §F.1 system-level absorptance
    /// A = 1 − R − T (`Solution.absorptance`) so it equals the system-level A
    /// within solver tolerance and introduces NO second absorption definition divergent
    /// from §F.1 (the per-layer flux walk above is reported separately by
    /// `layerAbsorptance`, which honours the carried-forward `propagate` limitation).
    let totalAbsorbedPower
        (system : OpticalSystem)
        (info : IncidentLightInfo)
        : AbsorptanceValue =

        OpticalSystemSolver(info, system).solution.absorptance
