namespace Analytics

open Berreman.Constants
open Berreman.Fields
open Berreman.Media
open Berreman.BerremanMatrix
open Berreman.FieldFunctions
open Berreman.Solvers

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

    /// §F.6 — |E|² versus depth, sampled by stepping the transmitted base-solution
    /// `EmField` through each film. Each `Layer` is cut into `samplesPerLayer` sub-steps
    /// of its `Thickness`; the field is propagated one sub-step at a time and |E|² read
    /// from `EmField.e` at each cumulative depth. Returns
    /// `samplesPerLayer × (film count)` samples with monotonically non-decreasing depths.
    let fieldDepthProfile
        (system : OpticalSystem)
        (info : IncidentLightInfo)
        (samplesPerLayer : int)
        : (double<meter> * float) list =

        let entry = BaseOpticalSystemSolver(info, system.baseSystem).emSys.transmitted
        let n = max 1 samplesPerLayer

        let stepLayer (acc : (double<meter> * float) list, depth : double<meter>, field : EmField) (layer : Layer) =
            let subThickness =
                match layer.thickness with
                | Thickness h -> h / float n
                | Infinity -> 0.0<meter>

            let subLayer = { layer with thickness = Thickness subThickness }

            let rec loop i (acc, depth : double<meter>, field : EmField) =
                if i >= n then (acc, depth, field)
                else
                    let field = field.propagate subLayer
                    let depth = depth + subThickness
                    let (E e) = field.e
                    let eSq = e.norm * e.norm
                    loop (i + 1) ((depth, eSq) :: acc, depth, field)

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
        : (Layer * float) list =

        let emSys = BaseOpticalSystemSolver(info, system.baseSystem).emSys
        let incident = emSys.incident
        let entry = emSys.transmitted

        let stepLayer (acc : (Layer * float) list, field : EmField) (layer : Layer) =
            let before = field.intensity incident
            let field = field.propagate layer
            let after = field.intensity incident
            ((layer, before - after) :: acc, field)

        let (results, _) =
            system.films
            |> List.fold stepLayer ([], entry)

        results |> List.rev

    /// §F.7 — total absorbed power. Anchored to the §F.1 system-level absorptance
    /// A = 1 − R − T (`AnalysisFunctions.absorptance`) so it equals the system-level A
    /// within solver tolerance and introduces NO second absorption definition divergent
    /// from §F.1 (the per-layer flux walk above is reported separately by
    /// `layerAbsorptance`, which honours the carried-forward `propagate` limitation).
    let totalAbsorbedPower
        (system : OpticalSystem)
        (info : IncidentLightInfo)
        : float =

        OpticalSystemSolver(info, system).solution
        |> AnalysisFunctions.absorptance
