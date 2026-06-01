namespace OpticalConstructor.Domain

open Berreman.Constants
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.BerremanMatrix
open Berreman.Solvers

/// The beam-tree orchestration layer (§A.4 / Part B-domain). This is the NET-NEW
/// topology that sits ABOVE the single-system 4×4 solver and routes its
/// reflected/transmitted outputs along each branch. It is expressed as F#
/// records and discriminated unions (Part A constraint 1), is immutable
/// throughout, and embeds NO solver internals — the existing
/// `OpticalSystemSolver` / `BaseOpticalSystemSolver` are CALLED, not re-typed,
/// and inter-element propagation reuses `EmField.propagate` (`BerremanMatrix.fs:224`).
module BeamTree =

    /// The two attachment points of a non-mirror element (§A.4 / B.2). A simple
    /// comparable DU so it can key a child-attachment `Map`.
    type BeamBranch =
        | Reflected
        | Transmitted

    /// NET-NEW validation error DU (§B.3 / R-11). Mirror nodes carry the
    /// reflected branch ONLY; attaching on `Transmitted` is rejected with a
    /// `Result.Error` of this type — never an exception.
    type BeamTreeError =
        | MirrorBranchMustBeReflected

    /// A placed element (§A.4). The mirror cases (`FlatMirror`, and — owned by
    /// Part C — `CurvedMirror`) are the deliberate special case: they carry the
    /// reflected branch only, by design ignoring transmitted light. `Sample`
    /// carries the engine `OpticalSystem` (`Media.fs:94`) it represents.
    type ConstructorElement =
        | Source
        | Polarizer
        | Sample of OpticalSystem
        | Lens
        | CurvedMirror
        | FlatMirror
        | Analyzer
        | Detector

    /// A node of the beam tree. Carries one `ConstructorElement`; its
    /// `OpticalSystem` (the stack solved here, `Media.fs:94`); its solver inputs
    /// as the engine's own local-beam-state `IncidentLightInfo` (`Fields.fs:338`
    /// — direction, energy, polarization, ellipticity); its child attachments
    /// keyed by `BeamBranch`; and a per-element default-unit hook (B.8 — display
    /// metadata only, it MUST NOT change any stored SI value). A non-mirror node
    /// may hold a `Reflected` child, a `Transmitted` child, both, or neither; a
    /// mirror node holds at most the `Reflected` key (enforced by `attach`).
    type BeamNode =
        {
            element : ConstructorElement
            system : OpticalSystem
            incident : IncidentLightInfo
            children : Map<BeamBranch, BeamNode>
            defaultUnit : Units.UnitOfMeasure
        }

        /// True when the node's element is a mirror — the reflected-only special
        /// case of §B.3.
        member node.isMirror =
            match node.element with
            | FlatMirror | CurvedMirror -> true
            | Source | Polarizer | Sample _ | Lens | Analyzer | Detector -> false

        /// Smart constructor (§B.3 / R-11). Attaches `child` on `branch` of
        /// `parent`, returning a `Result.Error MirrorBranchMustBeReflected` when
        /// the parent is a mirror and the branch is `Transmitted` (the child is
        /// NOT added); a `Reflected` attach on a mirror, and either branch on a
        /// non-mirror, succeed. Immutable: a new parent node is returned.
        static member attach (branch : BeamBranch) (child : BeamNode) (parent : BeamNode) : Result<BeamNode, BeamTreeError> =
            match parent.isMirror, branch with
            | true, Transmitted -> Error MirrorBranchMustBeReflected
            | _ -> Ok { parent with children = parent.children |> Map.add branch child }

    /// The beam tree orchestration aggregate: a tree rooted at a single
    /// `BeamNode`. This is the carrier the project aggregate (§A.7) holds.
    type BeamTree =
        {
            root : BeamNode
        }

    /// Evaluate a node by the EXISTING engine solver (§A.4 / AC-A3): build the
    /// `OpticalSystemSolver` (`Solvers.fs:199`) over the node's incident light
    /// and stack and read its `Solution.emSys` (`Solvers.fs:156`) — the
    /// `EmFieldSystem` whose `reflected`/`transmitted` (`Fields.fs:553-554`) are
    /// the two outgoing beams. No parallel solver is invoked.
    let solve (node : BeamNode) : EmFieldSystem =
        OpticalSystemSolver(node.incident, node.system).solution.emSys

    /// The outgoing beam on `branch` of a solved node: `reflected` for
    /// `Reflected`, `transmitted` for `Transmitted` (`Fields.fs:552-554`). Each
    /// branch carries its own field state, so detectors on different branches
    /// report independent results.
    let branchEmField (branch : BeamBranch) (ems : EmFieldSystem) : EmField =
        match branch with
        | Reflected -> ems.reflected
        | Transmitted -> ems.transmitted

    /// The incident field driving a child attached on `branch` (§B.4 / AC-B4):
    /// the parent's branch field advanced across the intervening element/gap
    /// `gap` through the reused `EmField.propagate` seam (`BerremanMatrix.fs:224`).
    /// No plane-wave propagation is re-implemented.
    let childIncidentField (branch : BeamBranch) (gap : Layer) (parentEms : EmFieldSystem) : EmField =
        (branchEmField branch parentEms).propagate gap

    /// Route the parent's branch-`b` outgoing beam to the downstream node and
    /// solve it (§B.4 / AC-B4): take the propagated child-incident `EmField` and
    /// solve it against the child's `ShortOpticalSystem` (`Media.fs:56`) through
    /// the engine's `EmField`-driven overload `BaseOpticalSystemSolver(emf, system)`
    /// (`Solvers.fs:149`). No separate Jones/Stokes accumulator is introduced —
    /// the energy/polarization carried along the branch is the field state inside
    /// the propagated `EmField` (its `emComponents`, `Fields.fs:427`).
    let routeAndSolve (branch : BeamBranch) (gap : Layer) (parentEms : EmFieldSystem) (childSystem : ShortOpticalSystem) : EmFieldSystem =
        BaseOpticalSystemSolver(childIncidentField branch gap parentEms, childSystem).emSys

    /// A continuously-varying-index layer (§B.11 / R-14). `indexProfile` maps a
    /// depth (in canonical meters) to the engine tensor model `OpticalProperties`
    /// (`MaterialProperties.fs:165`). This is NOT an engine type and is never
    /// stored in the canonical JSON — only the expanded `films` persist.
    type GradientLayer =
        {
            totalThickness : float<meter>
            subLayerCount : int
            indexProfile : float<meter> -> OpticalProperties
        }

    /// Auto-discretize a gradient-index layer into a flat list of ordinary,
    /// reused `Layer` records (§B.11 / AC-B11): slice `totalThickness` into
    /// exactly `subLayerCount` equal-`Thickness` sub-layers (`Media.fs:12,24`),
    /// each sampling `indexProfile` at its mid-depth. The result is
    /// indistinguishable from manually entered layers and assignable straight
    /// into `OpticalSystem.films` (`Media.fs:98`) — no gradient-specific engine
    /// type, no solve-time discretization. Uniform slicing only; the count is
    /// caller-supplied and validated `>= 1`.
    let discretize (g : GradientLayer) : Layer list =
        if g.subLayerCount < 1 then
            invalidArg "subLayerCount" "GradientLayer.subLayerCount must be >= 1."

        let n = g.subLayerCount
        let dz = g.totalThickness / float n

        [ for i in 0 .. n - 1 ->
            let midDepth = (float i + 0.5) * dz
            {
                properties = g.indexProfile midDepth
                thickness = Thickness.Thickness dz
            } ]
