namespace OpticalConstructor.Domain

open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.Media
open Berreman.Solvers
open OpticalConstructor.Domain.BeamTree

/// Curved-element domain model (§C.1–C.10). This is the single admitted physics
/// extension (§A.5): a lens or non-flat mirror is decomposed into a finite set of
/// LOCAL flat interfaces, each seen by a local plane wave carrying its own
/// direction, and the EXISTING engine 4×4 solver is run at each local angle of
/// incidence. Nothing here forks an engine primitive — three engine seams are
/// reused unchanged: `OpticalSystemSolver` (`Solvers.fs:199`),
/// `IncidentLightInfo.rotateY` (`Fields.fs:411`), and — for inter-element
/// propagation — `EmField.propagate` (`BerremanMatrix.fs:224`, reused via
/// `BeamTree.childIncidentField`). The `BeamTree`/`BeamNode`/`BeamBranch`/
/// `ConstructorElement` types (Part A §A.4 / slice 002) are imported, never
/// redefined. All lengths are stored in canonical SI meters (`Constants.fs:6`);
/// nm/µm/mm entry is converted at the UI boundary only (Part A canonical-units rule).
module CurvedElements =

    /// The surface figure (§C.2). Exactly two cases for the committed [Core] scope
    /// (010 line 88): `Spherical` — curvature fully described by
    /// `CurvedSurface.radiusOfCurvature` — and `Aspheric` — a conic constant plus an
    /// even-asphere polynomial (coefficients in `double<meter>`). No toroidal /
    /// freeform / diffractive case is admitted; they are absent from 010.
    type SurfaceFigure =
        | Spherical
        | Aspheric of conic : double * evenPolynomial : double<meter> list

    /// The curved-surface geometry record (§C.1). One physical surface = one
    /// `CurvedSurface`; a biconvex/meniscus lens is two records (§C.4), so each
    /// surface is a single local-interface evaluation. No thickness or wavelength
    /// representation is introduced here — thickness lives on the coating's `Layer`s
    /// (`Media.fs:12`), wavelength on the source via `WaveLength.value` (`Fields.fs:284`).
    type CurvedSurface =
        {
            /// Signed radius of curvature in canonical meters (§C.4). The SINGLE sign
            /// convention, with NO per-element override flag and NO convention
            /// selector: a surface whose center of curvature lies on the
            /// incoming-light side (concave toward the source) is NEGATIVE; a convex
            /// surface is POSITIVE. A concave vs. convex mirror is distinguished
            /// solely by this sign. `sag` (§C.2) returns the correctly signed height
            /// under exactly this convention.
            radiusOfCurvature : double<meter>

            /// The clear half-aperture radius in canonical meters.
            semiAperture : double<meter>

            /// The surface figure (§C.2).
            figure : SurfaceFigure

            /// The coating as an ordinary engine `OpticalSystem` (`Media.fs:94`,
            /// §C.8). The lens/mirror substrate IS this system's `lower`/`substrate`;
            /// no curved-specific layer or coating type exists.
            coating : OpticalSystem
        }

    /// One sampled local flat interface across the aperture (§C.3). `localAoiShift`
    /// is the engine `Angle` (`Geometry`, the type behind `IncidenceAngle`,
    /// `Fields.fs:22`) so it feeds straight into `IncidentLightInfo.rotateY`
    /// (`Fields.fs:411`).
    type LocalZone =
        {
            /// Radial position of the zone in canonical meters, from axis (0) to
            /// `semiAperture`.
            radialPosition : double<meter>

            /// The angle between the local surface normal at this radius and the
            /// optical axis, derived from `sag` (§C.2). `Angle.zero` on the axis.
            localAoiShift : Angle

            /// The coating for this zone — by default the single shared
            /// `CurvedSurface.coating` instance (§C.8); `gradeCoating` (§C.9) may
            /// rescale a per-zone copy for depth-graded EUV mirrors.
            coating : OpticalSystem
        }

    /// Default depth-grading magnitude: ~5% multilayer-period change from aperture
    /// center to edge (010 line 92, §C.9, [Core]). A dimensionless fraction; the
    /// thickness it scales stays `double<meter>` — the 5% is never stored as a
    /// non-SI quantity, only applied to meter-valued `Thickness.Thickness` layers.
    let depthGradingFraction = 0.05

    /// Surface sag `z(r)` (§C.2/§C.4): the surface height at radial position `r` for
    /// the given `figure` and signed `radiusOfCurvature`. For `Spherical` this is the
    /// exact spherical sag; for `Aspheric` the conic-plus-even-polynomial sag. The
    /// result carries the sign convention of §C.4 directly (it inherits the sign of
    /// the curvature `1/R`): a convex surface (positive `R`) gives a positive sag, a
    /// concave surface (negative `R`) a negative sag. Pure function, no side effects.
    let sag (figure : SurfaceFigure) (radiusOfCurvature : double<meter>) (r : double<meter>) : double<meter> =
        let R = radiusOfCurvature

        // Exact conic sag z = c r^2 / (1 + sqrt(1 - (1+k) c^2 r^2)), c = 1/R. Reduces
        // to the exact spherical sag when k = 0, and carries the sign of c (= sign of R).
        let conicSag (k : double) : double<meter> =
            if R = 0.0<meter> then 0.0<meter>
            else
                let c = 1.0 / R                  // double<1/meter>
                let cr2 = c * c * r * r          // dimensionless
                let underRoot = 1.0 - (1.0 + k) * cr2
                let root = if underRoot <= 0.0 then 0.0 else sqrt underRoot
                (c * r * r) / (1.0 + root)

        match figure with
        | Spherical -> conicSag 0.0
        | Aspheric (conic, evenPolynomial) ->
            // Even asphere polynomial evaluated in the dimensionless ratio (r / R) so
            // each meter-valued coefficient contributes a meter-valued sag term.
            let rho = if R = 0.0<meter> then 0.0 else r / R
            let polyTerms =
                evenPolynomial
                |> List.mapi (fun i a -> a * (rho ** (2.0 * float (i + 1))))
            let poly = (0.0<meter>, polyTerms) ||> List.fold (+)
            conicSag conic + poly

    /// Local surface-normal tilt (the local AOI shift) at radial position `r`,
    /// derived from `sag` (§C.2): the arctangent of the local sag slope. `sag` is
    /// even in `r`, so the central difference is exactly zero on the axis and the
    /// axis zone is `Angle.zero`.
    let private localShiftAt (surface : CurvedSurface) (r : double<meter>) : Angle =
        if r = 0.0<meter> then Angle.zero
        else
            let h0 = (abs surface.semiAperture) * 1.0e-6
            let h = if h0 > 0.0<meter> then h0 else (abs r) * 1.0e-6
            let dz =
                sag surface.figure surface.radiusOfCurvature (r + h)
                - sag surface.figure surface.radiusOfCurvature (r - h)
            let slope = dz / (2.0 * h)          // dimensionless
            atan slope |> Angle.radian

    /// Decompose a curved surface into `zoneCount` local flat interfaces (§C.3),
    /// sampled UNIFORMLY in aperture radius from the axis (`radialPosition = 0`) to
    /// `semiAperture`. No adaptive refinement, no caching, no defaulted count — the
    /// count is a required argument owned by the orchestration caller. The axis zone
    /// (radius 0) has `localAoiShift = Angle.zero`. Every zone shares the single
    /// immutable `CurvedSurface.coating` instance by value (§C.8); grading (§C.9) is
    /// applied later and per zone, never here.
    let sampleZones (surface : CurvedSurface) (zoneCount : int) : LocalZone list =
        if zoneCount < 1 then
            invalidArg "zoneCount" "sampleZones zoneCount must be >= 1."

        [ for i in 0 .. zoneCount - 1 ->
            let r =
                if zoneCount = 1 then 0.0<meter>
                else surface.semiAperture * (float i / float (zoneCount - 1))
            {
                radialPosition = r
                localAoiShift = localShiftAt surface r
                coating = surface.coating
            } ]

    /// Evaluate one zone through the engine 4×4 solver at its local angle of
    /// incidence (§C.5). (1) Form the local incident light by applying the zone's
    /// `localAoiShift` to `info` via `IncidentLightInfo.rotateY` (`Fields.fs:411`,
    /// which adds the angle onto `incidenceAngle`); (2) run the UNFORKED per-system
    /// solver `OpticalSystemSolver(localInfo, zone.coating, parameters)`
    /// (`Solvers.fs:199`). `parameters` (`Solvers.fs:15`) is passed through
    /// unchanged — no curved-specific reflection-count or threshold. The returned
    /// `Solution` carries the engine reflected/transmitted split (`Solvers.fs:156`),
    /// consumed downstream exactly as a flat element's output is.
    let solveZone (info : IncidentLightInfo) (zone : LocalZone) (parameters : SolverParameters) : Solution =
        let localInfo = info.rotateY zone.localAoiShift
        OpticalSystemSolver(localInfo, zone.coating, parameters).solution

    /// Build the per-zone beam fan onto the Part A `BeamTree`, honoring the mirror
    /// special case (§C.6). Shapes ONE zone node's branch children by element type:
    /// a `Lens` zone node exposes BOTH `BeamBranch.Reflected` and
    /// `BeamBranch.Transmitted`; a `CurvedMirror` (like any mirror) exposes
    /// `BeamBranch.Reflected` ONLY and drops the transmitted field (010 lines 78-80).
    /// The aperture fan — one node per `LocalZone` (§C.5) — is realized by the
    /// orchestration caller mapping this over the per-zone nodes, since
    /// `BeamNode.children` is a `Map<BeamBranch, BeamNode>` (≤ two entries) and so a
    /// single parent cannot itself hold the N-zone fan. `BeamTree` / `BeamNode` /
    /// `BeamBranch` / `ConstructorElement` are imported, never redefined.
    let attachCurvedElement (element : ConstructorElement) (node : BeamNode) : BeamNode =
        // The element identity (Lens vs CurvedMirror) drives the fan shape.
        let zoneNode = { node with element = element; children = Map.empty }

        // A child node capturing the outgoing beam on a branch. Topology only — the
        // field state is carried by the engine solve (`solveZone`), not the node.
        let outgoing = { zoneNode with element = Detector; children = Map.empty }

        // Reuse the slice-002 smart constructor `BeamNode.attach`, which itself
        // enforces the mirror reflected-only rule; a rejected Transmitted attach on a
        // mirror is the deliberate drop.
        let attach (branch : BeamBranch) (parent : BeamNode) : BeamNode =
            match BeamNode.attach branch outgoing parent with
            | Ok p -> p
            | Error _ -> parent

        match element with
        | Lens ->
            zoneNode |> attach BeamBranch.Reflected |> attach BeamBranch.Transmitted
        | CurvedMirror | FlatMirror ->
            zoneNode |> attach BeamBranch.Reflected
        | Source | Polarizer | Sample _ | Analyzer | Detector ->
            // Not a curved element; §C.6 fans only Lens/CurvedMirror. No fan added.
            zoneNode

    /// Apply EUV depth-graded multilayer spacing as a per-zone thickness rescale
    /// (§C.9, [Core]). Returns the zone's coating with every finite
    /// `Thickness.Thickness d` (`Media.fs:12`) of the bilayer `films` stack scaled by
    /// a factor that varies with the zone's local AOI — so the multilayer period
    /// changes across the aperture (~5% center-to-edge default). The factor is
    /// computed from `localAoiShift`, normalized by the edge shift at `semiAperture`
    /// so the axis zone is exactly 1.0 (unchanged) and the edge zone hits the ~5%
    /// default. `Thickness.Infinity` is left untouched; all scaled thicknesses remain
    /// `double<meter>`. No new graded-layer type, period table, or schema versioning.
    /// A non-graded element reuses the single shared coating (§C.8) with no rescale —
    /// it simply does not call this function.
    let gradeCoating (surface : CurvedSurface) (zone : LocalZone) : OpticalSystem =
        let edgeShift = (localShiftAt surface surface.semiAperture).value
        let factor =
            if abs edgeShift < 1.0e-12 then 1.0
            else 1.0 + depthGradingFraction * (zone.localAoiShift.value / edgeShift)

        let scale (t : Thickness) : Thickness =
            match t with
            | Thickness.Thickness d -> Thickness.Thickness (d * factor)
            | Thickness.Infinity -> Thickness.Infinity

        let gradedFilms =
            zone.coating.films
            |> List.map (fun layer -> { layer with thickness = scale layer.thickness })

        { zone.coating with films = gradedFilms }
