namespace OpticalConstructor.Domain

open Berreman.Constants
open OpticalConstructor.Domain.Placement

/// Spec 0046 — the Main-scene BEAM TREE: which beam each element sits on, where it lands on that beam,
/// the drawn ray segments, and the path the light took to reach every element. Pure and Avalonia-free,
/// so every rule is provable headless; the Main workbench only adapts its live model to it.
///
/// The light source is the ray ROOT. The MAIN beam leaves the source and follows each element's primary
/// branch (`RayModel.primaryBranch` — a mirror reflects it, everything else transmits it). A polarizer,
/// lens or sample that emits BOTH branches also opens a REFLECTED side beam off its front face; an
/// element hosted on that side beam snaps onto the reflected ray, recursively. On every beam the
/// elements are ordered by their along-beam coordinate (the placement point's x) and the x differences
/// are the along-beam gaps — the main-beam rule since spec 0027, now shared by the side beams. Every
/// element is crossed by the ONE `RayModel.crossElement` law: the ray meets the drawn front face, bends
/// inside, and leaves parallel; the reflected ray leaves the entry point.
module BeamLayout =

    /// Which beam of the scene an element sits on. Every element starts on the MAIN beam; an element
    /// moved onto the reflected ray of a partially-reflecting element is `OnReflectedBeamOf` that
    /// element. Referenced by the serializable element id, never by an index, so it survives save-load.
    type BeamHost =
        | OnMainBeam
        | OnReflectedBeamOf of Library.ElementId

    /// Whether an element opens a reflected SIDE beam that other elements can sit on: a sample emitting
    /// BOTH branches does (its transmitted branch continues the beam it sits on); a mirror's reflected
    /// branch IS the beam it continues; a polarizer and a lens only transmit (spec 0046), and a source /
    /// detector reflects nothing.
    type SideBeam =
        | OpensReflectedBeam
        | NoSideBeam

    let sideBeamOf (p : ElementPlacement) : SideBeam =
        match p.catalogueKind with
        | Sample ->
            match p.emission with
            | EmitBoth -> OpensReflectedBeam
            | EmitReflectedOnly | EmitTransmittedOnly -> NoSideBeam
        | LightSource | LinearPolarizer | CircularPolarizer | FlatMirror | CurvedMirror | Lens | Detector -> NoSideBeam

    /// Whether the element menu may switch an element's reflected / transmitted rays, and why not when
    /// it may not. `supported` is what the element's binding allows — a bound sample's geometry-constrained
    /// `supportedEmission`, `EmitBoth` for anything else.
    type RayControl =
        | RaysSwitchable
        | RaysFixed of reason : string

    let rayControlOf (supported : Emission) (p : ElementPlacement) : RayControl =
        match p.catalogueKind with
        | LightSource -> RaysFixed "a light source emits along its own aim"
        | Detector -> RaysFixed "a detector terminates the beam it receives"
        | FlatMirror | CurvedMirror -> RaysFixed "a full mirror always reflects"
        | Lens -> RaysFixed "a lens always transmits"
        | LinearPolarizer | CircularPolarizer -> RaysFixed "a polarizer always transmits"
        | Sample ->
            match supported with
            | EmitBoth -> RaysSwitchable
            | EmitReflectedOnly -> RaysFixed "the bound sample is observable in reflection only"
            | EmitTransmittedOnly -> RaysFixed "the bound sample is observable in transmission only"

    /// Switch an element's reflected or transmitted ray on / off through the `Emission` smart setters,
    /// so the both-off state stays unrepresentable (turning the last ray off turns the other back on).
    /// Inert when `rayControlOf` fixes the element's rays.
    let toggleRay (branch : BeamTree.BeamBranch) (supported : Emission) (p : ElementPlacement) : ElementPlacement =
        match rayControlOf supported p with
        | RaysFixed _ -> p
        | RaysSwitchable ->
            match branch with
            | BeamTree.Reflected -> { p with emission = Emission.withReflected (not p.emission.emitsReflected) p.emission }
            | BeamTree.Transmitted -> { p with emission = Emission.withTransmitted (not p.emission.emitsTransmitted) p.emission }

    /// The longest an OPEN ray is drawn — a reflected ray with nothing on it yet, or a beam with no detector
    /// at its end — so the ray is visible to attach to. It stops sooner at the first element it meets.
    let openBeamLength : float<meter> = 0.5<meter>

    /// One scene element as the layout reads it. The FIRST element of the list handed to `layout` is the
    /// ray root (the Main scene seeds the light source there).
    type LayoutElement =
        {
            id : Library.ElementId
            placement : ElementPlacement
            host : BeamHost
            /// The element's draw zoom: it is DRAWN as its bounding box times this, so the ray meets the
            /// drawn faces and an open ray stops at the drawn box.
            zoom : float
        }

    /// The drawn half-depth of an element along its face normal — where the ray meets its faces.
    let halfDepthOf (e : LayoutElement) : float<meter> = e.zoom * e.placement.box.b / 2.0

    /// One step of the light's path: an element and the branch the light left it by.
    type PathStep =
        {
            element : Library.ElementId
            branch : BeamTree.BeamBranch
        }

    /// Whether light reaches an element (spec 0046): a detector TERMINATES its beam, so an element further
    /// along that beam — and everything on its reflected rays — is laid out but receives no light.
    type Illumination =
        | Lit
        | Unlit

    /// One element as laid out: its drawn centre, the beam direction arriving at it (`None` for the
    /// root), the beam it actually sits on (its host, or the main beam when that host's reflected ray
    /// does not exist), the path the light took from the root to reach it, and whether light reaches it.
    type PlacedElement =
        {
            id : Library.ElementId
            centre : Vector3
            incoming : Vector3 option
            beam : BeamHost
            path : PathStep list
            light : Illumination
        }

    /// How a lit ray ends: on a detector, or open (nothing terminates it — free for a detector).
    type RayEnd =
        | EndsAtDetector
        | EndsOpen

    /// One ray that carries light — the main beam, or the reflected ray of a partially-reflecting element
    /// light reaches — and how it ends.
    type LitRay =
        {
            host : BeamHost
            ends : RayEnd
        }

    /// Which point an element rotates about on its beam (spec 0046, a global option). About its CENTRE
    /// (the default), the incident ray meets its front face further from the face's middle the more the
    /// element is tilted. About the middle of its INCIDENT face, the incident ray always meets the element
    /// at that same point — essential for stacks such as EUV mirrors — and the transmitted ray, bent by
    /// Snell's law inside, leaves the back face off its middle whenever the incidence is not normal.
    type RotationPivot =
        | PivotAtCentre
        | PivotAtIncidentFace

    /// Where a drawn ray segment runs: through free space, or inside an element between its faces.
    type SegmentKind =
        | InFreeSpace
        | InsideElement

    /// One straight drawn piece of a ray, in canonical-meter table-frame coordinates.
    type RaySegment =
        {
            start : Vector3
            finish : Vector3
            kind : SegmentKind
        }

    /// The laid-out scene: every element in INPUT order, every drawn ray segment, and every lit ray in
    /// walk order (the main beam first, then each reflected ray in the order the light reaches its host).
    type SceneLayout =
        {
            elements : PlacedElement list
            segments : RaySegment list
            rays : LitRay list
        }

    let private add (a : Vector3) (b : Vector3) : Vector3 = Vector3.create (a.x + b.x) (a.y + b.y) (a.z + b.z)
    let private sub (a : Vector3) (b : Vector3) : Vector3 = Vector3.create (a.x - b.x) (a.y - b.y) (a.z - b.z)
    let private scale (s : float) (a : Vector3) : Vector3 = Vector3.create (s * a.x) (s * a.y) (s * a.z)
    let private along (x : ElementPlacement) : float = x.placementPoint.x / 1.0<meter>

    /// The point of the line through `linePoint` along `dir` that is abreast of `point` — where an
    /// outgoing beam is measured from, so the along-beam gap after an element counts from beside its
    /// centre even when the beam leaves laterally shifted (transmitted) or off its front face (reflected).
    let private abreast (point : Vector3) (linePoint : Vector3) (dir : Vector3) : Vector3 =
        let d = dir.normalized
        add linePoint (scale ((sub point linePoint).dot d) d)

    /// The beam each element actually sits on. A host is honoured when it names a non-root element that
    /// opens a reflected side beam and the hosting does not loop back; otherwise the element falls back to
    /// the main beam (its host removed, its host's reflected ray switched off, or a cycle).
    let effectiveHosts (elements : LayoutElement list) : Map<Library.ElementId, BeamHost> =
        match elements with
        | [] -> Map.empty
        | root :: rest ->
            let byId =
                rest
                |> List.fold (fun (m : Map<Library.ElementId, LayoutElement>) e -> if Map.containsKey e.id m then m else Map.add e.id e m) Map.empty
            // Memoized, so every element resolves ONCE and each chain of hosts ends on the main beam: in a
            // loop (A on B, B on A) the element reached second falls back to the main beam, and the first
            // then sits on its reflected ray — never both stranded on each other.
            let rec resolve
                (memo : Map<Library.ElementId, BeamHost>) (visiting : Set<Library.ElementId>) (e : LayoutElement)
                : BeamHost * Map<Library.ElementId, BeamHost> =
                match Map.tryFind e.id memo with
                | Some host -> host, memo
                | None ->
                    let visiting' = Set.add e.id visiting
                    let host, memo' =
                        match e.host with
                        | OnMainBeam -> OnMainBeam, memo
                        | OnReflectedBeamOf parentId ->
                            match Map.tryFind parentId byId with
                            | Some parent when parentId <> root.id && not (Set.contains parentId visiting') ->
                                match sideBeamOf parent.placement with
                                | OpensReflectedBeam ->
                                    let _, resolvedMemo = resolve memo visiting' parent
                                    OnReflectedBeamOf parentId, resolvedMemo
                                | NoSideBeam -> OnMainBeam, memo
                            | Some _ | None -> OnMainBeam, memo
                    host, Map.add e.id host memo'
            rest |> List.fold (fun (memo : Map<Library.ElementId, BeamHost>) e -> snd (resolve memo Set.empty e)) Map.empty

    /// A drawn piece of ray before the open ones are clipped: a closed run (face to face, or inside an
    /// element), or an OPEN ray leaving element `emitter` at `origin` along `direction`, whose length is
    /// decided once every element's drawn box is known.
    type private PendingSegment =
        | ClosedRun of RaySegment
        | OpenRay of origin : Vector3 * direction : Vector3 * emitter : int

    /// An element's DRAWN box: its centre and its oriented axes (N1 = depth, N2 / N3 = the face) each with
    /// the half extent along it — exactly the box the renderer draws.
    type private DrawnBox =
        {
            centre : Vector3
            axes : (Vector3 * float) list
        }

    /// How far along `direction` from `origin` the ray first ENTERS the box (the slab test in the box's
    /// own frame), or `None` when it misses, the box is behind, or the origin is already inside it.
    let private entryDistance (origin : Vector3) (direction : Vector3) (box : DrawnBox) : float option =
        let o = sub origin box.centre
        let d = direction.normalized
        let narrowed =
            box.axes
            |> List.fold (fun (span : (float * float) option) (axis, half) ->
                match span with
                | None -> None
                | Some (tNear, tFar) ->
                    let oc = o.dot axis
                    let dc = d.dot axis
                    if abs dc < 1.0e-12 then
                        if abs oc > half then None else Some (tNear, tFar)
                    else
                        let t1 = (-half - oc) / dc
                        let t2 = (half - oc) / dc
                        let tNear' = max tNear (min t1 t2)
                        let tFar' = min tFar (max t1 t2)
                        if tNear' > tFar' then None else Some (tNear', tFar')) (Some (System.Double.NegativeInfinity, System.Double.PositiveInfinity))
        match narrowed with
        | Some (tNear, _) when tNear > 1.0e-9 -> Some tNear
        | Some _ | None -> None

    /// The walk's accumulated output: the placed elements by input index, the pending segments (newest
    /// first) and the lit rays (newest first).
    type private WalkState =
        {
            placed : Map<int, PlacedElement>
            segments : PendingSegment list
            rays : LitRay list
        }

    /// Lay the scene out (spec 0046) with every element rotating about `pivot`. Walks the main beam from
    /// the root, crossing every element by `RayModel.crossElementThrough` at its DRAWN orientation
    /// (`RayModel.orientedToBeam`) and depth, recursing into each reflected side beam, and emits the drawn
    /// segments: free-space runs face to face, the refracted run INSIDE each transmitting element, and —
    /// past the last element of a beam no detector terminates, or along a reflected ray with nothing on
    /// it — an OPEN run, which stops at the first drawn element box it meets (so a back-reflection ends on
    /// the element it travels back to and never runs behind the source) and is at most `openBeamLength`.
    /// The beam point an element is snapped to is its CENTRE (`PivotAtCentre`) or the middle of its
    /// INCIDENT face (`PivotAtIncidentFace` — the incident ray then always enters there). A detector
    /// TERMINATES its beam: elements further along it are laid out on the straight continuation (so they
    /// do not jump) but are `Unlit`, and no ray is drawn to them or along their reflected rays.
    let layoutWith (pivot : RotationPivot) (elements : LayoutElement list) : SceneLayout =
        match elements with
        | [] -> { elements = []; segments = []; rays = [] }
        | root :: rest ->
            let hosts = effectiveHosts elements
            let hostOf (e : LayoutElement) : BeamHost = Map.tryFind e.id hosts |> Option.defaultValue OnMainBeam
            let indexed = rest |> List.mapi (fun i e -> i + 1, e)
            let membersOf (host : BeamHost) : (int * LayoutElement) list =
                indexed |> List.filter (fun (_, e) -> hostOf e = host) |> List.sortBy (fun (_, e) -> along e.placement)
            let litRay (host : BeamHost) (members : (int * LayoutElement) list) : LitRay =
                let ends =
                    match members |> List.exists (fun (_, e) -> e.placement.catalogueKind = Detector) with
                    | true -> EndsAtDetector
                    | false -> EndsOpen
                { host = host; ends = ends }

            /// Walk one beam. `draw` is where the drawn ray currently is, `reference` the point the next gap
            /// is measured from, `dir` the beam direction, `prevX` the along-beam coordinate and `emitter`
            /// the index of the element the beam left, `path` the steps that led onto this beam, `light`
            /// whether light still travels along it.
            let rec walk
                (draw : Vector3) (reference : Vector3) (dir : Vector3) (prevX : float) (emitter : int) (path : PathStep list) (light : Illumination)
                (members : (int * LayoutElement) list)
                (state : WalkState)
                : WalkState =
                // Segments are drawn only while light travels this beam.
                let drawn (pieces : PendingSegment list) : PendingSegment list =
                    match light with
                    | Lit -> pieces
                    | Unlit -> []
                match members with
                | [] -> { state with segments = drawn [ OpenRay (draw, dir, emitter) ] @ state.segments }
                | (index, e) :: later ->
                    let x = along e.placement
                    let beamPoint = add reference (scale (max 0.0 (x - prevX)) dir)
                    let normal = r1Axis (RayModel.orientedToBeam dir e.placement)
                    let halfDepth = halfDepthOf e
                    let centre, crossing =
                        match pivot with
                        | PivotAtCentre -> beamPoint, RayModel.crossElement beamPoint dir normal halfDepth
                        | PivotAtIncidentFace ->
                            let c = RayModel.centreBehindIncidentFace beamPoint dir normal halfDepth
                            c, RayModel.crossElementThrough beamPoint c dir normal halfDepth
                    let approach = drawn [ ClosedRun { start = draw; finish = crossing.entry; kind = InFreeSpace } ]
                    let here = { id = e.id; centre = centre; incoming = Some dir; beam = hostOf e; path = path; light = light }
                    let state' = { state with placed = Map.add index here state.placed; segments = approach @ state.segments }
                    match e.placement.catalogueKind with
                    | Detector ->
                        // A detector terminates its beam: anything further along it is laid out on the straight
                        // continuation but receives no light.
                        walk crossing.exit (abreast beamPoint crossing.exit crossing.transmitted) crossing.transmitted x index path Unlit later state'
                    | LightSource | LinearPolarizer | CircularPolarizer | Sample | Lens | FlatMirror | CurvedMirror ->
                        let branch = RayModel.primaryBranch e.placement
                        let onward = path @ [ { element = e.id; branch = branch } ]
                        let draw', reference', dir', insideRun =
                            match branch with
                            | BeamTree.Transmitted ->
                                let inside =
                                    if crossing.entry = crossing.exit then []
                                    else [ ClosedRun { start = crossing.entry; finish = crossing.exit; kind = InsideElement } ]
                                crossing.exit, abreast beamPoint crossing.exit crossing.transmitted, crossing.transmitted, inside
                            | BeamTree.Reflected ->
                                crossing.entry, abreast beamPoint crossing.entry crossing.reflected, crossing.reflected, []
                        let state'' = { state' with segments = drawn insideRun @ state'.segments }
                        let afterSide =
                            match sideBeamOf e.placement with
                            | OpensReflectedBeam ->
                                let sideMembers = membersOf (OnReflectedBeamOf e.id)
                                let rays =
                                    match light with
                                    | Lit -> litRay (OnReflectedBeamOf e.id) sideMembers :: state''.rays
                                    | Unlit -> state''.rays
                                walk
                                    crossing.entry
                                    (abreast beamPoint crossing.entry crossing.reflected)
                                    crossing.reflected
                                    x
                                    index
                                    (path @ [ { element = e.id; branch = BeamTree.Reflected } ])
                                    light
                                    sideMembers
                                    { state'' with rays = rays }
                            | NoSideBeam -> state''
                        walk draw' reference' dir' x index onward light later afterSide

            // The root is placed freely in 3-D (spec 0046): the beam leaves it at its height.
            let rootCentre = centreOf root.placement
            let rootDir = (r1Axis root.placement).normalized
            let rootFace = add rootCentre (scale (halfDepthOf root / 1.0<meter>) rootDir)
            let rootPlaced = { id = root.id; centre = rootCentre; incoming = None; beam = OnMainBeam; path = []; light = Lit }
            let mainMembers = membersOf OnMainBeam
            let final =
                walk rootFace rootCentre rootDir (along root.placement) 0 [ { element = root.id; branch = BeamTree.Transmitted } ] Lit mainMembers
                    { placed = Map.ofList [ 0, rootPlaced ]; segments = []; rays = [ litRay OnMainBeam mainMembers ] }

            // Every placed element's DRAWN box (oriented on its beam, the root by its own aim), by index.
            let boxes : (int * DrawnBox) list =
                elements
                |> List.mapi (fun i e -> i, e)
                |> List.choose (fun (i, e) ->
                    Map.tryFind i final.placed
                    |> Option.map (fun p ->
                        let drawnPlacement =
                            match p.incoming with
                            | Some d -> RayModel.orientedToBeam d e.placement
                            | None -> e.placement
                        let (n1, n2, n3) = orientedBasis drawnPlacement
                        let half (extent : float<meter>) : float = e.zoom * extent / 2.0 / 1.0<meter>
                        i, { centre = p.centre; axes = [ n1, half e.placement.box.b; n2, half e.placement.box.a1; n3, half e.placement.box.a2 ] }))
            let close (pendingSegment : PendingSegment) : RaySegment =
                match pendingSegment with
                | ClosedRun s -> s
                | OpenRay (origin, direction, emitter) ->
                    let reach =
                        boxes
                        |> List.choose (fun (i, box) -> if i = emitter then None else entryDistance origin direction box)
                        |> List.fold min (openBeamLength / 1.0<meter>)
                    { start = origin; finish = add origin (scale reach direction.normalized); kind = InFreeSpace }
            {
                elements =
                    elements
                    |> List.mapi (fun i e ->
                        Map.tryFind i final.placed
                        |> Option.defaultValue { id = e.id; centre = centreOf e.placement; incoming = None; beam = OnMainBeam; path = []; light = Unlit })
                segments = final.segments |> List.rev |> List.map close
                rays = List.rev final.rays
            }

    /// Lay the scene out with every element rotating about its centre (the default `RotationPivot`).
    let layout (elements : LayoutElement list) : SceneLayout = layoutWith PivotAtCentre elements

    /// The first lit ray that no detector terminates, in walk order (spec 0046) — where a newly added
    /// detector goes; `None` when every lit ray already ends on a detector.
    let firstFreeRay (scene : SceneLayout) : BeamHost option =
        scene.rays
        |> List.tryPick (fun r ->
            match r.ends with
            | EndsOpen -> Some r.host
            | EndsAtDetector -> None)

    /// Every element hosted — directly or through other side beams — on the reflected rays of `id`,
    /// i.e. the elements that move with it. Moving an element onto a beam inside this set would loop.
    let hostedUnder (elements : LayoutElement list) (id : Library.ElementId) : Set<Library.ElementId> =
        let rec grow (acc : Set<Library.ElementId>) (frontier : Set<Library.ElementId>) : Set<Library.ElementId> =
            let next =
                elements
                |> List.choose (fun e ->
                    match e.host with
                    | OnReflectedBeamOf p when Set.contains p frontier && not (Set.contains e.id acc) -> Some e.id
                    | OnReflectedBeamOf _ | OnMainBeam -> None)
                |> Set.ofList
            if Set.isEmpty next then acc else grow (Set.union acc next) next
        grow Set.empty (Set.singleton id)

    /// The reflected beams `id` may be moved onto: every OTHER non-root element that opens a reflected
    /// side beam and is not hosted under `id` itself (which would loop). The root is never moved.
    let attachableHosts (elements : LayoutElement list) (id : Library.ElementId) : Library.ElementId list =
        match elements with
        | [] -> []
        | root :: rest ->
            if root.id = id then []
            else
                let under = hostedUnder elements id
                rest
                |> List.filter (fun e ->
                    e.id <> id
                    && not (Set.contains e.id under)
                    && (match sideBeamOf e.placement with
                        | OpensReflectedBeam -> true
                        | NoSideBeam -> false))
                |> List.map (fun e -> e.id)

    // --- What the experiments can capture (spec 0046 task 5) -----------------

    /// The branches through which the light reaching any of `detectors` left the `measured` element,
    /// counting only LIT detectors (one behind another detector receives nothing) that also see the
    /// `varied` element (the variation must reach them).
    /// `T` for a detector behind it on its transmitted beam, `R` for one on its reflected beam.
    let detectedBranches
        (scene : SceneLayout)
        (detectors : Library.ElementId list)
        (varied : Library.ElementId)
        (measured : Library.ElementId)
        : BeamTree.BeamBranch list =
        scene.elements
        |> List.filter (fun p ->
            List.contains p.id detectors
            && (match p.light with
                | Lit -> true
                | Unlit -> false))
        |> List.choose (fun p ->
            let sees (id : Library.ElementId) = p.path |> List.exists (fun s -> s.element = id)
            match sees varied with
            | true -> p.path |> List.tryFind (fun s -> s.element = measured) |> Option.map (fun s -> s.branch)
            | false -> None)
        |> List.distinct

    /// The capture modes the detected branches allow: a transmitted detector offers T, a reflected one R,
    /// both offer T, R and Both; none offers nothing.
    let capturesFor (branches : BeamTree.BeamBranch list) : Experiments.MeasurementMode list =
        match List.contains BeamTree.Transmitted branches, List.contains BeamTree.Reflected branches with
        | true, true -> [ Experiments.CaptureTransmitted; Experiments.CaptureReflected; Experiments.CaptureBoth ]
        | true, false -> [ Experiments.CaptureTransmitted ]
        | false, true -> [ Experiments.CaptureReflected ]
        | false, false -> []

    /// The capture an experiment actually runs with: the requested one when the detectors allow it, else
    /// the best they do allow (Both when all are, otherwise the single one), or `None` when none is.
    let effectiveCapture (available : Experiments.MeasurementMode list) (requested : Experiments.MeasurementMode) : Experiments.MeasurementMode option =
        match List.contains requested available with
        | true -> Some requested
        | false ->
            match List.contains Experiments.CaptureBoth available with
            | true -> Some Experiments.CaptureBoth
            | false -> List.tryHead available
