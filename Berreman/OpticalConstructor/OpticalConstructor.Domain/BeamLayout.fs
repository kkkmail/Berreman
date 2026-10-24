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

    /// Whether an element opens a reflected SIDE beam that other elements can sit on: a polarizer, lens
    /// or sample emitting BOTH branches does (its transmitted branch continues the beam it sits on); a
    /// mirror's reflected branch IS the beam it continues, and a source / detector reflects nothing.
    type SideBeam =
        | OpensReflectedBeam
        | NoSideBeam

    let sideBeamOf (p : ElementPlacement) : SideBeam =
        match p.catalogueKind with
        | LinearPolarizer | CircularPolarizer | Sample | Lens ->
            match p.emission with
            | EmitBoth -> OpensReflectedBeam
            | EmitReflectedOnly | EmitTransmittedOnly -> NoSideBeam
        | LightSource | FlatMirror | CurvedMirror | Detector -> NoSideBeam

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
        | LinearPolarizer | CircularPolarizer | Lens | Sample ->
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

    /// The length of a beam drawn past its last element when nothing terminates it — a reflected ray with
    /// nothing on it yet, or a main beam with no detector at its end — so the ray is visible to attach to.
    let openBeamLength : float<meter> = 0.5<meter>

    /// One scene element as the layout reads it. The FIRST element of the list handed to `layout` is the
    /// ray root (the Main scene seeds the light source there).
    type LayoutElement =
        {
            id : Library.ElementId
            placement : ElementPlacement
            host : BeamHost
            /// The DRAWN half-depth of the element along its face normal (its box depth times its draw
            /// zoom, halved) — where the ray meets its faces, so the ray and the drawing agree.
            halfDepth : float<meter>
        }

    /// One step of the light's path: an element and the branch the light left it by.
    type PathStep =
        {
            element : Library.ElementId
            branch : BeamTree.BeamBranch
        }

    /// One element as laid out: its drawn centre, the beam direction arriving at it (`None` for the
    /// root), the beam it actually sits on (its host, or the main beam when that host's reflected ray
    /// does not exist), and the path the light took from the root to reach it.
    type PlacedElement =
        {
            id : Library.ElementId
            centre : Vector3
            incoming : Vector3 option
            beam : BeamHost
            path : PathStep list
        }

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

    /// The laid-out scene: every element in INPUT order, and every drawn ray segment.
    type SceneLayout =
        {
            elements : PlacedElement list
            segments : RaySegment list
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

    /// Lay the scene out (spec 0046). Walks the main beam from the root, crossing every element by
    /// `RayModel.crossElement` at its DRAWN orientation (`RayModel.orientedToBeam`) and depth, recursing
    /// into each reflected side beam, and emits the drawn segments: free-space runs face to face, the
    /// refracted run INSIDE each transmitting element, and an open-ended run of `openBeamLength` past the
    /// last element of a beam unless a detector terminates it.
    let layout (elements : LayoutElement list) : SceneLayout =
        match elements with
        | [] -> { elements = []; segments = [] }
        | root :: rest ->
            let hosts = effectiveHosts elements
            let hostOf (e : LayoutElement) : BeamHost = Map.tryFind e.id hosts |> Option.defaultValue OnMainBeam
            let indexed = rest |> List.mapi (fun i e -> i + 1, e)
            let membersOf (host : BeamHost) : (int * LayoutElement) list =
                indexed |> List.filter (fun (_, e) -> hostOf e = host) |> List.sortBy (fun (_, e) -> along e.placement)

            /// Walk one beam. `draw` is where the drawn ray currently is, `reference` the point the next gap
            /// is measured from, `dir` the beam direction, `prevX` the along-beam coordinate of the element
            /// the beam left, `path` the steps that led onto this beam.
            let rec walk
                (draw : Vector3) (reference : Vector3) (dir : Vector3) (prevX : float) (path : PathStep list)
                (members : (int * LayoutElement) list)
                (placed : Map<int, PlacedElement>) (segments : RaySegment list)
                : Map<int, PlacedElement> * RaySegment list =
                match members with
                | [] ->
                    let tail = { start = draw; finish = add draw (scale (openBeamLength / 1.0<meter>) dir); kind = InFreeSpace }
                    placed, tail :: segments
                | (index, e) :: later ->
                    let x = along e.placement
                    let centre = add reference (scale (max 0.0 (x - prevX)) dir)
                    let normal = r1Axis (RayModel.orientedToBeam dir e.placement)
                    let crossing = RayModel.crossElement centre dir normal e.halfDepth
                    let approach = { start = draw; finish = crossing.entry; kind = InFreeSpace }
                    let here = { id = e.id; centre = centre; incoming = Some dir; beam = hostOf e; path = path }
                    let placed' = Map.add index here placed
                    match e.placement.catalogueKind, later with
                    | Detector, [] ->
                        // A detector at the end of its beam terminates it: nothing is drawn past its face.
                        placed', approach :: segments
                    | _ ->
                        let branch = RayModel.primaryBranch e.placement
                        let onward = path @ [ { element = e.id; branch = branch } ]
                        let draw', reference', dir', insideRun =
                            match branch with
                            | BeamTree.Transmitted ->
                                let inside =
                                    if crossing.entry = crossing.exit then []
                                    else [ { start = crossing.entry; finish = crossing.exit; kind = InsideElement } ]
                                crossing.exit, abreast centre crossing.exit crossing.transmitted, crossing.transmitted, inside
                            | BeamTree.Reflected ->
                                crossing.entry, abreast centre crossing.entry crossing.reflected, crossing.reflected, []
                        let placedSide, segmentsSide =
                            match sideBeamOf e.placement with
                            | OpensReflectedBeam ->
                                walk
                                    crossing.entry
                                    (abreast centre crossing.entry crossing.reflected)
                                    crossing.reflected
                                    x
                                    (path @ [ { element = e.id; branch = BeamTree.Reflected } ])
                                    (membersOf (OnReflectedBeamOf e.id))
                                    placed'
                                    (insideRun @ approach :: segments)
                            | NoSideBeam -> placed', insideRun @ approach :: segments
                        walk draw' reference' dir' x onward later placedSide segmentsSide

            let rootCentre = RayModel.pointToVector3 root.placement.placementPoint
            let rootDir = (r1Axis root.placement).normalized
            let rootFace = add rootCentre (scale (root.halfDepth / 1.0<meter>) rootDir)
            let rootPlaced = { id = root.id; centre = rootCentre; incoming = None; beam = OnMainBeam; path = [] }
            let placed, segments =
                walk rootFace rootCentre rootDir (along root.placement) [ { element = root.id; branch = BeamTree.Transmitted } ] (membersOf OnMainBeam) (Map.ofList [ 0, rootPlaced ]) []
            {
                elements = elements |> List.mapi (fun i e -> Map.tryFind i placed |> Option.defaultValue { id = e.id; centre = RayModel.pointToVector3 e.placement.placementPoint; incoming = None; beam = OnMainBeam; path = [] })
                segments = List.rev segments
            }

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
    /// counting only detectors that also see the `varied` element (the variation must reach them).
    /// `T` for a detector behind it on its transmitted beam, `R` for one on its reflected beam.
    let detectedBranches
        (scene : SceneLayout)
        (detectors : Library.ElementId list)
        (varied : Library.ElementId)
        (measured : Library.ElementId)
        : BeamTree.BeamBranch list =
        scene.elements
        |> List.filter (fun p -> List.contains p.id detectors)
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
