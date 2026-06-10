namespace OpticalConstructor.Domain

/// Element groups + experiment collections (Spec 0026 Part G / Part H, slice 007). This is
/// a NET-NEW, pure, headless-testable domain layer sitting above the placement layer
/// (`Placement.fs`): it introduces NO drawing code, NO storage code (the separate
/// groups/collections JSON is `OpticalConstructor.Storage/GroupsLibrary.fs`), and NO
/// parallel element catalogue or project type (constraint 0.1). A group is a named set of
/// member elements, EACH holding its full configuration as a reused `Placement.ElementPlacement`
/// (its placement point, the three rotations with per-axis locks, value id, emission metadata,
/// and display unit — all canonical SI, constraint 0.2). The two member modes and the on/off /
/// swap operations change only WHICH members are currently in the beam, never their stored
/// configuration (AC-G1).
module Groups =

    /// The two member modes a group can take (G.1.2). There is deliberately NO third mode
    /// (non-requirement). A fieldless DU — the shared `ProjectJson.options` serialize it as a
    /// bare string, and the groups schema constrains it with an `enum`.
    type MemberMode =
        | MutuallyExclusive
        | MultiSelect

    /// One member of an element group (G.1.1): its FULL configuration (a reused
    /// `Placement.ElementPlacement`, so a member carries its placement, the three rotations with
    /// per-axis locks, value id, emission metadata, and display unit) plus whether it is
    /// currently in the beam. The on/off and swap choices flip only `inBeam`; the stored
    /// `placement` is never mutated by them (G.1.2 / AC-G1).
    type GroupMember =
        {
            placement : Placement.ElementPlacement
            inBeam : bool
        }

        /// A member freshly added to a group — its stored configuration, out of the beam.
        static member ofPlacement (p : Placement.ElementPlacement) : GroupMember =
            { placement = p; inBeam = false }

    /// A named, reusable element group (G.1.1): a set of member elements, each holding its full
    /// configuration, placeable on / removable from the central-ray path as a unit, and one of
    /// the two member modes (G.1.2). Groups build on the favourites concept (`FavoriteGroup`,
    /// `UserEnvironment.fs:126`) so a commonly used group can be pinned and dropped back in later
    /// (G.1.3); that pin/reuse is the favourites notion, this is the on-table group itself.
    type ElementGroup =
        {
            name : string
            mode : MemberMode
            members : GroupMember list
        }

        /// A new, empty named group of the given mode.
        static member create (name : string) (mode : MemberMode) : ElementGroup =
            { name = name; mode = mode; members = [] }

    /// Operations over an `ElementGroup` that change only `inBeam` (never a member's stored
    /// configuration), respecting the group's member mode (G.1.2 / AC-G1).
    module ElementGroup =

        /// The members currently in the beam (G.1.2).
        let inBeam (g : ElementGroup) : GroupMember list =
            g.members |> List.filter (fun m -> m.inBeam)

        /// Add a member (its full configuration, out of the beam) to the group (G.1.1).
        let addMember (p : Placement.ElementPlacement) (g : ElementGroup) : ElementGroup =
            { g with members = g.members @ [ GroupMember.ofPlacement p ] }

        /// Put member `i` in or out of the beam, respecting the group's member mode (G.1.2):
        ///   * `MutuallyExclusive` — putting one IN the beam takes every other member OUT (at most
        ///     one member in the beam at a time); putting it OUT leaves the rest unchanged.
        ///   * `MultiSelect` — members toggle independently (zero, one, or several in the beam).
        /// Either way every member's stored `placement` is preserved — only `inBeam` flips (AC-G1).
        /// An out-of-range index is a no-op (the group is returned unchanged).
        let setInBeam (i : int) (on : bool) (g : ElementGroup) : ElementGroup =
            if i < 0 || i >= List.length g.members then g
            else
                let members' =
                    g.members
                    |> List.mapi (fun j m ->
                        match g.mode with
                        | MutuallyExclusive ->
                            if j = i then { m with inBeam = on }
                            elif on then { m with inBeam = false }   // exclusivity: others leave the beam
                            else m
                        | MultiSelect ->
                            if j = i then { m with inBeam = on } else m)
                { g with members = members' }

        /// Swap which member of a mutually-exclusive group is in the beam (G.1.2): member `i`
        /// goes in, every other member goes out. A LOSSLESS swap — every member's stored
        /// configuration is preserved, so swapping back restores the previous member exactly
        /// (AC-G1). An out-of-range index is a no-op.
        let swapTo (i : int) (g : ElementGroup) : ElementGroup =
            if i < 0 || i >= List.length g.members then g
            else { g with members = g.members |> List.mapi (fun j m -> { m with inBeam = (j = i) }) }

    // -----------------------------------------------------------------------
    // Experiment collections (Part H / R-5).
    // -----------------------------------------------------------------------

    /// A named on/off toggle (H.1): an experiment, an element, or a group within a collection.
    /// `name` is the experiment label / element value-id / group name; `enabled` is its current
    /// on/off state across the collection. A plain serializable record (array of `{name,enabled}`
    /// in the JSON), so on/off state round-trips through the groups/collections file (AC-H1).
    type Toggle =
        {
            name : string
            enabled : bool
        }

        static member on (name : string) : Toggle = { name = name; enabled = true }
        static member off (name : string) : Toggle = { name = name; enabled = false }

    /// An experiment collection (H.1.1): a named set of experiments sharing the same sample(s),
    /// with elements and groups turned on and off across the whole collection. Collections are
    /// stored in the SAME separate JSON file as groups (G.3) and round-trip through it (AC-H1).
    type ExperimentCollection =
        {
            name : string
            /// The shared sample value-ids the experiments in the collection revolve around (H.1.1).
            sampleValueIds : string list
            /// The experiments grouped by the collection, each on or off (H.1.1).
            experiments : Toggle list
            /// Elements (by value-id) toggled on/off across the collection (H.1.1).
            elements : Toggle list
            /// Groups (by name) toggled on/off across the collection (H.1.1).
            groups : Toggle list
        }

        /// A new, empty collection around the given shared sample value-ids.
        static member create (name : string) (sampleValueIds : string list) : ExperimentCollection =
            { name = name; sampleValueIds = sampleValueIds; experiments = []; elements = []; groups = [] }

    /// Set the on/off state of a named toggle within a `Toggle list` (H.1.1) — the shared
    /// "turn an experiment / element / group on or off across the collection" operation. A name
    /// not present is a no-op.
    let setToggle (name : string) (enabled : bool) (toggles : Toggle list) : Toggle list =
        toggles |> List.map (fun t -> if t.name = name then { t with enabled = enabled } else t)

    // -----------------------------------------------------------------------
    // The on-disk library shape (Part G/H storage owns the IO; this is the value it carries).
    // -----------------------------------------------------------------------

    /// The reusable groups + collections library (G.3 / H.1): the value the separate per-user
    /// app-data JSON file holds. `OpticalConstructor.Storage/GroupsLibrary.fs` owns the load/save
    /// and schema validation of this value; it is kept apart from any per-project `.ocproj`.
    type GroupsLibrary =
        {
            groups : ElementGroup list
            collections : ExperimentCollection list
        }

        /// The empty library — no groups, no collections (the fall-back when no file exists).
        static member empty : GroupsLibrary = { groups = []; collections = [] }
