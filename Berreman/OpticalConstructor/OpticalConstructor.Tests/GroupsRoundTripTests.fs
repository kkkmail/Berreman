namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Storage
open OpticalConstructor.Storage.Errors
open Xunit

/// Element-group / experiment-collection domain + storage tests (Spec 0026 Part G/H, slice 007,
/// gate `constructor-unit-tests`). PURE Domain + Storage — no Avalonia (the AC-G2 primary-detector
/// behaviour lives in `ConstructorView` and is proved in the `ui-tests` `GroupDetectorUndoTests`,
/// keeping this suite Avalonia-free). Coverage:
///   * AC-G1 — the two member modes (mutually exclusive / multi-select) and a LOSSLESS swap that
///             preserves every member's stored configuration (only `inBeam` flips).
///   * AC-G3 — the groups + collections library round-trips through the separate, schema-validated
///             JSON, independent of any `.ocproj`; a wrong `schemaVersion` is rejected on load; the
///             file lives at the per-user app-data path and missing-file load is total.
///   * AC-H1 — an experiment collection's shared samples and on/off toggles round-trip; `setToggle`
///             flips just the named one.
module GroupsRoundTripTests =

    let private at (x : float) (y : float) : TablePoint = { x = x * 1.0<meter>; y = y * 1.0<meter> }

    /// A fully-configured member placement (rotated R1/R2, value-bound) so a swap that kept only
    /// `inBeam` but dropped the stored configuration would be caught by an equality check.
    let private configured (kind : CatalogueKind) (x : float) : ElementPlacement =
        let p = ElementPlacement.create kind (at x 0.0)
        { p with r1 = Angle.degree 12.0; r2 = Angle.degree 34.0; valueId = Some (sprintf "value-%s-%g" (string kind) x) }

    let private addMembers (ps : ElementPlacement list) (g : Groups.ElementGroup) : Groups.ElementGroup =
        ps |> List.fold (fun acc p -> Groups.ElementGroup.addMember p acc) g

    let private inBeamFlags (g : Groups.ElementGroup) : bool list =
        g.members |> List.map (fun m -> m.inBeam)

    let private okOr (r : Result<'a, StorageError>) : 'a =
        match r with
        | Ok v -> v
        | Error e -> failwith $"unexpected storage error: {e}"

    // =======================================================================
    // AC-G1 — member modes + lossless swap (pure `Groups` domain).
    // =======================================================================

    [<Fact>]
    let ``AC-G1 a mutually-exclusive group keeps at most one member in the beam`` () =
        let g =
            Groups.ElementGroup.create "filters" Groups.MutuallyExclusive
            |> addMembers [ configured LinearPolarizer 0.1; configured CircularPolarizer 0.2; configured Sample 0.3 ]
        // Putting member 1 in the beam leaves only it in (the others are taken out).
        let g1 = Groups.ElementGroup.setInBeam 1 true g
        Assert.Equal<bool list>([ false; true; false ], inBeamFlags g1)
        // Putting member 2 in then takes member 1 out — exclusivity holds across changes.
        let g2 = Groups.ElementGroup.setInBeam 2 true g1
        Assert.Equal<bool list>([ false; false; true ], inBeamFlags g2)
        Assert.Equal(1, Groups.ElementGroup.inBeam g2 |> List.length)

    [<Fact>]
    let ``AC-G1 swapping a mutually-exclusive group is lossless (only inBeam flips)`` () =
        let members = [ configured LinearPolarizer 0.1; configured CircularPolarizer 0.2; configured Sample 0.3 ]
        let g = Groups.ElementGroup.create "filters" Groups.MutuallyExclusive |> addMembers members
        // Several swaps in a row: each puts exactly one member in the beam.
        let swapped = g |> Groups.ElementGroup.swapTo 2 |> Groups.ElementGroup.swapTo 0 |> Groups.ElementGroup.swapTo 1
        // Every member's STORED configuration is byte-identical to the original — nothing but the
        // in-beam choice changed, so swapping back would restore the previous member exactly (AC-G1).
        Assert.Equal<ElementPlacement list>(members, swapped.members |> List.map (fun m -> m.placement))
        Assert.Equal<bool list>([ false; true; false ], inBeamFlags swapped)

    [<Fact>]
    let ``AC-G1 a multi-select group toggles members independently`` () =
        let g =
            Groups.ElementGroup.create "stack" Groups.MultiSelect
            |> addMembers [ configured Lens 0.1; configured Sample 0.2; configured FlatMirror 0.3 ]
        // Zero, one or several members may be in the beam at once.
        let on = g |> Groups.ElementGroup.setInBeam 0 true |> Groups.ElementGroup.setInBeam 2 true
        Assert.Equal<bool list>([ true; false; true ], inBeamFlags on)
        // Toggling member 0 OFF leaves member 2 untouched (independent toggles).
        let off = Groups.ElementGroup.setInBeam 0 false on
        Assert.Equal<bool list>([ false; false; true ], inBeamFlags off)

    [<Fact>]
    let ``AC-G1 an out-of-range index is a no-op`` () =
        let g = Groups.ElementGroup.create "g" Groups.MultiSelect |> addMembers [ configured Lens 0.1 ]
        Assert.Equal<Groups.ElementGroup>(g, Groups.ElementGroup.setInBeam 5 true g)
        Assert.Equal<Groups.ElementGroup>(g, Groups.ElementGroup.swapTo -1 g)

    // =======================================================================
    // AC-G3 / AC-H1 — the separate groups/collections library round-trip + schema.
    // =======================================================================

    /// A representative library: a mutually-exclusive group (one member in the beam), a multi-select
    /// group, and an experiment collection around two shared samples with on/off toggles.
    let private library : Groups.GroupsLibrary =
        let exclusive =
            Groups.ElementGroup.create "filters" Groups.MutuallyExclusive
            |> addMembers [ configured LinearPolarizer 0.1; configured CircularPolarizer 0.2 ]
            |> Groups.ElementGroup.setInBeam 0 true
        let multi =
            Groups.ElementGroup.create "lenses" Groups.MultiSelect
            |> addMembers [ configured Lens 0.4 ]
        let collection =
            { Groups.ExperimentCollection.create "sweep" [ "sample-A"; "sample-B" ] with
                experiments = [ Groups.Toggle.on "exp-1"; Groups.Toggle.off "exp-2" ]
                elements = [ Groups.Toggle.on "value-Lens-0.4" ]
                groups = [ Groups.Toggle.on "filters" ] }
        { groups = [ exclusive; multi ]; collections = [ collection ] }

    [<Fact>]
    let ``AC-G3 the groups library round-trips through schema-validated JSON`` () =
        let json = GroupsLibrary.serialize library |> okOr
        // The canonical artefact is JSON text carrying the schema envelope.
        Assert.StartsWith("{", json.TrimStart())
        Assert.Contains("schemaVersion", json)
        // Deserialize runs validate-on-load BEFORE binding; the round-trip is lossless.
        let back = GroupsLibrary.deserialize json |> okOr
        Assert.Equal<Groups.GroupsLibrary>(library, back)

    [<Fact>]
    let ``AC-G3 deserialize validates on load and rejects a wrong schemaVersion`` () =
        // A `schemaVersion` mismatch fails the schema `const` and surfaces as a validation error —
        // never a silent migration (out-of-scope, 0.6).
        let bad = (GroupsLibrary.serialize library |> okOr).Replace("\"1.0\"", "\"9.9\"")
        match GroupsLibrary.deserialize bad with
        | Error (SchemaValidationError _) -> ()
        | other -> failwith $"expected SchemaValidationError, got {other}"

    [<Fact>]
    let ``AC-G3 a nested groups-file violation keeps its instance-location detail (reused validate seam)`` () =
        // A document well-formed at the TOP level (schemaVersion/groups/collections present) but with a
        // malformed member placement DEEP inside a group. Because `GroupsLibrary.validate` reuses
        // `SchemaValidation.collectMessages` (which recurses `results.Details`, R-4), the error keeps its
        // nested instance location instead of collapsing to the generic top-level fallback message.
        let nested =
            """{ "schemaVersion": "1.0",
                 "groups": [ { "name": "g", "mode": "MultiSelect",
                               "members": [ { "placement": { "r1": "not-a-number" }, "inBeam": true } ] } ],
                 "collections": [] }"""
        match GroupsLibrary.deserialize nested with
        | Error (SchemaValidationError msgs) ->
            let joined = String.concat " | " msgs
            // The nested member path is pinpointed — not the generic "groups schema validation failed".
            Assert.Contains("members", joined)
            Assert.DoesNotContain("groups schema validation failed", joined)
        | other -> failwith $"expected SchemaValidationError, got {other}"

    [<Fact>]
    let ``AC-G3 the library lives at the per-user app-data path and missing-file load is total`` () =
        let path = GroupsLibrary.libraryPath ()
        Assert.EndsWith("groups.json", path)
        // Mirrors the environment store's Softellect/Berreman/OpticalConstructor app-data folder.
        Assert.Contains("OpticalConstructor", path)
        // Load of a non-existent file falls back to the empty library, never throwing.
        Assert.Equal<Groups.GroupsLibrary>(Groups.GroupsLibrary.empty, GroupsLibrary.load (path + ".does-not-exist"))

    [<Fact>]
    let ``AC-H1 a collection's shared samples and on/off toggles round-trip and setToggle flips one`` () =
        let back = GroupsLibrary.serialize library |> okOr |> GroupsLibrary.deserialize |> okOr
        let coll = List.head back.collections
        // The shared sample value-ids and the experiment/element/group on-off state survived.
        Assert.Equal<string list>([ "sample-A"; "sample-B" ], coll.sampleValueIds)
        Assert.Equal<Groups.Toggle list>((List.head library.collections).experiments, coll.experiments)
        Assert.Equal<Groups.Toggle list>((List.head library.collections).groups, coll.groups)
        // Turning a named experiment off across the collection flips just that one (H.1.1).
        let flipped = Groups.setToggle "exp-1" false coll.experiments
        Assert.Equal<bool list>([ false; false ], flipped |> List.map (fun t -> t.enabled))
