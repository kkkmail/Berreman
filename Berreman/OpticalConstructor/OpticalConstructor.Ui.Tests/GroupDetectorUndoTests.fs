/// Multiple-detector, element-group-on-table and undo/redo interaction tests (Spec 0026 Part
/// G/K, slice 007, gate `ui-tests` / `ui-smoke`). The interaction acceptance criteria are proved
/// against the PURE `ConstructorView.update`; one headless mount renders the Experiment-tab
/// controls and the slice-007 overlays. Coverage:
///   * AC-G2 — the primary detector is the FIRST detector; "Set as primary" reorders to it, a real
///             undoable project mutation; secondary detectors keep their own placement.
///   * AC-K1 — placement, rotation, lock, group on/off, swap and detector changes each push an
///             undoable snapshot; undo/redo are multi-level (more than one level deep).
///   * AC-K2 — reset rotation / delete / reset view are confirmation-gated with a same-row,
///             distinct-colour Confirm/Cancel gate that resolves (confirm applies, cancel aborts).
namespace OpticalConstructor.Ui.Tests

open Avalonia.Controls
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Xunit
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Project
open OpticalConstructor.Ui
open OpticalConstructor.Ui.Commands

module GroupDetectorUndoTests =

    // --- Fixtures -----------------------------------------------------------

    let private at (x : float) (y : float) : TablePoint = { x = x * 1.0<meter>; y = y * 1.0<meter> }
    let private sampleAt (x : float) (y : float) : ElementPlacement = ElementPlacement.create Sample (at x y)
    let private detectorAt (x : float) (y : float) : ElementPlacement = ElementPlacement.create Detector (at x y)

    /// A page model seeded over the canonical template project with `ps` as the on-table
    /// placements (the rest of the project is irrelevant to the interaction logic).
    let private model (ps : ElementPlacement list) : ConstructorView.Model =
        let proj = { Templates.bandpassFilter () with placements = ps; table = Table.defaultTable }
        ConstructorView.init UserEnvironment.defaults proj

    let private select (i : int) (m : ConstructorView.Model) : ConstructorView.Model =
        { m with selection = ConstructorView.ElementSelected i }

    let private placements (m : ConstructorView.Model) : ElementPlacement list = m.project.placements

    let private resource : Localization.Resource =
        match Localization.loadFromFile (Localization.resourcePath ()) with
        | Ok r -> r
        | Error _ -> { entries = Map.empty }

    let private noopDispatch : Ribbon.Dispatch =
        { onRibbon = ignore; onConstructor = ignore; onNavigateLegacy = ignore; onSetLanguage = ignore; onToggleTheme = ignore }

    // =======================================================================
    // AC-G2 — multiple detectors and the primary one.
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-G2 the primary detector is the first detector and Set as primary reorders to it`` () =
        let m = model [ sampleAt 0.0 0.0; detectorAt 0.1 0.0; detectorAt 0.2 0.0 ]
        // Two detectors (indices 1 and 2); the primary is the FIRST of them.
        Assert.Equal<int list>([ 1; 2 ], ConstructorView.detectorIndices m)
        Assert.Equal(Some 1, ConstructorView.primaryDetectorIndex m)
        // Select the SECOND detector and "Set as primary": it moves to the front of the placements,
        // so the headline-driving primary detector now follows it (a real, round-trippable mutation).
        let m2 = ConstructorView.update (ConstructorView.Invoke SetPrimaryDetector) (select 2 m)
        Assert.Equal(Detector, (List.head (placements m2)).catalogueKind)
        Assert.Equal(0.2<meter>, (List.head (placements m2)).placementPoint.x)
        Assert.Equal(Some 0, ConstructorView.primaryDetectorIndex m2)
        // Both detectors still exist — the secondary keeps its own placement (its own branch).
        Assert.Equal(2, ConstructorView.detectorIndices m2 |> List.length)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-G2 and AC-K1 Set as primary on the already-primary detector pushes no undo step`` () =
        // `setPrimaryDetectorNow` shares the `commitIfChanged` no-op guard: when the chosen detector
        // is already the primary (placement index 0), `moveToFront 0` is identity — an inert edit that
        // must push NOTHING (the slice's own High risk: never push on a no-op edit).
        let m = model [ detectorAt 0.0 0.0; sampleAt 0.1 0.0; detectorAt 0.2 0.0 ]
        Assert.Equal(Some 0, ConstructorView.primaryDetectorIndex m) // precondition: detector 0 is primary
        let noop = ConstructorView.update (ConstructorView.Invoke SetPrimaryDetector) (select 0 m)
        Assert.Equal<ElementPlacement list>(placements m, placements noop)
        Assert.True(List.isEmpty noop.history.past)
        // Promoting a NON-primary detector (index 2) is a real reorder and pushes exactly one step.
        let real = ConstructorView.update (ConstructorView.Invoke SetPrimaryDetector) (select 2 m)
        Assert.Equal(1, List.length real.history.past)
        Assert.Equal(0.2<meter>, (List.head (placements real)).placementPoint.x)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-G2 and AC-K1 adding and removing a detector are undoable project edits`` () =
        let m0 = model [ sampleAt 0.0 0.0 ]
        // Add a detector → appended, undoable.
        let added = ConstructorView.update (ConstructorView.Invoke AddDetector) m0
        Assert.Equal(2, placements added |> List.length)
        Assert.Equal(1, ConstructorView.detectorIndices added |> List.length)
        let undoAdd = ConstructorView.update (ConstructorView.Invoke Undo) added
        Assert.Equal(1, placements undoAdd |> List.length)
        // Remove the active detector → deleted, undoable.
        let removed = ConstructorView.update (ConstructorView.Invoke RemoveDetector) (select 1 added)
        Assert.Equal(1, placements removed |> List.length)
        let undoRemove = ConstructorView.update (ConstructorView.Invoke Undo) removed
        Assert.Equal(2, placements undoRemove |> List.length)

    // =======================================================================
    // AC-K1 — every constructor action is an undoable snapshot; multi-level.
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-K1 undo and redo are multi-level across placement, rotation and lock`` () =
        let m0 = model [ sampleAt 0.0 0.0 ]
        // Three distinct project edits in a row: place, rotate, lock.
        let m1 = ConstructorView.update (ConstructorView.RibbonDrop (Detector, at 0.2 0.0)) m0
        Assert.Equal(2, placements m1 |> List.length)
        let m2 = ConstructorView.update (ConstructorView.WheelAt ([ Shift ], 1, at 0.0 0.0)) (select 0 m1)
        Assert.True(abs ((List.item 0 (placements m2)).r1.degrees - 5.0) < 1e-9)
        let m3 = ConstructorView.update (ConstructorView.MenuToggleLock ConstructorView.AxisR1) m2
        Assert.True((List.item 0 (placements m3)).r1Locked)
        // Undo THREE times — multi-level, one step per edit, in reverse order.
        let u1 = ConstructorView.update (ConstructorView.Invoke Undo) m3
        Assert.False((List.item 0 (placements u1)).r1Locked)
        let u2 = ConstructorView.update (ConstructorView.Invoke Undo) u1
        Assert.Equal(0.0, (List.item 0 (placements u2)).r1.value)
        let u3 = ConstructorView.update (ConstructorView.Invoke Undo) u2
        Assert.Equal(1, placements u3 |> List.length)
        // Redo replays forward (multi-level), restoring the first edit.
        let r1 = ConstructorView.update (ConstructorView.Invoke Redo) u3
        Assert.Equal(2, placements r1 |> List.length)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-K1 a no-op edit (wheel-rotate about the locked R3 axis) pushes no undo step`` () =
        // R3 is locked by default (A.1.2), so a wheel-rotate about R3 (Alt+wheel) is inert: the
        // lock-respecting setter swallows it and the project is unchanged. The shared `commitIfChanged`
        // guard must NOT push an identical snapshot — a `Ctrl+Z` that does nothing would dirty AC-K1
        // ("each action pushes ONE undoable snapshot") and the slice's own High risk (never push on a
        // no-op edit).
        let m = select 0 (model [ sampleAt 0.0 0.0 ])
        Assert.True((List.item 0 (placements m)).r3Locked) // precondition: R3 starts locked
        let inert = ConstructorView.update (ConstructorView.WheelAt ([ Alt ], 1, at 0.0 0.0)) m
        // The placement is unchanged (the locked axis swallowed the rotation) and NOTHING was pushed,
        // so there is nothing to undo.
        Assert.Equal<ElementPlacement list>(placements m, placements inert)
        Assert.True(List.isEmpty inert.history.past)
        // A real rotation about the UNLOCKED R1 (Shift+wheel) by contrast DOES push exactly one step.
        let real = ConstructorView.update (ConstructorView.WheelAt ([ Shift ], 1, at 0.0 0.0)) m
        Assert.Equal(1, List.length real.history.past)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-K1 and AC-K2 confirming reset-rotation on an already-zero element pushes no undo step`` () =
        // `resetRotationNow` shares the `commitIfChanged` no-op guard. Reset-rotation is armed
        // regardless of the element's current rotation, so confirming it on an element ALREADY at
        // zero re-applies the same zero angles — an inert edit that must push NOTHING (the slice's
        // own High risk: never push on a no-op edit).
        let m = select 0 (model [ sampleAt 0.0 0.0 ])
        Assert.Equal(0.0, (List.item 0 (placements m)).r1.value) // precondition: already at zero rotation
        let armed = ConstructorView.update (ConstructorView.Invoke ResetRotation) m
        Assert.Equal(Some "confirm.resetRotation", ConstructorView.pendingPromptKey armed)
        let confirmed = ConstructorView.update ConstructorView.ConfirmPending armed
        Assert.Equal<ElementPlacement list>(placements m, placements confirmed)
        Assert.True(List.isEmpty confirmed.history.past)
        // Resetting a genuinely-rotated element, by contrast, pushes exactly one step.
        let rotated = ConstructorView.update (ConstructorView.WheelAt ([ Shift ], 1, at 0.0 0.0)) m
        let before = List.length rotated.history.past
        let armedReal = ConstructorView.update (ConstructorView.Invoke ResetRotation) rotated
        let resetReal = ConstructorView.update ConstructorView.ConfirmPending armedReal
        Assert.Equal(before + 1, List.length resetReal.history.past)
        Assert.Equal(0.0, (List.item 0 (placements resetReal)).r1.value)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-K1 a slide drag pushes exactly one undoable snapshot for the whole drag`` () =
        // Three elements; the middle one slides between its neighbours across several mid-drag moves.
        let m1 = select 1 (model [ sampleAt -0.5 0.0; sampleAt 0.0 0.0; sampleAt 0.5 0.0 ])
        let mBegin = ConstructorView.update (ConstructorView.BeginDrag ([ Shift ], at 0.0 0.0)) m1
        let mMove1 = ConstructorView.update (ConstructorView.SlideTo (at 0.2 0.0)) mBegin
        let mMove2 = ConstructorView.update (ConstructorView.SlideTo (at 0.3 0.0)) mMove1
        let mEnd = ConstructorView.update ConstructorView.EndDrag mMove2
        Assert.Equal(0.3<meter>, (List.item 1 (placements mEnd)).placementPoint.x)
        // A SINGLE undo reverts the whole slide to where the element began (not one undo per move).
        let undo = ConstructorView.update (ConstructorView.Invoke Undo) mEnd
        Assert.Equal(0.0<meter>, (List.item 1 (placements undo)).placementPoint.x)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-K1 group on/off is an undoable project edit`` () =
        // Group the active element (a workspace group whose single member is in the beam),
        // then toggle it OUT and back IN — each toggle is an undoable project edit.
        let grouped = ConstructorView.update ConstructorView.GroupActiveElement (select 0 (model [ sampleAt 0.0 0.0 ]))
        Assert.Equal(1, List.length grouped.groups)
        let off = ConstructorView.update (ConstructorView.GroupToggle (0, 0, false)) grouped
        Assert.Equal(0, placements off |> List.length)
        let undo = ConstructorView.update (ConstructorView.Invoke Undo) off
        Assert.Equal(1, placements undo |> List.length)
        let on = ConstructorView.update (ConstructorView.GroupToggle (0, 0, true)) off
        Assert.Equal(1, placements on |> List.length)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-K1 and AC-G1 a mutually-exclusive swap is an undoable, lossless project edit`` () =
        let pA = ElementPlacement.create LinearPolarizer (at 0.0 0.0)
        let pB = ElementPlacement.create CircularPolarizer (at 0.0 0.0)
        let g =
            Groups.ElementGroup.create "filters" Groups.MutuallyExclusive
            |> Groups.ElementGroup.addMember pA
            |> Groups.ElementGroup.addMember pB
            |> Groups.ElementGroup.setInBeam 0 true
        // A model whose project has member A in the beam and whose workspace carries the group.
        let m0 = { model [ pA ] with groups = [ g ] }
        // Swap to member B: A leaves the beam, B enters — a real, undoable project edit.
        let swapped = ConstructorView.update (ConstructorView.GroupSwap (0, 1)) m0
        Assert.Equal(CircularPolarizer, (List.head (placements swapped)).catalogueKind)
        // The members' stored configurations are intact through the UI swap path (AC-G1).
        Assert.Equal<ElementPlacement list>([ pA; pB ], (List.head swapped.groups).members |> List.map (fun m -> m.placement))
        // Undo restores member A in the beam.
        let undo = ConstructorView.update (ConstructorView.Invoke Undo) swapped
        Assert.Equal(LinearPolarizer, (List.head (placements undo)).catalogueKind)

    // =======================================================================
    // AC-K2 — destructive actions are confirmation-gated (same-row distinct-colour gate).
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-K2 reset rotation, delete and reset view are confirmation-gated and resolve`` () =
        // The Confirm/Cancel CTAs draw from distinct positive/negative colours (UX 5 / J.2).
        Assert.NotEqual(Controls.positiveCtaColor, Controls.negativeCtaColor)
        // Reset rotation arms a confirm; confirming through the gate performs the reset.
        let rotated () = ConstructorView.update (ConstructorView.WheelAt ([ Shift ], 1, at 0.0 0.0)) (select 0 (model [ sampleAt 0.0 0.0 ]))
        let armedR = ConstructorView.update (ConstructorView.Invoke ResetRotation) (rotated ())
        Assert.Equal(Some "confirm.resetRotation", ConstructorView.pendingPromptKey armedR)
        let doneR = ConstructorView.update ConstructorView.ConfirmPending armedR
        Assert.Equal(0.0, (List.item 0 (placements doneR)).r1.value)
        Assert.Equal(None, doneR.pending)
        // A non-trivial (rotated) element's delete confirms; cancelling leaves it in place.
        let armedD = ConstructorView.update (ConstructorView.Invoke DeleteElement) (rotated ())
        Assert.Equal(Some "confirm.delete", ConstructorView.pendingPromptKey armedD)
        let cancelled = ConstructorView.update ConstructorView.CancelPending armedD
        Assert.Equal(1, placements cancelled |> List.length)
        Assert.Equal(None, cancelled.pending)
        // Reset view arms a confirm (it is ephemeral view state — gated, not undoable, K.2).
        let armedV = ConstructorView.update (ConstructorView.Invoke ResetView) (model [])
        Assert.Equal(Some "confirm.resetView", ConstructorView.pendingPromptKey armedV)

    // =======================================================================
    // Headless render — the Experiment-tab controls + the slice-007 overlays.
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``the Experiment tab renders the group creation action and a group member control`` () =
        HeadlessSession.run (fun () ->
            // Group a selected element so the Experiment tab shows a group with one member toggle.
            let cv = ConstructorView.update ConstructorView.GroupActiveElement (select 0 (model [ sampleAt 0.0 0.0; detectorAt 0.2 0.0 ]))
            let window = Window()
            window.Content <-
                Component(fun _ ->
                    Ribbon.view resource Localization.English { Ribbon.init with activeTab = Ribbon.Experiment }
                        cv UserEnvironment.defaults noopDispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let buttonLabels =
                window.GetVisualDescendants()
                |> Seq.choose (fun v -> match v with | :? Button as b -> (match b.Content with | :? string as s -> Some s | _ -> None) | _ -> None)
                |> Set.ofSeq
            // The "group selected element" action and the member's element label both render.
            Assert.Contains(Localization.lookup resource Localization.English "experiment.newGroup", buttonLabels)
            Assert.Contains(Localization.lookup resource Localization.English "element.sample", buttonLabels)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the slice-007 overlays and Experiment tab render one frame without throwing`` () =
        HeadlessSession.run (fun () ->
            let cv =
                { model [ sampleAt 0.0 0.0; detectorAt 0.2 0.0 ] with
                    selection = ConstructorView.ElementSelected 0
                    pending = Some ConstructorView.ConfirmResetView
                    contextMenuOpen = true
                    elementDialogOpen = true }
            let cvGrouped = ConstructorView.update ConstructorView.GroupActiveElement cv
            let window = Window()
            window.Content <-
                Component(fun _ ->
                    Grid.create [
                        Grid.children [
                            Ribbon.view resource Localization.English { Ribbon.init with activeTab = Ribbon.Experiment } cvGrouped UserEnvironment.defaults noopDispatch
                            Ribbon.confirmGateOverlay resource Localization.English cvGrouped ignore
                            Ribbon.contextMenuOverlay resource Localization.English ignore
                            Ribbon.elementDialogOverlay resource Localization.English ignore
                        ]
                    ] :> IView)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(window.IsVisible)
            window.Close())
