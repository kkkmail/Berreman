# Architecture critique -- 003.slice-md cycle 1

## Summary

Clean implementation with one or two issues worth the judge's attention. The
edit-dispatch path, the §0.4-marshaled async node-solve `Cmd`, and the
delete-confirm/undo UI all land where the spec asks, route every mutation
through the frozen `ConstructionPage.update` / `StackEditor.applyStackMsg`, and
keep the root model pure (§0.5). The single most important finding is a
spec-vs-frozen-code mismatch: R-2's premise that `AttachChild` / `ConfirmDeleteNode`
"mark a node busy" is false against the frozen `update` (only `EditStack` sets
`busy`), and the worker correctly attaches the solve `Cmd` anyway and documented
the gap — the judge should register this as a known divergence, not a defect.

## Separation of concerns

The async effect is placed correctly. `nodeSolveCmd` / `constructionCmd` live as
private host-layer helpers in `Shell.fs` (lines 138-170), the off-thread solve runs
on `Task.Run`, and completion is marshaled with `Dispatcher.UIThread.Post` — so no
`CancellationTokenSource` or solve handle ever enters the root model (§0.5 honoured).
The view (`ConstructionView.fs`) is a thin binding: it builds `StackMsg` payloads
and dispatches, re-implementing no mutation. Good layering discipline overall.

One subtlety worth flagging: `constructionCmd` reads `model.construction.pendingDeletion`
(the *pre*-update model) to recover the deleted path for `ConfirmDeleteNode`
(`Shell.fs:188`), because the post-update model has already cleared it. This is
correct and necessary, but it couples the host `Cmd` to the ordering of the pure
update — the inline comment carries that knowledge, which is the right mitigation.

## Consistency

`stackPanel` follows the slice-002 `ChartView.chartPanel` precedent (takes
`ConstructionPage.Model` + a `Msg -> unit` dispatch, not `RootModel`), so it
composes under `Shell` via `RootMsg.Construction >> dispatch` with no module cycle
(`Shell.fs:240`). All ten `StackMsg` variants get exactly one control, reusing the
frozen DU constructors (`StackEditor.AddLayer`, `GroupLayers`, `AsWedge`, etc.)
rather than re-deriving payloads — exactly what R-1 intends.

Minor nit against the project style guide ("no `open` of project-internal
namespaces unless the file uses 3+ symbols"): `open Berreman.Geometry`
(`ConstructionView.fs:25`) is used for just two symbols — `Rotation.rotatePiX`
(`Geometry.fs:616`) and `WedgeAngle.defaultValue`. A qualified reference would
match the stated convention. Low priority; not worth a re-spawn on its own.

## Spec fit

R-1, R-2, R-3 are all delivered, and the two ACs are exercised by
`ConstructionEditTests.fs`. The hardcoded payloads (vacuum for every medium/material,
`rotatePiX`, `defaultNewLayer`) are not under-delivery — R-1 explicitly scopes this
to "one control per `StackMsg` to prove the edit-dispatch path," and the SoW Deferred
section names richer affordances as later polish. No scope creep either.

The one genuine spec/code seam: R-2 (and the `Shell.update` comment at line 184)
both assert that `EditStack` / `AttachChild` / `ConfirmDeleteNode` are
"busy-marking" transitions, but the frozen `ConstructionPage.update` sets `busy`
only on `EditStack` (via `editNode`, `ConstructionPage.fs:166-167`); `AttachChild`
and `deleteNodeNow` never touch `busy`. The worker still attaches a solve `Cmd` for
those two so dependent views refresh (Commitment 4), and recorded the no-busy-flash
gap in the SoW Deferred section. That is the most defensible reading without editing
a frozen module — flagging it so the judge weighs it as an intentional, documented
divergence rather than a silent miss.

## Evolvability

The `busy → off-thread solve → marshaled NodeSolved` shape is cleanly factored for
U7's fit `Cmd` to reuse, as the hand-off promises. Two things a later slice will
have to revisit:

1. **Stale `results` after deletion.** `ConfirmDeleteNode` re-solves the deleted
   node's *parent* (`parentPath`, `Shell.fs:133`), but `NodeSolved`'s frozen update
   only *adds* solved paths and prunes `busy` — it never removes the deleted
   sub-tree's entries from `model.results`. So a deleted node's `EmFieldSystem`
   lingers in `results` keyed by a now-unreachable path. Harmless today (tree-walking
   views won't render an orphan key), but a future result-surface that iterates
   `results` directly would read a ghost. Pruning belongs in the frozen update, so
   this is a note for a later slice, not this one.

2. **Globally-pending gate rendered in a per-node panel.** `nodeControls` derives
   the Confirm/Cancel gate from `confirmationPrompt model`, which reads the single
   global `pendingDeletion` slot. If an operator requests deletion of node X then
   selects node Y, Y's stack panel will show the confirm gate (the model has one
   pending slot, the view renders it per selected node). This faithfully reflects
   the frozen model's single-slot design, so it is defensible, but a later UX slice
   may want the gate scoped to the pending node.

## Risks

- **No-op edits still burn undo + flash busy.** "Up" on layer 0 dispatches
  `ReorderLayer (0, 0)` and "Down" on the last layer dispatches an out-of-range
  `ReorderLayer (index, index+1)`. `reorderLayer` (`StackEditor.fs:57-62`) is total
  and returns the system unchanged in both cases, so nothing breaks — but `editNode`
  still snapshots `undo`, marks the node `busy`, and the host fires a background
  solve for a no-op. The single-level undo slot is consumed and a busy flash shows
  for an edit that changed nothing. Cheap fix on the view side: omit "Up" at index 0
  and "Down" at the last index. Asymmetric clamping already hints at this — "Up" uses
  `max 0 (index-1)` while "Down" leans on `reorderLayer`'s OOB guard.
- **Real-time spin in the async test.** The AC-U2.1 test pumps
  `Dispatcher.UIThread.RunJobs()` in a `Thread.Sleep 10` loop up to 300 iterations
  (`ConstructionEditTests.fs:67-71`). Bounded (~3s ceiling) and it asserts the
  marshaled `NodeSolved` actually lands, which is the right thing to prove, but it is
  a wall-clock wait that could flake under a loaded CI agent. Acceptable for a
  headless async gate; worth watching if `ui-tests` ever goes intermittent.

## Bottom line

I would ship this. The effect is marshaled per §0.4, the model stays pure per §0.5,
no frozen module is edited, every `StackMsg` is wired, and both ACs have real
headless tests. The findings are a documented spec/frozen-code mismatch on which
transitions are "busy-marking" (handled the only defensible way), two evolvability
notes that belong to future slices (stale `results`, global-vs-per-node gate), and
small view-side polish (no-op Up/Down, a two-symbol `open`). None rises to a
re-spawn in my read — the judge has the gate results and decides.
