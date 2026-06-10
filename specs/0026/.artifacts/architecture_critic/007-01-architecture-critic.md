# Architecture critique -- 007.slice-md cycle 2

## Summary

The re-spawn did exactly what cycle 1 steered, and both deciding findings are genuinely
closed with regression tests: `editActive` now routes through the worker's own
`commitIfChanged` (`ConstructorView.fs:208`) so a locked-axis rotation pushes no empty undo
step, and `GroupsLibrary.validate` calls the now-public `SchemaValidation.collectMessages`
(`GroupsLibrary.fs:66`) so nested groups-file errors keep their `results.Details` location.
One residual deserves a look: the no-op guard is **not** as uniform as the SoW claims --
`resetRotationNow` still commits unconditionally and can push an empty undo step on an
already-zero element, the same class as the finding just fixed. Nothing here binds the
verdict.

## Layering

Still clean and directional, with no new violations. `ConstructorView` now `open`s
`OpticalConstructor.Storage` for `History.EditHistory` (UI -> Storage, correct), and
`Ribbon` `open`s `OpticalConstructor.Domain` for `Groups` (UI -> Domain, correct). The model
carries `History.EditHistory` (Storage) and `Groups.ElementGroup list` (Domain), both pure,
serializable values with no Avalonia handle, so 0.3 holds. `Groups.fs` is pure Domain over
reused `ElementPlacement` (no parallel catalogue, 0.1); `GroupsLibrary.fs` is Storage over
Domain + `ProjectJson`. No reverse edges.

## Consistency

- **The no-op guard is not actually uniform -- a residual of the fixed finding.** The SoW
  asserts "The guard is now UNIFORM," but only `editActive`, the arrow-key slide and the
  slide drag route through `commitIfChanged`. `resetRotationNow` (`ConstructorView.fs:350`)
  and `setPrimaryDetectorNow` (`:435`) still call the unconditional `commit`. Reset-rotation
  is confirmation-gated but armed regardless of the element's current rotation, so confirming
  it on an already-zero element pushes an identical snapshot -- exactly the empty-`Ctrl+Z`
  step the cycle-1 fix removed, on a different path. The smallest fix is the same one: route
  `resetRotationNow` through `commitIfChanged`. (`setPrimaryDetectorNow` only no-ops in the
  narrow case where the primary detector already sits at placement index 0, since
  `moveToFront` on a non-zero index does reorder; lower priority.)
- **Dead `experiment.collections` string carried from cycle 1.** It is still defined
  (`strings.json:132`) yet referenced nowhere -- `experimentView` surfaces groups and a
  detector summary but no collection on/off control. Collections have a Domain + Storage +
  round-trip shape with no front-door surface; the leftover label confirms one was
  anticipated.
- **Minor: the detector summary leaks an internal index.** `experimentView` prints the
  primary's *placement* index as `#%d` (`Ribbon.fs:309`), so the user-facing "Primary #N"
  shows a `project.placements` position, not a 1-based detector number -- mildly confusing in
  a multi-element table.

## Spec fit

- Both judge-required disclosures landed: table resize having no UI surface to wire is now in
  Gotchas, and the redundant in-beam state is reframed in Deferred as a real
  duplicate-placement hazard rather than benign durability. That was the substance of the
  re-spawn beyond the two code fixes, and it is honest.
- Part H collections remain Domain + Storage only (AC-H1 is phrased around the round-trip,
  which `GroupsRoundTripTests` discharges); the absence of an on/off surface is within a
  reasonable reading, as cycle 1 noted.
- Live `GroupsLibrary.load`/`save` wiring is still deferred (disclosed): the app-data path
  and IO are tested but not booted, so groups are in-session only this slice.

## Evolvability

The carried duplicate-placement hazard is the one thing the live-wiring slice must resolve,
and the worker now records it. One concrete facet worth folding into that note: it is not
only undo-desync. `groupActiveElementNow` (`:495`) snapshots the active element's placement
**by value** into the group member; if the user then moves or rotates that on-table element
(mutating `project.placements[i]`), the member's stored `placement` goes stale, and
`syncGroupChange`'s `removeFirst` value-equality match (`:466`) will no longer find it on a
toggle-out -- removing nothing, or the wrong equal-valued placement. So *both* a project undo
and any post-creation edit desync the two stores. Keying members to placements by id rather
than mirroring a bool + matching by value dissolves all three symptoms at once. By contrast,
the primary-detector-as-placement-order choice remains a good evolvability call: it avoids a
new `OpticalConstructorProject` field and keeps the slice-001/004 anchors stable.

## Risks

- **`resetRotationNow` empty-snapshot push** (Consistency above): user-visible as a `Ctrl+Z`
  that does nothing once; not data loss.
- **The between-edits invariant is deliberately broken during a `SlideDrag`.** `SlideTo`
  mutates `project` without committing and `EndDrag` commits once (`:790`) -- correct for
  "a drag is one undo step." But the `commit`/`commitIfChanged` precondition
  (`history.present = project`) is false mid-drag, so any edit dispatched while a drag is live
  (e.g. a wheel-rotate through `editActive`) would commit against the pre-drag baseline and
  bundle the in-flight slide into a single step. Avalonia pointer capture makes this
  interleaving unlikely; flagging the implicit ordering assumption.
- **`setPrimaryDetectorNow` remaps `rayOf` and `selection` but not `pending`/`drag`**, which
  also carry placement indices. Safe today because the confirm gate is modal and set-primary
  is not dispatched mid-drag, but it is an implicit invariant if those ever become concurrent.

## Bottom line

Ship-leaning. The re-spawn closed both findings the cycle-1 judge steered on, each with a
regression test (the locked-axis no-op guard confirmed red->green; the nested-error-detail
test), genuinely reused the spec-named validate seam by exposing `collectMessages`, and
landed both disclosure gaps. Gates are green and EOL is clean (`git diff --numstat` equals
`--ignore-cr-at-eol` for every source file; only the supervisor-owned `.manifest.state.json`
differs). The residual `resetRotationNow` no-op push is the same class as the fixed finding
and a one-line follow-up, not a re-spawn blocker, and the in-beam redundancy is correctly
carried per the judge's explicit "do not redesign this cycle." I would pass it, leaving the
`resetRotationNow` guard, the dead `experiment.collections` string, and the
member-by-id rekey as cheap cleanups for whoever opens the live-wiring slice. The judge
decides.
