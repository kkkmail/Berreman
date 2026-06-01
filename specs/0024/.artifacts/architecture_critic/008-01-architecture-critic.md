# Architecture critique -- 008.slice-md cycle 1

## Summary

Clean slice with one or two issues worth a note. The lifecycle wiring,
environment persistence, and the 2-D-orthographic 3-D view all land where the
spec asked, respect the frozen-core mandate (§0.1), and reuse the existing
schema-validated seams (no private deserialize, no clone reference). The single
finding I'd flag is an evolvability hazard: `SystemView3DView` re-derives a tree
traversal that must stay bit-for-bit in lockstep with the frozen
`placeElements`, and the coupling is enforced only by element count, not by
structure.

## Separation of concerns

`Shell.saveCmd` (`Shell.fs:359-388`) carries a fair amount of work in one `Cmd`
builder: serialize via the frozen `ConstructionPage.saveProject`, raise the save
picker, create the directory, write the file, and marshal the result back. This
is consistent with the slice's mandate to wire IO into the composition root
through existing seams rather than invent a new IO module, and it mirrors the
established `nodeSolveCmd` / `fitCmd` shape in the same file, so I would not push
it behind a new seam now. Noting it only so the next IO-heavy slice considers a
small `Shell.Io` helper module if a third writer appears.

## Consistency

Error surfacing is formatted three different ways. `openPathCmd` and the
template/gallery handlers render `StorageError` with `sprintf "%A"`
(`Shell.fs:333`, `434`, `440`), while the save write path surfaces `e.Message`
(`Shell.fs:380`), and the status prefixes vary (`"Error: "`, `"Template error: "`,
`"Gallery error: "`). It is cosmetic, but a single `ioError` formatter would keep
the toolbar status line uniform and is the kind of consistency the surrounding
view modules already hold. Low priority.

## Spec fit

Strong. AC-U8.1 (Save writes `<name>.ocproj.json`, Open round-trips the
schema-validated `ProjectFile.openProject`), AC-U8.2 (`Templates.loadTemplate` /
`Help.openEntry`), AC-U8.3 (theme/panel/dock flip `env` through the pure
`AppShell` reducers + a fire-and-forget `UserEnvironment.save`), and AC-U8.4 (a
GL-free 2-D orthographic Canvas projection) are each addressed, and the
constraints hold: the `IStorageProvider` and persist path live in host-layer
module fields, not the root model (§0.5); every off-thread continuation marshals
through `Dispatcher.UIThread.Post` (§0.4); `New` correctly does not require a
picker. The plan's "edit `ResultsView.fs` and/or add a 3-D sub-view" was
satisfied by a new sibling `SystemView3DView.fs` composed into the results panel
(`Shell.fs:544-552`) — the better of the two options under §0.1, not
under-delivery. No scope creep.

## Evolvability

The one finding I'd weight. `SystemView3DView.preorderPaths` (`SystemView3DView.fs:52`)
re-implements the pre-order, `Map.toList`-ordered walk that the frozen
`SystemView3D.placeElements`' private `go` performs (`SystemView3D.fs:96-116`),
then `List.zip placed paths` (`SystemView3DView.fs:99`) pairs the i-th placed box
with the i-th node path to look up its solved field. The two traversals match
today, so the pairing is correct. But the coupling is enforced only by equal
list length: if a future slice ever reorders `placeElements`' child iteration
(e.g. sorts branches differently, or interleaves children with siblings),
`List.zip` still succeeds with the same count while silently pairing each box
with the *wrong* node's `EmFieldSystem` — drawing beams from the wrong origin
with no error and no failing test (the existing `SystemView3DTests` only counts
`Rectangle`/`Line` instances, not their pairing). The frozen `placeElements`
already exposes `PlacedElement.element`; the lowest-risk hardening, when the
frozen seam is next opened, is to have it also carry the `NodePath` so the view
need not re-derive the order at all. Short of that, a test asserting that
`preorderPaths` and `placeElements` agree on count *and* element identity would
turn the silent mispairing into a gate failure.

## Risks

- **Picker faulted/cancelled → silent no-op.** Both `openCmd` and `saveCmd` guard
  the picker continuation with `if t.IsCompletedSuccessfully` (`Shell.fs:351`,
  `383`) and otherwise do nothing — a faulted or cancelled picker surfaces no
  `IoError` to the status line. Acceptable for this slice (the headless path is
  the tested one and a user-cancelled dialog is a no-op by design), but worth a
  follow-up so a genuine picker failure isn't swallowed.
- **Process-shared mutable statics under test parallelism.** `storageProvider`
  and `environmentPath` (`Shell.fs:51`, `66`) are module-level `mutable`s — the
  same host-field pattern slice-007's `fitCts` set, and §0.5-compliant. The SoW
  reasons through the cross-test contamination cases (only the AC-U8.3 test
  redirects `environmentPath`; the Save test clears the provider), and headless
  Avalonia serializes the UI thread, so this is low risk in practice. Flagging
  only because shared global mutables that the test suite must coordinate around
  accumulate cost as the suite grows; a per-run reset fixture would future-proof
  it.

## Bottom line

I'd ship this. The slice meets all four acceptance criteria, holds every binding
constraint (frozen core, host-held handles, UI-thread marshaling, no new
committable file type, no clone reference), and is internally consistent with the
slice-002..007 view precedent. The only finding with teeth is the
`preorderPaths`/`placeElements` traversal coupling, and it is latent — it bites a
future slice, not this one, and is invisible to the current gates. I'd let it
land and record the count+identity-pairing test (or the `NodePath`-carrying
`PlacedElement`) as a hardening item for whenever the frozen `SystemView3D` seam
is next opened. The error-formatting and silent-picker-failure notes are minor.
The judge decides; my read is ship.
