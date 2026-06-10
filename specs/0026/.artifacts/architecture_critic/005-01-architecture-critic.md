# Architecture critique -- 005.slice-md cycle 1

## Summary

A clean, well-layered slice. The constraint-0.4 lynchpin (AC-E1: one registry,
two surfaces) is genuinely realized — the key map and mouse map are `List.collect`
projections off a single `registry` list, not a second hand-maintained table — and
the new code respects the project's purity, SI, and no-clone mandates. The findings
below are minor (a context-sensitive command, ephemeral index-keyed state, latent
pan/rotate coupling, and a SoW count nit); none rise to a re-spawn.

## Layering

Direction is correct and acyclic. `Commands.fs` is pure with **no** dependency on
`UserEnvironment` or Avalonia — the configurable key map is applied through
`withKeyOverrides : (string*string) list -> CommandDef list` (Commands.fs:394), so
the override surface is plain string pairs and the compile order
(`UserEnvironment.fs:58` → `Commands.fs:119` → `ConstructorView.fs:120`, with
`ConstructorTable`/`Drawer` at 112-113) is a strict DAG. `ConstructorView` opens
only same-or-lower modules (Domain `Placement`/`Project`/`RayModel`/`Table`, Ui
`Commands`/`UserEnvironment`/`ConstructorTable`/`Drawer`/`Schematic`); no Domain
file is edited to point back at the Ui. The FuncUI reference is the public NuGet
`Avalonia.FuncUI` 1.6.0 (fsproj:158), explicitly not the audit-gated clone
(constraint 0.3 holds).

## Separation of concerns

The most notable wrinkle: the plain left-drag gesture `DragGesture(Left, ∅)` maps
to one registry command, `PanView`, but `BeginDrag` (ConstructorView.fs:508-512)
reinterprets it as *either* a pan (empty table) *or* the inert+hint guard (over an
element, E.4.4). So a command named "PanView" carries a second responsibility the
name does not advertise. This is defensible — AC-E1 still holds (one gesture → one
command; the hit-test, not a second binding, disambiguates) and the SoW flags it —
but a future reader will look for an "inert-drag" command and not find one. A
one-line rename or a doc-comment on the `PanView` case pointing at the E.4.4 split
would remove the surprise. Otherwise the pure/boundary split is good: lines 1-577
are Avalonia-free `Model`/`Msg`/`update`, 578-774 the `Canvas` view + event
translation, mirroring `ConstructionPage` exactly.

## Consistency

Strong fit with the surrounding code. Serialization reuses the reflection-based
`JsonSerializer.SerializeToNode(settings, ProjectJson.options)` path
(UserEnvironment.fs:302), so adding the `keyMap` record field auto-serializes with
no second JSON stack — identical to how slice 003 added `language`; the schema gains
a matching `keyMap`/`keyBindingOverride` `$defs` pair and a `required` entry, so
validate-on-load stays honest. Gesture value shapes are pure comparable DUs that key
a `Map`, matching the project's pure-geometry idiom. F# conventions (PascalCase
types, camelCase locals, 4-space indent) are observed.

One documentation nit, not a code defect: the SoW changelog line reads "19 `ui-tests`
+ 1 `ui-smoke`", but the "What's working" section says 20 and the file actually
declares **20** `[<Trait("Category","ui-tests")>]` facts plus one `ui-smoke`
(consistent with the reported 44 → 64 delta). The "19" is a stale number.

## Spec fit

All seven owned ACs (E1-E5, C2, C3) have direct tests against the pure `update`, and
the deferred items (ribbon shell, group on/off behaviour, multi-level history,
detector *Set as primary*) are correctly absent and documented — no scope creep, no
parallel catalogue/solver/project (constraint 0.1), canonical `<meter>` throughout
(constraint 0.2). Two small approximations worth recording, both defensible under the
schematic mandate (0.5):

- `RibbonDrop` ignores its drop `_point` (ConstructorView.fs:546) and snaps to
  `centralRayMiddle`. AC-E4 says "middle of the *closest* central-ray path"; with a
  single source/detector there is exactly one CR, so "closest" is trivial and the
  literal "middle" is honoured. Fine today; if multi-source CRs ever exist, the drop
  point becomes load-bearing.
- `slideBounds`/`slideActiveToX` constrain and move only along **x**
  (ConstructorView.fs:214-244). For an element reassigned to a reflected/transmitted
  branch (`rayOf`), "slide along its ray" is then an x-axis approximation rather than
  a true along-branch slide. Acceptable for a 2-D schematic; note it so a later
  physical-grade slice does not assume exact along-ray motion.

## Evolvability

The registry is a real single extension point: slice 006 (ribbon) reads
`contextMenuCommands`/`idOf` and projects from `registry`; slice 007 adds the
group-toggle/`Set as primary` cases in that one list — no second wiring site, as the
hand-off requires. The `lastCommand` field is a clean forward-hook for the ribbon.
Single-level undo via `undo`/`redo : OpticalConstructorProject option`
(ConstructorView.fs:98-99) is the documented stand-in for slice 007's `EditHistory`;
the swap is localized to the `Model` fields and the `snapshot` helper, so slice 007
is not cornered.

One latent coupling to flag for slice 006: pan is stored in the pre-rotation
projected frame (`ConstructorTable.project` adds `panX/panY` *before* `projectToCanvas`
applies the view R1), and `PanByScreen` adds raw screen-pixel deltas straight onto
`panX/panY`. With `view.r1 = 0` everywhere this slice (no gesture sets view rotation
yet — correctly noted in the SoW), `fromScreen`/`projectToCanvas` are exact inverses
and everything is correct. But the moment slice 006 wires a view-rotate control, a
screen-space pan delta will need un-rotating before it is added, and pan will appear
to "tumble" with the view. Worth a comment at the `panBy` seam so that future change
is not a silent regression.

A smaller note: `applyCommand` no-ops the gesture-driven cases
(`RotateR1|...|PlaceFromRibbon -> m`, ConstructorView.fs:419-420), so a slice-006
ribbon that tries `Invoke RotateR1` gets a silent nothing. The code comments say so,
but a future caller would benefit from those cases being unrepresentable in `Invoke`
(e.g. a separate "parameterless command" type) rather than silently swallowed.

## Risks

- **Index-keyed ephemeral state.** `selection = ElementSelected of int` and
  `rayOf : Map<int, RayId>` are keyed by list position. `deleteNow` re-keys both and
  resets selection (ConstructorView.fs:295-306), but `Undo`/`Redo` swap whole
  projects without revalidating either; a stale `ElementSelected i` is saved only by
  the `activeIndex` bounds guard, and restored indices lose their `rayOf` entry
  (defaulting to `CentralRay`). Low impact because `rayOf` is explicitly ephemeral
  and undocumented-as-persisted, but any future reorder/insert op must remember to
  re-key. A stable per-element id would retire this whole class of bug when slice 007
  touches history.
- **Module-level `mutable lastPointer`** (ConstructorView.fs:595) is shared across
  any second mount of the page. This mirrors the established `Shell.fs` host-field
  pattern (`fitCts`, `storageProvider`), so it is consistent for a single-window app;
  flagged only so a future multi-document surface does not inherit it unexamined.

## Bottom line

I would ship this without a re-spawn. The slice delivers its hardest requirement
(the single-source registry) honestly, layers cleanly, reuses the engine/units/
settings spines rather than re-typing them, and keeps the MVU model pure and
serializable. The open items are a naming/clarity wrinkle (`PanView` double duty), a
documentation count nit in the SoW, and two latent couplings (pan-vs-view-rotation,
index-keyed ephemeral state) that are inert this slice and should be picked up when
slices 006/007 add the view-rotate gesture and the history stack. None of these
binds the verdict — that is the judge's call — but my read is green.
