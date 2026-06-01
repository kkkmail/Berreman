# Architecture critique -- 014.slice-md cycle 1

## Summary

Clean, faithful tranche: four pure Avalonia-free seams plus a minimal MVU
wiring edit, every one delegating real work to the correct owning seam
(003 `deserializeProject`, 004 `resolveMaterial`, 012 `plotComparison` via
Part H §H.4). No layering violations, strong consistency with the
established "pure core, deferred view" precedent, and 17 new tests that
exercise the delegations rather than asserting shape. The one judgment call
worth the judge's attention is not a defect in this slice but a trajectory
one: every Avalonia *view/event* binding (the J.1 `Canvas`, the J.4
`DragDrop` handlers) is again deferred to a "later UI-wiring slice" that no
remaining slice in this arc (015, 016) actually schedules.

## Separation of concerns

R-4's drag-drop wiring lands in the existing `StackEditor.fs`
(`SetLayerMaterial` case + `setLayerMaterial`/`layerMaterialDrop`) rather
than a new file, which puts it outside the slice's four-new-file "Files in
scope" list. This is the right call and the SoW Gotchas owns it: R-4 is a
wiring requirement into the *existing* owning stack-editor MVU, and that
module already imports the slice-004 resolver. Inventing a new module to
hold a single `StackMsg` case would have fragmented the editor's update.
Each of the four new modules holds one concern (geometry / expansion /
factories / selection state) with no bleed.

## Consistency

Excellent. `Schematic.curated` keys (`silicon`, `langasite`, `glass-1.52`,
`glass-1.50`, `glass-1.75`, `glass-2.00`, `uniaxial-crystal`,
`biaxial-crystal`) match `MaterialLibrary.builtInEntries` ids one-for-one
(`MaterialLibrary.fs:82-131`) — no dead curated colours. The FNV-1a fallback
(`Schematic.fs:72-73`) is the deliberate, documented avoidance of
`String.GetHashCode`'s per-process seed, correctly satisfying R-1 item 6's
"same colour across redraws," and the mask `&&& 0x7fffffff` keeps the
palette index non-negative. `loadTemplate` (`Templates.fs:156-159`) routes
through `ProjectJson.deserializeProject`, which is exactly the validate-on-
load core `openProject` runs (`ProjectFile.fs:38`) minus the disk read — a
faithful in-memory equivalent, not a private deserialize. One trivial nit:
`Schematic.layout`'s parameter `unit : UnitOfMeasure` (`Schematic.fs:198`)
shadows the F# `unit` type; harmless but a rename (`displayUnit`) would read
cleaner.

## Spec fit

All five acceptance cores land and are provable headless. AC-J2 is the spec's
literal `List.replicate count cell |> List.concat` with no `Validation.fs`
reference (the forward-reference discipline is honoured exactly). AC-J3's DBR
films are provably `RepeatBuilder.expand dbrCell dbrPeriods`
(`Templates.fs:126`, `TemplatesTests.fs:44-49`). AC-J5 delegates to
`SeriesData.seriesComparison` / `Plot1DView.renderComparison`, which reuse
`Charting.plotComparison`'s per-system layout with no Part-J-local overlay
(`SeriesData.fs:60`, `Plot1DView.fs:62`). The under-delivery to flag — and it
is consistent, not negligent — is that AC-J1 ("MUST render each film band...
draw the ray... within one MVU `view` re-evaluation") and AC-J4 ("MUST wire
Avalonia drag-and-drop") are phrased as user-facing rendering/wiring, yet only
the geometry projection and the MVU message/update land; the `Canvas` view and
the `DragDrop` event handlers are deferred. The SoW is candid about this and
ties it to the whole Ui tree having no FuncUI DSL yet plus the headless-gate
constraint, so it is defensible — but the literal AC-J1/AC-J4 acceptance is
satisfied only at the seam, not at the surface.

## Evolvability

Two forward risks, neither blocking:

1. **The deferred view binding is accumulating with no scheduled home.**
   Slice 014 is the third+ Ui slice to push "the actual control binding" to
   a "later UI-wiring slice." But the remaining arc slices are 015
   (favorites/preferences/theme/validation) and 016 (job runner/help/3D
   viewport) — neither names a FuncUI host that renders the schematic
   `Canvas` or attaches the `DragDrop` handlers. If no slice in 0022 binds
   these, the "UX shell" arc closes with every UX surface headless. This may
   be by design (a separate host arc), but the judge should confirm the
   intent rather than let the deferral compound silently.

2. **R-1 item 6's colour-by-id has no data source in the model yet.**
   `Schematic.layout` takes `materialKey : int -> string` (`Schematic.fs:198`)
   to externalise the film→materialEntry-id lookup, but neither the engine
   `Layer` (properties + thickness only) nor `OpticalConstructorProject`
   (`Project.fs:26-35`, systems + sources, no per-layer assignment) stores
   which `materialEntry` a film came from. The colour function is correct and
   deterministic, but at view-binding time the caller has no stored key to
   pass. R-1 item 6 is thus implementable but not yet *drivable*; whichever
   slice binds the view will need a film→id mapping that the §A.7 model does
   not currently carry.

## Risks

- `bandHeight` for a 1 mm film returns ~2.0e5 px (`pixelsPerMeter = 2.0e8`,
  `Schematic.fs:106,115`); only monotonicity is required and tested, and the
  SoW notes normalisation is the deferred view's job — but that normalisation
  is entirely untested and the raw scale is unusable for any real canvas, so
  the layout pass it depends on carries unmeasured risk into the binding slice.
- `comparisonSeries` / `renderOverlay` do not gate on `canOverlay` (≥2
  visible) before delegating (`Workspace.fs:95-114`); they will overlay a
  single system. AC-J5's "two or more" is enforced only if the caller checks
  `canOverlay` first. Minor — the predicate exists — but the contract lives in
  the caller, not the seam.

## Bottom line

I would ship this. The cores are faithful, the delegations are exactly the
owned seams (verified against 003/004/012 and Part H), the tests assert
behaviour over shape, and the cross-slice forward-reference discipline is
clean. The deferred view binding and the absent film→id key are real but are
trajectory concerns for slices 015/016, not defects in 014's deliverable —
and the SoW discloses both honestly. I have no gate authority; my read is
green with a note to the judge to confirm where the accumulating UI-host
binding is meant to land before the arc closes.
