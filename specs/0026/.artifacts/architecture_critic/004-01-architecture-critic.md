# Architecture critique -- 004.slice-md cycle 1

## Summary

A clean, correctly-layered pure-geometry slice with one or two minor
consistency gaps. The pure-projection split (table-frame `TablePoint` out of
the drawer, `DrawPoint` after the view transform), the additive
schema/record extension, and the CTA consolidation all fit the surrounding
code and reuse the engine seams instead of re-typing them. The most
actionable finding: the source/detector role shades and the toggle accent
colour are styled with ad-hoc literals that contradict the slice's own
"single restyle seam" and "matches the existing SteelBlue toggle" claims.

## Separation of concerns

- The drawer reconstructs a throwaway `ElementPlacement`
  (`Drawer.fs:111-112`: `ElementPlacement.create kind point` then
  `{ p0 with r1 = …; r2 = …; r3 = …; valueId = … }`) solely to reach
  `defaultBox kind` + `orientedBasis`. Reusing slice-001's rotation law
  rather than re-deriving it is exactly right (constraint 0.1), but the
  reconstruction is an awkward seam: it can only ever render the *default*
  box for the kind, never an element's actual `box`, and it side-steps the
  lock-respecting setters (correct for a renderer, and documented).
  Acceptable now — see Evolvability.
- Styling ownership is split. `ConstructorTable.fs` is declared (SoW
  "Architecture") as the home of "every named drawing-weight/colour
  constant", and the default interior correctly pulls from it
  (`Drawer.fs:42-43`). But the two *required* role departures — `sourceShade`
  and `detectorShade` — are hard-coded literals in the drawer
  (`Drawer.fs:46`, `:50`: `rgb 90 90 90`/0.55, `rgb 15 15 15`/0.90).
  Restyling a role look means editing `Drawer.fs`, not the constants block,
  which undercuts the single-seam claim.

## Consistency

- `accentColor` / `toggleOnColor` (`Controls.fs:68`, `:103`) are documented
  as "a steel blue, matching the active-toggle colour the existing views use
  (`Button.background Brushes.SteelBlue`)", but the value is `rgb 33 118 174`
  (#2176AE), whereas `Brushes.SteelBlue` is #4682B4 (the established toggle
  hue at `MaterialsView.fs:147`, `ResultsView.fs:189`, `SourceView.fs:108`,
  `Shell.fs:504`). The CTA rewire in this same module was scrupulously
  hue-preserving (SeaGreen/IndianRed unchanged, `Controls.fs:48,51`); the
  toggle flavour quietly introduces a *different* blue while claiming a
  match, so a later migration of those toggles to `Controls.toggle` would
  visibly restyle them. Either correct the comment or use the real SteelBlue
  bytes.
- The private `rgb` helper is now triplicated verbatim (`Schematic.fs:45`,
  `ConstructorTable.fs:43`, `Drawer.fs:29`). Trivial, but since these are all
  in `OpticalConstructor.Ui`, one shared internal helper would do.

## Spec fit

Strong. AC-C1/C4/C5/C6/J1 each have a matching headless assertion and the
gate counts moved as the SoW claims. Two within-spec observations worth
recording, neither a defect: (1) the active-element "indicator" is delivered
as two constants + the WCAG `contrastRatio` proof (`ConstructorTable.fs:107`,
`:111`, `:127`) with no function emitting the halo/frame geometry —
defensible because this slice is explicitly pure-values-only and the binding
is slice 005, but the producer is absent; (2) the drawer renders the default
box per kind, not an arbitrary placement's box. Both are consistent with the
declared scope. No scope creep: the `ConstructionView.fs` edit is the
J.2/R-9-sanctioned two-line CTA rewire and nothing more.

## Evolvability

- The drawer's loose signature (`point, r1, r2, r3, kind, valueId, showBox`)
  is spec-mandated (C.4.1), but it is exactly the field list
  `ElementPlacement` already bundles. Slice 005 will hold real
  `ElementPlacement` values, must decompose them to call `draw`, and `draw`
  then reconstructs a placement — a round-trip through the same fields. A
  thin `drawPlacement (p : ElementPlacement) (showBox : bool)` overload
  forwarding to `draw` (and reading `p.box`) would smooth that seam and let
  the drawer honour a real box later without a signature change.
- The active-element seam is implicit: slice 005 is expected to stroke the
  drawer's `frame` points with `activeIndicatorColor` /
  `activeIndicatorWeightPx`. That works, but nothing names it; a one-line
  `activeIndicatorStroke : Stroke` next to the other strokes in
  `ConstructorTable.fs` would make the intended consumer obvious.

## Risks

- The root `table` property is optional in the schema
  (`required: ["schemaVersion", "beamTree", "systems"]`,
  `optical-constructor-project.schema.json:7`) but mandatory in the record,
  so a hand-authored JSON omitting `table` passes schema validation yet fails
  F# binding with a generic parse error. This exactly mirrors the slice-001
  `placements` precedent and is harmless on the blank-slate / no-migration
  path (0.6), so it is a deliberate consistency call, not a regression —
  flagged only so the judge sees it is intentional and matches precedent.
- The new flavour *functions* (`button` / `toggle` / `numericWithUnits` /
  `destructiveGate`, `Controls.fs:113-182`) have no caller and no test; only
  the pure `ButtonFlavor` records are asserted. The build gate catches DSL
  type errors, but their runtime render shape is unexercised until slice 005
  wires them — a small latent risk, not blocking.

## Bottom line

I would ship this. It is a clean, correctly-layered pure-geometry slice that
reuses the engine seams (`orientedBasis`, `TablePoint`, `SchematicColor`, the
units spine) instead of re-typing them, the schema/record extension is purely
additive and faithful to the slice-001 precedent, and every acceptance
criterion has a headless proof. The findings are all minor and non-blocking;
the strongest is the styling-centralization gap (role shades plus the
SteelBlue-mismatched accent live as ad-hoc literals despite the "single
restyle seam" claim), which a future restyle would trip over. None gate the
slice — the judge can fold them into the next cycle or wave them through.
