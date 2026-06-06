# Architecture critique -- 001.slice-md cycle 1

## Summary

A clean, well-layered slice. The pure-headless `Placement.fs` is exactly
the right shape: it reuses the engine `Angle` (`Geometry.fs:34`) and the
`Units` spine, keeps every magnitude in canonical meters, and the geometry
math (`orientedBasis`, `tableFrameOrientation`) provably satisfies AC-A1,
AC-A3 and AC-A4. The single most important note is an *evolvability* one,
not a defect: the new `placements` list hangs off the project aggregate
with no key tying a placement to its beam-tree node, and `CatalogueKind`
duplicates the node's `ConstructorElement` -- a correspondence slices 002
and 004 will have to retrofit, possibly forcing a change to the very
placement anchors the hand-off says to keep stable.

## Separation of concerns

`placeholderSampleSystem` (Placement.fs:304-311) puts a physics stand-in
(vacuum `OpticalSystem`) inside what is otherwise a pure geometry/catalogue
module. It is forced by the DU shape -- `ConstructorElement.Sample of
OpticalSystem` (BeamTree.fs:38) requires a system, so `toConstructorElement`
must supply one to stay total and pure -- and the SoW Gotchas flag it as a
stand-in. Acceptable as-is; just note that "what stack does a Sample carry"
is a Part-F concern that has leaked one module early, and a future reader
could mistake the vacuum placeholder for real physics. No change needed this
slice.

## Consistency

Strong fit with the surrounding code: `Angle` reused rather than re-derived
(R-9), the `Vector3`/`float<meter>` choice documented against the Math.NET
`RealVector3` serialization limitation (constraint 0.3), the fsproj compile
order correct (`Placement.fs` after `BeamTree.fs`, before `Project.fs`), and
the schema's bare-number/bare-string shapes matched to the
FSharp.SystemTextJson options in `ProjectJson.fs:70-80`. One small
asymmetry: the emission setters are `static member`s on `Emission`
(Placement.fs:126-138) while the rotation setters are module-level functions
(`withR1`/`setR1Locked`, Placement.fs:215-234). Both patterns exist in the
codebase (`BeamNode.attach` vs `solve`), so this is stylistic only.

## Spec fit

In scope and complete for the four owned files, but one scoping claim is now
false. The slice's own testing plan asserts "ui-smoke / ui-tests do not
apply: this slice touches no `OpticalConstructor.Ui` or
`OpticalConstructor.Ui.Tests` file." The diff edits both
`OpticalConstructor.Ui/Templates.fs` and
`OpticalConstructor.Ui.Tests/SystemView3DTests.fs` (the `placements = []`
sweep). The edits are mechanical and the `build` gate (whole-`.slnx`)
compiles them, so correctness is covered -- but the statement is stale and
the SoW reports the collateral as "8 sites" without disclosing that two are
UI-project files. In F# a mandatory record field unavoidably breaks every
literal construction, so the sweep itself is correct; the gap is
transparency, and whether the judge wants the `ui-tests` gate exercised
given the UI projects were in fact touched. Recommend the SoW name the seven
collateral files and correct the "no UI file" claim.

## Evolvability

The main forward-looking concern. `OpticalConstructorProject` now carries
`placements : ElementPlacement list` (Project.fs:42) *alongside* `beamTree`,
with no field linking a placement to the `BeamNode` it places, and each
`ElementPlacement` carries its own `catalogueKind` that re-states the kind
already encoded in that node's `ConstructorElement`. Nothing can currently
enforce that a placement and its node agree, and the beam tree is a *tree*
while `placements` is a *flat list*, so even positional correspondence is
ambiguous. Slice 002 (ray routing, consumes `orientedNormals`/`emission`)
and slice 004 (table/drawer) both need "which placement belongs to which
element"; resolving it later will likely force an id/key onto either
`BeamNode` or `ElementPlacement` -- i.e. a change to the placement
schema/record shape, which is exactly what the hand-off asks to leave stable
for slice 004's table extension. This is not a slice-001 defect (the slice
was told only to reserve the slot and prove the round-trip), but it is the
shape most likely to corner a downstream slice. Suggestion for the judge:
have 002/004's design pin the element-to-placement linkage *now*, before the
schema hardens, so it does not become a retrofit across the placement
anchors.

## Risks

- **Required-in-record vs not-required-in-schema.** `placements` is a
  mandatory F# field but is absent from the schema root `required`
  (schema:7). A document lacking `placements` passes schema validation yet
  fails to bind (FSharp.SystemTextJson missing-field -> `JsonParseError`).
  This exactly mirrors the pre-existing `sources` field, so it is
  *consistent* with precedent and low-risk under the blank-slate rule
  (constraint 0.6, schemaVersion still "1.0"); flagging only so the latent
  binder/schema divergence is on record.
- **Placement snapshots in undo/redo.** Because `History.fs` snapshots the
  whole project, placements now participate in undo/redo for free -- a
  benefit, not a hazard, but it means placement edits are durably captured
  from the first build with no extra wiring (good).
- **Emission default for terminal elements.** `defaultEmission`
  (Placement.fs:168-171) gives every non-mirror element `EmitBoth`,
  including `Detector`, which physically terminates rays. The spec's R-7.2
  default rule only special-cases mirrors, so this follows the letter of the
  spec; note it as a likely Part-B refinement, not a fix.

## Bottom line

I would ship this. The owned deliverables are present, the geometry is
correct against its ACs, layering and units discipline are clean, and the
only spec contradiction (the "no UI file" claim) is a documentation/scoping
miss over mechanical, build-validated edits rather than a code fault. My one
substantive ask is architectural and forward-looking: the missing
element-to-placement linkage and the `catalogueKind`/`ConstructorElement`
duplication are a shape slices 002/004 should design against before the
placement schema hardens. None of this is gate-binding and I hold no
authority over the verdict -- I'd lean pass with a recommendation that the
SoW be corrected to name the collateral UI files and that the downstream
linkage be settled in the next slice's plan.
