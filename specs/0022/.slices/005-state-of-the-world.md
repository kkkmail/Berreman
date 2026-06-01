# 005 — State of the world: Optical-system construction layer (Part B UI)

## Where we are

Slice 005 builds Part B's UI construction layer on top of the slice-002 beam-tree
domain, the slice-003 storage core, and the slice-004 materials. It lands the
visual stack/layer editor over the reused `OpticalSystem.films`, the incident/exit
medium selectors over `upper`/`lower`, the three-state substrate switch over the
reused `Substrate` DU, and the top-level construction page with the operator-facing
UX for destructive structural edits (live descendant-count gate, node-granularity
busy, results refresh, single-level undo). It fills the field-level shapes of the
`beamNode`/`beamBranch`/`opticalSystem`/`layer` schema `$defs` and proves the
beam-tree-and-systems JSON round-trip (AC-B9). Per the §0/P3 testability mandate the
edit model and the page `update` are Avalonia-free, so every acceptance criterion is
unit-tested without a UI; the FuncUI view that binds these cores to controls is
deferred to a later UI-wiring slice (the project keeps the public MIT
`Avalonia.FuncUI` NuGet reference from slice 001; the audit-gated clone stays
unreferenced). No engine type is forked: per-node solving routes through the reused
`OpticalSystemSolver` via `BeamTree.solve`, edits are record copy-and-update over the
engine `Layer`/`OpticalSystem` records, and the medium selectors consume the resolved
`OpticalProperties` from slice 004's `resolveMaterial` seam.

## What's working

- Add the stack/layer editor (`StackEditor.fs`): add/delete/reorder/duplicate/group as pure, immutable transforms producing a new `films : List<Layer>`, plus layer/system orientation reusing the engine `Layer.rotate`/`OpticalSystem.rotate` — engine records never mutated (AC-B5).
- Add the incident/exit medium selectors that set the reused `upper`/`lower` `OpticalProperties`, preserving `films`/`substrate`, resolving a medium by refractive index (engine `fromRefractionIndex`) or by the slice-004 by-id `resolveMaterial` seam (AC-B6).
- Add the three-state substrate switch (thin film / plate / wedge) toggling `OpticalSystem.substrate` to `None`/`Some (Plate _)`/`Some (Wedge _)`; re-solving routes unchanged through the existing `OpticalSystemSolver` multi-reflection branch (AC-B7).
- Add the construction page (`ConstructionPage.fs`): per-node solving through the reused solver (AC-B1), a live-derived descendant-count pre-confirmation gate, node-granularity busy with refresh from the new `EmFieldSystem`, single-level undo, opposing attach action reusing `BeamNode.attach`, a default-landing nav entry, and a committable JSON project path defaulting to the working folder (never the repo root, never `.binz`) (AC-B10).
- Fill the `beamNode`/`beamBranch`/`opticalSystem`/`layer` schema `$defs`; the beam tree and its systems round-trip through the JSON-canonical `OpticalConstructorProject`, schema-validated on load, with no FsPickler/`.binz` write (AC-B9).
- Extend `ProjectJsonRoundtripTests.fs` and add `StackEditTests.fs`: constructor-unit-tests rise 43 → 66; build green at Release/x64; `BerremanTests` held at 70.

## Tests

- `build` — PASS (exit 0, "Build succeeded.", 0 Error(s); no lowercase `error`). Log `.artifacts/005-build.log`.
- `unit-tests` — PASS (exit 0, Passed 70 / Skipped 5 / Total 75; `BerremanTests` untouched). Baseline `berreman_unit_tests = 70` held. Log `.artifacts/005-unit-tests.log`.
- `constructor-unit-tests` — PASS (exit 0, Passed 66 / Total 66; up from slice-004 baseline 43). Log `.artifacts/005-constructor-unit-tests.log`.

Coverage: AC-B5 (`StackEditTests` — add/delete/reorder/duplicate/group/rotate yield
the expected `films` with retained engine records reference-identical, source
unmutated); AC-B6 (medium selectors set `upper`/`lower` preserving `films`/`substrate`;
by-refractive-index and by-material-library resolution, unknown id → `Error
(UnknownMaterialId _)`); AC-B7 (switch sets `None`/`Plate`/`Wedge`; a Plate substrate
yields a `Multiple` solution and the page's solve matches the direct
`OpticalSystemSolver`); AC-B1 (page `solveNode` reflected/transmitted equal the direct
`OpticalSystemSolver(...).solution.emSys`); AC-B9 (rich beam tree + Plate/Wedge systems
round-trip, schema-validated, no `.binz`; a corrupted `incident` is rejected on load by
the filled `beamNode` `$def`); AC-B10 (live descendant count, gate open/confirm/cancel,
one-click childless delete, root not deletable, busy→refresh, single-level undo,
opposing attach, default-landing nav entry, committable JSON path). None deferred.

## Architecture

- **Avalonia-free MVU cores (P3).** `StackEditor` is a pure edit seam: a `StackMsg` DU and `applyStackMsg : StackMsg -> OpticalSystem -> OpticalSystem`, every operation a record copy-and-update over `films`. `ConstructionPage` is a pure `Model`/`Msg`/`update` with no Avalonia type, so the descendant-count gate, busy set, results map, and undo snapshot are all unit-tested headlessly. The FuncUI view is a thin later step that binds these — no model state lives in the view.
- **Node addressing by branch path.** A node is addressed by `NodePath = BeamBranch list` (`[]` = root). `BeamBranch` is a comparable fieldless DU, so `NodePath` keys the per-node `busy : Set` and `results : Map`. `descendantCount` folds the children map live on every delete request — never a cached count (§B.10 item 3).
- **Reused solver, never forked.** `ConstructionPage.solveNode` = `BeamTree.solve` = `OpticalSystemSolver(node.incident, node.system).solution.emSys`. A structural edit marks only the affected sub-tree busy and re-solves it; the page never owns a parallel solver loop (§B.1 scope exclusion: no cache/memoization/retry shim).
- **Schema `$defs` describe the real serialized shape.** The filled `beamNode`/`beamBranch`/`opticalSystem`/`layer` `$defs` match the FSharp.SystemTextJson output exactly — including `children` serialized as an **array of `[branch, node]` pairs** (a DU map key cannot be a JSON property name) and the `Case`/`Fields` adjacent-tag encoding for `Thickness`/`substrate`/`element`/`waveLength`. They stay permissive on the DU-tagged `element`/`opticalProperties` (left as the slice-003 `true` anchors, not in R-5's fill list) so validate-on-load never rejects a legitimate document.

## Deferred

- The FuncUI `view` bodies (binding the stack editor / page cores to Avalonia controls) and the application shell that hosts the nav entry — a later UI-wiring slice. P3 keeps the model/`update` Avalonia-free; this slice provides the testable cores and the presentation-data helpers (`layerRowLabels`, `confirmationPrompt`, `isNodeBusy`, `canUndo`).
- The [Standard] repeat/period builder — Part J §J.2 (slice 014).
- Curved elements (lenses / non-flat mirrors) — Part C (slice 006).
- Multi-level undo/redo history — Part I §I.6 (slice 013); only single-level undo lands here.
- Index→tensor material construction / dispersion resolution — Part D (slice 004); consumed here via `resolveMaterial`, never re-resolved.
- Schema versioning, autosave, recent-files, undo/redo persistence — out of scope (§B.9 / 010 §7).
- The `opticalProperties`/`constructorElement` `$defs` remain the permissive slice-003 `true` anchors (not in R-5's fill list); a later slice may tighten them if a directive calls for it.

## Architecture decisions

(see **Architecture** above.)

## Gotchas

- **`children` serializes as an array of pairs, not an object.** Because `Map<BeamBranch, BeamNode>` has a DU key, FSharp.SystemTextJson emits `[[ "Reflected", node ], [ "Transmitted", node ]]`. The filled `beamNode` `$def` models this with `prefixItems` + `$ref beamBranch`/`beamNode`; an object-shaped `children` would (correctly) fail validation. The ground-truth shape was captured empirically before authoring the schema.
- **`StackEditor.SubstrateChoice` case names are `AsThinFilm`/`AsPlate`/`AsWedge`** to avoid clashing with the engine `Substrate` cases `Plate`/`Wedge` (`Media.fs:40`); `applySubstrate` maps them to the engine DU.
- **"Group" is a contiguous-gather, not a repeat/period.** §B.5 lists "group" with no precise definition and `films` is a flat `List<Layer>`; `groupLayers` gathers the selected indices into a contiguous block at the first selected index (a pure new `films` list). The [Standard] repeat/period builder is explicitly Part J §J.2, not this.
- **Module-vs-type name shadowing.** `BeamTree`, `MaterialLibrary`, and `Project` are each both a module and a record type; call the module lets unqualified after `open` (`solve`, `standard`) — `BeamTree.solve`/`MaterialLibrary.standard` resolve to the *type* and do not compile.
- **`OpticalSystem.rotate` reverses film order** (`Media.fs:81-86`); `StackEditor.rotateSystem` is a thin reuse of it, so a whole-system rotation is not a pure per-row map. The per-layer `rotateLayer` (reusing `Layer.rotate`) preserves order and is the row-level control.
- The prior slice-004 hand-off risks (schema↔serializer tag-shape mismatch for `dispersionModel`; closure-vs-`DispersionModel` round-trip gap) are NOT touched here — no `materials` field is serialized through the aggregate in this slice, so those remain deferred to the materials-wiring slice.

## Changelog

- 2026-05-31 (slice 005): Add the Part B UI construction layer — `StackEditor.fs` (pure add/delete/reorder/duplicate/group/rotate transforms over `OpticalSystem.films`, incident/exit medium selectors, three-state substrate switch, default-unit display) and `ConstructionPage.fs` (Avalonia-free page MVU: per-node solve via the reused `OpticalSystemSolver`, live descendant-count gate, node-granularity busy + results refresh, single-level undo, opposing attach, nav entry, committable JSON path). Fill the `beamNode`/`beamBranch`/`opticalSystem`/`layer` schema `$defs`; extend `ProjectJsonRoundtripTests.fs` for the AC-B9 beam-tree round-trip and add `StackEditTests.fs`. All three gates green: build 0 errors, unit-tests 70 held, constructor-unit-tests 43 → 66.

```yaml
gates:
  berreman_unit_tests:    70
  constructor_unit_tests: 66
```
