# 005 — Impl plan: Optical-system construction layer (Part B UI)

## Approach

Slice 005 builds the Part B UI construction layer over the reused engine
stack types. Per the §0/P3 testability mandate, every requirement is realized
as an **Avalonia-free pure MVU core** (model + messages + `update` + edit
transforms) so the acceptance criteria are unit-testable without a UI; thin
FuncUI `view` functions render those cores so the editor/selectors/switch/page
exist as real surfaces. Nothing forks the engine: solving routes through the
reused `OpticalSystemSolver`, edits are record copy-and-update over
`OpticalSystem`, and the resolved `OpticalProperties` is consumed (never
re-derived) via slice-004's `resolveMaterial` seam.

## Files

New:
- `OpticalConstructor.Ui/StackEditor.fs` — pure `StackEdit` transforms over
  `OpticalSystem.films` (add/delete/reorder/duplicate/group), incident/exit
  medium selectors (`upper`/`lower`), three-state substrate switch
  (`None`/`Plate`/`Wedge`), default-unit display, layer/system rotation reusing
  `Layer.rotate`/`OpticalSystem.rotate`, plus a FuncUI `view`.
- `OpticalConstructor.Ui/ConstructionPage.fs` — top-level page MVU: per-node
  busy, live descendant-count pre-confirmation gate, results refresh from the
  new `EmFieldSystem`, single-level undo, opposing add/attach actions, nav-entry
  + committable-file-path helpers, plus a FuncUI `view`.
- `OpticalConstructor.Tests/StackEditTests.fs` — AC-B5/B6/B7/B1/B10 over the
  pure cores.

Edited:
- `OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` — register the two new
  modules in build order.
- `OpticalConstructor.Storage/schema/optical-constructor-project.schema.json` —
  fill the field-level shapes of the `beamNode`/`beamBranch`/`opticalSystem`/
  `layer` `$defs` (`unitOfMeasure` already filled by slice 004), kept
  permissive enough that the real FSharp.SystemTextJson output validates.
- `OpticalConstructor.Tests/ProjectJsonRoundtripTests.fs` — extend for AC-B9
  (beam-tree `$def` field-level round-trip, schema-validated, no `.binz`).
- `OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — register
  `StackEditTests.fs`.

## Risks

- **Schema↔serializer tag-shape mismatch** (flagged by slice 004). The DU
  encodings (`element` `Sample` adjacent-tag, `WaveLength`, `substrate`) must
  not be over-constrained or validate-on-load rejects a legitimate document.
  Mitigation: empirically dump the serialized JSON first, then author `$defs`
  that describe the field-level shape while staying permissive on the DU-tagged
  fields; confirm via the extended round-trip test.
- **FuncUI view compile risk.** Views build against the public MIT
  `Avalonia.FuncUI` NuGet (not the audit-gated clone). Mitigation: keep the
  pure cores complete and self-contained; keep views thin; iterate against the
  `build` gate.
- **"group" semantics.** §B.5 lists "group" with no precise definition and the
  [Standard] repeat/period builder is explicitly Part J. Interpret group as
  gathering a selection of layers into a contiguous block (a pure new `films`
  list); record the choice in Gotchas.
