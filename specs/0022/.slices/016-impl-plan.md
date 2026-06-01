# 016 impl-plan — UX shell III: JobRunner, Help/gallery, SystemView3D (Part J §J.10–J.12)

## Scope (last slice of the arc)

Three new Ui modules + tests, closing Part J. All headless-provable per §0/P3
(model/geometry carry no Avalonia/OpenTK type); the view binding stays deferred,
exactly as every prior Ui module records. The audit-gated FuncUI clone stays
UNREFERENCED.

## Approach

### R-1 §J.10 `JobRunner.fs` — background job harness
The single shared background-job harness, generic over the per-step work so it
hosts BOTH heavy sweeps (`Variables.calculate`/`calculate3D`) and Part G fits
without re-implementing engine routing or optimizer internals — it consumes them
through an injected `step : int -> 'a` closure, matching how `SynthesisFitPage`
supplies "only the per-iteration progress payload" (slice 011).

- `JobProgress` (completed / total option / elapsed) — determinate (sweep
  completed-points, `total = Some n`) and indeterminate (`total = None`, item 2)
  states; `fraction`/`isIndeterminate` helpers.
- `sweepTotalPoints (x : RangedVariable) = x.length + 1` — the sweep total is the
  engine's own `0..x.length` inclusive count (`Variables.fs:42,237`), so progress
  granularity is DRIVEN BY the `RangedVariable` point count, not re-derived.
- `runPoints` / `runIterations` over a shared cooperative loop: check the
  `CancellationToken` BETWEEN steps (item 5), append each result, fire
  `onProgress` with elapsed (Stopwatch). On cancel return `CancelledWith partial`
  so the caller DROPS the partial; completion returns `RanToCompletion`.
- `startBackground` runs the loop on a thread-pool worker (item 1, no frozen UI);
  exceptions become a `Failed` result, never crash the worker.
- MVU harness (`JobModel<'r>` / `JobMsg<'r>` / `updateJob`): Start keeps prior
  `committedResults`; `RunCompleted` refreshes them (item 4 MVU refresh);
  `RunCancelled` leaves them UNTOUCHED (item 5 — partial dropped is the undo);
  `buttonLabel` = Start↔Cancel single button two states (item 5).
- `sidecarDirectory`/`derivedArtefactPath` confine derived `.binz` to the
  project sidecar dir (reusing `Storage.Sidecar.extension`), never repo root
  (item 6).

### R-2 §J.11 `Help.fs` — glossary, tooltips, gallery
- `glossary` units entries STATE the §A.10 eV/cm⁻¹ conversions by CALLING
  `Domain.Units` (`evNmProduct`, `toMeters`/`fromMeters` Wavenumber) — no second
  conversion implementation.
- `tooltips` — static strings co-located here (authorable pipeline out of scope).
- `gallery` — shipped sample `OpticalConstructorProject`s built from engine
  presets corresponding to the named `Analytics/Examples/*.fsx` cases (AR/
  multilayer, EUV Mo-Si DBR, active crystal, wedge, dispersive glass); each opens
  through the SAME schema-validated path `ProjectFile.openProject` uses
  (`ProjectJson.serializeProject >> deserializeProject`), exactly like
  `Templates.loadTemplate`. No `.fsx` is executed at runtime.

### R-3 §J.12 `SystemView3D.fs` — OpenTK system/beam viewport (geometry pure)
- Place each `ConstructorElement` of the open project's `BeamTree`/`BeamNode`
  topology to scale along the beam path (canonical-SI meters → viewport coords at
  the render boundary only; nothing written back, §A.3).
- Draw beam segments by READING each branch's solved direction from
  `EmField.normal` (the Poynting unit vector) of `EmFieldSystem.reflected`/
  `.transmitted` (`Fields.fs:553-554`) via the existing `BeamTree.solve`/
  `branchEmField` seams — CONSUME the already-solved tree, never re-solve/re-route.
- Geometry is a pure projection carrying NO OpenTK type (headless-provable), the
  custom OpenTK surface binding deferred like the schematic `Canvas`.

## Files
New: `JobRunner.fs`, `Help.fs`, `SystemView3D.fs` (Ui);
`JobRunnerCancelTests.fs` (Tests).
Edited: `OpticalConstructor.Ui.fsproj` (register 3 modules after AppShell, no
clone ref); `OpticalConstructor.Tests.fsproj` (register `JobRunnerCancelTests`);
`SchematicGeometryTests.fs` (add the AC-J12 `SystemView3D` geometry portion).

## Risks / mitigations
- JobRunner cancel must leave prior results intact + sidecar-confined → prove
  `CancelledWith` drops partial while `committedResults` survive; path-under-dir
  assertion.
- SystemView3D must NOT re-solve → read directions through `BeamTree.solve`
  ONCE and assert the renderer takes the solved `EmFieldSystem` as input
  (segment dir = `branchEmField`.normal), no solver call in the render path.
- `--warnaserror+:25`: keep matches exhaustive.
- Confirm no FuncUI-clone reference in the Ui fsproj before exit (last slice).
