# 005 — State of the world: centralized commands, mouse/keyboard interaction & the constructor MVU surface

## Where we are

Slice 005 realises **Part E** (the centralized command model + mouse and keyboard) of
Spec 0026 (Optical Constructor UI), the FuncUI `Canvas` MVU page (`ConstructorView.fs`)
that hosts the slice-004 top-down surface, and the **Part C** view interactions that
depend on the page (pan/zoom/reset, table selection). It is the largest UI slice: the
`Commands.fs` registry is the single source from which both the key map and the mouse map
derive (AC-E1 / constraint 0.4), and `ConstructorView.fs` wires every gesture through it
with no second binding site. It builds on slice 001 (`Placement`), 002 (`RayModel`), 003
(`UserEnvironment`/`Language`), and 004 (`Table`/`Drawer`/`ConstructorTable`/`Controls`).
It deliberately ships NO ribbon/menu shell (slice 006) and NO undo/redo participation
across all actions or element-group on/off behaviour (slice 007); it exposes the registry
those slices project from and binds the command shapes they extend.

## What's working

- Add `Commands.fs`: the centralized command registry — a `Command` DU, a three‑state
  `Binding` (keyboard‑only / mouse‑only / keyboard‑plus‑mouse), pure comparable gesture
  shapes, and the key map + mouse map projected from the ONE registry (AC‑E1).
- Add `ConstructorView.fs`: the pure MVU model/msg/update + the FuncUI `Canvas` view —
  active‑element commands, rotations with lock inertness, slide/reassign/inert‑plain‑drag,
  drag‑to‑place snap, table/view + global commands, pan/zoom/rotate/reset, and table
  selection (the in‑plane R1 view rotation spins the whole table together, C.2.4).
- Make the key map configurable and persisted: add `KeyMap` (+ the 5° rotation step) to
  `EnvironmentSettings` and the env schema, applied over the default registry (E.8.1).
- Add `CommandRegistryTests.fs` (20 `ui-tests` + 1 `ui-smoke` mount) covering AC‑E1..E5,
  AC‑C2/C.2.4, AC‑C3; add the configurable‑key‑map round‑trip to `EnvironmentRoundTripTests`.
- Drive the existing engine/aggregate (no parallel catalogue/solver/project type); the
  view re‑renders on every model change, so the table redraws on drop for free.

## Tests

All seven applicable gates pass locally (logs under
`C:\GitHub\Berreman\specs\0026\.artifacts\`):

- `build` — PASS (exit 0, "Build succeeded.", 0 Error(s), 0 lowercase `error` matches —
  the gate's `stdout_match` veto). Log `005-build.log`.
- `unit-tests` — PASS (exit 0, Passed 84 / Skipped 5 / Total 89; `BerremanTests`
  untouched). Baseline `berreman_unit_tests = 84` held. Log `005-unit-tests.log`.
- `constructor-unit-tests` — PASS (exit 0, Passed 225 / Total 225; 222 prior + 3 new
  AC‑E8 key‑map round‑trip tests). Log `005-constructor-unit-tests.log`.
- `ui-tests` — PASS (exit 0, Passed 64 / Total 64; +20 new `CommandRegistryTests`,
  Category!=ui-smoke). Log `005-ui-tests.log`.
- `ui-smoke` — PASS (exit 0, Passed 2; the existing app‑host smoke + the new constructor
  surface mount‑and‑render one frame). Log `005-ui-smoke.log`.
- `impl-log-structure`, `state-of-world-structure` — PASS (required ATX headings present).

Coverage: AC‑E1 single‑source registry (no gesture bound twice; both maps project from
the registry) + `SlideAlongRay` declaring key‑plus‑mouse on one definition; AC‑E8 key‑map
override + gesture parse/format round‑trip; AC‑E2 Shift/Ctrl+Shift/Alt wheel → R1/R2/R3 by
the configured step, inert on a locked axis, R3 requiring unlock; AC‑E3 Shift+drag slide
bounded by neighbours, Ctrl+drag ray reassign, inert plain‑drag + movable hint; AC‑E4
ribbon drop snapped to the CR middle; AC‑E5 Ctrl+Z/Ctrl+Y/Ctrl+S/Esc; AC‑C2 plain‑ and
Ctrl‑wheel zoom + reset view; C.2.4 the in‑plane R1 view rotation spinning every element
together (elements travel with the table); AC‑C3 element vs empty‑table selection. None
deferred.

```yaml
gates:
  berreman_unit_tests:    84
  constructor_unit_tests: 225
```

## Architecture

- **The registry is the single source — declared once, surfaced through pure projections.**
  `Commands.fs` is the one place a command is added. Each `CommandDef` carries the
  three‑state `Binding`; the key map (`Map<KeyGesture, Command>`) and the mouse map
  (`Map<MouseGesture, Command>`) are `List.collect`‑ed off the registry, so AC‑E1 is a
  property of the projection — there is no second binding site. The element context‑menu
  contents are the same registry filtered by `inContextMenu`. Slice 006 (ribbon) and 007
  (group toggle / Set‑as‑primary) extend this one list.
- **`Commands.fs` is pure and persistence‑agnostic.** The gesture value shapes carry NO
  Avalonia type (constraint 0.3), so the key/mouse maps are provable headless. The
  configurable key map (E.8.1) is applied through `withKeyOverrides : (id, gesture-string)
  list -> registry`, so `Commands` has NO dependency on `UserEnvironment` and the compile
  order (`UserEnvironment` → `Commands` → `ConstructorView`) stays acyclic. The Avalonia
  `KeyModifiers`/`Key`/pointer types map onto the value shapes at the view boundary only.
- **The MVU model is pure and serializable (0.3), matching `Shell.RootModel`.** It holds
  the canonical `OpticalConstructorProject` + the ephemeral `Table.TableViewState` + the
  selection + the configured `KeyMap` + transient drag/menu/confirm state — no Avalonia
  handle. The `update` is Avalonia‑free (like `ConstructionPage`), so every AC is proved
  by dispatching messages — the headless harness does NOT fire real pointer/wheel events.
  The single host‑layer mutable (`lastPointer`, for pan/slide deltas) lives OUTSIDE the
  model, mirroring the `Shell.fs` module‑level host fields.
- **Reuse, not re‑type.** Rotations reuse the lock‑respecting `Placement.withR1/withR2/
  withR3` (so R3 inert‑until‑unlocked falls out for free); the view reset reuses
  `Table.resetView`; the geometry reuses `ConstructorTable.project` / `Drawer`; the CR
  snap reuses `RayModel` source/detector defaults. No parallel element catalogue, solver,
  or project type is introduced (constraint 0.1). The page completes slice 004's deferred
  screen‑tilt: `projectToCanvas` applies the view's in‑plane R1 about the canvas centre on
  top of `ConstructorTable.project`, so the plate, elements, and rays all rotate together
  (C.2.4 — the view R1 is screen‑relative; R2/R3 keep the top‑down approximation, 0.5).
- **Configurable key map persisted with the settings store (E.8.1).** A pure `KeyMap`
  (`rotationStepDegrees` defaulting to 5° per E.3.1 + a `KeyBindingOverride list`) is a new
  field of `EnvironmentSettings`, round‑tripping through the slice‑003 `ProjectJson.options`
  and a new `keyMap`/`keyBindingOverride` `$def` pair — no parallel preferences file.

## Deferred

- The ribbon / collapsed‑ribbon menu shell that PROJECTS controls from the registry, and
  the catalogue drag‑in / value‑id modal — slice 006 (Part D / Part F). This slice exposes
  the registry + the `ConstructorView` model/msg surface those project from.
- Multi‑level undo/redo and the snapshot‑push participation across ALL project‑mutating
  actions (AC‑K1/K2) — slice 007 (Part K). This slice binds `Ctrl+Z`/`Ctrl+Y` and
  dispatches real single‑level undo/redo of the project; the full history is slice 007.
- Element‑group on/off behaviour and the detector *Set as primary* command — slice 007
  (Part G). This slice binds only the number‑key group‑toggle command SHAPE (a no‑op).
- The per‑element property toggles surfaced by the context menu (lock/unlock each rotation,
  toggle reflected/transmitted) are element‑state edits dispatched as `ConstructorView`
  messages (`MenuToggleLock`/`MenuToggleEmission`), not gesture‑bound registry commands —
  matching the R‑2 MUST‑declare list, which enumerates the bound commands.
- The out‑of‑plane R2/R3 view tilt is the documented top‑down approximation (a 2‑D
  schematic surface realises only the in‑plane R1; a 3‑D tumble is not added, 0.5/0.6).
  No gesture sets the view rotation this slice (E.5 binds pan/zoom/reset/group, not
  view‑rotate); the capability + reset are in place for a slice‑006 ribbon control.
- No real device/material picker behind `valueId` (0.6); no caching / retained‑mode scene
  graph / drawing‑backend abstraction (0.6); `Schematic.fs` left untouched.

## Gotchas

- **EOL: this repo is CRLF‑committed, NOT LF.** Verified: HEAD blobs carry CR on every line
  across the whole tree (`git show HEAD:<f> | grep -c $'\r'` for core `Geometry.fs`, the
  tests, and all of `OpticalConstructor`), with `core.autocrlf=false` and no
  `.gitattributes`. The Edit and Write tools on this host BOTH emit CRLF, which MATCHES —
  so every touched file produced a clean `git diff` (e.g. the schema edit is a 27‑line diff,
  not whole‑file churn). Do NOT strip CR / normalize to LF here; that would create churn.
  (A prior note claimed the repo was LF — it is not; corrected.)
- **`Commands.Key` vs `Avalonia.Input.Key` collide by short name.** `ConstructorView`
  opens `Commands` (for the pure core) and therefore must NOT `open Avalonia.Input`; the
  Avalonia key/modifier types are referenced fully‑qualified (`Avalonia.Input.Key.Left`,
  `Avalonia.Input.KeyModifiers.Control`). Enum range checks use `int k` (F# does not give
  `>=` on the .NET enum directly).
- **Plain left‑drag is context‑sensitive, resolved in the handler, not the registry.**
  `DragGesture(Left, ∅)` maps to one command (`PanView`); the page interprets it as pan on
  the empty table and as the inert + hinted guard on an element (E.4.4). A single gesture
  still maps to exactly one registry command (AC‑E1 holds); the hit‑test disambiguates.
- **Reset rotation resets all three axes regardless of lock.** It is a deliberate,
  confirmed action (the confirmation IS the gate), so `ConfirmResetRotation` sets r1=r2=r3=0
  directly rather than routing through the lock‑respecting setters.
- **Ray reassignment cycles CR → reflected → transmitted.** The project stores `placements`,
  not per‑element ray ids, so the page holds an ephemeral `rayOf : Map<int, RayId>` (default
  `CentralRay`); `Ctrl+drag` cycles it. This is ephemeral UI state, not persisted, and not
  covered by the single‑level project undo.
- **Headless pointer/wheel/key events do not fire**, so the interaction logic is proved on
  the pure `update` (dispatch the `Msg` directly); the `ui-smoke` test only asserts the
  view mounts and renders one frame.

## Changelog

- 2026-06-06 (slice 005): Land Part E + the Part C view interactions — the centralized
  command registry and the constructor MVU surface. New `Commands.fs` (the one registry:
  `Command` DU + three‑state `Binding` + the key/mouse map projections + gesture
  parse/format + configurable overrides) and `ConstructorView.fs` (pure MVU model/msg/update
  for active‑element/rotation/slide/reassign/drag‑to‑place/table‑view/global commands +
  pan/zoom/reset + table selection, plus the FuncUI `Canvas` view and event translation).
  Add the configurable `KeyMap` (+ 5° rotation step) to `EnvironmentSettings` and the env
  schema. Add `CommandRegistryTests.fs` (19 `ui-tests` + 1 `ui-smoke`) and 3 AC‑E8 key‑map
  round‑trip tests in `EnvironmentRoundTripTests`. All seven gates pass
  (berreman_unit_tests 84 held, constructor_unit_tests 222 → 225, ui-tests 44 → 64,
  ui-smoke 1 → 2).
