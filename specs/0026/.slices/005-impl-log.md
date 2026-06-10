# 005 — Impl log: centralized commands, mouse/keyboard interaction & the constructor MVU surface

## Progress

- [x] Read system prompt, project prompt, slice spec, slice‑004 SoW, gates, and the
  reused modules (`Placement`, `RayModel`, `Table`, `Drawer`, `ConstructorTable`,
  `Controls`, `UserEnvironment`, `Localization`, `BeamTree`, `Project`, `Shell`, the
  headless test harness `TestApp`/`SmokeTests`/`ConstructionEditTests`).
- [x] Wrote impl‑plan + this skeleton.
- [x] Add the configurable key map to `UserEnvironment.fs` + the schema (E.8.1 / E.3.1).
- [x] Write `Commands.fs` (the centralized registry, AC‑E1).
- [x] Write `ConstructorView.fs` (the MVU page + interactions).
- [x] Write `CommandRegistryTests.fs` + the env key‑map round‑trip test.
- [x] Wire the fsprojs, run the gates, normalize EOL, write the state‑of‑the‑world.

## Files modified

New:
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/Commands.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/CommandRegistryTests.fs`

Edited:
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/UserEnvironment.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/optical-constructor-environment.schema.json`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/EnvironmentRoundTripTests.fs`

## Decisions

- The registry is persistence‑agnostic (string‑keyed key overrides via
  `withKeyOverrides`), so `Commands.fs` carries no dependency on `UserEnvironment`; the
  page applies the configured `KeyMap` over the default registry. This keeps the
  compile order (`UserEnvironment` → `Commands` → `ConstructorView`) acyclic and lets
  slice 006 project ribbon + menu from the same registry with no second wiring site.
- The element context menu surfaces the registry commands flagged `inContextMenu`
  PLUS the per‑element property toggles (lock/unlock each rotation, toggle
  reflected/transmitted). The property toggles are element‑state edits (not key/mouse
  bound commands), matching the R‑2 MUST‑declare list which enumerates the bound
  commands; group on/off and detector *Set as primary* are slice 007.
- Single‑level undo/redo this slice (the `Ctrl+Z`/`Ctrl+Y` commands dispatch real
  edits); the multi‑level history + all‑action snapshot‑push participation is slice 007
  (Part K), per the slice Non‑requirements.

## Testing state

All seven applicable gates pass locally (see `Tests` in the state‑of‑the‑world for
the full coverage map):

- `build` — PASS (exit 0; 0 lowercase `error` matches). `005-build.log`.
- `unit-tests` — PASS (Passed 84 / Skipped 5; baseline `berreman_unit_tests = 84` held).
  `005-unit-tests.log`.
- `constructor-unit-tests` — PASS (Passed 225; 222 → 225, +3 AC‑E8 key‑map round‑trip).
  `005-constructor-unit-tests.log`.
- `ui-tests` — PASS (Passed 64; 44 → 64, +20 `CommandRegistryTests`). `005-ui-tests.log`.
- `ui-smoke` — PASS (Passed 2; +1 constructor‑surface mount). `005-ui-smoke.log`.
- `impl-log-structure`, `state-of-world-structure` — PASS (required ATX headings present).

`commit_ready: true` — every R‑1..R‑9 requirement and every owned AC (AC‑E1..E5, AC‑C2,
AC‑C3) lands in this round; nothing is deferred to a "round 2".

## Artifacts

- Gate logs under `C:\GitHub\Berreman\specs\0026\.artifacts\`:
  `005-build.log`, `005-unit-tests.log`, `005-constructor-unit-tests.log`,
  `005-ui-tests.log`, `005-ui-smoke.log`.
