# Step 0256 — state of the world

## Where we are

This is spec 0050, section S8.2 ("Setups as slots"), second step. Step 0255 added the identities a setup is stored
and referenced under (`SetupName`, `SetupRevision`, `SetupReference`, `SetupError`). This step adds the setup itself
as pure Domain data in the new `OpticalConstructor.Domain/Setups.fs`. A setup is an ordered optical path of slots,
each one table element that holds an item or nothing, plus the earlier revisions an experiment can still reference.
Nothing in the product reads these types yet. `Experiments.ElementDescriptor` and `Scene.SceneElement`, whose
content `SetupSlot` now carries, are deleted by steps 260 and 261.

## What's working

- Added `Setups.fs`, compiled directly after `BeamLayout.fs`. It holds:
  - `SlotTurn` (`PlacementTurn` / `DialTurn of reading`);
  - `SlotContent` (`EmptySlotContent` / `ItemInSlot`, with `itemOpt`);
  - `SetupSlot` (with `layoutElement`, the one-ray-law shape);
  - `SetupRevisionRecord`;
  - `Setup`.
- A setup answers its slots at its current revision and at each history revision. Any other revision is refused
  with `UnknownSetupRevisionErr` naming `'<name>' revision <n>`.
- `Setup.create` makes a setup at revision 1 with no history. It refuses a name whose `.setup-json` file a held
  setup already has, ignoring case.
- `Setup.revisedWith` applies the one version rule (`Lifecycle.decideVersioning`). Equal slots keep the setup,
  unpinned changes mutate it in place, and pinned changes mint the next revision, moving the old slots into
  `history`.
- Added `SetupTests`, five facts covering the census identities.

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster: build, elevation,
constructor-unit-tests (whole) and the fifteen s26 invariants. The worker's development runs over the final tree
used each gate's own command. Logs are under `specs/0050/.artifacts/0256-dev-*`.

- **build:** exit 0, 0 errors, 71 s (deadline 600 s). The first build after a stale obj printed MathNet's two
  pre-existing `SYSLIB0051` warnings (vendored C#).
- **constructor-unit-tests:** `Passed!`, 1413 succeeded, 0 failed, 71 s wall (deadline 180 s). That is
  1408 + 5, and all five `SetupTests` facts are `Passed` in the TRX.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 204s`, 3 min 24 s wall (deadline 600 s),
  known findings 0.
- **Incremental rebuild:** 0 warnings, 0 errors.
- **The fifteen s26 invariants,** by their greps, all find nothing.
- No development run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1413
```

## Architecture

- **Setups.fs sits right after `BeamLayout.fs`.** A slot is a `BeamLayout.BeamHost` on the beam tree, and its
  `layoutElement` is exactly the `BeamLayout.LayoutElement` the one ray law lays out. So a setup's slots are drawn
  and traced by the same code as today's scene elements.
- **History.** It is kept as an item keeps its history (step 215): `SetupRevisionRecord`s oldest first, each the
  slots a revision held. `slotsAt` resolves an experiment's referenced revision against the current one and then
  the history.
- **One version rule.** `revisedWith` builds `VersionPayload<SetupSlot list, unit>`, so a setup's slots alone decide
  (it has no metadata beyond its name). It hands them, with the `PinState`, to `Lifecycle.decideVersioning`.
- **One stem test.** `create` detects a clash by `FileStem.firstFreeText name [ held ] <> name`, step 0255's
  `SetupName.firstFree` pattern. The case-insensitive stem comparison stays in one place. The reason names both
  names and the shared `<stem>.setup-json`, in step 216's item wording.

## Deferred

Nothing in this step's scope is deferred.

## Gotchas

- `Setups` reuses the record labels `name`, `revision`, `slots` and `id`, which `Kit` (`Item`, `ItemRevisionRecord`,
  `SetupReference`) and `BeamLayout` (`LayoutElement`) also use. In a file that opens both, annotate record
  constructions (`let x : SetupRevisionRecord = { … }`). Every construction in `Setups.fs` and `SetupTests.fs` is
  annotated.
- The slice listed five opens. `Berreman.Geometry` is added for `Angle`. `ItemModels` is opened as the slice says;
  only `SlotTurn`'s doc names `DialSign`.
- `withR1` respects locks. A `LinearPolarizer`'s R1 starts free, so the fixture's `lp` turns. A `LightSource`'s does
  not, because every rotation starts locked.

## Changelog

- 2026-11-10 — Step 0256: added the setup shape (`SlotTurn`, `SlotContent`, `SetupSlot`, `SetupRevisionRecord`,
  `Setup`), with per-revision slot lookup, held items, `create` and the pinned-revision `revisedWith`; five new
  `SetupTests` facts.
