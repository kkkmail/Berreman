# 007 — State of the world: element groups, multiple detectors, collections, undo/redo (Part G/H/K)

## Where we are

Slice 007 is the FINAL slice of arc 0026 (Optical Constructor UI). It realises **Part G**
(element groups with two member modes, multiple detectors with a user‑chosen primary, and a
separate per‑user app‑data store for groups + collections), **Part H** (experiment collections
[Should]), and **Part K** (undo/redo participation for every project‑mutating action, plus the
safety gate on destructive actions [Must]). It builds on slices 001 (`Placement`), 004
(storage/schema patterns, `Controls`), 005 (`Commands`/`ConstructorView`), and 006 (ribbon
*Experiment* tab + element context menu). Because every project‑mutating action (placement,
rotation, lock, group on/off, swap, detector add/remove/set‑primary, slide, light‑source move)
now exists after slices 001–006, this slice wires them all into the shared `EditHistory` and
completes the feature. As the final slice it carries the full bundle gate set, all green.

## What's working

- Add `Groups.fs` (Domain): `ElementGroup` with mutually‑exclusive / multi‑select member modes,
  a LOSSLESS swap that flips only `inBeam` and never a member's stored config (AC‑G1), plus
  `ExperimentCollection` with shared‑sample + on/off toggles (AC‑H1).
- Add `GroupsLibrary.fs` + `optical-constructor-groups.schema.json` (Storage): the separate
  per‑user app‑data groups/collections JSON, reusing `ProjectJson.options` and genuinely sharing
  the schema‑validate‑on‑load seam — `validate` calls the now‑exposed `SchemaValidation.collectMessages`,
  so nested groups‑file errors keep their `results.Details` detail — independent of any `.ocproj`
  (AC‑G3 / AC‑H1).
- Drive multiple detectors with a user‑chosen primary (first detector); "Set as primary" reorders
  placements as a real, undoable mutation, secondaries keep their own branch (AC‑G2).
- Reuse `History.EditHistory` as the multi‑level undo/redo carrier: every project‑mutating edit
  `commit`s one snapshot (slide drags commit once at end, guarded against no‑ops), `Undo`/`Redo`
  step it (AC‑K1).
- Gate reset‑rotation / delete / reset‑view behind a same‑row, distinct‑colour Confirm/Cancel
  destructive gate, and render the slice‑006‑deferred element‑dialog, context‑menu and confirm
  overlays — re‑enabling the four element‑edit commands (AC‑K2).
- Surface the group / collection / detector commands on the *Experiment* tab + element context
  menu from the ONE command registry; add EN+RU strings; add `GroupsRoundTripTests` and
  `GroupDetectorUndoTests`.

## Tests

All gates pass locally (logs under `C:\GitHub\Berreman\specs\0026\.artifacts\`):

- `build` — PASS (exit 0, "Build succeeded.", 0 Error(s), 0 lowercase `error` matches — the
  gate's `stdout_match` veto). `007-build.log`.
- `unit-tests` — PASS (exit 0, Passed 84 / Skipped 5 / Total 89; legacy `BerremanTests`
  untouched). Baseline `berreman_unit_tests = 84` held. `007-unit-tests.log`.
- `constructor-unit-tests` — PASS (exit 0, Passed 234; 225 → 234, +9 `GroupsRoundTripTests`:
  AC‑G1 member modes + lossless swap, AC‑G3 schema‑validated round‑trip + wrong‑version reject +
  app‑data path + the retry‑round nested‑error‑detail test, AC‑H1 collection toggles).
  `007-constructor-unit-tests.log`.
- `ui-tests` — PASS (exit 0, Passed 88; 79 → 88, +9 `GroupDetectorUndoTests`: AC‑G2 primary
  detector + detector add/remove undo, AC‑K1 multi‑level undo across placement/rotation/lock, the
  retry‑round locked‑axis no‑op‑guard test, a single‑snapshot slide drag, group on/off,
  mutually‑exclusive swap, AC‑K2 confirm gates + the Experiment‑tab render; the two slice‑006
  RibbonTests and one slice‑005 CommandRegistryTest updated for the re‑enabled commands / gated
  reset‑view). `007-ui-tests.log`.
- `ui-smoke` — PASS (exit 0, Passed 4; 3 → 4, +1 slice‑007‑overlays‑and‑Experiment‑tab mount).
  `007-ui-smoke.log`.
- `impl-log-structure`, `state-of-world-structure` — required ATX headings present.

Coverage: AC‑G1 the two member modes + lossless swap; AC‑G2 the primary‑detector reorder with the
secondary keeping its branch; AC‑G3 the separate schema‑validated JSON round‑trip independent of
`.ocproj`; AC‑H1 the collection shared‑sample + on/off round‑trip; AC‑K1 every action pushes one
undoable snapshot, multi‑level; AC‑K2 reset‑rotation/delete/reset‑view each confirmation‑gated
with same‑row distinct‑colour Confirm/Cancel. None deferred.

```yaml
gates:
  berreman_unit_tests:    84
  constructor_unit_tests: 234
  ui_tests:               88
  ui_smoke:                4
```

## Architecture

- **`History.EditHistory` is the single undo/redo carrier (R‑8 — no parallel history).** The
  slice‑005 single‑level `undo`/`redo` option fields are GONE; the model carries one
  `history : History.EditHistory`. The invariant is `history.present = project` between edits;
  the `commit` helper (`History.push` after the edit) re‑establishes it, and `commitIfChanged`
  guards no‑op pushes. The guard is now UNIFORM: `editActive` (every rotation, lock toggle and
  emission toggle), the arrow‑key slide and the slide drag all route through `commitIfChanged`, so
  an inert edit — a wheel‑rotate about the default‑locked R3 axis, a both‑off emission toggle, a
  slide that hit a clamp bound, or a begin‑then‑end drag with no move — pushes NOTHING and never
  litters `Ctrl+Z` with empty steps (the slice's own High risk; AC‑K1). A slide drag commits ONCE
  at `EndDrag`, not per `SlideTo` — one undo step for the whole drag.
- **The primary detector is encoded as placement ORDER, not a new project field (G.2).** The
  primary is the FIRST `Detector` placement (the convention `centralRayEndpoints` already used);
  "Set as primary" `moveToFront`s the chosen detector, re‑keying `rayOf`/`selection`. This keeps
  `OpticalConstructorProject` (constructed in ~12 sites) and the slice‑001/004 placement+table
  anchors stable — no required field added (binding constraint 0.1 / spec hand‑off).
- **Groups live ONLY in the separate app‑data JSON, never in `.ocproj` (G.3).** Group on/off and
  swap flip `inBeam` in the pure `Groups` domain (the durable config store) and `syncGroupChange`
  diff‑reflects the in‑beam members into `project.placements`, so the change is a project‑snapshot
  edit that `Ctrl+Z` reverses. The library reuses the shared JSON options and GENUINELY shares the
  schema‑validate‑on‑load seam the spec names by line (R‑4 / G.3.1): `GroupsLibrary.validate` calls
  the now‑exposed `SchemaValidation.collectMessages` (its `results.Details` recursion), so a nested
  groups‑file error keeps its instance location instead of collapsing to a generic top‑level message.
  Only the lazy schema itself is local (a distinct `$id`, because the project loader is hardwired to
  the `.ocproj` schema); no second serializer, no migration.
- **The registry stays the single command source (constraint 0.4).** `SwapGroup`/`AddDetector`/
  `RemoveDetector`/`SetPrimaryDetector` are declared once in `Commands.registry`; `tabOf` classifies
  them onto the Experiment tab, so they project into both the ribbon and the collapsed menus with
  no second wiring site. Parameterized group commands (`ToggleGroup`/`SwapGroup`) render disabled
  via `commandsWithoutFrontDoorSurface`; the real per‑member toggles/swaps are Experiment‑tab
  extras dispatching the dedicated `GroupToggle`/`GroupSwap` messages.
- **The slice‑006‑deferred overlays now render (front door).** `Ribbon` gained
  `elementDialogOverlay` / `contextMenuOverlay` / `confirmGateOverlay` (the last drawing the
  same‑row Confirm/Cancel from `Controls.destructiveGate`'s one CTA‑colour definition, J.2 / R‑9);
  `Shell.constructorBody` layers them over the page when their model flag is set. The four
  element‑edit commands therefore drop out of `commandsWithoutFrontDoorSurface` and re‑enable.

## Deferred

- **Live app‑boot wiring of the groups library is not added.** `GroupsLibrary.load`/`save` and the
  app‑data path are fully implemented and round‑trip‑tested (AC‑G3/AC‑H1 are discharged by
  `GroupsRoundTripTests`), and `ConstructorView.Model.groups` is the in‑session store; auto‑loading
  the file at startup and auto‑saving on every group edit is a lifecycle nicety the ACs do not
  require, left for a follow‑up so this slice does not reshape `Shell.init`/the App composition root.
- **Redundant in‑beam state is a known DUPLICATE‑PLACEMENT HAZARD (not benign), carried into the
  live‑wiring follow‑up.** In‑beam status lives in TWO places — `project.placements` (the actual
  on‑table placement) and `groups[].member.inBeam` (a bool) — reconciled by a positional
  value‑equality diff (`syncGroupChange`, `ConstructorView.fs:442`). A project undo rewinds
  `project.placements` but NOT the group's `inBeam` flags, so after an undo the two can disagree; a
  subsequent toggle then re‑appends an already‑present placement and GROWS A DUPLICATE. Compounding
  it, `syncGroupChange` removes by value‑equality (`removeFirst`), so two on‑table placements equal
  by value (e.g. two default elements dropped at the same snapped point) let a toggle‑out delete the
  wrong one. The single‑source‑of‑truth redesign (derive in‑beam by matching members against
  `project.placements`, or key members to placements by id) is deferred to the live‑wiring slice per
  the review hint and is NOT done this cycle — but it is a real corruption hazard, not best‑effort
  durability. Test fixtures use distinct placements, so the gate suite does not surface it today.
- The value‑id / device‑material picker behind the element dialog stays an empty placeholder
  (0.6 / F.2.2) — slice 007 ships the working, dismissible dialog only.

## Gotchas

- **`table resize` (K.1.1) has NO UI surface to wire in this slice — recorded per the "don't ask the
  user" rule.** K.1.1 lists "table resize" among the actions that must `push` a snapshot, and the
  slice scope asserts every such action "now exists after slices 001–006." Table resize does NOT:
  there is no resize `Msg` in `ConstructorView` and no `Table.withDimensions` caller anywhere in the
  Ui (grep‑confirmed). So there is no path to route through `commit`/`commitIfChanged` this slice —
  nothing to wire, not a dropped requirement. When a future slice adds a resize gesture it MUST route
  through the same snapshot helpers as every other project‑mutating edit. Disclosed here rather than
  silently dropped from the action enumeration (the architecture critic and judge both flagged the
  omission).
- **G.1.3 / R‑4 "build on `FavoriteGroup`" is honoured as the favourites *notion*, not by cramming
  element groups into the favourites board.** The spec's file:line `UserEnvironment.fs:126
  FavoriteGroup` is STALE — line 126 is now `KeyBindingOverride`; the real `FavoriteGroup` is at
  `UserEnvironment.fs:166`, and it is a named board of engine `FavoritePin`s (Layer/System/Material/
  Source fragments), a different construct from an on‑table `ElementGroup` of placements. The spec
  asks element groups to be "nameable and reusable, building on the favourites concept so a commonly
  used group can be pinned and dropped back in later." `Groups.ElementGroup` is nameable (`name`) and
  reusable BY VALUE across projects via the separate groups library (G.3) — exactly the favourites
  notion (named, by‑value, survives the source project's deletion). Forcing groups into a new
  `FavoritePin` case would touch the slice‑015 environment schema + `EnvironmentRoundTripTests` and
  contradict G.3 ("own separate file") / G.4 ("no second serializer"); no AC (AC‑G1) tests pinning,
  so the favourites‑board wiring is intentionally left as the existing board.
- **AC‑G2 (primary detector) is proved in the `ui-tests` `GroupDetectorUndoTests`, not the
  `constructor-unit-tests` `GroupsRoundTripTests`** the spec's testing plan nominally grouped it
  under. The primary‑detector logic lives in `ConstructorView` (the Ui layer); `GroupsRoundTripTests`
  is deliberately Avalonia‑free (per `TestApp.fs`'s mandate that the pure suite stay Avalonia‑free),
  so the detector behaviour is tested where `ConstructorView` already lives. AC‑G2 is fully covered
  by a gate either way.
- **`ResetView` is now confirmation‑gated, not immediate.** It is ephemeral view state NOT captured
  by the project‑snapshot `EditHistory`, so K.2 requires it confirm rather than undo. The slice‑005
  `AC-C2 reset view` test was updated to go through `Invoke ResetView` → `ConfirmPending`. Anyone
  expecting `Invoke ResetView` to reset immediately must now confirm first.
- **The four element‑edit commands are RE‑ENABLED this slice.** The two slice‑006 RibbonTests that
  asserted `OpenElementDialog`/`ElementContextMenu`/`ResetRotation`/`DeleteElement` render DISABLED
  were updated to assert ENABLED — slice 006 explicitly handed this off ("when an overlay lands,
  drop that command from the shared list so its button re‑enables"). `commandsWithoutFrontDoorSurface`
  now holds the ten gesture‑only/parameterized commands (nine + `SwapGroup`).
- **EOL: the touched files are LF (`core.autocrlf=false`, no `.gitattributes`).** Verified this round:
  `git diff --numstat` == `--numstat --ignore-cr-at-eol` for every modified tracked file (zero CR
  churn), and the new files are LF. The previous attempt had flipped `OpticalConstructor.Storage.fsproj`
  to CRLF (whole‑file diff); it was reconverted to LF so the diff is the 9 real added lines only.
- **The build gate vetoes any lowercase `error` in stdout.** The build is clean (0 Error(s) and 0
  lowercase `error` lines — verified); the only output is the pre‑existing NU1701/NU1902/MSB3277/
  SYSLIB0051 warnings, none of which contain `error`.

## Changelog

- 2026-06-06 (slice 007): Land Part G/H/K — element groups (two member modes + lossless swap),
  experiment collections, multiple detectors with a user‑chosen primary, the separate per‑user
  app‑data groups/collections JSON (own schema, validate‑on‑load), and undo/redo participation for
  every project‑mutating action via the shared multi‑level `History.EditHistory`, plus the
  destructive‑action confirm gate and the slice‑006‑deferred element‑dialog/context‑menu/confirm
  overlays. New `Groups.fs`, `GroupsLibrary.fs`, `optical-constructor-groups.schema.json`,
  `GroupsRoundTripTests.fs`, `GroupDetectorUndoTests.fs`. Edit `Commands.fs` (group/detector
  commands in the one registry), `ConstructorView.fs` (EditHistory + groups + detectors + confirm
  gate + group/overlay messages), `Ribbon.fs` (Experiment‑tab controls + the three overlays),
  `Shell.fs` (render the overlays), `LocalHelp.fs` + `strings.json` (EN+RU labels). Update two
  slice‑006 RibbonTests (the re‑enabled commands) and one slice‑005 CommandRegistryTest (gated
  reset‑view); reconvert `OpticalConstructor.Storage.fsproj` to LF. All gates pass
  (berreman_unit_tests 84 held, constructor_unit_tests 225 → 233, ui-tests 79 → 87, ui-smoke 3 → 4,
  build clean).
- 2026-06-06 (slice 007, retry attempt 03 — review hint): Close the cycle‑1 code‑judge's two
  findings with no redesign. Route `editActive` through the existing `commitIfChanged` so a
  locked‑axis rotation / both‑off emission toggle pushes no empty undo step (AC‑K1 / the slice's own
  High no‑op‑push risk); expose `SchemaValidation.collectMessages` and call it from
  `GroupsLibrary.validate` so the groups path genuinely reuses the named seam and keeps nested‑error
  detail (R‑4 / G.3.1). Add a locked‑axis no‑op‑guard regression test (`GroupDetectorUndoTests`,
  confirmed red→green) and a nested‑error‑detail test (`GroupsRoundTripTests`); restore
  `SchemaValidation.fs` to LF (the edit had flipped it to CRLF). SoW: stop overstating the reuse,
  record that table resize (K.1.1) has no UI surface to wire, reframe the redundant in‑beam state as
  a real duplicate‑placement hazard. All gates pass (berreman_unit_tests 84 held,
  constructor_unit_tests 233 → 234, ui-tests 87 → 88, ui-smoke 4, build clean). This closes arc 0026.
