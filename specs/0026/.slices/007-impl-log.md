# 007 — Impl log: element groups, multiple detectors, collections, undo/redo (Part G/H/K)

## Progress

- [x] Read system prompt, project prompt, slice spec, gates, prior SoW; mapped the OpticalConstructor
  Domain/Storage/UI seams (BeamTree, Project, Placement, History, SchemaValidation, ProjectJson,
  UserEnvironment, Commands, ConstructorView, Ribbon, Controls, Localization) and the test harnesses.
- [x] Wrote impl-plan.
- [x] Domain: Groups.fs (ElementGroup, member modes, lossless swap, ExperimentCollection, GroupsLibrary).
- [x] Storage: GroupsLibrary.fs + groups schema (separate app-data JSON, validate-on-load, distinct $id).
- [x] UI: ConstructorView (EditHistory `commit`/`commitIfChanged` replacing single-level undo; detectors;
  groups + group/overlay messages; reset-view confirm gate; `commandsWithoutFrontDoorSurface` re-scoped);
  Commands (4 group/detector commands in the one registry); LocalHelp + strings.json (EN+RU);
  Ribbon (Experiment-tab group/detector controls + element-dialog/context-menu/confirm overlays);
  Shell (render the three overlays).
- [x] Tests: GroupsRoundTripTests (constructor-unit-tests) + GroupDetectorUndoTests (ui-tests/ui-smoke);
  registered both in their .fsproj; updated two slice-006 RibbonTests (re-enabled commands) and one
  slice-005 CommandRegistryTest (gated reset-view).
- [x] Gates: build, unit-tests, constructor-unit-tests, ui-smoke, ui-tests — all PASS.

## Retry round (attempt 03) — review-hint fixes

The cycle-1 code-judge returned `route-back-to-worker` (gates were green; two cheap, spec-grounded
findings). This round closes them with minimal, localized edits — no redesign — and keeps every gate
green. TDD: the no-op fix has a regression test that was confirmed RED first, then GREEN.

- **No-op push on `editActive` (the deciding finding, AC-K1 / High risk).** `editActive`
  (`ConstructorView.fs:200`) ended in an unconditional `commit`, so a wheel-rotate about the
  default-locked R3 axis (and a both-off `toggleEmission`, which routes through `editActive`) pushed
  an identical snapshot — a `Ctrl+Z` that does nothing. Routed `editActive` through the worker's own
  `commitIfChanged` (one line). The between-edits invariant it relies on holds at edit start.
  Regression test added: `GroupDetectorUndoTests`'s "a no-op edit (wheel-rotate about the locked R3
  axis) pushes no undo step" — confirmed FAIL against the old `commit` (history.past had one entry),
  PASS after the fix; an unlocked R1 rotation still pushes exactly one step.
- **F1 — genuinely reuse the schema-validate seam (R-4 / G.3.1).** `GroupsLibrary.validate` read only
  top-level `results.Errors`, dropping the `results.Details` recursion `SchemaValidation.collectMessages`
  performs, so nested groups-file errors lost their detail. Exposed `SchemaValidation.collectMessages`
  (removed `private`) and made `GroupsLibrary.validate` call it — now genuine reuse of the named seam,
  not a shallower re-implementation. Regression test added: `GroupsRoundTripTests`'s "a nested
  groups-file violation keeps its instance-location detail" — a top-level-valid doc with a malformed
  member placement now surfaces the nested `/groups/0/members/0/...` location instead of the generic
  fallback. Updated the module/function docs and the SoW to describe genuine reuse.
- **SoW disclosures.** Recorded in Gotchas that "table resize" (K.1.1) has no UI surface to wire this
  slice (no resize `Msg`, no `Table.withDimensions` caller in the Ui). Reframed the redundant in-beam
  state (`project.placements` + `groups[].member.inBeam`) in Deferred as a real duplicate-placement
  hazard (a stale `inBeam` after a project undo can re-append an already-present placement), not the
  benign "best-effort durability" it was. Per the hint, did NOT redesign the in-beam single-source-of-
  truth or the value-equality remove this cycle.

Retry-round files touched: `ConstructorView.fs` (route `editActive` through `commitIfChanged`),
`SchemaValidation.fs` (expose `collectMessages`; restored to LF — the edit had flipped it to CRLF),
`GroupsLibrary.fs` (reuse `collectMessages`; doc), `GroupDetectorUndoTests.fs` (+1 no-op test),
`GroupsRoundTripTests.fs` (+1 nested-detail test), and this impl-log + the SoW.

## Files modified

New:
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Groups.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/GroupsLibrary.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/optical-constructor-groups.schema.json`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/GroupsRoundTripTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/GroupDetectorUndoTests.fs`

Edited:
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` (compile Groups.fs)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj` (compile
  GroupsLibrary.fs + content-copy the groups schema; reconverted to LF — the previous attempt had
  flipped it to CRLF)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/Commands.fs` (SwapGroup/AddDetector/RemoveDetector/
  SetPrimaryDetector in the one registry)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorView.fs` (History.EditHistory carrier;
  detector helpers; group workspace + messages; reset-view confirm gate; surfaceless-list re-scope)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/LocalHelp.fs` (label keys for the four new commands)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/Ribbon.fs` (tabOf classify; Experiment-tab extras;
  element-dialog/context-menu/confirm overlays)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/Shell.fs` (render the three overlays in constructorBody)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/strings.json` (EN+RU group/detector/confirm/overlay keys)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`,
  `.Storage/...fsproj`, `.Tests/...fsproj`, `.Ui.Tests/...fsproj` (compile registrations)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/RibbonTests.fs` (the four element-edit commands
  re-enabled), `CommandRegistryTests.fs` (reset-view confirmation-gated)

Project.fs and the project schema were NOT changed: groups live only in the separate app-data library
(G.3), and the primary detector is encoded as placement order, so no `OpticalConstructorProject` field
or `.ocproj` schema change is needed (binding constraint 0.1; placement/table anchors kept stable).

## Decisions / Gotchas

- Primary detector = the FIRST `Detector` placement (the convention `centralRayEndpoints` already used),
  NOT a new `OpticalConstructorProject` field — the record is constructed in ~12 sites and the spec keeps
  Project.fs anchors stable. "Set as primary" `moveToFront`s the chosen detector (a real undoable edit).
- Groups/collections live ONLY in the separate app-data JSON, never in `.ocproj` (G.3). Group on/off
  diff-syncs in-beam member placements into `project.placements` so undo (project snapshot) reverses it;
  the group's own `inBeam` flags are the durable config store (best-effort across a project undo).
- `History.EditHistory` replaces the slice-005 single-level `undo`/`redo` option fields (R-8 reuse). The
  invariant `history.present = project` is re-established by `commit` after each edit; `commitIfChanged`
  guards no-op pushes; a slide drag commits ONCE at `EndDrag`. The slice-005 AC-E5 single-level undo test
  still passes against the multi-level history (verified).
- Reset view is confirmation-gated (K.2) because it is ephemeral view state outside `EditHistory`; this
  changed the slice-005 `AC-C2 reset view` test (now goes through `Invoke ResetView` → `ConfirmPending`).
- The four element-edit commands re-enable this slice (their overlays now render) — the slice-006 hand-off.
  Two slice-006 RibbonTests updated accordingly; `commandsWithoutFrontDoorSurface` now holds the ten
  gesture-only/parameterized commands (nine + `SwapGroup`).
- AC-G2 (primary detector) is proved in the ui-tests `GroupDetectorUndoTests` (where `ConstructorView`
  lives) rather than the Avalonia-free `GroupsRoundTripTests`, per `TestApp.fs`'s pure-suite mandate.

## Testing state

All gates PASS locally (commit_ready: true). Logs under `C:\GitHub\Berreman\specs\0026\.artifacts\`:

- `build` — PASS (exit 0; "Build succeeded."; 0 Error(s); 0 lowercase `error` — the stdout_match veto). `007-build.log`
- `unit-tests` — PASS (Passed 84 / Skipped 5 / Total 89; baseline `berreman_unit_tests = 84` held). `007-unit-tests.log`
- `constructor-unit-tests` — PASS (Passed 234; 225 → 234, +9 GroupsRoundTripTests incl. the retry-round
  nested-error-detail test). `007-constructor-unit-tests.log`
- `ui-tests` — PASS (Passed 88; 79 → 88, +9 GroupDetectorUndoTests incl. the retry-round locked-axis
  no-op-guard test). `007-ui-tests.log`
- `ui-smoke` — PASS (Passed 4; 3 → 4, +1 overlays/Experiment-tab mount). `007-ui-smoke.log`
- `impl-log-structure`, `state-of-world-structure` — required ATX headings present.

Every slice requirement (R-1 .. R-9 / AC-G1, AC-G2, AC-G3, AC-H1, AC-K1, AC-K2) is addressed this round;
nothing is deferred to a hypothetical round 2. No in-tree persisted operator state was added — the
groups/collections library lives at the per-user app-data path outside the repository.

## Artifacts

Captured gate logs under `C:\GitHub\Berreman\specs\0026\.artifacts\`:

- `007-build.log` — build gate (`dotnet build Berreman.slnx -c Release`).
- `007-unit-tests.log` — legacy BerremanTests gate.
- `007-constructor-unit-tests.log` — OpticalConstructor.Tests gate (incl. GroupsRoundTripTests).
- `007-ui-tests.log` — OpticalConstructor.Ui.Tests, `Category!=ui-smoke` (incl. GroupDetectorUndoTests).
- `007-ui-smoke.log` — OpticalConstructor.Ui.Tests, `Category=ui-smoke`.
