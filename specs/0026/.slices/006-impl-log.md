# 006 — Impl log: ribbon/menu shell, front door, Legacy; catalogue & value-id modal

## Progress

- [x] Read system prompt, project prompt, slice spec, slice-005 SoW, the 006 gates,
  and the reused modules (`Commands`, `ConstructorView`, `Localization`,
  `UserEnvironment`, `Controls`, `Placement`, `BeamTree`, `Project`, `Shell`,
  `Program`, the headless test harness `TestApp`/`SmokeTests`/`PanelViewTests`/
  `CommandRegistryTests`).
- [x] Wrote impl-plan + this skeleton.
- [x] Added EN+RU localization keys to `strings.json` (ribbon tabs, commands, catalogue,
  nav, value-id modal, help — both languages complete, AC-I3).
- [x] Edited `ConstructorView.fs` (value-id modal state/messages + `activeElement`).
- [x] Wrote `LocalHelp.fs` (help contexts + the one command-label-key mapping).
- [x] Wrote `Ribbon.fs` (tabs/menus projected from the registry, catalogue, Legacy
  entry, value-id modal + help overlays).
- [x] Edited `Shell.fs` (Constructor + Legacy pages, RootModel/RootMsg, front-door nav,
  default landing, SetLanguage).
- [x] Wired the fsprojs + `Program.fs`; extended `SmokeTests.fs`; wrote `RibbonTests.fs`.
- [x] Ran build/unit-tests/ui-tests/ui-smoke (all PASS), wrote the state-of-the-world.

## Files modified

New:
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/Ribbon.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/LocalHelp.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/RibbonTests.fs`

Edited:
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/Shell.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/strings.json`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SmokeTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`

## Decisions

- **One projection, no second wiring site (AC-D2).** `Ribbon.tabOf : Command ->
  RibbonTab` classifies every registry command onto exactly one tab; `Ribbon.tabCommands`
  filters `Commands.registry` by it. BOTH the expanded ribbon and the collapsed menus
  render from `tabCommands`, so a command added once to the registry appears in both with
  no second site. `tabOf` ends in a wildcard so a future registry command still surfaces
  (defaults to the always-present Build tab) — slice 007's group/detector commands land
  automatically (and can be reclassified there).
- **The ribbon takes plain callbacks, never `Shell`.** `Ribbon.Dispatch` carries
  `onRibbon`/`onConstructor` plus three shell hooks (open Legacy / set language / toggle
  theme). The shell wires them to its `RootMsg`; the ribbon has no `Shell` reference, so
  the compile order (`Commands` → `ConstructorView` → `LocalHelp` → `Ribbon` → `Shell`)
  stays acyclic.
- **The value-id modal action lives in `ConstructorView`; the overlay renders in the
  shell.** `ConstructorView` owns `valueIdModalOpen` + `OpenValueIdModal`/
  `CloseValueIdModal` (and folds the modal into `CancelOrDeselect`); the shell renders the
  dimming overlay (it holds the loaded `strings` resource for the localized title/close).
  Same split for the local-help overlay (`helpOpen` is slice-005 state; `LocalHelp`
  resolves the context-sensitive text; the shell renders it).
- **Nested navigation, minimal disruption.** `Page` gains `Constructor` + `Legacy`. The
  front-door nav (Constructor / Legacy) is the new top bar; the legacy `Construction` /
  `SynthesisFit` pages keep their own sub-nav + lifecycle toolbar under the Legacy branch,
  rendering exactly as before — so the spec-0024 panel/lifecycle/fit view tests are
  untouched. `loadProjectInto` still lands on `Construction` (LifecycleTests:127 holds).
- **Constructor-as-default-landing is realised in `Shell.initFrom`** (`page =
  Page.Constructor`), which `Program.fs` already mounts; `Program.fs` only gained a
  clarifying comment (the localization completeness surface there is slice 003's).
- **`LocalHelp` owns the one command-label-key mapping** the ribbon reuses for both its
  expanded controls and its collapsed menus, so a command's user-facing label is declared
  once. The Settings tab's language switch (`ShellMsg.SetLanguage`) is functional, which
  also exercises the bilingual layout (D.4).

## Testing state

All applicable gates pass locally (logs under
`C:\GitHub\Berreman\specs\0026\.artifacts\`):

- `build` — PASS (exit 0, "Build succeeded.", 0 Error(s), 0 lowercase `error` matches —
  the gate's `stdout_match` veto). `006-build.log`.
- `unit-tests` — PASS (exit 0, Passed 84 / Skipped 5 / Total 89; `BerremanTests`
  untouched). Baseline `berreman_unit_tests = 84` held. `006-unit-tests.log`.
- `ui-tests` — PASS (exit 0, Passed 75 / Total 75; 64 → 75, +11 new `RibbonTests`
  Category!=ui-smoke). `006-ui-tests.log`.
- `ui-smoke` — PASS (exit 0, Passed 3; 2 → 3, +1 constructor-front-door-with-modal mount;
  the app-host smoke now renders the Constructor + Legacy pages too). `006-ui-smoke.log`.
- `impl-log-structure`, `state-of-world-structure` — PASS (required ATX headings present).

`commit_ready: true` — every R-1..R-11 requirement and every owned AC (AC-D1..D4,
AC-F1, AC-F2) lands in this round; nothing is deferred to a "round 2".

## Artifacts

- Gate logs under `C:\GitHub\Berreman\specs\0026\.artifacts\`:
  `006-build.log`, `006-unit-tests.log`, `006-ui-tests.log`, `006-ui-smoke.log`.

## Round 2 (retry) — code-judge route-back

The cycle-1 code-judge routed back (gates all green, every AC met) for one real
quality defect on the new default landing page plus three cheap companion fixes. The
retry hint is the contract for this round; none of it needs structural change.

- [x] Re-read the code-judge + architecture/reuse critic reports and the cycle-1 outputs.
- [x] **Fix 1 (primary): inert gesture-only ribbon/menu buttons.** `ConstructorView.applyCommand`
  returns the model unchanged for the gesture-only commands (`RotateR1/R2/R3`,
  `SlideAlongRay`, `MoveToRay`, `PanView`, `ZoomView`, `PlaceFromRibbon`) and the slice-007
  `ToggleGroup` placeholder, but the ribbon rendered each as a clickable button in BOTH the
  ribbon and the collapsed menus. Added `ConstructorView.isParameterlessInvokable : Command
  -> bool` (false for exactly those 9 inert arms, adjacent to `applyCommand` with a "keep in
  sync" comment) + `Controls.disabledButton`; `Ribbon.commandButton` now renders the
  gesture-only commands DISABLED (label visible / discoverable, but not a silent no-op
  click) in both surfaces. `tabCommands`/`tabOf` unchanged, so AC-D1/AC-D2 projection
  invariants still hold.
- [x] **Fix 2: dedicated `CloseHelp`.** Added `ConstructorView.Msg.CloseHelp` (only sets
  `helpOpen = false`); `Ribbon.helpOverlay` closes via it instead of `Invoke CancelOrDeselect`,
  so dismissing context help no longer deselects the active element / collapses the
  contextual Element tab. Mirrors `CloseValueIdModal`.
- [x] **Fix 3: dead `Ribbon.elementActions`.** Deleted the `ElementAction` type
  (`RegistryAction`/`BindValueId`) and the `elementActions` list (referenced only by the
  AC-F2 test, never by a view); repointed the AC-F2 test at the REAL exposure path — the
  bind-value button rendered on the contextual Element tab.
- [x] **Fix 4: reuse-critic F1 dedup.** Repointed `Ribbon.roleTitleKey TableRole` at the
  existing `table.title` and dropped the duplicate `element.opticalTable` `strings.json` key.
- [x] Added a `ui-tests` guard: gesture-only commands are not parameterless-invokable and
  render disabled; every other registry command is invokable and `ResetView` stays enabled.
- [x] Normalized touched files back to LF (host Edit tool emits CRLF; these files are
  LF-committed — verified the prior round's "CRLF-committed" SoW note was wrong via `file`).
- [x] Re-ran build / unit-tests / ui-tests / ui-smoke; updated the SoW Deferred section.

## Round 3 (retry) — code-judge route-back (cycle 2) → operator issue-hint

The cycle-2 code-judge routed back (gates all green) and the resolver issued the
operator hint for this round. The principle the cycle-1 retry established ("a generated
ribbon button with no visible effect must not be a live, clickable control") was applied
only to the *inert-return* subset; four element-edit commands on the contextual Element
tab — `OpenElementDialog`, `ElementContextMenu`, `ResetRotation`, `DeleteElement` — flip a
model flag or arm a `pending`, but no overlay renders their surface on the new DEFAULT
landing page, and `isParameterlessInvokable` returned `true` for them (the `| _ -> true`
wildcard), so the ribbon rendered them ENABLED. `ResetRotation`/`DeleteElement` are
sharper: they arm a `pending` the user can neither see nor clear. The hint is the
contract; no structural change needed (two code files, `Ribbon.fs` already keys off the
predicate).

- [x] Re-read the cycle-2 code-judge + architecture/reuse critics + resolver hint and the
  prior round's outputs.
- [x] **Fix (primary): the four element-edit commands render DISABLED.** Introduced ONE
  source-of-truth list `ConstructorView.commandsWithoutFrontDoorSurface` (the nine
  gesture-only commands AND the four element-edit commands) and rewrote
  `isParameterlessInvokable cmd = not (List.contains cmd commandsWithoutFrontDoorSurface)`.
  The ribbon's `commandButton` (`Ribbon.fs:224`) already reads the predicate, so it now
  renders all thirteen DISABLED in BOTH the ribbon and the collapsed menus — no `Ribbon.fs`
  edit. The list is deliberately BROADER than `applyCommand`'s inert-return arms (the four
  DO mutate the model), so the seam is a shared LIST, not model-equality; the docstring
  records this and the "drop it here when slice 007 renders the overlay" rule.
- [x] **Collapsed the duplication.** `RibbonTests.surfacelessCommands` now derives from
  `ConstructorView.commandsWithoutFrontDoorSurface` (no second hand-maintained nine/thirteen
  list); the predicate test asserts every command in the shared list is non-invokable and
  every other registry command IS invokable, plus an explicit guard that the four
  element-edit commands are in the disabled set.
- [x] **Extended a ui-test (rendered path).** Added
  `the contextual Element tab renders the four surfaceless element-edit commands disabled`:
  mounts the Element tab over a model with an element selected and asserts the four render as
  DISABLED buttons while an invokable Element-tab command (`Duplicate`) stays enabled.
- [x] **Kept `tabOf`/`tabCommands` untouched** — AC-D1 (every tab populated from the
  registry) and AC-D2 (every registry command surfaced exactly once across the tabs) hold;
  only the rendered enabled/disabled state of the four changed.
- [x] **Reconciled the contradictory CRLF-vs-LF SoW Gotcha.** Verified ground truth:
  `git ls-files --eol` reports ConstructorView.fs / Controls.fs as `i/lf w/lf`,
  `core.autocrlf=false`, no `.gitattributes`, and a byte scan of the touched files shows
  CRLF=0 (pure LF). The Round-2 impl-log note ("LF-committed") was correct; the SoW Gotcha
  ("CRLF-committed, NOT LF") was stale and is now corrected. The Edit tool preserved LF on
  both touched files (numstat == numstat `--ignore-cr-at-eol`, no CR churn).
- [x] Re-ran every gate (below); all green.

### Round 3 gate results

- `build` — PASS (exit 0, "Build succeeded.", 0 lowercase `error` matches). `006-build.log`.
- `unit-tests` — PASS (exit 0, Passed 84 / Skipped 5 / Total 89; baseline
  `berreman_unit_tests = 84` held; `BerremanTests` untouched). `006-unit-tests.log`.
- `ui-tests` — PASS (exit 0, Passed 79 / Total 79; 78 → 79, +1 Element-tab disabled-render
  test; the renamed predicate test stays at one). `006-ui-tests.log`.
- `ui-smoke` — PASS (exit 0, Passed 3). `006-ui-smoke.log`.
- `impl-log-structure`, `state-of-world-structure` — required ATX headings present.

`commit_ready: true` — the route-back finding is closed (the four element-edit commands
render disabled via the one shared list), the three-way duplication is collapsed to one
source of truth, a rendered-path ui-test guards it, AC-D1/AC-D2 invariants are intact, the
SoW Gotcha is reconciled, and every gate is green.
