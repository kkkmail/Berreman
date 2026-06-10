# 006 — State of the world: ribbon/menu shell, front door, Legacy; catalogue & value-id modal

## Where we are

Slice 006 realises **Part D** (the office-style ribbon-and-menu shell + the single
command source) and **Part F** (the standard element catalogue + the value-id binding
placeholder) of Spec 0026 (Optical Constructor UI). It turns the slice-005
`ConstructorView` page into the application's front door: a collapsible ribbon whose tabs
and drop-down menus are BOTH projected from the one slice-005 `Commands.registry` (no
second wiring site, constraint 0.4 / AC-D2), a contextual Element tab, the standard
catalogue, and the working empty value-id modal. The constructor becomes a top-level page
and the default landing; the *Legacy* entry still opens the present dockable-panel screen.
It builds on slice 003 (`Localization`), 004 (`Table`/`Drawer`/`ConstructorTable`/
`Controls`), and 005 (`Commands`/`ConstructorView`). It hangs nothing on the *Experiment*
tab's group/collection/detector semantics — that tab exists and projects its registry
command, but the semantics land in slice 007 (Part G/H/K).

## What's working

- Add `Ribbon.fs`: the ribbon/menu shell projected from the ONE command registry —
  Build/Element/Trace‑View/Experiment/Settings tabs, the collapsed drop‑down menus, and
  the contextual Element tab, all from `tabCommands` with no second wiring site (AC‑D1/D2/D3).
- Add the standard catalogue (Optical Table + LS/LP/CP/S/Lens/Flat Mirror/Curved Mirror/D)
  on the Build tab — no analyzer entry, LP/CP map to `Polarizer`, >2 polarizers allowed (AC‑F1).
- Wire the value‑id binding action + a working, dismissible empty modal into `ConstructorView`
  (`OpenValueIdModal`/`CloseValueIdModal`, `valueIdModalOpen`); add the `activeElement` helper (AC‑F2/D3).
- Make the constructor the top‑level front door and default landing page, add the *Legacy*
  entry, and route both through the existing `RootModel`/`RootMsg` (AC‑D4 / D.5 / D.6).
- Add `LocalHelp.fs`: context‑sensitive help resolving through `Localization`; add EN+RU
  `strings.json` keys for the ribbon/catalogue/nav/modal/help (both languages complete).
- Add `RibbonTests.fs` (15 `ui-tests` + 1 `ui-smoke`, incl. the retry guards that the
  surface-less commands render DISABLED in the ribbon/menus) and extend the app‑host smoke
  to render the Constructor + Legacy pages.

## Tests

All applicable gates pass locally (logs under
`C:\GitHub\Berreman\specs\0026\.artifacts\`):

- `build` — PASS (exit 0, "Build succeeded.", 0 Error(s), 0 lowercase `error` matches —
  the gate's `stdout_match` veto). `006-build.log`.
- `unit-tests` — PASS (exit 0, Passed 84 / Skipped 5 / Total 89; `BerremanTests`
  untouched). Baseline `berreman_unit_tests = 84` held. `006-unit-tests.log`.
- `ui-tests` — PASS (exit 0, Passed 79 / Total 79; 75 → 79 across the two retry cycles:
  the parameterless-invokable predicate, the Trace/View disabled-button render, `CloseHelp`
  keeps the selection, and the cycle-2 Element-tab disabled-render guard for the four
  element-edit commands; Category!=ui-smoke). `006-ui-tests.log`.
- `ui-smoke` — PASS (exit 0, Passed 3; 2 → 3, +1 constructor‑front‑door‑with‑modal mount).
  `006-ui-smoke.log`.
- `impl-log-structure`, `state-of-world-structure` — PASS (required ATX headings present).

Coverage: AC‑D1 the Build/Element/Trace‑View/Experiment/Settings tab set, each populated
from the registry; AC‑D2 every registry command surfaced exactly once across the tabs, and
a command rendered in BOTH the expanded ribbon and the collapsed menus (one projection, no
second site); AC‑D3 the contextual Element tab appended on selection; AC‑D4 ribbon labels
under EN and RU + the functional language switch; AC‑F1 the catalogue roster (no analyzer,
LP/CP → `Polarizer`, >2 polarizers); AC‑F2 the value‑id action in the element local menu +
the open/close/dismiss modal; D.5/D.6 the default landing + the Legacy navigation. The
constructor‑unit‑tests gate does not apply this slice (no Domain/Storage change). None
deferred.

```yaml
gates:
  berreman_unit_tests:    84
  constructor_unit_tests: 225
```

## Architecture

- **The registry is still the single source — the ribbon and menus are pure projections.**
  `Ribbon.tabOf : Command -> RibbonTab` classifies every registry command onto one tab;
  `Ribbon.tabCommands` filters `Commands.registry` by it. BOTH the expanded ribbon controls
  and the collapsed drop‑down menus render from `tabCommands`, so AC‑D2 is a property of the
  projection — there is no second binding site. `tabOf` ends in a wildcard, so a command a
  later slice adds to the registry still surfaces in both (defaults to the Build tab); slice
  007 reclassifies its group/detector commands here.
- **The ribbon is decoupled from the shell (acyclic compile order).** `Ribbon.Dispatch`
  carries plain callbacks (`onRibbon`/`onConstructor` + open‑Legacy / set‑language /
  toggle‑theme); the shell wires them to its `RootMsg`. The ribbon never references `Shell`,
  so the order `Commands` → `ConstructorView` → `LocalHelp` → `Ribbon` → `ConstructionView`
  → `Shell` stays acyclic. `LocalHelp` owns the one command‑label‑key mapping the ribbon
  reuses for both its expanded controls and its menus (a command's label declared once).
- **Pure, serializable models (constraint 0.3).** `Ribbon.Model` (collapsed flag + active
  tab) and the new `RootModel` fields (`constructor : ConstructorView.Model`, `ribbon :
  Ribbon.Model`, `strings : Localization.Resource`) carry no Avalonia handle — `strings` is
  a `Map`. Every Part‑D/F acceptance criterion is provable on the pure projections / pure
  `update`; two headless mounts (the AC‑D2 render and the constructor‑front‑door smoke) only
  assert the views render one frame.
- **Reuse, not re‑invention (R‑7 / R‑10 / AC‑J1).** The ribbon buttons reuse the slice‑004
  `Controls.fs` flavours; the catalogue maps through `Placement.toConstructorElement` (no new
  DU case; `Analyzer` untouched, F.3); navigation extends the existing `Page`/`RootModel`/
  `RootMsg` (no parallel nav). The legacy `Construction`/`SynthesisFit` pages keep their own
  sub‑nav + lifecycle toolbar under the Legacy branch, rendering exactly as before.
- **Nested top‑level navigation.** The front‑door nav (Constructor / Legacy) is the new top
  bar (reusing the `navButton` active styling, R‑11); `Page.Constructor` renders the
  ribbon + canvas + modal overlays; `Page.Legacy`/`Construction`/`SynthesisFit` render the
  present dockable screen. The value‑id and help overlays are layered over the constructor
  page in a single‑cell `Grid`, so an open dialog covers the page (a schematic‑grade modal).
- **Invokability is read from ONE "no front-door surface" list (retry, cycle 2).** The
  ribbon's `commandButton` asks `ConstructorView.isParameterlessInvokable cmd`, which is now
  `not (List.contains cmd ConstructorView.commandsWithoutFrontDoorSurface)` — the single
  source of truth holding the nine GESTURE-ONLY commands (`applyCommand` returns the model
  unchanged) AND the four ELEMENT-EDIT commands (`OpenElementDialog`/`ElementContextMenu`/
  `ResetRotation`/`DeleteElement`) that mutate the model but render no surface on the front
  door yet. The list is DELIBERATELY BROADER than `applyCommand`'s inert-return arms — the
  four DO change the model, so the seam is a shared list, not model-equality — which is why
  the earlier cycle's disabled set (nine only) left those four as enabled silent-no-op
  buttons on the default landing page; this cycle folds them in. The `RibbonTests`
  disabled-set derives from the same list (no second hand-maintained copy), collapsing the
  prior three-way duplication the code's own docstring flagged. The non-invokable commands
  render via `Controls.disabledButton`. The classification (`tabOf`/`tabCommands`) is
  untouched, so every command still surfaces exactly once across the tabs (AC‑D2) and on its
  tab (AC‑D1); only the button's enabled state changes. The help overlay now dismisses
  through a dedicated `ConstructorView.CloseHelp`
  (mirroring `CloseValueIdModal`) instead of `Invoke CancelOrDeselect`, so closing help no
  longer deselects the active element. The dead `Ribbon.elementActions`/`ElementAction` list
  was deleted (the real value‑id exposure is `bindValueButton` on the contextual Element tab,
  which the AC‑F2 test now asserts via a headless render), and the catalogue's optical‑table
  label reuses the existing `table.title` key (no `element.opticalTable` duplicate to drift).

## Deferred

- **Gesture-only ribbon/menu commands render DISABLED, not clickable.** `RotateR1/R2/R3`,
  `SlideAlongRay`, `MoveToRay`, `PanView`, `ZoomView`, `PlaceFromRibbon` need event context
  (a wheel notch, a drag delta, a drop kind/point), and the slice-007 `ToggleGroup` is a
  placeholder — `ConstructorView.applyCommand` returns the model unchanged for all of them,
  so `ConstructorView.isParameterlessInvokable` is false for exactly these nine and the
  ribbon renders them as disabled buttons in BOTH the ribbon and the collapsed menus (they
  stay discoverable on their tab but cannot silently no-op on a click). Giving them a
  single-step parameterless invocation (one rotation notch, one slide/zoom step) or a real
  gesture surface on the ribbon is a later-slice UX call; this slice only removes the
  silent-no-op defect. `ResetView`/`Undo`/`Redo`/`Save`/`Cancel` and the active-element
  commands stay enabled.
- The on-canvas right-click context menu, the element dialog (`elementDialogOpen`), and the
  pending-confirm gate are model flags the four element-edit commands
  `ElementContextMenu`/`OpenElementDialog`/`ResetRotation`/`DeleteElement` set, but no overlay
  renders them on the constructor front door yet (a slice-005 carryover). Because those four
  mutate the model with no rendered surface — and `ResetRotation`/`DeleteElement` arm a
  `pending` the user can neither see nor clear — they are now in
  `ConstructorView.commandsWithoutFrontDoorSurface`, so the ribbon and the collapsed menus
  render them DISABLED rather than as enabled silent-no-op (or armed-pending) buttons on the
  default landing page. The contextual Element tab's value-id action + modal IS rendered and
  working (AC-F2). Wiring the context-menu/dialog/confirm overlays onto the front door is
  slice 007; when an overlay lands, drop that command from the shared list so its button
  re-enables.
- The *Experiment* tab's group / experiment‑collection / detector semantics (Part G/H) —
  slice 007. The tab exists and projects its registry command (`ToggleGroup`), and slice 007
  hangs its new commands off the same registry (so they surface in the ribbon + menus
  automatically) and off the element context menu.
- Multi‑level undo/redo across all project‑mutating actions (Part K) — slice 007. The
  registry already binds Undo/Redo; the ribbon surfaces them on the Build tab.
- The Trace/View tab surfaces the registry view commands (Pan/Zoom/Reset view); the
  show‑central‑ray‑only / show‑bounding‑box toggles are `ConstructorView` model fields with
  no toggle message yet (slice 005 did not add one) — a follow‑up ribbon control, out of
  this slice's `ConstructorView` edit scope (value‑id modal + contextual‑tab selection).
- The real device/material picker behind the value‑id modal is out of scope (0.6 / F.2.2) —
  this slice ships only the working menu action + the working empty modal.
- The constructor page and the legacy construction page hold independent project snapshots
  this slice (no live sync between the two surfaces) — there is no requirement to sync them,
  and the canonical aggregate is the single project type either edits.

## Gotchas

- **EOL: the touched UI files are LF‑committed (`core.autocrlf=false`, no `.gitattributes`).**
  Verified ground truth this round — `git ls-files --eol` reports the tracked touched files
  as `i/lf w/lf` and a byte scan of every touched file shows CRLF=0 (pure LF). The host's
  Edit tool PRESERVES a file's existing EOL (LF files stay LF), so editing produces a clean
  diff with no CR churn (`git diff --numstat` == `--numstat --ignore-cr-at-eol`). Do NOT
  pre‑normalize or force CRLF on these files — just verify with the two numstats. (This
  corrects an earlier round's Gotcha that claimed the repo was "CRLF‑committed, NOT LF";
  that note was stale — the repo is mixed-EOL overall, but these UI files are LF.)
- **`Ribbon` is both a module and a `RootMsg`/sub‑model name.** The shell aliases the module
  as `Rb` (mirroring the existing `WS = Workspace` alias) so the case `RootMsg.Ribbon` and
  `Rb.update`/`Rb.Model` never collide in value position. Inside `Ribbon.fs`, `LocalHelp` is
  aliased `LH` for the same reason (the `Commands.LocalHelp` command case vs the module).
- **The build gate vetoes any lowercase `error` in stdout.** The build is clean (0 Error(s),
  and 0 lowercase `error` lines — verified); the only output is the pre‑existing
  NU1701/NU1902/MSB3277/SYSLIB0051/FS1125 warnings, none of which contain `error`.
- **`tabOf`'s wildcard is load‑bearing.** It is what makes a future registry command appear
  in the ribbon + menus without editing the projection. Removing it (going exhaustive) would
  turn a slice‑007 registry addition into a compile error rather than an auto‑surfaced tab
  control — keep the wildcard.
- **The contextual Element tab is appended, not inserted.** `visibleTabs` returns
  `persistentTabs @ [ElementTab]` (D.3 "appended beside … rather than taking over"); the
  effective active tab falls back to Build if a stale `activeTab = ElementTab` survives a
  deselection.

## Changelog

- 2026-06-06 (slice 006): Land Part D + Part F — the ribbon‑and‑menu shell, the constructor
  front door, the Legacy entry, the standard catalogue, and the value‑id binding modal. New
  `Ribbon.fs` (tabs + collapsed menus projected from the ONE slice‑005 registry, the
  contextual Element tab, the catalogue, the Legacy entry, the modal + help overlays) and
  `LocalHelp.fs` (context‑sensitive help + the one command‑label‑key mapping). Edit
  `ConstructorView.fs` (value‑id modal state/messages + `activeElement`), `Shell.fs`
  (Constructor + Legacy pages, `RootModel`/`RootMsg`, front‑door nav, default landing,
  `SetLanguage`), `Program.fs` (default‑landing note), and `strings.json` (EN+RU ribbon/
  catalogue/nav/modal/help keys). Add `RibbonTests.fs` (11 `ui-tests` + 1 `ui-smoke`) and
  extend `SmokeTests`. All gates pass (berreman_unit_tests 84 held, ui-tests 64 → 75,
  ui-smoke 2 → 3).
- 2026-06-06 (slice 006, retry): Fix the inert gesture-only ribbon/menu buttons on the new
  default landing page. Add `ConstructorView.isParameterlessInvokable` + `Controls.disabledButton`;
  the ribbon renders the nine gesture-only commands (Rotate R1/R2/R3, Slide, Move-to-ray,
  Pan, Zoom, Place, Toggle-group) DISABLED in both the ribbon and the collapsed menus instead
  of as silent no-op clicks, and the SoW Deferred section now calls them out. Add a dedicated
  `ConstructorView.CloseHelp` so closing context help no longer deselects the active element.
  Delete the dead `Ribbon.elementActions`/`ElementAction` list (the AC-F2 test now asserts the
  real exposure — the bind-value button on the contextual Element tab). Dedup the catalogue's
  optical-table label onto the existing `table.title` key (drop `element.opticalTable`). Gates
  stay green (berreman_unit_tests 84 held, ui-tests 75 → 78, ui-smoke 3, build clean).
- 2026-06-06 (slice 006, retry cycle 2): Close the cycle-2 route-back — the four element-edit
  commands (`OpenElementDialog`/`ElementContextMenu`/`ResetRotation`/`DeleteElement`) routed to
  the contextual Element tab rendered ENABLED on the default landing page yet had no rendered
  front-door surface (Reset rotation / Delete additionally armed an unresolvable `pending`).
  Introduce ONE source-of-truth list `ConstructorView.commandsWithoutFrontDoorSurface` (the
  nine gesture-only + these four) and derive `isParameterlessInvokable` and the `RibbonTests`
  disabled-set from it, collapsing the prior three-way duplication; the list is deliberately
  broader than `applyCommand`'s inert arms (these four mutate the model). The ribbon's
  `commandButton` already keys off the predicate, so only `ConstructorView.fs` and
  `RibbonTests.fs` changed — `tabOf`/`tabCommands` untouched (AC-D1/AC-D2 hold). Add a ui-test
  asserting the four render disabled on the Element tab; reconcile the stale CRLF-vs-LF SoW
  Gotcha to the verified LF ground truth. Gates stay green (berreman_unit_tests 84 held,
  ui-tests 78 → 79, ui-smoke 3, build clean).
