# Code judge — 008.slice-md cycle 1

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0035\.slices\008.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0035\.slices\008-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0035\.slices\008-impl-log.md`
- Gate results: build **pass**, unit-tests **pass**, constructor-unit-tests **pass**, ui-smoke **pass**, ui-tests **pass**
- Critic critiques: (none — the critic roster was empty this cycle)

## Rationale

Every deterministic gate is green and the diff satisfies every stated
requirement of slice 008. I verified this against `git diff HEAD` rather than
trusting the SoW alone.

**Requirement-by-requirement.** (1) The two-case content-mode DU is present and
is a DU, not a bool: `Ribbon.BayContentMode = InRibbonPane | FullSurface`
carried as a new `mode` field on `Ribbon.Bay` (`Ribbon.fs`). This honours the
project's "elevate every primitive / no naked bool" discipline. (2) In
`TableAndElementRotationView.mainView` the table canvas and its
pointer/wheel handlers are now built inside a `tableSurface ()` thunk that is
only realized for an `InRibbonPane`/`None` active bay; a `FullSurface` bay takes
the `belowStrip` branch that places the bay's own `b.content` and wires no table
gestures — exactly the "canvas + gestures only for a table bay" requirement.
(3) Materials (`materialsBay`) and Library (`samplesBay`) are tagged
`Ribbon.FullSurface`. (4) Both are moved to the LAST two positions in `mainBays`
and in `BayNames.all` (`[ … experiments; details; materials; library ]`).
(5) The spec-0033 G2 deferred-reorder comment and the Details-LAST pin are
retired in both `BayNames.all` and the two order-pinning tests.

**Test coverage of new public surface.** The new behavior is exercised by a test
in the diff. `MainWorkbenchTests` adds a `ui-smoke` acceptance test that asserts
the workbench-last order, that a table bay (Rotation) realizes `UiIds.canvas`,
that clicking Materials then Library shows each workbench's search box while
`UiIds.canvas` is **absent** (`Assert.False (isPresent … canvas)`), and that
returning to a table bay restores the canvas — real assertions, not a vacuous
pass. The ribbon's mode branch is additionally pinned by `RibbonPaneTests` (its
synthetic bays are `InRibbonPane`, proving the generic ribbon still hosts a
keyed pane and does not special-case the name "Materials"), and the reorder is
pinned by the updated `ExperimentControlsTests` full-order literal and
`LayerBandsControlsTests`. The ui-smoke count moving 90 → 91 confirms the added
test compiled and ran green; the shared helpers it uses (`isPresent`, `clickOn`,
`mountMain`, `freshStores`) are the established test-support functions used
across eight test files.

**SoW / impl-log fidelity.** The logs line up with the diff, including the
retry-02 root cause: the `WrapPanel` tab-strip change is present in `Ribbon.fs`
(with the 4-px bottom margin per tab), matching the "tabs overflowed off-window,
so wrap them" narrative. The impl-log claims the throwaway `ZZZDiagWorkbench.fs`
diagnostic was removed — `git status` confirms no such file lingers in the tree.
The one architecturally load-bearing choice (carry the mode as data on `Bay`
vs. map by name in the host) keeps the generic ribbon ignorant of tables and is
consistent with the step-007 genericity invariant.

No critic raised a finding; nothing in the diff contradicts a slice requirement,
introduces a layering violation, or leaves new public surface untested. This
meets the `done-green` bar on every clause of the rubric.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "All five gates pass and the diff satisfies every slice-008 requirement: a two-case Ribbon.BayContentMode DU (never a bool) with a mode field on Ribbon.Bay; the table canvas + pointer/wheel gestures are gated to table (InRibbonPane) bays only via a tableSurface thunk while FullSurface bays fill the belowStrip area with their own keyed content and no gestures; Materials and Library are tagged FullSurface and moved to the LAST two positions in both mainBays and BayNames.all; the Details-LAST pin and the spec-0033 G2 deferred-reorder comment are retired. New public surface is exercised by a ui-smoke test in the diff (MainWorkbenchTests asserts canvas present for a table bay and absent for both full-surface workbenches, plus workbench-last order), with RibbonPaneTests and the updated order tests pinning the mode branch and reorder. SoW and impl-log match the diff, including the WrapPanel tab-strip retry-02 fix and the removed throwaway diagnostic. No critics ran and no finding contradicts a slice requirement.", "retry_hint": ""}
```
