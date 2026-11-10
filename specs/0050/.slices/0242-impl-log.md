# Step 0242 — impl log

## Progress

1. Read the protocol, the project prompt, the slice, AGENTS.md, manual 015 and the 0241 impl-log and state of the
   world. Surveyed `KitWindowView.fs`, `ItemsPanelView.fs`, `ConstructorFlowStep`, `Ribbon.ribbonView`,
   `MaterialsWindowView.verbButton` / `actionButton`, `KitWindowTests`, `ItemsPanelTests` and `ScrollExtentTests`.
2. `UiIds.fs`: the flow-strip group after the Items-panel group — `flowStrip`, `flowItemsStep`, `flowSetupsStep`,
   `flowExperimentsStep`, each with a one-line doc.
3. `TableAndElementRotationView.fs`: `ConstructorFlowStep.flowUiId`, one-line doc.
4. New `FlowStripView.fs` (`flowStripView`), compiled after `ItemsPanelView.fs` with one `<Compile>` line.
5. `KitWindowView.kitWindowView`: kit bar (top), the strip docked left, the step's surface as the fill, keyed `items`
   or `workbench`.
   - **First Ui build: red.** `View.withKey` keys a typed `IView<'view>`; both surfaces are plain `IView`. Each
     surface now sits in a keyed `Border`, the shape of `Ribbon.ribbonView`'s keyed pane.
6. Tests: three `KitWindowTests` facts, one `ScrollExtentTests` fact.
7. **First selected ui-tests run: red, 26 of 28** (`0242-ui-tests.log`):
   - `the kit window: the kit bar and the page stay reachable` — at 1000 × 540 the tallest kit bar leaves the strip
     too little height and the Experiments box's bottom is clipped. **Fix (product, in scope):** the strip is docked
     left inside a vertical `ScrollViewer` (`Auto`) in `kitWindowView`.
   - `the kit window's Items step leaves everything reachable` — at 800 × 600, 71 corners of the add form's offers
     unreachable. **Cause:** the offer list (`addItemOfferList`, step 241) is a `ScrollViewer` nested in the panel's
     own `ScrollViewer`. Avalonia's `ScrollContentPresenter` marks a bring-into-view request handled as soon as it
     scrolls, so the law's single `BringIntoView` never reaches the outer viewer. **Evidence that it predates this
     step:** the same fixture mounted over `ItemsPanelView.itemsPanelView` alone fails with the identical 71 corners.
     **Fix (in the authorized `ScrollExtentTests.fs`):** `unreachable` now sends one request per enclosing
     `ScrollViewer`. A second request passes an inner presenter that has nothing left to scroll and reaches the next
     viewer out. This matches the helper's documented contract ("`BringIntoView` scrolls every enclosing viewer")
     and what a user can do: scroll the list, then the page. No assertion is removed.
8. After both fixes: all 15 `ScrollExtentTests` facts pass (both traits).
9. **Mutation:** the strip back in a plain `Border` (no scroll) under the new harness →
   `the kit window: the kit bar and the page stay reachable` fails at 1000 × 540 on the same two corners. The
   harness still catches real clipping. Source restored byte for byte from `0242-KitWindowView.fs.bak`.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/FlowStripView.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitWindowView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`

All seven are in the slice's authorized `edits`.

10. Extended the smoke fact with the keyed surface's contract: the table canvas is a new instance after Items, and
    the same instance across Experiments → Setups. A tenth mutation (the workbench keyed by its step) is caught by
    it. `0242-mutations.py` / `0242-mutations-run.log`: nine mutations, all caught, every source restored by SHA-256
    and rebuilt.

## Decisions

- **Keyed surface in a `Border`.** The brief asks for `View.withKey` with `items` / `workbench`. FuncUI 2.0's
  `View.withKey` takes a typed `IView<'view>`, and `itemsPanelView` / `mainView` return a plain `IView`. Each surface
  is therefore the child of a keyed `Border`, which is exactly `Ribbon.ribbonView`'s keyed pane. The keys and their
  effect are as written: a step change between Items and the workbench recreates the surface; Setups ↔ Experiments
  patches the same workbench (the smoke fact proves both).
- **The strip scrolls in its host.** `flowStripView` is the brief's `Border` (`flowStrip`, margin 8, 4, 0, 4) over a
  vertical `StackPanel`. `kitWindowView` docks it left inside a vertical `ScrollViewer` (`Auto`). Without it, the
  tallest kit bar at 1000 × 540 clips the Experiments box, and the existing law `the kit window: the kit bar and
  the page stay reachable` fails. The slice requires that law to keep its assertion with the strip present.
- **The scroll-extent law sends one request per enclosing viewer.** See progress 7. The 71 corners are the Items
  panel's own (step 241): the add form's offer list is a `ScrollViewer` inside the panel's `ScrollViewer`. Avalonia's
  `ScrollContentPresenter` marks a request handled once it scrolls, so one `BringIntoView` never reaches the outer
  viewer. Repeating the request once per enclosing `ScrollViewer` lets each level scroll in turn. That is what the
  helper's doc already promised ("`BringIntoView` scrolls every enclosing viewer") and what a user does: scroll the
  list, then the page. It changes no assertion, and an unscrolled strip still fails (progress 9). Editing
  `ItemsPanelView.fs` instead is outside this step's authorized edits.
- **The step boxes' look.** The shown step is an `actionButton` on `positiveBackground` and the others are
  `verbButton`s, step 241's chosen-option look. A box keeps its key across the switch; every box sends the same
  constant `ShowFlowStep step`, so a press handler kept across the switch is still right.
- **The Items-step fixture** adds three circular polarizers (`circularPolarizerModelId`, fixed mounting) through
  `AddItem`, selects the second, opens its first parameter (`parameters` head) as unknown, then opens the add form.
  It asserts that state before mounting, so the law cannot pass over a fixture that silently lost the form or the
  editor.

11. Comment-only edit (the strip scroller's comment cites spec 0050 S7.2), final solution rebuild, then the final
    runs below.
12. Whole ui-tests suite as a regression check: one red outside the selection,
    `InverseConstructorTests.recommitting a copied experiment preserves its assigned physical ids`. It is model-only
    and deterministic, and dates from step 0227's commit refusal. Reproduced on a `git archive HEAD` copy (see
    Testing state).

## Testing state

Development runs over the final tree (no gate verdict is claimed, Invariant 6):

- build: exit 0, `0 Warning(s)`, `0 Error(s)`.
- constructor-unit-tests: `Passed!`, 1377 / 1377, 66 s.
- ui-smoke: `Passed!`, 244 / 244, 201 s (deadline 600 s). The new smoke fact is `Passed` in the TRX.
- ui-tests over `KitWindowTests` and `ScrollExtentTests`: `Passed!`, 28 / 28, 25 s. The three new facts are `Passed`
  in the TRX.
- elevation from `f395597`: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 202 s.
- The fourteen s24 invariant greps: none finds anything.
- Mutations: nine, all caught.
- Whole ui-tests suite (not the gate's selection): 672 / 673. The failure is the pre-existing
  `InverseConstructorTests` fact above; `InverseConstructorTests.fs` is outside this step's edits and the fact is
  left as found. Step 0261 selects that class.
- `HEAD` reproduction: `git archive HEAD` was extracted to `.artifacts/0242-head-check/`, without this step's
  changes. Its Ui.Tests built (exit 0) and the same fact failed with the identical message, so the red predates this
  step. The scratch copy was deleted afterwards and its two logs kept.

## Artifacts

- `specs/0050/.artifacts/0242-build.log` — the final solution build.
- `specs/0050/.artifacts/0242-ui-tests.log` — the final selected ui-tests run (the first, red run is described in
  progress 7).
- `specs/0050/.artifacts/0242-ui-smoke.log`, `0242-constructor-unit-tests.log`, `0242-elevation.log`,
  `0242-ui-tests-whole.log`.
- `specs/0050/.artifacts/0242-mutations.py`, `0242-mutations-run.log`.
- `specs/0050/.artifacts/0242-head-check-build.log`, `0242-head-check-test.log` — the `HEAD` reproduction of the
  `InverseConstructorTests` red.
- `specs/0050/.artifacts/0242-ScrollExtentTests.fs.bak`, `0242-KitWindowView.fs.bak` — restore points of the two
  experiments.

## Gotchas

- **Nested scroll viewers and `BringIntoView`.** A `ScrollContentPresenter` that scrolls marks the request handled,
  so a single request does not reach an enclosing viewer. The law's `unreachable` now repeats the request once per
  enclosing `ScrollViewer`. A focus-driven bring-into-view in the real app has the same limit; the user still reaches
  every corner by scrolling each viewer.
- **Headless fonts are wide.** The strip's boxes measure about 200 px headless ("Experiments" is 154 px of text).
- **The table canvas and a ribbon pane carry their id as `Name`, not AutomationId.** The new facts look them up with
  `tryNamedControl`.
- **A from-scratch single-project build recompiles vendored MathNet** and shows `SYSLIB0051` there (pre-existing,
  0241's gotcha). The solution builds (the gate's command) showed `0 Warning(s)`.
