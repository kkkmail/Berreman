# Step 0242 — impl plan

Obligation `S72-FLOW-STRIP-VIEW`: the constructor window's left flow strip, each step reachable directly, and the
surface its step shows.

## Approach

1. `OpticalConstructor.Controls/UiIds.fs`: a new group after the Items-panel group, `// --- The constructor's flow
   strip (FlowStripView). ---`, with `flowStrip`, `flowItemsStep`, `flowSetupsStep` and `flowExperimentsStep`, each
   with a one-line doc.
2. `OpticalConstructor.Ui/TableAndElementRotationView.fs`: `ConstructorFlowStep.flowUiId : ConstructorUiId`, an
   intrinsic member (Controls is compiled before the Ui project).
3. New `OpticalConstructor.Ui/FlowStripView.fs` (`module OpticalConstructor.Ui.FlowStripView`), compiled directly after
   `ItemsPanelView.fs` and before `KitWindowView.fs` (one `<Compile>` line, no comment). `flowStripView` is a `Border`
   carrying `flowStrip`, margin 8, 4, 0, 4, over a vertical `StackPanel` of one box per `ConstructorFlowStep.all`:
   the window's step an `actionButton` on `positiveBackground`, the others `verbButton`s, each sending
   `ShowFlowStep step`.
4. `OpticalConstructor.Ui/KitWindowView.fs`: `kitWindowView`'s `DockPanel` holds the kit bar (top), the strip (left),
   then the step's surface as the fill — `itemsPanelView` for Items, `mainView` for Setups and Experiments — keyed
   `items` / `workbench` with `View.withKey`, so a change between the two recreates the surface and Setups ↔
   Experiments keeps the workbench.
5. Tests:
   - `KitWindowTests`: `the flow strip UiIds are the stable intent-named ids` (no trait);
     `the flow strip shows Items, Setups and Experiments left of the surface its step shows` (no trait);
     `pressing the flow strip's steps shows the Items panel, the Experiments bay and the table` (`ui-smoke`).
   - `ScrollExtentTests`: `the kit window's Items step leaves everything reachable` — `assertReachable
     "KitWindowItems"` over `kitWindowView` on `initMain ()`, three items added by `AddItem`, the second selected, its
     first parameter's editor open as unknown, the add form open, on `ItemsFlowStep`.

## Files

All within the slice's authorized `edits`: `UiIds.fs`, `TableAndElementRotationView.fs`, `FlowStripView.fs` (new),
`OpticalConstructor.Ui.fsproj`, `KitWindowView.fs`, `KitWindowTests.fs`, `ScrollExtentTests.fs`.

## Risks

- The strip takes width from the left of the workbench: the existing kit-window scroll-extent law
  (`the kit window: the kit bar and the page stay reachable`) and every smoke fact that drives a constructor window
  must still hold. Run the whole ui-smoke suite and the full ui-tests suite once to see no other class regressed.
- The step boxes switch between `verbButton` and `actionButton` under the same key; both send the same constant
  message, so a press handler kept across the switch is still correct.
- The ribbon pane is found by `Border.Name` (`ConstructorUiId.ribbonPane`), the canvas by `Canvas.Name`, not by
  AutomationId — the tests look them up by name.
- ELEV: the new free function takes two parameters (model, dispatch), as `itemsPanelView` does; the member
  `flowUiId` is on the type. Names are new and unique (grep found none).
