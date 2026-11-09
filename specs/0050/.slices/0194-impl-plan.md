# Step 0194 — impl plan

The Data locations window: a `HostWindow` over the step-193 screen, single-instance in the host registry under a new
`WindowKey` case (001 §G, spec 0050 S5.3).

## Approach

1. `OpticalConstructor.Controls/UiIds.fs`: in the Data locations group, add `dataLocationsWindow`
   (`"DataLocationsWindow"`) with a one-line doc.
2. `OpticalConstructor.Ui/WindowLauncher.fs`: `WindowKey` gains `DataLocationsWindowKey`, with a one-line doc. The only
   exhaustive match over `WindowKey` is the test root factory (grepped), so no product match breaks.
3. New `OpticalConstructor.Ui/DataLocationsWindow.fs`, compiled directly after `KitStartWindow.fs` (one `<Compile>`,
   no comment). `type DataLocationsWindow(locations : DataLocations, explorer : ExplorerProxy)` inherits `HostWindow`,
   as `KitStartWindow` does: title `Data locations`, `Name` and `AutomationId` from
   `ConstructorUiId.dataLocationsWindow`, 720 × 460, then `WindowFit.keepOnScreen this`, then `Program.mkSimple`
   over the context's `initDataLocations`, `updateDataLocations` and `dataLocationsView`.
   - Step 193 moved `initDataLocations` onto `DataLocationsContext` as a member (ELEV013), so the init is
     `fun () -> context.initDataLocations` over a typed context record.
4. `WireUiFinalCompositionTests.fs`: the root factory answers `DataLocationsWindowKey`, and the root-factory smoke
   fact opens it too.
5. Tests:
   - `ScrollExtentTests`: `the Data locations window leaves everything reachable`.
   - `DataLocationsWindowTests`: `the Data locations window is titled and carries its stable id` (no trait) and
     `the Data locations window opens once through the registry` (`ui-smoke`), through `WindowLauncher.create`.

## Risks

- Name collisions (ELEV014/015/016): grep `DataLocationsWindow`, `DataLocationsWindowKey`, `dataLocationsWindow`
  across Berreman and Softellect before writing.
- The registry is app-global: the smoke fact closes its window, so no key leaks into the root-factory sweep.
- `ScrollExtentTests` reachability: the screen is a `ScrollViewer` over a `MinWidthPanel`, as the kit start screen,
  so it should scroll at 800 × 600 and 1000 × 540.
