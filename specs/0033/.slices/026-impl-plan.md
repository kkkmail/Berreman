# Impl plan — spec 0033, slice 026 (WIRE_UI)

## Reading of the slice

Step 026 finalizes the Main-scene composition root and verifies the whole
surface headless. Investigation shows slice 024 already threaded the exact
composition the slice text describes — `MainConstructorWindow`
(`OpticalConstructor.App/Program.fs`) builds `SampleProxy.createInMemory ()`
first, then `MaterialProxy.createInMemory (samplesReferencing samples)`, beside
the existing library/experiments proxies, and injects all four through
`initMainWith`; the launcher path is unchanged. Slice 024's own comment records
"the final WIRE_UI step owns the composition acceptance" — so this round's work
is that acceptance: a headless wiring assertion that drives input on the REAL
wired window (real stores, real `EditorLaunchers.defaults`) and asserts the
semantic-tree projection, per the WIRE_UI family obligation.

Existing ui-smoke coverage renders both editor windows standalone
(`MaterialEditorWindowTests` / `SampleEditorWindowTests`), the Materials/Library
bays over INJECTED stores + recording launchers (`MainWorkbenchTests`), and the
Selector bay over a hand-built model (`LibraryControlsTests`). Nothing yet
drives the REAL `MainConstructorWindow`'s wired proxies + default launchers
end-to-end (the existing real-window test only swaps Render/Rotation tabs).

## Mechanism (validated up front)

The default launchers open UNOWNED editor windows (`.Show()`), so the test
observes them via the public global routed event `Window.WindowOpenedEvent`
(`.Raised : IObservable<struct (obj * RoutedEventArgs)>`) — empirically
confirmed to fire for an unowned `Show()` under the headless platform
(scratch probe `specs/0033/.artifacts/026-scratch-windowopened.fsx`). The whole
test body runs as one dispatched action on the shared headless UI thread, so
the global subscription cannot observe another test's windows.

## Files to modify

1. **NEW** `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/WireUiCompositionTests.fs`
   — two `[<Trait("Category", "ui-smoke")>]` facts driving the REAL
   `OpticalConstructor.App.MainConstructorWindow`:
   - *Bay sweep*: click every ribbon tab (`BayNames.all`) — one frame per bay
     without throwing — with signature-control assertions for the three
     slice-named bays (Selector: kind label / readout / tree; Materials: search
     box + seeded rows; Library: search box + seeded rows). Seeded rows prove
     the root wired LIVE stores.
   - *Editors + root coupling*: Materials → Add opens the REAL
     `MaterialEditorWindow` via `EditorLaunchers.defaults` (asserted by
     AutomationId through `WindowOpenedEvent`); Library → select + Edit opens
     the REAL `SampleEditorWindow`; then removing a REFERENCED material through
     the real window surfaces "still referenced" — proving the root's materials
     store consults the root's LIVE samples store (`samplesReferencing`).
2. `OpticalConstructor.Ui.Tests.fsproj` — `<Compile Include="WireUiCompositionTests.fs" />`
   after `MainWorkbenchTests.fs`, with the house comment.
3. `OpticalConstructor.App/Program.fs` — comment-only: the 024 "threaded
   mechanically; the final WIRE_UI step owns the composition acceptance" note
   updates to record the step-026 finalized state.

No `OpticalConstructor.TestWindows` change expected — every automation id the
assertions need already exists.

## Risks

- The editor-window observation relies on `WindowOpenedEvent` firing headless —
  de-risked by the scratch probe above.
- Row clicks need the row inside the scroll viewport — mitigated by narrowing
  the search first (the `MainWorkbenchTests` precedent).
- `ui-tests` gate count must not regress: the new file carries ONLY ui-smoke
  facts, so `Category!=ui-smoke` is untouched (306 stays).

## Gates (local advisory runs; the arc-runner gate engine is the authority)

build, unit-tests (119 baseline), constructor-unit-tests (416), ui-smoke (81 +
new facts), ui-tests (306).
