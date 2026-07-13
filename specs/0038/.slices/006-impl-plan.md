# Step 006 — impl plan

## Goal (slice 006, IMPLEMENT)

Hoist the five-proxy composition out of `MainConstructorWindow`'s constructor
(`OpticalConstructor.App/Program.fs`) into ONE app-scope composition value built at
startup before any window opens: an `AppContext` record (the `*Context` convention)
bundling LibraryProxy / ExperimentProxy / MaterialProxy / SampleProxy / CategoryProxy
plus the step-005 `WorkbenchSettings` record, injected into `MainConstructorWindow`
and every window opened later — so launcher-opened and constructor-opened windows
share the same in-memory stores. The parameterless `DefaultStores` composition
(`Ui/TableAndElementRotationView.fs:270-280` after the step-003 move) stays the
test-scene default. A headless test proves two surfaces composed from the app scope
observe the same store.

## Approach

1. **New `OpticalConstructor.Ui/AppContext.fs`** (namespace `OpticalConstructor.Ui`,
   compiled last in the Ui list): `[<ReferenceEquality>]` record `AppContext` with
   fields `library / experiments / materials / samples / categories / settings`
   (proxies are themselves `[<ReferenceEquality>]` function records — the context
   compares by reference: ONE context IS the app scope). `static member create
   (settings : WorkbenchSettings) : AppContext` builds the stores in the canonical
   coupling order (samples → materials over `samplesReferencing` → categories over
   `materialsReferencingCategory`, plus the library/experiments read seams) — the
   exact composition `MainConstructorWindow` performs today.
2. **`App/Program.fs`**: `Startup` gains `let context = AppContext.create
   workbenchSettings` (module init runs when `App.Initialize` reads
   `Startup.settings` — before any window opens). `MainConstructorWindow(context :
   AppContext)` consumes the injected proxies via `initMainWith`;
   `LauncherWindow(context : AppContext)` holds the scope and passes it to every
   `MainConstructorWindow` its Main button opens;
   `desktop.MainWindow <- LauncherWindow(Startup.context)`. The
   `open OpticalConstructor.Domain.Library` (only needed for in-window proxy
   construction) goes away.
3. **Ui.Tests re-points** (constructor signature change): `WireUiCompositionTests.mountRoot`,
   `LauncherTests` (two `LauncherWindow()` sites), `TableAndElementRotationTests:218`
   — each builds a FRESH `AppContext.create WorkbenchSettings.defaults` per mount
   (test isolation: the stores are mutable).
4. **New `Ui.Tests/AppContextTests.fs`**:
   - pure pins (`ui-tests` gate): `create` carries the settings verbatim; `create`
     couples materials→LIVE samples (`removeMaterial glass152` →
     `MaterialStillReferenced`); two workbench models composed from ONE scope via
     `initMainWith` observe the same store (a `removeSample` through model 1 vanishes
     from model 2's listing); two separate `create` scopes do NOT share.
   - headless acceptance (`ui-smoke` gate): TWO real `MainConstructorWindow`s over
     ONE context — Materials bay → Add → the REAL Material editor → name + Save
     through window 1; the minted id is recovered through the shared store; window 2's
     Materials bay lists the new row (and window 1 does after its next render).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/AppContext.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/WireUiCompositionTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LauncherTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/TableAndElementRotationTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/AppContextTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`

## Risks

- The Material editor's Save with only a name set (default complexity) must produce a
  valid entry — the 023 round-trip test filled indices; if the default state is not
  savable the headless test adds the index edits. Verified by the diagnostic run.
- Two concurrent Elmish windows on one headless session — each `HostWindow` runs its
  own loop; the `WindowOpenedEvent` subscription is installed only after both are
  shown so exactly the editor is captured.
- `TableAndElementRotationTests` does not open `OpticalConstructor.Ui` (only the view
  module) — the one construction site is fully qualified instead of adding opens.
- LF endings; zero new warnings; no git commands (arc-runner owns commits).
