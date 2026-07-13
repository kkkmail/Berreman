# Step 007 — impl plan

## Goal (slice 007, ADD_CONTRACT SVC_XDUO_0001 WindowLauncher)

Declare (lifecycle `declared` — the real construction is step 008's
IMPLEMENT_CONTRACT) the window-opening seam in `OpticalConstructor.Ui`,
generalizing the `EditorLaunchers` function-record precedent
(`Ui/TableAndElementRotationView.fs:116-141` after the step-003 move):

- `WindowKey` DU naming every managed window — `MaterialsWindowKey`,
  `LibraryWindowKey`, `SolverHandoffWindowKey`, `CategoryEditorKey`, and the
  id-keyed editor keys `MaterialEditorKey of MaterialId` /
  `SampleEditorKey of SampleId` (structural equality/comparison over the
  elevated Guid ids — the registry keys uniformly by id from the first moment).
- `EntryFreshness = NewUnsaved | Persisted` (spec §0.1: the operator's
  "is-new flag" as a two-case DU, never a bool) — new entities mint their Guid
  at Add-window open; step 008 routes Save on it (add… vs update…).
- `[<ReferenceEquality>]` record `WindowLauncher` of camelCase
  Result-returning functions:
  - `openOrActivate : WindowKey -> Result<WindowLaunchOutcome, WindowLauncherError>`
    — create through the injected per-key window factory when the key is
    absent, activate the live window when present (the `RetargetedWindow`
    outcome case is declared now; step 016 makes Select-state opens re-target).
  - `forgetWindow : WindowKey -> Result<unit, WindowLauncherError>` — forget a
    key on close (absent key → typed `WindowNotRegistered`, never a throw).
  - `decideSelectModality : unit -> Result<SelectWindowModality, WindowLauncherError>`
    — the step-005 `SelectWindowModality` switch baked in at construction
    (Browse windows never consult it; step 008 maps Modal → `ShowDialog`
    owner = requester, Modeless → `Show`).
- A mock, `WindowLauncher.createMock (factory) (modality)`, over a private
  `ref Map<WindowKey, Window>` (the `createInMemory` ref-Map precedent) with
  the caller-injected stub factory recording creations — enough registry
  bookkeeping to pin the open-or-activate semantics without any real
  windowing behaviour (no Activate/Show/Closed hooks — step 008's job).

Plus the mock-driven Ui.Tests suite exercising every record field through its
exact signature; acceptance: a second open of the same `WindowKey` activates
instead of creating, two different editor keys create two windows.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/WindowLauncher.fs` (new) —
  module `WindowLauncher` (the `WorkbenchSettings` module-wrapping precedent)
  holding the five types + `createMock`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
  — compile entry after `AppContext.fs` (end of list).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/WindowLauncherTests.fs`
  (new) — pure signature pins (gate `ui-tests`): modality decision for both
  switch values, forget-of-unknown-key typed error, factory-failure
  propagation without registering, id-keyed key equality + `EntryFreshness`
  distinctness; headless behaviour (gate `ui-smoke`, windows constructed but
  never shown, inside `HeadlessSession.run`): second-open-activates for every
  singleton key, two minted editor keys create two windows while the same
  minted id activates, forget-then-reopen creates afresh.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
  — compile entry after `AppContextTests.fs`.

## Risks

- `Window()` construction needs the headless platform — every test that lets
  the stub factory run wraps in `HeadlessSession.run` (the LauncherTests
  pattern); the pure pins never invoke the factory.
- FS0025 is `--warnaserror` — every match over `WindowLaunchOutcome` (three
  cases incl. the step-016 `RetargetedWindow`) carries an explicit failure arm.
- The contract registry `.contracts-json` already records SVC_XDUO_0001 as
  `declared` at step 7 (supervisor-maintained) — no worker edit needed.
- LF endings; zero new warnings; no git commands (arc-runner owns commits).
