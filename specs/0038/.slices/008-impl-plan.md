# Step 008 — impl plan (IMPLEMENT_CONTRACT SVC_XDUO_0001 WindowLauncher, attempt 1)

## Goal

Implement the step-007-declared `WindowLauncher` contract for real: a host-layer
`WindowRegistry` (ONE module-level mutable `Map<WindowKey, Window>`, outside any
Elmish model), `WindowLauncher.create` doing open-or-activate over it
(activate = `Window.Activate`; create = injected factory → register →
unregister-on-`Closed` → show), the Select-state show policy (`ShowDialog` owned
by the requesting window under `ModalSelectWindows`, `Show` under
`ModelessSelectWindows`; Browse always `Show`), the id-mint moved OFF the save
path (Add-opened editors mint `MaterialId.create ()` / `SampleId.create ()` at
window open, carry `EntryFreshness = NewUnsaved`, and Save routes
`addMaterial`/`addSample` vs `updateMaterial`/`updateSample` on that freshness),
and the workbench Add/Edit/Categories verbs rewired through the launcher
(subsuming `EditorLaunchers.defaults`). Lifecycle → `implemented`.

## Key design decisions

1. **The registry is module-level and shared; launcher records are cheap views
   over it.** The declared `openOrActivate : WindowKey -> Result<…>` carries no
   requesting-window/mode context, so per-call context (Browse vs Select owner)
   is baked into each `create`d record — and single-instance semantics across
   ALL of them comes from the ONE `Map` (`create factory modality openMode`).
   A new supporting DU `WindowOpenMode = BrowseOpen | SelectOpen of Window`
   carries the show policy; step 016's Select windows compose `SelectOpen`
   launchers over the same registry with `AppContext.settings`' switch.
2. **`WindowLauncher.fs` moves UP the compile order** (before
   `SampleEditorView.fs`): the editor views route Save on `EntryFreshness` and
   `EditorLaunchers.defaults` (in `TableAndElementRotationView.fs`) routes
   through the launcher — both compile after it. The step-007 "after AppContext"
   placement was for a `create` that took the context; the real `create` takes
   the factory/modality/mode directly, so nothing in the file needs AppContext.
3. **The id-mint happens at the verb dispatch (the window-open request):**
   `MatAdd` mints `newMaterialId ()` into a new
   `MaterialEditorView.MaterialEditorIntent = NewMaterial of MaterialId |
   EditMaterial of MaterialEntry` (replacing the `MaterialEntry option` seam
   argument — an option-plus-implicit-mint becomes a NAMED intent, the
   `SampleEditorIntent` precedent); `SmpAdd`/`SmpMakeMultilayer` mint
   `newSampleId ()` into `SampleEditorIntent`'s NEW cases, which gain a
   `mintedId` payload. `defaults` derives the registry key from the intent.
4. **Editor `EditorTarget`s become records `{ id; freshness }`** (id always
   present); `SaveClicked` matches the freshness — `NewUnsaved` → add,
   `Persisted` → update. The window ctors take the intents.
5. **Platform throws (factory ctor, `Activate`, `Show`/`ShowDialog`) are caught
   at the launcher boundary** and mapped onto `WindowFactoryFailed(key, reason)`
   (doc widened to "the open path failed"; no new error case, so no DU-extension
   ripple under `--warnaserror+:25`).
6. **Stale-close guard:** the `Closed` hook unregisters only when the registered
   window IS the closing window (reference equality), so a forget-then-recreate
   followed by the OLD window's close cannot drop the successor's registration.

## Files to modify

- `Ui/WindowLauncher.fs` — `WindowOpenMode`, private `WindowRegistry`, `create`;
  header/docs to `implemented`.
- `Ui/OpticalConstructor.Ui.fsproj` — move the compile entry up; comments.
- `Ui/MaterialEditorView.fs` — `MaterialEditorIntent`, record `EditorTarget`,
  `init` over the intent, Save on freshness.
- `Ui/MaterialEditorWindow.fs` — ctor takes the intent.
- `Ui/SampleEditorView.fs` — `SampleEditorIntent` NEW cases gain `mintedId`,
  record `EditorTarget`, init/Save reshaped.
- `Ui/SampleEditorWindow.fs` — title patterns.
- `Ui/TableAndElementRotationView.fs` — `EditorLaunchers.openMaterialEditor`
  takes the intent; `defaults` rewired through `WindowLauncher.create` (Browse);
  `MatAdd`/`MatEdit`/`SmpAdd`/`SmpMakeMultilayer`/`SmpEdit` arms build intents.
- `App/Program.fs` — comment accuracy only (the verbs now route through the
  SVC_XDUO_0001 launcher).
- `Ui.Tests/WindowLauncherTests.fs` — real-launcher suite: registry
  activate/forget/Closed-unregister/stale-close pins, Select-modality show
  behaviour (owner under modal, unowned under modeless), and the slice-mandated
  end-to-end proofs through `EditorLaunchers.defaults` (Edit-same-material-twice
  activates ONE window; Add-twice creates TWO NewUnsaved windows whose distinct
  upfront Guids persist through `addMaterial`; a sample Add persists through
  `addSample` under its minted id).
- `Ui.Tests/MainWorkbenchTests.fs` — recording-launcher patterns for the new
  intents + a pure pin that two Adds mint two distinct upfront ids; ctor sites.
- `Ui.Tests/MaterialEditorWindowTests.fs`, `Ui.Tests/SampleEditorWindowTests.fs`,
  `Ui.Tests/EmbeddedChartTests.fs` — intent/ctor/target-assert ripples.
- `specs/0038/.contracts-json` — SVC_XDUO_0001 lifecycle `implemented`,
  implementStep 8 (per the IMPLEMENT_CONTRACT obligation).

## Risks

- The shared registry is global test state: every test that opens through the
  registry must close its windows (the `Closed` hook unregisters). Existing
  WireUiComposition tests already close every editor they open.
- Deterministic seed ids (glass152 etc.) become registry keys in Edit flows —
  same-key tests must clean up or later tests would ACTIVATE a stale window.
- `ShowDialog` requires a visible owner headless; the modal test shows the owner
  first and asserts through `Window.Owner`.
- `--warnaserror+:25` (incomplete matches): every reshaped DU match must stay
  complete — the intent payloads only ADD binders, no new cases anywhere.
