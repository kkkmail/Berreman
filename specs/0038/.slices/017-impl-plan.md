# Step 017 — impl plan (IMPLEMENT, attempt 1)

## Goal

Rework the Selector-bay flow (spec 0038 Part G): with an element selected, a **Choose…**
verb opens the Library window in Select state constrained to the element's kind (through
`WindowLauncher`); the inline quick-pick strip (today's kind-constrained rows + confirm
panel via `LibraryControls`) renders ONLY while the kind-constrained entry count is BELOW
the step-005 `QuickPickThreshold` (default 5) — at or above it the bay shows Choose…
alone. BOTH paths converge on the SAME bind message that commits `placement.valueId`
(the step-016 targeted `BindValueIdTo`). Stable ids for Choose… and the strip; headless
proofs for the gating and for the identical-valueId convergence.

## Approach

All production changes live in `OpticalConstructor.Ui/TableAndElementRotationView.fs`
(touches: Ui + Ui.Tests only; App composition stays for step 47):

1. **Model** gains `quickPickThreshold : WorkbenchSettings.QuickPickThreshold` and
   `selectWindowModality : WorkbenchSettings.SelectWindowModality`, defaulted in
   `initWith` (the step-47 composition threads the appsettings values by record update).
2. **`EditorLaunchers`** gains `openLibrarySelectWindow` — the Select-state open of the
   single-instance Library window through `WindowLauncher.SelectOpen` (fresh open =
   `LibraryWindow(..., mode = Select ctx)`; a LIVE key re-targets through the step-016
   `LibraryWindow.Retarget` downcast). Returns the session's cancel-and-close handle
   (`(unit -> unit) option`; None = failed open, typed error dropped at this unit seam —
   the record's precedent).
3. **One commit site**: `update` goes `let rec`; the `BindValueId` and
   `ConfirmBindValueId` arms DELEGATE to `BindValueIdTo` (resolving the selected element
   / pending entry), so `placement.valueId` is committed in exactly one arm. The
   `BindValueIdTo` arm upgrades `activeSelect = None` to `cancelActiveSelect` (a strip
   commit while the Choose… window is still open cancels AND closes it; the window path
   already closed itself — the second `Close()` is Avalonia's safe no-op).
4. **Session bookkeeping messages**: `SelectSessionStarted of SelectSession` (store the
   handle) and `SelectSessionEnded of SelectSession` (reference-keyed clear, so a
   superseded session never clears its successor).
5. **View**: public `SelectorOffer` DU (`QuickPickAndChoose | ChooseAlone |
   NoSelectorOffer`) + `selectorOffer` (count = the kind-constrained entry ids the strip
   is built from, vs `model.quickPickThreshold`); `chooseFromLibrary` composes the
   `SelectionContext` in the VIEW (the workbench runs `mkSimple`, so the render's
   `dispatch` is the only return path) — `onSelected` dispatches the targeted
   `BindValueIdTo`, `onCancelled` a ref-cell-fed reference-keyed `SelectSessionEnded`;
   the Choose… button resolves its owner window from the pointer event's `TopLevel` and
   re-subscribes on the element id (its closure captures only render-stable state);
   `selectorBayContent` renders Choose… (+ the keyed, AutomationId'd strip below the
   threshold) and keeps today's disabled empty bay for no selection. New `WorkbenchIds`:
   `SelectorChooseButton`, `SelectorQuickPickStrip`.

## Tests

- `MainSceneMsgTests` (pure, ui-tests): strip-confirm ≡ targeted-bind model equality
  (the convergence), `BindValueId` delegation, `selectorOffer` gating incl. the
  at-threshold boundary, session start/end reference discipline, bind-closes-session.
- `LibraryWindowTests` (headless, ui-smoke): over-threshold kind → Choose… and NO strip;
  under-threshold kind → strip + Choose…; the acceptance — binding through the strip and
  through the real Choose…-opened Select window land the IDENTICAL valueId (Component
  mount exposing the live model); a second Choose… re-targets (one window, session
  handle survives reference-keyed).
- `LibraryControlsTests`: the two headless strip drives select a Sample (11 seeded
  entries ≥ default 5 → gate closed) — widen the threshold in their fixtures; the strip
  mechanics they pin are unchanged below the threshold.

## Risks

- Double `Close()` on the Select window when a bind lands while it is open — analyzed
  safe (Avalonia `CloseInternal` is idempotent; the host's `SelectDismissed` queues
  behind the in-flight update and finds `Browse`).
- Stale-closure hazards in FuncUI `SubPatchOptions.OnChangeOf` — the Choose… handler
  captures only app-scope-stable state and re-keys on the element id; the strip keeps
  its stateless message handlers (the resolution happens in `update`).
- App-global `WindowRegistry`: every headless test that opens a Select window closes it.
