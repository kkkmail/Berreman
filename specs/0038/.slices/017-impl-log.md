# Step 017 — impl log (IMPLEMENT, attempt 1)

## Progress

- [x] Recon: slice, step-016 SoW + machinery (WindowMode, WindowLauncher SelectOpen /
      Retarget, SelectSession, BindValueIdTo), LibraryControls strip, WorkbenchSettings
      QuickPickThreshold, existing test suites.
- [x] Production: TableAndElementRotationView.fs rework (Model fields, launcher seam,
      one-commit-site update, Choose…/strip bay content, stable ids).
- [x] Tests: MainSceneMsgTests +4 pure, LibraryWindowTests +4 headless,
      LibraryControlsTests fixtures widened, MainWorkbenchTests stub launcher extended.
- [x] Diagnostic build + all four suites (build clean; 119 / 571 / 148 / 398).
- [x] SoW + exit.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
  - `open OpticalConstructor.Domain.WorkbenchSettings` (the `QuickPickThreshold` single-case
    DU's case shadows the type under `WorkbenchSettings.`-qualified expression resolution, so
    `QuickPickThreshold.defaultValue` needs the module open).
  - `EditorLaunchers` + `openLibrarySelectWindow`: the Select-state open of the single-instance
    Library window through `WindowLauncher.SelectOpen` (fresh open = `LibraryWindow(...,
    mode = Select ctx)`; a LIVE key re-targets through the step-016 `LibraryWindow.Retarget`
    downcast; Created/Activated/Retargeted all return `Some (fun () -> window.Close ())`,
    a failed open `None` — the typed error is dropped at this unit seam, the record's
    precedent).
  - `Model` + `quickPickThreshold` / `selectWindowModality` (defaulted in `initWith`; step 47
    threads the appsettings values by record update).
  - `Msg` + `SelectSessionStarted` / `SelectSessionEnded of SelectSession`.
  - `update` → `let rec`; `BindValueId` and `ConfirmBindValueId` DELEGATE to `BindValueIdTo`
    (one commit site of `placement.valueId`); `BindValueIdTo` now runs `cancelActiveSelect`
    (a strip commit while the Choose… window is open cancels AND closes it); the two session
    arms (`SelectSessionEnded` is reference-keyed over the `[<ReferenceEquality>]`
    `SelectSession`).
  - View: public `SelectorOffer` DU + `selectorOffer` (count = `allowedEntryIds` — the same
    kind-constrained set the strip rows are built from — strictly below `quickPickThreshold`
    → strip); `chooseFromLibrary` (composes the `SelectionContext` over the render's
    dispatch; `onSelected` = the targeted `BindValueIdTo`, `onCancelled` = ref-cell-fed
    reference-keyed `SelectSessionEnded`); `selectorChooseButton` (owner window resolved from
    the pointer event's `TopLevel`; subscription re-keys on the element id — the closure's
    only render-varying capture); `selectorBayContent` (no selection → today's disabled empty
    bay; else Choose… + the keyed AutomationId'd strip below the threshold); `mainBays` wires
    the Selector bay through it. `WorkbenchIds` + `SelectorChooseButton` /
    `SelectorQuickPickStrip`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MainSceneMsgTests.fs`
  - `open OpticalConstructor.Domain`; step-017 pure section: strip-confirm / direct-bind ≡
    targeted-bind model equality (the convergence proof), threshold gating incl. the
    at-threshold boundary and no-selection cases, session start/end reference discipline,
    committed-bind-cancels-and-closes (incl. the vanished-target variant).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LibraryWindowTests.fs`
  - step-017 headless section: `mountMainExposed` (re-rendering FuncUI Component exposing the
    LATEST model — the acceptance compares model-level valueIds); over-threshold → Choose…
    alone / no strip; under-threshold → strip + rows + Choose…; THE acceptance — strip bind
    and real Choose…→Select-window bind land the IDENTICAL valueId (real launcher, real
    LibraryWindow, kind-constrained banner + corpus asserted, session handle held while open
    and cleared by the bind); second Choose… re-targets (one window; window-side close clears
    the handle through the reference-keyed end).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LibraryControlsTests.fs`
  - `withWideQuickPick` fixture (threshold 100) for the two headless strip drives: the
    seeded Sample kind (11 entries) now sits over the default threshold, and those tests pin
    the STRIP mechanics, which are unchanged below the threshold.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MainWorkbenchTests.fs`
  - the recording `EditorLaunchers` stub gained the new `openLibrarySelectWindow` field.

## Decisions

- **Convergence = ONE commit arm.** `update` goes `let rec`; `BindValueId` and
  `ConfirmBindValueId` delegate to `BindValueIdTo` instead of committing themselves. The
  strip's Confirm button lives in `LibraryControls` (Controls project — NOT in this step's
  touches) whose `unit -> unit` handlers are subscribed under a fixed
  `SubPatchOptions.OnChangeOf` token, so a view-side closure carrying the pending id would go
  stale; message-based delegation resolves the ids in `update` over the CURRENT model and
  still lands every path on the single `BindValueIdTo` commit site. The pure convergence test
  asserts the resulting MODELS are structurally identical.
- **Choose… composes in the VIEW.** The workbench is `mkSimple` (no Cmd seam and no captured
  dispatch, unlike the window hosts' `Cmd.ofEffect`), so the render's `dispatch` is the only
  return path into the loop. The handler captures only render-stable state (proxies /
  launcher seam / modality / the element it re-keys on), so the `OnChangeOf` subscription
  cannot go stale.
- **Reference-keyed session end.** A re-target fires the superseded session's `onCancelled`
  while the successor is being stored; `SelectSessionEnded` compares the
  `[<ReferenceEquality>]` handle, so the stale cancel is a no-op (proved pure + headless).
- **A committed bind cancels AND closes a still-open session** (`cancelActiveSelect` inside
  `BindValueIdTo`): the strip path can commit while the Choose… window is open. On the window
  path the window already closed itself — the second `Close()` is Avalonia's idempotent
  no-op (`CloseInternal` null-conditions the disposed PlatformImpl), verified headless by the
  acceptance test binding through the real window with a live session handle.
- **The threshold count** is `allowedEntryIds` (the `entriesForKind` set the strip rows are
  built from), not the Library window's live-store corpus — the gate decides the INLINE
  strip, so it counts what the strip would offer.

## Testing state

Diagnostic verification only — gate execution belongs to the arc-runner's deterministic gate
engine after this worker exits (IMPLEMENT Invariant 6).

- `dotnet build Berreman.slnx -c Release`: 0 errors, no MSB3277; the only warnings are the
  step-001-catalogued pre-existing set in untouched files (NU1701 Wolfram ×2, SYSLIB0051
  vendored MathNet ×2, FS3873 Dispersion, FS1125 SeriesDataTests ×4; FS0044 ChartWindow in
  the incremental Ui build). Zero warnings from the touched projects.
- ui-smoke: **148/148** (checkpoint 144, +4). ui-tests: **398/398** (checkpoint 394, +4).
- OpticalConstructor.Tests: **571/571** (== checkpoint). BerremanTests: **119 passed /
  5 pre-existing skips** (== checkpoint).
- Line endings: `git diff --numstat` ≡ `--ignore-cr-at-eol --numstat` (no CRLF churn;
  read-only check).

## Artifacts

- `specs/0038/.artifacts/017-diag-build.log`
- `specs/0038/.artifacts/017-diag-ui-smoke.log`
- `specs/0038/.artifacts/017-diag-ui-tests.log`
- `specs/0038/.artifacts/017-diag-constructor-tests.log`
- `specs/0038/.artifacts/017-diag-unit-tests.log`

## Gotchas

- The task file's system-prompt path was stale AGAIN (`C:\GitHub\AI-Strategy-Generator\
  implement_worker.system-md` does not exist); the IMPLEMENT worker prompt lives at
  `src/ai_strategy_generator/multistep/implement_worker.system-md` — the step-007..016
  gotcha recurred.
- The slice's `TestWindows/TableAndElementRotationView.fs:768,780` reference resolved by
  symbol to the relocated `Ui/TableAndElementRotationView.fs` — the `BindValueId` and
  `ConfirmBindValueId` commit points, both now delegating to `BindValueIdTo`.
- "The SAME bind message" is realized as ONE commit arm + delegation (see Decisions): the
  strip's control-level Confirm cannot dispatch a parameter-carrying message without a
  stale-closure hazard, so `ConfirmBindValueId` survives as a parameterless forwarder that
  REDUCES to the identical `BindValueIdTo`; the pure test pins the reduction as model
  equality. Recorded as the chosen interpretation.
- `WorkbenchSettings.QuickPickThreshold.defaultValue` does NOT compile — the single-case DU's
  case shadows the type under module-qualified long-ident resolution (unlike
  `SelectWindowModality`, whose cases are named differently). Open the module and use the
  unqualified type name.
- Adding `open Avalonia.FuncUI` ABOVE `open Avalonia.FuncUI.Elmish` trips FS0893
  (partially-qualified open) in Ui.Tests; `Avalonia.FuncUI.Component` is used fully
  qualified in `LibraryWindowTests.mountMainExposed` instead.
