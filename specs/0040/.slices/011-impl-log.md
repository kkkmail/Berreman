# Step 011 — impl-log

Spec 0040 Part D.4 (step 011, IMPLEMENT) — a film-less Plate sets its substrate
plate through the Materials window in Select state.

## Progress

- [x] Read system prompt, project prompt, slice spec, prior slice (010) outputs.
- [x] Surveyed `SelectionTarget` (WindowMode.fs), the two `targetText`s
      (Materials/Library window views), `chooseMaterialForLayer` /
      `chooseMaterialVerb` / `halfSpacesRow` / `SetSubstrateClicked`
      (SampleEditorView.fs), `SetSubstrate` (SampleStackEditor.fs), and the
      SampleEditor headless-test harness.
- [x] WindowMode.fs — add `SampleSubstrateTarget`.
- [x] MaterialsWindowView.fs + LibraryWindowView.fs — `targetText` arms.
- [x] SampleEditorView.fs — Msg / update / pick verb / halfSpacesRow.
- [x] SampleEditorWindowTests.fs — headless + pure proofs.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/WindowMode.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialsWindowView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/LibraryWindowView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/SampleEditorView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SampleEditorWindowTests.fs`

## Testing state

Per IMPLEMENT worker Invariant 6 the arc-runner's deterministic gate engine is the
gate authority (runs the roster after this session exits). Local verification this
round, purely to avoid shipping a non-building/hollow result:

- `build` — `dotnet build Berreman.slnx -c Release` → **Build succeeded, 0 Error(s)**.
  4 warnings, all pre-existing and NOT from our F# code: 2× `NU1701` (Wolfram, the
  exempt advisory) + 2× `SYSLIB0051` in the vendored `MathNetNumerics/Numerics`
  C# source. No `FS####` warning from any touched file — the added `SelectionTarget`
  case's two exhaustive-match breaks (`targetText` in the Materials + Library window
  views) are resolved.
- `ui-tests` + `ui-smoke` — full `OpticalConstructor.Ui.Tests` run: **672 passed,
  0 failed**. Filtered to the two new tests: **2 passed** (the pure
  `BindMaterialToSubstrate` fact → `ui-tests`; the headless `acceptance (011)` fact
  → `ui-smoke`).
- `constructor-unit-tests` — `OpticalConstructor.Tests`: **686 passed, 0 failed**
  (Domain change is a pure additive DU case; behaviour unchanged).
- `unit-tests` (BerremanTests) — solver core, does not reference OpticalConstructor;
  untouched.

commit_ready: true — every requirement in the slice is addressed this round.

## Artifacts

- `C:\GitHub\Berreman\specs\0040\.artifacts\011-build.log` — local Release build.
- `C:\GitHub\Berreman\specs\0040\.artifacts\011-ui-tests.log` — Ui.Tests run.

## Gotchas

- **Adding a `SelectionTarget` case breaks BOTH `targetText`s.** `MaterialsWindowView`
  and `LibraryWindowView` each match `SelectionTarget` exhaustively; the new
  `SampleSubstrateTarget` arm had to land in both or FS0025 (warning-as-error class)
  fails the build. No other exhaustive match over `SelectionTarget` exists (grep-verified:
  the only other sites construct cases or are doc comments).
- **The substrate target carries no payload.** A sample holds ONE substrate plate — no
  positional identity to address (unlike a film row's `LayerPosition`). The return
  routes through the editor's captured dispatch (`BindMaterialToSubstrate`); a closed
  editor makes it a dead-dispatch no-op. "A vanished editor is a no-op plus status line"
  therefore reduces here to: `SetSubstrate` never rejects (the pick always lands), the
  totality's `Error` arm is the defensive status-line path, and a closed window is the
  no-op — there is no film-row-style vanished slot for a single substrate.
- **`SetSubstrateClicked` removed, not left dead.** Its only trigger — the substrate
  'Set from chosen' `verbButton` — was replaced by the new `setSubstrateVerb`. The
  message + its update arm were removed together (keeping `update` exhaustive) rather
  than left as an unreachable case; grep confirmed nothing else references it.
- **The substrate Set… verb resolves its owner window from the click's visual tree.**
  It cannot use the plain `verbButton` (unit-onClick) — a modal Select open needs the
  owner `Window`, resolved via `TopLevel.GetTopLevel e.Source` exactly like the proven
  `chooseMaterialVerb`. It reuses the `setSubstrateButton` id and is ALWAYS enabled (it
  IS the pick, so no `hasChosenMaterial` gate — that was the film-less-Plate gap).
- **Scope is the substrate only.** The lower half-space keeps its `hasChosenMaterial`-
  gated 'Set from chosen' button unchanged — the slice targets the substrate slot alone,
  so the asymmetry is intentional.
