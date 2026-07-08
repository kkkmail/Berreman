# 019 — impl-log (WIRE_UI — composition acceptance for the category catalogue)

## Progress

- [x] Read system prompt (`wire_ui_worker.system-md` + the shared
      `arc-runner.system-md` base), project prompt (`arc-runner.user-md`), slice
      spec (`019.slice-md`), CLAUDE.md, and the referenced code.
- [x] Confirm the composition root wiring is final: `Program.fs:138-145` already
      builds the five in-memory proxies (samples → materials over
      `samplesReferencing` → categories over `materialsReferencingCategory`, beside
      library / experiments) and injects all five through `initMainWith`. Slice 009
      landed this; step 019 confirms it final (comment-only touch).
- [x] Add the Category-editor composition-acceptance `ui-smoke` fact to
      `WireUiCompositionTests`.
- [x] Refresh the `WireUiCompositionTests` module doc-comment (five proxies /
      three editors / reordered full-surface bays) and the `Program.fs` composition
      comments (comment-only, no behavioural change).
- [x] Verify LF line endings preserved (no CRLF churn; `git diff --numstat` vs
      `--ignore-cr-at-eol` identical).
- [x] Write state-of-the-world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/WireUiCompositionTests.fs`
  — NEW `[<Trait("Category","ui-smoke")>]` fact
  *"composition acceptance: Categories… on the REAL Main window opens the real
  Category editor over the root-wired CategoryProxy"*: mounts the real
  `MainConstructorWindow`, subscribes to the public `Window.WindowOpenedEvent`,
  clicks the Materials tab then the **Categories…** verb
  (`WorkbenchIds.categoriesButton`), and asserts exactly one window opens, visible,
  carrying `CategoryEditorView.UiIds.window`. Module doc-comment refreshed to name
  the five proxies / three editor windows / reordered full-surface bays.
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs` — comment-only:
  the composition comments now state five proxies inject through `initMainWith` and
  that step 019 delivers the acceptance covering all three editors. No code change.

## Testing state

Invariant 6 (WIRE_UI worker ACTS only): I did **not** run `build`,
`unit-tests`, `constructor-unit-tests`, `ui-smoke`, or `ui-tests`. The
arc-runner's deterministic gate engine runs the step's gates after this session
exits and is the sole gate authority. The new fact mirrors the proven
Material/Sample editor blocks in the same file verbatim (same `mountRoot` /
`Window.WindowOpenedEvent` / `clickOn` seam), and the Categories… verb is the
same clickable `Border` shape (`workbenchButton`) `clickOn` already drives for the
Add / Remove-confirm buttons — so it compiles and runs against the existing seams.

Expected post-round baselines (advisory only; the source deltas cannot regress the
other suites — no core / UI-less / non-ui-smoke test file changed):

- `berreman_unit_tests` 119 (unchanged — no core file touched)
- `constructor_unit_tests` 447 (unchanged — no UI-less project touched)
- `ui_smoke_tests` 105 → **106** (one new composition-acceptance fact)
- `ui_tests` 329 (unchanged — the new fact is a ui-smoke fact)

## Artifacts

None — a headless wiring assertion produces no captured logs / screenshots /
traces. `specs/0035/.artifacts/` holds only the arc-runner's own round trails.

## Gotchas

- **The composition root was already wired (slice 009), not new work this round.**
  `Program.fs` builds and injects all five proxies through `initMainWith`;
  step 019 owns the ACCEPTANCE, not the wiring. `Program.fs` is a comment-only
  touch this round — the 0033/026 WIRE_UI precedent (that step also confirmed the
  root final rather than re-wiring). The slice text reads as if the root wiring is
  new; the code and slice-009 SoW already record it landed, so I verified rather
  than re-wired (base protocol §7 skepticism rule).
- **The task-file system-prompt path
  `C:\GitHub\AI-Strategy-Generator\wire_ui_worker.system-md` does not exist** (the
  drift noted since slice 015 continues); the real file is
  `src\ai_strategy_generator\multistep\wire_ui_worker.system-md`. Read there.
- **The Category editor is proven from the ROOT, not a stand-in.** The launcher
  (`EditorLaunchers.defaults.openCategoryEditor`) receives `model.categories` — the
  proxy the composition root composed over `materialsReferencingCategory` — so the
  fact opening the real `CategoryEditorWindow` proves the `CategoryProxy` is wired
  at the root, distinct from `MainWorkbenchTests` (which injects stores) and
  `CategoryEditorWindowTests` (which drives the editor over a stub proxy).
- **`.manifest.state.json` (modified, CRLF-warned) and the untracked `.claude/`
  folder are the arc-runner's / harness's own files** — left alone, as in prior
  slices.
