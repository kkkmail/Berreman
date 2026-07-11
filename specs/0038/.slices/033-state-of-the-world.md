# Step 033 — state of the world

## Where we are

Step 033 (IMPLEMENT, `depends_on: [3]`) adds unsaved-edit confirmation to BOTH
editors in `OpticalConstructor.Ui` — the Material editor
(`MaterialEditorView` / `MaterialEditorWindow`) and the Sample editor
(`SampleEditorView` / `SampleEditorWindow`). It closes the last gap in the editor
lifecycle before the arc moves on: a dirty editor can no longer lose the user's
work through Cancel or the window chrome.

## What's working

- Add a captured-at-load edit snapshot to each editor model and derive
  `isDirty` = snapshot structurally unequal to the current edit slice (plain
  records/DUs; no dirty flag maintained).
- Gate BOTH exits behind a Discard changes / Keep editing confirm when dirty: the
  Cancel button routes through `CancelClicked`, and the window chrome routes
  through an `OnClosing` override that shares the same decision.
- Close a pristine editor silently through Cancel or the chrome, exactly as before.
- Render the confirm surface (one row, distinct negative/positive styling) in place
  of Save/Cancel while confirming; Keep editing returns, Discard closes without saving.
- Cover the behaviour with pure model tests and headless proofs on both editors.

## Tests

Gates not run in-session (Invariant 6 — the worker acts; the arc-runner gate engine
runs the gates after exit). Changes are scoped to `OpticalConstructor.Ui` and
`OpticalConstructor.Ui.Tests`, so the `build`, `ui-smoke`, and `ui-tests` gates are
the load-bearing ones; `unit-tests` / `constructor-unit-tests` are untouched.

New/updated tests:
- Material & Sample pure model tests: pristine closes silently, a dirty Cancel shows
  the confirm, Keep editing returns, Discard closes without saving, and a lossless
  round-trip / non-structural change stays pristine. The pre-existing
  "Cancel writes nothing" tests were updated to the new discard-confirm semantics.
- Headless proofs on both real editor windows: pristine Cancel closes immediately;
  a dirty Cancel shows the confirm and Discard closes without persisting; the
  `OnClosing` chrome path is equally gated (driven through the shared
  `ChromeCloseIntercepted` seam, since the OS title-bar X is unreachable headlessly).

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 636
  ui_smoke_tests: 170
  ui_tests: 436
```

## Architecture

- **Dirtiness is derived, never stored.** Each model captures a `*EditSnapshot`
  tuple of exactly the structurally-comparable editable fields at `init`
  (`(name, description, category, editor)` for Material;
  `(name, description, substrate, editor.structure)` for Sample). `isDirty` compares
  it to the live slice. The `[<ReferenceEquality>]` on the models (they hold
  function-valued proxies / dispersion-function preset properties) does not affect
  this — the snapshot tuples are compared structurally.
- **One decision, two exits.** `CancelClicked` holds the dirty/pristine branch; the
  chrome `OnClosing` override delegates to it, so the button and the X are provably
  equally gated. A named `ExitPrompt = Editing | ConfirmingDiscard` drives the view
  (no naked bool).
- **Only the genuine chrome close is gated.** `OnClosing` intercepts a NON-programmatic
  close (`not e.IsProgrammatic`) only; a programmatic `Close()` — Save success,
  Discard, a pristine Cancel (all via `requestClose`), plus app/launcher/staleness
  closes and the existing tests' teardown `Close()` — proceeds untouched. This is
  why the existing headless tests that toggle the ladder (dirtying the editor) and
  then call `window.Close()` still close cleanly and do not leak into the shared
  headless session.

## Deferred

- Simulating a real OS title-bar X in headless: `Avalonia.Headless` exposes no
  window-manager close, so the chrome gate is proved through the public
  `ChromeCloseIntercepted` seam that `OnClosing` delegates to. The `e.IsProgrammatic`
  branch and the `e.Cancel <- true` glue are the only untested lines.

## Gotchas

- **Do NOT gate programmatic `Close()`.** Existing `MaterialEditorWindowTests`
  (and others) click ladder toggles — which dirty the editor — and then call
  `window.Close()` for teardown. `HeadlessSession` is a single assembly-wide session
  with no per-test window disposal, so gating those programmatic closes would leave
  dirty editors open and leak them across tests. Keying the gate on
  `not e.IsProgrammatic` keeps every existing programmatic close behaving exactly as
  before.
- Avalonia is **12.1.0**: `Window.OnClosing(WindowClosingEventArgs)` raises the
  `Closing` event; the override calls `base.OnClosing` only on the allowed path so
  the FuncUI host tears down on real closes and not on an intercepted one.
- `MaterialEditorWindow` moved from `Program.mkSimple` to `Program.mkProgram` to
  mirror the latest model into a window field (as `SampleEditorWindow` already did
  for its dispatch), which the chrome gate reads.

## Changelog

- 2026-07-11 — Step 033: unsaved-edit discard confirm on both editors. Snapshot-based
  `isDirty`; `ExitPrompt` + Discard/Keep confirm surface; `CancelClicked` gating;
  `OnClosing` chrome gate (non-programmatic only) via the shared
  `ChromeCloseIntercepted` seam. Pure + headless tests on both editors; the prior
  dirty-Cancel-closes assertions updated to the new semantics.
