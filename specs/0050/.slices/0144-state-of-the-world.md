# Where we are

Step 0144 implements S42-DIALOG after kit snapshot capture, resolution, overlay and confirmation. The existing import dialog accepts the kit context and every resolved row, preserves the kit's default choices, and calls its confirmation operation only on Apply. Implementation is complete; executable verification belongs to the arc-runner supervisor.

# What's working

- Added kit snapshot requests with the kit name, complete row count and successful-Apply summary.
- Routed kit Apply through confirmation and version pinning, preserving shared callback, close and error handling.
- Added kit-specific action captions while preserving catalogue/drop captions and other actions.
- Added the three authorized facts covering defaults, cancellation, confirmation, dependency refusal, reuse rows and rendered captions.
- Made three dependent request matches exhaustive without changing catalogue queue or dropped-file behavior.

# Tests

No build, test, analyzer or invariant gate was run, per IMPLEMENT Invariant 6. The supervisor owns `build`, `elevation`, all-discovered `ui-smoke`, selected `OpticalConstructor.Ui.Tests.ImportDialogTests`, and the eleven S16 invariants in `0144.gates`.

Each added fact uses the Built-in multilayer sample's snapshot closure, a second read-only proxy over the same in-memory folder, and an empty receiving library recording writes and pins. Existing recording callbacks and headless view helpers provide semantic and rendered coverage. Assertions include no mutation before Apply or on Cancel, no writes on default confirmation, sample-only dependency refusal with recovery, complete import/pinning, unchanged kit metadata/snapshots, all-Reuse rows retained until explicit confirmation, and the requested kit/catalogue captions.

The selected class gains exactly three facts: its nine existing non-smoke facts become an expected 12, with its existing smoke fact unchanged. This is source accounting, not a measured result. No numeric `ui_tests` checkpoint exists in the supervisor checkpoint file, so none is invented here. The YAML repeats checkpoint `135`'s existing all-discovered smoke count; no baseline was edited.

```yaml
gates:
  ui_smoke_tests: 215
```

# Architecture

`ImportDialogRequest.ResolveKitSnapshot` holds the existing `KitLibraryContext` and resolved decisions. Its rows use the same model initialization and renderer as other requests. Apply delegates to `kit.confirm`, returning its actual outcome through the existing callback before closing; failure remains a `CatalogueImportError` in the open model. No import policy, validation or pinning implementation is copied into the UI.

Caption specialization is a match on origin and action. No new window, automation ID, message, persisted representation, compatibility path or migration is introduced. The changed request is an in-memory UI contract only.

The new union case necessarily affects three exhaustive consumers outside the named edit list. `LibraryWindowView.fs` and `OpticalConstructor.App/Program.fs` leave catalogue queues unchanged for kit requests; `LibraryWindowTests.fs` continues rejecting non-drop requests. These one-line dependent repairs satisfy the shape change and zero-warning requirement, and are recorded as the sole file-scope deviation.

# Deferred

None of the requested implementation is deferred. Executable verification remains assigned to the supervisor by the worker protocol.

# Gotchas

- `specs/0050/.manifest.state.json` was modified before the worker started and remains untouched.
- The kit context owns confirmation; using the dialog's generic catalogue apply path would omit version pins.
- Snapshot reuse rows count toward the heading and remain visible even when every row reuses held content.
- No existing test identity or assertion was removed, and no census, gate, oracle, checkpoint or version file was changed.
- All worker output documents use the supplied paths. No persistent run artifacts were produced.

# Changelog

2026-11-07 — Added kit snapshot decisions to the shared import dialog, origin-specific captions, three authorized facts and dependent exhaustive-match repairs; supervisor verification pending.
