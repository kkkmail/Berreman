# Where we are

Slice 0156 wires picked-kit snapshot resolution into the launcher after 0155's read-only offer. Retry 02 repairs the opener acceptance's overlay proof to follow step 0142's per-entry resolution contract. All requested implementation and the three authorized smoke facts are authored. The WIRE_UI worker protocol assigns executable verification to the supervisor after worker exit; no runtime pass is claimed for this repair.

# What's working

- Routed every nonempty kit snapshot list, including all-Reuse rows, through the existing import dialog before opening a constructor.
- Rebuilt the confirmed overlay with `withRebindings` and opened Main or Inverse over the resolved kit in the requested access mode.
- Released pending kits on Cancel or dialog close; reused the launcher diagnostic and released the kit if overlay reconstruction fails.
- Added the three declared smoke facts covering visible rows, Cancel/reopen/Apply, kit-lock ownership, unchanged library entries and import logs, and kit-backed per-entry resolution through the launched constructor's actual stores.
- Preserved existing assertions and `WireUiFinalCompositionTests` unchanged.

# Tests

No build, test, analyzer or invariant gate was executed by this worker. The supervisor runs build, elevation, constructor-unit-tests, ui-smoke, selected LauncherTests ui-tests and all eleven s17 invariants. Exactly three new ui-smoke identities were authored; none were renamed or removed. The named opener acceptance includes both a missing user material and an all-Reuse Silicon snapshot, and checks explicit confirmation after cancellation. The cancellation fact also exercises closing the dialog directly.

The fixtures use canonical `openKitAt`, `writeSnapshot` and `close`, under test output. They observe every mutating catalogue-import proxy operation and compare held entries and import logs, including before Apply and after Cancel. After the real dialog's Apply opens Main, the helper reads its `sessionLibraryStores` composition property and requires both `tryGetMaterial` and exact-version `resolveVersion` to equal the canonical decoded snapshot. It also requires the listing/search functions to remain the app's original functions. The intentionally missing user material remains absent from the database. Real dialog rows and window observations retain the headless semantic-tree coverage.

These are inherited measured counts from checkpoint 154, not results measured or baselines authored in this round. The three new facts are not counted as passed. No ui_tests count is invented for the selected class.

```yaml
gates:
  constructor_unit_tests: 1149
  ui_smoke_tests: 221
```

# Architecture

The launcher reuses `ResolveKitSnapshot`, `ImportDialogWindow`, `OpenedKit.pendingDecisions` and `OpenedKit.withRebindings`. A single local Apply flag at the window boundary distinguishes cancellation from ownership transfer to the constructor. The flag is set before overlay reconstruction; failure closes the kit and renders the existing launcher error. The constructor retains its existing close handler. The shared opener continues to cover read-write/read-only and Main/Inverse, while an empty snapshot list opens immediately.

Error text construction was moved into one local helper used by both initial-open and post-confirmation failures. No new messages, IDs, domain behavior, durable shapes, migrations or serializers were added.

Retry 02 adds a read-only `MainConstructorWindow.sessionLibraryStores` property exposing the very same record supplied to the scene initializer. The test exercises the stores of the window opened by Apply; it does not build a separate overlay or substitute library listings. This small composition seam stays in the authorized `Program.fs` surface.

# Deferred

No implementation work was deferred. Gate execution belongs to the supervisor under the act-only worker protocol; `commit_ready` remains false pending that verification.

# Gotchas

- Reuse is a pending row. Only an empty snapshot list bypasses confirmation.
- Apply must mark ownership before the dialog closes, or the Closed handler would release the constructor's kit.
- Cancel and direct dialog close release the pending kit without touching the library; closing an applied constructor releases its own kit.
- The two all-Reuse obligations share the real-window assertion helper; the named opener acceptance also invokes it explicitly.
- Material listings/searches intentionally pass through to the database (step 0142). Kit-only resolution must be proved through per-entry reads, not Materials-browser membership.
- Source diff counts match `--ignore-cr-at-eol`. The supervisor-owned pre-existing `.manifest.state.json` change is untouched.
- Gate snapshots, census, oracle, checkpoints, analyzer settings, version rings, ASG/STL, notebooks and final-composition source remain unchanged.

# Changelog

- 2026-11-08: Wired kit-snapshot confirmation into the launcher and authored all three declared headless smoke facts for supervisor verification.
- 2026-11-08 (retry 02): Replaced the invalid Materials-browser overlay assertion with per-id and exact-version reads through the launched constructor's scene stores; preserved all confirmation, lock, no-write and all-Reuse coverage.
