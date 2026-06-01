# 008 impl-plan — Lifecycle + environment persistence + 3-D view (Part U8)

## Approach

Final slice of the spec-0024 UI-wiring arc. Wires four user-facing capabilities
onto the already-built, frozen seams, touching only the composition root (`Shell.fs`,
`Program.fs`) and new sibling `*View.fs` modules (§0.1):

- **R-1 Open/Save/New** — a new `LifecycleView.fs` owns an `IoMsg` sub-message and a
  lifecycle toolbar (New / Open / Save + template + gallery buttons). `Shell.fs` adds a
  `RootMsg.Io` case and the effectful `Cmd`s: Save serializes via
  `ConstructionPage.saveProject` and writes `<name>.ocproj.json`; Open reads through
  `ProjectFile.openProject` (schema-validated). The `IStorageProvider` is a host-layer
  module field in `Shell.fs` (§0.5), set by `Program.fs` on window-open; every
  background continuation marshals back via `Dispatcher.UIThread.Post` (§0.4). The
  picker is only a path source — the load/save logic lives in `Io (OpenPath …)` /
  `Io SaveRequested`, so it is fully testable headlessly with the picker absent.
- **R-2 Templates / gallery** — toolbar buttons dispatch `Io (LoadTemplate …)` /
  `Io (LoadGallery …)`; the update calls `Templates.loadTemplate` / `Help.openEntry`
  (the existing schema-validated factories — no private deserialize).
- **R-3 Environment persistence** — the existing `ShellMsg` theme/panel/dock reducers
  already update `model.env`; `Shell.update` now attaches a fire-and-forget persist
  `Cmd` calling `UserEnvironment.save` for those three messages. A test seam overrides
  the persist path so the round-trip is verified without touching real per-user state.
- **R-4 3-D view** — a new `SystemView3DView.fs` renders a 2-D orthographic top-view
  projection of `SystemView3D.placeElements` (element boxes) + `SystemView3D.beamSegments`
  (beam lines, read from the already-solved per-node `EmFieldSystem`s in
  `construction.results`) on the slice-002 `ChartHosts.canvasHost` — no GL. Wired into
  the `results` panel content in `Shell.fs`.

## Files

New:
- `OpticalConstructor.Ui/LifecycleView.fs` — `IoMsg` + lifecycle toolbar view.
- `OpticalConstructor.Ui/SystemView3DView.fs` — the 2-D orthographic 3-D view.
- `OpticalConstructor.Ui.Tests/LifecycleTests.fs`, `SystemView3DTests.fs` — headless tests.

Edited (composition root only):
- `OpticalConstructor.Ui/Shell.fs` — `Io` `RootMsg` case + IO/persist `Cmd`s + host
  `IStorageProvider`/persist-path fields + `status` model field + results-panel 3-D wiring.
- `OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` — register the two new `.fs`.
- `OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` — register the new tests.
- `OpticalConstructor.App/Program.fs` — register the window `StorageProvider` on open.

## Risks

- **Host `IStorageProvider` is a shared static**: the headless smoke test boots the real
  App and may set it, so the Save test forces it to `None` (`clearStorageProviderForTests`)
  for a deterministic fallback path.
- **§0.4 marshaling**: every off-thread continuation (open/save/persist) posts via
  `Dispatcher.UIThread.Post`; persist is a no-dispatch fire-and-forget so §0.4 is moot for it.
- **placeElements/beamSegments zip**: `preorderPaths` mirrors `placeElements`' DFS order
  exactly (pre-order, `Map.toList` child order) so each placed element pairs with the
  correct solved node.
