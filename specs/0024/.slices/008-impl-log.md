# 008 impl-log — Lifecycle + environment persistence + 3-D view (Part U8)

## Progress

- [x] LifecycleView.fs (IoMsg + lifecycle toolbar)
- [x] SystemView3DView.fs (2-D orthographic 3-D view)
- [x] Shell.fs wiring (Io case, IO/persist Cmds, host fields, status, 3-D in results)
- [x] Program.fs StorageProvider registration
- [x] fsproj registrations (Ui + Ui.Tests)
- [x] LifecycleTests.fs + SystemView3DTests.fs
- [x] Gates green (build / unit-tests / constructor-unit-tests / ui-smoke / ui-tests)

## Files modified

New (OpticalConstructor.Ui):
- `LifecycleView.fs` — owns `IoMsg` (the open/save/new/template/gallery sub-message the
  shell wraps as `RootMsg.Io`) and the lifecycle toolbar (New / Open / Save + template +
  gallery palettes + the IO status line). Public MIT FuncUI DSL only.
- `SystemView3DView.fs` — the 2-D orthographic top-view of `SystemView3D.placeElements`
  (element boxes) + `SystemView3D.beamSegments` (beam rays, read from the already-solved
  per-node `EmFieldSystem`s in `construction.results`) on the slice-002
  `ChartHosts.canvasHost`. Pure render-boundary projection (§A.3); no GL.

New (OpticalConstructor.Ui.Tests):
- `LifecycleTests.fs` — 5 `ui-tests`: Save→Open round-trip (AC-U8.1), Save-button dispatch
  (R-1), template load (AC-U8.2), gallery load (AC-U8.2), env theme/panel/dock persist (AC-U8.3).
- `SystemView3DTests.fs` — 1 `ui-test`: the 3-D view renders boxes + beam lines on a
  Canvas without GL (AC-U8.4).

Edited (composition root only):
- `OpticalConstructor.Ui/Shell.fs` — added `RootMsg.Io`, the `status` model field, the
  host-layer `IStorageProvider`/persist-path fields (§0.5) with test seams, the IO `Cmd`s
  (`openCmd`/`openPathCmd`/`saveCmd`, §0.4-marshaled), the fire-and-forget `persistEnvCmd`
  (R-3) attached to theme/panel/dock shell messages, `updateIo`, the lifecycle toolbar in
  `view`, and the 3-D view in the `results` panel content.
- `OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` — registered the two new `.fs` before
  `ConstructionView.fs`/`Shell.fs`.
- `OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` — registered the two test files.
- `OpticalConstructor.App/Program.fs` — `MainWindow.Opened` registers the window
  `StorageProvider` with the shell host layer (R-1 / §0.5).

No frozen module was edited (§0.1): every change is a new sibling `*View.fs` module or the
composition root (`Shell.fs`, `Program.fs`).

## Testing state

All slice-roster gates pass locally (run in order; first failure short-circuits):

- `build` — `dotnet build Berreman.slnx -c Release` — 0 errors (inherited-only warnings:
  MSB3277 WindowsBase/WebView2, FS1125 SeriesDataTests; none introduced this slice).
- `unit-tests` (BerremanTests) — 84 passed / 5 skipped.
- `constructor-unit-tests` (OpticalConstructor.Tests) — 204 passed (still Avalonia-free).
- `ui-smoke` — 1 passed (the real App boots headlessly; every panel/page renders one frame,
  including the lifecycle toolbar and the 3-D view in the results panel).
- `ui-tests` — 25 passed (prior 19 + 6 new U8 tests).

See `../.artifacts/008-gate-summary.txt` for the captured run.

## Artifacts

- `C:\GitHub\Berreman\specs\0024\.artifacts\008-gate-summary.txt` — local gate-run summary.

## Gotchas

- **Seed normal-incidence Poynting normals are degenerate.** The default bandpass-filter seed
  (near-fully-transmitting, normal incidence) has a Poynting norm below `almostZero`, so
  `SystemView3D.beamDirection` returns `None` and `beamSegments` is empty for it — this is the
  frozen seam's by-design "drop a degenerate branch" behaviour, not a bug. The AC-U8.4 test
  therefore drives a vacuum/200 nm-glass/glass probe system (the frozen `SchematicGeometryTests`
  AC-J12 fixture) whose branches expose non-degenerate normals, so beam rays render.
- **Host `IStorageProvider` is a process-shared static.** The `ui-smoke` test boots the real App
  and registers a provider on window-open, so the Save round-trip test calls
  `Shell.clearStorageProviderForTests ()` to force the deterministic working-folder fallback.
- **Picker is only a path source.** Open/Save load/write logic lives in `Io (OpenPath …)` /
  `Io SaveRequested`, so the IO is fully testable headlessly with the picker absent (R-1).
