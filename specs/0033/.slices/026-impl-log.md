# Impl log — spec 0033, slice 026 (WIRE_UI)

## Progress

- [x] Read task file, system prompt (wire_ui_worker + arc-runner base), project prompt, slice spec.
- [x] Surveyed the composition root and the existing ui-smoke coverage.
- [x] Validated the editor-window observation mechanism (WindowOpenedEvent under headless).
- [x] Wrote impl-plan.
- [x] Wrote `WireUiCompositionTests.fs` (2 ui-smoke composition-acceptance facts) + fsproj entry.
- [x] Program.fs comment finalization (comment-only; the composition itself was already final).
- [x] Local advisory gate runs — all five green, logs in `.artifacts/026-*.log`.
- [x] State-of-the-world.

## Files modified

- **NEW** `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/WireUiCompositionTests.fs`
  — the WIRE_UI composition acceptance: two `[<Trait("Category", "ui-smoke")>]` facts
  driving the REAL `OpticalConstructor.App.MainConstructorWindow` headless.
  1. *Bay sweep*: every ribbon tab in `BayNames.all` renders one frame on the real
     window; the Selector bay mounts its unconditional ids (kind label / readout /
     tree), the Materials bay lists the SEEDED glass152 + silicon rows and the
     Library bay the SEEDED glassFilm600 row — proof the root wired LIVE stores.
  2. *Editors + root coupling*: Materials → Add opens the REAL step-023
     `MaterialEditorWindow` and Library → select + Edit opens the REAL step-022
     `SampleEditorWindow`, both through `EditorLaunchers.defaults` (observed via the
     public `Window.WindowOpenedEvent.Raised`, asserted by their `UiIds.window`
     automation ids + title); then removing the REFERENCED glass152 through the real
     window surfaces the "still referenced … Glass plate" inline message with the row
     still listed — the root's materials store consults the root's LIVE samples store
     (`samplesReferencing`).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
  — `<Compile Include="WireUiCompositionTests.fs" />` after `MainWorkbenchTests.fs`,
  with the house comment.
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs` — comment-only:
  the slice-024 "threaded mechanically; the final WIRE_UI step owns the composition
  acceptance" note now records the finalized state (all four proxies through
  `initMainWith`, real launchers, verified by `WireUiCompositionTests`). No code
  change — see Decisions.

## Decisions

- **The composition root needed no code change.** Slice 024 already landed the exact
  composition the slice text mandates: `MainConstructorWindow`
  (`OpticalConstructor.App/Program.fs`) builds `SampleProxy.createInMemory ()` first,
  then `MaterialProxy.createInMemory (samplesReferencing samples)`, beside the
  library/experiments proxies, and injects all four through `initMainWith`; `initWith`
  seeds `launchers = EditorLaunchers.defaults`, which open the REAL step-022/023
  editor windows; the launcher path is unchanged. 024's comment explicitly deferred
  "the composition acceptance" to this WIRE_UI step — so this round ships the
  acceptance proof, not a re-wiring.
- **Observation seam for default-launcher windows**: the default launchers open
  UNOWNED windows (`.Show()`), invisible to `OwnedWindows`, and the headless session
  app has a null `ApplicationLifetime` (no `Windows` list). The public global routed
  event `Window.WindowOpenedEvent` (`.Raised : IObservable<struct (obj *
  RoutedEventArgs)>`) is the seam the desktop lifetime itself uses for window
  tracking — validated headless via
  `specs/0033/.artifacts/026-scratch-windowopened.fsx` (fires once for an unowned
  `Show()`). The subscription is disposable (`use`) and starts only after the Main
  window is shown; the whole test body runs as ONE dispatched action on the shared
  headless UI thread, so it cannot observe another test's windows.
- **TDD note**: this slice is a wiring-acceptance round over already-landed
  composition, not a bug fix — there is no missing production symbol to capture red.
  The new facts are executable acceptance criteria; they pass against the wired root
  and would fail loudly on any future root de-wiring (store not seeded → row asserts
  fail; launcher seam broken → `opened.Count` asserts fail; coupling dropped → the
  refusal-message assert fails).

## Testing state

Local runs are ADVISORY only (Invariant 6: the arc-runner's deterministic gate
engine re-runs the roster authoritatively after exit). All five gates green:

- `build` — `dotnet build Berreman.slnx -c Release -nologo -v:m` (cwd `Berreman/`):
  exit 0, **0 errors**; 94 warnings = the pre-existing MSB3277/NU190x/SYSLIB0051/
  FS0044/FS3873/FS1125 noise (none in this round's files — `WireUi` grep of the log
  is empty). Log: `.artifacts/026-build.log`.
- `unit-tests` (BerremanTests, `--no-build`) — **119 passed** (= baseline), 5 skipped
  (pre-existing), 0 failed. Log: `.artifacts/026-unit-tests.log`.
- `constructor-unit-tests` — **416 passed** (= baseline), 0 failed.
  Log: `.artifacts/026-constructor-unit-tests.log`.
- `ui-smoke` — **83 passed** (81 baseline + 2 new composition-acceptance facts),
  0 failed. Log: `.artifacts/026-ui-smoke.log`.
- `ui-tests` (`Category!=ui-smoke`) — **306 passed** (= baseline; the new file
  carries only ui-smoke facts), 0 failed. Log: `.artifacts/026-ui-tests.log`.

Line endings: the new/modified files are pure LF (`git diff --numstat` identical
with/without `--ignore-cr-at-eol`; byte-scan of the new file: 0 CRLF).

## Artifacts

- `specs/0033/.artifacts/026-build.log`, `026-ui-smoke.log`, `026-ui-tests.log`,
  `026-constructor-unit-tests.log`, `026-unit-tests.log` — advisory gate runs.
- `specs/0033/.artifacts/026-scratch-reflect.fsx`, `026-scratch-reflect2.fsx`,
  `026-scratch-reflect3.fsx` — Avalonia 12.0.5 reflection probes (window-tracking
  internals, `Raised` signature, lifetime setter visibility).
- `specs/0033/.artifacts/026-scratch-windowopened.fsx` — headless probe:
  `WindowOpenedEvent` fires for an unowned `Show()` (output: 1 event).

## Gotchas

- The task file's system-prompt path
  `C:\GitHub\AI-Strategy-Generator\wire_ui_worker.system-md` does not exist — the
  real file is
  `C:\GitHub\AI-Strategy-Generator\src\ai_strategy_generator\multistep\wire_ui_worker.system-md`
  (the same 015–025 path drift; read from there).
- `## Operator note` in the project prompt is present but empty — no operator
  constraints in flight this round.
- The slice text says "Program.fs:114-129 builds … and injects all four" as if new
  work; in the checked-out tree slice 024 had already moved that composition in
  (Program.fs:129-140 after this round's comment). Interpreted per the recorded 024
  hand-off ("the final WIRE_UI step owns the composition acceptance"): 026 = verify
  the wired root headless, don't duplicate the wiring.
- `.manifest.state.json` (modified, with its CRLF warning) and the untracked
  `.claude/` folder are the arc-runner's / harness's own files — left alone, as in
  slices 001–025.
