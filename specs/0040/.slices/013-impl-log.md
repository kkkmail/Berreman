# Step 013 — WIRE_UI — impl-log

## Progress

- [x] Read system prompt (`wire_ui_worker.system-md` + base `arc-runner.system-md`),
      project prompt (`arc-runner.user-md`), slice spec, and the depended-on surfaces
      (002/003/006/007/010/011/012 impl + the two existing WIRE_UI composition files).
- [x] Add `WireUi0040CompositionTests.fs` — the `ui-smoke` closing wiring assertion (4 facts).
- [x] Register it in `OpticalConstructor.Ui.Tests.fsproj` (last in compile order).
- [x] Confirm the composition — NO `OpticalConstructor.App` change needed (the root already
      composes all four windows over `AppContext` / the launcher factory seam).
- [x] Local due-diligence build + `ui-smoke` run (advisory — see Gotchas).
- [x] Write state-of-the-world.

## Files modified

- (new) `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/WireUi0040CompositionTests.fs`
  — 4 `[<Trait("Category","ui-smoke")>]` composition facts:
  1. the Materials + Library windows open from the REAL App-root ribbon strip and render the
     collapsed tree with a visible selection (steps 002/003);
  2. the Material editor composes over the app scope with a wrapping description + restricted
     gyration/μ tabs (steps 006/007);
  3. a Plate Sample editor renders R/T checkboxes and sets its substrate through the Materials
     Select window (steps 010/011);
  4. a ThinFilm Sample editor pins R on + disabled with no T control (step 010).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
  — added the `<Compile Include="WireUi0040CompositionTests.fs" />` last (after
  `WireUiFinalCompositionTests.fs`).

## Decisions

- Modelled on `WireUiFinalCompositionTests.fs` (the spec 0038 WIRE_UI closing acceptance) and
  `WireUiCompositionTests.fs`: build ONE `AppContext.create WorkbenchSettings.defaults` per
  proof (fresh scope — the stores are mutable), drive REAL input, assert the semantic-tree
  projection.
- Materials/Library open through the REAL App-root ribbon strip
  (`OpticalConstructor.App.MainConstructorWindow` + `WorkbenchIds.open*Button`) over the app
  scope — the strongest "composes at the App root" statement, and the surface the
  collapsed-tree/selection behaviour lives on. Every strip-opened window is CLOSED before the
  next open (no `WindowRegistry` key leak — the `WindowLauncherTests` discipline).
- The two editors compose over the app-scope store proxies directly (`ctx.materials` /
  `ctx.samples` / `ctx.categories`) — the `WireUiFinalCompositionTests` precedent (they are
  opened by the Materials/Library windows' own launchers, proven elsewhere).
- The film-less `Plate` fixture drives BOTH the R/T checkboxes (Plate geometry) and the
  substrate `Set…` verb (its only route to the Materials window); the seeded ThinFilm
  `SeedSamples.glassFilm600` drives the R-fixed/no-T assertion.
- Did NOT re-assert the step-006 g₁₁/g₃₃ restriction or the alphabetical sort over the visual
  tree — those are proven by the per-window suites (`NkDispersionChartTests`,
  `MaterialsWindowTests`/`LibraryWindowTests`). This closing file proves they COMPOSE at the
  App root without throwing.

## Testing state

Local due-diligence runs (advisory — the arc-runner gate engine is the authority):

- `build` — VERIFIED green (`dotnet build Berreman.slnx -c Release` → `Build succeeded. 0
  Error(s)`; only the exempt non-our-code `NU1701` Wolfram advisory and `SYSLIB0051` in the
  vendored MathNet C# source). No FS#### / MSB warnings from our code. (First attempt failed
  with FS0039 on `MouseDown`/`KeyPressQwerty` — the `Avalonia.Headless` open was missing; fixed
  and re-verified.)
- `ui-smoke` (`Category=ui-smoke`) — VERIFIED 194 passed / 0 failed (190 baseline + 4 new
  `WireUi0040CompositionTests` facts).
- `ui-tests` (`Category!=ui-smoke`) — the new file adds ONLY `ui-smoke` facts, so the
  `Category!=ui-smoke` count is unchanged (483 baseline); not re-run.
- `unit-tests` / `constructor-unit-tests` — the new file touches only
  `OpticalConstructor.Ui.Tests`; those counts are unaffected.

## Artifacts

- `specs/0040/.artifacts/013-build.log` — first (failed) solution build.
- `specs/0040/.artifacts/013-build2.log` — green Ui.Tests build after the fix.
- `specs/0040/.artifacts/013-uismoke-new.log` — the 4 new facts (all pass).
- `specs/0040/.artifacts/013-uismoke-full.log` — full ui-smoke suite (194 pass).

## Gotchas

- **`Avalonia.Headless` MUST be opened** for the headless input extension methods
  (`MouseDown` / `MouseUp` / `KeyPressQwerty` / `KeyReleaseQwerty` on `Window`). Mirroring
  `WireUiCompositionTests` I initially dropped that `open`; the build failed FS0039 until it
  was restored. All other opens mirror the two existing composition-test files.
- **WIRE_UI Invariant 6 ("act only; run no checks") vs. CLAUDE.md ("a green build is
  non-negotiable").** I resolved the tension by running the build + `ui-smoke` locally strictly
  as DUE DILIGENCE (to avoid shipping a non-compiling / hollow round — the same choice the
  prior IMPLEMENT slices recorded), NOT as the gate authority: the arc-runner's deterministic
  gate engine remains the sole gate authority and re-runs every gate after this session exits.
  My exit `gate_results` are advisory per the base protocol.
- **Registry-key hygiene is load-bearing.** The `WindowRegistry` is app-global; a Materials /
  Library / Materials-Select window left open would make a later test's strip click activate a
  stale window over the wrong stores. Every opened window is closed before the next open and
  before the root closes.
- **No `OpticalConstructor.App` source change.** The slice `touches` App, but the composition
  root already wires all four windows over `AppContext` / `EditorLaunchers.defaults`; "confirm
  every window still composes at the App root" is satisfied by the wiring assertion driving
  `MainConstructorWindow(AppContext.create …)` and the app-scope proxies — no product-code edit
  was needed, consistent with "No new wiring is introduced beyond the steps above."
