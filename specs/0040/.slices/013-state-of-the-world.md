# Step 013 — state of the world

## Where we are

Spec 0040 step 013 (WIRE_UI) — the CLOSING composition acceptance for the arc. This is a
STRUCTURAL COMPOSITION step: no new behaviour, no new wiring. Steps 002/003 gave the faceted
tree its collapsed-by-default open and its visible entry-leaf selection; 005/006/007 totalled the
Anisotropy/Transparency facets, restricted the Material editor's gyration/μ preview tabs to the
applicable components, and made its description box wrap; 010/011/012 exposed a sample's R/T
emission (Plate) with R pinned for a ThinFilm, let a film-less Plate set its substrate through the
Materials Select window, and seeded a placed sample's emission. This step finalises the
composition and drives the WHOLE changed surface headless over ONE app scope composed exactly like
`Startup.context` (`AppContext.create WorkbenchSettings.defaults`), asserting each surface renders
one frame without throwing. Scope: `OpticalConstructor.Ui.Tests` (one new `ui-smoke` file) +
`OpticalConstructor.App` (composition confirmed — no source change); `depends_on:
[3, 5, 6, 7, 10, 11, 12]`.

## What's working

- Add a `WireUi0040CompositionTests` ui-smoke wiring assertion (4 facts) that drives the four
  changed surfaces over ONE root-composed `AppContext`, rendering each without throwing.
- Open the Materials and Library windows from the REAL App-root ribbon strip and render the
  collapsed tree with a visible entry-leaf selection (steps 002/003).
- Compose the Material editor over the app scope with a wrapping description and the restricted
  gyration/μ tabs that appear exactly when active/magnetic (steps 006/007).
- Compose the Sample editor over the app scope — R/T checkboxes for a Plate (a T click clears T,
  leaves R), R fixed-on/disabled with no T for a ThinFilm, and a film-less Plate setting its
  substrate through the Materials Select window (steps 010/011).
- Confirm the composition root already wires all four windows — no `OpticalConstructor.App` source
  change was required.

## Tests

Per the WIRE_UI worker Invariant 6, gate execution belongs to the arc-runner's deterministic gate
engine after this session exits; this worker runs no gates as an authority. Local due-diligence
runs (to avoid a non-building / hollow result — CLAUDE.md's non-negotiable green build) and how the
change lands per gate:

- `build` — VERIFIED green (`dotnet build Berreman.slnx -c Release` → `Build succeeded. 0
  Error(s)`; only the exempt non-our-code `NU1701` Wolfram advisory + `SYSLIB0051` in the vendored
  MathNet C# source). No FS#### / MSB warnings from our code.
- `ui-smoke` (`OpticalConstructor.Ui.Tests`, `Category=ui-smoke`) — VERIFIED 194 passed / 0 failed
  (190 baseline + 4 new `WireUi0040CompositionTests` facts).
- `ui-tests` (`Category!=ui-smoke`) — the new file adds ONLY `ui-smoke` facts, so the
  `Category!=ui-smoke` count is unchanged (483 baseline); regression-free.
- `unit-tests` (BerremanTests) / `constructor-unit-tests` (OpticalConstructor.Tests) — the new file
  touches only `OpticalConstructor.Ui.Tests`; those counts are unaffected.

Acceptance mapping (step 013): "the `ui-smoke` suite MUST render one frame each — without throwing —
for the Materials window (collapsed sorted tree, visible selection), the Library window (same), the
Material editor (restricted gyration/μ tabs, wrapping description), and the Sample editor (R/T
checkboxes for a Plate, R fixed for a ThinFilm, substrate Set… via Materials Select), and every
test gate MUST be at or above its baseline count" → the four `WireUi0040CompositionTests` facts,
verified green locally (ui-smoke 194 ≥ 190 baseline).

```yaml
gates:
  berreman_unit_tests: 0
  constructor_unit_tests: 0
  ui_smoke_tests: 0
  ui_tests: 0
```

## Architecture

- **Close the arc with a composition sweep, not new behaviour.** The file mirrors the two existing
  WIRE_UI composition files (`WireUiCompositionTests` spec 0033/0035/0038, `WireUiFinalCompositionTests`
  spec 0038 step 047): each proof builds ONE `AppContext.create WorkbenchSettings.defaults` — the
  exact `Startup.context` composition — and drives REAL input on the REAL wired views, asserting the
  semantic-tree projection. The stores are mutable, so a fresh scope per proof gives test isolation.
- **Prove "composes at the App root" by opening through the real strip.** The Materials / Library
  windows open through `OpticalConstructor.App.MainConstructorWindow` + the ribbon strip's
  `Materials…` / `Library…` buttons over the app scope — the end-to-end launcher path, the strongest
  statement that the changed surfaces still compose at the root. The two editors compose over the
  app-scope store proxies directly (the `WireUiFinalCompositionTests` editor precedent).
- **No product-code change.** The composition root already wires all four windows over `AppContext`
  / `EditorLaunchers.defaults`; "confirm every window still composes at the App root" is satisfied by
  the wiring assertion, consistent with "No new wiring is introduced beyond the steps above."
- **Do not re-prove sub-behaviours.** The step-006 g₁₁/g₃₃ restriction and the alphabetical sort are
  proven by the per-window suites; this closing file proves only that the surfaces COMPOSE and render
  without throwing over ONE app scope.

## Deferred

- Nothing for this arc. Step 013 is the closing WIRE_UI step; every enumerated surface (Materials
  window, Library window, Material editor, Sample editor) is driven over the app root this round.
- A `ui-tests` structural pin over the app scope (as `WireUiFinalCompositionTests` carries for its
  scene seam) was not added — this arc introduced no new app-scope store proxy to pin; the four
  changed surfaces are view/behaviour changes covered by the `ui-smoke` wiring assertion.

## Gotchas

- **`Avalonia.Headless` MUST be opened** for the headless input extensions
  (`MouseDown`/`MouseUp`/`KeyPressQwerty`/`KeyReleaseQwerty` on `Window`). The first build failed
  FS0039 until that `open` was restored (the two sibling composition files carry it).
- **WIRE_UI Invariant 6 ("run no checks") vs. CLAUDE.md ("green build is non-negotiable").** The
  local build + ui-smoke run here are DUE DILIGENCE only (avoid a non-compiling round), NOT the gate
  authority — the arc-runner's gate engine re-runs every gate after exit and is authoritative; the
  exit `gate_results` are advisory.
- **Registry-key hygiene is load-bearing.** The `WindowRegistry` is app-global; a Materials /
  Library / Materials-Select window left open would make a later test's strip click activate a stale
  window over the wrong stores. Every opened window is closed before the next open and before the
  root window closes.
- **The film-less `Plate` fixture drives two behaviours** — it exposes the R/T checkboxes (Plate
  geometry, independent of films) AND is the only-route-to-Materials substrate `Set…` case; the
  seeded ThinFilm `SeedSamples.glassFilm600` drives the R-fixed/no-T assertion.

## Changelog

- 2026-07-13 — Step 013 (WIRE_UI): closed arc 0040 with a `WireUi0040CompositionTests` ui-smoke
  wiring assertion (4 facts) that drives the four changed surfaces over ONE root-composed
  `AppContext` — the Materials/Library windows opened from the REAL App-root ribbon strip (collapsed
  tree + visible selection), the Material editor (wrapping description + restricted gyration/μ tabs),
  and the Sample editor (Plate R/T checkboxes, ThinFilm R fixed, film-less-Plate substrate via
  Materials Select) — each rendering one frame without throwing. No `OpticalConstructor.App` source
  change (the root already composes all four). Build + ui-smoke (194) verified green locally.
