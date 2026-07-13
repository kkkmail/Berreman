# State of the world — step 031 (IMPLEMENT: out-of-band dispersion diagnostic)

## Where we are

Spec 0038 Part I's Optical Constructor arc. Steps 25/30 grew the experiment into a
full versioned setup and made an element's bound/unbound state VISIBLE on the
schematic (dashed/ghosted). This step adds a SEPARATE, out-of-band diagnostic —
independent of bound/unbound — that flags when a reachable material's DEFINED
dispersion does not cover the wavelengths the experiment actually requests (a
single λ for a fixed setting, the full swept range for a wavelength sweep). The
diagnostic is a pure Domain function carrying per-material defined-vs-requested
ranges as typed data; the Ui renders a small ⚠ warning badge with a hover tooltip
on flagged elements and shows the same text in the Details bay.

## What's working

- Add the pure `OpticalConstructor.Domain.OutOfBandDiagnostic` — `RequestedWavelengths`,
  `ReachableMaterial`, `OutOfBandFinding`, `DispersionCoverage`, segment-union coverage,
  and the tooltip/Details text builders; constant and coded-preset materials never flag.
- Flag when a reachable material's defined dispersion segments leave the request partly
  uncovered, carrying the material name and both nm ranges; 600 nm inside 300–700 stays
  in-band while a 200–800 sweep flags.
- Wire the Ui host to resolve a bound sample's reachable material versions and compute
  the coverage warning against the scene's requested wavelengths (the Experiments-bay
  draft's sweep, else the fixed source λ).
- Render a ⚠ warning badge with a hover tooltip on each flagged element (host-owned,
  renderer-independent) and surface the same warning text in the Details bay.
- Add 13 Domain unit tests and 2 headless Ui tests (the flag path + the badge/tooltip
  render), registered in their fsprojs.

## Tests

Per the IMPLEMENT act-only invariant (Invariant 6), the worker ran NO gates this round;
the arc-runner's gate engine is the sole authority. Static verification only: acceptance
cases are pinned by the new tests (600 nm-in-band no-flag, 200–800 sweep flags with
material + both ranges, multi-segment unions, gap non-coverage, constant/coded never
flag, headless badge + tooltip); all matches are exhaustive (`--warnaserror+:25`); the
diagnostic surface carries only elevated types with the nm boundary crossing solely
through `Units`; the default unflagged scene yields no badges so ui-smoke's mainView
render is unchanged; no file is mixed-EOL.

Expected roster (engine-run): `build`, `unit-tests`, `constructor-unit-tests`,
`ui-smoke`, `ui-tests`. This round adds 13 facts under `constructor-unit-tests`
(623 → 636) and 2 facts under `ui-tests` (426 → 428); `ui-smoke` (160) and the solver
`unit-tests` (119) counts are untouched.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 636
  ui_smoke_tests: 160
  ui_tests: 428
```

## Architecture

- The diagnostic lives ENTIRELY in the pure Domain (`OutOfBandDiagnostic`): it takes
  already-resolved reachable materials + the requested wavelengths and returns a typed
  `DispersionCoverage`. The Ui host only (a) resolves reachability from a binding and
  (b) renders the badge — logic stays testable without a window.
- Coverage merges the defined segment intervals (touching/overlapping fuse) and, because
  a request is contiguous, treats it as covered iff it fits inside ONE merged interval —
  so a request straddling a gap is out of band even when both ends land in segments.
- "Dispersive" means "declares dispersion segments as DATA" (`MaterialComplexity`'s eps
  `EpsDispersiveValue` segments). A constant eps or a coded preset (`complexity = None`)
  exposes no defined segments and never flags — the diagnostic only states ranges a
  material declares.
- Requested wavelengths route through ONE seam (`requestedWavelengthsFor`): a wavelength
  sweep → its nm range (via the sole `Units.toWaveLength` conversion), else a fixed λ.
  The Ui feeds it the Experiments-bay draft; `requestedWavelengthsOf` feeds it a step-25
  `Experiment`.
- The badge is a host overlay appended in `mainTableCanvas` (not in `ElementRenderer`),
  so it is renderer-independent (shape AND wireframe) and layers above the schematic; the
  tooltip uses Avalonia's attached `ToolTip.Tip`, a new pattern set via `AttrBuilder`.

## Deferred

- **A directly-material-bound element.** No table element binds a material directly today
  (only samples reach materials), so the host resolves the sample path alone; the pure
  function already accepts any reachable-material list, so a future direct binding is a
  non-breaking addition.
- **Coded-preset defined ranges.** Silicon / langasite (dispersion coded, not data) carry
  no `EditSegment.interval`, so the diagnostic cannot state their defined band and never
  flags them — surfacing coded presets' valid bands is out of scope (would need the engine
  preset to publish its own range).

## Gotchas

- The badge is host-owned and renderer-independent (appended in `mainTableCanvas`), not a
  `Drawable` field — so it shows under both renderer looks without touching `ElementRenderer`.
- Requested wavelengths in the LIVE scene come from the Experiments-bay draft (a wavelength
  sweep) or the fixed source λ (`runWaveLength`, 600 nm default); a scene with no configured
  sweep therefore checks the single source λ.
- The tooltip is the first `ToolTip.Tip` use in the app — set through the same `AttrBuilder`
  attached-property mechanism as the AutomationId, guarded in the test against the null
  AutomationId every unmarked control carries.
- The working tree is uniform CRLF on this host; `.gitattributes` is `text eol=lf`, so git
  normalizes to LF on commit — no file is mixed-EOL, so no churn is introduced.
- The task file's system-prompt path is stale (`.../implement_worker.system-md` at the repo
  root); the real file is under `.../src/ai_strategy_generator/multistep/`.

## Changelog

- 2026-07-11 — step 031: added the pure `OutOfBandDiagnostic` Domain module (requested-
  wavelengths + reachable-material types, segment-union coverage, per-material
  defined-vs-requested findings, tooltip/Details text) with 13 unit tests; wired the Ui host
  to resolve a bound sample's reachable material versions, compute the coverage warning
  against the scene's requested wavelengths, render a ⚠ warning badge + hover tooltip on
  flagged elements, and surface the same text in the Details bay; added a headless
  badge/tooltip render test.
