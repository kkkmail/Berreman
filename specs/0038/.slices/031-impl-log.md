# Step 031 — impl-log (IMPLEMENT: out-of-band dispersion diagnostic)

## Progress

- [x] Pure Domain module `OutOfBandDiagnostic.fs` — types + coverage logic + text + derivation
- [x] Registered in `OpticalConstructor.Domain.fsproj` (compiles last)
- [x] Domain unit tests `OutOfBandDiagnosticTests.fs` (+ Tests.fsproj)
- [x] Ui host wiring: reachable-material resolution, `outOfBandWarningFor`, badge overlay, Details text
- [x] Badge automation id in the scene `UiIds` module
- [x] Ui.Tests headless badge test `OutOfBandBadgeTests.fs` (+ Ui.Tests.fsproj)
- [x] EOL check (uniform CRLF working tree — `.gitattributes eol=lf` normalizes on commit; no mixed EOL)

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OutOfBandDiagnostic.fs` — NEW, pure module:
  - `RequestedWavelengths = FixedWavelength of WaveLength | SweptWavelengths of WaveLengthInterval`
    (+ `.span` member = the request as a contiguous interval).
  - `ReachableMaterial = { materialName; definedSegments : WaveLengthInterval list }`.
  - `OutOfBandFinding = { materialName; definedRange; requestedRange }`.
  - `DispersionCoverage = InBand | OutOfBand of OutOfBandFinding list`.
  - `definedSegmentIntervals : MaterialComplexity -> WaveLengthInterval list` (reads each eps segment's
    `wavelengthInterval`; a constant `EpsWithoutDispValue` → `[]`).
  - `reachableMaterialOf : MaterialEntry -> ReachableMaterial` (a coded preset `complexity = None` → no
    defined segments → never flags).
  - private `mergeIntervals` (touching/overlapping fuse), `covers` (a contiguous request fits inside ONE
    merged interval), `definedSpan` (min lower … max upper).
  - `checkOutOfBandDispersion` / `checkMaterialsOutOfBand`, `requestedWavelengthsFor` /
    `requestedWavelengthsOf` (sweep → nm range via the sole `Units.toWaveLength` seam; else the fixed λ),
    `nmRangeLabel` / `findingText` / `coverageWarning`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` — registers
  `OutOfBandDiagnostic.fs` last (depends on MaterialLibrary / Experiments / Units; nothing depends on it).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`:
  - `UiIds.badgePrefix` + `UiIds.outOfBandBadge (i : int)` (indexed automation id — the
    `LayerBandsControls.UiIds.band` pattern).
  - `sampleMaterialVersions` / `reachableMaterialEntries` — resolve a bound sample's layer material
    VERSIONS through `model.materials.resolveVersion`; non-sample / unbound reach none.
  - `sceneRequestedWavelengths` — the Experiments-bay draft's wavelength sweep, else the fixed source λ
    (the existing `runWaveLength`).
  - `outOfBandWarningFor` (public) — the per-element coverage warning; shared by the Details bay + badge.
  - `badgeAutomationId` / `badgeToolTip` (AttrBuilder attached-property helpers) + `outOfBandBadge` (a "⚠"
    `TextBlock` at the element's projected centre, an AutomationId, an Avalonia `ToolTip.Tip`) +
    `outOfBandBadges` (public) — the flagged-element badges over the whole scene.
  - `mainTableCanvas` appends `outOfBandBadges model` to the canvas children.
  - `detailsState` appends the same warning to the selected element's title (`⚠ …`).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OutOfBandDiagnosticTests.fs` — NEW (13 facts).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — registers it.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OutOfBandBadgeTests.fs` — NEW (2 facts:
  pure host derivation + headless badge/tooltip render).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` — registers it.

## Testing state

Per the IMPLEMENT act-only invariant (Invariant 6), the worker ran NO gates this round — the arc-runner's
gate engine is the sole authority. Static verification only:

- **Acceptance coverage.** Domain tests pin: 600 nm fixed inside 300–700 → InBand; a 200–800 sweep against
  the same segment → OutOfBand naming the material + both ranges; adjacent multi-segment unions cover a
  spanning request while a gap does not; a constant material and a coded-preset (`complexity = None`) never
  flag; the requested-wavelengths derivation + tooltip/Details text. Ui.Tests pin: a wavelength sweep past a
  bound sample's defined band flags exactly that element (naming material + both ranges), a fixed in-band
  request flags none, and the badge control + its tooltip text render headless.
- **Exhaustive matches** (FS25 is `--warnaserror`): every match over `EpsWithDispValue` / `EpsDispersiveValue`
  / `VariableParameter option` / `StackItem` / `Selection` / the coverage & option results has all cases or a
  wildcard.
- **Elevated types**, no naked primitives in the diagnostic surface (WaveLength / WaveLengthInterval / the
  named DUs); the nm boundary crosses ONLY through `Units.toWaveLength` / `wavelengthToUnit`.
- **Order-sensitivity:** the badge helpers sit after `runWaveLength`/`boundEntry` and before
  `mainTableCanvas`; `outOfBandWarningFor` is defined before `detailsState`; the badge id is in the early
  `UiIds` module.
- **Render safety:** the tooltip is a NEW pattern — Avalonia's attached `ToolTip.Tip` set via the same
  `AttrBuilder` mechanism the AutomationId uses; the default (unflagged) scene yields NO badges, so the
  `@ outOfBandBadges model` append is empty there and ui-smoke's mainView render is unchanged.
- **EOL:** the working tree is uniform CRLF (every source file, including untouched ones); `.gitattributes`
  is `text eol=lf`, so git normalizes to LF on commit. No file is mixed-EOL (CR-line count == total-line
  count per file). No churn introduced relative to the working tree.

Expected roster (engine-run): `build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`, `ui-tests`.
Deltas from the step-030 baseline: +13 `constructor-unit-tests` (623 → 636), +2 `ui-tests` (426 → 428);
`ui-smoke` (160) and `unit-tests` (119) untouched.

`commit_ready: true` — every requirement of the slice is addressed this round.

## Artifacts

None — pure code + headless tests; no captured logs/screenshots this round.

## Gotchas

- **"A directly bound material" has no element kind today.** In the current Library only a SAMPLE reaches
  materials (its layers' `MaterialVersionId`s); source / detector / polarizer presets carry no material.
  The PURE Domain function accepts any reachable-material list, so a future directly-material-bound element
  is a non-breaking addition; the host `reachableMaterialEntries` resolves the sample path alone (recorded
  interpretation — the slice's "a directly bound material" clause has no current binding site).
- **"Dispersive" means "declares dispersion segments as DATA."** A coded-preset dispersive material (silicon
  / langasite, `complexity = None`) exposes no `EditSegment.interval`, so the diagnostic cannot state a
  defined range for it and never flags it — the check only speaks about ranges a material declares. Constant
  materials likewise never flag (no segments). This is the literal reading of "the union of that material's
  DEFINED dispersion segments".
- **Requested wavelengths in the live scene** come from the Experiments-bay DRAFT (a wavelength sweep when
  `draft.variable = VaryWaveLength`) else the fixed source λ (`runWaveLength`, 600 nm default). The pure
  `requestedWavelengthsOf` also derives the same from a step-25 `Experiment`; both route through the single
  `requestedWavelengthsFor` seam.
- **Badge is host-owned, renderer-independent.** It is appended in `mainTableCanvas` (not inside
  `ElementRenderer`), so it shows under BOTH the shape and wireframe looks, on top of the schematic. It uses
  a "⚠" glyph + a red foreground; the warning is textual (tooltip + Details), never hue-only.
- **Coverage of a swept range is single-merged-interval containment.** Because a sweep is contiguous, it is
  covered iff it fits inside ONE merged segment; a request straddling a gap between two disjoint segments is
  out of band even when both its ends land in segments.
- **Stale system-prompt path in the task file.** It names
  `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md`; the file lives at
  `.../src/ai_strategy_generator/multistep/implement_worker.system-md`. Read there; no scope impact.

## Changelog

- 2026-07-11 — Step 031 (IMPLEMENT): added the pure Domain `OutOfBandDiagnostic` (requested-wavelengths +
  reachable-material types, segment-union coverage, per-material defined-vs-requested findings, tooltip/
  Details text) and its 13 unit tests; wired the Ui host to resolve a bound sample's reachable material
  versions, compute the coverage warning against the scene's requested wavelengths, render a ⚠ warning
  badge + hover tooltip on flagged elements (`mainTableCanvas`), and surface the same text in the Details
  bay; added the headless badge/tooltip render test.
