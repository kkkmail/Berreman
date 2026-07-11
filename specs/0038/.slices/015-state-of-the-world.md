# Step 015 — state of the world

## Where we are

Step 015 lands the centrepiece of spec 0038 Part F: the single-instance
Library window (UICOMP_XDUO_0010) — the step-012 `FacetedTreeControls`
instantiated a second time (the step-013 Materials-window precedent), now over
the WHOLE `LibraryEntry` corpus (live samples through the shared `SampleProxy`
store plus the sources / detectors / polarizers from the read-only
`LibraryProxy`) through the step-011 library facet catalogue with the seeded
default representation "By kind". The Library (samples workbench) BAY is gone
from the ribbon — no full-surface bay remains — and the tab-strip row's
right-aligned `Library…` button opens the window under `LibraryWindowKey`
through the step-008 launcher. The step-014 `EntryProtection` data is now
ENFORCED at the window's Remove verb: a `ProtectedBuiltIn` entry refuses with
a typed reason. Next in Part F/G: step 016 gives both windows the
Browse/Select mode DU.

## What's working

- Add LibraryWindowView.fs + LibraryWindow.fs (UICOMP_XDUO_0010): a HostWindow
  mounting a pure MVU model that projects the LIVE corpus (SampleProxy samples
  + LibraryProxy presets) through the step-011 libraryFacets into
  FacetedTreeControls — by-kind tree with entry leaves first and every entry
  kind counted, count-previewed offers, breadcrumb chips with after-counts,
  Enter/LostFocus text filter, Show/Search gating, and a selection view panel
  (name — kind, protection note, full description).
- First numeric-facet host: film-thickness offers and tree branches are the
  step-010 buckets (never raw magnitudes) plus the manual min–max box; bucket
  and manual ranges apply as ordinary NumericRangeSelection chips through
  round-trippable "lo:hi" tokens.
- Verbs rewired from the retired Library bay: Add sample / Make multilayer
  (ids minted at dispatch) / Edit (samples only) through the step-008
  WindowLauncher under SampleEditorKey; Remove keeps its inline confirm gate
  and typed blocks and REFUSES a ProtectedBuiltIn entry with a typed reason,
  never arming the gate.
- Remove the Library bay: samplesBay, BayNames.library, the Smp… messages and
  arms, the bay-only model fields and RemoveConfirm are gone; EditorLaunchers
  drops openSampleEditor (moved into the window) and gains openLibraryWindow;
  the right-aligned Library… strip button opens the single-instance window
  (the App composition threads mechanically through defaults).
- 30 new headless/pure window tests; the bay-coupled suites reworked to drive
  the window (AppContext two-surface, WireUi composition, launcher sample-Add).
- Suites 571 / 119 / 135 (+4) / 379 (+12); build clean, no MSB3277, zero
  warnings from touched projects.

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this
  worker exits (ADD_COMPONENT Invariant 6 — the worker acts, it runs no
  checks). The roster for this step is `build`, `unit-tests`,
  `constructor-unit-tests`, `ui-smoke`, `ui-tests`.
- Diagnostic verification (not gate authority): `dotnet build Berreman.slnx -c
  Release` succeeded with 0 errors, **no MSB3277**, and zero warnings from the
  touched projects (Ui / App / Ui.Tests) — the only warnings are the
  step-001-catalogued pre-existing set in untouched files (FS1125
  SeriesDataTests ×4, FS3873 Dispersion, SYSLIB0051 vendored MathNet ×2,
  NU1701 Wolfram.NETLink ×2). Suites: ui-smoke **135/135** (checkpoint 131,
  +8 new window proofs −4 retired bay proofs: strip-button single-instance
  with close-reopen, the by-kind tree listing every entry kind with counts,
  Add-sample persisting through the real editor into the shared store, the
  protected-remove typed refusal changing nothing, the confirm-gated sample
  remove in the same render pass, Edit/Make-multilayer through the real
  editor, the selection view panel, and Show/Search gating with zero rows),
  ui-tests **379/379** (checkpoint 367, +22 new window pure tests −10 retired
  bay tests), OpticalConstructor.Tests **571/571** (== checkpoint),
  BerremanTests **119 passed / 5 pre-existing skips** (== checkpoint). Logs in
  `specs/0038/.artifacts/015-diag-*.log`.
- Nothing deferred.

## Architecture

- **The window is a projection host over TWO read seams**: samples come from
  the LIVE shared `SampleProxy` (so this window's own verbs — and any other
  window's writes — show on the next dispatch-driven pass) while the preset
  entries come from the read-only `LibraryProxy` (`entriesForKind` over the
  four non-sample kinds, deduped by `entryId` — a compound polarizer serves
  both polarizer kinds). The static seeded samples inside the proxy are
  excluded in favour of the live store; `liveEntries` is the reusable seam.
- **"By kind" is the step-011 catalogue order as ONE representation record**
  (kind → polarizer category → lifted material facets → sample-structural
  facets): the kind facet leads the tree, the polarizer facet vanishes over
  polarizer-free populations and the sample facets over sample-free ones by
  the engine's applicability discipline alone — no subtree special-casing.
- **The numeric path stays data-shaped end to end** (§D.0): the window is the
  first host to exercise the step-012 control's `ManualRangeOffered` /
  `applyManualRange` seam — offers AND tree branches re-project the step-010
  buckets per pass, a bucket's value code is its half-open range
  round-trip-formatted ("lo:hi" — ':' so an exponent's '-' can never split
  wrong), the manual box parses "lo-hi" or one exact value, and both apply as
  ordinary `NumericRangeSelection` chips; range display text reuses
  `NumericBucket.label`'s nm/µm unit switch rather than re-deriving it.
- **Protection enforcement is a typed verb refusal, not a hidden verb**: the
  Remove verb stays visible for any selection, and `RequestRemoveSelected`
  matches `entry.protection` — `ProtectedBuiltIn` → `ProtectedEntryRefused`
  naming the entry (the gate never arms), `UserManaged` samples → the confirm
  gate → `removeSample` (store refusals surface as `SampleRemoveRefused`), and
  the unreachable UserManaged preset case is `PresetNotRemovable` so the match
  is total without a throw.
- **The launcher seam reshaped, not paralleled** (the step-013 discipline):
  `EditorLaunchers` lost the bay-only `openSampleEditor` (it moved into
  `LibraryWindow`'s own context, still through the step-008 `SampleEditorKey`
  over the ONE registry) and gained `openLibraryWindow`; `defaults` bakes the
  window factory, so the App composition threads mechanically (step 47 owns
  acceptance; the ctor takes optional `TreeAutoBuildThreshold` /
  `ThicknessBucketCap` overrides for that threading).
- **No full-surface bay remains**: `mainBays` is seven in-pane bays; the
  ribbon's `FullSurface` mode and the workbench's keyed below-strip slot stay
  in place (generic machinery) for any future full-surface bay.

## Deferred

- Browse+Select modes for BOTH windows (`LibraryWindowMode`,
  `SelectionContext`, the Select re-target) — step 016; the launcher-form
  Library/Materials buttons — step 45; threading the app-configured
  `treeAutoBuildThreshold` / `thicknessBucketCap` into the windows — step 47
  (the ctor overrides exist; defaults bake the Domain defaults).
- Live category names inside the lifted material-category facet (the
  Materials window swaps the def's extractor; this window reads the step-011
  catalogue as-is — a rename re-labels only the Materials window's facet until
  a later slice lifts the same substitution here).
- `SampleLibraryControls` is no longer referenced by the Ui project — it stays
  in Controls with its own tests until a later sweep retires it (the
  step-013 `MaterialsControls` precedent).
- Multi-key (OR) selection within one facet from this window's UI (the engine
  supports it data-side).
- The pre-existing warnings in untouched files remain for spec 0038 Part N's
  sweep (carried from steps 002–014).

## Gotchas

- **The task file's system-prompt path was stale again** — the ADD_COMPONENT
  worker prompt lives under
  `src/ai_strategy_generator/multistep/add_component_worker.system-md` in the
  tool repo; located and read in full (the step-007..014 gotcha recurred). The
  slice's `TableAndElementRotationView.fs:2340` / `ElementId.fs:276-281` line
  references were stale too — resolved by symbol.
- **`AppContext.library` (the mock `LibraryProxy`) closes over the STATIC
  `seedEntries`** — it never sees the live samples store. The window's corpus
  merges live samples with proxy presets (`liveEntries`); reuse that seam for
  any "all live entries" need instead of `entriesForKind` alone.
- **Never pin a numeric assertion on a store thickness magnitude** — layer
  thicknesses round-trip through meters a few ulp off nominal (step-011
  gotcha): the window tests use ranges strictly between rungs ("160-260") and
  pin degenerate-value parsing purely.
- **`EditorLaunchers` changed shape again**: an out-of-tree substitute record
  must drop `openSampleEditor` and add `openLibraryWindow`; the workbench
  `RemoveConfirm<'id>` type is gone (its last instantiation left with the
  bay).
- **`LibraryWindowKey` is app-global registry state in tests** — every test
  that opens the real window (directly or via the strip button) must CLOSE it,
  or a later strip click ACTIVATES a stale window over the wrong stores
  (extends the step-008/013 registry gotcha; a TWO-Main-window test must close
  the first surface's Library window before the second can open its own).
- **No registry edit was needed**: `.contract-ids/XDUO-json` and
  `specs/0038/.contracts-json` already carry UICOMP_XDUO_0010 (declared,
  step 15) — the supervisor maintains both (the step-007 precedent).
- Step 002–014 carried-over gotchas remain valid (baselines come from
  `.checkpoints-json`, not the SoW YAML; the FuncUI Elmish host skips a
  structurally-equal model — a test refreshing after an out-of-band store
  write dispatches a CHANGING commit; the appsettings.json write-back into
  test output copies is expected).

## Changelog

- 2026-07-11 — Step 015 (ADD_COMPONENT UICOMP_XDUO_0010, attempt 1): added the
  single-instance Library window (`LibraryWindowView.fs` + `LibraryWindow.fs`
  — FacetedTreeControls over the step-011 library facets with the seeded
  "By kind" representation, live corpus from SampleProxy + LibraryProxy,
  step-010 bucket offers/branches + manual min–max for film thickness, a
  selection view panel, Add sample / Make multilayer / Edit / Remove through
  the step-008 launcher with the typed ProtectedBuiltIn remove refusal),
  removed the Library bay from the ribbon (BayNames/mainBays/Smp… arms/bay
  fields/RemoveConfirm; EditorLaunchers reshaped), added the right-aligned
  `Library…` strip button opening the window under `LibraryWindowKey`, and
  moved/authored the test coverage (30 window tests; bay suites reworked).
  Build clean (no MSB3277); suites 571 / 119 / 135 / 379.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 571
  ui_smoke_tests: 135
  ui_tests: 379
```
