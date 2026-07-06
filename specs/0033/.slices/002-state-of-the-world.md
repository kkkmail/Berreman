# State of the world — spec 0033, slice 002

# Where we are

Slice 002 is the second IMPLEMENT step of arc 0033: it elevates material and
sample identity. `MaterialId` and `SampleId` are now Guid-backed single-case DUs
in `OpticalConstructor.Domain` — no domain record carries a raw string
material/sample id anywhere. The built-in materials and the 11 seeded samples
carry FIXED literal Guids (deterministic across runs); imports mint fresh ids;
the Selector `valueId` binding seam is unchanged (a sample's `entryId` is its
Guid string form). This builds directly on slice 001's stack-as-data types and
feeds every later slice that references materials or samples by id.

# What's working

- Add Guid-backed MaterialId (value / create / tryCreate) + fixed-literal MaterialIds for the 12 built-ins; MaterialEntry.id, SampleLayer.materialId, SampleStructure.lower re-typed
- Add Guid-backed SampleId; the 11 seeded samples become the named SeedSamples module (fixed literal Guids) and seedTrees leaves reference them programmatically — no repeated id literals
- resolveMaterial / resolveMaterialWithDisp are MaterialId lookups; UnknownMaterialId now carries reason : string naming the offending Guid
- MaterialImport mints MaterialId.create() per imported entry; library JSON round-trips the Guid string form with a typed error on a non-Guid id (no legacy path)
- StackEditor.layerMaterialDrop / MaterialsView take MaterialId; the drag payload carries the Guid string form parsed back at the drop boundary
- Add create-store-lookup round-trip tests for minted MaterialId / SampleId and every seeded sample (+4 tests, all suites green)

# Tests

All gates in the slice roster pass in the worker's local (advisory) run; the
arc-runner gate engine re-runs them authoritatively after exit.

- `build` — solution builds Release/x64, 0 errors (`--warnaserror+:25` clean).
- `unit-tests` (BerremanTests) — 84 passed, 5 skipped (pre-existing skips), 0 failed.
- `constructor-unit-tests` — 310 passed, 0 failed (4 tests added this round:
  minted-MaterialId round-trip, seeded-sample entryId round-trip, minted-SampleId
  round-trip, seeded-ids-distinct; none removed).
- `ui-smoke` — 54 passed, 0 failed.
- `ui-tests` — 249 passed, 0 failed.

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    84
  constructor_unit_tests: 310
  ui_smoke_tests:         54
  ui_tests:               249
```

# Architecture

- **Identity is a Guid-backed single-case DU, fixed for seeds, minted for
  imports.** `MaterialIds` / `SeedSamples` hold the only id literals
  (`Guid.Parse` of fixed strings) so trees and tests are deterministic;
  `create ()` mints `Guid.NewGuid()` at genuine creation sites (imports);
  `tryCreate` parses the Guid string form only at the two IO boundaries
  (library JSON, the Avalonia drag payload) and returns `Option` — no throw,
  no fallback.
- **The valueId seam stays a string.** `LibraryEntry.entryId` returns the Guid
  STRING form for samples, so the Selector/`BindValueId`/`tryGetEntry` plumbing
  and the `LibraryControls` automation-id scheme are untouched; only the values
  flowing through them changed.
- **Case/type name collision handled by module-level helpers**
  (`tryMaterialId`, `newMaterialId`, `newSampleId`) mirroring the existing
  `Library.elementId` precedent — qualified access to a static member on a
  DU whose case shares its name resolves to the case.
- **Colour keying:** TestWindows band colours are a `Map<MaterialId, string>`
  over `MaterialIds` (host id literals gone); `Schematic.colorForMaterial`
  keeps its string "identity key" signature because `Schematic.layout` callers
  supply synthetic per-layer keys — the curated map is re-keyed by the
  built-ins' Guid string forms.

# Deferred

- Structure rotation in `sampleToSystem` — step 20 of this arc (unchanged from
  slice 001's deferral).
- Source/detector/polarizer preset ids remain strings — the slice elevates
  material/sample ids only; elevating the remaining preset ids is future work
  if a later step mandates it.
- No persisted-format migration: pre-existing library JSON files with
  non-Guid ids now fail import with a typed error, per the slice's "no
  migration" mandate.

# Gotchas

- Four `OpticalConstructor.Ui.Tests` files (outside the declared `touches`)
  bound seeded samples by the old string-id literals; they now reference the
  seeded samples programmatically — required to keep the `ui-smoke`/`ui-tests`
  gates green.
- Qualified `MaterialLibrary.MaterialId.tryCreate` resolves to the union case,
  not the type — use the module-level helpers (`tryMaterialId` etc.) from
  qualified call sites.
- `StackEditTests.fs`'s worktree copy was CRLF before this round (index LF);
  normalized to LF. No CRLF churn (`git diff --numstat` identical with/without
  `--ignore-cr-at-eol`). `specs/0033/.manifest.state.json` also carries CRLF
  but is the arc-runner's own file — left alone.
- The unknown-material chart message in the TestWindows host now reads
  "Cannot run: the sample references an unknown material (unknown material id
  '<guid>')." — the error case carries `reason`, not the raw id.

# Changelog

- 2026-07-05 — slice 002: MaterialId/SampleId elevated to Guid-backed DUs;
  fixed-literal seed ids (MaterialIds / SeedSamples); resolveMaterial is a
  MaterialId lookup; UnknownMaterialId carries reason; imports mint ids;
  JSON/drag boundaries parse the Guid string form; +4 round-trip tests, all
  suites green.
- 2026-07-05 — slice 001: sample stacks made DATA (SampleStructure + typed
  material resolution); id-branching sampleToSystem replaced by total
  ResolvedSample mapping; 4 new material built-ins; 11 samples re-seeded
  structurally; host resolves once per run; +5 tests, all suites green.
