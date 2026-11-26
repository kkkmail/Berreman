# Step 0462 — state of the world

## Where we are

Spec 0050, section S13.2 ("PTB OCDB, the Henke compound calculator and glass catalogues"). This step adds the reader
for a user-supplied Zemax glass catalogue (`.agf`, as Schott, Ohara, Hoya and CDGM publish theirs).
`GlassCatalogueText.importGlasses` turns each `NM` block into one user glass. Each glass's AGF dispersion formula is
rewritten as the refractiveindex.info formula of the same equation and lowered by the existing `lowerFormulaAxis`, so
no second formula engine exists. Nothing is bundled. Step 0463 wires the reader into the dropped-file import path,
using `GlassCatalogueText.ofBytes` and the `FileImportProvenance` from step 0461.

## What's working

- Added `AgfFormulaNumber` (with `family`) and its `riiFormOf`, which rewrites AGF formulas 1–13 term for term as
  refractiveindex.info formulas 1, 2, 3, 4, 5 and 7.
- Added `GlassCatalogueText` with `ofBytes`, which decodes UTF-8 or UTF-16 with or without a byte-order mark.
- `importGlasses` reads `NM` / `CD` / `LD` into one glass each, files it under glass, and names it after the glass.
  It refuses the whole text and names every glass that does not read or lower.
- `entryOfAxis` and the µm range helpers are now shared by the refractiveindex.info page and the AGF reader.
- Added the fixture `test-glasses.agf` and three facts. Every equation matches within 1e-12, and constructor-unit-tests
  rose by 3 to 1831.

## Tests

These are local development runs; the supervisor re-runs the roster.

- **build:** green on the final code (12 s incremental; the first full build took 49 s), against a 600 s deadline.
  The only warnings are the exempt `NU1902`.
- **constructor-unit-tests:** whole suite green on the final code, 1831 of 1831, 108 s against a 180 s deadline.
  - That is checkpoint 0461's 1828 plus the three new facts.
  - The three facts are: `a glass catalogue's glasses lower through MaterialImport's formulas to the AGF equations`,
    `a glass catalogue reads in UTF-8 or UTF-16 and takes each glass's wavelength range` and
    `a glass catalogue with a glass no formula lowers, or with no glass, is refused`.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` on the final code, 232 s against a 600 s
  deadline. Known findings are 0. Suppressed findings rose from 1757 to 1759: two measured-only `ELEV004` hits,
  since ELEV004 is excluded from the gate.
- **kit-acceptance** over `OpticalConstructor.Ui.Tests.KitAcceptanceTests`: `Passed!`, 14 succeeded and 1 skipped (the
  explicit five-seed ensemble), in 1436 s against a 3600 s deadline. AT-1 and AT-2's expectations and tolerances are
  untouched. The counts match 0460's.
  - It ran before two local identifier renames in `MaterialImport.fs` (`head` → `prefix`, an inner `read` →
    `imported`). They change no type, signature or behaviour.
  - The build, the constructor suite and elevation were re-run after them. Kit-acceptance was not repeated.
- **invariant-s44-\* (21):** all hold on the final tree.

```yaml
gates:
  constructor_unit_tests: 1831
  kit_acceptance_tests: 14
```

## Architecture

- **One formula engine.** An AGF glass becomes a refractiveindex.info page (`RiiFormulaNumber` plus a coefficient
  block) and goes through the unchanged `lowerFormulaAxis` and `entryOfAxis`.
  - Every AGF term maps to an existing term exactly; no number is approximated.
  - Conrady's λ^3.5 is refused by the existing non-integer-exponent rule.
- **ELEV-driven signature deviations** (impl-log Gotchas). The how_to's literal signatures would fail ELEV009 /
  ELEV010, which check members.
  - `riiFormOf` takes and returns the existing `[<NumericCarrier>]` `FormulaCoefficients`, made public, instead of
    `float array`. Its pairing moved to the private `CoefficientPair.pairsOf`.
  - `ofBytes` takes STL's `FileByteContent`, as `ContentHash.ofBytes` does, instead of `byte[]`.
- **All or nothing.** Every glass's refusal is collected, one per line, into one `MalformedSourceFileErr`, so a
  catalogue never imports in part. A text with no `NM` line is `NoDataErr`, and any exception is
  `MalformedSourceFileErr`.

## Deferred

Nothing. Wiring the reader into `decideDroppedFiles` is step 463's brief, not a deferral.

## Gotchas

- A glass with no `CD` line, or with two `CD` or two `LD` lines, is refused rather than imported as zeros. That is
  the reading chosen for "does not read".
- The formula number reads through `Double.TryParse` plus `Double.IsInteger`, so `1.0` reads as formula 1.
- `FormulaCoefficients` is now public in `OpticalConstructor.Storage.MaterialImport`. `CoefficientPair` and
  `RiiCoefficient` stay private, so the public surface gains only the carrier.
- Mutation check: changing formula 9's `C1 − 1` to `C1` fails the first fact at TEST-F9. The file was restored before
  the final runs.

## Changelog

- 2026-11-17 — step 0462: AGF glass-catalogue reader (`AgfFormulaNumber`, `riiFormOf`, `GlassCatalogueText`,
  `ofBytes`, `importGlasses`) over the existing formula lowering; fixture `test-glasses.agf` and three facts added;
  gates green locally.
