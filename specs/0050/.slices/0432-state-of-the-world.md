# Where we are

Step 0432 lands the first part of AT-2, the absorbing-biaxial-noisy acceptance kit (spec 0050 S12.2; 001 §Q AT-2). It
is built only through the app's own steps: the Library window's drop, the import dialog, the start screen and the kit
window's messages. AT-1 now gets its test clock and start screen from the same two functions. Later steps 0433–0439
extend `buildAbsorbingKit` with the grids, the data, the study and the fit. The supervisor has not yet run the gates.

# What's working

- Added AT-2's build: the absorbing material and plate are dropped as entry files and imported, then the kit gets the `{obj1}` template, eight items and a table capturing the plate in T and R.
- Added `absorbingBuild`, one lazy build shared by AT-2's facts, and the fact asserting its kit file, items, `library/` snapshots and table.
- AT-1 now composes its clock, start screen, fixed ids, Add item form and kit-file checks through functions shared with AT-2; its messages and assertions are unchanged.
- The reflected detector takes a fixed id from `ElementIdProvider`, as `.manual/033` requires.

# Tests

No gate was run in this round. IMPLEMENT Invariant 6 leaves gate execution and verdicts to the supervisor after the
worker exits. As in steps 0428–0431, the final JSON reports no gate results and `commit_ready=false` only because no
gate was run here. No requirement or authority is blocked, and no development run took place, so no deadline was
exceeded.

The 0432 roster is unchanged:
- build;
- elevation (the edited file is in a test project, outside the ELEV scope);
- kit-acceptance, selecting `OpticalConstructor.Ui.Tests.KitAcceptanceTests`;
- ui-smoke over every discovered smoke test;
- ui-tests, selecting `StudyViewTests` and `DataStepTests` (both untouched);
- the twenty-one s42 invariants.

The new fact `AT-2 builds the absorbing-biaxial-noisy kit's library entries, items and table through the app's
messages` raises the kit-acceptance count by one.

These counts are static source counts, not measured runs: `KitAcceptanceTests` declares 7 facts with
`Category=kit-acceptance`, up from 6. The supervisor's checkpoints stay authoritative.

```yaml
gates:
  kit_acceptance_tests: 7
```

# Architecture

Everything is in `KitAcceptanceTests.fs`. `buildAbsorbingKit (run : FolderName) : BuiltAcceptanceKit` answers the
built folder, the app context, the window model and the test clock, in that order. The material and the sample are
written as sealed entry files under `<run>/entries/`, beside the kit and outside it. A Library window composed from
the app context's stores (as `LibraryWindowTests`' `stubContext` composes one) receives
`DropLibraryEntryFiles`. The import dialog then applies its default rows, and the window takes
`LibraryImportApplied`.

Item and slot identities come from `mintingFrom`, which AT-1 now uses too. Items go through `addItemThroughForm`,
whose `choiceFor` matches step 216's payloads, the stack choice included. Expected item models are read from the
shipped entries through the app's `ItemModelProxy`.

`library/` holds seven snapshots, not the slice's four. Item recording snapshots each model's version and its
binding's closure, so the source-preset and stack-sample binding models and the material's Crystal category are
written too (see the impl-log's Decisions). The fact asserts exactly those seven, each byte-identical to its dropped
or `Catalogue/Builtin/` file.

# Deferred

Nothing is deferred. Steps 0433–0439 add AT-2's later parts, as the spec plans.

# Gotchas

- In step 0433, recording setups snapshots the reflected detector's leftover seed `PresetIds.detectorIntensity`:
  `SetupSlot.withItem` keeps a standalone model's prebound `valueId`. The seven-snapshot set must then include it.
- `absorbingBuild` uses the fixed run name `AT-2`. A build deletes the previous run's folder first. A failed build
  keeps its folder for diagnosis. No fact closes the shared kit.
- The extra-request failure of AT-1's fixed-id minters now gives the count as a number instead of a word.

# Changelog

- 2026-11-15 — Step 0432: AT-2's first part (library drop, kit, eight items, T/R table, shared lazy build, fact);
  AT-1 composes its clock, start screen, fixed ids, item form and kit-file checks through the shared functions.
