# Step 0034 — ADD_RED — state of the world

## Where we are

Spec 0050, section S1.5: "The built-in catalogue generated once from the code seeds". Steps 32 and 33 gave `CatalogueProxy` its
in-memory double and its folder implementation. This step declares `RED_0006`: five facts say the catalogue in the test output
equals the code seeds, and all five are red because the build copies no `Catalogue/Builtin/` yet. Step 35 exports the seeds
through `CatalogueProxy.create`, adds the copy to `OpticalConstructor.Storage.fsproj`, and closes the red.

## What's working

- Added `BuiltInCatalogueParityTests` with the slice's five facts, compiled after `CatalogueFolderTests.fs`
- Each fact reads `Catalogue/Builtin/` from the test output through `CatalogueProxy.create`, and fails on an error
- Sealed the expected catalogue from every seed in the test, with nothing taken from the export
- Compared decoded entries with their seeds by id, using each kind's own decoder
- Observed all five facts red locally with `CatalogueIoErr`; no production file changed

## Tests

The gate engine is the gate authority, and these are local observations made to record the red.

- **Test project build:** exit 0, 0 errors, and only the 2 vendored MathNet `SYSLIB0051` warnings.
- **`BuiltInCatalogueParityTests` alone:** exit 2, `total: 5`, `failed: 5`, `succeeded: 0`. All five fail with `expected Ok, got
  CatalogueIoErr`, naming the missing `Catalogue\Builtin\.catalogue-json`. The failing output is in `0034-impl-log.md` (Testing
  state).
- **Not run:** the solution build, the `elevation` gate, the six s05 invariant commands, and the rest of
  `constructor-unit-tests`. A grep of the new `.fs` for the invariant patterns finds nothing.
- **Never executed:** the facts' green path, because no catalogue exists yet. That they can go green at step 35 is reasoned from
  existing facts, not measured.

The count below is **carried, not measured**: the last green checkpoint of 786. This step adds five reds and no passing fact, and
the whole suite was not run.

```yaml
gates:
  constructor_unit_tests: 786
```

## Architecture

- **No production code changed, and no new test seam was added.** The class uses only `CatalogueProxy.create`, step 25's
  builders and decoders, step 30's `sealed`, and step 31's `manifestHash`.
- **One private helper, `assertDecodesToSeeds`, carries facts (b), (c) and (d):** it filters a kind, decodes with that kind's
  decoder, sorts both sides by id, and compares the ids and then the whole records.

## Deferred

Nothing. All five facts the slice names are written and observed red. Step 35 closes them, as the step's own pairing declares.

## Gotchas

- **Local runs against "you run no checks".** The ADD_RED role orders both observing the red and running no checks, and
  `.manual/` holds no ruling. This round built the test project and ran this one class, and reports no gate result, while step
  0033 ran nothing. An operator ruling would settle it; see the impl-log's Gotchas.
- **Step 35's effects list creates no tests,** so a defect in these facts would surface there as a conflict with its authority.
- **The comparison must stay by id:** a catalogue lists entries by kind and file name, and the seeds keep display order.
- **Do not leave a catalogue in a test output while this step is red.** The facts read `AppContext.BaseDirectory`, and a build
  does not remove what is already there.
- **A file renamed in `Catalogue/Builtin/` after step 35 would stay in an existing `bin`,** because `PreserveNewest` never
  deletes, and it would read as `UnlistedEntryFileErr`. This follows from the code as read; nothing here exercises it.
- **`.artifacts/` is git-ignored,** so the logs named in the impl-log stay local.

## Changelog

- 2026-11-05: step 0034 added `BuiltInCatalogueParityTests` with five facts, observed locally red with `CatalogueIoErr` because
  the test output holds no `Catalogue/Builtin/`; no production file changed.
