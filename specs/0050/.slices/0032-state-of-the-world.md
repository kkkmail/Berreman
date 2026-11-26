# Step 0032 — ADD_CONTRACT STORE_BigBoy_0001 CatalogueProxy — state of the world

## Where we are

Spec 0050, section S1.5: "The built-in catalogue generated once from the code seeds". Steps 24 to 31 gave the entry
files, the manifest model and their JSON. This step declares the seam over a whole catalogue folder in Domain: its
data, its errors, its proxy with an in-memory double, and the shipped catalogue's frozen id. `STORE_BigBoy_0001` stays
`declared`. Step 33 implements the proxy over a real folder, and steps 34 and 35 use it for the parity test and the
one-time export.

## What's working

The code is written, but its facts have never run, and attempt 2's fixture edit has not been compiled; see Tests.

- Added `CatalogueFiles.fs` with `CatalogueIds.builtInCatalogue`, the shipped catalogue's frozen id
- Added `CatalogueError`: five reason-carrying cases plus `InvalidCatalogueEntriesErr` over step 26's refusal
- Added `CatalogueData` with `tryCreate` (envelopes in manifest row order) and `validate` (the one consistency rule)
- Added `CatalogueProxy` (`readCatalogue`, `writeCatalogue`) and its in-memory double `createInMemory`
- Added `CatalogueProxyTests` with the slice's four facts, over test-local fixtures that read no code seed

## Tests

**This worker ran no gate, compiled nothing and executed no test, on either attempt.** The role prompt says "you act; you
run no checks".

What the gate engine has measured so far:

- `build` passed on attempt 1's tree (20:12:42Z), before attempt 2 edited the test fixtures. The edited file has not
  been compiled.
- `elevation` and `constructor-unit-tests` have not run, so the four facts have never executed.

Read-only checks made:

- All four fact names match the slice's test ids exactly.
- The hash literals are 64 lowercase hex characters and distinct.
- `CatalogueProxyTests.fs` contains none of the names step 58's acceptance grep looks for.
- Both `Compile` lines sit where the slice puts them.
- `CatalogueProxy` has exactly its two fields.
- Files are LF with no CR bytes and no BOM.
- The new names collide with nothing in Berreman or STL.

The count below is **derived, not measured**: the arc-runner's checkpoint of 776 plus the 4 new facts.

```yaml
gates:
  constructor_unit_tests: 780
```

## Architecture

- **One sort rule, one consistency rule.** `CatalogueData.tryCreate` and `validate` both go through
  `CatalogueManifest.tryCreate`, so the sort and the duplicate refusals stay step 26's alone. Both stores call the same
  `validate`.
- **`validate` tests two things.** The manifest must be the one the entries make, and the envelopes' rows must be its
  entries in order. The rebuilt manifest is sorted, so comparing it alone cannot see envelopes out of order.
- **A proxy per catalogue folder.** `CatalogueProxy` takes no folder argument: one is built where it is composed, and
  Storage's `create` (step 33) closes over the folder.
- **The double holds at most one catalogue**, in a closure-local `ref`, so mutation stays inside the closure, as in
  `SourceProxy.createInMemory`.
- **The test fixtures are test-local records** keyed to `MaterialIds`, `SampleIds`, `PresetIds` and `CategoryIds`, which
  step 58 keeps. The file depends on no code seed.

## Deferred

Nothing. Every requirement of the slice is addressed in this round: items (1) to (5) and facts (a) to (d). The folder
implementation is step 33's by the spec's own plan, and the error render is step 39's.

## Gotchas

- **The edited fixtures have not met the compiler, and no fact has run.** A compile error, a failing fact or an ELEV
  finding would first appear at the gate engine. One more failure on this slice exhausts its budget (`count=1/2`).
- **The prompts disagree on local checks.** "You run no checks" (`add_contract_worker.system-md`) conflicts with "Run
  the relevant tests after every change" (AGENTS.md). This round followed the role prompt; steps 26 to 31 took the other
  reading. An operator ruling would settle it for the remaining steps. See the impl-log's Gotchas.
- **`contentHash` is a label on two records** (`CatalogueManifestRow`, `LibraryEntryEnvelope`). Annotate the value in a
  `{ x with contentHash = … }` expression.
- **`CatalogueProxyTests.fs` must not read a code seed, because nothing later can repair it.** Step 58 deletes those
  seeds, its acceptance grep must find their names nowhere under `Berreman/`, and its edit list omits this file. So the
  fixtures are test-local, and the file's comments do not name the deleted seeds. Attempt 1 said step 58's work would
  move these fixtures; slice 0058 says the opposite, and that claim is withdrawn.

## Changelog

- 2026-11-05: step 0032 added `CatalogueFiles.fs` (`CatalogueIds`, `CatalogueError`, `CatalogueData`, `CatalogueProxy`,
  `createInMemory`) and `CatalogueProxyTests` with four facts; not compiled or run in this round.
- 2026-11-05: attempt 2, after the code judge's route-back: `CatalogueProxyTests` fixtures became test-local records that
  read no code seed; `CatalogueFiles.fs` and the facts are unchanged; not compiled or run in this round.
