# Step 0033 — IMPLEMENT_CONTRACT STORE_BigBoy_0001 CatalogueProxy — state of the world

## Where we are

Spec 0050, section S1.5: "The built-in catalogue generated once from the code seeds". Step 32 declared `CatalogueProxy` in
Domain with an in-memory double. This step gives it the real folder implementation in Storage, so `STORE_BigBoy_0001` is now
`implemented`. Step 34 reads the shipped catalogue through it for the parity facts, and step 35's one-time export writes
through it.

## What's working

The code is written, but it has never been compiled and its facts have never run; see Tests.

- Added `StorageError.describe`, the one-line render a Domain error's reason carries
- Added `LibraryFileText.writeAtomically`: write a `.tmp` beside the file, then move it over the file
- Added `CatalogueProxy.create` over a real folder, reading and validating in the slice's order
- Made a write refuse a folder holding an unnamed entry file, and write the manifest last
- Added `CatalogueFolderTests` with the slice's six facts over real folders under the test output
- Marked `STORE_BigBoy_0001` implemented at step 33 in `.contracts-json`

## Tests

**This worker compiled nothing, executed no test, and ran no ELEV analyzer and no invariant command.** The role prompt says
"you act; you run no checks". `build`, `elevation`, `constructor-unit-tests` and the six s05 invariants are all the gate
engine's and have not run on this tree.

Measured, as edit hygiene only: every touched source and project file has 0 CR bytes and no BOM, the tracked `Berreman/`
files show no line-ending churn, and the six fact names equal the slice's test ids.

The count below is **derived, not measured**: the arc-runner's checkpoint of 780 plus the 6 new facts.

```yaml
gates:
  constructor_unit_tests: 786
```

## Architecture

- **The record stays in Domain, and `create` is a static extension in Storage** (`.spec-md` §5). A caller opens
  `CatalogueFolderStore` to see it.
- **No signature carries a primitive, so nothing is marked.** Names are compared inside `tryFindUnnamed`, which answers a
  `FileName option`, because a `FileName -> FileName -> bool` would be ELEV010.
- **The read and write bodies are `member private` on `FolderName`.** A private free function of one `FolderName` would be
  ELEV013P.
- **One set difference answers three questions:** a row the folder lacks, a file no row names on read, and a file the
  catalogue does not name on write.
- **A row is matched against the folder's listing before it is opened,** so a manifest row cannot reach outside the folder.
- **Every text is rendered before the first write,** so a serialization failure leaves the folder untouched. The write
  validates no schema, so it also runs where no `schema/` folder sits beside the assembly (step 35's script).

## Deferred

Nothing. Every requirement of the slice is addressed in this round: items (1) and (2)(a)(b), facts (a) to (f), and the
registry update. Whether they compile and pass is what the gates measure next.

## Gotchas

- **Nothing here has met the compiler.** A compile error, a failing fact or an ELEV finding first appears at the gate engine.
- **The prompts disagree on local checks.** "You run no checks" (`implement_contract_worker.system-md`) conflicts with "Run
  the relevant tests after every change" (AGENTS.md). This round followed the role prompt. An operator ruling would settle it
  for the remaining steps. See the impl-log's Gotchas.
- **A catalogue write cannot shrink a folder.** A catalogue that drops an entry is refused with `UnlistedEntryFileErr` while
  that entry's file remains, by the slice's rule that a write never deletes a file.
- **A failed write can leave new entry files under the old manifest.** Each file is atomic, but the set is not. A reader then
  gets a typed `ManifestMismatchErr`, never a half-written file, and writing again repairs the folder.
- **`CatalogueFolderTests.fs` reads code seeds.** The slice orders it, and slice 0053 re-points it by well-known id before
  step 58 deletes them.
- **`.contracts-json` holds CRLF in the working copy** and LF in the index. Edit it by bytes if you need to keep the working
  copy as found.
- **No fact reaches several `describe` lines** or the write's serialization-failure arm. The impl-log lists them.

## Changelog

- 2026-11-05: step 0033 added `StorageError.describe`, `CatalogueFolderStore.fs` (`writeAtomically`, `CatalogueProxy.create`)
  and `CatalogueFolderTests` with six facts, and marked `STORE_BigBoy_0001` implemented; not compiled or run in this round.
