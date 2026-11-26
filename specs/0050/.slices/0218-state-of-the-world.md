# Step 0218 — state of the world (IMPLEMENT: a kit's item files through `KitProxy`)

## Where we are

Spec 0050, section S6 (send and receive kits; copy experiments between kits), is landing the shared item
contracts. They must exist before S7, so that copying an experiment can map its items (001 §H, §S6; S7.1).

- Steps 213–216 built the item and `AddItem`.
- Step 217 gave the item its own canonical `<label>.item-json` file and schema.
- This step reads and writes a kit's items through two typed `KitProxy` members over the per-kind verbs. Per step
  127, a kind adds no proxy verb.
- Step 219 records an item through `recordItem` (snapshot, pin, then write).

## What's working

- Add `KitProxy.writeItem`. It writes an item's canonical file whole to `<label>.item-json`, and a rendering refusal
  is `KitFileRejectedErr` naming the file.
- Add `KitProxy.readItems`. It reads every item file in the label's ordinal order. A file that does not read, holds
  another label or repeats an id refuses the whole read, naming the file.
- Share one private read fold and one private render-and-write step across the experiment, setup and item readers
  and writers. The existing members' behaviour is unchanged.
- Add `KitItemFileTests`, four facts over CPL1, LP1 and Phone. Constructor unit tests: 1304 → 1308.

## Tests

These are development runs, recorded in `0218-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 warnings, 0 errors on the final source.
- **constructor-unit-tests:** 1308 passed, 0 failed, 0 skipped (+4), in 65 s against a 180 s estimate. All four
  declared identities pass:
  - `a kit's item files round-trip, each stored under its label`;
  - `writing an item again under its label replaces its file`;
  - `an item file that does not read, holds another label or repeats an id refuses the read, naming the file`;
  - `a read-only kit refuses an item write and writes nothing`.
- **The facts bite.** Five code mutations of `readItems` / `writeItem` were each caught:
  - no sort;
  - no repeated-id rule;
  - no label-stem rule;
  - a case-sensitive stem rule;
  - the wrong write stem.

  The source was then restored byte for byte.
- **elevation** (from the arc origin `f395597`, ELEV004 excluded): `ELEV-GATE: OK - 0 new finding(s) in 12
  project(s)`, 196 s against a 600 s deadline.
- **The fourteen `invariant-s22-*` gates:** each holds.
- The UI suites are not in this step's roster. No UI file or shared fixture changed.

```yaml
gates:
  constructor_unit_tests: 1308
```

## Architecture

- **Items are read and written by typed members over the per-kind verbs, as experiments and setups are** (001 §P;
  step 127; step 148).
  - `writeItem` and `readItems` call `writeKindFile` / `listKindFiles` / `readKindFile`.
  - So `readOnlyView` refuses the write (`KitReadOnlyErr`) and lets the read pass, with no item-specific guard.
- **One fold reads every per-kind collection.** `readEveryKindFile` lists, reads and parses each file, then gives it
  to the reader's `accept` together with those already read.
  - The first refusal, named by its file, answers the whole read.
  - Experiments accept by id stem, setups accept every file, and items accept by label stem and unique id.
- **One step writes every rendered content file.** `writeRenderedKindFile` maps a rendering `StorageError` to
  `KitFileRejectedErr` naming `<stem><extension>`, then writes the file whole.
- **A repeated id names both files that hold it.** Listing is ordinal, so the file read second is not always the
  newcomer.

## Deferred

Nothing in this step's scope. By the spec's plan:

- `recordItem` (snapshot and pin the item's references, then `writeItem`) is step 219;
- the reopen-against-an-empty-library resolution is step 219;
- copying an experiment with its item mapping is later in S6.

No delete or rename member exists for an item in this section, as the brief says.

## Gotchas

- **The shared fold accumulates newest-first.** Every reader on `readEveryKindFile` sorts its own answer.
- **`Other.item-json` holding CPL1's text breaks both the label rule and the id rule.** The label rule answers
  first.
- **`KitItemFileTests` uses the shared `KitContentRoundTripFixture.createContentTestKit`** (in
  `SceneRoundTripTests.fs`). Its folder is `bin/…/KitItemFileTests/<guid>`.
- **The test's `folderBytes` near-duplicates the private helpers** in `KitOpeningTests`, `KitWindowModelTests` and
  `KitStartTests`. Sharing one would mean editing files outside this step's authorized edits. Step 177's judge
  ruled the same case advisory.

## Changelog

- 2026-11-09 — step 0218: added `KitProxy.writeItem` / `KitProxy.readItems`; shared the per-kind read fold and the
  render-and-write step across the experiment, setup and item members; added four `KitItemFileTests` facts;
  constructor unit tests 1304 → 1308.
