# Step 0047 state of the world

## Where we are

Spec 0050 S1.7 now derives the Library's By kind grouping from its offered
entries. Attempt 02 repairs the supervisor-reported FS0491 in the new UI fact
by exercising the internal projection through the public rendered view.
The implementation and four declared regression facts are present. The
arc-runner owns post-repair build, test, analyzer and invariant execution;
no successful gate result is claimed here.

## What's working

- Add pure By kind grouping with ordered kind/facet groups and stable leaves.
- Derive writable-store trees from held entries, including the empty root.
- Derive live trees from offered entries with distinct ids and first-error propagation.
- Return the catalogue import bundle's own library-entry proxy.
- Add three domain facts and a headless Select-bay saved-mirror geometry fact.
- Scroll the smoke fixture's identified control into view before clicking.

## Tests

Added the exact declared facts in LibraryProxyTests and LibraryControlsTests:

- `the By kind tree groups entries by kind, then facet, then leaf`
- `the library-entry store derives its By kind tree from what it holds`
- `the live Library's By kind tree holds every live entry exactly once`
- `the Select bay lists a sample created in the samples store under its geometry`

The domain facts cover the explicit expected tree, the empty tree, source leaf
order, store insertion, complete live membership with saved sample/source ids,
uniqueness and first listing error propagation. The UI fact renders mainView
with a Sample selected and quick-pick enabled, exercising libraryState through
its product consumer. It checks the saved mirror's stable-id leaf and caption
within the Mirrors group, group/leaf indentation, and the enabled leaf on an
unbound selection. Existing test identities and acceptance remain intact.

No build, tests, analyzers or gate commands were run, per the IMPLEMENT worker
protocol. The supervisor's roster remains build, elevation,
constructor-unit-tests, ui-smoke, the selected ui-tests classes and all seven
s07 invariants. Source and diff review found no line-ending churn in the four
edited source files; this is not a gate result. The supervisor's preceding
build failed with FS0491 on the test's internal-member access. That access is
removed; the repaired code has not yet been measured by the supervisor.

These are inherited checkpoint 45 values, not measurements from this round.
ui_tests retains the established absence marker. Baselines remain owned by the
supervisor.

```yaml
gates:
  constructor_unit_tests: 802
  ui_smoke_tests: 208
  ui_tests: 0
```

## Architecture

LibraryTree.byKind is a pure member over LibraryEntry list. Sample and
polarizer facets use their DU declaration order; source facets explicitly
place fixed lines before tunable sources. Empty groups disappear, detectors
have direct leaves, and labels/ids come from the entries.

liveLibrary shares its existing entriesForKind closure with libraryTrees.
The latter traverses the five prescribed kinds through Result.bind, retains
the first listing error and deduplicates by entryId before deriving the tree.
createInMemoryEmpty derives from its mutable store on every call. Catalogue
import no longer substitutes the self-seeding Library's grouping.

No proxy or durable storage shape changed, so no migration is introduced.
The product UI consumer stays unchanged. The retry uses the existing headless
harness instead of exposing the internal libraryState member or duplicating
its projection. The test locates the tree and leaf with existing identifiers,
reads group membership from ordered rendered rows, and closes its window in
finally. The smoke-fixture adjustment uses
the existing semantic id and native BringIntoView operation because geometry
ordering places its plate below the tree viewport.

## Deferred

No implementation requirement is deferred. Gate execution and acceptance
belong to the supervisor under Invariant 6, and commit readiness is unclaimed.
The self-seeding createInMemory helper and seedTrees remain as this slice
explicitly requires until step 58.

## Gotchas

- Existing entriesForKind propagates preset errors but treats sample/source
  listing failures as empty lists; the tree reuses those semantics unchanged.
- The only new discovered test identities are the four declared by the slice.
- libraryState is internal; UI tests must reach it through mainView with the
  fixture's quick-pick threshold widened so the inline tree is rendered.
- The pre-existing specs/0050/.manifest.state.json change was left untouched.
- No versions, gate definitions, oracle, analyzer configuration, external
  repository or git state were changed by this worker.

## Changelog

- 2026-11-06: Derived By kind trees from live and held entries, removed the import override, added the four regression facts and adjusted smoke-test scrolling.
- 2026-11-06 (attempt 02): Replaced the UI fact's inaccessible libraryState call with a headless mainView assertion of the saved mirror's group, identity, caption and indentation.
