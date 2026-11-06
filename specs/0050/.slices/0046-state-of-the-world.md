# Step 0046 state of the world

## Where we are

Spec 0050 S1.7 category resolution uses categories supplied by callers.
Attempt 04 repairs the supervisor's measured test-inventory mismatch by moving
four storage regression scenarios into related accepted facts. All assertions
and accepted test identities are preserved. The category resolution, schema
repair and elevated CategoryName result from prior attempts remain implemented.
Formal gate execution and acceptance belong to the arc-runner.

## What's working

- Resolve category ids, names and diagnostic labels through the supplied list.
- Pass live category stores into facets and editor labels without a seed fallback.
- Validate caller-supplied category labels while retaining structural constraints.
- Reject missing export categories before writing and unknown import categories.
- Preserve all four storage regressions within three accepted export/import facts.
- Restore the 802-test inventory, including both required category-list facts.

## Tests

Attempt 04 local `dotnet test -c Release` rebuilt the constructor project and
its dependencies and passed all **802 tests**, with zero failures/skips and no
emitted warnings. This is local evidence, not a formal gate result.

The TRX test-name multiset exactly matches attempt 01's 802 observations. Against
attempt 03's 806 observations, only the four unexpected identities named in the
retry hint are absent. All seven ExportImportTests fact names match HEAD. Both
facts declared in this slice's effects.tests are present and passed:

- `a category resolves by id and by name through the list it is given`
- `the category facet names each material's category through the categories it is given`

The four regression bodies remain intact in accepted facts:

- AC-I10 export validation also rejects eight malformed structures and verifies
  that a missing category leaves an existing destination unchanged.
- The 0035 category round-trip also checks a caller-renamed Glass label, structural
  validity, imported identity, CategoryId and description.
- The 0035 unknown-category import also rejects Glass against an empty supplied
  list, proving the seed catalogue cannot supply a fallback.

Current evidence is under specs/0050/.artifacts:

- `0046-attempt-04-local-constructor.log`
- `0046-attempt-04-constructor-results/constructor.trx`
- `0046-attempt-04-test-inventory.json`

Historical local evidence remains recorded in the implementation log: attempt
02 reproduced the renamed-category export failure before its production repair;
attempt 03 built the full solution with zero warnings/errors, passed 806
constructor, 208 UI smoke and 138 selected UI tests, and measured zero enforced
Domain/UI analyzer findings under the existing exclusions. Those are prior
observations, not new runs. This retry changes only constructor test organization.

The supervisor owns the resolved formal gates: build, elevation,
constructor-unit-tests, ui-smoke, selected ui-tests and the seven s07 invariants.
No gate outcome or new baseline is authored by this worker. The following values
are inherited from checkpoint 44; ui_tests remains the prior absence marker.

```yaml
gates:
  constructor_unit_tests: 800
  ui_smoke_tests: 208
  ui_tests: 0
```

## Architecture

Domain lookup is pure over a supplied MaterialCategory list. Facets share
CategoryId.categoryName; an absent id produces its GUID label as CategoryName.
UI projections obtain the live list through CategoryProxy and supply an empty
list on listing failure. LibraryWindow passes its existing category proxy to
its view context. Report.fs takes the category list at the JSON boundary.

The standalone material-entry schema removes only the fixed category enum in
memory while retaining structural constraints and internal references. Export
resolves membership through tryFindCategory before validation and writing;
import uses tryFindCategoryByName and retains its typed unknown-name error.
The published schema and durable storage shape are unchanged; no migration,
cache, alternate parser or fallback catalogue was introduced.

Attempt 03 returns CategoryName instead of the slice's literal string spelling
because the measured ELEV010 takes precedence under AGENTS.md and spec 0050
section 2. UI caption/facet conversions preserve the behavioral oracle. Attempt
04 changes no production decision; it keeps storage scenarios inside existing
facts to respect the slice's declared test identities.

## Deferred

No implementation requirement is deferred. Formal gate execution remains with
the supervisor under the worker protocol; no waiver or gate success is claimed.

## Gotchas

- Authorized edits to a test file do not authorize extra discovered test names.
  The four retry regressions now run inside the three related accepted facts.
- standardCategories remains defined until step 58, as this slice requires.
- Attempt 04 edits only ExportImportTests.fs and the three worker handoff files.
  The supervisor's existing manifest and expected-red state changes are intact.
- Modified source uses LF; whitespace and CR-insensitive diff reviews passed.
- No acceptance/oracle, gate roster, analyzer configuration, version, external
  repository or git-state mutation was made by this worker.

## Changelog

- 2026-11-05: Threaded supplied categories through lookup, facets, UI and material JSON; added the two required facts.
- 2026-11-05 (attempt 02): Reproduced and repaired renamed-category export validation, retaining structural and typed membership failures.
- 2026-11-05 (attempt 03): Returned CategoryName to repair ELEV010 while preserving label behavior and regression coverage.
- 2026-11-05 (attempt 04): Folded four extra discovery entries into accepted facts, retained every regression body, and passed the exact 802-test constructor inventory locally.
