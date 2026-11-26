# Step 0037 - RENAME_CONTRACT - state of the world

## Where we are

Spec 0050, section S1.6: the catalogue import seam now lives in Domain as
`CatalogueImport`. This slice moves and renames the existing implementation;
`seedAll` still consumes the Domain seeds, as the step explicitly requires.

## What's working

- Moved the import seam into Domain as CatalogueImportProxy, CatalogueImportError and InMemoryCatalogueStores.
- Retained the four save fields, reference equality, seed order, first-error behavior and in-memory store implementation.
- Renamed both catalogue import test modules, preserving all six facts and their assertions.
- Removed the Seeding project, build output, solution entry and test project reference; updated seam documentation.

## Tests

No gates or acceptance checks were run. The RENAME_CONTRACT worker protocol
assigns verification to the supervisor after exit. These gates remain pending:

- `build`
- `elevation`
- `constructor-unit-tests` (all discovered tests, including the six renamed facts)
- `invariant-s06-elev-list-empty`
- `invariant-s06-no-vstest`
- `invariant-s06-no-kit-kit-json`
- `invariant-s06-no-eps-axis-evaluated`
- `invariant-s06-complexity-mandatory`
- `invariant-s06-no-seeded-string-preset-ids`

The count below is copied from the supervisor's step-34 checkpoint in
`.checkpoints-json`. It is historical, not a new measurement or baseline change.
This slice adds or removes no facts.

```yaml
gates:
  constructor_unit_tests: 791
```

## Architecture

`CatalogueImport.fs` compiles directly after `SampleStore.fs`. The original
functional-proxy record and its in-memory builder move together into Domain;
there is no duplicate implementation, compatibility alias or new dependency.
`ImportRejectedErr` retains its diagnostic reason. The five store-bundle fields
and both method bodies retain their existing behavior under the renamed types.
No durable data format changes.

## Deferred

Nothing within this slice's implementation scope. Verification belongs to the
supervisor under the worker protocol; no gate is waived or claimed to pass.

## Gotchas

- The rejection fact intentionally retains its original `SeedRejected` wording;
  only the reference-equality fact's name changes, as the slice specifies.
- The seed-fed `seedAll` remains intentional for this move-only step; no file
  import behavior was added.
- The deterministic recipe at `.artifacts/0037-rename-catalogue-import.ps1`
  requires the original source paths and refuses existing move destinations.
- The retired directory was resolved within the workspace and checked for
  reparse points before recursive deletion. Source writes used LF and UTF-8.
- The supervisor's pre-existing manifest modification remains untouched.
  No git staging, commit, version bump or external-repository edit was performed.

## Changelog

- 2026-11-05: Moved and renamed the catalogue import seam and six facts into their prescribed Domain/test homes, removed the Seeding project and recorded the supervisor handoff.
