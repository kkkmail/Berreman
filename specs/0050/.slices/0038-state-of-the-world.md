# Step 0038 - IMPLEMENT - state of the world

## Where we are

Spec 0050, section S1.6: the shipped catalogue now has one Storage factory,
`CatalogueProxy.createBuiltIn ()`, shared by callers and the existing parity
tests. This completes slice 0038's implementation of S16-BUILTIN-FOLDER;
verification remains with the supervisor.

## What's working

- Added CatalogueProxy.createBuiltIn over the shipped Catalogue/Builtin folder beside the assembly.
- Routed all five built-in catalogue parity facts through the shared factory.
- Added the named location fact asserting successful reads, catalogue identity and full equality with an independent folder read.

## Tests

No gates were run or claimed to pass. The IMPLEMENT worker's invariant 6
assigns verification to the arc-runner after exit. The resolved roster remains
pending:

- `build`
- `elevation`
- `constructor-unit-tests` (all discovered, including the new fact and five parity facts)
- `invariant-s06-elev-list-empty`
- `invariant-s06-no-vstest`
- `invariant-s06-no-kit-kit-json`
- `invariant-s06-no-eps-axis-evaluated`
- `invariant-s06-complexity-mandatory`
- `invariant-s06-no-seeded-string-preset-ids`

The count below is inherited from `0037-state-of-the-world.md`, which attributes
it to the supervisor's step-34 checkpoint. It is historical, not a measurement
or baseline change in this round. This slice adds one fact and removes none.

```yaml
gates:
  constructor_unit_tests: 791
```

## Architecture

The static member extends the existing Domain proxy in Storage beside `create`
and delegates to it. The elevated folder is constructed from the fully qualified
`System.AppContext.BaseDirectory`. The existing Content item supplies the files.
There are no new data formats, dependencies, caches or fallback paths.

## Deferred

Nothing within the implementation scope. The supervisor owns the pending gate
execution under the worker protocol; no acceptance obligation is waived.

## Gotchas

- The location fact intentionally spells out the expected path independently;
  the parity reader no longer owns a path expression.
- `System.AppContext` is fully qualified in the factory because Ui owns an
  `AppContext` type too.
- Source diff review found no CRLF churn in the three edited code files.
- The pre-existing supervisor manifest modification was left untouched. No
  version bump, git mutation or external-repository edit was performed.
- `commit_ready: true` denotes completed implementation ready for supervisor
  gates under the role-specific protocol, not a claim of passing checks.

## Changelog

- 2026-11-05: Added the shipped-catalogue factory and location fact, switched parity reads to the factory and recorded the supervisor handoff.
