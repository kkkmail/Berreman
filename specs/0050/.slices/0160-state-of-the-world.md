# Where we are

Slice 0160, S4.4 `S44-PROJECT-CODEC`, is blocked before source implementation by
a concrete conflict with the accepted caller-owned category contract. The slice
requires moving the original fixed-enum material schema unchanged while keeping
an existing renamed-category export/validate/import regression green. These
requirements cannot both hold. Only the required worker handoff files were
written; the codec, aggregate and tests remain unchanged.

# What's working

- Identified the exact schema/test conflict and the accepted step-0046 repair
  that the literal slice would reverse.
- Preserved all production behavior, accepted test assertions and identities
  while recording the operator decision required to finish the retirement.
- Identified two documentation references omitted from the explicit edit list
  but included in the required whole-tree absence obligation.

# Tests

No build, test, analyzer, acceptance grep or invariant gate was executed. The
supervisor owns the build, elevation, all-discovered constructor-unit-tests and
twelve s18 invariant gates. No current pass/fail result is claimed.

The historical `.artifacts/0046-retry-red.log:2-4` records this exact renamed-
category export failure before the enum was removed. The step-0046 accepted
review confirms that the repaired behavior and regression were retained.
This evidence was read, not rerun.

The following is inherited from checkpoint 158, not a new measurement or an
authored baseline. No tests changed this round. Once authorized and implemented,
the slice's one migration, seven deletions and one addition imply 1136 cases,
six fewer than this inherited count; the supervisor must measure that result.

```yaml
gates:
  constructor_unit_tests: 1142
```

# Architecture

Current material metadata validation deliberately omits the obsolete fixed
category enum, while export and import resolve category membership through the
caller-supplied list (`S17-CATEGORY-LIST`). All other structure is validated.
The intended standalone schema and reuse of `SchemaValidation.validateWith`
remain appropriate, but moving the original enum unchanged contradicts that
accepted behavior. No architecture or durable shape was changed this round.

# Deferred

No work is assigned to a later slice. The entire implementation remains blocked
within step 0160 pending an operator ruling, and `commit_ready` is false.

The decision owner is the spec 0050 operator/spec owner. Resolve
`S44-PROJECT-CODEC` versus `S17-CATEGORY-LIST`: preferably authorize preserving
the effective structural schema without the enum and make the new negative
example a structural violation, retaining the existing test identity; otherwise
explicitly supersede caller-owned category behavior and its accepted assertions.
The worker has no authority to make either contract change silently.

Also authorize documentation-only retirement edits to Storage `CanonicalJson.fs`
and `EntryFileJson.fs`, which the required absence census reaches but the explicit
edit list omits. See the implementation log for exact locations and evidence.

# Gotchas

- `0160.slice-md:23` requires exact definitions and a direct `validateWith` call;
  its acceptance at line 25 also retains every other ExportImportTests fact.
- The schema category enum at `optical-constructor-project.schema.json:86`
  excludes `Glass from the caller`, which the retained 0035 category round-trip
  fact explicitly requires to export and validate (`ExportImportTests.fs:146-179`).
- Step 0046's accepted repair removes that enum in `Report.fs:59`; putting it
  back reintroduces its historically measured failure. Weakening that regression
  or special casing its data is not an authorized repair.
- `CanonicalJson.fs:128` and `EntryFileJson.fs:87` still name retired symbols in
  docs but are absent from this slice's explicit `effects.edits`.
- The supervisor-owned `.manifest.state.json` was dirty on entry and untouched.
  No source, test, schema, census, oracle, gate, checkpoint or version was changed.

# Changelog

- 2026-11-08: Documented the step-0160 category-schema authority conflict and
  omitted documentation edit scope; left production and tests unchanged.
