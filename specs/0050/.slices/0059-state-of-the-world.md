# Step 0059 state of the world

## Where we are

Step 0059 retires STORE_XDUO_0007, the EF Core wiring proof, at the start of
S2.1. The requested deletions and reference cleanup are implemented. The
Database project remains in the solution with its existing properties and
SQLite dependency. Attempt 02 preserves the reviewed implementation and is
blocked by a measured gate-credit authority conflict: the required removal of
seven executed cases yields 794, but four method-level deletion credits produce
a gate floor of 797. Resolving that accounting is outside this slice's effects.

## What's working

- Removed the Test entity, TestDbContext and all initial-migration files.
- Deleted the proof's seven test cases and database settings fixture.
- Pruned proof-only test references and obsolete project documentation.
- Retired unused provider and MSSQL keys; retained and documented the SQLite key.
- Diagnosed the method-versus-case count conflict without changing tests or gates.

## Tests

No build, tests, analyzers, acceptance searches or invariant gates were run by
this worker, per RETIRE_CONTRACT Invariant 6. Existing supervisor evidence in
`.manifest-arc-runner.log` reports build and elevation passing, followed by a
constructor-unit-tests gate failure on 2026-11-06 at 13:55:47Z. The test process
itself exited zero: 794 succeeded, zero failed and zero skipped; the existing
TRX agrees. The gate rejected `794 < baseline 797`.

The selected AppContextTests/LauncherTests UI gate and nine s08 invariant gates
were not reached after the failure. Their results remain unestablished; no new
gate result or commit readiness is asserted by this worker.

The retired class has four theory rows and three facts. Checkpoint 57 records
801, and immutable S21-RETIRE-EF-PROOF explicitly requires seven fewer, or 794.
The repository inventory has method granularity: four deleted method identities
earn four credits in ASG's `authorized_count_credits`, yielding 797 instead.
The measured 794 agrees with the required retirement. The YAML below preserves
the inherited checkpoint, not a replacement floor or a successful measurement
for this slice. No ui_tests checkpoint is stored; zero remains the previous
handoff's absence marker. The supervisor owns any future measured checkpoint.

```yaml
gates:
  constructor_unit_tests: 801
  ui_tests: 0
```

## Architecture

The obsolete proof and its only consumer are removed together, with no
compatibility shim, migration replacement or new behavior. The C# Database
project intentionally has no handwritten source in this step. Its SQLite
provider dependency is preserved. The constructor tests retain their existing
Domain, Storage, Optimization and Ui references; their direct App and Database
references and direct SQLite package were specific to the removed proof.

ConfigKey.sqliteConnectionString retains the OpticalConstructorSqlite wire key
for the library database. Other keys, defaults and appsettings.json are
unchanged. Durable proof storage is retired under the unreleased-product
compatibility break specified by spec 0050 §7; no migration path is added.

## Deferred

No slice implementation work is deferred. Verification is the supervisor's
responsibility and is blocked by the gate-credit authority conflict. A
supervisor/operator resolution is required before this slice can be completed;
this is not a waiver or work assigned to a later product slice.

## Gotchas

- No target-repository Roslyn retirement command was available; the protocol's
  manual fallback applied exact deletions and narrow text edits.
- S21-RETIRE-EF-PROOF requires exactly seven fewer cases and says nothing
  replaces the deleted facts. Raising the suite from 794 to 797 would violate
  that acceptance, so no product/test repair is authorized for this failure.
- ASG's `executability/runtime_tests.py:80` credits one per deleted method and
  explicitly documents that this is short for a theory. Correcting the credit
  machinery or its accepted inputs is outside the ten allowed edit paths; ASG
  is read-only under the project prompt. See the impl-log's Authority conflict
  section for the measured evidence and missing authority.
- No replacement facts, gate changes, checkpoint adjustments, spec/oracle edits
  or ASG/STL modifications were made. Attempt 02 only updates the three supplied
  worker Markdown outputs.
- The Database project's properties, NU1903 comment and SQLite reference stay.
- The supervisor's dirty `.manifest.state.json` and
  `.expected-red-observed-json` were left untouched during attempt 02.
- Commit readiness is false until the authority conflict is resolved and all
  required supervisor gates complete successfully.

## Changelog

- 2026-11-06, step 0059 attempt 01: retired the EF wiring proof, its seven test cases and fixture, pruned references and unused settings, and wrote the supervisor handoff.
- 2026-11-06, step 0059 attempt 02: preserved the retirement, traced the 794/797 failure to four method credits for seven cases, and documented the blocking authority conflict.
