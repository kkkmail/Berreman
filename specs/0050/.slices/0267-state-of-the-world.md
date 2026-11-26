# Step 0267 — state of the world

## Where we are

This is step 0267 of spec 0050 and the first step of section S8.3 (the naming template and the grid generator). The
kit file now carries its naming template as stored text, defaulting to 001 §J's standard template. The template's
grammar (step 268), reading and writing (steps 269-271), the reference pipeline's labels (step 270) and the grid
generator follow in later steps of this section.

## What's working

- Add `NamingTemplate` to `Kit.fs`, the kit's naming template as its text. `NamingTemplate.standard` holds 001 §J's
  `{source}-({obj1}#{phi1}-{obj2}#{phi2})-{analyzer}-{note}`, the one place that text is written.
- `KitFile` carries `namingTemplate` after `sendExclude`, and `KitFile.create` sets the standard template. The
  folder-named kit and the scratch kit take it with no change.
- `kit.schema.json` (version 1) requires `namingTemplate` as a string; the shared options write it as a JSON string.
- New fact `a kit's naming template keeps its text, and a new kit takes the standard one`. The kit-file JSON facts
  check the template round-trips verbatim, and that a missing or numeric one is refused.

## Tests

No gate verdict is claimed (Invariant 6); the supervisor runs the frozen roster. The development runs below used each
gate's own command over the final tree, with logs in `specs/0050/.artifacts/0267-dev-*.log`.

- **build:** exit 0, 0 warnings, 0 errors, 3 s incremental (deadline 600 s). The red-check build before it
  recompiled the vendored MathNet and showed its two existing `SYSLIB0051` warnings
  (`MathNetNumerics/Numerics/Exceptions.cs`, untouched), as at steps 0265 and 0266.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, 210 s (deadline 600 s).
- **constructor-unit-tests:** `Passed!`, 1430 succeeded, 0 failed, 0 skipped, 75 s (deadline 180 s).
  - The count is 1429 at step 0266, so it rises by 1, as the acceptance requires.
  - `a kit's naming template keeps its text, and a new kit takes the standard one`,
    `a kit file round-trips through its canonical text` and
    `a schema-invalid or malformed kit file is refused before it binds` are `Passed` in
    `TestResults/constructor-unit-tests.trx` under the census's exact names.
- **The fifteen s27 invariants** each find nothing (`0267-dev-invariants.log`).
- **Red, measured** (`0267-red-check-*.log`):
  - against HEAD's `Kit.fs`, the tests fail to compile: FS0039 `NamingTemplate` and FS1129 `namingTemplate`;
  - with the new `Kit.fs` but HEAD's schema, both changed JSON facts fail. The schema refuses `namingTemplate` as an
    additional property, and a missing key reaches the binder instead of `SchemaValidationErr`;
  - both files were restored and verified by SHA-256.
- `git diff --numstat` equals `--ignore-cr-at-eol`: no CRLF churn. No run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1430
```

## Architecture

- `NamingTemplate` is a plain single-case union over the template's text, declared in `Kit.fs` before `KitFile`. It
  is deliberately not validated here. The grammar belongs to `NamingTemplates.fs` (step 268), and every use of a
  template answers a `Result`.
- The stored shape needs no Storage code. `KitFile` is serialized through the shared options, which unwrap the
  single-case union to a JSON string, and the canonical writer places `namingTemplate` between `name` and `notes`.
- The schema version stays 1, with no up-converter or default-on-read (.spec-md §7). Kit files of the old shape exist
  only under test output folders, and `git ls-files` holds no `.kit-json`.

## Deferred

Nothing. The template's grammar, its reading and writing, and the grid generator are separate steps of S8.3
(268-271 and later). They are not this step's scope.

## Gotchas

- Schemas are `Content` files copied into every consumer's `bin/…/schema/`. An incremental build copies them only
  when the source is newer, so after editing a schema check the copies before trusting a test run.
- `NamingTemplate`'s doc names `NamingTemplates.fs`, which step 268 creates, as the slice
  prescribes.
- A hand-built full `KitFile` record must now set `namingTemplate`. Build one through
  `{ KitFile.create … with … }`, as every test does.

## Changelog

- 2026-11-10 — step 0267: the kit file stores its naming template (`NamingTemplate`, `NamingTemplate.standard`),
  `KitFile.create` sets the standard one and `kit.schema.json` requires it; constructor-unit-tests 1429 → 1430.
