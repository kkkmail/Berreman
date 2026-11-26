# Step 0267 — impl log

## Progress

1. Read the task, both system prompts, the project prompt, the slice spec and the `.gates` snapshot. No operator
   note was in flight.
2. Surveyed the kit file: `KitFile` is serialized directly through the shared options (`KitFolderStore.fs`,
   `nodeOf this` → `canonicalFileText`), which unwrap single-case unions and sort keys. A new field needs no
   serializer code. Only `KitFile.create` builds a full record; every test builds one through `{ KitFile.create … with … }`.
   `git ls-files` holds no `.kit-json`, so requiring the key breaks no stored kit.
3. Checked `NamingTemplate` against Berreman and STL (`git grep`): no namesake type, DU case or member (ELEV014/016).
4. `Kit.fs`: added `NamingTemplate of string` (`.value`, `static member standard`) directly before `KitFile`. Added
   `namingTemplate : NamingTemplate` after `sendExclude`, and `KitFile.create` sets `NamingTemplate.standard`.
5. `kit.schema.json`: under `schemaVersion = 1`, `namingTemplate` joins `required` and `properties` as
   `{ "type": "string" }`. `additionalProperties: false` stays, and the schema version stays 1. The description now cites S8.3.
6. `KitTests`: new fact `a kit's naming template keeps its text, and a new kit takes the standard one`. It uses step 122's
   id, name, author and instant.
7. `KitFileJsonTests`: the fixture kit holds
   `NamingTemplate "{source:CPL1=CPL,LP1=LP}-({obj1}#{phi1})-{analyzer:CPL2=CPL}"`.
   - The round-trip fact lists the nine keys in order. It asserts the raw text holds
     `"namingTemplate": "<template>",` verbatim.
   - The refusal fact also removes `namingTemplate` and sets it to the number `7`. Both are refused with
     `SchemaValidationErr`.
8. Red check, then the development runs of each gate's command over the final tree (below).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/kit.schema.json`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitFileJsonTests.fs`

These are exactly the four edits the step authorizes. `specs/0050/.manifest.state.json` was already modified when
the round started (the supervisor's); it was not touched.

## Decisions

- **No serializer change.** The shared options already write a single-case union as its string. The canonical writer
  orders keys ordinally, so `namingTemplate` lands between `name` and `notes`, as the slice's key list expects.
- **The verbatim check is on the raw text.** `Assert.Contains` of
  `"namingTemplate": "{source:CPL1=CPL,LP1=LP}-({obj1}#{phi1})-{analyzer:CPL2=CPL}",` matches how the fact already
  checks `created` and `modified`. It proves the template is the string value of `namingTemplate`, written with no
  escaping.
- **Docs cite the newer spec beside the old one.** `KitFile`'s doc cites `S4.1, S8.3`, and so does the schema's
  `description`.
- **No validation, up-converter or default-on-read** (slice text; .spec-md §7).

## Testing state

No gate verdict is claimed (Invariant 6). Development runs used each gate's own command over the final tree; the
logs are in `specs/0050/.artifacts/0267-dev-*.log`.

**Red, measured before the final runs:**

1. With HEAD's `Kit.fs` (`0267-red-check-build.log`), the build fails in `OpticalConstructor.Tests` with:
   - FS0039, `NamingTemplate` is not defined (KitTests.fs:79-81);
   - FS1129, `KitFile` has no label `namingTemplate` (KitFileJsonTests.fs:30).
2. With the new `Kit.fs` and HEAD's schema (`0267-red-check-build-2.log`, `0267-red-check-tests.log`), both changed
   `KitFileJsonTests` facts fail on behaviour:
   - the round trip is refused because `additionalProperties` rejects `["namingTemplate"]`;
   - the missing-key refusal answers `Missing field … namingTemplate` instead of `SchemaValidationErr`.

   The `KitTests` fact passes, since it does not touch the schema.
3. Both files were restored and checked with `sha256sum -c` against hashes taken before the check. Every
   `bin/**/schema/kit.schema.json` copy matches the source.

| Gate | Outcome | Duration / deadline |
|---|---|---|
| build | exit 0, 0 warnings, 0 errors | 3 s (incremental) / 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0 | 210 s / 600 s |
| constructor-unit-tests | `Passed!`, 1430 succeeded, 0 failed, 0 skipped (1429 at step 0266, +1) | 75 s / 180 s |
| fifteen s27 invariants | each finds nothing (`0267-dev-invariants.log`) | — |

- All three census identities are `Passed` in `TestResults/constructor-unit-tests.trx` under their exact names.
- The red-check build showed the vendored MathNet's two existing `SYSLIB0051` warnings while MathNet recompiled
  (`MathNetNumerics/Numerics/Exceptions.cs`, untouched), as at steps 0265 and 0266. The final build showed none.
- `git diff --numstat` equals `--ignore-cr-at-eol`: no CRLF churn. No run exceeded its deadline.

## Artifacts

- `specs/0050/.artifacts/0267-red-check-build.log`, `0267-red-check-build-2.log`, `0267-red-check-tests.log`,
  `0267-red-check-sha.txt`
- `specs/0050/.artifacts/0267-dev-build.log`, `0267-dev-elevation.log`, `0267-dev-constructor-unit-tests.log`,
  `0267-dev-invariants.log`

## Gotchas

- `NamingTemplate`'s doc names `NamingTemplates.fs`, which step 268 creates. The slice prescribes it: "the grammar is
  `NamingTemplates.fs`'s (step 268)".
- Schemas are `Content` copied to every consumer's `bin/…/schema/`. After swapping a schema by hand, confirm the copies
  match before trusting a test run. An incremental build copies them only when the source is newer.
