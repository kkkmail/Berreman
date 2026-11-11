# Step 0299 — impl plan

## Approach

The kit file holds the lab's import profiles (001 §K, §E; S9.2). Every new type goes in
`OpticalConstructor.Domain/Kit.fs`, after `DataColumnNumber` (step 298) and before `NamingTemplate`, in the slice's
order:

1. `ScanColumn`, a record: `column`, `valueUnit`, `reading`.
2. `ReadingColumns`: `SignalColumns` (signal, σ) | `MuellerElementColumns` (m00…m33, throughput).
3. `ColumnRoles`, a record: `scan`, `reading`, and the four `…Opt` columns.
4. `ImportProfileError`: seven `… of reason : string` cases, each ending in `Err`, with `describe` in step 39's form.
5. `ImportProfile`, a record: `name`, `delimiter`, `header`, `bom`, `roles`.
6. `type ImportProfileName with member this.availableAmong`. It is an intrinsic extension, because it needs
   `ImportProfile`, which is declared after `ImportProfileName`. A built-in name gives `BuiltInProfileNameErr`, and a
   name ordinal-equal to a held profile's gives `DuplicateProfileNameErr`. Each reason names the profile.

Changes to `KitFile`:

- It gains `importProfiles : ImportProfile list` after `namingTemplate`.
- `create` sets `importProfiles = []`. That leaves `namedKitFile` and `scratchKit` unchanged.
- It gains `withImportProfile`, which is `availableAmong` and, on `Ok`, appends the profile.

The disk form goes through the shared `CanonicalJson.options`:

- The five closed sets are already int tags (step 298).
- `DataColumnNumber` unwraps to an int, and `option` becomes the value or `null`.
- `ReadingColumns` is the serializer's adjacent-tag object `{ "Case", "Fields" }`. This is the same form as
  `DataRowSelection`, so the schema says only "an object".

`kit.schema.json`, version 1's `then` block:

- `importProfiles` becomes a required key: an array of objects with exactly `name`, `delimiter`, `header`, `bom` and
  `roles`.
- `roles` has exactly `scan`, `reading` and the four `…Opt` keys.
- A local `$defs` holds `columnNumber` (an integer of at least 1) and `optionalColumnNumber` (`null` or a
  `columnNumber`), so the column rule is written once.
- `additionalProperties: false` holds at every level.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/kit.schema.json`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitTests.fs`: two new facts and a test-local `Lab scan`
  profile.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitFileJsonTests.fs`: the fixture holds `Lab scan` and
  `Polarimeter`. Two facts' expectations change.

## Risks

- **Record-label resolution.** `reading` is a label of both `ScanColumn` and `ColumnRoles`, and `name` is a label of
  both `ImportProfile` and `KitFile`. Test fixtures annotate their types.
- **Name collisions.** Every new type and case name was checked against Berreman and STL sources, with no hits.
- Old-shape kit files exist only under test output. No committed `.kit-json` exists, so the schema stays version 1.
