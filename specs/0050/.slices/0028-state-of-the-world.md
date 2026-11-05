# Step 0028 — IMPLEMENT — state of the world

## Where we are

This is spec 0050, section S1.4: "Library entry files: envelope, provenance, canonical JSON, content
hash, schemas".
- Steps 21–23 moved the one serializer options value into `CanonicalJson` and taught it complex numbers
  and real matrices.
- Steps 24–26 gave Domain the entry vocabulary.
- Step 27 added the canonical form and the content hash.

This step makes a closed set's disk tag its int (AGENTS.md "A closed set's wire/disk tag is an integer";
.spec-md §4 "Shapes"). The four sets that carry an int tag, `value` and `tryOfWire`, are now written as
that int and read back through `tryOfWire`, so an entry file's `schema.kind`, a tabulated table's
`interpolation` and a coded preset's published law are numbers on disk. Steps 30 and 31 build on this
when they seal and parse entry files and the manifest.

## What's working

- Added the private generic `ClosedSetTagConverter<'Tag>` to `CanonicalJson`. It writes a closed set's
  `value` as a JSON number and reads it back through `tryOfWire`.
- Registered it in `CanonicalJson.options`, before the `JsonFSharpConverter`, for four sets:
  - `LibraryEntryKind` (tags 1–6);
  - `TabulatedInterpolation` (1);
  - `PublishedIndexLaw` (1–3);
  - `PublishedGyrationLaw` (1).
- Refused an unknown tag, a non-integer or the old case name with `JsonException`, which the callers report
  as `MalformedJsonErr`.
- Added two `CanonicalJsonTests` facts:
  - every case of the four sets, read off the type by reflection, is written as its tag and reads back
    equal, including `EntrySchema.kind` nested in a record;
  - `9`, `4`, `0` and a case name are refused.

  The constructor suite is 761/761.

## Tests

The arc-runner's gate engine is the gate authority. These are local observations:

- `build`: exit 0, `0 Warning(s)`, `0 Error(s)` (`.artifacts/0028-build.log`).
- `constructor-unit-tests`, whole suite: 761 total, 761 succeeded, 0 failed, 0 skipped. That is 759 plus
  the two new facts, each discovered under its slice id
  (`.artifacts/0028-constructor-unit-tests-whole.log`).
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 5 project(s)`, with 0 known findings: Storage plus its
  dependents App, TestWindows.App, TestWindows and Ui (`.artifacts/0028-elevation-local.log`).
- The six s04 invariants: each grep finds nothing.
- The red run before the converter is in `.artifacts/0028-red-run.log`.

```yaml
gates:
  constructor_unit_tests: 761
```

## Architecture

- **One converter body for every int-tagged closed set.** `ClosedSetTagConverter<'Tag>` takes the set's
  tag function and its `tryOfWire`, and `options` registers one instance per set. Four hand-written
  classes would repeat one body four times (AGENTS.md "Reuse, never copy"). A future int-tagged set is one
  registration line.
- **The refusal is the converter's.** The read accepts only a JSON number that is an `Int32` known to
  `tryOfWire`. FSharp.SystemTextJson alone accepted `9` as a `LibraryEntryKind`.
- **No other union changes form.** The fieldless unions without an int tag (`SubstrateKind`,
  `DetectorKind`, `CategoryVisibility`, ...) keep the case-name form that the project, groups and
  environment files store. The frozen-category fact (step 27) still pins that form, and its hash.

## Deferred

Nothing. Every requirement of the slice landed this round:
- the four converters, each writing `value` and reading through `tryOfWire`;
- `JsonException` on an unknown tag;
- no other union changes form;
- facts (a) and (b).

Giving int tags to the case-name unions is out of scope by the slice's own words.

## Gotchas

- **The case-name form of the four sets is refused, not read.** No committed file holds them by name
  (`git grep` over every `.json`, schema and test literal). There is no up-converter (AGENTS.md "No
  legacy, no migration").
- **Registration order matters.** The tag converters must stay ahead of the `JsonFSharpConverter`. STJ
  takes the first converter whose `CanConvert` matches, and the F# converter would write a case name.
- **Building the test project alone** shows two `SYSLIB0051` warnings from the vendored MathNet. The
  `.slnx` build the gate runs shows none.

## Changelog

- 2026-11-05: step 0028 wrote the int-tagged closed sets (`LibraryEntryKind`, `TabulatedInterpolation`,
  `PublishedIndexLaw`, `PublishedGyrationLaw`) as their tag through one generic `ClosedSetTagConverter`
  in `CanonicalJson.options`. An unknown tag is now a `JsonException`. Two `CanonicalJsonTests` facts
  were added (761/761).
