# Step 0028 — IMPLEMENT — impl plan

## Approach

A closed set's disk tag is its int (AGENTS.md "A closed set's wire/disk tag is an integer"; .spec-md §4
"Shapes"). `CanonicalJson.options` learns to write four closed sets as their `value` and to read them back
through `tryOfWire`:

- `LibraryEntryKind` (Domain, step 24);
- `TabulatedInterpolation` (Core `Dispersion.fs`, step 2);
- `PublishedIndexLaw` (Core `Dispersion.fs`, step 8);
- `PublishedGyrationLaw` (Core `Dispersion.fs`, step 10).

The four converters would have identical bodies, and AGENTS.md "Reuse, never copy" calls a duplicated
function body a defect. So one private generic `JsonConverter<'Tag>` class, written as step 23's converters
are, takes the set's tag function and its `tryOfWire`. The options register four instances of it, one per
set, before the `JsonFSharpConverter`, so they win over the serializer's case-name form:

- `Write` writes the case's int tag as a JSON number.
- `Read` accepts only a JSON number that is an `Int32` known to `tryOfWire`. Anything else (an unknown int,
  a fraction, a string such as the old case name) throws `JsonException` naming the set. The callers
  (`nodeOf`, `ProjectJson`, the entry readers to come) already report that as `MalformedJsonErr`.

The options' doc comment names the int-tagged sets. No other union changes its form: the fieldless unions
without an int tag keep the case-name form the project, groups and environment files store.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/CanonicalJson.fs`: the converter class, four
  registrations and the doc line. It gains `open Berreman.Dispersion`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CanonicalJsonTests.fs` gains two facts, under the
  exact slice ids:
  - (a) `every closed set with an int tag is written as its tag and read back`: every case of the four
    sets, enumerated exhaustively, is written as its `value` and reads back equal.
  - (b) `an unknown disk tag is refused`: `9` as a `LibraryEntryKind` and `4` as a `PublishedIndexLaw`
    throw `JsonException`.

## Risks

- A converter's primary-constructor parameters carry `int`. ELEV003P exempts members, and I expect a
  constructor to count as one; the local elevation run will confirm it. If it does not, the fallback is an
  abstract base with abstract members.
- A stored file that holds one of the four sets by case name would stop reading. None exists:
  `git grep` finds the case names in no committed `.json` and in no JSON literal in a test. The project
  schema does not constrain these values.
- The new facts' JSON literals hold no `\n`, so the shell-escape trap from step 27 does not apply. I edit
  through the Edit tool anyway.
