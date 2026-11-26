# Step 0298 — state of the world

## Where we are

Spec 0050 section S9.2 covers import profiles and the column-mapping wizard (001 §K).

- Step 291 gave experiments a mapped attachment: a kit data file, the `ImportProfileName` it is read through, and its
  rows.
- This step opens S9.2. It adds the closed sets an import profile is made of: how a lab file is laid out and what a
  scan column holds.
- Nothing reads them yet. Step 299 puts profiles in the kit file, and the wizard comes in step 309.

## What's working

- Move `ImportProfileName` (members unchanged) to directly before `NamingTemplate`. Add the built-in names
  `phoneRoiCsv` and `muellerObservations`, and `builtIns`, which lists all four in order.
- Add five closed sets to `Kit.fs`, each with a 1-based int disk tag, `tryOfWire`, `text` and `all`:
  `DataDelimiter` (with `character`), `HeaderRowChoice`, `ByteOrderMark`, `ScanColumnUnit` (with `spectralUnitOpt`) and
  `ScanReading`.
- Add `DataColumnNumber`, a lab-file column counted from 1. `tryCreate` refuses anything below 1, and `index` gives the
  0-based cell position.
- Register the five sets in `CanonicalJson.options` as int tags. An unknown tag throws `JsonException`, which callers
  report as `MalformedJsonErr`.
- `KitTests` gains `the closed sets of an import profile round-trip their disk tags and render their texts`. The
  `CanonicalJsonTests` tag facts now also cover the five sets.

## Tests

Development runs used the gates' own commands over the final build, after the mutation was restored and the solution
rebuilt. The arc-runner's gate engine runs the gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors, no `error` in stdout | 55 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded | 201 s | 600 s |
| constructor-unit-tests | Passed!, 1496 of 1496, 0 failed, 0 skipped | 70 s | 180 s |
| invariant-s30-\* (16) | each `INVARIANT HOLDS` (`section_invariants --find-nothing`, exit 0) | < 1 s each | 300 s |

- **constructor-unit-tests: 1496**, which is the 1495 baseline plus the new fact. In the TRX, the new fact and both
  changed facts (`every closed set with an int tag is written as its tag and read back`, `an unknown disk tag is
  refused`) are `Passed`.
- **Mutation check (red).** `DataDelimiter.tryOfWire 3` was made to return `None`, and the `ScanColumnUnit` converter
  was removed. The new fact then failed (`Expected: Some(TabDelimiter)`, `Actual: null`). So did the JSON fact
  (`'3' is not a DataDelimiter tag`). Both files were restored byte for byte (SHA-256 checked) and the solution was
  rebuilt before the runs above.

```yaml
gates:
  constructor_unit_tests: 1496
```

## Architecture

- **All the profile vocabulary lives in `Kit.fs`, ahead of `KitFile`.** `ImportProfileName`, the five sets and
  `DataColumnNumber` now sit between `SendExcludeGlob` and `NamingTemplate`, so step 299 can add profiles to `KitFile`.
  `DataRowKey`, `DataRowSelection` and `MappedAttachment` stay after `DataAttachment`, where step 291 put them.
- **The disk form is the step-28 `ClosedSetTagConverter`**, registered once per set before the `JsonFSharpConverter`,
  just as `KitStar` (step 166) and `ExperimentRole` are. No other option changed.
- **A spectral column's text comes from `Units.UnitOfMeasure.abbreviation`.** `Units.fs` is the one place a unit symbol
  is written, so the symbol is not copied into `Kit.fs`. Only the two angle symbols, `°` and `rad`, are written in
  `Kit.fs`, because `Units` has no angle unit.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **`ScanColumnUnit.text` reads the `Units` abbreviations.** If a `UnitOfMeasure` abbreviation changes, the column
  unit's text changes with it, and the `KitTests` fact (which pins the literal `nm`, `µm`, `eV`, `cm⁻¹`) catches it. The
  `µ` is U+00B5, the same byte sequence as in `Units.fs`.
- **`Kit.fs` reaches `UnitOfMeasure` qualified** (`Units.Nanometer`, `Units.UnitOfMeasure option`). It does not open
  `Units`.
- **`CanonicalJson.fs` and `CanonicalJsonTests.fs` now open `OpticalConstructor.Domain.Kit`.** No name collides.
  ELEV keeps type and case names unique, and every new name was checked against Berreman and STL first.
- **The new fact checks by reflection that each `all` is complete.** If a later step adds a case, it must also add the
  case to `all` and give it a tag, or this fact fails.
- **The full build shows two `SYSLIB0051` warnings** from vendored `MathNetNumerics/Numerics/Exceptions.cs`, but only
  when MathNet itself recompiles. Step 297's first full build showed the same two. They are not from this change: the
  final incremental build reported 0 warnings.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0298: S9.2's first step. `ImportProfileName` moves ahead of `KitFile` and gains the four
  `builtIns`. Five int-tagged closed sets (`DataDelimiter`, `HeaderRowChoice`, `ByteOrderMark`, `ScanColumnUnit`,
  `ScanReading`) and `DataColumnNumber` join `Kit.fs`, and `CanonicalJson.options` writes the five sets as their tags.
  constructor-unit-tests rises by one, to 1496.
