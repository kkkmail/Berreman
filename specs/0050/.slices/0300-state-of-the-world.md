# Step 0300 — state of the world

## Where we are

Spec 0050 section S9.2 covers import profiles and the column-mapping wizard (001 §K).

- Step 298 added the closed sets an import profile is made of.
- Step 299 put kit profiles into the kit file.
- This step adds the profiles that are code, the profile an experiment's name resolves to, the points a profile maps a
  lab file to, and the wizard's one rule from column choices to roles. All of it is pure Domain data in the new
  `ImportProfiles.fs`. Nothing reads it before step 301. Step 302 reads lab files through it, and the wizard comes in
  step 309.

## What's working

- Add `ImportProfiles.fs`: the four built-in import profiles, named once, and `ChosenImportProfile`, which resolves an
  experiment's profile name to a built-in first, then to a kit profile.
- Give the observable and the sample cut int disk tags (`value` / `tryOfWire`). `Observable.tryOfCell` reads a lab
  file's `T`, `R` or tag cell.
- Add the mapped points: `ScanCell.displayValue` (degrees, or nanometres), `MappedRow`, `KeyedObservation` and
  `MappedPoints` (an X,Y series, Phone ROI capture rows, keyed observations or mapped rows), with `pointCount`.
- Add `ColumnRoles.tryOfChoices`, the one rule from the wizard's per-column choices (`ColumnRoleChoice`, m00…m33 by
  `MuellerElementIndex`) to a profile's roles, with typed errors for a missing, repeated or mixed reading.
- `ImportProfileTests` gains five facts. constructor-unit-tests rises by 5, to 1503.

## Tests

These were development runs of each gate's own command over the final build. The arc-runner's gate engine runs the
gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors, no `error` in stdout | 15 s incremental (first full build 46 s) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded | 208 s | 600 s |
| constructor-unit-tests | Passed!, 1503 of 1503, 0 failed, 0 skipped | 71 s | 180 s |
| invariant-s30-\* (16) | each `INVARIANT HOLDS` (`section_invariants --find-nothing`, exit 0) | < 1 s each | 300 s |

- **constructor-unit-tests: 1503**, which is the 1498 baseline plus the 5 new facts. Each new identity is `Passed` in
  the TRX, exactly as the slice names it:
  - `the built-in import profiles are named once, and a built-in is found before a kit profile`
  - `the observable and the cut carry disk tags, and a lab's observable cell reads T, R or the tag`
  - `a scan cell gives its value in its quantity's display unit`
  - `a column mapping's roles come from one choice per column`
  - `mapped points count what they hold`
- **The first elevation run was red** (ELEV010 on `tryOfCell (cell : string)`). It was fixed in scope; see
  Architecture. The re-run above is green.
- **Mutation check (red).** One defect per fact (kit before built-in, no trim, radians unconverted, key dropped from the
  repeated-role check, a series counted as 0) failed all five facts, each naming its defect. The file was restored byte
  for byte (SHA-256 checked) and the solution rebuilt before the runs above.

```yaml
gates:
  constructor_unit_tests: 1503
```

## Architecture

- **Deviation from the slice: `Observable.tryOfCell` takes a `DataFileCell`, not a `string`.**
  - **Conflict.** ELEV010 refuses a member signature that names our types and also carries a bare `string`. It exempts
    only the elevation-pattern names (`tryOfWire` is one; `tryOfCell` is not), and no boundary mark silences it.
    AGENTS.md rules that the analyzers win over any document (spec 0048 `.manual/002` R6).
  - **Fix.** One wrapper, `DataFileCell of string` with `.value`, next to the existing `DataFileRow` / `DataFileText`.
    The contract symbol and its reading rule are unchanged.
  - **Effect on later steps.** Step 302 will make each CSV cell a `DataFileCell` where the text arrives in Storage.
- **The extensions keep `MuellerInverse.fs` untouched.** `Observable` and `SampleCut` gain their disk tags as extension
  members in `ImportProfiles.fs`. That file compiles last in the Domain, because its points hold `MuellerRawRow` and
  `MuellerObservation`. The built-ins are named, never stored, so they have no int tag.
- **`tryOfChoices` checks the single roles first.** σ, timestamp, capture index, observable, key and throughput each
  take at most one column. It then derives the reading:
  - any element or throughput column means a Mueller reading;
  - a signal or σ beside the elements is mixed;
  - otherwise the reading is exactly one signal.

  The Mueller elements are taken in index order, and an unmapped column is read by nothing.
- **`ScanCell.displayValue` builds its `SweptValue` once, from the display number it computes.**
  - A spectral column takes its unit from `ScanColumnUnit.spectralUnitOpt` (step 298), so the mapping is not repeated.
  - Nanometres are returned as written, with no metre round trip.
  - Re-wrapping `magnitude.value` would be ELEV001, and a `UnitMagnitude.toSweptValue` would be ELEV006 against the
    existing `SweptValue.toUnitMagnitude`.

## Deferred

Nothing. Every requirement of the slice landed this round. The one deviation, the `DataFileCell` parameter, is
recorded above with its measured reason.

## Gotchas

- **`ColumnRoles` and `MappedRow` share all six record labels.** A record literal is resolved to `MappedRow` (the later
  type) unless it is annotated `: ColumnRoles`.
- **`column` labels three records**: `ScanColumn`, `ScanCell` and `ColumnChoice`. Annotate the literal.
- **FSharp.Core also has an `Observable` module.** The tests call the type's static extensions as
  `MuellerInverse.Observable.tryOfWire` / `.tryOfCell`.
- **The `Lab scan` fixture is reused, not copied.** The slice calls it a test-local profile, but `ImportProfileTests`
  reads `KitTests.labScanProfile` (steps 299/300), which has exactly the shape the slice describes. Change it in one
  place.
- **`ItemEvaluation.sequenced` is fixed to `ItemEvaluationError`.** It cannot collect `ImportProfileError`s, so the
  element check uses `List.tryPick` for the first offending index and copies no collector.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0300: the built-in import profiles and the mapped points (S9.2).
  - New `ImportProfiles.fs` adds:
    - the int disk tags of the observable and the cut;
    - `BuiltInImportProfile` and `ChosenImportProfile`;
    - `ScanCell`, `MappedRow`, `KeyedObservation` and `MappedPoints`;
    - `ColumnRoleChoice`, and `ColumnRoles.tryOfChoices`.
  - `tryOfCell` takes a `DataFileCell` (ELEV010).
  - constructor-unit-tests rises by five, to 1503.
