# Step 0256 — impl log

## Progress

- Read the task file, both system prompts, the project prompt, the slice and its `.gates` snapshot. No `.manual/`
  ruling names step 0256 (016 binds 0252, 0254 and 0259–0261; 014 binds 0185–0188 and 0260). 013, which applies
  from 0160 onward, only fixes the elevation scope.
- Checked the new type, case and member names against the repo and STL: `Setups`, `SlotTurn`, `SlotContent`,
  `SetupSlot`, `SetupRevisionRecord`, `Setup`, `PlacementTurn`, `DialTurn`, `EmptySlotContent`, `ItemInSlot`,
  `slotsAt`, `heldItems`, `revisedWith`. None is taken.
- [x] `Setups.fs`, with one bare `<Compile Include="Setups.fs" />` directly after `BeamLayout.fs`.
- [x] `SetupTests.fs`, with one bare compile line appended last in the test project. It holds the census's five
  facts, names verbatim.
- [x] First build: exit 0, 0 errors. Its two warnings are MathNet's `SYSLIB0051`, from the vendored C# after a stale
  obj, as step 0255 recorded.
- [x] Constructor suite: `Passed!`, 1413 succeeded (1408 + 5). All five `SetupTests` facts are `Passed` in the TRX.
- [x] The fifteen s26 invariant greps all find nothing.
- [x] Elevation: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 204s`, known findings 0.
- [x] Incremental rebuild: 0 warnings, 0 errors. The first build compiled `Setups.fs` and `SetupTests.fs` with no
  F# warning.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Setups.fs` (new): `SlotTurn`, `SlotContent` (`itemOpt`),
  `SetupSlot` (`layoutElement`), `SetupRevisionRecord`, and `Setup` (`reference`, `slotsAt`, `heldItems`, `create`,
  `revisedWith`).
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`: one compile line.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupTests.fs` (new): `module SetupTests`, five facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: one compile line.

## Decisions

- **`Setup.create`'s clash test reuses `FileStem.firstFreeText`:** `firstFreeText name.value [ other.name.value ]`
  differs from `name.value` exactly when the two stems are equal ignoring case. This is step 0255's
  `SetupName.firstFree` pattern, so the case-insensitive stem comparison still lives in one place. The test is run
  per held setup, so the reason can name the setup it clashes with.
- **The refusal reason follows step 216's item wording** (`Item.relabelledAmong`):
  `'<name>' beside '<held>': both are stored as <held stem>.setup-json, ignoring case`.
- **`slotsAt`'s refusal** carries `{ name = this.name; revision = requested }.describe`, so revision 4 of `Bench`
  reads `'Bench' revision 4`.
- **`heldItems`** concatenates the current slots, then each history record's slots oldest first, and then applies
  `List.choose itemOpt |> List.distinct`. `distinct` keeps each item's first occurrence.
- **`revisedWith`** builds both payloads as `VersionPayload<SetupSlot list, unit>`, so a setup's slots alone decide.
  The three `VersionDecision` cases map as the slice says. The minted record holds the old revision and old slots,
  appended to `history`.
- Every record construction where labels collide is annotated (`SetupReference`, `SetupRevisionRecord`, `Setup`,
  `LayoutElement`). `Kit` and `Setups` share the `name` / `revision` / `slots` / `id` labels.
- **Opens:** the slice's five opens, plus `Berreman.Geometry` for `Angle` in `DialTurn of reading : Angle`.
  `ItemModels` is opened as the slice says. Only `SlotTurn`'s doc names `DialSign`; F# does not warn on an unused
  open.
- **Tests:** fixture ids, names and item ids are elevated where they are written: `Library.ElementId "lp"`,
  `SetupName "Bench"`, `ItemId (Guid.Parse …)`. Beyond the slice's minimum, the facts also check:
  - a slot on a reflected beam keeps that host;
  - `DialTurn` content names its item;
  - `reference`;
  - a non-clashing `Bench 2` is created beside the held setups;
  - the turned slots really differ from the originals (`withR1` respects locks, and a linear polarizer's R1 is
    free).

## Testing state

Development runs over the final tree used each gate's own command. No gate verdict is claimed (Invariant 6).

- **build:** exit 0, 0 errors, 71 s (deadline 600 s). The first build printed MathNet's two pre-existing
  `SYSLIB0051` warnings after a stale obj. The incremental rebuild: 0 warnings, 0 errors.
- **constructor-unit-tests (whole):** `Passed!`, 1413 succeeded, 0 failed, 71 s wall (deadline 180 s).
  - The baseline was 1408, so the count rose by 5.
  - All five census identities are `Passed` in `TestResults/constructor-unit-tests.trx`, names verbatim.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 204s`, 3 min 24 s wall (deadline 600 s).
  - Suppressed ELEV004 findings rose 1349 → 1352. ELEV004 is excluded from the gate but measured (R7).
  - The three new ones are `.value` renders in the refusal reasons, the same shape as `Item.relabelledAmong`.
- **The fifteen s26 invariants:** each grep finds nothing.
- `git diff --numstat` matches `--ignore-cr-at-eol`, and the two new files have no CR, so there is no CRLF churn.
  Only the four authorized files changed under `Berreman/`. `specs/0050/.manifest.state.json` was already modified
  by the supervisor before the round.
- No development run exceeded its deadline.

## Artifacts

- `specs/0050/.artifacts/0256-dev-build.log`
- `specs/0050/.artifacts/0256-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0256-dev-elevation.log`

## Gotchas

- `Setups` reuses the record labels `name`, `revision`, `slots` and `id`, which `Kit` and `BeamLayout` also use. A
  file that opens both must annotate its record constructions.
- Opens: the slice's five, plus `Berreman.Geometry` for `Angle`. `ItemModels` is kept as the slice says.
- `withR1` respects locks. A `LightSource` cannot be turned through it, because every rotation starts locked; the
  fixture turns `lp`.
