# Step 0257 — impl log

## Progress

- Read the task, system prompts, project prompt, slice, `Setups.fs`, `SetupTests.fs`, `BeamLayout.fs`
  (`layoutWith`, `PlacedElement.path`), `Placement.fs` (`CatalogueKind`, `defaultEmission`), `RayModel.fs`
  (default source x −1 m, detector x 1 m).
- New names (`SlotPosition`, `SlotRole`, its five cases, `RoledSlot`, `inLightOrder`, `lightOrder`) checked unique
  across `Berreman/` and STL. The only match was a test-local `inLightOrder` value in `ExperimentRunTests.fs`,
  which is not a declared name.
- `Setups.fs`, after `Setup`:
  - `SlotPosition of int` with `.value`.
  - `SlotRole`: `SourceSlotRole`, plus `PrepSlotRole`, `ObjectSlotRole`, `AnalyzerSlotRole` and `DetectorSlotRole`,
    each carrying a `SlotPosition`. Its `.text` gives the chip name (`Source`, `Prep k`, `Obj k`, `Analyzer k`,
    `Detector k`). It has no `value` member and no wire tag.
  - `RoledSlot = { role; slot }`.
  - `type SetupSlot with static member inLightOrder`.
  - `type Setup with member this.lightOrder` = `SetupSlot.inLightOrder this.slots`.
- `inLightOrder`:
  - lays the slots out with `BeamLayout.layout`, which keeps the input order, and zips each slot with its
    `PlacedElement`;
  - orders the slots once: the root, then the other slots with a non-empty path, sorted stably by path length so
    table order breaks ties, then the slots with an empty path in table order;
  - then filters that one order per role: sources, preps, objects, analyzers, detectors.
  - A `Sample` slot is an object.
  - Any polarizer, mirror or lens slot is an analyzer when a step of its path is a `Sample` slot, and a prep
    otherwise. The match on `CatalogueKind` is exhaustive with no wildcard.
- `SetupTests.fs`: the three facts named in the census, over test-local slots between x −1 m and x 1 m.
  - The fixture's `sampleA` sets `emission = EmitBoth` explicitly. That is the Sample default, but stating it shows
    where the reflected ray comes from.
  - The first fact also checks `Setup.lightOrder` on a setup made by `Setup.create`.
  - The no-object case uses the table order `[ src; lp2; det; lp1 ]`, so the order comes from light depth and not
    from the input list.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Setups.fs` (+89)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupTests.fs` (+83)

Only these two authorized files changed under `Berreman/`. The supervisor had already modified
`specs/0050/.manifest.state.json` before the round.

## Testing state

Each development run below used its gate's own command on the final tree. These runs do not claim any gate verdict
(Invariant 6).

- **build:** exit 0, 0 errors, 72 s wall (deadline 600 s).
  - The only warnings are MathNet's two existing `SYSLIB0051` warnings in `MathNetNumerics/Numerics/Exceptions.cs`.
    That is vendored code, not ours; step 0256 recorded the same pair.
  - Log: `0257-dev-build.log`.
- **constructor-unit-tests (whole):** `Test run summary: Passed!`, 1416 succeeded, 0 failed, 67 s wall (deadline
  180 s).
  - At step 0256 the count was 1413, so it rose by 3.
  - In `TestResults/constructor-unit-tests.trx` all three census identities show `Passed`, with names verbatim.
  - Log: `0257-dev-constructor-unit-tests.log`.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 192s`, run without a base (all projects),
  192 s wall (deadline 600 s).
  - Suppressed ELEV004 findings stayed at 1352.
  - Log: `0257-dev-elevation.log`.
- **The fifteen s26 invariants:** decoded from `0257.gates` and run directly. Each finds nothing.
- **Line endings:** `git diff --numstat` matches `--ignore-cr-at-eol`, so the edits added no CRLF.
- No development run exceeded its deadline.

## Artifacts

- `specs/0050/.artifacts/0257-dev-build.log`
- `specs/0050/.artifacts/0257-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0257-dev-elevation.log`

## Gotchas

- **Root versus unreached slot.** The root's laid-out path is `[]`, the same as the fallback `layoutWith` gives a
  slot it never places.
  - So "unreached" means an empty path on a slot other than the first one. The root goes first, at depth 0.
  - Every other reached slot has depth 1 or more.
- **Unreached slots never happen today.** `effectiveHosts` sends any slot whose host is unusable back to the main
  beam, and the walk visits every beam.
  - In practice every slot is placed. The unreached branch only implements the slice's rule.
  - No test can build an unreached slot through the public layout. The census names no fact for it.
- **Interpretation:** "any other slot" means the five optical kinds: linear and circular polarizers, flat and
  curved mirrors, and lenses. I spelled them out instead of using a wildcard, so a new `CatalogueKind` forces a
  decision about its role.
