# Step 0257 — state of the world

## Where we are

This is spec 0050, section S8.2, "Setups as slots". Step 0256 made a setup into Domain data: slots, revisions and
the version rule. Step 0257 gives every slot its role on the light path, as listed in 001 §J: Source, Prep 1…k,
Obj 1…n, Analyzer 1…k, Detector.

The role is worked out from the slot's kind and from the path the one ray law (`BeamLayout.layout`) lays it on. It
is never stored. So when an element slides past a sample, its role changes, and there is still only one source of
truth.

## What's working

- Added `SlotPosition`, `SlotRole` and `RoledSlot` to `Setups.fs`. A role renders its chip name (`Source`, `Prep k`,
  `Obj k`, `Analyzer k`, `Detector k`).
- Added `SetupSlot.inLightOrder`, which reads each slot's role off its laid-out path. A light source is the source,
  a sample is an object and a detector is a detector. Any other element is an analyzer once a sample is on its
  path, including a sample's reflected ray, and a prep before that.
- Within each role, slots are numbered by light depth and then by table order, so two samples in series become
  Obj 1 and Obj 2.
- Added `Setup.lightOrder`, which gives the roles of the setup's current slots.
- Three new `SetupTests` facts cover roles in series, analyzers on a reflected ray, the no-object case where every
  element is a prep, and the chip names.

## Tests

These are development runs with each gate's own command on the final tree. They do not claim a gate verdict
(Invariant 6).

- **build:** exit 0, 0 errors, 72 s (deadline 600 s). The only warnings are the two `SYSLIB0051` warnings from the
  vendored MathNet code, which were already there.
- **constructor-unit-tests (whole):** Passed, 1416 succeeded, 0 failed, 67 s (deadline 180 s).
  - The count rose by 3 from 1413.
  - All three census identities pass, with names verbatim.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 192 s (deadline 600 s).
- **The fifteen s26 invariants:** each grep finds nothing.
- No development run exceeded its effective deadline.

```yaml
gates:
  constructor_unit_tests: 1416
```

## Architecture

- **Derived, not stored.** `SlotRole` has no `value` member and no `tryOfWire`, because it never reaches disk. Each
  time, it is computed from the slot's `CatalogueKind` and its `BeamLayout.PlacedElement.path`.
- **One ordering for all roles.** `inLightOrder` orders the zipped slots and laid-out elements once:
  - the root first;
  - then the reached slots, stably by path length, so table order breaks ties;
  - then the unreached slots, in table order.

  Each role is a filter over that one ordering, so "by light depth, then table order, unreached last" holds for
  every role at once.
- **Explicit kinds.** The prep and analyzer kinds are listed exhaustively (polarizers, mirrors, lens), so a new
  `CatalogueKind` cannot fall silently into a role.

## Deferred

- Nothing. The slice is complete as written.

## Gotchas

- **Root versus unreached slot.** The root's path is `[]`, just like the fallback `BeamLayout.layoutWith` gives an
  element it never places. "Unreached" therefore means an empty path on a slot other than the first.
- **The unreached case cannot happen today.** `effectiveHosts` sends a slot with an unusable host to the main beam,
  so every slot gets laid out. The "unreached last" rule is implemented, but no public layout can produce an
  unreached slot to test it.
- **`sampleA` and `EmitBoth`.** `EmitBoth` is already a sample's default emission. The test fixture sets it
  explicitly anyway, so the reflected-ray case does not depend on that default.

## Changelog

- 2026-11-10: step 0257: added the slot roles derived from light order (`SlotPosition`, `SlotRole`, `RoledSlot`,
  `SetupSlot.inLightOrder`, `Setup.lightOrder`) and three `SetupTests` facts. The constructor-unit-tests count went
  from 1413 to 1416.
