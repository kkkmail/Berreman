# Step 0271 — state of the world

## Where we are

This is step 0271 of spec 0050, the fifth step of section S8.3 (the naming template and the grid generator).
- Step 0267 stored the kit's naming template as text.
- Step 0268 gave it a grammar.
- Step 0269 read and wrote names as text.
- Step 0270 moved the reference pipeline onto `readName`.

This step joins names to the kit's setups:
- `nameReadingOf` / `setupNameOf` write a setup's name from its slots in light order, its items under the template's
  aliases and its objects at their angles;
- `itemsOf` reads a name back into the items of its slots;
- `setupNameAmong` gives the name a new setup takes.

Steps 272 and 276 (the window and the grid) call `setupNameAmong`.

## What's working

- Add `SlotItem`, `ItemLabel.namePart` and `NameText.toSetupName` to `NamingTemplates.fs`.
- Add `NamingTemplate.nameReadingOf`. It lays the slots out in light order and takes, for each placeholder:
  - a slot's item under its first alias of that label (ignoring case), else under its label;
  - an object's dial reading, else its R1;
  - the note.
- Add `NamingTemplate.itemsOf`: each part names its item through the placeholder's alias, else by its own text,
  ignoring case. A miss answers `UnknownNameItemErr` naming the part and the placeholder.
- Add `NamingTemplate.setupNameOf`: the slots' name as a `SetupName`; a blank name answers `MissingNameValueErr`
  naming the template. Add `setupNameAmong`: that name made free among the taken names, or the given name when the
  template cannot name the slots.
- `NamingTemplateTests` gains the four census facts; constructor-unit-tests 1440 → 1444.

## Tests

No gate verdict is claimed (Invariant 6); the supervisor runs the frozen roster. Development runs used each gate's
own command over the final tree, with logs in `specs/0050/.artifacts/0271-dev-*.log`.

- **build:** exit 0, 0 warnings, 0 errors, 53 s (deadline 600 s).
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, 188 s (deadline 600 s). The
  scope was all 12 projects, since a standalone run has no comparison base.
- **constructor-unit-tests:** `Passed!`, 1444 succeeded, 0 failed, 0 skipped, 70 s (deadline 180 s).
  - The count rose by 4, as the acceptance requires.
  - The TRX shows each census identity `Passed` under its exact name:
    - `a setup's name is written from its slots in light order, its items under their aliases and its objects at
      their angles`;
    - `a name reads back into the items of its slots and their angles`;
    - `writing a setup's name and reading it back gives its items and angles`;
    - `a setup the template cannot name says which placeholder is empty, and a new setup takes the template's name
      made free, or the name it is given`.
- **The fifteen s27 invariants** each find nothing (`0271-dev-invariants.log`).
- **Red, measured** (`0271-red-check-build.log`): with the tests in place and no production code, the test project
  failed with FS0039 on `SlotItem`, `nameReadingOf`, `itemsOf`, `setupNameOf` and `setupNameAmong`.
- Both touched files are LF. No run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1444
```

## Architecture

- **A name's values come from the slots by role, not by table order.** `nameReadingOf` finds each placeholder's slot
  in `SetupSlot.inLightOrder` (step 257). A table listed in any order names the same way, as long as the light is
  first, as its root.
- **An object's angle is what the setup records:**
  - a dial-mounted item's raw reading (`DialTurn`), per 001 §I "experiments record raw dial readings";
  - otherwise the placement's R1.

  `QZd` at R1 30° on a backwards dial zeroed at 10° is written `#-20`.
- **One rule for "no value".** A missing role, an empty slot, or an item that `items` lacks gives neither a part nor
  an angle. `obj k` / `phi k` therefore stay together, matching `readName`'s grouping (step 269).
- **Aliases are per placeholder.** `CPL` reads as `CPL1` under `{source:…}` and as `CPL2` under `{analyzer:…}`. A
  part no alias owns is matched against item labels, ignoring case. Labels are unique by stem ignoring case
  (step 216), so at most one item matches.
- **Conversions run one way.** `ItemLabel.namePart` and `NameText.toSetupName` have no inverse (ELEV006). Fact 3
  turns a `SetupName` into a `NameText` at the test's own boundary.
- No other member, cache or option was added. Held setups keep their names when the template changes.

## Deferred

Nothing. The calls from the New Setup window and the grid generator (steps 272 and 276) are separate S8.3 steps by
the spec's plan, not this step's scope.

## Gotchas

- **The light is named only by `{light}`.** Neither `lab` nor `NamingTemplate.standard` has that placeholder, so a
  slot list's `Laser` never appears in its name or in `itemsOf`. Fact 3 compares only the roles the template names.
- **`setupNameAmong` returns `unnamed` unchanged.** The caller passes a name that is already free, such as S8.2's
  `SetupName.firstFree`. The template's own name is the only one made free here.
- **The blank-name error** comes only from a template whose every value is optional and absent (e.g. `{note}` with no
  note). Its reason names the template and quotes the `SetupError`.

## Changelog

- 2026-11-10 — step 0271: a setup's name is written from its slots in light order and read back into its items and
  angles. Added `nameReadingOf`, `itemsOf`, `setupNameOf`, `setupNameAmong`, `SlotItem`, `ItemLabel.namePart` and
  `NameText.toSetupName`. Constructor-unit-tests went 1440 → 1444.
