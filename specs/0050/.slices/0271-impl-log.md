# Step 0271 — impl log

## Progress

1. Read the task, both system prompts, the project prompt, the slice spec and the `.gates` snapshot. No operator note
   was in flight. Then read the code this step builds on:
   - `NamingTemplates.fs` (steps 268–269);
   - `Setups.fs` (`SlotContent.itemOpt`, `SetupSlot.inLightOrder`, `withItem`);
   - `Kit.fs` (`ItemLabel`, `ItemDraft`, `SetupName.tryCreate` / `nextFreeAmong`, `SetupError`);
   - `Placement.withR1`;
   - `SetupTests.fs` and `NamingTemplateTests.fs`.
2. Tests first. `NamingTemplateTests.fs` opens `System`, `OpticalConstructor.Domain`, `Placement` and `ItemModels`,
   and gains the slice's fixtures:
   - the slots `src`, `lpA`, `sampleA`, `sampleB`, `lpB` and `det`, made as `SetupTests` makes them (all on the main
     beam);
   - the items `Laser` (heNe633 preset), `CPL1` / `CPL2` (circular polarizer model) and `QZ` / `LR` / `QZd` (black-box
     Mueller model). All are made through `ItemDraft.addTo`; `QZd` is `DialMounted DialNegative` with its `dialZero`
     at 10°;
   - each item put in its slot by `withItem` after `withR1`;
   - the template `lab`.

   The bench lists the light first and the rest out of light order, so light order is exercised. The step adds the
   four census facts.
   - **Red**, measured in `0271-red-check-build.log`: the test project failed with FS0039 on the missing
     `SlotItem`, `nameReadingOf`, `itemsOf`, `setupNameOf` and `setupNameAmong`. The FS0656 / FS0072 / FS0001 lines
     there follow from those missing names.
3. Production code, appended to `NamingTemplates.fs` after step 269's declarations:
   - `ItemLabel.namePart`;
   - `NameText.toSetupName`;
   - `SlotItem`;
   - `NamingTemplate.nameReadingOf` / `itemsOf` / `setupNameOf` / `setupNameAmong`, as the slice describes.

   Each doc is one to three lines citing spec 0050 S8.3.
4. **Green:** the solution builds with 0 warnings and 0 errors. `NamingTemplateTests` passes 14/14, and the whole
   constructor suite passes 1444/1444.
5. Ran each gate's command over the final tree (see Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/NamingTemplates.fs` (+116)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/NamingTemplateTests.fs` (+237: four opens, the fixtures and
  four facts)

These are exactly the two edits the step authorizes. No project file changed.
`specs/0050/.manifest.state.json` was already modified when the round started (it is the supervisor's file); this
round did not touch it.

## Decisions

- **An angle needs an item, as a part does.** The slice says a missing slot, an empty slot and an item that `items`
  lacks give no value. `nameReadingOf` applies that rule to `{phi k}` as well as `{obj k}`: an object's angle is taken
  only when Obj k holds an item of `items`. `obj k` / `phi k` form one optional group in `readName` (step 269), so
  the two never disagree.
- **The note is taken only where the template has `{note}`.** That is "for each placeholder of `pieces`"; a template
  without one reads `noteOpt = None`.
- **`setupNameOf`'s blank-name error** is `MissingNameValueErr "'<template>' writes '<name>': <SetupError.describe>"`.
  It names the template and keeps the setup-name refusal's text.
- **`UnknownNameItemErr`'s reason** is `'<part>' in '<placeholder text>' of '<template>'`. This matches the form of
  `MissingNameValueErr` from step 269.
- **Fact 3, "the slots' items by role":** the light slot holds `Laser`, but `lab` has no `{light}` placeholder. So the
  expected list is the slots' `SlotItem`s in light order, restricted to the roles of `lab`'s slot placeholders. The
  restriction is computed through the file's existing `piecesOf` / `placeholdersOf` helpers. Each bench's object
  angles are written out explicitly (45/45, 45, −20 for QZd's dial reading, 22.5/45), not re-derived by the
  production rule.
- **Extra assertions inside the census facts** cover behaviour the slice prescribes; no new identity is added:
  - fact 1: an item `items` lacks gives no value, and the full expected `NameReading` with note `2`;
  - fact 4: a template that writes a blank name (`{note}`) answers `MissingNameValueErr` naming it, and
    `setupNameAmong` then gives `unnamed`.
- **Test fixtures are local** to `NamingTemplateTests`, as the slice prescribes ("test-local items"). `SetupTests`'
  slot helpers are private, and `SetupTests.fs` is outside this step's effects.
- The ordered fold in `itemsOf` is local. The shared collector `ItemEvaluation.sequenced` is typed to
  `ItemEvaluationError` and compiles after `NamingTemplates.fs`, so it cannot be reused here.

## Testing state

No gate verdict is claimed (Invariant 6). Development runs used each gate's own command over the final tree:

- **build** (`0271-dev-build.log`): exit 0, 0 warnings, 0 errors, 53 s (deadline 600 s).
- **constructor-unit-tests** (`0271-dev-constructor-unit-tests.log`): `Passed!`, 1444 succeeded, 0 failed, 0 skipped,
  70 s (deadline 180 s). That is 1440 → 1444, +4. The TRX shows all four census identities `Passed` under their exact
  names.
- **elevation** (`0271-dev-elevation.log`): `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0,
  188 s (deadline 600 s). The scope was all 12 projects (no comparison base), and Domain reports `findings=0`.
- **The fifteen s27 invariants** (`0271-dev-invariants.log`): each finds nothing.
- Both touched files are LF (`git ls-files --eol`: `i/lf w/lf`). `git diff --numstat` equals `--ignore-cr-at-eol`.

## Artifacts

- `specs/0050/.artifacts/0271-red-check-build.log`
- `specs/0050/.artifacts/0271-dev-build.log`
- `specs/0050/.artifacts/0271-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0271-dev-elevation.log`
- `specs/0050/.artifacts/0271-dev-invariants.log`
