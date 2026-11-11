# Step 0272 — impl plan

The window names every NEW setup by the kit's naming template (spec 0050 S8.3; 001 §J), keeping S8.2's
name for a table the template cannot name.

## Approach

`OpticalConstructor.Ui/TableAndElementRotationView.fs` opens `OpticalConstructor.Domain.NamingTemplates`
(step 268), so `NamingTemplate.setupNameAmong` (step 271) is in scope.

1. `ExpCommit`, `activeSetupOpt = None` (step 261 (8)(b)): `taken` = the names of `kitSetups`, `slots` =
   `model.recordedSlots`; the name is
   `model.kit.kitFile.namingTemplate.setupNameAmong slots model.kitItems taken (SetupName.firstFree taken)`,
   then `Setup.create name slots model.kitSetups` as before. `nextFreeAmong` keeps it free, so `create`
   refuses none.
2. `SwapSlot` (step 264): S8.2's name (`active.name.nextFreeAmong held` / `SetupName.firstFree held`)
   becomes the `unnamed` argument of `setupNameAmong swapped model.kitItems held unnamed`, and that is the
   fresh name `Setup.swapTarget` takes.
3. Nothing else: `CreateSetup` keeps the typed name; a copied setup keeps its own. No message, field or
   option is added. The template is read from `model.kit.kitFile` (step 131).

## Tests (`OpticalConstructor.Tests/SetupModelTests.fs`)

A private helper stocks the kit with LP1, QZ, Laser, LPd (`stocked`) and QZ2, adds a linear polarizer, a
sample (turned to R1 45° while the add has it selected) and a second linear polarizer, assigns LP1 / QZ /
LPd by `AssignSlot`, asserts the light order (Source, Prep 1, Obj 1, Analyzer 1, Detector 1) and commits
varying the first polarizer.

- `a commit on a table the kit's template names records it under that name` — the kit file holds
  `NamingTemplate.standard`; the kit holds `LP1-(QZ#45)-LPd.setup-json` at revision 1, E1 references it,
  `activeSetupOpt` is it.
- `a swap to content no setup holds takes the template's name of the swapped table` — after that commit,
  `SwapSlot (sample, QZ2)` writes `LP1-(QZ2#45)-LPd.setup-json`, activates it, and the bytes of
  `LP1-(QZ#45)-LPd.setup-json` are unchanged.

Existing facts keep `Setup 1` / `Bench A (2)`: their tables hold no item in Prep 1, Obj 1 or Analyzer 1,
which the standard template requires.

## Risks

- Opening `NamingTemplates` in the view adds record labels (`role`, `item`, `part`, `label`, …) to scope;
  record-construction inference elsewhere in the 5 000-line file could shift. The build will show it; the
  fix would be a type annotation at the affected site.
- ELEV: no new public signature; the two edits are inside existing arms.
