# Step 0272 — impl log

## Progress

- [x] Read the task, both system prompts, the project prompt, the slice, and steps 261 / 263 / 264 / 268 /
      269 / 271 / 273 for the shapes this step consumes (`setupNameAmong`, `recordedSlots`, `swapTarget`,
      `KitContext.kitFile`).
- [x] View: `open OpticalConstructor.Domain.NamingTemplates`; `ExpCommit` on a table that is no setup yet names
      the setup `namingTemplate.setupNameAmong slots model.kitItems taken (SetupName.firstFree taken)`.
- [x] View: `SwapSlot`'s fresh name is `namingTemplate.setupNameAmong swapped model.kitItems held unnamed`,
      `unnamed` being step 264's S8.2 name (`<active> (2)` / `Setup <n>`).
- [x] Tests: the two `SetupModelTests` facts and their shared fixture `templateCommitted`.
- [x] Red check: with the two arms temporarily put back to S8.2's names, both new facts failed
      (`Expected: ["LP1-(QZ#45)-LPd.setup-json"] Actual: ["Setup 1.setup-json"]`; the swap fact failed on its
      precondition the same way). Restored, rebuilt, re-ran green.
- [x] Local build / constructor-unit-tests / ui-smoke / selected ui-tests / elevation / 15 s27 invariants.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs` — the `NamingTemplates`
  open; the `ExpCommit` no-active-setup branch and the `SwapSlot` fresh name (10 +, 4 −).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupModelTests.fs` — module doc names S8.3;
  private fixture `templateCommitted`; facts `a commit on a table the kit's template names records it under
  that name` and `a swap to content no setup holds takes the template's name of the swapped table`.

## Decisions

- The template is read from `model.kit.kitFile.namingTemplate` at the moment of the commit / swap (step 131's
  `KitContext.kitFile`), so a template saved in the window (step 273) names the next setup. No cache.
- `ExpCommit` binds `slots = model.recordedSlots` once and passes the same list to the template and to
  `Setup.create`, so the name is written from exactly what is recorded (a dial-mounted item's reading included).
- In `SwapSlot`, step 264's `fresh` is renamed `unnamed` (its role as `setupNameAmong`'s fallback) and the
  template's name is the new `fresh`; `swapTarget` still decides switch-vs-create on content first, so a held
  setup holding the swapped content is switched to whatever the template would call it.
- Test fixture order: polarizer, sample, then the sample is turned to R1 45° while the add still has it
  selected (`RotSetAxis` acts on the selection), then the second polarizer; LP1 / QZ / LPd are assigned by
  `AssignSlot`. The fixture asserts the light order (Source, Prep 1, Obj 1, Analyzer 1, Detector 1 with the
  expected slot ids) and the sample's R1 so the name's provenance is pinned, not assumed.
- The tuple returned by the fixture is `model * ElementId` (two distinct types, ELEV011-clean in spirit
  although tests are outside the analyzer scope).

## Testing state

| Gate | Result | Measured | Deadline |
|---|---|---|---|
| build | pass (0 errors; only the vendored MathNet `SYSLIB0051`, pre-existing) | 73 s full, 15 s incremental | 600 s |
| constructor-unit-tests | pass — 1446 / 1446 (baseline 1444, +2) | 68 s | 180 s |
| ui-smoke | pass — 245 / 245 | 201 s (run concurrently with the constructor suite) | 600 s |
| ui-tests (5 selected classes) | pass — 76 / 76 | 103 s | 360 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)` | 202 s | 600 s |
| invariant-s27-* (15) | all pass (each git grep finds nothing) | < 1 s each | 300 s |

No gate exceeded its effective deadline.

## Artifacts

Under `specs/0050/.artifacts/`: `0272-build.log`, `0272-constructor-unit-tests.log`, `0272-red-build.log`,
`0272-red-tests.log` (the red run), `0272-ui-smoke.log`, `0272-ui-tests.log`, `0272-elevation.log`.

## Gotchas

- Opening `NamingTemplates` in the 5 000-line view brings record labels (`role`, `item`, `part`, `label`, …)
  into scope; the build showed no inference shift, so no annotation was needed.
- Existing facts keep S8.2's names because their tables hold no item in Prep 1, Obj 1 or Analyzer 1, which
  `NamingTemplate.standard` requires (step 268): step 261's `Setup 1` (a bare polarizer), step 264's
  `Bench A (2)` / `Setup 1` (source, sample, detector: no Prep slot at all), and the UI facts that find
  `Setup 1.setup-json`. All re-ran green.
- `Array.sort` of file names is ordinal, so `LP1-(QZ#45)-LPd` sorts before `LP1-(QZ2#45)-LPd` (`#` < `2`).
