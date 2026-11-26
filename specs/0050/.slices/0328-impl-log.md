# Step 0328 — impl log

## Progress

- [x] Read the protocol, project prompt, slice, step 312/323–327 context
- [x] KitWindowTests: end-to-end Data step acceptance fact (passes alone, 11 s)
- [x] AGENTS.md: Main ribbon clause + "The Data step never edits a lab file"
- [x] Mutation check of the new fact: 5 of 5 caught, sources restored byte-identical
- [x] Development runs of every gate's own command over the final tree, all green

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`:
  - the new `ui-smoke` fact `acceptance: the Data step splits a lab file across the kit's experiments, excludes a point
    and corrects a setup, all kept in the kit`;
  - its mount helper `withDispatchingKitWindow` over the record `DispatchingKitWindow`, and its lab file
    `splitPhoneRoiBytes`;
  - `sourceOffer`, `kitItemIdOf` and `tableSlotIdOf`. The last two are hoisted out of step 284's fact, which now calls
    them in place of its local `itemOf` and `slotOf`;
  - step 312's `labFileOutsideKits` now takes the file's bytes, and step 312's fact passes its `phoneRoiBytes`.
- `AGENTS.md`: the Main ribbon clause; the new gotcha after 'Lab files are read through import profiles'.

No product file changed. `OpticalConstructor.App/Program.fs` is untouched, as the slice says.

## Decisions

- **Mount.** The slice's mount is `kitWindowView` over the model, dispatching through `updateTableAndElementRotation`,
  with the shell `KitWindowShell.inert`.
  - Built as `ConstructorScene.mount` builds it: `Program.mkProgram` in a `HostWindow`, with the init command
    capturing the dispatch through `Cmd.ofEffect`. The fact can then dispatch `SplitByColumn`, `SetExclusionReason` and
    `SetCorrectionNote` itself, as the slice requires, and read the latest model.
  - `withKitWindow` uses `Program.mkSimple` and exposes no dispatch, so it could not be reused.
  - The window is 1400 × 900. The offer toggle's caption, `Create LP1-(QZ2#0)-LPd over 'LP1-(QZ2#0)-LPd' (new setup)`,
    is long, and every press is at a control's centre.
- **Items.** LP1, QZ, QZ2, Laser and LPd are added through the Items panel's messages, with step 284's
  `withOfferAdded`.
  - Laser is the HeNe 633 nm source preset, as `SetupModelTests`' Laser is, through a new `sourceOffer`.
  - All five are fixed-mounted. The slice names no dial; `SetupModelTests` dial-mounts LPd, which would only
    complicate a name that has no analyzer angle.
- **Laser stays off the table.** The slice puts only LP1, QZ and LPd on it, and the standard template names no
  `{light}`.
- **The table's sample is the Inverse constructor's seeded one** (`src`, `sample`, `det`).
  - The fact selects it as `ExperimentControlsTests` selects a slot (`selection = ElementSelected …`, the only way short
    of a pointer click), turns it with `RotSetAxis (R1, 45°)`, then adds the two polarizers. The add spreads them
    along the beam, one on either side of the sample.
  - A light-order assertion pins Source, Prep 1, Object 1, Analyzer 1 and Detector 1.
- **The AGENTS.md Main ribbon sentence.** The slice says "the sentence naming the Experiments bay's 'Data files'
  section becomes: Attach data file… lives in the Inverse constructor's Data step (DataStepView.fs), not in the
  Experiments bay; the rest of the bullet is unchanged".
  - The clause naming the section is replaced.
  - The same sentence's second clause, `a collected experiment's View opens its chart (there is no separate Open chart
    button)`, is kept, read as part of "the rest of the bullet": it says nothing about data files and would otherwise be
    lost.
- **Reuse, not copy.**
  - Step 312's lab-file writer gained a bytes parameter instead of being duplicated.
  - Step 284's local item and slot lookups became the module's `kitItemIdOf` and `tableSlotIdOf`, so the new fact calls
    the same two bodies.
- **Assertions beyond the slice's list**, each a direct read of what the slice prescribes:
  - the told split status, with its counts;
  - the created experiment's setup reference and its keyed rows;
  - the correction's id kept, and the experiment files unchanged after it;
  - the live model agreeing with the file read back;
  - the picked file outside the kit unchanged as well.

## Testing state

Development runs of each gate's own command from `0328.gates` over the final tree. The summary is in
`.artifacts/0328-dev-gates-summary.log`.

- build: exit 0, 0 warnings / 0 errors on the final run (16 s).
  - The round's first build (78 s) recompiled the vendored MathNet and printed its two known, pre-existing `SYSLIB0051`
    warnings, as in steps 320–326. MathNet is not touched here.
- elevation: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 209 s.
  - Known findings 0. The suppressed ELEV004 count is unchanged at 1453.
- constructor-unit-tests: Passed!, 1552 of 1552, 77 s.
- ui-smoke: Passed!, 258 of 258, 233 s. That is 257 + 1, the new fact; its TRX id is exact and its outcome Passed.
- ui-tests (`DataStepTests`, `KitWindowTests`, `ScrollExtentTests`): Passed!, 41 of 41, 34 s.
  - DataStepTests 9, KitWindowTests 16 and ScrollExtentTests 16.
  - KitWindowTests' non-smoke count is 16 before and after.
- invariant-s31-\* (16): each `INVARIANT HOLDS`, exit 0.
- No run exceeded its effective deadline: build 600 s, elevation 600 s, constructor 180 s, ui-smoke 600 s, ui-tests
  360 s, invariants 300 s.

Mutations (`.artifacts/0328-dev-mutation.log`, driver `.artifacts/0328-mutation-tools/mutate.py`), each caught by the
new fact:

- the offer toggle sending nothing;
- Exclude sending a blank reason;
- the matrix entry reviewing nothing;
- the correction dropping its audit trail;
- the split mapping every row instead of the group's key.

Every source was restored byte-identical, and the solution was rebuilt before the final runs.

## Artifacts

- `.artifacts/0328-dev-build.log`, `0328-dev-elevation.log`, `0328-dev-constructor-unit-tests.log`,
  `0328-dev-ui-smoke.log`, `0328-dev-ui-tests.log`, `0328-dev-invariants.log`, `0328-dev-gates-summary.log`
- `.artifacts/0328-dev-trx/` (constructor-unit-tests, ui-smoke, ui-tests TRX)
- `.artifacts/0328-dev-new-fact.log` (the fact alone), `0328-dev-mutation.log`, `0328-mutation-tools/mutate.py`

## Gotchas

- **The Inverse constructor seeds a sample slot** (`src`, `sample`, `det`); the Main constructor seeds none.
  - An added sample becomes a second object. With the turn on it and QZ in the seeded one, the template wrote
    `LP1-(QZ#0)-LPd`, which was the first red of this round.
  - Use the seeded sample.
- **`AddElement` places a new middle element at `-0.3 + 0.2 × (middle elements)` m from the source.** With the seeded
  sample, the first polarizer lands before it and the second after it.
- **The `LP1-(QZ#45)-LPd-2` group** reads through the template with the note `2`. It is offered as an experiment of that
  name over the held setup, stays unticked, and writes nothing.
- **`Darkness` reads through no template way**, because `{source}` and `{obj1}` are required, so it stays unmatched.
- **The split's told status counts the experiments written:** 1 matched and 1 created, 2 of 4 groups left unattached.
- `.manifest.state.json` was already modified when the round started. It is the supervisor's file and was not touched.
