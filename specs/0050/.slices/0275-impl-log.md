# Step 0275 — impl log

## Progress

- [x] Read the protocol, the project prompt, the slice and the code it names (`Experiments.fs`, `ExperimentProxyTests.fs`).
- [x] `Experiments.commitOverSetups` directly after `commit`, with a two-line doc citing spec 0050 S8.3.
- [x] `ExperimentProxyTests`: `committing the draft over setups appends one experiment over each, and an incomplete draft appends none`.
- [x] Local build / constructor-unit-tests / elevation / the fifteen s27 invariants.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Experiments.fs` — new `commitOverSetups (references : SetupReference list) (c : ExperimentCollection) : ExperimentCollection`
  (+13 lines). Nothing else changed.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentProxyTests.fs` — the new fact (+42 lines), in the
  "committing over the active setup revision" section, after `committing references the setup revision and the varied
  slot with its label`.

## Decisions

- **The body folds `commit`.** When `c.canCommit` holds, `fresh = { c.draft with editingId = None }`, and each
  reference in order is `commit reference { collection with draft = fresh }`. `commit` then takes its append branch: it
  appends an experiment with the next id, the draft's varied element (with its label), capture, range and wavelength,
  and no data file, and moves `nextId` forward. Resetting the draft to `fresh` before each step stops the second
  reference from updating the first experiment, because `commit` leaves the draft editing what it just appended. The
  answer's draft is `fresh`, so a later `commit` appends. No second experiment builder exists (AGENTS.md "Reuse, never
  copy").
- **An incomplete draft answers `c` itself**, unchanged in every field, the draft included. That matches `commit`'s
  `None -> c` and the test's "answers the collection unchanged".
- **A committable draft with no references** returns no new experiments, and its draft edits nothing. The step does
  not name this case. It follows from "the answer's draft is the draft editing nothing", and no option was added for it.
- No other function or option was added.

## Testing state

Everything ran on the final tree with the repo's own commands. Captures are under `specs/0050/.artifacts/0275-local/`.

- build: `dotnet build Berreman.slnx -c Release -nologo -v:m` exited 0 with `0 Error(s)` in 74 s (deadline 600 s). The
  only two warnings are the vendored MathNetNumerics `SYSLIB0051` ones that were already there (as in step 0274's
  log). None came from our code.
- constructor-unit-tests, whole suite: `Test run summary: Passed!`, total 1453, succeeded 1453, failed 0, skipped 0,
  in 68 s (deadline 180 s). Checkpoint 273 had 1452, so the count rose by 1 as the step requires. The TRX shows
  `OpticalConstructor.Tests.ExperimentProxyTests.committing the draft over setups appends one experiment over each, and
  an incomplete draft appends none` as `Passed`.
- elevation, from the arc origin `f395597c071910aaa215518626563f6c784bfb62` (`.arc-base-json`):
  `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 200s` (deadline 600 s).
- invariant-s27-*: all fifteen print `INVARIANT HOLDS`, run through the ASG evaluator commands in `0275.gates`.

Red: the fact calls `commitOverSetups`. Without the production symbol it cannot compile, which counts as red under
the base protocol. In this round I wrote the production function before the test, so the red build was never
captured separately.

## Artifacts

- `specs/0050/.artifacts/0275-local/build.log`
- `specs/0050/.artifacts/0275-local/constructor-unit-tests.log`
- `specs/0050/.artifacts/0275-local/elevation.log`
- `specs/0050/.artifacts/0275-local/invariants.log`

## Gotchas

- `commit` leaves the draft EDITING the experiment it just appended. A naive `List.fold (fun c r -> commit r c)` would
  append the first experiment and then UPDATE it for every later reference, which is one experiment, not one per
  setup. Each fold step must therefore start from the draft editing nothing.
- The test's `E1` holds a data attachment (`{ e1 with dataFileOpt = Some … }`). The draft edits E1, so the "no data
  file" assertion on E2 and E3 actually tells the two apart: an update-in-place path would keep E1's file, and an
  append takes none. E1 itself is asserted unchanged, attachment included, with its own 0…360° range kept even though
  the draft's range is 0…90°.
- `over` in the test takes an `ExperimentId` instead of an `int`, so the fixture builds its ids at the literal and
  does not re-wrap a bare local (AGENTS.md "Elevate at origin … holds for test fixtures too").
