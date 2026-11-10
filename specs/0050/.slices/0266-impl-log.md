# Step 0266 — implementation log

## Progress

- Read the task, the ASG system prompts, the project prompt, the slice, its gate snapshot and 0265's state of the world.
- Wrote the impl-plan.
- `TableAndElementRotationView.fs`:
  - added `model.runEmission`, beside `tryItemModelChoice` and `resolveSlotItem`;
  - `chartForRun` reads `runEmission` where it built the default emission, and an `Error` shows
    `The run cannot be evaluated`;
  - updated the `chartForRun` doc and the failure-chart comment.
- `ExperimentControlsTests.fs`: added the `laserDraft` fixture, the shared `laserThroughPolarizer` table and the two
  named facts.
- First solution build: exit 0, with no warning from our code. The vendored MathNet recompiled and showed its two
  existing `SYSLIB0051` warnings (`MathNetNumerics/Numerics/Exceptions.cs`, untouched), as on step 265's first build.
- The two new facts pass (2 of 2).
- **Red check against HEAD's view file** (`0266-red-check-*.log`; `cp` and `git show` only, no git mutation):
  - With HEAD's view file and the new tests, the build fails with FS0039: `runEmission` does not exist. That is the
    symbol red.
  - With HEAD's view file and the tests minus their `runEmission` lines, the build passes and both facts fail on
    behaviour:
    - `a source item emits …`: x = 0 expected 1.5, got 1 (the default source);
    - `a source item whose parameters …`: the chart still plotted (`Assert.Empty` failed).
  - Both new files were restored and checked against their SHA-256 (`0266-red-check-sha.txt`).
  - `git diff --numstat` equals `--ignore-cr-at-eol`: no CRLF churn.

- Rebuilt the solution over the final tree: 0 warnings, 0 errors. Ran each gate's own command (Testing state).

### Attempt 02 — code-judge route-back (cycle 1)

- The judge (`code_judge/0266-01-code-judge.md`) found one unmet slice instruction: the `chartForRun` doc was four
  lines and did not say the run's λ stays the bound preset's.
- Changed only the `///` doc above `let chartForRun` in `TableAndElementRotationView.fs`, now three lines. It says:
  - each captured branch's detector reads `runEmission` (the source item's own light, else the default);
  - that light passes through the path's polarizers, samples and slot items in light order, with the varied one swept;
  - λ stays the bound preset's (`boundSource`, which `withItem` binds for a source item);
  - citations: spec 0046, spec 0050 S8.1 and S8.2.
- The code, the `runEmission` doc and both tests are untouched.
- `git diff --numstat` equals `--ignore-cr-at-eol` and `file` reports LF only: no CRLF churn.
- Rebuilt and re-ran every gate's own command (Testing state, attempt 02).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs`

## Decisions

- **"The first `LightSource` slot of the table holding an item"** is read as the first light-source slot that holds
  an item (`List.tryPick`). That is the grammar of the brief and of `boundSource` ("the first light-source element
  bound to a source entry"). The default Main table has one source slot, so the two readings agree on every seeded
  table.
- **One resolution of the source's item.** `runEmission` calls step 265's `resolveSlotItem`. The kit-item lookup, the
  model resolution and both `UnknownSlotItemErr` texts therefore stay in one place, shared by `AssignSlot`, `SwapSlot`,
  the slots' items and now the source ("Reuse, never copy").
- **The source is checked before the slots' items.** `chartForRun` matches `model.runEmission, slotItems`. The source
  leads the light, so its failure is told first when both fail. With no source item, `runEmission` never fails, so
  every existing chart takes the old path unchanged.
- **No default-emission fallback on error.** A source item that cannot emit shows the failure chart and is never run
  as the preset it binds. The default emission is used only when no source slot holds an item, as the brief states.
- **λ is untouched.** `operatingWaveLength` still reads `boundSource`. `withItem` binds a source item's slot to its
  preset (step 258), so the item's λ is the preset's. The docs say so.
- **Test fixture.** The model version is `BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId`, step
  210's shipped entry over step 216's `SourcePresetChoice` payload, with `BuiltinCatalogue.source PresetIds.heNe633`.
  At runtime, `tryItemModelChoice` resolves it through `ItemModelProxy`. Nothing is manufactured.
- **Coverage beyond the named assertions, inside the two authorized facts:**
  - each fact asserts `runEmission` itself: the default emission, the item's `emissionWith` result, and the `Error`
    the chart shows;
  - the defaults fact also compares the item at its defaults with the same table without the item;
  - the second fact also covers the `UnknownSlotItemErr` arm, with a source item the kit no longer holds. Step 265 did
    the same for a slot's item.

## Testing state

No gate verdict is claimed (Invariant 6). Development runs of each gate's own command over the final tree:

| Gate | Outcome | Duration / deadline |
|---|---|---|
| build | exit 0, 0 warnings, 0 errors | 36 s / 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0 | 190 s / 600 s |
| constructor-unit-tests | `Passed!`, 1429 succeeded | 70 s / 180 s |
| ui-smoke | `Passed!`, 245 succeeded | 199 s / 600 s |
| ui-tests (six selected classes) | `Passed!`, 91 succeeded (89 at HEAD, +2) | 108 s / 360 s |
| fifteen s26 invariants | each finds nothing | — |

Both new identities are `Passed` in `TestResults/ui-tests.trx` under the census's exact names. No run exceeded its
deadline.

**Attempt 02** (after the doc fix, over the final tree; logs `0266-dev-*-attempt-02.log`):

| Gate | Outcome | Duration / deadline |
|---|---|---|
| build | exit 0, 0 warnings, 0 errors; `OpticalConstructor.Ui` recompiled | 16 s / 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0 | 193 s / 600 s |
| constructor-unit-tests | `Passed!`, 1429 succeeded | 71 s / 180 s |
| ui-smoke | `Passed!`, 245 succeeded | 209 s / 600 s |
| ui-tests (six selected classes) | `Passed!`, 91 succeeded, both new facts `Passed` in the TRX | 105 s / 360 s |
| fifteen s26 invariants | each finds nothing | — |

Elevation ran alongside constructor-unit-tests and ui-smoke. No run exceeded its deadline.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0266-dev-build.log`, `0266-dev-elevation.log`, `0266-dev-constructor-unit-tests.log`, `0266-dev-ui-smoke.log`,
  `0266-dev-ui-tests.log`, `0266-dev-invariants.log`;
- `0266-red-check-build.log`, `0266-red-check-tests.log`, `0266-red-check-sha.txt`;
- attempt 02: `0266-dev-build-attempt-02.log`, `0266-dev-elevation-attempt-02.log`,
  `0266-dev-constructor-unit-tests-attempt-02.log`, `0266-dev-ui-smoke-attempt-02.log`,
  `0266-dev-ui-tests-attempt-02.log`, `0266-dev-invariants-attempt-02.log`.

## Gotchas

- **`initMain`'s source slot `src` is unbound.** With no source item, a run reads 600 nm. `AssignSlot` with the HeNe
  item binds it to `heNe633` through `withItem`. The facts assert that binding.
- **`SetParameterState` accepts 1.5 for a fraction.** `Parameter.validate` checks only finiteness. The range is the
  evaluator's `checkedFor` (step 245), which is why the run, not the edit, refuses it.
- **A record copy needs parentheses** before a member access: `({ held with kitItems = [] }).experimentResult`.
- **The vendored MathNet recompiles** on the round's first build and shows its two existing `SYSLIB0051` warnings.
  Later incremental builds show none.
