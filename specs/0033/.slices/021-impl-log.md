# Impl log — spec 0033, slice 021 (IMPLEMENT)

## Progress

- [x] Read task file, worker system prompt (per-anchor IMPLEMENT delta + shared
  base), project prompt, slice spec 021, slice 020 state-of-the-world.
- [x] Scouted all touch points: StackEditor.fs (StackMsg :161, applyStackMsg
  :176, groupLayers :69 — untouched), Validation.fs (validateRepeatCount :56),
  ElementId.fs (SampleStructure :103, SampleLayer :80, PeriodGroup :89,
  StackItem :96, CrystalOrientation :63, expandedFilms :112),
  MaterialLibrary.fs (MaterialId :27), RepeatBuilder.fs (expand :24), both
  fsprojs. No name clashes (SampleStackEditor / LayerPosition / MakeRepeatBlock
  unused).
- [x] Impl plan written.
- [x] Domain: SampleStackEditor.fs (state, positions, messages, typed errors,
  applySampleStackMsg) + fsproj registration.
- [x] Tests: SampleStackEditorTests.fs (every message arm + both acceptance
  criteria) + fsproj registration.
- [x] Advisory local gate runs (attempt 02 — all five gates re-run fresh,
  all green; logs in `.artifacts/021-<gate>.log`).
- [x] LF check (`git diff --numstat` = `--ignore-cr-at-eol`; both new `.fs`
  files 0 CRLF; the `.manifest.state.json` CRLF warning is the arc-runner's
  own file, left alone).
- [x] State-of-the-world.

## Files modified

- `OpticalConstructor.Domain/SampleStackEditor.fs` (NEW) — the pure,
  Avalonia-free sample-stack edit model: `LayerPosition` (`AtSingleLayer of
  itemIndex | AtCellLayer of itemIndex * cellIndex`), `SampleStackEditState`
  (`structure : SampleStructure`, `selection : Set<LayerPosition>`,
  `ofStructure`), the three-case reason-carrying `SampleStackEditError`
  (`InvalidRepeatCount | SelectionNotFoldable | NotARepeatGroup`), the
  eleven-arm `SampleStackMsg`, and
  `applySampleStackMsg : SampleStackMsg -> SampleStackEditState ->
  Result<SampleStackEditState, SampleStackEditError>` with the private
  helpers (`isValidPosition`, `positionsOfMaterial`, `mapSelected`,
  `removeSelected`, `moveWithin`/`moveSelected` pinned-frontier swaps,
  `makeRepeatBlock`, `setRepeatCount`).
- `OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` — registered
  `SampleStackEditor.fs` after `Propagation.fs` (needs only ElementId /
  MaterialLibrary, both earlier in the compile order).
- `OpticalConstructor.Tests/SampleStackEditorTests.fs` (NEW) — 30 tests
  covering every message arm without a window, both acceptance criteria, and
  the typed rejections (incl. the cross-check that
  `Ui.Validation.validateRepeatCount` enforces the same `count >= 1` rule).
- `OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — registered
  `SampleStackEditorTests.fs` last in the compile list.

## Decisions

- **`SelectLayer` ACCUMULATES; `SelectByMaterial` REPLACES.** Multi-select
  needs accumulation (MakeRepeatBlock folds a contiguous run), and the
  slice's `SelectByMaterial` → `SetThicknessOfSelected` acceptance reads as
  "the selection IS the matching set", so it replaces. `ClearSelection`
  resets. An invalid `SelectLayer` position is a no-op (the StackEditor
  out-of-range precedent, StackEditor.fs `applyStackMsg`).
- **A repetition adds no position.** `AtCellLayer (group, cell)` addresses
  the ONE cell slot; editing through it edits every repetition (the slice's
  "editing a period's cell edits every repetition"). `SetRepeatCount` leaves
  the cell untouched, so whole periods are added/removed and selections stay
  valid.
- **Uniform `Result` shape.** All eleven arms return
  `Result<state, SampleStackEditError>`; the selection/edit arms always `Ok`,
  only `MakeRepeatBlock` / `SetRepeatCount` reject — with typed,
  reason-carrying errors, restating the `count >= 1` rule of
  `Validation.validateRepeatCount` (Ui/Validation.fs:56) because Domain
  cannot reference Ui.
- **`MakeRepeatBlock` folds only a contiguous run of TOP-LEVEL single
  layers** — cell positions, gaps, or an empty selection are
  `SelectionNotFoldable`; the fold replaces the run in place with ONE
  `Repeated { cell; count }` and clears the selection (its positions no
  longer exist).
- **`RemoveSelected` on the last cell layer drops the whole group** (an
  empty cell expands to nothing — a zero-layer group is not representable
  state worth keeping).
- **Moves are per-container pinned-frontier sequential swaps** returning a
  full old→new permutation: cell selections move within their own group's
  cell (reordering every repetition), top-level selections move within
  `films` (a `Repeated` item moves as one unit), and the selection is
  remapped through the permutations so it follows the layers.
- Selection scope is the films stack only — the slice's message set has no
  substrate/lower arms (deferred with the rest of the substrate editing
  vocabulary).

## Testing state

All five roster gates pass in this round's local ADVISORY runs (the
arc-runner gate engine re-runs them authoritatively after exit):

- `build` — `dotnet build Berreman.slnx -c Release` exit 0, **0 errors**,
  91 warnings (the pre-existing MSB3277/FS1125 noise; none from the new files).
- `unit-tests` (BerremanTests) — **119 passed**, 5 skipped (pre-existing),
  0 failed (= 119 baseline; no core file touched).
- `constructor-unit-tests` — **407 passed**, 0 failed (377 baseline + 30 new
  SampleStackEditor tests).
- `ui-smoke` — **59 passed**, 0 failed (= baseline).
- `ui-tests` — **263 passed**, 0 failed (= baseline).

## Artifacts

- `specs/0033/.artifacts/021-build.log` — build gate capture.
- `specs/0033/.artifacts/021-unit-tests.log` — BerremanTests capture.
- `specs/0033/.artifacts/021-constructor-unit-tests.log` — constructor tests capture.
- `specs/0033/.artifacts/021-ui-smoke.log` — ui-smoke capture.
- `specs/0033/.artifacts/021-ui-tests.log` — ui-tests capture.

(All five re-captured fresh in attempt 02; attempt 01's build /
unit-tests / constructor-unit-tests captures showed the same green results,
its ui-smoke capture was truncated mid-run and ui-tests never ran.)

## Gotchas

- The task file's system-prompt path
  `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md` does not exist
  (same drift as slices 015–020); the real file is
  `AI-Strategy-Generator\src\ai_strategy_generator\multistep\implement_worker.system-md`.
- **Attempt 02 retry decision.** Attempt 01 exited 0 without a
  state-of-the-world: it had finished the code, tests, and fsproj
  registrations and was killed mid `ui-smoke` advisory run (its
  `021-ui-smoke.log` breaks off after "A total of 1 test files matched";
  no `021-ui-tests.log`, no SoW). The retry hint said "run the slice from
  scratch"; since the on-disk implementation was complete and its earlier
  advisory captures were green, attempt 02 reviewed the implementation
  against the slice spec (all eleven arms + both acceptance criteria
  present), re-ran ALL five gates fresh (overwriting the stale captures),
  and completed the missing round outputs — rather than deleting and
  re-typing identical code.
