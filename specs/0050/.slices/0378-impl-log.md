# Step 0378 — impl log

## Progress

- [x] Production: `StudyRun`, `SessionDark`, `StudyData`, `Study.dataWith`, two new `StudyForwardError` cases,
  `RecordedReading.kindText` (2026-11-13)
- [x] Tests: three new facts and the widened render fact (2026-11-13)
- [x] Build, constructor-unit-tests, elevation, s37 invariants (2026-11-13, all green on the final source)

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/StudyForward.fs`
  - adds `PrefitFailedErr` and `DarkReadingErr` with their renders;
  - moves `residualAgainst`'s local `kindOf` onto `RecordedReading` as `kindText`;
  - adds `StudyRun`, `SessionDark` and `StudyData` with `darkOf`, and `Study.dataWith`, after `prefitWith`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyForwardTests.fs`
  - widens the render fact by the two new cases;
  - lifts step 377's LP2 turn to `lp2Turn`;
  - gives `prefitDataOver` a points map (`prefitPoints` keeps step 377's), and splits out `contextOverData`;
  - adds three facts.

## Decisions

- **"A session listing a run" means a session listing a fitted run** (one of `runs`). Step 379 asks for "the session
  of `layout.sessions` listing" a run, so only those sessions need a level. A session whose runs are all Dark gets no
  entry; the dark fact pins this with K2 alone in `Session 3`.
- **The dark level is the pooled mean of every row**, not the mean of each run's mean. The slice says "the mean of the
  signals of every row of `session.darkRunsIn`". The dark fact pins the difference: (0.2 + 0.4 + 0.9) / 3 = 0.5, not
  (0.3 + 0.9) / 2 = 0.6.
- **`darkRunsIn` reads the prefits' experiments**, as the slice says. A disabled Dark run is therefore neither a
  session's own nor one of "every Dark run". With K1 disabled, no session has a level.
- **Every Dark prefit is read once, in prefit order, before any session's mean.** So a Dark run without signals
  refuses even when no session would read it. Its refusal does not depend on session layout.
- **A Dark run that keeps no point** (every point excluded) answers `DarkReadingErr "<run> keeps no point"`. The
  slice's "each row a `RecordedSignal`" would hold vacuously for it. But a session reading only that run would then
  average nothing (`List.averageBy` throws on an empty list) or silently fall back to `darkOffset`. Refusing keeps
  errors as values. It is asserted inside the authorized fact `a failed pre-fit check or a dark run that records no
  signal is refused`; no new test identity was added.
- **Refusal order:**
  1. the first failed check, in prefit order, before any data is placed;
  2. then a fitted run's `UnpairedPointsErr`;
  3. then a Dark run's `DarkReadingErr`.

  Each walk uses the shared ordered `ItemEvaluation.sequenced`.
- **`kindOf` became `RecordedReading.kindText`**, a move from `residualAgainst`'s local. The dark refusal names a
  Ψ, Δ or Mueller row's kind through it rather than re-typing the texts. `residualAgainst`'s mismatch texts are
  unchanged; the existing mismatch fact still passes.
- **An `ObservationRow` in a Dark run** is named "a Mueller observation", since the row carries no `RecordedReading`.

## Testing state

Every gate in the 0378 roster was run locally on the final source:

- **build:** exit 0, `0 Error(s)`, no lowercase `error`. The warnings are only the exempt NU1902 advisories, plus
  vendored MathNet's SYSLIB0051 on its full recompile. 77 s full, 17 s incremental; deadline 600 s.
- **constructor-unit-tests:** `Test run summary: Passed!`, 1645 succeeded, up from 1642 by exactly 3, with 0 failed
  and 0 skipped. The TRX shows each of the three new facts and the widened render fact `Passed`. 73 s; deadline
  180 s.
- **elevation** (arc base `f395597c`, ELEV004 excluded): `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`.
  228 s; deadline 600 s. It was run twice, once before and once after a final local rename and comment rewording,
  and both runs were OK.
- **20 s37 invariants:** each prints `INVARIANT HOLDS`.
- **Line endings:** LF, confirmed with `git diff --numstat`, which matches `--ignore-cr-at-eol`.

## Artifacts

- `specs/0050/.artifacts/0378-build.log`
- `specs/0050/.artifacts/0378-constructor-unit-tests.log`, plus the TRX at
  `Berreman/OpticalConstructor/OpticalConstructor.Tests/TestResults/constructor-unit-tests.trx`
- `specs/0050/.artifacts/0378-elevation.log`

## Gotchas

- The first heredoc append of the test facts failed to parse in bash, and nothing was written. The facts were then
  appended with the Edit tool.
- `specs/0050/.manifest.state.json` was already modified by the supervisor before this round. It was not touched
  here.
- No operator note or operator ruling applies to step 0378.
