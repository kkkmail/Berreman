# Step 0378 — impl plan

## Approach

Gather, once and from the study's pre-fit (step 377), what the S11.2 residual reads (step 379 consumes it):

- each fitted run's kept, checked rows;
- each session's dark level.

All of it goes in `StudyForward.fs`, after `prefitWith`:

1. Add `StudyRun { experiment; rows }`, `SessionDark { session; dark : DarkMean }` and
   `StudyData { runs; darks }` with `member darkOf : SessionName -> DarkMean option`.
2. Add `Study.dataWith context prefits : Result<StudyData, StudyForwardError>`:
   - The first prefit whose check is `Error` answers `PrefitFailedErr (name, error)`. The checks are collected
     through `ItemEvaluation.sequenced`, the shared ordered collector, before any data is read.
   - `runs` holds the `ResidualRunUse` prefits in order, each `points.measuredRowsFor experiment.scan` (step 371).
   - Every Dark prefit's rows must each be a `RecordedSignal`. Otherwise `DarkReadingErr` names the run. A Dark run that
     keeps no point is refused the same way, because a mean over nothing is no level.
   - `darks` covers each session of `this.sessionsIn context.kit.experiments` that lists a fitted run. Its value is the
     pooled mean signal of every row of `session.darkRunsIn` over the prefits' experiments: the session's own Dark runs,
     else every Dark run the fit reads. A session with no Dark run gets no entry.
3. Add `StudyForwardError` cases `PrefitFailedErr of experiment : ExperimentName * error : PrefitError` and
   `DarkReadingErr of reason : string`, with the renders the slice gives.
4. Move `residualAgainst`'s local `kindOf` onto `RecordedReading` as `kindText`. The dark refusal names the reading's
   kind through it, so the text is reused, not copied.

## Tests (`StudyForwardTests`)

Three new facts over step 377's Malus fixture, which the facts generalise:

- the data seam's `readMapped` takes a points map;
- a context builder takes a data proxy.

The facts:

- `a study's data holds each fitted run's kept rows in study order`
- `a session's dark level is the mean signal of its own dark runs, else of every dark run, and none without a dark run`
- `a failed pre-fit check or a dark run that records no signal is refused`

The render fact `every forward error renders its condition and its payload` is widened by the two new cases.

## Risks

- **Record-label inference.** `StudyRun.experiment` and `.rows` now shadow `ExperimentPrefit.experiment` and
  `MappedAttachment.rows` for un-annotated lookups in files that open `StudyForward`. The build will show any such
  site, and the fix is a type annotation.
- **ELEV.**
  - Every new name is unique (checked by grep over Berreman and STL).
  - The `DarkMean` is born by piping the average into its constructor (ELEV001).
  - Members only, no free functions.
