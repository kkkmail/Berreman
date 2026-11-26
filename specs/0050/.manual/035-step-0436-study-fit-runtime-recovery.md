# Step 0436: the study engine's fit in bounded time

2026-11-16. Post-mortem `.slices/0436-01.postmortem-md` and review
`.slices/0436-01.postmortem-review-md`: AT-2's joint fit through the study engine was
still in its profile pass at 7171.6 s against the step's 1800 s fit deadline and
kit-acceptance's 3600 s. One residual of AT-2's 44 configurations costs 1.31 s because
`Study.residualWith` solves its rows one after another, and every phase of the fit is
made of residuals: the main solve, each Jacobian and every profile re-fit. On top of
that a `FittedProblem` recomputes its Jacobian and report on every σ, quality and
profile read. The repair is outside 0436's effects (`KitAcceptanceTests.fs` only).

## Binding ruling

At 0436, authorize editing `Berreman/OpticalConstructor/OpticalConstructor.Domain/StudyForward.fs`
and `Berreman/OpticalConstructor/OpticalConstructor.Optimization/FittedProblems.fs`; and,
only under (3) below, `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs`
with extending these existing facts: in `OpticalConstructor.Tests/FitResultsTests.fs`,
`a cancelled profile answers ProfileCancelledErr, and a watch after earlier iterations counts on from them`;
in `OpticalConstructor.Tests/StudyEngineTests.fs`,
`a profile pass under a cancelled watch answers FitCancelledErr`. No test identity is
added, renamed or removed.

The rule: (1) a `FittedProblem` evaluates its Jacobian and its report once each,
however often its quality, σ, entry sigmas, suspects and profiles read them.
(2) `Study.residualWith` binds its runs in order and solves the rows of the bound runs
concurrently, answering the same vector, entry for entry, or the first refusal in the
order the serial walk met it. (3) Only if AT-2's fit through `runStudyFitThroughShell`
still measures above 900 s on the arc's machine, a profile pass may also run its
re-fits concurrently, its points and profiles in their planned order and its watch
told one contiguous run of counts. Every number a fit run stores is unchanged: no
solver, step, tolerance, threshold or start changes, and the optics-mueller kit still
equals the committed kit file by file. AT-2's fit ends within 900 s, measured, so
0439's second fit stays inside kit-acceptance's 3600 s; the step's 1800 s deadline is
unchanged.

Verify before commit: the 26 gates of `.slices/0436.gates` as declared, plus the
`constructor-unit-tests` gate as declared in `.slices/0426.gates`, recording AT-2's
measured fit duration. Resume normally with executability profile 1 and the operator
reply, without `--accept-current-slice`, so normal review and the frozen roster publish
measured checkpoint 435; this overrides the general committed-green acceptance rule for
this recovery only. At 0437-0494 preserve the single evaluation, the concurrent
residual, the bound and R0001-R0003; at 0460 (`StudyForward.fs`) keep the residual
concurrent; at 0476 the offset frame's move keeps one Jacobian and one report per
problem. The additional production authority is confined to 0436. No gate, floor,
deadline, census, durable format, test effect or published-spec amendment is
authorized.
