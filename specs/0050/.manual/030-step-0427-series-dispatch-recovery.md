# Step 0427: automatic dispatch of independently reconstructed series

2026-11-15. Post-mortem `.slices/0427-01.postmortem-md` and review
`.slices/0427-01.postmortem-review-md`: AT-1's ObjectStage reaches the nonlinear engine
because `StagedStudy.engineWith` admits at most one unknown matrix per fitted path, and
that engine refuses the anchored session gains AT-1 requires. The linear engine already
fits lone objects from their own runs and reconstructs a series' combined matrix as a
diagnostic (`objectSolvesWith`), as `.spec-md:373` and 001 §L require.

## Binding ruling

At 0427, authorize editing `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs`
and extending these existing facts in `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs`:
`fully or partly unknown black-box matrices behind fixed instruments are linear, while a free gain or an unresolved series is nonlinear`;
`a series of two black-box objects is reconstructed as one combined matrix`;
`a fit run records its name, stamp, stage, engine, fitted runs, the data it read and the vector it used and produced`.
No test identity is added, renamed or removed.

The rule: a fitted path bearing two or more unknown matrices takes the linear engine only
when every unknown-bearing item on it is also fitted from lone runs of the same fit, so
the path's rows feed only its series diagnostic, never constituent coefficients. An
unresolved series (a constituent without lone runs), a repeated unknown item on one path
(double pass) and a coupled or linked product stay nonlinear; every existing assertion,
including the rank-deficiency refusal, is preserved. Do not merely remove the
one-matrix guard. Preserve every AT-1 acceptance requirement of 0427 unchanged.

Verify before commit: the 26 gates of `.slices/0427.gates` as declared, plus the
`constructor-unit-tests` gate as declared in `.slices/0426.gates` (its gate selects the
whole suite), because this repair edits `OpticalConstructor.Optimization` and
`OpticalConstructor.Tests`. Resume normally with executability profile 1 and the operator
reply, without `--accept-current-slice`, so normal review and the frozen roster publish
measured checkpoint 426. This overrides the general committed-green acceptance rule for
this recovery only.

At 0428-0494 preserve this dispatch behavior and its regressions; the additional repair
authority is confined to 0427. No gate, floor, deadline, census, durable format or
published-spec amendment is authorized.
