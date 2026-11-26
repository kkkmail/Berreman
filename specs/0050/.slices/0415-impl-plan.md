# Implementation plan

Implement spec 0050 S11.5's Fit step after Unknowns, reusing the selected study, `Study.prefitWith`, the shell's study-fit host, and the shared study bar and action controls. Add the prescribed model selection/calibration state, stable IDs, scrolling view and flow routing without changing storage or fit execution.

Modify only the eleven product/test paths authorized by slice 0415. Add exactly the four declared test identities and extend the six existing identities with the required model, layout, read-only, host/cancel and reachability assertions. Preserve the Unknowns repair: reserved scrollbar space and the bounded render-until-hit helper for study input.

The primary risks are retained FuncUI action closures when study/calibration changes, rendering stale hit targets, and scroll reachability on small windows. Key state-dependent action containers and reuse existing test fixtures and input helpers. Gate execution belongs to the supervisor under IMPLEMENT Invariant 6; this worker will record no gate pass claims or new measured baselines.

Attempt 02 repairs the supervisor's build diagnostics without changing acceptance. Annotate the experiment lookup/update in the new model fact. Replace the UI suite's unavailable `FitRunFileFixtures` references with one UI-local history fixture in the already-authorized `ScrollExtentTests.fs`, which compiles before `StudyViewTests.fs`. Both files call that fixture; no test-project reference, fixture copy, new test identity or gate change is needed. Preserve all existing assertions and update the mandatory handoff with verification still owned by the supervisor.
