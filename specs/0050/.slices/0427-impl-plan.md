# Slice 0427 implementation plan

Extend only `KitAcceptanceTests.fs` for S121-AT1-STUDY. Continue the existing
message-driven AT-1 builder after its experiment checkpoint: create QZ-LR with
the 53 experiments in kit order, choose StagedFit and the six reference session
gain models, run calibration through StudyFitJobHost.posting, publish it, then
run object reconstruction. Inject the existing test clock into both runs and
replay queued progress/completion messages after a bounded completion event.

Add the single authorized staged-study fact and private reference assertion
helpers. Reopen the built folder through a second KitProxy and inspect the
stored study, ordered fit runs, calibration provenance/sigmas and revisions,
then compare the five instrument angles, all three matrices and the product
RMSE ratio with MuellerReference. Preserve the setup and experiment facts at
their pre-publication checkpoints with all existing assertions intact.

Risks are deterministic clock propagation, asynchronous completion ordering,
the source fast-axis offset convention and accidentally weakening earlier
checkpoint coverage. Reuse the existing window fit-host pattern, domain gain
offers, angle accessors and shared reference tolerance. No production, gate,
oracle, census, version, raw data or external-repository changes are planned.

Per implement_worker.system-md Invariant 6, the supervisor owns all build,
test, analyzer and invariant execution. This worker implements and records the
handoff without running gates or manufacturing measured baselines.

Attempt 02 repairs the supervisor-reported strict-indentation error in
`runAcceptanceFit`'s nested record update. Review the added staged-study code
against its existing type declarations and preserve every assertion and the
single authorized test identity. Record the measured supervisor failure
separately from the pending verification of this repair.

## Attempt 03: measured acceptance failure and authority boundary

Trace the supplied stage-two failure through the real RunFit dispatch and the
fixture's unknowns before changing the test. Preserve all three facts, their
assertions, the 53 experiments, both unknown object matrices and six gain rules.
If the cause lies outside KitAcceptanceTests.fs, record the exact production
owner and conflicting acceptance obligations rather than bypassing the engine.

Source inspection identifies that boundary: StudyEngine.fs's engineWith rejects
every fitted path with more than one unknown-bearing matrix. AT-1 requires both
QZ and LR unknown on its series paths, so RunFit dispatches to fitNonlinear,
which rejects the required anchored gains. The existing reconstruction test
calls reconstructLinear directly and therefore does not cover this dispatch.

The repair needs operator authority for StudyEngine.fs and existing coverage in
StudyEngineTests.fs (steps 0386/0390/0391), reconciling S113-ENGINE-CHOICE with
S121-AT1-STUDY: independently reconstructed series diagnostics must remain
separate from genuinely nonlinear products of unresolved unknown matrices.
Write the blocked handoff; do not edit production, tests, gates or the oracle
under this slice's test-file-only authorization.
