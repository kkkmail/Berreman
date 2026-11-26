# Step 0050 implementation plan

Implement S17-NO-SEED-EXPERIMENTS within the three authorized source files.
Replace the two seed-dependent facts in ExperimentProxyTests with the exact
empty-proxy facts named by the slice, retaining two facts and checking ids 1
and 9999. Construct the diagnostic dark line locally with all seven fields
from the retired template; preserve its existing assertions.

Delete seedExperiments and its documentation from Experiments.fs. Keep
createInMemoryExperiments's signature and return Ok [] for listing and Ok None
for every lookup. Document the empty in-memory contract in one line citing
spec 0050 S1.7. No persisted format, proxy shape or other seed changes.

Review the source diff, the immutable acceptance obligation and line endings.
The main risks are a surviving seed reference and an accidentally changed
diagnostic fixture or test count. The IMPLEMENT worker's Invariant 6 assigns
all gates to the supervisor; run no build, tests, analyzers or invariant gates.
Write the implementation log and state of the world at their supplied paths,
identifying inherited counts separately from unmeasured gate outcomes.
