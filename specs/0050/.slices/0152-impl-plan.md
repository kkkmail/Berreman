# Implementation plan — 0152

Replace the named experiment collection store with the constructor's KitContext. Reuse recordExperiment, the kit deletion seam, file-backed measured data, and the existing attachment validation function. Move attachment inspection to loadKit and show persistence failures through KitStatus while preserving the previous collection and data statuses.

Apply manual/010's construction override: initWith accepts an initialized kit; scratch entry points return Result. Update App and diagnostic composition to report construction errors before opening windows, and adapt affected tests through shared success fixtures. Remove the retired collection proxy, messages, controls and composition fields while retaining the solver hand-off snapshot.

Update only the declared test identities and their necessary fixture callers. Preserve assertions for attachment validation and add the two authorized persistence facts. Manual/006 corrects the constructor retirement to six executed cases (five methods); the UI decrease remains one.

Risks are failed writes accidentally changing live state, kit lock ownership during reopen tests, and missing callers of the newly fallible initializers. The arc-runner owns build, test, elevation and invariant execution under the IMPLEMENT worker protocol; this worker runs no gates or checks. All persistent artifacts stay under specs/0050/.artifacts.

Attempt 02 repairs the three FS0041 diagnostics reported by the supervisor in InverseConstructorTests.fs. Give the existing xUnit assertions explicit string, experiment-list and status-map type arguments, preserving their operands, test identities and acceptance obligations. Retain the prior implementation and inherited baseline counts; record the repair and pending supervisor verification in the prescribed handoff files.
