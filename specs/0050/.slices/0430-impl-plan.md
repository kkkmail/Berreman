# Implementation plan

Implement only S121-AT1-SEND in `OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`.
Reuse `buildOpticsMuellerKit`, its app context and deterministic identities/clock,
the real archive proxy, the immediate archive host and the existing file/byte
assertions. Drive SendKit and OpenKitZip through their semantic update functions,
replaying each host's recorded progress and completion messages. Extract beneath
the fresh run folder into a separate folder whose leaf is optics-mueller.

Compare all relative paths and file bytes except `.oc/`, and inspect the zip to
prove that local state was excluded. Compose the QZ-LR residual at its parameter
layout's start vector through prefitWith, dataWith and residualWith for both
window models. Require successful, nonempty, element-for-element equal vectors.
Close both kits and remove only the builder-owned run directory in finalizers.

Preserve existing facts, R0001-R0003, product code, committed data, gate metadata
and test census. Write the canonical log and state-of-the-world handoff. The
IMPLEMENT worker's Invariant 6 reserves all build/test/gate execution and verdicts
for the supervisor; no local gate result or new baseline will be claimed.
