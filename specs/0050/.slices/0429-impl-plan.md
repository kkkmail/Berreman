# Implementation plan

Implement S12.1's committed-kit read path in the two authorized UI test files.
Add `TestAppScope.emptyLibraryAppContext` using isolated test locations, an
empty shipped catalogue folder and the real in-memory AppContext composition.
Add exactly the declared kit-acceptance fact, opening the committed kit through
the start-screen read-only message and composing `inverseModelOver`.

Reuse the existing file inventory, lab-group reader and reference assertions.
Assert the empty stores, snapshot-backed resolution of all nine items, stored
inventory and fit runs, attachment hashes and Phone ROI mappings. Run ObjectStage
directly through StudyFitParam with the production fit environment and a fixed
clock, then compare every committed relative path and byte after closure.

Preserve R0001 series dispatch, R0002 analyzer traversal and R0003 deterministic
element identities. No product, committed data, existing test identity, gate,
version or supervisor metadata changes are authorized. Main risks are accidentally
importing the shipped catalogue, comparing only part of the folder, or invoking a
fit-run write on the read-only kit.

IMPLEMENT Invariant 6 reserves build, test and gate execution to the supervisor.
This worker implements and reviews source, records the handoff and makes no gate
verdict or new count-baseline claim.
