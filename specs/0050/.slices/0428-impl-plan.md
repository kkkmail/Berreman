# Implementation plan

Implement S121-COMMITTED-KIT in the existing KitAcceptanceTests module. Reuse
buildOpticsMuellerKit, relativeFiles and assertSameBytes to compare the union
of both folders' relative file paths in sorted order, excluding only the
root .oc/ subtree. Report the built folder's absolute path, first differing
relative path and missing/extra/different-bytes reason. Close the kit in all
cases, retain failed output for inspection and delete successful run output.
Preserve the three existing facts and add exactly the authorized identity.

Generate the reference files through the existing AT-1 builder and copy its
root files and library/ snapshots without rewriting their bytes. Never copy
over data/ or .oc/. Add only the requested four-line reference-kit gotcha to
AGENTS.md. Preserve R0001's series dispatch and R0002's analyzer traversal.

The IMPLEMENT protocol's Invariant 6 reserves all gate execution to the
supervisor. The slice's bootstrap recipe calls for test execution, which
conflicts with that protocol. Use the already-built AT-1 builder directly
for the required data-generation action; do not execute or report a gate.
Record the generation provenance and leave the new comparison fact and the
frozen roster for supervisor verification. This changes the bootstrap route,
not the builder, serialized output, acceptance obligations or gate roster.

Risks are accidental lab-file writes, nondeterministic fit output, incomplete
file enumeration and removal of a failed kit before its diagnostic can be
used. No product code, format, version, gate, census or external repository
change is planned. Persistent generation artifacts belong in .artifacts/.

## Attempt 02: diagnose the measured byte mismatch

Read the supervisor's retained failed kit alongside the generated reference,
and trace differing identities back through the application messages. Keep
the exact byte comparison and all prior acceptance assertions unchanged.
Inspect saved output only; do not run gates under the IMPLEMENT protocol.

The first difference is the analyzer's persisted ElementId. AddElement calls
the private freshId function in OpticalConstructor.Ui/TableAndElementRotationView.fs,
which calls Guid.NewGuid directly. Unlike kit/item IDs and the clock, element
IDs have no provider on the model. Record the full extent of this cause using
the already-produced files and the frozen S121-COMMITTED-KIT obligation.

If completing the repair requires that production seam, report the exact
authority conflict: TableAndElementRotationView.fs is outside 0428's effects,
and R0001/R0002 grant additional edits only at 0427. Do not normalize compared
files, change persisted identities behind the messages, or recopy random
output as if it fixed repeatability. Preserve the prior implementation and
write the canonical handoffs with commit_ready=false and the needed scope.
