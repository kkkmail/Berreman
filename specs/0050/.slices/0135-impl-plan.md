# Implementation plan — 0135

Replace the experiment's absolute-path data reference with the existing
DataAttachment, preserving the measured-data parsers and validation behavior.
Extend ExperimentDataProxy with attachment and status operations, and build the
storage adapter over KitProxy. Reuse attachLabFile, attachmentStatus and
readDataFile; only reading the picked file's bytes remains direct IO.

Update the two UI composition sites, picker messages, folder memory and displayed
paths. An attachment failure must preserve the experiment and expose a typed
status. Successful attachment retains today's parse and validation statuses.

Adapt fixtures and mocks in the authorized constructor and UI test files,
preserving assertions and applying exactly the two declared renames and three
new facts. Cover outside copy, in-place attachment and unreadable picked files.
Update the prescribed comments and remove the retired path type everywhere
except the two explicitly retained Mueller history notes.

Risks are F# declaration order, shared record-shape consumers, error-channel
translation and accidental test-identity changes. Keep edits within the slice's
23 authorized files plus its three handoff documents. Preserve the supervisor's
existing manifest-state edit. No version changes or git mutations.

The IMPLEMENT worker acts only: the supervisor owns build, elevation, constructor
tests, UI smoke/tests and all eleven s16 invariants. No worker gates or measured
baseline claims will be made.

## Attempt 02 — ELEV013 repair

Apply the supervisor's retry instruction by marking ExperimentDataStore with
RequireQualifiedAccess. Preserve createFileBacked (kit : KitProxy) :
ExperimentDataProxy and its body. The ELEV013 rule explicitly sanctions public
functions in this module shape, and the existing production and test callers
already qualify the factory. Only the storage module declaration and these
three handoff documents need changes in this attempt. Leave test identities,
analyzer configuration and the empty findings ledger unchanged; the supervisor
will execute the gates.

## Attempt 03 — ELEV001 repair

Move the kit-relative-path caption conversion from SolverHandoffView to a
KitRelativePath.toControlCaption extension in ConstructorUiIdOfDomain, the
existing UI home for Domain-to-Controls conversions. Call that member from
handoffFileCaption, preserving the exact displayed path and missing-file text.
Both files are authorized by this slice. Preserve acceptance, test identities
and assertions; update only these two product files and the three handoff
documents. The supervisor owns all post-repair gate execution.
