# Step 0063 implementation plan

Implement S21-ROW-MAPPING in the three authorized source files. Reuse
EntryFileJson's fileText and parseEntry, the existing lifecycle/pin decoders,
the EF row and LibraryDatabase.run. The stored payload remains the canonical
entry file; no serializer, persisted format or schema version changes.

First add the two declared LibraryDatabaseTests facts: store every shipped
envelope with alternating lifecycle/pin states, read with a second context,
and compare payload bytes to the shipped files; reject mismatched row columns,
unknown tags and payload tampering. Then add LibraryRowInvalidErr and its
description, the two storage records, provenance projections and the two
StorageBoundary mapping members. Validate tags before parsing, propagate parser
errors, and compare every column against the regenerated row in declaration
order.

Risks are accidental normalization of noncanonical payload text, accepting a
query column unsupported by the payload, and confusing lifecycle/pin state
with content metadata. Keep comparison explicit and cover both tag precedence
and parser error propagation within the declared rejection fact.

The IMPLEMENT worker protocol reserves all build, analyzer, test and invariant
execution for the supervisor. Write the implementation log and state of the
world without claiming gate results or inventing measured baselines. Preserve
the existing supervisor edit to specs/0050/.manifest.state.json.
