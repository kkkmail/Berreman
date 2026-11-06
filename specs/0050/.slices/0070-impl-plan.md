# Step 0070 implementation plan

Move the category store's four refusal constructors into a CategoryError
augmentation immediately above CategoryProxy's in-memory augmentation in
MaterialLibrary.fs. Give each member the specified concrete signature and a
one-line contract comment citing spec 0050 S2.2. Replace the store's local
unknown helper and inline refusal formatting with calls to those members.

Preserve every message, quotation, sort order, reference count and branch order.
The duplicate refusal uses the existing category's identity, which is the map
key used for the lookup. The reference refusal takes EntryName values projected
from the same referencing entries. No fact, proxy shape or stored format changes.

Only MaterialLibrary.fs and the three task-supplied handoff documents change.
Preserve the supervisor's pre-existing .manifest.state.json modification.
Review the diff for message equivalence and LF preservation, then perform a
focused development build/test for the existing category behavior under the
higher-priority coding instructions. The supervisor alone executes and reports
the acceptance roster: build, elevation, constructor-unit-tests and ten s09
invariants. Carry forward checkpoint 68's 835 constructor cases as an inherited
baseline, never as a fresh measurement.
