# Step 0072 implementation plan

Move the existing sample-store payload, query filter and refusal text into the
members specified by S2.3 and acceptance obligation S23-SAMPLE-MEMBERS. In
Domain/SampleStore.fs, expose SampleMetadata and add Sample.versionPayload above
the store. In Domain/ElementId.fs, add SampleQuery.selectFrom and the three
SampleError factories. Replace the store's local implementations with calls to
these members, deleting the old bodies in the same change.

Preserve the exact metadata fields, ordinal case-insensitive name matching,
optional substrate filter, input order, diagnostic strings and ascending pinned
version list. Keep versions elevated until the shared diagnostic formats them.
Do not change tests, storage contracts, version rings or unrelated source files.
The main risks are accidental changes to diagnostic ordering and the versioning
payload; follow the existing material-store extraction pattern.

Perform focused development verification of the affected constructor project
and existing SampleProxyTests, as required by the coding environment. Acceptance
gate execution and measured full-suite counts belong to the supervisor; do not
report local checks as gate results. Write the implementation log and final
state of the world at the supplied paths, with captured output under the supplied
artifacts folder. Preserve the pre-existing manifest state modification.
