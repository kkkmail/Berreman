# Step 0068 implementation plan

Move the existing material version payload and refusal formatting into shared
members, as required by S22-MATERIAL-MEMBERS. In MaterialStore.fs, expose
MaterialMetadata and replace the local payloadOf with MaterialEntry.versionPayload.
In MaterialLibrary.fs, add MaterialError.unknownMaterial and stillReferenced
immediately after MaterialError, and versionsPinned beside unknownMaterialVersion.
Update the in-memory store and resolveMaterialWithDisp to call those members.

Keep the payload fields, refusal wording, quoted/sorted sample names, sorted
version numbers and reference-before-pin refusal priority unchanged. Remove the
old local implementations in the same change. No test facts, proxy contracts,
serialized formats or other product files change.

Review the two-file diff and perform local development compilation and existing
material tests under the higher-priority development verification instructions.
Acceptance execution remains with the supervisor: build, elevation,
constructor-unit-tests and the ten s09 invariants in 0068.gates. Record local
evidence separately, without claiming acceptance results or a measured baseline.
