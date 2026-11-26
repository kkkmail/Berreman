# Step 0065 implementation plan

Replace the live-experiment usage lookup with the existing `PinState` on each
stored material and sample version. Keep the shared version decision function,
version references, metadata editing and sample-reference removal guard. Add
idempotent one-way proxy pin verbs and reuse the material-version error text.

Update Domain stores and contracts, UI error matches and documentation, the
built-in catalogue fixture, and every affected constructor/UI test caller.
Rename the declared facts without changing their substance; replace the two
retired seam facts with the two specified pin lifecycle facts.

Risks are missed proxy record fields, accidentally pinning every version rather
than the requested version, losing pins on metadata/lifecycle edits, and changes
to the constructor case count. Preserve existing wire shapes and version tags;
no migration, dependency, configuration or version-ring change is needed.

The supervisor owns the resolved acceptance gates. The development environment
also requires appropriate local build/test verification; any such scoped checks
will be distinguished from acceptance results, with artifacts under the supplied
arc artifacts directory. No git mutations will be performed.
