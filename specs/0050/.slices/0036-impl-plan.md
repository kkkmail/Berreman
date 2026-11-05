# Step 0036 - IMPLEMENT - plan

Delete `Berreman/OpticalConstructor/Catalogue/export-builtin.fsx`, the one-time
exporter retired by spec 0050, 001 section B and `.spec-md` sections 3.6 and 7.
Keep the committed `Catalogue/Builtin/` files, Storage content item, seed values
and five `BuiltInCatalogueParityTests` facts unchanged. Add no replacement
export path.

The only implementation change is the script deletion. Write the required
impl-log and state-of-the-world beside this slice. Preserve the pre-existing
supervisor changes to `.manifest.state.json` and `.manifest.overrides.jsonl`.

The IMPLEMENT role assigns all checks to the supervisor. Do not run the build,
tests or six s05 invariants, and do not claim their results. The supervisor
also owns staging: plain `git ls-files` continues to include a deleted tracked
file until its deletion is staged. No git mutation is authorized for this worker.
