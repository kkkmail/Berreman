# Step 0045 implementation plan

Extend the existing BuiltinCatalogue fixture with the six S1.7 helpers. Build
each store from its existing empty in-memory implementation, fill it through
its own write verb in catalogue row order, and report the refused entry and
error on failure. Reuse the existing lazy catalogue load and decoded entry
helpers; retain no store state between calls. The library holds samples first,
then presets, and source lookup filters only SourceItem entries.

Change only OpticalConstructor.Tests/BuiltinCatalogue.fs and
BuiltinCatalogueFixtureTests.fs. Add the exact required fact for complete id
membership, library resolution, ordered sources, tunable-source lookup and
sample-store isolation. The shared fixture is already linked into the UI suite,
so no project file change is needed.

The main risks are accidentally sharing mutable stores, losing catalogue order,
or swallowing a write refusal. Fresh construction, sequential writes and
explicit error matches address these. No product code, seed data, durable shape
or migration changes are needed. Preserve the existing supervisor modification
to .manifest.state.json and leave version locks and git state alone.

Perform local build/test validation required by the coding-session instructions;
formal gates and baseline updates remain the supervisor's responsibility.
Capture output only under the supplied specs/0050/.artifacts folder and write
the prescribed implementation log and state-of-the-world files.
