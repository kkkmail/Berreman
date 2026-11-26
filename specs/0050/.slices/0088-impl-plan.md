# Implementation plan

Wire `AppContext.create` to `LibraryDatabase.openOrCreate` and the existing
database proxy constructors. Return the storage error unchanged on open failure;
import the configured shipped Built-in catalogue only for a newly created
database, retaining the existing logged partial-import behavior. Leave the
experiment, scene, export and settings composition unchanged.

Update the launcher to consume the result. Successful opens keep the four
existing surface actions. Failed opens display the storage error under the title
with a centralized automation ID, and disable all four buttons without attaching
click handlers. Keep the additional error content reachable within the window.

Adapt TestAppScope and launcher consumers. Add exactly the three declared facts
covering initial import versus reopen, preserved mismatched schema, and disabled
launcher actions. File fixtures use a unique directory beneath the test output;
the shipped catalogue remains the shared read-only output asset.

Product edits: AppContext.fs, Program.fs and UiIds.fs. Test edits:
TestAppScope.fs, AppContextTests.fs, LauncherTests.fs and
WireUiFinalCompositionTests.fs. The other named composition tests already use
TestAppScope and need no mechanical edits. Preserve the supervisor's existing
manifest-state change. Do not change versions, migrations, or analyzer settings.

Verification covers the resolved 0088 roster: Release build, elevation, all
ui-smoke facts, the four selected ui-tests classes and eleven s11 invariants.
The supervisor retains formal acceptance and checkpoint ownership.
