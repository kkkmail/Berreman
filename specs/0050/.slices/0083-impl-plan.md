# Step 0083 implementation plan

Implement S24-SCOPE-LOCATIONS by reading workbench settings and data locations through the existing single settings provider, saving once, and passing both values through Startup into AppContext. Reuse readDataLocations and resolveLocations for normal and fallback startup. Keep all stores in memory and direct only the existing export adapter to the configured exports folder.

Add TestAppScope immediately after TestApp.fs, with an in-memory connection and owner-specific folders under the test assembly output. Route all six named UI test modules through the fixture while preserving their assertions. Add the declared AppContextTests fact exercising a real two-point CSV export into its configured folder.

Modify only the eleven authorized source/project paths and the supplied plan, log and state-of-world paths. Preserve the pre-existing manifest-state change. The main risks are missing a changed-signature caller, F# compile ordering and test exports escaping their output folder. No stored shape changes, migrations, package changes or version bump are needed.

The supervisor owns acceptance gates and measured checkpoints. Higher-priority development instructions require local verification: build the affected UI test project in Release/x64, exercise the selected test classes and the smoke suite, and record these as local observations without claiming supervisor gate results. Persistent logs and reports belong in the supplied .artifacts folder.
