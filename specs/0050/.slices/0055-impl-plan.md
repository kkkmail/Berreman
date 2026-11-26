# Step 0055 implementation plan

Re-point the nine authorized UI test consumers to the existing BuiltinCatalogue
fixture. Replace code catalogue reads and self-populating stores with fixture
members, preserving reference callbacks, versions-in-use arguments, fact names,
assertions, counts and numeric inputs. Resolve any positional catalogue reads
through the same entries' well-known ids and retain product ordering contracts.

The source scope is LibraryWindowTests.fs, InverseConstructorTests.fs,
LightSourceEditorTests.fs, SolverHandoffWindowTests.fs, ScrollExtentTests.fs,
WindowLauncherTests.fs, OutOfBandBadgeTests.fs, WorkbenchInfoAndCaptureTests.fs
and FsxAcceptanceTests.fs in OpticalConstructor.Ui.Tests. Reuse the fixture
already linked into that project; add no product code, fixture member or fact.
Update affected catalogue/store comments and remove imports that become unused.

The main risks are changing entry identity when catalogue order differs and
disturbing script acceptance inputs when composing fresh stores. Read the fixture
and consumers before editing, then review the bounded source diff. Gate execution
belongs to the supervisor under Invariant 6 and implement_worker.system-md;
record no invented results or baselines. Preserve the pre-existing supervisor
edit to specs/0050/.manifest.state.json and write the required handoff files.
