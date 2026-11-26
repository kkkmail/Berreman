# Step 0056 implementation plan

Re-point the eight authorized UI test files to the existing BuiltinCatalogue
fixture and well-known SampleIds. Keep sample payload projections where the
tests already consume samples; use SampleIds directly where only an id is read.
Preserve every fact name, trait, assertion, count and numeric input. Update
comments describing catalogue inputs and remove imports that become unused.

Files: AppContextTests.fs, ElementMenuTests.fs, ExperimentControlsTests.fs,
LayerBandsControlsTests.fs, LibraryControlsTests.fs, SourceAndDetectorMoveTests.fs,
WireUi0040CompositionTests.fs and WireUiCompositionTests.fs, all under
Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests. No product file,
fixture member or fact is added. No durable format or migration is involved.

Review catalogue identity and any positional/ordered reads so row order cannot
change the entry tested. Keep all source edits in LF. Leave the pre-existing
supervisor-owned manifest-state change untouched. Invariant 6 and the IMPLEMENT
worker protocol assign all build, test, analyzer, acceptance-grep and invariant
execution to the supervisor; this worker records no gate results.
