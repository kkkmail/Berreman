# Step 0078 implementation plan

Extract the ten live-library and preset-store facts in LibraryProxyTests.fs
into an abstract LibraryProxyContract. Supply LiveLibraryStores with the
composed LibraryProxy, SampleProxy and SourceProxy, and expose an independent
emptyPresetStore factory. Preserve every fact name and assertion.

Keep LibraryProxyTests as the concrete in-memory subclass, using a fresh
BuiltinCatalogue import and empty user-source store per factory call. Keep
the ten pure/stub facts on that class. Remove the shared module-level proxy;
leave the project compile position and production stores unchanged.

The main risks are inherited xUnit discovery and accidental fixture sharing.
Use the established material/source contract layout, annotate fact returns
as unit, and obtain each live composition once per fact. Preserve the live
tree error assertion by copying the supplied library's read surface and
replacing its listing function with the existing failure stub.

Write the implementation log and state of the world at the supplied paths.
The supervisor owns the resolved build, elevation, constructor-unit-tests
and eleven s10 invariant gates and their measured checkpoint.
