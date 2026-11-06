# Step 0049 implementation plan

Make MaterialEditorWindow's third constructor parameter a mandatory CategoryProxy,
use it directly in the editor context, and delete the self-seeding fallback and
its comment. Shorten the class documentation to the caller-supplied store contract.
MaterialsWindow already supplies its shared categories by name; retain that call.

Update the 21 constructor calls without categories in EmbeddedChartTests,
MaterialEditorWindowTests and ScrollExtentTests to use the existing
OpticalConstructor.Tests.BuiltinCatalogue.categoryStore (fun _ -> []). Keep every
fact name and assertion. MaterialsWindowTests, SampleEditorWindowTests,
WireUi0040CompositionTests and WireUiFinalCompositionTests already supply stores;
retain those stores without editing their calls. Reuse the linked catalogue
fixture; add no seed data, helpers or project references.

The main risk is missing a constructor call or replacing a deliberately shared
store with a fresh fixture. Read all callers and review the scoped diff and LF
line endings. No durable format changes or migration are involved.

The IMPLEMENT worker's Invariant 6 assigns all gate execution to the supervisor.
Record the pending build, elevation, all ui-smoke, the five selected ui-tests
classes and seven s07 invariants without running or claiming their results.
Write the implementation log and state of the world at the supplied paths.
