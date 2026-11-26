# Step 0043 implementation plan

Replace only the fixture composition in `MainWorkbenchTests.fs`. Reuse
`OpticalConstructor.Tests.BuiltinCatalogue.freshStores` and its existing
`InMemoryCatalogueStores` bundle. Compose `initMainWith` from the bundle's
materials, samples and categories, with `liveLibrary` over its imported presets
and a fresh user-source store, matching `DefaultStores.create`.

Update the fresh-model and headless callers to pass the bundle. Use its
materials and categories in the create-picker fact. Remove unused Lifecycle,
MaterialStore and SampleStore opens; document the catalogue composition on the
two helpers. Preserve all five fact names, assertions and traits, including
the `SeedSamples.multilayerQw` band-state fixture.

The acceptance oracle's `S16-WORKBENCH-FIXTURE` obligation and the compiled
0043 gate snapshot agree: all UI smoke tests and the selected non-smoke
MainWorkbenchTests remain required, along with build, elevation and the six
s06 invariants. Formal acceptance belongs to the supervisor. Local verification
required by the coding-session instructions will be recorded separately, with
logs and reports under `specs/0050/.artifacts/0043-local-*`.

No production API, durable shape, catalogue file, package or project reference
changes are needed. The main risks are namespace resolution for the linked
fixture and accidentally changing a fact's assertions while adapting callers.
