# Step 0058 implementation log

## Progress

- Read the repository instructions, worker protocols, project prompt, slice,
  gate roster, naming conventions and prior handoff. The only initial dirty
  path is the supervisor's `specs/0050/.manifest.state.json`; leave it untouched.
- Before removal: traced the self-seeding declarations and the empty-store
  callers. The slice's exact pre-rename qualified-call search returned no
  matches, so there is no missed product or test call to a self-seeding store.
- Wrote the implementation plan. Next remove only the retired catalogue values
  and factories, retain stable IDs and editor structure, and rename empty-store
  callers without changing their behavior.
- After removal: deleted material/category catalogue values and complexities,
  the private constant-complexity extension and interval, sample/source modules,
  preset values, fixed grouping trees and the four self-seeding factories.
  Preserved all well-known IDs, both quarter-wave thicknesses and the editor's
  starter structure. Removed the five material-library opens used only by the
  deleted builders; retained the active-optics extension used by toProperties.
- Renamed the four empty factories and all callers, including the existing
  library-tree test. The empty factory signatures and bodies remain unchanged
  except for their names. Private store initialization/versioning bodies are
  retained; local `seeded` bindings now describe initial versions or presets.
- Updated the identity, version-one, category/store, library/facet and AGENTS
  documentation. The docs now describe imported catalogue data and distinguish
  first-version references from latest-version resolution. No catalogue file,
  serialization shape, test identity or assertion changed.
- Before final handoff: review the source diff for dangling references, retained
  editor data, factory equivalence and LF-only changes; then write the required
  state-of-the-world with inherited checkpoint counts and pending gate status.
- After source review: no retired catalogue symbol, obsolete empty-factory name
  or MaterialLibrary.standard reference remains under Berreman/ or AGENTS.md.
  The unrelated private constantComplexity helpers in FsxAcceptanceTests and
  OutOfBandDiagnosticTests remain; they do not depend on the deleted extension.
  Each renamed store still starts empty, the library derives its tree from held
  entries, and catalogue import and fixture writes retain their original bodies.
- Reviewed the retained quarter-wave/editor structure and all identity constants
  in the diff. Test changes are only factory-call renames. Normal and
  ignore-cr-at-eol numstats agree; no line-ending churn was introduced. Git's
  CRLF warning concerns only the pre-existing dirty supervisor manifest.
- Wrote `0058-state-of-the-world.md` at the supplied path. Implementation is
  complete; commit readiness remains unverified until the supervisor runs the
  unchanged roster. No gate pass, checkpoint update or new test count is claimed.

## Files modified

- `0058-impl-plan.md` and this log: implementation approach and durable progress.
- `MaterialLibrary.fs`, `ElementId.fs`, `MaterialStore.fs`, `SampleStore.fs`:
  retired catalogue values/factories, renamed empty factories and current docs.
- `CatalogueImport.fs`, `BuiltinCatalogue.fs`, `LibraryProxyTests.fs`,
  `MainWorkbenchTests.fs`: mechanical empty-store caller renames.
- `LibraryFacets.fs`, `AGENTS.md`: file-backed catalogue documentation.
- `0058-state-of-the-world.md`: completed implementation and supervisor handoff.

## Testing state

No build, tests, analyzers or invariant gates run by this worker. The IMPLEMENT
protocol's Invariant 6 assigns verification to the supervisor after handoff.
The roster is build, elevation, constructor-unit-tests, ui-smoke, the six
selected ui-tests classes, and the seven s07 invariants. No new test or test
identity change is required by this deletion/rename slice.

The latest inherited checkpoints record constructor_unit_tests = 801 at index
56 and ui_smoke_tests = 208 at index 54. There is no ui_tests checkpoint value;
the handoff retains the prior step-0054 zero absence marker, not a measurement.
The constructor and smoke counts are expected to remain unchanged because no
test declaration or assertion was added, deleted or renamed.

## Artifacts

No run artifacts were created. The three worker outputs are at the exact
supplied `.slices/0058-*` paths.

## Gotchas

- The existing empty-store call in `LibraryProxyTests.fs:151` is absent from
  the enumerated edit paths. Rename it under the explicit "every caller follows"
  requirement and the declared `OpticalConstructor.Tests` scope; keep its fact
  and assertions unchanged. It is not a forbidden residual self-seeding call.
- `CatalogueFiles.fs` already documents its current `createInMemory` double,
  and `CatalogueImport.fs` has no stale open comments. Do not manufacture edits.
- Preserve the supervisor's dirty manifest and all gate/checkpoint files.
