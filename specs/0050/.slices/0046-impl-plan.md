# Step 0046 implementation plan

## Attempt 04 repair

The supervisor measured four unexpected ExportImportTests identities, with no
missing tests and all 806 observed cases passing. The slice authorizes only the
two CategoryProxyTests/LibraryFacetsTests additions. Preserve the four storage
regression bodies by moving their scenarios into the existing accepted facts:
structural rejection and pre-write category rejection into AC-I10 export,
renamed-category round-trip into the 0035 round-trip, and empty-list import
rejection into the 0035 unknown-category fact. Keep every assertion, fixture and
accepted fact name; remove only the four extra discovery declarations.

Only ExportImportTests.fs and the three worker handoffs need edits. Production
behavior, the immutable oracle, gate roster and supervisor state are unchanged.
Validate the constructor project locally under the coding-session requirement,
including the exact test inventory against the prior accepted identities plus
the two authorized additions. Formal gate execution remains with the supervisor.

## Attempt 03 repair

Repair the measured ELEV010 on CategoryId.categoryName by returning the existing
CategoryName type. Keep both lookup behavior and the missing-id GUID label.
Reuse CategoryName.caption at the UI boundary, add its facetKey conversion beside
the existing facet vocabulary conversions, and read the name's value only when
rendering the material editor label or asserting its text in the existing test.

This changes the slice's requested string return type because AGENTS.md and
the spec give the analyzers precedence over that spelling. All changes stay in
the authorized effects: MaterialLibrary.fs, LibraryFacets.fs,
MaterialsWindowView.fs, MaterialEditorView.fs and CategoryProxyTests.fs. No
schema, acceptance expectation, test name or analyzer configuration changes.
Run appropriate local build/test validation as required by the coding-session
instructions; formal gate execution and acceptance remain with the supervisor.

## Attempt 02 repair

The review found that Report.fs emits caller-supplied category names but its
material-entry validator still enforces the project schema's original category
enum. Adapt only the standalone library schema in Report.fs: retain its shared
structural definition and references, remove the fixed category enum, and check
exported category identities against the caller's list before writing. Keep
unknown import names as typed errors through the existing domain resolver.

Add the described-glass renamed-category round-trip regression first, plus
focused structural-validation and missing-category failure coverage in
ExportImportTests.fs. Preserve every existing fact and assertion. Record repair
evidence in this log and state-of-the-world; formal gate acceptance remains with
the supervisor. No project-schema file, stored shape or migration changes.

## Original implementation

Make category lookup depend on the caller's category list throughout Domain,
material/library facets, the material editor, and the material JSON boundary.
Pass live CategoryProxy listings from UI contexts, using an empty list on a
listing error so missing identities retain their GUID diagnostic label.
Remove the Materials window's duplicate category extraction override.

Update the existing fixtures and LibraryWindowContext records in the authorized
test files without changing existing fact names or assertions. Add the two
specified regression facts for renamed categories and category facet keys.
Keep standardCategories in place, as required by this slice.

Files: Domain MaterialLibrary.fs and LibraryFacets.fs; UI MaterialsWindowView.fs,
LibraryWindowView.fs, LibraryWindow.fs and MaterialEditorView.fs; Storage
Report.fs; constructor tests CategoryProxyTests.fs, LibraryFacetsTests.fs,
PolarizerBehaviorTests.fs and ExportImportTests.fs; UI tests LibraryWindowTests.fs,
LightSourceEditorTests.fs and WorkbenchInfoAndCaptureTests.fs.

Risks: partial applications after signature changes, a missed library context,
and accidentally retaining seed fallback when category listing fails. There is
no stored-format change or migration. The supervisor owns formal gate execution
and acceptance; the worker will not claim gate results or author baselines.
