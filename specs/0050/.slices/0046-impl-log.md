# Step 0046 implementation log

## Progress

### Attempt 04

- Before repair: read the task, protocols, project prompt, slice, gate roster,
  immutable S17-CATEGORY-LIST oracle and prior implementation. The supplied
  diagnostic reports no missing tests and four unexpected ExportImportTests
  identities, all introduced during attempt 02; all 806 observations passed.
- Plan: move those four scenarios into the three related accepted facts without
  dropping assertions or changing their fixtures. The accepted test names and
  the two additions declared in effects.tests stay unchanged. This repairs test
  discovery within the authorized ExportImportTests.fs edit; no gate, oracle,
  schema, product-code or supervisor-state edit is needed.
- After repair: the malformed-entry and absent-category export bodies now run
  inside the existing AC-I10 export fact; the renamed-category body runs inside
  the existing 0035 round-trip fact; the seed-fallback rejection body runs inside
  the existing 0035 unknown-category fact. Only the four extra Fact declarations
  and their names were removed; all scenario bodies were moved intact.
- Before local validation: rebuild and run the constructor project in Release/x64
  and compare its TRX identities with the prior 802-test observation, which
  already contained the two authorized additions. This is local validation, not
  a formal gate result; the supervisor will rerun its resolved roster.
- After validation: local `dotnet test -c Release` rebuilt the constructor
  project/dependencies and passed all 802 tests (exit 0, no failures/skips or
  emitted warnings). Its TRX test-name multiset exactly matches attempt 01's
  802 observations and differs from attempt 03 only by the four reported extra
  identities. Both required facts pass. All seven ExportImportTests fact names
  match HEAD, and their existing assertions are retained alongside the moved
  regression scenarios. The comparison is captured in the inventory artifact.
- Final source review: whitespace checks passed; CR-insensitive numstats match
  and ExportImportTests.fs uses LF. The constructor-only test organization edit
  requires no production, UI, analyzer or acceptance change. No authority
  conflict remains; formal gate execution belongs to the supervisor.

### Attempt 03

- Before repair: read the retry task, worker protocols, project prompt, slice,
  resolved roster, prior handoffs and the measured ELEV010. Traced every
  categoryName caller; the new MaterialCategory list argument makes the raw
  string result violate the mixed-signature rule despite this being a member.
- Plan: return the existing CategoryName, reuse its existing caption conversion,
  add a source-owned facetKey conversion, and adapt the editor text and test
  text assertions. The GUID diagnostic and caller-list-only lookup stay intact.
- The slice explicitly spells a string return, but AGENTS.md and spec 0050 §2
  give measured analyzers precedence. Record that necessary type adjustment here;
  every affected file is already in the slice's authorized edits.
- Implemented the typed result and all four caller locations. Existing caption
  conversion is reused; CategoryName.facetKey follows the existing vocabulary
  pattern in LibraryFacets.fs. Both lookup-label assertions retain their exact
  expected strings, and the material editor unwraps only at text rendering.
- Before local validation: started a Release/x64 solution build. The coding
  session requires appropriate checks; local test evidence will be recorded
  separately from the supervisor's formal gate acceptance.
- After validation: the full solution built with zero warnings/errors. All 806
  constructor tests, 208 UI smoke tests and 138 selected UI tests passed with
  zero failures/skips. Existing test names and expected label strings remain.
- Direct fsharp-analyzers 0.41.0 with Softellect.Analyzers 10.1.402.136 completed
  for Domain and UI. Their SARIF reports contain zero enforced findings after
  applying the repository's existing ELEV004 and third-party T exclusions.
  Domain has 247 raw findings and UI 415; raw CLI exit -2 reflects those excluded
  diagnostics. The measured categoryName ELEV010 is absent. No formal gate driver
  was invoked and no analyzer rule or known-findings list was changed.
- Final review: the immutable S17-CATEGORY-LIST behavioral contract is preserved;
  all five retry source files and the handoff markdown use LF. Product whitespace
  checks passed and CR-insensitive diff numstats match. The supervisor's existing
  .manifest.state.json and .expected-red-observed-json changes were left intact.

### Attempt 02

- Attempt 02, before repair: read the retry task and review finding. Confirmed
  Report.fs loads the project materialEntry definition unchanged, whose category
  enum rejects the caller's renamed label. The previous passing tests supplied
  only original category names and did not cover this boundary failure.
- Plan: add a described-glass export/import regression and malformed/missing
  category coverage, then specialize the standalone library schema and preserve
  typed rejection of category ids absent from the supplied list. Only Report.fs
  and ExportImportTests.fs need further product edits in this attempt.
- Attempt 02, regression added before production repair: the focused local test
  fails at export with SchemaValidationErr ["document violated the materialEntry
  $def"], exactly reproducing the review finding (0046-retry-red.log/TRX).
  Local validation follows the coding-session requirement; no formal gate was
  invoked. Additional facts cover retained structural constraints, refused
  missing-category export without overwriting, and import without seed fallback.
- Repaired Report.fs: the standalone library schema removes only the inherited
  category enum in memory, retaining required fields, property types and all
  internal definitions/references. Export resolves each CategoryId through
  tryFindCategory over the supplied list and validates its JSON before writing.
  Unknown ids return SchemaValidationErr; import keeps MalformedJsonErr for names
  absent from that list. No seed fallback or alternate parser was introduced.
- After repair: local `dotnet test -c Release` rebuilt the constructor test
  project and dependencies, then passed all 806 tests with zero failures/skips.
  The TRX records all four new ExportImportTests facts as Passed. The regression
  that failed before repair now passes with the exact renamed JSON label and the
  original CategoryId. Existing facts and assertions remain intact.
- Reviewed the diff: only Report.fs and ExportImportTests.fs gained product
  edits during this retry. The published project schema, supervisor manifest,
  package/version files and other repositories were not edited. Formal gate
  execution/acceptance still belongs to the supervisor.

### Attempt 01 (retained history)

- Read the task, worker protocols, project prompt, AGENTS.md, naming conventions,
  slice and resolved gate roster. The existing manifest state change belongs to
  the supervisor and is outside this edit.
- Before implementation: trace category readers, facet consumers, material JSON
  callers and library window context construction. Add the two required facts
  before changing production behavior, then update the shared resolution and
  all authorized callers.
- Added both named regression facts before production edits. Category lookup
  now takes the supplied list, and shared material/library facets pass it to the
  sole category-name resolver. The seed catalogue remains defined.
- Next: thread live category listings through UI contexts and Report.fs, delete
  the Materials window extractor override, and update every affected fixture call.
- Completed live category plumbing in both windows, the view-only material label
  and the material JSON boundary. Removed the duplicate extractor and seed
  fallback. All existing affected test callers now supply fixture categories;
  the three test context builders supply fresh fixture category stores.
- Before local validation: reviewed all changed call sites and the production
  diff. The coding-session instruction requires appropriate build/test checks;
  run local validation while leaving formal gate execution/acceptance to the
  supervisor. This follows the preceding step's same distinction.
- Local Release/x64 solution build succeeded. All 802 constructor tests and
  208 UI smoke tests passed with no failures or skips. The constructor TRX
  confirms both newly required facts passed by their exact names.
- The selected seven UI test classes are running. Source review found no missed
  changed-signature call site, no seed reference in LibraryFacets.fs, and no seed
  lookup in the three resolution APIs. Existing fact names/assertions are retained.
- The selected UI run completed: 138 passed, zero failures and zero skips.
  Whitespace review passed; all fourteen modified F# files are LF, and diff
  numstats match with and without CR-at-EOL ignoring. No implementation remains.

## Files modified

- The fourteen authorized F# files listed in the plan, plus the worker plan and
  this log at the task's exact paths.
- Attempt 02 further changes only Report.fs, ExportImportTests.fs and the three
  worker handoff markdown files at the task's supplied paths.
- Attempt 03 changes MaterialLibrary.fs, LibraryFacets.fs, MaterialsWindowView.fs,
  MaterialEditorView.fs and CategoryProxyTests.fs, plus the three worker handoffs.
- Attempt 04 changes only ExportImportTests.fs and the three worker handoffs.

## Testing state

- Attempt 04 local constructor rebuild/test: exit 0, 802 passed, zero failed or
  skipped, no emitted warnings. Exact TRX inventory matches the earlier 802-test
  observation, with both authorized additions present. The four formerly
  independent regressions now execute inside accepted facts; no coverage was
  removed. No formal gate result is claimed.
- Attempt 03 local build: full Release/x64 solution, exit 0, zero warnings/errors.
- Attempt 03 local tests: 806 constructor, 208 UI smoke and 138 selected UI passed,
  zero failures/skips. Selection is the slice's seven named UI classes.
- Attempt 03 direct analyzer diagnostics: Domain and UI each have zero enforced
  SARIF findings under the existing exclusions; the reported ELEV010 is gone.
  Raw CLI exit -2 in each reflects excluded diagnostics, not an accepted gate
  result. Formal full dependent-project verification remains with the supervisor.
- Attempt 02 local red: the new renamed-category fact failed at export with
  SchemaValidationErr before the repair (one test, exit 2).
- Attempt 02 local green: constructor project/dependencies built in Release/x64;
  all 806 constructor tests passed, zero failed/skipped (exit 0). No build warning
  was emitted in this run. Full solution and UI results below are attempt 01
  observations; they were not rerun for this storage-only retry.
- Formal gates are not run or claimed by this worker. The resolved roster is
  build, elevation, constructor-unit-tests, ui-smoke, selected ui-tests and the
  seven s07 invariants; acceptance belongs to the arc-runner.
- Local build: exit 0, zero errors, no warnings from product F# or project
  configuration. Two SYSLIB0051 warnings originate in unchanged vendored
  MathNetNumerics/Numerics/Exceptions.cs (lines 24 and 77), whose serialization
  constructors call the obsolete BCL Exception constructor. No warning is
  suppressed; vendored code is outside the slice's authorized effects.
- Local constructor suite: 802 passed; UI smoke: 208 passed; selected UI: 138
  passed. Formal elevation and section invariants remain with the supervisor.

## Artifacts

- specs/0050/.artifacts/0046-attempt-04-local-constructor.log
- specs/0050/.artifacts/0046-attempt-04-constructor-results/constructor.trx
- specs/0050/.artifacts/0046-attempt-04-test-inventory.json
- specs/0050/.artifacts/0046-attempt-03-local-build.log
- specs/0050/.artifacts/0046-attempt-03-local-constructor.log and
  0046-attempt-03-constructor-results/constructor.trx
- specs/0050/.artifacts/0046-attempt-03-local-smoke.log and
  0046-attempt-03-smoke-results/smoke.trx
- specs/0050/.artifacts/0046-attempt-03-local-ui.log and
  0046-attempt-03-ui-results/ui.trx
- specs/0050/.artifacts/0046-attempt-03-domain.sarif and
  0046-attempt-03-local-domain-analyzer.log
- specs/0050/.artifacts/0046-attempt-03-ui.sarif and
  0046-attempt-03-local-ui-analyzer.log
- specs/0050/.artifacts/0046-retry-red.log and
  0046-retry-red-results/renamed-category.trx
- specs/0050/.artifacts/0046-retry-constructor.log and
  0046-retry-constructor-results/constructor.trx
- specs/0050/.artifacts/0046-local-build.log
- specs/0050/.artifacts/0046-local-constructor-tests.log and
  0046-local-constructor-results/constructor.trx
- specs/0050/.artifacts/0046-local-ui-smoke.log and
  0046-local-ui-smoke-results/ui-smoke.trx
- specs/0050/.artifacts/0046-local-ui-tests.log and
  0046-local-ui-results/ui.trx

## Gotchas

- Attempt 04 repairs the distinction between authorized file edits and test
  discovery identities: ExportImportTests.fs may change, but this slice declares
  only the two new CategoryProxyTests/LibraryFacetsTests facts. Additional storage
  scenarios therefore run inside related accepted facts, retaining all assertions.
- Attempt 03 necessarily returns CategoryName rather than the spec's string:
  measured ELEV010 takes precedence under AGENTS.md and spec 0050 §2. This is a
  source-level type correction within authorized effects, with no changed
  behavioral acceptance, durable format, migration or new boundary annotation.
- Review required an implementation adjustment within Report.fs: the library's
  structural validator must not inherit the project's fixed category enum.
  Only the in-memory standalone definition is adapted, preserving the schema
  file outside this slice's effects. Category membership remains a separate
  typed domain lookup; no category list is cached and no stored shape changes.
- The IMPLEMENT role's act-only rule supersedes the base protocol's legacy
  instruction to run formal gates. The higher-priority coding-session directive
  still requires local validation. No formal result or new baseline is claimed.
- standardCategories stays until step 58; only its resolution consumers change.
- No version bump, git mutation, catalogue mutation or external-repository write.
- Durable storage shapes are unchanged: the material JSON still carries display
  metadata, resolved against the supplied categories; no migration applies.
