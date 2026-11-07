# Operator recovery — 2026-11-07

Approved plan: `0118-01.postmortem-review-md`; decision:
`../.manual/008-step-0118-picker-tests-recovery.md`. This measured operator
recovery supersedes the earlier worker-only testing state below. Historical
claims that a retry authorized extra identities are incorrect: the new operator
ruling supplies that authority.

- Renamed the fixture binding/RHS to `providerInstance` to remove FS0905.
- A real runtime preflight passed 14/15 AppContextTests cases and exposed a
  directory-URI trailing-separator mismatch. Normalized the observed directory
  before the same equality assertion. Preserved all four facts and every
  assertion; production sources and package references are unchanged.
- Added the three exact step-118 operator create effects, preserving the step-52
  rename. Full 494-step validation passed; refreshed the profile-1 receipt while
  preserving all 988 compiled files and published requirements byte-for-byte.
- All 15 unchanged gates passed: build (zero warnings/errors), elevation,
  ui-smoke (215), selected ui-tests (126) and eleven s14 invariants. Both test
  selections match the accepted identities exactly, with no failures or skips.
- Supplementary elevation against accepted 0117 (`32c7211`) covered the four
  affected product projects and found zero new findings. Reviewed the completed
  product diff and fixture assertions; see the saved final review.
- ASG separately fixes false auth classification and clarifies census authority;
  377 relevant tests and strict mypy passed (ASG 0090 manual 031).
- Evidence is in `../.artifacts/0118-operator-repair/`. The initial rebuild's two
  vendored MathNet SYSLIB0051 warnings and the runtime preflight failure remain
  recorded; the final gate build is warning-free.
- Git HEAD/index, arc state/counters, numeric baselines/checkpoints, expected-red
  records and version remain unchanged. No checkpoint 117, commit or launch.
  Wait for the operator to commit both repositories; a later approved relaunch
  uses `--accept-current-slice --executability-profile 1`.

---

# Implementation log

## Progress

- Attempt 04, before repair: read the task and worker protocols, canonical repository guidance, slice, resolved gate snapshot, previous attempts and current source. The supervisor reports three FS0366 errors for direct implementations of IStorageFolder, IStorageItem and IStorageProvider. Avalonia marks these interfaces NotClientImplementable; adding the reference-only sentinel method would not provide a valid runtime implementation. Plan to use a public, parameterless DispatchProxy subclass confined to this test file, preserving factory registration, test scenarios and all behavioral assertions. Production changes and supervisor state remain untouched. No gates or checks will be run.
- Attempt 04, after repair: replaced the concrete storage interface implementations with LibraryPickerStorageTestProxy. Folder doubles supply their name, URI, bookmark capability and disposal; provider doubles record start-folder lookups and picker options and return the same completed tasks. Unsupported storage operations name the unexpected method. The factory returns the generated IStorageProvider, and provider-identity assertions compare that same instance. The configured-export fact and all picker outcome assertions remain intact. No production source, package or project changes were needed.
- Attempt 04, before handoff: review the edited source and canonical documents, record the current fixture architecture and supervisor-reported failure without claiming execution, and leave every resolved gate and measured count to the supervisor.
- Attempt 04, completed: reviewed the source and preserved the original scenario assertions, adjusting only provider-identity checks to the generated interface instance. Updated the canonical plan, log and state-of-the-world. Source diff sizes agree with and without ignoring carriage returns. No build, tests, analyzers, invariant commands, baseline edits, git mutations, version changes or supervisor-state writes were performed. All required implementation work is present; verification remains the supervisor's next action.

- Attempt 03, before repair: read the task, worker protocols, repository guidance, slice, gate roster, earlier retry request, source changes, and prior worker outputs. The supervisor reports four FS0039 errors from AvaloniaLocator.CurrentMutable/EnterScope in AppContextTests. Runtime reflection alone was misleading: these private APIs are stripped from Avalonia's reference surface. Inspected installed package metadata and upstream Avalonia source to choose the public AppBuilder.With<IStorageProviderFactory> configuration path.
- Attempt 03 plan: replace locator mutation with an isolated headless app builder and window-owned provider functions. Serialize AppContextTests using an xUnit nonparallel collection, preserving the three picker facts, declared export-folder fact, production behavior, and selected gate membership. No gates or checks will be run by this worker.
- Attempt 03, after repair: removed bindLibraryPickerProvider and all three AvaloniaLocator.EnterScope calls. LibraryPickerTestAppBuilder reuses TestAppBuilder and registers IStorageProviderFactory through AppBuilder.With. LibraryPickerTestWindow carries the per-test provider function; runLibraryPickerTest creates and disposes an isolated HeadlessUnitTestSession. A nonparallel xUnit collection protects Avalonia's global services from simultaneous assembly-session work. The existing picker observations and assertions are retained, including factory-request observation for the unavailable-provider case.
- Attempt 03, before handoff: review the final source and canonical documents, correct the previous attempt's scoped-locator narrative, and leave build, test, analyzer, and invariant execution to the supervisor.
- Attempt 03, completed: read the consumer reference metadata for AppBuilder.With and IStorageProviderFactory.CreateProvider, and the headless package metadata for StartNew, Dispatch, and Dispose; the replacement APIs are public. Reviewed the source diff and updated the canonical state-of-the-world. The four added facts retain their names and assertions, production sources are unchanged by this retry, and diff sizes agree with and without ignoring carriage returns. No gates, tests, baseline updates, git mutations, or writes to supervisor state were performed.

- Attempt 02: read the retry task and code-judge finding. The production implementation is retained; the missing Some-start-folder behavior must be exercised through the actual helper.
- Before the coverage repair: inspect the installed Avalonia 12.1.4 interface metadata and headless-session fixture, then add controlled-provider tests in AppContextTests. Use a scoped factory, completed tasks, and dispatcher draining; record observations outside the helper's exception handler so assertions cannot be swallowed.
- Added three headless facts to AppContextTests without changing the declared AppContext fact or production code. The confirmation fact calls MaterialsWindowView.pickLibraryFolder with Some start, verifies the provider received that path, checks title/single-selection/resolved SuggestedStartLocation, and observes exactly the first selected FolderName on the UI thread. The cancellation fact covers None start and an empty selection without changing the prior folder. The unavailable-provider fact observes a factory request that raises PlatformNotSupportedException and confirms no callback or selection change.
- All provider replacement occurs inside AvaloniaLocator.EnterScope on the existing headless UI session. Provider responses are completed tasks; Dispatcher.UIThread.RunJobs drains the posted work. The fixture opens no native dialog and writes no files. Assertions inspect recorded observations after production execution, outside its catch boundary.
- Before handoff for attempt 02: review the added test source and update the canonical state-of-the-world, preserving the supervisor's authority over gate execution and measured counts.
- Completed attempt-02 source review and updated the canonical state-of-the-world. Source diff sizes match with and without ignoring carriage returns. Only AppContextTests and the three canonical worker documents were edited in this retry; production changes from attempt 01 and supervisor state were retained. No gates were executed or green-lit.
- Read AGENTS.md, the worker protocols, project prompt, slice, resolved gate roster, naming conventions, and S33-SHARED-WINDOW-PIECES acceptance obligation.
- Inspected the existing verb renderers, folder picker, data-file start-folder resolution, AppContext.compose, and TestAppScope fixtures.
- Before the regression test: add exactly the declared AppContextTests fact over configured app locations and the standalone in-memory context.
- Added the declared fact before changing production behavior. It compares elevated FolderName options directly and reuses the existing TestAppScope fixtures and import factory. No test execution was performed under the worker protocol.
- Before production implementation: extract the shared pointer-aware verb and parameterized picker, rewire Library callers, and set the export suggestion in the shared app composition.
- Added MaterialsWindowView.verbButtonOn and its verbButton adapter, retaining keys, automation ids, accessible captions, styling, and handled-pointer dispatch.
- Moved the folder picker to MaterialsWindowView.pickLibraryFolder with the supplied title, optional provider-resolved starting location, first-selection handling, and UI-thread callback. Cancellation and unavailable providers remain inert.
- Deleted Library's private verbButton, verbButtonOn, and pickCatalogueFolder. All their callers now use the shared Materials helpers; registration retains its title, no suggested start, and RegisterCatalogueFolder message.
- Set catalogueImports.exportFolderOpt to Some locations.cataloguesFolder in AppContext.compose, covering both create and recreateLibrary without changing other factory defaults.
- Before handoff: review the four source diffs and write the required state-of-the-world; leave execution and measured counts to the supervisor.
- Reviewed the source diff: the shared callback retains handled presses, the registration call retains its title and message, and the configured folder is applied before startup import through the common composition. The existing factory still sets exportFolderOpt to None.
- Diff sizes match with and without ignoring carriage returns; no source line-ending churn was introduced. No git mutations, version changes, or writes to supervisor state were made.
- Wrote the state-of-the-world at the supplied path. All implementation requirements are addressed; supervisor verification remains pending.

## Files modified

- Created the implementation plan and log at the supplied paths.
- AppContextTests.fs: added the catalogue-export-folder regression fact and three controlled-provider picker facts; attempt 03 replaces unavailable locator APIs with public app-builder registration, isolated sessions, and a nonparallel xUnit collection. Attempt 04 replaces direct storage interface implementations with runtime test proxies while retaining those facts and assertions.
- MaterialsWindowView.fs: shared event-aware verb renderer, unit adapter, and parameterized folder picker.
- LibraryWindowView.fs: removed the superseded wrappers and picker and redirected their call sites.
- AppContext.fs: configured the export suggestion in the shared composition function.
- Wrote 0118-state-of-the-world.md at the supplied path.

## Testing state

No checks or gates run. IMPLEMENT worker invariant 6 assigns execution and measured results to the supervisor. Required roster: build, elevation, ui-smoke, selected ui-tests, and eleven s14 invariants. Expected discovery increase: four ui-tests facts in total (the declared AppContext fact and three picker facts authorized by the attempt-02 retry). No worker-authored baseline or checkpoint changes.

The attempt-04 task supplies the supervisor's measured build failure: three FS0366 errors for the non-client-implementable members of IStorageFolder, IStorageItem and IStorageProvider. Those direct implementations have been replaced in source. The repair has not been built or executed by this worker; warning status, analyzer results, and test outcomes require supervisor verification. The final summary reports no worker gate results and does not assert commit readiness.

## Artifacts

No runtime artifacts generated. Persistent worker output belongs under specs/0050/.artifacts.

## Gotchas

- Avalonia marks its storage interfaces NotClientImplementable. Runtime test proxies avoid compiling direct implementations against the reference-only sentinel members. This remains confined to the test boundary; production and package references are unchanged. The proxy base must remain public, non-sealed and parameterless so DispatchProxy can derive from it. References: https://github.com/AvaloniaUI/Avalonia/blob/master/src/Avalonia.Base/Platform/Storage/IStorageProvider.cs and https://learn.microsoft.com/en-us/dotnet/api/system.reflection.dispatchproxy.create?view=net-10.0.
- Attempt 02 relied on runtime metadata for AvaloniaLocator; Avalonia marks it PrivateApi and strips its members from the consumer reference assembly. Attempt 03 uses AppBuilder.With and HeadlessUnitTestSession.StartNew instead. Upstream references: https://github.com/AvaloniaUI/Avalonia/blob/master/src/Avalonia.Base/AvaloniaLocator.cs and https://github.com/AvaloniaUI/Avalonia/blob/master/src/Avalonia.Controls/AppBuilder.cs.
- The new nonparallel collection is necessary because independent headless sessions still manipulate process-global Avalonia services. It changes scheduling only, not test membership, assertions, or gate selection.
- The retry explicitly asks for helper coverage in an already selected class while retaining the declared fact. AppContextTests is both an authorized edit and selected by ui-tests, so all three new facts live there; the gate roster and existing window facts are unchanged.
- The unavailable-provider scenario deliberately raises from the controlled factory: a provider could not be supplied, and the production IO boundary must leave the caller unchanged. It is separate from cancellation, which returns an empty folder list from an available provider.
- The pre-existing modification to specs/0050/.manifest.state.json belongs to the supervisor and is left untouched.
- Library's existing verb renderer sets an accessible caption while Materials' renderer does not. Preserve that caption property in the shared renderer so this extraction does not regress Library accessibility.
- The existing libraryVerbButtonWithBackground also renders selected backup rows with a distinct fill. It is not one of the three superseded declarations and remains for that specialized use; ordinary verb calls all move to MaterialsWindowView.
