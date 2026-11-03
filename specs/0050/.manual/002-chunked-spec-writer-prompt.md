# 002 — The chunked spec-writer prompt for spec 0050

Spec 0050 is a spec-writer → arc-runner campaign that implements the feature list
`specs/0050/.manual/001-library-files-database-multi-setup-inverse.md` in full.

## 1. Before launching

- **Spec id `0050`, branch `0050`.** `specs/0050/.manual/` holds the feature list (`001`) and this prompt (`002`). The
  spec-writer adds the rest of the folder.
- **Feature list:** `001` is the spec's feature list.
- **`unit-tests` gate:** `BerremanTests` takes about 30 minutes whole, and MTP is silent for minutes between the start
  and the summary. Before the arc starts, `.spec-gates` runs it by class on the steps that touch its code and whole
  only on the final step, with a deadline and `silent_for_seconds` that fit 30 minutes (GATES below).

## 2. Pointers (paste into the spec-writer's Pointers)

```
specs/0050/.manual/001-library-files-database-multi-setup-inverse.md
specs/0046/.manual/018-sharing-libraries-inverse-planner.md
AGENTS.md
naming_conventions.md
Directory.Build.targets
Elevation/elevation-known-findings.json
Berreman/Berreman/
Berreman/Analytics/
Berreman/OpticalProperties/
Berreman/OpticalConstructor/OpticalConstructor.Domain/
Berreman/OpticalConstructor/OpticalConstructor.Storage/
Berreman/OpticalConstructor/OpticalConstructor.Optimization/
Berreman/OpticalConstructor/OpticalConstructor.Ui/AppContext.fs
Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs
Berreman/OpticalConstructor/OpticalConstructor.Ui/SolverHandoffView.fs
Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs
Berreman/OpticalConstructor/OpticalConstructor.App/
Berreman/OpticalConstructor/OpticalConstructor.Database/
Berreman/OpticalConstructor/OpticalConstructor.Seeding/Seeding.fs
Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs
Berreman/OpticalConstructor/OpticalConstructor.Controls/WindowLayout.fs
Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/FsxAcceptanceTests.fs
Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs
Berreman/BerremanTests/InverseFitHarness.fs
Berreman/BerremanTests/BiaxialSample.fs
Berreman/BerremanTests/AbsorbingBiaxialSample.fs
Berreman/BerremanTests/AbsorbingBiaxialNoisyTests.fs
Berreman/BerremanTests/MuellerReconstructionTests.fs
Berreman/BerremanTests/MuellerInverseTests.fs
Berreman/BerremanTests/BiaxialInverseTests.fs
Berreman/Data/MuellerMatrix/README.md
specs/0044/.manual/004-active-anisotropic-inverse-problem.md
specs/0044/.manual/017-feasible-biaxial-experiments.md
specs/0044/.manual/020-handover.md
.gates/
```

## 3. General comment (paste verbatim)

```text
Write the spec that implements the feature list
specs/0050/.manual/001-library-files-database-multi-setup-inverse.md in full.

001 IS BINDING AND AUTHORITATIVE.
- Every item in 001 is decided; do not re-open any of it.
- Verify each fact you build on against the tree; do not assume it. Where 001 and the code disagree,
  the code is the baseline, and 001 is the target.
- If something is genuinely ambiguous, pick the reading most consistent with 001 and AGENTS.md, and
  record it in the .spec-md problem statement. Do not ask.

GOAL
Move the Optical Constructor from in-memory, code-seeded stores to two homes:
- SLOW data (the library) in a local SQLite database fed by typed JSON catalogue files;
- FAST data in a user-chosen experiment-kit FOLDER of plain files, used by BOTH the direct and the
  inverse constructor.
On top of that, build the data model that makes multi-sample, multi-setup inverse problems simple:
- Items: physical pieces with their own known or unknown parameters.
- Setups: items placed in slots, two or more samples in series.
- Experiments: one recorded run each, with its data files.
- Studies: many experiments fitted together, staged or joint.
Then add online material sources and the Measurement Planner, gyrotropic crystals first.

BINDING RULES (every section)
1. AGENTS.md in full (CLAUDE.md only imports it):
   - elevate every primitive (single-case DUs with .value / tryCreate; no naked bool; no enum);
   - the ELEV rules: unique names instead of RequireQualifiedAccess; two-word DU case names; *Error
     cases end in Err / Exn; a free function of one of our types becomes its member; no wide tuples;
     elevate at origin, never at the call site; integer value tags on closed sets;
   - where any document disagrees with the analyzers, the analyzers win;
   - errors as Result values whose cases carry a reason;
   - IO only through *Proxy records built at the composition root, with in-memory stubs of the same
     shape in tests;
   - camelCase record fields; interpolated strings only; `name : Type`; braces on their own line;
   - explicit public signatures; short comments that cite the spec; no new comments in project files;
   - Release x64 only; central package versions in the root Directory.Build.targets; no nested
     Directory.Build.props; one .fsproj per folder;
   - zero warnings from our code; LF line endings;
   - never touch *.nb; reference the vendored MathNet projects; never bump any version ring.
2. The tree is ELEV-clean: Elevation/elevation-known-findings.json lists no finding, so every ELEV
   finding (ELEV004 excepted) fails the elevation gate.
   - Every line the spec writes is ELEV-clean.
   - Never add an entry to the known-findings list, and never edit .elevation-analyzers.json or the
     analyzers.
   - A mark goes on a declaration, never on a module:
     - [<ElevationBoundary(nameof IoBoundary, "<reason>")>] (or InteropBoundary / StorageBoundary /
       ContractBoundary) only at a genuine seam;
     - [<NumericCarrier("<reason>")>] only on a type whose fields or generic arguments are large
       numerical data.
   - Large numerical data, above all at a third-party numeric library (MathNet, Alglib), keeps its fast
     representation, and the library call is marked InteropBoundary on its declaration; every small
     value with a meaning is elevated.
   - Every name of ours is unique: an Elmish surface's types and functions carry the surface
     (KitStartModel, kitStartView), and when two of ours collide, the LOWER project keeps the name.
3. The database holds SLOW data only (001 §C): library entries with their versions, categories, the
   catalogue-import log, and pulled-source records. Kits, items, setups, experiments, studies, fit runs,
   measured data and the kit history are NEVER stored in it.
4. Kit folders (001 §E, §F): both constructors require one.
   - The kit root is FLAT. The kit file is `.kit-json`, and every kit file's kind is its extension.
   - Subfolders hold only additional data (library/, data/, .oc/).
   - Every name follows 001 §A.
5. No legacy, no migration (001 §R).
   - Delete superseded code IN THE STEP that replaces it.
   - No schema up-converters: a version mismatch is a typed error.
   - No EF Core migrations: EnsureCreated plus a schema stamp.
   - No compatibility shims, no fallback readers, no "keep old path as option".
6. REUSE, NEVER COPY (001 §Q, §R).
   - Every reuse 001 names is a MOVE or a call: delete at the source in the same step that adds at the
     destination.
   - A duplicated function body, a re-transcribed constant or a second parser for the same format is a
     defect. Write briefs so the reuse_critic can check it.
7. No new inverse-physics tests.
   - AT-2 replicates the existing AbsorbingBiaxialNoisyTests through the kit model.
   - The Planner's acceptance replays existing facts (001 §M).
   - BerremanTests' numbers stay unchanged by every move.
8. Tests run on Microsoft.Testing.Platform (AGENTS.md):
   - xUnit v3 only, with xUnit v3 filters (--filter-method / --filter-trait / --filter-not-trait);
   - no VSTest package, no xUnit v2 package, no Xunit.Abstractions; skip at run time with Assert.Skip.
9. UI (AGENTS.md "Automate by meaning"):
   - every user action is a semantic message (001 §P) with stable automation ids;
   - every id is a uniquely named static member of ConstructorUiId
     (OpticalConstructor.Controls/UiIds.fs); an id derived from a Domain value is an extension member
     of ConstructorUiId in the project that owns the value's type
     (OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs);
   - every new window calls WindowFit.keepOnScreen and joins ScrollExtentTests;
   - behaviour is tested in Domain first, structure in Avalonia.Headless.
10. No test, fixture or kit lives under Documents; test kits and databases go under the test run's
    output directory (001 §G, §Q).
11. NOTHING IS DEFERRED.
   - A step finishes its whole brief. It leaves no stub, no TODO, no "later" or "follow-up".
   - No brief hands part of its own work to a later step.
   - Work that cannot be finished as written is a blocked step naming the question, and the arc stops
     there.
   - The OUT OF SCOPE list below is the operator's decision, not a deferral.

SECTIONS
You own the final plan. Each section is at most about forty steps, and dependencies point backward.
The document paragraph for each section is its spec; put that paragraph's essentials into the brief,
because mappers do not read each other's sections.

 1. Library entries as data files (001 §A, §B):
    - the tabulated EpsWithDispValue case; canonical JSON + SHA-256; per-kind JSON Schemas, reusing the
      ProjectJson serializer and validator stack;
    - generate Berreman/OpticalConstructor/Catalogue/Builtin/ ONCE from the code seeds (GUIDs and values
      byte-for-byte), then delete the seed values;
    - CatalogueImport over the existing SeedingProxy shape; tests load the catalogue files.
 2. Library database (001 §C, §G):
    - replace the Test entity with the library model; EnsureCreated; the Pinned flag replacing
      VersionsInUse;
    - SQLite create for every library proxy, with contract suites run against BOTH in-memory and SQLite
      :memory:;
    - locations from appsettings.json (the OpticalConstructorSqlite connection string); backup and
      restore; AppContext switches.
 3. Catalogue import and library-entry exchange (001 §B): import on every start by manifest hash; the
    decision table and its one-row-per-entry dialog; extra read-only catalogue folders; export to
    -json / catalogue folders (export pins); single-file drop; licence attribution display.
 4. Kit folder core (001 §E, §A):
    - KitProxy (folder + in-memory), .kit-json, atomic writes, .oc/lock, external-change check;
    - relative paths + hashes; copy-in of attached files; library snapshot + resolution on open;
    - today's experiments and scenes persist into the kit;
    - DELETE .ocproj (ProjectFile, the Project aggregate) and the Scene / ExperimentCollection seams.
 5. Start screen and history (001 §F):
    - KitHistoryProxy over the per-user preferences, REUSING RecentFiles.bump (+ star / unstar / forget /
      evict); starred kits always on top, sorted by last open date; KitHistoryLimit = 100;
    - missing-folder handling; recent.json replaced;
    - the Launcher's Main AND Inverse both go through the start screen; the Data locations panel.
 6. Send and receive kits (001 §H): KitArchiveProxy (zip honouring sendExclude, open from zip); copy
    experiments between kits with item mapping.
 7. Items and item models (001 §I): the Kit module (Item, ItemId, ItemLabel, Parameter, ParameterState),
    ItemModel library entries with generated ParameterAxis lists, dial mapping, revisions, the Items
    panel.
 8. Setups as slots and the run law (001 §J, §N):
    - N objects in series (delete measuredElementOf); a real source whose default reproduces today's
      charts;
    - the slot bar and quick swap; the grid generator;
    - the naming template, which REPLACES MuellerExperimentLabel.parsed and its experimentRegex.
 9. Experiments v2 and data import (001 §K):
    - roles, sessions, multi-variable scans;
    - the column-mapping wizard with import profiles stored in .kit-json. Built-in profiles REUSE
      MeasuredData's parsers and MuellerDataStore's parseMuellerCsv, and a Mueller-matrix observation
      profile is added;
    - split-by-column + template match, exclusions with reasons, setup corrections with audit notes;
    - DELETE the MuellerDataSet DU and the archive-keyed MuellerDataProxy (data.zip stays as AT-1's data
      source only).
10. Promote the inverse harness and the reference pipeline (001 §Q "Reuse"):
    - MOVE into OpticalConstructor.Optimization: the bench noise model, observe / residual /
      throughput-residual, scalingAround, the fit wrappers, RecoveredQuantity / RecoveryStatistics /
      statisticsFor, predictedRelativeErrors and combinationVariance;
    - create Berreman/BerremanTestSupport/ (own folder, own .fsproj) holding BiaxialSample and
      AbsorbingBiaxialSample, referenced by BerremanTests and OpticalConstructor.Ui.Tests;
    - promote the MuellerReconstructionTests §7.2 hand-composed pipeline into product code and re-point
      §7.2 at it; move its expected literals into one shared reference module (no file read for
      expectations, spec 0044 §8.5);
    - BerremanTests stays green with unchanged numbers.
11. Studies and fitting (001 §L, §N):
    - scopes and links; sessions and gain models (MuellerReconstruction.familyGain becomes their first
      implementation);
    - staged and joint fits; automatic engine choice, REUSING buildDesign / reconstruct and InverseFit /
      MuellerInverseSolver over MuellerInverse.forwardModels;
    - generated axes; append-only fit runs; results (FitQuality reused), realizability, product check,
      profile likelihood;
    - publish / promote-to-library; the study view;
    - DELETE the SolverHandoffView stub, reusing its checks as pre-fit validation.
12. The two acceptance tests (001 §Q):
    - AT-1 optics-mueller and AT-2 AbsorbingBiaxialNoisyTests, built ONLY through semantic messages,
      with the folder round-trip (write, SendKit, unzip, reopen against an EMPTY library, compare the kit
      and the start-point residual);
    - AT-2's five-seed ensemble is xUnit v3 Explicit;
    - add the gate descriptor .gates/OpticalConstructor/kit-acceptance.gates in the shape of the other
      MTP test gates (--filter-trait "Category=kit-acceptance", TRX result_capture, class selector,
      phase verify, requires_gates [build]), and add --filter-not-trait "Category=kit-acceptance" to
      ui-tests.
13. Online material sources (001 §D): MaterialSourceProxy. First refractiveindex.info (offline snapshot +
    update, REUSING MaterialImport as the record parser); then PTB OCDB with uncertainties; then the
    Henke compound calculator. Provenance, licence, new version on update.
14. Measurement Planner, gyrotropic crystals first (001 §M):
    - inputs from the kit;
    - layer 1 symmetry table;
    - layer 2: Fisher information / rank / confounded combinations / Cramér–Rao, REUSING
      FitQuality.residualJacobian and its ResidualJacobian carrier (columnNorms, conditionNumber,
      correlation) and the promoted predictedRelativeErrors;
    - greedy D-optimal ranking; the plan emitted as Planned setups and experiments in the kit; the cache
      in .oc/planner/;
    - acceptance = the existing MuellerInverseTests / BiaxialInverseTests facts listed in 001 §M.

SECTION INVARIANTS
Declare these on every section AFTER the one that establishes the rule. They run after every step, so
never declare one on the section that makes it true. Commands contain no backslash; use -F for literal
text.
- all:      ASSERT_INVARIANT elev-list-empty must-find-nothing: git grep -n -F "ELEV0" -- Elevation/elevation-known-findings.json
- all:      ASSERT_INVARIANT no-kit-dot-kit must-find-nothing: git grep -n -F "kit.kit-json" -- Berreman/
- all:      ASSERT_INVARIANT no-vstest must-find-nothing: git grep -n -E "Microsoft.NET.Test.Sdk|xunit.runner.visualstudio|Xunit.SkippableFact|IsTestingPlatformApplication" -- "*.fsproj" "*.props" "*.targets"
- after 1:  ASSERT_INVARIANT no-code-seeds must-find-nothing: git grep -n -E "let (builtInEntries|seedEntries|standardCategories) " -- Berreman/OpticalConstructor/
- after 4:  ASSERT_INVARIANT no-ocproj must-find-nothing: git grep -n -i -F "ocproj" -- Berreman/OpticalConstructor/
- after 8:  ASSERT_INVARIANT no-name-regex must-find-nothing: git grep -n -F "experimentRegex" -- Berreman/
- after 9:  ASSERT_INVARIANT no-dataset-du must-find-nothing: git grep -n -F "MuellerDataSet" -- Berreman/
- after 10: ASSERT_INVARIANT harness-moved must-find-nothing: git grep -n -F "let noisyObserveAbsoluteWith" -- Berreman/BerremanTests/
Add similar ones wherever a brief says "nothing may name X".

GATES (the spec's .spec-gates)
- build: every step.
- elevation: every step that edits an .fs or .fsproj. elevation-full: every step that renames across
  projects, and the final step.
- constructor-unit-tests, ui-smoke, ui-tests: as declared under .gates/, selected by class
  (gate_selection) on the steps that touch their code; whole on the last step of each section.
- unit-tests (BerremanTests) takes about 30 minutes whole (specs/0044/.manual/020-handover.md):
  - select it by class on every step that touches what it covers;
  - narrow its applies_to to the code BerremanTests actually covers: Berreman/BerremanTests/**,
    Berreman/BerremanTestSupport/**, Berreman/Berreman/**, Berreman/OpticalProperties/**,
    Berreman/Analytics/**, Berreman/OpticalConstructor/OpticalConstructor.Optimization/**, and the
    inverse Domain files MuellerInverse.fs and MuellerReconstruction.fs;
  - it then runs in sections 10-11 and wherever those files change, not on every UI step;
  - run it whole only on the final step, with a deadline and silent_for_seconds that fit 30 minutes.
- kit-acceptance (new, section 12): applies to the Study / run-law / import code and the acceptance test
  files.
Keep count_at_least baselines non-regressing. A test that moves between projects counts in its new
project in the same step.

ACCEPTANCE (the spec is done when)
- AT-1 and AT-2 pass under kit-acceptance, with the assertions listed in 001 §Q.
- The Planner reproduces the 001 §M facts.
- Every gate is green with zero warnings from our code, elevation-full included, and
  Elevation/elevation-known-findings.json still lists no finding.
- BerremanTests' numbers are unchanged.
- The code no longer contains the 001 §R removals.
- The app persists the library in the database and everything else in kit folders.

OUT OF SCOPE
- ELEV004 (measured, not gated) and any change to the analyzers or to STL;
- version bumps of any ring;
- new inverse-physics tests;
- the public MathNet NuGet;
- a WinForms port (but keep UI logic in pure Domain / State, as AGENTS.md requires);
- cloud or shared databases;
- Bayesian sequential design;
- shipped planner playbooks.
```
