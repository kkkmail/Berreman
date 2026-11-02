# 021 — Spec-writer → arc-runner: the verdict and the chunked spec-writer prompt

**Revision 2 (2026-11-01, the operator's fourth round; spec 0048 `.manual/001`):**

- The ELEV gates now exist and found 3,757 findings. Their cleanup is the FIRST part of this campaign (§1, and PART A
  of the prompt).
- Every test gate runs on Microsoft.Testing.Platform.
- Build machines run without OneDrive.
- `CLAUDE.md` is now a pointer to `AGENTS.md`.
- The spec id is `0048`.

## 1. First part: the ELEV ledger (`specs/0048/.manual/002`)

The elevation gates were set up in this round (`0048/.manual/001` §3) and run over the tree: **3,757 enforced findings
in all 13 F# product projects** (ELEV004 excluded, as in STL). So the campaign starts with them, before any of
`020`'s phases:

- **PART A, the cleanup,** fixes the ledger's packages E1–E8 bottom-up:
  1. the solver;
  2. Analytics;
  3. Controls;
  4. Domain;
  5. Optimization / Storage / Seeding;
  6. Ui;
  7. TestWindows;
  8. App.

  At an estimated 150–250 steps, that is four to six sections.
- **Code that 020 deletes is not cleaned in PART A.** It is listed in `002` §4, and its entries go in the PART B step
  that deletes it.
- **The gate stays usable from the first step.** `Elevation/elevation-known-findings.json` records today's findings.
  - A recorded finding does not fail `elevation`; a new one does.
  - A recorded finding that is gone fails as stale, so every fix deletes its entry in the same step.
  - PART B adds no finding at all.
- **The campaign ends** when the list is empty and `elevation-full` is green.
- **Before launch:** `002` §6 asks seven rulings (R1–R7). Each has a default that the prompt below adopts, unless you
  overrule it.

## 2. Verdict: this is a spec-writer → arc-runner cycle (a sectioned / chunked campaign), not a task for one agent

- **Size.**
  - PART A: about 150–250 steps, four to six sections.
  - PART B (`020`, a feature list of about 60 KB): ten phases (§S), realistically 250–400 steps.
  - The section planner's sizing rule (at most about forty steps per section, each section authorable by one fresh
    agent) yields about eighteen to twenty sections.

  No single agent session holds that, with its impl log, through to green gates.
- **Breadth.** It touches:
  - every F# product project, for ELEV;
  - every constructor project (Domain, Storage, Optimization, Controls, Ui, App, Database, Seeding);
  - `BerremanTests`, plus a new shared test-support project;
  - EF Core;
  - two file families (library `-json`, kit `-json`);
  - five or more new UI surfaces;
  - two long acceptance tests.
- **Hard ordering with green checkpoints:**
  - PART A goes bottom-up by project reference;
  - PART B goes library data → database → kit folder → items → setups → experiments → harness promotion → studies →
    acceptance tests → online sources → planner.

  Each phase must be green before the next starts. Per-step gates, `count_at_least` baselines, the known-findings list
  and checkpoints are exactly what the arc-runner gives.
- **Destructive removals and a "reuse, never copy" rule.**
  - The removals are no-legacy: `.ocproj`, the code seeds, the in-memory seams, `MuellerDataSet`, the stub solver
    screen.
  - PART A's renames ripple across projects.

  All of this benefits from per-step critics (`reuse_critic`, `architecture_critic`) and from section invariants that
  fail the exact step that breaks them.
- **Precedent.**
  - Spec 0038, similar breadth but smaller, went through the spec-writer from a preliminary spec
    (`specs/0038/.manual/006-…`).
  - STL's ELEV rounds (STL 0193 and later) cleaned a whole tier finding by finding under the same gate.
- **What one agent could do instead:** a pilot of E1 (the solver's 115 findings). It is not recommended as a substitute,
  because the value is in the ordered whole.

## 3. Before launching (operator notes — not part of the prompt)

- **Spec id: `0048`.** We are on branch `0048`, and `specs/0048/.manual/` already holds this round's log (`001`), the
  ledger (`002`) and its tool. The spec-writer adds the rest of the folder.
- **Feature lists.** Feed `020` (revision 5) and the ledger `0048/.manual/002` as the spec's feature list(s), or rely on
  the pointers below. The prompt names both binding.
- **OneDrive.** Run `C:\GitHub\AI-Strategy-Generator\Disable-OneDrive.ps1` elevated on the arc machine before launch,
  with `-DryRun` first. OneDrive was running on this machine during this round.
- **The ELEV rulings:** `0048/.manual/002` §6, R1–R7. Answer them before launch, or let the defaults stand. The prompt
  adopts the defaults.
- **Gate cost.**
  - `unit-tests` (`BerremanTests`, about 30 min whole):
    - `runtime_estimate_seconds: 180` is far too short for a whole run, and MTP is silent for minutes between the start
      and the summary;
    - the gate now has a class selector, so a step runs only the classes it touches;
    - the prompt also asks for its `applies_to` to be narrowed;
    - either raise its deadline and `silent_for_seconds` for the final whole run, or mark the selector
      `required: true` (STL's choice for its long suites). Check `.spec-gates` before launching the arc.
  - `elevation`:
    - a change in a low project analyzes everything that references it: about 2.5–4.5 min when Domain or the solver
      changes, under a minute for a leaf;
    - `elevation-full` is 4–5 min.
- **`.user-md/Berreman/arc-runner.user-md` is stale.** Workers read it as their second prompt, and it says:
  - "There is no UI";
  - "no top-level CLAUDE.MD style guide";
  - only the `build` + `unit-tests` gates.

  Refresh it before the arc (`AGENTS.md` is the style guide now; the gates are the seven under `.gates/`).
- **Branch:** the arc runs on `0048`, or on its own branch from it.

## 4. Pointers (paste into the spec-writer's Pointers)

```
specs/0048/.manual/002-elevation-ledger.md
specs/0048/.manual/001-onedrive-mtp-and-elevation-gates-log.md
specs/0046/.manual/020-library-files-database-multi-setup-inverse.md
specs/0046/.manual/018-sharing-libraries-inverse-planner.md
AGENTS.md
naming_conventions.md
.elevation-analyzers.json
Elevation/Run-ElevationGate.ps1
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
Berreman/OpticalConstructor/OpticalConstructor.App/
Berreman/OpticalConstructor/OpticalConstructor.Database/
Berreman/OpticalConstructor/OpticalConstructor.Seeding/Seeding.fs
Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs
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

## 5. General comment (paste verbatim)

```text
Write the spec that implements, IN THIS ORDER:
- PART A: the ELEV ledger specs/0048/.manual/002-elevation-ledger.md, down to zero enforced findings
  outside the code PART B deletes;
- PART B: the feature list specs/0046/.manual/020-library-files-database-multi-setup-inverse.md
  (revision 5), in full.

BOTH ARE BINDING.
- 020's §4 lists the operator's decisions, and its §5 has no open questions; do not re-open anything 020
  decides.
- The ledger's §6 rulings R1-R7 stand at their stated defaults unless the operator's answers are attached
  to this prompt.
- 020 §0 records the code as of commit c0bfb48; the ledger records the tree of 2026-11-01. Verify each
  fact you build on against the tree; do not assume it.
- Where a document and the code disagree, the code is the baseline, and the document is the target.
- If something is genuinely ambiguous, pick the reading most consistent with the documents and AGENTS.md,
  and record it in the .spec-md problem statement. Do not ask.

GOAL
PART A: make the tree satisfy the ELEV rules (AGENTS.md "Elevate every primitive" and "The ELEV
analyzers") so that Elevation/elevation-known-findings.json shrinks to the entries of code PART B
deletes, without changing a single number any test asserts.
PART B: move the Optical Constructor from in-memory, code-seeded stores to two homes:
- SLOW data (the library) in a local SQLite database fed by typed JSON catalogue files;
- FAST data in a user-chosen experiment-kit FOLDER of plain files, used by BOTH the direct and the
  inverse constructor.
On top of that, build the data model that makes multi-sample, multi-setup inverse problems simple:
- Items: physical pieces with their own known or unknown parameters.
- Setups: items placed in slots, two or more samples in series.
- Experiments: one recorded run each, with its data files.
- Studies: many experiments fitted together, staged or joint.
Then add online material sources and the Measurement Planner, gyrotropic crystals first. Every line PART B
writes is ELEV-clean from the start, and its deletions remove their known entries; the campaign ends with
the known-findings list EMPTY.

BINDING RULES (every section)
1. AGENTS.md in full (CLAUDE.md only imports it):
   - elevate every primitive (single-case DUs with .value / tryCreate; no naked bool; no enum);
   - the ELEV rules: unique names instead of RequireQualifiedAccess; two-word DU case names; *Error
     cases end in Err / Exn; a free function of one of our types becomes its member; no wide tuples;
     elevate at origin, never at the call site; integer value tags on closed sets;
   - errors as Result values whose cases carry a reason;
   - IO only through *Proxy records built at the composition root, with in-memory stubs of the same
     shape in tests;
   - camelCase record fields; interpolated strings only; `name : Type`; braces on their own line;
   - explicit public signatures; short comments that cite the spec; no new comments in project files;
   - Release x64 only; central package versions in the root Directory.Build.targets; no nested
     Directory.Build.props; one .fsproj per folder;
   - zero warnings from our code; LF line endings;
   - never touch *.nb; reference the vendored MathNet projects; never bump any version ring.
2. The known-findings list (Elevation/elevation-known-findings.json):
   - A step that fixes a finding deletes its entry (or lowers its count) IN THE SAME STEP; a stale entry
     fails the elevation gate.
   - A step never adds an entry, never re-records the list, and never edits .elevation-analyzers.json to
     silence a finding.
   - A boundary is marked [<ElevationBoundary>] only at a genuine IO seam, with the reason beside it.
   - ELEV004 stays out of the gate command.
3. ELEV fixes change types, never numbers. BerremanTests, constructor-unit-tests, ui-smoke and ui-tests
   keep their counts and their asserted values. A rename across projects runs elevation-full. The LOWER
   project keeps a contested name (ledger §6 R2).
4. The database holds SLOW data only (020 §C): library entries with their versions, categories, the
   catalogue-import log, and pulled-source records. Kits, items, setups, experiments, studies, fit runs,
   measured data and the kit history are NEVER stored in it.
5. Kit folders (020 §E, §F): both constructors require one.
   - The kit root is FLAT. The kit file is `.kit-json`, and every kit file's kind is its extension.
   - Subfolders hold only additional data (library/, data/, .oc/).
   - Every name follows 020 §A.
6. No legacy, no migration (020 §R).
   - Delete superseded code IN THE STEP that replaces it, together with its known-findings entries.
   - No schema up-converters: a version mismatch is a typed error.
   - No EF Core migrations: EnsureCreated plus a schema stamp.
   - No compatibility shims, no fallback readers, no "keep old path as option".
7. REUSE, NEVER COPY (020 §Q, §R).
   - Every reuse 020 names is a MOVE or a call: delete at the source in the same step that adds at the
     destination.
   - A duplicated function body, a re-transcribed constant or a second parser for the same format is a
     defect. Write briefs so the reuse_critic can check it.
8. No new inverse-physics tests.
   - AT-2 replicates the existing AbsorbingBiaxialNoisyTests through the kit model.
   - The Planner's acceptance replays existing facts (020 §M).
   - BerremanTests' numbers must stay unchanged by every move.
9. Tests run on Microsoft.Testing.Platform (AGENTS.md):
   - xUnit v3 only, with xUnit v3 filters (--filter-method / --filter-trait / --filter-not-trait);
   - no VSTest package, no xUnit v2 package, no Xunit.Abstractions; skip at run time with Assert.Skip.
10. UI (AGENTS.md "Automate by meaning"):
   - every user action is a semantic message (020 §P) with stable automation ids in the one UiIds module,
     named uniquely (ledger §6 R1);
   - every new window calls WindowFit.keepOnScreen and joins ScrollExtentTests;
   - behaviour is tested in Domain first, structure in Avalonia.Headless.
11. Build machines have no OneDrive (020 §G). No test, fixture or kit lives under Documents; test kits
    and databases go under the test run's output directory.

SUGGESTED SECTIONS
You own the final plan. Each section is at most about forty steps, and dependencies point backward.
The document paragraph for each section is its spec; put that paragraph's essentials into the brief,
because mappers do not read each other's sections. Group each step by FILE: a step fixes one file's (or a
few small files') findings, deletes their entries, and runs build + elevation + the test gates of that
project selected by class.

PART A — the ELEV ledger (ledger §5, packages E1-E8; skip the files listed in ledger §4):
 A1. E1 the solver (OpticalModel, Berreman Core, OpticalProperties) and E2 Analytics:
     - physical quantities get their own types (R3), the MathNet adapter is the numeric boundary,
       [<NumericCarrier>] where a type holds primitives on purpose, and Softellect.Sys is referenced where
       an attribute is used (R4);
     - the FsxAcceptanceTests (trait fsx-acceptance) and BerremanTests keep passing unchanged.
 A2. E3 Controls:
     - UiIds flat, uniquely named, string values byte-for-byte, marked [<ElevationBoundary>] (R1);
     - the per-control State / Handlers / view / init / update renamed per surface (R2);
     - ExperimentChart, ExperimentControls, RendererControls, FacetedTreeControls.
 A3. E4 Domain: ELEV020 error-case renames first; then DispersionModels, MuellerReconstruction,
     MaterialComplexityEditor, the editors and the rest. Leave the files 020 deletes.
 A4. E5 Optimization, Storage, Seeding: elevate above the IO seam; [<ElevationBoundary>] on the seam
     itself.
 A5. E6 Ui (1,252; TableAndElementRotationView alone is 298): Elmish names per R2. Leave
     SolverHandoffView.fs and TableScene / SceneInput, which 020 replaces.
 A6. E7 TestWindows + TestWindows.App (R5: in scope, last), then E8 App (AppConfig's parse results).
     Close PART A with elevation-full green.

PART B — the feature list 020:
 1. Library entries as data files (020 §A, §B):
    - the tabulated EpsWithDispValue case; canonical JSON + SHA-256; per-kind JSON Schemas, reusing the
      ProjectJson serializer and validator stack;
    - generate Berreman/OpticalConstructor/Catalogue/Builtin/ ONCE from the code seeds (GUIDs and values
      byte-for-byte), then delete the seed values and their known entries;
    - CatalogueImport over the existing SeedingProxy shape; tests load the catalogue files.
 2. Library database (020 §C, §G):
    - replace the Test entity with the library model; EnsureCreated; the Pinned flag replacing
      VersionsInUse;
    - SQLite create for every library proxy, with contract suites run against BOTH in-memory and SQLite
      :memory:;
    - locations from appsettings.json; backup and restore; AppContext switches.
 3. Catalogue import and library-entry exchange (020 §B): import on every start by manifest hash; the
    decision table and its one-row-per-entry dialog; extra read-only catalogue folders; export to
    -json / catalogue folders (export pins); single-file drop; licence attribution display.
 4. Kit folder core (020 §E, §A):
    - KitProxy (folder + in-memory), .kit-json, atomic writes, .oc/lock, external-change check;
    - relative paths + hashes; copy-in of attached files; library snapshot + resolution on open;
    - today's experiments and scenes persist into the kit;
    - DELETE .ocproj (ProjectFile, the Project aggregate) and the Scene / ExperimentCollection seams.
 5. Start screen and history (020 §F):
    - KitHistoryProxy over the per-user preferences, REUSING RecentFiles.bump (+ star / unstar / forget /
      evict); starred kits always on top, sorted by last open date; KitHistoryLimit = 100;
    - missing-folder handling; recent.json replaced;
    - the Launcher's Main AND Inverse both go through the start screen; the Data locations panel.
 6. Send and receive kits (020 §H): KitArchiveProxy (zip honouring sendExclude, open from zip); copy
    experiments between kits with item mapping.
 7. Items and item models (020 §I): the Kit module (Item, ItemId, ItemLabel, Parameter, ParameterState),
    ItemModel library entries with generated ParameterAxis lists, dial mapping, revisions, the Items
    panel.
 8. Setups as slots and the run law (020 §J, §N):
    - N objects in series (delete measuredElementOf); a real source whose default reproduces today's
      charts;
    - the slot bar and quick swap; the grid generator;
    - the naming template, which REPLACES MuellerReconstruction.parseExperiment.
 9. Experiments v2 and data import (020 §K):
    - roles, sessions, multi-variable scans;
    - the column-mapping wizard with import profiles stored in .kit-json. Built-in profiles REUSE
      MeasuredData's parsers and MuellerDataStore.parseMuellerCsv, and a Mueller-matrix observation
      profile is added;
    - split-by-column + template match, exclusions with reasons, setup corrections with audit notes;
    - DELETE the MuellerDataSet DU and the archive-keyed MuellerDataProxy (data.zip stays as AT-1's data
      source only).
10. Promote the inverse harness and the reference pipeline (020 §Q "Reuse"):
    - MOVE into OpticalConstructor.Optimization: the bench noise model, observe / residual /
      throughput-residual, scalingAround, the fit wrappers, RecoveredQuantity / RecoveryStatistics /
      statisticsFor, predictedRelativeErrors and combinationVariance;
    - create Berreman/BerremanTestSupport/ (own folder, own .fsproj) holding BiaxialSample and
      AbsorbingBiaxialSample, referenced by BerremanTests and OpticalConstructor.Ui.Tests;
    - promote the MuellerReconstructionTests §7.2 hand-composed pipeline into product code and re-point
      §7.2 at it; move its expected literals into one shared reference module (no file read for
      expectations, the spec 0044 §8.5 decision);
    - BerremanTests stays green with unchanged numbers.
11. Studies and fitting (020 §L, §N):
    - scopes and links; sessions and gain models (MuellerReconstruction.familyGain becomes their first
      implementation, not a parallel one);
    - staged and joint fits; automatic engine choice, REUSING buildDesign / reconstruct and InverseFit /
      MuellerInverseSolver over MuellerInverse.forwardModels;
    - generated axes; append-only fit runs; results (FitQuality reused), realizability, product check,
      profile likelihood;
    - publish / promote-to-library; the study view;
    - DELETE the SolverHandoffView stub, reusing its checks as pre-fit validation.
12. The two acceptance tests (020 §Q):
    - AT-1 optics-mueller and AT-2 AbsorbingBiaxialNoisyTests, built ONLY through semantic messages,
      with the folder round-trip (write, SendKit, unzip, reopen against an EMPTY library, compare the kit
      and the start-point residual);
    - AT-2's five-seed ensemble is xUnit v3 Explicit;
    - add the gate descriptor .gates/OpticalConstructor/kit-acceptance.gates in the shape of the other
      MTP test gates (--filter-trait "Category=kit-acceptance", TRX result_capture, class selector,
      phase verify, requires_gates [build]), and add --filter-not-trait "Category=kit-acceptance" to
      ui-tests.
13. Online material sources (020 §D): MaterialSourceProxy. First refractiveindex.info (offline snapshot +
    update, REUSING MaterialImport as the record parser); then PTB OCDB with uncertainties; then the
    Henke compound calculator. Provenance, licence, new version on update.
14. Measurement Planner, gyrotropic crystals first (020 §M):
    - inputs from the kit;
    - layer 1 symmetry table;
    - layer 2: Fisher information / rank / confounded combinations / Cramér–Rao, REUSING
      FitQuality.residualJacobian, MuellerInverse.jacobianColumnNorms and the promoted
      predictedRelativeErrors;
    - greedy D-optimal ranking; the plan emitted as Planned setups and experiments in the kit; the cache
      in .oc/planner/;
    - acceptance = the existing MuellerInverseTests / BiaxialInverseTests facts listed in 020 §M.

SECTION INVARIANTS
Declare these on every section AFTER the one that establishes the rule. They run after every step, so
never declare one on the section that makes it true. Commands contain no backslash; use -F for literal
text. The known-findings list names a file only in its "file" field, so a path grep sees only entries.
- after A1: ASSERT_INVARIANT elev-solver-clean must-find-nothing: git grep -n -F -e "Berreman/Berreman/" -e "Berreman/OpticalModel/" -e "Berreman/OpticalProperties/" -e "Berreman/Analytics/" -- Elevation/elevation-known-findings.json
- after A2: ASSERT_INVARIANT elev-controls-clean must-find-nothing: git grep -n -F "/OpticalConstructor.Controls/" -- Elevation/elevation-known-findings.json
- after A6: ASSERT_INVARIANT elev-heads-clean must-find-nothing: git grep -n -F -e "/OpticalConstructor.TestWindows/" -e "/OpticalConstructor.TestWindows.App/" -e "/OpticalConstructor.App/" -e "/OpticalConstructor.Optimization/" -- Elevation/elevation-known-findings.json
- after 1:  ASSERT_INVARIANT no-code-seeds must-find-nothing: git grep -n -E "let (builtInEntries|seedEntries|standardCategories) " -- Berreman/OpticalConstructor/
- all:      ASSERT_INVARIANT no-kit-dot-kit must-find-nothing: git grep -n -F "kit.kit-json" -- Berreman/
- all:      ASSERT_INVARIANT no-vstest must-find-nothing: git grep -n -E "Microsoft.NET.Test.Sdk|xunit.runner.visualstudio|Xunit.SkippableFact|IsTestingPlatformApplication" -- "*.fsproj" "*.props" "*.targets"
- after 4:  ASSERT_INVARIANT no-ocproj must-find-nothing: git grep -n -i -F "ocproj" -- Berreman/OpticalConstructor/
- after 8:  ASSERT_INVARIANT no-name-regex must-find-nothing: git grep -n -F "experimentRegex" -- Berreman/
- after 9:  ASSERT_INVARIANT no-dataset-du must-find-nothing: git grep -n -F "MuellerDataSet" -- Berreman/
- after 10: ASSERT_INVARIANT harness-moved must-find-nothing: git grep -n -F "let noisyObserveAbsoluteWith" -- Berreman/BerremanTests/
Add similar ones wherever a brief says "nothing may name X".

GATES (the spec's .spec-gates)
- build: every step.
- elevation: every step that edits an .fs or .fsproj. elevation-full: every step that renames across
  projects or edits .elevation-analyzers.json, the last step of PART A, and the final step.
- constructor-unit-tests, ui-smoke, ui-tests: as declared under .gates/, selected by class
  (gate_selection) on the steps that touch their code; whole on the last step of each section.
- unit-tests (BerremanTests) takes about 30 minutes whole (specs/0044/.manual/020-handover.md):
  - select it by class on every step that touches what it covers;
  - narrow its applies_to to the code BerremanTests actually covers: Berreman/BerremanTests/**,
    Berreman/BerremanTestSupport/**, Berreman/Berreman/**, Berreman/OpticalProperties/**,
    Berreman/Analytics/**, Berreman/OpticalConstructor/OpticalConstructor.Optimization/**, and the
    inverse Domain files MuellerInverse.fs and MuellerReconstruction.fs;
  - it then runs in A1, A3, A4 and sections 10-11, and wherever those files change, not on every UI step;
  - run it whole only on the final step, with a deadline and silent_for_seconds that fit 30 minutes.
- kit-acceptance (new, section 12): applies to the Study / run-law / import code and the acceptance test
  files.
Keep count_at_least baselines non-regressing. A test that moves between projects counts in its new
project in the same step.

ACCEPTANCE (the spec is done when)
- Elevation/elevation-known-findings.json lists no finding, and elevation-full is green.
- AT-1 and AT-2 pass under kit-acceptance, with the assertions listed in 020 §Q.
- The Planner reproduces the 020 §M facts.
- Every gate is green with zero warnings from our code.
- BerremanTests' numbers are unchanged.
- The code no longer contains the 020 §R removals.
- The app persists the library in the database and everything else in kit folders.

OUT OF SCOPE
- ELEV004 (the by-hand round after the ledger, R7) and any change to the analyzers or to STL;
- version bumps of any ring;
- new inverse-physics tests;
- the public MathNet NuGet;
- a WinForms port (but keep UI logic in pure Domain / State, as AGENTS.md requires);
- cloud or shared databases;
- Bayesian sequential design (the 020 §M "later");
- shipped planner playbooks.
```
