# 021 — Spec-writer → arc-runner: the verdict and the chunked spec-writer prompt

## 1. Verdict: this is a spec-writer → arc-runner cycle (a sectioned / chunked campaign), not a task for one agent

- **Size.**
  - `020` is a feature list of about 57 KB.
  - Its phasing (§S) has ten phases. Realistically that is 250–400 steps.
  - The section planner's sizing rule (at most about forty steps per section, each section authorable by one fresh agent) yields about fourteen sections.

  No single agent session holds that, with its impl log, through to green gates.
- **Breadth.** It touches:
  - every constructor project (Domain, Storage, Optimization, Controls, Ui, App, Database, Seeding);
  - `BerremanTests`, plus a new shared test-support project;
  - EF Core;
  - two file families (library `-json`, kit `-json`);
  - five or more new UI surfaces;
  - two long acceptance tests.
- **Hard ordering with green checkpoints:** library data → database → kit folder → items → setups → experiments → harness promotion → studies → acceptance tests → online sources → planner. Each phase must be green before the next starts. Per-step gates, `count_at_least` baselines and checkpoints are exactly what the arc-runner gives.
- **Destructive, no-legacy removals** (`.ocproj`, the code seeds, the in-memory seams, `MuellerDataSet`, the stub solver screen) and the **"reuse, never copy"** rule. Both benefit from per-step critics (`reuse_critic`, `architecture_critic`) and from section invariants that fail the exact step that breaks them.
- **Precedent.**
  - Spec 0038, similar breadth but smaller, went through the spec-writer from a preliminary spec (`specs/0038/.manual/006-…`).
  - Spec 0044 was a direct agent, but it was one physics problem with an impl log, which this is not.
- **What one agent could do instead:** a pilot of phase 1 only (library entries as data files). It is not recommended, because the value is in the ordered whole and the later phases constrain the earlier ones' shapes.

## 2. Before launching (operator notes — not part of the prompt)

- **Spec id.** The recent specs step by two (0038, 0040, 0042, 0044, 0046), so the suggestion is **`0048`**; `0047` works as well. The spec-writer creates the folder.
- **Feature list.** Either feed `020` as the spec's feature list (copy it to `specs/<id>/.feature-list-md` and include it), or rely on the pointer below. The prompt names it binding either way.
- **Gate cost.**
  - `unit-tests` (`BerremanTests`) now takes about **30 min**: 203 tests per `specs/0044/.manual/020-handover.md`. Both `CLAUDE.md`'s "~13 minutes" and the gate's `runtime_estimate_seconds: 180` are stale.
  - Its `applies_to` includes `Berreman/**/*.fs`, so as configured it runs on EVERY step of the arc — at 250+ steps, days of gate time.
  - The prompt asks the spec-writer to narrow it in the spec's gate plan. Check `.spec-gates` before launching the arc.
- **`.user-md/Berreman/arc-runner.user-md` is stale.** Workers read it as their second prompt, and it says:
  - "There is no UI";
  - "no top-level CLAUDE.MD style guide";
  - only the `build` + `unit-tests` gates.

  Refresh it before the arc. I did not change it; you said to make no other changes.
- **Branch:** the arc runs on its own branch from `master` after 0046 merges, or from `0046`, whichever you prefer.

## 3. Pointers (paste into the spec-writer's Pointers)

```
specs/0046/.manual/020-library-files-database-multi-setup-inverse.md
specs/0046/.manual/018-sharing-libraries-inverse-planner.md
CLAUDE.md
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

## 4. General comment (paste verbatim)

```text
Write the spec that implements the feature list
specs/0046/.manual/020-library-files-database-multi-setup-inverse.md (revision 4) in full.

020 IS BINDING. Its §4 lists the operator's decisions, and §5 has no open questions; do not re-open
anything 020 decides. 020 §0 records the code as of commit c0bfb48. Verify each fact you build on
against the tree; do not assume it. Where 020 and the code disagree, the code is the baseline, and 020
is the target. If something is genuinely ambiguous, pick the reading most consistent with 020 and
CLAUDE.md and record it in the .spec-md problem statement. Do not ask.

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
1. CLAUDE.md in full:
   - elevate every primitive (single-case DUs with .value / tryCreate; no naked bool; no enum);
   - errors as Result values whose cases carry a reason;
   - IO only through *Proxy records built at the composition root, with in-memory stubs of the same
     shape in tests;
   - camelCase record fields; interpolated strings only; `name : Type`; braces on their own line;
   - explicit public signatures;
   - Release x64 only; central package versions in the root Directory.Build.targets; no nested
     Directory.Build.props; one .fsproj per folder;
   - zero warnings from our code; LF line endings;
   - never touch *.nb; reference the vendored MathNet projects; never bump any version ring.
2. The database holds SLOW data only (020 §C): library entries with their versions, categories, the
   catalogue-import log, and pulled-source records. Kits, items, setups, experiments, studies, fit runs,
   measured data and the kit history are NEVER stored in it.
3. Kit folders (020 §E, §F): both constructors require one.
   - The kit root is FLAT. The kit file is `.kit-json`, and every kit file's kind is its extension.
   - Subfolders hold only additional data (library/, data/, .oc/).
   - Every name follows 020 §A.
4. No legacy, no migration (020 §R).
   - Delete superseded code IN THE STEP that replaces it.
   - No schema up-converters: a version mismatch is a typed error.
   - No EF Core migrations: EnsureCreated plus a schema stamp.
   - No compatibility shims, no fallback readers, no "keep old path as option".
5. REUSE, NEVER COPY (020 §Q, §R).
   - Every reuse 020 names is a MOVE or a call: delete at the source in the same step that adds at the
     destination.
   - A duplicated function body, a re-transcribed constant or a second parser for the same format is a
     defect. Write briefs so the reuse_critic can check it.
6. No new inverse-physics tests.
   - AT-2 replicates the existing AbsorbingBiaxialNoisyTests through the kit model.
   - The Planner's acceptance replays existing facts (020 §M).
   - BerremanTests' numbers must stay unchanged by every move.
7. UI (CLAUDE.md "Automate by meaning"):
   - every user action is a semantic message (020 §P) with stable automation ids in the one UiIds module;
   - every new window calls WindowFit.keepOnScreen and joins ScrollExtentTests;
   - behaviour is tested in Domain first, structure in Avalonia.Headless.

SUGGESTED SECTIONS
You own the final plan. Each section is at most about forty steps, and dependencies point backward.
The 020 paragraph for each section is its spec; put that paragraph's essentials into the brief, because
mappers do not read each other's sections.
 1. Library entries as data files (020 §A, §B):
    - the tabulated EpsWithDispValue case; canonical JSON + SHA-256; per-kind JSON Schemas, reusing the
      ProjectJson serializer and validator stack;
    - generate Berreman/OpticalConstructor/Catalogue/Builtin/ ONCE from the code seeds (GUIDs and values
      byte-for-byte), then delete the seed values;
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
    - add the gate descriptor .gates/OpticalConstructor/kit-acceptance.gates (Category=kit-acceptance)
      and exclude that category from the ui-tests filter.
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
dots.
- after 1:  ASSERT_INVARIANT no-code-seeds must-find-nothing: git grep -n -E "let (builtInEntries|seedEntries|standardCategories) " -- Berreman/OpticalConstructor/
- all:      ASSERT_INVARIANT no-kit-dot-kit must-find-nothing: git grep -n -F "kit.kit-json" -- Berreman/
- after 4:  ASSERT_INVARIANT no-ocproj must-find-nothing: git grep -n -i -F "ocproj" -- Berreman/OpticalConstructor/
- after 8:  ASSERT_INVARIANT no-name-regex must-find-nothing: git grep -n -F "experimentRegex" -- Berreman/
- after 9:  ASSERT_INVARIANT no-dataset-du must-find-nothing: git grep -n -F "MuellerDataSet" -- Berreman/
- after 10: ASSERT_INVARIANT harness-moved must-find-nothing: git grep -n -F "let noisyObserveAbsoluteWith" -- Berreman/BerremanTests/
Add similar ones wherever a brief says "nothing may name X".

GATES (the spec's .spec-gates)
- build, constructor-unit-tests, ui-smoke, ui-tests: as declared under .gates/.
- unit-tests (BerremanTests) takes about 30 minutes (specs/0044/.manual/020-handover.md). Narrow its
  applies_to to the code BerremanTests actually covers: Berreman/BerremanTests/**,
  Berreman/BerremanTestSupport/**, Berreman/Berreman/**, Berreman/OpticalProperties/**,
  Berreman/OpticalConstructor/OpticalConstructor.Optimization/**, and the inverse Domain files
  MuellerInverse.fs and MuellerReconstruction.fs. That way it runs in sections 10–11 and wherever those
  files change, not on every UI step. The build gate still compiles BerremanTests on every step.
- kit-acceptance (new, section 12): applies to the Study / run-law / import code and the acceptance test
  files.
Keep count_at_least baselines non-regressing. A test that moves between projects counts in its new
project in the same step.

ACCEPTANCE (the spec is done when)
- AT-1 and AT-2 pass under kit-acceptance, with the assertions listed in 020 §Q.
- The Planner reproduces the 020 §M facts.
- Every gate is green with zero warnings from our code.
- BerremanTests' numbers are unchanged.
- The code no longer contains the 020 §R removals.
- The app persists the library in the database and everything else in kit folders.

OUT OF SCOPE
- version bumps of any ring;
- new inverse-physics tests;
- the public MathNet NuGet;
- a WinForms port (but keep UI logic in pure Domain / State, as CLAUDE.md requires);
- cloud or shared databases;
- Bayesian sequential design (the 020 §M "later");
- shipped planner playbooks.
```
