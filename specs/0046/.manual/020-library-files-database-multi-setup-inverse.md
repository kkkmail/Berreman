# 020 — Library files, the local database, one-file exchange, and multi-setup inverse experiments (feature list)

Research and proposal only. No code was changed. Inputs:
- `019-handover.md`, `002-plan.md`, `018-sharing-libraries-inverse-planner.md`;
- the constructor code as it stands at `c0bfb48`;
- the Mueller experiment in `C:\GitHub\optics-mueller` (`AGENTS.md`, `notes/*`,
  `analysis/matrix_solution/*`);
- specs 0038 (§13–16), 0042 and 0044;
- a web survey (sources at the end).

## Short answers to the questions asked

- **Where the library lives:** in JSON files, typed by extension (the STL/ASG convention, e.g. `.contracts-json`):
  - `*.material-json`, `*.sample-json`, `*.source-json`, `*.polarizer-json`, `*.detector-json`;
  - `*.category-json`, `*.kit-json`;
  - one `catalogue.manifest-json` per catalogue folder.
- **What the JSON files are for:** the shipped catalogue, and the format of any single exchanged entry. The app's working copy is not JSON.
- **When the files are imported:** at **every** start. A manifest-hash comparison makes this cheap, and an actual import runs only when a catalogue changed. Built-in entries are never edited in place: an import mints a new version, and it never touches user entries.
- **The local database:** yes — **SQLite** through EF Core, which the repo already pins and has proven (`OpticalConstructor.Database`). It is the single local store for:
  - materials, samples, sources, polarizers and detectors, with their full version history;
  - the kit (physical items);
  - setups, experiments and studies;
  - the measured data files themselves, stored by content hash;
  - fit results.

  Every existing in-memory proxy gets its promised disk-backed `create`.
- **Where the data lives:**
  - the database: `%LOCALAPPDATA%\Softellect\OpticalConstructor\` (local, never synced);
  - user-visible output: `Documents\Optical Constructor\`, i.e. exports (already there) and bundles.

  Do not put the SQLite file in Documents: Windows 11 OneDrive Known Folder Move redirects Documents into a sync folder, and SQLite over sync or network file systems is a known corruption path.
- **Exchange:** one file, the bundle from `018` (working name `.ocbundle`; the extension is still the operator's call). It is a zip of `-json` entries plus the raw data files plus a hashed manifest. The bundle is **not** a copy of the database, so the database schema stays free to migrate.
- **The inverse problem:** the missing concepts are:
  - the **Kit**: the physical pieces the user owns, each with its OWN parameters, known or unknown;
  - **Setups**: which pieces sit in which slot on the beam, two or more samples allowed;
  - the **Study**: many experiments over one kit, fitted together.

  An unknown such as the retardance of CPL1 belongs to the PIECE. It is therefore shared automatically by every experiment that uses that piece. That one rule is what makes "swap the pieces, attach a file to each run, fit everything together" work.

---

## 0. What the app does today (the evaluation)

- **Nothing persists.**
  - Every store is in memory, and every start re-seeds (`Berreman/OpticalConstructor/OpticalConstructor.Ui/AppContext.fs:69`).
  - `.ocproj` exists (`OpticalConstructor.Storage/ProjectFile.fs`) but no window saves or opens it.
  - `Scene.SceneProxy` and `ExperimentCollectionProxy` are in-memory seams that wait for a disk `create`.
  - The EF Core project holds one `Test` table that nothing references.
  - `OpticalConstructor.Seeding` pushes the code seeds through `SeedingProxy` into in-memory stores only.
- **The library is code.**
  - `MaterialLibrary.builtInEntries`, `Library.SeedSamples`, `SeedSources`, the polarizer and detector presets in `Library.seedEntries` (`OpticalConstructor.Domain/ElementId.fs:856`), and `standardCategories`.
  - Their GUIDs are frozen as future foreign keys, which is good and must survive the move to files.
  - Material physics is already data (`MaterialComplexity`), with one exception: **tabulated imports are closures** (`complexity = None`, `OpticalConstructor.Storage/MaterialImport.fs:97`), so they cannot be stored as they are.
- **An Experiment** (`OpticalConstructor.Domain/Experiments.fs:213`) is one frozen snapshot of the table:
  - one varied element with one variable over one range;
  - one capture mode;
  - **one** data file, stored as a path that breaks on another machine.
- **Table elements are "kind + library entry".**
  - The table has no notion of *this particular* CPL1, so two LPs of the same preset are indistinguishable.
  - Polarizers are ideal or constant-Mueller presets (`ElementId.fs:304`). Nothing parameterised exists, such as an LP + retarder with unknown δ and axis.
- **One sample per run.**
  - The measured element is "the first element bound to a sample" (`OpticalConstructor.Ui/TableAndElementRotationView.fs:1339`).
  - The run law lets the first polarizer set a unit-intensity state (`OpticalConstructor.Domain/ExperimentRun.fs:36`), so neither the source's own polarization nor an absolute scale can enter a fit.
- **There is no inverse solve in the UI.** The Inverse constructor ends at the stub `SolverHandoffView` ("the actual inverse solver runs separately").
- **The Mueller pipeline exists, but outside the model.**
  - `MuellerReconstruction` (spec 0042) reproduces the optics-mueller fit: families, dial signs, CPL model, dark and gain blocks, a 16-parameter linear least-squares fit.
  - It does so by **regex over experiment names** (`MuellerReconstruction.fs:400`, `CPL-(QZ#45-LR#45)-CPL`) and a hard-coded `MuellerDataSet` DU over a zip next to the executable.
  - `MuellerInverse` (spec 0044) is the multi-cut joint fit of material constants.

  Neither is reachable from the constructor. The features below generalise what both hard-code.
- **The data-file schemas are too narrow for real data.**
  - Intensity is strict `X,Y`.
  - Ellipsometric files must be `wavelength_nm,psi_deg,delta_deg`.
  - A real lab file is one CSV with many runs keyed by a column (`experiment`), a raw dial reading (`description`), a timestamp (`captured_at`) and several signal columns (`avg_R`, `avg_total`).

## 1. The reference case — the optics-mueller experiment, as the model must see it

- **Kit:**
  - a red laser (the source; nearly linear, intensity drifts);
  - lens and diaphragm (passive, ignored by the optics);
  - LP1 (source) and LP2 (analyzer);
  - CPL1 (source, LP → λ/4) and CPL2 (analyzer, λ/4 → LP). Each is "LP + retarder" with unknown retardance δ (fitted ≈ 80–84°, not 90°) and, for CPL1, an unknown retarder-axis offset (fitted 39.3° instead of 45°);
  - two objects: QZ (quartz) and LR (red Lego glass), each an unknown 4×4 Mueller matrix;
  - the phone (an intensity detector with a dark offset and a gain that jumps between session blocks).
- **Setups:**
  - source slot ∈ {LP1, CPL1} × analyzer slot ∈ {LP2, CPL2}, giving four families;
  - object slot 1 / slot 2 ∈ {empty (AIR), QZ, LR, QZ then LR};
  - object rotation φ ∈ {0, +45, +90}.
- **One experiment** is one analyzer-dial sweep, 0…360° every 20°, sometimes with a repeated first point. It records the raw dial reading, and the dial-to-physical mapping (zero, sign) differs between LP and CPL.
- **Diagnostics:**
  - Darkness (dark level);
  - AIR repeats (`-BR`, `-2R`) as drift checks;
  - "BullshitCheck" (laser off and on), whose timestamp splits a gain block;
  - lamp tests, ignored.
- **Corrections after the fact:**
  - LR was labelled +45/+90 but physically sat at −45/−90 — the setup is fixed, the data is not;
  - one contaminated frame is excluded with a reason;
  - the gain rules differ per family.
- **Unknowns:**
  - CPL1 (2), CPL2 (1–2), the LP2 and CPL2 dial zeros;
  - M_QZ (16) and M_LR (16);
  - about five per-block gains and one dark level.
- **Fit:** two-stage in the repo (AIR calibrates the pieces, then a linear fit per object), then a check that the product M_LR · M_QZ predicts the combined runs.
- **What the app must therefore do:**
  - hold the kit;
  - build about 40 setups by swapping pieces;
  - attach four CSVs that each feed many experiments;
  - tag diagnostics;
  - declare unknowns on pieces, sessions and objects;
  - fit jointly or in stages;
  - publish calibrated pieces back to the kit for the next study.

## 2. Best practice and similar software (web survey, short)

- **Global or co-refinement with shared parameters** is the standard answer to "combine experiments":
  - *Origin and GraphPad Prism*: a shared parameter gets one value fitted from all data sets, which also changes the reported errors.
  - *refnx*: `GlobalObjective` over per-data-set objectives. Sharing works by REUSING the same parameter object across models; that is the "parameter belongs to the piece" rule above.
  - *SasView*: simultaneous fit with constraints that link models; per-data-set weights since v6.
  - *Woollam*: multi-sample analysis reduces parameter correlation, and CompleteEASE has explicit parameter coupling.
- **Polarimeter calibration with imperfect, unknown elements.**
  - The Eigenvalue Calibration Method (Compain, Poirier & Drévillon 1999) finds the generator and analyzer matrices (W, A; 32 coefficients) from three or four reference samples (air, polarizers at known nominal angles, a retarder). It assumes nothing about the elements except completeness and needs no precise alignment.
  - Dual-rotating-retarder calibration (Goldstein & Chipman; Chenault et al.) fits retardance and orientation errors, because errors under 1° give over 10 % matrix error.
  - Hence: "calibration experiments" are a first-class role, and piece parameters are fit targets.
- **Physical realizability** of a fitted Mueller matrix (the cone of Mueller matrices, coherency eigenvalues ≥ 0) is a standard post-fit check.
- **Library and catalogue handling (Zemax OpticStudio):**
  - catalogues are plain-text AGF files in `Documents\Zemax\Glasscat`, with a user-settable path and per-project catalogue folders;
  - its own guidance warns that a data folder on a network or cloud drive breaks offline use;
  - the `.zar` archive is its single-file exchange (see `018`).
- **SQLite as a document format.**
  - SQLite's own case for using a database file as the application file format: atomic, queryable, one file.
  - The same project's network caveats: file locking misbehaves on network and sync file systems; WAL needs shared memory.
  - Forum reports (Tropy, RootsMagic, Sonarr) show real corruption from OneDrive-synced databases.
  - Conclusion: a local SQLite database is the right working store, but not the exchange file and not inside a synced folder.

---

## 3. Feature list

### A. The library as external files (update without recompiling)

- **One entry per file**, in a folder tree per catalogue. It diffs, reviews and merges in git, and a single entry can be emailed or dropped in.
- **Typed extensions (the STL/ASG `-json` convention):**
  - `.material-json`, `.sample-json`, `.source-json`, `.polarizer-json`, `.detector-json`, `.category-json`;
  - `.kit-json` (a lab inventory; §E) and `.setup-json` (§F);
  - `catalogue.manifest-json` (name, catalogue id, catalogue version, list of entry hashes).

  Content is plain JSON; the extension names the schema.
- **Every entry file carries:**
  - `schema` (kind + schema version), `id` (the frozen GUID), `version`;
  - `name`, `category` / facets, `description`;
  - `payload` (the elevated domain value, e.g. `MaterialComplexity`);
  - `provenance` (built-in / user / imported-from-bundle / pulled-from-source, with citation and licence — see `018` §2);
  - `contentHash` (SHA-256 over canonical JSON: sorted keys, invariant-culture numbers).
- **JSON Schema per kind**, validated on load. The storage layer already uses System.Text.Json + FSharp.SystemTextJson + JsonSchema.Net for `.ocproj`, so reuse that stack.
- **Schema evolution:**
  - each kind has pure up-converters vN → vN+1;
  - a file newer than the app is rejected with a typed `UnsupportedSchemaVersion` error, never a crash.
- **Physics must be pure data before it can leave the code.**
  - Add a **tabulated** `EpsWithDispValue` case (λ, n, k arrays + interpolation rule) so refractiveindex.info tabulated imports become storable and versionable instead of closure-backed `complexity = None`.
  - Any entry that cannot serialise is a build-time test failure.
- **Shipped catalogue:**
  - `<install>\Catalogue\Builtin\…` is read-only, copied by the build from a repo folder (e.g. `Berreman/OpticalConstructor/Catalogue/`).
  - The code seeds are exported ONCE to these files by a throw-away tool, keeping GUIDs and values byte-for-byte. Then the Domain seed values are **deleted** so there is one source of truth.
  - Tests load the same files (a test fixture reads the repo catalogue folder).
- **Import on every start (cheap):**
  - compare each catalogue's manifest hash with the one recorded in the database;
  - import only when it changed;
  - log the decisions.
- **Per-entry import decision** (one rule for catalogues AND bundles; the `018` table):

  | Incoming | Action |
  |---|---|
  | New id | Add |
  | Same id, same hash | Skip |
  | Same id, new hash | Mint the next version of that entry; for a protected built-in, the new version becomes the latest built-in |
  | Entry removed from the catalogue | Mark `InactiveEntry` (still resolvable) |

  Never delete, never touch user entries. Experiments pinned to an old version keep it.
- **Extra catalogue sources:**
  - "Add catalogue folder…" registers a read-only folder: a lab share, OneDrive, or a git checkout;
  - this is the shared-folder library from `018`;
  - JSON on a synced folder is safe because the app only reads it;
  - the database never lives there.
- **Export:**
  - any entry → its `-json` file;
  - a selection or a whole category → a catalogue folder with a manifest.

  This is how a curated user library becomes a shipped one.
- **`SeedingProxy` / `seedAll` become `CatalogueImport`:** the same seam shape, fed from parsed files instead of Domain values. The existing step-042 in-memory proof stays as the test double.

### B. The local database (SQLite via EF Core)

- **One database file** holds everything the user creates:
  - all library kinds, with their full version history and lifecycle;
  - categories and the kit;
  - setups, experiments and studies;
  - attached data blobs, fit results;
  - the catalogue-import log and the recent-files store.
- **Hybrid shape:**
  - relational columns for what is queried or constrained: id, version, kind, name, lifecycle, protection, facet keys, foreign keys (sample-version → material-version, experiment → setup-version → item-version);
  - the elevated domain value as a schema-versioned JSON payload column, using the same serializer as §A.

  This avoids mapping deep F# DU trees onto an ORM, and keeps one canonical JSON form per entity — the same bytes the exchange writes.
- **Versions are immutable rows.**
  - "Edit latest" follows the existing `Lifecycle.decideVersioning` rule.
  - `VersionsInUse` becomes truthful at last: it is a query over persisted experiments and studies, not `VersionsInUse.empty`.
- **Measured data is stored IN the database**, content-addressed: SHA-256, the original file name, the import time, and the raw bytes.
  - Experiments reference the blob by hash, never by path.
  - Re-attaching the same file is free.
  - The original path is kept only as a hint.
- **Every existing proxy keeps its shape and gains a SQLite-backed `create`:**
  - `MaterialProxy`, `CategoryProxy`, `SampleProxy`, `SourceProxy`;
  - the `LibraryProxy` live view, `SceneProxy`, `ExperimentCollectionProxy`, `ExperimentDataProxy`;
  - plus the new `KitProxy`, `SetupProxy` and `StudyProxy`.

  The composition root (`AppContext.create`) is the only place that changes.
- **Contract tests:** one test suite per proxy runs against BOTH the in-memory store and SQLite in `:memory:` mode, so the two cannot drift.
- **Migrations:**
  - EF Core migrations replace the `Test` table;
  - migrations apply at start, after an automatic backup (`VACUUM INTO` a dated copy, the last N kept);
  - a database with a newer schema than the app opens read-only with a typed error.
- **Settings and safety:**
  - the connection string comes from `appsettings.json` (the elevated `ConnectionString` already exists in `AppConfig.fs`);
  - WAL journal mode, since the file is local only;
  - one writer (the app); a second instance opens read-only or refuses with a clear message;
  - a menu item "Back up database now…" and "Restore from backup…".

### C. Where the data lives

- **Database:** `%LOCALAPPDATA%\Softellect\OpticalConstructor\opticalconstructor.db`, with the backups beside it in `Backups\`. Local and machine-specific, never redirected by OneDrive KFM.
- **User files:** `Documents\Optical Constructor\`, with `Exports\` (already used by `ExperimentExportFile`), `Bundles\` (default save and open folder), and `Catalogues\` (user-exported catalogues).
- **Shipped catalogue:** `<install>\Catalogue\` (read-only).
- **Every location is overridable** in `appsettings.json`, for a portable install or a lab machine.
- **A "Data locations" panel in the Launcher** shows the paths, with an "Open folder" button for each.
- Note on `CLAUDE.md`: its "never `%LOCALAPPDATA%`" rule covers arc-runner WORKER output, not the product's data. This feature does not conflict with it.

### D. Exchange — one file

- **`.ocbundle`**: a zip (System.IO.Compression) holding `bundle.manifest-json` + `-json` entries + `data/<sha256>.<ext>` raw files.
  - The manifest holds the schema version, app version, author, date, the root object, and a hash for every entry and blob.
  - Contents follow `018` §1.
- **What can be exported:** any root — one material, one sample, a kit, a setup, a study (with its experiments, data and fit results), or a whole catalogue.
- **The dependency closure is always embedded,** built-ins included, because the receiver may run another catalogue version:
  - study → experiments → setups → kit items → samples → material versions → categories → data blobs;
  - for a study, also its unknowns, fit runs and notes.
- **Import:**
  - one dialog with one row per entry: status, default action, and an override (the `018` decision table);
  - a name-only collision imports side by side as "Si — from A", with an explicit "use my Si instead" re-binding and a diff preview;
  - nothing is ever overwritten silently.
- **A single `-json` file is also importable** (drag and drop onto the Library or Kit window), through the same decision table.
- **Review round-trip:** notes and comments on experiments and fit results travel in the bundle; B's reply is a new bundle whose entries are new versions.
- **Why not ship the SQLite file:** it ties exchange to the internal schema, carries the sender's unrelated data, cannot be partly imported, and is unsafe to pass through sync folders. The bundle is small, inspectable after unzipping, and stable across database migrations.

### E. The Kit — physical items with their own parameters (new data model)

- **`Kit`** = a named, versioned lab inventory: `KitId`, name, notes, `items : KitItem list`. A user may hold several (e.g. "Home bench", "Lab B ellipsometer").
- **`KitItem`** = one PHYSICAL piece:
  - a `KitItemId` (GUID) and a short `ItemLabel` (`LP1`, `CPL2`, `QZ`, `Phone`);
  - the library model it is an instance of (`ItemModel` below, carrying library version refs);
  - its own `ItemParameter` values;
  - optional photo or notes.

  Two LPs of the same library preset are two items with independent parameters.
- **`ItemModel`** — parameterised physics as data. Each model declares its parameter axes, so the fitter's `ParameterAxis` list is generated rather than hand-written per problem.

  | Model | Parameters |
  |---|---|
  | Light source | λ, or tunable limits (exists) + an output polarization state: azimuth, ellipticity, degree of polarization + an intensity scale. Lets "the laser is almost linear" be modelled, or absorbed into the first polarizer as today |
  | Ideal LP / ideal CP | None (exists) |
  | Real linear polarizer | Extinction ratio or diattenuation, axis offset |
  | Retarder plate | Retardance δ (optionally dispersive), fast-axis offset |
  | Compound in light order | E.g. **CPL** = `LP · Retarder(axis = 45° + Δ, δ)` — "LP-CP of unknown CP rotation" is this item with Δ and δ unknown. Generalises the existing `ConstantMueller` component list with parameters |
  | Sample by stack | The existing `Sample`: materials + thicknesses + cut + orientation. Its unknowns can be any thickness, orientation, or a parameter of a material it uses |
  | Sample as black-box Mueller matrix | 16 parameters (or 15 + scale) at its reference orientation — QZ and LR here. Rotation enters as `R(−φ) M R(φ)` |
  | Empty slot (air) | Identity, the calibration reference |
  | Intensity detector | Gain (per session block — see scope), dark offset, optional noise model |
  | Ellipsometer | (exists) Plus optional calibration offsets |

- **`ItemParameter`** = a named value with a **state**:
  - `Known of value`;
  - `Unknown of start * bounds * prior option`;
  - `Calibrated of value * sigma * fromStudy : StudyId * at : DateTimeOffset`.
- **Dial mapping:** a rotatable item carries a `DialMapping`, i.e. a zero offset and a sign (`DialSign` exists in `MuellerReconstruction`). Experiments record the RAW dial reading, as the lab does. The dial zero is an ordinary parameter (LP2's zero 155.44° raw was fitted).
- **Items are versioned** like samples. A calibration published from a study mints the item's next version, so older studies keep the values they were fitted with.
- **Material parameters as unknowns:**
  - a study may "open" a material version, which creates a study-local working copy whose chosen components are `Unknown`;
  - every sample of the study made of that material shares them — this is the 0044 multi-cut case (three cuts, one crystal);
  - publishing mints the material's next version, with provenance "fitted in study S".

### F. Setups, slots and quick swap

- **`Setup`** = an ordered optical path of **slots**, each holding a kit item or nothing:
  - Source, Prep 1…k (polarizers or compensators before the objects), Object 1…n (**two or more samples in series** are allowed — "pick two samples for the experiment"), Analyzer 1…k, Detector;
  - per slot: the item's placement (position on the beam, R1/R2/R3, or a dial reading).

  The table drawing renders the active setup, and the existing `BeamLayout` / `RayModel` laws are unchanged.
- **Setups are first-class and versioned** (`SetupId`). An experiment REFERENCES a setup version instead of embedding a frozen descriptor list. The existing `ElementDescriptor` list becomes the setup's slot list.
- **Quick swap (the lab gesture):**
  - a slot bar above the table shows one chip per slot (`Source: Laser | Prep: CPL1 ▾ | Obj 1: QZ ▾ @ 45° ▾ | Obj 2: LR ▾ @ 45° ▾ | Analyzer: CPL2 ▾ | Detector: Phone`);
  - a chip's dropdown lists only kit items compatible with that slot;
  - choosing one swaps the piece, which is either a new setup or an existing one if that combination already exists;
  - keyboard: Tab between chips, letters pick.
- **Naming template:**
  - each setup and experiment gets a name from a per-kit template, e.g. `{source}-({obj1}#{phi1}-{obj2}#{phi2})-{analyzer}-{note}` (the lab's own scheme);
  - the template also PARSES names back, so imported data files auto-match (§G);
  - this generalises `MuellerReconstruction.parseExperiment` from a hard-coded regex into data.
- **Setup grid generator:** pick a set of options per slot (Source {LP1, CPL1} × Analyzer {LP2, CPL2} × Objects {∅, QZ, LR, QZ→LR} × φ {0, 45, 90}), untick impossible or unwanted combinations, and create all setups and experiments at once.
- **The direct problem keeps working.** A forward Main scene is a setup with no data. Varying several things on one table stays as it is: several experiments over one setup.

### G. Experiments and data attachment

- **`Experiment`** (the existing name stays) = one recorded run:
  - a setup version and a **scan**: one or more variables over values or ranges (the analyzer dial 0…360 step 20; the object rotation fixed per experiment, or itself scanned; λ for a tunable source);
  - the capture (T / R, from the detectors as today);
  - a **role**: `Science | Calibration | Dark | DriftCheck | Diagnostic | Ignored`;
  - a session (§H);
  - the attached data.
- **Generic tabular import with a column-mapping wizard** (replacing the strict `X,Y` v1 as the only shape; v1 stays as a preset):
  - a delimiter, header and BOM sniffer with a preview;
  - map columns to:
    - the scan variable(s), with unit and raw-dial vs physical angle;
    - the signal (`avg_total`, or `avg_R`, or any column);
    - an optional σ;
    - an optional timestamp (`captured_at`), an optional capture index, and the experiment key;
  - mappings are saved per kit as named **import profiles** ("Phone ROI CSV v1"), so the next file needs one click.
- **One file → many experiments:**
  - split by a key column (`experiment`), then match each group to an experiment by the naming template;
  - unmatched groups are listed, and creating setups for them is offered (the template parse supplies the slots and angles);
  - the four optics-mueller CSVs attach in four steps.
- **Raw data is immutable.**
  - Point exclusion carries a reason (as in optics-mueller `summary.json`).
  - Excluded points stay visible in plots, marked.
  - Nothing edits the stored blob.
- **Corrections without touching data:**
  - an experiment's setup can be re-pointed after the fact (e.g. "LR was at −45, not +45");
  - this carries a mandatory audit note and keeps the history, so a fit can be re-run with the corrected setup.
- **Other observables:**
  - Ψ / Δ series (exists);
  - full measured Mueller matrices per configuration (the `MuellerObservation` shape of spec 0044, from a commercial polarimeter);
  - N / C / S later.

  A study can mix them.
- **Per-experiment status badges:** no data / mapped / validated (point count) / excluded points / out-of-range (the existing typed validation, generalised to multi-variable scans).

### H. Studies — combining experiments into one inverse problem

- **`Study`** grows from today's experiment collection: a named, versioned set of experiments over ONE kit (possibly many setups, many samples, many sessions), plus:
  - the **unknowns**: every `Unknown` parameter of the items, materials and samples the included experiments use, each listed with an owner and a scope;
  - the **sessions**: time blocks, each with its own nuisance parameters;
  - the fit settings and a history of fit runs.
- **Parameter scope** is the sharing rule:

  | Scope | Meaning |
  |---|---|
  | `ItemScope` | One value for every experiment that uses the item (CPL1.δ, M_QZ) |
  | `MaterialScope` | One value for every sample of that material (multi-cut) |
  | `SessionScope` | One value per session block (the phone gain before and after the laser restart) |
  | `ExperimentScope` | One value per run (a local offset) |

  Linking two parameters explicitly ("CPL2.δ = CPL1.δ", the same filter type) is a constraint — the CompleteEASE "coupling" / SasView constraint.
- **Sessions and gain models as data**, replacing the hand-written family rules in `MuellerReconstruction`:
  - a session is a time interval, auto-split at Diagnostic experiments such as the laser restart, or split by hand;
  - per session the gain is either free or anchored to its AIR / Calibration experiments, optionally interpolated linearly between two AIR repeats;
  - the dark level comes from the Dark experiments (subtract, or fit as an offset).
- **Fit modes:**
  - **Joint**: everything free at once.
  - **Staged** (the lab's way):
    - stage 1 fits the item parameters from Calibration experiments (AIR through each source/analyzer family — ECM-style);
    - "Publish to kit" marks them `Calibrated`;
    - stage 2 fits the objects with those values fixed, or as priors with their σ.

  Staged and joint results can be compared side by side.
- **Engine choice is automatic.** When the only unknowns enter linearly (one black-box Mueller matrix, known source and analyzer), the problem is linear least squares (`MuellerReconstruction.reconstruct`, SVD, rank and condition reported). Otherwise it is the nonlinear path (Alglib adapter, multi-start — the existing `InverseFit` / `MuellerInverseSolver` machinery, made generic over the generated parameter axes).
- **Combining experiments across studies:**
  - "Add experiments from another study" — same kit, or with item mapping;
  - enable / disable per experiment (ablation);
  - a weight per experiment or per detector (SasView-style).
- **Results:**
  - per-parameter value ± σ (from the Jacobian covariance) and the correlation matrix;
  - rank and condition number;
  - per-experiment residual plots and RMSE;
  - the physical-realizability check for every fitted Mueller matrix;
  - for series objects, the **product check** (fitted M_comb against the product M_LR · M_QZ in light order, with RMSE ratio — exactly the step-2 report);
  - a profile-likelihood check for suspect parameters (`018` §3).
- **Publishing:**
  - item → new item version (`Calibrated`);
  - material → new material version (provenance = study);
  - a black-box Mueller object can optionally be saved as a library entry (a constant-Mueller sample).
- **Measurement Planner hook (`018` §3):** the planner reads the Kit (what you own) and the Study (what is unknown), and emits proposed experiments as setups + scans in this same model.

### I. Solver and physics changes these features need

- **The run law walks any number of objects.**
  - `ExperimentRun` folds slot by slot, in light order, through each item's Mueller matrix at its experiment orientation.
  - "First bound sample" (`measuredElementOf`) goes away; capture branches (T / R) are decided per object on the path to the detector.
- **The source is real.**
  - A source item with a Stokes state and an intensity scale replaces "the first polarizer sets a unit-intensity state".
  - The old behaviour is the default model (unpolarized source, unit scale after the first polarizer), so existing charts do not move.
- **One item-model evaluator,** `ItemModel × parameters × orientation × λ → MuellerMatrix`:
  - stack samples go through Berreman (`Propagation.sampleMuellerSpun`, flux-normalised as today);
  - black-box and compound items go through the Stokes/Mueller algebra.

  The fitter sees one function.
- **The forward model of an experiment** = slots + scan point + parameters → predicted reading, with intensity → S₀ times session gain plus dark, and an ellipsometer → Ψ / Δ. The residual stack over all included experiments is the study objective.
- **Parameter-axis generation:** each `ItemModel` / material complexity publishes its `ParameterAxis` list, so a new model needs no new fitter code (`MuellerInverse` is already generic in `'Parameters`; this supplies the axes from data).
- **Long fits** run on the existing `JobRunner`, with progress and cancel. A result is stored only on completion.

### J. UI changes

- **Launcher:**
  - `Main / Inverse / Materials / Library` stay;
  - add **Kit**;
  - add a "Data locations" footer link (§C).
- **Kit window** (single instance, `MinWidthPanel` + `keepOnScreen`):
  - the item list, and "Add item from library model…";
  - a parameter grid per item: name, state Known / Unknown / Calibrated, value, bounds, σ, provenance;
  - the dial mapping;
  - export / import `.kit-json`.
- **Inverse constructor** (the Main scene in inverse state) gains a left **flow strip**, each step reachable directly:
  1. Kit;
  2. Setups (the slot bar + grid generator);
  3. Experiments (the list, grouped by setup family);
  4. Data (attach, mapping wizard, split-by-column, match report);
  5. Unknowns (the study parameter table with scope and links);
  6. Fit;
  7. Results;
  8. Publish.

  The Experiments bay's "Data files" section moves into step 4.
- **Slot bar** over the table (§F): chips, compatible-item dropdowns, a φ / dial field per chip, and a "Record experiment" button. The table drawing follows the active setup.
- **Experiment matrix view:** rows = source × objects × φ, columns = analyzer. Each cell is an experiment with a data badge. This is how a user sees "which of my 40 runs still lack data" at a glance.
- **Data attach dialog:**
  - a preview of the first rows;
  - column-role dropdowns and an import-profile picker;
  - a key-column split with a matched / unmatched table;
  - per-point exclusion from the preview plot, with a reason.
- **Study window:**
  - the unknowns table (filter by owner and scope; fix / free toggles; bounds; priors; links);
  - Run joint / Run stage;
  - the run history;
  - results tabs: values, correlation heat map, residuals per experiment, realizability, product check.
- **Every new control** gets a stable automation id in `UiIds` (`KitItemList`, `SlotChip_<slot>`, `SetupGridGenerateButton`, `DataSplitKeyColumn`, `StudyRunFitButton`, …), and every new window joins `ScrollExtentTests`.

### K. Proxies, semantic commands, and the Mueller acceptance test

- **IO stays in proxies:**
  - `KitProxy`, `SetupProxy`, `StudyProxy`;
  - `MeasuredBlobProxy` (store and fetch raw data by hash);
  - `CatalogueSourceProxy` (list and read catalogue folders);
  - `BundleProxy` (write and read `.ocbundle`);
  - a `ClockProvider` for session times.

  Logic receives a `StudyContext` (data + proxies + providers).
- **Every user step is a semantic message**, so a test drives the app by meaning, not by pixels:
  - `CreateKit`, `AddKitItem`, `SetParameterState`;
  - `CreateSetup`, `AssignSlot`, `SwapSlot`, `GenerateSetupGrid`, `RecordExperiment`, `SetExperimentRole`;
  - `AttachDataFile` (with an import profile), `SplitByColumn`, `MatchByTemplate`, `ExcludePoint`, `CorrectSetup`;
  - `CreateStudy`, `AddExperimentsToStudy`, `SetParameterScope`, `LinkParameters`;
  - `RunFit` (joint / stage), `PublishCalibration`;
  - `ExportBundle`, `ImportBundle`.

  The same messages back the buttons.
- **Acceptance test "Mueller QZ + LR"** (trait `mueller-acceptance`, the `FsxAcceptanceTests` pattern). The headless app, over SQLite `:memory:` and in-memory blob proxies, runs the whole optics-mueller protocol through those messages:
  1. **Kit:** laser, LP1, LP2, CPL1 / CPL2 (LP + retarder, δ and Δ unknown), QZ / LR (black-box Mueller), phone (gain per session, dark).
  2. **Setups:** the grid over 4 families × {∅, QZ, LR, QZ → LR} × φ {0, 45, 90}. The **two-sample setups** (QZ in Object 1, LR in Object 2) are the "pick two samples" requirement.
  3. **Data:** attach the committed CSVs (the same data `MuellerDataStore` ships), split by `experiment`, match by the lab template. Darkness, BullshitCheck, AIR-BR / -2R get their roles; the LR ±45 correction and the one excluded frame are applied as data.
  4. **Fit:**
     - staged — AIR calibration, publish, then the object fit;
     - joint.
  5. **Assertions:**
     - M_QZ, M_LR and M_comb, CPL retardances and dial zeros match `analysis/matrix_solution/step2_linear/report.md` within tolerance (the `MuellerReconstruction` results are the oracle);
     - the product-check RMSE ratio reproduces;
     - the bundle round-trip (export study → import into an empty database) reproduces the same fit.
- **Synthetic twin test:** generate readings from KNOWN CPL δ / Δ and known QZ / LR matrices through the forward model, then recover them. This pins light order (M_LR · M_QZ, not M_QZ · M_LR) and the `R(−φ) M R(φ)` convention independently of lab data.
- **0044 twin:** a multi-cut study (three samples, one shared open material) reproduces a `MuellerInverseTests` case through the same Study model, proving `MaterialScope`.
- **Catalogue and database tests:**
  - catalogue export → import → equality with today's seeds (GUIDs, values, hashes);
  - every proxy contract on both stores;
  - migration from an empty database;
  - a newer-schema refusal;
  - an import-decision-table property test (FsCheck: re-importing the same catalogue is a no-op; import order does not matter).

### L. Proposed phasing (each slice green on its own)

1. **Pure data first:** the tabulated-material data case; canonical JSON + hash; per-kind schemas; export of the code seeds to the catalogue files; tests load the files; Domain seeds deleted.
2. **SQLite stores** behind the existing proxies, plus migrations, backup, locations and contract tests. `AppContext` switches over and the app now persists.
3. **Catalogue import on start** with the decision table; extra catalogue folders.
4. **Bundle** export / import + the import dialog (single `-json` drop included).
5. **Kit + item models + parameters** (Domain + `KitProxy` + Kit window).
6. **Setups as slots;** the N-object run law; the real source; the slot bar and grid generator.
7. **Experiments v2:** roles, sessions, multi-variable scans, the import wizard with profiles, split-by-column, template match, exclusions, corrections.
8. **Study + fit:** scopes and links, staged and joint fits, results, publish. Then the Mueller acceptance test and the synthetic twin.
9. **0044 multi-cut through the Study model; Measurement Planner** (`018` §3) on top.

## 4. Open questions for the operator (short; none blocks writing the spec)

- **Extensions:** confirm the `.<kind>-json` family (`material-json`, `sample-json`, `kit-json`, `setup-json`, …) and the bundle extension (`.ocbundle`?).
- **Code seeds:** delete them after the export to files (one source of truth), or keep them as a test-only fixture?
- **Kit sharing:** is a kit per user database (one person's bench), or can several users share one kit through a catalogue folder?
- **Naming:** keep `Experiment` for one run and introduce `Study` for the combined inverse problem (the experiment collection grows into it), or rename?
- **First target for slice 8:** the optics-mueller staged fit (proves the pipeline against existing results), or the joint fit?

## Sources

- SQLite as an application file format: https://sqlite.org/appfileformat.html
- SQLite over a network (locking caveats): https://sqlite.org/useovernet.html
- OneDrive sync and SQLite reports: https://forums.tropy.org/t/error-with-onedrive-sync/2961 ·
  https://sqlitetoolsforrootsmagic.com/forum/topic/rmv11-any-idea-what-the-new-temporay-files-are-for/ ·
  https://github.com/Sonarr/Sonarr/issues/1886
- OneDrive Known Folder Move (Documents redirected): https://learn.microsoft.com/en-us/sharepoint/redirect-known-folders
- OpticStudio catalogue folders (AGF in `Documents\Zemax\Glasscat`, network-drive caveat):
  https://optics.ansys.com/hc/en-us/articles/42661745091603 ·
  https://community.zemax.com/licensing-and-installation-8/what-is-the-programdata-zemax-folder-for-446
- refnx co-refinement with shared parameters (`GlobalObjective`): https://refnx.readthedocs.io/en/latest/reflectometry_global.html
- Origin global fitting with parameter sharing: https://www.originlab.com/doc/Origin-Help/GlobalFitting-Param-Sharing ·
  global vs independent fit: https://docs.originlab.com/quick-help/globatfit-independentfit/
- GraphPad Prism global nonlinear regression: https://graphpad.com/guides/prism/latest/curve-fitting/reg_how-global-nonlinear-regressio.htm
- SasView simultaneous fitting and constraints: https://sasview.github.io/downloads/simultaneous_1d_fitting_in_sasview_v5.pdf ·
  weights and priors in simultaneous fits: https://arxiv.org/pdf/2311.06408
- Multi-sample ellipsometry reduces correlation (Synowicki & Hilfiker, WVASE32):
  https://www.laserfocusworld.com/test-measurement/test-measurement/article/16550827/multiwavelength-ellipsometry-fathoms-materials ·
  CompleteEASE parameter coupling: https://arxiv.org/pdf/2511.13991
- Eigenvalue Calibration Method (Compain, Poirier, Drévillon 1999) and variants:
  https://arxiv.org/pdf/1509.05270 · https://www.jeos.org/index_php/jeos_rp/article/view/595 ·
  https://link.springer.com/article/10.1007/s13320-025-0767-z · https://doi.org/10.1364/ao.409799
- Dual-rotating-retarder calibration of retardance and orientation errors (Chenault, Pezzaniti, Chipman 1992):
  https://remotesensing.spiedigitallibrary.org/conference-proceedings-of-spie/1746/0000/Mueller-matrix-algorithms/10.1117/12.138793.full ·
  https://pure.bit.edu.cn/en/publications/error-analysis-of-the-calibration-of-a-dual-rotating-retarder-mue/
- Physical realizability (the cone of Mueller matrices): https://arxiv.org/pdf/2309.00161
- Earlier reports in this folder: `018-sharing-libraries-inverse-planner.md` (bundle, decision table, online libraries,
  Measurement Planner); optics-mueller `notes/matrix_solution.md`, `notes/final_normalization_caveat.md`,
  `analysis/matrix_solution/step1/report.md`, `analysis/matrix_solution/step2_linear/report.md`.
