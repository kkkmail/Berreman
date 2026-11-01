# 020 — Library files, the slow-data database, experiment-kit folders, and multi-setup inverse experiments (feature list)

Research and proposal only. No code was changed.

**Revision 2 (2026-11-01, after the operator's comments).** This revision changes the architecture:

- **The database must not be overstuffed.** It holds **slow data only**, i.e. the library.
- **An experiment kit is not slow data.** The kit — its items, setups, experiments, studies, fit results and the measured data FILES — lives as plain files in an **experiment-kit folder** that the user chooses or creates.
- **The app keeps a history of kit folders:** 100 by default (configurable), starrable, sorted by the last open date.
- **Sending a kit means sending its folder.** The separate bundle format from `018` is no longer needed.

Inputs:
- `019-handover.md`, `002-plan.md`, `018-sharing-libraries-inverse-planner.md`;
- the constructor code at `c0bfb48`;
- the Mueller experiment in `C:\GitHub\optics-mueller` (`AGENTS.md`, `notes/*`, `analysis/matrix_solution/*`);
- specs 0038 (§13–16), 0042 and 0044;
- a web survey (sources at the end).

## Short answers to the questions asked

- **Two homes for data.**
  - **Slow data** (changes rarely, shared by every kit): the library, in a local SQLite database.
  - **Fast data** (one experimental campaign): an experiment-kit folder of plain files.
- **The library leaves the code** and becomes JSON files, typed by extension in the STL/ASG convention (`.material-json`, `.sample-json`, `.source-json`, `.polarizer-json`, `.detector-json`, `.category-json`, plus one `catalogue.manifest-json` per catalogue folder).
  - It is imported at **every** start; a manifest-hash comparison makes that cheap.
  - An import never edits a built-in in place (a change mints a new version), and it never touches user entries.
- **The database:** SQLite through EF Core, which the repo already pins and has proven (`OpticalConstructor.Database`). It holds only:
  - library entries (materials, categories, samples, sources, polarizer / detector / item-model presets) with their version history;
  - the catalogue-import log.

  Nothing per-experiment goes there: no kits, setups, experiments, data, fit results or recent-folder lists.
- **Experiment kits are folders.** The user starts a kit by choosing or creating a folder. The folder holds:
  - the kit's physical items, its setups, experiments and studies, and its fit runs, one file each;
  - the raw measured data files, exactly as the lab produced them;
  - a snapshot of every library version the kit uses.

  It is therefore self-contained: copy it, zip it, or put it under git.
- **Kit-folder history:** the most recent 100 folders by default (`appsettings.json`), starrable and sorted by last open date. It lives in the existing per-user preferences file, not in the database.
- **Locations:**
  - **Database:** `%LOCALAPPDATA%\Softellect\Berreman\OpticalConstructor\library.db`, local and never synced. On Windows 11, OneDrive Known Folder Move often syncs Documents, and SQLite over sync or network file systems is a known corruption path.
  - **Preferences:** the existing `%APPDATA%\Softellect\Berreman\OpticalConstructor\`.
  - **Kit folders:** anywhere the user picks, suggested under `Documents\Optical Constructor\Kits\`. Because they are plain files written atomically, OneDrive and git are fine for them.
- **Exchange:**
  - **a kit** = its folder ("Send kit…" zips it into one file);
  - **one library entry** = its `-json` file;
  - **a curated library** = a catalogue folder.

  The receiving app checks the kit's library snapshot against its own database (the `018` decision table) and never overwrites anything silently.
- **The inverse problem:**
  - the kit holds the **items** — the physical pieces (LP1, CPL1, QZ, …), each with its OWN parameters, known or unknown;
  - **setups** place items in slots on the beam, two or more samples allowed;
  - a **study** fits many experiments together.

  An unknown such as the retardance of CPL1 belongs to the ITEM, so every experiment that uses that item shares it automatically. That one rule is what makes "swap the pieces, attach a file to each run, fit everything together" work.

---

## 0. What the app does today (the evaluation)

- **Nothing persists.**
  - Every store is in memory, and every start re-seeds (`Berreman/OpticalConstructor/OpticalConstructor.Ui/AppContext.fs:69`).
  - `.ocproj` exists (`OpticalConstructor.Storage/ProjectFile.fs`) but no window saves or opens it.
  - `Scene.SceneProxy` and `ExperimentCollectionProxy` are in-memory seams that wait for a disk `create`.
  - The EF Core project holds one `Test` table that nothing references.
  - `OpticalConstructor.Seeding` pushes the code seeds through `SeedingProxy` into in-memory stores only.
  - The only things on disk are the recent-files list (`OpticalConstructor.Storage/RecentFiles.fs`: `%APPDATA%\Softellect\Berreman\OpticalConstructor\recent.json`, capped at 16) and the user-environment preferences (`lastFolders` per picker).
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
  - It does so by **regex over experiment names** (`MuellerReconstruction.fs:400`, `CPL-(QZ#45-LR#45)-CPL`) and a hard-coded `MuellerDataSet` DU over a zip shipped next to the executable.
  - `MuellerInverse` (spec 0044) is the multi-cut joint fit of material constants.

  Neither is reachable from the constructor. The features below generalise what both hard-code.
- **The data-file schemas are too narrow for real data.**
  - Intensity is strict `X,Y`.
  - Ellipsometric files must be `wavelength_nm,psi_deg,delta_deg`.
  - A real lab file is one CSV with many runs keyed by a column (`experiment`), a raw dial reading (`description`), a timestamp (`captured_at`) and several signal columns (`avg_R`, `avg_total`).

## 1. The reference case — the optics-mueller experiment, as the model must see it

- **Items:**
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
- **The repository already is, in effect, an experiment-kit folder:** raw data under `data/`, notes, analysis outputs and photos. The kit-folder design below lets that folder open as a kit by adding the kit's own files next to the lab's files, without moving anything.
- **What the app must therefore do:**
  - hold the items;
  - build about 40 setups by swapping pieces;
  - attach four CSVs that each feed many experiments;
  - tag diagnostics;
  - declare unknowns on items, sessions and objects;
  - fit jointly or in stages;
  - publish calibrated item values for the next study.

## 2. Best practice and similar software (web survey, short)

- **Global or co-refinement with shared parameters** is the standard answer to "combine experiments":
  - *Origin and GraphPad Prism*: a shared parameter gets one value fitted from all data sets, which also changes the reported errors.
  - *refnx*: `GlobalObjective` over per-data-set objectives. Sharing works by REUSING the same parameter object across models; that is the "parameter belongs to the item" rule.
  - *SasView*: simultaneous fit with constraints that link models; per-data-set weights since v6.
  - *Woollam*: multi-sample analysis reduces parameter correlation, and CompleteEASE has explicit parameter coupling.
- **Polarimeter calibration with imperfect, unknown elements.**
  - The Eigenvalue Calibration Method (Compain, Poirier & Drévillon 1999) finds the generator and analyzer matrices (W, A; 32 coefficients) from three or four reference samples (air, polarizers, a retarder). It assumes nothing about the elements except completeness and needs no precise alignment.
  - Dual-rotating-retarder calibration (Goldstein & Chipman; Chenault et al.) fits retardance and orientation errors, because errors under 1° give over 10 % matrix error.
  - Hence: "calibration experiments" are a first-class role, and item parameters are fit targets.
- **Physical realizability** of a fitted Mueller matrix (the cone of Mueller matrices, coherency eigenvalues ≥ 0) is a standard post-fit check.
- **Library versus project data (Zemax OpticStudio).**
  - Catalogues are plain-text AGF files in a user data folder (`Documents\Zemax\Glasscat`, path settable).
  - Since 21.3 it also has **project directories**: a per-lens-file location for catalogues, so a project carries what it needs.
  - Its own guidance warns that putting the shared data folder on a network or cloud drive breaks offline use.
  - This is the same split as here: slow shared library in one place, a self-contained project folder per piece of work.
- **SQLite as a document format.**
  - SQLite's own case for using a database file as the application file format: atomic, queryable, one file.
  - The same project's network caveats: file locking misbehaves on network and sync file systems; WAL needs shared memory.
  - Forum reports (Tropy, RootsMagic, Sonarr) show real corruption from OneDrive-synced databases.
  - Conclusion: a LOCAL SQLite database fits the slow library. A folder of plain files fits work that is sent, synced or versioned.

---

## 3. Feature list

### A. The library as external files (update without recompiling)

- **One entry per file**, in a folder tree per catalogue. It diffs, reviews and merges in git, and a single entry can be emailed or dropped in.
- **Typed library extensions (the STL/ASG `-json` convention):**
  - `.material-json`, `.sample-json`, `.source-json`, `.polarizer-json`, `.detector-json`;
  - `.item-model-json` (the parameterised piece models of §F);
  - `.category-json`;
  - `catalogue.manifest-json` (name, catalogue id, catalogue version, list of entry hashes).

  Content is plain JSON; the extension names the schema. The kit-folder file kinds are separate (§C).
- **Every entry file carries:**
  - `schema` (kind + schema version), `id` (the frozen GUID), `version`;
  - `name`, `category` / facets, `description`;
  - `payload` (the elevated domain value, e.g. `MaterialComplexity`);
  - `provenance` (built-in / user / imported-from-kit / pulled-from-source, with citation and licence — `018` §2);
  - `contentHash` (SHA-256 over canonical JSON: sorted keys, invariant-culture numbers).
- **JSON Schema per kind**, validated on load. The storage layer already uses System.Text.Json + FSharp.SystemTextJson + JsonSchema.Net for `.ocproj`, so reuse that stack.
- **Schema evolution:**
  - each kind has pure up-converters vN → vN+1;
  - a file newer than the app is rejected with a typed `UnsupportedSchemaVersion` error, never a crash.

  The same rule holds for kit-folder files.
- **Physics must be pure data before it can leave the code.**
  - Add a **tabulated** `EpsWithDispValue` case (λ, n, k arrays + interpolation rule) so refractiveindex.info tabulated imports become storable and versionable instead of closure-backed `complexity = None`.
  - Any entry that cannot serialise is a test failure.
- **Shipped catalogue:**
  - `<install>\Catalogue\Builtin\…` is read-only, copied by the build from a repo folder (e.g. `Berreman/OpticalConstructor/Catalogue/`).
  - The code seeds are exported ONCE to these files by a throw-away tool, keeping GUIDs and values byte-for-byte. Then the Domain seed values are **deleted** so there is one source of truth.
  - Tests load the same files (a fixture reads the repo catalogue folder).
- **Import on every start (cheap):**
  - compare each catalogue's manifest hash with the one recorded in the database;
  - import only when it changed;
  - log the decisions.
- **Per-entry import decision** (one rule for catalogues AND for library snapshots inside received kits; the `018` table):

  | Incoming | Action |
  |---|---|
  | New id | Add |
  | Same id, same hash | Skip |
  | Same id, new hash | Mint the next version of that entry; for a protected built-in, the new version becomes the latest built-in |
  | Entry removed from the catalogue | Mark `InactiveEntry` (still resolvable) |

  Never delete, never touch user entries. Kits pinned to an old version keep it.
- **Extra catalogue sources:**
  - "Add catalogue folder…" registers a read-only folder: a lab share, OneDrive, or a git checkout;
  - this is the shared-folder library from `018`;
  - JSON on a synced folder is safe because the app only reads it.
- **Export:**
  - any entry → its `-json` file;
  - a selection or a whole category → a catalogue folder with a manifest.

  This is how a curated user library becomes a shipped one.
- **`SeedingProxy` / `seedAll` become `CatalogueImport`:** the same seam shape, fed from parsed files instead of Domain values. The existing step-042 in-memory proof stays as the test double.

### B. The library database — slow data ONLY (SQLite via EF Core)

- **What it holds:**
  - library entries of every kind, with their immutable version history, lifecycle and protection;
  - categories;
  - the catalogue-import log (catalogue id, manifest hash, time, decisions).

  Item models (§F) are library entries too: "LP + retarder" is a reusable TYPE; the physical CPL1 is not.
- **What it never holds** — anything that belongs to one experimental campaign or changes per session:
  - kit items, setups, experiments, studies, fit runs;
  - measured data;
  - the kit-folder history and other preferences.

  The rule for future additions: if it would travel with a kit, it goes in the kit folder; if every kit shares it and it changes rarely, it may go in the database.
- **Hybrid shape:**
  - relational columns for what is queried or constrained: id, version, kind, name, lifecycle, protection, facet keys, foreign keys (sample version → material version);
  - the elevated domain value as a schema-versioned JSON payload column, using the same canonical serializer as §A.

  The library file, the database payload and the kit's snapshot of an entry are byte-identical, and the hash proves it.
- **Versions are immutable rows**; "edit latest" follows the existing `Lifecycle.decideVersioning` rule.
- **"In use" without seeing the kits.** Experiments now live in folders the database cannot enumerate, so `VersionsInUse` cannot be a query over experiments. Instead:
  - the first time a kit pins a library version (binds it in a setup, or snapshots it), the database marks that version **`Pinned`**;
  - this is a one-way flag (never cleared) and a tiny slow-data write;
  - a pinned version is immutable, so editing it mints the next version.
  - The kit's own snapshot (§C) is the second safeguard: a hash mismatch between snapshot and database is reported, never resolved silently.
- **The library proxies keep their shapes and gain a SQLite-backed `create`:** `MaterialProxy`, `CategoryProxy`, `SampleProxy`, `SourceProxy`, the `LibraryProxy` live view, and the preset / item-model stores. `SceneProxy` and `ExperimentCollectionProxy` do NOT move to the database; their disk form is the kit folder (§C).
- **Contract tests:** one suite per library proxy runs against BOTH the in-memory store and SQLite in `:memory:` mode.
- **Migrations:**
  - EF Core migrations replace the `Test` table;
  - migrations apply at start, after an automatic backup (`VACUUM INTO` a dated copy, the last N kept);
  - a database with a newer schema than the app opens read-only with a typed error.
- **Settings and safety:**
  - the connection string comes from `appsettings.json` (the elevated `ConnectionString` already exists in `AppConfig.fs`);
  - WAL journal mode, since the file is local only;
  - one writer (the app);
  - a menu item "Back up library now…" and "Restore library from backup…".

### C. The experiment-kit folder — fast data as plain files

- **A kit is a folder.**
  - It is recognised by `kit.kit-json` at its root. The manifest holds the kit id (GUID), name, schema version, the app version that last wrote it, created and modified times, notes, the naming template (§G), the import profiles (§H) and the send-exclusion globs.
  - Everything else in the folder is either a kit file the app owns or a user file the app only reads.
- **Layout** (one entity per file, LF, pretty-printed, stable key order, so the folder diffs in git):

  ```
  <kit folder>/
    kit.kit-json                                   manifest
    items/<label>.item-json                        one physical piece each (§F)
    setups/<name>.setup-json                       slot assignments (§G)
    experiments/<name>.experiment-json             one recorded run each (§H)
    studies/<name>.study-json                      unknowns, scopes, links, fit settings (§I)
    studies/<name>/runs/<utc-stamp>.fitrun-json    every fit: inputs, parameter vector, results
    library/<kind>/<id>@v<n>.<kind>-json           snapshot of every library version the kit uses
    data/…                                         raw measured files, any layout, never modified
    .oc/                                           app-owned cache and lock, safe to delete, never sent
  ```

  `data/` is only the default for new attachments. Any file under the kit root may be referenced, so an existing lab folder (e.g. the optics-mueller repo with its `data/raw/*.csv`) becomes a kit by adding `kit.kit-json` and the app's subfolders, moving nothing.
- **References inside a kit are relative paths** (forward slashes) plus the SHA-256 recorded when the file was attached.
  - On open, and before each fit, changed or missing data files are flagged ("data changed since attach / since the last fit run"), never ignored.
  - Absolute paths are never stored.
- **Attaching a file from outside the kit copies it into `data/`** (keeping its name, suffixing on collision), so the folder stays self-contained.
- **Raw data is read-only for the app.** Exclusions, corrections and mappings are recorded in the experiment file, never in the data file.
- **Library closure snapshot.**
  - Every library version the kit references (samples, materials, item models, sources, detectors, polarizers) is copied into `library/` when first referenced.
  - Snapshots of `ProtectedBuiltIn` entries are included as well, because the receiver may run another catalogue version.
  - The kit therefore opens on any machine, even one whose database lacks those entries.
- **Resolution on open** (the `018` decision table, per snapshot entry):

  | Snapshot vs local database | Action |
  |---|---|
  | Same id, version and hash | Use the database entry |
  | Missing locally | Use the snapshot read-only; offer "Add to my library" |
  | Same id and version, different hash | Report a divergence; the kit keeps its snapshot |
  | Different id, same name | Side by side, no merging |

  The import dialog lists every row, and nothing changes in the database without the user's confirmation.
- **Writes are atomic** (write `*.tmp`, then replace), so a crash never leaves half a file, and sync clients only ever see whole files.
- **Fit runs are append-only.** Each one stores the full parameter vector it used and produced, so an old result stays reproducible after items are recalibrated.
- **Concurrency.**
  - Inside the app, a kit folder opens in one window at a time.
  - Across processes and machines, a `.oc/lock` file (machine, user, process, time) warns when the same kit is already open, with an "open read-only" choice.
  - On window activation the app re-checks the files' timestamps and hashes and offers to reload changes made outside it (another editor, a git pull, a sync).
- **Large raw files.** The lab may keep raw images in the kit (optics-mueller: about 24 MB per DNG). The app reads only what experiments reference, and `kit.kit-json` carries `sendExclude` globs (e.g. `**/*.dng`) that "Send kit…" honours.
- **`.ocproj` is superseded.** The kit folder is the project for experiment work. The never-wired single-file `.ocproj` is retired, or kept only as a "Save scratch scene" format — see §4.

### D. Starting work — the kit-folder history

- **The start screen** opens from the Launcher's **Inverse** button, and from **Main → Open kit**. It offers:
  - **New kit…**: choose or create a folder. An empty folder is initialised; a folder that already has files keeps them, and `kit.kit-json` and the app's subfolders are added.
  - **Open kit folder…**: a folder picker; a folder without `kit.kit-json` offers "Make this a kit".
  - **Open kit from zip…**: choose a target folder, extract, open.
  - **The history list.**
- **The history:**
  - up to **100** folders by default, configurable as `ExperimentKitHistoryLimit` in `appsettings.json`;
  - each row shows the kit name, folder path, last-opened date and a **star**;
  - sorted by last-opened date, newest first;
  - **starred kits are pinned in a section at the top** (also sorted by last-opened date) and are never evicted by the cap — the oldest UNSTARRED entry goes first;
  - a filter box (name / path);
  - a folder that no longer exists shows greyed, with **Locate…** and **Remove from history**.
- **Storage:**
  - the history lives in the per-user preferences, NOT the database;
  - it extends the existing recent store (`RecentFiles.fs`, `%APPDATA%\Softellect\Berreman\OpticalConstructor\`), whose single-store consolidation spec 0038 §16.3 already asked for;
  - entry: folder, kit id, name cache, last opened, starred (a two-case DU `Starred | NotStarred`, never a bool);
  - pure list logic (`bump`, `star`, `unstar`, `forget`, `evict`) with a clock provider, unit-tested without IO.
- **The forward constructor** can still run without a kit (a scratch scene, as today). **Save as kit…** turns the current scene and its experiments into a new kit folder.

### E. Locations (summary)

- **Library database:** `%LOCALAPPDATA%\Softellect\Berreman\OpticalConstructor\library.db`, with backups in `Backups\` beside it. Local and machine-specific; never on OneDrive or a share.
- **Preferences** (kit-folder history, picker last-folders, window state): the existing `%APPDATA%\Softellect\Berreman\OpticalConstructor\`.
- **Kit folders:** anywhere the user chooses; the suggested parent is `Documents\Optical Constructor\Kits\`. Plain files and atomic writes make OneDrive, a share or git workable. Do not open the same kit on two machines at the same time; the lock file warns.
- **Exports and catalogue exports:** `Documents\Optical Constructor\Exports\` (already used by `ExperimentExportFile`) and `…\Catalogues\`.
- **Shipped catalogue:** `<install>\Catalogue\`, read-only.
- **Every location is overridable** in `appsettings.json`; a Launcher "Data locations" panel shows each path with an "Open folder" button.
- Note on `CLAUDE.md`: its "never `%LOCALAPPDATA%`" rule covers arc-runner WORKER output, not the product's data. This feature does not conflict with it.

### F. Exchange

- **A kit:** copy the folder.
  - **Send kit…** writes one `<kit name>.zip` of the folder, excluding `.oc/` and the `sendExclude` globs.
  - **Open kit from zip…** extracts and opens it.
  - A plain `.zip` is enough because the folder already describes itself; there is no separate bundle format.
- **A library entry:** its `-json` file, imported by drag and drop onto the Library or Materials window through the same decision table (§A).
- **A curated library:** a catalogue folder (§A); "Add catalogue folder…" on the receiving side.
- **Moving work between kits:** "Copy experiments from another kit…" copies the chosen experiments with their setups, their data files and the items they use. Items are mapped onto existing items in the target kit where the user says so, otherwise added.
- **Review round-trip:** notes on experiments, studies and fit runs live in their files, so B's comments come back in the returned folder or zip. A git-managed kit gets the full history for free.

### G. Items — the physical pieces of a kit (new data model; files in `items/`)

- **`KitItem`** = one PHYSICAL piece:
  - a `KitItemId` (GUID) and a short `ItemLabel` (`LP1`, `CPL2`, `QZ`, `Phone`);
  - the library model it is an instance of (an `ItemModel` reference, pinned by version and snapshotted);
  - its own `ItemParameter` values;
  - optional photo (a file in the kit) and notes.

  Two LPs of the same library model are two items with independent parameters.
- **`ItemModel`** — parameterised physics as data, a library entry (slow). Each model declares its parameter axes, so the fitter's `ParameterAxis` list is generated rather than hand-written per problem.

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
  - `Calibrated of value * sigma * fromFitRun`.
- **Dial mapping:** a rotatable item carries a `DialMapping`, i.e. a zero offset and a sign (`DialSign` exists in `MuellerReconstruction`). Experiments record the RAW dial reading, as the lab does. The dial zero is an ordinary parameter (LP2's zero 155.44° raw was fitted).
- **Item revisions.**
  - An item file keeps a revision number and a short history of its parameter values.
  - "Publish calibration" from a fit run writes the next revision.
  - Reproducibility of older results comes from the fit runs, which store the vector they used, so items need no database versioning.
- **Material parameters as unknowns:**
  - a study may "open" a material version, which creates a study-local working copy, stored in the study file, whose chosen components are `Unknown`;
  - every sample of the study made of that material shares them — the 0044 multi-cut case (three cuts, one crystal);
  - **Promote to library** is the explicit step that mints the material's next version in the database (slow data), with provenance "fitted in kit K, study S, run R".

### H. Setups, slots and quick swap (files in `setups/`)

- **`Setup`** = an ordered optical path of **slots**, each holding a kit item or nothing:
  - Source, Prep 1…k (polarizers or compensators before the objects), Object 1…n (**two or more samples in series** are allowed — "pick two samples for the experiment"), Analyzer 1…k, Detector;
  - per slot: the item's placement (position on the beam, R1/R2/R3, or a dial reading).

  The table drawing renders the active setup, and the existing `BeamLayout` / `RayModel` laws are unchanged.
- **Setups are files with a revision number.** An experiment REFERENCES a setup (name + revision) instead of embedding a frozen descriptor list; the existing `ElementDescriptor` list becomes the setup's slot list.
- **Quick swap (the lab gesture):**
  - a slot bar above the table shows one chip per slot (`Source: Laser | Prep: CPL1 ▾ | Obj 1: QZ ▾ @ 45° ▾ | Obj 2: LR ▾ @ 45° ▾ | Analyzer: CPL2 ▾ | Detector: Phone`);
  - a chip's dropdown lists only kit items compatible with that slot;
  - choosing one swaps the piece, which is either a new setup or an existing one if that combination already exists;
  - keyboard: Tab between chips, letters pick.
- **Naming template:**
  - each setup and experiment gets a name from the kit's template, e.g. `{source}-({obj1}#{phi1}-{obj2}#{phi2})-{analyzer}-{note}` (the lab's own scheme);
  - the template also PARSES names back, so imported data files auto-match (§I);
  - this generalises `MuellerReconstruction.parseExperiment` from a hard-coded regex into kit data.
- **Setup grid generator:** pick a set of options per slot (Source {LP1, CPL1} × Analyzer {LP2, CPL2} × Objects {∅, QZ, LR, QZ→LR} × φ {0, 45, 90}), untick impossible or unwanted combinations, and create all setups and experiments at once.
- **The direct problem keeps working.** A forward Main scene is a setup with no data. Varying several things on one table stays as it is: several experiments over one setup.

### I. Experiments and data attachment (files in `experiments/`, data in the kit)

- **`Experiment`** (the existing name stays) = one recorded run:
  - a setup reference and a **scan**: one or more variables over values or ranges (the analyzer dial 0…360 step 20; the object rotation fixed per experiment, or itself scanned; λ for a tunable source);
  - the capture (T / R, from the detectors as today);
  - a **role**: `Science | Calibration | Dark | DriftCheck | Diagnostic | Ignored`;
  - a session (§J);
  - the data attachment: a relative path in the kit + hash + import profile + row selection.
- **Generic tabular import with a column-mapping wizard** (replacing the strict `X,Y` v1 as the only shape; v1 stays as a preset):
  - a delimiter, header and BOM sniffer with a preview;
  - map columns to:
    - the scan variable(s), with unit and raw-dial vs physical angle;
    - the signal (`avg_total`, or `avg_R`, or any column);
    - an optional σ;
    - an optional timestamp (`captured_at`), an optional capture index, and the experiment key;
  - mappings are saved in `kit.kit-json` as named **import profiles** ("Phone ROI CSV v1"), so the next file needs one click and the profile travels with the kit.
- **One file → many experiments:**
  - split by a key column (`experiment`), then match each group to an experiment by the naming template;
  - unmatched groups are listed, and creating setups for them is offered (the template parse supplies the slots and angles);
  - the four optics-mueller CSVs attach in four steps.
- **Raw data is immutable.**
  - Point exclusion carries a reason (as in optics-mueller `summary.json`) and is stored in the experiment file.
  - Excluded points stay visible in plots, marked.
- **Corrections without touching data:**
  - an experiment's setup can be re-pointed after the fact (e.g. "LR was at −45, not +45");
  - this carries a mandatory audit note kept in the experiment file's history, so a fit can be re-run with the corrected setup.
- **Other observables:**
  - Ψ / Δ series (exists);
  - full measured Mueller matrices per configuration (the `MuellerObservation` shape of spec 0044, from a commercial polarimeter);
  - N / C / S later.

  A study can mix them.
- **Per-experiment status badges:** no data / mapped / validated (point count) / excluded points / data changed since attach / out-of-range (the existing typed validation, generalised to multi-variable scans).

### J. Studies — combining experiments into one inverse problem (files in `studies/`)

- **`Study`** grows from today's experiment collection: a named set of experiments inside ONE kit (possibly many setups, many samples, many sessions), plus:
  - the **unknowns**: every `Unknown` parameter of the items, opened materials and samples the included experiments use, each listed with an owner and a scope;
  - the **sessions**: time blocks, each with its own nuisance parameters;
  - the fit settings;
  - its fit runs (append-only files).
- **Parameter scope** is the sharing rule:

  | Scope | Meaning |
  |---|---|
  | `ItemScope` | One value for every experiment that uses the item (CPL1.δ, M_QZ) |
  | `MaterialScope` | One value for every sample of an opened material (multi-cut) |
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
    - "Publish calibration" marks them `Calibrated` in the item files;
    - stage 2 fits the objects with those values fixed, or as priors with their σ.

  Staged and joint runs can be compared side by side.
- **Engine choice is automatic.** When the only unknowns enter linearly (one black-box Mueller matrix, known source and analyzer), the problem is linear least squares (`MuellerReconstruction.reconstruct`, SVD, rank and condition reported). Otherwise it is the nonlinear path (Alglib adapter, multi-start — the existing `InverseFit` / `MuellerInverseSolver` machinery, made generic over the generated parameter axes).
- **Ablation and weighting:** enable / disable per experiment, and a weight per experiment or per detector (SasView-style). Experiments from another kit come in through "Copy experiments from another kit…" (§F).
- **Results**, written into the fit-run file:
  - per-parameter value ± σ (from the Jacobian covariance) and the correlation matrix;
  - rank and condition number;
  - per-experiment residuals and RMSE;
  - the physical-realizability check for every fitted Mueller matrix;
  - for series objects, the **product check** (fitted M_comb against the product M_LR · M_QZ in light order, with RMSE ratio — exactly the step-2 report);
  - a profile-likelihood check for suspect parameters (`018` §3).
- **Publishing:**
  - item → next item revision in the kit (`Calibrated`);
  - material → **Promote to library** (database, explicit);
  - a black-box Mueller object → optionally **Promote to library** as a constant-Mueller sample model.
- **Measurement Planner hook (`018` §3):** the planner reads the kit's items (what you own) and the study (what is unknown), and emits proposed experiments as setups + scans in this same model.

### K. Solver and physics changes these features need

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
- **Long fits** run on the existing `JobRunner`, with progress and cancel. A fit-run file is written only on completion.

### L. UI changes

- **Launcher:**
  - `Main / Inverse / Materials / Library` stay; **Inverse** opens the kit start screen (§D);
  - a "Data locations" footer link (§E).
- **Kit start screen** (§D):
  - the history list with stars, filter and missing-folder handling;
  - New kit… / Open kit folder… / Open kit from zip….
- **Inverse constructor** (the Main scene in inverse state, bound to the open kit; its title shows the kit name and folder) gains a left **flow strip**, each step reachable directly:
  1. Items;
  2. Setups (the slot bar + grid generator);
  3. Experiments (the list, grouped by setup family);
  4. Data (attach, mapping wizard, split-by-column, match report);
  5. Unknowns (the study parameter table with scope and links);
  6. Fit;
  7. Results;
  8. Publish.

  The Experiments bay's "Data files" section moves into step 4.
- **Items panel:**
  - the item list, and "Add item from library model…";
  - a parameter grid per item: name, state Known / Unknown / Calibrated, value, bounds, σ, provenance;
  - the dial mapping;
  - the revision history.
- **Slot bar** over the table (§H): chips, compatible-item dropdowns, a φ / dial field per chip, and a "Record experiment" button. The table drawing follows the active setup.
- **Experiment matrix view:** rows = source × objects × φ, columns = analyzer. Each cell is an experiment with a data badge, so "which of my 40 runs still lack data" is visible at a glance.
- **Data attach dialog:**
  - a preview of the first rows;
  - column-role dropdowns and an import-profile picker;
  - a key-column split with a matched / unmatched table;
  - per-point exclusion from the preview plot, with a reason.
- **Study view:**
  - the unknowns table (filter by owner and scope; fix / free toggles; bounds; priors; links);
  - Run joint / Run stage;
  - the fit-run history;
  - results tabs: values, correlation heat map, residuals per experiment, realizability, product check.
- **Kit menu:** Send kit…, Open kit folder in Explorer, Copy experiments from another kit…, Reload changes, Close kit.
- **Every new control** gets a stable automation id in `UiIds` (`KitHistoryList`, `KitHistoryStarToggle`, `NewKitButton`, `KitItemList`, `SlotChip_<slot>`, `SetupGridGenerateButton`, `DataSplitKeyColumn`, `StudyRunFitButton`, `SendKitButton`, …), and every new window joins `ScrollExtentTests`.

### M. Proxies, semantic commands, and the Mueller acceptance test

- **IO stays in proxies:**
  - library proxies over SQLite (§B);
  - **`ExperimentKitProxy`**, created per opened folder by `ExperimentKitProxy.create (folder : KitFolder)`. It reads and writes the manifest, items, setups, experiments, studies and fit runs; reads data files by relative path; copies external files in; writes library snapshots; takes and releases the lock; and checks for external changes;
  - **`KitHistoryProxy`** over the preferences file;
  - **`CatalogueSourceProxy`** (list and read catalogue folders);
  - **`KitArchiveProxy`** (zip and unzip a kit);
  - a `ClockProvider` for last-opened dates and session times.

  Logic receives a `KitContext` (kit data + proxies + providers). Every proxy has an in-memory stub of the same shape.
- **Every user step is a semantic message**, so a test drives the app by meaning, not by pixels:
  - `CreateKitInFolder`, `OpenKitFolder`, `StarKit`, `UnstarKit`, `ForgetKit`;
  - `AddKitItem`, `SetParameterState`;
  - `CreateSetup`, `AssignSlot`, `SwapSlot`, `GenerateSetupGrid`, `RecordExperiment`, `SetExperimentRole`;
  - `AttachDataFile` (with an import profile), `SplitByColumn`, `MatchByTemplate`, `ExcludePoint`, `CorrectSetup`;
  - `CreateStudy`, `AddExperimentsToStudy`, `SetParameterScope`, `LinkParameters`;
  - `RunFit` (joint / stage), `PublishCalibration`, `PromoteToLibrary`;
  - `SendKit`, `OpenKitFromZip`.

  The same messages back the buttons.
- **Acceptance test "Mueller QZ + LR"** (trait `mueller-acceptance`, the `FsxAcceptanceTests` pattern). It runs against SQLite `:memory:` for the library and an in-memory `ExperimentKitProxy` seeded with the committed optics-mueller CSVs (the data `MuellerDataStore` ships today):
  1. **Kit:** create the kit; add the laser, LP1, LP2, CPL1 / CPL2 (LP + retarder, δ and Δ unknown), QZ / LR (black-box Mueller), and the phone (gain per session, dark).
  2. **Setups:** the grid over 4 families × {∅, QZ, LR, QZ → LR} × φ {0, 45, 90}. The **two-sample setups** (QZ in Object 1, LR in Object 2) are the "pick two samples" requirement.
  3. **Data:** attach the CSVs, split by `experiment`, match by the lab template. Darkness, BullshitCheck, AIR-BR / -2R get their roles; the LR ±45 correction and the one excluded frame are applied as data.
  4. **Fit:**
     - staged — AIR calibration, publish, then the object fit;
     - joint.
  5. **Assertions:**
     - M_QZ, M_LR and M_comb, CPL retardances and dial zeros match `analysis/matrix_solution/step2_linear/report.md` within tolerance (the `MuellerReconstruction` results are the oracle);
     - the product-check RMSE ratio reproduces.
- **Folder twin of the same test:**
  - write the kit through the REAL folder `ExperimentKitProxy` into a throw-away folder under the test run's output directory;
  - zip it with `SendKit`, unzip it elsewhere, and open it against an EMPTY library database (resolution through the snapshot);
  - re-run the fit and assert the same numbers.

  This proves the folder is self-contained.
- **Synthetic twin test:** generate readings from KNOWN CPL δ / Δ and known QZ / LR matrices through the forward model, then recover them. This pins light order (M_LR · M_QZ, not M_QZ · M_LR) and the `R(−φ) M R(φ)` convention independently of lab data.
- **0044 twin:** a multi-cut study (three samples, one opened material) reproduces a `MuellerInverseTests` case through the same Study model, proving `MaterialScope`.
- **Library, history and folder tests:**
  - catalogue export → import → equality with today's seeds (GUIDs, values, hashes);
  - every library proxy contract on both stores;
  - migration from an empty database, and a newer-schema refusal;
  - `Pinned` blocks in-place edits;
  - an import-decision-table property test (FsCheck: re-importing the same catalogue is a no-op; import order does not matter);
  - history: cap eviction skips starred entries, sort by last open, a missing folder is flagged;
  - kit files: round-trip, atomic-write, hash-change detection and lock tests.

### N. Proposed phasing (each slice green on its own)

1. **Pure data first:** the tabulated-material data case; canonical JSON + hash; per-kind schemas; export of the code seeds to the catalogue files; tests load the files; Domain seeds deleted.
2. **Library database (slow data only)** behind the library proxies, plus migrations, backup, `Pinned`, locations and contract tests. Catalogue import on start, with the decision table and extra catalogue folders.
3. **Experiment-kit folder:**
   - the format, `ExperimentKitProxy`, the lock and change detection;
   - the start screen and history (100, stars, last-open sort);
   - today's experiment collection and scene persist into the kit;
   - data copy-in + hashes;
   - the library snapshot + resolution on open;
   - Main "Save as kit…".
4. **Send kit / open kit from zip;** single-entry library exchange; "Copy experiments from another kit".
5. **Items + item models + parameters** (Domain, item files, the Items panel).
6. **Setups as slots;** the N-object run law; the real source; the slot bar and grid generator.
7. **Experiments v2:** roles, sessions, multi-variable scans, the import wizard with kit-stored profiles, split-by-column, template match, exclusions, corrections.
8. **Study + fit:** scopes and links, staged and joint fits, fit-run files, results, publish / promote. Then the Mueller acceptance test, its folder twin and the synthetic twin.
9. **0044 multi-cut through the Study model; Measurement Planner** (`018` §3) on top.

## 4. Open questions for the operator (short; none blocks writing the spec)

- **Extensions:** confirm the `.<kind>-json` family (library: `material-json`, `sample-json`, `item-model-json`, …; kit: `kit-json`, `item-json`, `setup-json`, `experiment-json`, `study-json`, `fitrun-json`). Is a plain `.zip` acceptable for "Send kit…"?
- **History:** starred kits pinned in a top section (as proposed), or one list sorted only by last-open date with a "starred only" filter?
- **Code seeds:** delete them after the export to files (one source of truth), or keep them as a test-only fixture?
- **`.ocproj`:** retire it in favour of kit folders, or keep it as a "save scratch scene" format for the forward constructor?
- **Naming:** keep `Experiment` for one run and `Study` for the combined inverse problem (the experiment collection grows into it), or rename?
- **First target for slice 8:** the optics-mueller staged fit (proves the pipeline against existing results), or the joint fit?

## Sources

- SQLite as an application file format: https://sqlite.org/appfileformat.html
- SQLite over a network (locking caveats): https://sqlite.org/useovernet.html
- OneDrive sync and SQLite reports: https://forums.tropy.org/t/error-with-onedrive-sync/2961 ·
  https://sqlitetoolsforrootsmagic.com/forum/topic/rmv11-any-idea-what-the-new-temporay-files-are-for/ ·
  https://github.com/Sonarr/Sonarr/issues/1886
- OneDrive Known Folder Move (Documents redirected): https://learn.microsoft.com/en-us/sharepoint/redirect-known-folders
- OpticStudio catalogue folders, project directories and the network-drive caveat:
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
- Earlier reports in this folder: `018-sharing-libraries-inverse-planner.md` (identity, decision table, online libraries,
  Measurement Planner); optics-mueller `notes/matrix_solution.md`, `notes/final_normalization_caveat.md`,
  `analysis/matrix_solution/step1/report.md`, `analysis/matrix_solution/step2_linear/report.md`.
