# 020 — Library files, the slow-data database, experiment-kit folders, and multi-setup inverse experiments (feature list)

Research and proposal only. No code was changed.

**Revision 5 (2026-11-01, the operator's fourth round; spec 0048 `.manual/001`):**

- **Build machines run without OneDrive.** ASG's `Disable-OneDrive.ps1` removes it, for every account. §G says what that settles for the build and what it leaves to the product.
- **Every test gate runs on Microsoft.Testing.Platform**, so gate filters are xUnit v3's (`--filter-not-trait "Category=ui-smoke"`), not VSTest's (§Q).
- **The ELEV gates now exist.** They found 3,757 findings in the current tree (ledger `specs/0048/.manual/002`). `021` puts that cleanup first, before this list's phases.

**Revision 4 (2026-11-01, the operator's third round of comments)** — every open question is now decided (§4, §5):

- **The kit root is flat;** subfolders hold only additional data (§A).
- **AT-2 is confirmed** as `AbsorbingBiaxialNoisyTests` (§1b, §Q).
- **The Measurement Planner's first target is gyrotropic crystals,** with exact references to the existing code, specs and tests (§M).

**Revision 3 (2026-11-01, the operator's second round of comments)** — every change below is a decision now (§4):

- **The direct and the inverse problem work the same way: both work inside an experiment-kit folder.** There is no scratch scene and no `.ocproj`.
- **File names carry no useless part.** The kit's own file is `.kit-json`, not `kit.kit-json`, and every other name follows one rule (§A).
- **No legacy, no migration.** There are no schema up-converters and no database migrations, and superseded code is deleted rather than kept beside the new code (§R).
- **Agreed:** the extensions; starred kits always on top, sorted by last open date; the names `Experiment` (one run) and `Study` (the combined inverse problem).
- **Two acceptance tests** close slice 8 (§Q):
  - one replicates optics-mueller;
  - one replicates the most complex existing inverse test, `AbsorbingBiaxialNoisyTests` (several samples, noisy measurements).

  Both REUSE the existing code — nothing is copied — and no new inverse test is written.
- **The relevant parts of `018` are now part of the plan:** identity, immutability, provenance, the import decisions, online material sources, and the Measurement Planner (§B–§D, §H, §M).

**Revision 2:**
- the database holds slow data only (the library);
- experiment kits are folders of plain files with a history of recent kit folders;
- sending a kit means sending its folder.

Inputs:
- `019-handover.md`, `002-plan.md`, `018-sharing-libraries-inverse-planner.md`;
- the constructor and test code at `c0bfb48`;
- the Mueller experiment in `C:\GitHub\optics-mueller`;
- specs 0038 (§13–16), 0042 and 0044;
- a web survey (sources at the end).

## Short answers

- **Two homes for data.**
  - **Slow data** (changes rarely, shared by every kit): the library, in a local SQLite database.
  - **Fast data** (one experimental campaign): an experiment-kit folder of plain files.
- **The library leaves the code** and becomes JSON files, typed by extension (`.material-json`, `.sample-json`, `.source-json`, `.polarizer-json`, `.detector-json`, `.item-model-json`, `.category-json`; one `.catalogue-json` per catalogue folder).
  - It is imported at every start; a manifest-hash comparison makes that cheap.
  - A changed built-in mints a new version; user entries are never touched.
  - The code seeds are exported once and then deleted.
- **The database:** SQLite through EF Core, holding only:
  - library entries with their versions;
  - categories;
  - the catalogue-import log.

  It is created from the model, with no migrations.
- **Every constructor session is in a kit folder.** Main (direct) and Inverse both start at the kit start screen. A kit holds:
  - its items, setups and experiments (with or without data);
  - its studies and fit runs;
  - the raw data files;
  - a snapshot of every library version it uses.
- **The kit-folder history:** 100 folders by default (`KitHistoryLimit`). Starred kits always sit on top, and each group is sorted by last open date. The history lives in the per-user preferences, not in the database.
- **Exchange:**
  - a kit = its folder (or one zip of it);
  - a library entry = its `-json` file;
  - a curated library = a catalogue folder.

  A received kit's library snapshot is checked against the local database, and nothing is overwritten silently.
- **The inverse problem:**
  - **items** are the physical pieces, each with its OWN parameters, known or unknown;
  - **setups** place items in slots on the beam, two or more samples allowed;
  - a **study** fits many experiments together.

  An unknown belongs to its item, or to its material when several samples share it, so every experiment that uses that item or material shares it automatically.

---

## 0. What the app does today (the evaluation)

- **Nothing persists.**
  - Every store is in memory, and every start re-seeds (`Berreman/OpticalConstructor/OpticalConstructor.Ui/AppContext.fs:69`).
  - `.ocproj` (`OpticalConstructor.Storage/ProjectFile.fs`) is never saved or opened by any window.
  - `Scene.SceneProxy` and `ExperimentCollectionProxy` are in-memory seams.
  - The EF Core project holds one `Test` table that nothing references.
  - `OpticalConstructor.Seeding` pushes the code seeds into in-memory stores.
  - On disk there are only `recent.json` (`OpticalConstructor.Storage/RecentFiles.fs`, `%APPDATA%\Softellect\Berreman\OpticalConstructor\`, capped at 16) and the user-environment preferences.
- **The library is code.**
  - `MaterialLibrary.builtInEntries`, `Library.SeedSamples`, `SeedSources`, the presets in `Library.seedEntries` (`OpticalConstructor.Domain/ElementId.fs:856`), and `standardCategories`, all with frozen GUIDs.
  - Material physics is data (`MaterialComplexity`), with one exception: **tabulated imports are closures** (`complexity = None`, `OpticalConstructor.Storage/MaterialImport.fs:97`).
- **An Experiment** (`OpticalConstructor.Domain/Experiments.fs:213`) is a frozen table snapshot:
  - one varied element with one variable over one range;
  - one capture mode;
  - ONE data file, stored as a machine-specific path.
- **Table elements are "kind + library entry"**, so two LPs of one preset are indistinguishable.
  - Polarizers are ideal or constant-Mueller (`ElementId.fs:304`); nothing is parameterised.
  - One sample per run: `measuredElementOf` takes the first bound sample (`OpticalConstructor.Ui/TableAndElementRotationView.fs:1339`).
  - The first polarizer sets a unit-intensity state (`OpticalConstructor.Domain/ExperimentRun.fs:36`).
- **There is no inverse solve in the UI.** The Inverse flow ends at the stub `SolverHandoffView`.
- **The working inverse code is all outside the constructor:**
  - `MuellerReconstruction` (spec 0042) reproduces the optics-mueller fit, through a regex over run names (`MuellerReconstruction.fs:400`) and the hard-coded `MuellerDataSet` DU over `Berreman/Data/MuellerMatrix/data.zip`;
  - `MuellerInverse` (spec 0044) is the multi-cut material fit;
  - the noise model, the fit wrappers and the ensemble statistics live in the TEST project (`BerremanTests/InverseFitHarness.fs`).

  The constructor reaches none of it.
- **The data-file schemas are too narrow:** strict `X,Y` and `wavelength_nm,psi_deg,delta_deg`. Real lab files are one CSV with many runs keyed by a column, raw dial readings, timestamps and several signal columns.

## 1. The two reference cases

### 1a. optics-mueller (real data)

- **Items:**
  - a laser (nearly linear, intensity drifts); lens and diaphragm (passive);
  - LP1 / LP2;
  - CPL1 / CPL2 (LP + retarder, δ ≈ 80–84° unknown, CPL1 axis offset unknown — fitted 39.3° instead of 45°);
  - QZ and LR (unknown 4×4 Mueller matrices);
  - the phone (dark offset, gain that jumps between session blocks).
- **Setups:**
  - source {LP1, CPL1} × analyzer {LP2, CPL2};
  - object slots 1 / 2 ∈ {AIR, QZ, LR, QZ then LR};
  - φ ∈ {0, +45, +90}.
- **One experiment** = an analyzer-dial sweep, 0…360° every 20°, recorded as raw dial readings. The dial zero and sign differ between LP and CPL.
- **Diagnostics:** Darkness, AIR repeats (`-BR`, `-2R`), BullshitCheck (a laser restart that splits a gain block), and lamp tests (ignored).
- **Corrections:** LR was labelled +45/+90 but physically sat at −45/−90 (fix the setup, not the data); one excluded frame; per-family gain rules.
- **Unknowns:** CPL1 (2), CPL2 (1–2), the dial zeros, M_QZ (16), M_LR (16), about five gains, one dark level.
- **Fit:** staged (AIR calibration, then the linear object fit), then the product check M_LR · M_QZ.
- **The repository is, in effect, already a kit folder:** adding `.kit-json` makes it one, and nothing moves.

### 1b. The most complex existing inverse test — `AbsorbingBiaxialNoisyTests` (synthetic, noisy)

- **Unknowns (15):** an absorbing, gyrotropic, triclinic crystal — n₁, n₂, n₃, six ε″ components, six gyration components.
- **Samples:** five 100 µm plates of ONE crystal:
  - the two optic-axis cuts (`opticAxisCut`, `otherOpticAxisCut`);
  - the Z, X and Y cuts.
- **Measurements:** 44 configurations (sets `a1`, `a2`, `a3` in `BerremanTests/AbsorbingBiaxialSample.fs`), each a measured Mueller matrix in transmission or reflection — normalized elements plus the absolute throughput.
- **Noise (`NoiseParam.decentLab`):** angles ±0.2°, elements ±0.005, throughput ±1 %. Five fixed seeds; the fit starts from `perturbedStart` inside `searchBox`.
- **Runtime:** about 946 s.
- **Why it is the right second test:** several samples share one material — the multi-cut, `MaterialScope` case — the measurements are noisy, and the fit is the largest the code has (15 unknowns).
- **Confirmed by the operator** (round 4) as AT-2: five plates, 15 unknowns — "a great test to play around".

## 2. Best practice and similar software (web survey, short)

- **Global or co-refinement with shared parameters** is the standard answer to "combine experiments":
  - *Origin and GraphPad Prism*: one value per shared parameter, fitted from all data sets;
  - *refnx*: `GlobalObjective`, sharing by REUSING the same parameter object — the "parameter belongs to the item" rule;
  - *SasView*: constraints that link models, plus weights;
  - *Woollam*: multi-sample analysis reduces correlation; CompleteEASE has parameter coupling.
- **Polarimeter calibration with imperfect elements.**
  - The Eigenvalue Calibration Method (Compain, Poirier & Drévillon 1999) finds W and A from three or four reference samples, with no assumptions beyond completeness and no precise alignment.
  - Dual-rotating-retarder calibration (Goldstein & Chipman; Chenault et al.) fits retardance and orientation errors (under 1° of error gives over 10 % matrix error).
  - Hence: calibration experiments are a role, and item parameters are fit targets.
- **Physical realizability** of a fitted Mueller matrix (coherency eigenvalues ≥ 0) is a standard post-fit check.
- **Library versus project (Zemax OpticStudio):**
  - a shared catalogue folder (AGF files, path settable);
  - per-project directories since 21.3;
  - a warning against network or cloud drives for the shared data folder.

  This is the same split as here.
- **SQLite as a document format.**
  - SQLite's own case for single-file documents.
  - Its network caveats (locking misbehaves on network and sync file systems).
  - Forum reports of OneDrive-synced databases corrupting.
  - Conclusion: a LOCAL SQLite file fits the slow library; plain-file folders fit work that is sent, synced or versioned.

---

## 3. Feature list

### A. Names — one rule, applied everywhere

- **The rule** (no useless name part):
  1. A file that exists **once per folder** is named by its extension alone: `.kit-json`, `.catalogue-json`.
  2. A file that exists **many times** is named by its own identity (label, name, id + version, timestamp) plus its kind extension.
  3. **No folder or name part repeats** what the extension already says.
     - **The kit root is flat (decided):** every kit file sits at the root, and its kind is its extension.
     - Subfolders hold only ADDITIONAL data: `library/` (copies of slow data), `data/` (raw lab files), `.oc/` (app cache), and any further data a later feature needs.
     - A subfolder never exists to group one kind of kit file.
- **Library extensions (agreed):** `.material-json`, `.sample-json`, `.source-json`, `.polarizer-json`, `.detector-json`, `.item-model-json`, `.category-json`; the catalogue manifest is `.catalogue-json`.
- **Kit extensions (agreed):** `.kit-json` (the kit itself), `.item-json`, `.setup-json`, `.experiment-json`, `.study-json`, `.fitrun-json`.
- **Catalogue folder layout:**

  ```
  <catalogue>/
    .catalogue-json                          name, catalogue id, version, entry hashes
    Silicon.material-json
    Langasite (La3Ga5SiO14).material-json
    Transparent glass (n = 1.52).material-json
    …
  ```

  - The file name is the entry's name, made filesystem-safe; uniqueness per kind within a catalogue is checked on export.
  - Identity is the GUID INSIDE the file, never the file name (`018` §1).
  - There are no category folders: the category lives in the file, and a folder would only repeat it, or contradict it.
- **Kit folder layout:**

  ```
  <kit>/
    .kit-json                                the kit: id, name, author, notes, naming template, import profiles, sendExclude
    LP1.item-json  CPL1.item-json  QZ.item-json  Phone.item-json  …
    CPL-(QZ#45-LR#45)-CPL.setup-json  …
    CPL-(QZ#45-LR#45)-CPL.experiment-json  …
    QZ-LR.study-json
    QZ-LR@20261101T120000Z.fitrun-json  …
    library/<id>@v<n>.<kind>-json            snapshot of every library version the kit uses
    data/…                                   raw lab files (default home for attachments), never modified
    .oc/                                     app-owned cache and lock; safe to delete; never sent
  ```

  Names that come from the naming template keep the lab's characters. Only characters Windows forbids (`: * ? " < > |`) are replaced.
- **Code names drop the parts their scope already supplies:**
  - module `Kit` with `Item`, `ItemId`, `ItemLabel`, `Parameter`, `ParameterState`; the library type `ItemModel`;
  - `KitFolder` (the folder path type);
  - proxies `KitProxy`, `KitHistoryProxy`, `KitArchiveProxy`, `CatalogueProxy`, `MaterialSourceProxy`;
  - `KitContext`; the setting `KitHistoryLimit`;
  - messages `CreateKit`, `OpenKit`, `OpenKitZip`, `StarKit`, `UnstarKit`, `ForgetKit`, `SendKit`;
  - UI labels "New kit…", "Open kit…", "Open kit from zip…", "Send kit…".

  The UI calls the concept "experiment kit"; the code says `Kit`, because there is no other kind of kit.
- **Database file:** `library.db`.

### B. The library as external files (update without recompiling)

- **One entry per file;** it diffs, reviews and merges in git, and a single entry can be emailed or dropped in.
- **Every entry file carries:**
  - `schema` (kind + schema version), `id` (the frozen GUID), `version`;
  - `name`, `category` / facets, `description`;
  - `payload` (the elevated domain value);
  - `provenance` — built-in / user / imported-from-kit / pulled-from-source with source, record, source version, **licence and citation** (`018` §1–2);
  - `contentHash` (SHA-256 over canonical JSON: sorted keys, invariant-culture numbers).

  **Identity is GUID + version + hash, never the name** (`018` §1).
- **JSON Schema per kind,** validated on load. The System.Text.Json + FSharp.SystemTextJson + JsonSchema.Net stack in `ProjectJson` is REUSED. The `.ocproj` aggregate itself goes (§R).
- **No up-converters.** A file whose schema version differs from the app's is rejected with a typed `UnsupportedSchemaVersion` error, never a crash. The same holds for kit files.
- **Physics must be pure data.** A **tabulated** `EpsWithDispValue` case (λ, n, k arrays + interpolation rule) replaces the closure-backed tabulated imports. Any entry that cannot serialise is a test failure.
- **Shipped catalogue:**
  - `<install>\Catalogue\Builtin\`, read-only, copied by the build from `Berreman/OpticalConstructor/Catalogue/Builtin/`;
  - the code seeds are exported ONCE by a throw-away tool (GUIDs and values byte-for-byte), then the Domain seed values are **deleted**;
  - tests load the same files.
- **Licence attribution:** CC-BY entries show their attribution in the Library and Materials windows and in the Details view, and kits carry it in their snapshots (`018` §2).
- **Import on every start:** compare each catalogue's `.catalogue-json` hash with the one recorded in the database, and import only on change.
- **Per-entry import decision** (one table for catalogues, kit snapshots and single dropped files; `018` §1):

  | Incoming | Action |
  |---|---|
  | New id | Add |
  | Same id, same hash | Reuse |
  | Same id, version not held locally | Add that version; the local "latest" moves only for a protected built-in from a catalogue |
  | Same id and version, different hash | A divergence; import as a flagged fork with a new id |
  | Different id, same name | Side by side ("Si — from A"), with an explicit "use my Si instead" re-binding and a diff preview |
  | Entry removed from its catalogue | Mark `InactiveEntry` (still resolvable) |

  The decisions show as one dialog with a row per entry, and nothing is overwritten silently.
- **Extra catalogue folders** (the `018` shared-folder library): read-only folders on a share, OneDrive or a git checkout.
  - Permissions are the folder's own.
  - Versions are immutable files, so the only possible conflict is two people adding the same name, which is harmless because the ids differ.
- **Export:** any entry → its `-json` file; a selection or a category → a catalogue folder. Exporting **pins** the exported versions (§C).
- **`SeedingProxy` / `seedAll` become `CatalogueImport`:** the same seam shape, fed from parsed files; the step-042 in-memory proof stays as the test double.

### C. The library database — slow data ONLY (SQLite, no migrations)

- **What it holds:** library entries of every kind (materials, samples, sources, polarizers, detectors, item models) with immutable version history, lifecycle and protection; categories; the catalogue-import log; pulled-source records (§D).
- **What it never holds:**
  - kit items, setups, experiments, studies, fit runs;
  - measured data;
  - the kit history and other preferences.

  The rule for additions: if it would travel with a kit, it goes in the kit; if every kit shares it and it changes rarely, it may go in the database.
- **Hybrid shape:** relational columns for what is queried or constrained (id, version, kind, name, lifecycle, protection, facet keys, sample-version → material-version keys), plus the canonical JSON payload. The library file, the database payload and a kit's snapshot of an entry are byte-identical, and the hash proves it.
- **Versions are immutable;** "edit latest" follows `Lifecycle.decideVersioning`.
- **`Pinned` replaces `VersionsInUse`.**
  - A version becomes `Pinned` — a one-way flag — when a kit first references it, or when it is exported (`018`: immutable once saved or exported).
  - A pinned version never mutates; an edit mints the next version.
  - The kit's snapshot hash is the second safeguard.
- **No migrations.**
  - The database is created from the model (`EnsureCreated`) and stamped with its schema version.
  - A mismatch is a typed `LibrarySchemaMismatch` error, and the remedy is to recreate the database from the catalogues.
  - User entries survive only through "Export my library" first. That is acceptable while there is no release; migrations arrive with the first release that has users.
- **The library proxies keep their shapes and gain a SQLite `create`:** `MaterialProxy`, `CategoryProxy`, `SampleProxy`, `SourceProxy`, the `LibraryProxy` live view, and the preset and item-model stores.
- **Contract tests:** each library proxy's suite runs against the in-memory store AND SQLite `:memory:`.
- **Settings:**
  - the connection string comes from `appsettings.json` (the elevated `ConnectionString` in `AppConfig.fs`);
  - WAL mode (the file is local only);
  - one writer;
  - "Back up library now…" (`VACUUM INTO`) and "Restore library…".

### D. Online material sources (from `018` §2) — slow data

- **`MaterialSourceProxy`** (search, fetch) with adapters, in this order:
  1. **refractiveindex.info** (CC0): an offline snapshot of the GitHub database plus an optional "Update snapshot". The existing YAML importer (`MaterialImport`) is REUSED as the record parser.
  2. **PTB OCDB** (CC BY 4.0): EUV / VUV n, k **with uncertainties**.
  3. **A Henke-based compound calculator** (CXRO f₁, f₂ + formula + density) for EUV and X-ray. Check its redistribution terms before shipping the tables.
- **A pulled record becomes a user material version** in the database, with provenance: source, record path, snapshot date or commit, licence, citation.
- **Updating a source** mints a NEW version when the record changed and never mutates one.
- **Uncertainties travel with the material** (PTB's σ) and become priors in studies and inputs to the Planner (§L, §M).
- **User-supplied glass catalogues** (Schott / Ohara / Hoya / CDGM AGF): import only, never bundle.
- **Materials Project** dielectric tensors are a prior (a starting guess) only, never reference data.
- **Paywalled sources** stay manual paste.
- **Known gaps:**
  - no free gyration or magneto-optic data (user-entered from papers);
  - how refractiveindex.info encodes uniaxial and biaxial crystals must be checked on the first real import.

### E. The experiment-kit folder — fast data as plain files

- **A kit is a folder** recognised by its `.kit-json` (§A layout).
  - The kit file holds the kit id (GUID), name, author, created and modified times, notes, the naming template (§J), the import profiles (§K) and the `sendExclude` globs.
  - Every other file is either a kit file the app owns or a lab file the app only reads.
- **The direct and the inverse problem use the SAME kit.**
  - Forward experiments are experiments without data.
  - Inverse studies add data and unknowns.
  - One kit may hold both, so a user can design a run forward, then attach its measured file to the same experiment.
- **References inside a kit are relative paths** (forward slashes) plus the SHA-256 recorded at attach time.
  - A changed or missing data file is flagged on open and before each fit ("data changed since attach / since the last fit run").
  - Absolute paths are never stored.
- **Attaching a file from outside the kit copies it into `data/`,** keeping its name and suffixing on collision.
- **Lab files are read-only for the app.** Exclusions, corrections and mappings live in the experiment file.
- **Library closure snapshot (`018` §1, the Zemax `.zar` / KiCad lesson: always embed).**
  - Every library version the kit references is copied into `library/` when first referenced, built-ins included, because the receiver may run another catalogue version.
  - So the kit opens on any machine.
- **Resolution on open** (the §B decision table, applied to the snapshot):
  - same id, version and hash → the database entry;
  - missing locally → the snapshot read-only, with "Add to my library";
  - divergence → reported; the kit keeps its snapshot;
  - name-only collision → side by side, with the explicit re-binding.

  One dialog shows all rows; nothing changes in the database unconfirmed.
- **Writes are atomic** (write `*.tmp`, then replace).
- **Fit runs are append-only** and store the full parameter vector they used and produced.
- **Concurrency:**
  - one window per kit;
  - a `.oc/lock` (machine, user, process, time) warns across processes and machines, with an "open read-only" choice;
  - on activation, files changed outside the app (another editor, git, a sync) are offered for reload.
- **Large lab files** (e.g. 24 MB DNG images) may stay in the kit. The app reads only what experiments reference, and "Send kit…" honours `sendExclude` (e.g. `**/*.dng`).
- **Review round-trip (`018` §1):** notes on experiments, studies and fit runs live in their files, so a reviewer's comments come back in the returned folder or zip; a git-managed kit gets the full history.

### F. Starting work — the kit start screen and history

- **Both Launcher buttons, Main and Inverse, open the start screen.**
  - Choosing a kit opens the constructor in that mode.
  - Inside a kit the mode can be switched.
  - The Materials and Library windows work on the library database and need no kit.
- **Start-screen actions:**
  - **New kit…**: choose or create a folder. An empty folder is initialised; a folder that already has files keeps them, and `.kit-json` plus the app's subfolders are added.
  - **Open kit…**: a folder without `.kit-json` offers "Make this a kit".
  - **Open kit from zip…**: choose a target folder, extract, open.
  - **The history list.**
- **History (decided):**
  - up to `KitHistoryLimit` = 100 folders (`appsettings.json`);
  - **starred kits always at the top**, sorted by last open date; then the rest, sorted the same way;
  - the cap evicts the oldest UNSTARRED entry and never a starred one;
  - each row shows the name, folder, last-open date and the star;
  - a filter box;
  - a missing folder shows greyed, with **Locate…** / **Remove from history**.
- **Storage:**
  - the history lives in the per-user preferences (`%APPDATA%\Softellect\Berreman\OpticalConstructor\`), replacing the 16-entry `recent.json` list (the one recent store spec 0038 §16.3 asked for);
  - entry: folder, kit id, name cache, last opened, `Starred | NotStarred`;
  - the existing pure MRU logic (`RecentFiles.bump`) is REUSED and extended with `star`, `unstar`, `forget` and `evict` over a clock provider.

### G. Locations

- **Library database:** `%LOCALAPPDATA%\Softellect\Berreman\OpticalConstructor\library.db`, with `Backups\` beside it. Local; never on OneDrive or a share.
- **Preferences:** `%APPDATA%\Softellect\Berreman\OpticalConstructor\`.
- **Kit folders:** anywhere; the suggested parent is `Documents\Optical Constructor\Kits\`. Plain files plus atomic writes make OneDrive, a share or git workable; the lock warns against opening one kit on two machines at once.
- **Exports:** `Documents\Optical Constructor\Exports\` (already used by `ExperimentExportFile`) and `…\Catalogues\`.
- **Shipped catalogue:** `<install>\Catalogue\`.
- **Every path is overridable** in `appsettings.json`; a Launcher "Data locations" panel shows them, each with "Open folder".
- `CLAUDE.md`'s "never `%LOCALAPPDATA%`" rule covers arc-runner WORKER output, not product data.
- **Build machines have no OneDrive** (operator, 2026-11-01). ASG's `Disable-OneDrive.ps1` removes it from every build machine, for every account. It sets the policies that keep it out, and refuses to run while a cloud-only file would be lost.
  - **On a build machine:** `Documents`, `%APPDATA%` and `%LOCALAPPDATA%` are on the local disk. No test, fixture, acceptance kit or gate may rely on a OneDrive folder or work around one. Test kits and databases live under the test run's output directory (§Q), never under `Documents`.
  - **On users' machines,** OneDrive may still own `Documents`. That is why the rules above hold: the database is local in `%LOCALAPPDATA%`, never in a synced folder; kits are plain files written atomically, with a lock.

### H. Exchange

- **A kit:** copy its folder.
  - **Send kit…** writes one `<kit name>.zip` (excluding `.oc/` and `sendExclude`), and **Open kit from zip…** extracts it.
  - There is no separate bundle format; this answers `018`'s bundle-extension question.
- **A library entry:** its `-json` file, dropped onto the Library or Materials window (§B decision table).
- **A curated library:** a catalogue folder, registered on the receiving side.
- **Between kits:** "Copy experiments from another kit…" brings the experiments with their setups, data files and the items they use. Items map onto existing items where the user says so; otherwise they are added.

### I. Items — the physical pieces (`<label>.item-json`)

- **`Kit.Item`** = one PHYSICAL piece:
  - `ItemId` (GUID) and an `ItemLabel` (`LP1`, `CPL2`, `QZ`, `Phone`);
  - the `ItemModel` it instantiates (pinned and snapshotted);
  - its own `Parameter` values;
  - optional photo (a kit file) and notes.

  Two LPs of one model are two items with independent parameters.
- **`ItemModel`** — parameterised physics, a library entry (`.item-model-json`). Each model publishes its parameter axes, so the fitter's `ParameterAxis` list is generated, not hand-written.

  | Model | Parameters |
  |---|---|
  | Light source | λ or tunable limits (exists) + output polarization (azimuth, ellipticity, degree of polarization) + an intensity scale |
  | Ideal LP / ideal CP | None (exists) |
  | Real linear polarizer | Extinction ratio or diattenuation, axis offset |
  | Retarder plate | Retardance δ (optionally dispersive), fast-axis offset |
  | Compound in light order | E.g. CPL = `LP · Retarder(axis = 45° + Δ, δ)` — "LP-CP of unknown CP rotation"; generalises `ConstantMueller` with parameters |
  | Sample by stack | The existing `Sample` (materials, thicknesses, cut, orientation); unknowns may be a thickness, an orientation, or a material parameter |
  | Sample as black-box Mueller matrix | 16 parameters (or 15 + scale) at reference orientation; rotation as `R(−φ) M R(φ)` |
  | Empty slot (air) | Identity, the calibration reference |
  | Intensity detector | Gain (session-scoped), dark offset, noise model |
  | Mueller polarimeter / ellipsometer | Records a full Mueller matrix, or Ψ / Δ; noise model (angles, elements, throughput — §Q reuse) |

- **`Parameter`** = a name plus a `ParameterState`: `Known of value` | `Unknown of start * bounds * prior option` | `Calibrated of value * sigma * fromFitRun`.
- **Dial mapping:** a rotatable item has a zero and a sign (`DialSign` exists). Experiments record raw dial readings, and the zero is an ordinary parameter.
- **Revisions:** an item file carries a revision number and a short value history; "Publish calibration" writes the next revision. Old results stay reproducible because each fit run stores its parameter vector.
- **Material parameters as unknowns:**
  - a study may OPEN a material version, which creates a study-local working copy, stored in the study file, with `Unknown` components;
  - every sample made of it shares them (`MaterialScope` — case 1b);
  - **Promote to library** mints the material's next version in the database, with provenance "fitted in kit K, study S, run R".

### J. Setups, slots and quick swap (`<name>.setup-json`)

- **`Setup`** = an ordered optical path of slots, each holding an item or nothing:
  - Source, Prep 1…k, Object 1…n (**two or more samples in series**), Analyzer 1…k, Detector;
  - per slot: position on the beam, R1 / R2 / R3, or a dial reading.

  The table draws the active setup, and the `BeamLayout` / `RayModel` laws are unchanged.
- **Experiments reference setups** (name + revision). The `ElementDescriptor` list becomes the slot list.
- **Quick swap:**
  - a slot bar above the table shows one chip per slot (`Source: Laser | Prep: CPL1 ▾ | Obj 1: QZ ▾ @ 45° ▾ | Obj 2: LR ▾ @ 45° ▾ | Analyzer: CPL2 ▾ | Detector: Phone`);
  - each dropdown lists compatible items only;
  - choosing one is either a new setup or an existing one;
  - keyboard: Tab between chips, letters pick.
- **Naming template:** each kit carries one (e.g. `{source}-({obj1}#{phi1}-{obj2}#{phi2})-{analyzer}-{note}`).
  - It names setups and experiments, and PARSES names back to match imported data.
  - It replaces the hard-coded regex in `MuellerReconstruction.parseExperiment`, which becomes the template engine's first test case; it is not kept beside it.
- **Grid generator:** a set of options per slot → their product, with impossible or unwanted combinations unticked → all setups and experiments created at once.
- **Direct problem:** several experiments over one setup, as today, now saved in the kit.

### K. Experiments and data (`<name>.experiment-json`, lab files in the kit)

- **`Experiment`** (decided name) = one recorded run:
  - a setup reference and a **scan**: one or more variables (dial 0…360 step 20; φ; incidence × azimuth × observable; λ on a tunable source);
  - a capture (T / R, from the detectors);
  - a role: `Science | Calibration | Dark | DriftCheck | Diagnostic | Ignored`;
  - a session;
  - the data attachment: a relative path + hash + import profile + row selection.
- **Column-mapping import wizard:**
  - sniff the delimiter, header and BOM, and show a preview;
  - map columns to scan variable(s) (unit; raw dial vs physical), signal, σ, timestamp, capture index, experiment key — or to Mueller elements m00…m33 plus configuration columns;
  - save the mapping as a named **import profile** in `.kit-json`.

  Built-in profiles REUSE the existing parsers instead of keeping a separate v1 path:
  - `MeasuredData.parseIntensitySeries` / `parseEllipsometricSeries`;
  - `MuellerDataStore.parseMuellerCsv` (the "Phone ROI CSV" profile).
- **One file → many experiments:** split by a key column, match each group by the naming template, list the unmatched groups and offer to create setups for them (the template parse supplies slots and angles).
- **Raw data is immutable;** exclusions carry a reason and stay visible in plots, marked.
- **Corrections:** re-point an experiment's setup after the fact (e.g. "LR was at −45"), with a mandatory audit note.
- **Observables:** intensity, Ψ / Δ, full Mueller matrices (the 0044 `MuellerObservation` shape, normalized + absolute throughput), and later N / C / S. A study mixes them.
- **Status badges:** no data / mapped / validated (points) / excluded points / data changed / out of range.

### L. Studies — combining experiments into one inverse problem (`<name>.study-json`, `<study>@<stamp>.fitrun-json`)

- **`Study`** (decided name) grows from the experiment collection: a named set of experiments inside ONE kit, plus:
  - the unknowns (owner + scope);
  - sessions;
  - fit settings;
  - its append-only fit runs.
- **Scope is the sharing rule:**

  | Scope | Meaning |
  |---|---|
  | `ItemScope` | One value for every experiment using the item (CPL1.δ, M_QZ) |
  | `MaterialScope` | One value for every sample of an opened material (case 1b) |
  | `SessionScope` | One value per session block (gain) |
  | `ExperimentScope` | One value per run |

  Explicit links ("CPL2.δ = CPL1.δ") are constraints.
- **Sessions and gain models as data:**
  - auto-split at Diagnostic experiments (or by hand);
  - per session the gain is free, anchored to its AIR runs, or interpolated between two AIR repeats;
  - dark from the Dark runs.

  These rules replace the hand-written family rules (`MuellerReconstruction.familyGain` becomes their first implementation, not a parallel one).
- **Fit modes:**
  - **joint**;
  - **staged**: stage 1 calibrates items on Calibration runs (ECM-style); **Publish calibration**; stage 2 fits objects with those values fixed, or as priors with σ.
- **Engines — the existing ones, reused:**
  - the linear path is `MuellerReconstruction.buildDesign` + `reconstruct` (SVD, rank, condition);
  - the nonlinear path is `InverseFit` / `MuellerInverseSolver` (Alglib, multi-start) over `MuellerInverse.forwardModels`, driven by the generated parameter axes.

  The choice is automatic: linear when the unknowns enter linearly.
- **Weights and ablation:** a weight per experiment or per detector; enable / disable per experiment.
- **Results**, stored in the fit run:
  - value ± σ (Jacobian covariance; `FitQuality` REUSED);
  - correlation matrix, rank, condition;
  - per-experiment residuals and RMSE;
  - Mueller realizability;
  - the product check for series objects (M_comb against M_LR · M_QZ);
  - **post-fit identifiability by profile likelihood** for suspect parameters (`018` §3, Raue et al.).
- **Publishing:** item → next revision (`Calibrated`); material or black-box object → **Promote to library**.

### M. Measurement Planner (from `018` §3) — kit-driven

- **First target (decided): gyrotropic crystals,** transparent and absorbing, from uniaxial to triclinic. This is the class the existing inverse code already covers, so the planner starts on proven ground.
  - **Code:**
    - `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerInverse.fs`: `UniaxialParameters` (4 unknowns), `TriclinicParameters` (9), `AbsorbingTriclinicParameters` (15), `SampleCut`, `orientForCut`, `internalAngle`, `gyrationProjection`, `jacobianColumnNorms` (`:884`);
    - `OpticalConstructor.Optimization/FitQuality.fs`: `residualJacobian`, `FitReport.standardErrors`;
    - `predictedRelativeErrors` / `combinationVariance` (in `BerremanTests/InverseFitHarness.fs` today; promoted by §Q).
  - **Specs:** `specs/0044/.manual/004-active-anisotropic-inverse-problem.md` sets out the physics:
    - a wave sees only the tensor components transverse to its propagation direction;
    - along the optic axis the plate is a pure circular retarder;
    - across the optic axis the activity is buried under the birefringence;
    - Snell caps the internal angle at arcsin(1/n_o) ≈ 40.4° for quartz, so more than one cut is required.

    `017-feasible-biaxial-experiments.md` covers the binormal optic-axis plate (100 µm vendor minimum, 0.11 wave per degree of misalignment). `015` and `019` are the noisy ensembles.
  - **Acceptance — the planner must reproduce existing facts.** Their designs and helpers are reused; no new physics test is written:
    - **Layer 1 / rank, `MuellerInverseTests`:** "C1 is completely blind to g33, which is why a second crystal cut is required", and "C1 alone cannot determine three of the four constants, which is why the design needs more cuts". The planner reports what C1 leaves unrecoverable and proposes the second cut.
    - **Layer 1 / rank, `BiaxialInverseTests`:** "the cuts buy the gyration DIAGONAL and only the tilt buys its OFF-DIAGONAL". For a normal-incidence-only kit the planner flags g12, g13 and g23 as unrecoverable, and it ranks oblique incidence AND further cuts as the remedy.
    - **Full rank, `MuellerInverseTests`:** "the full measurement set is well conditioned and every parameter is observable". The planner reports full rank for that design.
    - **Layer 2 / Cramér–Rao, `MuellerInverseTests`:** "an ensemble of noisy experiments recovers every constant, and its scatter IS what one fit's own covariance predicts". The planner's bounds for that design equal the promoted `predictedRelativeErrors`, and likewise for AT-2's design.
  - **Literature anchors** (`018` §3):
    - Arteaga, Canillas & Jellison: quartz g₁₁ and g₃₃ by two-modulator generalized ellipsometry;
    - HAUP-class measurements for activity along birefringent directions;
    - the 18 of 21 gyrotropic point groups (arXiv 2003.10872).
  - **Later targets:** isotropic films (the thickness ↔ n confounding of thin-film stacks) and uniaxial non-gyrotropic crystals.

- **Its inputs are the kit; there is no separate "lab profile" to type in:**
  - **lab profile** = the kit's items: sources and λ ranges, polarizers, compensators, compounds, detectors, angle ranges, and each detector's **noise model** (the same bench model §Q promotes);
  - **sample inventory** = the kit's sample items: cuts, thicknesses, substrates;
  - **prior** = `Known` / `Calibrated` parameters, material class or point group, dispersion choices, source uncertainties (PTB σ);
  - **targets** = the study's chosen unknowns.
- **Layer 1, structural (exact, tabulated):**
  - the point group fixes the tensor components: isotropic 1, uniaxial 2, biaxial 3 + orientation;
  - optical activity in 18 of the 21 non-centrosymmetric classes;
  - what each cut sees: a c-cut at normal incidence sees n_o only (and g₃₃); n_e needs oblique incidence or an a-cut;
  - activity along birefringent directions needs full-Mueller or HAUP-class measurements;
  - low-symmetry crystals need several cuts × azimuths × angles;
  - intensity-only detectors lose phase.

  It answers "this kit can never recover X" and "you need an off-axis cut".
- **Layer 2, numerical (local, at a nominal point):**
  - the Fisher information from the Jacobians of every candidate experiment — the study engine's own forward model and Jacobian, REUSED;
  - **rank** (identifiable combinations);
  - **smallest singular vectors** (confounded combinations, e.g. thickness ↔ n);
  - **Cramér–Rao bounds** per target;
  - **greedy D-optimal** (or target-restricted A-optimal) selection of the next experiment;
  - the instrument-matrix condition number for choosing polarizer and compensator states.
- **Output:**
  - a **recoverability report**;
  - a **measurement plan** emitted as setups + experiments with role `Planned` into the same kit, so "plan → measure → attach → fit" is one object model.
- **Cache:**
  - key = hash of (items + parameter states + targets + nominal point);
  - stored in the kit's `.oc/planner/` (derived, safe to delete, never sent);
  - candidate Jacobians are cached per (sample, experiment), so adding one item computes only the new candidates.
- **Shipped playbooks** (layer 1 and coarse layer 2 per crystal class × common kits) are catalogue data: slow, later, with an extension to be agreed.
- **Workflow:**
  1. kit;
  2. planner report and plan;
  3. measure;
  4. attach;
  5. fit;
  6. post-fit identifiability;
  7. the planner proposes the next experiment.

  No LLM is involved; it is deterministic linear algebra over the solver.
- **Later:** Bayesian expected information gain with sequential design.

### N. Solver and physics changes

- **The run law walks any number of objects.** `ExperimentRun` folds slot by slot, in light order, through each item's Mueller matrix. `measuredElementOf` ("the first bound sample") is deleted, and capture branches are decided per object.
- **The source is real:** a source item with a Stokes state and an intensity scale. "Unpolarized, unit scale after the first polarizer" is its default parameter set, so existing charts keep their values.
- **One item-model evaluator,** `ItemModel × parameters × orientation × λ → MuellerMatrix`:
  - stack samples go through Berreman (`Propagation.sampleMuellerSpun` for the table; `MuellerInverse.orientForCut` + `createBerremanForward` for cut plates — REUSED);
  - black-box and compound items go through the Stokes/Mueller algebra.
- **The forward model of an experiment** = slots + scan point + parameters → predicted reading: intensity → S₀ · gain + dark; polarimeter → Mueller elements + throughput; ellipsometer → Ψ / Δ.
- **Parameter-axis generation:** a material complexity or item model publishes its `ParameterAxis` list. Case 1b's generated axes must equal `AbsorbingTriclinicParameters.axes` — an exact test.
- **Long fits** run on `JobRunner`, with progress and cancel; the fit run is written on completion.

### O. UI

- **Launcher:** `Main / Inverse / Materials / Library`. Main and Inverse open the kit start screen (§F). A footer link opens "Data locations".
- **Constructor:**
  - the title shows the kit name and folder;
  - a left **flow strip**: Items → Setups → Experiments → Data → Unknowns → Plan → Fit → Results → Publish, each step reachable directly;
  - the Experiments bay's "Data files" section moves into Data.
- **Items panel:** the list, "Add item from library model…", a parameter grid (state, value, bounds, σ, provenance), the dial mapping, revisions.
- **Slot bar** over the table (§J), with "Record experiment"; the **grid generator**.
- **Experiment matrix view:** rows source × objects × φ, columns analyzer, with a data badge per cell.
- **Data dialog:** preview, column roles, an import-profile picker, the key split with a matched / unmatched table, per-point exclusion with a reason.
- **Study view:**
  - the unknowns table (owner, scope, fix / free, bounds, priors, links);
  - Run joint / Run stage;
  - the fit-run history;
  - results tabs: values, correlation, residuals, realizability, product check, profile likelihood.
- **Plan view:** the recoverability report (rank, confounded combinations, CRB per target) and the ranked candidates, with "Add to kit".
- **Kit menu:** Send kit…, Show in Explorer, Copy experiments from another kit…, Reload changes, Close kit.
- **Import dialog** (§B / §E): one row per entry, its status, the default action and an override.
- **Automation:** every new control gets a stable id in `UiIds` (`KitHistoryList`, `KitHistoryStarToggle`, `NewKitButton`, `KitItemList`, `SlotChip_<slot>`, `SetupGridGenerateButton`, `DataSplitKeyColumn`, `StudyRunFitButton`, `PlanRunButton`, `SendKitButton`, …), and every new window joins `ScrollExtentTests`.

### P. Proxies and semantic commands

- **IO stays in proxies:**
  - the library proxies over SQLite;
  - **`KitProxy`**, built per opened folder by `KitProxy.create (folder : KitFolder)`: kit, items, setups, experiments, studies and fit runs; data by relative path; copy-in; snapshots; lock; change check;
  - `KitHistoryProxy` (preferences);
  - `CatalogueProxy` (catalogue folders);
  - `KitArchiveProxy` (zip / unzip);
  - `MaterialSourceProxy` (online sources);
  - a `ClockProvider`.

  Logic receives a `KitContext`, and every proxy has an in-memory stub of the same shape.
- **Every user step is a semantic message:**
  - `CreateKit`, `OpenKit`, `OpenKitZip`, `StarKit`, `UnstarKit`, `ForgetKit`, `SendKit`;
  - `AddItem`, `SetParameterState`;
  - `CreateSetup`, `AssignSlot`, `SwapSlot`, `GenerateSetupGrid`, `RecordExperiment`, `SetExperimentRole`;
  - `AttachDataFile`, `SplitByColumn`, `MatchByTemplate`, `ExcludePoint`, `CorrectSetup`;
  - `CreateStudy`, `AddExperimentsToStudy`, `OpenMaterial`, `SetParameterScope`, `LinkParameters`;
  - `RunPlan`, `RunFit`, `PublishCalibration`, `PromoteToLibrary`.

  The same messages back the buttons, so tests drive by meaning, never by pixels.

### Q. The two acceptance tests (slice 8) — replication by REUSE, no copy-paste

**Common rules:**
- Each test builds its kit ONLY through the semantic messages (§P).
- Library: SQLite `:memory:`. Kit: an in-memory `KitProxy` stub.
- **The data and the expected values come from where they already live;** nothing is transcribed or copied.
- At the end the kit is written through the REAL folder `KitProxy` into a throw-away folder under the test run's output directory, zipped with `SendKit`, unzipped elsewhere, and reopened against an EMPTY library database. The test asserts:
  - the reopened kit is equal to the original;
  - the study's residual vector at its start point is equal too.

  This proves the folder is self-contained without paying for a second fit.
- Trait `kit-acceptance`, the `FsxAcceptanceTests` pattern, in `OpticalConstructor.Ui.Tests` (the headless app).

**AT-1 — optics-mueller:**
- **Kit:** laser, LP1, LP2, CPL1 / CPL2 (compound LP + retarder, δ and Δ unknown), QZ / LR (black-box Mueller), phone (session gain, dark).
- **Setups:** the grid over 4 families × {AIR, QZ, LR, QZ → LR} × φ. The **two-sample** setups put QZ in Object 1 and LR in Object 2.
- **Data:** the committed `Berreman/Data/MuellerMatrix/data.zip`.
  - The in-memory kit is seeded from its CSV entries through the existing `MuellerDataStore` reader, so there is no second copy of the data.
  - Attach, split by `experiment`, match by the lab template.
  - Roles for Darkness, BullshitCheck and the AIR repeats.
  - The LR correction and the excluded frame are applied as data.
- **Study:** staged (stage 1 AIR calibration → publish → stage 2 objects).
- **Expected:** the reference pipeline's full-precision values that `MuellerReconstructionTests` §7.2 ("section 7.2 end-to-end…") already asserts (provenance `Berreman/Data/MuellerMatrix/reference_step2_linear_summary.json`).
- **Reuse, so that ONE implementation exists:**
  - the stage pipeline that §7.2 composes by hand (`loadDataSet`, `reduceAirTraces`, `unwrapFit`, stage-1 constants, `familyGain`, `correctSignal`, `buildDesign`, `reconstruct`) is promoted into product code as the study's staged Mueller path, and §7.2 is re-pointed to call it;
  - the transcribed expected literals move from the §7.2 test body into one shared reference module, which both tests open. The §8.5 decision "the test reads no file for its expectations" stays intact.
- **Assertions:** M_QZ, M_LR, M_{QZ+LR}, the CPL retardances, the angle offset and the dial zeros within the existing `dataMatrixTol`; the product-check RMSE ratio.

**AT-2 — `AbsorbingBiaxialNoisyTests` (case 1b):**
- **Kit:**
  - a source at `referenceWaveLength`;
  - an ideal polarimeter detector with the `NoiseParam.decentLab` noise model;
  - five sample items — 100 µm plates cut `opticAxisCut`, `otherOpticAxisCut`, `ZCut`, `XCut`, `YCut` — all made of ONE material version holding the `absorbing` ground truth.
- **Setups and experiments:** one experiment per plate, with a multi-variable scan (incidence × azimuth × T / R) that reproduces exactly `a1 @ a2 @ a3` = 44 configurations. This is the grid generator's job, checked configuration by configuration against `fullConfigurations`.
- **Data:**
  - for each seed, the observations come from the EXISTING `noisyObserveAbsoluteWith forward absorbing NoiseParam.decentLab seed fullConfigurations`;
  - they are written as kit data files through the Mueller-matrix import profile, and read back through the same profile.
- **Study:**
  - open the material with all 15 components `Unknown` (`MaterialScope` across the five plates);
  - start = `perturbedStart`, box = `searchBox`.
- **Assertions, cheap to expensive:**
  1. **Data identity:** the observations the study loads equal the harness's, exactly.
  2. **Model identity:** the generated parameter axes equal `AbsorbingTriclinicParameters.axes`, and the study's residual vector at `perturbedStart` equals the harness's `throughputResidualFor` vector (≤ 1e-12 relative).
  3. **Fit:** one seed through the study engine recovers the 15 constants and the derived quantities (birefringences, isotropic absorption) within the bands `AbsorbingBiaxialNoisyTests` asserts. Those band checks move into one shared function that both tests call.
  4. **Full five-seed ensemble** (same statistics through the reused `statisticsFor`): marked xUnit v3 `Explicit`, so it runs on demand only. It costs the existing test's ~16 minutes, and the `ui-tests` gate filter (`--filter-not-trait "Category=ui-smoke"`, MTP since spec 0048 `.manual/001`) would otherwise pick it up.
- **Reuse, so that ONE implementation exists:**
  - **Promoted to product** (`OpticalConstructor.Optimization`), because the study engine, the "simulate measurement" path and the Planner need them:
    - the bench noise model (`NoiseSeed`, `AngleUncertainty`, `ElementUncertainty`, `ThroughputUncertainty`, `NoiseParam`, `UniformDeviateProvider`, `disturb`, `misread`, `misreadAbsolute`, `noisyObserveWith`, `noisyObserveAbsoluteWith`);
    - `observeWith`, `residualFor`, `throughputResidualFor`;
    - `scalingAround`, `fitWithResidual`, `fitObservations`, `fitAbsorbingObservations`;
    - `RecoveredQuantity`, `RecoveryStatistics`, `statisticsFor`;
    - `predictedRelativeErrors`, `combinationVariance` (the Planner's layer 2 reuses them, §M).

    `BerremanTests/InverseFitHarness.fs` keeps only test assertions (`assertMuellerEqual`, the error formatting) and opens the product modules. This is a MOVE; BerremanTests' behaviour and numbers are unchanged.
  - **The ground truth and designs** (`BiaxialSample.fs`, `AbsorbingBiaxialSample.fs`: constants, `buildAbsorbing`, the cuts, `a1` / `a2` / `a3`, `perturbedStart`, `searchBox`) move into a shared test-support project, `Berreman/BerremanTestSupport/`, one `.fsproj` in its own folder. Both `BerremanTests` and `OpticalConstructor.Ui.Tests` reference it, so one compiled copy serves both test suites.
- **Not written now** (operator): no new inverse test is authored. AT-2 replicates the existing one through the kit model, and BerremanTests is not run as part of this plan.

### R. What is removed (no legacy, no migration)

- **`.ocproj`:** `ProjectFile`, the `ProjectJson` project aggregate and the `Project` record. Kits replace them, and the JSON + schema stack is reused (§B).
- **`Scene.SceneProxy` and `ExperimentCollectionProxy`** (in-memory seams), replaced by `KitProxy`.
- **The 16-entry `recent.json` list,** replaced by the kit history (`bump` reused).
- **The code seed values and `seedAll`'s Domain source,** replaced by catalogue files + `CatalogueImport`.
- **The `Test` EF table / initial migration**, replaced by the library model with `EnsureCreated`.
- **`SolverHandoffView`'s stub screen**, replaced by the study view. Its three validation checks are reused as the study's pre-fit checks.
- **`MuellerDataSet`** (a hard-coded DU of data sets) and the archive-keyed `MuellerDataProxy`. `parseMuellerCsv` is reused as an import profile, and the archive stays only as AT-1's data source.
- **The strict v1 `X,Y` path,** whose parsers are reused as built-in import profiles.
- **`measuredElementOf` and the "first polarizer sets unit intensity" special case,** replaced by N objects and a real source with that default.
- **No up-converters and no migrations** anywhere; a version mismatch is a typed error.

### S. Proposed phasing (each slice green on its own)

1. **Pure data + names:** the tabulated-material case; canonical JSON + hash; per-kind schemas; seeds exported to catalogue files and deleted; tests load the files.
2. **Library database** (slow data only, `EnsureCreated`, `Pinned`, backup, locations, contract tests); catalogue import with the decision dialog; extra catalogue folders.
3. **Kit folder** for BOTH constructors:
   - `KitProxy`, lock, change check;
   - start screen + history (stars on top, `KitHistoryLimit`);
   - experiments and scenes persist into the kit;
   - copy-in + hashes; snapshot + resolution;
   - `.ocproj`, the scene and collection seams and `recent.json` deleted.
4. **Send kit / open kit from zip;** single-entry drops; copy experiments between kits.
5. **Items + item models + parameters.**
6. **Setups as slots;** the N-object run law; the real source; slot bar; grid generator; the naming template (replacing `parseExperiment`).
7. **Experiments v2:** roles, sessions, multi-variable scans, the import wizard with profiles (the existing parsers reused), split and match, exclusions, corrections.
8. **Study + fit:**
   - scopes and links, staged and joint fits, fit runs, results, publish / promote;
   - first the harness and pipeline promotion and the test-support project (§Q reuse);
   - then **AT-1 and AT-2**.
9. **Online material sources** (`MaterialSourceProxy`: refractiveindex.info, then PTB OCDB, then Henke).
10. **Measurement Planner, gyrotropic crystals first** (§M): layer 1, then the layer-2 recoverability report and plan generation with the kit cache. Acceptance is the set of existing facts listed in §M.

## 4. Decisions recorded (operator, rounds 2–4)

- **The database is not overstuffed:** slow data (the library) only. Kits, setups, experiments, studies, fit runs, data and the history are files.
- **A kit is a folder the user chooses or creates;** direct and inverse work both live in kits.
- **History:** `KitHistoryLimit` = 100 (configurable); starred kits always at the top, sorted by last open date; the rest by last open date.
- **Extensions agreed** (§A), with the singleton rule: `.kit-json`, `.catalogue-json`.
- **Names:** `Experiment` = one run; `Study` = the combined inverse problem.
- **No legacy, no migration** (§R).
- **Two acceptance tests:** optics-mueller, and the most complex existing inverse test (`AbsorbingBiaxialNoisyTests`). Both use reused code, not copies, and no new inverse test is written now.
- **Kit root** (round 4): flat; subfolders only for additional data.
- **AT-2** (round 4): `AbsorbingBiaxialNoisyTests` (five plates, 15 unknowns), confirmed.
- **Planner first target** (round 4): gyrotropic crystals, with the exact references and acceptance facts in §M.

## 5. Open questions

None. Every question raised in revisions 1–3 is decided (§4).

## Sources

- SQLite as an application file format: https://sqlite.org/appfileformat.html
- SQLite over a network (locking caveats): https://sqlite.org/useovernet.html
- OneDrive sync and SQLite reports: https://forums.tropy.org/t/error-with-onedrive-sync/2961 ·
  https://sqlitetoolsforrootsmagic.com/forum/topic/rmv11-any-idea-what-the-new-temporay-files-are-for/ ·
  https://github.com/Sonarr/Sonarr/issues/1886
- OneDrive Known Folder Move: https://learn.microsoft.com/en-us/sharepoint/redirect-known-folders
- OpticStudio catalogue folders, project directories, network-drive caveat:
  https://optics.ansys.com/hc/en-us/articles/42661745091603 ·
  https://community.zemax.com/licensing-and-installation-8/what-is-the-programdata-zemax-folder-for-446
- refnx co-refinement (`GlobalObjective`): https://refnx.readthedocs.io/en/latest/reflectometry_global.html
- Origin global fitting with parameter sharing: https://www.originlab.com/doc/Origin-Help/GlobalFitting-Param-Sharing ·
  https://docs.originlab.com/quick-help/globatfit-independentfit/
- GraphPad Prism global regression: https://graphpad.com/guides/prism/latest/curve-fitting/reg_how-global-nonlinear-regressio.htm
- SasView simultaneous fitting: https://sasview.github.io/downloads/simultaneous_1d_fitting_in_sasview_v5.pdf ·
  weights and priors: https://arxiv.org/pdf/2311.06408
- Multi-sample ellipsometry: https://www.laserfocusworld.com/test-measurement/test-measurement/article/16550827/multiwavelength-ellipsometry-fathoms-materials ·
  CompleteEASE coupling: https://arxiv.org/pdf/2511.13991
- Eigenvalue Calibration Method and variants: https://arxiv.org/pdf/1509.05270 ·
  https://www.jeos.org/index_php/jeos_rp/article/view/595 · https://link.springer.com/article/10.1007/s13320-025-0767-z ·
  https://doi.org/10.1364/ao.409799
- Dual-rotating-retarder calibration errors: https://remotesensing.spiedigitallibrary.org/conference-proceedings-of-spie/1746/0000/Mueller-matrix-algorithms/10.1117/12.138793.full ·
  https://pure.bit.edu.cn/en/publications/error-analysis-of-the-calibration-of-a-dual-rotating-retarder-mue/
- Physical realizability (the cone of Mueller matrices): https://arxiv.org/pdf/2309.00161
- From `018` (online sources, planner): refractiveindex.info https://refractiveindex.info/about ·
  https://github.com/polyanskiy/refractiveindex.info-database · PTB OCDB https://www.ocdb.ptb.de/ ·
  CXRO https://henke.lbl.gov/optical_constants/ · FIM design https://arxiv.org/pdf/2103.08973 ·
  https://arxiv.org/pdf/2504.12133 · profile likelihood https://pubmed.ncbi.nlm.nih.gov/19505944 ·
  gyrotropic point groups https://arxiv.org/pdf/2003.10872 — the full list is in `018`.
- Repository references: `018-sharing-libraries-inverse-planner.md`; `Berreman/Data/MuellerMatrix/README.md`;
  `Berreman/BerremanTests/MuellerReconstructionTests.fs` (§7.2 end-to-end); `Berreman/BerremanTests/AbsorbingBiaxialNoisyTests.fs`,
  `AbsorbingBiaxialSample.fs`, `InverseFitHarness.fs`; spec 0044 `020-handover.md`; optics-mueller `notes/matrix_solution.md`,
  `analysis/matrix_solution/step2_linear/report.md`.
