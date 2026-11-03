# 001 — Library files, the slow-data database, experiment-kit folders, and multi-setup inverse experiments (feature list)

The feature list of spec 0050. Every item below is decided.

## Summary

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
  - the catalogue-import log;
  - pulled-source records.

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

## 0. The code today

- **Nothing persists.**
  - Every store is in memory, and every start re-seeds (`OpticalConstructor.Ui/AppContext.fs`).
  - `.ocproj` (`OpticalConstructor.Storage/ProjectFile.fs`) is never saved or opened by any window.
  - `Scene.SceneProxy` and `ExperimentCollectionProxy` are in-memory seams.
  - The EF Core project (`OpticalConstructor.Database`) holds one `Test` table that nothing references.
  - `OpticalConstructor.Seeding` pushes the code seeds into in-memory stores.
  - On disk there are only `recent.json` (`OpticalConstructor.Storage/RecentFiles.fs`, `%APPDATA%\Softellect\Berreman\OpticalConstructor\`, capped at 16) and the user-environment preferences.
- **The library is code.**
  - `MaterialLibrary.builtInEntries`, `Library.SeedSamples`, `SeedSources`, the presets in `Library.seedEntries` (`OpticalConstructor.Domain/ElementId.fs`), and `standardCategories`, all with frozen GUIDs.
  - Material physics is data (`MaterialComplexity`), with one exception: **tabulated imports are closures** (`complexity = None` in `OpticalConstructor.Storage/MaterialImport.fs`).
- **An Experiment** (`OpticalConstructor.Domain/Experiments.fs`) is a frozen table snapshot:
  - one varied element with one variable over one range;
  - one capture mode;
  - ONE data file, stored as a machine-specific path.
- **Table elements are "kind + library entry"**, so two LPs of one preset are indistinguishable.
  - Polarizers are ideal or constant-Mueller (`ElementId.fs`); nothing is parameterised.
  - One sample per run: `measuredElementOf` takes the first bound sample (`OpticalConstructor.Ui/TableAndElementRotationView.fs`).
  - The first polarizer sets a unit-intensity state (`OpticalConstructor.Domain/ExperimentRun.fs`).
- **There is no inverse solve in the UI.** The Inverse flow ends at the stub `SolverHandoffView`.
- **The working inverse code is all outside the constructor:**
  - `MuellerReconstruction` (spec 0042) reproduces the optics-mueller fit, through a regex over run names (`experimentRegex`, read by `MuellerExperimentLabel.parsed`) and the hard-coded `MuellerDataSet` DU over `Berreman/Data/MuellerMatrix/data.zip`;
  - `MuellerInverse` (spec 0044) is the multi-cut material fit;
  - the noise model, the fit wrappers and the ensemble statistics live in the TEST project (`BerremanTests/InverseFitHarness.fs`).

  The constructor reaches none of it.
- **The data-file schemas are too narrow:** strict `X,Y` and `wavelength_nm,psi_deg,delta_deg`. Real lab files are one CSV with many runs keyed by a column, raw dial readings, timestamps and several signal columns.

## 1. The two reference cases

Each reference case is a committed experiment kit in its own subfolder of `<repo-root>/data/`:

- **`data/optics-mueller/`**: case 1a, used by AT-1 (§Q);
- **`data/absorbing-biaxial-noisy/`**: case 1b, used by AT-2 (§Q).

They are complete kit folders (§A, §E): `.kit-json`, items, setups, experiments, the study and its fit runs, `library/`
and the raw lab files under `data/`. The acceptance tests prove the kit IO against them, and the user opens them from
the start screen ("Open kit…") to try the UI on real and on synthetic data. Only `data/chroma/` (the web app's
knowledge base) and each kit's `.oc/` are git-ignored.

### 1a. optics-mueller (real data, `C:\GitHub\optics-mueller`)

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
- **The repository is already a kit folder in shape:** adding `.kit-json` makes it one, and nothing moves.

### 1b. The most complex existing inverse test — `AbsorbingBiaxialNoisyTests` (synthetic, noisy)

- **Unknowns (15):** an absorbing, gyrotropic, triclinic crystal — n₁, n₂, n₃, six ε″ components, six gyration components.
- **Samples:** five 100 µm plates of ONE crystal:
  - the two optic-axis cuts (`opticAxisCut`, `otherOpticAxisCut`);
  - the Z, X and Y cuts.
- **Measurements:** 44 configurations (sets `a1`, `a2`, `a3` in `BerremanTests/AbsorbingBiaxialSample.fs`), each a measured Mueller matrix in transmission or reflection — normalized elements plus the absolute throughput.
- **Noise (`NoiseParam.decentLab`):** angles ±0.2°, elements ±0.005, throughput ±1 %. Five fixed seeds; the fit starts from `perturbedStart` inside `searchBox`.
- **Runtime:** about 946 s.
- Several samples share one material (the `MaterialScope` case), the measurements are noisy, and the fit is the largest the code has (15 unknowns).

---

## 2. Feature list

### A. Names — one rule, applied everywhere

- **The rule** (no useless name part):
  1. A file that exists **once per folder** is named by its extension alone: `.kit-json`, `.catalogue-json`.
  2. A file that exists **many times** is named by its own identity (label, name, id + version, timestamp) plus its kind extension.
  3. **No folder or name part repeats** what the extension already says.
     - **The kit root is flat:** every kit file sits at the root, and its kind is its extension.
     - Subfolders hold only ADDITIONAL data: `library/` (copies of slow data), `data/` (raw lab files), `.oc/` (app cache).
     - A subfolder never exists to group one kind of kit file.
- **Library extensions:** `.material-json`, `.sample-json`, `.source-json`, `.polarizer-json`, `.detector-json`, `.item-model-json`, `.category-json`; the catalogue manifest is `.catalogue-json`.
- **Kit extensions:** `.kit-json` (the kit itself), `.item-json`, `.setup-json`, `.experiment-json`, `.study-json`, `.fitrun-json`.
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
  - Identity is the GUID INSIDE the file, never the file name.
  - There are no category folders: the category lives in the file.
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

  The UI calls the concept "experiment kit"; the code says `Kit`.
- **Database file:** `library.db`.

### B. The library as external files

- **One entry per file;** it diffs, reviews and merges in git, and a single entry can be emailed or dropped in.
- **Every entry file carries:**
  - `schema` (kind + schema version), `id` (the frozen GUID), `version`;
  - `name`, `category` / facets, `description`;
  - `payload` (the elevated domain value);
  - `provenance` — built-in / user / imported-from-kit / pulled-from-source with source, record, source version, **licence and citation**;
  - `contentHash` (SHA-256 over canonical JSON: sorted keys, invariant-culture numbers).

  **Identity is GUID + version + hash, never the name.**
- **JSON Schema per kind,** validated on load. The System.Text.Json + FSharp.SystemTextJson + JsonSchema.Net stack in `ProjectJson` is REUSED. The `.ocproj` aggregate itself goes (§R).
- **No up-converters.** A file whose schema version differs from the app's is rejected with a typed `UnsupportedSchemaVersion` error, never a crash. The same holds for kit files.
- **Physics is pure data.** A **tabulated** `EpsWithDispValue` case (λ, n, k arrays + interpolation rule) replaces the closure-backed tabulated imports. Any entry that cannot serialise is a test failure.
- **Shipped catalogue:**
  - `<install>\Catalogue\Builtin\`, read-only, copied by the build from `Berreman/OpticalConstructor/Catalogue/Builtin/`;
  - the code seeds are exported ONCE by a throw-away tool (GUIDs and values byte-for-byte), then the Domain seed values are **deleted**;
  - tests load the same files.
- **Licence attribution:** CC-BY entries show their attribution in the Library and Materials windows and in the Details view, and kits carry it in their snapshots.
- **Import on every start:** compare each catalogue's `.catalogue-json` hash with the one recorded in the database, and import only on change.
- **Per-entry import decision** (one table for catalogues, kit snapshots and single dropped files):

  | Incoming | Action |
  |---|---|
  | New id | Add |
  | Same id, same hash | Reuse |
  | Same id, version not held locally | Add that version; the local "latest" moves only for a protected built-in from a catalogue |
  | Same id and version, different hash | A divergence; import as a flagged fork with a new id |
  | Different id, same name | Side by side ("Si — from A"), with an explicit "use my Si instead" re-binding and a diff preview |
  | Entry removed from its catalogue | Mark `InactiveEntry` (still resolvable) |

  The decisions show as one dialog with a row per entry, and nothing is overwritten silently.
- **Extra catalogue folders:** read-only folders on a share or a git checkout.
  - Permissions are the folder's own.
  - Versions are immutable files, so the only possible conflict is two people adding the same name, which is harmless because the ids differ.
- **Export:** any entry → its `-json` file; a selection or a category → a catalogue folder. Exporting **pins** the exported versions (§C).
- **`SeedingProxy` / `seedAll` become `CatalogueImport`:** the same seam shape, fed from parsed files; the in-memory implementation stays as the test double.

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
  - A version becomes `Pinned` — a one-way flag — when a kit first references it, or when it is exported.
  - A pinned version never mutates; an edit mints the next version.
  - The kit's snapshot hash is the second safeguard.
- **No migrations.**
  - The database is created from the model (`EnsureCreated`) and stamped with its schema version.
  - A mismatch is a typed `LibrarySchemaMismatch` error, and the remedy is to recreate the database from the catalogues.
  - "Export my library" writes the user entries to a catalogue folder, which the recreated database imports.
- **The library proxies keep their shapes and gain a SQLite `create`:** `MaterialProxy`, `CategoryProxy`, `SampleProxy`, `SourceProxy`, the `LibraryProxy` live view, and the preset and item-model stores.
- **Contract tests:** each library proxy's suite runs against the in-memory store AND SQLite `:memory:`.
- **Settings:**
  - the connection string is the `OpticalConstructorSqlite` entry of `appsettings.json` (`AppConfig.sqliteConnectionString`);
  - WAL mode (the file is local only);
  - one writer;
  - "Back up library now…" (`VACUUM INTO`) and "Restore library…".

### D. Online material sources — slow data

- **`MaterialSourceProxy`** (search, fetch) with adapters, in this order:
  1. **refractiveindex.info** (CC0): an offline snapshot of the GitHub database plus "Update snapshot". The existing YAML importer (`MaterialImport`) is REUSED as the record parser.
  2. **PTB OCDB** (CC BY 4.0): EUV / VUV n, k **with uncertainties**.
  3. **A Henke-based compound calculator** (CXRO f₁, f₂ + formula + density) for EUV and X-ray.
- **A pulled record becomes a user material version** in the database, with provenance: source, record path, snapshot date or commit, licence, citation.
- **Updating a source** mints a NEW version when the record changed and never mutates one.
- **Uncertainties travel with the material** (PTB's σ) and become priors in studies and inputs to the Planner (§L, §M).
- **User-supplied glass catalogues** (Schott / Ohara / Hoya / CDGM AGF): import only, never bundle.
- **Materials Project** dielectric tensors are a prior (a starting guess) only, never reference data.
- **Paywalled sources** stay manual paste.
- **Gyration and magneto-optic data** have no free source; they are user-entered.

### E. The experiment-kit folder — fast data as plain files

- **A kit is a folder** recognised by its `.kit-json` (§A layout).
  - The kit file holds the kit id (GUID), name, author, created and modified times, notes, the naming template (§J), the import profiles (§K) and the `sendExclude` globs.
  - Every other file is either a kit file the app owns or a lab file the app only reads.
- **The direct and the inverse problem use the SAME kit.**
  - Forward experiments are experiments without data.
  - Inverse studies add data and unknowns.
  - One kit holds both, so a user designs a run forward, then attaches its measured file to the same experiment.
- **References inside a kit are relative paths** (forward slashes) plus the SHA-256 recorded at attach time.
  - A changed or missing data file is flagged on open and before each fit ("data changed since attach / since the last fit run").
  - Absolute paths are never stored.
- **Attaching a file from outside the kit copies it into `data/`,** keeping its name and suffixing on collision.
- **Lab files are read-only for the app.** Exclusions, corrections and mappings live in the experiment file.
- **Library closure snapshot.**
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
- **Review round-trip:** notes on experiments, studies and fit runs live in their files, so a reviewer's comments come back in the returned folder or zip; a git-managed kit gets the full history.

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
- **History:**
  - up to `KitHistoryLimit` = 100 folders (`appsettings.json`);
  - **starred kits always at the top**, sorted by last open date; then the rest, sorted the same way;
  - the cap evicts the oldest UNSTARRED entry and never a starred one;
  - each row shows the name, folder, last-open date and the star;
  - a filter box;
  - a missing folder shows greyed, with **Locate…** / **Remove from history**.
- **Storage:**
  - the history lives in the per-user preferences (`%APPDATA%\Softellect\Berreman\OpticalConstructor\`), replacing the 16-entry `recent.json` list;
  - entry: folder, kit id, name cache, last opened, `Starred | NotStarred`;
  - the existing pure MRU logic (`RecentFiles.bump`) is REUSED and extended with `star`, `unstar`, `forget` and `evict` over a clock provider.

### G. Locations

- **Library database:** `%LOCALAPPDATA%\Softellect\Berreman\OpticalConstructor\library.db`, with `Backups\` beside it, on the local disk, never on a share or a synced folder.
- **Preferences:** `%APPDATA%\Softellect\Berreman\OpticalConstructor\`.
- **Kit folders:** anywhere; the suggested parent is `Documents\Optical Constructor\Kits\`. Plain files plus atomic writes make a share or git workable; the lock warns against opening one kit on two machines at once.
- **Exports:** `Documents\Optical Constructor\Exports\` (already used by `ExperimentExportFile`) and `…\Catalogues\`.
- **Shipped catalogue:** `<install>\Catalogue\`.
- **Every path is overridable** in `appsettings.json`; a Launcher "Data locations" panel shows them, each with "Open folder".
- `AGENTS.md`'s rule against `%LOCALAPPDATA%` covers arc-runner WORKER output, not product data.
- **Tests:** test kits and databases live under the test run's output directory (§Q), never under `Documents`. The two committed reference kits in `<repo-root>/data/` (§1) are read-only to tests: a test opens them read-only and never writes there.

### H. Exchange

- **A kit:** copy its folder.
  - **Send kit…** writes one `<kit name>.zip` (excluding `.oc/` and `sendExclude`), and **Open kit from zip…** extracts it.
  - There is no separate bundle format.
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
  | Retarder plate | Retardance δ (dispersive where the model says so), fast-axis offset |
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
  - a study OPENS a material version, which creates a study-local working copy, stored in the study file, with `Unknown` components;
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
  - It replaces `MuellerExperimentLabel.parsed` and its `experimentRegex`; their cases become the template engine's first test cases.
- **Grid generator:** a set of options per slot → their product, with impossible or unwanted combinations unticked → all setups and experiments created at once.
- **Direct problem:** several experiments over one setup, saved in the kit.

### K. Experiments and data (`<name>.experiment-json`, lab files in the kit)

- **`Experiment`** = one recorded run:
  - a setup reference and a **scan**: one or more variables (dial 0…360 step 20; φ; incidence × azimuth × observable; λ on a tunable source);
  - a capture (T / R, from the detectors);
  - a role: `Science | Calibration | Dark | DriftCheck | Diagnostic | Ignored | Planned`;
  - a session;
  - the data attachment: a relative path + hash + import profile + row selection.
- **Column-mapping import wizard:**
  - sniff the delimiter, header and BOM, and show a preview;
  - map columns to scan variable(s) (unit; raw dial vs physical), signal, σ, timestamp, capture index, experiment key — or to Mueller elements m00…m33 plus configuration columns;
  - save the mapping as a named **import profile** in `.kit-json`.

  Built-in profiles REUSE the existing parsers:
  - `MeasuredData.parseIntensitySeries` / `parseEllipsometricSeries`;
  - `MuellerDataStore.parseMuellerCsv` (the "Phone ROI CSV" profile);
  - a Mueller-matrix observation profile (§Q, AT-2).
- **One file → many experiments:** split by a key column, match each group by the naming template, list the unmatched groups and offer to create setups for them (the template parse supplies slots and angles).
- **Raw data is immutable;** exclusions carry a reason and stay visible in plots, marked.
- **Corrections:** re-point an experiment's setup after the fact (e.g. "LR was at −45"), with a mandatory audit note.
- **Observables:** intensity, Ψ / Δ, full Mueller matrices (the 0044 `MuellerObservation` shape, normalized + absolute throughput). A study mixes them.
- **Status badges:** no data / mapped / validated (points) / excluded points / data changed / out of range.

### L. Studies — combining experiments into one inverse problem (`<name>.study-json`, `<study>@<stamp>.fitrun-json`)

- **`Study`** grows from the experiment collection: a named set of experiments inside ONE kit, plus:
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
  - auto-split at Diagnostic experiments, or by hand;
  - per session the gain is free, anchored to its AIR runs, or interpolated between two AIR repeats;
  - dark from the Dark runs.

  These rules replace the hand-written family rules: `MuellerReconstruction.familyGain` becomes their first implementation.
- **Fit modes:**
  - **joint**;
  - **staged**: stage 1 calibrates items on Calibration runs (the Eigenvalue Calibration Method); **Publish calibration**; stage 2 fits objects with those values fixed, or as priors with σ.
- **Engines — the existing ones, reused:**
  - the linear path is `MuellerReconstruction.buildDesign` + `reconstruct` (SVD, rank, condition);
  - the nonlinear path is `InverseFit` / `MuellerInverseSolver` (Alglib, multi-start) over `MuellerInverse.forwardModels`, driven by the generated parameter axes.

  The choice is automatic: linear when the unknowns enter linearly.
- **Weights and ablation:** a weight per experiment or per detector; enable / disable per experiment.
- **Results**, stored in the fit run:
  - value ± σ (Jacobian covariance; `FitQuality` REUSED);
  - correlation matrix, rank, condition;
  - per-experiment residuals and RMSE;
  - Mueller realizability (coherency eigenvalues ≥ 0);
  - the product check for series objects (M_comb against M_LR · M_QZ);
  - **post-fit identifiability by profile likelihood** for suspect parameters.
- **Publishing:** item → next revision (`Calibrated`); material or black-box object → **Promote to library**.

### M. Measurement Planner — kit-driven, gyrotropic crystals first

- **Target: gyrotropic crystals,** transparent and absorbing, from uniaxial to triclinic — the class the existing inverse code covers.
  - **Code:**
    - `OpticalConstructor.Domain/MuellerInverse.fs`: `UniaxialParameters` (4 unknowns), `TriclinicParameters` (9), `AbsorbingTriclinicParameters` (15), `SampleCut`, `orientForCut`, `internalAngle`, `gyrationProjection`;
    - `OpticalConstructor.Optimization/FitQuality.fs`: `residualJacobian` and its `ResidualJacobian` carrier (`columnNorms`, `conditionNumber`, `correlation`), `FitReport.standardErrors`;
    - `predictedRelativeErrors` / `combinationVariance` (promoted by §Q).
  - **Physics:** `specs/0044/.manual/004-active-anisotropic-inverse-problem.md`:
    - a wave sees only the tensor components transverse to its propagation direction;
    - along the optic axis the plate is a pure circular retarder;
    - across the optic axis the activity is buried under the birefringence;
    - Snell caps the internal angle at arcsin(1/n_o) ≈ 40.4° for quartz, so more than one cut is required.

    `specs/0044/.manual/017-feasible-biaxial-experiments.md` covers the binormal optic-axis plate (100 µm vendor minimum, 0.11 wave per degree of misalignment); `015` and `019` there are the noisy ensembles.
  - **Acceptance — the planner reproduces existing facts.** Their designs and helpers are reused; no new physics test is written:
    - **Layer 1 / rank, `MuellerInverseTests`:** "C1 is completely blind to g33, which is why a second crystal cut is required", and "C1 alone cannot determine three of the four constants, which is why the design needs more cuts". The planner reports what C1 leaves unrecoverable and proposes the second cut.
    - **Layer 1 / rank, `BiaxialInverseTests`:** "the cuts buy the gyration DIAGONAL and only the tilt buys its OFF-DIAGONAL". For a normal-incidence-only kit the planner flags g12, g13 and g23 as unrecoverable, and it ranks oblique incidence AND further cuts as the remedy.
    - **Full rank, `MuellerInverseTests`:** "the full measurement set is well conditioned and every parameter is observable". The planner reports full rank for that design.
    - **Layer 2 / Cramér–Rao, `MuellerInverseTests`:** "an ensemble of noisy experiments recovers every constant, and its scatter IS what one fit's own covariance predicts". The planner's bounds for that design equal the promoted `predictedRelativeErrors`, and likewise for AT-2's design.
- **Its inputs are the kit; there is no separate "lab profile" to type in:**
  - **lab profile** = the kit's items: sources and λ ranges, polarizers, compensators, compounds, detectors, angle ranges, and each detector's **noise model** (the bench model §Q promotes);
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
- **Workflow:**
  1. kit;
  2. planner report and plan;
  3. measure;
  4. attach;
  5. fit;
  6. post-fit identifiability;
  7. the planner proposes the next experiment.

  It is deterministic linear algebra over the solver.

### N. Solver and physics changes

- **The run law walks any number of objects.** `ExperimentRun` folds slot by slot, in light order, through each item's Mueller matrix. `measuredElementOf` ("the first bound sample") is deleted, and capture branches are decided per object.
- **The source is real:** a source item with a Stokes state and an intensity scale. "Unpolarized, unit scale after the first polarizer" is its default parameter set, so existing charts keep their values.
- **One item-model evaluator,** `ItemModel × parameters × orientation × λ → MuellerMatrix`:
  - stack samples go through Berreman (`Propagation.sampleMuellerSpun` for the table; `MuellerInverse.orientForCut` + `createBerremanForward` for cut plates — REUSED);
  - black-box and compound items go through the Stokes/Mueller algebra.
- **The forward model of an experiment** = slots + scan point + parameters → predicted reading: intensity → S₀ · gain + dark; polarimeter → Mueller elements + throughput; ellipsometer → Ψ / Δ.
- **Parameter-axis generation:** a material complexity or item model publishes its `ParameterAxis` list. Case 1b's generated axes equal `AbsorbingTriclinicParameters.axes` — an exact test.
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
- **Automation:** every new control gets a stable id, a uniquely named static member of `ConstructorUiId` (`OpticalConstructor.Controls/UiIds.fs`) — ids such as `KitHistoryList`, `KitHistoryStarToggle`, `NewKitButton`, `KitItemList`, `SlotChip_<slot>`, `SetupGridGenerateButton`, `DataSplitKeyColumn`, `StudyRunFitButton`, `PlanRunButton`, `SendKitButton`. Every new window calls `WindowFit.keepOnScreen` and joins `ScrollExtentTests`.

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

### Q. The two acceptance tests — replication by REUSE

**Common rules:**
- **The two kits are committed** in `data/optics-mueller/` and `data/absorbing-biaxial-noisy/` (§1). Section 12 creates them by running the build below once, through the app's own folder `KitProxy`, and commits the result. They are never hand-written.
- **Build:** each test builds its kit ONLY through the semantic messages (§P), against a SQLite `:memory:` library, and writes it through the REAL folder `KitProxy` into a throw-away folder under the test run's output directory. It asserts that the built kit equals the committed kit, file by file (every kit file and every file under `data/` and `library/`; `.oc/` excluded). This proves the write path, and that the committed kit is what the app produces.
- **Open:** the test then opens the COMMITTED kit read-only through the real folder `KitProxy`, against an EMPTY library database, and runs its study from there. This proves the read path: the library snapshot, the relative paths and hashes, and the data import. A test never writes into `data/`.
- **Send:** the built kit is zipped with `SendKit`, unzipped elsewhere and reopened; the reopened kit and the study's residual vector at its start point equal the built ones.
- **The data and the expected values come from where they already live;** nothing is transcribed. The raw lab files live in the committed kit, once (AT-1 below), or are regenerated and compared byte for byte (AT-2 below).
- Trait `kit-acceptance`, the `FsxAcceptanceTests` pattern, in `OpticalConstructor.Ui.Tests` (the headless app).

**AT-1 — optics-mueller:**
- **Kit:** laser, LP1, LP2, CPL1 / CPL2 (compound LP + retarder, δ and Δ unknown), QZ / LR (black-box Mueller), phone (session gain, dark).
- **Setups:** the grid over 4 families × {AIR, QZ, LR, QZ → LR} × φ. The **two-sample** setups put QZ in Object 1 and LR in Object 2.
- **Data:** the lab files of `Berreman/Data/MuellerMatrix/` MOVE into the kit: the CSV entries of `data.zip` into `data/optics-mueller/data/raw/`, and the reference pipeline's scripts, `README.md` and `reference_step1_summary.json` / `reference_step2_linear_summary.json` into `data/optics-mueller/data/reference/`. `Berreman/Data/MuellerMatrix/` is then deleted, with its `Content` link in `BerremanTests.fsproj`; `MuellerReconstructionTests` §7.2 (re-pointed by section 10) reads the kit's files. There is no second copy of the data.
  - The CSVs are read through the existing `MuellerDataStore` parser (`parseMuellerCsv`, the "Phone ROI CSV" import profile).
  - Attach, split by `experiment`, match by the lab template.
  - Roles for Darkness, BullshitCheck and the AIR repeats.
  - The LR correction and the excluded frame are applied as data.
- **Study:** staged (stage 1 AIR calibration → publish → stage 2 objects).
- **Expected:** the reference pipeline's full-precision values that `MuellerReconstructionTests` §7.2 ("section 7.2 end-to-end…") asserts (provenance `data/optics-mueller/data/reference/reference_step2_linear_summary.json`).
- **Reuse, so that ONE implementation exists:**
  - the stage pipeline that §7.2 composes by hand (`loadDataSet`, `reduceAirTraces`, `unwrapFit`, stage-1 constants, `familyGain`, `correctSignal`, `buildDesign`, `reconstruct`) is promoted into product code as the study's staged Mueller path, and §7.2 is re-pointed to call it;
  - the transcribed expected literals move from the §7.2 test body into one shared reference module, which both tests open. The test reads no file for its expectations (spec 0044 §8.5).
- **Assertions:** M_QZ, M_LR, M_{QZ+LR}, the CPL retardances, the angle offset and the dial zeros within the existing `dataMatrixTol`; the product-check RMSE ratio.

**AT-2 — `AbsorbingBiaxialNoisyTests` (case 1b):**
- **Kit:**
  - a source at `referenceWaveLength`;
  - an ideal polarimeter detector with the `NoiseParam.decentLab` noise model;
  - five sample items — 100 µm plates cut `opticAxisCut`, `otherOpticAxisCut`, `ZCut`, `XCut`, `YCut` — all made of ONE material version holding the `absorbing` ground truth.
- **Setups and experiments:** one experiment per plate, with a multi-variable scan (incidence × azimuth × T / R) that reproduces exactly `a1 @ a2 @ a3` = 44 configurations. This is the grid generator's job, checked configuration by configuration against `fullConfigurations`.
- **Data:**
  - for each of the five seeds, the observations come from the EXISTING `noisyObserveAbsoluteWith forward absorbing NoiseParam.decentLab seed fullConfigurations`;
  - they are written as kit data files under `data/absorbing-biaxial-noisy/data/` through the Mueller-matrix import profile, committed with the kit, and read back through the same profile;
  - the test regenerates them from the seeds and requires the committed files byte-identical, so the data is never transcribed.
- **Study:**
  - open the material with all 15 components `Unknown` (`MaterialScope` across the five plates);
  - start = `perturbedStart`, box = `searchBox`.
- **Assertions, cheap to expensive:**
  1. **Data identity:** the observations the study loads equal the harness's, exactly.
  2. **Model identity:** the generated parameter axes equal `AbsorbingTriclinicParameters.axes`, and the study's residual vector at `perturbedStart` equals the harness's `throughputResidualFor` vector (≤ 1e-12 relative).
  3. **Fit:** one seed through the study engine recovers the 15 constants and the derived quantities (birefringences, isotropic absorption) within the bands `AbsorbingBiaxialNoisyTests` asserts. Those band checks move into one shared function that both tests call.
  4. **Full five-seed ensemble** (same statistics through the reused `statisticsFor`): marked xUnit v3 `Explicit`, so it runs on demand only. It costs the existing test's ~16 minutes, and the `ui-tests` gate filter (`--filter-not-trait "Category=ui-smoke"`) would otherwise pick it up.
- **Reuse, so that ONE implementation exists:**
  - **Promoted to product** (`OpticalConstructor.Optimization`), because the study engine, the "simulate measurement" path and the Planner need them:
    - the bench noise model (`NoiseSeed`, `AngleUncertainty`, `ElementUncertainty`, `ThroughputUncertainty`, `NoiseParam`, `UniformDeviateProvider`, `disturb`, `misread`, `misreadAbsolute`, `noisyObserveWith`, `noisyObserveAbsoluteWith`);
    - `observeWith`, `residualFor`, `throughputResidualFor`;
    - `scalingAround`, `fitWithResidual`, `fitObservations`, `fitAbsorbingObservations`;
    - `RecoveredQuantity`, `RecoveryStatistics`, `statisticsFor`;
    - `predictedRelativeErrors`, `combinationVariance` (the Planner's layer 2 reuses them, §M).

    `BerremanTests/InverseFitHarness.fs` keeps only test assertions (`assertMuellerEqual`, the error formatting) and opens the product modules. This is a MOVE; BerremanTests' behaviour and numbers are unchanged.
  - **The ground truth and designs** (`BiaxialSample.fs`, `AbsorbingBiaxialSample.fs`: constants, `buildAbsorbing`, the cuts, `a1` / `a2` / `a3`, `perturbedStart`, `searchBox`) move into a shared test-support project, `Berreman/BerremanTestSupport/`, one `.fsproj` in its own folder. Both `BerremanTests` and `OpticalConstructor.Ui.Tests` reference it, so one compiled copy serves both test suites.
- **No new inverse test is authored.** AT-2 replicates the existing one through the kit model.

### R. What is removed (no legacy, no migration)

- **`.ocproj`:** `ProjectFile`, the `ProjectJson` project aggregate and the `Project` record. Kits replace them, and the JSON + schema stack is reused (§B).
- **`Scene.SceneProxy` and `ExperimentCollectionProxy`** (in-memory seams), replaced by `KitProxy`.
- **The 16-entry `recent.json` list,** replaced by the kit history (`bump` reused).
- **The code seed values and `seedAll`'s Domain source,** replaced by catalogue files + `CatalogueImport`.
- **The `Test` EF table and its initial migration**, replaced by the library model with `EnsureCreated`.
- **`SolverHandoffView`'s stub screen**, replaced by the study view. Its three validation checks are reused as the study's pre-fit checks.
- **`MuellerDataSet`** (a hard-coded DU of data sets), the archive-keyed `MuellerDataProxy` and `Berreman/Data/MuellerMatrix/`. `parseMuellerCsv` is reused as an import profile, and the archive's lab files and reference files move into the AT-1 kit, `data/optics-mueller/data/` (§Q).
- **`MuellerExperimentLabel.parsed` and `experimentRegex`,** replaced by the naming template (§J).
- **The strict v1 `X,Y` path,** whose parsers are reused as built-in import profiles.
- **`measuredElementOf` and the "first polarizer sets unit intensity" special case,** replaced by N objects and a real source with that default.
- **No up-converters and no migrations** anywhere; a version mismatch is a typed error.

### S. Implementation sections (in this order; each green on its own)

1. **Library entries as data files** (§A, §B):
   - the tabulated `EpsWithDispValue` case; canonical JSON + SHA-256; per-kind JSON Schemas, reusing the `ProjectJson` serializer and validator stack;
   - generate `Berreman/OpticalConstructor/Catalogue/Builtin/` ONCE from the code seeds (GUIDs and values byte-for-byte), then delete the seed values;
   - `CatalogueImport` over the existing `SeedingProxy` shape; tests load the catalogue files.
2. **Library database** (§C, §G):
   - replace the `Test` entity with the library model; `EnsureCreated`; the `Pinned` flag replacing `VersionsInUse`;
   - a SQLite `create` for every library proxy, with contract suites run against BOTH the in-memory store and SQLite `:memory:`;
   - locations from `appsettings.json` (the `OpticalConstructorSqlite` connection string); backup and restore; the `AppContext` switch to the database.
3. **Catalogue import and library-entry exchange** (§B): import on every start by manifest hash; the decision table and its one-row-per-entry dialog; extra read-only catalogue folders; export to `-json` files and catalogue folders (export pins); single-file drop; licence attribution display.
4. **Kit folder core** (§E, §A):
   - `KitProxy` (folder + in-memory), `.kit-json`, atomic writes, `.oc/lock`, the external-change check;
   - relative paths + hashes; copy-in of attached files; library snapshot + resolution on open;
   - today's experiments and scenes persist into the kit;
   - DELETE `.ocproj` (`ProjectFile`, the `Project` aggregate) and the `Scene` / `ExperimentCollection` seams.
5. **Start screen and history** (§F):
   - `KitHistoryProxy` over the per-user preferences, REUSING `RecentFiles.bump` (+ `star` / `unstar` / `forget` / `evict`); starred kits always on top, sorted by last open date; `KitHistoryLimit` = 100;
   - missing-folder handling; `recent.json` replaced;
   - the Launcher's Main AND Inverse both go through the start screen; the Data locations panel.
6. **Send and receive kits** (§H): `KitArchiveProxy` (zip honouring `sendExclude`, open from zip); copy experiments between kits with item mapping.
7. **Items and item models** (§I): the `Kit` module (`Item`, `ItemId`, `ItemLabel`, `Parameter`, `ParameterState`), `ItemModel` library entries with generated `ParameterAxis` lists, dial mapping, revisions, the Items panel.
8. **Setups as slots and the run law** (§J, §N):
   - N objects in series (delete `measuredElementOf`); a real source whose default reproduces today's charts;
   - the slot bar and quick swap; the grid generator;
   - the naming template, which REPLACES `MuellerExperimentLabel.parsed` and its `experimentRegex`.
9. **Experiments v2 and data import** (§K):
   - roles, sessions, multi-variable scans;
   - the column-mapping wizard with import profiles stored in `.kit-json`; built-in profiles REUSE `MeasuredData`'s parsers and `MuellerDataStore`'s `parseMuellerCsv`, and the Mueller-matrix observation profile is added;
   - split by column + template match, exclusions with reasons, setup corrections with audit notes;
   - DELETE the `MuellerDataSet` DU and the archive-keyed `MuellerDataProxy`. MOVE `Berreman/Data/MuellerMatrix/` into the AT-1 kit folder: the CSV entries of `data.zip` into `data/optics-mueller/data/raw/`, the scripts, `README.md` and reference summaries into `data/optics-mueller/data/reference/` (§Q AT-1). Delete the folder and its `Content` link in `BerremanTests.fsproj`, and re-point `MuellerReconstructionTests` to read the kit's files through the import profile. Its numbers stay unchanged.
10. **Promote the inverse harness and the reference pipeline** (§Q "Reuse"):
    - MOVE into `OpticalConstructor.Optimization`: the bench noise model, observe / residual / throughput-residual, `scalingAround`, the fit wrappers, `RecoveredQuantity` / `RecoveryStatistics` / `statisticsFor`, `predictedRelativeErrors` and `combinationVariance`;
    - create `Berreman/BerremanTestSupport/` (own folder, own `.fsproj`, a plain library) holding `BiaxialSample` and `AbsorbingBiaxialSample`, referenced by `BerremanTests` and `OpticalConstructor.Ui.Tests`;
    - promote the `MuellerReconstructionTests` §7.2 hand-composed pipeline into product code and re-point §7.2 at it; move its expected literals into one shared reference module (no file read for expectations, spec 0044 §8.5);
    - `BerremanTests` stays green with unchanged numbers.
11. **Studies and fitting** (§L, §N):
    - scopes and links; sessions and gain models (`MuellerReconstruction.familyGain` becomes their first implementation);
    - staged and joint fits; the automatic engine choice, REUSING `buildDesign` / `reconstruct` and `InverseFit` / `MuellerInverseSolver` over `MuellerInverse.forwardModels`;
    - generated axes; append-only fit runs; results (`FitQuality` reused), realizability, product check, profile likelihood;
    - publish / promote to library; the study view;
    - DELETE the `SolverHandoffView` stub, reusing its checks as pre-fit validation.
12. **The two acceptance tests and the two reference kits** (§1, §Q):
    - AT-1 optics-mueller and AT-2 `AbsorbingBiaxialNoisyTests`, built ONLY through semantic messages;
    - create the committed kits `data/optics-mueller/` (around the lab files section 9 moved there) and `data/absorbing-biaxial-noisy/` (with AT-2's five generated observation files) by running that build once through the app's folder `KitProxy`;
    - each test asserts its built kit equals the committed one, then opens the committed kit read-only against an EMPTY library and runs its study, then round-trips the built kit through `SendKit`;
    - AT-2's five-seed ensemble is xUnit v3 `Explicit`;
    - they run under the `kit-acceptance` gate.
13. **Online material sources** (§D): `MaterialSourceProxy`. First refractiveindex.info (offline snapshot + update, REUSING `MaterialImport` as the record parser); then PTB OCDB with uncertainties; then the Henke compound calculator. Provenance, licence, a new version on update.
14. **Measurement Planner, gyrotropic crystals first** (§M):
    - inputs from the kit;
    - the layer-1 symmetry table;
    - layer 2: Fisher information / rank / confounded combinations / Cramér–Rao, REUSING `FitQuality.residualJacobian` and its `ResidualJacobian` carrier (`columnNorms`, `conditionNumber`, `correlation`) and the promoted `predictedRelativeErrors`;
    - greedy D-optimal ranking; the plan emitted as `Planned` setups and experiments in the kit; the cache in `.oc/planner/`;
    - acceptance = the existing `MuellerInverseTests` / `BiaxialInverseTests` facts listed in §M.

### T. Invariants

Each holds from the section named, through the end of the spec.

- From section 1: `git grep -n -E "let (builtInEntries|seedEntries|standardCategories) " -- Berreman/OpticalConstructor/` finds nothing.
- Throughout: `git grep -n -F "kit.kit-json" -- Berreman/` finds nothing.
- From section 4: `git grep -n -i -F "ocproj" -- Berreman/OpticalConstructor/` finds nothing.
- From section 8: `git grep -n -F "experimentRegex" -- Berreman/` finds nothing.
- From section 9: `git grep -n -F "MuellerDataSet" -- Berreman/` finds nothing.
- From section 9: `git grep -n -F "Data/MuellerMatrix" -- Berreman/` finds nothing.
- From section 10: `git grep -n -F "let noisyObserveAbsoluteWith" -- Berreman/BerremanTests/` finds nothing.

### U. Gates this work adds to the usual ones

- **`kit-acceptance`** (`.gates/OpticalConstructor/kit-acceptance.gates`, already in the catalog): AT-1 and AT-2, trait `kit-acceptance`, in `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`. The gate is forward-pointing until section 12 creates that file. It applies to that file, the run law (`OpticalConstructor.Domain/ExperimentRun.fs`), `OpticalConstructor.Optimization`, `BerremanTestSupport` and the two committed kits (`data/optics-mueller/**`, `data/absorbing-biaxial-noisy/**`). `ui-tests` already excludes the trait.
- **`unit-tests`** applies only to the code `BerremanTests` covers. In this work that is sections 10 and 11, and any step that changes the solver, `OpticalConstructor.Optimization`, `MuellerInverse.fs` or `MuellerReconstruction.fs`.

### V. Done when

- AT-1 and AT-2 pass under `kit-acceptance`, with the assertions listed in §Q.
- The two reference kits are committed in `data/optics-mueller/` and `data/absorbing-biaxial-noisy/`, equal to what the build produces, and open in the app from the start screen.
- The Planner reproduces the §M facts.
- Every gate is green with zero warnings from our code, `elevation-full` included, and `Elevation/elevation-known-findings.json` still lists no finding.
- `BerremanTests`' numbers are unchanged.
- The code no longer contains the §R removals.
- The app persists the library in the database and everything else in kit folders.

### W. Out of scope

- New inverse-physics tests.
- A WinForms port (UI logic stays in pure Domain / State, as `AGENTS.md` requires).
- Cloud or shared databases.
- Bayesian sequential design.
- Shipped planner playbooks.

### X. What this work reads

The code and documents this work builds on. Read each before writing the steps that touch it.

- **Repository rules:** `AGENTS.md`, `naming_conventions.md`, `.gates/`.
- **Solver and data:** `Berreman/Berreman/`, `Berreman/Analytics/`, `Berreman/OpticalProperties/`, `Berreman/Data/MuellerMatrix/README.md` (moved into `data/optics-mueller/data/reference/` by section 9), `.gitignore` (`data/` is tracked; only `data/chroma/` and each kit's `.oc/` are ignored).
- **Constructor:**
  - `Berreman/OpticalConstructor/OpticalConstructor.Domain/`, `OpticalConstructor.Storage/`, `OpticalConstructor.Optimization/`, `OpticalConstructor.App/`, `OpticalConstructor.Database/`;
  - `OpticalConstructor.Seeding/Seeding.fs`;
  - `OpticalConstructor.Ui/AppContext.fs`, `TableAndElementRotationView.fs`, `SolverHandoffView.fs`;
  - `OpticalConstructor.Ui.Tests/FsxAcceptanceTests.fs`.
- **Inverse tests:** in `Berreman/BerremanTests/`, the files `InverseFitHarness.fs`, `BiaxialSample.fs`, `AbsorbingBiaxialSample.fs`, `AbsorbingBiaxialNoisyTests.fs`, `MuellerReconstructionTests.fs`, `MuellerInverseTests.fs`, `BiaxialInverseTests.fs`.
- **Specs:**
  - `specs/0046/.manual/018-sharing-libraries-inverse-planner.md`;
  - `specs/0044/.manual/004-active-anisotropic-inverse-problem.md`, `017-feasible-biaxial-experiments.md`, `020-handover.md`.
