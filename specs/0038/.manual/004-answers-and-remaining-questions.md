# 004 — Round 2: locked decisions, your `.ocproj` question, and the last few questions (reply to 003)

Discussion only — no code changed. Almost everything is now agreed; this document
records the locked decisions (so the preliminary spec has an explicit baseline),
answers your `.ocproj` question, delivers the two proposals you asked for
(thickness bucketing, out-of-band indicator), and asks the last five questions
(numbered **Q25–Q29**, continuing the 002 numbering). If 005 answers these, 006
will be the preliminary spec.

---

## 1. Locked decisions from 003 (the spec baseline)

- **Relocation (Q1):** the working constructor moves out of `TestWindows` to a
  proper product project; test windows stay; a new *test-window launcher* wires all
  diagnostic scenes; the product launcher lives outside test projects. (Two
  mechanics questions below: Q26, Q27.)
- **Placement (Q2):** Launcher gets `Main / Inverse / Materials / Library`; the
  same Materials/Library windows are also openable from the constructor.
- **Window policy (Q3):** Materials/Library — single instance, activate on
  re-open. Edit windows — multiple allowed, keyed by identity: opening a
  material/sample that is already open re-activates its window. (Note: "Add"
  windows have no id yet, so each Add is a new window until first save.)
- **Terminology (Q4):** **Library** = the library of optical elements (samples now
  unified with polarizers, sources, detectors, …). **Materials** = the infinite
  homogeneous property descriptions. Two windows, one shared faceted-tree control.
- **Modality (Q5):** Select-state windows modal/non-modal via an **appsettings**
  switch, default `false` (non-modal). Implementation note: modal (`ShowDialog`)
  needs an owner window, so all window opening goes through one `WindowLauncher`
  seam that reads the switch; Browse-state windows are always non-modal.
- **Selector (Q6):** "Choose…" (full Library in Select state) always available;
  the inline quick-pick strip appears only when the kind-constrained entry count
  is below an appsettings threshold (default 5 if missing).
- **Ideal elements (Q7):** become ordinary seeded library entries carrying a
  protection marker (a DU per house style, e.g.
  `EntryProtection = ProtectedBuiltIn | UserManaged`) — protected entries cannot
  be marked inactive/superseded/deleted. The concept of "ideal element" disappears
  from the code; they are just entries. New table elements are pre-bound to their
  seeded defaults — everything except the sample (see Q28 for the exact defaults).
- **Compounds (Q8):** stored as ordered component lists; product Mueller matrix
  computed on demand.
- **Facets (Q9–Q13):** transparent/absorbing refinement only under Constant;
  substrate-material facet applies to Plate/Wedge only (dependent facet — it
  simply doesn't appear for thin-film-only populations); "has thin films" derived
  from `films` non-empty; thickness facet is **per-film** thickness (bucketing
  proposal in §3); material facets on samples = distinct union over all
  constituent materials (substrate + layers), i.e. "any constituent matches".
- **Tree (Q14):** search/application order is separate from representation order;
  the tree never reshuffles implicitly — reordering is an explicit user action.
- **Settings (Q15):** `appsettings.json` is introduced **now**, accessed only via
  `Softellect.Sys.AppSettings` — never read/written directly (details in §4).
- **Material editor (Q16):** two panes split by a **vertical splitter** — ladder
  in the left pane inside a ScrollViewer, chart filling the right pane at full
  height — plus tabbed charts (n,k / Gyration / μ) with legends. (Restating my
  interpretation of "vertical split" so a misread gets caught here, not in code.)
- **Refresh (Q17):** re-query proxies when a Select window returns / on window
  activation; live cross-window notifications deferred.
- **Lifecycle (Q18–Q20):** versioning + supersede is the mechanism; export embeds
  resolved versions; "used" = referenced by a persisted experiment or saved
  project; metadata edits don't version, physics edits do. Version creation rule
  per your Q20 answer, restated precisely:
  - editing a **used** vN and saving a physics change ⇒ vN+1 is created
    (and only if the physics actually differs — structural equality check; saving
    an identical value stays vN);
  - an **unused** version is mutated in place by further edits, no matter how
    many;
  - assumption I'm baking in unless you object: the library always edits the
    **latest** version; older versions are view-only (you can copy from them, not
    edit them).
- **Experiments (Q21):** setup captured as an ordered element-descriptor list
  referencing versioned library entries; sample and source optional (no-sample /
  dark-line calibration experiments are first-class).
- **Table display (Q22):** two independent indicators — (1) bound/unbound (dashed
  ghost vs solid, `Bound | Unbound | NotBindable`), (2) a warning indicator with
  hover tooltip for "bound but out of defined band" (proposal in §3, question
  Q29).
- **Inverse data (Q23):** no normalization — the app gathers data; the solver
  owns corrections (later).
- **Inverse scope:** the solver itself is **out of scope**; the wiring is **in
  scope** — Inverse launcher button, inverse-state constructor (sample unbound or
  hint-bound; no hint ⇒ no chart), experiment collections with per-experiment CSV
  data files (strict schema; wavelength/psi/delta for ellipsometric), loading via
  proxies tested against mocks and in-memory streams (no IO in tests), ending at a
  stub Inverse Problem Solver screen that shows general information about the
  received experiments, runs basic validation, and states that the solver comes
  separately.
- **Storage (Q24):** no real database, no DB/file split in this version. A nearly
  empty C# EFC project with one "test" table proves the wiring (create/migrate
  works), configured MSSQL/SQLite via appsettings; the app keeps running on the
  in-memory proxy structures; the F# seeding project pushes the existing Domain
  seed values through proxies (no data re-declared in the seeding project).
- **Consolidations:** recent-files stores → one place; UiIds → one constants
  module.

---

## 2. Your question: what are the `.ocproj` files?

`.ocproj` is the app's own **project-file format**: one self-contained UTF-8 JSON
text file that saves an optical-table **scene** — the beam tree, the per-element
engine `OpticalSystem`s, source specs, element placements, and the table plate
(`OpticalConstructorProject`, `Project.fs:26-48`). It is schema-validated on every
load (`schema/optical-constructor-project.schema.json`), written/read by
`ProjectFile.saveProject/openProject` (`OpticalConstructor.Storage/ProjectFile.fs`),
and has supporting machinery around it: autosave (`<name>.ocproj.autosave`),
recent-files MRU, undo/redo history snapshots, and a `.sidecars` folder for bulk
derived artifacts (sweep tables etc.). It does **not** contain materials, samples,
categories, or experiments — only the scene.

The honest status, though: **the live workbench never calls it.** Save/Open of
`.ocproj` is wired only in the old `Shell.fs` UI — which, per §0 of 002, is dead
code for the Main flow — and in tests. So today the product has no working
save/open at all; `.ocproj` is designed-but-orphaned persistence. Given your Q24
answer (in-memory structures stay, no DB), `.ocproj` remains the only scene
persistence mechanism there is, which raises:

**Q25.** When the constructor moves to its proper place (Q1), should 0038 also
port the `.ocproj` Save/Open (+ autosave/recent) wiring from the dead Shell into
the real workbench — my recommendation, since otherwise users still cannot save a
scene — or is scene persistence deferred to its own spec?

---

## 3. The two proposals you asked for

### Thickness bucketing (Q12)

Concrete algorithm, pure and unit-testable:

1. Collect the distinct per-film thicknesses of the **currently constrained**
   population (after all other applied constraints).
2. Generate candidate boundaries on the 1–2–5 log ladder spanning the population:
   … 1, 2, 5, 10, 20, 50, 100, 200, 500 nm, 1, 2, 5 µm …
3. Keep only buckets that contain at least one item; merge adjacent ladder buckets
   while the total bucket count exceeds a display cap (default 8, appsettings),
   preferring to merge the buckets with the fewest items.
4. Each bucket displays as `10–20 nm (7)`; buckets recompute whenever the
   constraint set changes (your 500 → 100 → ranges-within-those behavior).
5. A manual `min–max` entry is always offered alongside the buckets and becomes an
   ordinary constraint chip when applied.

Edge cases pinned down: a single distinct value renders as one exact-value bucket
(`600 nm (3)`); values sitting exactly on a boundary go to the lower bucket
(half-open intervals `[lo, hi)`); units are always displayed in nm below 1 µm and
µm above (reusing the existing display-unit conventions).

### Out-of-band indicator (Q22, second meaning)

An element is flagged **out-of-band** when it is bound, at least one material
reachable through its binding is dispersive, and the wavelength(s) the current run
will evaluate fall (partly) outside the union of that material's defined dispersion
segments (`EditSegment.interval`). Visual: the element renders normally (it *is*
set) plus a small warning badge at its corner; hover tooltip names the offending
material(s) and the defined vs requested ranges, e.g.
`"BK7-like: defined 400–700 nm, requested 300–800 nm"`. Same check surfaces in the
Details bay as text. Constant materials never flag; unbound elements show the
unbound visual instead (the two indicators are independent by design).

**Q29.** "Out of band" is evaluated against the wavelengths the current
run/experiments will actually request (single λ for a fixed source; the full sweep
range for wavelength sweeps) — confirm, or would you rather flag against a fixed
reference range regardless of the run?

---

## 4. Softellect.Sys appsettings + EFC example — verified, ready for the spec

Understood on Q15 — appsettings now, and only through the Softellect library. I
checked the actual API so the spec can bind to it precisely:

- `Softellect.Sys` **10.1.301.54 is already referenced** by
  `OpticalConstructor.Storage`, `.Optimization`, and `BerremanTests` — no new
  dependency, just add the reference where the composition root reads settings.
- `Softellect.Sys.AppSettings.AppSettingsProvider.tryCreate()` opens
  `appsettings.json`; typed getters `getIntOrDefault` / `getBoolOrDefault` /
  `getDoubleOrDefault` / `getOrDefault<'T> tryCreate` fit the elevated-type rule
  (custom DUs come through their own `tryCreate`). Connection strings have their
  own section accessors (`tryGetConnectionString`) — which the EFC test project
  will use.
- A property worth exploiting: the provider is created with `SetOnMissing = true`,
  so **reading a missing key writes the default back into the file** — the
  appsettings file self-documents with every knob and its default after first run
  (your "default 5 if missing" comes for free, visibly).
- Initial key inventory (one `ConfigKey` constant each, single module):
  `SelectWindowsModal = false`, `QuickPickThreshold = 5`,
  `TreeAutoBuildThreshold = 100`, `ThicknessBucketCap = 8`, plus the EFC
  `connectionStrings` entries and a provider choice (`MSSQL` / `SQLite`).
- EFC example located in STL: `Apps/DistrProc/Migrations/` — `CommonDbContext.cs`
  base + per-service `DbContext` + generated migrations, C# projects. The
  nearly-empty BMN EFC project follows that shape with one `Test` table.

---

## 5. Remaining questions (the whole round)

| # | Topic | Question |
|---|---|---|
| Q25 | `.ocproj` | Port scene Save/Open (+ autosave/recent) from the dead Shell into the relocated workbench within 0038 (recommended), or defer scene persistence? |
| Q26 | Relocation target | Move the workbench views into the existing `OpticalConstructor.Ui` project, retiring its dead Shell code (recommended — the name is right and `UserEnvironment` already lives there), or create a fresh project? |
| Q27 | Test launcher | Give `TestWindows` its own tiny executable host project for the new test launcher (recommended — the product App then drops its reference to test code entirely), or keep a "Test windows…" button on the product launcher? |
| Q28 | Prebind defaults | Confirm the seeded defaults for pre-binding: LP → ideal linear (`pol-lp`), CP → ideal circular **left** (`pol-cp-left` — or should right be the default?), light source → the seeded 600 nm source, detector → intensity detector. |
| Q29 | Out-of-band | Evaluate against the wavelengths the current run actually requests (recommended, §3) — confirm? |

If 005 settles these, 006 is the preliminary spec.
