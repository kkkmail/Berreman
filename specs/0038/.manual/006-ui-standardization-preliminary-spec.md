# 0038-006 — UI standardization: windows, faceted trees, lifecycle, experiments & inverse wiring (preliminary spec)

**Status: preliminary — feature list / mini-spec, no code yet.** Assembled from the
agreed discussion `001` → `005` (`001-task.txt`, `002` comments, `003` answers,
`004` round 2, `005` answers). This document is the input to the next spec-writer →
arc-runner cycle; every type named below is a *target shape*, not final. It changes
no files. Numbered decisions from the discussion are recorded in §17; the three
interpretation calls made where 005 left latitude were reviewed and **approved by
the operator after 006** — they are flagged **[DECIDED]** inline and recorded as
binding decisions in §17.

---

## 1. Goal

Standardize the Optical Constructor UI around a small set of pluggable pieces:

- **The workbench moves to its proper home** (out of `TestWindows`); test scenes get
  their own tiny executable launcher; the product Launcher gains
  `Main / Inverse / Materials / Library` (§4).
- **Materials and Library leave the ribbon** and become their own single-instance
  windows, openable from both the Launcher and the constructor (§5, §8, §9).
- **One faceted, filterable, reconfigurable tree control** powers both windows:
  attribute-driven branches, live counts, count-preview on offered constraints,
  breadcrumb of applied constraints, dynamic numeric buckets, non-empty branches
  only (§7).
- **Selector = Library in a Select state** — the same window in code, opened
  kind-constrained with Select/Close buttons; the sample editor picks materials the
  same way from the Materials window (§9, §10, §12).
- **Ideal elements become ordinary protected library entries** (LP, CPL, CPR, …)
  and every new table element except a sample is pre-bound to its seeded default;
  compound constant-Mueller elements (LPCP, CPLP, …) become describable (§10).
- **Material/sample lifecycle**: versioning + supersede, active/inactive, no
  deletion of used things (§13).
- **Experiments hold the full setup** (ordered element descriptors, sample and
  source optional — calibration and dark-line experiments are first-class), and all
  scene/experiment IO is wired **through proxies without real IO** (§14).
- **Set / not-set / out-of-band are visible on the table** (§11).
- **Material editor** gets a split layout + tabbed multi-curve charts; both editors
  get unsaved-edit confirmation (§12).
- **Inverse-problem wiring** (NOT the solver): Inverse launcher button, inverse
  constructor state, experiment collections with per-experiment CSV data files,
  pure parsers behind a data proxy, a stub solver screen that receives and
  validates everything (§15).
- **`appsettings.json` via `Softellect.Sys.AppSettings`**, a nearly-empty C# EFC
  project proving DB wiring, and an F# seeding project pushing the existing Domain
  seeds through proxies (§6, §16).

---

## 2. Current state (verified facts the spec-writer can rely on)

- The screen users see is `TableAndElementRotationView.mainView` in
  **`OpticalConstructor.TestWindows`** (~2 400 lines), hosted by
  `MainConstructorWindow` (`…App/Program.fs:118-150`). The `OpticalConstructor.Ui`
  shell (`Shell.fs`, `Ribbon.fs`, `MaterialsView.fs`, `ConstructionPage.fs`, …) is
  dead code for the Main flow (`Program.fs:116-117`).
- Materials and Library are `FullSurface` ribbon bays
  (`…TestWindows/TableAndElementRotationView.fs:2339-2340`); the Selector bay binds
  entries via kind-filtered rows + confirm panel (`…Controls/LibraryControls.fs`).
- The Selector crash ("Cannot set Name : styled element already styled") is
  `LibraryControls` setting write-once `Border.name` on unkeyed generated rows
  (`LibraryControls.fs:112,148,220`); the fix pattern (AutomationId + comment)
  already exists in `MaterialsControls.fs:139-146` / `ExperimentControls.fs:264`.
  Latent copies: `RotationControls.fs:119`, `RayPositionControls.fs:71`,
  `ElementPaletteControls.fs:55`, `ExperimentControls.fs:240,581`.
- Domain proxies (`LibraryProxy`, `SampleProxy`, `MaterialProxy`, `CategoryProxy`,
  `ExperimentProxy`) are `[<ReferenceEquality>]` records over in-memory `ref Map`s,
  created **inside** `MainConstructorWindow` (`…App/Program.fs:129-148`). Seeds
  live next to their types with fixed literal Guids (`MaterialIds`
  `…Domain/MaterialLibrary.fs:51-63`, `builtInEntries` `:363-461`, `SeedSamples`
  `…Domain/ElementId.fs:381-525`, `seedEntries` `:532-541`).
- Ideal polarizers are already seeded entries (`pol-lp`, `pol-cp-left`,
  `pol-cp-right`, `ElementId.fs:537-539`); their Mueller matrices are computed on
  demand (`…Domain/Propagation.fs:54-89`). Detectors: `det-intensity`,
  `det-ellipsometer` (`ElementId.fs:535-536`); Ψ/Δ physics exists
  (`Propagation.fs:304-394`).
- Experiments hold only the varied element (`Experiment`, `ElementId.fs:909-917`);
  a second, older toggle-based `Groups.ExperimentCollection` (`Groups.fs:114-129`)
  is persisted in the groups JSON — a name collision with the new concept.
- `.ocproj` (scene JSON, `…Storage/ProjectFile.fs`) is wired only in the dead
  shell + tests; **out of scope this cycle** (operator, 005).
- No lifecycle/versioning concept exists; removal is hard-blocked while referenced
  (`MaterialStillReferenced`, `ElementId.fs:751-766`).
- No `appsettings.json` exists; user settings are `%AppData%…\environment.json`
  (`…Ui/UserEnvironment.fs`). `Softellect.Sys` **10.1.301.54** is already
  referenced by Storage/Optimization/BerremanTests; its
  `AppSettings.AppSettingsProvider` opens `appsettings.json` with typed
  `get*OrDefault` accessors and `SetOnMissing = true` (missing keys are written
  back with their defaults). STL EFC example: `Softellect/Apps/DistrProc/Migrations/`
  (`CommonDbContext.cs` + per-service contexts + migrations).
- Two parallel recent-file stores exist (`EnvironmentSettings.recentFiles` and
  `…Storage/RecentFiles.fs`); `EnvironmentSettings.lastFolders` is persisted but
  never read/updated by any picker; UiIds are scattered as per-control modules.

---

## 3. Binding constraints (every part)

3.1 **Elevate every primitive** (CLAUDE.md). New ids are single-case DUs with
`.value` + `create`/`tryCreate`; enumerated conditions are named multi-case DUs (no
naked `bool`, no `enum`); every `*Error` case carries `reason : string` or a typed
payload; public functions have explicit concrete signatures. The operator's
"is-new flag" and "protected flag" (005/003) are therefore two-case DUs
(`EntryFreshness`, `EntryProtection`), not bools.

3.2 **No real IO.** All catalogue, scene, and experiment IO goes through
`[<ReferenceEquality>]` proxy records with `createInMemory` implementations over
private `ref Map`s — the existing seam. This cycle writes **no** catalogue/scene/
experiment files to disk and adds **no** schema/migration work; every app start
re-seeds. `.ocproj` stays unwired. The only real IO permitted: (a) the existing
`environment.json` user-settings persistence, (b) `appsettings.json` through
`Softellect.Sys.AppSettings` only, and (c) the thin measured-data CSV file-read
adapter of §15 **[DECIDED — see §17]**.

3.3 **UI is testable without a window.** Every new surface is a pure Domain edit
model + a handlers/message record; behaviour is unit-tested in Domain,
structure/interaction in `Avalonia.Headless` gates. Stable intent-named
`[<Literal>]` automation ids for every new control — consolidated into ONE ids
module this cycle (§16.3).

3.4 **FuncUI dynamic-list hygiene.** Generated rows/nodes NEVER set
`StyledElement.Name`; they use `AutomationProperties.AutomationId` and per-item
`View.withKey`. Filter text boxes over tree-rebuilding state commit on
Enter/LostFocus (never per keystroke).

3.5 **Interpolated strings, never `sprintf`**; four-space indent; opening `{` on
its own line; space before annotation colons; camelCase record fields; LF endings.

3.6 **Zero warnings from our code** (CLAUDE.md); only `NU1701`/`NU1901`–`NU1904`
NuGet advisories are exempt. The relocation (§4) must not reintroduce `MSB3277`.

3.7 **No legacy, no fallback, no migration.** Nothing library-related is
serialized yet; re-typed structures replace old ones cleanly, seeds re-seed.

---

## 4. Relocation: the workbench moves home; test scenes get their own launcher

4.1 **Workbench → `OpticalConstructor.Ui`** (operator, 005/Q26). Move the working
views out of `TestWindows` — `TableAndElementRotationView`, `ElementRenderer`,
`Catalogue`, `MaterialEditorView/Window`, `SampleEditorView/Window`,
`CategoryEditorWindow`, `NkDispersionChart` (until §12 replaces it), and friends —
into `OpticalConstructor.Ui`, **retiring the dead shell code there** (`Shell.fs`,
`Ribbon.fs`, `MaterialsView.fs`, `ConstructionPage.fs`, `ConstructorView.fs`,
`LifecycleView.fs`, `Templates.fs`, `Help.fs` and their tests where they pin dead
behaviour). `UserEnvironment.fs` stays (it is live). Namespaces follow the project;
file-linking is not used — this is a real move (one `.fsproj` per folder).

4.2 **Test launcher executable** (operator, 005/Q27). A new tiny host project
(new folder, e.g. `OpticalConstructor.TestWindows.App`) with a `TestLauncherWindow`
wiring the seven diagnostic scenes currently on the product launcher
(`…App/Program.fs:177-224`). `OpticalConstructor.App` drops its reference to
`TestWindows` entirely; test code leaves the product dependency graph.

4.3 **Product launcher** (`…App/Program.fs`, `LauncherWindow`): buttons become
`Main / Inverse / Materials / Library` (this order; operator 001+003), stable
`Name`s + automation ids. `Inverse` opens the §15 inverse-state constructor;
`Materials`/`Library` open the §8/§9 windows.

---

## 5. App-scoped composition, window registry, and the WindowLauncher seam

5.1 **Proxies move to app scope.** The five proxies (plus the new ones this spec
adds) are created ONCE at startup and injected into every window. Launcher-opened
and constructor-opened windows see the same in-memory stores.

5.2 **Window policy** (operator, 003/Q3 + 005):

- Materials window, Library window: **single instance** — a second open request
  activates the existing window (re-targeting it if opened in Select state, §9.4).
- Editor windows (material/sample/category): **multiple instances**, keyed by the
  edited entity's id; opening an entity already being edited re-activates its
  window. **New entities mint their Guid upfront** at Add-window open
  (`MaterialId.create ()` / `SampleId.create ()`) and carry
  `EntryFreshness = NewUnsaved | Persisted` — so the window registry keys uniformly
  by id from the first moment and no id-assignment is scattered across save paths
  (operator, 005).
- A `WindowRegistry` (host-layer, outside the Elmish model — the
  `Shell.storageProvider` precedent) maps `WindowKey -> Window`.

5.3 **`WindowLauncher` seam.** All window opening goes through one injected record
(the `EditorLaunchers` precedent, `…TestWindows/TableAndElementRotationView.fs:116-141`,
generalized): it consults the registry (activate vs create) and reads the
`SelectWindowsModal` appsettings switch — Select-state windows open modal
(`ShowDialog`, owner = requesting window) when `true`, non-modal when `false`
(default `false`; operator, 003/Q5). Browse-state windows are always non-modal.

---

## 6. appsettings.json via Softellect.Sys (introduced NOW)

- Read/write ONLY through `Softellect.Sys.AppSettings.AppSettingsProvider`
  (operator, 003/Q15) — never `File.ReadAllText`/`IConfiguration`. Provider is
  created at the composition root; typed values flow in through config/context
  records (no ambient reads from views or Domain).
- One `[<RequireQualifiedAccess>]` module of `ConfigKey` constants (single place).
  Initial inventory: `SelectWindowsModal = false` (§5.3), `QuickPickThreshold = 5`
  (§9.5), `TreeAutoBuildThreshold = 100` (§7.6), `ThicknessBucketCap = 8` (§7.5),
  plus the §16 `connectionStrings` entries and DB provider choice.
- `SetOnMissing = true` means first run writes every default back — the file
  self-documents; tests cover the defaults path (missing file/keys) purely by
  constructing settings records, not by touching disk.
- `appsettings.json` is a build-copied content item of `OpticalConstructor.App`
  (and the §16 projects); user preferences REMAIN in `environment.json` — the two
  do not merge.

---

## 7. The faceted tree — one control for Materials and Library

The core new abstraction (faceted navigation): items + attributes → a filterable,
reconfigurable tree with counts. Pure F# core (new Domain module, e.g.
`…Domain/Facets.fs`), domain-free rendering control
(`…Controls/FacetedTreeControls.fs`), instantiated twice (§8, §9).

### 7.1 Attribute model (target shape)

```fsharp
type AttributeKey = | AttributeKey of string        // .value; stable, not localized
type DiscreteKey  = | DiscreteKey of string          // stable code of a discrete value

type AttributeValue =
    | DiscreteValue of DiscreteKey
    | NumericValue of double                          // bucketed dynamically (§7.5)

type Applicability = ApplicableAttribute | InapplicableAttribute

type AttributeKind =
    | DiscreteAttribute                               // value set = corpus-derived or fixed
    | NumericAttribute                                // gets bucket construction (§7.5)

type AttributeDef<'item> =
    { key : AttributeKey
      name : string                                   // display label
      kind : AttributeKind
      appliesTo : 'item -> Applicability               // dependent facets (§7.2)
      extract : 'item -> AttributeValue list }          // LIST — multi-valued (§7.3)
```

Constraints are values (`AppliedConstraint = { key; selection }`, selection =
discrete key set (OR within a facet) or numeric range); the engine is a pure fold:
`filter : AttributeDef<'item> list -> AppliedConstraint list -> 'item list -> 'item list`
plus `countFor` (count-preview of a candidate constraint) and
`buildTree : Representation -> …` — all unit-tested without UI. Corpus sizes are
tens–hundreds; naive folds, no indices.

### 7.2 Dependent facets

`appliesTo` removes an attribute entirely when inapplicable (handedness/symmetry
class only when optically active; dispersion model only when dispersive;
scalar/gyromagnetic only when magnetic; substrate material only for Plate/Wedge —
operator, 003/Q10). Physics-derived offers reuse the existing functions
(`availableGyrationClasses`, `…Domain/MaterialComplexityEditor.fs:373-385`), never
re-derived.

### 7.3 Material facets (extractors run on `properties : OpticalPropertiesWithDisp` —
`complexity` is `None` for coded presets)

| Facet | Source | Notes |
|---|---|---|
| Category | `MaterialEntry.category` | corpus-derived value set |
| Anisotropy | `ConstantEpsValue`/`EpsDispersiveValue` cases | Isotropic/Uniaxial/Biaxial |
| Constant vs dispersive | `EpsWithDispValue` case | |
| Transparent vs absorbing | `ConstantEpsValue` case | **constant materials only** (operator, 003/Q9) |
| Dispersion model | `DispersionModel` (10 cases) | **multi-valued** (per axis/segment) — counts don't sum to total, by design |
| Optical activity → symmetry class, handedness | `GyrationClass`, `Handedness` | dependent |
| Magnetic → scalar / gyromagnetic | `ConstantMuValue`/`MuWithDispValue` | dependent |

### 7.4 Library (sample) facets

All material facets applied as the **distinct union over constituent materials**
(substrate + layers — "any constituent matches"; operator, 003/Q13), plus:
substrate material (Plate/Wedge only); "has thin films" derived from
`structure.films` non-empty, independent of `SubstrateKind` (operator, 003/Q11);
film material(s) (corpus-derived, sorted by name, multiselect); per-film thickness
buckets (§7.5). Non-sample entries (sources/polarizers/detectors) carry the kind
facet and their own small facets (e.g. polarizer category, §10.3).

### 7.5 Numeric bucketing (agreed algorithm, 003/Q12 + 004 §3)

1–2–5 log-ladder boundaries spanning the **currently constrained** population;
empty buckets dropped; adjacent buckets merged (fewest-items first) while count >
`ThicknessBucketCap` (default 8); half-open `[lo, hi)`; single distinct value ⇒
exact-value bucket; display `10–20 nm (7)`; nm below 1 µm, µm above; recomputed on
every constraint change; a manual min–max entry always offered and applied as an
ordinary constraint chip.

### 7.6 Tree behaviour

- Branch = facet value under the current **representation** (an ordered
  `AttributeKey` list); unexpanded branches show their element count; zero-count
  branches never shown; text filter participates as a constraint.
- **Search order ≠ representation order** (operator, 003/Q14): applying
  constraints never reshuffles the tree; a breadcrumb strip shows applied
  constraints in application order, each removable, each with its after-count;
  count-preview on every offered value. Reordering the representation is an
  explicit user action (named representations; the seeded "By kind" tree is the
  Library default).
- Result count is always live (cheap); **tree materialization** is gated: auto
  when result count ≤ `TreeAutoBuildThreshold` (default 100), else a Show/Search
  button (operator, 001).
- Rows keyed (`View.withKey`), AutomationId only (§3.4); tree area in a
  ScrollViewer (may span pages vertically).

---

## 8. Materials window

`Browse` mode replaces the Materials bay: faceted tree (§7.3) + view panel + the
existing verbs (Add / Edit / Remove / Categories…) rewired to the window; opened
from Launcher and constructor through `WindowLauncher` (single instance).
`Select` mode (§9.4) is used by the sample editor (§12.3). The Materials **bay is
removed** from the ribbon.

## 9. Library window and the Selector flow

9.1 **Terminology** (operator, 003/Q4): *Library* = the library of optical
elements (samples ∪ polarizers ∪ sources ∪ detectors …); *Materials* = homogeneous
optical-property descriptions. Two windows, one `FacetedTree` control.

9.2 The Library window's corpus is `LibraryEntry` (all kinds), default
representation grouped by kind; sample subtree carries §7.4 facets. Browse mode
replaces the Library bay (removed from the ribbon); verbs: Add sample / Make
multilayer / Edit / Remove as today, plus lifecycle verbs (§13).

9.3 **Mode DU** (same window in code — never a copy):

```fsharp
type LibraryWindowMode =
    | Browse
    | Select of SelectionContext

type SelectionContext =
    { constraint : KindConstraint                     // pre-applied, not removable in Select
      target : SelectionTarget                        // table element id | sample-layer position
      onSelected : LibraryEntryId -> unit
      onCancelled : unit -> unit }
```

Select mode adds exactly two buttons — **Select** (returns highlighted entry,
closes) and **Close** (cancels) — everything else is the ordinary window, so
add-on-the-fly works because it IS the library.

9.4 **Staleness rules** (non-modal default): changing the table selection (or the
target's disappearance) cancels/closes the open Select window (the
`…TableAndElementRotationView.fs:941-944` precedent); a second Choose closes the
first; `onSelected` dispatches a targeted message — vanished target ⇒ no-op +
status line, never a throw.

9.5 **Selector bay flow**: click Selector → click element → **Choose…** opens the
Library window in Select state constrained to the element's kind. The inline
quick-pick strip renders ONLY when the kind-constrained entry count <
`QuickPickThreshold` (default 5); both paths converge on the same bind message
(operator, 003/Q6).

## 10. Ideal elements become protected library entries; pre-binding

10.1 `EntryProtection = ProtectedBuiltIn | UserManaged` on library entries.
Protected entries cannot be deleted, marked inactive, or superseded (operator,
003/Q7). The notion "ideal element" disappears — they are just entries.

10.2 **Pre-binding on add** (operator, 003/Q7 + 005/Q28): every new table element
except a sample is created pre-bound to its seeded default — light source →
`src-600`, detector → `det-intensity`, and the polarizer palette exposes **three**
add buttons: **LP** (→ `pol-lp`), **CPL** (→ `pol-cp-left`), **CPR**
(→ `pol-cp-right`) — `CatalogueKind` is unchanged (LP ⇒ `LinearPolarizer`,
CPL/CPR ⇒ `CircularPolarizer`); only the palette and the pre-bound entry differ
**[DECIDED — see §17]**. Samples stay unbound (that is the inverse hook,
§15).

10.3 **Behavior as data** (target shape):

```fsharp
type PolarizerBehavior =
    | ComputedIdeal of PolarizerKind                  // physics synthesized as today
    | ConstantMueller of MuellerComponent list        // ordered; product on demand

type MuellerComponent =
    { matrix : MuellerMatrix                           // stored at reference orientation
      offset : Angle }                                  // rotated at run time: R(−θ)·M·R(θ)

type PolarizerCategory = LpCategory | CpCategory | LpCpCategory | CpLpCategory | CustomMueller
```

Compounds stored as ordered component lists (operator, 003/Q8). Constant-Mueller
entries live only in the Stokes/Mueller pipeline (they never enter the Berreman
stack). The dedicated editor for these entries is explicitly **out of scope**
(operator, 001) — the DU just makes it a non-breaking future addition. The silent
ideal-analyzer fallback (`runAnalyzerKind`,
`…TableAndElementRotationView.fs:1418-1427`) is retired in favor of pre-binding.

## 11. Table visuals: bound / unbound / out-of-band

11.1 The renderer input gains binding state (a DU, three-valued):
`BindingState = Bound | Unbound | NotBindable` (lens/mirror = `NotBindable`,
rendered normally). Unbound ⇒ dashed outline + ghosted fill; bound ⇒ solid
(pattern, not hue — colorblind-safe). `Drawable` (`…ElementRenderer.fs:26-32`)
carries it; readout/Details keep the text.

11.2 **Out-of-band indicator** (operator, 003/Q22 + 005/Q29): independent of
bound/unbound. An element flags when it is bound, some material reachable through
its binding is dispersive, and the wavelength(s) **the actual experiment
requests** (single λ for a fixed setting; the full range for a sweep — e.g. 600 nm
is in a 300–700 nm band, a 200–800 nm sweep is not) fall partly outside the union
of that material's defined dispersion segments (`EditSegment.interval`). Visual: a
small warning badge on the element + hover tooltip naming the offending
material(s) with defined vs requested ranges; same text in the Details bay.
Constant materials never flag.

## 12. Editor rework

12.1 **Material editor layout**: two panes split by a **vertical splitter** —
the progressive ladder in the left pane inside a ScrollViewer, the chart filling
the right pane at full height with sensible minima (operator, 003/Q16). The
DockPanel squeeze (`…MaterialEditorView.fs:1067-1132`) is retired.

12.2 **Tabbed multi-curve charts** replacing the ε₁₁-only preview
(`NkDispersionChart.fs:40-64`): tab **n, k** — all principal axes (n_x/n_y/n_z,
k_x/k_y/k_z on the right axis) with legend + per-series toggles; tab **Gyration**
— g components vs λ (present only when optically active); tab **μ** — Polder
components vs λ (only when magnetic). Tabs appear/disappear with the ladder
toggles, so "what is being drawn" is always answered by the visible tab + legend.

12.3 **Sample editor material picking**: the `WrapPanel` of material buttons
(`…SampleEditorView.fs:740-758`) is replaced by the **Materials window in Select
state** targeted at a `LayerPosition` (§9.3/§9.4 rules; deleted row ⇒ no-op).
Material list staleness: **re-query the proxy when a Select window returns and on
window activation** (operator, 003/Q17) — replacing the load-once snapshot
(`…SampleEditorWindow.fs:32-35`); live cross-window notifications deferred.

12.4 **Unsaved-edit confirmation** (both editors): dirty = structural inequality
between the edit state captured at load and the current state (plain records/DUs;
no flags to maintain); the confirm intercepts BOTH Cancel and window close
(`OnClosing` on the `HostWindow`) — today `CancelClicked` closes silently
(`MaterialEditorView.fs:423-424`, `SampleEditorView.fs:549-550`).

## 13. Lifecycle: versioning + supersede, active/inactive

13.1 Two-level identity (operator, 003/Q18-20 + 004 §1): `MaterialId` stays the
stable, name-carrying identity; references (sample layers, experiment
descriptors) point at a **version**:

```fsharp
type VersionNumber = | VersionNumber of int           // .value, .next
type MaterialVersionId = { materialId : MaterialId; version : VersionNumber }
type SampleVersionId   = { sampleId : SampleId;     version : VersionNumber }
type EntryLifecycle = ActiveEntry | InactiveEntry
```

13.2 **Version creation rule** (operator, 003/Q20, restated in 004 §1 and
confirmed): editing a **used** vN and saving a physics change creates vN+1 —
and only if the physics actually differs (structural equality; an identical save
stays vN). An **unused** version mutates in place no matter how often edited.
The library always edits the **latest** version; older versions are view-only.
Metadata edits (name, description, category) never version; physics edits do.

13.3 **"Used"** = referenced by a persisted experiment or saved project (operator,
003/Q19). Nothing is persisted yet, so the check is a seam —
`versionsInUse : unit -> Set<VersionRef>` computed from the in-memory experiment
store — fully unit-testable now, truthful later when persistence arrives.

13.4 **Inactive** hides an entry from new-use offers (pickers, Select mode,
default facet counts — with a "show inactive/superseded" toggle); resolution of
existing references ignores it. Superseded versions behave as inactive
automatically. `ProtectedBuiltIn` entries can be neither inactivated nor
superseded (§10.1). Deletion remains hard-blocked while referenced (existing
`MaterialStillReferenced` behaviour), and additionally for any used version.

13.5 Export-embeds-versions (004 §1) is a recorded decision for the future
export/report path; **no export work this cycle** (no real IO, §3.2).

## 14. Experiments hold the full setup; scene/experiment IO through proxies (no real IO)

14.1 `Experiments.Experiment` grows a **setup**: an ordered element-descriptor
list — kind, placement/orientation summary, bound entry **version** reference
(§13.1) — replacing the single varied-element reference; the varied element is
identified within the list. Sample and light source are optional
(`E1 = LS+LP+S+LP(rotate)`, `E2 = LS+LP+LP(rotate)` — no sample,
`E3 =` nothing — dark line; operator, 001). Detector kind is part of the chain and
determines the expected data-file shape (§15.3).

14.2 **Naming consolidation**: the old toggle-based `Groups.ExperimentCollection`
(`Groups.fs:114-129`) is renamed out of the way (e.g. `WorkbenchToggleSet`) so
*experiment collection* means the §15 concept exclusively.

14.3 **Scene/experiment IO wired through proxies WITHOUT real IO** (operator,
005/Q25): save/load of the scene and of experiment collections are proxy fields
(`Result`-returning, camelCase) with `createInMemory` implementations (a named
in-memory scene/collection store). Round-trips are unit tests. `.ocproj` and any
disk format are explicitly out of scope; the proxies are the seam a future
storage cycle swaps.

## 15. Inverse problem — wiring only (solver explicitly out of scope)

15.1 **Launcher button `Inverse`** opens the same constructor in an inverse state:
the sample element is unbound (§10.2) or bound to a **hint** sample; no hint ⇒ no
experiment chart; hint ⇒ chart as today (operator, 001).

15.2 The user builds an **experiment collection** (experiments per §14.1, each
possibly sample-less/dark) and attaches **one data file per experiment**.

15.3 **Data files.** Strict v1 schemas (operator, 003/005): intensity — first row
labels (ignored), then comma-separated `X,Y`, invariant culture, typed parse
errors; the X column means whatever the experiment's varied parameter says, and a
range/units mismatch against the experiment is a loud typed validation error.
Ellipsometric — `wavelength_nm, psi_deg, delta_deg` (+ optional `aoi_deg`), one
file per experiment; NCS / Mueller-element imports are future parsers, not schema
changes.

15.4 **Parsers are pure** (`string -> Result<IntensitySeries, ExperimentDataError>`
etc.), tested against in-memory strings; file access sits behind

```fsharp
[<ReferenceEquality>]
type ExperimentDataProxy =
    { tryLoadIntensity : DataFilePath -> Result<IntensitySeries, ExperimentDataError>
      tryLoadEllipsometric : DataFilePath -> Result<EllipsometricSeries, ExperimentDataError> }
```

with a mock for every test and a thin real-file adapter (read text → pure parser)
as the runtime wiring **[DECIDED — see §17]**.

15.5 **Last folder**: the file picker starts at the persisted last folder and
updates it only on a confirmed selection (cancel changes nothing) — wiring the
existing inert `EnvironmentSettings.lastFolders` through `SuggestedStartLocation`
(`…Ui/Shell.fs:403-450` precedent code moves with §4).

15.6 **Stub solver screen**: the Inverse flow ends at a simple screen listing
general information about the received collection (experiments, setups, detector
kinds, attached files, parse/validation status per file) and the message that the
actual solver will be done separately. It performs basic validation (all elements
except sample specified; every experiment has a readable, schema-valid file;
ranges consistent) and reports typed errors. **No solving, no normalization** — data is
gathered raw; corrections belong to the future solver (operator, 003/Q23, 005).

## 16. Storage scaffolding and consolidations

16.1 **Nearly-empty C# EFC project** (STL `Apps/DistrProc/Migrations` shape): one
`Test` table, a `DbContext`, an initial migration; provider (MSSQL/SQLite) and
connection strings from `appsettings.json` via `AppSettingsProvider`
(`tryGetConnectionString`). Its only purpose is proving EFC wiring
creates/migrates; **the app does not reference or use it** (operator, 003/005 —
no real database, no DB/file split this cycle).

16.2 **F# seeding project**: takes a `SeedingProxy` (record of `Result`-returning
`save…` functions) and pushes the **existing Domain seed values**
(`builtInEntries`, `SeedSamples`, `seedEntries`, `standardCategories` — which stay
where they live, close to the source) through it. Today's implementation of the
proxy is the in-memory stores — i.e. seeding runs with no database at all
(operator, 001). Seeded Guids are frozen forever (future foreign keys).

16.3 **Consolidations** (operator, 003): recent-files — ONE store (collapse
`EnvironmentSettings.recentFiles` vs `…Storage/RecentFiles.fs`); automation ids —
ONE `[<Literal>]` constants module replacing the dozen per-control `UiIds`
modules.

---

## 17. Decisions (from the operator, this cycle) + recorded interpretations

Operator decisions: Q1–Q24 (003), Q25–Q29 (005), as embedded above — notably:
Library = optical-element library, Materials separate (Q4); modal-Select via
appsettings default false (Q5); quick-pick threshold 5 (Q6); ideal elements =
protected library entries + pre-binding (Q7); component-list compounds (Q8);
facet answers (Q9–Q13); no tree reshuffle (Q14); appsettings NOW via
Softellect.Sys (Q15); vertical splitter + tabs (Q16); re-query on return (Q17);
versioning B + export-embeds-A (Q18), used = persisted-experiment/saved-project
(Q19), used-edit ⇒ auto next version, unused edits mutate in place (Q20, 005 §1);
ordered element descriptors (Q21); two set-indicators incl. out-of-band vs the
actual experiment (Q22, Q29); raw data, solver owns corrections (Q23); no real
DB, nearly-empty EFC + test table (Q24); `.ocproj` out of scope, scene/experiment
IO proxied without real IO (Q25); workbench → `.Ui` (Q26); test-windows exe
(Q27); defaults LP/CPL/CPR (Q28); upfront Guid + is-new marker for new entities
(005 window-policy note).

Interpretations recorded where 005 left latitude — **all three reviewed and
approved by the operator after 006; they are binding decisions**:

1. **[Q28 palette]** "We need default LP, CPL, CPR" is implemented as three add
   buttons (LP / CPL / CPR), each pre-binding its ideal entry; `CatalogueKind`
   keeps `LinearPolarizer`/`CircularPolarizer` unchanged.
2. **[CSV read]** "No real IO" (005/Q25) is read as covering scene/experiment
   *persistence*; the measured-data CSV **file read** stays a thin real adapter
   behind `ExperimentDataProxy` (001 asked for a file-choose + proxy-load flow;
   all tests use mocks/in-memory streams).
3. **[Is-new marker]** "guid + is new = true flag" (005) is modelled as
   `EntryFreshness = NewUnsaved | Persisted` per the no-naked-bool rule.

---

## 18. Proposed phasing (small slices, each independently green)

1. **Crash fix + list-hygiene sweep** — `LibraryControls` (and
   `RotationControls`, `RayPositionControls`, `ElementPaletteControls`,
   `ExperimentControls`) to AutomationId + `withKey`; no behaviour change.
   Immediately shippable.
2. **Relocation** — workbench views → `OpticalConstructor.Ui` (dead shell
   retired); `OpticalConstructor.TestWindows.App` test launcher; product launcher
   `Main/Inverse/Materials/Library` (Inverse/Materials/Library may open
   placeholders until 5–7/13). Warning-clean.
3. **appsettings + composition** — `AppSettingsProvider` at the root; ConfigKey
   module; app-scoped proxies; `WindowRegistry` + `WindowLauncher` (single
   instance, id-keyed editors, upfront Guids + `EntryFreshness`, modal switch).
4. **Faceted core (Domain)** — attribute model, constraints, counts,
   count-preview, bucketing, representations; pure tests only.
5. **FacetedTree control + Materials window** — Browse mode replaces the
   Materials bay; verbs rewired; headless proofs.
6. **Library window** — merged element-preset corpus; `PolarizerBehavior` +
   `PolarizerCategory` + `EntryProtection`; Browse mode replaces the Library bay.
7. **Select mode + Selector rework + sample-editor picker** — mode DU, staleness
   rules, quick-pick threshold, pre-binding LP/CPL/CPR/source/detector, re-query
   on return.
8. **Lifecycle** — version types, creation rule, active/inactive, used-seam;
   picker/facet filtering; lifecycle verbs in both windows.
9. **Experiments** — full-setup descriptors (versioned refs), optional
   sample/source, `Groups.ExperimentCollection` rename; scene/experiment
   in-memory IO proxies + round-trip tests.
10. **Table visuals** — `BindingState` rendering + out-of-band badge/tooltip.
11. **Editors** — material-editor split layout + tabbed multi-curve charts;
    sample/material dirty-confirm incl. `OnClosing`.
12. **Inverse wiring** — inverse state, hint semantics, collections, pure
    parsers + `ExperimentDataProxy` (+ thin file adapter per §17.2), last-folder,
    stub solver screen with validation.
13. **Storage scaffolding** — EFC test-table project (appsettings-configured
    MSSQL/SQLite) + F# seeding project over `SeedingProxy`.
14. **Consolidations + warning sweep** — single recent-files store; single
    automation-ids module; residual warnings.

---

## 19. References (web)

**Faceted navigation (the tree, counts, breadcrumbs)**
- NN/g — Filters vs. facets — <https://www.nngroup.com/articles/filters-vs-facets/>
- NN/g — Applying filters — <https://www.nngroup.com/articles/applying-filters/>

**Numeric bucketing (1–2–5 ladder)**
- Preferred numbers (1–2–5 series / Renard) — <https://en.wikipedia.org/wiki/Preferred_number>

**Versioning / supersede**
- Slowly Changing Dimensions (Type 2) — <https://en.wikipedia.org/wiki/Slowly_changing_dimension>
- Temporal databases — <https://en.wikipedia.org/wiki/Temporal_database>

**Polarimetry / ellipsometry (constant Mueller elements, data formats)**
- Mueller calculus (incl. rotation of Mueller matrices) — <https://en.wikipedia.org/wiki/Mueller_calculus>
- Ellipsometry (Ψ/Δ) — <https://en.wikipedia.org/wiki/Ellipsometry>
- J.A. Woollam — Ellipsometry FAQ (Ψ/Δ data conventions) — <https://www.jawoollam.com/resources/ellipsometry-faq>

**Avalonia / FuncUI (the crash class)**
- `StyledElement.Name` (write-once after styling) — <https://api-docs.avaloniaui.net/docs/P_Avalonia_StyledElement_Name>
- Avalonia.FuncUI virtual-DOM patcher — <https://github.com/fsprojects/Avalonia.FuncUI>

**EF Core (the test-table project)**
- EF Core docs (providers: SQL Server, SQLite; migrations) — <https://learn.microsoft.com/en-us/ef/core/>
