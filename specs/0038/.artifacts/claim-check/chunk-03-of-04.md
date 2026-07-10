{
  "claims": [
    {
      "id": 1,
      "phrase": "`MaterialId` / `SampleId` stay the stable identities",
      "spec_location": "§H.0",
      "evidence": "Class-anchored grep: type MaterialId at Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs:27; type SampleId at Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs:41 — exactly one definition each. Trigger 'stay' inferred (implies the identity types exist today).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 2,
      "phrase": "nothing is persisted yet, so the check is a seam — `versionsInUse : unit -> Set<VersionRef>` computed from the in-memory experiment store",
      "spec_location": "§H.0",
      "evidence": "Negative current-state claim (phrase inferred). Grep 'Experiment' over OpticalConstructor.Storage/*.fs -> 0 hits (constructor experiments have no persistence); grep MaterialVersionId|SampleVersionId|VersionRef|EntryLifecycle|ProtectedBuiltIn repo-wide -> 0 hits (version identities do not exist yet, so no persisted artefact can reference a version); ProjectJson.fs serializes no scene/element/experiment content.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 3,
      "phrase": "Deletion remains hard-blocked while referenced (the `MaterialStillReferenced` block, `…Domain/ElementId.fs:755-765`)",
      "spec_location": "§H.0",
      "evidence": "Read Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs:750-767 — removeMaterial's samplesReferencing branch spans exactly 755-765 and ends in Error (MaterialStillReferenced ...) at line 765.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 4,
      "phrase": "Both registered proxies re-shape in place per §0.2",
      "spec_location": "§H.0",
      "evidence": "Phrase inferred ('registered proxies' implies existing wiring); the §0.2 anchor lies outside this chunk. MaterialProxy.createInMemory documented and implemented at ElementId.fs:769 with the live SampleProxy store referenced at ElementId.fs:772; composition-root createInMemory wiring present in OpticalConstructor.App/Program.fs. 'Both' read from Part-H context as the material + sample proxies.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 5,
      "phrase": "`Experiments.Experiment` holds only the varied element today (`…Domain/ElementId.fs:909-917`)",
      "spec_location": "§I.0",
      "evidence": "module Experiments at ElementId.fs:796; type Experiment at exactly 909-917 with fields id/elementId/elementLabel/variable/measurement/range — the varied element only, no setup/element-descriptor list.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 6,
      "phrase": "the OLD toggle-based `Groups.ExperimentCollection` (`…Domain/Groups.fs:114-129`)",
      "spec_location": "§I.0",
      "evidence": "type ExperimentCollection at Groups.fs:114-129 (record 114-125 + static create 127-129); toggle-based as claimed — experiments/elements/groups are all Toggle list fields.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 7,
      "phrase": "persisted inside the groups JSON whose load/save lives in `…Storage/GroupsLibrary.fs`",
      "spec_location": "§I.0",
      "evidence": "OpticalConstructor.Storage/GroupsLibrary.fs exists: load at :115, save at :127, schema validate-on-load per :81; Groups.fs:144-147 GroupsLibrary record holds collections : ExperimentCollection list, the value that JSON file carries.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 8,
      "phrase": "per the doc at `…Domain/Groups.fs:141-143`",
      "spec_location": "§I.0",
      "evidence": "Read Groups.fs:141-143 — the doc reads: '`OpticalConstructor.Storage/GroupsLibrary.fs` owns the load/save and schema validation of this value; it is kept apart from any per-project `.ocproj`.' — exactly what the spec cites.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 9,
      "phrase": "is renamed `WorkbenchToggleSet` (type rename only — the JSON shape is unchanged)",
      "spec_location": "§I.0",
      "evidence": "Trigger 'unchanged'. Forward-looking constraint whose referenced existing artefact resolves: Groups.fs:111-113 doc — collections are stored in the SAME separate JSON file as groups and round-trip through it (AC-H1) — and GroupsLibrary.fs owns that file's load/save/schema, so the JSON shape the rename must preserve exists.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 10,
      "phrase": "*experiment collection* means the live constructor concept exclusively (its `commit`, `…Domain/ElementId.fs:1010`)",
      "spec_location": "§I.0",
      "evidence": "let commit (c : ExperimentCollection) : ExperimentCollection at exactly ElementId.fs:1010, inside the Experiments module (the live constructor concept).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 11,
      "phrase": "NO disk format, `.ocproj` stays unwired (operator, Q25)",
      "spec_location": "§I.0",
      "evidence": "Inferred ('stays'). The .ocproj seam exists (ProjectFile.fs:17 let extension = \".ocproj\") but carries no scene/experiment-collection content today: grep scene|placement|element over ProjectJson.fs -> only a matrix comment at :29; grep 'Experiment' over Storage/*.fs -> 0 hits. Scene/experiment-collection persistence is currently not wired into .ocproj, matching the claim.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 12,
      "phrase": "The renderer input `Drawable` (`…TestWindows/ElementRenderer.fs:26-32`) gains a three-valued `BindingState`",
      "spec_location": "§J.0",
      "evidence": "type Drawable at exactly OpticalConstructor.TestWindows/ElementRenderer.fs:26-32 (fields placement/centre/zoom/opticalSign) — the existing renderer input the part extends.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 13,
      "phrase": "the readout/Details keep the text",
      "spec_location": "§J.0",
      "evidence": "Phrase inferred ('keep' = unchanged). Both artefacts exist in TableAndElementRotationView.fs: readout automation id at :42 with TextBlocks at :1133/:2348 and readoutText at :1088; the Details bay at :237-239 ('the selected element's bound Library entry') with the bound-entry readout noted at :1297.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 14,
      "phrase": "the union of that material's defined dispersion segments (`EditSegment.interval`, `…Domain/MaterialComplexityEditor.fs:100-102`)",
      "spec_location": "§J.0",
      "evidence": "type EditSegment at MaterialComplexityEditor.fs:100 with interval : WaveLengthInterval at :102 — the cited symbol sits inside the cited range.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 15,
      "phrase": "The Material editor's `DockPanel` layout squeezes the ladder and the preview into one column (`…TestWindows/MaterialEditorView.fs:1067-1132`)",
      "spec_location": "§K.0",
      "evidence": "let view at MaterialEditorView.fs:1067 ending :1132; DockPanel.create at :1071 and :1106; ladder panels stacked Dock.Top (:1096-1124) above the centre-filling preview (:1130) in a single column — no splitter, matching the claim.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 16,
      "phrase": "The single-curve preview (`n = Re[√ε₁₁]` / `k = Im[√ε₁₁]` only — `…TestWindows/NkDispersionChart.fs:40-64`)",
      "spec_location": "§K.0",
      "evidence": "NkDispersionChart.fs:40-42 samples Complex.Sqrt of (getEps w)[0,0] (ε₁₁); nkDispersionChart (:48-64) emits exactly two series, 'n' (Real) and 'k' (Imaginary) — no per-axis or gyration/mu curves.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 17,
      "phrase": "series assembled through the engine's `RhoWithDispValue.toRhoWithDisp`, `OpticalProperties/Active.fs:152`",
      "spec_location": "§K.0",
      "evidence": "member this.toRhoWithDisp : RhoWithDisp at exactly Berreman/OpticalProperties/Active.fs:152, on type RhoWithDispValue (:146).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 18,
      "phrase": "`MuWithDispValue.toMuWithDisp`, `Berreman/Berreman/Dispersion.fs:671`",
      "spec_location": "§K.0",
      "evidence": "member this.toMuWithDisp : MuWithDisp at exactly Berreman/Berreman/Dispersion.fs:671, on type MuWithDispValue (:667); the :662-666 doc confirms the Polder tensor semantics the spec describes.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 19,
      "phrase": "today `CancelClicked` closes silently (`…TestWindows/MaterialEditorView.fs:423-424`)",
      "spec_location": "§K.0",
      "evidence": "MaterialEditorView.fs:423-424 — '| CancelClicked -> m.context.requestClose ()' with no dirty-state confirmation, exactly the silent close the spec describes.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 20,
      "phrase": "today `CancelClicked` closes silently (`…TestWindows/SampleEditorView.fs:549-550`)",
      "spec_location": "§K.0",
      "evidence": "SampleEditorView.fs:549-550 — '| CancelClicked -> m.context.requestClose ()' with no dirty-state confirmation, same silent-close pattern.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 21,
      "phrase": "no hint ⇒ no experiment chart; hint ⇒ chart as today",
      "spec_location": "§L.0",
      "evidence": "Trigger 'as today'; no file pointer in the sentence, resolved class-anchored: 'experiment chart' maps to the single definition type ExperimentChart at OpticalConstructor.Controls/ExperimentChart.fs:18 (exactly one grep match) — the existing chart mechanism the claim defers to exists.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 22,
      "phrase": "the text-taking precedent `SpectralImport.parseSpectrumCsv`, `…Storage/SpectralImport.fs`",
      "spec_location": "§L.0",
      "evidence": "let parseSpectrumCsv (csvText : string) : Result<(WaveLength * float) list, SpectrumImportError> at OpticalConstructor.Storage/SpectralImport.fs:46 — pure, text-taking, Result-returning, matching the cited precedent.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 23,
      "phrase": "wiring the inert `EnvironmentSettings.lastFolders` (`…Ui/UserEnvironment.fs:184`, persisted but never read by any picker)",
      "spec_location": "§L.0",
      "evidence": "lastFolders : string list at exactly UserEnvironment.fs:184; EnvironmentSettings is persisted per the :176-180 doc. Repo-wide grep for lastFolders -> only the definition, the empty default (:240) and EnvironmentRoundTripTests (:55,:98); no picker reads it — the negative half of the claim holds.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 24,
      "phrase": "connection strings from `appsettings.json` via `AppSettingsProvider.tryGetConnectionString`",
      "spec_location": "§M.0",
      "evidence": "Inferred class-anchored claim (API assumed existing). Resolves in the external Softellect codebase the same sentence models on: type AppSettingsProvider at C:/GitHub/Softellect/Sys/AppSettings.fs:411 with member tryGetConnectionString at :439.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 25,
      "phrase": "modeled on the STL example (`C:/GitHub/Softellect/Apps/DistrProc/Migrations/Common/CommonDbContext.cs`)",
      "spec_location": "§M.0",
      "evidence": "File exists on disk at the cited absolute path (1,696 bytes, checked via ls).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 26,
      "phrase": "the EXISTING Domain seed values — `standardCategories` (`…Domain/MaterialLibrary.fs:118`)",
      "spec_location": "§M.0",
      "evidence": "let standardCategories : MaterialCategory list at exactly MaterialLibrary.fs:118.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 27,
      "phrase": "`builtInEntries` (`:363`)",
      "spec_location": "§M.0",
      "evidence": "let builtInEntries : MaterialEntry list at exactly MaterialLibrary.fs:363.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 28,
      "phrase": "`SeedSamples` (`…Domain/ElementId.fs:381`)",
      "spec_location": "§M.0",
      "evidence": "module SeedSamples at exactly ElementId.fs:381; the :376-380 doc confirms ids are FIXED literal Guids, consistent with 'seeded Guids are frozen forever'.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 29,
      "phrase": "`seedEntries` (`:532`)",
      "spec_location": "§M.0",
      "evidence": "let seedEntries : LibraryEntry list at exactly ElementId.fs:532.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 30,
      "phrase": "Today's implementation of the proxy is the in-memory stores, i.e. seeding runs with no database at all",
      "spec_location": "§M.0",
      "evidence": "Phrase inferred ('Today's implementation' is current-state phrasing; the SeedingProxy itself is new, the stores it wraps exist). createInMemory implementations exist in Domain (MaterialLibrary.fs, ElementId.fs) and are wired at the App composition root (Program.fs); 14 files reference createInMemory.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 31,
      "phrase": "Two parallel recent-file stores exist",
      "spec_location": "§N.0",
      "evidence": "Numeric-count claim, re-derived: grep recentFiles -> only EnvironmentSettings.recentFiles (Ui/UserEnvironment.fs:185, default :241, round-trip tests); the only other recent-file mechanism is the Storage/RecentFiles.fs module. Live count = 2, matches the cited count.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 32,
      "phrase": "`EnvironmentSettings.recentFiles` (`…Ui/UserEnvironment.fs:185`)",
      "spec_location": "§N.0",
      "evidence": "recentFiles : string list at exactly UserEnvironment.fs:185.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 33,
      "phrase": "the bounded-MRU module `…Storage/RecentFiles.fs` … (the Storage module, keeping its pure `bump` invariant)",
      "spec_location": "§N.0",
      "evidence": "OpticalConstructor.Storage/RecentFiles.fs exists; let bump (path : string) (existing : string list) : string list at :38 — pure MRU (case-insensitive dedupe, push front, truncate to maxRecent), kept separate from the IO per the :35-37 doc.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 34,
      "phrase": "replacing the per-control `UiIds` modules (id VALUES unchanged, so tests only re-point)",
      "spec_location": "§N.0",
      "evidence": "Class-anchored grep 'module UiIds' -> 21 definitions: 11 across OpticalConstructor.Controls (CategoryControls, MaterialsControls, RendererControls, ElementPaletteControls, RayPositionControls, LibraryControls, SampleLibraryControls, Ribbon, LayerBandsControls, RotationControls, ExperimentControls) and 10 across OpticalConstructor.TestWindows views — the per-control modules exist as the plural claims.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 35,
      "phrase": "The product launcher becomes `Main / Inverse / Materials / Library` (this order)",
      "spec_location": "§N.0 (also anchors §L.0 'The launcher's `Inverse`')",
      "evidence": "Inferred ('becomes' implies an existing launcher). LauncherWindow at OpticalConstructor.App/Program.fs:156 ('The simple launcher form (Spec 0027): Main opens the existing Optical Constructor window', :152), set as the startup window at :252.",
      "verdict": "CONFIRMED"
    }
  ]
}
