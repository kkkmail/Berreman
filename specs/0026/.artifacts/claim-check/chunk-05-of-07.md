{
  "claims": [
    {
      "id": 1,
      "phrase": "secondary detectors MUST report their own branch independently (consistent with each branch carrying its own field, BeamTree.fs:98)",
      "spec_location": "§G.2.1",
      "evidence": "Read BeamTree.fs:94-101 -> `let branchEmField (branch : BeamBranch) (ems : EmFieldSystem) : EmField` at :98; doc comment :94-97 'Each branch carries its own field state, so detectors on different branches report independent results' -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 2,
      "phrase": "reusing the shared JSON options and the schema-validate-on-load seam (SchemaValidation.fs:61)",
      "spec_location": "§G.3.1",
      "evidence": "Read SchemaValidation.fs:58-68 -> `let validate (element : JsonElement) : Result<unit, StorageError>` at :61, the validate-before-bind seam -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 3,
      "phrase": "mirroring the environment store's location convention (UserEnvironment.fs:279, the `Softellect/Berreman/OpticalConstructor` app-data folder)",
      "spec_location": "§G.3.2",
      "evidence": "Read UserEnvironment.fs:276-284 -> `let settingsPath ()` at :279 builds Path.Combine(ApplicationData, 'Softellect', 'Berreman', 'OpticalConstructor') -> matches the cited folder convention",
      "verdict": "CONFIRMED"
    },
    {
      "id": 4,
      "phrase": "a `*.json` name does not collide with any root `.gitignore` pattern such as `*.cache`/`*.log`/`*.tmp`/`[Bb]in/`/`[Oo]bj/`",
      "spec_location": "§G.3.2",
      "evidence": "Read root .gitignore -> cited patterns present (`*.[Cc]ache` :209 covers *.cache, `*.log` :80, `*.tmp` :78, `[Bb]in/` :23, `[Oo]bj/` :24); no `*.json` ignore pattern exists, so a `*.json` name does not collide. Inferred assumption-shape claim about the existing .gitignore",
      "verdict": "CONFIRMED"
    },
    {
      "id": 5,
      "phrase": "Reuse the JSON options and validation seam used by the project and environment stores (SchemaValidation.fs:61; UserEnvironment.fs:185 defaults/load/save pattern) rather than a new serializer",
      "spec_location": "§G.4",
      "evidence": "Read SchemaValidation.fs:61 -> `let validate`; Read UserEnvironment.fs:183-194 -> `let defaults : EnvironmentSettings` at :185, with load (:290) and save (:302) forming the defaults/load/save pattern -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 6,
      "phrase": "reuse the `FavoriteGroup` notion (UserEnvironment.fs:126) for pin/reuse",
      "spec_location": "§G.4",
      "evidence": "Read UserEnvironment.fs:125-130 -> `type FavoriteGroup = { name : string; pins : FavoritePin list }` at :126 -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 7,
      "phrase": "mirroring how the schema content item is shipped, SchemaValidation.fs:17, and read from the runtime base directory",
      "spec_location": "§I.1.1",
      "evidence": "Read SchemaValidation.fs:14-21 -> `let schemaRelativePath` at :17 (build-copied schema content item, relative to assembly base directory); :20-21 reads it via AppContext.BaseDirectory -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 8,
      "phrase": "`EnvironmentSettings` (UserEnvironment.fs:141) MUST be extended with a `language` field, defaulting to English",
      "spec_location": "§I.2.1",
      "evidence": "Read UserEnvironment.fs:141-153 -> `type EnvironmentSettings = { ... }` at :141 (no language field yet, consistent with 'MUST be extended') -> existing record resolves",
      "verdict": "CONFIRMED"
    },
    {
      "id": 9,
      "phrase": "the environment schema (`Berreman/OpticalConstructor/OpticalConstructor.Ui/optical-constructor-environment.schema.json`) MUST be extended to match",
      "spec_location": "§I.2.1",
      "evidence": "Glob -> Berreman/OpticalConstructor/OpticalConstructor.Ui/optical-constructor-environment.schema.json exists -> the schema to be extended resolves",
      "verdict": "CONFIRMED"
    },
    {
      "id": 10,
      "phrase": "MUST write through the existing save path (UserEnvironment.fs:302)",
      "spec_location": "§I.2.1",
      "evidence": "Read UserEnvironment.fs:299-310 -> `let save (path : string) (settings : EnvironmentSettings) : Result<unit, StorageError>` at :302 -> matches the existing save path",
      "verdict": "CONFIRMED"
    },
    {
      "id": 11,
      "phrase": "The startup check and the copyable error surface MUST be wired in the app entry point (`Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`)",
      "spec_location": "§I.3.1",
      "evidence": "Glob -> Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs exists -> the cited app entry point resolves",
      "verdict": "CONFIRMED"
    },
    {
      "id": 12,
      "phrase": "Reuse the `EnvironmentSettings` store (UserEnvironment.fs:141/:302) for the saved language",
      "spec_location": "§I.5",
      "evidence": "Read UserEnvironment.fs:141 -> `type EnvironmentSettings`; :302 -> `let save`. Both pointers resolve",
      "verdict": "CONFIRMED"
    },
    {
      "id": 13,
      "phrase": "reuse the build-copied-content-item + base-directory read pattern (SchemaValidation.fs:17) for shipping and reading `strings.json`",
      "spec_location": "§I.5",
      "evidence": "Read SchemaValidation.fs:17 -> `let schemaRelativePath` (build-copied content item); :20-21 reads from AppContext.BaseDirectory -> the build-copied + base-directory pattern resolves",
      "verdict": "CONFIRMED"
    },
    {
      "id": 14,
      "phrase": "Today CTA colours are duplicated per view (e.g. `positiveCta`/`negativeCta`, ConstructionView.fs:40)",
      "spec_location": "§J.0",
      "evidence": "Read ConstructionView.fs:38-41 -> `let private positiveCta : IBrush` at :40, `let private negativeCta` at :41 -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 15,
      "phrase": "The existing `positiveCta`/`negativeCta` (ConstructionView.fs:40) MUST be expressed as the destructive-gate flavour in `Controls.fs`",
      "spec_location": "§J.2",
      "evidence": "Read ConstructionView.fs:40-41 -> positiveCta/negativeCta brush definitions both present -> the existing pointer resolves",
      "verdict": "CONFIRMED"
    },
    {
      "id": 16,
      "phrase": "The engine already provides multi-step whole-project snapshot undo/redo (History.fs:20, `EditHistory`; `:33` `push`; `:39` `undo`; `:47` `redo`)",
      "spec_location": "§K.0",
      "evidence": "Read History.fs:14-50 -> `type EditHistory` at :20, `let push` at :33, `let undo` at :39, `let redo` at :47 -> all four pointers match",
      "verdict": "CONFIRMED"
    },
    {
      "id": 17,
      "phrase": "MUST `push` the new project onto the shared `EditHistory` (History.fs:33)",
      "spec_location": "§K.1.1",
      "evidence": "Read History.fs:31-34 -> `let push (next : OpticalConstructorProject) (h : EditHistory) : EditHistory` at :33 -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 18,
      "phrase": "Undo and redo MUST be multi-level (History.fs is a multi-level zipper); the constructor MUST NOT degrade it to single-level",
      "spec_location": "§K.1.2",
      "evidence": "Read History.fs:17-50 -> EditHistory record carries `past`/`present`/`future` project-snapshot lists (a zipper); undo (:39) and redo (:47) step the lists -> multi-level zipper confirmed",
      "verdict": "CONFIRMED"
    },
    {
      "id": 19,
      "phrase": "distinct positive-CTA / negative-CTA colours (Part J.2 / ConstructionView.fs:40)",
      "spec_location": "§K.2.1",
      "evidence": "Read ConstructionView.fs:40-41 -> positiveCta (SeaGreen) / negativeCta (IndianRed) brushes at :40/:41 -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 20,
      "phrase": "Reuse `History.fs` (History.fs:20) as the undo/redo carrier; do not add a command-pattern diff log or a parallel history",
      "spec_location": "§K.3",
      "evidence": "Read History.fs:20 -> `type EditHistory` (the undo/redo carrier) -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 21,
      "phrase": "its nav/shell entry is added to the existing `Page` union (Shell.fs:73)",
      "spec_location": "Operator-facing UX commitments §1 (Discoverability)",
      "evidence": "Read Shell.fs:72-76 -> `type Page = | Construction | SynthesisFit` at :73 -> existing union resolves",
      "verdict": "CONFIRMED"
    },
    {
      "id": 22,
      "phrase": "marked active with distinct styling like the current `navButton` (Shell.fs:500)",
      "spec_location": "Operator-facing UX commitments §1 (Discoverability)",
      "evidence": "Read Shell.fs:500-506 -> `let private navButton (dispatch : RootMsg -> unit) (label : string) (target : Page) (active : Page) : IView` at :500, sets SteelBlue background when active -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 23,
      "phrase": "The *Legacy* entry (D.5) opens the present dockable-panel main screen (`constructionBody`, Shell.fs:572)",
      "spec_location": "Operator-facing UX commitments §1 (Discoverability)",
      "evidence": "Read Shell.fs:569-577 -> `let private constructionBody (model : RootModel) (dispatch : RootMsg -> unit) : IView` at :572, the dockable-panel page body -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 24,
      "phrase": "The per-node solve reuses the existing async per-node mechanism (BeamTree.fs:91) at per-element granularity",
      "spec_location": "Operator-facing UX commitments §2 (Progress granularity)",
      "evidence": "Read BeamTree.fs:86-92 -> `let solve (node : BeamNode) : EmFieldSystem` at :91, the per-node solve via the EXISTING engine solver (:86). The per-node artefact resolves; 'async' is a semantic descriptor not reflected at the cited line but out of scope for pointer resolution",
      "verdict": "CONFIRMED"
    },
    {
      "id": 25,
      "phrase": "`.ocproj` user documents remain committable and are explicitly never git-ignored (ProjectFile.fs:14)",
      "spec_location": "Operator-facing UX commitments §6 (Committable-state under dot-folder)",
      "evidence": "Read ProjectFile.fs:12-17 -> comment at :14-15 '`.ocproj` files are user documents and are never added to `.gitignore`' above `let extension = \".ocproj\"` (:17); root .gitignore has no `.ocproj` pattern -> matches",
      "verdict": "CONFIRMED"
    }
  ]
}
