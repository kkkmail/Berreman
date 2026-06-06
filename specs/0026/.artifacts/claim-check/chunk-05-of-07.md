{
  "claims": [
    {
      "id": 1,
      "phrase": "secondary detectors MUST report their own branch independently (consistent with each branch carrying its own field, BeamTree.fs:98)",
      "spec_location": "§G.2.1",
      "evidence": "Read BeamTree.fs:94-101 -> let branchEmField at :98 plus comment :95-97 'Each branch carries its own field state, so detectors on different branches report independent results' -> matches 'each branch carrying its own field'",
      "verdict": "CONFIRMED"
    },
    {
      "id": 2,
      "phrase": "reusing the shared JSON options and the schema-validate-on-load seam (SchemaValidation.fs:61)",
      "spec_location": "§G.3.1",
      "evidence": "Read SchemaValidation.fs:58-73 -> let validate (element : JsonElement) at :61 is the validate-on-load seam (SchemaValidation module, validates BEFORE binding). 'shared JSON options' has no separate pointer but the seam pointer resolves",
      "verdict": "CONFIRMED"
    },
    {
      "id": 3,
      "phrase": "mirroring the environment store's location convention (UserEnvironment.fs:279, the `Softellect/Berreman/OpticalConstructor` app-data folder)",
      "spec_location": "§G.3.2",
      "evidence": "Read UserEnvironment.fs:276-284 -> let settingsPath () at :279 builds Path.Combine(ApplicationData, 'Softellect', 'Berreman', 'OpticalConstructor') -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 4,
      "phrase": "a `*.json` name does not collide with any root `.gitignore` pattern such as `*.cache`/`*.log`/`*.tmp`/`[Bb]in/`/`[Oo]bj/`",
      "spec_location": "§G.3.2",
      "evidence": "Grep root .gitignore -> [Bb]in/ (:23), [Oo]bj/ (:24), *.tmp (:78), *.log (:80) all present; cited `*.cache` is actually `*.cachefile` (:98) but no pattern matches *.json, so the substantive non-collision claim holds",
      "verdict": "CONFIRMED"
    },
    {
      "id": 5,
      "phrase": "Reuse the JSON options and validation seam used by the project and environment stores (SchemaValidation.fs:61; UserEnvironment.fs:185 defaults/load/save pattern) rather than a new serializer",
      "spec_location": "§G.4",
      "evidence": "Read SchemaValidation.fs:61 -> validate seam; Read UserEnvironment.fs:183-195 -> let defaults at :185, with load (:290) and save (:302) forming the defaults/load/save pattern -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 6,
      "phrase": "reuse the `FavoriteGroup` notion (UserEnvironment.fs:126) for pin/reuse",
      "spec_location": "§G.4",
      "evidence": "Read UserEnvironment.fs:125-130 -> type FavoriteGroup at :126 (name + pins) -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 7,
      "phrase": "mirroring how the schema content item is shipped, SchemaValidation.fs:17, and read from the runtime base directory",
      "spec_location": "§I.1.1",
      "evidence": "Read SchemaValidation.fs:14-21 -> let schemaRelativePath at :17 (build-copied schema content item, relative to assembly base directory, read via AppContext.BaseDirectory at :20-21) -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 8,
      "phrase": "`EnvironmentSettings` (UserEnvironment.fs:141) MUST be extended with a `language` field",
      "spec_location": "§I.2.1",
      "evidence": "Read UserEnvironment.fs:141-153 -> type EnvironmentSettings at :141 -> exists (no language field yet, consistent with 'MUST be extended')",
      "verdict": "CONFIRMED"
    },
    {
      "id": 9,
      "phrase": "MUST write through the existing save path (UserEnvironment.fs:302)",
      "spec_location": "§I.2.1",
      "evidence": "Read UserEnvironment.fs:299-310 -> let save (path) (settings) : Result<unit, StorageError> at :302 -> matches existing save path",
      "verdict": "CONFIRMED"
    },
    {
      "id": 10,
      "phrase": "Reuse the `EnvironmentSettings` store (UserEnvironment.fs:141/:302) for the saved language; reuse the build-copied-content-item + base-directory read pattern (SchemaValidation.fs:17)",
      "spec_location": "§I.5",
      "evidence": "Read UserEnvironment.fs:141 (type EnvironmentSettings) and :302 (let save); SchemaValidation.fs:17 (schemaRelativePath, build-copied content item) -> all resolve",
      "verdict": "CONFIRMED"
    },
    {
      "id": 11,
      "phrase": "Today CTA colours are duplicated per view (e.g. `positiveCta`/`negativeCta`, ConstructionView.fs:40)",
      "spec_location": "§J.0",
      "evidence": "Read ConstructionView.fs:38-41 -> let private positiveCta at :40 and negativeCta at :41 -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 12,
      "phrase": "The existing `positiveCta`/`negativeCta` (ConstructionView.fs:40) MUST be expressed as the destructive-gate flavour in `Controls.fs`",
      "spec_location": "§J.2",
      "evidence": "Read ConstructionView.fs:40-41 -> positiveCta/negativeCta exist at :40/:41 -> matches the existing pointer",
      "verdict": "CONFIRMED"
    },
    {
      "id": 13,
      "phrase": "The engine already provides multi-step whole-project snapshot undo/redo (History.fs:20, `EditHistory`; `:33` `push`; `:39` `undo`; `:47` `redo`)",
      "spec_location": "§K.0",
      "evidence": "Read History.fs in full -> type EditHistory at :20, let push at :33, let undo at :39, let redo at :47 -> all four pointers match",
      "verdict": "CONFIRMED"
    },
    {
      "id": 14,
      "phrase": "MUST `push` the new project onto the shared `EditHistory` (History.fs:33)",
      "spec_location": "§K.1.1",
      "evidence": "Read History.fs:31-34 -> let push (next) (h : EditHistory) : EditHistory at :33 -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 15,
      "phrase": "Undo and redo MUST be multi-level (History.fs is a multi-level zipper)",
      "spec_location": "§K.1.2",
      "evidence": "Read History.fs:5-14 -> module doc 'EditHistory is an immutable F# zipper over three project-snapshot lists ... This is the multi-level history' -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 16,
      "phrase": "distinct positive-CTA / negative-CTA colours (Part J.2 / ConstructionView.fs:40)",
      "spec_location": "§K.2.1",
      "evidence": "Read ConstructionView.fs:40-41 -> positiveCta/negativeCta brushes at :40/:41 -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 17,
      "phrase": "Reuse `History.fs` (History.fs:20) as the undo/redo carrier; do not add a command-pattern diff log or a parallel history",
      "spec_location": "§K.3",
      "evidence": "Read History.fs:20 -> type EditHistory (the undo/redo carrier) -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 18,
      "phrase": "its nav/shell entry is added to the existing `Page` union (Shell.fs:73)",
      "spec_location": "Operator-facing UX commitments §1 (Discoverability)",
      "evidence": "Read Shell.fs:72-76 -> type Page = Construction | SynthesisFit at :73 -> existing union resolves",
      "verdict": "CONFIRMED"
    },
    {
      "id": 19,
      "phrase": "marked active with distinct styling like the current `navButton` (Shell.fs:500)",
      "spec_location": "Operator-facing UX commitments §1 (Discoverability)",
      "evidence": "Read Shell.fs:500-506 -> let private navButton (dispatch) (label) (target) (active) at :500, marks isActive with SteelBlue background -> matches",
      "verdict": "CONFIRMED"
    },
    {
      "id": 20,
      "phrase": "The *Legacy* entry (D.5) opens the existing dockable screen.",
      "spec_location": "Operator-facing UX commitments §1 (Discoverability)",
      "evidence": "No <file>:<symbol> pointer in the sentence or adjacent bullet for 'the existing dockable screen'; only an internal §D.5 reference (out of this chunk). Un-anchored 'the existing <X>' phrase -> no concrete artefact to resolve",
      "verdict": "REFUTED-NO-POINTER"
    },
    {
      "id": 21,
      "phrase": "The per-node solve reuses the existing async per-node mechanism (BeamTree.fs:91) at per-element granularity",
      "spec_location": "Operator-facing UX commitments §2 (Progress granularity)",
      "evidence": "Read BeamTree.fs:86-92 -> let solve (node : BeamNode) : EmFieldSystem at :91 is the per-node solve mechanism (exists). Note: :91 is synchronous ('No parallel solver is invoked'); the 'async' descriptor is not reflected at that line, but the cited per-node artefact resolves",
      "verdict": "CONFIRMED"
    },
    {
      "id": 22,
      "phrase": "`.ocproj` user documents remain committable and are explicitly never git-ignored (ProjectFile.fs:14)",
      "spec_location": "Operator-facing UX commitments §6 (Committable-state under dot-folder)",
      "evidence": "Read ProjectFile.fs:12-17 -> comment at :14-15 '`.ocproj` files are user documents and are never added to `.gitignore`' above let extension = '.ocproj' (:17) -> matches",
      "verdict": "CONFIRMED"
    }
  ]
}
