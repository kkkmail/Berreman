{
  "claims": [
    {
      "id": 1,
      "phrase": "Today there is no key‑binding or command infrastructure to reuse (only per‑button `onClick`); this part introduces the centralized command model both the ribbon (Part D) and the input handlers read from.",
      "spec_location": "§E.0 Problem statement",
      "evidence": "Inferred current-state assumption phrase (\"Today there is no ... to reuse\" / \"only per-button onClick\") not matched verbatim against the trigger list; treated as a 'this is the existing state' claim. Glob OpticalConstructor.Ui/Commands.fs -> no file. Grep KeyBinding|KeyGesture|KeyMap|keyBinding|HotKey|onKeyDown|KeyDown over Berreman/OpticalConstructor -> 0 hits (no existing command/keybinding infrastructure). Grep onClick over the same tree -> 22 occurrences across 8 view files (ChartView/FitView/ConstructionView/LifecycleView/MaterialsView/ResultsView/Shell/SourceView), confirming per-button onClick is the only existing reachability.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 2,
      "phrase": "the configured key map MUST persist with the environment settings (Part I / UserEnvironment.fs:141). Reuse that settings store rather than adding a parallel preferences file.",
      "spec_location": "§E.8.1",
      "evidence": "Read UserEnvironment.fs:141 -> matched `type EnvironmentSettings` (the persistent user-environment settings store carrying favorites/lastFolders/recentFiles/layout/theme/... fields). The settings store to reuse exists at the cited pointer.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 3,
      "phrase": "The domain already enumerates element kinds (BeamTree.fs:35).",
      "spec_location": "§F.0 Problem statement",
      "evidence": "Read BeamTree.fs:35 -> matched `type ConstructorElement` whose cases (Source/Polarizer/Sample/Lens/CurvedMirror/FlatMirror/Analyzer/Detector, lines 36-43) enumerate the element kinds. Enumeration exists at the cited pointer.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 4,
      "phrase": "Map catalogue kinds onto the existing `ConstructorElement` cases (BeamTree.fs:35) via the A.5.2 function",
      "spec_location": "§F.3 Reuse before invention",
      "evidence": "Read BeamTree.fs:35 -> `type ConstructorElement` with DU cases at lines 36-43. Existing ConstructorElement cases confirmed at the cited pointer.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 5,
      "phrase": "do not add new element DU cases for LP/CP (they are `Polarizer`)",
      "spec_location": "§F.3 Reuse before invention",
      "evidence": "Read BeamTree.fs:37 -> matched `| Polarizer` case within the `ConstructorElement` union (BeamTree.fs:35). The reuse-target case exists.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 6,
      "phrase": "do not remove `Analyzer` (BeamTree.fs:42)",
      "spec_location": "§F.3 Reuse before invention",
      "evidence": "Read BeamTree.fs:42 -> matched `| Analyzer` case within the `ConstructorElement` union. Cited file and line both match.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 7,
      "phrase": "building on the existing favourites concept (`FavoriteGroup`, UserEnvironment.fs:126) so a commonly used group can be pinned and dropped back in later.",
      "spec_location": "§G.1.3",
      "evidence": "Read UserEnvironment.fs:126 -> matched `type FavoriteGroup = { name : string; pins : FavoritePin list }`. Existing favourites type confirmed at the cited pointer.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 8,
      "phrase": "secondary detectors MUST report their own branch independently (consistent with each branch carrying its own field, BeamTree.fs:98).",
      "spec_location": "§G.2.1",
      "evidence": "Read BeamTree.fs:98 -> matched `let branchEmField (branch : BeamBranch) (ems : EmFieldSystem) : EmField`, returning `ems.reflected`/`ems.transmitted` per branch; its doc-comment (lines 94-97) states each branch carries its own field state so detectors on different branches report independent results. Cited file and line both match.",
      "verdict": "CONFIRMED"
    }
  ]
}
