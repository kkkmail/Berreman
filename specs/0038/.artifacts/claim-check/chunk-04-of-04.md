{
  "claims": [
    {
      "id": 1,
      "phrase": "Two parallel recent-file stores exist — `EnvironmentSettings.recentFiles` (`…Ui/UserEnvironment.fs:185`) and the bounded-MRU module `…Storage/RecentFiles.fs`",
      "spec_location": "§N.0 Problem statement (chunk 4/4, .spec-md lines 492-504)",
      "evidence": "Numeric-count claim (two stores), re-derived: Grep 'recentFiles|RecentFiles|recent\\.json' over Berreman/**/*.fs returns exactly 2 production files — Berreman/OpticalConstructor/OpticalConstructor.Ui/UserEnvironment.fs and Berreman/OpticalConstructor/OpticalConstructor.Storage/RecentFiles.fs (the only other 2 hits are test files HistoryTests.fs / EnvironmentRoundTripTests.fs, which are consumers, not stores). Live count = 2 matches the cited count.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 2,
      "phrase": "`EnvironmentSettings.recentFiles` (`…Ui/UserEnvironment.fs:185`)",
      "spec_location": "§N.0 Problem statement (chunk 4/4, .spec-md lines 492-504)",
      "evidence": "Read Berreman/OpticalConstructor/OpticalConstructor.Ui/UserEnvironment.fs:185 -> `recentFiles : string list` is exactly the field at line 185, inside `type EnvironmentSettings` declared at line 181. Pointer resolves precisely.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 3,
      "phrase": "the bounded-MRU module `…Storage/RecentFiles.fs` — ONE remains (the Storage module, keeping its pure `bump` invariant; the environment field is removed)",
      "spec_location": "§N.0 Problem statement (chunk 4/4, .spec-md lines 492-504)",
      "evidence": "Read Berreman/OpticalConstructor/OpticalConstructor.Storage/RecentFiles.fs -> `module RecentFiles` at :16, `[<Literal>] let maxRecent = 16` bound at :21-22, and pure `bump (path : string) (existing : string list) : string list` at :38 (case-insensitive dedup, push-to-front, List.truncate maxRecent — a bounded MRU whose invariant is IO-free, matching 'pure bump invariant').",
      "verdict": "CONFIRMED"
    },
    {
      "id": 4,
      "phrase": "replacing the per-control `UiIds` modules (id VALUES unchanged, so tests only re-point)",
      "spec_location": "§N.0 Problem statement (chunk 4/4, .spec-md lines 492-504)",
      "evidence": "Trigger 'unchanged' + definite-article 'the per-control UiIds modules'. Class-anchored: Grep 'module UiIds' -> 21 production matches (11 under OpticalConstructor.Controls: SampleLibraryControls.fs:88, RotationControls.fs:62, Ribbon.fs:52, RendererControls.fs:112, RayPositionControls.fs:41, ElementPaletteControls.fs:42, MaterialsControls.fs:101, CategoryControls.fs:71, LibraryControls.fs:77, LayerBandsControls.fs:43, ExperimentControls.fs:193; 10 under OpticalConstructor.TestWindows view files). The multiple matches ARE the claim's content (the spec asserts plural per-control modules), so multi-match confirms rather than ambiguates. Spot-check Ribbon.fs:51-57 shows [<RequireQualifiedAccess>] module UiIds with stable string id values; MaterialsControls.fs / SampleLibraryControls.fs / CategoryControls.fs carry [<Literal>] ids — the id values the spec says stay unchanged exist today.",
      "verdict": "CONFIRMED"
    }
  ]
}
