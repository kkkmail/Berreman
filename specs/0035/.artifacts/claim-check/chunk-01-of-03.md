{
  "claims": [
    {
      "id": 1,
      "phrase": "The category change follows the existing elevated-id precedent `MaterialId` (`…Domain/MaterialLibrary.fs:27`)",
      "spec_location": "§0.1 Binding constraints",
      "evidence": "Read Domain/MaterialLibrary.fs:27 -> `type MaterialId =` (single-case DU `MaterialId of Guid` with `.value`, `create`, `tryCreate`). Cited symbol/line present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 2,
      "phrase": "the existing elevated-id precedent ... and `SampleId` (`…Domain/ElementId.fs:41`)",
      "spec_location": "§0.1 Binding constraints",
      "evidence": "Read Domain/ElementId.fs:41 -> `type SampleId =` (single-case DU `SampleId of Guid` with `.value`, `create`). Cited symbol/line present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 3,
      "phrase": "Categories change from the closed multi-case union `MaterialCategory` (`…Domain/MaterialLibrary.fs:67`) to an open `CategoryId`-keyed set",
      "spec_location": "§0.2 Binding constraints",
      "evidence": "[inferred existing-artefact claim; 'change from <existing union>' phrasing, no default trigger word] Read Domain/MaterialLibrary.fs:67 -> `type MaterialCategory =` closed union (68 Glass, 69 Metal, 70 Semiconductor, 71 Crystal, 72 Vacuum). Cited symbol/line present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 4,
      "phrase": "The `UnsupportedComplexity` view-only fallback for dispersive ρ/μ (`…Domain/MaterialComplexityEditor.fs:140,784,790`) ... are REMOVED, not paralleled",
      "spec_location": "§0.2 Binding constraints",
      "evidence": "[inferred existing-artefact claim; 'are REMOVED' presupposes the artefact exists] Read Domain/MaterialComplexityEditor.fs:140 -> `| UnsupportedComplexity of reason : string` (case decl); :784 -> `| Some (MuWithDispValue _) ->` dispersive-Polder-mu arm emitting `Error (UnsupportedComplexity ...)` at 785; :790 -> `| Some (RhoWithDispValue _) ->` dispersive-gyration arm emitting `Error (UnsupportedComplexity ...)` at 791. All three sites present (the cited lines head the two view-only fallback arms plus the case declaration).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 5,
      "phrase": "the `NotAFiniteTermSum` / `SegmentNotLowerable` transcendental-rejection path (`…Domain/DispersionModels.fs:505,666`; `MaterialComplexityEditor.fs:132,637`) are REMOVED, not paralleled",
      "spec_location": "§0.2 Binding constraints",
      "evidence": "[inferred existing-artefact claim; 'are REMOVED' presupposes the artefact exists] Read Domain/DispersionModels.fs:505 -> `| NotAFiniteTermSum of reason : string` (case decl); :666 -> `| TaucLorentz _ ->` first transcendental-rejection arm emitting `Error (NotAFiniteTermSum ...)` at 667 (GaussianOscillator/ForouhiBloomer/BrendelBormann follow at 668-673). Read Domain/MaterialComplexityEditor.fs:132 -> `| SegmentNotLowerable of reason : string` (case decl); :637 -> `| Error (NotAFiniteTermSum reason) -> Error (SegmentNotLowerable $\"segment %d{segmentIndex}: %s{reason}\")` (the lowering-rejection wiring). All four sites present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 6,
      "phrase": "built by a `createInMemory` closing over a private mutable `Map`, exactly like `SampleProxy.createInMemory` / `MaterialProxy.createInMemory` (`…Domain/ElementId.fs:616,686`)",
      "spec_location": "§0.3 Binding constraints",
      "evidence": "Read Domain/ElementId.fs:616 -> `static member createInMemory () : SampleProxy =` (closes over `ref (Map.ofList seeded)`); :686 -> `static member createInMemory (samplesReferencing : MaterialId -> Sample list) : MaterialProxy =` (closes over `ref (builtInEntries |> ... |> Map.ofList)`). Both cited members present at their lines.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 7,
      "phrase": "and the `LibraryProxy` shape (`…Domain/ElementId.fs:276`)",
      "spec_location": "§0.3 Binding constraints",
      "evidence": "Read Domain/ElementId.fs:276 -> `type LibraryProxy =` a `[<ReferenceEquality>]` record of `Result`-returning functions (`entriesForKind`, `libraryTrees`, `tryGetEntry`). Cited symbol/line present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 8,
      "phrase": "Every new interactive control carries a stable, intent-named `[<Literal>]` automation id in one `[<RequireQualifiedAccess>]` ids module, following `MaterialEditorView` / `SampleEditorView`",
      "spec_location": "§0.4 Binding constraints",
      "evidence": "[inferred existing-artefact claim; 'following <existing>' precedent phrasing] Class-anchored: Grep resolves TestWindows/MaterialEditorView.fs and TestWindows/SampleEditorView.fs (both files exist). Cited precedent views present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 9,
      "phrase": "the WebView2 reference chain the Ui project carries (`Microsoft.Web.WebView2`, `…Ui/OpticalConstructor.Ui.fsproj:176`)",
      "spec_location": "§0.6 Binding constraints",
      "evidence": "Read Ui/OpticalConstructor.Ui.fsproj:176 -> `<PackageReference Include=\"Microsoft.Web.WebView2\" Version=\"1.0.4078.44\" />`. Cited reference present at the line.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 10,
      "phrase": "used by the ChartHosts adapters, `:75,80`",
      "spec_location": "§0.6 Binding constraints",
      "evidence": "Read Ui/ChartHosts.fs:75-86 -> the WebView2 host adapter section: :75 `/// Attempt to host a Plotly chart by navigating a WebView2 to the chart's embedded HTML`; :79-80 names `the \\`Microsoft.Web.WebView2\\` package ships its control assemblies for `net462` / Windows-desktop TFMs only`. Cited neighborhood matches (note the adapter's own comment records it contributes no compile-time reference on net10.0 and degrades to the placeholder, but the WebView2-host adapter code region is exactly at the cited lines).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 11,
      "phrase": "Today a material's category is a closed compile-time union `MaterialCategory = Glass | Metal | Semiconductor | Crystal | Vacuum` (`…Domain/MaterialLibrary.fs:67`)",
      "spec_location": "§A.0 Problem statement",
      "evidence": "Read Domain/MaterialLibrary.fs:67 -> `type MaterialCategory =`; cases at 68-72 are exactly Glass | Metal | Semiconductor | Crystal | Vacuum. Cited symbol/line and enumerated cases match.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 12,
      "phrase": "stored directly on `MaterialEntry.category` (`:119`)",
      "spec_location": "§A.0 Problem statement",
      "evidence": "Read Domain/MaterialLibrary.fs:119 -> `category : MaterialCategory` (field of the `MaterialEntry` record, type opened at 115). Cited field/line present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 13,
      "phrase": "and `MaterialQuery.category` (`:368`)",
      "spec_location": "§A.0 Problem statement",
      "evidence": "Read Domain/MaterialLibrary.fs:368 -> `category : MaterialCategory option` (field of the `MaterialQuery` record, type opened at 365). Cited field/line present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 14,
      "phrase": "filtered by `byCategory` (`:146`)",
      "spec_location": "§A.0 Problem statement",
      "evidence": "Read Domain/MaterialLibrary.fs:146 -> `let byCategory (category : MaterialCategory) (lib : MaterialLibrary) : MaterialEntry list =`. Cited symbol/line present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 15,
      "phrase": "and `byQuery` (`:417`)",
      "spec_location": "§A.0 Problem statement",
      "evidence": "Read Domain/MaterialLibrary.fs -> `let byQuery (q : MaterialQuery) ...` is declared at 415; the cited line 417 is `let byCat =`, byQuery's own category-filtering body (417-420 applies `byCategory` to the query's `category` facet). The cited line falls inside the named function and contains exactly the 'filtered by byQuery' content, so the pointer resolves (2-line offset from the `let byQuery` header, well within the same function span 415-424).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 16,
      "phrase": "and hardcoded again in the material-editor create picker (`…TestWindows/MaterialEditorView.fs:551,561`)",
      "spec_location": "§A.0 Problem statement",
      "evidence": "Read TestWindows/MaterialEditorView.fs:551 -> `let private categoryRow (m : Model) (dispatch : Msg -> unit) : IView =`; :561 -> `[ Glass; Metal; Semiconductor; Crystal; Vacuum ]` (the hardcoded category list mapped to clickBoxes). Both cited sites present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 17,
      "phrase": "and the Materials-bay category facet (`…TestWindows/TableAndElementRotationView.fs:1910,1915,1924,1934,2002`)",
      "spec_location": "§A.0 Problem statement",
      "evidence": "Read TestWindows/TableAndElementRotationView.fs:1910 -> `let materialCategories : MaterialLibrary.MaterialCategory list`; :1915 -> `let materialCategoryCode ...`; :1924 -> `let private materialCategoryLabel ...`; :1934 -> `let materialCategoryOfCode ...`; :2002 -> `categoryOptions =` (the Materials-bay view-model builder that maps `materialCategories` to facet options). All five cited sites present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 18,
      "phrase": "A real `BuiltInCategory` category the single seeded vacuum material (`…Domain/MaterialLibrary.fs:314`) references",
      "spec_location": "§A.0 Recorded decisions (Vacuum)",
      "evidence": "Read Domain/MaterialLibrary.fs:314 -> `id = MaterialIds.vacuum` inside the vacuum `MaterialEntry` (block 313-320: name \"Vacuum\", category Vacuum, `properties = OpticalProperties.vacuum.dispersive`). Cited seeded vacuum material present at the line.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 19,
      "phrase": "mirroring `MaterialProxy.removeMaterial`'s `MaterialStillReferenced` hard-block (`…Domain/MaterialLibrary.fs:135`; assembled in `ElementId.fs:715`)",
      "spec_location": "§A.0 Recorded decisions (Removal never cascades)",
      "evidence": "[inferred existing-artefact claim; 'mirroring <existing>' precedent phrasing] Read Domain/MaterialLibrary.fs:135 -> `| MaterialStillReferenced of reason : string` (MaterialError case); Read Domain/ElementId.fs:715 -> `removeMaterial =` whose body (715-730) consults `samplesReferencing` and returns `Error (MaterialStillReferenced ...)` naming the referencing samples. Both cited sites present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 20,
      "phrase": "The referencing lookup is a `materialsReferencingCategory` seam over the live materials store, exactly as `samplesReferencing` feeds material removal today (`…Domain/ElementId.fs:740`)",
      "spec_location": "§A.0 Recorded decisions (Removal never cascades)",
      "evidence": "['as today' + 'the existing seam' family] Read Domain/ElementId.fs:740 -> `let samplesReferencing (samples : SampleProxy) (id : MaterialId) : Sample list =` (filters the live `listSamples` for structures referencing the material). Cited seam present at the line; `materialsReferencingCategory` is the NEW analogue this cycle mints (not audited here).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 21,
      "phrase": "The current five categories seed with FIXED literal Guids (deterministic tests)",
      "spec_location": "§A.1 Seeded catalogue and re-pointing",
      "evidence": "Numeric-count claim ('current five categories'). Re-derived live count from Domain/MaterialLibrary.fs:67-72 -> `MaterialCategory` has exactly 5 cases (Glass, Metal, Semiconductor, Crystal, Vacuum). Cited count matches live count.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 22,
      "phrase": "mirroring `MaterialLibrary.MaterialIds` (`…Domain/MaterialLibrary.fs:51`)",
      "spec_location": "§A.1 Seeded catalogue and re-pointing",
      "evidence": "[inferred existing-artefact claim; 'mirroring <existing>' precedent phrasing] Read Domain/MaterialLibrary.fs:51 -> `module MaterialIds =` holding FIXED literal Guids parsed once (silicon..activeCrystal, lines 52-63). Cited module/line present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 23,
      "phrase": "Re-seed every `MaterialEntry.category` (`…Domain/MaterialLibrary.fs:252` onward) to the matching seeded `CategoryId`",
      "spec_location": "§A.1 Seeded catalogue and re-pointing",
      "evidence": "Read Domain/MaterialLibrary.fs:252 -> `category = Semiconductor` (the first seeded entry's — Silicon's — category assignment). The `builtInEntries` list (247-345) sets `category = ...` on every entry from line 252 onward. Cited '252 onward' anchor present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 24,
      "phrase": "The create picker (`MaterialEditorView.categoryRow`) offers only `SelectableOnCreate` categories (Vacuum removed — not greyed)",
      "spec_location": "§A.1 Seeded catalogue and re-pointing",
      "evidence": "Symbol-anchored: Read TestWindows/MaterialEditorView.fs:551 -> `let private categoryRow (m : Model) (dispatch : Msg -> unit) : IView =`. Cited symbol present (the 'offers only SelectableOnCreate' behaviour is the NEW change, not audited here).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 25,
      "phrase": "the Materials-bay facet (`TableAndElementRotationView.materialCategories` / `…Code` / `…Label` / `…OfCode`, `:1910`) is driven by the catalogue",
      "spec_location": "§A.1 Seeded catalogue and re-pointing",
      "evidence": "Symbol-anchored: Read TestWindows/TableAndElementRotationView.fs -> `materialCategories` (1910), `materialCategoryCode` (1915), `materialCategoryLabel` (1924), `materialCategoryOfCode` (1934) all present; cited `:1910` anchors the cluster. Named symbols resolve.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 26,
      "phrase": "`<UICOMP:CategoryControls>` is the domain-free manager control (the `LibraryControls` shape) over a pure `CategoryEditor` Domain edit model",
      "spec_location": "§A.2 The mutating category proxy and the manager surface",
      "evidence": "Class-anchored precedent reference: Grep resolves Controls/LibraryControls.fs (the `LibraryControls` domain-free manager control) — exists. `<UICOMP:CategoryControls>` and `CategoryEditor` are NEW ids minted by this cycle (contract table), not audited here.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 27,
      "phrase": "`<UICOMP:CategoryEditorWindow>` hosts it in `OpticalConstructor.TestWindows` beside the existing editor windows",
      "spec_location": "§A.2 The mutating category proxy and the manager surface",
      "evidence": "[existing-artefact claim: 'the existing editor windows'] Class-anchored: OpticalConstructor.TestWindows project exists and contains editor windows including MaterialEditorWindow.fs and SampleEditorWindow.fs (Glob). The cited host project and its existing editor windows resolve.",
      "verdict": "CONFIRMED"
    }
  ]
}
