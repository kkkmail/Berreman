namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Media
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Domain.Facets
open OpticalConstructor.Domain.FacetBuckets
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.DispersionModels
open OpticalConstructor.Domain.MaterialComplexityEditor
open OpticalConstructor.Domain.LibraryFacets

/// Spec 0038 Part D (step 011) — the two concrete facet catalogues, pinned
/// over the SEEDED corpora (`builtInEntries`, `SeedSamples`/`seedEntries`)
/// plus synthetic entries for what the seeds cannot reach (a data-dispersive
/// multi-model material, magnetic materials, a wedge sample, a mixed
/// transparent+absorbing film, and a small film corpus for the bucket flow).
/// Pure construction throughout: no window, no IO, no solver run.
module LibraryFacetsTests =

    // ---- shared accessors ----

    let private materialDef (key : AttributeKey) : AttributeDef<MaterialEntry> =
        materialFacets |> List.find (fun d -> d.key = key)

    let private libraryDefs : AttributeDef<LibraryEntry> list = libraryFacets builtInEntries

    let private libraryDef (key : AttributeKey) : AttributeDef<LibraryEntry> =
        libraryDefs |> List.find (fun d -> d.key = key)

    let private entryOf (id : MaterialId) : MaterialEntry =
        builtInEntries |> List.find (fun e -> e.id = id)

    let private siliconEntry = entryOf MaterialIds.silicon
    let private langasiteEntry = entryOf MaterialIds.langasite
    let private vacuumEntry = entryOf MaterialIds.vacuum
    let private glass152Entry = entryOf MaterialIds.glass152
    let private glass150Entry = entryOf MaterialIds.glass150
    let private euvMolybdenumEntry = entryOf MaterialIds.euvMolybdenum
    let private euvSiliconEntry = entryOf MaterialIds.euvSilicon
    let private uniaxialEntry = entryOf MaterialIds.uniaxialCrystal
    let private biaxialEntry = entryOf MaterialIds.biaxialCrystal
    let private activeEntry = entryOf MaterialIds.activeCrystal

    let private applies (def : AttributeDef<'Item>) (item : 'Item) : bool =
        match def.appliesTo item with
        | ApplicableAttribute -> true
        | InapplicableAttribute -> false

    let private extractedKeys (def : AttributeDef<'Item>) (item : 'Item) : string list =
        def.extract item
        |> List.map (fun value ->
            match value with
            | DiscreteValue k -> k.value
            | NumericValue v -> $"%g{v}")

    let private numericValues (def : AttributeDef<'Item>) (item : 'Item) : double list =
        def.extract item
        |> List.choose (fun value ->
            match value with
            | NumericValue v -> Some v
            | DiscreteValue _ -> None)

    let private discrete (key : AttributeKey) (values : string list) : AppliedConstraint =
        { key = key; selection = DiscreteSelection (values |> List.map DiscreteKey |> Set.ofList) }

    let private branchSummary (node : FacetNode) : (string * int) list =
        node.branches |> List.map (fun b -> b.label, b.count.value)

    let private materialRepresentation : Representation =
        Representation (materialFacets |> List.map (fun d -> d.key))

    let private libraryRepresentation : Representation =
        Representation (libraryDefs |> List.map (fun d -> d.key))

    // ---- synthetic material entries (what the seeds cannot reach) ----

    let private materialEntryWith (idText : string) (name : string) (complexity : MaterialComplexity) : MaterialEntry =
        {
            id = Guid.Parse idText |> MaterialId
            name = name
            category = CategoryIds.crystal
            description = None
            properties = complexity.toProperties
            complexity = Some complexity
        }

    let private modelOfKind (kindCode : string) : DispersionModel =
        defaultModelChoices |> List.find (fun m -> modelKindCode m = kindCode)

    let private nmInterval (lowerNm : float) (upperNm : float) : WaveLengthInterval =
        { lower = toWaveLength Nanometer lowerNm; upper = toWaveLength Nanometer upperNm }

    /// A uniaxial two-segment dispersive entry whose axis models lower to all
    /// three storable shapes: Cauchy → real n/k terms, Sellmeier → complex ε
    /// terms, Tauc–Lorentz → the evaluated closure. ONE item, THREE distinct
    /// dispersion-model values — the multi-valued proof.
    let private syntheticDispersive : MaterialEntry =
        let seg1 : UniaxialEpsSegment =
            {
                wavelengthInterval = nmInterval 400.0 700.0
                ordinaryDispersion = toEpsAxis (modelOfKind "Cauchy")
                extraordinaryDispersion = toEpsAxis (modelOfKind "Sellmeier")
            }
        let seg2 : UniaxialEpsSegment =
            {
                wavelengthInterval = nmInterval 700.0 1000.0
                ordinaryDispersion = toEpsAxis (modelOfKind "TaucLorentz")
                extraordinaryDispersion = toEpsAxis (modelOfKind "TaucLorentz")
            }
        materialEntryWith
            "6b0e6b9e-1a2f-4c3d-8e5a-9f0b1c2d3e4f"
            "Synthetic uniaxial dispersive"
            {
                eps = EpsWithDispValue (UniaxialDispersive [ seg1; seg2 ])
                magnetic = None
                active = None
            }

    let private constantIsoEps : EpsWithDispValue =
        EpsWithoutDispValue (IsotropicTransparent (RefractionIndex 1.5))

    let private scalarMagneticEntry : MaterialEntry =
        materialEntryWith
            "7c1f7caf-2b30-4d4e-9f6b-a01c2d3e4f50"
            "Scalar magnetic"
            {
                eps = constantIsoEps
                magnetic = Some (MuWithoutDispValue (ScalarMu (MuValue 1.1)))
                active = None
            }

    let private polderMagneticEntry : MaterialEntry =
        materialEntryWith
            "8d208db0-3c41-4e5f-a07c-b12d3e4f5061"
            "Gyromagnetic"
            {
                eps = constantIsoEps
                magnetic =
                    Some (MuWithoutDispValue (GyromagneticMu
                        {
                            muDiagonal = MuValue 1.0
                            muParallel = MuValue 1.05
                            gyration = MuValue 0.1
                            axis = GyrationAxis.defaultValue
                        }))
                active = None
            }

    let private dispersivePolderEntry : MaterialEntry =
        materialEntryWith
            "9e319ec1-4d52-4f60-b18d-c23e4f506172"
            "Dispersive Polder"
            {
                eps = constantIsoEps
                magnetic = Some (MuWithDispValue defaultPolderDispersion)
                active = None
            }

    // ---- synthetic samples ----

    // The layer pins version one of the material (spec 0038 step 022 — a `SampleLayer` carries a
    // `MaterialVersionId`; the facet extractors project it back to the identity).
    let private layer (materialId : MaterialId) (thicknessNm : float) : SampleLayer =
        { materialId = MaterialVersionId.firstOf materialId; thickness = Thickness.nm (thicknessNm * 1.0<nm>); orientation = PrimaryAxes }

    let private filmSample (idText : string) (name : string) (films : StackItem list) : Sample =
        {
            id = Guid.Parse idText |> SampleId
            name = name
            structure = { films = films; substrate = None; lower = None }
            substrate = ThinFilm
            description = name
        }

    /// Glass + EUV-molybdenum films: one Transparent and one Absorbing
    /// constituent — the lifted transparency's distinct-union probe.
    let private mixedFilmSample : Sample =
        filmSample
            "af42afd2-5e63-4071-829e-d34f50617283"
            "Mixed transparency film"
            [
                SingleLayer (layer MaterialIds.glass152 100.0)
                SingleLayer (layer MaterialIds.euvMolybdenum 5.0)
            ]

    /// One Repeated period of a 12 nm / 30 nm cell, five periods: the
    /// per-film thickness facet reads PER-LAYER values (never the 42 nm
    /// period or the 210 nm total), the cell counted once.
    let private repeatedFilmSample : Sample =
        {
            id = Guid.Parse "d275d205-8196-43a4-85d1-067283940516" |> SampleId
            name = "Repeated film"
            structure =
                {
                    films =
                        [
                            Repeated
                                {
                                    cell = [ layer MaterialIds.glass152 12.0; layer MaterialIds.glass152 30.0 ]
                                    count = 5
                                }
                        ]
                    substrate = None
                    lower = None
                }
            substrate = ThinFilm
            description = "Repeated film"
        }

    let private polarizerEntry : LibraryEntry =
        seedEntries
        |> List.pick (fun e ->
            match e with
            | PolarizerItem _ -> Some e
            | SampleItem _ | SourceItem _ | DetectorItem _ -> None)

    /// The bucket-flow corpus: off-rung thicknesses (12 / 30 / 700 nm — never
    /// exactly on a 1-2-5 rung, so the meter round-trip cannot flip a bucket)
    /// plus a non-sample entry the applicability gate must exclude.
    let private bucketCorpus : LibraryEntry list =
        [
            SampleItem (filmSample "b053b0e3-6f74-4182-a3af-e45061728394" "Film A" [ SingleLayer (layer MaterialIds.glass152 12.0) ])
            SampleItem (filmSample "c164c1f4-7085-4293-b4c0-f56172839405" "Film B" [ SingleLayer (layer MaterialIds.glass152 30.0); SingleLayer (layer MaterialIds.glass152 700.0) ])
            SampleItem repeatedFilmSample
            polarizerEntry
        ]

    // =======================================================================
    // Material facets — category, anisotropy, dispersion, transparency.
    // =======================================================================

    [<Fact>]
    let ``the category facet applies to every entry and keys the seeded catalogue names`` () =
        let def = materialDef materialCategoryKey
        for entry in builtInEntries do
            Assert.True(applies def entry)
        Assert.Equal<string list>([ "Semiconductor" ], extractedKeys def siliconEntry)
        Assert.Equal<string list>([ "Vacuum" ], extractedKeys def vacuumEntry)
        Assert.Equal<string list>([ "Crystal" ], extractedKeys def activeEntry)

    [<Fact>]
    let ``the category branches are corpus-derived from the seeded catalogue`` () =
        let tree = buildTree (Representation [ materialCategoryKey ]) materialFacets [] builtInEntries
        let node = List.exactlyOne tree.facets
        Assert.Equal<(string * int) list>(
            [ "Crystal", 4; "Glass", 4; "Metal", 1; "Semiconductor", 2; "Vacuum", 1 ],
            branchSummary node)

    [<Fact>]
    let ``the anisotropy facet is inapplicable to the coded presets`` () =
        let def = materialDef materialAnisotropyKey
        Assert.False(applies def siliconEntry)
        Assert.False(applies def langasiteEntry)
        Assert.False(applies def vacuumEntry)
        Assert.True(applies def glass152Entry)
        Assert.True(applies def euvMolybdenumEntry)
        Assert.True(applies def activeEntry)

    [<Fact>]
    let ``the anisotropy facet classifies the encoded eps value-tree shape`` () =
        let def = materialDef materialAnisotropyKey
        Assert.Equal<string list>([ "Isotropic" ], extractedKeys def glass152Entry)
        Assert.Equal<string list>([ "Isotropic" ], extractedKeys def euvSiliconEntry)
        Assert.Equal<string list>([ "Biaxial" ], extractedKeys def biaxialEntry)
        // The seeded "Uniaxial crystal" is ENCODED as per-axis BiaxialTransparent
        // values (MaterialLibrary seed comment: its (o, e, e) diagonal has no
        // engine uniaxial shape), so the facet classifies the encoding.
        Assert.Equal<string list>([ "Biaxial" ], extractedKeys def uniaxialEntry)
        // The Uniaxial vocabulary key exists even though no seed extracts it.
        Assert.Equal("Uniaxial", (anisotropyKey Uniaxial).value)

    [<Fact>]
    let ``the constant-vs-dispersive facet applies to every entry including coded presets`` () =
        let def = materialDef materialDispersionKey
        for entry in builtInEntries do
            Assert.True(applies def entry)
        Assert.Equal<string list>([ "Dispersive" ], extractedKeys def siliconEntry)
        Assert.Equal<string list>([ "Dispersive" ], extractedKeys def langasiteEntry)
        Assert.Equal<string list>([ "Constant" ], extractedKeys def vacuumEntry)
        Assert.Equal<string list>([ "Constant" ], extractedKeys def glass150Entry)

    [<Fact>]
    let ``the transparency facet is offered for constant data-carrying materials only`` () =
        let def = materialDef materialTransparencyKey
        Assert.True(applies def glass152Entry)
        Assert.True(applies def euvMolybdenumEntry)
        // Dispersive → never offered (operator, 003/Q9).
        Assert.False(applies def siliconEntry)
        // Constant but CODED (no value tree to classify): the vacuum spacer.
        Assert.False(applies def vacuumEntry)

    [<Fact>]
    let ``the transparency facet classifies transparent vs absorbing constant eps`` () =
        let def = materialDef materialTransparencyKey
        Assert.Equal<string list>([ "Transparent" ], extractedKeys def glass152Entry)
        Assert.Equal<string list>([ "Transparent" ], extractedKeys def activeEntry)
        Assert.Equal<string list>([ "Absorbing" ], extractedKeys def euvMolybdenumEntry)

    // =======================================================================
    // Dependent facets vanish; the seeded corpus tree.
    // =======================================================================

    [<Fact>]
    let ``dependent facets vanish entirely over a population they never apply to`` () =
        // Silicon + langasite: coded presets — no value trees at all. Only the
        // category and constant-vs-dispersive facets survive; anisotropy,
        // transparency, dispersion model, activity, handedness and magnetic
        // all vanish (no greyed/empty nodes exist).
        let tree = buildTree materialRepresentation materialFacets [] [ siliconEntry; langasiteEntry ]
        Assert.Equal<AttributeKey list>(
            [ materialCategoryKey; materialDispersionKey ],
            tree.facets |> List.map (fun f -> f.key))

    [<Fact>]
    let ``the seeded corpus offers activity facets but no magnetic and no dispersion-model facet`` () =
        let tree = buildTree materialRepresentation materialFacets [] builtInEntries
        Assert.Equal<AttributeKey list>(
            [
                materialCategoryKey
                materialAnisotropyKey
                materialDispersionKey
                materialTransparencyKey
                materialGyrationClassKey
                materialHandednessKey
            ],
            tree.facets |> List.map (fun f -> f.key))
        let gyrationNode = tree.facets |> List.find (fun f -> f.key = materialGyrationClassKey)
        Assert.Equal<(string * int) list>([ "Planar mm2 (g₁₂)", 1 ], branchSummary gyrationNode)
        let handNode = tree.facets |> List.find (fun f -> f.key = materialHandednessKey)
        Assert.Equal<(string * int) list>([ "Right-handed", 1 ], branchSummary handNode)

    // =======================================================================
    // Optical activity and magnetic.
    // =======================================================================

    [<Fact>]
    let ``the activity facets apply only to entries whose value tree carries a gyration`` () =
        let classDef = materialDef materialGyrationClassKey
        let handDef = materialDef materialHandednessKey
        Assert.True(applies classDef activeEntry)
        Assert.True(applies handDef activeEntry)
        // Langasite IS optically active physics, but its entry is a coded
        // preset (complexity = None) — no gyration value tree, so the facet
        // vanishes for it (recorded interpretation).
        Assert.False(applies classDef langasiteEntry)
        Assert.False(applies handDef langasiteEntry)
        Assert.False(applies classDef glass152Entry)

    [<Fact>]
    let ``the gyration-class and handedness extractors read the stored gyrotropic value`` () =
        Assert.Equal<string list>([ "Planar mm2 (g₁₂)" ], extractedKeys (materialDef materialGyrationClassKey) activeEntry)
        Assert.Equal<string list>([ "Right-handed" ], extractedKeys (materialDef materialHandednessKey) activeEntry)

    [<Fact>]
    let ``the offered gyration-class vocabulary derives from availableGyrationClasses`` () =
        Assert.Equal(7, List.length offeredGyrationClassKeys)
        Assert.Equal(7, offeredGyrationClassKeys |> List.distinct |> List.length)
        for anisotropy in [ Isotropic; Uniaxial; Biaxial ] do
            for gyration in availableGyrationClasses anisotropy do
                Assert.Contains(gyrationClassKey gyration, offeredGyrationClassKeys)

    [<Fact>]
    let ``the magnetic facet is offered only when magnetic and classifies scalar vs gyromagnetic`` () =
        let def = materialDef materialMagneticKey
        Assert.False(applies def glass152Entry)
        Assert.False(applies def siliconEntry)
        Assert.True(applies def scalarMagneticEntry)
        Assert.Equal<string list>([ "Scalar" ], extractedKeys def scalarMagneticEntry)
        Assert.Equal<string list>([ "Gyromagnetic" ], extractedKeys def polderMagneticEntry)
        // The dispersive Polder case is always the full tensor (the engine has
        // no scalar dispersive μ), so it classifies Gyromagnetic.
        Assert.Equal<string list>([ "Gyromagnetic" ], extractedKeys def dispersivePolderEntry)

    // =======================================================================
    // The dispersion-model facet.
    // =======================================================================

    [<Fact>]
    let ``every DispersionModel kind lands on the vocabulary key its lowering denotes`` () =
        let expected =
            [
                "ConstantNK", "Real n/k terms"
                "Cauchy", "Real n/k terms"
                "SumOfTerms", "Real n/k terms"
                "Sellmeier", "Complex ε terms"
                "Lorentz", "Complex ε terms"
                "Drude", "Complex ε terms"
                "TaucLorentz", "Evaluated (transcendental)"
                "GaussianOscillator", "Evaluated (transcendental)"
                "ForouhiBloomer", "Evaluated (transcendental)"
                "BrendelBormann", "Evaluated (transcendental)"
            ]
            |> Map.ofList
        // defaultModelChoices carries one representative of each of the ten kinds.
        Assert.Equal(10, List.length defaultModelChoices)
        for model in defaultModelChoices do
            Assert.Equal(expected[modelKindCode model], (modelDispersionKey model).value)

    [<Fact>]
    let ``the dispersion-model facet applies only to data-dispersive entries`` () =
        let def = materialDef materialDispersionModelKey
        // No seeded built-in carries a dispersive VALUE TREE: the nine
        // expressible seeds are constant and the coded presets are closures.
        for entry in builtInEntries do
            Assert.False(applies def entry)
        Assert.True(applies def syntheticDispersive)

    [<Fact>]
    let ``the dispersion-model facet is multi-valued per axis and segment`` () =
        let keys = extractedKeys (materialDef materialDispersionModelKey) syntheticDispersive
        // 2 segments × 2 axes = 4 classifications ...
        Assert.Equal(4, List.length keys)
        // ... over 3 distinct vocabulary values from ONE item.
        Assert.Equal<string list>(
            [ "Complex ε terms"; "Evaluated (transcendental)"; "Real n/k terms" ],
            keys |> List.distinct |> List.sort)

    [<Fact>]
    let ``one multi-model entry counts once in each dispersion-model branch`` () =
        let tree = buildTree (Representation [ materialDispersionModelKey ]) materialFacets [] [ syntheticDispersive ]
        let node = List.exactlyOne tree.facets
        // Three branches, each counting the single item once: branch counts
        // sum to 3 while the result total is 1 — multi-valued by design.
        Assert.Equal<(string * int) list>(
            [ "Complex ε terms", 1; "Evaluated (transcendental)", 1; "Real n/k terms", 1 ],
            branchSummary node)

    // =======================================================================
    // Library facets — kind, lifted material facets.
    // =======================================================================

    [<Fact>]
    let ``the kind facet applies to every library entry and groups the seeded corpus`` () =
        let def = libraryDef entryKindFacetKey
        for entry in seedEntries do
            Assert.True(applies def entry)
        let tree = buildTree (Representation [ entryKindFacetKey ]) libraryDefs [] seedEntries
        let node = List.exactlyOne tree.facets
        Assert.Equal<(string * int) list>(
            [ "Detector", 2; "Polarizer", 3; "Sample", 11; "Source", 1 ],
            branchSummary node)

    [<Fact>]
    let ``material facets lift to samples over every constituent including the lower half-space`` () =
        // Langasite film on silicon: langasite is a film, silicon is the LOWER
        // half-space — both are constituents, so the lifted category facet is
        // their distinct union.
        let keys = extractedKeys (libraryDef materialCategoryKey) (SampleItem SeedSamples.langasiteSilicon)
        Assert.Equal<string list>([ "Crystal"; "Semiconductor" ], List.sort keys)

    [<Fact>]
    let ``a sample matches a lifted material constraint through any constituent`` () =
        let byCategory (name : string) : string list =
            filter libraryDefs [ discrete materialCategoryKey [ name ] ] seedEntries
            |> List.map (fun e -> e.displayName)
        // Only the quarter-wave multilayer carries the vacuum spacer.
        Assert.Equal<string list>([ SeedSamples.multilayerQw.name ], byCategory "Vacuum")
        // Only the EUV stack carries a metal (molybdenum) — its OTHER layer is
        // a semiconductor; either constituent alone makes the sample match.
        Assert.Equal<string list>([ SeedSamples.euvMoSi.name ], byCategory "Metal")

    [<Fact>]
    let ``lifted material facets never offer themselves on non-sample entries`` () =
        Assert.False(applies (libraryDef materialCategoryKey) polarizerEntry)
        Assert.False(applies (libraryDef materialAnisotropyKey) polarizerEntry)
        Assert.Empty(extractedKeys (libraryDef materialCategoryKey) polarizerEntry)

    [<Fact>]
    let ``a lifted facet vanishes when no constituent offers it`` () =
        // Both constituents of langasite-on-silicon are coded presets, so the
        // lifted anisotropy/transparency facets are inapplicable and their
        // nodes are absent from the tree over that population.
        let entry = SampleItem SeedSamples.langasiteSilicon
        Assert.False(applies (libraryDef materialAnisotropyKey) entry)
        Assert.False(applies (libraryDef materialTransparencyKey) entry)
        let tree = buildTree libraryRepresentation libraryDefs [] [ entry ]
        let keys = tree.facets |> List.map (fun f -> f.key)
        Assert.DoesNotContain(materialAnisotropyKey, keys)
        Assert.DoesNotContain(materialTransparencyKey, keys)
        // The substrate-material facet is likewise absent (ThinFilm geometry).
        Assert.DoesNotContain(sampleSubstrateMaterialKey, keys)
        Assert.Contains(materialCategoryKey, keys)

    [<Fact>]
    let ``the lifted transparency is the distinct union over applicable constituents`` () =
        // The quarter-wave stack: glass classifies Transparent, the vacuum
        // spacer is a coded preset the facet skips.
        Assert.Equal<string list>(
            [ "Transparent" ],
            extractedKeys (libraryDef materialTransparencyKey) (SampleItem SeedSamples.multilayerQw))
        // Glass + EUV molybdenum: one Transparent and one Absorbing constituent.
        Assert.Equal<string list>(
            [ "Absorbing"; "Transparent" ],
            extractedKeys (libraryDef materialTransparencyKey) (SampleItem mixedFilmSample) |> List.sort)

    [<Fact>]
    let ``the lifted dispersion facet classifies constituents through their properties`` () =
        Assert.Equal<string list>(
            [ "Constant" ],
            extractedKeys (libraryDef materialDispersionKey) (SampleItem SeedSamples.glassPlate1mm))
        Assert.Equal<string list>(
            [ "Dispersive" ],
            extractedKeys (libraryDef materialDispersionKey) (SampleItem SeedSamples.langasiteSilicon))

    // =======================================================================
    // Sample-structural facets.
    // =======================================================================

    [<Fact>]
    let ``the substrate-material facet is offered for plate and wedge geometries only`` () =
        let def = libraryDef sampleSubstrateMaterialKey
        Assert.True(applies def (SampleItem SeedSamples.glassPlate1mm))
        Assert.True(applies def (SampleItem { SeedSamples.glassPlate1mm with substrate = Wedge }))
        Assert.False(applies def (SampleItem SeedSamples.glassFilm600))
        Assert.False(applies def polarizerEntry)

    [<Fact>]
    let ``the substrate-material facet keys the substrate layer's display name`` () =
        let def = libraryDef sampleSubstrateMaterialKey
        Assert.Equal<string list>(
            [ "Transparent glass (n = 1.52)" ],
            extractedKeys def (SampleItem SeedSamples.glassPlate1mm))
        Assert.Equal<string list>(
            [ "Active (gyrotropic) crystal" ],
            extractedKeys def (SampleItem SeedSamples.activeCrystal))

    [<Fact>]
    let ``has-thin-films derives from films alone, independent of the substrate kind`` () =
        let def = libraryDef sampleHasThinFilmsKey
        Assert.Equal<string list>([ hasThinFilmsValue.value ], extractedKeys def (SampleItem SeedSamples.glassFilm600))
        Assert.Equal<string list>([ noThinFilmsValue.value ], extractedKeys def (SampleItem SeedSamples.glassPlate1mm))
        // A PLATE carrying films still reads "has thin films" (operator, 003/Q11).
        let plateWithFilms =
            { SeedSamples.glassPlate1mm with
                structure =
                    { SeedSamples.glassPlate1mm.structure with
                        films = [ SingleLayer (layer MaterialIds.glass152 100.0) ] } }
        Assert.Equal<string list>([ hasThinFilmsValue.value ], extractedKeys def (SampleItem plateWithFilms))
        Assert.False(applies def polarizerEntry)

    [<Fact>]
    let ``the film-material facet lists distinct constituent film names`` () =
        let def = libraryDef sampleFilmMaterialKey
        // The quarter-wave stack names glass ONCE although it appears in the
        // repeated cell AND as the closing single layer.
        Assert.Equal<string list>(
            [ "Transparent glass (n = 1.52)"; "Vacuum" ],
            extractedKeys def (SampleItem SeedSamples.multilayerQw) |> List.sort)
        Assert.Equal<string list>(
            [ "Molybdenum (Mo, EUV)"; "Silicon (Si, EUV)" ],
            extractedKeys def (SampleItem SeedSamples.euvMoSi) |> List.sort)
        // No films — the facet does not offer itself.
        Assert.False(applies def (SampleItem SeedSamples.glassPlate1mm))

    [<Fact>]
    let ``film-material branches are name-sorted`` () =
        let corpus = [ SampleItem SeedSamples.multilayerQw; SampleItem SeedSamples.euvMoSi ]
        let tree = buildTree (Representation [ sampleFilmMaterialKey ]) libraryDefs [] corpus
        let node = List.exactlyOne tree.facets
        Assert.Equal<string list>(
            [ "Molybdenum (Mo, EUV)"; "Silicon (Si, EUV)"; "Transparent glass (n = 1.52)"; "Vacuum" ],
            node.branches |> List.map (fun b -> b.label))

    // =======================================================================
    // Per-film thickness → the step-10 buckets.
    // =======================================================================

    [<Fact>]
    let ``per-film thickness extracts one nm magnitude per layer`` () =
        let def = libraryDef sampleFilmThicknessKey
        let glassFilm = numericValues def (SampleItem SeedSamples.glassFilm600)
        Assert.Equal(1, List.length glassFilm)
        Assert.Equal(600.0, List.exactlyOne glassFilm, 1e-6)
        // The EUV stack's cell is two 2.65 nm layers: one value PER LAYER of
        // the cell (counted once — not per period, and never the period sum).
        let euv = numericValues def (SampleItem SeedSamples.euvMoSi)
        Assert.Equal(2, List.length euv)
        for v in euv do
            Assert.Equal(2.65, v, 1e-6)
        // The repeated 12/30 nm sample reads per-layer values, cell once:
        // never the 42 nm period, never the 210 nm total.
        let repeated = numericValues def (SampleItem repeatedFilmSample)
        Assert.Equal(2, List.length repeated)
        Assert.Equal(12.0, repeated[0], 1e-6)
        Assert.Equal(30.0, repeated[1], 1e-6)
        // Film-less samples do not offer the facet.
        Assert.False(applies def (SampleItem SeedSamples.glassPlate1mm))

    [<Fact>]
    let ``per-film thicknesses flow into the step-10 buckets`` () =
        let buckets =
            bucketsFor
                ThicknessBucketCap.defaultValue
                (libraryDef sampleFilmThicknessKey)
                libraryDefs
                []
                bucketCorpus
        // Population (per item, deduped): A → 12; B → 30, 700; repeated → 12, 30.
        // The polarizer is inapplicable and contributes nothing. 1-2-5 ladder
        // over [12, 700]: 10, 20, 50, 100, 200, 500, 1000; empty buckets drop.
        Assert.Equal<(float * float * int) list>(
            [ 10.0, 20.0, 2; 20.0, 50.0, 2; 500.0, 1000.0, 1 ],
            buckets |> List.map (fun b -> b.range.lower, b.range.upper, b.count.value))

    [<Fact>]
    let ``a thickness bucket applies as an ordinary constraint chip reproducing its count`` () =
        let buckets =
            bucketsFor
                ThicknessBucketCap.defaultValue
                (libraryDef sampleFilmThicknessKey)
                libraryDefs
                []
                bucketCorpus
        let chip = constraintFor sampleFilmThicknessKey (List.head buckets)
        let result = filter libraryDefs [ chip ] bucketCorpus |> List.map (fun e -> e.displayName)
        Assert.Equal<string list>([ "Film A"; "Repeated film" ], result)
