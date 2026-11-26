namespace OpticalConstructor.Tests

open System
open Berreman.Constants
open Berreman.Media
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.Studies
open Xunit
open CrystalMaterialFixtures

/// The test-local tabulated user material the study-table facts share: one isotropic tabulated segment at 400, 500 and
/// 700 nm, n 1.5, 1.6 and 1.9, k 0, 0.1 and 0.3, σn 0.01, 0.02 and 0.03, σk 0, 0.001 and 0.002 (spec 0050 S13.2). It
/// lives here because this file compiles first among those facts.
module TabulatedStudyFixtures =

    /// The version the tabulated material stands for.
    let tableVersion : MaterialVersionId =
        MaterialVersionId.firstOf (MaterialId (Guid.Parse "46000000-0000-4000-8000-000000000001"))

    /// The three rows without the source's uncertainty.
    let plainTable : TabulatedNkTable =
        match TabulatedNkTable.tryCreate [| 400e-9<meter>; 500e-9<meter>; 700e-9<meter> |] [| 1.5; 1.6; 1.9 |] [| 0.0; 0.1; 0.3 |] with
        | Ok table -> table
        | Error error -> failwith $"the fixture's table is refused: %A{error}"

    /// The three rows with the source's one-sigma n and k.
    let uncertainTable : TabulatedNkTable =
        match plainTable.withUncertainty { sigmaN = [| 0.01; 0.02; 0.03 |]; sigmaK = [| 0.0; 0.001; 0.002 |] } with
        | Ok table -> table
        | Error error -> failwith $"the fixture's uncertainties are refused: %A{error}"

    /// The user material whose physics is one isotropic tabulated segment of `table` over its samples, with no magnetic
    /// or active part, as a tabulated import builds it.
    let tableEntryOf (table : TabulatedNkTable) : MaterialEntry =
        {
            id = tableVersion.materialId
            name = EntryName "Probe tabulated glass"
            category = CategoryIds.glass
            description = Some (EntryDescription "Three tabulated rows of n and k")
            physics =
                ComplexityPhysics
                    {
                        eps = EpsWithDispValue (IsotropicDispersive [ { wavelengthInterval = table.sampleRange; dispersion = TabulatedNK table } ])
                        magnetic = None
                        active = None
                    }
        }

    /// The material with the source's uncertainties.
    let uncertainTableEntry : MaterialEntry = tableEntryOf uncertainTable

    let row1K : ParameterName = ParameterName.tableRowExtinction (ParameterNameIndex 1)
    let row2N : ParameterName = ParameterName.tableRowIndex (ParameterNameIndex 2)
    let row2K : ParameterName = ParameterName.tableRowExtinction (ParameterNameIndex 2)
    let row3N : ParameterName = ParameterName.tableRowIndex (ParameterNameIndex 3)
    let row3K : ParameterName = ParameterName.tableRowExtinction (ParameterNameIndex 3)

    /// `row2.n` freed from `start` within [1.5, 1.7], with no prior.
    let row2NFrom (start : float) : UnknownParameterData =
        {
            start = ParameterValue start
            bounds = { lower = ParameterValue 1.5; upper = ParameterValue 1.7 }
            priorOpt = None
        }

/// Spec 0050 S11.1, S13.2 — OpenMaterial's pure half: an opened crystal material's working copy takes its parameters from
/// the record's generated axes, an opened tabulated material's from its rows' n and k, one copy per version shared by every
/// sample made of it; any other complexity tree is refused.
module StudyMaterialTests =

    open TabulatedStudyFixtures

    /// The test-local material version every opened entry here stands for.
    let private openedVersion : MaterialVersionId =
        MaterialVersionId.firstOf (MaterialId (Guid.Parse "36100000-0000-4000-8000-000000000001"))

    /// A version no study here opens.
    let private unopenedVersion : MaterialVersionId = MaterialVersionId.firstOf MaterialIds.glass152

    let private crystalEntry (name : EntryName) (crystal : CrystalParameters) : MaterialEntry =
        {
            id = openedVersion.materialId
            name = name
            category = CategoryIds.crystal
            description = None
            physics = CrystalParameterPhysics crystal
        }

    let private absorbingEntry : MaterialEntry =
        crystalEntry (EntryName "Probe absorbing crystal") (AbsorbingTriclinicCrystal absorbingTriclinicParameters)

    /// QZ-LR over the shared probe copy, so a copy opened here is its second.
    let private heldStudy : Study = StudyFileFixtures.qzLrStudyOver StudyFileFixtures.studyProbeMaterial

    let private n3 : ParameterName = ParameterName "n3"

    /// `n3` freed: an unknown starting at 1.70 within [1.60, 1.80].
    let private freeN3 : ParameterState =
        UnknownParameter
            {
                start = ParameterValue 1.70
                bounds = { lower = ParameterValue 1.60; upper = ParameterValue 1.80 }
                priorOpt = None
            }

    let private openedOf (result : Result<OpenedMaterial, StudyError>) : OpenedMaterial =
        match result with
        | Ok material -> material
        | Error error -> failwith $"expected a working copy, got {error.describe}"

    let private studyOf (result : Result<Study, StudyError>) : Study =
        match result with
        | Ok study -> study
        | Error error -> failwith $"expected a study, got {error.describe}"

    let private fieldCaseOf (field : ParameterField<'Parameters>) : string =
        match field with
        | RefractionIndexField _ -> "RefractionIndexField"
        | RhoValueField _ -> "RhoValueField"
        | EpsValueField _ -> "EpsValueField"

    /// The opened copy of `probe` against the record's own axes: names, order and field cases; each generated read and
    /// write against the record axis's on `probe`; a crystal of another case read as `probe` and written back unchanged;
    /// and one known parameter per axis at the probe's value.
    let private assertGeneratedAxes
        (crystalOf : 'Record -> CrystalParameters)
        (probe : 'Record)
        (recordAxes : ParameterAxis<'Record> list)
        (otherCase : CrystalParameters)
        : unit =
        let crystal = crystalOf probe
        let entry = crystalEntry (EntryName "Probe crystal") crystal
        let opened = openedOf (OpenedMaterial.openFrom openedVersion entry)

        Assert.Equal(openedVersion, opened.version)
        Assert.Equal(entry.name, opened.name)
        Assert.Equal<OpenedMaterialPhysics>(OpenedCrystalPhysics crystal, opened.physics)

        let generated =
            match opened.physics with
            | OpenedCrystalPhysics c -> c.parameterAxes
            | OpenedTablePhysics _ -> failwith "expected an opened crystal, got an opened table"
        Assert.Equal<ParameterName list>(recordAxes |> List.map (fun axis -> axis.name), generated |> List.map (fun axis -> axis.name))

        for recordAxis, axis in List.zip recordAxes generated do
            let read = recordAxis.read probe
            let scaled = 2.5 * read
            Assert.Equal(fieldCaseOf recordAxis.field, fieldCaseOf axis.field)
            Assert.Equal(read, axis.read crystal)
            Assert.Equal(ParameterValue read, axis.parameterValueOf crystal)
            Assert.Equal(crystalOf (recordAxis.write scaled probe), axis.write scaled crystal)
            Assert.Equal(crystalOf (recordAxis.write scaled probe), axis.writeValue (ParameterValue scaled) crystal)
            Assert.Equal(read, axis.read otherCase)
            Assert.Equal(otherCase, axis.write scaled otherCase)

        let known : Parameter list =
            recordAxes |> List.map (fun axis -> { name = axis.name; state = KnownParameter (ParameterValue (axis.read probe)) })

        Assert.Equal<Parameter list>(known, opened.parameters)

    [<Fact>]
    let ``the generated axes of an opened absorbing triclinic material equal AbsorbingTriclinicParameters.axes`` () : unit =
        Assert.Equal(15, AbsorbingTriclinicParameters.axes.Length)
        assertGeneratedAxes AbsorbingTriclinicCrystal absorbingTriclinicParameters AbsorbingTriclinicParameters.axes (TriclinicCrystal triclinicParameters)

    [<Fact>]
    let ``an opened uniaxial or triclinic material generates its record's axes the same way`` () : unit =
        Assert.Equal(4, UniaxialParameters.axes.Length)
        assertGeneratedAxes UniaxialCrystal uniaxialParameters UniaxialParameters.axes (AbsorbingTriclinicCrystal absorbingTriclinicParameters)
        Assert.Equal(9, TriclinicParameters.axes.Length)
        assertGeneratedAxes TriclinicCrystal triclinicParameters TriclinicParameters.axes (UniaxialCrystal uniaxialParameters)

    [<Fact>]
    let ``a material whose physics is a complexity tree, or one opened twice, is refused`` () : unit =
        let glass : MaterialEntry =
            {
                absorbingEntry with
                    name = EntryName "Constant glass"
                    physics = ComplexityPhysics { eps = EpsWithoutDispValue (IsotropicTransparent (RefractionIndex 1.52)); magnetic = None; active = None }
            }

        match OpenedMaterial.openFrom openedVersion glass with
        | Error (MaterialWithoutParametersErr reason) -> Assert.Contains("'Constant glass'", reason)
        | other -> Assert.Fail($"expected Error (MaterialWithoutParametersErr _), got %A{other}")

        match heldStudy.openMaterial openedVersion glass with
        | Error (MaterialWithoutParametersErr reason) -> Assert.Contains("'Constant glass'", reason)
        | other -> Assert.Fail($"expected Error (MaterialWithoutParametersErr _) from the study, got %A{other}")

        let opened = studyOf (heldStudy.openMaterial openedVersion absorbingEntry)
        let copy = openedOf (OpenedMaterial.openFrom openedVersion absorbingEntry)
        Assert.Equal({ heldStudy with materials = [ StudyFileFixtures.studyProbeMaterial; copy ] }, opened)

        match opened.openMaterial openedVersion absorbingEntry with
        | Error (MaterialAlreadyOpenErr reason) ->
            Assert.Contains($"{openedVersion.materialId.value}", reason)
            Assert.Contains($"'{absorbingEntry.name.value}'", reason)
        | other -> Assert.Fail($"expected Error (MaterialAlreadyOpenErr _), got %A{other}")

    [<Fact>]
    let ``freeing a component makes it an unknown in place, and the working copy's crystal reads each parameter's current value`` () : unit =
        let study = studyOf (heldStudy.openMaterial openedVersion absorbingEntry)
        let copy = openedOf (OpenedMaterial.openFrom openedVersion absorbingEntry)
        let freed = studyOf (study.withMaterialParameterState openedVersion n3 freeN3)
        let freedCopy = freed.materials |> List.find (fun material -> material.version = openedVersion)
        let expectedParameters = copy.parameters |> List.map (fun parameter -> if parameter.name = n3 then { parameter with state = freeN3 } else parameter)

        Assert.Equal<Parameter list>(expectedParameters, freedCopy.parameters)
        Assert.Equal(n3, freedCopy.parameters.[2].name)
        Assert.Equal({ study with materials = [ StudyFileFixtures.studyProbeMaterial; freedCopy ] }, freed)
        Assert.Equal<OpenedMaterialPhysics>(copy.physics, freedCopy.physics)
        Assert.Equal<OpenedMaterialPhysics>(
            OpenedCrystalPhysics (AbsorbingTriclinicCrystal { absorbingTriclinicParameters with index3 = RefractionIndex 1.70 }),
            freedCopy.currentPhysics)
        Assert.Equal<OpenedMaterialPhysics>(copy.physics, copy.currentPhysics)

        // A calibrated component reads its calibrated value.
        let calibrated =
            CalibratedParameter
                {
                    value = ParameterValue 2.5e-3
                    sigma = ParameterSigma 1.0e-4
                    fitRun = FitRunName "QZ-LR@20261113T120000Z"
                }

        let calibratedCopy = openedOf (freedCopy.withParameterState (ParameterName "e11") calibrated)
        Assert.Equal<OpenedMaterialPhysics>(
            OpenedCrystalPhysics (AbsorbingTriclinicCrystal { absorbingTriclinicParameters with index3 = RefractionIndex 1.70; epsIm11 = EpsValue 2.5e-3 }),
            calibratedCopy.currentPhysics)

        match study.withMaterialParameterState openedVersion (ParameterName "n_o") freeN3 with
        | Error (UnknownStudyParameterErr reason) ->
            Assert.Contains("n_o", reason)
            Assert.Contains($"'{absorbingEntry.name.value}'", reason)
        | other -> Assert.Fail($"expected Error (UnknownStudyParameterErr _), got %A{other}")

        let reversed =
            UnknownParameter
                {
                    start = ParameterValue 1.70
                    bounds = { lower = ParameterValue 1.80; upper = ParameterValue 1.60 }
                    priorOpt = None
                }

        match study.withMaterialParameterState openedVersion n3 reversed with
        | Error (InvalidStudyParameterErr reason) -> Assert.Contains("n3", reason)
        | other -> Assert.Fail($"expected Error (InvalidStudyParameterErr _), got %A{other}")

        match study.withMaterialParameterState unopenedVersion n3 freeN3 with
        | Error (UnknownStudyMaterialErr reason) -> Assert.Contains($"{unopenedVersion.materialId.value}", reason)
        | other -> Assert.Fail($"expected Error (UnknownStudyMaterialErr _), got %A{other}")

        Assert.Equal({ heldStudy with materials = [ StudyFileFixtures.studyProbeMaterial; copy ] }, study)

    [<Fact>]
    let ``every sample made of an opened material shares its working copy`` () : unit =
        let layerOf (version : MaterialVersionId) : SampleLayer =
            {
                materialId = version
                thickness = Thickness.mm 1.0<mm>
                orientation = PrimaryAxes
            }

        let plate : Sample =
            {
                id = SampleId (Guid.Parse "36100000-0000-4000-8000-000000000011")
                name = EntryName "Probe plate"
                structure = { films = []; substrate = Some (layerOf openedVersion); lower = None }
                substrate = ThickPlate
                description = EntryDescription "A plate of the opened probe crystal"
                supportedEmission = ThickPlate.defaultSupportedEmission
            }

        let film : Sample =
            {
                plate with
                    id = SampleId (Guid.Parse "36100000-0000-4000-8000-000000000012")
                    name = EntryName "Probe film"
                    structure =
                        {
                            films = [ Repeated { cell = [ layerOf unopenedVersion; layerOf openedVersion ]; count = PeriodCount 3 } ]
                            substrate = None
                            lower = None
                        }
                    substrate = ThinFilm
                    description = EntryDescription "Three periods of glass and the opened probe crystal"
                    supportedEmission = ThinFilm.defaultSupportedEmission
            }

        let glassPlate : Sample =
            {
                plate with
                    id = SampleId (Guid.Parse "36100000-0000-4000-8000-000000000013")
                    name = EntryName "Glass plate"
                    structure = { films = []; substrate = Some (layerOf unopenedVersion); lower = None }
                    description = EntryDescription "A plate of a version the study does not open"
            }

        let study = studyOf (heldStudy.openMaterial openedVersion absorbingEntry)
        let copy = openedOf (OpenedMaterial.openFrom openedVersion absorbingEntry)

        Assert.Equal<OpenedMaterial list>([ copy ], plate.openedMaterialsIn study)
        Assert.Equal<OpenedMaterial list>([ copy ], film.openedMaterialsIn study)
        Assert.Equal<OpenedMaterial list>([], glassPlate.openedMaterialsIn study)

        // A change to the copy reaches every sample of it.
        let freed = studyOf (study.withMaterialParameterState openedVersion n3 freeN3)
        let freedCopy = freed.materials |> List.find (fun material -> material.version = openedVersion)
        Assert.Equal<OpenedMaterial list>([ freedCopy ], plate.openedMaterialsIn freed)
        Assert.Equal<OpenedMaterial list>([ freedCopy ], film.openedMaterialsIn freed)

        // In study order: a stack of both opened versions, the probe's last, answers the probe's copy first.
        let both : Sample =
            {
                film with
                    structure =
                        {
                            films = [ SingleLayer (layerOf openedVersion) ]
                            substrate = Some (layerOf StudyFileFixtures.studyProbeMaterial.version)
                            lower = None
                        }
            }

        Assert.Equal<OpenedMaterial list>([ StudyFileFixtures.studyProbeMaterial; copy ], both.openedMaterialsIn study)

    /// The component's state in the copy.
    let private stateIn (material : OpenedMaterial) (name : ParameterName) : ParameterState =
        (material.parameters |> List.find (fun parameter -> parameter.name = name)).state

    [<Fact>]
    let ``a tabulated material opens with one known component per row's n and k`` () : unit =
        let copy = openedOf (OpenedMaterial.openFrom tableVersion uncertainTableEntry)

        Assert.Equal(tableVersion, copy.version)
        Assert.Equal(uncertainTableEntry.name, copy.name)
        Assert.Equal<OpenedMaterialPhysics>(OpenedTablePhysics uncertainTable, copy.physics)

        // Each row's n then its k, rows 1-based in table order, known at the table's own values.
        let known (name : string) (value : float) : Parameter = { name = ParameterName name; state = KnownParameter (ParameterValue value) }

        Assert.Equal<Parameter list>(
            [ known "row1.n" 1.5; known "row1.k" 0.0; known "row2.n" 1.6; known "row2.k" 0.1; known "row3.n" 1.9; known "row3.k" 0.3 ],
            copy.parameters)

        Assert.Equal<ParameterName list>(copy.parameters |> List.map (fun parameter -> parameter.name), copy.physics.componentNames)
        Assert.Equal<ParameterName list>([ ParameterName "row1.k"; ParameterName "row2.n"; ParameterName "row3.k" ], [ row1K; row2N; row3K ])

        // The study opens it beside its crystal as it opens a crystal.
        Assert.Equal(
            { heldStudy with materials = [ StudyFileFixtures.studyProbeMaterial; copy ] },
            studyOf (heldStudy.openMaterial tableVersion uncertainTableEntry))

        // Two tabulated segments are no single table: refused, as a constant complexity is.
        let segment : IsotropicEpsSegment = { wavelengthInterval = plainTable.sampleRange; dispersion = TabulatedNK plainTable }

        let twoSegments : MaterialEntry =
            {
                uncertainTableEntry with
                    name = EntryName "Two-segment glass"
                    physics = ComplexityPhysics { eps = EpsWithDispValue (IsotropicDispersive [ segment; segment ]); magnetic = None; active = None }
            }

        match OpenedMaterial.openFrom tableVersion twoSegments with
        | Error (MaterialWithoutParametersErr reason) -> Assert.Contains("'Two-segment glass'", reason)
        | other -> Assert.Fail($"expected Error (MaterialWithoutParametersErr _), got %A{other}")

    [<Fact>]
    let ``a freed component of an uncertain table takes its source sigma as its prior, and an explicit prior is kept`` () : unit =
        let copy = openedOf (OpenedMaterial.openFrom tableVersion uncertainTableEntry)
        let freeRow2N = row2NFrom 1.6

        // row2.n freed with no prior: the opened 1.6 at the source's σn 0.02.
        let freed = openedOf (copy.withParameterState row2N (UnknownParameter freeRow2N))
        let sourcePrior : ParameterPrior = { centre = ParameterValue 1.6; sigma = ParameterSigma 0.02 }
        Assert.Equal<ParameterState>(UnknownParameter { freeRow2N with priorOpt = Some sourcePrior }, stateIn freed row2N)

        // row1.k's σk is 0, so it takes none.
        let freeRow1K : UnknownParameterData =
            {
                start = ParameterValue 0.0
                bounds = { lower = ParameterValue 0.0; upper = ParameterValue 0.05 }
                priorOpt = None
            }

        Assert.Equal<ParameterState>(UnknownParameter freeRow1K, stateIn (openedOf (copy.withParameterState row1K (UnknownParameter freeRow1K))) row1K)

        // An explicit prior is kept; a known or calibrated state is stored as given.
        let explicitPrior = { freeRow2N with priorOpt = Some { centre = ParameterValue 1.62; sigma = ParameterSigma 0.05 } }
        Assert.Equal<ParameterState>(UnknownParameter explicitPrior, stateIn (openedOf (copy.withParameterState row2N (UnknownParameter explicitPrior))) row2N)

        let knownRow3N = KnownParameter (ParameterValue 1.85)
        Assert.Equal<ParameterState>(knownRow3N, stateIn (openedOf (copy.withParameterState row3N knownRow3N)) row3N)

        let calibratedRow2K =
            CalibratedParameter
                {
                    value = ParameterValue 0.12
                    sigma = ParameterSigma 0.004
                    fitRun = FitRunName "Glass@20261117T120000Z"
                }

        Assert.Equal<ParameterState>(calibratedRow2K, stateIn (openedOf (copy.withParameterState row2K calibratedRow2K)) row2K)

        // The study stores the copy holding the source prior.
        let study = studyOf (heldStudy.openMaterial tableVersion uncertainTableEntry)
        let freedStudy = studyOf (study.withMaterialParameterState tableVersion row2N (UnknownParameter freeRow2N))
        Assert.Equal({ study with materials = [ StudyFileFixtures.studyProbeMaterial; freed ] }, freedStudy)

        // The same material without uncertainties frees row2.n with none.
        let plainCopy = openedOf (OpenedMaterial.openFrom tableVersion (tableEntryOf plainTable))
        Assert.Equal<ParameterState>(UnknownParameter freeRow2N, stateIn (openedOf (plainCopy.withParameterState row2N (UnknownParameter freeRow2N))) row2N)

    [<Fact>]
    let ``an opened table's current physics is its table with each row at its parameter's current value`` () : unit =
        let copy = openedOf (OpenedMaterial.openFrom tableVersion uncertainTableEntry)

        let changed =
            copy.withParameterState row2N (UnknownParameter (row2NFrom 1.65))
            |> Result.bind (fun held -> held.withParameterState row3K (KnownParameter (ParameterValue 0.35)))
            |> openedOf

        // n2 at its unknown's start, k3 at its known value; every other value and the uncertainties the table's own.
        Assert.Equal<OpenedMaterialPhysics>(
            OpenedTablePhysics { uncertainTable with n = [| 1.5; 1.65; 1.9 |]; k = [| 0.0; 0.1; 0.35 |] },
            changed.currentPhysics)

        // The copy keeps the opened version's table, and an unchanged copy's current physics is that table.
        Assert.Equal<OpenedMaterialPhysics>(OpenedTablePhysics uncertainTable, changed.physics)
        Assert.Equal<OpenedMaterialPhysics>(copy.physics, copy.currentPhysics)
