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

/// Spec 0050 S11.1 — OpenMaterial's pure half: an opened crystal material's working copy takes its parameters from the
/// record's generated axes, one copy per version shared by every sample made of it; a complexity tree is refused.
module StudyMaterialTests =

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
        Assert.Equal(crystal, opened.crystal)

        let generated = opened.crystal.parameterAxes
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
        Assert.Equal(copy.crystal, freedCopy.crystal)
        Assert.Equal(AbsorbingTriclinicCrystal { absorbingTriclinicParameters with index3 = RefractionIndex 1.70 }, freedCopy.currentCrystal)
        Assert.Equal(copy.crystal, copy.currentCrystal)

        // A calibrated component reads its calibrated value.
        let calibrated =
            CalibratedParameter
                {
                    value = ParameterValue 2.5e-3
                    sigma = ParameterSigma 1.0e-4
                    fitRun = FitRunName "QZ-LR@20261113T120000Z"
                }

        let calibratedCopy = openedOf (freedCopy.withParameterState (ParameterName "e11") calibrated)
        Assert.Equal(
            AbsorbingTriclinicCrystal { absorbingTriclinicParameters with index3 = RefractionIndex 1.70; epsIm11 = EpsValue 2.5e-3 },
            calibratedCopy.currentCrystal)

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
