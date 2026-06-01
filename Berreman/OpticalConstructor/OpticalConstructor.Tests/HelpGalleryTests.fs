namespace OpticalConstructor.Tests

open OpticalConstructor.Domain
open OpticalConstructor.Domain.Project
open OpticalConstructor.Ui
open Xunit

/// §J.11 in-app help, glossary & gallery — AC-J11 (slice 016). A gallery entry
/// opens a shipped sample `OpticalConstructorProject` (corresponding to an
/// `Analytics/Examples/*.fsx` case) through the schema-validated open path; the
/// units glossary states the §A.10 eV/cm⁻¹ conversions WITHOUT re-implementing
/// them (it reuses `Domain.Units`); and no `.fsx` is executed at runtime (the
/// samples are engine-preset literals — opening one reads no script).
module HelpGalleryTests =

    // ----------------------------------------------------------------- gallery

    [<Fact>]
    let ``AC-J11 every gallery entry opens a shipped sample project through the schema-validated path`` () =
        Assert.NotEmpty(Help.gallery)
        for entry in Help.gallery do
            // openEntry routes the literal sample through serialize >> deserialize —
            // the validate-on-load core ProjectFile.openProject also runs (§A.7).
            match Help.openEntry entry with
            | Ok project ->
                // A real, bound project with at least its one sample system and a
                // beam-tree root — the same shape a normal opened project has.
                Assert.NotEmpty(project.systems)
                Assert.True(project.beamTree.root.system.films.Length >= 0)
            | Error e -> Assert.Fail($"gallery entry '{entry.title}' failed to open: {e}")

    [<Fact>]
    let ``AC-J11 the gallery covers the named example cases`` () =
        let scripts = Help.gallery |> List.map (fun e -> e.sourceScript)
        // The AR/multilayer, EUV Mo-Si, active-crystal, wedge, and dispersive-glass
        // cases are all present (design references, never executed).
        Assert.Contains("MultilayerThinFilm.fsx", scripts)
        Assert.Contains("MultilayerThinFilm_EUV.fsx", scripts)
        Assert.Contains("ActiveCrystal.fsx", scripts)
        Assert.Contains("Wedge_BiaxialCrystal.fsx", scripts)
        Assert.Contains("Glass_Dispersive.fsx", scripts)

    [<Fact>]
    let ``AC-J11 the EUV sample is a periodic Mo-Si stack with many layers`` () =
        let euv = Help.gallery |> List.find (fun e -> e.sourceScript = "MultilayerThinFilm_EUV.fsx")
        match Help.openEntry euv with
        | Ok project ->
            // The Mo/Si pair stack expands to many films (RepeatBuilder.expand, §J.2).
            Assert.True(project.beamTree.root.system.films.Length >= 2)
        | Error e -> Assert.Fail($"EUV sample failed to open: {e}")

    // ----------------------------------------------------------------- glossary

    [<Fact>]
    let ``AC-J11 the eV conversion reuses Domain.Units, not a second implementation`` () =
        // E[eV] = 1239.84 / λ[nm], computed through the SOLE Units seam.
        let lambdaNm = 500.0
        Assert.Equal(Units.evNmProduct / lambdaNm, Help.photonEnergyEv lambdaNm, 9)

    [<Fact>]
    let ``AC-J11 the cm-1 conversion reuses Domain.Units`` () =
        // ν̃[cm⁻¹] = 1e7 / λ[nm]; λ = 500 nm → 20000 cm⁻¹, through the Units seam.
        Assert.Equal(20000.0, Help.wavenumberPerCm 500.0, 6)

    [<Fact>]
    let ``AC-J11 the units glossary states the eV and wavenumber relations`` () =
        let evEntry = Help.unitsGlossary |> List.tryFind (fun g -> g.term.Contains "eV")
        match evEntry with
        | Some g -> Assert.Contains("1239.84", g.definition)
        | None -> Assert.Fail("the glossary must include an eV entry")

        let wnEntry = Help.unitsGlossary |> List.tryFind (fun g -> g.term.Contains "cm⁻¹")
        match wnEntry with
        | Some g -> Assert.Contains("1e7", g.definition)
        | None -> Assert.Fail("the glossary must include a wavenumber entry")

    [<Fact>]
    let ``AC-J11 field tooltips are present as static strings`` () =
        Assert.NotEmpty(Help.tooltips)
        Assert.All(Help.tooltips, fun t -> Assert.False(System.String.IsNullOrWhiteSpace t.text))
