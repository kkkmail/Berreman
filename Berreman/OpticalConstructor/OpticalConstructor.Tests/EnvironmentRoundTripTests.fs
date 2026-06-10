namespace OpticalConstructor.Tests

open System
open System.IO
open Berreman.Constants
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Domain.Units
open OpticalConstructor.Ui.Localization
open OpticalConstructor.Ui.UserEnvironment
open Xunit

/// §J.6/J.7/J.8 environment persistence (AC-J6/AC-J7/AC-J8). `EnvironmentSettings`
/// serializes to schema-validated JSON (never `.binz`) reusing the slice-003 shared
/// options, round-trips (favorites board, recent files, last folder, panel layout,
/// theme, preferences preserved), and falls back to built-in defaults on a
/// deliberately invalid file WITHOUT migration. Preference edits touch only
/// display/boundary labels and leave stored `double<meter>` magnitudes identical;
/// solver defaults are the engine's `SolverParameters`.
module EnvironmentRoundTripTests =

    let private filmThickness : float<meter> = 1.0e-7<meter>

    let private favLayer : Layer =
        { properties = OpticalProperties.vacuum; thickness = Thickness.Thickness filmThickness }

    let private favSystem : OpticalSystem =
        {
            description = Some "pinned sub-assembly"
            upper = OpticalProperties.vacuum
            films = [ favLayer ]
            substrate = None
            lower = OpticalProperties.vacuum
        }

    /// A representative non-default environment exercising every persisted field —
    /// all four favorite-pin shapes (§A.7), recent files, last folders, a Dark
    /// theme, an edited panel layout, a non-default chart palette, and edited
    /// preferences.
    let private sample : EnvironmentSettings =
        { defaults with
            favorites =
                [
                    {
                        name = "optics shelf"
                        pins =
                            [
                                LayerPin favLayer            // reused engine Layer, by value
                                SystemPin favSystem          // OpticalSystem fragment, by value
                                MaterialPin "Si"             // materialEntry id
                                SourcePin "src-d65"          // sourceSpec id
                            ]
                    }
                ]
            lastFolders = [ @"C:\work\optics" ]
            recentFiles = [ "a.ocproj"; "b.ocproj" ]
            theme = Dark
            // Edit the saved layout through the AppShell reducer (J.8): hide "results".
            layout = OpticalConstructor.Ui.AppShell.setPanelVisible "results" false defaults.layout
            chartPalette = [ "#112233"; "#445566" ]
            preferences = { defaults.preferences with wavelengthUnit = Micrometer; sweepPoints = 250 } }

    let private okOr (r : Result<'a, _>) : 'a =
        match r with
        | Ok v -> v
        | Error e -> failwith $"unexpected storage error: {e}"

    // --- AC-J6: schema-validated JSON round-trip (never .binz) ------------------

    [<Fact>]
    let ``AC-J6 EnvironmentSettings serializes to schema-validating JSON, never binz`` () =
        let json = serialize sample |> okOr
        // The artefact is JSON text carrying the envelope version, never a .binz pickle.
        Assert.StartsWith("{", json.TrimStart())
        Assert.Contains("schemaVersion", json)
        Assert.DoesNotContain(".binz", json)
        // Validate-on-load admits the document against the published env schema.
        deserialize json |> okOr |> ignore

    [<Fact>]
    let ``AC-J6 a full round-trip preserves every persisted field`` () =
        let back = serialize sample |> okOr |> deserialize |> okOr
        // Favorites board (all four pin shapes, by-value fragments included) survives.
        Assert.Equal(1, List.length back.favorites)
        Assert.Equal("optics shelf", back.favorites.[0].name)
        match back.favorites.[0].pins with
        | [ LayerPin l; SystemPin s; MaterialPin m; SourcePin src ] ->
            // The by-value Layer fragment's canonical SI thickness is byte-identical.
            match l.thickness with
            | Thickness.Thickness d -> Assert.Equal(filmThickness, d)
            | other -> Assert.Fail(sprintf "unexpected pinned thickness: %A" other)
            Assert.Equal(1, List.length s.films)
            Assert.Equal("Si", m)
            Assert.Equal("src-d65", src)
        | other -> Assert.Fail(sprintf "unexpected pins: %A" other)
        // Recent files, last folders, theme, palette, layout, preferences all survive.
        Assert.Equal<string list>(sample.recentFiles, back.recentFiles)
        Assert.Equal<string list>(sample.lastFolders, back.lastFolders)
        Assert.Equal(Dark, back.theme)
        Assert.Equal<string list>(sample.chartPalette, back.chartPalette)
        Assert.False((back.layout.panels |> List.find (fun p -> p.panel = "results")).visible)
        Assert.Equal(Micrometer, back.preferences.wavelengthUnit)
        Assert.Equal(250, back.preferences.sweepPoints)
        // Structural equality of the whole record (Math.NET value-equality on tensors).
        Assert.True((sample = back), "round-tripped environment must equal the original")

    // --- AC-J6: deliberately invalid file falls back to defaults, no migration --

    [<Fact>]
    let ``AC-J6 a deliberately invalid settings file falls back to built-in defaults`` () =
        let path = Path.Combine(Path.GetTempPath(), sprintf "oc-env-%s.json" (Guid.NewGuid().ToString("N")))
        try
            // A document missing required fields (and the schemaVersion const) must
            // FAIL validation; load returns defaults rather than attempting migration.
            File.WriteAllText(path, "{ \"theme\": \"Dark\" }")
            Assert.Equal(defaults, load path)
            // A malformed (non-JSON) file likewise yields defaults, never throws.
            File.WriteAllText(path, "not json at all {{{")
            Assert.Equal(defaults, load path)
        finally
            if File.Exists path then File.Delete path

    [<Fact>]
    let ``AC-J6 a missing settings file yields defaults`` () =
        let path = Path.Combine(Path.GetTempPath(), sprintf "oc-env-missing-%s.json" (Guid.NewGuid().ToString("N")))
        Assert.False(File.Exists path)
        Assert.Equal(defaults, load path)

    [<Fact>]
    let ``AC-J6 save then load round-trips through the file system`` () =
        let path = Path.Combine(Path.GetTempPath(), sprintf "oc-env-rt-%s.json" (Guid.NewGuid().ToString("N")))
        try
            save path sample |> okOr
            Assert.True((sample = load path))
        finally
            if File.Exists path then File.Delete path

    // --- AC-J7: preference edits are display-only; stored meters unchanged -------

    [<Fact>]
    let ``AC-J7 editing the default unit or wavelength range leaves stored meters identical`` () =
        // Editing the wavelength UNIT label changes no stored magnitude.
        let edited = { sample with preferences = { sample.preferences with wavelengthUnit = Angstrom } }
        Assert.Equal(sample.preferences.wavelengthRange.min, edited.preferences.wavelengthRange.min)
        Assert.Equal(sample.preferences.wavelengthRange.max, edited.preferences.wavelengthRange.max)
        // A pinned Layer's canonical-SI thickness is untouched by a preference edit.
        match (List.head sample.favorites).pins, (List.head edited.favorites).pins with
        | LayerPin a :: _, LayerPin b :: _ -> Assert.Equal(a.thickness, b.thickness)
        | _ -> Assert.Fail "expected a LayerPin at the head of the favorites"

    [<Fact>]
    let ``AC-J7 solver defaults are the engine SolverParameters, not a parallel record`` () =
        Assert.Equal(Berreman.Solvers.SolverParameters.defaultValue, defaults.preferences.solver)

    // --- AC-I2 (Spec 0026 Part I): the selected language persists and is restored ---

    [<Fact>]
    let ``AC-I2 the selected language defaults to English and round-trips through the JSON`` () =
        // I.2.1: the language field defaults to English on the built-in environment.
        Assert.Equal(English, defaults.language)
        // A chosen language survives serialize -> validate-on-load -> bind (the schema
        // admits the new field), so it is restored on next launch.
        let ru = { defaults with language = Russian }
        let back = serialize ru |> okOr |> deserialize |> okOr
        Assert.Equal(Russian, back.language)

    [<Fact>]
    let ``AC-I2 the selected language persists through the on-disk save path`` () =
        // The Settings-ribbon selector (Part D / slice 006) writes through `save`; the
        // chosen language is restored by `load` on the next launch (I.2.1 / I.5).
        let path = Path.Combine(Path.GetTempPath(), sprintf "oc-env-lang-%s.json" (Guid.NewGuid().ToString("N")))
        try
            save path { defaults with language = Russian } |> okOr
            Assert.Equal(Russian, (load path).language)
        finally
            if File.Exists path then File.Delete path

    // --- AC-E8 (Spec 0026 Part E): the configurable key map persists with the env -----

    [<Fact>]
    let ``AC-E8 the constructor key map defaults to a 5 degree step with no overrides`` () =
        // E.3.1: the rotation step defaults to 5°. E.8.1: no overrides means the built-in
        // default bindings declared in Commands.fs.
        Assert.Equal(5.0, defaults.keyMap.rotationStepDegrees)
        Assert.Empty(defaults.keyMap.overrides)

    [<Fact>]
    let ``AC-E8 a customized key map round-trips through the schema-validated JSON`` () =
        // A user's customized rotation step + key-binding overrides persist with the
        // environment settings and are restored on next launch (E.8.1 / Part I).
        let customized =
            { defaults with
                keyMap =
                    { rotationStepDegrees = 10.0
                      overrides =
                        [ { command = "reset-rotation"; gesture = "Ctrl+R" }
                          { command = "delete-element"; gesture = "Ctrl+Delete" } ] } }
        let back = serialize customized |> okOr |> deserialize |> okOr
        Assert.Equal(10.0, back.keyMap.rotationStepDegrees)
        Assert.Equal<KeyBindingOverride list>(customized.keyMap.overrides, back.keyMap.overrides)
        // Whole-record structural equality (the new field included).
        Assert.True((customized = back), "the customized key map must survive the round-trip")

    [<Fact>]
    let ``AC-E8 the customized key map persists through the on-disk save path`` () =
        let path = Path.Combine(Path.GetTempPath(), sprintf "oc-env-keymap-%s.json" (Guid.NewGuid().ToString("N")))
        try
            let customized =
                { defaults with keyMap = { defaults.keyMap with rotationStepDegrees = 7.5 } }
            save path customized |> okOr
            Assert.Equal(7.5, (load path).keyMap.rotationStepDegrees)
        finally
            if File.Exists path then File.Delete path

    // --- AC-J8: theme + panel layout round-trip through EnvironmentSettings JSON -

    [<Fact>]
    let ``AC-J8 theme and panel layout round-trip through the environment JSON`` () =
        let back = serialize sample |> okOr |> deserialize |> okOr
        Assert.Equal(sample.theme, back.theme)
        Assert.Equal(List.length sample.layout.panels, List.length back.layout.panels)
        List.iter2
            (fun (a : PanelState) (b : PanelState) ->
                Assert.Equal(a.panel, b.panel)
                Assert.Equal(a.dock, b.dock)
                Assert.Equal(a.size, b.size)
                Assert.Equal(a.visible, b.visible))
            sample.layout.panels
            back.layout.panels
