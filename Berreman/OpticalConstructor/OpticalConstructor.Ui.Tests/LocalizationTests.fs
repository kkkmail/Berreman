/// EN/RU localization tests (Spec 0026 Part I, slice 003, gate `ui-tests`). Trait
/// `Category=ui-tests` (the `ui-tests` gate runs `--filter Category!=ui-smoke`). These
/// are pure logic tests over `OpticalConstructor.Ui.Localization` — no headless
/// Avalonia session is needed — covering:
///   * AC-I1 — UI strings resolve through the runtime-read `strings.json`; editing the
///             file changes them with no rebuild.
///   * AC-I3 — a string missing for the active language falls back to English and is
///             named in a copyable startup error; the shipped resource is complete.
///   * AC-I4 — scientific symbols (Ψ, Δ, °, R1/R2/R3, unit symbols) stay language-
///             neutral and are never carried as a translatable value.
namespace OpticalConstructor.Ui.Tests

open System
open System.IO
open Xunit
open OpticalConstructor.Ui.Localization

module LocalizationTests =

    let private okOr (r : Result<'a, LocalizationError>) : 'a =
        match r with
        | Ok v -> v
        | Error e -> failwith $"unexpected localization error: %A{e}"

    // --- AC-I1: strings resolve from the runtime resource; editing it changes them ---

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-I1 strings resolve from the runtime resource beside the assembly`` () =
        // The shipped resource loads from the runtime base directory (the build-copied
        // content item), not from compiled-in strings.
        let shipped = loadFromFile (resourcePath ()) |> okOr
        Assert.NotEmpty shipped.entries
        // A representative key resolves to its English value, and the Russian value is a
        // distinct translation — proving both pairs travel through one resource (I.1.1).
        Assert.Equal("Optical Constructor", lookup shipped English "app.title")
        Assert.NotEqual<string>(lookup shipped English "app.title", lookup shipped Russian "app.title")

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-I1 editing the resource file changes the strings with no rebuild`` () =
        // Write a temp resource and load it at run time: the loader reflects the edit,
        // so an operator/translator can change strings (or add a language) by editing
        // the file — no rebuild required (I.1.1).
        let path = Path.Combine(Path.GetTempPath(), $"""oc-strings-%s{Guid.NewGuid().ToString("N")}.json""")
        try
            File.WriteAllText(path, "{ \"app.title\": { \"en\": \"Edited Title\", \"ru\": \"Изменённый заголовок\" } }")
            let edited = loadFromFile path |> okOr
            Assert.Equal("Edited Title", lookup edited English "app.title")
            Assert.Equal("Изменённый заголовок", lookup edited Russian "app.title")
        finally
            if File.Exists path then File.Delete path

    // --- AC-I3: missing string -> English fallback + copyable error; shipped complete --

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-I3 a string missing for the active language falls back to English`` () =
        // A resource complete in EN but missing the RU value for one key.
        let json =
            "{ \"menu.file\": { \"en\": \"File\", \"ru\": \"Файл\" }, \"menu.help\": { \"en\": \"Help\" } }"
        let resource = parse json |> okOr
        // The active language (Russian) lacks "menu.help" -> the English text is shown;
        // a present RU value ("menu.file") is still used.
        Assert.Equal("Help", lookup resource Russian "menu.help")
        Assert.Equal("Файл", lookup resource Russian "menu.file")
        // An unknown key falls back to the key itself rather than throwing (never crashes).
        Assert.Equal("does.not.exist", lookup resource Russian "does.not.exist")

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-I3 the completeness check reports the missing key in a copyable message`` () =
        let json =
            "{ \"menu.file\": { \"en\": \"File\", \"ru\": \"Файл\" }, \"menu.help\": { \"en\": \"Help\" } }"
        let resource = parse json |> okOr
        // Russian is incomplete: "menu.help" is reported and named in a non-empty,
        // copyable message (so it can be pasted into a bug report / translation fix).
        let ru = check resource Russian
        Assert.Equal<string list>([ "menu.help" ], ru.missingKeys)
        Assert.False(String.IsNullOrWhiteSpace ru.message)
        Assert.Contains("menu.help", ru.message)
        // English is complete here -> no missing keys, empty message (never a false alarm).
        let en = check resource English
        Assert.Empty en.missingKeys
        Assert.Equal("", en.message)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-I3 the shipped resource is complete, so startup raises no error`` () =
        // The shipped strings.json carries every key for BOTH languages, so the startup
        // completeness check surfaces no copyable error for either — and, in turn, the
        // headless smoke host never constructs the error window.
        let resource = loadFromFile (resourcePath ()) |> okOr
        Assert.Empty (check resource English).missingKeys
        Assert.Empty (check resource Russian).missingKeys
        Assert.Equal(None, startupCheck English)
        Assert.Equal(None, startupCheck Russian)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-I3 a missing resource file is surfaced as a copyable message, not a crash`` () =
        // A missing resource is a startup error the entry point can show copyably, never
        // an exception across the boundary (I.3.1).
        let missing = Path.Combine(Path.GetTempPath(), $"""oc-missing-%s{Guid.NewGuid().ToString("N")}.json""")
        Assert.False(File.Exists missing)
        match loadFromFile missing with
        | Error (ResourceMissing p) -> Assert.Equal(missing, p)
        | other -> Assert.Fail($"expected ResourceMissing, got %A{other}")
        // A malformed file is likewise a returned parse error, not a thrown exception.
        let bad = Path.Combine(Path.GetTempPath(), $"""oc-bad-%s{Guid.NewGuid().ToString("N")}.json""")
        try
            File.WriteAllText(bad, "not json at all {{{")
            match loadFromFile bad with
            | Error (ResourceParseError _) -> ()
            | other -> Assert.Fail($"expected ResourceParseError, got %A{other}")
        finally
            if File.Exists bad then File.Delete bad

    // --- AC-I4: scientific symbols stay language-neutral and are never translated ------

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-I4 scientific symbols are language-neutral`` () =
        // Every neutral glyph resolves identically regardless of the active language —
        // it is a compiled constant, never read from the translatable resource (I.4.1).
        Assert.Contains("Ψ", Symbols.neutral)
        Assert.Contains("Δ", Symbols.neutral)
        Assert.Contains("°", Symbols.neutral)
        Assert.Contains("R1", Symbols.neutral)
        Assert.Contains("R2", Symbols.neutral)
        Assert.Contains("R3", Symbols.neutral)
        for g in Symbols.neutral do
            Assert.Equal(g, neutralSymbol English g)
            Assert.Equal(neutralSymbol English g, neutralSymbol Russian g)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-I4 the shipped resource never carries a neutral symbol as a translatable value`` () =
        // The shipped strings.json must not smuggle a scientific symbol into the
        // translatable set, so a translation pass cannot localize Ψ/Δ/°/R1-R3/unit
        // symbols (I.4.1). No entry's value equals a neutral glyph.
        let resource = loadFromFile (resourcePath ()) |> okOr
        let neutral = Set.ofList Symbols.neutral
        for KeyValue(key, byLang) in resource.entries do
            for KeyValue(lang, value) in byLang do
                Assert.False(
                    neutral.Contains value,
                    $"key '%s{key}' (%s{lang}) carries the language-neutral symbol '%s{value}' as a translatable value")

    // --- Spec 0038 Part B.1 (step 002): the loader/lookup edge contract. The retired
    // --- shell's startup-error WINDOW is gone; the Localization module itself stays
    // --- live (UserEnvironment reads it), so its documented tolerance for a partially
    // --- malformed, hand-edited resource is pinned here.

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``a non-object entry is skipped while the rest of the resource stays usable`` () =
        // I.1.1: a hand-edited file with one broken entry still yields every entry it
        // can — the broken key resolves to itself (visible, never a crash).
        let resource = parse "{ \"menu.file\": { \"en\": \"File\" }, \"broken\": 5 }" |> okOr
        Assert.Equal("File", lookup resource English "menu.file")
        Assert.Equal("broken", lookup resource English "broken")

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``a non-string language value is skipped, falling back like a missing translation`` () =
        let resource = parse "{ \"menu.file\": { \"en\": \"File\", \"ru\": 5 } }" |> okOr
        // The RU value is not a string → treated as missing: English text shows, and
        // the completeness check names the key for Russian.
        Assert.Equal("File", lookup resource Russian "menu.file")
        Assert.Equal<string list>([ "menu.file" ], (check resource Russian).missingKeys)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``a non-object resource root is a typed parse error`` () =
        match parse "[ 1, 2, 3 ]" with
        | Error (ResourceParseError _) -> ()
        | other -> Assert.Fail($"expected ResourceParseError, got %A{other}")

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``an empty active-language value falls back to the English text`` () =
        // I.3.1: empty means missing — a blank string must never reach the UI.
        let resource = parse "{ \"k\": { \"en\": \"Value\", \"ru\": \"\" } }" |> okOr
        Assert.Equal("Value", lookup resource Russian "k")

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``when even the English value is empty the key itself shows`` () =
        let resource = parse "{ \"k\": { \"en\": \"\", \"ru\": \"\" } }" |> okOr
        Assert.Equal("k", lookup resource Russian "k")
        Assert.Equal("k", lookup resource English "k")

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``the completeness report lists the missing keys sorted`` () =
        let resource = parse "{ \"z.key\": { \"en\": \"Z\" }, \"a.key\": { \"en\": \"A\" } }" |> okOr
        Assert.Equal<string list>([ "a.key"; "z.key" ], (check resource Russian).missingKeys)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``a missing-resource error message names the resource file and its path`` () =
        let message = describeError (ResourceMissing @"C:\nowhere\strings.json")
        Assert.Contains(resourceFileName, message)
        Assert.Contains(@"C:\nowhere\strings.json", message)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``a parse-error message carries the underlying reason copyably`` () =
        let message = describeError (ResourceParseError "unexpected token at line 3")
        Assert.Contains(resourceFileName, message)
        Assert.Contains("unexpected token at line 3", message)
