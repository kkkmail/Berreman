namespace OpticalConstructor.Tests

open Berreman.Media
open OpticalConstructor.Domain.BeamTree
open OpticalConstructor.Domain.Project
open OpticalConstructor.Storage
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Ui
open Xunit

/// §J.3 stack-template tests (slice 014, AC-J3). Prove each of the six code-defined
/// templates produces an `OpticalConstructorProject` that passes the §A.7 project
/// schema THROUGH the schema-validated open path, and that the DBR template's
/// periodic films are exactly `RepeatBuilder.expand` output (not a duplicated loop).
module TemplatesTests =

    let private okOr (r : Result<'a, StorageError>) : 'a =
        match r with
        | Ok v -> v
        | Error e -> failwith $"unexpected storage error: {e}"

    [<Fact>]
    let ``AC-J3 every template loads through the schema-validated open path`` () =
        // `loadTemplate` serializes the literal project and binds it back through
        // `ProjectJson.deserializeProject` — the validate-on-load core `openProject`
        // also runs — so §A.7 schema validation applies to every template uniformly.
        Assert.Equal(6, List.length Templates.all)
        for entry in Templates.all do
            let project = Templates.loadTemplate entry.build |> okOr
            // A valid bound project: a sample root over a non-empty films stack.
            match project.beamTree.root.element with
            | Sample _ -> ()
            | other -> failwith $"template '{entry.title}' root is not a Sample: {other}"
            Assert.False(List.isEmpty project.systems)

    [<Fact>]
    let ``AC-J3 each template's raw factory is a pure unit -> project`` () =
        // The factory is a literal: calling it twice yields equal projects (no disk,
        // no hidden state).
        for entry in Templates.all do
            Assert.Equal(entry.build (), entry.build ())

    [<Fact>]
    let ``AC-J3 the DBR template's films are exactly RepeatBuilder.expand output`` () =
        let project = Templates.dbr ()
        let expected = RepeatBuilder.expand Templates.dbrCell Templates.dbrPeriods
        Assert.Equal<Layer list>(expected, project.beamTree.root.system.films)
        // The period structure flattened to ordinary films: N*R layers.
        Assert.Equal(List.length Templates.dbrCell * Templates.dbrPeriods, List.length project.beamTree.root.system.films)

    [<Fact>]
    let ``AC-J3 a malformed template document is rejected on the open path`` () =
        // Sanity that the open path actually validates: corrupting the serialized
        // template's incident refractionIndex makes validate-on-load reject it.
        let json = Templates.arCoating () |> ProjectJson.serializeProject |> okOr
        let bad = json.Replace("\"refractionIndex\": 1", "\"refractionIndex\": \"nope\"")
        match ProjectJson.deserializeProject bad with
        | Error (SchemaValidationError _) -> ()
        | other -> failwith $"expected SchemaValidationError, got: {other}"
