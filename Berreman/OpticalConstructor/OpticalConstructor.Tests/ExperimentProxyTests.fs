namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Experiments

/// Spec 0027 (024) Phase 2 — pure domain tests for the Experiments: the single-case `Experiment` DU
/// (which element's R1 sweeps the full circle, by `ElementId`), the `ExperimentSet` shape, the
/// mock `listExperimentSets` / `tryGetExperimentSet`, and the functional-proxy stub seam.
module ExperimentProxyTests =

    let private proxy = Experiments.createInMemory ()

    [<Fact>]
    let ``RotateR1FullCircle reports the swept element by id`` () =
        let id = Library.elementId "x"
        let exp = RotateR1FullCircle id
        Assert.Equal<ElementId>(id, exp.sweptElement)

    [<Fact>]
    let ``the experiment description names the swept element id`` () =
        let exp = RotateR1FullCircle (Library.elementId "analyzer")
        Assert.Contains("analyzer", exp.description)
        Assert.Contains("360", exp.description)

    [<Fact>]
    let ``listExperimentSets returns at least one set`` () =
        match proxy.listExperimentSets () with
        | Ok sets -> Assert.NotEmpty sets
        | Error err -> Assert.Fail(sprintf "%A" err)

    [<Fact>]
    let ``tryGetExperimentSet hits a known id and misses an unknown id`` () =
        match proxy.tryGetExperimentSet "exp-rotate-analyzer" with
        | Ok (Some s) -> Assert.Equal("exp-rotate-analyzer", s.id)
        | other -> Assert.Fail(sprintf "expected the rotate-analyzer set, got %A" other)
        match proxy.tryGetExperimentSet "no-such-set" with
        | Ok None -> ()
        | other -> Assert.Fail(sprintf "expected Ok None, got %A" other)

    [<Fact>]
    let ``every seeded experiment set has at least one (setup-steps, experiment) step`` () =
        match proxy.listExperimentSets () with
        | Ok sets ->
            Assert.All(sets, fun s -> Assert.NotEmpty s.steps)
            // Every step's experiment is a RotateR1FullCircle (the only kind now); its swept element is a
            // non-empty id.
            for s in sets do
                for (_, exp) in s.steps do
                    match exp with
                    | RotateR1FullCircle id -> Assert.False(System.String.IsNullOrEmpty id.value)
        | Error err -> Assert.Fail(sprintf "%A" err)

    [<Fact>]
    let ``an ExperimentSet round-trips its setup-step / experiment shape`` () =
        let input = Library.elementId "input"
        let analyzer = Library.elementId "analyzer"
        let set =
            {
                id = "custom"
                name = "Custom"
                steps = [ ([ SetElementR1 (input, 45.0) ], RotateR1FullCircle analyzer) ]
            }
        match set.steps with
        | [ ([ SetElementR1 (el, deg) ], RotateR1FullCircle swept) ] ->
            Assert.Equal<ElementId>(input, el)
            Assert.Equal(45.0, deg)
            Assert.Equal<ElementId>(analyzer, swept)
        | other -> Assert.Fail(sprintf "unexpected steps shape: %A" other)

    [<Fact>]
    let ``a STUB proxy of the same shape drives the same listing logic`` () =
        // The functional-proxy seam: a test substitutes in-memory stub functions for the proxy fields.
        let only =
            {
                id = "stub-set"
                name = "Stub set"
                steps = [ ([], RotateR1FullCircle (Library.elementId "stub-el")) ]
            }
        let stub : ExperimentProxy =
            {
                listExperimentSets = fun () -> Ok [ only ]
                tryGetExperimentSet = fun id -> Ok (if id = "stub-set" then Some only else None)
            }
        match stub.listExperimentSets () with
        | Ok [ s ] -> Assert.Equal("stub-set", s.id)
        | other -> Assert.Fail(sprintf "%A" other)
        match stub.tryGetExperimentSet "stub-set" with
        | Ok (Some s) -> Assert.Equal("stub-set", s.id)
        | other -> Assert.Fail(sprintf "%A" other)
        match stub.tryGetExperimentSet "nope" with
        | Ok None -> ()
        | other -> Assert.Fail(sprintf "%A" other)
