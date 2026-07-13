namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain.Facets

/// Spec 0038 Part D (step 009) — the generic faceted-navigation engine, proven
/// over a FIXED six-item corpus with a multi-valued discrete facet (kind), a
/// single-valued discrete facet (family), a dependent numeric facet (thickness,
/// inapplicable where absent), and the plain-text name filter as an ordinary
/// constraint. Pure construction throughout: no window, no IO, no solver.
module FacetsTests =

    /// The test item shape. `kinds` is multi-valued (an item can carry several,
    /// or none, or the same one twice); `thicknessNm` drives the dependent
    /// numeric facet (inapplicable when `None`).
    type TestEntry =
        {
            entryName : string
            kinds : string list
            family : string
            thicknessNm : double option
        }

    let private alpha = { entryName = "alpha"; kinds = [ "red" ]; family = "glass"; thicknessNm = Some 10.0 }
    let private beta = { entryName = "beta"; kinds = [ "red"; "blue" ]; family = "glass"; thicknessNm = Some 20.0 }
    let private gamma = { entryName = "gamma"; kinds = [ "blue" ]; family = "metal"; thicknessNm = Some 20.0 }
    let private delta = { entryName = "delta"; kinds = [ "green" ]; family = "metal"; thicknessNm = None }
    let private epsilon = { entryName = "epsilon"; kinds = []; family = "glass"; thicknessNm = None }
    // Upper-case name (the case-insensitivity probe) and a duplicated kind
    // (the at-most-once-per-branch probe).
    let private alphaPrime = { entryName = "ALPHA-PRIME"; kinds = [ "red"; "red" ]; family = "polymer"; thicknessNm = Some 500.0 }

    let private corpus : TestEntry list = [ alpha; beta; gamma; delta; epsilon; alphaPrime ]

    let private familyKey = AttributeKey "family"
    let private kindKey = AttributeKey "kind"
    let private thicknessKey = AttributeKey "thickness"
    let private nameFilterKey = AttributeKey "name-filter"

    let private familyDef : AttributeDef<TestEntry> =
        {
            key = familyKey
            name = "Family"
            kind = DiscreteAttribute
            appliesTo = fun _ -> ApplicableAttribute
            extract = fun e -> [ DiscreteValue (DiscreteKey e.family) ]
        }

    let private kindDef : AttributeDef<TestEntry> =
        {
            key = kindKey
            name = "Kind"
            kind = DiscreteAttribute
            appliesTo = fun _ -> ApplicableAttribute
            extract = fun e -> e.kinds |> List.map (fun k -> DiscreteValue (DiscreteKey k))
        }

    let private thicknessDef : AttributeDef<TestEntry> =
        {
            key = thicknessKey
            name = "Thickness"
            kind = NumericAttribute
            appliesTo =
                fun e ->
                    match e.thicknessNm with
                    | Some _ -> ApplicableAttribute
                    | None -> InapplicableAttribute
            extract =
                fun e ->
                    match e.thicknessNm with
                    | Some t -> [ NumericValue t ]
                    | None -> []
        }

    let private defs : AttributeDef<TestEntry> list = [ familyDef; kindDef; thicknessDef ]

    let private discrete (key : AttributeKey) (values : string list) : AppliedConstraint =
        { key = key; selection = DiscreteSelection (values |> List.map DiscreteKey |> Set.ofList) }

    let private numericRange (key : AttributeKey) (lower : double) (upper : double) : AppliedConstraint =
        { key = key; selection = NumericRangeSelection { lower = lower; upper = upper } }

    let private discreteBranch (key : string) (count : int) : FacetBranch =
        { value = DiscreteValue (DiscreteKey key); label = key; count = ItemCount count }

    let private numericBranch (v : double) (label : string) (count : int) : FacetBranch =
        { value = NumericValue v; label = label; count = ItemCount count }

    // ---- filter ----

    [<Fact>]
    let ``filter with no applied constraints returns the whole corpus in order`` () =
        Assert.Equal<TestEntry list>(corpus, filter defs [] corpus)

    [<Fact>]
    let ``a discrete selection ORs within its facet: any selected key matches`` () =
        let result = filter defs [ discrete kindKey [ "red"; "blue" ] ] corpus
        Assert.Equal<TestEntry list>([ alpha; beta; gamma; alphaPrime ], result)

    [<Fact>]
    let ``applied constraints AND across facets`` () =
        let result = filter defs [ discrete kindKey [ "red" ]; discrete familyKey [ "glass" ] ] corpus
        Assert.Equal<TestEntry list>([ alpha; beta ], result)

    [<Fact>]
    let ``a constraint on an attribute excludes every item the attribute is inapplicable to`` () =
        // delta and epsilon carry no thickness — the dependent facet's
        // constraint can never admit them.
        let result = filter defs [ numericRange thicknessKey 0.0 1000.0 ] corpus
        Assert.Equal<TestEntry list>([ alpha; beta; gamma; alphaPrime ], result)

    [<Fact>]
    let ``numeric range membership is half-open: lower in, upper out`` () =
        let result = filter defs [ numericRange thicknessKey 10.0 20.0 ] corpus
        Assert.Equal<TestEntry list>([ alpha ], result)

    [<Fact>]
    let ``a degenerate numeric range selects exactly its value`` () =
        let result = filter defs [ numericRange thicknessKey 20.0 20.0 ] corpus
        Assert.Equal<TestEntry list>([ beta; gamma ], result)

    [<Fact>]
    let ``an inverted numeric range selects nothing`` () =
        Assert.Equal<TestEntry list>([], filter defs [ numericRange thicknessKey 30.0 10.0 ] corpus)

    [<Fact>]
    let ``a constraint whose key has no def matches nothing (fail-closed)`` () =
        let result = filter defs [ discrete (AttributeKey "nonexistent") [ "x" ] ] corpus
        Assert.Equal<TestEntry list>([], result)

    // ---- countFor ----

    [<Fact>]
    let ``countFor previews one candidate constraint under the applied set`` () =
        let applied = [ discrete familyKey [ "glass" ] ]
        // glass = alpha, beta, epsilon; of those only beta carries blue.
        Assert.Equal(1, (countFor defs applied (discrete kindKey [ "blue" ]) corpus).value)
        Assert.Equal(2, (countFor defs applied (discrete kindKey [ "red" ]) corpus).value)

    [<Fact>]
    let ``countFor over an empty applied set counts the candidate alone`` () =
        Assert.Equal(3, (countFor defs [] (discrete kindKey [ "red" ]) corpus).value)

    // ---- breadcrumb after-counts ----

    [<Fact>]
    let ``breadcrumb after-counts are empty for an empty applied set`` () =
        Assert.Equal<BreadcrumbCount list>([], breadcrumbCounts defs [] corpus)

    [<Fact>]
    let ``breadcrumb after-counts are cumulative in application order`` () =
        let applied = [ discrete kindKey [ "red"; "blue" ]; discrete familyKey [ "metal" ] ]
        let crumbs = breadcrumbCounts defs applied corpus
        Assert.Equal<AppliedConstraint list>(applied, crumbs |> List.map (fun c -> c.applied))
        // red|blue = alpha, beta, gamma, alphaPrime (4); of those metal = gamma (1).
        Assert.Equal<int list>([ 4; 1 ], crumbs |> List.map (fun c -> c.afterCount.value))

    [<Fact>]
    let ``breadcrumb after-counts follow application order, not facet identity`` () =
        let applied = [ discrete familyKey [ "metal" ]; discrete kindKey [ "red"; "blue" ] ]
        let crumbs = breadcrumbCounts defs applied corpus
        // metal = gamma, delta (2); of those red|blue = gamma (1).
        Assert.Equal<int list>([ 2; 1 ], crumbs |> List.map (fun c -> c.afterCount.value))

    // ---- buildTree ----

    [<Fact>]
    let ``buildTree over the unconstrained corpus carries per-branch counts in representation order`` () =
        let tree = buildTree (Representation [ familyKey; kindKey; thicknessKey ]) defs [] corpus
        let expected =
            {
                facets =
                    [
                        {
                            key = familyKey
                            name = "Family"
                            branches = [ discreteBranch "glass" 3; discreteBranch "metal" 2; discreteBranch "polymer" 1 ]
                        }
                        {
                            key = kindKey
                            name = "Kind"
                            branches = [ discreteBranch "blue" 2; discreteBranch "green" 1; discreteBranch "red" 3 ]
                        }
                        {
                            key = thicknessKey
                            name = "Thickness"
                            branches = [ numericBranch 10.0 "10" 1; numericBranch 20.0 "20" 2; numericBranch 500.0 "500" 1 ]
                        }
                    ]
            }
        Assert.Equal<FacetTree>(expected, tree)

    [<Fact>]
    let ``multi-valued attributes count per branch, so branch counts can exceed the total`` () =
        let applied = [ discrete kindKey [ "red"; "blue" ] ]
        let total = filter defs applied corpus |> List.length
        let tree = buildTree (Representation [ kindKey ]) defs applied corpus
        let kindBranches =
            match tree.facets with
            | [ node ] -> node.branches
            | other -> failwith $"expected exactly the kind facet, got %A{other}"
        // beta carries red AND blue, so it counts once in EACH branch.
        Assert.Equal<FacetBranch list>([ discreteBranch "blue" 2; discreteBranch "red" 3 ], kindBranches)
        Assert.Equal(4, total)
        Assert.True((kindBranches |> List.sumBy (fun b -> b.count.value)) > total)

    [<Fact>]
    let ``an item extracting the same value twice counts once in that branch`` () =
        let tree = buildTree (Representation [ kindKey ]) defs [] [ alphaPrime ]
        let expected =
            {
                facets = [ { key = kindKey; name = "Kind"; branches = [ discreteBranch "red" 1 ] } ]
            }
        Assert.Equal<FacetTree>(expected, tree)

    [<Fact>]
    let ``zero-count branches are absent: values filtered out of the population offer no branch`` () =
        let tree = buildTree (Representation [ kindKey ]) defs [ discrete familyKey [ "metal" ] ] corpus
        let expected =
            {
                facets =
                    [
                        {
                            key = kindKey
                            name = "Kind"
                            // metal = gamma, delta — "red" exists in the corpus but
                            // in no filtered item, so no red branch appears.
                            branches = [ discreteBranch "blue" 1; discreteBranch "green" 1 ]
                        }
                    ]
            }
        Assert.Equal<FacetTree>(expected, tree)

    [<Fact>]
    let ``an attribute inapplicable to every filtered item is omitted entirely`` () =
        let applied = [ discrete familyKey [ "metal" ]; discrete kindKey [ "green" ] ]
        let tree = buildTree (Representation [ familyKey; kindKey; thicknessKey ]) defs applied corpus
        // Only delta survives; it carries no thickness, so the thickness facet
        // vanishes entirely — no empty node, no zero branches.
        Assert.Equal<AttributeKey list>([ familyKey; kindKey ], tree.facets |> List.map (fun f -> f.key))

    [<Fact>]
    let ``an applicable attribute extracting no values yields no facet node`` () =
        // epsilon's kind facet applies but extracts [] — an empty facet header
        // is noise, so the node is omitted (recorded step-009 choice).
        let tree = buildTree (Representation [ kindKey; familyKey ]) defs [] [ epsilon ]
        let expected =
            {
                facets = [ { key = familyKey; name = "Family"; branches = [ discreteBranch "glass" 1 ] } ]
            }
        Assert.Equal<FacetTree>(expected, tree)

    [<Fact>]
    let ``facets follow representation order, never constraint application order`` () =
        let tree = buildTree (Representation [ thicknessKey; familyKey ]) defs [ discrete familyKey [ "glass" ] ] corpus
        let expected =
            {
                facets =
                    [
                        {
                            key = thicknessKey
                            name = "Thickness"
                            branches = [ numericBranch 10.0 "10" 1; numericBranch 20.0 "20" 1 ]
                        }
                        {
                            key = familyKey
                            name = "Family"
                            branches = [ discreteBranch "glass" 3 ]
                        }
                    ]
            }
        Assert.Equal<FacetTree>(expected, tree)

    [<Fact>]
    let ``a representation key with no def is skipped`` () =
        let tree = buildTree (Representation [ AttributeKey "nonexistent"; familyKey ]) defs [] [ alpha ]
        Assert.Equal<AttributeKey list>([ familyKey ], tree.facets |> List.map (fun f -> f.key))

    [<Fact>]
    let ``buildTree orders branches case-insensitively by display label, overriding the ordinal value order`` () =
        // Mixed-case discrete keys whose case-SENSITIVE ordinal order (the old
        // structural value sort — 'B'=66 < 'Z'=90 < 'a'=97, so "Banana"; "Zinc";
        // "apple") DISAGREES with the required case-INSENSITIVE label order
        // (operator 010/Q1). The probe a value sort could never pass.
        let labelKey = AttributeKey "label"
        let labelDef : AttributeDef<TestEntry> =
            {
                key = labelKey
                name = "Label"
                kind = DiscreteAttribute
                appliesTo = fun _ -> ApplicableAttribute
                extract = fun e -> [ DiscreteValue (DiscreteKey e.family) ]
            }
        let items =
            [
                { entryName = "z"; kinds = []; family = "Zinc"; thicknessNm = None }
                { entryName = "a"; kinds = []; family = "apple"; thicknessNm = None }
                { entryName = "b"; kinds = []; family = "Banana"; thicknessNm = None }
            ]
        let tree = buildTree (Representation [ labelKey ]) [ labelDef ] [] items
        let labels =
            match tree.facets with
            | [ node ] -> node.branches |> List.map (fun b -> b.label)
            | other -> failwith $"expected exactly the label facet, got %A{other}"
        Assert.Equal<string list>([ "apple"; "Banana"; "Zinc" ], labels)

    // ---- the plain-text filter as an ordinary constraint ----

    let private nameDef (query : string) : AttributeDef<TestEntry> =
        textFilterDef nameFilterKey "Name" (fun e -> e.entryName) (TextQuery query)

    [<Fact>]
    let ``the text filter matches case-insensitively over the supplied extractor`` () =
        let result = filter (defs @ [ nameDef "alpha" ]) [ textFilterConstraint nameFilterKey ] corpus
        Assert.Equal<TestEntry list>([ alpha; alphaPrime ], result)

    [<Fact>]
    let ``an empty text query matches every item`` () =
        let result = filter (defs @ [ nameDef "" ]) [ textFilterConstraint nameFilterKey ] corpus
        Assert.Equal<TestEntry list>(corpus, result)

    [<Fact>]
    let ``the text filter ANDs with facet constraints like any other constraint`` () =
        let allDefs = defs @ [ nameDef "alpha" ]
        let applied = [ textFilterConstraint nameFilterKey; discrete familyKey [ "glass" ] ]
        Assert.Equal<TestEntry list>([ alpha ], filter allDefs applied corpus)

    [<Fact>]
    let ``the text filter takes a breadcrumb after-count like any other constraint`` () =
        let allDefs = defs @ [ nameDef "alpha" ]
        let applied = [ textFilterConstraint nameFilterKey; discrete familyKey [ "glass" ] ]
        let crumbs = breadcrumbCounts allDefs applied corpus
        // "alpha" matches alpha and ALPHA-PRIME (2); of those glass = alpha (1).
        Assert.Equal<int list>([ 2; 1 ], crumbs |> List.map (fun c -> c.afterCount.value))

    [<Fact>]
    let ``countFor previews a facet candidate under an applied text filter`` () =
        let allDefs = defs @ [ nameDef "alpha" ]
        let applied = [ textFilterConstraint nameFilterKey ]
        Assert.Equal(1, (countFor allDefs applied (discrete familyKey [ "polymer" ]) corpus).value)
