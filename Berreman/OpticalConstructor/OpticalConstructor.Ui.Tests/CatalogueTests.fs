/// Spec 0027 / spec 0038 Part B.1 (step 002): the optical-element catalogue contract
/// the Main "Lego" scene, the palette buttons and the shared element renderer all key
/// off (`Catalogue.kindName` / `kindCode` / `opticalSign`). Pure lookups, gate
/// `ui-tests`: the short codes are the palette/add ids and the on-canvas labels, so
/// each mapping is pinned by value and the code/name sets are pinned collision-free.
namespace OpticalConstructor.Ui.Tests

open Xunit
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Ui

module CatalogueTests =

    /// Every catalogue kind (the full `CatalogueKind` DU, in catalogue order).
    let private allKinds : CatalogueKind list =
        [ LightSource; LinearPolarizer; CircularPolarizer; Sample; Lens; FlatMirror; CurvedMirror; Detector ]

    /// Resolve a kind from its short code, asserting the code is unambiguous.
    let private kindOfCode (code : string) : CatalogueKind =
        match allKinds |> List.filter (fun k -> Catalogue.kindCode k = code) with
        | [ k ] -> k
        | matches -> failwith $"catalogue code '{code}' matched {List.length matches} kinds"

    [<Theory>]
    [<InlineData("S", "Light source")>]
    [<InlineData("LP", "Linear polarizer")>]
    [<InlineData("CP", "Circular polarizer")>]
    [<InlineData("Sa", "Sample")>]
    [<InlineData("L", "Lens")>]
    [<InlineData("FM", "Flat mirror")>]
    [<InlineData("CM", "Curved mirror")>]
    [<InlineData("D", "Detector")>]
    let ``each catalogue code resolves exactly one kind carrying its display name`` (code : string) (name : string) =
        Assert.Equal(name, Catalogue.kindName (kindOfCode code))

    [<Fact>]
    let ``the short codes are unique across the catalogue`` () =
        let codes = allKinds |> List.map Catalogue.kindCode
        Assert.Equal(List.length codes, codes |> List.distinct |> List.length)

    [<Fact>]
    let ``the display names are unique across the catalogue`` () =
        let names = allKinds |> List.map Catalogue.kindName
        Assert.Equal(List.length names, names |> List.distinct |> List.length)

    [<Fact>]
    let ``the optical sign is converging exactly for the curved optics`` () =
        // +1 (a spherical cap) for lenses and curved mirrors; 0 (a plain cylinder)
        // for everything else — the shape renderer's branching contract.
        for k in allKinds do
            match k with
            | Lens | CurvedMirror -> Assert.Equal(1, Catalogue.opticalSign k)
            | _ -> Assert.Equal(0, Catalogue.opticalSign k)
