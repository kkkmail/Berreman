namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Ui.TableAndElementRotationView

/// Spec 0040 Part D.3 (step 012): a placed sample's default `Placement.emission` derives from the
/// SAMPLE it is bound to (its geometry-constrained `supportedEmission`, step 009) rather than the
/// generic per-kind `Placement.defaultEmission`, and the experiment's default `MeasurementMode`
/// follows through `MeasurementMode.ofEmission`. This module covers both the pure Domain rule
/// (`Library.placementEmissionForEntry`) and the end-to-end Ui MVU drive (place a Sample, bind it,
/// choose it to vary) so the placed element AND its experiment default land on the right values —
/// a ThinFilm defaults to reflected-only / `CaptureReflected`, a Plate to both / `CaptureBoth`.
module PlacedSampleEmissionTests =

    // ---- the seeded library the Main scene binds against (a ThinFilm + a Plate seed) ----

    let private lib : LibraryProxy = Library.createInMemory ()

    /// Resolve a seeded entry by its id (the entry id is the `valueId` written onto a bound element).
    let private entry (id : string) : LibraryEntry =
        match lib.tryGetEntry id with
        | Ok (Some e) -> e
        | other -> failwith $"seed entry '%s{id}' unexpectedly not resolvable: %A{other}"

    /// A seeded ThinFilm sample (`supportedEmission = EmitReflectedOnly`, step 009).
    let private thinFilmId : string = (SampleItem SeedSamples.glassFilm600).entryId
    /// A seeded Plate sample supporting BOTH branches (`supportedEmission = EmitBoth`).
    let private plateId : string = (SampleItem SeedSamples.glassPlate1mm).entryId
    /// A non-sample seeded entry (the 600 nm source) — carries no geometry emission constraint.
    let private sourceId : string = "src-600"

    // ============================ pure Domain: placementEmissionForEntry ============================

    [<Fact>]
    let ``a bound ThinFilm sample bounds the placement to its reflected-only supportedEmission`` () =
        // The fallback (the generic per-kind default) is IGNORED for a sample — its geometry wins.
        Assert.Equal<Emission>(EmitReflectedOnly, placementEmissionForEntry EmitBoth (entry thinFilmId))

    [<Fact>]
    let ``a bound Plate sample supporting both bounds the placement to EmitBoth`` () =
        Assert.Equal<Emission>(EmitBoth, placementEmissionForEntry EmitReflectedOnly (entry plateId))

    [<Fact>]
    let ``a non-sample entry carries no geometry constraint — the placement keeps its kind fallback`` () =
        // Both fallbacks pass through unchanged, proving it is genuinely the fallback (not a constant).
        Assert.Equal<Emission>(EmitBoth, placementEmissionForEntry EmitBoth (entry sourceId))
        Assert.Equal<Emission>(EmitReflectedOnly, placementEmissionForEntry EmitReflectedOnly (entry sourceId))

    [<Fact>]
    let ``the experiment default measurement follows the bound sample's emission`` () =
        // The composed rule the host relies on: MeasurementMode.ofEmission over the placement emission.
        Assert.Equal(CaptureReflected, MeasurementMode.ofEmission (placementEmissionForEntry EmitBoth (entry thinFilmId)))
        Assert.Equal(CaptureBoth, MeasurementMode.ofEmission (placementEmissionForEntry EmitReflectedOnly (entry plateId)))

    // ============================ end-to-end Ui MVU drive ============================

    /// Select the element at index `i`, then bind it to a Library entry (the `BindValueId` MVU path,
    /// which converges on the one targeted `BindValueIdTo` commit arm).
    let private bind (i : int) (entryId : string) (m : Model) : Model =
        { m with selection = ElementSelected i } |> update (BindValueId entryId)

    let private emissionOf (i : int) (m : Model) : Emission = (List.item i m.elements).placement.emission
    let private idOf (i : int) (m : Model) : string = (List.item i m.elements).id.value

    [<Fact>]
    let ``a placed ThinFilm sample defaults to reflected-only and its experiment to CaptureReflected`` () =
        // A sample is placed UNBOUND (the inverse hook) then bound; the bind seeds the emission.
        let m = initMain () |> update (AddElement Sample) |> bind 2 thinFilmId
        Assert.Equal<Emission>(EmitReflectedOnly, emissionOf 2 m)
        let chosen = m |> update (ExpChooseElement (idOf 2 m))
        Assert.Equal(CaptureReflected, chosen.experimentCollection.draft.measurement)

    [<Fact>]
    let ``a placed Plate sample supporting both defaults to EmitBoth and its experiment to CaptureBoth`` () =
        let m = initMain () |> update (AddElement Sample) |> bind 2 plateId
        Assert.Equal<Emission>(EmitBoth, emissionOf 2 m)
        let chosen = m |> update (ExpChooseElement (idOf 2 m))
        Assert.Equal(CaptureBoth, chosen.experimentCollection.draft.measurement)

    [<Fact>]
    let ``binding a placed sample from a ThinFilm to a Plate re-seeds the placed emission`` () =
        // The single seed point runs on every bind, so re-binding a placed sample tracks the new
        // geometry rather than leaving the first bind's emission stale.
        let m = initMain () |> update (AddElement Sample) |> bind 2 thinFilmId
        Assert.Equal<Emission>(EmitReflectedOnly, emissionOf 2 m)
        let rebound = m |> bind 2 plateId
        Assert.Equal<Emission>(EmitBoth, emissionOf 2 rebound)
