namespace OpticalConstructor.Tests

open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Ui.TableAndElementRotationView

/// Successful scene construction for tests; initialization failures fail the calling fact (spec 0050 .manual/010).
module SceneTestFixture =

    let kitSuccess (result : Result<'Item, KitError>) : 'Item =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let mainScene () : TableAndElementRotationModel = initMain () |> kitSuccess

    let staticScene () : TableAndElementRotationModel = initTableAndElementRotation () |> kitSuccess

    let mainSceneWith
        (library : Library.LibraryProxy)
        (experiments : Experiments.ExperimentProxy)
        (materials : MaterialLibrary.MaterialProxy)
        (samples : Library.SampleProxy)
        (categories : MaterialLibrary.CategoryProxy) : TableAndElementRotationModel =
        initMainWith library experiments materials samples categories |> kitSuccess

    let inverseScene
        (library : Library.LibraryProxy)
        (experiments : Experiments.ExperimentProxy)
        (materials : MaterialLibrary.MaterialProxy)
        (samples : Library.SampleProxy)
        (categories : MaterialLibrary.CategoryProxy) : TableAndElementRotationModel =
        initInverse library experiments materials samples categories |> kitSuccess

    /// The kind's element added and bound to its seeded preset, its chip closed: a kit window's empty palette slot filled
    /// by Choose… (spec 0051 `.manual/003` 3.8, D7); outside a kit window the add binds the preset itself.
    let withPresetElement (kind : Placement.CatalogueKind) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let added = model |> updateTableAndElementRotation (AddElement kind)
        let freshOpt = added.elements |> List.tryFind (fun slot -> not (model.elements |> List.exists (fun held -> held.id = slot.id)))
        match freshOpt, kind.defaultSeedEntry with
        | Some fresh, Some preset when fresh.placement.valueId = None ->
            let bound = added |> updateTableAndElementRotation (BindValueIdTo (fresh.id, preset))
            match bound.slotChipOpenOpt with
            | Some openId -> bound |> updateTableAndElementRotation (ToggleSlotChip openId)
            | None -> bound
        | _ -> added

