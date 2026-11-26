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
