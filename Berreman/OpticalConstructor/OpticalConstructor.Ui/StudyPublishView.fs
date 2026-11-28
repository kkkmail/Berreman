/// Publication of a fit run's calibration and library subjects, shown on the Kit page under the chosen run (spec 0050
/// S11.5; 0051 `.manual/007` 2.2, 8.6).
module OpticalConstructor.Ui.StudyPublishView

open System.Globalization
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Controls
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.StudyPublishing
open OpticalConstructor.Ui.ConstructorUiIdOfDomain
open OpticalConstructor.Ui.TableAndElementRotationView

/// The run's publication: the same revisions Publish calibration writes, then its promotions, which never replace
/// library history (spec 0050 S11.5; 0051 `.manual/007` 8.6).
let publishRunRows
    (model : TableAndElementRotationModel)
    (study : OpticalConstructor.Domain.Studies.Study)
    (run : OpticalConstructor.Domain.FitRuns.FitRun)
    (dispatch : TableAndElementRotationMsg -> unit)
    : IView list =
    let number (value : float) : string = value.ToString("G6", CultureInfo.InvariantCulture)
    let line (caption : ControlCaption) : IView =
        TextBlock.create [ TextBlock.text caption.value ] :> IView
    let heading (caption : ControlCaption) : IView =
        TextBlock.create [
            TextBlock.text caption.value
            TextBlock.fontWeight FontWeight.SemiBold
            TextBlock.margin (Thickness(0.0, 8.0, 0.0, 4.0))
        ] :> IView
    let action (id : ConstructorUiId) (caption : ControlCaption) (message : TableAndElementRotationMsg) : IView =
        CommandControls.commandButton id caption PositiveCommand ControlOffered (fun () -> dispatch message)
            [ Button.padding (Thickness(22.0, 6.0))
              Button.margin (Thickness(0.0, 0.0, 10.0, 4.0))
              Button.horizontalAlignment HorizontalAlignment.Left ]
    let selected : IView list =
            let preview = study.calibratedItemsWith run model.kitItems
            let previewRows : IView list =
                match preview with
                // Spec 0051 `.manual/003` 4.5: a published run says so once, as an outcome, not as a refusal beside it.
                | Error (CalibrationAlreadyPublishedErr _) ->
                    [ line (ControlCaption "Published: every item this fit run calibrates holds its values.") ]
                | Error error ->
                    [ TextBlock.create [
                        TextBlock.text error.describe
                        TextBlock.foreground (SolidColorBrush MaterialsWindowView.messageColor)
                      ] :> IView ]
                | Ok items ->
                    items |> List.map (fun item ->
                        let original = model.kitItems |> List.tryFind (fun held -> held.id = item.id)
                        let parameters =
                            item.parameters |> List.choose (fun parameter ->
                                let previous = original |> Option.bind (fun held -> held.tryFindParameter parameter.name)
                                match parameter.state with
                                | CalibratedParameter calibration when previous <> Some parameter ->
                                    Some $"{parameter.name.value} = {number calibration.value.value} ± {number calibration.sigma.value}"
                                | _ -> None)
                            |> String.concat ", "
                        let revision = item.revision.value.ToString(CultureInfo.InvariantCulture)
                        line (ControlCaption $"{item.label.value} → revision {revision}: {parameters}"))
            let publish : IView list =
                match preview, model.kitWrites with
                | Ok _, ControlOffered ->
                    [ action ConstructorUiId.publishCalibrationButton (ControlCaption "Publish calibration") (PublishCalibration run.name) ]
                | _ -> []
            let promotions : IView list =
                match run.promotionSubjectsWith study model.kitItems model.tryItemModelChoice with
                | [] -> [ line (ControlCaption "This fit run fitted no material or black box.") ]
                | subjects ->
                    subjects |> List.map (fun subject ->
                        let caption =
                            match subject with
                            | MaterialPromotion version ->
                                let name =
                                    study.materials
                                    |> List.tryFind (fun material -> material.version = version)
                                    |> Option.map (fun material -> material.name.value)
                                    |> Option.defaultValue (string version.materialId.value)
                                ControlCaption $"{name} v{version.version.value} → its next library version"
                            | BlackBoxPromotion item ->
                                ControlCaption $"{(item.labelAmong model.kitItems).value} (black box) → a custom-Mueller polarizer"
                        let promote : IView list =
                            match model.kitWrites with
                            | ControlWithheld -> []
                            | ControlOffered ->
                                [ action (ConstructorUiId.promoteButton subject) (ControlCaption "Promote") (PromoteToLibrary (run.name, subject)) ]
                        // Spec 0051 `.manual/003` 4.3: the row wraps at the page's width, so Promote never leaves the window.
                        WrapPanel.create [
                            WrapPanel.orientation Orientation.Horizontal
                            WrapPanel.itemSpacing 8.0
                            WrapPanel.children (line caption :: promote)
                        ] :> IView)
            [
                StackPanel.create [
                    StackPanel.children (
                        [
                            line (ControlCaption $"Fit run {run.name.value}")
                            heading (ControlCaption "Publish calibration")
                            StackPanel.create [
                                yield! ConstructorUiId.publishCalibrationPreview.groupAttributes<StackPanel> ()
                                AttrBuilder<StackPanel>.CreateProperty<string>(AutomationProperties.NameProperty, "Calibration preview", ValueNone)
                                StackPanel.orientation Orientation.Vertical
                                StackPanel.children previewRows
                            ]
                        ] @ publish @ [
                            line (ControlCaption "Publishing writes each item's next revision; the values it replaces stay in the item's revision history.")
                            heading (ControlCaption "Promote to library")
                            StackPanel.create [
                                yield! ConstructorUiId.promotionList.groupAttributes<StackPanel> ()
                                AttrBuilder<StackPanel>.CreateProperty<string>(AutomationProperties.NameProperty, "Promotion subjects", ValueNone)
                                StackPanel.orientation Orientation.Vertical
                                StackPanel.children promotions
                            ]
                            line (ControlCaption "Promoting mints a new library version under the fit's provenance; the version the study opened stays as it is.")
                        ])
                ]
                // Spec 0050 S11.5: actionButton retains its handler per ID, so another run needs fresh controls.
                |> View.withKey run.name.value
                :> IView
            ]
    selected
