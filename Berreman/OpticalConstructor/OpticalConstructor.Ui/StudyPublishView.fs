/// Publication of the selected fit run's calibration and library subjects (spec 0050 S11.5).
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

/// Previews the same revisions publication writes and offers promotion without replacing library history (spec 0050 S11.5).
let studyPublishView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
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
        (MaterialsWindowView.actionButton id caption MaterialsWindowView.positiveBackground (fun () -> dispatch message) :?> IView<Border>)
        |> View.withAttrs [
            Border.horizontalAlignment HorizontalAlignment.Left
            AttrBuilder<Border>.CreateProperty<string>(AutomationProperties.NameProperty, caption.value, ValueNone)
        ]
        :> IView
    let selected : IView list =
        match model.selectedStudyOpt, model.selectedFitRunOpt with
        | Some study, Some run ->
            let preview = study.calibratedItemsWith run model.kitItems
            let previewRows : IView list =
                match preview with
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
                        StackPanel.create [
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.spacing 8.0
                            StackPanel.children (line caption :: promote)
                        ] :> IView)
            [
                StackPanel.create [
                    StackPanel.children (
                        [
                            line (ControlCaption $"Fit run {run.name.value}")
                            heading (ControlCaption "Publish calibration")
                            StackPanel.create [
                                ConstructorUiId.publishCalibrationPreview.automationAttribute<StackPanel> ()
                                AttrBuilder<StackPanel>.CreateProperty<string>(AutomationProperties.NameProperty, "Calibration preview", ValueNone)
                                StackPanel.orientation Orientation.Vertical
                                StackPanel.children previewRows
                            ]
                        ] @ publish @ [
                            line (ControlCaption "Publishing writes each item's next revision; the values it replaces stay in the item's revision history.")
                            heading (ControlCaption "Promote to library")
                            StackPanel.create [
                                ConstructorUiId.promotionList.automationAttribute<StackPanel> ()
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
        | _ -> [ line (ControlCaption "No fit run yet: run the fit in the Fit step.") ]
    ScrollViewer.create [
        AttrBuilder<ScrollViewer>.CreateProperty<bool>(ScrollViewer.AllowAutoHideProperty, false, ValueNone)
        ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.content (
            StackPanel.create [
                ConstructorUiId.studyPublishPanel.automationAttribute<StackPanel> ()
                AttrBuilder<StackPanel>.CreateProperty<string>(AutomationProperties.NameProperty, "Study publication", ValueNone)
                StackPanel.orientation Orientation.Vertical
                StackPanel.margin 8.0
                StackPanel.children ([ StudyUnknownsView.studyBarView model dispatch ] @ selected)
            ])
    ] :> IView
