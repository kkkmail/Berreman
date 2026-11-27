/// The selected fit run's stored values and diagnostics in six tabs (spec 0050 S11.5).
module OpticalConstructor.Ui.StudyResultsView

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
open OpticalConstructor.Domain.FitRuns
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Optimization.StudyEngine
open OpticalConstructor.Ui.ConstructorUiIdOfDomain
open OpticalConstructor.Ui.TableAndElementRotationView

/// Projects the selected study's chosen or latest stored fit run without running a fit (spec 0050 S11.5).
let studyResultsView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let number (value : float) : string = value.ToString("G6", CultureInfo.InvariantCulture)
    let line (caption : ControlCaption) : IView =
        TextBlock.create [ TextBlock.text caption.value ] :> IView
    let cell (row : int) (column : int) (weight : FontWeight) (caption : ControlCaption) : IView =
        TextBlock.create [
            TextBlock.row row
            TextBlock.column column
            TextBlock.fontWeight weight
            TextBlock.margin (Thickness(0.0, 0.0, 12.0, 4.0))
            TextBlock.text caption.value
        ] :> IView
    // Spec 0051 `.manual/003` 4.3: a table scrolls sideways in its own scroller, never the page.
    let table (headers : ControlCaption list) (rows : ControlCaption list list) : IView =
        ScrollViewer.create [
            ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Auto
            ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Disabled
            ScrollViewer.content (
                Grid.create [
                    Grid.horizontalAlignment HorizontalAlignment.Left
                    Grid.columnDefinitions (List.replicate headers.Length "Auto" |> String.concat ",")
                    Grid.rowDefinitions (List.replicate (rows.Length + 1) "Auto" |> String.concat ",")
                    Grid.children (
                        (headers |> List.mapi (fun column caption -> cell 0 column FontWeight.SemiBold caption))
                        @ (rows |> List.mapi (fun row captions ->
                            captions |> List.mapi (fun column caption ->
                                cell (row + 1) column FontWeight.Normal caption)) |> List.concat))
                ])
        ] :> IView
    let itemLabels (items : ItemId list) (separator : string) : string =
        items |> List.map (fun item -> (item.labelAmong model.kitItems).value) |> String.concat separator
    let coordinateCaption (coordinate : FitCoordinate) : ControlCaption =
        match coordinate with
        | EntryCoordinate entries ->
            entries |> List.map (fun entry -> (model.fitEntryCaption entry).value) |> String.concat ", " |> ControlCaption
        | ElementCoordinate (row, column) -> ControlCaption $"m{row.value}{column.value}"
        | FringeVisibilityCoordinate -> ControlCaption "fringe visibility"
    let problemRows (problem : FitProblemQuality) : IView list =
        let subject =
            match problem.subject with
            | StudyEntriesProblem -> "Study residual"
            | ObjectSolveProblem items ->
                let labels = itemLabels items ", "
                $"Object {labels}"
            | AirFringeProblem runs ->
                let names = runs |> List.map _.value |> String.concat ", "
                $"AIR fringe {names}"
        let condition = problem.conditionOpt |> Option.map (fun value -> number value.value) |> Option.defaultValue "—"
        let rank = problem.rank.value.ToString("G6", CultureInfo.InvariantCulture)
        let summary = ControlCaption $"{subject}: rank {rank}, condition {condition}, χ² {number problem.chiSquared.value}, RMSE {number problem.rmse.value}"
        let correlation =
            match problem.correlationOpt with
            | None -> line (ControlCaption "no correlation: the Jacobian is rank-deficient")
            | Some matrix ->
                let coordinates = problem.coordinates |> List.map coordinateCaption
                let rows =
                    matrix.value |> Array.mapi (fun row values ->
                        coordinates.[row]
                        :: (values |> Array.map (fun value -> ControlCaption (value.ToString("F3", CultureInfo.InvariantCulture))) |> Array.toList))
                    |> Array.toList
                table (ControlCaption "" :: coordinates) rows
        [ line summary; correlation ]
    let profileRows (profile : ParameterProfile) : IView list =
        let reasons =
            profile.reasons |> List.map (function
                | UndeterminedSigmaReason -> "σ undetermined"
                | WideSigmaReason fraction -> $"σ is {number fraction.value} of its bounds"
                | CorrelatedEntryReason (partner, correlation) ->
                    $"correlated with {(model.fitEntryCaption partner).value} (ρ = {number correlation.value})")
            |> String.concat "; "
        line (ControlCaption $"{(model.fitEntryCaption profile.entry).value} ({reasons})")
        :: (profile.points |> List.map (fun point ->
            let outcome =
                match point.outcome with
                | ProfileConverged chiSquared -> $"χ² {number chiSquared.value}"
                | ProfileFailed failure -> failure.value
            line (ControlCaption $"{number point.value.value}: {outcome}")))
    let tabRows (run : FitRun) : IView list =
        match model.studySurface.resultsTab with
        | ValuesResultsTab ->
            match run.entries with
            | [] -> []
            | entries ->
                let rows =
                    entries |> List.map (fun entry ->
                        let reference : FitEntryRef =
                            {
                                parameter = entry.parameter
                                owner = entry.owner
                            }
                        let sigma =
                            run.results.sigmas
                            |> List.tryFind (fun sigma -> sigma.entry = reference)
                            |> Option.bind _.sigmaOpt
                            |> Option.map (fun value -> ControlCaption (number value.value))
                            |> Option.defaultValue (ControlCaption "undetermined")
                        [ model.fitEntryCaption reference; ControlCaption (number entry.start.value); ControlCaption (number entry.fitted.value); sigma ])
                [ table [ ControlCaption "Parameter"; ControlCaption "Start"; ControlCaption "Fitted"; ControlCaption "σ" ] rows ]
        | CorrelationResultsTab -> run.results.problems |> List.collect problemRows
        | ResidualsResultsTab ->
            run.results.runResiduals |> List.map (fun residual ->
                let count = residual.residuals.value.Length.ToString("G6", CultureInfo.InvariantCulture)
                line (ControlCaption $"{residual.run.value}: RMSE {number residual.rmse.value} over {count} residual(s)"))
        | RealizabilityResultsTab ->
            run.results.realizability |> List.map (fun result ->
                let verdict =
                    match result.realizability with
                    | RealizableMueller -> "realizable"
                    | UnrealizableMueller -> "not realizable"
                let eigenvalues = result.eigenvalues |> List.map (fun value -> number value.value) |> String.concat ", "
                line (ControlCaption $"{(result.item.labelAmong model.kitItems).value}: {verdict}; eigenvalues {eigenvalues}"))
        | ProductCheckResultsTab ->
            run.results.productChecks |> List.map (fun check ->
                let items = itemLabels check.items " → "
                line (ControlCaption $"{items}: product RMSE {number check.productRmse.value}, fit RMSE {number check.fitRmse.value}, ratio {number check.rmseRatio.value}, max |ΔM| {number check.difference.maxAbs.value}"))
        | ProfileResultsTab -> run.results.profiles |> List.collect profileRows
    let selected : IView list =
        match model.selectedFitRunOpt with
        | None -> [ line (ControlCaption "No fit run yet: run the fit in the Fit step.") ]
        | Some run ->
            let runCaption = ControlCaption $"Fit run {run.name.value} — {run.stage.text}, {run.engine.text}"
            let rows =
                match tabRows run with
                | [] -> [ line (ControlCaption "Nothing to show for this fit run.") ]
                | rows -> rows
            [
                TextBlock.create [
                    ConstructorUiId.fitResultsRunText.automationAttribute<TextBlock> ()
                    AttrBuilder<TextBlock>.CreateProperty<string>(AutomationProperties.NameProperty, runCaption.value, ValueNone)
                    TextBlock.text runCaption.value
                ]
                WrapPanel.create [
                    yield! ConstructorUiId.fitResultsTabStrip.groupAttributes<WrapPanel> ()
                    AttrBuilder<WrapPanel>.CreateProperty<string>(AutomationProperties.NameProperty, "Results tabs", ValueNone)
                    WrapPanel.children (FitResultsTab.all |> List.map (fun tab ->
                        let choose () = dispatch (ShowResultsTab tab)
                        CommandControls.choiceCommand tab.tabUiId tab.caption (if tab = model.studySurface.resultsTab then ChosenChoice else UnchosenChoice) choose))
                ]
                StackPanel.create [
                    yield! ConstructorUiId.fitResultsTable.groupAttributes<StackPanel> ()
                    AttrBuilder<StackPanel>.CreateProperty<string>(AutomationProperties.NameProperty, model.studySurface.resultsTab.caption.value, ValueNone)
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.margin (Thickness(0.0, 8.0, 0.0, 0.0))
                    StackPanel.children rows
                ]
                |> View.withKey model.studySurface.resultsTab.tabUiId.value
                :> IView
            ]
    ScrollViewer.create [
        AttrBuilder<ScrollViewer>.CreateProperty<bool>(ScrollViewer.AllowAutoHideProperty, false, ValueNone)
        ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
        // Spec 0051 `.manual/003` 4.3: laid out at the window's width, so no page is wider than its window.
        ScrollViewer.content (
            MinWidthPanel.create [
                (PanelMinWidth 520.0).minWidthPanelAttribute ()
                MinWidthPanel.child (
                    StackPanel.create [
                        yield! ConstructorUiId.studyResultsPanel.groupAttributes<StackPanel> ()
                        AttrBuilder<StackPanel>.CreateProperty<string>(AutomationProperties.NameProperty, "Study results", ValueNone)
                        StackPanel.orientation Orientation.Vertical
                        StackPanel.margin 8.0
                        StackPanel.children ([ StudyUnknownsView.studyBarView model dispatch ] @ selected)
                    ])
            ])
    ] :> IView
