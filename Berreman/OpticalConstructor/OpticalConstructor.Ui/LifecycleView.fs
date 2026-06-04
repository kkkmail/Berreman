/// Lifecycle toolbar view + IO sub-message (spec 0024 Part U8 / R-1, R-2). Owns the
/// `IoMsg` the shell routes through `RootMsg.Io`, plus the small File toolbar (New /
/// Open / Save) and the template-wizard / onboarding-gallery palettes.
///
/// Like the slice-002..007 view modules, this module takes its sub-message dispatch
/// (`IoMsg -> unit`) — NOT `RootModel`, which lives in `Shell.fs` — so it composes
/// under the root without a module cycle; `Shell` passes `RootMsg.Io >> dispatch`. The
/// view dispatches only the *trigger* cases (`NewProject`, `OpenRequested`,
/// `SaveRequested`, `LoadTemplate`, `LoadGallery`); the IO *result* cases (`OpenPath`,
/// `Loaded`, `Saved`, `IoError`) are dispatched by the `Shell.fs` background `Cmd`s.
///
/// Templates load via `Templates.loadTemplate` and gallery entries via `Help.openEntry`
/// in `Shell.update` — both the existing schema-validated factories (R-2 / §A.7); the
/// view only carries the entry to load and never invents a private deserialize.
///
/// New sibling `*View.fs` module per §0.1; authored against the public MIT
/// `Avalonia.FuncUI` 1.6.0 DSL surface (§0.2) — no clone reference.
module OpticalConstructor.Ui.LifecycleView

open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Domain.Project

/// The lifecycle / IO sub-message (R-1 / R-2). Trigger cases come from the toolbar
/// buttons; result cases are posted by the `Shell.fs` IO `Cmd`s (the file pickers /
/// reader / writer), marshaled onto the UI thread per §0.4.
type IoMsg =
    /// New: reset to a fresh default project.
    | NewProject
    /// Open: raise the `IStorageProvider` open picker (host layer, §0.5).
    | OpenRequested
    /// Open a specific path — the picker continuation, OR a direct load (tests).
    | OpenPath of string
    /// A project read + schema-validated off-thread; carries the source path.
    | Loaded of OpticalConstructorProject * string
    /// Save: write `<name>.ocproj.json` (picker for the destination when a host is set).
    | SaveRequested
    /// A project written; carries the written path.
    | Saved of string
    /// An IO/validation failure surfaced as a status message.
    | IoError of string
    /// Load a template through `Templates.loadTemplate` (R-2).
    | LoadTemplate of Templates.TemplateEntry
    /// Open a gallery sample through `Help.openEntry` (R-2).
    | LoadGallery of Help.GalleryEntry

let private actionButton (label : string) (msg : IoMsg) (dispatch : IoMsg -> unit) : IView =
    Button.create [
        Button.content label
        Button.onClick (fun _ -> dispatch msg)
    ] :> IView

/// The lifecycle toolbar (R-1 / R-2): the File actions, the template palette, the
/// onboarding gallery, and the last IO status line. Docked top, below the nav bar.
let lifecycleBar (status : string option) (dispatch : IoMsg -> unit) : IView =
    let fileRow =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.children [
                actionButton "New" NewProject dispatch
                actionButton "Open" OpenRequested dispatch
                actionButton "Save" SaveRequested dispatch
            ]
        ] :> IView
    let templatesRow =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 4.0
            StackPanel.children [
                yield TextBlock.create [
                    TextBlock.text "Templates:"
                    TextBlock.verticalAlignment VerticalAlignment.Center
                ] :> IView
                for entry in Templates.all do
                    yield actionButton entry.title (LoadTemplate entry) dispatch
            ]
        ] :> IView
    let galleryRow =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 4.0
            StackPanel.children [
                yield TextBlock.create [
                    TextBlock.text "Gallery:"
                    TextBlock.verticalAlignment VerticalAlignment.Center
                ] :> IView
                for entry in Help.gallery do
                    yield actionButton entry.title (LoadGallery entry) dispatch
            ]
        ] :> IView
    let statusRow =
        TextBlock.create [
            TextBlock.text (status |> Option.defaultValue "")
            TextBlock.foreground (Brushes.Gray :> IBrush)
        ] :> IView
    StackPanel.create [
        StackPanel.dock Dock.Top
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 4.0
        StackPanel.margin 4.0
        StackPanel.children [ fileRow; templatesRow; galleryRow; statusRow ]
    ] :> IView
