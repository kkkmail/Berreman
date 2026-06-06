/// Context-sensitive local help (Spec 0026 Part D / E.2 references, slice 006). The
/// constructor surfaces short, context-sensitive help — for the optical table, for the
/// active element's catalogue role, and for an individual command — and every help
/// string resolves through the slice-003 `Localization` resource (no compiled-in user
/// text, I.1.1), so it travels through the SAME EN/RU `strings.json` the rest of the UI
/// reads and an operator/translator can edit it with no rebuild.
///
/// The module is PURE and carries NO Avalonia type (constraint 0.3): it maps a help
/// context (a domain `CatalogueKind` or a `Commands.Command`) to a localization key and
/// resolves the text; the ribbon/shell render the resulting string. It also owns the
/// ONE command-label-key mapping the ribbon reuses for its button labels, so a command's
/// display label is declared in a single place (constraint 0.4 spirit) and the ribbon
/// has no second label-wiring site.
module OpticalConstructor.Ui.LocalHelp

open OpticalConstructor.Domain.Placement
open OpticalConstructor.Ui.Commands
open OpticalConstructor.Ui.Localization

/// A help context (E.2): the table workspace, the active element's catalogue role, or a
/// single command. The ribbon resolves the help text for the current selection.
type HelpContext =
    | TableHelp
    | ElementHelp of CatalogueKind
    | CommandHelp of Command

/// The localization key for an element catalogue role's help text.
let private elementHelpKey (kind : CatalogueKind) : string =
    match kind with
    | LightSource -> "help.element.lightSource"
    | LinearPolarizer -> "help.element.linearPolarizer"
    | CircularPolarizer -> "help.element.circularPolarizer"
    | Sample -> "help.element.sample"
    | Lens -> "help.element.lens"
    | FlatMirror -> "help.element.flatMirror"
    | CurvedMirror -> "help.element.curvedMirror"
    | Detector -> "help.element.detector"

/// The localization key (the `strings.json` handle) for a command's DISPLAY label. This
/// is the ONE place a command's user-facing label key is declared; the ribbon reuses it
/// for both its expanded controls and its collapsed menus, so a command added to the
/// registry needs exactly one label entry and no second wiring site (constraint 0.4).
let commandLabelKey (cmd : Command) : string =
    match cmd with
    | OpenElementDialog -> "command.openElement"
    | ElementContextMenu -> "command.elementMenu"
    | ResetRotation -> "command.resetRotation"
    | DeleteElement -> "command.delete"
    | DuplicateElement -> "command.duplicate"
    | CopyElement -> "command.copy"
    | PasteElement -> "command.paste"
    | LocalHelp -> "command.help"
    | NextElement -> "command.nextElement"
    | PreviousElement -> "command.previousElement"
    | RotateR1 -> "command.rotateR1"
    | RotateR2 -> "command.rotateR2"
    | RotateR3 -> "command.rotateR3"
    | SlideAlongRay -> "command.slide"
    | MoveToRay -> "command.moveToRay"
    | PanView -> "command.pan"
    | ZoomView -> "command.zoom"
    | ResetView -> "command.resetView"
    | ToggleGroup -> "command.toggleGroup"
    | Undo -> "command.undo"
    | Redo -> "command.redo"
    | SaveProject -> "command.save"
    | CancelOrDeselect -> "command.cancel"
    | PlaceFromRibbon -> "command.place"

/// The localization key for a help context's text.
let helpKey (ctx : HelpContext) : string =
    match ctx with
    | TableHelp -> "help.table"
    | ElementHelp kind -> elementHelpKey kind
    // A command's context help reuses its display label — the smallest meaningful
    // context-sensitive hint for a command, declared in the one label mapping above.
    | CommandHelp cmd -> commandLabelKey cmd

/// Resolve a command's display label in the active language (falls back to English,
/// then to the key, exactly like every other UI string — never crashes, never blank).
let commandLabel (resource : Resource) (language : Language) (cmd : Command) : string =
    lookup resource language (commandLabelKey cmd)

/// Resolve the help text for a context in the active language (I.3.1 fallback chain).
let helpText (resource : Resource) (language : Language) (ctx : HelpContext) : string =
    lookup resource language (helpKey ctx)
