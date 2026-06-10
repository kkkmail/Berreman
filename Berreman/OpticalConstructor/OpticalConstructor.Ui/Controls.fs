/// §J.1 / §J.2 — the single standardized control-flavours module (Spec 0026 Part J,
/// slice 004). This is the ONE place the constructor's standard control looks are
/// described: a default button, a primary/accent button, a destructive button, a
/// toggle, and a numeric-field-with-units (J.1.1). Each flavour is a function the
/// call sites use INSTEAD of styling controls ad hoc, and a new flavour is added by
/// overriding only the minimum set of properties that distinguish it (J.1.2) — the
/// pure `ButtonFlavor` records below make that "override only the distinguishing
/// property" pattern literal (`{ defaultButtonFlavor with background = … }`) and
/// headless-provable (AC-J1).
///
/// It also owns the same-row Confirm/Cancel **destructive-gate** flavour with its
/// distinct positive/negative CTA colours (UX commitment 5). The existing
/// `ConstructionView.fs:40` `positiveCta`/`negativeCta` — duplicated CTA colours
/// today — are rewired to draw from `positiveCtaBrush`/`negativeCtaBrush` here, so the
/// gate draws from ONE definition (J.2 / R-9).
///
/// Authored against the public MIT `Avalonia.FuncUI` DSL (constraint 0.3); the
/// audit-gated clone stays UNREFERENCED. Colours reuse the pure `SchematicColor`
/// value shape (Schematic.fs:35) so a flavour's identity is a serializable value, not
/// an Avalonia handle.
module OpticalConstructor.Ui.Controls

open Avalonia.Controls
open Avalonia.Media
open Avalonia.Media.Immutable
open Avalonia.Layout
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Ui.Schematic

/// Map a pure `SchematicColor` to an Avalonia solid brush — the single Avalonia
/// boundary in this module (the flavour identities stay pure `SchematicColor`). An
/// IMMUTABLE brush (like `Brushes.SeaGreen`) so it is thread-safe and constructs
/// without the Avalonia platform/Dispatcher — these are module-level values evaluated
/// off the UI thread (and the headless test thread); a mutable `SolidColorBrush` would
/// throw "Call from invalid thread" there.
let private toBrush (c : SchematicColor) : IBrush =
    ImmutableSolidColorBrush(Color.FromRgb(c.red, c.green, c.blue)) :> IBrush

// ---------------------------------------------------------------------------
// CTA colours (J.2 / R-9). The destructive-gate Confirm/Cancel pair draws from these
// ONE definitions; `ConstructionView.fs:40` references the brushes below. The hues
// match the previously-duplicated SeaGreen / IndianRed CTAs exactly, so the rewire is
// behaviour-preserving.
// ---------------------------------------------------------------------------

/// The positive (Confirm) CTA colour — SeaGreen `#2E8B57`.
let positiveCtaColor : SchematicColor = { red = 46uy; green = 139uy; blue = 87uy }

/// The negative (Cancel) CTA colour — IndianRed `#CD5C5C`.
let negativeCtaColor : SchematicColor = { red = 205uy; green = 92uy; blue = 92uy }

/// The positive (Confirm) CTA brush — the single definition `ConstructionView.fs:40`
/// draws from (J.2 / R-9).
let positiveCtaBrush : IBrush = toBrush positiveCtaColor

/// The negative (Cancel) CTA brush — the single definition `ConstructionView.fs:40`
/// draws from (J.2 / R-9).
let negativeCtaBrush : IBrush = toBrush negativeCtaColor

// ---------------------------------------------------------------------------
// Button flavours (J.1.1 / J.1.2). A flavour is a pure style value; a new one is
// built by overriding ONLY the distinguishing properties of the base flavour.
// ---------------------------------------------------------------------------

/// The accent colour of the primary/accent button — a steel blue, matching the
/// active-toggle colour the existing views use (`Button.background Brushes.SteelBlue`).
let accentColor : SchematicColor = { red = 33uy; green = 118uy; blue = 174uy }

/// The destructive button colour — the negative-CTA hue (a destructive action reads
/// in the same red as the Cancel/abort gate).
let destructiveColor : SchematicColor = negativeCtaColor

/// White foreground for the filled (primary / destructive) flavours.
let private white : SchematicColor = { red = 255uy; green = 255uy; blue = 255uy }

/// A standard control flavour for a button (J.1.1): the visually-distinguishing style
/// as a pure, serializable value. `None` background/foreground means "the theme
/// default"; a filled flavour overrides them.
type ButtonFlavor =
    {
        background : SchematicColor option
        foreground : SchematicColor option
        bold : bool
    }

/// The default button flavour (J.1.1): theme background/foreground, normal weight.
let defaultButtonFlavor : ButtonFlavor = { background = None; foreground = None; bold = false }

/// The primary/accent button flavour (J.1.1) — the default OVERRIDDEN with the accent
/// fill, a white foreground, and bold weight (the minimum distinguishing set, J.1.2).
let primaryButtonFlavor : ButtonFlavor =
    { defaultButtonFlavor with background = Some accentColor; foreground = Some white; bold = true }

/// The destructive button flavour (J.1.1) — the default OVERRIDDEN with only the
/// destructive fill + white foreground (J.1.2); weight is inherited from the base.
let destructiveButtonFlavor : ButtonFlavor =
    { defaultButtonFlavor with background = Some destructiveColor; foreground = Some white }

/// The accent colour an "on" toggle shows (J.1.1). An "off" toggle is transparent,
/// matching the established `Button.background (if active then SteelBlue else
/// Transparent)` toggle idiom in SourceView / MaterialsView / ResultsView / Shell.
let toggleOnColor : SchematicColor = accentColor

// ---------------------------------------------------------------------------
// Flavour functions (J.1.1). The call sites use these INSTEAD of styling controls ad
// hoc. They are the seam the FuncUI `Canvas` page (slice 005) and the ribbon (slice
// 006) consume; this slice ships them and the pure flavour records they apply.
// ---------------------------------------------------------------------------

/// Build a button from a flavour, applying only the flavour's set properties (an unset
/// background/foreground keeps the theme default).
let button (flavor : ButtonFlavor) (label : string) (onClick : unit -> unit) : IView =
    let attrs =
        [ Some (Button.content label)
          flavor.background |> Option.map (fun c -> Button.background (toBrush c))
          flavor.foreground |> Option.map (fun c -> Button.foreground (toBrush c))
          Some (Button.onClick (fun _ -> onClick ())) ]
        |> List.choose id
    Button.create attrs :> IView

/// The default-button flavour function (J.1.1).
let defaultButton (label : string) (onClick : unit -> unit) : IView = button defaultButtonFlavor label onClick

/// The primary/accent-button flavour function (J.1.1).
let primaryButton (label : string) (onClick : unit -> unit) : IView = button primaryButtonFlavor label onClick

/// The destructive-button flavour function (J.1.1).
let destructiveButton (label : string) (onClick : unit -> unit) : IView = button destructiveButtonFlavor label onClick

/// A disabled button (J.1.1): a default-flavour button rendered non-interactive — its
/// label stays visible (so the command is still discoverable on its tab) but it cannot be
/// clicked. The ribbon uses this for gesture-only commands that have no parameterless
/// invocation, so they read as "not clickable here" rather than as silent no-ops.
let disabledButton (label : string) : IView =
    Button.create [
        Button.content label
        Button.isEnabled false
    ] :> IView

/// A toggle flavour (J.1.1): an accent fill when on, transparent when off (the
/// established toggle-button idiom). Clicking flips the state through `onToggle`.
let toggle (label : string) (isOn : bool) (onToggle : bool -> unit) : IView =
    Button.create [
        Button.content label
        Button.background (if isOn then toBrush toggleOnColor else (Brushes.Transparent :> IBrush))
        Button.onClick (fun _ -> onToggle (not isOn))
    ] :> IView

/// A numeric-field-with-units flavour (J.1.1): a label, a numeric text entry, and a
/// trailing unit symbol on one row — the standard "value [unit]" editor the
/// element/table size inputs use. Mirrors the existing `SourceView.numberField`
/// shape; the caller parses the text (the model never stores a non-SI datum).
let numericWithUnits (label : string) (text : string) (unitSymbol : string) (onTextChanged : string -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 4.0
        StackPanel.children [
            TextBlock.create [ TextBlock.text label; TextBlock.verticalAlignment VerticalAlignment.Center ]
            TextBox.create [ TextBox.width 120.0; TextBox.text text; TextBox.onTextChanged onTextChanged ]
            TextBlock.create [ TextBlock.text unitSymbol; TextBlock.verticalAlignment VerticalAlignment.Center ]
        ]
    ] :> IView

/// The same-row destructive gate (J.2 / R-9 / UX commitment 5): a prompt with a
/// positive-CTA Confirm and a negative-CTA Cancel on one row, both drawing from the
/// single CTA definitions above. This is the flavour `ConstructionView`'s delete gate
/// expresses; the gate draws from one definition rather than per-view ad-hoc brushes.
let destructiveGate
    (prompt : string)
    (confirmLabel : string)
    (cancelLabel : string)
    (onConfirm : unit -> unit)
    (onCancel : unit -> unit)
    : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 12.0
        StackPanel.children [
            TextBlock.create [ TextBlock.text prompt; TextBlock.verticalAlignment VerticalAlignment.Center ]
            Button.create [
                Button.content confirmLabel
                Button.background positiveCtaBrush
                Button.onClick (fun _ -> onConfirm ())
            ]
            Button.create [
                Button.content cancelLabel
                Button.background negativeCtaBrush
                Button.onClick (fun _ -> onCancel ())
            ]
        ]
    ] :> IView
