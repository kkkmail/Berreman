/// §E.1 — the centralized command model (Spec 0026 Part E, slice 005). This is the ONE
/// registry every key map, every mouse map, and every ribbon/menu control derives from
/// (constraint 0.4 / AC-E1): a new command is added in this one place, and a command
/// reachable both by a key and a mouse gesture declares BOTH bindings on one definition
/// (the three-state `Binding`). Slice 006 (Ribbon) projects ribbon + menu controls from
/// this same registry with no second wiring site; slice 007 (Part G) adds the
/// group-toggle and detector *Set as primary* commands HERE.
///
/// The module is PURE and carries NO Avalonia type (constraint 0.3): the gesture value
/// shapes (`KeyGesture` / `MouseGesture`) are plain, comparable F# values, so the key
/// map and the mouse map are ordinary `Map` lookups projected from the one registry and
/// every binding rule is provable headless. The Avalonia `KeyModifiers`/`Key`/pointer
/// types are mapped onto these value shapes at the `ConstructorView` event boundary
/// only.
///
/// The registry is persistence-agnostic: the configurable key map (§E.8.1) is applied
/// through `withKeyOverrides`, which takes plain `(command-id, gesture-string)` pairs —
/// so this module has NO dependency on `UserEnvironment` and the compile order
/// (`UserEnvironment` -> `Commands` -> `ConstructorView`) stays acyclic.
module OpticalConstructor.Ui.Commands

// ---------------------------------------------------------------------------
// Pure gesture value shapes (no Avalonia type). Comparable so a gesture keys a Map.
// ---------------------------------------------------------------------------

/// A keyboard modifier (§E.1 / §E.3). A fieldless DU — comparable, so it keys a `Set`.
type Modifier =
    | Ctrl
    | Shift
    | Alt

/// A key on the keyboard, named language-neutrally (§E.1) — the small closed vocabulary
/// the constructor binds: letters, digits, the four arrows, and the named editing keys.
/// Comparable so a `(key, modifiers)` gesture is a valid `Map` key. The Avalonia
/// `Key` enum maps onto this at the `ConstructorView` boundary only.
type Key =
    | Letter of char
    | Digit of int
    | ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | Tab
    | Delete
    | Backspace
    | Escape
    | F1

/// A keyboard gesture (§E.1.1): a key plus the set of modifiers held. The `Set` makes
/// modifier order/duplication irrelevant, so equal gestures compare equal.
type KeyGesture =
    {
        key : Key
        modifiers : Set<Modifier>
    }

/// A mouse button (§E.1).
type MouseButtonKind =
    | LeftButton
    | RightButton
    | MiddleButton

/// A mouse gesture (§E.1.1): a distinct raw pointer interaction the surface recognizes.
/// The modifier set disambiguates same-button gestures — a plain left-drag (pan) vs a
/// `Shift`+left-drag (slide) vs a `Ctrl`+left-drag (reassign), and a plain wheel (zoom)
/// vs a `Shift`/`Ctrl+Shift`/`Alt` wheel (R1/R2/R3).
type MouseGesture =
    | Press of MouseButtonKind * Set<Modifier>
    | DoublePress of MouseButtonKind
    | DragGesture of MouseButtonKind * Set<Modifier>
    | WheelGesture of Set<Modifier>
    | RibbonDropGesture

// ---------------------------------------------------------------------------
// The three-state binding (§E.1.1 / constraint 0.4). A command reachable both ways
// declares BOTH bindings on one definition (`KeyboardAndMouse`).
// ---------------------------------------------------------------------------

/// A command's binding: keyboard-only, mouse-only, or keyboard-plus-mouse (§E.1.1).
type Binding =
    | KeyboardOnly of KeyGesture list
    | MouseOnly of MouseGesture list
    | KeyboardAndMouse of KeyGesture list * MouseGesture list

    /// The keyboard gestures this binding declares (empty for a mouse-only command).
    member b.keyGestures : KeyGesture list =
        match b with
        | KeyboardOnly ks -> ks
        | MouseOnly _ -> []
        | KeyboardAndMouse (ks, _) -> ks

    /// The mouse gestures this binding declares (empty for a keyboard-only command).
    member b.mouseGestures : MouseGesture list =
        match b with
        | KeyboardOnly _ -> []
        | MouseOnly ms -> ms
        | KeyboardAndMouse (_, ms) -> ms

// ---------------------------------------------------------------------------
// The command DU + scope (§E.1.2 / §E.2-§E.7). Every command operates on the ACTIVE
// element unless it is explicitly a table/view or global command.
// ---------------------------------------------------------------------------

/// Every command the constructor surface declares (§E.2-§E.7). A fieldless DU — the
/// digit a group-toggle carries and the kind a ribbon drop carries are read from the
/// concrete event by the page, not encoded in the command, so a gesture maps to exactly
/// one command. Slice 007 adds the group on/off and detector *Set as primary* cases.
type Command =
    // Active-element commands (§E.2).
    | OpenElementDialog
    | ElementContextMenu
    | ResetRotation
    | DeleteElement
    | DuplicateElement
    | CopyElement
    | PasteElement
    | LocalHelp
    | NextElement
    | PreviousElement
    // Rotations (§E.3).
    | RotateR1
    | RotateR2
    | RotateR3
    // Move the active element (§E.4).
    | SlideAlongRay
    | MoveToRay
    // Table & view commands (§E.5).
    | PanView
    | ZoomView
    | ResetView
    | ToggleGroup
    // Global commands (§E.6).
    | Undo
    | Redo
    | SaveProject
    | CancelOrDeselect
    // Drag-to-place from the ribbon (§E.7).
    | PlaceFromRibbon
    // Element groups & detectors (Part G, slice 007). The group/member a swap targets and
    // the detector a set-primary targets are read from the concrete UI control / active
    // selection by the page, not encoded in the command (so a gesture maps to one command):
    // `ToggleGroup` (above) is group on/off; these add swap and the detector commands.
    | SwapGroup
    | AddDetector
    | RemoveDetector
    | SetPrimaryDetector

/// Whether a command targets the active element, the table/view, or is global (§E.1.2).
type CommandScope =
    | ActiveElement
    | TableOrView
    | Global

/// One command definition (§E.1.1): the command, its scope, its three-state binding,
/// whether it surfaces in the element context menu (the menu/ribbon projection, §E.2),
/// and a stable, language-neutral `id` used as the configurable key-map override key
/// (§E.8.1) and the ribbon/menu projection handle. The `id` is NOT a user-facing label —
/// labels resolve through `Localization` (slice 006).
type CommandDef =
    {
        command : Command
        scope : CommandScope
        binding : Binding
        inContextMenu : bool
        id : string
    }

// ---------------------------------------------------------------------------
// Gesture constructors (used by the registry and the tests).
// ---------------------------------------------------------------------------

/// A keyboard gesture with no modifier.
let key (k : Key) : KeyGesture = { key = k; modifiers = Set.empty }

/// A keyboard gesture with the given modifiers.
let keyWith (mods : Modifier list) (k : Key) : KeyGesture = { key = k; modifiers = Set.ofList mods }

let private wheel (mods : Modifier list) : MouseGesture = WheelGesture (Set.ofList mods)
let private drag (button : MouseButtonKind) (mods : Modifier list) : MouseGesture = DragGesture (button, Set.ofList mods)
let private press (button : MouseButtonKind) (mods : Modifier list) : MouseGesture = Press (button, Set.ofList mods)

let private kbd (ks : KeyGesture list) : Binding = KeyboardOnly ks
let private mouse (ms : MouseGesture list) : Binding = MouseOnly ms
let private both (ks : KeyGesture list) (ms : MouseGesture list) : Binding = KeyboardAndMouse (ks, ms)

// ---------------------------------------------------------------------------
// THE registry (§E.1.1 / AC-E1). The single source from which the key map, the mouse
// map, and the menu/ribbon projection all derive.
// ---------------------------------------------------------------------------

/// The centralized command registry (§E.1.1 / constraint 0.4). Every key map, mouse map,
/// and ribbon/menu control derives from this one list; a new command is added HERE only.
let registry : CommandDef list =
    [
        // --- Active-element commands (§E.2) -----------------------------------
        { command = OpenElementDialog; scope = ActiveElement; id = "open-element-dialog"
          inContextMenu = true;  binding = mouse [ DoublePress LeftButton ] }
        { command = ElementContextMenu; scope = ActiveElement; id = "element-context-menu"
          inContextMenu = false; binding = mouse [ press RightButton [] ] }
        { command = ResetRotation; scope = ActiveElement; id = "reset-rotation"
          inContextMenu = true;  binding = kbd [ keyWith [ Shift ] (Letter 'R') ] }
        { command = DeleteElement; scope = ActiveElement; id = "delete-element"
          inContextMenu = true;  binding = kbd [ key Delete; key Backspace ] }
        { command = DuplicateElement; scope = ActiveElement; id = "duplicate-element"
          inContextMenu = true;  binding = kbd [ keyWith [ Ctrl ] (Letter 'D') ] }
        { command = CopyElement; scope = ActiveElement; id = "copy-element"
          inContextMenu = false; binding = kbd [ keyWith [ Ctrl ] (Letter 'C') ] }
        { command = PasteElement; scope = ActiveElement; id = "paste-element"
          inContextMenu = false; binding = kbd [ keyWith [ Ctrl ] (Letter 'V') ] }
        { command = LocalHelp; scope = ActiveElement; id = "local-help"
          inContextMenu = true;  binding = kbd [ key F1 ] }
        { command = NextElement; scope = ActiveElement; id = "next-element"
          inContextMenu = false; binding = kbd [ key Tab ] }
        { command = PreviousElement; scope = ActiveElement; id = "previous-element"
          inContextMenu = false; binding = kbd [ keyWith [ Shift ] Tab ] }

        // --- Rotations (§E.3) — each step defaults to 5°, configurable; inert on a
        //     locked axis (R3 starts locked). -----------------------------------
        { command = RotateR1; scope = ActiveElement; id = "rotate-r1"
          inContextMenu = true; binding = mouse [ wheel [ Shift ] ] }
        { command = RotateR2; scope = ActiveElement; id = "rotate-r2"
          inContextMenu = true; binding = mouse [ wheel [ Ctrl; Shift ] ] }
        { command = RotateR3; scope = ActiveElement; id = "rotate-r3"
          inContextMenu = true; binding = mouse [ wheel [ Alt ] ] }

        // --- Move the active element (§E.4) -----------------------------------
        // Slide is the canonical keyboard-PLUS-mouse command: Shift+left-drag OR the
        // arrow keys (Shift+arrow for a larger step). Both bindings on ONE definition.
        { command = SlideAlongRay; scope = ActiveElement; id = "slide-along-ray"
          inContextMenu = false
          binding =
            both
                [ key ArrowLeft; key ArrowRight; key ArrowUp; key ArrowDown
                  keyWith [ Shift ] ArrowLeft; keyWith [ Shift ] ArrowRight
                  keyWith [ Shift ] ArrowUp; keyWith [ Shift ] ArrowDown ]
                [ drag LeftButton [ Shift ] ] }
        { command = MoveToRay; scope = ActiveElement; id = "move-to-ray"
          inContextMenu = false; binding = mouse [ drag LeftButton [ Ctrl ] ] }

        // --- Table & view commands (§E.5) -------------------------------------
        // Pan = plain left-drag (interpreted as pan only on the empty table; a plain
        // left-drag on an element is inert + hinted by the page, §E.4.4).
        { command = PanView; scope = TableOrView; id = "pan-view"
          inContextMenu = false; binding = mouse [ drag LeftButton [] ] }
        // Zoom = plain wheel OR Ctrl+wheel (the OS-standard Ctrl+wheel=zoom convention).
        { command = ZoomView; scope = TableOrView; id = "zoom-view"
          inContextMenu = false; binding = mouse [ wheel []; wheel [ Ctrl ] ] }
        { command = ResetView; scope = TableOrView; id = "reset-view"
          inContextMenu = false; binding = kbd [ keyWith [ Ctrl ] (Digit 0) ] }
        // The number keys select the corresponding group (Part G; the behaviour is
        // slice 007 — this slice binds the command shape only).
        { command = ToggleGroup; scope = TableOrView; id = "toggle-group"
          inContextMenu = true;  binding = kbd [ for d in 1 .. 9 -> key (Digit d) ] }

        // --- Global commands (§E.6) -------------------------------------------
        { command = Undo; scope = Global; id = "undo"
          inContextMenu = false; binding = kbd [ keyWith [ Ctrl ] (Letter 'Z') ] }
        { command = Redo; scope = Global; id = "redo"
          inContextMenu = false; binding = kbd [ keyWith [ Ctrl ] (Letter 'Y'); keyWith [ Ctrl; Shift ] (Letter 'Z') ] }
        { command = SaveProject; scope = Global; id = "save-project"
          inContextMenu = false; binding = kbd [ keyWith [ Ctrl ] (Letter 'S') ] }
        { command = CancelOrDeselect; scope = Global; id = "cancel-or-deselect"
          inContextMenu = false; binding = kbd [ key Escape ] }

        // --- Drag-to-place from the ribbon (§E.7) -----------------------------
        { command = PlaceFromRibbon; scope = TableOrView; id = "place-from-ribbon"
          inContextMenu = false; binding = mouse [ RibbonDropGesture ] }

        // --- Element groups & detectors (Part G, slice 007) -------------------
        // Surfaced on the Experiment tab and (the detector ones) the element context menu.
        // They carry no single hotkey — the group member a swap targets is chosen from the
        // Experiment-tab control, and the detector a remove/set-primary targets is the active
        // selection — so they declare an empty keyboard binding (`KeyboardOnly []`): still
        // single-sourced HERE and projected into the ribbon/menus exactly once (AC-E1/AC-D2).
        { command = SwapGroup; scope = TableOrView; id = "swap-group"
          inContextMenu = false; binding = kbd [] }
        { command = AddDetector; scope = TableOrView; id = "add-detector"
          inContextMenu = false; binding = kbd [] }
        { command = RemoveDetector; scope = ActiveElement; id = "remove-detector"
          inContextMenu = true;  binding = kbd [] }
        { command = SetPrimaryDetector; scope = ActiveElement; id = "set-primary-detector"
          inContextMenu = true;  binding = kbd [] }
    ]

// ---------------------------------------------------------------------------
// Projections from the one registry (§E.1.1). The key map and the mouse map are these.
// ---------------------------------------------------------------------------

/// Every `(keyGesture, command)` pair in the registry (the key map, flattened) — the
/// single source the key lookup is built from (AC-E1).
let keyBindings : (KeyGesture * Command) list =
    registry |> List.collect (fun d -> d.binding.keyGestures |> List.map (fun g -> g, d.command))

/// Every `(mouseGesture, command)` pair in the registry (the mouse map, flattened).
let mouseBindings : (MouseGesture * Command) list =
    registry |> List.collect (fun d -> d.binding.mouseGestures |> List.map (fun g -> g, d.command))

/// The default key map — a gesture-to-command lookup projected from the registry.
let keyLookup : Map<KeyGesture, Command> = Map.ofList keyBindings

/// The default mouse map — a gesture-to-command lookup projected from the registry.
let mouseLookup : Map<MouseGesture, Command> = Map.ofList mouseBindings

/// Resolve a keyboard gesture against the DEFAULT key map (§E.1.1).
let lookupKey (g : KeyGesture) : Command option = Map.tryFind g keyLookup

/// Resolve a mouse gesture against the mouse map (§E.1.1).
let lookupMouse (g : MouseGesture) : Command option = Map.tryFind g mouseLookup

/// The definition of a command (its scope, binding, menu flag, id).
let private defByCommand : Map<Command, CommandDef> =
    registry |> List.map (fun d -> d.command, d) |> Map.ofList

/// The registry definition for a command (`None` only for a command not in the registry,
/// which cannot happen for a registered `Command` case).
let defOf (command : Command) : CommandDef option = Map.tryFind command defByCommand

/// The stable, language-neutral id of a command (§E.8.1 override key / ribbon handle).
let idOf (command : Command) : string =
    match defOf command with
    | Some d -> d.id
    | None -> ""

/// The commands surfaced in the element context menu (§E.2). The menu/ribbon (slice 006)
/// projects these from the one registry; the page adds the per-element property toggles.
let contextMenuCommands : Command list =
    registry |> List.filter (fun d -> d.inContextMenu) |> List.map (fun d -> d.command)

// ---------------------------------------------------------------------------
// Gesture string parse/format (§E.8.1) — the human-editable form persisted in the key
// map (e.g. "Ctrl+Shift+Z", "Shift+R", "Delete", "F1", "Ctrl+0", "Shift+Left").
// ---------------------------------------------------------------------------

/// The canonical token for a key (the part after the modifiers in a gesture string).
let private keyToken (k : Key) : string =
    match k with
    | Letter c -> string (System.Char.ToUpperInvariant c)
    | Digit d -> string d
    | ArrowLeft -> "Left"
    | ArrowRight -> "Right"
    | ArrowUp -> "Up"
    | ArrowDown -> "Down"
    | Tab -> "Tab"
    | Delete -> "Delete"
    | Backspace -> "Backspace"
    | Escape -> "Escape"
    | F1 -> "F1"

/// Format a keyboard gesture as its canonical string (§E.8.1): modifiers in the fixed
/// order Ctrl, Shift, Alt, then the key token, joined by "+".
let formatKeyGesture (g : KeyGesture) : string =
    let mods =
        [ if g.modifiers.Contains Ctrl then "Ctrl"
          if g.modifiers.Contains Shift then "Shift"
          if g.modifiers.Contains Alt then "Alt" ]
    String.concat "+" (mods @ [ keyToken g.key ])

/// Parse a key token (case-insensitive) into a `Key`; `None` for an unknown token.
let private parseKeyToken (token : string) : Key option =
    match token.ToUpperInvariant() with
    | "LEFT" -> Some ArrowLeft
    | "RIGHT" -> Some ArrowRight
    | "UP" -> Some ArrowUp
    | "DOWN" -> Some ArrowDown
    | "TAB" -> Some Tab
    | "DELETE" | "DEL" -> Some Delete
    | "BACKSPACE" | "BACK" -> Some Backspace
    | "ESCAPE" | "ESC" -> Some Escape
    | "F1" -> Some F1
    | t when t.Length = 1 && System.Char.IsLetter t.[0] -> Some (Letter (System.Char.ToUpperInvariant t.[0]))
    | t when t.Length = 1 && System.Char.IsDigit t.[0] -> Some (Digit (int t.[0] - int '0'))
    | _ -> None

/// Parse a modifier token (case-insensitive); `None` for an unknown token.
let private parseModifier (token : string) : Modifier option =
    match token.ToUpperInvariant() with
    | "CTRL" | "CONTROL" -> Some Ctrl
    | "SHIFT" -> Some Shift
    | "ALT" -> Some Alt
    | _ -> None

/// Parse a gesture string into a `KeyGesture` (§E.8.1). Returns `None` on a malformed
/// string (unknown key/modifier token, or no key) so a bad override is ignored rather
/// than throwing — the default binding then stands.
let parseKeyGesture (s : string) : KeyGesture option =
    let tokens = s.Split('+') |> Array.map (fun t -> t.Trim()) |> Array.filter (fun t -> t <> "")
    match Array.toList tokens with
    | [] -> None
    | toks ->
        let keyTok = List.last toks
        let modToks = toks |> List.take (List.length toks - 1)
        match parseKeyToken keyTok with
        | None -> None
        | Some k ->
            let mods = modToks |> List.map parseModifier
            if List.exists Option.isNone mods then None
            else Some { key = k; modifiers = mods |> List.choose id |> Set.ofList }

// ---------------------------------------------------------------------------
// The configurable key map (§E.8.1). Overrides are applied OVER the default registry,
// so the customized key map is still projected from the one registry — no second site.
// ---------------------------------------------------------------------------

/// Replace a command definition's keyboard gesture with `g`, preserving any mouse part.
let private withKeyGesture (g : KeyGesture) (d : CommandDef) : CommandDef =
    let binding' =
        match d.binding with
        | KeyboardOnly _ -> KeyboardOnly [ g ]
        | MouseOnly ms -> KeyboardAndMouse ([ g ], ms)
        | KeyboardAndMouse (_, ms) -> KeyboardAndMouse ([ g ], ms)
    { d with binding = binding' }

/// Apply the configured key-binding overrides over the default registry (§E.8.1). Each
/// override is a `(command-id, gesture-string)` pair; a malformed gesture or an unknown
/// id is ignored (the default binding stands). The result is still a registry the key
/// map projects from — there is no second binding site.
let withKeyOverrides (overrides : (string * string) list) : CommandDef list =
    let parsed =
        overrides
        |> List.choose (fun (cmdId, gestureStr) ->
            match parseKeyGesture gestureStr with
            | Some g -> Some (cmdId, g)
            | None -> None)
        |> Map.ofList
    registry
    |> List.map (fun d ->
        match Map.tryFind d.id parsed with
        | Some g -> withKeyGesture g d
        | None -> d)

/// The key map projected from a (possibly customized) registry.
let keyLookupOf (defs : CommandDef list) : Map<KeyGesture, Command> =
    defs
    |> List.collect (fun d -> d.binding.keyGestures |> List.map (fun g -> g, d.command))
    |> Map.ofList

/// Resolve a keyboard gesture against the key map customized by `overrides` (§E.8.1).
/// With no overrides this equals `lookupKey`.
let resolveKey (overrides : (string * string) list) (g : KeyGesture) : Command option =
    if List.isEmpty overrides then lookupKey g
    else Map.tryFind g (keyLookupOf (withKeyOverrides overrides))
