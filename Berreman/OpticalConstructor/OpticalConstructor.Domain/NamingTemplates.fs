namespace OpticalConstructor.Domain

open System
open System.Globalization
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.Experiments

/// The kit's naming template: its grammar, and the names it writes for setups and experiments and reads back to match
/// imported data (spec 0050 S8.3; 001 §J).
module NamingTemplates =

    /// One piece of text a name holds: an item's name, the note, or the literal text between placeholders
    /// (spec 0050 S8.3).
    type NamePart =
        | NamePart of string

        member this.value : string = let (NamePart text) = this in text

    /// In its placeholder, the kit item of `label` is written as `part` (spec 0050 S8.3; 001 §A).
    type NameAlias =
        {
            label : ItemLabel
            part : NamePart
        }

    /// Whether a name must hold a value for a placeholder (spec 0050 S8.3).
    type PlaceholderNeed =
        | RequiredPlaceholder
        | OptionalPlaceholder

    /// What a placeholder names, in the polarimeter's words: a slot by its role, an object's angle, or the note. It is
    /// never stored apart from the template's text, so it carries no wire tag (spec 0050 S8.3; 001 §J, §O).
    type TemplatePlaceholder =
        | SlotPlaceholder of role : SlotRole
        | AnglePlaceholder of position : SlotPosition
        | NotePlaceholder

        /// The placeholder's word with its number: `source` is a Prep slot, `light` the Source slot (spec 0050 S8.3).
        member this.text : string =
            match this with
            | SlotPlaceholder SourceSlotRole -> "light"
            | SlotPlaceholder (PrepSlotRole position) -> $"source{position.value}"
            | SlotPlaceholder (ObjectSlotRole position) -> $"obj{position.value}"
            | SlotPlaceholder (AnalyzerSlotRole position) -> $"analyzer{position.value}"
            | SlotPlaceholder (DetectorSlotRole position) -> $"detector{position.value}"
            | AnglePlaceholder position -> $"phi{position.value}"
            | NotePlaceholder -> "note"

        /// The light and the first slot of each role, with its angle, are required; a later slot, its angle and the
        /// note are optional (spec 0050 S8.3).
        member this.need : PlaceholderNeed =
            let ofPosition (position : SlotPosition) : PlaceholderNeed =
                if position = SlotPosition 1 then RequiredPlaceholder else OptionalPlaceholder

            match this with
            | SlotPlaceholder SourceSlotRole -> RequiredPlaceholder
            | SlotPlaceholder (PrepSlotRole position)
            | SlotPlaceholder (ObjectSlotRole position)
            | SlotPlaceholder (AnalyzerSlotRole position)
            | SlotPlaceholder (DetectorSlotRole position)
            | AnglePlaceholder position -> ofPosition position
            | NotePlaceholder -> OptionalPlaceholder

    /// A placeholder as the template writes it, with the aliases its slot's items are written under (spec 0050 S8.3).
    type TemplateValue =
        {
            placeholder : TemplatePlaceholder
            aliases : NameAlias list
        }

    /// One piece of a template's text: literal text, or a placeholder's value (spec 0050 S8.3).
    type TemplatePiece =
        | LiteralPiece of NamePart
        | ValuePiece of TemplateValue

    /// Why a text is no naming template, or a name does not fit one; each reason names the template's text, the
    /// placeholder or the name (spec 0050 S8.3).
    type NamingTemplateError =
        | TemplateSyntaxErr of reason : string
        | DuplicatePlaceholderErr of reason : string
        | AdjacentPlaceholdersErr of reason : string
        | DuplicateAliasErr of reason : string
        | MissingNameValueErr of reason : string
        | NameMismatchErr of reason : string
        | UnknownNameItemErr of reason : string

        member this.describe : string =
            match this with
            | TemplateSyntaxErr reason -> $"the naming template is not valid: {reason}"
            | DuplicatePlaceholderErr reason -> $"the naming template names a placeholder twice: {reason}"
            | AdjacentPlaceholdersErr reason -> $"the naming template has two placeholders with nothing between them: {reason}"
            | DuplicateAliasErr reason -> $"the naming template aliases an item twice: {reason}"
            | MissingNameValueErr reason -> $"the name has nothing for a placeholder it needs: {reason}"
            | NameMismatchErr reason -> $"the name does not fit the naming template: {reason}"
            | UnknownNameItemErr reason -> $"the name names no item of the kit: {reason}"

    type NamingTemplate with

        /// The template's text read left to right into its literals and placeholder values, or the reason it is no
        /// naming template (spec 0050 S8.3; 001 §J).
        member this.pieces : Result<TemplatePiece list, NamingTemplateError> =
            let text = this.value
            let braces = [| '{'; '}' |]
            let reserved = "{}:=,"
            let syntaxError (reason : string) : NamingTemplateError = TemplateSyntaxErr $"'{text}': {reason}"

            let positionOf (written : string) (number : string) : Result<SlotPosition, NamingTemplateError> =
                if number = "" then
                    Ok (SlotPosition 1)
                elif number.StartsWith '0' || not (String.forall Char.IsAsciiDigit number) then
                    Error (syntaxError $"in {written}, '{number}' is no slot number (ASCII digits not starting with 0)")
                else
                    match Int32.TryParse(number, NumberStyles.None, CultureInfo.InvariantCulture) with
                    | true, position -> Ok (SlotPosition position)
                    | false, _ -> Error (syntaxError $"in {written}, '{number}' is too large for a slot number")

            let placeholderOf (written : string) (head : string) : Result<TemplatePlaceholder, NamingTemplateError> =
                let word, number =
                    match head |> Seq.tryFindIndex Char.IsAsciiDigit with
                    | Some at -> head.Substring(0, at), head.Substring at
                    | None -> head, ""

                let slot (role : SlotPosition -> SlotRole) : Result<TemplatePlaceholder, NamingTemplateError> =
                    positionOf written number |> Result.map (fun position -> SlotPlaceholder (role position))

                match word with
                | "light" when number = "" -> Ok (SlotPlaceholder SourceSlotRole)
                | "note" when number = "" -> Ok NotePlaceholder
                | "light"
                | "note" -> Error (syntaxError $"in {written}, '{word}' takes no number")
                | "source" -> slot PrepSlotRole
                | "obj" -> slot ObjectSlotRole
                | "analyzer" -> slot AnalyzerSlotRole
                | "detector" -> slot DetectorSlotRole
                | "phi" -> positionOf written number |> Result.map AnglePlaceholder
                | _ -> Error (syntaxError $"in {written}, '{word}' names no placeholder")

            let aliasOf (written : string) (entry : string) : Result<NameAlias, NamingTemplateError> =
                let clean (side : string) : bool = side <> "" && not (String.exists (fun c -> reserved.Contains c) side)

                match entry.Split '=' with
                | [| label; part |] when clean label && clean part ->
                    match ItemLabel.tryCreate label with
                    | Ok itemLabel -> Ok { label = itemLabel; part = NamePart part }
                    | Error error -> Error (syntaxError $"in {written}, the alias '{entry}': {error.describe}")
                | _ ->
                    let rule = $"label=part, each side non-empty and without any of {reserved}"
                    Error (syntaxError $"in {written}, the alias '{entry}' is not {rule}")

            let valueOf (written : string) (body : string) : Result<TemplateValue, NamingTemplateError> =
                let head, aliasesOpt =
                    match body.IndexOf ':' with
                    | -1 -> body, None
                    | at -> body.Substring(0, at), Some (body.Substring(at + 1))

                placeholderOf written head
                |> Result.bind (fun placeholder ->
                    match placeholder, aliasesOpt with
                    | _, None -> Ok { placeholder = placeholder; aliases = [] }
                    | SlotPlaceholder _, Some aliases ->
                        let collect (held : Result<NameAlias list, NamingTemplateError>) (entry : string) : Result<NameAlias list, NamingTemplateError> =
                            held |> Result.bind (fun read -> aliasOf written entry |> Result.map (fun alias -> alias :: read))

                        aliases.Split ','
                        |> Array.fold collect (Ok [])
                        |> Result.map (fun read -> { placeholder = placeholder; aliases = List.rev read })
                    | AnglePlaceholder _, Some _
                    | NotePlaceholder, Some _ -> Error (syntaxError $"in {written}, only a slot placeholder takes aliases"))

            let withLiteral (start : int) (finish : int) (read : TemplatePiece list) : TemplatePiece list =
                if finish > start then LiteralPiece (NamePart (text.Substring(start, finish - start))) :: read else read

            let rec scan (start : int) (read : TemplatePiece list) : Result<TemplatePiece list, NamingTemplateError> =
                match text.IndexOfAny(braces, start) with
                | -1 -> Ok (List.rev (withLiteral start text.Length read))
                | at when text[at] = '}' -> Error (syntaxError $"the '}}' at character {at + 1} closes no placeholder")
                | opening ->
                    match text.IndexOfAny(braces, opening + 1) with
                    | -1 -> Error (syntaxError $"the '{{' at character {opening + 1} is never closed")
                    | at when text[at] = '{' -> Error (syntaxError $"the '{{' at character {at + 1} opens inside a placeholder")
                    | closing ->
                        let written = text.Substring(opening, closing - opening + 1)

                        valueOf written (text.Substring(opening + 1, closing - opening - 1))
                        |> Result.bind (fun value -> scan (closing + 1) (ValuePiece value :: withLiteral start opening read))

            let values (pieces : TemplatePiece list) : TemplateValue list =
                pieces
                |> List.choose (fun piece ->
                    match piece with
                    | ValuePiece value -> Some value
                    | LiteralPiece _ -> None)

            let repeatedPlaceholder (pieces : TemplatePiece list) : NamingTemplateError option =
                values pieces
                |> List.countBy (fun value -> value.placeholder)
                |> List.tryFind (fun (_, count) -> count > 1)
                |> Option.map (fun (placeholder, _) ->
                    let repeat = $"'{placeholder.text}' appears more than once"
                    DuplicatePlaceholderErr $"'{text}': {repeat} (a placeholder without a number is its first)")

            let adjacentPlaceholders (pieces : TemplatePiece list) : NamingTemplateError option =
                List.pairwise pieces
                |> List.tryPick (fun pair ->
                    match pair with
                    | ValuePiece first, ValuePiece second ->
                        Some (AdjacentPlaceholdersErr $"'{text}': '{first.placeholder.text}' and '{second.placeholder.text}'")
                    | _ -> None)

            let repeatedAlias (pieces : TemplatePiece list) : NamingTemplateError option =
                let rec firstRepeat (value : TemplateValue) (seen : NameAlias list) (rest : NameAlias list) : NamingTemplateError option =
                    match rest with
                    | [] -> None
                    | alias :: later ->
                        let sameLabel (held : NameAlias) : bool =
                            String.Equals(held.label.value, alias.label.value, StringComparison.OrdinalIgnoreCase)

                        let pair (held : NameAlias) : string =
                            $"'{text}': '{value.placeholder.text}' aliases '{held.label.value}' and '{alias.label.value}'"

                        match seen |> List.tryFind sameLabel with
                        | Some held -> Some (DuplicateAliasErr $"{pair held}, one label ignoring case")
                        | None ->
                            match seen |> List.tryFind (fun held -> held.part = alias.part) with
                            | Some held -> Some (DuplicateAliasErr $"{pair held}, both as '{alias.part.value}'")
                            | None -> firstRepeat value (alias :: seen) later

                values pieces |> List.tryPick (fun value -> firstRepeat value [] value.aliases)

            scan 0 []
            |> Result.bind (fun pieces ->
                match [ repeatedPlaceholder; adjacentPlaceholders; repeatedAlias ] |> List.tryPick (fun refusal -> refusal pieces) with
                | Some error -> Error error
                | None -> Ok pieces)

        /// The template of that text, or the reason it is none (spec 0050 S8.3).
        static member tryCreate (text : string) : Result<NamingTemplate, NamingTemplateError> =
            let template = NamingTemplate text
            template.pieces |> Result.map (fun _ -> template)

    /// A whole name as text: a setup's or an experiment's name, or a label a lab file carries (spec 0050 S8.3).
    type NameText =
        | NameText of string

        member this.value : string = let (NameText text) = this in text

    /// The text a name holds for the slot of `role` (spec 0050 S8.3).
    type SlotPart =
        {
            role : SlotRole
            part : NamePart
        }

    /// The angle a name holds for the object at `position` (spec 0050 S8.3).
    type ObjectAngle =
        {
            position : SlotPosition
            angle : Angle
        }

    /// The values a name holds, each list in its template's order (spec 0050 S8.3; 001 §J).
    type NameReading =
        {
            parts : SlotPart list
            angles : ObjectAngle list
            noteOpt : NamePart option
        }

        /// The text the name holds for the slot of `role`, if any (spec 0050 S8.3).
        member this.tryFindPart (role : SlotRole) : NamePart option =
            this.parts |> List.tryFind (fun slotPart -> slotPart.role = role) |> Option.map (fun slotPart -> slotPart.part)

        /// The angle the name holds for the object at `position`, if any (spec 0050 S8.3).
        member this.tryFindAngle (position : SlotPosition) : Angle option =
            this.angles
            |> List.tryFind (fun objectAngle -> objectAngle.position = position)
            |> Option.map (fun objectAngle -> objectAngle.angle)

    type NamingTemplate with

        /// The name the template writes for the reading: each value after the text its placeholder owns, an optional
        /// value the reading lacks left out with that text, and the tail last (spec 0050 S8.3; 001 §J).
        member this.writeName (reading : NameReading) : Result<NameText, NamingTemplateError> =
            // Degrees to three decimals, invariant, with no exponent, trailing zero or sign on zero (spec 0050 S8.3).
            let degreesText (angle : Angle) : string =
                let rounded = Math.Round(angle.degrees, 3)
                let unsigned = if rounded = 0.0 then 0.0 else rounded
                unsigned.ToString("0.###", CultureInfo.InvariantCulture)

            let valueOf (placeholder : TemplatePlaceholder) : string option =
                match placeholder with
                | SlotPlaceholder role -> reading.tryFindPart role |> Option.map (fun part -> part.value)
                | AnglePlaceholder position -> reading.tryFindAngle position |> Option.map degreesText
                | NotePlaceholder -> reading.noteOpt |> Option.map (fun note -> note.value)

            let rec write (owned : string) (pieces : TemplatePiece list) (written : string list) : Result<NameText, NamingTemplateError> =
                match pieces with
                | [] -> Ok (NameText (String.concat "" (List.rev (owned :: written))))
                | LiteralPiece literal :: later -> write (owned + literal.value) later written
                | ValuePiece value :: later ->
                    match valueOf value.placeholder, value.placeholder.need with
                    | Some text, _ -> write "" later (text :: owned :: written)
                    | None, OptionalPlaceholder -> write "" later written
                    | None, RequiredPlaceholder -> Error (MissingNameValueErr $"'{value.placeholder.text}' of '{this.value}'")

            this.pieces |> Result.bind (fun pieces -> write "" pieces [])

        /// The values the name holds, by the first way of leaving out the template's optional groups that reads it (all
        /// kept first, an earlier group kept longest), or `NameMismatchErr` naming the name (spec 0050 S8.3; 001 §J).
        member this.readName (name : NameText) : Result<NameReading, NamingTemplateError> =
            let text = name.value

            let angleOf (value : string) : Angle option =
                let digits (part : string) : bool = part <> "" && String.forall Char.IsAsciiDigit part
                let unsigned = if value.StartsWith '-' then value.Substring 1 else value

                let shaped =
                    match unsigned.IndexOf '.' with
                    | -1 -> digits unsigned
                    | dot -> digits (unsigned.Substring(0, dot)) && digits (unsigned.Substring(dot + 1))

                let styles = NumberStyles.AllowLeadingSign ||| NumberStyles.AllowDecimalPoint

                match shaped, Double.TryParse(value, styles, CultureInfo.InvariantCulture) with
                | true, (true, degrees) when Double.IsFinite degrees -> Some (Angle.degree (degrees * 1.0<deg>))
                | _ -> None

            let readPieces (pieces : TemplatePiece list) : NameReading option =
                let ownedEntries, tail =
                    pieces
                    |> List.fold
                        (fun (entries : (string * TemplatePlaceholder) list, pending : string) piece ->
                            match piece with
                            | LiteralPiece literal -> entries, pending + literal.value
                            | ValuePiece value -> (pending, value.placeholder) :: entries, "")
                        ([], "")

                let entries = List.rev ownedEntries

                let groups : TemplatePlaceholder list list =
                    entries
                    |> List.choose (fun (_, placeholder) ->
                        match placeholder.need, placeholder with
                        | RequiredPlaceholder, _ -> None
                        | OptionalPlaceholder, SlotPlaceholder (ObjectSlotRole position)
                        | OptionalPlaceholder, AnglePlaceholder position -> Some (Choice1Of2 position, placeholder)
                        | OptionalPlaceholder, _ -> Some (Choice2Of2 placeholder, placeholder))
                    |> List.groupBy fst
                    |> List.map (fun (_, members) -> List.map snd members)

                // Binary counting over the groups, the last the lowest digit and a left-out group a one.
                let rec ways (rest : TemplatePlaceholder list list) : seq<TemplatePlaceholder list> =
                    match rest with
                    | [] -> Seq.singleton []
                    | group :: later ->
                        seq {
                            yield! ways later
                            yield! ways later |> Seq.map (fun leftOut -> group @ leftOut)
                        }

                let rec readFrom (at : int) (kept : (string * TemplatePlaceholder) list) (read : NameReading) : NameReading option =
                    match kept with
                    | [] when String.Equals(text.Substring at, tail, StringComparison.Ordinal) ->
                        Some { parts = List.rev read.parts; angles = List.rev read.angles; noteOpt = read.noteOpt }
                    | [] -> None
                    | (owned, placeholder) :: later when text.Substring(at).StartsWith(owned, StringComparison.Ordinal) ->
                        let start = at + owned.Length

                        let follow =
                            match later with
                            | (next, _) :: _ -> next
                            | [] -> tail

                        let finishOpt =
                            if start >= text.Length then None
                            elif follow = "" then Some text.Length
                            else
                                match text.IndexOf(follow, start + 1, StringComparison.Ordinal) with
                                | -1 -> None
                                | finish -> Some finish

                        finishOpt
                        |> Option.bind (fun finish ->
                            let value = text.Substring(start, finish - start)

                            let withValue : NameReading option =
                                match placeholder with
                                | SlotPlaceholder role -> Some { read with parts = { role = role; part = NamePart value } :: read.parts }
                                | AnglePlaceholder position ->
                                    angleOf value
                                    |> Option.map (fun angle -> { read with angles = { position = position; angle = angle } :: read.angles })
                                | NotePlaceholder -> Some { read with noteOpt = Some (NamePart value) }

                            withValue |> Option.bind (readFrom finish later))
                    | _ :: _ -> None

                ways groups
                |> Seq.tryPick (fun leftOut ->
                    let kept = entries |> List.filter (fun (_, placeholder) -> not (List.contains placeholder leftOut))
                    readFrom 0 kept { parts = []; angles = []; noteOpt = None })

            this.pieces
            |> Result.bind (fun pieces ->
                match readPieces pieces with
                | Some reading -> Ok reading
                | None -> Error (NameMismatchErr $"'{text}' against '{this.value}'"))

    type ItemLabel with

        /// The label as a name writes it where no alias renames it (spec 0050 S8.3).
        member this.namePart : NamePart = NamePart this.value

    type NameText with

        /// The name as a setup's, refused when blank (spec 0050 S8.3).
        member this.toSetupName : Result<SetupName, SetupError> = SetupName.tryCreate this.value

    /// The kit item a name places in the slot of `role` (spec 0050 S8.3).
    type SlotItem =
        {
            role : SlotRole
            item : ItemId
        }

    type NamingTemplate with

        /// The values the template takes from the slots in light order: a slot's item of `items` under its placeholder's
        /// first alias of that label ignoring case, else its label; an object's `turnAngle` (S8.4; 001 §I); and the note.
        /// A missing slot, an empty one and an item `items` lacks give none (spec 0050 S8.3; 001 §J).
        member this.nameReadingOf (slots : SetupSlot list) (items : Item list) (noteOpt : NamePart option) : Result<NameReading, NamingTemplateError> =
            let lightOrder = SetupSlot.inLightOrder slots

            let tryFindHeld (role : SlotRole) : (SetupSlot * Item) option =
                lightOrder
                |> List.tryFind (fun roled -> roled.role = role)
                |> Option.bind (fun roled ->
                    roled.slot.content.itemOpt
                    |> Option.bind (fun id -> items |> List.tryFind (fun item -> item.id = id))
                    |> Option.map (fun item -> roled.slot, item))

            let partOf (value : TemplateValue) (item : Item) : NamePart =
                let labels (alias : NameAlias) : bool =
                    String.Equals(alias.label.value, item.label.value, StringComparison.OrdinalIgnoreCase)

                match value.aliases |> List.tryFind labels with
                | Some alias -> alias.part
                | None -> item.label.namePart

            let withValue (read : NameReading) (piece : TemplatePiece) : NameReading =
                match piece with
                | LiteralPiece _ -> read
                | ValuePiece value ->
                    match value.placeholder with
                    | SlotPlaceholder role ->
                        match tryFindHeld role with
                        | Some (_, item) -> { read with parts = read.parts @ [ { role = role; part = partOf value item } ] }
                        | None -> read
                    | AnglePlaceholder position ->
                        match tryFindHeld (ObjectSlotRole position) with
                        | Some (slot, _) -> { read with angles = read.angles @ [ { position = position; angle = slot.turnAngle } ] }
                        | None -> read
                    | NotePlaceholder -> { read with noteOpt = noteOpt }

            this.pieces |> Result.map (List.fold withValue { parts = []; angles = []; noteOpt = None })

        /// The item each part of the reading names in its placeholder, in order: the item labelled by the placeholder's
        /// alias of that part, else by the part itself, ignoring case; a part naming none answers `UnknownNameItemErr`
        /// (spec 0050 S8.3; 001 §J).
        member this.itemsOf (reading : NameReading) (items : Item list) : Result<SlotItem list, NamingTemplateError> =
            // Item labels are unique by file stem ignoring case (step 216), so at most one item matches.
            let tryFindLabelled (label : string) : Item option =
                items |> List.tryFind (fun item -> String.Equals(item.label.value, label, StringComparison.OrdinalIgnoreCase))

            this.pieces
            |> Result.bind (fun pieces ->
                let aliasesOf (role : SlotRole) : NameAlias list =
                    pieces
                    |> List.tryPick (fun piece ->
                        match piece with
                        | ValuePiece value when value.placeholder = SlotPlaceholder role -> Some value.aliases
                        | ValuePiece _
                        | LiteralPiece _ -> None)
                    |> Option.defaultValue []

                let itemOf (slotPart : SlotPart) : Result<SlotItem, NamingTemplateError> =
                    let namedOpt =
                        match aliasesOf slotPart.role |> List.tryFind (fun alias -> alias.part = slotPart.part) with
                        | Some alias -> tryFindLabelled alias.label.value
                        | None -> tryFindLabelled slotPart.part.value

                    match namedOpt with
                    | Some item -> Ok { role = slotPart.role; item = item.id }
                    | None ->
                        let placeholder = SlotPlaceholder slotPart.role
                        Error (UnknownNameItemErr $"'{slotPart.part.value}' in '{placeholder.text}' of '{this.value}'")

                let collect (held : Result<SlotItem list, NamingTemplateError>) (slotPart : SlotPart) : Result<SlotItem list, NamingTemplateError> =
                    held |> Result.bind (fun found -> itemOf slotPart |> Result.map (fun slotItem -> slotItem :: found))

                reading.parts |> List.fold collect (Ok []) |> Result.map List.rev)

        /// The name the template writes for the slots, without a note, as a setup's; a blank one answers
        /// `MissingNameValueErr` naming the template (spec 0050 S8.3).
        member this.setupNameOf (slots : SetupSlot list) (items : Item list) : Result<SetupName, NamingTemplateError> =
            this.nameReadingOf slots items None
            |> Result.bind (fun reading -> this.writeName reading)
            |> Result.bind (fun name ->
                name.toSetupName
                |> Result.mapError (fun error -> MissingNameValueErr $"'{this.value}' writes '{name.value}': {error.describe}"))

        /// A new setup's name: the template's made free among `taken`, or `unnamed` when it cannot name the slots. Held
        /// setups keep their names; a template names what is made after it (spec 0050 S8.2, S8.3).
        member this.setupNameAmong (slots : SetupSlot list) (items : Item list) (taken : SetupName list) (unnamed : SetupName) : SetupName =
            match this.setupNameOf slots items with
            | Ok name -> name.nextFreeAmong taken
            | Error _ -> unnamed

    type NameText with

        /// The name as an experiment's, refused when blank (spec 0050 S9.1).
        member this.toExperimentName : Result<ExperimentName, ExperimentError> = ExperimentName.tryCreate this.value

    type NamingTemplate with

        /// A new experiment's name: the template's for the slots and the note, made free among `taken`, or `unnamed`
        /// when it cannot name them, as `setupNameAmong` names a setup (spec 0050 S9.1, S8.3).
        member this.experimentNameAmong
            (slots : SetupSlot list)
            (items : Item list)
            (noteOpt : NamePart option)
            (taken : ExperimentName list)
            (unnamed : ExperimentName)
            : ExperimentName =
            match this.nameReadingOf slots items noteOpt |> Result.bind (fun reading -> this.writeName reading) with
            | Ok name ->
                match name.toExperimentName with
                | Ok experimentName -> experimentName.nextFreeAmong taken
                | Error _ -> unnamed
            | Error _ -> unnamed
