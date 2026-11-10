namespace OpticalConstructor.Domain

open System
open System.Globalization
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Setups

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
