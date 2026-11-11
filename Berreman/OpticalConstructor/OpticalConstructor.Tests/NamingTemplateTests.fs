namespace OpticalConstructor.Tests

open Xunit
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.NamingTemplates

module NamingTemplateTests =

    let private plainValue (placeholder : TemplatePlaceholder) : TemplatePiece =
        ValuePiece
            {
                placeholder = placeholder
                aliases = []
            }

    let private piecesOf (template : NamingTemplate) : TemplatePiece list =
        match template.pieces with
        | Ok pieces -> pieces
        | Error error -> failwith $"'{template.value}' was refused: {error.describe}"

    let private placeholdersOf (pieces : TemplatePiece list) : TemplatePlaceholder list =
        pieces
        |> List.choose (fun piece ->
            match piece with
            | ValuePiece value -> Some value.placeholder
            | LiteralPiece _ -> None)

    /// The error both `pieces` and `tryCreate` answer for a text that is no template, after checking they agree and
    /// that the reason names the text.
    let private refusalOf (text : string) : NamingTemplateError =
        match (NamingTemplate text).pieces, NamingTemplate.tryCreate text with
        | Error fromPieces, Error fromCreate ->
            Assert.Equal<NamingTemplateError>(fromPieces, fromCreate)
            Assert.Contains(text, fromPieces.describe)
            fromPieces
        | Ok pieces, _ -> failwith $"'{text}' was read into {pieces.Length} pieces"
        | _, Ok created -> failwith $"'{text}' was created as '{created.value}'"

    [<Fact>]
    let ``the standard template is the lab's: the source, two objects at their angles, the analyzer and the note`` () : unit =
        let prep1 = SlotPlaceholder (PrepSlotRole (SlotPosition 1))
        let obj1 = SlotPlaceholder (ObjectSlotRole (SlotPosition 1))
        let phi1 = AnglePlaceholder (SlotPosition 1)
        let obj2 = SlotPlaceholder (ObjectSlotRole (SlotPosition 2))
        let phi2 = AnglePlaceholder (SlotPosition 2)
        let analyzer1 = SlotPlaceholder (AnalyzerSlotRole (SlotPosition 1))

        let expected : TemplatePiece list =
            [
                plainValue prep1
                LiteralPiece (NamePart "-(")
                plainValue obj1
                LiteralPiece (NamePart "#")
                plainValue phi1
                LiteralPiece (NamePart "-")
                plainValue obj2
                LiteralPiece (NamePart "#")
                plainValue phi2
                LiteralPiece (NamePart ")-")
                plainValue analyzer1
                LiteralPiece (NamePart "-")
                plainValue NotePlaceholder
            ]

        Assert.Equal<Result<TemplatePiece list, NamingTemplateError>>(Ok expected, NamingTemplate.standard.pieces)

        [ prep1; obj1; phi1; analyzer1 ]
        |> List.iter (fun placeholder -> Assert.Equal<PlaceholderNeed>(RequiredPlaceholder, placeholder.need))

        [ obj2; phi2; NotePlaceholder ]
        |> List.iter (fun placeholder -> Assert.Equal<PlaceholderNeed>(OptionalPlaceholder, placeholder.need))

    [<Fact>]
    let ``a placeholder without a number names the first slot of its role, and each role has its word`` () : unit =
        let expected : TemplatePlaceholder list =
            [
                SlotPlaceholder SourceSlotRole
                SlotPlaceholder (PrepSlotRole (SlotPosition 2))
                SlotPlaceholder (ObjectSlotRole (SlotPosition 3))
                AnglePlaceholder (SlotPosition 3)
                SlotPlaceholder (AnalyzerSlotRole (SlotPosition 1))
                SlotPlaceholder (DetectorSlotRole (SlotPosition 2))
                NotePlaceholder
            ]

        let separator = LiteralPiece (NamePart "/")

        let expectedPieces : TemplatePiece list =
            (expected |> List.map plainValue |> List.collect (fun value -> [ separator; value ]) |> List.tail)
            @ [ LiteralPiece (NamePart ".csv") ]

        let pieces = piecesOf (NamingTemplate "{light}/{source2}/{obj3}/{phi3}/{analyzer}/{detector2}/{note}.csv")

        Assert.Equal<TemplatePiece list>(expectedPieces, pieces)
        Assert.Equal<TemplatePlaceholder list>(expected, placeholdersOf pieces)

        Assert.Equal<string list>(
            [ "light"; "source2"; "obj3"; "phi3"; "analyzer1"; "detector2"; "note" ],
            expected |> List.map (fun placeholder -> placeholder.text))

        [ "source"; "obj"; "phi"; "analyzer"; "detector" ]
        |> List.iter (fun word ->
            Assert.Equal<TemplatePiece list>(piecesOf (NamingTemplate $"{{{word}1}}"), piecesOf (NamingTemplate $"{{{word}}}")))

        Assert.Equal<PlaceholderNeed>(RequiredPlaceholder, (SlotPlaceholder SourceSlotRole).need)
        Assert.Equal<PlaceholderNeed>(RequiredPlaceholder, (SlotPlaceholder (DetectorSlotRole (SlotPosition 1))).need)
        Assert.Equal<PlaceholderNeed>(OptionalPlaceholder, (SlotPlaceholder (PrepSlotRole (SlotPosition 2))).need)
        Assert.Equal<PlaceholderNeed>(OptionalPlaceholder, (AnglePlaceholder (SlotPosition 3)).need)

    [<Fact>]
    let ``an alias writes a kit item under another name in its placeholder`` () : unit =
        let prep1 : TemplatePiece =
            ValuePiece
                {
                    placeholder = SlotPlaceholder (PrepSlotRole (SlotPosition 1))
                    aliases =
                        [
                            { label = ItemLabel "CPL1"; part = NamePart "CPL" }
                            { label = ItemLabel "LP1"; part = NamePart "LP" }
                        ]
                }

        Assert.Equal<TemplatePiece list>([ prep1 ], piecesOf (NamingTemplate "{source:CPL1=CPL,LP1=LP}"))

        let analyzer1 : TemplatePiece =
            ValuePiece
                {
                    placeholder = SlotPlaceholder (AnalyzerSlotRole (SlotPosition 1))
                    aliases = [ { label = ItemLabel "CPL2"; part = NamePart "CPL" } ]
                }

        let expected : TemplatePiece list =
            [
                prep1
                LiteralPiece (NamePart "-(")
                plainValue (SlotPlaceholder (ObjectSlotRole (SlotPosition 1)))
                LiteralPiece (NamePart "#")
                plainValue (AnglePlaceholder (SlotPosition 1))
                LiteralPiece (NamePart ")-")
                analyzer1
            ]

        Assert.Equal<TemplatePiece list>(expected, piecesOf (NamingTemplate "{source:CPL1=CPL,LP1=LP}-({obj1}#{phi1})-{analyzer:CPL2=CPL}"))

    [<Fact>]
    let ``a text that is no naming template is refused with its reason`` () : unit =
        [
            "{sauce}"
            "{obj0}"
            "{obj01}"
            "{light1}"
            "{obj1"
            "obj1}"
            "{ob{j1}"
            "{phi1:A=B}"
            "{note:A=B}"
            "{source:CPL1}"
            "{source:=CPL}"
        ]
        |> List.iter (fun text ->
            match refusalOf text with
            | TemplateSyntaxErr _ -> ()
            | other -> Assert.Fail($"'{text}' expected TemplateSyntaxErr, got {other.describe}"))

        match refusalOf "{source}-{source1}" with
        | DuplicatePlaceholderErr _ -> ()
        | other -> Assert.Fail($"expected DuplicatePlaceholderErr, got {other.describe}")

        match refusalOf "{obj1}{phi1}" with
        | AdjacentPlaceholdersErr _ -> ()
        | other -> Assert.Fail($"expected AdjacentPlaceholdersErr, got {other.describe}")

        [ "{source:CPL1=CPL,cpl1=C}"; "{source:A=CPL,B=CPL}" ]
        |> List.iter (fun text ->
            match refusalOf text with
            | DuplicateAliasErr _ -> ()
            | other -> Assert.Fail($"'{text}' expected DuplicateAliasErr, got {other.describe}"))

        Assert.Equal<Result<NamingTemplate, NamingTemplateError>>(
            Ok NamingTemplate.standard,
            NamingTemplate.tryCreate NamingTemplate.standard.value)

    [<Fact>]
    let ``every naming template error renders its condition and its payload`` () : unit =
        let syntaxReason = "'{sauce}': 'sauce' names no placeholder"
        let duplicateReason = "'{source}-{source1}': 'source1' appears twice"
        let adjacentReason = "'{obj1}{phi1}': 'obj1' and 'phi1'"
        let aliasReason = "'{source:A=CPL,B=CPL}': 'A' and 'B' both as 'CPL'"
        let missingReason = "'QZ#45--LR#0': nothing for 'obj1'"
        let mismatchReason = "'QZ_45': no '-(' after 'source1'"
        let unknownReason = "'XYZ' in 'obj1'"

        let cases : (NamingTemplateError * string) list =
            [
                TemplateSyntaxErr syntaxReason, $"the naming template is not valid: {syntaxReason}"
                DuplicatePlaceholderErr duplicateReason, $"the naming template names a placeholder twice: {duplicateReason}"
                AdjacentPlaceholdersErr adjacentReason, $"the naming template has two placeholders with nothing between them: {adjacentReason}"
                DuplicateAliasErr aliasReason, $"the naming template aliases an item twice: {aliasReason}"
                MissingNameValueErr missingReason, $"the name has nothing for a placeholder it needs: {missingReason}"
                NameMismatchErr mismatchReason, $"the name does not fit the naming template: {mismatchReason}"
                UnknownNameItemErr unknownReason, $"the name names no item of the kit: {unknownReason}"
            ]

        cases |> List.iter (fun (error, expected) -> Assert.Equal(expected, error.describe))
        Assert.Equal(cases.Length, cases |> List.map (fun (error, _) -> error.describe) |> List.distinct |> List.length)

    let private sourceRole : SlotRole = PrepSlotRole (SlotPosition 1)
    let private analyzerRole : SlotRole = AnalyzerSlotRole (SlotPosition 1)

    /// The reading the lab's grammar gives a label: the source, the objects at their angles in order, the analyzer
    /// and the note, each list in the standard template's order.
    let private labReading (source : NamePart) (objects : (NamePart * Angle) list) (analyzer : NamePart) (noteOpt : NamePart option) : NameReading =
        let positions = objects |> List.mapi (fun index _ -> SlotPosition (index + 1))

        {
            parts =
                [ { role = sourceRole; part = source } ]
                @ List.map2 (fun position (part, _) -> { role = ObjectSlotRole position; part = part }) positions objects
                @ [ { role = analyzerRole; part = analyzer } ]
            angles = List.map2 (fun position (_, angle) -> { position = position; angle = angle }) positions objects
            noteOpt = noteOpt
        }

    let private readingOf (name : NameText) : NameReading =
        match NamingTemplate.standard.readName name with
        | Ok reading -> reading
        | Error error -> failwith $"'{name.value}' was refused: {error.describe}"

    [<Fact>]
    let ``the lab's labels read into their source, objects, angles, analyzer and note`` () : unit =
        let cpl = NamePart "CPL"
        let lp = NamePart "LP"
        let qz = NamePart "QZ"
        let lr = NamePart "LR"
        let air = NamePart "AIR"

        let cases : (NameText * NameReading) list =
            [
                NameText "CPL-(QZ#90-LR#90)-CPL", labReading cpl [ qz, Angle.degree 90.0<deg>; lr, Angle.degree 90.0<deg> ] cpl None
                NameText "CPL-(AIR#0)-CPL", labReading cpl [ air, Angle.degree 0.0<deg> ] cpl None
                NameText "LP-(LR#90)-CPL-2", labReading lp [ lr, Angle.degree 90.0<deg> ] cpl (Some (NamePart "2"))
                NameText "CPL-(AIR#0)-CPL-BR", labReading cpl [ air, Angle.degree 0.0<deg> ] cpl (Some (NamePart "BR"))
                NameText "CPL-(LR#-45)-CPL", labReading cpl [ lr, Angle.degree (-45.0<deg>) ] cpl None
                NameText "LP-(QZ#22.5)-LP", labReading lp [ qz, Angle.degree 22.5<deg> ] lp None
            ]

        cases |> List.iter (fun (name, expected) -> Assert.Equal<NameReading>(expected, readingOf name))

        let both = readingOf (NameText "CPL-(QZ#90-LR#90)-CPL")
        Assert.Equal<NamePart option>(Some cpl, both.tryFindPart sourceRole)
        Assert.Equal<NamePart option>(Some lr, both.tryFindPart (ObjectSlotRole (SlotPosition 2)))
        Assert.Equal<NamePart option>(Some cpl, both.tryFindPart analyzerRole)
        Assert.Equal<Angle option>(Some (Angle.degree 90.0<deg>), both.tryFindAngle (SlotPosition 2))
        Assert.Equal<NamePart option>(None, both.noteOpt)

        let air0 = readingOf (NameText "CPL-(AIR#0)-CPL")
        Assert.Equal<NamePart option>(Some air, air0.tryFindPart (ObjectSlotRole (SlotPosition 1)))
        Assert.Equal<Angle option>(Some (Angle.degree 0.0<deg>), air0.tryFindAngle (SlotPosition 1))
        Assert.Equal<NamePart option>(None, air0.tryFindPart (ObjectSlotRole (SlotPosition 2)))
        Assert.Equal<Angle option>(None, air0.tryFindAngle (SlotPosition 2))

        let noted = readingOf (NameText "LP-(LR#90)-CPL-2")
        Assert.Equal<NamePart option>(Some lp, noted.tryFindPart sourceRole)
        Assert.Equal<NamePart option>(Some cpl, noted.tryFindPart analyzerRole)
        Assert.Equal<NamePart option>(Some (NamePart "2"), noted.noteOpt)

    [<Fact>]
    let ``a name that does not fit the template is refused naming it`` () : unit =
        [
            NameText "Darkness"
            NameText "BullshitCheck"
            NameText "BullshitCheck-2"
            NameText "LP-(Darkness)-CPL2"
            NameText "CPL-(QZ#ab)-CPL"
            NameText "CPL-(QZ#90-LR)-CPL"
            NameText ""
        ]
        |> List.iter (fun name ->
            match NamingTemplate.standard.readName name with
            | Error (NameMismatchErr reason) ->
                Assert.Contains($"'{name.value}'", reason)
                Assert.Contains(NamingTemplate.standard.value, reason)
            | Error other -> Assert.Fail($"'{name.value}' expected NameMismatchErr, got {other.describe}")
            | Ok reading -> Assert.Fail($"'{name.value}' was read into {reading.parts.Length} parts"))

    [<Fact>]
    let ``writing a reading back gives the name it was read from`` () : unit =
        [
            NameText "CPL-(QZ#90-LR#90)-CPL"
            NameText "CPL-(QZ#0-LR#0)-LP-2"
            NameText "LP-(AIR#0)-LP-2R"
            NameText "CPL-(AIR#0)-CPL-BR"
            NameText "LP-(LR#45)-CPL-2"
            NameText "CPL-(QZ#45-LR#45)-CPL-2"
            NameText "CPL-(LR#-45)-CPL"
            NameText "LP-(QZ#22.5)-LP"
        ]
        |> List.iter (fun name ->
            let written = NamingTemplate.standard.readName name |> Result.bind (fun reading -> NamingTemplate.standard.writeName reading)
            Assert.Equal<Result<NameText, NamingTemplateError>>(Ok name, written))

    [<Fact>]
    let ``a reading writes each value in its place and leaves out an optional one it lacks with the text it owns`` () : unit =
        let cpl = NamePart "CPL"
        let qz45 = labReading cpl [ NamePart "QZ", Angle.degree 45.0<deg> ] cpl None

        let writes (expected : NameText) (reading : NameReading) : unit =
            Assert.Equal<Result<NameText, NamingTemplateError>>(Ok expected, NamingTemplate.standard.writeName reading)

        let atAngle (angle : Angle) : NameReading =
            { qz45 with angles = [ { position = SlotPosition 1; angle = angle } ] }

        writes (NameText "CPL-(QZ#45)-CPL") qz45
        writes (NameText "CPL-(QZ#45)-CPL-2") { qz45 with noteOpt = Some (NamePart "2") }
        writes (NameText "CPL-(QZ#45-LR#45)-CPL") (labReading cpl [ NamePart "QZ", Angle.degree 45.0<deg>; NamePart "LR", Angle.degree 45.0<deg> ] cpl None)
        writes (NameText "CPL-(QZ#0.333)-CPL") (atAngle (Angle.degree (1.0 / 3.0 * 1.0<deg>)))
        writes (NameText "CPL-(QZ#0)-CPL") (atAngle (Angle.degree (-0.0001<deg>)))

        let noAnalyzer = { qz45 with parts = qz45.parts |> List.filter (fun slotPart -> slotPart.role <> analyzerRole) }

        match NamingTemplate.standard.writeName noAnalyzer with
        | Error (MissingNameValueErr reason) -> Assert.Contains("'analyzer1'", reason)
        | Error other -> Assert.Fail($"expected MissingNameValueErr, got {other.describe}")
        | Ok name -> Assert.Fail($"expected MissingNameValueErr, got '{name.value}'")
