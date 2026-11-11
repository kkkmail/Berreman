namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.NamingTemplates
open OpticalConstructor.Domain.MuellerReconstruction

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

    [<Fact>]
    let ``a lab label's reading names its reference family, matrix kind and object angle`` () : unit =
        let reads (name : NameText) (family : Family) (kind : MatrixKind) (phi : Angle) : unit =
            let reading = readingOf name
            Assert.Equal<Family>(family, reading.family)
            Assert.Equal<MatrixKind>(kind, reading.matrixKind)
            Assert.Equal<Angle>(phi, reading.objectPhi)
            Assert.Equal<Angle * MatrixKind>((phi, kind), reading.objectRotation)

        reads (NameText "CPL-(QZ#90-LR#90)-CPL") CplCpl QzLrProduct (Angle.degree 90.0<deg>)
        reads (NameText "CPL-(AIR#0)-CPL") CplCpl Air (Angle.degree 0.0<deg>)
        reads (NameText "LP-(LR#90)-CPL-2") LpCpl Lr (Angle.degree 90.0<deg>)
        reads (NameText "CPL-(QZ#45)-LP-2") CplLp Qz (Angle.degree 45.0<deg>)
        reads (NameText "LP-(QZ#0)-LP-2R") LpLp Qz (Angle.degree 0.0<deg>)

    // ============================ spec 0050 S8.3: a setup's name from its slots, and back ============================

    let private emptySlot (id : Library.ElementId) (kind : CatalogueKind) (point : TablePoint) : SetupSlot =
        {
            id = id
            placement = ElementPlacement.create kind point
            zoom = BeamLayout.ElementZoom 5.0
            beam = BeamLayout.OnMainBeam
            content = EmptySlotContent
        }

    let private src : SetupSlot = emptySlot (Library.ElementId "src") LightSource RayModel.defaultSourcePoint
    let private lpA : SetupSlot = emptySlot (Library.ElementId "lpA") LinearPolarizer { x = -0.3<meter>; y = 0.0<meter> }
    let private sampleA : SetupSlot = emptySlot (Library.ElementId "sampleA") Sample { x = -0.1<meter>; y = 0.0<meter> }
    let private sampleB : SetupSlot = emptySlot (Library.ElementId "sampleB") Sample { x = 0.1<meter>; y = 0.0<meter> }
    let private lpB : SetupSlot = emptySlot (Library.ElementId "lpB") LinearPolarizer { x = 0.3<meter>; y = 0.0<meter> }
    let private det : SetupSlot = emptySlot (Library.ElementId "det") Detector RayModel.defaultDetectorPoint

    let private heNeChoice : ItemModelChoice =
        SourcePresetChoice (BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId, BuiltinCatalogue.source Library.PresetIds.heNe633)

    let private circularPolarizerChoice : ItemModelChoice =
        LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId)

    let private blackBoxChoice : ItemModelChoice = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.blackBoxMuellerModelId)

    /// The draft's item, failing with the refusal's text when `addTo` refuses it.
    let private itemOf (id : ItemId) (label : ItemLabel) (model : ItemModelChoice) (dial : ItemDial) : Item =
        let draft : ItemDraft =
            {
                id = id
                label = label
                model = model
                dial = dial
            }

        match draft.addTo [] with
        | Ok item -> item
        | Error error -> failwith $"'{label.value}' was refused: {error.describe}"

    let private laser : Item =
        itemOf (ItemId (Guid.Parse "5a6b7c8d-9eaf-4b01-92c3-4e5f6a7b8c91")) (ItemLabel "Laser") heNeChoice FixedMounting

    let private cpl1 : Item =
        itemOf (ItemId (Guid.Parse "6b7c8d9e-af01-4c12-a3d4-5f6a7b8c9da2")) (ItemLabel "CPL1") circularPolarizerChoice FixedMounting

    let private cpl2 : Item =
        itemOf (ItemId (Guid.Parse "7c8d9eaf-0112-4d23-b4e5-6a7b8c9daeb3")) (ItemLabel "CPL2") circularPolarizerChoice FixedMounting

    let private qz : Item = itemOf (ItemId (Guid.Parse "8d9eaf01-1223-4e34-85f6-7b8c9daebfc4")) (ItemLabel "QZ") blackBoxChoice FixedMounting
    let private lr : Item = itemOf (ItemId (Guid.Parse "9eaf0112-2334-4f45-9607-8c9daebfc0d5")) (ItemLabel "LR") blackBoxChoice FixedMounting

    /// QZd sits on a dial that reads backwards, its zero at 10°.
    let private qzd : Item =
        let item =
            itemOf (ItemId (Guid.Parse "af011223-3445-4056-a718-9daebfc0d1e6")) (ItemLabel "QZd") blackBoxChoice (DialMounted DialNegative)

        match item.withParameterState ParameterName.dialZero (KnownParameter (ParameterValue (Angle.degree 10.0<deg>).value)) with
        | Ok zeroed -> zeroed
        | Error error -> failwith $"'QZd' refused a dial zero: {error.describe}"

    let private kitItems : Item list = [ laser; cpl1; cpl2; qz; lr; qzd ]

    /// The slot holding the item, put there by `withItem` as the table assigns it.
    let private holding (item : Item) (model : ItemModelChoice) (slot : SetupSlot) : SetupSlot =
        match slot.withItem item model with
        | Ok held -> held
        | Error error -> failwith $"'{item.label.value}' did not fit '{slot.id.value}': {error.describe}"

    /// The slot turned to that R1, then holding the item.
    let private turnedHolding (r1 : Angle) (item : Item) (model : ItemModelChoice) (slot : SetupSlot) : SetupSlot =
        holding item model { slot with placement = withR1 r1 slot.placement }

    let private qz45 : SetupSlot = turnedHolding (Angle.degree 45.0<deg>) qz blackBoxChoice sampleA
    let private lr45 : SetupSlot = turnedHolding (Angle.degree 45.0<deg>) lr blackBoxChoice sampleB

    /// The bench with these object slots, the light first and the others out of light order: the Laser, CPL1 before the
    /// objects, CPL2 behind them and an empty detector.
    let private benchWith (objA : SetupSlot) (objB : SetupSlot) : SetupSlot list =
        [
            holding laser heNeChoice src
            det
            holding cpl2 circularPolarizerChoice lpB
            objB
            objA
            holding cpl1 circularPolarizerChoice lpA
        ]

    let private fullBench : SetupSlot list = benchWith qz45 lr45
    let private qzOnlyBench : SetupSlot list = benchWith qz45 sampleB

    /// QZd at R1 30°, which its backwards dial zeroed at 10° reads as 10° − 30° = −20°.
    let private dialBench : SetupSlot list = benchWith (turnedHolding (Angle.degree 30.0<deg>) qzd blackBoxChoice sampleA) sampleB

    let private lab : NamingTemplate =
        match NamingTemplate.tryCreate "{source:CPL1=CPL,LP1=LP}-({obj1}#{phi1}-{obj2}#{phi2})-{analyzer:CPL2=CPL,LP2=LP}-{note}" with
        | Ok template -> template
        | Error error -> failwith $"the lab template was refused: {error.describe}"

    let private readByLab (name : NameText) : NameReading =
        match lab.readName name with
        | Ok reading -> reading
        | Error error -> failwith $"'{name.value}' was refused: {error.describe}"

    let private assertAngle (expected : Angle) (actual : Angle) : unit = Assert.Equal(expected.value, actual.value, 1e-12)

    [<Fact>]
    let ``a setup's name is written from its slots in light order, its items under their aliases and its objects at their angles`` () : unit =
        let names (template : NamingTemplate) (items : Item list) (expected : SetupName) (slots : SetupSlot list) : unit =
            Assert.Equal<Result<SetupName, NamingTemplateError>>(Ok expected, template.setupNameOf slots items)

        names lab kitItems (SetupName "CPL-(QZ#45-LR#45)-CPL") fullBench
        names NamingTemplate.standard kitItems (SetupName "CPL1-(QZ#45-LR#45)-CPL2") fullBench
        names lab kitItems (SetupName "CPL-(QZ#45)-CPL") qzOnlyBench
        names lab kitItems (SetupName "CPL-(QZd#-20)-CPL") dialBench

        // An item the items lack gives no value, as an empty slot gives none.
        names lab (kitItems |> List.filter (fun item -> item.id <> lr.id)) (SetupName "CPL-(QZ#45)-CPL") fullBench

        // CPL1 and CPL2 are each written under their own placeholder's alias; the light, which `lab` never names, is not.
        let expected : NameReading =
            {
                parts =
                    [
                        { role = sourceRole; part = NamePart "CPL" }
                        { role = ObjectSlotRole (SlotPosition 1); part = NamePart "QZ" }
                        { role = ObjectSlotRole (SlotPosition 2); part = NamePart "LR" }
                        { role = analyzerRole; part = NamePart "CPL" }
                    ]
                angles =
                    [
                        { position = SlotPosition 1; angle = Angle.degree 45.0<deg> }
                        { position = SlotPosition 2; angle = Angle.degree 45.0<deg> }
                    ]
                noteOpt = Some (NamePart "2")
            }

        let noted = lab.nameReadingOf fullBench kitItems (Some (NamePart "2"))
        Assert.Equal<Result<NameReading, NamingTemplateError>>(Ok expected, noted)

        Assert.Equal<Result<NameText, NamingTemplateError>>(
            Ok (NameText "CPL-(QZ#45-LR#45)-CPL-2"),
            noted |> Result.bind (fun reading -> lab.writeName reading))

    [<Fact>]
    let ``a name reads back into the items of its slots and their angles`` () : unit =
        let reading = readByLab (NameText "CPL-(QZ#45-LR#45)-CPL")

        let expected : SlotItem list =
            [
                { role = sourceRole; item = cpl1.id }
                { role = ObjectSlotRole (SlotPosition 1); item = qz.id }
                { role = ObjectSlotRole (SlotPosition 2); item = lr.id }
                { role = analyzerRole; item = cpl2.id }
            ]

        Assert.Equal<Result<SlotItem list, NamingTemplateError>>(Ok expected, lab.itemsOf reading kitItems)
        Assert.Equal<Angle option>(Some (Angle.degree 45.0<deg>), reading.tryFindAngle (SlotPosition 1))
        Assert.Equal<Angle option>(Some (Angle.degree 45.0<deg>), reading.tryFindAngle (SlotPosition 2))

        // A part that no alias owns is an item's label, ignoring case.
        let lowered = readByLab (NameText "CPL-(qz#45-LR#45)-CPL")
        Assert.Equal<Result<SlotItem list, NamingTemplateError>>(Ok expected, lab.itemsOf lowered kitItems)

        match lab.itemsOf (readByLab (NameText "CPL-(XX#45)-CPL")) kitItems with
        | Error (UnknownNameItemErr reason) ->
            Assert.Contains("'XX'", reason)
            Assert.Contains("'obj1'", reason)
        | Error other -> Assert.Fail($"expected UnknownNameItemErr, got {other.describe}")
        | Ok slotItems -> Assert.Fail($"'XX' was read into {slotItems.Length} items")

    [<Fact>]
    let ``writing a setup's name and reading it back gives its items and angles`` () : unit =
        let named = piecesOf lab |> placeholdersOf

        // Each bench with its objects' angles in position order; QZd's is its dial reading.
        let cases : (SetupSlot list * Angle list) list =
            [
                fullBench, [ Angle.degree 45.0<deg>; Angle.degree 45.0<deg> ]
                qzOnlyBench, [ Angle.degree 45.0<deg> ]
                dialBench, [ Angle.degree (-20.0<deg>) ]
                benchWith (turnedHolding (Angle.degree 22.5<deg>) qz blackBoxChoice sampleA) lr45, [ Angle.degree 22.5<deg>; Angle.degree 45.0<deg> ]
            ]

        for slots, objectAngles in cases do
            // The items of the slots `lab` names, in light order: it names no light, so the Laser is not among them.
            let held : SlotItem list =
                SetupSlot.inLightOrder slots
                |> List.filter (fun roled -> named |> List.contains (SlotPlaceholder roled.role))
                |> List.choose (fun roled -> roled.slot.content.itemOpt |> Option.map (fun item -> { role = roled.role; item = item }))

            let name =
                match lab.setupNameOf slots kitItems with
                | Ok setupName -> NameText setupName.value
                | Error error -> failwith $"the bench was not named: {error.describe}"

            let reading = readByLab name
            Assert.Equal<Result<SlotItem list, NamingTemplateError>>(Ok held, lab.itemsOf reading kitItems)
            Assert.Equal(objectAngles.Length, reading.angles.Length)

            objectAngles
            |> List.iteri (fun index expected ->
                match reading.tryFindAngle (SlotPosition (index + 1)) with
                | Some angle -> assertAngle expected angle
                | None -> Assert.Fail($"'{name.value}' read no angle for Obj {index + 1}"))

    [<Fact>]
    let ``a setup the template cannot name says which placeholder is empty, and a new setup takes the template's name made free, or the name it is given`` () : unit =
        let noPrep = fullBench |> List.map (fun slot -> if slot.id = lpA.id then slot.withoutItem else slot)

        match lab.setupNameOf noPrep kitItems with
        | Error (MissingNameValueErr reason) -> Assert.Contains("'source1'", reason)
        | Error other -> Assert.Fail($"expected MissingNameValueErr, got {other.describe}")
        | Ok name -> Assert.Fail($"the bench without CPL1 was named '{name.value}'")

        let unnamed = SetupName "Setup 3"
        Assert.Equal<SetupName>(unnamed, lab.setupNameAmong noPrep kitItems [] unnamed)
        Assert.Equal<SetupName>(SetupName "CPL-(QZ#45-LR#45)-CPL", lab.setupNameAmong fullBench kitItems [] unnamed)

        Assert.Equal<SetupName>(
            SetupName "CPL-(QZ#45-LR#45)-CPL (2)",
            lab.setupNameAmong fullBench kitItems [ SetupName "cpl-(qz#45-lr#45)-cpl" ] unnamed)

        // A template that writes a blank name for the slots names itself, and a new setup takes the given name.
        let noteOnly = NamingTemplate "{note}"

        match noteOnly.setupNameOf fullBench kitItems with
        | Error (MissingNameValueErr reason) -> Assert.Contains("'{note}'", reason)
        | Error other -> Assert.Fail($"expected MissingNameValueErr, got {other.describe}")
        | Ok name -> Assert.Fail($"'{{note}}' named the bench '{name.value}'")

        Assert.Equal<SetupName>(unnamed, noteOnly.setupNameAmong fullBench kitItems [] unnamed)

    [<Fact>]
    let ``an experiment is named by the kit's template from its setup's slots, made free among the kit's experiments, or takes the name it is given`` () : unit =
        let unnamed = (ExperimentId 3).defaultName
        let names (expected : ExperimentName) (slots : SetupSlot list) (noteOpt : NamePart option) (taken : ExperimentName list) : unit =
            Assert.Equal<ExperimentName>(expected, lab.experimentNameAmong slots kitItems noteOpt taken unnamed)

        names (ExperimentName "CPL-(QZ#45-LR#45)-CPL") fullBench None []
        names (ExperimentName "CPL-(QZ#45-LR#45)-CPL (2)") fullBench None [ ExperimentName "cpl-(qz#45-lr#45)-cpl" ]
        names (ExperimentName "CPL-(QZ#45-LR#45)-CPL-2") fullBench (Some (NamePart "2")) []

        // Without CPL1 the template has nothing for 'source1', so the experiment takes the name it is given.
        let noPrep = fullBench |> List.map (fun slot -> if slot.id = lpA.id then slot.withoutItem else slot)
        names (ExperimentName "E3") noPrep None []

        match (NameText "  ").toExperimentName with
        | Error (InvalidExperimentNameErr reason) -> Assert.Contains("'  '", reason)
        | Error other -> Assert.Fail($"expected InvalidExperimentNameErr, got {other.describe}")
        | Ok name -> Assert.Fail($"a blank text was kept as '{name.value}'")
