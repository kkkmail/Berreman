namespace OpticalConstructor.Domain

open Berreman.Geometry
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.NamingTemplates

/// The grid generator: every combination of a table's slot options made a setup at once (spec 0050 S8.3; 001 §J).
module SetupGrids =

    /// One option for a slot: it holds that item, or nothing, turned to `r1` (spec 0050 S8.3).
    type SlotChoice =
        {
            itemOpt : ItemId option
            r1 : Angle
        }

    /// One axis of a grid: a slot of the table and its options in order (spec 0050 S8.3; 001 §J).
    type SlotOptions =
        {
            slot : Library.ElementId
            choices : SlotChoice list
        }

    /// The option a combination takes for one slot (spec 0050 S8.3).
    type SlotPick =
        {
            slot : Library.ElementId
            choice : SlotChoice
        }

    /// One pick per axis, in axis order: one setup a grid can make (spec 0050 S8.3).
    type GridCombination =
        {
            picks : SlotPick list
        }

        /// The table's slots in order, each picked one turned and then filled as `AssignSlot` fills it, every slot
        /// recorded as a commit records the table; a slot the table lacks or one item in two picked slots is refused
        /// (spec 0050 S8.2, S8.3; 001 §I, §P).
        member this.slotsOver
            (table : SetupSlot list)
            (items : Item list)
            (choiceOf : Item -> ItemModelChoice option)
            : Result<SetupSlot list, SetupError> =
            let filled (pick : SlotPick) (slot : SetupSlot) (picked : SetupSlot list) : Result<SetupSlot, SetupError> =
                let turned : SetupSlot = { slot with placement = { slot.placement with r1 = pick.choice.r1 } }

                match pick.choice.itemOpt with
                | None -> Ok turned.withoutItem
                | Some itemId ->
                    match items |> List.tryFind (fun item -> item.id = itemId) with
                    | None -> Error (UnknownSlotItemErr $"the kit holds no item {itemId.value}")
                    | Some item ->
                        let holder (other : SetupSlot) : bool = other.id <> slot.id && other.content.itemOpt = Some itemId

                        match picked |> List.tryFind holder, choiceOf item with
                        | Some other, _ -> Error (SlotItemTwiceErr $"'{item.label.value}' in '{other.id.value}' and '{slot.id.value}'")
                        | None, Some choice -> turned.withItem item choice
                        | None, None -> Error (UnknownSlotItemErr $"'{item.label.value}': its model is not in the library")

            let pickInto (held : Result<SetupSlot list, SetupError>) (pick : SlotPick) : Result<SetupSlot list, SetupError> =
                held
                |> Result.bind (fun picked ->
                    match table |> List.tryFind (fun slot -> slot.id = pick.slot) with
                    | None -> Error (UnknownSetupSlotErr $"'{pick.slot.value}'")
                    | Some slot ->
                        filled pick slot picked
                        |> Result.map (fun made -> (picked |> List.filter (fun other -> other.id <> made.id)) @ [ made ]))

            this.picks
            |> List.fold pickInto (Ok [])
            |> Result.map (fun picked ->
                table
                |> List.map (fun slot ->
                    let current = picked |> List.tryFind (fun made -> made.id = slot.id) |> Option.defaultValue slot
                    current.recordedWith items))

    /// A grid: its axes, and the combinations left out, impossible or unwanted (spec 0050 S8.3; 001 §J).
    type SetupGrid =
        {
            axes : SlotOptions list
            unticked : GridCombination list
        }

        /// The product of the axes' options, the first axis varying slowest; no axis gives one empty combination (spec
        /// 0050 S8.3).
        member this.combinations : GridCombination list =
            let rec product (axes : SlotOptions list) : SlotPick list list =
                match axes with
                | [] -> [ [] ]
                | axis :: later ->
                    let rest = product later

                    axis.choices
                    |> List.collect (fun choice ->
                        let pick : SlotPick = { slot = axis.slot; choice = choice }
                        rest |> List.map (fun picks -> pick :: picks))

            product this.axes |> List.map (fun picks -> { picks = picks })

        /// The combinations not unticked, in order (spec 0050 S8.3).
        member this.ticked : GridCombination list =
            this.combinations |> List.filter (fun combination -> not (List.contains combination this.unticked))

        /// The grid with the combination left out, listed once (spec 0050 S8.3).
        member this.untick (combination : GridCombination) : SetupGrid =
            if List.contains combination this.unticked then
                this
            else
                { this with unticked = this.unticked @ [ combination ] }

        /// The grid with the combination ticked again (spec 0050 S8.3).
        member this.tick (combination : GridCombination) : SetupGrid =
            { this with unticked = this.unticked |> List.filter (fun held -> held <> combination) }

        /// The grid over `axes` with every combination the table cannot make unticked, in order (spec 0050 S8.3; 001 §J).
        static member create
            (axes : SlotOptions list)
            (table : SetupSlot list)
            (items : Item list)
            (choiceOf : Item -> ItemModelChoice option)
            : SetupGrid =
            let grid : SetupGrid = { axes = axes; unticked = [] }

            let impossible : GridCombination list =
                grid.combinations
                |> List.choose (fun combination ->
                    match combination.slotsOver table items choiceOf with
                    | Ok _ -> None
                    | Error _ -> Some combination)

            { grid with unticked = impossible }

        /// One setup per ticked combination, in order: the held or earlier-made setup whose current slots equal its own,
        /// else a new one the template names free among those, else `Setup N`. The first combination that cannot be made
        /// refuses the whole grid. It writes nothing; the host records (spec 0050 S8.3; 001 §J, §P `GenerateSetupGrid`).
        member this.generate
            (table : SetupSlot list)
            (items : Item list)
            (choiceOf : Item -> ItemModelChoice option)
            (template : NamingTemplate)
            (held : Setup list)
            : Result<Setup list, SetupError> =
            let setupOf (made : Result<Setup list, SetupError>) (combination : GridCombination) : Result<Setup list, SetupError> =
                made
                |> Result.bind (fun earlier ->
                    combination.slotsOver table items choiceOf
                    |> Result.bind (fun slots ->
                        let known = held @ earlier

                        match known |> List.tryFind (fun setup -> setup.slots = slots) with
                        | Some setup -> Ok (earlier @ [ setup ])
                        | None ->
                            let taken = known |> List.map (fun setup -> setup.name)
                            let name = template.setupNameAmong slots items taken (SetupName.firstFree taken)
                            Setup.create name slots known |> Result.map (fun setup -> earlier @ [ setup ])))

            this.ticked |> List.fold setupOf (Ok [])
