namespace OpticalConstructor.Domain

open Berreman.Geometry
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Setups

/// The kit's experiments laid out by what they measure: rows source × objects × φ, columns analyzer (001 §O
/// "Experiment matrix view"; spec 0050 S9.3).
module ExperimentMatrices =

    /// An object slot's item and its angle, the slot's `turnAngle`: a dial reading, else R1 (spec 0050 S9.3).
    type MatrixObject =
        {
            item : ItemLabel
            angle : Angle
        }

    /// A row of the matrix: the items of the Prep slots, then the objects at their angles, each in light order (spec
    /// 0050 S9.3).
    type MatrixRowKey =
        {
            sources : ItemLabel list
            objects : MatrixObject list
        }

    /// A column of the matrix: the items of the analyzer slots in light order (spec 0050 S9.3).
    type MatrixColumnKey =
        {
            analyzers : ItemLabel list
        }

    /// The experiments measured at one row and column, in the collection's order (spec 0050 S9.3).
    type MatrixCell =
        {
            row : MatrixRowKey
            column : MatrixColumnKey
            experiments : ExperimentId list
        }

    /// The experiment matrix: its rows, columns and cells in order of first appearance, and the experiments whose setup
    /// revision the kit lacks (spec 0050 S9.3).
    type ExperimentMatrix =
        {
            rows : MatrixRowKey list
            columns : MatrixColumnKey list
            cells : MatrixCell list
            unplaced : ExperimentId list
        }

        /// The cell at that row and column; none when no experiment was measured there (spec 0050 S9.3).
        member this.tryFindCell (row : MatrixRowKey) (column : MatrixColumnKey) : MatrixCell option =
            this.cells |> List.tryFind (fun cell -> cell.row = row && cell.column = column)

        /// Each experiment placed by its revision's slots in light order: the Prep items and the objects at their angles
        /// are its row, the analyzer items its column; an empty slot or an item `items` lacks adds nothing, and an
        /// experiment whose revision `slotsIn` refuses is unplaced (spec 0050 S9.3).
        static member ofExperiments (experiments : Experiment list) (setups : Setup list) (items : Item list) : ExperimentMatrix =
            let labelOf (slot : SetupSlot) : ItemLabel option =
                slot.content.itemOpt
                |> Option.bind (fun id -> items |> List.tryFind (fun item -> item.id = id))
                |> Option.map (fun item -> item.label)

            // An experiment's own one-experiment cell, merged with its neighbours' below.
            let cellOf (id : ExperimentId) (slots : SetupSlot list) : MatrixCell =
                let held : (RoledSlot * ItemLabel) list =
                    SetupSlot.inLightOrder slots |> List.choose (fun roled -> labelOf roled.slot |> Option.map (fun label -> roled, label))

                let sources : ItemLabel list =
                    held
                    |> List.choose (fun (roled, label) ->
                        match roled.role with
                        | PrepSlotRole _ -> Some label
                        | SourceSlotRole
                        | ObjectSlotRole _
                        | AnalyzerSlotRole _
                        | DetectorSlotRole _ -> None)

                let objects : MatrixObject list =
                    held
                    |> List.choose (fun (roled, label) ->
                        match roled.role with
                        | ObjectSlotRole _ -> Some { item = label; angle = roled.slot.turnAngle }
                        | SourceSlotRole
                        | PrepSlotRole _
                        | AnalyzerSlotRole _
                        | DetectorSlotRole _ -> None)

                let analyzers : ItemLabel list =
                    held
                    |> List.choose (fun (roled, label) ->
                        match roled.role with
                        | AnalyzerSlotRole _ -> Some label
                        | SourceSlotRole
                        | PrepSlotRole _
                        | ObjectSlotRole _
                        | DetectorSlotRole _ -> None)

                {
                    row = { sources = sources; objects = objects }
                    column = { analyzers = analyzers }
                    experiments = [ id ]
                }

            let resolved : (ExperimentId * Result<SetupSlot list, SetupError>) list =
                experiments |> List.map (fun experiment -> experiment.id, experiment.slotsIn setups)

            let placed : MatrixCell list =
                resolved
                |> List.choose (fun (id, slotsResult) ->
                    match slotsResult with
                    | Ok slots -> Some (cellOf id slots)
                    | Error _ -> None)

            let cells : MatrixCell list =
                placed
                |> List.map (fun cell -> cell.row, cell.column)
                |> List.distinct
                |> List.map (fun (row, column) ->
                    {
                        row = row
                        column = column
                        experiments =
                            placed
                            |> List.filter (fun cell -> cell.row = row && cell.column = column)
                            |> List.collect (fun cell -> cell.experiments)
                    })

            {
                rows = placed |> List.map (fun cell -> cell.row) |> List.distinct
                columns = placed |> List.map (fun cell -> cell.column) |> List.distinct
                cells = cells
                unplaced =
                    resolved
                    |> List.choose (fun (id, slotsResult) ->
                        match slotsResult with
                        | Ok _ -> None
                        | Error _ -> Some id)
            }
