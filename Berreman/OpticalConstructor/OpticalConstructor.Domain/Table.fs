namespace OpticalConstructor.Domain

open Berreman.Constants
open Berreman.Geometry

/// Optical table & top-down view state (Spec 0026 Part C, slice 004). This is a
/// NET-NEW, pure, headless-testable domain module sitting on the units spine
/// (`Units.fs`) and the engine `Angle` type (`Geometry.fs:34`). It holds two pure
/// values:
///   * `OpticalTable` — the physical plate (width/length/thickness in canonical SI
///     meters + a display unit). It is the ONLY part of this slice persisted with
///     the project (C.1.2): `Project.fs` carries one as a sibling of slice 001's
///     `placements`, and it round-trips through the canonical JSON (AC-C1).
///   * `TableViewState` — the table's own R1/R2/R3 measured relative to the screen,
///     the pan offset, and the zoom (C.2.1). It is ephemeral UI state (the
///     interaction gestures that drive it are slice 005), NOT persisted; this slice
///     owns the default straight-top-down `(0, 0, 0)` orientation and the reset
///     target + default zoom as pure values.
///
/// Every magnitude is canonical SI meters (constraint 0.2); rotations reuse the
/// engine `Angle` (R-9); no Avalonia type appears here (constraint 0.3). The grey
/// plate, the to-scale layout, and the drawing weights are the top-down rendering
/// layer's concern (`ConstructorTable.fs`, Ui).
module Table =

    /// The default plate width (across the bench, +Y) in canonical meters (C.1.1):
    /// `1.2 m`. A single edit here restyles every fresh table.
    let defaultWidth : float<meter> = 1.2<meter>

    /// The default plate length (along the central ray, +X) in canonical meters
    /// (C.1.1): `2.0 m` — the long bench axis the default 2.0 m source→detector span
    /// (`RayModel.defaultSourceDetectorDistance`) runs along.
    let defaultLength : float<meter> = 2.0<meter>

    /// The default plate thickness in canonical meters (C.1.1): `0.10 m`.
    let defaultThickness : float<meter> = 0.10<meter>

    /// The neutral default zoom multiplier of the top-down view (C.2.1): `1.0`. The
    /// top-down layout (`ConstructorTable.fs`) maps canonical meters to drawing units
    /// at a base pixels-per-meter; zoom multiplies it, so `1.0` is "fit at the base
    /// scale" — a sensible straight-top-down default.
    let defaultZoom : float = 1.0

    /// The optical table plate (C.1.1 / C.1.2). Width, length, and thickness are held
    /// in canonical SI meters (constraint 0.2); `displayUnit` is display metadata only
    /// (mirroring `BeamNode.defaultUnit`), never a storage unit. The plate is drawn to
    /// scale as a grey plate by the top-down layer (C.1.3) so elements and beams sit at
    /// true relative scale.
    type OpticalTable =
        {
            width : float<meter>
            length : float<meter>
            thickness : float<meter>
            displayUnit : Units.UnitOfMeasure
        }

        /// A fresh table at the default `1.2 m × 2.0 m × 0.10 m` size (C.1.1), with a
        /// bench-scale millimeter display unit (matching `ElementPlacement.create`).
        static member defaultTable : OpticalTable =
            {
                width = defaultWidth
                length = defaultLength
                thickness = defaultThickness
                displayUnit = Units.Millimeter
            }

    /// A module-level alias for `OpticalTable.defaultTable` so a fresh project literal
    /// reads `table = Table.defaultTable` (C.1.2).
    let defaultTable : OpticalTable = OpticalTable.defaultTable

    /// Edit the plate size (C.1.2). The new width/length/thickness are taken as already
    /// canonical SI meters (the display→SI conversion happens at the `Units.fs` boundary
    /// before this is called); the display unit is preserved.
    let withSize (width : float<meter>) (length : float<meter>) (thickness : float<meter>) (t : OpticalTable) : OpticalTable =
        { t with width = width; length = length; thickness = thickness }

    /// Set the plate's preferred display unit (display metadata only; the stored
    /// magnitudes stay canonical SI).
    let withDisplayUnit (u : Units.UnitOfMeasure) (t : OpticalTable) : OpticalTable =
        { t with displayUnit = u }

    /// The top-down view state (C.2.1): the table's own R1/R2/R3 measured relative to
    /// the screen, the pan offset (in drawing units), and the zoom multiplier. Pure,
    /// serializable values — no Avalonia handle (constraint 0.3). The pan/zoom/rotate
    /// gestures that mutate this and table-selection-on-click are slice 005 (AC-C2 /
    /// AC-C3); this slice owns only the default top-down value and the reset target.
    type TableViewState =
        {
            r1 : Angle
            r2 : Angle
            r3 : Angle
            panX : float
            panY : float
            zoom : float
        }

        /// The default view (C.2.1): straight top-down — all three rotations `(0, 0, 0)`,
        /// no pan, the default zoom. This is also the reset target (C.2.5).
        static member topDown : TableViewState =
            {
                r1 = Angle.zero
                r2 = Angle.zero
                r3 = Angle.zero
                panX = 0.0
                panY = 0.0
                zoom = defaultZoom
            }

    /// The default top-down view state (C.2.1).
    let defaultView : TableViewState = TableViewState.topDown

    /// Reset the view to the straight-top-down default (C.2.5): orientation `(0, 0, 0)`,
    /// no pan, the default zoom. Reset discards any pan/zoom/rotation the user dialled.
    let resetView (_ : TableViewState) : TableViewState = TableViewState.topDown
