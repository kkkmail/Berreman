namespace OpticalConstructor.Ui.Tests

open Xunit
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.TableView
open OpticalConstructor.TestWindows
open OpticalConstructor.TestWindows.ElementMovementView

/// Tests for the element-movement test window (Spec 0027, task 010): a single element slid along the
/// central ray by drag / arrow keys, clamped to the plate. Pure MVU — no headless session needed.
module ElementMovementTests =

    let private close (a : float) (b : float) : bool = abs (a - b) <= 1.0e-9

    [<Fact>]
    let ``init: a single Sample element in the middle of the table`` () =
        let m = init ()
        Assert.Equal(Sample, m.element.catalogueKind)
        Assert.True(close (elementX m) 0.0, "starts at the table centre")
        Assert.False(m.dragging)

    [<Fact>]
    let ``SlideBy nudges the element along the beam and clamps to the plate edges`` () =
        let m = init ()
        Assert.True(close (elementX (update (SlideBy 0.3) m)) 0.3)
        Assert.True(close (elementX (update (SlideBy -0.3) (update (SlideBy 0.3) m))) 0.0)
        // The plate half-length is 1.0 m, so a big slide pins at the edge (±1.0), never beyond.
        Assert.True(close (elementX (update (SlideBy 5.0) m)) 1.0)
        Assert.True(close (elementX (update (SlideBy -5.0) m)) -1.0)

    [<Fact>]
    let ``SlideTo sets the position and ResetPosition returns it to the centre`` () =
        let m = update (SlideTo 0.5) (init ())
        Assert.True(close (elementX m) 0.5)
        Assert.True(close (elementX (update ResetPosition m)) 0.0)

    [<Fact>]
    let ``a drag moves the element ONLY when it begins on the element`` () =
        let m0 = init ()
        // A press on the element (its centre is the screen centre at x = 0) starts the drag; the element
        // then follows the pointer's x — +0.4 m to the right is +0.4·pixelsPerMeter screen pixels.
        let dragged =
            m0
            |> update (BeginDrag { sx = center.sx; sy = center.sy })
            |> update (DragTo { sx = center.sx + pixelsPerMeter * 0.4; sy = center.sy })
        Assert.True(dragged.dragging)
        Assert.True(close (elementX dragged) 0.4)
        // A press far OFF the element does not start a drag, so the subsequent move is inert.
        let notDragged =
            m0
            |> update (BeginDrag { sx = center.sx; sy = center.sy + 250.0 })
            |> update (DragTo { sx = center.sx + pixelsPerMeter * 0.4; sy = center.sy })
        Assert.False(notDragged.dragging)
        Assert.True(close (elementX notDragged) 0.0)
