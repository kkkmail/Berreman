namespace OpticalConstructor.TestWindows

open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.VisualTree
open OpticalConstructor.Domain.TableView

/// Shared pointer→canvas mapping for the test scenes (Spec 0027, task 010 fix). The control bar is now
/// docked ABOVE the canvas, so the canvas is offset within the window; `e.GetPosition null` (top-level
/// coordinates) would be wrong by the bar's height and break selection / hit-testing. This returns the
/// pointer in CANVAS-local coordinates — the same frame the projection draws in — by locating the named
/// canvas in the visual tree and asking for the position relative to it.
module SceneInput =

    let canvasPoint (canvasName : string) (e : PointerEventArgs) : ScreenPoint =
        let canvas =
            match e.Source with
            | :? Visual as v ->
                // The hit visual is the canvas itself (empty click), a child shape (canvas is an
                // ancestor), or the surrounding border (canvas is a descendant) — check all three.
                Seq.concat [ Seq.singleton v; v.GetVisualAncestors(); v.GetVisualDescendants() ]
                |> Seq.tryPick (function :? Canvas as c when c.Name = canvasName -> Some c | _ -> None)
            | _ -> None
        let p =
            match canvas with
            | Some c -> e.GetPosition c
            | None -> e.GetPosition null
        { sx = p.X; sy = p.Y }
