namespace OpticalConstructor.Controls

open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// Spec 0027 (026) — the reusable LAYER-STACK BAND view (the Details bay). A vertical stack of coloured
/// rectangles, each sized (height) by its relative thickness and labelled with its material + thickness.
/// DOMAIN-FREE: the host (TestWindows) flattens a sample's engine `OpticalSystem` into `Band`s — borrowing
/// the V1 `Schematic.fs` colour / height idea — and injects them; this control just draws what it is given.
/// For tall stacks (the 41-layer / 100-pair multilayers) the host collapses runs of identical layers into a
/// single "×N" band, so the band list stays readable rather than listing every physical layer.
module LayerBandsControls =

    /// One drawn band: its label (material + thickness, possibly "×N"), a relative height weight (>0 — the
    /// thicker the layer, the taller the band) and an `#RRGGBB` fill colour.
    type Band =
        {
            label : string
            heightWeight : float
            colorHex : string
        }

    /// The bay's pure state: a "what it is" headline and the bands to draw (empty when nothing is selected
    /// or the selected element has no layered sample bound).
    type State =
        {
            title : string
            bands : Band list
        }

    let empty : State =
        {
            title = ""
            bands = []
        }

    /// Stable automation ids (CLAUDE.md UI guidance).
    [<RequireQualifiedAccess>]
    module UiIds =
        let stack = "LayerBandsStack"
        let title = "LayerBandsTitle"
        /// The i-th band's drawn rectangle (0-based, top-to-bottom in band order).
        let band (i : int) : string = "LayerBand_" + string i

    let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBorder = color 120 120 120

    /// The drawn pixel height range a band's relative weight maps into: a very thin layer is still visible
    /// (a floor), a very thick one is capped so one band can't dwarf the rest.
    let private minBandHeight = 18.0
    let private maxBandHeight = 64.0

    /// Map a band's relative weight into a drawn pixel height, normalised against the stack's largest weight
    /// (so the tallest band hits the cap and the rest scale below it, never under the floor).
    let private bandHeight (maxWeight : float) (weight : float) : float =
        let w = max 0.0 weight
        let frac = if maxWeight > 1e-12 then w / maxWeight else 0.0
        minBandHeight + (maxBandHeight - minBandHeight) * frac

    /// Parse an `#RRGGBB` hex colour, falling back to a neutral grey on a malformed string (so the control
    /// is total — a bad host colour never throws).
    let private parseHex (hex : string) : Color =
        match Color.TryParse hex with
        | true, c -> c
        | _ -> color 200 200 200

    /// One drawn band — a coloured, bordered rectangle (height ∝ thickness) labelled with its material +
    /// thickness, carrying the stable per-index automation id.
    let private bandView (maxWeight : float) (index : int) (b : Band) : IView =
        Border.create [
            Border.name (UiIds.band index)
            Border.height (bandHeight maxWeight b.heightWeight)
            Border.background (brush (parseHex b.colorHex))
            Border.borderBrush (brush idleBorder)
            Border.borderThickness 1.0
            Border.margin (Thickness(0.0, 0.0, 0.0, 2.0))
            Border.horizontalAlignment HorizontalAlignment.Stretch
            Border.child (
                TextBlock.create [
                    TextBlock.text b.label
                    TextBlock.fontSize 11.0
                    TextBlock.margin (Thickness(6.0, 0.0, 0.0, 0.0))
                    TextBlock.verticalAlignment VerticalAlignment.Center
                ])
        ] :> IView

    /// The Layer-Bands view — a titled vertical stack of bands (in a `ScrollViewer` for tall stacks). When
    /// there are no bands it shows just the title (the host supplies a hint title for the empty / no-layers
    /// case), so the view is always renderable.
    let view (state : State) : IView =
        let maxWeight =
            match state.bands with
            | [] -> 1.0
            | _ -> state.bands |> List.map (fun b -> b.heightWeight) |> List.max
        let bandViews = state.bands |> List.mapi (fun i b -> bandView maxWeight i b)
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 6.0
            StackPanel.children [
                TextBlock.create [
                    TextBlock.name UiIds.title
                    TextBlock.text state.title
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.maxWidth 360.0
                    TextBlock.fontWeight FontWeight.SemiBold
                ]
                ScrollViewer.create [
                    ScrollViewer.maxHeight 320.0
                    ScrollViewer.content (
                        StackPanel.create [
                            StackPanel.name UiIds.stack
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.width 300.0
                            StackPanel.children bandViews
                        ])
                ]
            ]
        ] :> IView
