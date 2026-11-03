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

    /// A band's relative height weight (> 0): the thicker the layer, the taller the band.
    type BandWeight =
        | BandWeight of float

        member this.value = let (BandWeight w) = this in w

        /// This weight normalised against the stack's `largest`: 1 for the tallest band, 0 … 1 for the rest.
        member this.normalisedAgainst (largest : BandWeight) : BandWeight =
            let weight = max 0.0 this.value
            BandWeight (if largest.value > 1e-12 then weight / largest.value else 0.0)

    /// A band's `#RRGGBB` fill colour, as the host supplies it.
    type BandColorHex =
        | BandColorHex of string

        member this.value = let (BandColorHex h) = this in h

        /// The fill, falling back to a neutral grey on a malformed string (so the control is total — a bad
        /// host colour never throws).
        member this.fill : Color =
            match Color.TryParse this.value with
            | true, c -> c
            | _ -> Color.FromRgb(200uy, 200uy, 200uy)

    /// One drawn band: its label (material + thickness, possibly "×N"), its relative height weight and its
    /// fill colour.
    type Band =
        {
            label : ControlCaption
            heightWeight : BandWeight
            colorHex : BandColorHex
        }

    /// The bay's pure state: a "what it is" headline and the bands to draw (empty when nothing is selected
    /// or the selected element has no layered sample bound).
    type LayerBandsControlsState =
        {
            title : ControlCaption
            bands : Band list
        }

    let emptyLayerBandsControls : LayerBandsControlsState =
        {
            title = ControlCaption ""
            bands = []
        }

    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBorder = Color.FromRgb(120uy, 120uy, 120uy)

    /// One drawn band — a coloured, bordered rectangle (height ∝ thickness) labelled with its material +
    /// thickness, carrying the stable per-index automation id.
    let private bandView (largest : BandWeight) (position : ConstructorUiPosition) (b : Band) : IView =
        // The drawn height range a band maps into: a very thin layer is still visible (a floor), a very
        // thick one is capped so one band can't dwarf the rest; the tallest band hits the cap.
        let minHeight = 18.0
        let maxHeight = 64.0
        Border.create [
            Border.name (ConstructorUiId.layerBand position).value
            Border.height (minHeight + (maxHeight - minHeight) * (b.heightWeight.normalisedAgainst largest).value)
            Border.background (brush b.colorHex.fill)
            Border.borderBrush (brush idleBorder)
            Border.borderThickness 1.0
            Border.margin (Thickness(0.0, 0.0, 0.0, 2.0))
            Border.horizontalAlignment HorizontalAlignment.Stretch
            Border.child (
                TextBlock.create [
                    TextBlock.text b.label.value
                    TextBlock.fontSize 11.0
                    TextBlock.margin (Thickness(6.0, 0.0, 0.0, 0.0))
                    TextBlock.verticalAlignment VerticalAlignment.Center
                ])
        ] :> IView

    type LayerBandsControlsState with

        /// The Layer-Bands view — a titled vertical stack of bands (in a `ScrollViewer` for tall stacks). When
        /// there are no bands it shows just the title (the host supplies a hint title for the empty / no-layers
        /// case), so the view is always renderable.
        member this.view : IView =
            let largest =
                match this.bands with
                | [] -> BandWeight 1.0
                | _ -> this.bands |> List.map (fun b -> b.heightWeight) |> List.max
            let bandViews = this.bands |> List.mapi (fun i b -> bandView largest (ConstructorUiPosition i) b)
            StackPanel.create [
                StackPanel.orientation Orientation.Vertical
                StackPanel.spacing 6.0
                StackPanel.children [
                    TextBlock.create [
                        TextBlock.name ConstructorUiId.layerBandsTitle.value
                        TextBlock.text this.title.value
                        TextBlock.textWrapping TextWrapping.Wrap
                        TextBlock.maxWidth 360.0
                        TextBlock.fontWeight FontWeight.SemiBold
                    ]
                    ScrollViewer.create [
                        ScrollViewer.maxHeight 320.0
                        ScrollViewer.content (
                            StackPanel.create [
                                StackPanel.name ConstructorUiId.layerBandsStack.value
                                StackPanel.orientation Orientation.Vertical
                                StackPanel.width 300.0
                                StackPanel.children bandViews
                            ])
                    ]
                ]
            ] :> IView
