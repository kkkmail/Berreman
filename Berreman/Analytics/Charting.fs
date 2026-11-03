namespace Analytics

open Berreman.Fields
open Berreman.Solvers
open Berreman.FieldFunctions
open Berreman.Dispersion
open Analytics.Variables
open Plotly.NET
open Giraffe.ViewEngine

module Charting =

    /// A series' name in a chart's legend.
    type SeriesName =
        | SeriesName of string

        member this.value = let (SeriesName n) = this in n


    let createDescription (title : ChartDescription) (subtitle : ChartDescription) : Giraffe.ViewEngine.HtmlElements.XmlNode list =
        [
            h1 [] [ str title.value ]
            h2 [] [ str subtitle.value ]
        ]


    /// Plots several functions (e.g. [ R; T; I; ... ] ) on the same plot.
    let plot (f : FixedInfo) (fn : List<OpticalFunction>) (x : RangedVariable) =
        let data = calculate f x
        let description = (f.getDescription x, ChartDescription "") ||> createDescription


        let getFuncData (e : OpticalFunction) =
            data
            |> Array.choose (fun p -> p.solution.func e |> Option.map (fun y -> (p.plotValue, y.value)))

        Chart.combine (fn |> List.map (fun e -> Chart.Line(getFuncData e, Name = e.info.fullName)))
        |> Chart.withXAxisStyle(x.name, MinMax = (x.plotMinValue, x.plotMaxValue))
        |> Chart.withDescription description
        |> Chart.show


    /// Plots several different models (function by function) on the same plots.
    let plotComparison (f : list<FixedInfo>) (fn : List<OpticalFunction>) (x : RangedVariable) =
        let data = f |> List.map (fun e -> calculate e x)
        let (d, _) = f |> List.fold (fun (acc, i) r -> (acc + "(" + i.ToString() + "): " + (r.getDescription x).value + ChartDescription.lineBreak, i + 1)) ("", 0)
        let description = (ChartDescription d, ChartDescription "") ||> createDescription

        let getFuncData (d : SweepPoint[]) (e : OpticalFunction) =
            d
            |> Array.choose (fun p -> p.solution.func e |> Option.map (fun y -> (p.plotValue, y.value)))

        let plotFun (f : OpticalFunction) =
            Chart.combine (data |> List.mapi (fun i e -> Chart.Line(getFuncData e f, Name = f.info.fullName + " (" + i.ToString() + ")")))
            |> Chart.withXAxisStyle(x.name, MinMax = (x.plotMinValue, x.plotMaxValue))
            |> Chart.withDescription description
            |> Chart.show

        fn |> List.map plotFun


    let mapFun (data : #seq<#seq<SurfacePoint>>) (fn : OpticalFunction) : OpticalFunctionValue[][] =
        data
        |> Seq.map (fun r -> r |> Seq.choose (fun e -> e.solution.func fn) |> Array.ofSeq)
        |> Array.ofSeq


    /// Plots 3D functions.
    let plot3D (f : FixedInfo) (fn : List<OpticalFunction>) (x : RangedVariable) (y : RangedVariable) =
        let xVal = x.plotPoints
        let yVal = y.plotPoints
        let data = calculate3D f x y
        let description = (f.getDescription (x, y), ChartDescription "") ||> createDescription


        let plotFun e =
            let zVal = mapFun data e |> Array.map (Array.map (fun v -> v.value))

            // kk:20180922 The axes are somehow mysteriously swapped. Here we swap X with Y back for both data and names.
            Chart.Surface(zVal, yVal, xVal, Opacity = 0.7, Contours = TraceObjects.Contours.initXyz(Show = true), Name = e.info.name.value)
            |> Chart.withXAxisStyle(y.name)
            |> Chart.withYAxisStyle(x.name)
            |> Chart.withZAxisStyle(e.info.name.value)
            //|> Chart.withTitle(title)
            |> Chart.withDescription description

        fn |> List.map plotFun


    let plotDispersion
        (calc : OpticalPropertiesWithDisp -> Range<WaveLength> -> DispersionPoint[])
        (name : SeriesName)
        (o : OpticalPropertiesWithDisp)
        (r : Range<WaveLength>)
        : unit =
        let data = calc o r |> Array.map (fun p -> (p.waveLengthPlotValue, p.propertyValue))
        let x = r |> WaveLengthRange
        //let description = o.description

        Chart.Line(data, Name = name.value)
        |> Chart.withXAxisStyle(x.name, MinMax = (x.plotMinValue, x.plotMaxValue))
        |> Chart.show //WithDescription description


    let plotN11 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateN11Re (SeriesName "Re[e11]") o r
    let plotXi11 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateXi11Im (SeriesName "Im[e11]") o r

    let plotN22 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateN22Re (SeriesName "Re[e22]") o r
    let plotXi22 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateXi22Im (SeriesName "Im[e22]") o r

    let plotN33 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateN33Re (SeriesName "Re[e33]") o r
    let plotXi33 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateXi33Im (SeriesName "Im[e33]") o r


    let plotRho11 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateRho11Im (SeriesName "rho11") o r
    let plotRho22 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateRho22Im (SeriesName "rho22") o r
    let plotRho33 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateRho33Im (SeriesName "rho33") o r
