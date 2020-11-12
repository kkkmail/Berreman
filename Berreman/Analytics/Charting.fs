namespace Analytics

open System
open System.IO
open Berreman.Fields
open Berreman.Solvers
open Berreman.FieldFunctions
open Berreman.Dispersion
open Analytics.Variables
open FSharp.Plotly
open FSharp.Plotly.GenericChart
open FSharp.Plotly.ChartDescription

module Charting =

    type Chart
        with

        /// Show chart in browser
        static member ShowWithDescription2 (show : bool) (d : Description) (ch:GenericChart) =
            let guid = Guid.NewGuid().ToString()
            let html = GenericChart.toEmbeddedHtmlWithDescription d ch
            let tempPath = Path.GetTempPath()
            let file = sprintf "%s.html" guid
            let path = Path.Combine(tempPath, file)
            File.WriteAllText(path, html)
            if show then System.Diagnostics.Process.Start(path) |> ignore


        /// Saves chart in a specified file name and shows it in the browser. The caller is responsible for full path / filename / extension.
        static member ShowFileWithDescription (show : bool) (fullFileName : string) (d : Description) (ch:GenericChart) =
            let html = GenericChart.toEmbeddedHtmlWithDescription d ch
            File.WriteAllText(fullFileName, html)
            if show then System.Diagnostics.Process.Start(fullFileName) |> ignore


    /// Plots several functions (e.g. [ R; T; I; ... ] ) on the same plot.
    let plot (f : FixedInfo) (fn : List<OpticalFunction>) (x : RangedVariable) =
        let data = calculate f x
        let description =
            {
                Heading = f.opticalSystem.description |> Option.defaultValue String.Empty
                Text = f.getDescription x
            }

        let getFuncData (e : OpticalFunction) =
            data
            |> Array.map (fun (v, s) -> (v, s.func e))
            |> Array.choose (fun (x, yo) -> match yo with | Some y -> Some (x, y) | None -> None )

        Chart.Combine (fn |> List.map (fun e -> Chart.Line(getFuncData e, Name = e.info.fullName)))
        |> Chart.withX_AxisStyle(x.name, MinMax = (x.plotMinValue, x.plotMaxValue))
        |> Chart.ShowWithDescription2 true description


    /// Plots several different models (function by function) on the same plots.
    let plotComparison (f : list<FixedInfo>) (fn : List<OpticalFunction>) (x : RangedVariable) =
        let data = f |> List.map (fun e -> calculate e x)
        let (description, _) = f |> List.fold (fun (acc, i) r -> (acc + "(" + i.ToString() + "): " + r.getDescription x + lineBrake, i + 1)) ("", 0)

        let d =
            {
                Heading = "Comparison Chart"
                Text = description
            }

        let getFuncData (d : array<float * Solution> ) (e : OpticalFunction) =
            d
            |> Array.map (fun (v, s) -> (v, s.func e))
            |> Array.choose (fun (x, yo) -> match yo with | Some y -> Some (x, y) | None -> None )

        let plotFunc (f : OpticalFunction) =
            Chart.Combine (data |> List.mapi (fun i e -> Chart.Line(getFuncData e f, Name = f.info.fullName + " (" + i.ToString() + ")")))
            |> Chart.withX_AxisStyle(x.name, MinMax = (x.plotMinValue, x.plotMaxValue))
            //|> Chart.withTitle(title)
            |> Chart.ShowWithDescription2 true d

        fn |> List.map plotFunc


    let mapFun (data : #seq<#seq<double * double * Solution>>) (fn : OpticalFunction) =
        data
        |> Seq.map (fun r -> r |> Seq.map (fun (_, _, e) -> e.func fn) |> Seq.choose id |> Array.ofSeq)
        |> Array.ofSeq


    /// Plots 3D functions.
    let plot3D (f : FixedInfo) (fn : List<OpticalFunction>) (x : RangedVariable) (y : RangedVariable) =
        let xVal = x.plotPoints
        let yVal = y.plotPoints
        let data = calculate3D f x y

        let description =
            {
                Heading = f.opticalSystem.description |> Option.defaultValue String.Empty
                Text = f.getDescription (x, y)
            }


        let plotFun e =
            let zVal = mapFun data e

            // kk:20180922 The axes are somehow mysteriously swapped. Here we swap X with Y back for both data and names.
            Chart.Surface(zVal, yVal, xVal, Opacity = 0.7, Contours = Contours.initXyz(Show = true), Name = e.info.name)
            |> Chart.withX_AxisStyle(y.name)
            |> Chart.withY_AxisStyle(x.name)
            |> Chart.withZ_AxisStyle(e.info.name)
            //|> Chart.withTitle(title)
            |> Chart.ShowWithDescription2 true description

        fn |> List.map plotFun


    let plotDispersion calc name (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) =
        let data = calc o r
        let x = r |> WaveLengthRange
        //let description = o.description

        Chart.Line(data, Name = name)
        |> Chart.withX_AxisStyle(x.name, MinMax = (x.plotMinValue, x.plotMaxValue))
        |> Chart.Show //WithDescription description


    let plotN11 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateN11Re "Re[e11]" o r
    let plotXi11 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateXi11Im "Im[e11]" o r

    let plotN22 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateN22Re "Re[e22]" o r
    let plotXi22 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateXi22Im "Im[e22]" o r

    let plotN33 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateN33Re "Re[e33]" o r
    let plotXi33 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateXi33Im "Im[e33]" o r


    let plotRho11 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateRho11Im "rho11" o r
    let plotRho22 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateRho22Im "rho22" o r
    let plotRho33 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateRho33Im "rho33" o r
