namespace Analytics

open FSharp.Collections.ParallelSeq
open System.Numerics
open MathNet.Numerics.LinearAlgebra
open Berreman.MathNetNumericsMath
open Berreman.MatrixExp

open Berreman.Constants
open Berreman.Fields
open Berreman.BerremanMatrix
open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.Solvers
open Berreman.FieldFunctions
open OpticalProperties.Standard
open Berreman

open Analytics.Variables

//open FSharp.Charting
open FSharp.Plotly
//open XPlot.Plotly

module Charting_Old = 
    let x = 1

    ////let gePlotData() : (float * float)[] =
    ////    [| for i in 1..100 -> (float i, float (i * i)) |]


    ////let plotData () = 
    ////    Chart.Line(gePlotData(), Name = "Test")
    ////    //|> Chart.WithXAxis(Enabled = true, Title = (sprintf "%A" h.matchFunction), Max = maxVal, Min = h.interval.minValue)
    ////    //|> Chart.WithYAxis(Enabled = true, Title = "Probability of failing match", Max = 1.0, Min = 0.0)
    ////    //|> Chart.WithLegend(Docking = ChartTypes.Docking.Left, InsideArea = true, Title = h.description)

    ////    |> Chart.WithXAxis(Enabled = true, Title = "X")
    ////    |> Chart.WithYAxis(Enabled = true, Title = "Y")
    ////    |> Chart.WithLegend(Docking = ChartTypes.Docking.Left, InsideArea = true, Title = "Legend")
    ////    |> Chart.Show


    //let system = 
    //    {
    //        upper = OpticalProperties.vacuum
    //        films = []
    //        lower = OpticalProperties.transparentGlass
    //    }


    //let getLight e a = 
    //    {
    //        wavelength = WaveLength.nm 600.0
    //        refractionIndex = RefractionIndex.vacuum
    //        incidenceAngle = IncidenceAngle.create a
    //        polarization = Polarization.defaultValue
    //        ellipticity = e
    //    }


    //let getSol a = 
    //    //printfn "a = %A" a
    //    let sol = BaseOpticalSystemSolver(system, getLight Ellipticity.defaultValue a)
    //    //printfn "sol = %A" sol
    //    sol.emSys.rp


    //let getEmSys e a = 
    //    let sol = BaseOpticalSystemSolver(system, getLight e a)
    //    sol.emSys


    //let gePlotData1() : (float * float)[] =
    //    [| for i in 0..89 -> (float i, (float i |> Angle.degree |> getSol)) |]


    //let plotAbc (f : List<OpticalFunction>) (g : Angle -> EmFieldSystem) =
    //    let data = [| for i in 0..89 -> (float i, (float i |> Angle.degree |> g)) |]

    //    let getFuncData (e : OpticalFunction) = data |> Array.map (fun (x, s) -> (x, s.func e))

    //    //Chart.Combine (f |> List.map (fun e -> Chart.Line(getFuncData e, Name = e.info.fullName)))
    //    //|> Chart.WithXAxis(Enabled = true, Title = "incidence angle", Min = 0.0, Max = 90.0)
    //    //|> Chart.WithLegend(InsideArea = true) // Docking = ChartTypes.Docking.Left, Title = "Title"
    //    //|> Chart.Show

    //     //FSharp.Plotly
    //    Chart.Combine (f |> List.map (fun e -> Chart.Line(getFuncData e, Name = e.info.fullName)))
    //    |> Chart.withX_AxisStyle(@"$\Phi$", MinMax = (0.0, 90.0))
    //    |> Chart.Show

    //    // Xplot.Plotly
    //    //Chart.Combine (f |> List.map (fun e -> Chart.Line(getFuncData e, Name = e.info.fullName)))
    //    //|> Chart.withX_AxisStyle(@"$\Phi$", MinMax = (0.0, 90.0))
    //    //|> Chart.Show

    //    //let lineTrace1 =
    //    //    Scatter(
    //    //        x = [1; 2; 3; 4],
    //    //        y = [10; 15; 13; 17],
    //    //        mode = "markers",
    //    //        name = "n1"
    //    //    )

    //    //let lineTrace2 =
    //    //    Scatter(
    //    //        x = [2; 3; 4; 5],
    //    //        y = [16; 5; 11; 9],
    //    //        mode = "lines",
    //    //        name = "n2"
    //    //    )

    //    //let lineTrace3 =
    //    //    Scatter(
    //    //        x = [1; 2; 3; 4],
    //    //        y = [12; 9; 15; 12],
    //    //        mode = "lines+markers",
    //    //        name = "$n_3$"
    //    //    )

    //    //[lineTrace1; lineTrace2; lineTrace3]
    //    //|> Chart.Plot
    //    //|> Chart.WithWidth 700
    //    //|> Chart.WithHeight 500
    //    //|> Chart.Show

    //    //|> Chart.withY_AxisStyle(f.info.fullName)
    //    //|> Chart.WithXAxis(Enabled = true, Title = "incidence angle", Min = 0.0, Max = 90.0)
    //    //|> Chart.WithLegend(InsideArea = true) // Docking = ChartTypes.Docking.Left, Title = "Title"



    //let plotAbc3D (f : OpticalFunction) (g : Ellipticity -> Angle -> EmFieldSystem) =
    //    let subData (Ellipticity e) = [| for i in 0..89 -> (float i |> Angle.degree |> g (Ellipticity e)).func f |]
    //    let data = 
    //        [| for i in 0..100 -> i |]
    //        |> PSeq.map (fun i -> (((float i) / 100.0) |> Ellipticity.create|> subData))
    //        |> Array.ofSeq

    //    let getFuncData (e : OpticalFunction) = data |> Array.map (fun x -> x |> Array.map (fun y -> y))

    //    Chart.Surface(data)
    //    |> Chart.withX_AxisStyle("$\Phi$", MinMax = (0.0, 90.0))
    //    |> Chart.withY_AxisStyle("e", MinMax = (0.0, 1.0))
    //    |> Chart.withZ_AxisStyle(f.info.fullName)
    //    |> Chart.Show


    //let plot() = plotAbc [ Rp; Tp ] (getEmSys Ellipticity.defaultValue)
    //let plot3D() = plotAbc3D Rp getEmSys


    /////////////////////////////////////////

    //let plotAbcNew (fn : List<OpticalFunction>) (f : FixedInfo) (x : RangedVariable) =
    //    let data = calculate f x
    //    let getFuncData (e : OpticalFunction) = data |> List.map (fun (v, s) -> (v, s.func e)) |> Array.ofList

    //     //FSharp.Plotly
    //    Chart.Combine (fn |> List.map (fun e -> Chart.Line(getFuncData e, Name = e.info.fullName)))
    //    |> Chart.withX_AxisStyle(x.name, MinMax = (x.plotMinValue, x.plotMaxValue))
    //    |> Chart.Show


    //let v1 = IncidenceAngleRange (Range<_>.create IncidenceAngle.normal IncidenceAngle.maxValue)
    //let v2 = EllipticityRange (Range<_>.create Ellipticity.defaultValue Ellipticity.maxValue)
    //let info1 = WaveLength.nm 600.0 |> IncidentLightInfo.create
    //let info2 = IncidentLightInfo.createInclined (WaveLength.nm 600.0) (Angle.degree 59.0 |> IncidenceAngle.create)
    //let f1 = { incidentLightInfo = info1; opticalSystem = system.fullSystem } : FixedInfo
    //let f2 = { incidentLightInfo = info2; opticalSystem = system.fullSystem } : FixedInfo
    ////let fn = [ Rp; Rs; Tp; Ts ]
    //let fn = [ R; T ]

    //let plot1() = plotAbcNew fn f1 v1
    //let plot2() = plotAbcNew fn f2 v2
