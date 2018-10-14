﻿namespace Analytics

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
open Berreman.Dispersion
open OpticalProperties.Standard
open Berreman
open OpticalProperties
open Analytics.StandardSystems

open Standard

open Analytics.Variables

//open FSharp.Charting
open FSharp.Plotly
//open XPlot.Plotly

module Charting =

    let plot (f : FixedInfo) (x : RangedVariable) (fn : List<OpticalFunction>) =
        let data = calculate f x

        let getFuncData (e : OpticalFunction) = 
            data 
            |> Array.map (fun (v, s) -> (v, s.func e))
            |> Array.choose (fun (x, yo) -> match yo with | Some y -> Some (x, y) | None -> None )

         //FSharp.Plotly
        Chart.Combine (fn |> List.map (fun e -> Chart.Line(getFuncData e, Name = e.info.fullName)))
        |> Chart.withX_AxisStyle(x.name, MinMax = (x.plotMinValue, x.plotMaxValue))
        |> Chart.Show


    let mapFun (data : #seq<#seq<double * double * Solution>>) (fn : OpticalFunction) = 
        data
        |> Seq.map (fun r -> r |> Seq.map (fun (_, _, e) -> e.func fn) |> Seq.choose id |> Array.ofSeq)
        |> Array.ofSeq


    let plot3D (f : FixedInfo) (x : RangedVariable) (y : RangedVariable) (fn : List<OpticalFunction>) =
        let xVal = x.plotPoints
        let yVal = y.plotPoints
        let data = calculate3D f x y

        let plotFun e = 
            let zVal = mapFun data e

            // kk:20180922 The axes are somehow mysteriouly reversed. Here we swap X with Y for both the data and the names.
            Chart.Surface(zVal, yVal, xVal, Opacity = 0.7, Contours = Contours.initXyz(Show = true), Name = e.info.name)
            |> Chart.withX_AxisStyle(y.name)
            |> Chart.withY_AxisStyle(x.name)
            |> Chart.withZ_AxisStyle(e.info.name)
            |> Chart.Show

        fn |> List.map (fun e -> plotFun e)


    let plotDispersion calc name (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = 
        let data = calc o r
        let x = r |> WaveLengthRange

         //FSharp.Plotly
        Chart.Line(data, Name = name)
        |> Chart.withX_AxisStyle(x.name, MinMax = (x.plotMinValue, x.plotMaxValue))
        |> Chart.Show


    let plotN11 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateN11Re "Re[e11]" o r
    let plotXi11 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateXi11Im "Im[e11]" o r

    let plotN22 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateN22Re "Re[e22]" o r
    let plotXi22 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateXi22Im "Im[e22]" o r

    let plotN33 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateN33Re "Re[e33]" o r
    let plotXi33 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateXi33Im "Im[e33]" o r


    let plotRho11 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateRho11Im "rho11" o r
    let plotRho22 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateRho22Im "rho22" o r
    let plotRho33 (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) = plotDispersion calculateRho33Im "rho33" o r
