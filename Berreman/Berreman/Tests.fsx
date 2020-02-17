//===========================================================
#I __SOURCE_DIRECTORY__
#r "System.Core.dll"
#r "System.dll"
#r "System.Numerics.dll"
//===========================================================
#r "../packages/MathNet.Numerics.4.5.1/lib/net461/MathNet.Numerics.dll"
#r "../packages/MathNet.Numerics.FSharp.4.5.1/lib/net45/MathNet.Numerics.FSharp.dll"
#r "../packages/System.ValueTuple.4.5.0/lib/net47/System.ValueTuple.dll"
//===========================================================
#r "./bin/Debug/Berreman.dll"
//#r "./bin/Debug/Extreme.Numerics.Net40.dll"
//#r "./bin/Debug/Extreme.Numerics.Generic.Net40.dll"
//#r "./bin/Debug/Extreme.Numerics.FSharp.Net40.dll"
//===========================================================
//open Extreme.Mathematics
//open Extreme.Mathematics.LinearAlgebra
//NumericsConfiguration.Providers.RegisterGenericProvider()
//open Berreman.ExtremeNumericsMath

open System.Numerics
open Berreman.MathNetNumericsMath
open Berreman.MatrixExp

open Berreman.Constants
open Berreman.Fields
open Berreman.BerremanMatrix
open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.Solvers

//===========================================================
let info = 
    {
        wavelength = 500.0 * nm
        refractionIndex = 1.0
        incidenceAngle = 0.0 * degree |> IncidenceAngle
        polarization = 0.0 * degree |> Polarization
        ellipticity = Ellipticity.defaultValue
    }


let em = EmField.create info
printfn "em = %A" em

//let i4 = ComplexMatrix4x4.identity * (cplx 2.0)
//printfn "i4 = %A" i4

//let m1 = 
//    [
//        [0.0 |> cplx; 0.0 |> cplx; 0.0 |> cplx; 1.0 |> cplx]
//        [0.0 |> cplx; 0.0 |> cplx; 1.0 |> cplx; 0.0 |> cplx]
//        [0.0 |> cplx; 1.0 |> cplx; 0.0 |> cplx; 0.0 |> cplx]
//        [1.0 |> cplx; 0.0 |> cplx; 0.0 |> cplx; 0.0 |> cplx]
//    ]
//    |> matrix
//    |> ComplexMatrix4x4
//printfn "m1 = %A" m1

//let b1 = m1.matrixExp(Complex(0.0, Constants.Pi))
//printfn "b1 = %A" b1

//let m2 = 
//    [
//        [0.0 |> cplx; 0.0 |> cplx; 0.0 |> cplx; 1.0 |> cplx]
//        [0.0 |> cplx; 0.0 |> cplx; 1.0 |> cplx; 0.0 |> cplx]
//        [0.0 |> cplx; 1.0 |> cplx; 0.0 |> cplx; 0.0 |> cplx]
//        [0.0 |> cplx; 0.0 |> cplx; 0.0 |> cplx; 0.0 |> cplx]
//    ]
//    |> matrix
//    |> ComplexMatrix4x4
//printfn "m2 = %A" m2

//let b2 = m2.matrixExp(Complex(0.0, Constants.Pi))
//printfn "b2 = %A" b2

//printfn "Completed"

let o = OpticalProperties.defaultValue 1.5
printfn "o = %A" o

let (BerremanMatrix b) = BerremanMatrix.create o em
let (ComplexMatrix4x4 v) = b
printfn "b = %A" b
printfn "b.Determinant = %A" (v.determinant)

let b2 = b.matrixExp(createComplex 0.0 pi)
printfn "b2 = %A" b2

//printfn "Calling evd..."
//let evd = v.evd()
//printfn "evd = %A" evd
//printfn "Completed."
