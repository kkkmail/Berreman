namespace Berreman

module MatrixEvd= 

    open System.Numerics
    //open MathNet.Numerics
    //open MathNet.Numerics.LinearAlgebra
    open MathNetNumericsMath
    open DotNumerics.LinearAlgebra.CSLapack

    type MathNetNumericsMath.ComplexMatrix
    with
        member this.evd = 
            let (MathNetNumericsMath.ComplexMatrix m) = this
            let evd = m.Evd()

            {
                eigenValues = evd.EigenValues |> MathNetNumericsMath.ComplexVector
                eigenVectors = evd.EigenVectors |> MathNetNumericsMath.ComplexMatrix
            }

