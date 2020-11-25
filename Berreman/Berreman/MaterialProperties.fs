namespace Berreman
module MaterialProperties =

    open System.Numerics
    open MathNetNumericsMath
    open Geometry

    /// DU to be used for choosing Eps / Mu / Rho.
    type OpticalPropertyComponent =
        | EpsComp
        | MuComp
        | RhoComp


    /// DU to be used for choosing re or im.
    type UseReIm =
        | UseRe
        | UseIm


    /// DU to be used for performing some transformation: for Eps we usually want to take a square root first.
    type OpticalTransformation =
        | NoTransformation
        | SquareRoot
        | MultByMillion

        member this.transform : (Complex -> Complex) =
            match this with
            | NoTransformation -> id
            | SquareRoot -> sqrt
            | MultByMillion -> fun x -> x * (cplx 1000000.0)


    // Covers only real refraction indices.
    type RefractionIndex =
        | RefractionIndex of double

        member r.value = let (RefractionIndex v) = r in v
        static member create n = RefractionIndex n
        static member vacuum = RefractionIndex.create 1.0


    type ComplexRefractionIndex =
        | ComplexRefractionIndex of Complex

        member r.value = let (ComplexRefractionIndex v) = r in v
        static member create n = ComplexRefractionIndex n


    type EpsValue =
        | EpsValue of double

        member eps.value = let (EpsValue e) = eps in e

        static member fromRefractionIndex (RefractionIndex n) = EpsValue (n * n)


    type Eps =
        | Eps of ComplexMatrix3x3

        static member create a = a |> ComplexMatrix3x3.create |> Eps
        static member fromRe a = a |> ComplexMatrix3x3.fromRe |> Eps
        static member vacuum = ComplexMatrix3x3.identity |> Eps

        member eps.Item
            with get(i, j) =
                let (Eps (ComplexMatrix3x3 v)) = eps
                v.[i, j]

        member eps.Item
            with get(i : Index, j : Index) =
                let (Eps (ComplexMatrix3x3 v)) = eps
                v.[i.numeric, j.numeric]

        static member fromRefractionIndex (RefractionIndex n) =
            (n * n |> cplx) * ComplexMatrix3x3.identity |> Eps

        static member fromRefractionIndex (RefractionIndex n1, RefractionIndex n2, RefractionIndex n3) =
            [
                [ n1 * n1; 0.; 0. ]
                [ 0.; n2 * n2; 0. ]
                [ 0.; 0.; n3 * n3 ]
            ]
            |> Eps.fromRe

        static member fromComplexRefractionIndex (ComplexRefractionIndex n1, ComplexRefractionIndex n2, ComplexRefractionIndex n3) =
            [
                [ n1 * n1; complexZero; complexZero ]
                [ complexZero; n2 * n2; complexZero ]
                [ complexZero; complexZero; n3 * n3 ]
            ]
            |> Eps.create

        static member fromComplexRefractionIndex n = Eps.fromComplexRefractionIndex (n, n, n)

        member eps.re =
            let (Eps e) = eps
            e.re.toComplex() |> Eps

    type MuValue =
        | MuValue of double

        member mu.value = let (MuValue m) = mu in m


    type Mu =
        | Mu of ComplexMatrix3x3

        static member (*) (ComplexVector3 a, Mu (ComplexMatrix3x3 b)) : ComplexVector3 = a * b |> ComplexVector3
        static member (*) (Mu (ComplexMatrix3x3 a), ComplexVector3 b) : ComplexVector3 = a * b |> ComplexVector3
        static member create a = a |> ComplexMatrix3x3.create |> Mu
        static member fromRe a = a |> ComplexMatrix3x3.fromRe |> Mu
        static member vacuum = ComplexMatrix3x3.identity |> Mu

        member mu.Item
            with get(i, j) =
                let (Mu (ComplexMatrix3x3 v)) = mu
                v.[i, j]

        member mu.Item
            with get(i : Index, j : Index) =
                let (Mu (ComplexMatrix3x3 v)) = mu
                v.[i.numeric, j.numeric]


    type RhoValue =
        | RhoValue of double

        member rho.value = let (RhoValue r) = rho in r


    type Rho =
        | Rho of ComplexMatrix3x3

        static member (*) (ComplexVector3 a, Rho (ComplexMatrix3x3 b)) : ComplexVector3 = a * b |> ComplexVector3
        static member (*) (Rho (ComplexMatrix3x3 a), ComplexVector3 b) : ComplexVector3 = a * b |> ComplexVector3
        static member create a = a |> ComplexMatrix3x3.create |> Rho
        static member fromIm a = a |> ComplexMatrix3x3.fromIm |> Rho
        static member vacuum = ComplexMatrix3x3.zero |> Rho

        member rho.Item
            with get(i, j) =
                let (Rho (ComplexMatrix3x3 v)) = rho
                v.[i, j]

        member rho.Item
            with get(i : Index, j : Index) =
                let (Rho (ComplexMatrix3x3 v)) = rho
                v.[i.numeric, j.numeric]


    type RhoT =
        | RhoT of ComplexMatrix3x3

        static member (*) (ComplexVector3 a, RhoT (ComplexMatrix3x3 b)) : ComplexVector3 = a * b |> ComplexVector3
        static member (*) (RhoT (ComplexMatrix3x3 a), ComplexVector3 b) : ComplexVector3 = a * b |> ComplexVector3

        member this.Item
            with get(i, j) =
                let (RhoT (ComplexMatrix3x3 v)) = this
                v.[i, j]


    type OpticalProperties =
        {
            eps : Eps
            mu : Mu
            rho : Rho
        }

        member this.rhoT : RhoT =
            let (Rho (ComplexMatrix3x3 r)) = this.rho
            r.conjugateTranspose |> ComplexMatrix3x3 |> RhoT

        static member fromEpsion eps =
            {
                eps = eps
                mu = ComplexMatrix3x3.identity |> Mu
                rho = ComplexMatrix3x3.zero |> Rho
            }

        static member fromRefractionIndex n = Eps.fromRefractionIndex n |> OpticalProperties.fromEpsion
        static member fromRefractionIndex (n1, n2, n3) = Eps.fromRefractionIndex(n1, n2, n3) |> OpticalProperties.fromEpsion
        static member vacuum = Eps.vacuum |> OpticalProperties.fromEpsion

        member this.rotate (Rotation r) =
            let c = r.toComplex()
            let cInv = c.inverse
            let rotate e = cInv * e * c

            {
                eps =
                    let (Eps a) = this.eps
                    a |> rotate |> Eps
                mu =
                    let (Mu a) = this.mu
                    a |> rotate |> Mu
                rho =
                    let (Rho a) = this.rho
                    a |> rotate |> Rho
            }

        member this.rotatePiX = this.rotate Rotation.rotatePiX
        member this.rotateX a = Rotation.rotateX a |> this.rotate
        member this.rotateY a = Rotation.rotateY a |> this.rotate
        member this.rotateZ a = Rotation.rotateZ a |> this.rotate

        member this.opticalComponent c =
            match c with
            | EpsComp ->
                let (Eps eps) = this.eps
                eps
            | MuComp ->
                let (Mu mu) = this.mu
                mu
            | RhoComp ->
                let (Rho rho) = this.rho
                rho
