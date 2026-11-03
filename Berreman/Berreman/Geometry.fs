namespace Berreman
open System
open System.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNetNumericsMath
open MatrixExp
open Constants

module Geometry =

    let thread a f =
        match a with
        | Some b -> f b |> Some
        | None -> None


    /// DU indices to be used in 3x3 matrices, like Eps, Mu, Rho.
    type Index =
        | One
        | Two
        | Three

        member this.numeric =
            match this with
            | One -> 0
            | Two -> 1
            | Three -> 2


    type Angle =
        | Angle of double

        /// Radians in one degree.
        static member radiansPerDegree : float = Math.PI / 180.0

        /// The degrees in r radians.
        static member radiansToDegrees (r : float) : float = r * 180.0 / Math.PI

        member angle.value = let (Angle a) = angle in a
        member angle.degrees = let (Angle a) = angle in (a / Angle.radiansPerDegree)
        static member degree (a : float<deg>) : Angle = float a * Angle.radiansPerDegree |> Angle
        static member zero = Angle 0.0
        static member pi = Angle Math.PI
        static member halfPi = Angle (Math.PI / 2.0)
        static member (+) (Angle a, Angle b) = a + b |> Angle
        static member (-) (Angle a, Angle b) = a - b |> Angle
        static member (~-) (Angle a) = -a |> Angle


    type RealVector2 =
        | RealVector2 of RealVector
        member this.Item
            with get i =
                let (RealVector2 v) = this
                v.[i]

        member this.x = this.[0]
        member this.y = this.[1]

        member this.norm =
            let (RealVector2 v) = this
            v.norm

        static member create a = RealVector.create a |> RealVector2

        static member zeroVector = [ 0.; 0. ] |> RealVector2.create


    type RealVector3 =
        | RealVector3 of RealVector
        member this.Item
            with get i =
                let (RealVector3 v) = this
                v.[i]

        member this.x = this.[0]
        member this.y = this.[1]
        member this.z = this.[2]

        member this.norm =
            let (RealVector3 v) = this
            v.norm

        static member create a = RealVector.create a |> RealVector3

        static member cross (u : RealVector3) (v : RealVector3) =
            [
                u.y * v.z - u.z * v.y
                u.z * v.x -  u.x * v.z
                u.x * v.y - u.y * v.x
            ]
            |> RealVector3.create

        static member zeroVector = [ 0.; 0.; 0. ] |> RealVector3.create


    /// Orthonormal real basis.
    type RealBasis3 =
        {
            vX : RealVector3
            vY : RealVector3
            vZ : RealVector3
        }
        static member defaultValue =
            {
                vX = [ 1.0; 0.0; 0.0 ] |> RealVector3.create
                vY = [ 0.0; 1.0; 0.0 ] |> RealVector3.create
                vZ = [ 0.0; 0.0; 1.0 ] |> RealVector3.create
            }


    type RealVector4 =
        | RealVector4 of RealVector
        member this.Item
            with get i =
                let (RealVector4 v) = this
                v.[i]

        static member create a = RealVector.create a |> RealVector4
        static member (+) (RealVector4 a, RealVector4 b) = a + b |> RealVector4
        static member (-) (RealVector4 a, RealVector4 b) = a - b |> RealVector4

        static member Zero
            with get () = [ 0.0; 0.0; 0.0; 0.0 ] |> RealVector4.create


    type ComplexVector2 =
        | ComplexVector2 of ComplexVector
        member this.Item
            with get i =
                let (ComplexVector2 v) = this
                v.[i]

        member this.x = this.[0]
        member this.y = this.[1]

        static member create a = ComplexVector.create a |> ComplexVector2
        static member (+) (ComplexVector2 a, ComplexVector2 b) = a + b |> ComplexVector2
        static member (-) (ComplexVector2 a, ComplexVector2 b) = a - b |> ComplexVector2

        static member Zero
            with get () = [ 0.0; 0.0 ] |> ComplexVector.fromRe |> ComplexVector2


    type ComplexVector3 =
        | ComplexVector3 of ComplexVector
        member this.Item
            with get i =
                let (ComplexVector3 v) = this
                v.[i]

        member this.x = this.[0]
        member this.y = this.[1]
        member this.z = this.[2]

        static member create a = a |> ComplexVector.create |> ComplexVector3
        static member fromRe (a : #seq<double>) = a |> ComplexVector.fromRe |> ComplexVector3
        static member fromIm (a : #seq<double>) = a |> ComplexVector.fromIm |> ComplexVector3
        static member (+) (ComplexVector3 a, ComplexVector3 b) : ComplexVector3 = a + b |> ComplexVector3

        static member cross (u : ComplexVector3) (v : ComplexVector3) =
            [
                u.y * v.z - u.z * v.y
                u.z * v.x -  u.x * v.z
                u.x * v.y - u.y * v.x
            ]
            |> ComplexVector3.create

        static member (*) (ComplexVector3 a, ComplexVector3 b) = a * b

        member this.conjugate =
            let (ComplexVector3 v) = this
            v.conjugate |> ComplexVector3

        member this.re =
            let (ComplexVector3 v) = this
            v.re |> RealVector3

        member this.im =
            let (ComplexVector3 v) = this
            v.im |> RealVector3

        static member (*) (a : Complex, ComplexVector3 b) =
            a * b |> ComplexVector3

        static member (*) (ComplexVector3 a, b : Complex) =
            a * b |> ComplexVector3

        static member (/) (ComplexVector3 a, b : Complex) =
            (a * (complexOne / b)) |> ComplexVector3

        member this.norm =
            let (ComplexVector3 v) = this
            v.norm


    /// Orthonormal complex basis
    type ComplexBasis3 =
        {
            cX : ComplexVector3
            cY : ComplexVector3
            cZ : ComplexVector3
        }
        member b.toX (v : ComplexVector3) =
            b.cX * ((b.cX.conjugate * v) / (b.cX * b.cX.conjugate))

        member b.toY (v : ComplexVector3) =
            b.cY * ((b.cY.conjugate * v)/ (b.cY * b.cY.conjugate))

        member b.toZ (v : ComplexVector3) =
            b.cZ * ((b.cZ.conjugate * v)/ (b.cZ * b.cZ.conjugate))

        static member defaultValue =
            {
                cX = [ Complex.ofReal 1.0; Complex.ofReal 0.0; Complex.ofReal 0.0 ] |> ComplexVector3.create
                cY = [ Complex.ofReal 0.0; Complex.ofReal 1.0; Complex.ofReal 0.0 ] |> ComplexVector3.create
                cZ = [ Complex.ofReal 0.0; Complex.ofReal 0.0; Complex.ofReal 1.0 ] |> ComplexVector3.create
            }


    type ComplexVector4 =
        | ComplexVector4 of ComplexVector
        member this.Item
            with get i =
                let (ComplexVector4 v) = this
                v.[i]

        static member (*) (a : Complex, ComplexVector4 b) : ComplexVector4 = a * b |> ComplexVector4
        static member (*) (ComplexVector4 a, b : Complex) : ComplexVector4 = a * b |> ComplexVector4
        static member (*) (ComplexVector4 a, ComplexVector4 b) : Complex = a * b
        static member create a = a |> ComplexVector.create |> ComplexVector4
        static member fromRe (a : #seq<double>) = a |> ComplexVector.fromRe |> ComplexVector4
        static member fromIm (a : #seq<double>) = a |> ComplexVector.fromIm |> ComplexVector4
        member this.conjugate = let (ComplexVector4 v) = this in v.conjugate |> ComplexVector4
        member this.norm =  let (ComplexVector4 v) = this in v.norm


    type RealMatrix2x2 =
        | RealMatrix2x2 of RealMatrix
        member this.Item
            with get(i, j) =
                let (RealMatrix2x2 v) = this
                v.[i, j]

        static member create a = a |> RealMatrix.create |> RealMatrix2x2
        static member identity = DiagonalMatrix.create 2 1.0 |> RealMatrix |> RealMatrix2x2

        static member (*) (RealMatrix2x2 a, RealMatrix2x2 b) : RealMatrix2x2 =
            a * b |> RealMatrix2x2

        static member (*) (RealVector2 a, RealMatrix2x2 b) : RealVector2 =
            a * b |> RealVector2

        static member (*) (RealMatrix2x2 a, RealVector2 b) : RealVector2 =
            a * b |> RealVector2

        member this.inverse =
            let (RealMatrix2x2 m) = this
            m.inverse |> RealMatrix2x2


    type RealMatrix3x3 =
        | RealMatrix3x3 of RealMatrix
        member this.Item
            with get(i, j) =
                let (RealMatrix3x3 v) = this
                v.[i, j]

        static member create a = a |> RealMatrix.create |> RealMatrix3x3
        static member identity = DiagonalMatrix.create 3 1.0 |> RealMatrix |> RealMatrix3x3

        static member (*) (RealMatrix3x3 a, RealMatrix3x3 b) : RealMatrix3x3 =
            a * b |> RealMatrix3x3

        static member (*) (RealVector3 a, RealMatrix3x3 b) : RealVector3 =
            a * b |> RealVector3

        static member (*) (RealMatrix3x3 a, RealVector3 b) : RealVector3 =
            a * b |> RealVector3

        member this.inverse =
            let (RealMatrix3x3 m) = this
            m.inverse |> RealMatrix3x3


    type RealMatrix4x4 =
        | RealMatrix4x4 of RealMatrix
        member this.Item
            with get(i, j) =
                let (RealMatrix4x4 v) = this
                v.[i, j]

        static member create a = RealMatrix.create a |> RealMatrix4x4

        static member Zero
            with get () =
                [
                    [ 0.0; 0.0; 0.0; 0.0 ]
                    [ 0.0; 0.0; 0.0; 0.0 ]
                    [ 0.0; 0.0; 0.0; 0.0 ]
                    [ 0.0; 0.0; 0.0; 0.0 ]
                ]
                |> RealMatrix4x4.create


    type ComplexMatrix2x2 =
        | ComplexMatrix2x2 of ComplexMatrix
        member this.Item
            with get(i, j) =
                let (ComplexMatrix2x2 v) = this
                v[i, j]

        member this.Item
            with get(i : Index, j : Index) =
                let (ComplexMatrix2x2 v) = this
                v[i.numeric, j.numeric]

        static member (*) (ComplexMatrix2x2 a, ComplexMatrix2x2 b) : ComplexMatrix2x2 =
            a * b |> ComplexMatrix2x2

        static member (*) (a : Complex, ComplexMatrix2x2 b) : ComplexMatrix2x2 =
            a * b |> ComplexMatrix2x2

        static member (*) (ComplexMatrix2x2 a, b : Complex) : ComplexMatrix2x2 =
            a * b |> ComplexMatrix2x2

        static member (*) (ComplexVector2 a, ComplexMatrix2x2 b) : ComplexVector2 =
            a * b |> ComplexVector2

        static member (*) (ComplexMatrix2x2 a, ComplexVector2 b) : ComplexVector2 =
            a * b |> ComplexVector2

        static member identity = DiagonalMatrix.create 2 (Complex.ofReal 1.0) |> ComplexMatrix |> ComplexMatrix2x2
        static member zero = DiagonalMatrix.create 2 (Complex.ofReal 0.0) |> ComplexMatrix |> ComplexMatrix2x2
        static member create a = a |> ComplexMatrix.create |> ComplexMatrix2x2
        static member fromRe a = a |> ComplexMatrix.fromRe |> ComplexMatrix2x2
        static member fromIm a = a |> ComplexMatrix.fromIm |> ComplexMatrix2x2

        member this.inverse =
            let (ComplexMatrix2x2 m) = this
            m.inverse |> ComplexMatrix2x2

        member this.re =
            let (ComplexMatrix2x2 m) = this
            m.re |> RealMatrix2x2

        member this.im =
            let (ComplexMatrix2x2 m) = this
            m.im |> RealMatrix2x2

        // static member Zero
        //     with get () =
        //         [
        //             [ 0.0; 0.0 ]
        //             [ 0.0; 0.0 ]
        //         ]
        //         |> ComplexMatrix2x2.create


    type RealMatrix2x2
        with
        member this.toComplex () =
            let (RealMatrix2x2 (RealMatrix m)) = this
            let len = m.RowCount
            [| for i in 0..(len-1) -> [| for j in 0..(len-1) -> Complex.ofReal m.[i, j] |] |] |> ComplexMatrix2x2.create


    type ComplexMatrix3x3 =
        | ComplexMatrix3x3 of ComplexMatrix
        member this.Item
            with get(i, j) =
                let (ComplexMatrix3x3 v) = this
                v.[i, j]

        member this.Item
            with get(i : Index, j : Index) =
                let (ComplexMatrix3x3 v) = this
                v.[i.numeric, j.numeric]

        static member (*) (ComplexMatrix3x3 a, ComplexMatrix3x3 b) : ComplexMatrix3x3 =
            a * b |> ComplexMatrix3x3

        static member (*) (a : Complex, ComplexMatrix3x3 b) : ComplexMatrix3x3 =
            a * b |> ComplexMatrix3x3

        static member (*) (ComplexMatrix3x3 a, b : Complex) : ComplexMatrix3x3 =
            a * b |> ComplexMatrix3x3

        static member (*) (ComplexVector3 a, ComplexMatrix3x3 b) : ComplexVector3 =
            a * b |> ComplexVector3

        static member (*) (ComplexMatrix3x3 a, ComplexVector3 b) : ComplexVector3 =
            a * b |> ComplexVector3

        static member identity = DiagonalMatrix.create 3 (Complex.ofReal 1.0) |> ComplexMatrix |> ComplexMatrix3x3
        static member zero = DiagonalMatrix.create 3 (Complex.ofReal 0.0) |> ComplexMatrix |> ComplexMatrix3x3
        static member create a = a |> ComplexMatrix.create |> ComplexMatrix3x3
        static member fromRe a = a |> ComplexMatrix.fromRe |> ComplexMatrix3x3
        static member fromIm a = a |> ComplexMatrix.fromIm |> ComplexMatrix3x3

        member this.inverse =
            let (ComplexMatrix3x3 m) = this
            m.inverse |> ComplexMatrix3x3

        member this.re =
            let (ComplexMatrix3x3 m) = this
            m.re |> RealMatrix3x3

        member this.im =
            let (ComplexMatrix3x3 m) = this
            m.im |> RealMatrix3x3


    type RealMatrix3x3
        with
        member this.toComplex () =
            let (RealMatrix3x3 (RealMatrix m)) = this
            let len = m.RowCount
            [| for i in 0..(len-1) -> [| for j in 0..(len-1) -> Complex.ofReal m.[i, j] |] |] |> ComplexMatrix3x3.create


    // It is only needed for 4x4 matrices here.
    type EigenValueVector =
        {
            value : Complex
            vector : ComplexVector4
        }


    type EigenBasis =
        {
            evv0 : EigenValueVector
            evv1 : EigenValueVector
        }
        member this.values = [ this.evv0.value; this.evv1.value ]
        member this.vectors = [ this.evv0.vector; this.evv1.vector ]
        member this.e0 = this.evv0.vector
        member this.e1 = this.evv1.vector
        member this.v0 = this.evv0.value
        member this.v1 = this.evv1.value

        static member create l =
            let fail() = failwith "EigenBasis::Invalid input data."

            match l with
            | [] -> fail()
            | h0 :: t ->
                match t with
                | [] -> fail()
                | h1 :: t1 ->
                    match t1 with
                    | [] ->
                        {
                            evv0 = { value = fst h0; vector = snd h0 }
                            evv1 = { value = fst h1; vector = snd h1 }
                        }
                    | _ -> fail()


    type FullEigenBasis =
        {
            down : EigenBasis
            up : EigenBasis
        }


    type ComplexMatrix4x4 =
        | ComplexMatrix4x4 of ComplexMatrix
        member this.Item
            with get(i, j) =
                let (ComplexMatrix4x4 v) = this
                v.[i, j]

        static member create a = ComplexMatrix.create a |> ComplexMatrix4x4
        static member fromRe a = a |> ComplexMatrix.fromRe |> ComplexMatrix4x4
        static member fromIm a = a |> ComplexMatrix.fromIm |> ComplexMatrix4x4

        static member (*) (ComplexMatrix4x4 a, ComplexMatrix4x4 b) : ComplexMatrix4x4 =
            a * b |> ComplexMatrix4x4

        static member (*) (a : Complex, ComplexMatrix4x4 b) : ComplexMatrix4x4 =
            a * b |> ComplexMatrix4x4

        static member (*) (ComplexMatrix4x4 a, b : Complex) : ComplexMatrix4x4 =
            a * b |> ComplexMatrix4x4

        static member (*) (ComplexVector4 a, ComplexMatrix4x4 b) : ComplexVector4 =
            a * b |> ComplexVector4

        static member (*) (ComplexMatrix4x4 a, ComplexVector4 b) : ComplexVector4 =
            a * b |> ComplexVector4

        member this.matrixExp (x : Complex) : ComplexMatrix4x4 =
            let (ComplexMatrix4x4 v) = this * x
            v.matrixExp() |> ComplexMatrix4x4

        static member identity = DiagonalMatrix.create 4 (Complex.ofReal 1.0) |> ComplexMatrix |> ComplexMatrix4x4

        member this.re =
            let (ComplexMatrix4x4 m) = this
            m.re |> RealMatrix4x4

        member this.im =
            let (ComplexMatrix4x4 m) = this
            m.im |> RealMatrix4x4


    type Angle
        with

        /// Rotation around x axis.
        member this.xRotation : RealMatrix3x3 =
            let (Angle xAngle) = this
            [
                [ 1.; 0.; 0. ]
                [ 0.; cos(xAngle); -sin(xAngle) ]
                [ 0.; sin(xAngle); cos(xAngle) ]
            ]
            |> RealMatrix3x3.create

        /// Rotation around y axis.
        member this.yRotation : RealMatrix3x3 =
            let (Angle yAngle) = this
            [
                [ cos(yAngle); 0.; sin(yAngle) ]
                [ 0.; 1.; 0. ]
                [ -sin(yAngle); 0.; cos(yAngle) ]
            ]
            |> RealMatrix3x3.create

        /// Rotation around z axis.
        member this.zRotation : RealMatrix3x3 =
            let (Angle zAngle) = this
            [
                [ cos(zAngle); -sin(zAngle); 0. ]
                [ sin(zAngle); cos(zAngle); 0. ]
                [ 0.; 0.; 1. ]
            ]
            |> RealMatrix3x3.create


    /// Rotation in opposite direction is marked with "-" in the code and "m" in the name.
    /// Rotation for angle (Pi - z) means rotating for angle (Pi - alphaZ) around z axis.
    type RotationConvention =
        | ZmXpZm // Rotation around (-z), (x'), (-z'')
        | ZmYpXp // Rotation around (-z), (y'), (x'')
        | ZmYmXp // Rotation around (-z), (-y'), (x'')
        | ZpYpXp // Rotation around (z), (y'), (x'')

        | YmZmXp // Rotation around (-y), (-z'), (x'')

        // For compatibility with Mathematica code.
        | PiZmXpPiZm //Rotation around (Pi - z), (x'), (Pi - z'') = Euler angles in Mathematica code.

        static member conventionMapping convention =
            match convention with
            | ZmXpZm -> [ (fun (a : Angle) -> (-a).zRotation); (fun a -> a.xRotation); (fun a -> (-a).zRotation) ]
            | ZmYpXp -> [ (fun (a : Angle) -> (-a).zRotation); (fun a -> a.yRotation); (fun a -> a.xRotation) ]
            | ZmYmXp -> [ (fun (a : Angle) -> (-a).zRotation); (fun a -> (-a).yRotation); (fun a -> a.xRotation) ]
            | ZpYpXp -> [ (fun (a : Angle) -> a.zRotation); (fun a -> a.yRotation); (fun a -> a.xRotation) ]

            | YmZmXp -> [ (fun (a : Angle) -> (-a).yRotation); (fun a -> (-a).zRotation); (fun a -> a.xRotation) ]

            | PiZmXpPiZm -> [ (fun (a : Angle) -> (Angle.pi - a).zRotation); (fun a -> a.xRotation); (fun a -> (Angle.pi - a).zRotation) ]


    type Rotation =
        | Rotation of RealMatrix3x3

        static member create convention phi theta psi =
            [ phi; theta; psi ]
            |> List.zip (RotationConvention.conventionMapping convention)
            |> List.map (fun (r, a) -> r a)
            |> List.fold (fun acc r -> r * acc) RealMatrix3x3.identity

        static member createZmXpZm = Rotation.create ZmXpZm
        static member createZmYpXp = Rotation.create ZmYpXp
        static member createZmYmXp = Rotation.create ZmYmXp
        static member createYmZmXp = Rotation.create YmZmXp
        static member createZpYpXp = Rotation.create ZpYpXp
        static member createPiZmXpPiZm = Rotation.create PiZmXpPiZm
        static member rotatePiX = Rotation.createZmXpZm Angle.zero Angle.pi Angle.zero |> Rotation
        static member rotateX a = Rotation.createZmYpXp Angle.zero Angle.zero a |> Rotation
        static member rotateY a = Rotation.createZmYpXp Angle.zero a Angle.zero |> Rotation
        static member rotateZ a = Rotation.createZmYpXp a Angle.zero Angle.zero |> Rotation
//        static member rotateZY a b = Rotation.createZmYpXp a b Angle.zero |> Rotation
        static member rotateYZ a b = Rotation.createYmZmXp a b Angle.zero |> Rotation
        static member rotateHalfPiY = Rotation.rotateZ Angle.halfPi


    type RealVector3
        with

        member v.rotate (Rotation r) =
            let rInv = r.inverse
            rInv * v


    type ComplexVector3
        with

        member v.rotate (Rotation r) =
            let rInv = r.toComplex().inverse
            rInv * v
