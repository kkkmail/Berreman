namespace Berreman
open System.Numerics
open MathNetNumericsMath
open MatrixExp

module Geometry =

    let thread a f =
        match a with
        | Some b -> f b |> Some
        | None -> None


    let realIdentityMatrix n = realDiagonalMatrix n 1.0
    let realZeroMatrix n = realDiagonalMatrix n 0.0

    let complexIdentityMatrix n = complexDiagonalMatrix n (cplx 1.0)
    let complexZeroMatrix n = complexDiagonalMatrix n (cplx 0.0)


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

        member angle.value = let (Angle a) = angle in a
        member angle.degrees = let (Angle a) = angle in (a / degree)
        static member degree a = a * degree |> Angle
        static member radian r = r |> Angle
        static member zero = Angle.radian 0.0
        static member pi = Angle.radian pi
        static member halfPi = Angle.radian (pi / 2.0)
        static member (+) (Angle a, Angle b) = a + b |> Angle
        static member (-) (Angle a, Angle b) = a - b |> Angle
        static member (~-) (Angle a) = -a |> Angle


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

        static member (*) (RealVector3 a, RealVector3 b) : double = a * b
        static member (*) (a : double, RealVector3 b) = a * b |> RealVector3
        static member (*) (RealVector3 a, b : double) = b * a
        static member (/) (RealVector3 a, b : double) = a / b |> RealVector3
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
        member b.toX (v : RealVector3) =
            b.vX * (b.vX * v)

        member b.toY (v : RealVector3) =
            b.vY * (b.vY * v)

        member b.toZ (v : RealVector3) =
            b.vZ * (b.vZ * v)

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
        static member fromRe a = a |> ComplexVector.fromRe |> ComplexVector3
        static member fromIm a = a |> ComplexVector.fromIm |> ComplexVector3
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
                cX = [ cplx 1.0; cplx 0.0; cplx 0.0 ] |> ComplexVector3.create
                cY = [ cplx 0.0; cplx 1.0; cplx 0.0 ] |> ComplexVector3.create
                cZ = [ cplx 0.0; cplx 0.0; cplx 1.0 ] |> ComplexVector3.create
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
        static member fromRe a = a |> ComplexVector.fromRe |> ComplexVector4
        static member fromIm a = a |> ComplexVector.fromIm |> ComplexVector4
        member this.conjugate = let (ComplexVector4 v) = this in v.conjugate |> ComplexVector4
        member this.norm =  let (ComplexVector4 v) = this in v.norm


    type RealMatrix3x3 =
        | RealMatrix3x3 of RealMatrix
        member this.Item
            with get(i, j) =
                let (RealMatrix3x3 v) = this
                v.[i, j]

        member this.Item
            with get(i : Index, j : Index) =
                let (RealMatrix3x3 v) = this
                v.[i.numeric, j.numeric]

        static member create a = a |> RealMatrix.create |> RealMatrix3x3
        static member identity = realIdentityMatrix 3 |> RealMatrix3x3

        static member (*) (RealMatrix3x3 a, RealMatrix3x3 b) : RealMatrix3x3 =
            a * b |> RealMatrix3x3

        static member (*) (a : double, RealMatrix3x3 b) : RealMatrix3x3 =
            a * b |> RealMatrix3x3

        static member (*) (RealMatrix3x3 a, b : double) : RealMatrix3x3 =
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

        static member identity = complexIdentityMatrix 3 |> ComplexMatrix3x3
        static member zero = complexZeroMatrix 3 |> ComplexMatrix3x3
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
            [| for i in 0..(len-1) -> [| for j in 0..(len-1) -> cplx m.[i, j] |] |] |> ComplexMatrix3x3.create


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

        static member identity = complexIdentityMatrix 4 |> ComplexMatrix4x4

        member this.re =
            let (ComplexMatrix4x4 m) = this
            m.re |> RealMatrix4x4

        member this.im =
            let (ComplexMatrix4x4 m) = this
            m.im |> RealMatrix4x4


    /// Rotation around x axis.
    let xRotation (Angle xAngle) =
        [
            [ 1.; 0.; 0. ]
            [ 0.; cos(xAngle); -sin(xAngle) ]
            [ 0.; sin(xAngle); cos(xAngle) ]
        ]
        |> RealMatrix3x3.create


    /// Rotation around y axis.
    let yRotation (Angle yAngle) =
        [
            [ cos(yAngle); 0.; sin(yAngle) ]
            [ 0.; 1.; 0. ]
            [ -sin(yAngle); 0.; cos(yAngle) ]
        ]
        |> RealMatrix3x3.create


    /// Rotation around z axis.
    let zRotation (Angle zAngle) =
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
            | ZmXpZm -> [ (fun a -> zRotation (-a)); xRotation; (fun a -> zRotation (-a)) ]
            | ZmYpXp -> [ (fun a -> zRotation (-a)); yRotation; xRotation]
            | ZmYmXp -> [ (fun a -> zRotation (-a)); (fun a -> yRotation (-a)); xRotation]
            | ZpYpXp -> [ zRotation; yRotation; xRotation]

            | YmZmXp -> [ (fun a -> yRotation (-a)); (fun a -> zRotation (-a)); xRotation]

            | PiZmXpPiZm -> [ (fun a -> zRotation (Angle.pi - a)); xRotation; (fun a -> zRotation (Angle.pi - a)) ]


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
