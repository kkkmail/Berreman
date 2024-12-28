namespace Berreman
open System
open System.Numerics

//======================================================================
// Discriminated Union to specify Forward or Backward transform.
// 'sign' will return -1. for ForwardTransform and +1. for BackwardTransform.
//======================================================================
module FourierTransformPrimitives =

    type FourierTransformDirection =
        | ForwardTransform
        | BackwardTransform
        with
            member this.sign =
                match this with
                | ForwardTransform -> -1.0
                | BackwardTransform -> +1.0

    /// <summary>
    /// Multiplies each element in the array by 1 / sqrt(N), where N is the length of the array,
    /// to achieve symmetric (unitary) normalization.
    /// This function mutates the array in-place, and also returns the same array.
    /// In-place normalization for a Complex array, multiplying each element
    /// by 1 / sqrt(N). Mutates the array and also returns the same reference.
    /// </summary>
    let normalizeInPlace (arr : Complex[]) =
        let n = arr.Length
        if n > 0 then
            let factor = 1.0 / sqrt (float n)
            for i in 0 .. (n - 1) do
                arr.[i] <- arr.[i] * factor
        arr

    /// <summary>
    /// Accepts any #seq&lt;Complex&gt; (e.g. list, seq, array).
    /// If it's already a Complex[], we just do in-place.
    /// Otherwise, we copy to a new array, normalize in-place, then return it.
    /// </summary>
    /// <param name="xs">A collection of Complex (e.g., array, list, seq).</param>
    let normalize (xs : #seq<Complex>) =
        match box xs with
        | :? (Complex[]) as arr ->
            // Already a Complex[], normalize in-place, return original array
            normalizeInPlace arr
        | _ ->
            // It's some other seq type, convert to an array, then in-place normalize
            let arr = Seq.toArray xs
            normalizeInPlace arr

//======================================================================
// SimpleFourierTransform with recursive FFT.
//======================================================================

module SimpleFourierTransform =

    open FourierTransformPrimitives

    let pi  = Math.PI
    let tau = 2. * pi

    let twiddle a = Complex.FromPolarCoordinates(1., a)

    let rec private fftImpl (ftd : FourierTransformDirection) = function
        | []  -> []
        | [x] -> [x]
        | x ->
            x
            |> List.mapi (fun i c -> i % 2 = 0, c)
            |> List.partition fst
            |> fun (even, odd) -> fftImpl ftd (List.map snd even), fftImpl ftd (List.map snd odd)
            ||> List.mapi2 (fun i even odd ->
                // Use ftd.sign for the exponent sign
                let s = ftd.sign
                let btf = odd * twiddle (s * tau * (float i / float x.Length))
                even + btf, even - btf
            )
            |> List.unzip
            ||> List.append

    let fft ftd a = fftImpl ftd a |> normalize

//======================================================================
// Optimized FourierTransform with the same DU approach.
//======================================================================
module FourierTransform =

    open FourierTransformPrimitives

    let maxSize = 4096
    let pi      = Math.PI
    let tau     = 2. * pi

    module Details =

        let isPowerOf2 n = (n &&& (n - 1)) = 0


        let ilog2 n =
            if n < 2 then failwith "n must be greater than 1"
            if not (isPowerOf2 n) then failwith "n must be a power of 2"
            let rec loop n c s =
                let t = 1 <<< c
                if t = n then
                    c
                elif t > n then
                    loop n (c - s) (s >>> 1)
                else
                    loop n (c + s) (s >>> 1)
            loop n 16 8


        let twiddle a = Complex.FromPolarCoordinates(1., a)


        let twiddles =
            let unfolder c =
                if c < 2 * maxSize then
                    let vs = Array.init (c / 2) (fun i ->
                        // Precompute negative exponent. We'll flip (conjugate) if needed.
                        twiddle (-tau * float i / float c)
                    )
                    Some (vs, c * 2)
                else
                    None
            Array.unfold unfolder 1


        let rec loop (ftd : FourierTransformDirection) n2 ln s c f t =
            if c > 2 then
                let c2 = c >>> 1
                let struct (t, f) = loop ftd n2 (ln - 1) (s <<< 1) c2 f t

                let tws = twiddles.[ln]
                // If the DU = BackwardTransform => sign is +1 => flip exponent via conjugate
                // If DU = ForwardTransform => sign is -1 => keep original precomputed twiddle
                for j = 0 to c2 - 1 do
                    let w = tws.[j]
                    let w2 =
                        if ftd.sign > 0. then
                            Complex.Conjugate w
                        else
                            w

                    let off = s * j
                    let off2 = off <<< 1
                    if s > 1 then
                        for i = 0 to s - 1 do
                            let e = Array.get f (i + off2)
                            let o = Array.get f (i + off2 + s)
                            let a = w2 * o
                            Array.set t (i + off)       (e + a)
                            Array.set t (i + off + n2)  (e - a)
                    else
                        let e = Array.get f (2 * j)
                        let o = Array.get f (2 * j + s)
                        let a = w2 * o
                        Array.set t (j)       (e + a)
                        Array.set t (j + n2)  (e - a)

                struct (f, t)

            elif c = 2 then
                for i = 0 to s - 1 do
                    let e = Array.get f (i)
                    let o = Array.get f (i + s)
                    let a = o
                    Array.set t (i)       (e + a)
                    Array.set t (i + n2)  (e - a)

                struct (f, t)
            else
                struct (t, f)


    open Details


    let dft (ftd : FourierTransformDirection) (vs : Complex[]) =
        let l = vs.Length
        let am = tau / float l

        Array.init l (fun i ->
            let mutable s = Complex.Zero
            for j = 0 to l - 1 do
                // ftd.sign is -1 for forward, +1 for backward
                let angle = ftd.sign * float i * float j * am
                s <- s + vs.[j] * twiddle angle
            s)
        |> normalize


    let fft ftd (vs : Complex[]) =
        let n = vs.Length
        let ln = ilog2 n
        let vs0 = Array.copy vs
        let vs1 = Array.zeroCreate n
        let struct (_, t) = loop ftd (n >>> 1) ln 1 n vs0 vs1
        normalize t
