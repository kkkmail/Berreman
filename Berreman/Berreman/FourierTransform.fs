namespace Berreman
open System
open System.Numerics

//======================================================================
// See:      https://chatgpt.com/g/g-p-67701ade2a9c8191b305c09f2b6bc473-gaussian-beams-in-f/c/67701b6c-ad90-8009-9736-eccc1d4abf5f
// Based on: https://fssnip.net/7Tn/title/Optimized-Fast-Fourier-Transform-FFT
//
// Discriminated Union to specify Forward or Backward transform.
// 'sign' will return -1.0 for ForwardTransform and +1.0 for BackwardTransform.
//
// Under symmetric (unitary) normalization, each transform (forward or backward)
// includes a factor of 1/sqrt(N). So the complete formulas become:
//
// Forward transform:  X[k] = (1/sqrt(N)) * Σ (x[n] * e^(- i 2π k n / N))
// Backward transform: x[n] = (1/sqrt(N)) * Σ (X[k] * e^(+ i 2π k n / N))
//
// This matches the usual 'industry standard' for negative exponent in forward
// transforms, except we also do a 1/sqrt(N) factor for unitary normalization.
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
    /// to achieve symmetric (unitary) normalization. Mutates the array in-place,
    /// and also returns the same reference.
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
    let normalize (xs : #seq<Complex>) =
        match box xs with
        | :? (Complex[]) as arr ->
            normalizeInPlace arr
        | _ ->
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
            // Must keep it on one line in F#.
            |> fun (even, odd) -> fftImpl ftd (List.map snd even), fftImpl ftd (List.map snd odd)
            ||> List.mapi2 (fun i even odd ->
                // Forward => sign = -1 => e^(-i 2π i / N)
                // Backward => sign = +1 => e^(+i 2π i / N)
                let s = ftd.sign
                let btf = odd * twiddle (s * tau * (float i / float x.Length))
                even + btf, even - btf
            )
            |> List.unzip
            ||> List.append

    /// <summary>
    /// Simple FFT returning a normalized list of Complex.
    /// 'ftd' picks forward or backward transform sign, each with 1/sqrt(N) factor.
    /// </summary>
    let fft ftd a =
        fftImpl ftd a
        |> normalize

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
                if t = n then c
                elif t > n then loop n (c - s) (s >>> 1)
                else loop n (c + s) (s >>> 1)
            loop n 16 8

        let twiddle a = Complex.FromPolarCoordinates(1., a)

        let twiddles =
            let unfolder c =
                if c < 2 * maxSize then
                    let vs = Array.init (c / 2) (fun i ->
                        // Precompute negative exponent => e^(-i * 2π * i / c).
                        // We'll flip via conjugate if needed for backward transform.
                        twiddle (-tau * float i / float c)
                    )
                    Some(vs, c * 2)
                else
                    None
            Array.unfold unfolder 1

        let rec loop (ftd : FourierTransformDirection) n2 ln s c f t =
            if c > 2 then
                let c2 = c >>> 1
                let struct (t, f) = loop ftd n2 (ln - 1) (s <<< 1) c2 f t

                let tws = twiddles.[ln]
                for j = 0 to c2 - 1 do
                    let w = tws.[j]
                    // Forward => sign=-1 => keep w = e^(-i ...)
                    // Backward => sign=+1 => conj => e^(+i ...)
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
                            Array.set t (i + off) (e + a)
                            Array.set t (i + off + n2) (e - a)
                    else
                        let e = Array.get f (2 * j)
                        let o = Array.get f (2 * j + s)
                        let a = w2 * o
                        Array.set t j (e + a)
                        Array.set t (j + n2) (e - a)

                struct (f, t)

            elif c = 2 then
                for i = 0 to s - 1 do
                    let e = Array.get f i
                    let o = Array.get f (i + s)
                    let a = o
                    Array.set t i (e + a)
                    Array.set t (i + n2) (e - a)

                struct (f, t)
            else
                struct (t, f)

    open Details

    /// <summary>
    /// A direct DFT (O(N^2)) for reference or testing.
    /// 'ftd' determines sign => forward = -1 => e^(-i...), backward = +1 => e^(+i...).
    /// We then do a 1/sqrt(N) normalization for a "unitary" transform.
    /// </summary>
    let dft (ftd : FourierTransformDirection) (vs : Complex[]) =
        let l = vs.Length
        let am = tau / float l
        Array.init l (fun i ->
            let mutable s = Complex.Zero
            for j = 0 to l - 1 do
                let angle = ftd.sign * float i * float j * am
                // e^(i * angle) => if sign=-1 => e^(-i...), if +1 => e^(+i...)
                let tw = twiddle angle
                s <- s + vs.[j] * tw
            s)
        |> normalize

    /// <summary>
    /// A Cooley-Tukey FFT using precomputed negative exponents, flipping via conjugate if needed.
    /// Then we apply a 1/sqrt(N) normalization for a "unitary" transform.
    /// </summary>
    let fft ftd (vs : Complex[]) =
        let n = vs.Length
        let ln = ilog2 n
        let vs0 = Array.copy vs
        let vs1 = Array.zeroCreate n

        let struct (_, t) = loop ftd (n >>> 1) ln 1 n vs0 vs1
        normalize t
