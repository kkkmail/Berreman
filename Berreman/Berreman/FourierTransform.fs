﻿namespace Berreman
open System
open System.Numerics

/// https://fssnip.net/dC/title/fast-Fourier-transforms-FFT-
module SimpleFourierTransform =

  let pi              = Math.PI
  let tau             = 2.*pi

  let twiddle a = Complex.FromPolarCoordinates(1., a)

  let rec fft = function
    | []  -> []
    | [x] -> [x]
    | x ->
      x
      |> List.mapi (fun i c -> i % 2 = 0, c)
      |> List.partition fst
      |> fun (even, odd) -> fft (List.map snd even), fft (List.map snd odd)
      ||> List.mapi2 (fun i even odd ->
          let btf = odd * twiddle (-tau * (float i / float x.Length))
          even + btf, even - btf)
      |> List.unzip
      ||> List.append


/// Source: https://fssnip.net/7Tn/title/Optimized-Fast-Fourier-Transform-FFT
/// For examples and tests see: https://gist.github.com/mrange/da57f972b3dfdfb44f28fd340841586c
///  Inspired by: http://fssnip.net/dC/title/fast-Fourier-transforms-FFT-
///  and: https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm
///  Sacrifices idioms for performance
module FourierTransform =

  let maxSize       = 4096
  let pi            = Math.PI
  let tau           = 2.*pi


  module Details =

    let isPowerOf2 n  = (n &&& n - 1) = 0


    let ilog2 n       =
      if n < 2              then failwith "n must be greater than 1"
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
    let twiddle a     = Complex.FromPolarCoordinates(1., a)


    let twiddles      =
      let unfolder c =
        if c < 2*maxSize then
          let vs = Array.init (c / 2) (fun i -> twiddle (-tau * float i / float c))
          Some (vs, c*2)
        else
          None
      Array.unfold unfolder 1


    let rec loop n2 ln s c f t =
      if c > 2 then
        let c2 = c >>> 1
        let struct (t, f) = loop n2 (ln - 1) (s <<< 1) c2 f t

        let twiddles = twiddles.[ln]

        if s > 1 then
          for j = 0 to c2 - 1 do
            let w   = twiddles.[j]
            let off = s*j
            let off2= off <<< 1;
            for i = 0 to s - 1 do
              let e = Array.get f (i + off2 + 0)
              let o = Array.get f (i + off2 + s)
              let a = w*o
              Array.set t (i + off + 0)   (e + a)
              Array.set t (i + off + n2)  (e - a)
        else
          for j = 0 to c2 - 1 do
            let w = twiddles.[j]
            let e = Array.get f (2*j + 0)
            let o = Array.get f (2*j + s)
            let a = w*o
            Array.set t (j + 0)   (e + a)
            Array.set t (j + n2)  (e - a)

        struct (f, t)
      elif c = 2 then
        for i = 0 to s - 1 do
          let e = Array.get f (i + 0)
          let o = Array.get f (i + s)
          let a = o
          Array.set t (i + 0)   (e + a)
          Array.set t (i + n2)  (e - a)

        struct (f, t)
      else
        struct (t, f)


  open Details

  let dft (vs : Complex []) =
    let l   = vs.Length
    let am  = tau / float l
    let rec loop s j i =
      if j < l then
        let v = vs.[j]
        let n = v*twiddle (-float i * float j * am)
        loop (s + n) (j + 1) i
      else
        s
    Array.init l (loop Complex.Zero 0)


  let fft (vs : Complex []) : Complex [] =
    let n   = vs.Length
    let ln  = ilog2 n

    let vs0 = Array.copy vs
    let vs1 = Array.zeroCreate n

    let struct (_, t) = Details.loop (n >>> 1) ln 1 n vs0 vs1

    t
