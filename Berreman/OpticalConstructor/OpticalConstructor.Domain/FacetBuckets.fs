namespace OpticalConstructor.Domain

open System
open OpticalConstructor.Domain.Facets

/// Spec 0038 Part D (step 010): the numeric bucket builder every
/// `NumericAttribute` branch uses — pure, Avalonia-free, on top of step 009's
/// engine. Bucket boundaries come from the 1–2–5 log ladder spanning the
/// CURRENTLY CONSTRAINED population's min/max; empty buckets drop; adjacent
/// buckets merge fewest-items-first while the count exceeds
/// `ThicknessBucketCap` (step 005, default 8); intervals are half-open
/// `[lo, hi)`; a single distinct value collapses to one exact-value bucket.
/// Buckets are recomputed on every constraint change (the builder is pure —
/// callers simply re-derive), and a bucket applies as an ORDINARY
/// `NumericRangeSelection` constraint chip, exactly like a manual min–max
/// entry — no special path exists.
module FacetBuckets =

    /// The 1–2–5 rung mantissas of one decade, ascending.
    let private mantissas = [| 1.0; 2.0; 5.0 |]

    /// The ladder rung at (decade exponent, mantissa index): `m × 10^k`.
    let private valueAt (exponent : int, index : int) : double =
        mantissas[index] * Math.Pow(10.0, float exponent)

    let private stepUp (exponent : int, index : int) : int * int =
        if index = 2 then (exponent + 1, 0) else (exponent, index + 1)

    let private stepDown (exponent : int, index : int) : int * int =
        if index = 0 then (exponent - 1, 2) else (exponent, index - 1)

    /// One endpoint of a length label. Magnitudes are lengths in NANOMETERS;
    /// the §7.5 unit switch shows nm below 1 µm and µm at or above, the unit
    /// symbols reused from `Units.unitAbbrev` (the sole home of unit names).
    let private lengthPart (v : double) : string * string =
        if v < 1000.0 then $"%g{v}", Units.unitAbbrev Units.Nanometer
        else $"%g{v / 1000.0}", Units.unitAbbrev Units.Micrometer

    /// One display bucket of a numeric facet: a half-open `[lower, upper)`
    /// interval carrying the count of population magnitudes inside it, with
    /// the degenerate `lower = upper` pair meaning the exact value (the
    /// single-distinct-value collapse). The interval reuses step 009's
    /// `NumericRange`, so a bucket IS an ordinary constraint payload.
    type NumericBucket =
        {
            range : NumericRange
            count : ItemCount
        }

        /// Half-open membership mirroring `NumericRange`'s documented
        /// contract: `lower <= v < upper`, with the degenerate
        /// `lower = upper` bucket containing exactly its value.
        member this.contains (v : double) : bool =
            if this.range.lower = this.range.upper then v = this.range.lower
            else this.range.lower <= v && v < this.range.upper

        /// The display label, magnitudes read as lengths in nm: range buckets
        /// as `10-20 nm (7)`, exact-value buckets as `42 nm (3)`; nm below
        /// 1 µm, µm at or above, and an interval straddling 1 µm names each
        /// endpoint's own unit (`500 nm-1 µm (2)`).
        member this.label : string =
            let count = this.count.value

            if this.range.lower = this.range.upper then
                let number, suffix = lengthPart this.range.lower
                $"{number} {suffix} ({count})"
            else
                let lowerNumber, lowerSuffix = lengthPart this.range.lower
                let upperNumber, upperSuffix = lengthPart this.range.upper

                if lowerSuffix = upperSuffix then $"{lowerNumber}-{upperNumber} {lowerSuffix} ({count})"
                else $"{lowerNumber} {lowerSuffix}-{upperNumber} {upperSuffix} ({count})"

    /// The 1–2–5 log-ladder boundaries spanning `[minValue, maxValue]`:
    /// strictly increasing rungs `m × 10^k` (`m ∈ {1, 2, 5}`) from the
    /// largest rung at or below min to the smallest rung STRICTLY above max —
    /// so every population value lands in exactly one half-open pairwise
    /// interval, and a max sitting exactly on a rung extends one rung above
    /// (the top interval's upper bound is exclusive). `Math.Log10` is only
    /// the initial guess; the step-down/step-up correction makes the start
    /// exact under floating rounding. The ladder spans positive finite reals
    /// only: a non-positive, non-finite, or inverted span yields `[]`
    /// (total, no throw).
    let ladderBoundaries (minValue : double) (maxValue : double) : double list =
        if not (Double.IsFinite minValue) || not (Double.IsFinite maxValue)
           || minValue <= 0.0 || minValue > maxValue then []
        else
            let start =
                let guess = (int (Math.Floor(Math.Log10 minValue)), 2)
                let rec down pos = if valueAt pos > minValue then down (stepDown pos) else pos
                let rec up pos = if valueAt (stepUp pos) <= minValue then up (stepUp pos) else pos
                guess |> down |> up

            let rec collect pos acc =
                let v = valueAt pos
                if v > maxValue then List.rev (v :: acc)
                else collect (stepUp pos) (v :: acc)

            collect start []

    /// The merge of the adjacent pair at `index` (with the bucket after it):
    /// the union interval spans both — absorbing any dropped-empty gap
    /// between them, which holds no population magnitude by construction —
    /// and the counts add.
    let rec private mergePairAt (index : int) (buckets : NumericBucket list) : NumericBucket list =
        match index, buckets with
        | 0, first :: second :: rest ->
            {
                range = { lower = first.range.lower; upper = second.range.upper }
                count = ItemCount (first.count.value + second.count.value)
            } :: rest
        | _, [] -> []
        | remaining, head :: tail -> head :: mergePairAt (remaining - 1) tail

    /// Fewest-items-first merging: while the bucket count exceeds the cap,
    /// the adjacent PAIR with the fewest combined items merges (the leftmost
    /// pair on ties — `List.minBy` keeps the first minimum), then the scan
    /// repeats over the shortened list.
    let rec private mergeToCap (cap : int) (buckets : NumericBucket list) : NumericBucket list =
        if List.length buckets <= cap then buckets
        else
            let pairIndex =
                buckets
                |> List.pairwise
                |> List.mapi (fun index (first, second) -> index, first.count.value + second.count.value)
                |> List.minBy snd
                |> fst

            mergeToCap cap (mergePairAt pairIndex buckets)

    /// The bucket set of one population of magnitudes — the CURRENTLY
    /// CONSTRAINED population; callers re-derive on every constraint change:
    /// 1–2–5 ladder boundaries spanning the population's min/max, half-open
    /// `[lo, hi)` pairwise intervals, empty buckets dropped, then adjacent
    /// buckets merged fewest-items-first while the count exceeds the cap.
    /// A single distinct magnitude collapses to one exact-value bucket
    /// carrying the whole population count. Magnitudes are positive lengths
    /// in nm by contract (the elevated upstream types already forbid
    /// anything else); non-positive or non-finite values lie outside the log
    /// ladder's domain and are dropped defensively.
    let buildBuckets (cap : WorkbenchSettings.ThicknessBucketCap) (magnitudes : double list) : NumericBucket list =
        let population = magnitudes |> List.filter (fun v -> Double.IsFinite v && v > 0.0)

        match population |> List.distinct with
        | [] -> []
        | [ single ] ->
            [
                {
                    range = { lower = single; upper = single }
                    count = ItemCount (List.length population)
                }
            ]
        | _ ->
            ladderBoundaries (List.min population) (List.max population)
            |> List.pairwise
            |> List.map (fun (lo, hi) ->
                {
                    range = { lower = lo; upper = hi }
                    count = ItemCount (population |> List.filter (fun v -> lo <= v && v < hi) |> List.length)
                })
            |> List.filter (fun bucket -> bucket.count.value > 0)
            |> mergeToCap cap.value

    /// A bucket applied as an ORDINARY constraint chip: the bucket's
    /// half-open range as a `NumericRangeSelection` under the facet's key —
    /// the very path a manual min–max entry takes (there is no special
    /// bucket path), so applying a bucket reproduces exactly its displayed
    /// count through the ordinary engine `filter`.
    let constraintFor (key : AttributeKey) (bucket : NumericBucket) : AppliedConstraint =
        {
            key = key
            selection = NumericRangeSelection bucket.range
        }

    /// The bucket set of one numeric attribute over the CURRENTLY CONSTRAINED
    /// population: `filter` under the applied set, then the attribute's
    /// numeric magnitudes — applicability-gated, deduped per item (step 009's
    /// at-most-once-per-branch discipline), discrete values ignored — feed
    /// `buildBuckets`. Pure: a constraint change recomputes by re-calling,
    /// so the ladder always spans the constrained population's min/max.
    let bucketsFor
        (cap : WorkbenchSettings.ThicknessBucketCap)
        (attribute : AttributeDef<'Item>)
        (defs : AttributeDef<'Item> list)
        (applied : AppliedConstraint list)
        (items : 'Item list)
        : NumericBucket list =
        let magnitudes =
            filter defs applied items
            |> List.filter (fun item ->
                match attribute.appliesTo item with
                | ApplicableAttribute -> true
                | InapplicableAttribute -> false)
            |> List.collect (fun item ->
                attribute.extract item
                |> List.distinct
                |> List.choose (fun value ->
                    match value with
                    | NumericValue v -> Some v
                    | DiscreteValue _ -> None))

        buildBuckets cap magnitudes
