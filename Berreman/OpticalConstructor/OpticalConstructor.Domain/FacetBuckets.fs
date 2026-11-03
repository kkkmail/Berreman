namespace OpticalConstructor.Domain

open System
open OpticalConstructor.Domain.Units
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

    /// A rung of the 1–2–5 ladder by its position n = 3k + i on it: the rung `m × 10^k` with `m` the
    /// mantissa at index i (one position per rung, so stepping is ±1 — spec 0048 E018).
    type private LadderRung =
        | LadderRung of int

        member this.magnitude : UnitMagnitude =
            let (LadderRung n) = this
            let exponent = if n >= 0 then n / 3 else -((2 - n) / 3)
            UnitMagnitude (mantissas[n - 3 * exponent] * Math.Pow(10.0, float exponent))

        member this.up : LadderRung = let (LadderRung n) = this in LadderRung (n + 1)

        member this.down : LadderRung = let (LadderRung n) = this in LadderRung (n - 1)

    type UnitMagnitude with

        /// A length label's number, the magnitude read in NANOMETERS: the §7.5
        /// unit switch shows nm below 1 µm and µm at or above.
        member this.lengthNumber : string =
            if this.value < 1000.0 then $"%g{this.value}" else $"%g{this.value / 1000.0}"

        /// A length label's unit symbol (reused from `UnitOfMeasure.abbreviation`,
        /// the sole home of unit names) under the same switch.
        member this.lengthUnit : string =
            if this.value < 1000.0 then Nanometer.abbreviation else Micrometer.abbreviation

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

        /// Half-open membership, `NumericRange`'s own: `lower <= v < upper`,
        /// with the degenerate `lower = upper` bucket containing exactly its value.
        member this.contains (v : double) : bool = this.range.contains v

        /// The display label, magnitudes read as lengths in nm: range buckets
        /// as `10-20 nm (7)`, exact-value buckets as `42 nm (3)`; nm below
        /// 1 µm, µm at or above, and an interval straddling 1 µm names each
        /// endpoint's own unit (`500 nm-1 µm (2)`).
        member this.label : string =
            let count = this.count.value
            let lower = this.range.lower
            let upper = this.range.upper

            if lower.value = upper.value then
                $"{lower.lengthNumber} {lower.lengthUnit} ({count})"
            elif lower.lengthUnit = upper.lengthUnit then
                $"{lower.lengthNumber}-{upper.lengthNumber} {lower.lengthUnit} ({count})"
            else
                $"{lower.lengthNumber} {lower.lengthUnit}-{upper.lengthNumber} {upper.lengthUnit} ({count})"

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
    let ladderBoundaries (minValue : UnitMagnitude) (maxValue : UnitMagnitude) : UnitMagnitude list =
        let lowest = minValue.value
        let highest = maxValue.value

        if not (Double.IsFinite lowest) || not (Double.IsFinite highest)
           || lowest <= 0.0 || lowest > highest then []
        else
            let start =
                // The 5 × 10^k rung of the decade holding min.
                let guess = LadderRung (3 * int (Math.Floor(Math.Log10 lowest)) + 2)
                let rec down (rung : LadderRung) = if rung.magnitude.value > lowest then down rung.down else rung
                let rec up (rung : LadderRung) = if rung.up.magnitude.value <= lowest then up rung.up else rung
                guess |> down |> up

            let rec collect (rung : LadderRung) (acc : UnitMagnitude list) =
                let v = rung.magnitude
                if v.value > highest then List.rev (v :: acc)
                else collect rung.up (v :: acc)

            collect start []

    /// Fewest-items-first merging: while the bucket count exceeds the cap,
    /// the adjacent PAIR with the fewest combined items merges (the leftmost
    /// pair on ties), then the scan repeats over the shortened list. A merge's
    /// union interval spans both buckets — absorbing any dropped-empty gap
    /// between them, which holds no population magnitude by construction —
    /// and the counts add.
    let rec private mergeToCap (cap : WorkbenchSettings.ThicknessBucketCap) (buckets : NumericBucket list) : NumericBucket list =
        if List.length buckets <= cap.value then buckets
        else
            let fewest =
                buckets
                |> List.pairwise
                |> List.map (fun (first, second) -> first.count.value + second.count.value)
                |> List.min

            let rec mergeLeftmost (remaining : NumericBucket list) : NumericBucket list =
                match remaining with
                | first :: second :: rest when first.count.value + second.count.value = fewest ->
                    {
                        range = { lower = first.range.lower; upper = second.range.upper }
                        count = ItemCount (first.count.value + second.count.value)
                    } :: rest
                | head :: tail -> head :: mergeLeftmost tail
                | [] -> []

            mergeToCap cap (mergeLeftmost buckets)

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
    let buildBuckets (cap : WorkbenchSettings.ThicknessBucketCap) (magnitudes : UnitMagnitude list) : NumericBucket list =
        let population = magnitudes |> List.filter (fun v -> Double.IsFinite v.value && v.value > 0.0)

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
                    count = ItemCount (population |> List.filter (fun v -> lo.value <= v.value && v.value < hi.value) |> List.length)
                })
            |> List.filter (fun bucket -> bucket.count.value > 0)
            |> mergeToCap cap

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
            |> attribute.applicableItems
            |> List.collect (fun item ->
                attribute.extract item
                |> List.distinct
                |> List.choose (fun value ->
                    match value with
                    | NumericValue v -> Some v
                    | DiscreteValue _ -> None))

        buildBuckets cap magnitudes
