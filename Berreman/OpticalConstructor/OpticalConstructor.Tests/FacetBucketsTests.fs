namespace OpticalConstructor.Tests

open Xunit
open FsCheck
open FsCheck.FSharp
open OpticalConstructor.Domain.Facets
open OpticalConstructor.Domain.FacetBuckets
open OpticalConstructor.Domain.WorkbenchSettings

/// Spec 0038 Part D (step 010) — the 1–2–5 log-ladder numeric bucket builder:
/// ladder boundary generation, empty-bucket dropping, fewest-first merging
/// under `ThicknessBucketCap`, half-open `[lo, hi)` membership, the
/// single-distinct-value exact-bucket collapse, nm/µm labels, recomputation
/// over the CURRENTLY CONSTRAINED population, the bucket-as-ordinary-chip
/// path, and the FsCheck property that every magnitude lands in exactly one
/// bucket. Pure construction throughout: no window, no IO, no solver.
module FacetBucketsTests =

    let private capOf (raw : int) : ThicknessBucketCap =
        match ThicknessBucketCap.tryCreate raw with
        | Ok cap -> cap
        | Error e -> failwith $"test cap {raw} must be valid, got %A{e}"

    let private bucket (lo : double) (hi : double) (count : int) : NumericBucket =
        { range = { lower = lo; upper = hi }; count = ItemCount count }

    // ---- ladder boundary generation ----

    [<Fact>]
    let ``ladder boundaries step 1-2-5 per decade from the largest rung at or below min`` () =
        Assert.Equal<double list>([ 10.0; 20.0; 50.0; 100.0; 200.0; 500.0 ], ladderBoundaries 10.0 450.0)

    [<Fact>]
    let ``a min between rungs starts at the rung below it`` () =
        Assert.Equal<double list>([ 20.0; 50.0; 100.0; 200.0; 500.0 ], ladderBoundaries 30.0 400.0)

    [<Fact>]
    let ``a max exactly on a rung extends one rung above so the half-open top admits it`` () =
        Assert.Equal<double list>([ 10.0; 20.0; 50.0; 100.0; 200.0; 500.0; 1000.0 ], ladderBoundaries 10.0 500.0)

    [<Fact>]
    let ``an inverted or non-positive span yields no boundaries`` () =
        Assert.Equal<double list>([], ladderBoundaries 20.0 10.0)
        Assert.Equal<double list>([], ladderBoundaries 0.0 10.0)
        Assert.Equal<double list>([], ladderBoundaries (-5.0) 10.0)

    // ---- buildBuckets: empties dropped, half-open membership ----

    [<Fact>]
    let ``empty buckets are dropped: only intervals holding population magnitudes survive`` () =
        let buckets = buildBuckets ThicknessBucketCap.defaultValue [ 12.0; 15.0; 300.0 ]
        Assert.Equal<NumericBucket list>([ bucket 10.0 20.0 2; bucket 200.0 500.0 1 ], buckets)

    [<Fact>]
    let ``bucket membership is half-open: the lower edge is in, the upper edge falls in the next bucket`` () =
        let buckets = buildBuckets ThicknessBucketCap.defaultValue [ 10.0; 20.0; 20.0 ]
        Assert.Equal<NumericBucket list>([ bucket 10.0 20.0 1; bucket 20.0 50.0 2 ], buckets)

    [<Fact>]
    let ``contains mirrors the half-open rule and the exact-value degenerate`` () =
        let ranged = bucket 10.0 20.0 0
        Assert.True(ranged.contains 10.0)
        Assert.True(ranged.contains 19.999)
        Assert.False(ranged.contains 20.0)
        Assert.False(ranged.contains 9.999)
        let exact = bucket 42.0 42.0 0
        Assert.True(exact.contains 42.0)
        Assert.False(exact.contains 42.001)

    // ---- fewest-first merging under the cap ----

    [<Fact>]
    let ``the adjacent pair with the fewest combined items merges first`` () =
        // Non-empty buckets carry counts 5 / 1 / 2 / 7; cap 3 forces ONE
        // merge. Pair sums: 5+1=6, 1+2=3, 2+7=9 — the middle pair merges.
        let values = (List.replicate 5 10.0) @ [ 20.0 ] @ (List.replicate 2 50.0) @ (List.replicate 7 100.0)
        let buckets = buildBuckets (capOf 3) values
        Assert.Equal<NumericBucket list>([ bucket 10.0 20.0 5; bucket 20.0 100.0 3; bucket 100.0 200.0 7 ], buckets)

    [<Fact>]
    let ``merging cascades fewest-first and breaks ties leftmost`` () =
        // Counts 1 / 1 / 1 / 1, cap 2: the leftmost tied pair merges first
        // (1+1 at the head), then the remaining 1+1 pair — never a 2+1 merge.
        let buckets = buildBuckets (capOf 2) [ 10.0; 20.0; 50.0; 100.0 ]
        Assert.Equal<NumericBucket list>([ bucket 10.0 50.0 2; bucket 50.0 200.0 2 ], buckets)

    [<Fact>]
    let ``a merge bridges a dropped-empty gap: the union interval spans it`` () =
        // Non-empty buckets [10,20):3, [200,500):1, [500,1000):5 — everything
        // between was dropped empty. Cap 2 merges the fewest pair (3+1=4
        // beats 1+5=6), and the union interval spans the empty decades.
        let values = (List.replicate 3 12.0) @ [ 300.0 ] @ (List.replicate 5 700.0)
        let buckets = buildBuckets (capOf 2) values
        Assert.Equal<NumericBucket list>([ bucket 10.0 500.0 4; bucket 500.0 1000.0 5 ], buckets)

    [<Fact>]
    let ``a population already within the cap merges nothing`` () =
        let buckets = buildBuckets ThicknessBucketCap.defaultValue [ 10.0; 30.0; 70.0 ]
        Assert.Equal<NumericBucket list>([ bucket 10.0 20.0 1; bucket 20.0 50.0 1; bucket 50.0 100.0 1 ], buckets)

    // ---- single-distinct-value collapse ----

    [<Fact>]
    let ``a single distinct magnitude collapses to one exact-value bucket carrying the population count`` () =
        let buckets = buildBuckets ThicknessBucketCap.defaultValue [ 42.0; 42.0; 42.0 ]
        Assert.Equal<NumericBucket list>([ bucket 42.0 42.0 3 ], buckets)

    [<Fact>]
    let ``an empty population yields no buckets`` () =
        Assert.Equal<NumericBucket list>([], buildBuckets ThicknessBucketCap.defaultValue [])

    // ---- labels: nm below 1 µm, µm at or above ----

    [<Fact>]
    let ``a range bucket below 1 µm labels in nm`` () =
        Assert.Equal("10-20 nm (7)", (bucket 10.0 20.0 7).label)

    [<Fact>]
    let ``a range bucket at or above 1 µm labels in µm`` () =
        Assert.Equal("2-5 µm (3)", (bucket 2000.0 5000.0 3).label)

    [<Fact>]
    let ``a bucket straddling 1 µm names each endpoint's own unit`` () =
        Assert.Equal("500 nm-1 µm (2)", (bucket 500.0 1000.0 2).label)

    [<Fact>]
    let ``an exact-value bucket labels the single value`` () =
        Assert.Equal("42 nm (3)", (bucket 42.0 42.0 3).label)
        Assert.Equal("1.5 µm (1)", (bucket 1500.0 1500.0 1).label)

    // ---- recomputation over the constrained population; ordinary-chip path ----

    /// The corpus of the constrained-population tests: a thickness magnitude
    /// (nm) per slab plus a discrete family facet to narrow with.
    type private Slab =
        {
            slabName : string
            family : string
            thicknessNm : double
        }

    let private thin = { slabName = "thin"; family = "glass"; thicknessNm = 10.0 }
    let private thin2 = { slabName = "thin2"; family = "glass"; thicknessNm = 15.0 }
    let private mid = { slabName = "mid"; family = "metal"; thicknessNm = 200.0 }
    let private thick = { slabName = "thick"; family = "metal"; thicknessNm = 3000.0 }
    let private slabs : Slab list = [ thin; thin2; mid; thick ]

    let private familyKey = AttributeKey "family"
    let private thicknessKey = AttributeKey "thickness"

    let private familyDef : AttributeDef<Slab> =
        {
            key = familyKey
            name = "Family"
            kind = DiscreteAttribute
            appliesTo = fun _ -> ApplicableAttribute
            extract = fun s -> [ DiscreteValue (DiscreteKey s.family) ]
        }

    let private thicknessDef : AttributeDef<Slab> =
        {
            key = thicknessKey
            name = "Thickness"
            kind = NumericAttribute
            appliesTo = fun _ -> ApplicableAttribute
            extract = fun s -> [ NumericValue s.thicknessNm ]
        }

    let private slabDefs : AttributeDef<Slab> list = [ familyDef; thicknessDef ]

    [<Fact>]
    let ``buckets recompute from the narrowed population's min and max on constraint change`` () =
        let unconstrained = bucketsFor ThicknessBucketCap.defaultValue thicknessDef slabDefs [] slabs
        Assert.Equal<NumericBucket list>(
            [ bucket 10.0 20.0 2; bucket 200.0 500.0 1; bucket 2000.0 5000.0 1 ],
            unconstrained)

        let glassOnly = { key = familyKey; selection = DiscreteSelection (Set.singleton (DiscreteKey "glass")) }
        let narrowed = bucketsFor ThicknessBucketCap.defaultValue thicknessDef slabDefs [ glassOnly ] slabs
        // glass = 10 / 15 nm only: the ladder re-derives from THIS min/max —
        // one [10,20) bucket, not the 10–5000 span of the unconstrained set.
        Assert.Equal<NumericBucket list>([ bucket 10.0 20.0 2 ], narrowed)

    [<Fact>]
    let ``a bucket applies as an ordinary numeric-range chip reproducing exactly its count`` () =
        let buckets = bucketsFor ThicknessBucketCap.defaultValue thicknessDef slabDefs [] slabs
        let first =
            match buckets with
            | b :: _ -> b
            | [] -> failwith "expected buckets over the slab corpus"
        let chip = constraintFor thicknessKey first
        Assert.Equal(NumericRangeSelection first.range, chip.selection)
        Assert.Equal(first.count.value, filter slabDefs [ chip ] slabs |> List.length)

    [<Fact>]
    let ``a manual min-max entry is the same ordinary chip shape, never a special path`` () =
        // A hand-typed 12–250 nm range runs through the engine exactly like a
        // bucket chip: an ordinary NumericRangeSelection, half-open.
        let manual = { key = thicknessKey; selection = NumericRangeSelection { lower = 12.0; upper = 250.0 } }
        Assert.Equal<string list>([ "thin2"; "mid" ], filter slabDefs [ manual ] slabs |> List.map (fun s -> s.slabName))

    [<Fact>]
    let ``an item extracting the same magnitude twice counts once in its bucket`` () =
        let doubledDef : AttributeDef<Slab> =
            {
                key = thicknessKey
                name = "Thickness"
                kind = NumericAttribute
                appliesTo = fun _ -> ApplicableAttribute
                extract = fun s -> [ NumericValue s.thicknessNm; NumericValue s.thicknessNm ]
            }
        let buckets = bucketsFor ThicknessBucketCap.defaultValue doubledDef [ doubledDef ] [] [ thin; thin2 ]
        // 10 and 15 nm both land in [10,20): two ITEMS, not four extractions.
        Assert.Equal<NumericBucket list>([ bucket 10.0 20.0 2 ], buckets)

    // ---- the FsCheck property ----

    [<Fact>]
    let ``every magnitude lands in exactly one bucket and the cap is respected`` () =
        let inputs =
            gen {
                let! capRaw = Gen.choose (1, 12)
                let! size = Gen.choose (1, 60)
                let! raws = Gen.listOfLength size (Gen.choose (1, 5_000_000))
                return capRaw, raws |> List.map (fun r -> double r / 100.0)
            }

        let landsInExactlyOneBucket (capRaw : int, magnitudes : double list) : bool =
            let buckets = buildBuckets (capOf capRaw) magnitudes
            List.length buckets <= capRaw
            && magnitudes
               |> List.forall (fun v -> (buckets |> List.filter (fun b -> b.contains v) |> List.length) = 1)

        Check.One(Config.QuickThrowOnFailure.WithMaxTest(500), Prop.forAll (Arb.fromGen inputs) landsInExactlyOneBucket)
