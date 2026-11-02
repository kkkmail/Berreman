namespace OpticalConstructor.Domain

open System
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.MaterialLibrary

/// Spec 0038 Part D (step 009): the generic faceted-navigation engine — pure,
/// Avalonia-free, generic over the item type. Items + attribute definitions →
/// a filterable, reconfigurable tree with live counts. The engine is naive
/// folds throughout (corpus sizes are tens to hundreds — no indices, no
/// caching); the numeric bucket builder is step 010 and the concrete
/// material/library facet catalogues are step 011, both on top of this module.
module Facets =

    /// The identity of one attribute (facet) — e.g. "category", "anisotropy".
    /// A single-case string DU so a key can never be confused with a display
    /// name or a discrete value; `.value` is reached only at the IO boundary.
    type AttributeKey =
        | AttributeKey of string

        member this.value = let (AttributeKey k) = this in k

        /// The key read back from its text (a control's token); `None` for a blank text.
        static member tryCreate (text : string) : AttributeKey option =
            if String.IsNullOrWhiteSpace text then None else Some (AttributeKey text)

    /// The identity of one discrete value inside a facet — e.g. "uniaxial".
    /// At this layer the key doubles as the branch label; step 011's
    /// catalogues choose human-readable keys.
    type DiscreteKey =
        | DiscreteKey of string

        member this.value = let (DiscreteKey k) = this in k

        /// The key read back from its text (a control's token); `None` for a blank text.
        static member tryCreate (text : string) : DiscreteKey option =
            if String.IsNullOrWhiteSpace text then None else Some (DiscreteKey text)

    /// A plain-text filter query (the filter-box content). Matching is
    /// ordinal case-insensitive substring containment; the empty query
    /// matches every item (an empty filter box filters nothing).
    type TextQuery =
        | TextQuery of string

        member this.value = let (TextQuery q) = this in q

    /// The display text of a facet (its name) or of one of its branches (its label).
    type FacetCaption =
        | FacetCaption of string

        member this.value = let (FacetCaption c) = this in c

    /// One value an item carries for one attribute. Discrete values are keys
    /// into the facet's vocabulary; numeric values are raw magnitudes in the
    /// attribute's canonical unit (step 010 buckets them for display).
    type AttributeValue =
        | DiscreteValue of DiscreteKey
        | NumericValue of UnitMagnitude

        /// The branch display label of this value: a discrete key shows its
        /// key string; a raw numeric value shows its compact form (step 010's
        /// buckets carry their own range labels instead).
        member this.label =
            match this with
            | DiscreteValue k -> k.value
            | NumericValue v -> $"%g{v.value}"

    /// Whether an attribute offers itself for an item at all. Dependent
    /// facets vanish ENTIRELY when inapplicable (never an empty or greyed
    /// branch) — a named two-case DU, not a naked bool, so match sites read
    /// as prose.
    type Applicability =
        | ApplicableAttribute
        | InapplicableAttribute

        /// Elevate a raw "applies" condition at its computation site (a catalogue's `appliesTo`).
        static member ofBool (applies : bool) : Applicability =
            match applies with
            | true -> ApplicableAttribute
            | false -> InapplicableAttribute

    /// The two facet shapes: a discrete vocabulary (branch per key) or a
    /// numeric magnitude (branch per value now; per bucket from step 010).
    type AttributeKind =
        | DiscreteAttribute
        | NumericAttribute

    /// One attribute definition over the item type. `extract` returns a LIST
    /// — multi-valued facets are first-class (e.g. the dispersion-model facet
    /// is per axis/segment), so branch counts need not sum to the total.
    /// Extraction is meaningful only where `appliesTo` says the attribute
    /// applies; the engine never consults `extract` on an inapplicable item.
    type AttributeDef<'Item> =
        {
            key : AttributeKey
            name : FacetCaption
            kind : AttributeKind
            appliesTo : 'Item -> Applicability
            extract : 'Item -> AttributeValue list
        }

        /// The items this attribute applies to, in their order.
        member this.applicableItems (items : 'Item list) : 'Item list =
            items
            |> List.filter (fun item ->
                match this.appliesTo item with
                | ApplicableAttribute -> true
                | InapplicableAttribute -> false)

    /// The lower/upper pair of a numeric selection. Membership is half-open
    /// `[lower, upper)` — the step-010 bucket intervals, so a bucket applied
    /// as an ordinary constraint chip reproduces exactly its count — with the
    /// degenerate `lower = upper` pair meaning the exact value (the shape of
    /// step 010's single-distinct-value bucket collapse). An inverted pair
    /// (`lower > upper`) selects nothing; no `tryCreate` is needed because
    /// every pair has a total meaning.
    type NumericRange =
        {
            lower : UnitMagnitude
            upper : UnitMagnitude
        }

        /// Half-open `[lower, upper)` membership, with the degenerate
        /// `lower = upper` pair meaning the exact value.
        member this.contains (v : float) : bool =
            if this.lower.value = this.upper.value then v = this.lower.value
            else this.lower.value <= v && v < this.upper.value

    /// One facet's applied selection: a discrete key SET (OR within the
    /// facet — any selected key matches) or a numeric range. A manual
    /// min–max entry applies as an ordinary `NumericRangeSelection`, never a
    /// special path.
    type FacetSelection =
        | DiscreteSelection of Set<DiscreteKey>
        | NumericRangeSelection of NumericRange

    /// One applied constraint: a facet key plus its selection. The applied
    /// set ANDs across facets; order is the application (breadcrumb) order.
    type AppliedConstraint =
        {
            key : AttributeKey
            selection : FacetSelection
        }

    /// The tree-shaping choice: which attributes appear as facets, in which
    /// order. Search order ≠ representation order — applied constraints never
    /// reshuffle the tree.
    type Representation =
        | Representation of AttributeKey list

        member this.value = let (Representation keys) = this in keys

    /// An item count carried by branches, count-previews, and breadcrumb
    /// after-counts; `.value` is reached only at the display boundary.
    type ItemCount =
        | ItemCount of int

        member this.value = let (ItemCount c) = this in c

    /// One offered value under a facet: the value, its display label, and
    /// the count of filtered items carrying it (an item counts at most once
    /// per branch, once in EACH branch it carries a value of).
    type FacetBranch =
        {
            value : AttributeValue
            label : FacetCaption
            count : ItemCount
        }

    /// One facet node of the built tree: the attribute and its non-empty,
    /// deterministically ordered branches.
    type FacetNode =
        {
            key : AttributeKey
            name : FacetCaption
            branches : FacetBranch list
        }

    /// The built tree: facet nodes in representation order. Zero-count
    /// branches and attributes inapplicable to every filtered item never
    /// appear.
    type FacetTree =
        {
            facets : FacetNode list
        }

    /// One breadcrumb chip: an applied constraint paired with the cumulative
    /// result count after applying it and everything before it.
    type BreadcrumbCount =
        {
            applied : AppliedConstraint
            afterCount : ItemCount
        }

    /// The items satisfying one applied constraint, in their order: the
    /// constrained attribute must have a def, must apply to the item, and ANY
    /// extracted value must match the selection (OR within the facet). A
    /// cross-kind pair (a numeric value against a discrete selection and vice
    /// versa) never matches — a mis-kinded constraint selects nothing rather
    /// than throwing. Fail-closed on an unknown key — a constraint no def
    /// explains matches NOTHING, so a broken chip surfaces as zero results,
    /// never as a silently ignored filter.
    let private surviving (defs : AttributeDef<'Item> list) (applied : AppliedConstraint) (items : 'Item list) : 'Item list =
        match defs |> List.tryFind (fun d -> d.key = applied.key) with
        | None -> []
        | Some def ->
            let matches (value : AttributeValue) : bool =
                match applied.selection, value with
                | DiscreteSelection keys, DiscreteValue k -> keys |> Set.contains k
                | NumericRangeSelection range, NumericValue v -> range.contains v.value
                | DiscreteSelection _, NumericValue _ -> false
                | NumericRangeSelection _, DiscreteValue _ -> false

            def.applicableItems items |> List.filter (fun item -> def.extract item |> List.exists matches)

    /// The result set under the applied constraints: AND across facets, OR
    /// within one (the selection set). Corpus order is preserved.
    let filter (defs : AttributeDef<'Item> list) (applied : AppliedConstraint list) (items : 'Item list) : 'Item list =
        applied |> List.fold (fun remaining c -> surviving defs c remaining) items

    /// The count-preview of ONE candidate constraint under the applied set:
    /// the result count if the candidate were applied on top of everything
    /// already applied. OR-extension previews within an already-constrained
    /// facet are the caller's to express (the candidate then carries the
    /// extended selection replacing that facet's chip).
    let countFor (defs : AttributeDef<'Item> list) (applied : AppliedConstraint list) (candidate : AppliedConstraint) (items : 'Item list) : ItemCount =
        filter defs (applied @ [ candidate ]) items |> List.length |> ItemCount

    /// The breadcrumb strip's after-counts: one entry per applied constraint
    /// in application order, each carrying the cumulative result count after
    /// applying it and everything before it (a single incremental fold — the
    /// same counts prefix-filtering would give).
    let breadcrumbCounts (defs : AttributeDef<'Item> list) (applied : AppliedConstraint list) (items : 'Item list) : BreadcrumbCount list =
        applied
        |> List.fold
            (fun (remaining : 'Item list, acc : BreadcrumbCount list) (c : AppliedConstraint) ->
                let survivors = surviving defs c remaining
                survivors, { applied = c; afterCount = ItemCount (List.length survivors) } :: acc)
            (items, [])
        |> fun (_, acc) -> List.rev acc

    /// Build the facet tree over the CURRENTLY FILTERED population: facets in
    /// representation order (never reshuffled by constraints), each branch a
    /// distinct extracted value with its display label and item count.
    /// Multi-valued items count once in each branch they carry (so branch
    /// counts need not sum to the total) and at most once per branch.
    /// Branches derive from the filtered population, so zero-count branches
    /// are absent by construction; branch order is the case-insensitive
    /// (ordinal) sort of the branch DISPLAY LABEL (operator 010/Q1), so the
    /// tree reads alphabetically regardless of key casing or corpus order. An
    /// attribute inapplicable to every filtered item is omitted entirely, as is
    /// one that extracts no values at all and a representation key with no def.
    let buildTree (representation : Representation) (defs : AttributeDef<'Item> list) (applied : AppliedConstraint list) (items : 'Item list) : FacetTree =
        let filtered = filter defs applied items

        let facetFor (key : AttributeKey) : FacetNode option =
            match defs |> List.tryFind (fun d -> d.key = key) with
            | None -> None
            | Some def ->
                let branches =
                    def.applicableItems filtered
                    |> List.collect (fun item -> def.extract item |> List.distinct)
                    |> List.groupBy id
                    |> List.map (fun (value : AttributeValue, occurrences : AttributeValue list) ->
                        {
                            value = value
                            label = FacetCaption value.label
                            count = ItemCount (List.length occurrences)
                        })
                    |> List.sortWith (fun (a : FacetBranch) (b : FacetBranch) ->
                        String.Compare(a.label.value, b.label.value, StringComparison.OrdinalIgnoreCase))

                match branches with
                | [] -> None
                | _ -> Some { key = def.key; name = def.name; branches = branches }

        { facets = representation.value |> List.choose facetFor }

    /// The sentinel value the text-filter def extracts for a matching item;
    /// `textFilterConstraint` selects exactly it.
    let textMatchKey : DiscreteKey = DiscreteKey "text-match"

    /// The plain-text filter as an ORDINARY constraint (spec 0038 §D.0): a
    /// def over a caller-supplied name extractor whose `extract` yields the
    /// match sentinel exactly when the item's name contains the query
    /// (ordinal, case-insensitive). Paired with `textFilterConstraint` it
    /// ANDs with the facet constraints, previews through `countFor`, and
    /// takes a breadcrumb after-count with zero special-casing in the engine.
    let textFilterDef (key : AttributeKey) (name : FacetCaption) (extractName : 'Item -> EntryName) (query : TextQuery) : AttributeDef<'Item> =
        {
            key = key
            name = name
            kind = DiscreteAttribute
            appliesTo = fun _ -> ApplicableAttribute
            extract =
                fun item ->
                    if (extractName item).value.Contains(query.value, StringComparison.OrdinalIgnoreCase)
                    then [ DiscreteValue textMatchKey ]
                    else []
        }

    /// The applied-constraint half of the text filter: selects the match
    /// sentinel under the same key the def was built with.
    let textFilterConstraint (key : AttributeKey) : AppliedConstraint =
        {
            key = key
            selection = DiscreteSelection (Set.singleton textMatchKey)
        }
