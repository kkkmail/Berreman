# Step 009 — impl plan (attempt 1)

## Goal

Spec 0038 Part D, step 009 (IMPLEMENT): the generic faceted-navigation engine —
a new, pure, Avalonia-free `OpticalConstructor.Domain/Facets.fs` plus its unit
tests in `OpticalConstructor.Tests`. Generic over `'Item`; the numeric bucket
builder is step 010 and the concrete material/library facet catalogues are
step 011 (both depend on 9) — neither lands this round.

## Shape (pinned by the slice)

- `AttributeKey`, `DiscreteKey` — single-case string DUs with `.value`.
- `AttributeValue = DiscreteValue of DiscreteKey | NumericValue of double`.
- `Applicability = ApplicableAttribute | InapplicableAttribute`.
- `AttributeKind = DiscreteAttribute | NumericAttribute`.
- `AttributeDef<'Item> = { key; name : string; kind; appliesTo : 'Item -> Applicability;
  extract : 'Item -> AttributeValue list }` — extract returns a LIST (multi-valued
  facets first-class).
- `FacetSelection = DiscreteSelection of Set<DiscreteKey> (OR within a facet)
  | NumericRangeSelection of NumericRange` where `NumericRange` is the
  lower/upper pair. Membership is half-open `[lower, upper)` — matching the
  step-010 bucket intervals so a bucket applied as a chip reproduces its count —
  with the degenerate `lower = upper` range meaning the exact value (the
  step-010 single-value bucket collapse needs it representable).
- `AppliedConstraint = { key : AttributeKey; selection : FacetSelection }`.
- `Representation` — single-case DU over an ordered `AttributeKey list`.
- Engine (naive folds, corpus is tens to hundreds, no indices), explicit
  concrete signatures: `filter` (AND across facets, OR within one),
  `countFor` (count-preview of one candidate under the applied set =
  filter with the candidate appended), `breadcrumbCounts` (cumulative
  result count after each applied constraint in application order),
  `buildTree` (representation + defs + constraints + items → `FacetTree`;
  branches carry value label + count; zero-count branches dropped;
  attributes inapplicable to every filtered item omitted entirely).
- Plain-text filter as an ordinary constraint: `textFilterDef` builds an
  `AttributeDef` over a caller-supplied text extractor whose `extract`
  returns a match-sentinel `DiscreteValue` when the item's text contains the
  query (ordinal, case-insensitive), `textFilterConstraint` builds the
  matching `AppliedConstraint` — so the text filter ANDs with facets and
  gets a breadcrumb after-count with zero special-casing in the engine.
- Counts are elevated (`ItemCount` single-case DU with `.value`) per the
  repo's elevate-every-primitive rule; `name`/`label` stay `string` exactly
  as the slice pins them.

## Semantics decisions (recorded, tested)

- A constraint requires applicability: an item for which the constrained
  attribute is `InapplicableAttribute` fails that constraint.
- A constraint whose key has no def matches NOTHING (fail-closed — a broken
  chip surfaces as zero results, never as a silently ignored filter).
- Branch counts count ITEMS (an item extracting the same value twice counts
  once in that branch); multi-valued items count once in EACH branch, so
  branch counts need not sum to the total.
- Branches derive from the currently filtered population, so zero-count
  branches are absent by construction; branch order is the structural sort
  of `AttributeValue` (discrete by key string, numeric by value) —
  deterministic and independent of corpus order.
- An applicable attribute whose extract yields no values for any filtered
  item produces no facet node (an empty facet header is noise); a
  representation key with no def is skipped.

## Files

- NEW `Berreman/OpticalConstructor/OpticalConstructor.Domain/Facets.fs`
  (registered at the end of the Domain compile list — depends on nothing;
  steps 010/011 append their siblings after it).
- NEW `Berreman/OpticalConstructor/OpticalConstructor.Tests/FacetsTests.fs`
  (registered at the end of the Tests compile list): fixed 6-item corpus,
  every engine function covered — OR within / AND across, multi-valued
  counting, half-open + degenerate numeric ranges, zero-count absence,
  inapplicable omission, breadcrumb order sensitivity, text filter as an
  ordinary constraint, fail-closed unknown key.
- fsproj edits for both projects.

## Risks

- `--warnaserror+:25` — keep every match total.
- LF endings — verify no CRLF churn after editing.
- Structural comparison: `AttributeValue`/`DiscreteKey` must stay comparable
  (they are — string/double payloads) for `Set<DiscreteKey>` and branch sort.

## Gates (run by the arc-runner after exit)

Roster: build, unit-tests, constructor-unit-tests, ui-smoke, ui-tests.
This round only OpticalConstructor.Tests counts should move (+N new facts);
119 / 124 / 361 stay flat. Diagnostic runs (not gate authority) will confirm
and fill the SoW baseline YAML.
