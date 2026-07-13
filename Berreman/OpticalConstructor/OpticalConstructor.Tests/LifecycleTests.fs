namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain.MaterialLibrary   // MaterialId / MaterialIds
open OpticalConstructor.Domain.Library           // SampleId
open OpticalConstructor.Domain.Lifecycle

/// Spec 0038 Part H (step 020, AC-H1) — the pure entry-lifecycle foundation. The
/// centrepiece is the shared version-creation rule `decideVersioning`; these tests
/// ENUMERATE the used/unused × physics-changed/identical × metadata-only decision table
/// and prove the four acceptance statements (unused edits mutate in place, used physics
/// changes mint the next version, identical saves keep the version, metadata-only edits
/// never version). The rule takes injected structural-equality comparators, so lightweight
/// stand-in payloads — physics as `int`, metadata as `string`, `(=)` for both — fully
/// exercise it with no window and no store. The supporting surface (VersionNumber, the
/// version ids, EntryLifecycle, VersionRef, the VersionsInUse seam) is covered alongside.
module LifecycleTests =

    /// Run the shared rule over stand-in payloads (int physics, string metadata). Structural
    /// equality is `(=)` for both — exactly what the real stores pass over their value trees.
    let private decide
        (storedPhysics : int)
        (incomingPhysics : int)
        (storedMeta : string)
        (incomingMeta : string)
        (usage : VersionUsage)
        : VersionDecision =
        let stored : VersionPayload<int, string> = { physics = storedPhysics; metadata = storedMeta }
        let incoming : VersionPayload<int, string> = { physics = incomingPhysics; metadata = incomingMeta }
        decideVersioning (=) (=) stored incoming usage

    // ----- the decision table (used/unused × physics-changed/identical × metadata-only) -----

    [<Fact>]
    let ``identical save keeps the current version — unused`` () =
        Assert.Equal(KeepCurrent, decide 1 1 "name" "name" VersionUnused)

    [<Fact>]
    let ``identical save keeps the current version — used`` () =
        Assert.Equal(KeepCurrent, decide 1 1 "name" "name" VersionUsed)

    [<Fact>]
    let ``metadata-only edit never versions — unused mutates in place`` () =
        Assert.Equal(MutateInPlace, decide 1 1 "name" "renamed" VersionUnused)

    [<Fact>]
    let ``metadata-only edit never versions — used mutates in place, does NOT mint`` () =
        // The load-bearing rule: a USED version whose physics is unchanged and only its
        // metadata (name / description / category) changed must NOT mint a new version.
        Assert.Equal(MutateInPlace, decide 1 1 "name" "renamed" VersionUsed)

    [<Fact>]
    let ``unused physics change mutates in place`` () =
        Assert.Equal(MutateInPlace, decide 1 2 "name" "name" VersionUnused)

    [<Fact>]
    let ``used physics change mints the next version`` () =
        Assert.Equal(MintNextVersion, decide 1 2 "name" "name" VersionUsed)

    [<Fact>]
    let ``unused physics-and-metadata change mutates in place`` () =
        Assert.Equal(MutateInPlace, decide 1 2 "name" "renamed" VersionUnused)

    [<Fact>]
    let ``used physics-and-metadata change mints the next version (new metadata rides along)`` () =
        Assert.Equal(MintNextVersion, decide 1 2 "name" "renamed" VersionUsed)

    // ----- VersionNumber -----

    [<Fact>]
    let ``VersionNumber first is 1 and next increments monotonically`` () =
        Assert.Equal(1, VersionNumber.first.value)
        Assert.Equal(2, VersionNumber.first.next.value)
        Assert.Equal(3, VersionNumber.first.next.next.value)

    // ----- version ids -----

    [<Fact>]
    let ``a material version id pairs a material with a version and compares structurally`` () =
        let a : MaterialVersionId = { materialId = MaterialIds.silicon; version = VersionNumber.first }
        let b : MaterialVersionId = { materialId = MaterialIds.silicon; version = VersionNumber.first }
        Assert.Equal(a, b)
        Assert.Equal(MaterialIds.silicon, a.materialId)
        Assert.Equal(1, a.version.value)

    [<Fact>]
    let ``a sample version id pairs a sample with a version`` () =
        let sid = SampleId.create ()
        let sv : SampleVersionId = { sampleId = sid; version = VersionNumber.first.next }
        Assert.Equal(sid, sv.sampleId)
        Assert.Equal(2, sv.version.value)

    // ----- lifecycle state -----

    [<Fact>]
    let ``entry lifecycle is a distinct two-state active/inactive DU`` () =
        Assert.True(ActiveEntry <> InactiveEntry)
        // Extra parens: `Assert.False(x = y)` is parsed by F# as a named-argument
        // assignment (FS0505), not an equality test — so wrap it as an expression.
        Assert.False((ActiveEntry = InactiveEntry))

    // ----- cross-store version reference + the in-use seam -----

    [<Fact>]
    let ``version ref distinguishes a material version from a sample version`` () =
        let m : VersionRef = MaterialVersionRef { materialId = MaterialIds.silicon; version = VersionNumber.first }
        let s : VersionRef = SampleVersionRef { sampleId = SampleId.create (); version = VersionNumber.first }
        Assert.True(m <> s)
        match m with
        | MaterialVersionRef mv -> Assert.Equal(MaterialIds.silicon, mv.materialId)
        | SampleVersionRef _ -> Assert.Fail("expected a material version ref")

    [<Fact>]
    let ``usageOf reports VersionUsed for a ref the seam contains and VersionUnused otherwise`` () =
        let usedRef = MaterialVersionRef { materialId = MaterialIds.silicon; version = VersionNumber.first }
        let otherRef = SampleVersionRef { sampleId = SampleId.create (); version = VersionNumber.first }
        let inUse : VersionsInUse = { versionsInUse = fun () -> Set.ofList [ usedRef ] }
        Assert.Equal(VersionUsed, usageOf inUse usedRef)
        Assert.Equal(VersionUnused, usageOf inUse otherRef)

    [<Fact>]
    let ``a bound version unlocks the used-version rule through the seam`` () =
        // The seam + rule composed the way the stores (steps 21/22/25) will call them:
        // resolve usage from the in-use set, then decide. The bound version mints on a
        // physics change; an unbound one mutates in place.
        let boundRef = MaterialVersionRef { materialId = MaterialIds.langasite; version = VersionNumber.first }
        let inUse : VersionsInUse = { versionsInUse = fun () -> Set.ofList [ boundRef ] }

        let stored : VersionPayload<int, string> = { physics = 10; metadata = "langasite" }
        let incoming : VersionPayload<int, string> = { physics = 20; metadata = "langasite" }

        let boundDecision = decideVersioning (=) (=) stored incoming (usageOf inUse boundRef)
        Assert.Equal(MintNextVersion, boundDecision)

        let unboundRef = MaterialVersionRef { materialId = MaterialIds.silicon; version = VersionNumber.first }
        let unboundDecision = decideVersioning (=) (=) stored incoming (usageOf inUse unboundRef)
        Assert.Equal(MutateInPlace, unboundDecision)
