namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain.MaterialLibrary   // MaterialId / MaterialIds
open OpticalConstructor.Domain.Library           // SampleId
open OpticalConstructor.Domain.Lifecycle

/// Pin-based version decisions and their supporting lifecycle types
/// (spec 0038 Part H; spec 0050 S2.2).
module LifecycleTests =

    /// Run the shared rule over stand-in payloads (int physics, string metadata). Structural
    /// equality is `(=)` for both — exactly what the real stores pass over their value trees.
    let private decide
        (storedPhysics : int)
        (incomingPhysics : int)
        (storedMeta : string)
        (incomingMeta : string)
        (pin : PinState)
        : VersionDecision =
        let stored : VersionPayload<int, string> = { physics = storedPhysics; metadata = storedMeta }
        let incoming : VersionPayload<int, string> = { physics = incomingPhysics; metadata = incomingMeta }
        decideVersioning stored incoming pin

    // ----- the decision table (pinned/unpinned × physics-changed/identical × metadata-only) -----

    [<Fact>]
    let ``identical save keeps the current version — unpinned`` () =
        Assert.Equal(KeepCurrent, decide 1 1 "name" "name" VersionUnpinned)

    [<Fact>]
    let ``identical save keeps the current version — pinned`` () =
        Assert.Equal(KeepCurrent, decide 1 1 "name" "name" VersionPinned)

    [<Fact>]
    let ``metadata-only edit never versions — unpinned mutates in place`` () =
        Assert.Equal(MutateInPlace, decide 1 1 "name" "renamed" VersionUnpinned)

    [<Fact>]
    let ``metadata-only edit never versions — pinned mutates in place, does NOT mint`` () =
        // The load-bearing rule: a PINNED version whose physics is unchanged and only its
        // metadata (name / description / category) changed must NOT mint a new version.
        Assert.Equal(MutateInPlace, decide 1 1 "name" "renamed" VersionPinned)

    [<Fact>]
    let ``unpinned physics change mutates in place`` () =
        Assert.Equal(MutateInPlace, decide 1 2 "name" "name" VersionUnpinned)

    [<Fact>]
    let ``pinned physics change mints the next version`` () =
        Assert.Equal(MintNextVersion, decide 1 2 "name" "name" VersionPinned)

    [<Fact>]
    let ``unpinned physics-and-metadata change mutates in place`` () =
        Assert.Equal(MutateInPlace, decide 1 2 "name" "renamed" VersionUnpinned)

    [<Fact>]
    let ``pinned physics-and-metadata change mints the next version (new metadata rides along)`` () =
        Assert.Equal(MintNextVersion, decide 1 2 "name" "renamed" VersionPinned)

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

    [<Fact>]
    let ``lifecycle, protection and pin states round-trip their database tags`` () : unit =
        Assert.Equal(1, ActiveEntry.value)
        Assert.Equal(2, InactiveEntry.value)
        Assert.Equal<EntryLifecycle option>(Some ActiveEntry, EntryLifecycle.tryOfDb 1)
        Assert.Equal<EntryLifecycle option>(Some InactiveEntry, EntryLifecycle.tryOfDb 2)

        Assert.Equal(1, ProtectedBuiltIn.value)
        Assert.Equal(2, UserManaged.value)
        Assert.Equal<EntryProtection option>(Some ProtectedBuiltIn, EntryProtection.tryOfDb 1)
        Assert.Equal<EntryProtection option>(Some UserManaged, EntryProtection.tryOfDb 2)

        Assert.Equal(1, VersionPinned.value)
        Assert.Equal(2, VersionUnpinned.value)
        Assert.Equal<PinState option>(Some VersionPinned, PinState.tryOfDb 1)
        Assert.Equal<PinState option>(Some VersionUnpinned, PinState.tryOfDb 2)

        for tag in [ 0; 3 ] do
            Assert.Equal<EntryLifecycle option>(None, EntryLifecycle.tryOfDb tag)
            Assert.Equal<EntryProtection option>(None, EntryProtection.tryOfDb tag)
            Assert.Equal<PinState option>(None, PinState.tryOfDb tag)

    // ----- cross-store version reference -----

    [<Fact>]
    let ``version ref distinguishes a material version from a sample version`` () =
        let m : VersionRef = MaterialVersionRef { materialId = MaterialIds.silicon; version = VersionNumber.first }
        let s : VersionRef = SampleVersionRef { sampleId = SampleId.create (); version = VersionNumber.first }
        Assert.True(m <> s)
        match m with
        | MaterialVersionRef mv -> Assert.Equal(MaterialIds.silicon, mv.materialId)
        | SampleVersionRef _ -> Assert.Fail("expected a material version ref")
