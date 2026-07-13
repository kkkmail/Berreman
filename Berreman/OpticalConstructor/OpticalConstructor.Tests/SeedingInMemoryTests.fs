namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain.MaterialLibrary   // standardCategories / builtInEntries / IncludeInactive
open OpticalConstructor.Domain.Library           // SeedSamples / seedEntries / Sample / LibraryEntry
open OpticalConstructor.Seeding.Seeding           // SeedingProxy.createInMemory / SeedingError / seedAll / InMemorySeedStores

/// Spec 0038 step 042 (IMPLEMENT_CONTRACT STORE_XDUO_0008): the REAL in-memory `SeedingProxy`.
/// `SeedingProxy.createInMemory ()` builds four EMPTY in-memory stores (`createInMemoryEmpty`) and
/// adapts each store's add verb into the four `SeedingProxy` save fields. These tests prove that
/// `seedAll` over those blank stores reproduces the seeded catalogue — category / material / sample /
/// entry counts match the Domain seed lists, and the fixed seed ids resolve back to the SAME seed
/// instances — all `Result`-checked, with NO database and NO file IO. A third test proves the stores
/// are genuinely stateful (a second `seedAll` is rejected), not a discarding mock.
module SeedingInMemoryTests =

    /// Assert a fixed seed id resolved (through `tryGetMaterial` / `tryGetSample` / `tryGetEntry`) to
    /// the SAME seed instance. Reference identity via `Object.ReferenceEquals` — the seeds are
    /// module-level `let` values (computed once) and the store holds those very instances, so the
    /// resolved value IS the seed (stronger than value-equality, and safe for the seed types whose
    /// engine payloads carry no structural equality). `Result`-checked: a store error or a missing id
    /// fails the test.
    let private assertResolves (name : string) (expected : 'Item) (actual : Result<'Item option, 'Err>) : unit =
        match actual with
        | Ok (Some x) ->
            Assert.True(
                System.Object.ReferenceEquals(box expected, box x),
                $"{name}: resolved a different instance than the seed value")
        | Ok None -> Assert.Fail($"{name}: seed id did not resolve after seeding")
        | Error e -> Assert.Fail($"{name}: resolution errored: %A{e}")

    [<Fact>]
    let ``seedAll over empty in-memory stores reproduces the seeded catalogue counts`` () =
        let stores = SeedingProxy.createInMemory ()

        match seedAll stores.proxy with
        | Ok () -> ()
        | Error e -> Assert.Fail($"seedAll over empty in-memory stores should be Ok (), got %A{e}")

        // Category / material / sample counts equal the Domain seed-list lengths (latest of each,
        // IncludeInactive — nothing is retired, so latest = the seed).
        match stores.categories.listCategories () with
        | Ok cats -> Assert.Equal(List.length standardCategories, List.length cats)
        | Error e -> Assert.Fail($"listCategories failed: %A{e}")

        match stores.materials.listMaterials IncludeInactive with
        | Ok mats -> Assert.Equal(List.length builtInEntries, List.length mats)
        | Error e -> Assert.Fail($"listMaterials failed: %A{e}")

        match stores.samples.listSamples IncludeInactive with
        | Ok samps -> Assert.Equal(List.length SeedSamples.all, List.length samps)
        | Error e -> Assert.Fail($"listSamples failed: %A{e}")

        // The library-entry store has no "list all" verb; union `entriesForKind` over the kinds the
        // seeds actually use (derived from the seeds, not hard-coded), deduped by entry id — for the
        // seed set this yields exactly `seedEntries`. Each per-kind read is Result-checked.
        let seedKinds = seedEntries |> List.collect (fun e -> e.forKinds) |> List.distinct
        let perKind = seedKinds |> List.map (fun k -> k, stores.libraryEntries.entriesForKind k)
        for (k, r) in perKind do
            match r with
            | Ok _ -> ()
            | Error e -> Assert.Fail($"entriesForKind for %A{k} failed: %A{e}")
        let storedEntries =
            perKind
            |> List.collect (fun (_, r) -> match r with | Ok es -> es | Error _ -> [])
            |> List.distinctBy (fun e -> e.entryId)
        Assert.Equal(List.length seedEntries, List.length storedEntries)

    [<Fact>]
    let ``the fixed seed ids resolve to the same seed instances through the seeded in-memory stores`` () =
        let stores = SeedingProxy.createInMemory ()
        match seedAll stores.proxy with
        | Ok () -> ()
        | Error e -> Assert.Fail($"seedAll should be Ok (), got %A{e}")

        // Categories resolve by their fixed CategoryIds, to the same seed records.
        match stores.categories.listCategories () with
        | Ok cats ->
            standardCategories
            |> List.iter (fun c ->
                match cats |> List.tryFind (fun x -> x.id = c.id) with
                | Some x ->
                    Assert.True(
                        System.Object.ReferenceEquals(box c, box x),
                        $"category '{c.name}' resolved a different instance than the seed")
                | None -> Assert.Fail($"category id '{string c.id.value}' did not resolve after seeding"))
        | Error e -> Assert.Fail($"listCategories failed: %A{e}")

        // Materials / samples / library entries resolve by their fixed ids, to the same seed instances.
        builtInEntries |> List.iter (fun m -> assertResolves $"material '{m.name}'" m (stores.materials.tryGetMaterial m.id))
        SeedSamples.all |> List.iter (fun s -> assertResolves $"sample '{s.name}'" s (stores.samples.tryGetSample s.id))
        seedEntries |> List.iter (fun e -> assertResolves $"entry '{e.entryId}'" e (stores.libraryEntries.tryGetEntry e.entryId))

    [<Fact>]
    let ``re-seeding the already-seeded in-memory stores is rejected (the store is genuinely stateful)`` () =
        // Proves the in-memory stores actually retain state: the first seedAll fills them; the second
        // hits the first store's duplicate-id block, surfaced as SeedRejected, and no Ok () no-op.
        let stores = SeedingProxy.createInMemory ()
        match seedAll stores.proxy with
        | Ok () -> ()
        | Error e -> Assert.Fail($"first seedAll should be Ok (), got %A{e}")

        match seedAll stores.proxy with
        | Error (SeedRejected reason) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | Ok () -> Assert.Fail("re-seeding an already-seeded store should be rejected, not Ok ()")
