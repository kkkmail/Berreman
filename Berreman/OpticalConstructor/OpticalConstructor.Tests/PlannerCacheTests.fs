namespace OpticalConstructor.Tests

open System
open System.IO
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.MeasurementPlanner
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Storage.PlannerCacheStore
open PlannerInputsTests

/// The Planner's candidate cache under a kit's `.oc/planner/`: a candidate is keyed by its own items, branch, opened
/// material and targets; a hit is read back without computing, a miss computed and written under read-write access
/// only, and a file that does not bind or holds another key is a miss (spec 0050 S14.2).
module PlannerCacheTests =

    let private fixedClock : ClockProvider =
        { now = fun () -> ClockTime (DateTime(2026, 11, 18, 12, 0, 0, DateTimeKind.Utc)) }

    let private kitValue<'Value> (result : Result<'Value, KitError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private keyValue (result : Result<PlannerCacheKey, PlannerError>) : PlannerCacheKey =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private rowsValue (result : Result<CandidateRows, PlannerError>) : CandidateRows =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private nameOf (key : PlannerCacheKey) : FileName =
        key.cacheFileName |> Result.defaultWith (fun error -> failwith error.describe)

    let private entryTextOf (key : PlannerCacheKey) (rows : CandidateRows) : KitFileText =
        let entry : PlannerCacheEntry = { key = key; rows = rows }
        entry.fileText |> Result.defaultWith (fun error -> failwith error.describe)

    /// The kit's path of the key's file.
    let private plannerPathOf (key : PlannerCacheKey) : KitRelativePath =
        KitRelativePath.plannerCacheFileOf (nameOf key) |> kitValue

    /// Laser, Polarimeter and QZ-Z.
    let private items : Item list = [ laser; polarimeter; qzZ ]

    /// QZ-Z's two candidates, transmitted then reflected, over the targets n_e and g33.
    let private inputs : PlannerInputs = plannerInputsOf crystalStudy items choiceOf crystalVersion |> inputsValue

    let private transmitted : PlanCandidate = List.head inputs.candidates

    let private reflected : PlanCandidate = List.last inputs.candidates

    let private transmittedKey : PlannerCacheKey = inputs.cacheKeyWith items transmitted |> keyValue

    let private reflectedKey : PlannerCacheKey = inputs.cacheKeyWith items reflected |> keyValue

    let private transmittedRows : CandidateRows =
        {
            jacobian = ResidualJacobian [| [| 0.125; -2.5 |]; [| 1e-3; 0.1 |] |]
            noise = NoiseVariance 2.5e-5
        }

    let private reflectedRows : CandidateRows =
        {
            jacobian = ResidualJacobian [| [| -0.75; 3.0 |] |]
            noise = NoiseVariance 4e-4
        }

    /// A `compute` answering `rows` that counts its calls in `calls`.
    let private countedCompute (calls : int ref) (rows : CandidateRows) : unit -> Result<CandidateRows, PlannerError> =
        fun () ->
            calls.Value <- calls.Value + 1
            Ok rows

    /// The rows `context` answers for `key`, its `compute` answering `rows` and counting its calls in `calls`.
    let private rowsThrough
        (context : KitContext)
        (key : PlannerCacheKey)
        (calls : int ref)
        (rows : CandidateRows)
        : CandidateRows =
        context.cachedPlannerRows key (countedCompute calls rows) |> rowsValue

    let private newKitFile () : KitFile =
        KitFile.create
            (KitId (Guid.Parse "48500000-0000-4000-8000-000000000001"))
            (KitName "Planner cache")
            (KitAuthor "Planner cache tests")
            (fixedClock.now ())

    /// One in-memory folder initialised as a kit.
    let private newKitFolder () : InMemoryKitFolder =
        let folder = InMemoryKitFolder.create ()
        (KitProxy.createInMemory folder).initialiseKit (newKitFile ()) |> kitValue
        folder

    let private openOver (folder : InMemoryKitFolder) (access : KitAccess) : KitContext =
        KitContext.openKit (KitProxy.createInMemory folder) access fixedClock |> kitValue

    let private pathsOf (folder : InMemoryKitFolder) : string list =
        folder.paths () |> List.map (fun path -> path.value)

    [<Fact>]
    let ``a cache miss computes once and writes one file under .oc/planner, and a hit reads it back without computing`` () : unit =
        let folder = newKitFolder ()
        let context = openOver folder ReadWriteKitAccess
        let before = pathsOf folder
        let calls = ref 0

        let first = rowsThrough context transmittedKey calls transmittedRows
        Assert.Equal(transmittedRows, first)
        Assert.Equal(1, calls.Value)

        let written = (plannerPathOf transmittedKey).value
        Assert.Matches("^\\.oc/planner/[0-9a-f]{64}\\.candidate-json$", written)
        let cached = pathsOf folder
        let expectedPaths : string list =
            before @ [ ".oc/planner"; written ] |> List.sortWith (fun a b -> String.CompareOrdinal(a, b))

        Assert.Equal<string list>(expectedPaths, cached)

        Assert.Equal<Result<KitFileText option, KitError>>(
            Ok (Some (entryTextOf transmittedKey transmittedRows)),
            context.proxy.readPlannerCacheFile (nameOf transmittedKey)
        )

        // The same key again is a hit: the first rows, `compute` not called, nothing written.
        let second = rowsThrough context transmittedKey calls reflectedRows
        Assert.Equal(first, second)
        Assert.Equal(1, calls.Value)
        Assert.Equal<string list>(cached, pathsOf folder)

        // A miss whose `compute` refuses answers that refusal and writes nothing.
        let refusal : PlannerError = NoiselessPlannerCandidateErr "probe"
        let refusing () : Result<CandidateRows, PlannerError> = Error refusal
        Assert.Equal<Result<CandidateRows, PlannerError>>(
            Error refusal,
            context.cachedPlannerRows reflectedKey refusing
        )

        Assert.Equal<string list>(cached, pathsOf folder)

        // A store that refuses the write answers PlannerCacheErr with the store's reason.
        let refusingStore : KitContext =
            {
                context with
                    proxy = { context.proxy with writePlannerCacheFile = fun _ _ -> Error (KitIoErr "probe disk full") }
            }

        match refusingStore.cachedPlannerRows reflectedKey (countedCompute calls reflectedRows) with
        | Error (PlannerCacheErr reason as error) ->
            Assert.Contains("probe disk full", reason)
            Assert.StartsWith("the planner's cache cannot be read or written: ", error.describe)
        | Error error -> Assert.Fail $"Expected PlannerCacheErr, got: {error.describe}"
        | Ok _ -> Assert.Fail "A refused write must not answer rows."

        Assert.Equal(2, calls.Value)
        Assert.Equal<string list>(cached, pathsOf folder)
        Assert.Equal<Result<unit, KitError>>(Ok (), context.close ())

    [<Fact>]
    let ``a changed item or target is a new key, and an item no candidate reads leaves every key unchanged`` () : unit =
        let expectedKey : PlannerCacheKey =
            {
                source = laser
                plate = qzZ
                detector = polarimeter
                observable = TransmittedMueller
                material = inputs.prior.material
                targets = [ nE; g33 ]
            }

        Assert.Equal(expectedKey, transmittedKey)
        Assert.Equal({ expectedKey with observable = ReflectedMueller }, reflectedKey)
        Assert.NotEqual(nameOf transmittedKey, nameOf reflectedKey)

        // QZ-Z 0.6 mm thick in place of 0.5 mm is another key.
        let thicker : Item =
            qzZ.withParameterState ParameterName.substrateThickness (KnownParameter (ParameterValue 6e-4)) |> itemValue

        Assert.NotEqual(qzZ, thicker)
        let thickerKey : PlannerCacheKey = inputs.cacheKeyWith [ laser; polarimeter; thicker ] transmitted |> keyValue
        Assert.Equal(thicker, thickerKey.plate)
        Assert.NotEqual(nameOf transmittedKey, nameOf thickerKey)

        // The targets g33 alone are another key.
        let g33Only : PlannerInputs = { inputs with targets = [ g33 ] }
        let g33Key : PlannerCacheKey = g33Only.cacheKeyWith items transmitted |> keyValue
        Assert.Equal<ParameterName list>([ g33 ], g33Key.targets)
        Assert.NotEqual(nameOf transmittedKey, nameOf g33Key)

        // LP1 joins the kit: no candidate reads it, so the candidates and each key stay as they were.
        let withLp1Items : Item list = [ laser; lp1; polarimeter; qzZ ]
        let withLp1 : PlannerInputs = plannerInputsOf crystalStudy withLp1Items choiceOf crystalVersion |> inputsValue
        Assert.NotEqual<PlannerOptic list>(inputs.lab.optics, withLp1.lab.optics)
        Assert.Equal<PlanCandidate list>(inputs.candidates, withLp1.candidates)

        for candidate in inputs.candidates do
            let key : PlannerCacheKey = inputs.cacheKeyWith items candidate |> keyValue
            let keyWithLp1 : PlannerCacheKey = withLp1.cacheKeyWith withLp1Items candidate |> keyValue
            Assert.Equal(key, keyWithLp1)
            Assert.Equal(nameOf key, nameOf keyWithLp1)

        // An item the items lack, or a kit without a polarimeter, has no key.
        Assert.Equal<Result<PlannerCacheKey, PlannerError>>(
            Error (UnresolvedPlannerItemErr "QZ-Z: the kit holds no such item"),
            inputs.cacheKeyWith [ laser; polarimeter ] transmitted
        )

        let noPolarimeter : PlannerInputs = { inputs with lab = { inputs.lab with detectors = [] } }

        Assert.Equal<Result<PlannerCacheKey, PlannerError>>(
            Error (UnresolvedPlannerItemErr "no detector of the kit records a full Mueller matrix"),
            noPolarimeter.cacheKeyWith items transmitted
        )

    [<Fact>]
    let ``a read-only kit computes and writes nothing, and reads what a read-write open cached`` () : unit =
        let folder = newKitFolder ()
        let writer = openOver folder ReadWriteKitAccess
        let calls = ref 0
        Assert.Equal(transmittedRows, rowsThrough writer transmittedKey calls transmittedRows)
        Assert.Equal(1, calls.Value)

        let reader = openOver folder ReadOnlyKitAccess
        let cached = pathsOf folder

        // The read-write open's entry is a hit through the read-only one.
        Assert.Equal(transmittedRows, rowsThrough reader transmittedKey calls reflectedRows)
        Assert.Equal(1, calls.Value)

        // A miss computes and answers the rows, and nothing is written, so the next read computes again.
        Assert.Equal(reflectedRows, rowsThrough reader reflectedKey calls reflectedRows)
        Assert.Equal(2, calls.Value)
        Assert.Equal<string list>(cached, pathsOf folder)
        Assert.Equal<Result<KitFileText option, KitError>>(
            Ok None,
            reader.proxy.readPlannerCacheFile (nameOf reflectedKey)
        )

        Assert.Equal(reflectedRows, rowsThrough reader reflectedKey calls reflectedRows)
        Assert.Equal(3, calls.Value)
        Assert.Equal<string list>(cached, pathsOf folder)

        Assert.Equal<Result<unit, KitError>>(Ok (), reader.close ())
        Assert.Equal<Result<unit, KitError>>(Ok (), writer.close ())

    [<Fact>]
    let ``a cache file that does not bind, or holds another key, is a miss that is written over`` () : unit =
        let folder = newKitFolder ()
        let context = openOver folder ReadWriteKitAccess
        let name : FileName = nameOf transmittedKey
        let expected : KitFileText = entryTextOf transmittedKey transmittedRows
        let calls = ref 0

        let strayTexts : KitFileText list = [ KitFileText "{"; entryTextOf reflectedKey reflectedRows ]

        for stray in strayTexts do
            folder.placeLabFile (plannerPathOf transmittedKey) stray |> kitValue
            Assert.Equal<Result<KitFileText option, KitError>>(Ok (Some stray), context.proxy.readPlannerCacheFile name)
            let before = calls.Value

            Assert.Equal(transmittedRows, rowsThrough context transmittedKey calls transmittedRows)
            Assert.Equal(before + 1, calls.Value)
            Assert.Equal<Result<KitFileText option, KitError>>(
                Ok (Some expected),
                context.proxy.readPlannerCacheFile name
            )

            // The entry written over it is now a hit.
            Assert.Equal(transmittedRows, rowsThrough context transmittedKey calls reflectedRows)
            Assert.Equal(before + 1, calls.Value)

        // Text that does not bind, or binds to null, is MalformedJsonErr, never a throw.
        let malformedTexts : KitFileText list = [ KitFileText "{"; KitFileText "null"; KitFileText "{}" ]

        for malformed in malformedTexts do
            match malformed.parsePlannerCacheEntry with
            | Error (MalformedJsonErr _) -> ()
            | Error error -> Assert.Fail $"Expected MalformedJsonErr for '{malformed.value}', got: {error.describe}"
            | Ok _ -> Assert.Fail $"'{malformed.value}' must not bind to a planner cache entry."

        Assert.Equal<Result<PlannerCacheEntry, StorageError>>(
            Ok { key = transmittedKey; rows = transmittedRows },
            expected.parsePlannerCacheEntry
        )

        Assert.Equal<Result<unit, KitError>>(Ok (), context.close ())

    [<Fact>]
    let ``a folder kit keeps the planner's cache across opens`` () : unit =
        let path : FolderName =
            FolderName (Path.Combine(AppContext.BaseDirectory, "PlannerCacheTests", Guid.NewGuid().ToString("N")))

        Directory.CreateDirectory path.value |> ignore
        let kitFolder : KitFolder = KitFolder path
        (KitProxy.create kitFolder).initialiseKit (newKitFile ()) |> kitValue
        let calls = ref 0

        let writer = KitContext.openKit (KitProxy.create kitFolder) ReadWriteKitAccess fixedClock |> kitValue
        Assert.Equal(transmittedRows, rowsThrough writer transmittedKey calls transmittedRows)
        Assert.Equal(1, calls.Value)
        Assert.Equal<Result<unit, KitError>>(Ok (), writer.close ())

        let onDisk : string = Path.Combine(path.value, ".oc", "planner", (nameOf transmittedKey).value)
        Assert.True(File.Exists onDisk, $"Expected the cache file '{onDisk}'.")

        let reader = KitContext.openKit (KitProxy.create kitFolder) ReadOnlyKitAccess fixedClock |> kitValue
        Assert.Equal(transmittedRows, rowsThrough reader transmittedKey calls reflectedRows)
        Assert.Equal(1, calls.Value)
        Assert.Equal<Result<unit, KitError>>(Ok (), reader.close ())
