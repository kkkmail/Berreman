namespace OpticalConstructor.Tests

open System
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitArchive

module KitArchiveTests =

    let private kitPath (text : string) : KitRelativePath =
        match KitRelativePath.tryCreate text with
        | Ok path -> path
        | Error error -> failwith error.describe

    let private kitFileExcluding (globs : SendExcludeGlob list) : KitFile =
        let id = KitId (Guid.Parse "6f1c2a9e-3b7d-4e25-9a41-0c8d5e7f2b16")
        let name = KitName "Optics QZ"
        let author = KitAuthor "Test author"
        let instant = ClockTime (DateTime(2026, 11, 3, 12, 0, 0, DateTimeKind.Utc))
        {
            KitFile.create id name author instant with
                sendExclude = globs
        }

    let private assertMatches (glob : SendExcludeGlob) (expected : GlobPathMatch) (paths : string list) : unit =
        for text in paths do
            Assert.True((expected = glob.matchOf (kitPath text)), $"'{glob.value}' against '{text}'")

    [<Fact>]
    let ``a glob matches whole segments with a double star, and characters with star and question mark inside one segment, ignoring case`` () : unit =
        let anyDng = SendExcludeGlob "**/*.dng"
        assertMatches anyDng GlobPathMatched [ "IMG_0001.DNG"; "data/raw/a.dng"; "data/a.Dng" ]
        assertMatches anyDng GlobPathUnmatched [ "data/a.dngx" ]

        let bigFolder = SendExcludeGlob "data/raw/big/**"
        assertMatches bigFolder GlobPathMatched [ "data/raw/big/x.dng"; "data/raw/big/y/z.csv" ]
        assertMatches bigFolder GlobPathUnmatched [ "data/raw/big"; "data/raw/bigger/x" ]

        let rootCsv = SendExcludeGlob "*.csv"
        assertMatches rootCsv GlobPathMatched [ "run.csv" ]
        assertMatches rootCsv GlobPathUnmatched [ "data/run.csv" ]

        let oneCharacter = SendExcludeGlob "data/run ?.csv"
        assertMatches oneCharacter GlobPathMatched [ "data/run 1.csv" ]
        assertMatches oneCharacter GlobPathUnmatched [ "data/run 10.csv" ]

        let backslashed = SendExcludeGlob @"data\raw\*.csv"
        assertMatches backslashed GlobPathMatched [ "data/raw/a.csv" ]

        let everything = SendExcludeGlob "**"
        assertMatches everything GlobPathMatched [ ".kit-json"; "data/raw/a.csv" ]

    [<Fact>]
    let ``the app cache is never sent, and a glob leaves out what it matches and everything inside a folder it matches`` () : unit =
        let kitFile = kitFileExcluding [ SendExcludeGlob "**/*.dng"; SendExcludeGlob "data/scratch" ]
        let leftOut =
            [
                ".oc"
                ".oc/lock"
                ".OC/planner/x.json"
                "data/raw/IMG_0001.DNG"
                "data/scratch"
                "data/scratch/a.csv"
            ]
        let sent =
            [
                ".kit-json"
                "data"
                "data/raw"
                "data/run 1.csv"
                "library/x.material-json"
                ".ocx/a"
            ]

        for text in leftOut do
            Assert.Equal<KitSendChoice>(LeftOutOfSend, kitFile.sendChoiceOf (kitPath text))

        for text in sent do
            Assert.Equal<KitSendChoice>(SentWithKit, kitFile.sendChoiceOf (kitPath text))

    [<Fact>]
    let ``the sent entries keep the tree's order and drop what is left out`` () : unit =
        let kitJson = KitTreeFile (kitPath ".kit-json")
        let cache = KitTreeFolder (kitPath ".oc")
        let lock = KitTreeFile (kitPath ".oc/lock")
        let data = KitTreeFolder (kitPath "data")
        let raw = KitTreeFolder (kitPath "data/raw")
        let dng = KitTreeFile (kitPath "data/raw/IMG_0001.DNG")
        let run = KitTreeFile (kitPath "data/run 1.csv")
        let library = KitTreeFolder (kitPath "library")
        let notes = KitTreeFile (kitPath "notes.txt")
        let tree = [ kitJson; cache; lock; data; raw; dng; run; library; notes ]

        let ordinal = tree |> List.sortWith (fun first second -> String.CompareOrdinal(first.path.value, second.path.value))
        Assert.Equal<KitTreeEntry list>(tree, ordinal)

        let excludingDng = kitFileExcluding [ SendExcludeGlob "**/*.dng" ]
        Assert.Equal<KitTreeEntry list>([ kitJson; data; raw; run; library; notes ], excludingDng.sentEntries tree)

        let excludingNothing = kitFileExcluding []
        Assert.Equal<KitTreeEntry list>([ kitJson; data; raw; dng; run; library; notes ], excludingNothing.sentEntries tree)

    [<Fact>]
    let ``a kit is sent as a zip named for it`` () : unit =
        let expected = FileName "Optics_ QZ_LR.zip"
        Assert.Equal<FileName>(expected, (KitName "Optics: QZ/LR").zipFileName)
