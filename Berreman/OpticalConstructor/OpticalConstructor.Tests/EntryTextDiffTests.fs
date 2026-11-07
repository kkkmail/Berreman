namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain.EntryTextDiff

module EntryTextDiffTests =

    let private a = EntryTextLine "a"
    let private b = EntryTextLine "b"
    let private c = EntryTextLine "c"
    let private d = EntryTextLine "d"
    let private x = EntryTextLine "x"
    let private y = EntryTextLine "y"

    let private assertDiff (held : EntryTextLine list) (incoming : EntryTextLine list) (expected : EntryDiffLine list) : unit =
        let actual = EntryDiffLine.diffLines held incoming
        Assert.Equal<EntryDiffLine list>(expected, actual)

        let recoveredHeld =
            actual |> List.choose (function SameEntryLine line | RemovedEntryLine line -> Some line | AddedEntryLine _ -> None)
        let recoveredIncoming =
            actual |> List.choose (function SameEntryLine line | AddedEntryLine line -> Some line | RemovedEntryLine _ -> None)

        Assert.Equal<EntryTextLine list>(held, recoveredHeld)
        Assert.Equal<EntryTextLine list>(incoming, recoveredIncoming)

    [<Fact>]
    let ``equal texts diff as unchanged lines only`` () : unit =
        assertDiff [ a; b; c ] [ a; b; c ] [ SameEntryLine a; SameEntryLine b; SameEntryLine c ]
        assertDiff [] [] []
        let blank = EntryTextLine ""
        assertDiff [ blank; a; blank ] [ blank; a; blank ] [ SameEntryLine blank; SameEntryLine a; SameEntryLine blank ]

    [<Fact>]
    let ``a changed line is removed and then added between the unchanged lines`` () : unit =
        assertDiff [ a; b; c; d ] [ a; x; c; d ]
            [ SameEntryLine a; RemovedEntryLine b; AddedEntryLine x; SameEntryLine c; SameEntryLine d ]
        assertDiff [ a; b; c ] [ a; c ] [ SameEntryLine a; RemovedEntryLine b; SameEntryLine c ]
        assertDiff [ a; b; c; d ] [ x; b; y; d ]
            [ RemovedEntryLine a; AddedEntryLine x; SameEntryLine b; RemovedEntryLine c; AddedEntryLine y; SameEntryLine d ]
        assertDiff [ a; b; c ] [ b; c; a ]
            [ RemovedEntryLine a; SameEntryLine b; SameEntryLine c; AddedEntryLine a ]
        assertDiff [ a; b; a; b ] [ b; a; b; a ]
            [ RemovedEntryLine a; SameEntryLine b; SameEntryLine a; SameEntryLine b; AddedEntryLine a ]

        let prefix = List.replicate 10000 a
        let suffix = List.replicate 10000 d
        assertDiff (prefix @ [ b ] @ suffix) (prefix @ [ x ] @ suffix)
            ((prefix |> List.map SameEntryLine) @ [ RemovedEntryLine b; AddedEntryLine x ] @ (suffix |> List.map SameEntryLine))

    [<Fact>]
    let ``lines added or removed at either end leave the rest unchanged`` () : unit =
        assertDiff [ x; a; b ] [ a; b ] [ RemovedEntryLine x; SameEntryLine a; SameEntryLine b ]
        assertDiff [ a; b ] [ a; b; y ] [ SameEntryLine a; SameEntryLine b; AddedEntryLine y ]
        assertDiff [ a; b ] [ c; d ] [ RemovedEntryLine a; RemovedEntryLine b; AddedEntryLine c; AddedEntryLine d ]
        assertDiff [ a; b ] [ x; a; b ] [ AddedEntryLine x; SameEntryLine a; SameEntryLine b ]
        assertDiff [ a; b; y ] [ a; b ] [ SameEntryLine a; SameEntryLine b; RemovedEntryLine y ]
        assertDiff [] [ a; b ] [ AddedEntryLine a; AddedEntryLine b ]
        assertDiff [ a; b ] [] [ RemovedEntryLine a; RemovedEntryLine b ]
