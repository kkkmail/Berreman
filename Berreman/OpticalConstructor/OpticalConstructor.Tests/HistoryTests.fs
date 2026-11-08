namespace OpticalConstructor.Tests

open Softellect.Sys.Primitives
open OpticalConstructor.Storage
open Xunit

/// `RecentFiles.bump` keeps paths in most-recent-first order.
module HistoryTests =

    [<Fact>]
    let ``R-1 bump dedups case-insensitively, pushes most-recent first, and truncates to maxRecent`` () =
        // Most-recent-first ordering: a freshly bumped path leads the list.
        let afterB = RecentFiles.bump (FileName @"C:\kits\b") [ FileName @"C:\kits\a" ]
        Assert.Equal<FileName list>([ FileName @"C:\kits\b"; FileName @"C:\kits\a" ], afterB)

        // Case-insensitive dedup: re-pushing an existing path (differing only in case)
        // removes the old occurrence and re-seats it at the front — no duplicate.
        let reBumped = RecentFiles.bump (FileName @"C:\KITS\A") afterB
        Assert.Equal<FileName list>([ FileName @"C:\KITS\A"; FileName @"C:\kits\b" ], reBumped)

        // Truncation to maxRecent (16): bumping 20 distinct paths keeps only the 16
        // most-recent, with the very last bump at the head.
        let full =
            [ 1 .. 20 ]
            |> List.fold (fun acc i -> RecentFiles.bump (FileName $@"C:\kits\f%d{i}") acc) []
        Assert.Equal(RecentFiles.maxRecent, List.length full)
        Assert.Equal(FileName @"C:\kits\f20", List.head full)
        Assert.Equal(FileName @"C:\kits\f5", List.last full)
