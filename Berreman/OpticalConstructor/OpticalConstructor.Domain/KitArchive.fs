namespace OpticalConstructor.Domain

open System
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit

/// What travels when a kit is sent: one zip, without the app's cache or what `sendExclude` names (spec 0050 S6; 001 §H).
module KitArchive =

    /// Whether a send-exclusion glob matches a kit path (spec 0050 S6).
    type GlobPathMatch =
        | GlobPathMatched
        | GlobPathUnmatched

    type SendExcludeGlob with

        /// Matches the whole path from the kit root. `**` spans whole segments, and a final one needs at least one, so
        /// `a/**` is what lies inside `a`. `*` and `?` stay inside one segment; other characters ignore case (spec 0050 S6).
        member this.matchOf (path : KitRelativePath) : GlobPathMatch =
            let rec matchCharacters (pattern : char list) (text : char list) : GlobPathMatch =
                match pattern, text with
                | [], [] -> GlobPathMatched
                | '*' :: restPattern, _ ->
                    match matchCharacters restPattern text, text with
                    | GlobPathMatched, _ -> GlobPathMatched
                    | GlobPathUnmatched, _ :: restText -> matchCharacters pattern restText
                    | GlobPathUnmatched, [] -> GlobPathUnmatched
                | '?' :: restPattern, _ :: restText -> matchCharacters restPattern restText
                | expected :: restPattern, actual :: restText
                    when String.Equals(string expected, string actual, StringComparison.OrdinalIgnoreCase) ->
                    matchCharacters restPattern restText
                | _ -> GlobPathUnmatched

            let rec matchSegments (pattern : string list) (segments : string list) : GlobPathMatch =
                match pattern, segments with
                | [], [] -> GlobPathMatched
                | [ "**" ], [] -> GlobPathUnmatched
                | [ "**" ], _ -> GlobPathMatched
                | "**" :: restPattern, _ ->
                    match matchSegments restPattern segments, segments with
                    | GlobPathMatched, _ -> GlobPathMatched
                    | GlobPathUnmatched, _ :: restSegments -> matchSegments pattern restSegments
                    | GlobPathUnmatched, [] -> GlobPathUnmatched
                | globSegment :: restPattern, segment :: restSegments ->
                    match matchCharacters (List.ofSeq globSegment) (List.ofSeq segment) with
                    | GlobPathMatched -> matchSegments restPattern restSegments
                    | GlobPathUnmatched -> GlobPathUnmatched
                | _ -> GlobPathUnmatched

            let globSegments = this.value.Replace('\\', '/').Split('/') |> List.ofArray
            let pathSegments = path.value.Split('/') |> List.ofArray
            matchSegments globSegments pathSegments

    /// Whether a kit path travels when the kit is sent (spec 0050 S6).
    type KitSendChoice =
        | SentWithKit
        | LeftOutOfSend

    /// One file or folder under a kit, as a send walks its tree (spec 0050 S6).
    type KitTreeEntry =
        | KitTreeFile of KitRelativePath
        | KitTreeFolder of KitRelativePath

        member this.path : KitRelativePath =
            match this with
            | KitTreeFile path
            | KitTreeFolder path -> path

    type KitFile with

        /// Leaves out the app's cache and every path a `sendExclude` glob matches, itself or through a folder holding
        /// it (spec 0050 S6; 001 §A, §E).
        member this.sendChoiceOf (path : KitRelativePath) : KitSendChoice =
            let rec matchAny (globs : SendExcludeGlob list) (candidate : KitRelativePath) : GlobPathMatch =
                match globs with
                | [] -> GlobPathUnmatched
                | glob :: rest ->
                    match glob.matchOf candidate with
                    | GlobPathMatched -> GlobPathMatched
                    | GlobPathUnmatched -> matchAny rest candidate

            let rec choiceOf (candidate : KitRelativePath) : KitSendChoice =
                match matchAny this.sendExclude candidate, candidate.parentOpt with
                | GlobPathMatched, _ -> LeftOutOfSend
                | GlobPathUnmatched, Some folder -> choiceOf folder
                | GlobPathUnmatched, None -> SentWithKit

            match path.pathRole with
            | KitAppCachePath -> LeftOutOfSend
            | KitContentPath -> choiceOf path

        /// The entries that travel, in the tree's order (spec 0050 S6).
        member this.sentEntries (tree : KitTreeEntry list) : KitTreeEntry list =
            tree
            |> List.choose (fun entry ->
                match this.sendChoiceOf entry.path with
                | SentWithKit -> Some entry
                | LeftOutOfSend -> None)

    type KitName with

        /// The one place `.zip` is written: a kit is sent as its file stem's zip (spec 0050 S6; 001 §H).
        member this.zipFileName : FileName = this.fileStem.withExtension (FileExtension ".zip")
