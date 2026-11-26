namespace OpticalConstructor.Domain

/// Compares canonical entry files for the import preview (spec 0050 S3.2).
module EntryTextDiff =

    /// One line of an entry file, without its line ending (spec 0050 S3.2).
    type EntryTextLine =
        | EntryTextLine of string

        member this.value : string = let (EntryTextLine line) = this in line

    [<Struct>]
    type private EntryDiffOffset =
        | EntryDiffOffset of int

        member this.value : int = let (EntryDiffOffset offset) = this in offset
        member this.next : EntryDiffOffset = EntryDiffOffset (this.value + 1)
        static member zero : EntryDiffOffset = EntryDiffOffset 0

    [<Struct>]
    type private EntryCommonLineCount =
        | EntryCommonLineCount of int

        member this.value : int = let (EntryCommonLineCount count) = this in count
        member this.next : EntryCommonLineCount = EntryCommonLineCount (this.value + 1)
        static member zero : EntryCommonLineCount = EntryCommonLineCount 0

    /// A kept line, a removal from the held file, or an addition from the incoming file (spec 0050 S3.2).
    type EntryDiffLine =
        | SameEntryLine of EntryTextLine
        | RemovedEntryLine of EntryTextLine
        | AddedEntryLine of EntryTextLine

        /// Keeps common ends and the middle's longest common subsequence, emitting removals before additions
        /// between kept lines; filtering out either change kind recovers the other side (spec 0050 S3.2).
        static member diffLines (held : EntryTextLine list) (incoming : EntryTextLine list) : EntryDiffLine list =
            let diffMiddle (heldMiddle : EntryTextLine list) (incomingMiddle : EntryTextLine list) : EntryDiffLine list =
                match heldMiddle, incomingMiddle with
                | [], lines -> lines |> List.map AddedEntryLine
                | lines, [] -> lines |> List.map RemovedEntryLine
                | _, _ ->
                    let heldLines = List.toArray heldMiddle
                    let incomingLines = List.toArray incomingMiddle
                    let incomingOffsets = incomingLines |> Array.mapi (fun offset _ -> EntryDiffOffset offset)
                    let lengths =
                        Array.scanBack (fun heldLine (below : EntryCommonLineCount array) ->
                            Array.scanBack (fun (offset : EntryDiffOffset) right ->
                                if heldLine = incomingLines[offset.value] then below[offset.next.value].next
                                else max below[offset.value] right) incomingOffsets EntryCommonLineCount.zero)
                            heldLines (Array.create (incomingLines.Length + 1) EntryCommonLineCount.zero)

                    let rec walk (heldOffset : EntryDiffOffset) (incomingOffset : EntryDiffOffset)
                        (removedReversed : EntryDiffLine list) (addedReversed : EntryDiffLine list)
                        (diffReversed : EntryDiffLine list) : EntryDiffLine list =
                        if heldOffset.value = heldLines.Length && incomingOffset.value = incomingLines.Length then
                            List.rev (addedReversed @ removedReversed @ diffReversed)
                        elif heldOffset.value = heldLines.Length then
                            walk heldOffset incomingOffset.next removedReversed
                                (AddedEntryLine incomingLines[incomingOffset.value] :: addedReversed) diffReversed
                        elif incomingOffset.value = incomingLines.Length then
                            walk heldOffset.next incomingOffset
                                (RemovedEntryLine heldLines[heldOffset.value] :: removedReversed) addedReversed diffReversed
                        elif heldLines[heldOffset.value] = incomingLines[incomingOffset.value] then
                            let kept = SameEntryLine heldLines[heldOffset.value]
                            walk heldOffset.next incomingOffset.next [] [] (kept :: (addedReversed @ removedReversed @ diffReversed))
                        elif lengths[heldOffset.next.value][incomingOffset.value] >= lengths[heldOffset.value][incomingOffset.next.value] then
                            walk heldOffset.next incomingOffset
                                (RemovedEntryLine heldLines[heldOffset.value] :: removedReversed) addedReversed diffReversed
                        else
                            walk heldOffset incomingOffset.next removedReversed
                                (AddedEntryLine incomingLines[incomingOffset.value] :: addedReversed) diffReversed

                    walk EntryDiffOffset.zero EntryDiffOffset.zero [] [] []

            let rec keepSuffix (heldReversed : EntryTextLine list) (incomingReversed : EntryTextLine list)
                (same : EntryDiffLine list) : EntryDiffLine list =
                match heldReversed, incomingReversed with
                | heldLine :: heldRest, incomingLine :: incomingRest when heldLine = incomingLine ->
                    keepSuffix heldRest incomingRest (SameEntryLine heldLine :: same)
                | _, _ -> diffMiddle (List.rev heldReversed) (List.rev incomingReversed) @ same

            let rec keepPrefix (heldRest : EntryTextLine list) (incomingRest : EntryTextLine list)
                (sameReversed : EntryDiffLine list) : EntryDiffLine list =
                match heldRest, incomingRest with
                | heldLine :: heldTail, incomingLine :: incomingTail when heldLine = incomingLine ->
                    keepPrefix heldTail incomingTail (SameEntryLine heldLine :: sameReversed)
                | _, _ -> List.rev sameReversed @ keepSuffix (List.rev heldRest) (List.rev incomingRest) []

            keepPrefix held incoming []
