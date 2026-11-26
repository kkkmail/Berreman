# Step 0215 — impl log

## Progress

- [x] (1) `Kit.fs`: appended `ItemDial` (with `parameterAxes`), `ItemRevisionRecord` and `Item` (with
  `libraryReferences` and `tryFindParameter`) after `Parameter`, in the brief's order
- [x] (2) `KitItemTests.fs`: the two declared facts appended, and the built-in fixtures they share with step 213's
  fact lifted to module level
- [x] build, constructor-unit-tests, elevation and the fourteen invariants (development runs), plus a mutation check

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs` (+42 lines, appended at the end of the module):
  - `ItemDial` = `DialMounted of DialSign` | `FixedMounting`.
    - `member this.parameterAxes : ItemParameterAxis list` answers
      `[ { name = ParameterName "dialZero"; kind = AngleParameter; defaultValue = ParameterValue 0.0 } ]` for
      `DialMounted _` and `[]` for `FixedMounting`.
  - `ItemRevisionRecord = { revision : ItemRevision; parameters : Parameter list }`.
  - `Item = { id : ItemId; label : ItemLabel; model : ItemModelInstance; parameters : Parameter list; dial : ItemDial;
    photoOpt : KitRelativePath option; notes : ItemNotes; revision : ItemRevision; history : ItemRevisionRecord list }`.
    - `member this.libraryReferences : EntryReference list` is
      `[ model.entryReferenceOpt; model.bindingReferenceOpt ] |> List.choose id |> List.distinct`.
    - `member this.tryFindParameter (name : ParameterName) : Parameter option` is `List.tryFind` by name.

  Each type has a doc comment of one or two lines citing spec 0050 S7.1. `libraryReferences` has a two-line doc.
  `tryFindParameter` has none: its name and signature say it all (AGENTS.md: delete a comment that repeats an
  identifier). No `open` was needed: `ItemModels` (`DialSign`, `ItemParameterAxis`, `ParameterName`) and
  `LibraryEntryFiles` (`EntryReference`) are already open.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitItemTests.fs` (+94 / −7):
  - The two declared facts are appended.
  - Five fixtures moved from inside step 213's fact `an item's model is referenced as the library snapshots it` to
    module-level private values: `presetAtLatest`, `sourceModel`, `polarizerModel`, `stackModel`, `sampleVersion`
    and `sampleAtVersion`. The new fact needs the same fixtures, and AGENTS.md says "reuse, never copy". The names
    and definitions are unchanged, so step 213's assertions are untouched, word for word.

## Decisions

1. **`libraryReferences` keeps the brief's order**: the model's exact version first, then the binding when there
   is one. `List.distinct` removes a repeat and keeps the first occurrence.
2. **The dial zero's axis is written once, inside `ItemDial.parameterAxes`.** The brief names no shared constant,
   and the brief allows no other member, so no `ItemParameterAxis.dialZero` member was added.
3. **`tryFindParameter` answers the first parameter of that name.** Names are unique within an item by
   construction (step 216 generates them from a model's axes). Validation is not this step's concern.
4. **The tests write their items as records, as the brief says.** No `ItemDraft` exists until step 216.
   - CPL1 is the circular polarizer at version 1. It is written in full, so the fact also shows that the record
     holds every field:
     - a dial;
     - a photo `data/photos/CPL1.jpg`;
     - notes;
     - revision 2;
     - one history record at revision 1, whose `c2.retardance` differs from the current one.
   - `tryFindParameter` finds `dialZero` and the *current* `c2.retardance`, and answers `None` for `nothing`.
   - The source (`heNe633`), ideal-polarizer (`polarizerLp`) and stack (glass plate v1) items over the shipped
     built-in model versions each answer exactly `[ model version; binding ]`.
5. **One assertion beyond the brief's list, inside the declared identity:** a stack item whose sample shares the
   model's GUID and version. Its binding is then the same `EntryVersionReference` as its model, and
   `libraryReferences` names it once.
   - This is contrived, because a real sample never shares an item model's GUID.
   - It is the only input that exercises "with duplicate references removed". Without it, removing `List.distinct`
     would go unnoticed.
   - A one-line comment says so. No new test identity was added.

## Testing state

These are development runs; the gate engine gives the verdict (Invariant 6).

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`): exit 0, 0 errors.
  - The first build showed 2 `SYSLIB0051` warnings from the vendored `MathNetNumerics/Numerics/Exceptions.cs`.
    They appear whenever that project recompiles (step 214 recorded the same) and are not in this step's files.
  - The final build over the restored source: 0 warnings, 0 errors (`0215-dev-build-final.log`).
- **KitItemTests:** 8 / 8 passed (six from steps 213–214 and the two new facts). The class was re-run after the
  final build.
- **constructor-unit-tests**, whole suite: 1296 passed, 0 failed, 0 skipped, in 1 min 04 s against a 180 s
  estimate. The count rose from 1294 by 2, as the acceptance requires.
- **Mutation check** (`0215-dev-mutations.txt`). Each mutation was rebuilt and `KitItemTests` re-run; each failed
  exactly one fact (7 / 8 passed). The source was then restored byte for byte (`diff -q` against the saved copy).
  - Drop `List.distinct`: the references fact fails.
  - Put the binding before the model: the references fact fails.
  - `DialMounted _ -> []`: the dial fact fails.
  - `tryFindParameter` → `List.tryHead`: the references fact fails.
- **elevation** (`ASG_ARC_BASE_COMMIT` = the `.arc-base-json` origin `f395597`, ELEV004 excluded):
  `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 192s` against a 600 s deadline.
- **The fourteen `invariant-s22-*` gates**, run through `section_invariants --find-nothing` as the gates are: each
  answers `INVARIANT HOLDS`.
- **LF endings:** neither edited file has a CR byte, and `--numstat` equals `--ignore-cr-at-eol --numstat`.

## Artifacts

All in `specs/0050/.artifacts/`:

- `0215-dev-build.log`: the first build;
- `0215-dev-build-final.log`: the build over the restored source after the mutation check;
- `0215-dev-constructor-unit-tests.log`: the whole-suite run;
- `0215-dev-mutations.txt`: the four mutations, as the script that applied them;
- `0215-dev-mutation-build.log`: the last mutation's test-project build;
- `0215-dev-elevation.log`: the ELEV gate run;
- `0215-dev-invariants.log`: the fourteen invariant runs.

## Gotchas

- **`Item` repeats record labels from types declared earlier:**
  - `id` (`KitFile`);
  - `notes` (`KitFile`);
  - `parameters` and `revision` (`ItemRevisionRecord`).

  F# infers an unannotated record expression from its labels. The tests annotate every record value
  (`let revisionOne : ItemRevisionRecord = …`, `let cpl1 : Item = …`), and later code should too. No existing file
  needed a change: the solution built clean.
- **`ItemDial.parameterAxes` has the same name as `ItemModelForm.parameterAxes`.** Both are intrinsic members, so
  ELEV019 (which applies to extension members) does not fire. Step 216 concatenates the model's axes and then the
  dial's.
- **No operator note was in flight for this attempt.**
