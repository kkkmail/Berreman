"""Builds the ELEV ledger, specs/0048/.manual/002-elevation-ledger.jsonl, from the recorded findings (spec 0048 .manual/002).

The rows are DERIVED, never listed by hand, the way STL builds its elevation queues (STL specs/0229/.manual/tools/
New-ElevLoopQueue.py). The input is Elevation/elevation-known-findings.json, the tree's enforced findings when the gates
were set up; the STL tool reads a fresh sweep instead, and here the recorded list IS that sweep. Every finding sits in a
file, every file in the project whose folder holds it, and every project in a tier. Tiers drain in dependency order,
projects in ProjectReference order inside a tier, and each project's files in F# compile order. A tier's files are cut
into rows of at most --budget findings; a file over the budget is a row of its own, and a row never spans two tiers.

Run it once, when the ledger is made. A step flips its own row afterwards and never regenerates the file.

    python -I specs/0048/.manual/tools/build_elev_ledger.py [--budget N] [--out ledger.jsonl]
"""
import collections
import json
import os
import re
import sys
import xml.etree.ElementTree as ET

REPO = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "..", ".."))
KNOWN = "Elevation/elevation-known-findings.json"
SOLUTION = "Berreman/Berreman.slnx"
PLAN = "002-elevation-plan.md"

OC = "Berreman/OpticalConstructor/OpticalConstructor"
TIERS = [
    ("Solver", ["Berreman/OpticalModel/OpticalModel.fsproj", "Berreman/Berreman/Berreman.fsproj",
                "Berreman/OpticalProperties/OpticalProperties.fsproj"]),
    ("Analytics", ["Berreman/Analytics/Analytics.fsproj"]),
    ("Controls", [f"{OC}.Controls/OpticalConstructor.Controls.fsproj"]),
    ("Domain", [f"{OC}.Domain/OpticalConstructor.Domain.fsproj"]),
    ("Services", [f"{OC}.Optimization/OpticalConstructor.Optimization.fsproj",
                  f"{OC}.Storage/OpticalConstructor.Storage.fsproj",
                  f"{OC}.Seeding/OpticalConstructor.Seeding.fsproj"]),
    ("Ui", [f"{OC}.Ui/OpticalConstructor.Ui.fsproj"]),
    ("TestWindows", [f"{OC}.TestWindows/OpticalConstructor.TestWindows.fsproj",
                     f"{OC}.TestWindows.App/OpticalConstructor.TestWindows.App.fsproj"]),
    ("App", [f"{OC}.App/OpticalConstructor.App.fsproj"]),
]

# The test gates under .gates/, by the suite each one runs.
SUITE_GATES = {
    "Berreman/BerremanTests/BerremanTests.fsproj": ["unit-tests"],
    f"{OC}.Tests/OpticalConstructor.Tests.fsproj": ["constructor-unit-tests"],
    f"{OC}.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj": ["ui-smoke", "ui-tests"],
}

# Which of the plan's rulings (section 2) bind a row: the UI ids, the duplicate names, the numbers, the heads.
UI_ID_FILES = ("/UiIds.fs", "/TableAndElementRotationView.fs", "/OpticalConstructor.App/Program.fs")
NAME_RULES = {"ELEV014", "ELEV015", "ELEV016", "ELEV019"}
NUMERIC_TIERS = {"Solver", "Analytics", "Domain", "Services"}
HEAD_TIERS = {"TestWindows", "App"}

DONE_WHEN = ("every entry of `files` deleted from Elevation/elevation-known-findings.json and no new finding (the "
             "elevation gate green), the build green with zero warnings from our code, and every gate in `gates` green "
             f"at its count ({PLAN} section 5)")


def norm(path):
    return os.path.normpath(path).replace("\\", "/")


def rel(path):
    return norm(os.path.relpath(path, REPO))


def read_project(project):
    """(compile items in order, project references, packed on build)."""
    full = os.path.join(REPO, project)
    folder = os.path.dirname(full)
    compiles, refs, packed = [], [], False
    for el in ET.parse(full).getroot().iter():
        tag = el.tag.split("}")[-1]
        if tag == "GeneratePackageOnBuild" and (el.text or "").strip().lower() == "true":
            packed = True
        include = el.attrib.get("Include")
        if not include:
            continue
        if tag == "Compile":
            compiles.append(rel(os.path.join(folder, include)))
        elif tag == "ProjectReference":
            refs.append(rel(os.path.join(folder, include)))
    return compiles, refs, packed


def solution_projects():
    folder = os.path.dirname(os.path.join(REPO, SOLUTION))
    text = open(os.path.join(REPO, SOLUTION), encoding="utf-8").read()
    return [rel(os.path.join(folder, p)) for p in re.findall(r'Project Path="([^"]+\.fsproj)"', text)]


def topo_order(projects, refs):
    order, state = [], {}

    def visit(p):
        if state.get(p):
            return
        state[p] = 1
        for q in sorted(refs.get(p, [])):
            if q in projects:
                visit(q)
        order.append(p)

    for p in sorted(projects):
        visit(p)
    return order


def main():
    argv = sys.argv[1:]
    budget, out = 120, os.path.join(REPO, "specs", "0048", ".manual", "002-elevation-ledger.jsonl")
    if "--budget" in argv:
        i = argv.index("--budget"); budget = int(argv[i + 1]); del argv[i:i + 2]
    if "--out" in argv:
        i = argv.index("--out"); out = os.path.abspath(argv[i + 1]); del argv[i:i + 2]

    with open(os.path.join(REPO, KNOWN), encoding="utf-8") as f:
        known = json.load(f)["findings"]
    enforced = collections.defaultdict(collections.Counter)            # file -> rule -> count
    for e in known:
        enforced[e["file"]][e["rule"]] += e["count"]

    projects = solution_projects()
    compiles, refs, packed = {}, {}, {}
    for p in projects:
        compiles[p], refs[p], packed[p] = read_project(p)
    order = topo_order(set(projects), refs)
    position = {p: i for i, p in enumerate(order)}

    tier_of = {p: t for t, ps in TIERS for p in ps}
    tier_index = {t: i for i, (t, _) in enumerate(TIERS)}
    for p, t in tier_of.items():
        for q in refs[p]:
            if q in tier_of and tier_index[tier_of[q]] > tier_index[t]:
                sys.exit(f"{p} ({t}) references {q} ({tier_of[q]}), a later tier")

    compiled_by = collections.defaultdict(list)
    for p in order:
        for f in compiles[p]:
            compiled_by[f].append(p)

    def owner_of(f):
        holders = [p for p in compiled_by.get(f, []) if f.startswith(os.path.dirname(p) + "/")]
        return max(holders, key=lambda p: len(os.path.dirname(p))) if holders else None

    owned = collections.defaultdict(list)
    for f in enforced:
        p = owner_of(f)
        if p not in tier_of:
            sys.exit(f"{f}: owned by {p}, which is in no tier")
        owned[p].append(f)

    dependents = collections.defaultdict(set)
    for p in projects:
        for q in refs[p]:
            dependents[q].add(p)

    def closure_up(start):
        seen, todo = set(start), list(start)
        while todo:
            for q in dependents.get(todo.pop(), ()):
                if q not in seen:
                    seen.add(q); todo.append(q)
        return seen

    rows = []

    def add(row):
        row["id"] = f"E{len(rows) + 1:03d}"
        row.setdefault("note", "")
        rows.append(row)

    add({"state": "open", "kind": "baseline", "tier": "-",
         "title": "Baseline: confirm the recorded findings are the tree's",
         "task": "Before any source edit, run elevation-full (Elevation/Run-ElevationGate.ps1 -All -ExcludeRules ELEV004) "
                 f"over the tree the spec starts from. It must report 0 new and 0 stale ({PLAN} section 6). A difference "
                 "is a blocked row that names it: the list is never re-recorded and this file is never regenerated.",
         "gates": ["build", "elevation-full"]})
    add({"state": "open", "kind": "pin", "tier": "-",
         "title": "Softellect.Sys 10.1.402.120 -> 10.1.402.136, STL's current slow ring (R4)",
         "task": "Move the Softellect.Sys PackageVersion in the root Directory.Build.targets to 10.1.402.136, which is in "
                 "the local ../Packages feed, and restore. Repair any API drift in the projects that reference it; a "
                 "caller moves to the new name and never re-wraps. Never build or bump STL. Every elevate row that adds "
                 f"the first [<ElevationBoundary>] or [<NumericCarrier>] to a project adds the reference ({PLAN} section 2 "
                 "R4).",
         "gates": ["build", "unit-tests", "constructor-unit-tests", "ui-smoke", "ui-tests"],
         "rulings": ["R4"]})

    for tier, members in TIERS:
        sequence = []
        for p in sorted((p for p in members if owned[p]), key=lambda p: position[p]):
            sequence += sorted(owned[p], key=lambda f: compiles[p].index(f) if f in compiles[p] else 10 ** 6)
        chunks, chunk, size = [], [], 0
        for f in sequence:
            n = sum(enforced[f].values())
            if chunk and size + n > budget:
                chunks.append(chunk); chunk, size = [], 0
            chunk.append(f); size += n
        if chunk:
            chunks.append(chunk)

        for chunk in chunks:
            rules = collections.Counter()
            for f in chunk:
                rules.update(enforced[f])
            owners_here = []
            for f in chunk:
                if owner_of(f) not in owners_here:
                    owners_here.append(owner_of(f))
            compilers = sorted({q for f in chunk for q in compiled_by.get(f, [])}, key=lambda q: position[q])
            affected = closure_up(compilers)
            suites = sorted(q for q in affected if q in SUITE_GATES)
            areas = [f"{os.path.basename(os.path.dirname(p))} {sum(1 for f in chunk if owner_of(f) == p)} of "
                     f"{len(owned[p])} files" for p in owners_here]
            rulings = ((["R1"] if any(f.endswith(UI_ID_FILES) for f in chunk) else [])
                       + (["R2"] if NAME_RULES & set(rules) else [])
                       + (["R3"] if tier in NUMERIC_TIERS else [])
                       + (["R5"] if tier in HEAD_TIERS else []))
            add({"state": "open", "kind": "elevate", "tier": tier,
                 "title": f"{'; '.join(areas)} - {sum(rules.values())} finding(s)",
                 "projects": owners_here,
                 "files": chunk,
                 "findings": dict(sorted(rules.items())),
                 "compiled_by": compilers,
                 "packages": sorted(q for q in compilers if packed[q]),
                 "suites": suites,
                 "gates": ["build", "elevation"] + [g for s in suites for g in SUITE_GATES[s]],
                 "rulings": rulings,
                 "done_when": DONE_WHEN})

    for title, task, gates in (
        ("Close PART A: elevation-full green over an empty known-findings list",
         "Elevation/elevation-known-findings.json keeps its shape with no finding in it, so every finding fails the "
         "gate from here on. elevation-full reports 0, and every test gate is green at its count.",
         ["build", "elevation-full", "constructor-unit-tests", "ui-smoke", "ui-tests"]),
        ("The lists, no new work",
         "Check the three lists against the row notes and the branch diff: every [<ElevationBoundary>] and "
         "[<NumericCarrier>] with its reason, every public rename of a shipped package (specs/0048/.manual/renames.tsv) "
         f"and every stored-format change ({PLAN} section 5). No build, no suite.",
         []),
    ):
        add({"state": "open", "kind": "closing", "tier": "-", "title": title, "task": task, "gates": gates})

    with open(out, "w", encoding="utf-8", newline="\n") as fh:
        for r in rows:
            fh.write(json.dumps(r, ensure_ascii=True) + "\n")

    total = sum(sum(c.values()) for c in enforced.values())
    elevate = [r for r in rows if r["kind"] == "elevate"]
    print(f"{len(rows)} rows ({len(elevate)} elevate), {total} enforced findings in {len(enforced)} files -> {rel(out)}")
    for tier, _ in TIERS:
        here = [r for r in elevate if r["tier"] == tier]
        print(f"  {tier:<12} {len(here):>3} row(s) {sum(sum(r['findings'].values()) for r in here):>6} finding(s)")


if __name__ == "__main__":
    main()
