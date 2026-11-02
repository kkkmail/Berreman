"""Records the elevation findings of a full gate run as Elevation/elevation-known-findings.json (spec 0048 .manual/002).

Run once, over the SARIF reports of `Elevation/Run-ElevationGate.ps1 -All -ExcludeRules ELEV004`, when the gates were
set up. The list is STL's format and only shrinks afterwards: a fix removes its entry by hand, in the same change.
Re-recording it to make the gate green is exactly what it must never be used for.

    python -I specs/0048/.manual/tools/record_known_findings.py <reports folder> [--stats]
"""
import collections
import glob
import json
import os
import re
import sys

RULING = "spec 0048 .manual/002"
EXCLUDED = {"ELEV004"}


def key_text(text):
    return re.sub(r"\s+", " ", re.sub(r"[^\x20-\x7E]", "", text)).strip()


def findings(reports):
    seen = set()
    for path in sorted(glob.glob(os.path.join(reports, "*.sarif"))):
        project = os.path.basename(path)[: -len(".sarif")]
        with open(path, encoding="utf-8-sig") as f:
            run = json.load(f)["runs"][0]
        for r in run.get("results", []):
            loc = r["locations"][0]["physicalLocation"]
            item = (r["ruleId"], loc["artifactLocation"]["uri"], loc["region"]["startLine"],
                    loc["region"]["startColumn"], r["message"]["text"])
            if item in seen:
                continue
            seen.add(item)
            yield project, item


def main():
    reports = sys.argv[1]
    all_items = list(findings(reports))
    enforced = [(p, i) for p, i in all_items if i[0] not in EXCLUDED and not re.match(r"^ELEV\d+T$", i[0])]
    counts = collections.Counter((i[0], i[1], key_text(i[4])) for _, i in enforced)
    entries = [{"rule": r, "file": f, "message": m, "count": c, "ruling": RULING}
               for (r, f, m), c in sorted(counts.items(), key=lambda kv: (kv[0][1], kv[0][0], kv[0][2]))]
    doc = {
        "version": 1,
        "about": "Findings the elevation gate reports but does not fail on: the tree's findings when the gates were set "
                 "up (spec 0048 .manual/001), each waiting on the ledger in spec 0048 .manual/002. An entry is removed "
                 "in the change that fixes its finding; a stale entry fails the gate, so the list only shrinks.",
        "findings": entries,
    }
    # One entry per line, so a fix deletes one line (or lowers one count) in the diff.
    with open("Elevation/elevation-known-findings.json", "w", encoding="utf-8", newline="\n") as f:
        f.write('{\n "version": 1,\n "about": ' + json.dumps(doc["about"]) + ',\n "findings": [\n')
        f.write(",\n".join("  " + json.dumps(e, ensure_ascii=True) for e in entries))
        f.write("\n ]\n}\n")
    print(f"recorded {len(entries)} entries covering {sum(counts.values())} findings")

    if "--stats" in sys.argv:
        by_rule = collections.Counter(i[0] for _, i in all_items)
        print("ALL BY RULE", sorted(by_rule.items()))
        by_project_rule = collections.defaultdict(collections.Counter)
        for p, i in all_items:
            by_project_rule[p][i[0]] += 1
        for p in sorted(by_project_rule):
            print("PROJECT", p, sum(by_project_rule[p].values()), dict(sorted(by_project_rule[p].items())))
        by_file = collections.Counter(i[1] for _, i in enforced)
        for f, c in by_file.most_common(25):
            print("FILE", c, f)
        examples = collections.defaultdict(list)
        for _, i in enforced:
            if len(examples[i[0]]) < 3:
                examples[i[0]].append(f"{i[1]}:{i[2]} - {key_text(i[4])}")
        for r in sorted(examples):
            for e in examples[r]:
                print("EXAMPLE", r, e)


if __name__ == "__main__":
    main()
