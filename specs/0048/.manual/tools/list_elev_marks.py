"""Every [<ElevationBoundary>] and [<NumericCarrier>] the branch gained between two refs (0048 002 section 6, E044).

Reads every tracked .fs under Berreman/ at BASE and at HEAD from git (never the disk), lists each mark with
its kind, its verbatim reason and the declaration under it, and traces each HEAD mark to the oldest commit in
BASE..HEAD that added its attribute line. The row comes from that commit's message ("Exxx closed"); the by-hand
commit of 006 is labelled as such. It then checks the mark against that row's note in the queue: the note must
name the declaration. Modelled on STL specs/0219/.manual/tools/List-RingMarks.py; re-run to prove.

    python specs/0048/.manual/tools/list_elev_marks.py 92c11f7 HEAD
"""
import collections, json, re, subprocess, sys

QUEUE = "specs/0048/.manual/002-elevation-ledger.jsonl"
TESTS = ("BerremanTests", "OpticalConstructor.Tests", "OpticalConstructor.Ui.Tests")
ATTR = re.compile(r"\[<\s*(?:[^>\]]*;\s*)?(?:[\w.]+\.)?(ElevationBoundary|NumericCarrier)(?:Attribute)?\s*\(", re.S)
STR = re.compile(r'"((?:[^"\\]|\\.)*)"')
KIND = re.compile(r"nameof\s+(?:\w+\.)*(\w+)")
ROW = re.compile(r"\b(E\d{3}) (?:closed|blocked)")


def git(*args):
    return subprocess.run(["git", *args], capture_output=True, check=True).stdout.decode("utf-8-sig")


def argsEnd(text, i):
    depth, inStr = 1, False
    while i < len(text) and depth:
        c = text[i]
        if inStr:
            if c == "\\":
                i += 1
            elif c == '"':
                inStr = False
        elif c == '"':
            inStr = True
        elif c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
        i += 1
    return i


def declName(decl):
    d = re.sub(r"^(let|type|member|module)\s+(private\s+|inline\s+|rec\s+)*", "", decl)
    return re.split(r"[\s(<:=]", d, maxsplit=1)[0]


def project(path):
    parts = path.split("/")
    return parts[2] if parts[1] == "OpticalConstructor" else parts[1]


def marks(ref):
    found = []
    for path in git("ls-tree", "-r", "--name-only", ref, "--", "Berreman").splitlines():
        if not path.endswith(".fs"):
            continue
        text = git("show", f"{ref}:{path}")
        if "ElevationBoundary" not in text and "NumericCarrier" not in text:
            continue
        for m in ATTR.finditer(text):
            lineStart = text.rfind("\n", 0, m.start()) + 1
            if "//" in text[lineStart:m.start()]:
                continue
            end = argsEnd(text, m.end())
            args = text[m.end():end - 1]
            decl = next((s.strip() for s in text[text.find(">]", end) + 2:].split("\n")
                         if s.strip() and not s.strip().startswith(("[<", "//"))), "")
            strs, kind = STR.findall(args), KIND.search(args)
            found.append({
                "file": path,
                "project": project(path),
                "line": text.count("\n", 0, m.start()) + 1,
                "attrLine": text[lineStart:text.find("\n", m.start())].strip(),
                "attr": m.group(1),
                "kind": kind.group(1) if kind else "",
                "reason": " ".join((strs[-1] if strs else args.strip()).split()),
                "decl": decl,
                "private": " private " in f" {decl} ",
            })
    return found


def origin(base, head, mark):
    commits = git("log", "--format=%h", "-S", mark["attrLine"], f"{base}..{head}", "--", mark["file"]).split()
    if not commits:
        return "?", "?"
    commit = commits[-1]
    body = git("log", "-1", "--format=%B", commit)
    if body.lstrip().split("\n", 1)[-1].lstrip().startswith("by hand"):
        return "by hand (006)", commit
    row = ROW.search(body)
    return (row.group(1) if row else "?"), commit


def main():
    base, head = sys.argv[1], sys.argv[2] if len(sys.argv) > 2 else "HEAD"
    notes = {r["id"]: r.get("note", "") for r in map(json.loads, open(QUEUE, encoding="utf-8"))}
    before, after = marks(base), marks(head)
    key = lambda m: (m["file"], m["attr"], m["decl"], m["reason"])
    beforeKeys, afterKeys = {key(m) for m in before}, {key(m) for m in after}
    count = lambda ms: collections.Counter((m["project"], m["attr"]) for m in ms)
    cb, ca = count(before), count(after)

    print(f"# marks by project, {base} -> {head}\n")
    print("| project | attribute | at base | at head |\n|---|---|---|---|")
    for proj, attr in sorted(set(cb) | set(ca)):
        print(f"| {proj} | {attr} | {cb[(proj, attr)]} | {ca[(proj, attr)]} |")

    added = [m for m in after if key(m) not in beforeKeys]
    removed = [m for m in before if key(m) not in afterKeys]
    product = [m for m in added if m["project"] not in TESTS]
    for attr in ("ElevationBoundary", "NumericCarrier"):
        rows = sorted(((origin(base, head, m), m) for m in product if m["attr"] == attr),
                      key=lambda om: (om[0][1] != "?" and git("log", "-1", "--format=%ct", om[0][1]), om[1]["file"], om[1]["line"]))
        print(f"\n# {attr}, added: {len(rows)}\n")
        print("| # | row | commit | file:line | declaration | kind | in the row's note | reason, verbatim |\n|---|---|---|---|---|---|---|---|")
        for i, ((row, commit), m) in enumerate(rows, 1):
            name = declName(m["decl"])
            named = "yes" if re.search(rf"\b{re.escape(name)}\b", notes.get(row, "")) else "NO"
            shown = f"`{name}`" + (" (private)" if m["private"] else "")
            print(f"| {i} | {row} | `{commit}` | `{m['file'].removeprefix('Berreman/')}:{m['line']}` | {shown} | {m['kind'] or '-'} | {named} | {m['reason'].replace('|', '/')} |")

    others = [m for m in added if m["project"] in TESTS]
    print(f"\n# added in the test projects: {len(others)}\n")
    for m in others:
        (row, commit) = origin(base, head, m)
        print(f"- `{m['file']}:{m['line']}` {m['attr']} on `{declName(m['decl'])}` ({row}, `{commit}`): {m['reason']}")
    print(f"\n# at base, gone at head: {len(removed)}\n")
    for m in removed:
        print(f"- `{m['file']}:{m['line']}` {m['attr']} {m['kind']} on `{declName(m['decl'])}`: {m['reason']}")


if __name__ == "__main__":
    main()
