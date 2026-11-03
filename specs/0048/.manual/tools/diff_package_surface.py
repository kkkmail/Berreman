"""The public names the shipped packages lost between two refs, and where renames.tsv or a row note accounts for each (0048 E044).

Reads every tracked .fs of the packed projects (GeneratePackageOnBuild) at BASE and at HEAD from git, collects the
public declarations a caller can name - module-level lets, types, union cases, record fields and members - keyed by
file, enclosing module or type, kind and name, and prints every key at BASE that is gone at HEAD. Each is matched
against the `old` column of renames.tsv and against the "Declarations deleted" section of the notes of the rows
that name a package. A textual reading, not a compiler's: it lists candidates for a person to account for.

    python specs/0048/.manual/tools/diff_package_surface.py 92c11f7 HEAD
"""
import csv, json, re, subprocess, sys

PACKAGES = ("Berreman/Berreman/", "Berreman/OpticalModel/", "Berreman/OpticalProperties/", "Berreman/Analytics/")
QUEUE = "specs/0048/.manual/002-elevation-ledger.jsonl"
RENAMES = "specs/0048/.manual/renames.tsv"
MODULE = re.compile(r"^(\s*)module\s+(?:rec\s+)?([\w.]+)\s*(=)?")
TYPE = re.compile(r"^(\s*)(?:type|and)\s+(?:\[<[^>]*>\]\s*)?(private\s+|internal\s+)?([A-Za-z_]\w*)")
LET = re.compile(r"^(\s*)let\s+(?:inline\s+|rec\s+|mutable\s+)*(private\s+|internal\s+)?(?:inline\s+)?(\(?[^\s(:=<]+\)?)")
MEMBER = re.compile(r"^\s*(?:static\s+)?(?:member|abstract|override|default)\s+(private\s+|internal\s+)?(?:inline\s+)?(?:\w+\.)?([A-Za-z_]\w*)")
CASE = re.compile(r"^\s*\|\s*([A-Z]\w*)\s*(?:of\b.*)?$")
FIELD = re.compile(r"^\s*(?:mutable\s+)?([a-z_]\w*)\s*:\s*\S")


def git(*args):
    return subprocess.run(["git", *args], capture_output=True, check=True).stdout.decode("utf-8-sig")


def surface(ref):
    found = set()
    for path in git("ls-tree", "-r", "--name-only", ref, "--", *PACKAGES).splitlines():
        if not path.endswith(".fs"):
            continue
        stack = []  # (indent, name, kind, private, bodyIndent)
        for raw in git("show", f"{ref}:{path}").split("\n"):
            line = raw.split("//")[0].rstrip()
            if not line.strip() or line.strip().startswith(("(*", "*", "[<", "#")):
                continue
            indent = len(line) - len(line.lstrip())
            while stack and indent <= stack[-1][0] and not line.lstrip().startswith(("|", "}", "{", "with", "member", "static", "abstract", "override", "interface", "end")):
                stack.pop()
            hidden = any(s[3] for s in stack)
            owner = ".".join(s[1] for s in stack)
            if m := MODULE.match(line):
                stack.append((len(m.group(1)) if m.group(3) else -1, m.group(2), "module", " private " in line, 0))
                found.add((path, owner, "module", m.group(2))) if not hidden else None
                continue
            if m := TYPE.match(line):
                while stack and stack[-1][2] == "type" and len(m.group(1)) <= stack[-1][0]:
                    stack.pop()
                private = bool(m.group(2)) or hidden
                stack.append((len(m.group(1)), m.group(3), "type", private, 0))
                if not private:
                    found.add((path, owner, "type", m.group(3)))
                continue
            inType = bool(stack) and stack[-1][2] == "type"
            if m := MEMBER.match(line):
                if inType and not m.group(1) and not hidden:
                    found.add((path, owner, "member", m.group(2)))
                continue
            if m := LET.match(line):
                atModule = not stack or stack[-1][2] == "module" and (stack[-1][0] < 0 and indent == 0 or indent == stack[-1][0] + 4)
                if atModule and not m.group(2) and not hidden:
                    found.add((path, owner, "let", m.group(3).strip("()")))
                continue
            if inType and indent > stack[-1][0] and not hidden:
                if m := CASE.match(line):
                    found.add((path, owner, "case", m.group(1)))
                elif "->" not in line and "=" not in line and (m := FIELD.match(line)):
                    found.add((path, owner, "field", m.group(1)))
    return found


def main():
    base, head = sys.argv[1], sys.argv[2] if len(sys.argv) > 2 else "HEAD"
    rows = {r["id"]: r for r in map(json.loads, open(QUEUE, encoding="utf-8"))}
    deleted = {}
    for r in rows.values():
        if r.get("packages") and (m := re.search(r"Declarations? deleted:(.*?)(?=Public renames|$)", r.get("note", ""), re.S)):
            deleted[r["id"]] = m.group(1)
    renamed = {}
    for r in csv.DictReader(open(RENAMES, encoding="utf-8"), delimiter="\t"):
        renamed.setdefault(r["old"].split(" ")[0].split(".")[-1], []).append(r)
    before, after = surface(base), surface(head)
    afterNames = {(k[2], k[3]) for k in after}
    gone = sorted(before - after, key=lambda k: (k[0], k[1], k[2], k[3]))
    print(f"# public declarations of {', '.join(p.strip('/') for p in PACKAGES)}: {len(before)} at {base}, {len(after)} at {head}\n")
    print(f"# gone at {head} under their file and owner: {len(gone)}\n")
    print("| file | owner | kind | name | still declared elsewhere | renames.tsv | in a note's Declarations deleted |\n|---|---|---|---|---|---|---|")
    for path, owner, kind, name in gone:
        elsewhere = "yes" if (kind, name) in afterNames else "no"
        tsv = ", ".join(f"{r['row']}: {r['new']}" for r in renamed.get(name, [])) or "-"
        noted = ", ".join(row for row, text in deleted.items() if re.search(rf"\b{re.escape(name)}\b", text)) or "-"
        print(f"| `{path.removeprefix('Berreman/')}` | `{owner}` | {kind} | `{name}` | {elsewhere} | {tsv} | {noted} |")


if __name__ == "__main__":
    main()
