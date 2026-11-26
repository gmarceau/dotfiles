#!/usr/bin/env -S uv run --quiet
# /// script
# requires-python = ">=3.11"
# dependencies = ["plumbum", "pydash"]
# ///
"""Fail when a word appears anywhere a push of a git ref would publish it.

Usage: forbidden-word-audit.py WORD [REF]      (REF defaults to HEAD)

Scans everything reachable from REF, never the working folder, so
untracked and ignored files do not count:
  tree  every file name and file of REF's tree, symlinks as their target path
  diff  every file name and added or removed line in the history of REF
  meta  author and committer names and emails, commit subjects and bodies

The word matches whole and case-insensitively: letters, digits and "_"
are word characters, so a longer word that contains it does not count.
The word is an argument, so this script never matches itself.

Prints one line per hit, "KIND LOCATION: TEXT", and a count per kind on
stderr. Exits 0 with no hit, 1 with a hit, 2 on a usage or git error.
"""

import re
import sys
from dataclasses import dataclass
from typing import Literal

import pydash
from plumbum import ProcessExecutionError, local

Kind = Literal["tree", "diff", "meta"]


@dataclass(frozen=True)
class Hit:
    """One place where the word appears."""

    kind: Kind
    location: str  # "path:line" for tree, "commit path" for diff, "commit field" for meta
    text: str

    def format(self) -> str:
        """Return the hit as "KIND LOCATION: TEXT"."""
        return f"{self.kind} {self.location}: {self.text}"


def word_pattern(word: str) -> re.Pattern:
    """Return a case-insensitive pattern that matches WORD as a whole word."""
    return re.compile(rf"(?<!\w){re.escape(word)}(?!\w)", re.IGNORECASE | re.ASCII)


def tree_hits(repo: str, word: str, ref: str) -> list[Hit]:
    """Return the hits in the file names and files of REF's tree, binary
    files included.

    Uses `git grep -a -i -w -F`; its exit status 1 means no hit. `git grep`
    skips symlinks, so their target paths are read with `git ls-tree`."""
    git = local["git"]["-C", repo]
    output = git("grep", "-z", "-n", "-a", "-i", "-w", "-F", "-e", word, ref, retcode=(0, 1))
    hits = []
    for line in pydash.compact(output.split("\n")):  # not splitlines: it also splits on \f and \r
        path, number, text = line.removeprefix(f"{ref}:").split("\0", 2)
        hits.append(Hit("tree", f"{path}:{number}", text))
    pattern = word_pattern(word)
    for entry in pydash.compact(git("ls-tree", "-r", "-z", ref).split("\0")):
        info, path = entry.split("\t", 1)
        mode, _, blob = info.split()
        if pattern.search(path):
            hits.append(Hit("tree", path, "(file name)"))
        target = git("cat-file", "blob", blob) if mode == "120000" else ""
        if pattern.search(target):
            hits.append(Hit("tree", f"{path} (symlink)", target))
    return hits


def diff_hits(repo: str, pattern: re.Pattern, ref: str) -> list[Hit]:
    """Return the hits in the file names and the added and removed lines
    of `git log -p REF`, binary files included. Without renames, a renamed
    file shows both names; a merge shows its diff against its first parent,
    so lines added while resolving a conflict count."""
    output = local["git"]("-C", repo, "-c", "core.quotePath=false", "log", "-p", "--no-color",
                          "--no-ext-diff", "--no-renames", "--text", "--diff-merges=first-parent",
                          "--src-prefix=a/", "--dst-prefix=b/", "--format=commit %h", ref)
    hits = []
    commit = path = None
    in_header = False
    for line in output.split("\n"):  # not splitlines: it also splits on \f and \r
        if line.startswith("commit "):
            commit = line.removeprefix("commit ")
        elif line.startswith("diff --git "):
            # "diff --git a/PATH b/PATH": both paths are the same without renames.
            names = line.removeprefix("diff --git ")
            path = names[2:2 + (len(names) - 5) // 2]
            in_header = True
            if pattern.search(path):
                hits.append(Hit("diff", f"{commit} {path}", "(file name)"))
        elif line.startswith("@@"):
            in_header = False
        elif not in_header and line.startswith(("+", "-")) and pattern.search(line[1:]):
            hits.append(Hit("diff", f"{commit} {path}", line))
    return hits


def meta_hits(repo: str, pattern: re.Pattern, ref: str) -> list[Hit]:
    """Return the hits in each commit's author and committer names and
    emails, subject and body, for every commit reachable from REF."""
    fields = ["author-name", "author-email", "committer-name", "committer-email"]
    output = local["git"]("-C", repo, "log", "--format=%h%x00%an%x00%ae%x00%cn%x00%ce%x00%B%x1e", ref)
    hits = []
    for record in pydash.compact([record.strip("\n") for record in output.split("\x1e")]):
        commit, *values, message = record.split("\x00")
        for field, value in zip(fields, values):
            if pattern.search(value):
                hits.append(Hit("meta", f"{commit} {field}", value))
        for line in message.splitlines():
            if pattern.search(line):
                hits.append(Hit("meta", f"{commit} message", line))
    return hits


def audit(repo: str, word: str, ref: str) -> list[Hit]:
    """Return all hits of WORD in REF: tree, then diff, then meta."""
    pattern = word_pattern(word)
    return pydash.flatten([tree_hits(repo, word, ref),
                           diff_hits(repo, pattern, ref),
                           meta_hits(repo, pattern, ref)])


def main(argv: list[str]) -> int:
    """Run the audit on the current repository and return the exit code."""
    if len(argv) not in (1, 2):
        print(__doc__.splitlines()[2], file=sys.stderr)
        return 2
    word, ref = argv[0], (argv[1:] or ["HEAD"])[0]
    try:
        top = local["git"]("rev-parse", "--show-toplevel").strip()
        hits = audit(top, word, ref)
    except ProcessExecutionError as error:
        print(error.stderr.strip(), file=sys.stderr)
        return 2
    for hit in hits:
        print(hit.format())
    counts = pydash.count_by(hits, "kind")
    print(", ".join(f"{kind}: {counts.get(kind, 0)}" for kind in ("tree", "diff", "meta")),
          file=sys.stderr)
    return 1 if hits else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
