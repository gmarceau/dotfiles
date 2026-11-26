"""Tests for forbidden-word-audit.py

Run: uv run --with pytest --with plumbum --with pydash pytest ~/dotfiles/bin/forbidden-word-audit_test.py

Each test builds a throwaway git repository in pytest's tmp_path. The
audited word is "zork", so this file never contains a word worth hiding.
"""

import importlib.util
from pathlib import Path

import pytest
from plumbum import local

WORD = "zork"


def load_script():
    """Import forbidden-word-audit.py by path; its name has a hyphen."""
    path = Path(__file__).with_name("forbidden-word-audit.py")
    spec = importlib.util.spec_from_file_location("forbidden_word_audit", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


audit = load_script()


@pytest.fixture
def repo(tmp_path):
    """Return a git repository in tmp_path with a clean author and one commit."""
    git = local["git"]["-C", str(tmp_path)]
    git("init", "-q", "-b", "main")
    git("config", "user.name", "Me")
    git("config", "user.email", "me@example.com")
    commit(tmp_path, {"README": "hello\n"}, "Initial commit")
    return tmp_path


def commit(repo, files, message="change", author_email="me@example.com", committer_email="me@example.com"):
    """Write FILES ({path: text}) in REPO and commit them; return the commit id."""
    for name, text in files.items():
        (repo / name).write_text(text)
    git = local["git"]["-C", str(repo)]
    git("add", "-A")
    with local.env(GIT_AUTHOR_EMAIL=author_email, GIT_COMMITTER_EMAIL=committer_email):
        git("commit", "-q", "-m", message)
    return git("rev-parse", "--short", "HEAD").strip()


def run_main(repo, *args):
    """Run main in REPO with ARGS; return the exit code."""
    with local.cwd(str(repo)):
        return audit.main(list(args))


def kinds(hits):
    return [hit.kind for hit in hits]


def test_clean_repo_passes_with_exit_0(repo):
    assert audit.audit(str(repo), WORD, "HEAD") == []
    assert run_main(repo, WORD) == 0


def test_word_in_a_tracked_file_fails_and_names_file_and_line(repo, capsys):
    commit(repo, {"notes.txt": "one\nsee zork here\n"})
    assert run_main(repo, WORD) == 1
    assert "tree notes.txt:2: see zork here" in capsys.readouterr().out


def test_substrings_of_longer_words_pass(repo):
    commit(repo, {"notes.txt": "zorkish bigzork zork_x xzork\n"}, "zorkish message")
    assert audit.audit(str(repo), WORD, "HEAD") == []


def test_match_is_case_insensitive(repo):
    commit(repo, {"notes.txt": "ZORK\n"})
    assert "tree" in kinds(audit.audit(str(repo), WORD, "HEAD"))


def test_word_with_hyphen_or_slash_around_it_fails(repo):
    commit(repo, {"a.txt": "zork-glossary\n", "b.txt": "code/zork\n"})
    hits = [hit for hit in audit.audit(str(repo), WORD, "HEAD") if hit.kind == "tree"]
    assert [hit.location for hit in hits] == ["a.txt:1", "b.txt:1"]


def test_word_only_in_a_deleted_line_of_an_old_commit_fails(repo):
    old = commit(repo, {"notes.txt": "zork\n"})
    commit(repo, {"notes.txt": "clean\n"})
    hits = audit.audit(str(repo), WORD, "HEAD")
    assert kinds(hits) == ["diff", "diff"]
    assert hits[0].location.endswith(" notes.txt")
    assert {hit.text for hit in hits} == {"-zork", "+zork"}
    assert any(hit.location.startswith(old) for hit in hits)


def test_word_in_author_email_fails(repo):
    sha = commit(repo, {"a.txt": "x\n"}, author_email="me@zork.co")
    assert audit.Hit("meta", f"{sha} author-email", "me@zork.co") in audit.audit(str(repo), WORD, "HEAD")


def test_word_in_committer_email_fails(repo):
    sha = commit(repo, {"a.txt": "x\n"}, committer_email="me@zork.co")
    assert audit.Hit("meta", f"{sha} committer-email", "me@zork.co") in audit.audit(str(repo), WORD, "HEAD")


def test_word_in_a_commit_message_fails(repo):
    sha = commit(repo, {"a.txt": "x\n"}, "Subject\n\nBody about zork")
    assert audit.audit(str(repo), WORD, "HEAD") == [audit.Hit("meta", f"{sha} message", "Body about zork")]


def test_symlink_whose_target_path_has_the_word_fails(repo):
    (repo / "link").symlink_to("/home/zork/file")
    commit(repo, {})
    assert "tree" in kinds(audit.audit(str(repo), WORD, "HEAD"))


def test_symlink_to_a_file_containing_the_word_passes(repo, tmp_path_factory):
    outside = tmp_path_factory.mktemp("outside") / "secret.txt"
    outside.write_text("zork\n")
    (repo / "link").symlink_to(outside)
    commit(repo, {})
    assert audit.audit(str(repo), WORD, "HEAD") == []


def test_untracked_and_ignored_files_are_not_scanned(repo):
    commit(repo, {".gitignore": "ignored.txt\n"})
    (repo / "ignored.txt").write_text("zork\n")
    (repo / "untracked.txt").write_text("zork\n")
    assert audit.audit(str(repo), WORD, "HEAD") == []


def test_audits_the_given_ref_not_head(repo):
    clean = commit(repo, {"a.txt": "x\n"})
    commit(repo, {"b.txt": "zork\n"})
    assert audit.audit(str(repo), WORD, clean) == []
    assert run_main(repo, WORD, clean) == 0


def test_file_name_with_the_word_fails(repo):
    commit(repo, {"zork-notes.md": "clean\n"})
    hits = audit.audit(str(repo), WORD, "HEAD")
    assert "tree" in kinds(hits) and "diff" in kinds(hits)


def test_file_renamed_away_from_the_word_fails(repo):
    commit(repo, {"zork-notes.md": "clean\n"})
    local["git"]("-C", str(repo), "mv", "zork-notes.md", "notes.md")
    commit(repo, {})
    assert kinds(audit.audit(str(repo), WORD, "HEAD")) == ["diff", "diff"]


def test_double_dash_and_double_plus_content_lines_are_scanned(repo):
    commit(repo, {"a.sql": "-- zork\n"})
    sha = commit(repo, {"a.sql": "++ zork\n"})
    hits = [hit for hit in audit.audit(str(repo), WORD, "HEAD") if hit.location == f"{sha} a.sql"]
    assert {hit.text for hit in hits} == {"--- zork", "+++ zork"}


def test_run_from_a_subdirectory_audits_the_whole_tree(repo, capsys):
    (repo / "sub").mkdir()
    commit(repo, {"top.txt": "zork\n", "sub/a.txt": "x\n"})
    assert run_main(repo / "sub", WORD) == 1
    assert "tree top.txt:1: zork" in capsys.readouterr().out


def test_path_with_a_colon_is_reported_whole(repo):
    commit(repo, {"a:b.txt": "zork\n"})
    assert [hit.location for hit in audit.audit(str(repo), WORD, "HEAD") if hit.kind == "tree"] == ["a:b.txt:1"]


def test_word_is_matched_literally_not_as_a_regex(repo):
    commit(repo, {"a.txt": "zxrk\n"})
    assert audit.audit(str(repo), "z.rk", "HEAD") == []


def test_word_after_a_form_feed_or_carriage_return_fails(repo):
    commit(repo, {"a.el": "a\fzork\n", "b.txt": "first\rzork\r"})
    commit(repo, {"a.el": "clean\n", "b.txt": "clean\n"})
    assert sorted(hit.location.split()[1] for hit in audit.audit(str(repo), WORD, "HEAD")) == ["a.el", "a.el", "b.txt", "b.txt"]
    commit(repo, {"a.el": "a\fzork\n"})
    assert "tree" in kinds(audit.audit(str(repo), WORD, "HEAD"))


def test_word_added_in_a_merge_resolution_fails(repo):
    git = local["git"]["-C", str(repo)]
    commit(repo, {"a.txt": "base\n"})
    git("switch", "-q", "-c", "side")
    commit(repo, {"a.txt": "side\n"})
    git("switch", "-q", "main")
    commit(repo, {"a.txt": "main\n"})
    git("merge", "-q", "side", retcode=1)
    (repo / "a.txt").write_text("zork\n")
    git("add", "a.txt")
    git("commit", "-q", "--no-edit")
    merge = git("rev-parse", "--short", "HEAD").strip()
    commit(repo, {"a.txt": "clean\n"})
    assert any(hit.location == f"{merge} a.txt" and hit.text == "+zork"
               for hit in audit.audit(str(repo), WORD, "HEAD"))


def test_word_in_a_binary_file_fails(repo):
    (repo / "blob.bin").write_bytes(b"\x00\xff zork \x00\n")
    commit(repo, {})
    found = kinds(audit.audit(str(repo), WORD, "HEAD"))
    assert "tree" in found and "diff" in found


def test_missing_word_exits_2(repo):
    assert run_main(repo) == 2


def test_git_error_exits_2(tmp_path):
    assert run_main(tmp_path, WORD) == 2
