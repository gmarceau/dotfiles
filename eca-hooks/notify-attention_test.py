"""Tests for notify-attention.py

Mirrors the shell test suite (notify-attention_test.sh) case for case.

The osascript invocation is intercepted via unittest.mock so no real
notification is sent and no Notification Center spam occurs.
"""

import importlib
import json
import sys
from io import StringIO
from pathlib import Path
from unittest.mock import patch

import pytest

# ---------------------------------------------------------------------------
# Import the hook module by path so the test file can live next to it without
# a package structure.
# ---------------------------------------------------------------------------

HOOK_PATH = Path(__file__).parent / "notify-attention.py"

spec = importlib.util.spec_from_file_location("notify_attention", HOOK_PATH)
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def run_hook(payload: dict | str) -> tuple[list[tuple], int]:
    """Feed *payload* (dict or raw string) through main() and return
    (osascript_calls, exit_code).

    osascript_calls is a list of (title, subtitle, body) tuples extracted
    from the AppleScript passed to osascript -e.
    """
    if isinstance(payload, dict):
        raw = json.dumps(payload)
    else:
        raw = payload

    calls: list[tuple] = []

    def fake_notify(title: str, subtitle: str, body: str) -> None:
        calls.append((title, subtitle, body))

    exit_code = 0
    with patch.object(mod, "notify", side_effect=fake_notify):
        with patch("sys.stdin", StringIO(raw)):
            try:
                mod.main()
            except SystemExit as exc:
                exit_code = int(exc.code or 0)

    return calls, exit_code


def notification_text(calls: list[tuple]) -> str:
    """Flatten all notification arguments into one searchable string."""
    return " ".join(" ".join(str(v) for v in c) for c in calls)


# ---------------------------------------------------------------------------
# Gating
# ---------------------------------------------------------------------------

class TestGating:
    def test_ask_notifies(self):
        calls, _ = run_hook({
            "approval": "ask",
            "tool_name": "shell_command",
            "server": "eca",
            "tool_input": {"command": "ls -l"},
            "cwd": "/Users/g/code/myproject",
        })
        text = notification_text(calls)
        assert "shell_command" in text, f"expected 'shell_command' in: {text!r}"

    def test_allow_silent(self):
        calls, _ = run_hook({
            "approval": "allow",
            "tool_name": "shell_command",
            "server": "eca",
            "tool_input": {"command": "ls"},
            "cwd": "/tmp",
        })
        assert calls == [], "expected no notification for approval=allow"

    def test_deny_silent(self):
        calls, _ = run_hook({
            "approval": "deny",
            "tool_name": "shell_command",
            "server": "eca",
            "tool_input": {"command": "ls"},
            "cwd": "/tmp",
        })
        assert calls == [], "expected no notification for approval=deny"


# ---------------------------------------------------------------------------
# Message content
# ---------------------------------------------------------------------------

class TestMessageContent:
    def test_ask_user_shows_question(self):
        calls, _ = run_hook({
            "approval": "ask",
            "tool_name": "ask_user",
            "server": "eca",
            "tool_input": {"question": "Which database should I use?"},
            "cwd": "/tmp",
        })
        text = notification_text(calls)
        assert "Which database should I use?" in text

    def test_ask_user_hides_tool_name(self):
        calls, _ = run_hook({
            "approval": "ask",
            "tool_name": "ask_user",
            "server": "eca",
            "tool_input": {"question": "Which database should I use?"},
            "cwd": "/tmp",
        })
        text = notification_text(calls)
        assert "ask_user" not in text, f"raw tool name should not appear: {text!r}"

    def test_shell_command_shows_command(self):
        calls, _ = run_hook({
            "approval": "ask",
            "tool_name": "shell_command",
            "server": "eca",
            "tool_input": {"command": "rm -rf /tmp/x"},
            "cwd": "/tmp",
        })
        text = notification_text(calls)
        assert "shell_command: rm -rf /tmp/x" in text

    def test_edit_file_shows_path(self):
        calls, _ = run_hook({
            "approval": "ask",
            "tool_name": "edit_file",
            "server": "eca",
            "tool_input": {"path": "/src/main.clj"},
            "cwd": "/tmp",
        })
        text = notification_text(calls)
        assert "edit_file: /src/main.clj" in text

    def test_grep_shows_pattern(self):
        calls, _ = run_hook({
            "approval": "ask",
            "tool_name": "grep",
            "server": "eca",
            "tool_input": {"pattern": "foo.*bar"},
            "cwd": "/tmp",
        })
        text = notification_text(calls)
        assert "grep: foo.*bar" in text

    def test_unknown_tool_shows_name(self):
        calls, _ = run_hook({
            "approval": "ask",
            "tool_name": "weird_tool",
            "server": "eca",
            "tool_input": {"whatever": 1},
            "cwd": "/tmp",
        })
        text = notification_text(calls)
        assert "weird_tool" in text

    def test_mcp_tool_shows_server_slash_name(self):
        calls, _ = run_hook({
            "approval": "ask",
            "tool_name": "create_issue",
            "server": "github",
            "tool_input": {},
            "cwd": "/tmp",
        })
        text = notification_text(calls)
        assert "github/create_issue" in text

    def test_workspace_basename_in_subtitle(self):
        calls, _ = run_hook({
            "approval": "ask",
            "tool_name": "shell_command",
            "server": "eca",
            "tool_input": {"command": "ls"},
            "cwd": "/Users/g/code/myproject",
        })
        assert calls, "expected a notification"
        subtitle = calls[0][1]
        assert subtitle == "myproject", f"expected subtitle 'myproject', got {subtitle!r}"


# ---------------------------------------------------------------------------
# Escaping (applescript_escape + shorten)
# ---------------------------------------------------------------------------

class TestEscaping:
    def test_double_quotes_escaped(self):
        escaped = mod.applescript_escape('Use "foo" or bar?')
        assert '\\"foo\\"' in escaped

    def test_backslash_escaped(self):
        escaped = mod.applescript_escape("grep \\d foo")
        assert "\\\\d" in escaped

    def test_multiline_flattened(self):
        escaped = mod.applescript_escape("line one\nline two")
        assert "\n" not in escaped
        assert "line one" in escaped
        assert "line two" in escaped

    def test_long_body_truncated(self):
        long_text = "x" * 400
        short = mod.shorten(long_text)
        assert short.endswith("...")
        assert len(short) <= mod.BODY_MAX + 3  # +3 for "..."

    def test_long_body_readable_length(self):
        long_text = "x" * 400
        short = mod.shorten(long_text)
        # The capped text should be substantially shorter than the input.
        assert len(short) < 300


# ---------------------------------------------------------------------------
# Robustness
# ---------------------------------------------------------------------------

class TestRobustness:
    def test_malformed_json_silent(self):
        calls, _ = run_hook("not json at all")
        assert calls == [], "malformed JSON must stay silent"

    def test_malformed_json_exit0(self):
        """main() must not raise; the top-level guard ensures exit 0."""
        # We call main() directly (not through the __main__ guard), so just
        # verify no exception escapes.
        with patch.object(mod, "notify"):
            with patch("sys.stdin", StringIO("not json")):
                mod.main()  # must not raise

    def test_normal_notification_exit0(self):
        _, rc = run_hook({
            "approval": "ask",
            "tool_name": "shell_command",
            "server": "eca",
            "tool_input": {"command": "ls"},
            "cwd": "/tmp",
        })
        assert rc == 0

    def test_stdin_read_once(self):
        """Both the approval and the tool name survive a single stdin read."""
        calls, _ = run_hook({
            "approval": "ask",
            "tool_name": "shell_command",
            "server": "eca",
            "tool_input": {"command": "ls"},
            "cwd": "/tmp",
        })
        text = notification_text(calls)
        assert "shell_command: ls" in text


# ---------------------------------------------------------------------------
# Unit tests for pure helpers
# ---------------------------------------------------------------------------

class TestHelpers:
    def test_shorten_collapses_whitespace(self):
        result = mod.shorten("  hello   world  ")
        assert result == "hello world"

    def test_shorten_no_truncation_when_short(self):
        result = mod.shorten("short text")
        assert result == "short text"
        assert "..." not in result

    def test_question_of_non_ask_user(self):
        payload = {"tool_name": "shell_command", "tool_input": {"command": "ls"}}
        assert mod.question_of(payload) is None

    def test_tool_summary_url_arg(self):
        payload = {
            "tool_name": "web_search",
            "server": "eca",
            "tool_input": {"url": "https://example.com"},
        }
        assert mod.tool_summary_of(payload) == "web_search: https://example.com"
