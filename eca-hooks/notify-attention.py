#!/usr/bin/env -S uv run --quiet
# /// script
# requires-python = ">=3.11"
# dependencies = ["plumbum", "pydash"]
# ///
"""ECA preToolCall hook: raise a macOS notification when ECA needs the user.

Two cases reach this hook, and ``approval == "ask"`` gates BOTH of them:
  1. A tool call awaiting approval -> notify with the tool name + key arg.
  2. The native eca/ask_user tool  -> notify with the question text.

Contract (hooks.clj):
  stdin  : event JSON. Top-level keys are snake_case; nested keys (tool_input)
           are kebab-case, matching the LLM's own argument names.
           { hook_name, hook_type, tool_name, server, tool_input,
             tool_call_id, approval, cwd, chat_id, agent, full_model, ... }
  stdout : JSON honored by ECA. We emit nothing (pure side effect).
  exit   : must NOT be 2 -- exit 2 rejects the tool call and sends stderr to
           the LLM as the rejection reason. Always exit 0.
"""

import json
import sys

import pydash
from plumbum import local

BODY_MAX = 160


# ---------------------------------------------------------------------------
# Message assembly
# ---------------------------------------------------------------------------

def question_of(payload: dict) -> str | None:
    """Return the question text if this is an ask_user call, else None."""
    if payload.get("tool_name") != "ask_user":
        return None
    return pydash.get(payload, "tool_input.question") or None


def tool_summary_of(payload: dict) -> str:
    """One-line summary: 'tool_name: arg' or 'server/tool_name'."""
    name = payload.get("tool_name", "")
    server = payload.get("server", "")
    if server and server != "eca":
        name = f"{server}/{name}"

    tool_input = pydash.get(payload, "tool_input") or {}
    arg = pydash.find(pydash.at(tool_input, "command", "path", "pattern", "url"))
    return f"{name}: {arg}" if arg else name


def message_of(payload: dict) -> str | None:
    """Return the notification body, or None to stay silent."""
    if payload.get("approval") != "ask":
        return None
    return question_of(payload) or tool_summary_of(payload)


# ---------------------------------------------------------------------------
# Text processing
# ---------------------------------------------------------------------------

def shorten(text: str) -> str:
    """Collapse whitespace to single spaces and cap at BODY_MAX chars."""
    flat = " ".join(text.split())
    if len(flat) > BODY_MAX:
        flat = flat[:BODY_MAX] + "..."
    return flat


def applescript_escape(text: str) -> str:
    """Escape for embedding in an AppleScript double-quoted string literal.

    Rules:
      - Backslashes are doubled  (\\  ->  \\\\)
      - Double-quotes get a backslash prefix  ("  ->  \\")
      - Newlines are collapsed to spaces
    """
    flat = " ".join(text.splitlines())
    flat = flat.replace("\\", "\\\\")
    flat = flat.replace('"', '\\"')
    return flat


# ---------------------------------------------------------------------------
# Notification
# ---------------------------------------------------------------------------

def notify(title: str, subtitle: str, body: str) -> None:
    """Fire a macOS notification via osascript (plumbum)."""
    script = (
        f'display notification "{applescript_escape(body)}" '
        f'with title "{applescript_escape(title)}" '
        f'subtitle "{applescript_escape(subtitle)}" '
        f'sound name "Morse"'
    )
    local["osascript"]("-e", script)


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def main() -> None:
    raw = sys.stdin.read()
    try:
        payload = json.loads(raw)
    except json.JSONDecodeError:
        return  # malformed JSON -> stay silent, exit 0

    if not isinstance(payload, dict):
        return

    message = message_of(payload)
    if not message:
        return

    cwd = payload.get("cwd", "")
    workspace = local.path(cwd).name if cwd else ""

    try:
        notify("ECA", workspace, shorten(message))
    except Exception:
        pass  # never let notification errors surface to ECA


if __name__ == "__main__":
    try:
        main()
    except Exception:
        pass
    sys.exit(0)
