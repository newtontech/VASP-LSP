from __future__ import annotations

import json
from pathlib import Path
from types import SimpleNamespace
from typing import Any

from vasp_lsp import agent_operations as ops
from vasp_lsp.agent_lsp import AgentLSP
from vasp_lsp.tool import main


def _write_incar(tmp_path: Path, text: str = "ENCUT = 520\nISMEAR = -5\n") -> Path:
    path = tmp_path / "INCAR"
    path.write_text(text, encoding="utf-8")
    return path


def _diagnostic() -> dict[str, Any]:
    return {
        "code": "VASP.TEST",
        "severity": "error",
        "source": "vasp-test",
        "message": "test diagnostic",
        "range": {
            "start": {"line": 0, "character": 0},
            "end": {"line": 0, "character": 5},
        },
        "fix_hints": ["Set ENCUT to a validated cutoff."],
        "manual_ref": "https://vasp.at/wiki/ENCUT",
        "confidence": 0.95,
        "category": "type/value",
        "blocking": True,
    }


def _collect(_: Path) -> list[dict[str, Any]]:
    return [_diagnostic()]


def _file_type(_: Path) -> str:
    return "INCAR"


def test_agent_operations_cover_context_hover_complete_symbols_and_fix(
    tmp_path: Path, monkeypatch: Any
) -> None:
    path = _write_incar(tmp_path, "&SYSTEM\nENCUT = 520\n")

    def fake_import(module_name: str) -> Any | None:
        if module_name.endswith(".features.completion"):
            return SimpleNamespace(
                get_completions=lambda path, text, file_type: [
                    {"label": "ENCUT", "detail": "Cutoff energy", "kind": 10},
                    {"label": "ENCUT", "detail": "duplicate"},
                ]
            )
        if module_name.endswith(".features.hover"):
            return SimpleNamespace(hover=lambda token, path, text, file_type: f"hover for {token}")
        if module_name.endswith(".features.symbols"):
            return SimpleNamespace(
                document_symbols=lambda path, text: [
                    {"name": "SYSTEM", "kind": "section", "line": 1, "column": 2}
                ]
            )
        return None

    monkeypatch.setattr(ops, "_import_optional", fake_import)

    context = ops.operation_path(
        path,
        "context",
        software="vasp",
        file_type_func=_file_type,
        collect_diagnostics=_collect,
        line=1,
        character=2,
    )
    assert context["capabilities"]["status"] == "available"
    assert context["context"]["nearby_symbols"][0]["name"] == "SYSTEM"

    complete = ops.operation_path(
        path,
        "complete",
        software="vasp",
        file_type_func=_file_type,
        collect_diagnostics=_collect,
    )
    assert [item["label"] for item in complete["items"]] == ["ENCUT"]

    hover = ops.operation_path(
        path,
        "hover",
        software="vasp",
        file_type_func=_file_type,
        collect_diagnostics=_collect,
        line=1,
        character=2,
    )
    assert hover["contents"] == "hover for ENCUT"

    symbols = ops.operation_path(
        path,
        "symbols",
        software="vasp",
        file_type_func=_file_type,
        collect_diagnostics=_collect,
    )
    assert symbols["items"][0]["selectionRange"]["start"]["line"] == 0

    fix = ops.operation_path(
        path,
        "fix",
        software="vasp",
        file_type_func=_file_type,
        collect_diagnostics=_collect,
        line=0,
        character=1,
    )
    assert fix["actions"][0]["diagnostic_code"] == "VASP.TEST"


def test_agent_operations_generic_fallbacks_and_unknown_operation(tmp_path: Path) -> None:
    path = _write_incar(tmp_path, "ENCUT = 520\n# comment\nISMEAR = -5\n")

    complete = ops.operation_path(
        path,
        "complete",
        software="vasp",
        file_type_func=_file_type,
        collect_diagnostics=_collect,
    )
    labels = {item["label"] for item in complete["items"]}
    assert {"ENCUT", "ISMEAR", "Set ENCUT to a validated cutoff."} <= labels

    hover = ops.operation_path(
        path,
        "hover",
        software="vasp",
        file_type_func=_file_type,
        collect_diagnostics=_collect,
        line=0,
        character=1,
    )
    assert "VASP.TEST" in hover["contents"]
    assert "https://vasp.at/wiki/ENCUT" in hover["contents"]

    unknown = ops.operation_path(
        path,
        "unsupported",
        software="vasp",
        file_type_func=_file_type,
        collect_diagnostics=_collect,
    )
    assert unknown["capabilities"]["status"] == "unavailable"
    assert "Unknown operation" in unknown["summary"]["note"]


def test_agent_lsp_and_cli_non_check_operations(tmp_path: Path, capsys: Any) -> None:
    path = _write_incar(tmp_path)
    assert AgentLSP.from_path(path).context(line=0, character=2)["operation"] == "context"
    assert AgentLSP.from_text("ENCUT = 520\n", uri="file:///INCAR").symbols()["uri"] == (
        "file:///INCAR"
    )

    assert main(["symbols", str(path), "--format", "json"]) == 0
    payload = json.loads(capsys.readouterr().out)
    assert payload["operation"] == "symbols"
    assert payload["capabilities"]["operation"] == "symbols"
