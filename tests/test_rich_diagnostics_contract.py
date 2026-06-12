from __future__ import annotations

import json
from dataclasses import dataclass
from types import SimpleNamespace

import pytest

from vasp_lsp import tool
from vasp_lsp.agent_lsp import AgentLSP
from vasp_lsp.rich_diagnostics import (
    DIAGNOSTIC_CATEGORIES,
    agent_check_payload,
    diagnostic_to_dict,
    serialize_diagnostics,
    severity_label,
)


def test_diagnostic_engine_v1_contract_shape() -> None:
    payload = agent_check_payload(
        software="vasp",
        uri="file:///tmp/input",
        diagnostics=[],
    )
    assert payload["diagnostic_engine"] == "1.0"
    assert payload["ok"] is True
    assert payload["diagnostics"] == []
    assert set(DIAGNOSTIC_CATEGORIES) >= {"syntax", "schema", "type/value"}


def test_legacy_diagnostic_is_enriched() -> None:
    diagnostic = {
        "code": "VASP001",
        "severity": "error",
        "message": "unknown keyword",
        "line": 2,
        "column": 3,
        "source": "vasp-lsp",
    }
    item = diagnostic_to_dict(diagnostic, software="vasp", path="input")
    assert item["severity"] == "error"
    assert item["category"] == "schema"
    assert item["blocking"] is True
    assert item["range"]["start"] == {"line": 1, "character": 2}
    assert item["fix_hints"] == []


@dataclass
class _DataclassDiagnostic:
    code: str
    severity: int
    message: str
    line: int
    column: int
    suggested_fix: str


class _JsonDiagnostic:
    def to_json(self) -> dict[str, object]:
        return {
            "code": "VASPJSON",
            "severity": "Hint",
            "message": "deprecated input style",
            "range": {"start_line": 3, "start_col": 4, "end_line": 3, "end_col": 9},
        }


class _BrokenJsonDiagnostic:
    def to_json(self) -> dict[str, object]:
        raise TypeError("not serializable")


def test_rich_diagnostics_handles_dataclass_json_and_lsp_ranges() -> None:
    dataclass_item = diagnostic_to_dict(
        _DataclassDiagnostic(
            code="VASPDATA",
            severity=2,
            message="encut runtime risk",
            line=4,
            column=5,
            suggested_fix="raise ENCUT",
        ),
        software="vasp",
        path="INCAR",
        file_type="INCAR",
    )
    assert dataclass_item["severity"] == "warning"
    assert dataclass_item["category"] == "preflight/runtime-risk"
    assert dataclass_item["fix_hints"] == ["raise ENCUT"]

    json_item = diagnostic_to_dict(_JsonDiagnostic(), software="vasp")
    assert json_item["severity"] == "hint"
    assert json_item["category"] == "style/deprecation"
    assert json_item["range"]["start"] == {"line": 3, "character": 4}

    lsp_range = SimpleNamespace(
        start=SimpleNamespace(line=6, character=7),
        end=SimpleNamespace(line=6, character=13),
    )
    lsp_item = diagnostic_to_dict(
        SimpleNamespace(
            range=lsp_range,
            severity=1,
            code=None,
            source="",
            message="POTCAR file reference missing",
        ),
        software="vasp",
    )
    assert lsp_item["category"] == "cross-file reference"
    assert lsp_item["blocking"] is True

    fallback_item = diagnostic_to_dict(_BrokenJsonDiagnostic(), software="vasp")
    assert fallback_item["code"] == "diagnostic"
    assert severity_label("info") == "information"


def test_serialize_diagnostics_is_stable() -> None:
    items = serialize_diagnostics(
        [
            {"code": "B", "line": 3, "column": 1, "message": "second"},
            {"code": "A", "line": 1, "column": 1, "message": "first"},
        ],
        software="vasp",
    )
    assert [item["code"] for item in items] == ["A", "B"]


def test_agent_lsp_text_path_and_operations(monkeypatch: pytest.MonkeyPatch, tmp_path) -> None:
    calls = []

    def fake_check_path(path):
        calls.append(path)
        return {
            "uri": path.resolve().as_uri(),
            "operation": "check",
            "ok": True,
            "diagnostics": [],
            "summary": {"blocking": 0},
        }

    monkeypatch.setattr("vasp_lsp.agent_lsp.check_path", fake_check_path)

    agent = AgentLSP.from_text("ENCUT = 520\n")
    payload = agent.check()
    assert payload["uri"] == "file:///input"
    assert calls[0].name == "input"

    input_path = tmp_path / "INCAR"
    input_path.write_text("ENCUT = 520\n", encoding="utf-8")
    path_payload = AgentLSP.from_path(input_path).check()
    assert path_payload["uri"] == input_path.resolve().as_uri()

    assert agent.context(1, 2)["position"] == {"line": 1, "character": 2}
    complete_payload = agent.complete()
    assert complete_payload["capabilities"]["operation"] == "complete"
    assert "items" in complete_payload

    hover_payload = agent.hover()
    assert hover_payload["capabilities"]["operation"] == "hover"
    assert "contents" in hover_payload

    symbols_payload = agent.symbols()
    assert symbols_payload["capabilities"]["operation"] == "symbols"
    assert "items" in symbols_payload


def test_tool_main_emits_json_and_fail_on_blocking(
    monkeypatch: pytest.MonkeyPatch, tmp_path, capsys
) -> None:
    input_path = tmp_path / "INCAR"
    input_path.write_text("BADTAG = 1\n", encoding="utf-8")

    monkeypatch.setattr(
        tool,
        "_collect_diagnostics",
        lambda path: [
            {
                "code": "VASP001",
                "severity": "error",
                "message": "unknown keyword",
                "line": 1,
                "column": 1,
            }
        ],
    )

    payload = tool.check_path(input_path)
    assert payload["software"] == "vasp"
    assert payload["diagnostics"][0]["file_type"] == "INCAR"
    assert payload["ok"] is False

    assert tool.main(["check", str(input_path)]) == 0
    printed = json.loads(capsys.readouterr().out)
    assert printed["summary"]["blocking"] == 1

    assert tool.main(["check", str(input_path), "--fail-on-blocking"]) == 1
    capsys.readouterr()

    assert tool.main(["hover", str(input_path)]) == 0
    hover_payload = json.loads(capsys.readouterr().out)
    assert hover_payload["operation"] == "hover"
    assert hover_payload["capabilities"]["operation"] == "hover"
    assert hover_payload["capabilities"]["status"] in {"available", "unavailable"}


def test_tool_collect_diagnostics_and_file_type_smoke(tmp_path) -> None:
    input_path = tmp_path / "INCAR"
    input_path.write_text("ENCUT = 520\n", encoding="utf-8")

    assert isinstance(tool._collect_diagnostics(input_path), list)
    assert tool._file_type(input_path) == "INCAR"
    assert tool._file_type(tmp_path / "calc.vasp") == "vasp"
    assert tool._file_type(tmp_path / "README") == "readme"
