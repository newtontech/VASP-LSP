from lsprotocol.types import DiagnosticSeverity

from vasp_lsp.features.diagnostics import DiagnosticsProvider
from vasp_lsp.parsers.vasp_log_parser import VASPLogParser


def test_vasp_log_parser_converts_runtime_patterns_to_diagnostics() -> None:
    content = """running VASP
Error EDDDAV: Call to ZHEGV failed. Returncode = 5
more output
"""

    diagnostics = VASPLogParser(content, "file:///calc/slurm-123.out").parse()

    assert len(diagnostics) == 1
    diagnostic = diagnostics[0]
    assert diagnostic.id == "vasp.runtime.edddav_zhegv"
    assert diagnostic.line_index == 1
    assert diagnostic.severity == "error"
    assert diagnostic.confidence >= 0.8
    assert any("ALGO = Normal" in action.title for action in diagnostic.suggested_actions)


def test_vasp_log_parser_ignores_clean_logs() -> None:
    content = """DAV: 1 -0.111
reached required accuracy - stopping structural energy minimisation
General timing and accounting informations for this job:
"""

    assert VASPLogParser(content, "file:///calc/OUTCAR").parse() == []


def test_diagnostics_provider_handles_runtime_log_files() -> None:
    content = """Header
INVGRP: inverse of rotation matrix was not found
"""

    provider = DiagnosticsProvider()
    diagnostics = provider.get_diagnostics(content, "file:///calc/vasp.out")

    assert provider._get_file_type("file:///calc/OUTCAR") == "VASP_LOG"
    assert provider._get_file_type("file:///calc/slurm-123.out") == "VASP_LOG"
    assert len(diagnostics) == 1
    diagnostic = diagnostics[0]
    assert diagnostic.code == "vasp.runtime.invgrp_symmetry"
    assert diagnostic.severity == DiagnosticSeverity.Error
    assert diagnostic.source == "vasp-lsp-runtime"
    assert diagnostic.range.start.line == 1
    assert diagnostic.range.end.line == 1
    assert diagnostic.data["confidence"] == 0.9
    assert "ISYM = 0" in diagnostic.message


def test_diagnostic_snapshot_includes_runtime_metadata() -> None:
    content = "PSSYEVX: not enough eigenvalues found\n"

    snapshot = DiagnosticsProvider().get_diagnostics_snapshot(content, "file:///calc/stdout")

    assert snapshot["file_type"] == "VASP_LOG"
    assert snapshot["summary"]["error"] == 1
    item = snapshot["diagnostics"][0]
    assert item["code"] == "vasp.runtime.pssyevx_eigenvalues"
    assert item["confidence"] == 0.86
    assert item["category"] == "electronic_minimization"
    assert item["related_files"] == ["INCAR", "POSCAR", "OUTCAR", "stdout"]
