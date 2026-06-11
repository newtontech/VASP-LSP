"""Parser for VASP runtime logs such as OUTCAR, stdout, stderr, and slurm logs."""

from __future__ import annotations

from dataclasses import dataclass

from ..schemas.vasp_error_patterns import (
    RuntimeSuggestedAction,
    match_vasp_error_patterns,
)


@dataclass(frozen=True)
class VASPLogDiagnostic:
    """Structured runtime diagnostic produced from a VASP log line."""

    id: str
    severity: str
    confidence: float
    category: str
    line_index: int
    line_text: str
    related_files: tuple[str, ...]
    suggested_actions: tuple[RuntimeSuggestedAction, ...]


class VASPLogParser:
    """Parse VASP runtime logs into structured diagnostics."""

    def __init__(self, content: str, document_uri: str = ""):
        self.content = content
        self.document_uri = document_uri

    def parse(self) -> list[VASPLogDiagnostic]:
        """Return structured diagnostics for known VASP runtime failures."""
        diagnostics: list[VASPLogDiagnostic] = []
        for match in match_vasp_error_patterns(self.content):
            pattern = match.pattern
            diagnostics.append(
                VASPLogDiagnostic(
                    id=pattern.id,
                    severity=pattern.severity,
                    confidence=pattern.confidence,
                    category=pattern.category,
                    line_index=match.line_index,
                    line_text=match.line_text,
                    related_files=pattern.related_files,
                    suggested_actions=pattern.suggested_actions,
                )
            )
        return diagnostics
